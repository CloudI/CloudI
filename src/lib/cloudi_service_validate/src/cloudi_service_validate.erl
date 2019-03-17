%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Validate Service==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2015-2019 Michael Truog <mjtruog at protonmail dot com>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a
%%% copy of this software and associated documentation files (the "Software"),
%%% to deal in the Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%% and/or sell copies of the Software, and to permit persons to whom the
%%% Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%%% DEALINGS IN THE SOFTWARE.
%%%
%%% @author Michael Truog <mjtruog at protonmail dot com>
%%% @copyright 2015-2019 Michael Truog
%%% @version 1.8.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_validate).
-author('mjtruog at protonmail dot com').

-behaviour(cloudi_service).

%% external interface

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include_lib("cloudi_core/include/cloudi_service.hrl").

-define(DEFAULT_VALIDATE_REQUEST_INFO,        undefined).
-define(DEFAULT_VALIDATE_REQUEST,             undefined).
-define(DEFAULT_VALIDATE_RESPONSE_INFO,       undefined).
-define(DEFAULT_VALIDATE_RESPONSE,
        fun
            (<<>>, <<>>) ->
                false;
            (_, _) ->
                true
        end).
-define(DEFAULT_FAILURES_SOURCE_DIE,              false).
-define(DEFAULT_FAILURES_SOURCE_MAX_COUNT,            2). % see below:
        % (similar to the MaxR configuration value for services)
-define(DEFAULT_FAILURES_SOURCE_MAX_PERIOD,          60). % seconds, see below:
        % (similar to the MaxT configuration value for services)
        % If you want the source service to eventually fail,
        % use the service's MaxT/MaxR as the failures_source_max_period value
        % (e.g., 300/5 == 60 seconds).  Can also use the value 'infinity'
        % to accumulate a failure count indefinitely.
-define(DEFAULT_FAILURES_DEST_DIE,                false).
-define(DEFAULT_FAILURES_DEST_MAX_COUNT,              2). % see below:
        % (similar to the MaxR configuration value for services)
-define(DEFAULT_FAILURES_DEST_MAX_PERIOD,            60). % seconds, see below:
        % (similar to the MaxT configuration value for services)
        % If you want the destination service to eventually fail,
        % use the service's MaxT/MaxR as the failures_dest_max_period value
        % (e.g., 300/5 == 60 seconds).  Can also use the value 'infinity'
        % to accumulate a failure count indefinitely.

-record(request,
    {
        request_type :: cloudi_service:request_type(),
        name :: cloudi_service:service_name(),
        pattern :: cloudi_service:service_name_pattern(),
        timeout :: cloudi_service:timeout_value_milliseconds(),
        trans_id :: cloudi_service:trans_id(),
        source :: cloudi_service:source(),
        destination :: pid()
    }).

-record(state,
    {
        validate_request_info :: undefined | fun((any()) -> boolean()),
        validate_request :: undefined | fun((any(), any()) -> boolean()),
        validate_response_info :: undefined | fun((any()) -> boolean()),
        validate_response :: undefined | fun((any(), any()) -> boolean()),
        failures_source_die :: boolean(),
        failures_source_max_count :: pos_integer(),
        failures_source_max_period :: infinity | pos_integer(),
        failures_source = #{}
            :: #{pid() := list(cloudi_timestamp:seconds_monotonic())},
        failures_dest_die :: boolean(),
        failures_dest_max_count :: pos_integer(),
        failures_dest_max_period :: infinity | pos_integer(),
        failures_dest = #{}
            :: #{pid() := list(cloudi_timestamp:seconds_monotonic())},
        requests = #{}
            :: #{cloudi_service:trans_id() := #request{}}
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, Prefix, _Timeout, Dispatcher) ->
    Defaults = [
        {validate_request_info,         ?DEFAULT_VALIDATE_REQUEST_INFO},
        {validate_request,              ?DEFAULT_VALIDATE_REQUEST},
        {validate_response_info,        ?DEFAULT_VALIDATE_RESPONSE_INFO},
        {validate_response,             ?DEFAULT_VALIDATE_RESPONSE},
        {failures_source_die,           ?DEFAULT_FAILURES_SOURCE_DIE},
        {failures_source_max_count,     ?DEFAULT_FAILURES_SOURCE_MAX_COUNT},
        {failures_source_max_period,    ?DEFAULT_FAILURES_SOURCE_MAX_PERIOD},
        {failures_dest_die,             ?DEFAULT_FAILURES_DEST_DIE},
        {failures_dest_max_count,       ?DEFAULT_FAILURES_DEST_MAX_COUNT},
        {failures_dest_max_period,      ?DEFAULT_FAILURES_DEST_MAX_PERIOD}],
    [ValidateRequestInfo0, ValidateRequest0,
     ValidateResponseInfo0, ValidateResponse0,
     FailuresSrcDie, FailuresSrcMaxCount, FailuresSrcMaxPeriod,
     FailuresDstDie, FailuresDstMaxCount, FailuresDstMaxPeriod
     ] = cloudi_proplists:take_values(Defaults, Args),
    ValidateRequestInfo1 = cloudi_args_type:
                           function_optional(ValidateRequestInfo0, 1),
    ValidateRequest1 = cloudi_args_type:
                       function_optional(ValidateRequest0, 2),
    ValidateResponseInfo1 = cloudi_args_type:
                            function_optional(ValidateResponseInfo0, 1),
    ValidateResponse1 = cloudi_args_type:
                        function_optional(ValidateResponse0, 2),
    true = is_boolean(FailuresSrcDie),
    true = is_integer(FailuresSrcMaxCount) andalso (FailuresSrcMaxCount > 0),
    true = (FailuresSrcMaxPeriod =:= infinity) orelse
           (is_integer(FailuresSrcMaxPeriod) andalso
            (FailuresSrcMaxPeriod > 0)),
    true = is_boolean(FailuresDstDie),
    true = is_integer(FailuresDstMaxCount) andalso (FailuresDstMaxCount > 0),
    true = (FailuresDstMaxPeriod =:= infinity) orelse
           (is_integer(FailuresDstMaxPeriod) andalso
            (FailuresDstMaxPeriod > 0)),
    false = cloudi_service_name:pattern(Prefix),
    cloudi_service:subscribe(Dispatcher, "*"),
    {ok, #state{validate_request_info = ValidateRequestInfo1,
                validate_request = ValidateRequest1,
                validate_response_info = ValidateResponseInfo1,
                validate_response = ValidateResponse1,
                failures_source_die = FailuresSrcDie,
                failures_source_max_count = FailuresSrcMaxCount,
                failures_source_max_period = FailuresSrcMaxPeriod,
                failures_dest_die = FailuresDstDie,
                failures_dest_max_count = FailuresDstMaxCount,
                failures_dest_max_period = FailuresDstMaxPeriod}}.

cloudi_service_handle_request(RequestType, Name, Pattern, RequestInfo, Request,
                              Timeout, Priority, TransId, SrcPid,
                              #state{validate_request_info = RequestInfoF,
                                     validate_request = RequestF,
                                     requests = Requests} = State,
                              Dispatcher) ->
    case validate(RequestInfoF, RequestF,
                  RequestInfo, Request) of
        true ->
            [ValidateName] = cloudi_service_name:parse(Name, Pattern),
            case cloudi_service:get_pid(Dispatcher, ValidateName, Timeout) of
                {ok, {_, DstPid} = PatternPid} ->
                    case cloudi_service:send_async_active(Dispatcher,
                                                          ValidateName,
                                                          RequestInfo, Request,
                                                          Timeout, Priority,
                                                          PatternPid) of
                        {ok, ValidateTransId} ->
                            ValidateRequest =
                                #request{request_type = RequestType,
                                         name = Name,
                                         pattern = Pattern,
                                         timeout = Timeout,
                                         trans_id = TransId,
                                         source = SrcPid,
                                         destination = DstPid},
                            {noreply,
                             State#state{requests = maps:put(ValidateTransId,
                                                             ValidateRequest,
                                                             Requests)}};
                        {error, timeout} ->
                            request_failed(SrcPid, State)
                    end;
                {error, timeout} ->
                    request_failed(SrcPid, State)
            end;
        false ->
            request_failed(SrcPid, State)
    end.

cloudi_service_handle_info(#return_async_active{response_info = ResponseInfo,
                                                response = Response,
                                                timeout = Timeout,
                                                trans_id = ValidateTransId},
                           #state{validate_response_info = ResponseInfoF,
                                  validate_response = ResponseF,
                                  failures_source_die = FailuresSrcDie,
                                  failures_source_max_count =
                                      FailuresSrcMaxCount,
                                  failures_source_max_period =
                                      FailuresSrcMaxPeriod,
                                  failures_source = FailuresSrc,
                                  failures_dest_die = FailuresDstDie,
                                  failures_dest_max_count =
                                      FailuresDstMaxCount,
                                  failures_dest_max_period =
                                      FailuresDstMaxPeriod,
                                  failures_dest = FailuresDst,
                                  requests = Requests} = State,
                           Dispatcher) ->
    {#request{request_type = RequestType,
              name = Name,
              pattern = Pattern,
              trans_id = TransId,
              source = SrcPid,
              destination = DstPid},
     NewRequests} = maps:take(ValidateTransId, Requests),
    case validate(ResponseInfoF, ResponseF,
                  ResponseInfo, Response) of
        true ->
            cloudi_service:return_nothrow(Dispatcher, RequestType,
                                          Name, Pattern,
                                          ResponseInfo, Response,
                                          Timeout, TransId, SrcPid),
            {noreply, State#state{requests = NewRequests}};
        false ->
            {DeadSrc, NewFailuresSrc} = failure(FailuresSrcDie,
                                                FailuresSrcMaxCount,
                                                FailuresSrcMaxPeriod,
                                                SrcPid, FailuresSrc),
            if
                DeadSrc =:= true ->
                    ok;
                DeadSrc =:= false ->
                    cloudi_service:return_nothrow(Dispatcher, RequestType,
                                                  Name, Pattern,
                                                  <<>>, <<>>,
                                                  Timeout, TransId, SrcPid)
            end,
            {_, NewFailuresDst} = failure(FailuresDstDie,
                                          FailuresDstMaxCount,
                                          FailuresDstMaxPeriod,
                                          DstPid, FailuresDst),
            {noreply, State#state{failures_source = NewFailuresSrc,
                                  failures_dest = NewFailuresDst,
                                  requests = NewRequests}}
    end;

cloudi_service_handle_info(#timeout_async_active{trans_id = ValidateTransId},
                           #state{failures_source_die = FailuresSrcDie,
                                  failures_source_max_count =
                                      FailuresSrcMaxCount,
                                  failures_source_max_period =
                                      FailuresSrcMaxPeriod,
                                  failures_source = FailuresSrc,
                                  failures_dest_die = FailuresDstDie,
                                  failures_dest_max_count =
                                      FailuresDstMaxCount,
                                  failures_dest_max_period =
                                      FailuresDstMaxPeriod,
                                  failures_dest = FailuresDst,
                                  requests = Requests} = State,
                           Dispatcher) ->
    {#request{request_type = RequestType,
              name = Name,
              pattern = Pattern,
              timeout = Timeout,
              trans_id = TransId,
              source = SrcPid,
              destination = DstPid},
     NewRequests} = maps:take(ValidateTransId, Requests),
    {DeadSrc, NewFailuresSrc} = failure(FailuresSrcDie,
                                        FailuresSrcMaxCount,
                                        FailuresSrcMaxPeriod,
                                        SrcPid, FailuresSrc),
    if
        DeadSrc =:= true ->
            ok;
        DeadSrc =:= false ->
            cloudi_service:return_nothrow(Dispatcher, RequestType,
                                          Name, Pattern,
                                          <<>>, <<>>,
                                          Timeout, TransId, SrcPid)
    end,
    {_, NewFailuresDst} = failure(FailuresDstDie,
                                  FailuresDstMaxCount,
                                  FailuresDstMaxPeriod,
                                  DstPid, FailuresDst),
    {noreply, State#state{failures_source = NewFailuresSrc,
                          failures_dest = NewFailuresDst,
                          requests = NewRequests}};

cloudi_service_handle_info({'DOWN', _MonitorRef, process, Pid, _Info},
                           #state{failures_source_die = FailuresSrcDie,
                                  failures_source = FailuresSrc,
                                  failures_dest_die = FailuresDstDie,
                                  failures_dest = FailuresDst} = State,
                           _Dispatcher) ->
    NewFailuresSrc = if
        FailuresSrcDie =:= true ->
            maps:remove(Pid, FailuresSrc);
        FailuresSrcDie =:= false ->
            FailuresSrc
    end,
    NewFailuresDst = if
        FailuresDstDie =:= true ->
            maps:remove(Pid, FailuresDst);
        FailuresDstDie =:= false ->
            FailuresDst
    end,
    {noreply, State#state{failures_source = NewFailuresSrc,
                          failures_dest = NewFailuresDst}};

cloudi_service_handle_info(Request, State, _Dispatcher) ->
    {stop, cloudi_string:format("Unknown info \"~w\"", [Request]), State}.

cloudi_service_terminate(_Reason, _Timeout, _State) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

validate_f_return(Value) when is_boolean(Value) ->
    Value.

validate(undefined, undefined, _, _) ->
    true;
validate(undefined, RF, RInfo, R) ->
    validate_f_return(RF(RInfo, R));
validate(RInfoF, undefined, RInfo, _) ->
    validate_f_return(RInfoF(RInfo));
validate(RInfoF, RF, RInfo, R) ->
    validate_f_return(RInfoF(RInfo)) andalso validate_f_return(RF(RInfo, R)).

request_failed(SrcPid,
               #state{failures_source_die = FailuresSrcDie,
                      failures_source_max_count = FailuresSrcMaxCount,
                      failures_source_max_period = FailuresSrcMaxPeriod,
                      failures_source = FailuresSrc} = State) ->
    {DeadSrc, NewFailuresSrc} = failure(FailuresSrcDie,
                                        FailuresSrcMaxCount,
                                        FailuresSrcMaxPeriod,
                                        SrcPid, FailuresSrc),
    if
        DeadSrc =:= true ->
            {noreply,
             State#state{failures_source = NewFailuresSrc}};
        DeadSrc =:= false ->
            {reply, <<>>,
             State#state{failures_source = NewFailuresSrc}}
    end.

failure(false, _, _, _, Failures) ->
    {false, Failures};
failure(true, MaxCount, MaxPeriod, Pid, Failures) ->
    case erlang:is_process_alive(Pid) of
        true ->
            SecondsNow = cloudi_timestamp:seconds_monotonic(),
            case maps:find(Pid, Failures) of
                {ok, FailureList} ->
                    failure_check(SecondsNow, FailureList,
                                  MaxCount, MaxPeriod, Pid, Failures);
                error ->
                    erlang:monitor(process, Pid),
                    failure_check(SecondsNow, [],
                                  MaxCount, MaxPeriod, Pid, Failures)
            end;
        false ->
            {true, Failures}
    end.

failure_store(FailureList, FailureCount, MaxCount, Pid, Failures) ->
    NewFailures = maps:put(Pid, FailureList, Failures),
    if
        FailureCount == MaxCount ->
            failure_kill(Pid),
            {true, NewFailures};
        true ->
            {false, NewFailures}
    end.

failure_check(SecondsNow, FailureList, MaxCount, infinity, Pid, Failures) ->
    NewFailureCount = erlang:length(FailureList),
    failure_store([SecondsNow | FailureList], NewFailureCount + 1,
                  MaxCount, Pid, Failures);
failure_check(SecondsNow, FailureList, MaxCount, MaxPeriod, Pid, Failures) ->
    {NewFailureCount,
     NewFailureList} = cloudi_timestamp:seconds_filter_monotonic(FailureList,
                                                                 SecondsNow,
                                                                 MaxPeriod),
    failure_store([SecondsNow | NewFailureList], NewFailureCount + 1,
                  MaxCount, Pid, Failures).

failure_kill(Pid) ->
    erlang:exit(Pid, cloudi_service_validate).
