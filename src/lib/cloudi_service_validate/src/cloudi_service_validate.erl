%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Validate Service==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2015, Michael Truog <mjtruog at gmail dot com>
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%% 
%%%     * Redistributions of source code must retain the above copyright
%%%       notice, this list of conditions and the following disclaimer.
%%%     * Redistributions in binary form must reproduce the above copyright
%%%       notice, this list of conditions and the following disclaimer in
%%%       the documentation and/or other materials provided with the
%%%       distribution.
%%%     * All advertising materials mentioning features or use of this
%%%       software must display the following acknowledgment:
%%%         This product includes software developed by Michael Truog
%%%     * The name of the author may not be used to endorse or promote
%%%       products derived from this software without specific prior
%%%       written permission
%%% 
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
%%% DAMAGE.
%%%
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2015 Michael Truog
%%% @version 1.4.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_validate).
-author('mjtruog [at] gmail (dot) com').

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
        type :: cloudi_service:request_type(),
        name :: cloudi_service:service_name(),
        pattern :: cloudi_service:service_name_pattern(),
        timeout :: cloudi_service:timeout_value_milliseconds(),
        trans_id :: cloudi_service:trans_id(),
        source :: cloudi_service:source(),
        destination :: pid()
    }).

-ifdef(ERLANG_OTP_VERSION_16).
-type dict_proxy(_Key, _Value) :: dict().
-else.
-type dict_proxy(Key, Value) :: dict:dict(Key, Value).
-endif.
-record(state,
    {
        validate_request_info :: fun((any()) -> boolean()),
        validate_request :: fun((any(), any()) -> boolean()),
        validate_response_info :: fun((any()) -> boolean()),
        validate_response :: fun((any(), any()) -> boolean()),
        failures_source_die :: boolean(),
        failures_source_max_count :: pos_integer(),
        failures_source_max_period :: infinity | pos_integer(),
        failures_source = dict:new() :: dict_proxy(pid(),
                                                   list(erlang:timestamp())),
        failures_dest_die :: boolean(),
        failures_dest_max_count :: pos_integer(),
        failures_dest_max_period :: infinity | pos_integer(),
        failures_dest = dict:new() :: dict_proxy(pid(),
                                                 list(erlang:timestamp())),
        requests = dict:new() :: dict_proxy(cloudi_service:trans_id(),
                                            #request{})
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
    ValidateRequestInfo1 = case ValidateRequestInfo0 of
        undefined ->
            undefined;
        {ValidateRequestInfoModule, ValidateRequestInfoFunction}
            when is_atom(ValidateRequestInfoModule),
                 is_atom(ValidateRequestInfoFunction) ->
            true = erlang:function_exported(ValidateRequestInfoModule,
                                            ValidateRequestInfoFunction, 1),
            fun(ValidateRequestInfoArg1) ->
                ValidateRequestInfoModule:
                ValidateRequestInfoFunction(ValidateRequestInfoArg1)
            end;
        _ when is_function(ValidateRequestInfo0, 1) ->
            ValidateRequestInfo0
    end,
    ValidateRequest1 = case ValidateRequest0 of
        undefined ->
            undefined;
        {ValidateRequestModule, ValidateRequestFunction}
            when is_atom(ValidateRequestModule),
                 is_atom(ValidateRequestFunction) ->
            true = erlang:function_exported(ValidateRequestModule,
                                            ValidateRequestFunction, 2),
            fun(ValidateRequestArg1, ValidateRequestArg2) ->
                ValidateRequestModule:
                ValidateRequestFunction(ValidateRequestArg1,
                                        ValidateRequestArg2)
            end;
        _ when is_function(ValidateRequest0, 2) ->
            ValidateRequest0
    end,
    ValidateResponseInfo1 = case ValidateResponseInfo0 of
        undefined ->
            undefined;
        {ValidateResponseInfoModule, ValidateResponseInfoFunction}
            when is_atom(ValidateResponseInfoModule),
                 is_atom(ValidateResponseInfoFunction) ->
            true = erlang:function_exported(ValidateResponseInfoModule,
                                            ValidateResponseInfoFunction, 1),
            fun(ValidateResponseInfoArg1) ->
                ValidateResponseInfoModule:
                ValidateResponseInfoFunction(ValidateResponseInfoArg1)
            end;
        _ when is_function(ValidateResponseInfo0, 1) ->
            ValidateResponseInfo0
    end,
    ValidateResponse1 = case ValidateResponse0 of
        undefined ->
            undefined;
        {ValidateResponseModule, ValidateResponseFunction}
            when is_atom(ValidateResponseModule),
                 is_atom(ValidateResponseFunction) ->
            true = erlang:function_exported(ValidateResponseModule,
                                            ValidateResponseFunction, 2),
            fun(ValidateResponseArg1, ValidateResponseArg2) ->
                ValidateResponseModule:
                ValidateResponseFunction(ValidateResponseArg1,
                                         ValidateResponseArg2)
            end;
        _ when is_function(ValidateResponse0, 2) ->
            ValidateResponse0
    end,
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
    false = cloudi_x_trie:is_pattern(Prefix),
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

cloudi_service_handle_request(Type, Name, Pattern, RequestInfo, Request,
                              Timeout, Priority, TransId, SrcPid,
                              #state{validate_request_info = RequestInfoF,
                                     validate_request = RequestF,
                                     requests = Requests} = State,
                              Dispatcher) ->
    case validate(RequestInfoF, RequestF,
                  RequestInfo, Request) of
        true ->
            [ValidateName] = cloudi_service:service_name_parse(Name, Pattern),
            case cloudi_service:get_pid(Dispatcher, ValidateName, Timeout) of
                {ok, {_, DstPid} = PatternPid} ->
                    case cloudi_service:send_async_active(Dispatcher,
                                                          ValidateName,
                                                          RequestInfo, Request,
                                                          Timeout, Priority,
                                                          PatternPid) of
                        {ok, ValidateTransId} ->
                            ValidateRequest = #request{type = Type,
                                                       name = Name,
                                                       pattern = Pattern,
                                                       timeout = Timeout,
                                                       trans_id = TransId,
                                                       source = SrcPid,
                                                       destination = DstPid},
                            {noreply,
                             State#state{requests = dict:store(ValidateTransId,
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
    #request{type = Type,
             name = Name,
             pattern = Pattern,
             trans_id = TransId,
             source = SrcPid,
             destination = DstPid} = dict:fetch(ValidateTransId, Requests),
    NewRequests = dict:erase(ValidateTransId, Requests),
    case validate(ResponseInfoF, ResponseF,
                  ResponseInfo, Response) of
        true ->
            cloudi_service:return_nothrow(Dispatcher, Type, Name, Pattern,
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
                    cloudi_service:return_nothrow(Dispatcher,
                                                  Type, Name, Pattern,
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
    #request{type = Type,
             name = Name,
             pattern = Pattern,
             timeout = Timeout,
             trans_id = TransId,
             source = SrcPid,
             destination = DstPid} = dict:fetch(ValidateTransId, Requests),
    NewRequests = dict:erase(ValidateTransId, Requests),
    {DeadSrc, NewFailuresSrc} = failure(FailuresSrcDie,
                                        FailuresSrcMaxCount,
                                        FailuresSrcMaxPeriod,
                                        SrcPid, FailuresSrc),
    if
        DeadSrc =:= true ->
            ok;
        DeadSrc =:= false ->
            cloudi_service:return_nothrow(Dispatcher, Type, Name, Pattern,
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
            dict:erase(Pid, FailuresSrc);
        FailuresSrcDie =:= false ->
            FailuresSrc
    end,
    NewFailuresDst = if
        FailuresDstDie =:= true ->
            dict:erase(Pid, FailuresDst);
        FailuresDstDie =:= false ->
            FailuresDst
    end,
    {noreply, State#state{failures_source = NewFailuresSrc,
                          failures_dest = NewFailuresDst}};

cloudi_service_handle_info(Request, State, _Dispatcher) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(_Reason, _Timeout, #state{}) ->
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
            SecondsNow = cloudi_timestamp:seconds(),
            case dict:find(Pid, Failures) of
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

failure_store(FailureList, MaxCount, Pid, Failures) ->
    NewFailures = dict:store(Pid, FailureList, Failures),
    if
        erlang:length(FailureList) == MaxCount ->
            failure_kill(Pid),
            {true, NewFailures};
        true ->
            {false, NewFailures}
    end.

failure_check(SecondsNow, FailureList, MaxCount, infinity, Pid, Failures) ->
    failure_store([SecondsNow | FailureList], MaxCount, Pid, Failures);
failure_check(SecondsNow, FailureList, MaxCount, MaxPeriod, Pid, Failures) ->
    NewFailureList = cloudi_timestamp:seconds_filter(FailureList,
                                                     SecondsNow, MaxPeriod),
    failure_store([SecondsNow | NewFailureList], MaxCount, Pid, Failures).

failure_kill(Pid) ->
    erlang:exit(Pid, cloudi_service_validate).
