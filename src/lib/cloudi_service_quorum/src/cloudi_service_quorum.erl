%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Quorum Service==
%%% Using this service can provide Byzantine fault tolerance for any other
%%% services.  The service prefix is used to accept any service requests
%%% that match the prefix, while the suffix is used to achieve the
%%% configured number of service request responses
%%% (i.e., quorum: the minimum number of services able to process a
%%%  service request to achieve the same response).  If quorum is met,
%%% the agreed upon response is returned.  Otherwise, the service request to
%%% this service will timeout (i.e., receive no response).
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2013-2015, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2013-2015 Michael Truog
%%% @version 1.4.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_quorum).
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

-define(DEFAULT_QUORUM,                       byzantine). % see below:
        % quorum is specified either as an absolute integer or percentage,
        % or as 'byzantine' for (N - floor((N - 1) / 3))
        % ('byzantine' requires that less than 1/3rd of the
        %  responses be erroneous (e.g., a timeout or a non-matching response))
-define(DEFAULT_USE_RESPONSE_INFO,                 true). % match response_info
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
        source :: cloudi_service:source(),
        count_required :: pos_integer(),
        count_total :: pos_integer(),
        count_responses = 0 :: non_neg_integer(),
        count_correct = 0 :: non_neg_integer(),
        responses = [] :: list({{cloudi_service:response_info(),
                                 cloudi_service:response()} |
                                cloudi_service:response(), pos_integer()}),
        returned :: boolean()
    }).

-record(pending,
    {
        trans_id :: cloudi_service:trans_id(),
        dest :: pid()
    }).

-ifdef(ERLANG_OTP_VERSION_16).
-type dict_proxy(_Key, _Value) :: dict().
-else.
-type dict_proxy(Key, Value) :: dict:dict(Key, Value).
-endif.
-record(state,
    {
        prefix :: string(),
        quorum :: byzantine | number(),
        use_response_info :: boolean(),
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
        requests = dict:new() :: dict_proxy(cloudi_service:trans_id(), % orig
                                            #request{}),
        pending = dict:new() :: dict_proxy(cloudi_service:trans_id(), % new
                                           cloudi_service:trans_id()) % orig
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, Prefix, _Timeout, Dispatcher) ->
    Defaults = [
        {quorum,                        ?DEFAULT_QUORUM},
        {use_response_info,             ?DEFAULT_USE_RESPONSE_INFO},
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
    [Quorum, UseResponseInfo,
     ValidateRequestInfo0, ValidateRequest0,
     ValidateResponseInfo0, ValidateResponse0,
     FailuresSrcDie, FailuresSrcMaxCount, FailuresSrcMaxPeriod,
     FailuresDstDie, FailuresDstMaxCount, FailuresDstMaxPeriod
     ] = cloudi_proplists:take_values(Defaults, Args),
    true = ((is_atom(Quorum) andalso
             (Quorum =:= byzantine)) orelse
            (is_float(Quorum) andalso
             (Quorum > 0.0) andalso (Quorum =< 1.0)) orelse
            (is_integer(Quorum) andalso
             (Quorum > 0))),
    true = is_boolean(UseResponseInfo),
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
    {ok, #state{quorum = Quorum,
                use_response_info = UseResponseInfo,
                validate_request_info = ValidateRequestInfo1,
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
                              #state{quorum = Quorum,
                                     validate_request_info = RequestInfoF,
                                     validate_request = RequestF} = State,
                              Dispatcher) ->
    case validate(RequestInfoF, RequestF,
                  RequestInfo, Request) of
        true ->
            [QuorumName] = cloudi_service:service_name_parse(Name, Pattern),
            case cloudi_service:get_pids(Dispatcher, QuorumName, Timeout) of
                {ok, PatternPids} ->
                    Count = erlang:length(PatternPids),
                    CountRequired = if
                        Quorum =:= byzantine ->
                            if
                                Count < 4 ->
                                    ?LOG_ERROR("Byzantine quorum not met!"
                                               " ~p N=~p", [QuorumName, Count]),
                                    undefined;
                                true ->
                                    Count - cloudi_math:floor((Count - 1) / 3)
                            end;
                        is_integer(Quorum) ->
                            if
                                Quorum > Count ->
                                    ?LOG_ERROR("Absolute quorum not met!"
                                               " ~p N=~p", [QuorumName, Count]),
                                    undefined;
                                true ->
                                    Quorum
                            end;
                        is_float(Quorum) ->
                            erlang:min(Count,
                                       cloudi_math:ceil(Quorum * Count))
                    end,
                    if
                        CountRequired =:= undefined ->
                            request_failed(SrcPid, State);
                        true ->
                            mcast(PatternPids, QuorumName,
                                  Type, Name, Pattern, RequestInfo, Request,
                                  Timeout, Priority, TransId, SrcPid,
                                  CountRequired, State, Dispatcher)
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
                                                trans_id = QuorumTransId},
                           #state{use_response_info = UseResponseInfo,
                                  validate_response_info = ResponseInfoF,
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
                                  requests = Requests,
                                  pending = Pending} = State,
                           Dispatcher) ->
    #pending{trans_id = TransId,
             dest = DstPid} = dict:fetch(QuorumTransId, Pending),
    NewPending = dict:erase(QuorumTransId, Pending),
    #request{% return data
             type = Type,
             name = Name,
             pattern = Pattern,
             source = SrcPid,
             % quorum data
             count_required = CountRequired,
             count_total = CountTotal,
             count_responses = CountResponses,
             count_correct = CountCorrect,
             responses = Responses,
             returned = Returned} = Request = dict:fetch(TransId, Requests),
    NewCountResponses = CountResponses + 1,
    case validate(ResponseInfoF, ResponseF,
                  ResponseInfo, Response) of
        true ->
            NewRequests = if
                Returned =:= false ->
                    Key = if
                        UseResponseInfo =:= true ->
                            {ResponseInfo, Response};
                        UseResponseInfo =:= false ->
                            Response
                    end,
                    Count = case orddict:find(Key, Responses) of
                        {ok, I} ->
                            I + 1;
                        error ->
                            1
                    end,
                    NewCountCorrect = erlang:max(Count, CountCorrect),
                    NewReturned = if
                        Count == CountRequired ->
                            cloudi_service:return_nothrow(Dispatcher, Type,
                                                          Name, Pattern,
                                                          ResponseInfo,
                                                          Response,
                                                          Timeout, TransId,
                                                          SrcPid),
                            true;
                        (NewCountResponses == CountTotal) orelse
                        ((NewCountCorrect +
                          (CountTotal - NewCountResponses)) < CountRequired) ->
                            cloudi_service:return_nothrow(Dispatcher, Type,
                                                          Name, Pattern,
                                                          <<>>, <<>>,
                                                          Timeout, TransId,
                                                          SrcPid),
                            true;
                        true ->
                            Returned
                    end,
                    if
                        NewCountResponses == CountTotal ->
                            dict:erase(TransId, Requests);
                        NewCountResponses < CountTotal ->
                            NewResponses = orddict:store(Key, Count, Responses),
                            dict:store(TransId,
                                       Request#request{
                                           count_responses = NewCountResponses,
                                           count_correct = NewCountCorrect,
                                           responses = NewResponses,
                                           returned = NewReturned},
                                       Requests)
                    end;
                NewCountResponses == CountTotal ->
                    dict:erase(TransId, Requests);
                NewCountResponses < CountTotal ->
                    dict:store(TransId,
                               Request#request{
                                   count_responses = NewCountResponses},
                               Requests)
            end,
            {noreply, State#state{requests = NewRequests,
                                  pending = NewPending}};
        false ->
            {DeadSrc, NewFailuresSrc} = failure(FailuresSrcDie,
                                                FailuresSrcMaxCount,
                                                FailuresSrcMaxPeriod,
                                                SrcPid, FailuresSrc),
            {_, NewFailuresDst} = failure(FailuresDstDie,
                                          FailuresDstMaxCount,
                                          FailuresDstMaxPeriod,
                                          DstPid, FailuresDst),
            NewReturned = if
                DeadSrc =:= true ->
                    true;
                (Returned =:= false) andalso
                ((NewCountResponses == CountTotal) orelse
                 ((CountCorrect +
                   (CountTotal - NewCountResponses)) < CountRequired)) ->
                    cloudi_service:return_nothrow(Dispatcher, Type,
                                                  Name, Pattern,
                                                  <<>>, <<>>,
                                                  Timeout, TransId, SrcPid),
                    true;
                true ->
                    Returned
            end,
            NewRequests = if
                NewCountResponses == CountTotal ->
                    dict:erase(TransId, Requests);
                NewCountResponses < CountTotal ->
                    dict:store(TransId,
                               Request#request{
                                   count_responses = NewCountResponses,
                                   returned = NewReturned},
                               Requests)
            end,
            {noreply, State#state{failures_source = NewFailuresSrc,
                                  failures_dest = NewFailuresDst,
                                  requests = NewRequests,
                                  pending = NewPending}}
                
    end;

cloudi_service_handle_info(#timeout_async_active{trans_id = QuorumTransId},
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
                                  requests = Requests,
                                  pending = Pending} = State,
                           Dispatcher) ->
    #pending{trans_id = TransId,
             dest = DstPid} = dict:fetch(QuorumTransId, Pending),
    NewPending = dict:erase(QuorumTransId, Pending),
    #request{% return data
             type = Type,
             name = Name,
             pattern = Pattern,
             timeout = Timeout,
             source = SrcPid,
             % quorum data
             count_required = CountRequired,
             count_total = CountTotal,
             count_responses = CountResponses,
             count_correct = CountCorrect,
             returned = Returned} = Request = dict:fetch(TransId, Requests),
    NewCountResponses = CountResponses + 1,
    {DeadSrc, NewFailuresSrc} = failure(FailuresSrcDie,
                                        FailuresSrcMaxCount,
                                        FailuresSrcMaxPeriod,
                                        SrcPid, FailuresSrc),
    {_, NewFailuresDst} = failure(FailuresDstDie,
                                  FailuresDstMaxCount,
                                  FailuresDstMaxPeriod,
                                  DstPid, FailuresDst),
    NewReturned = if
        DeadSrc =:= true ->
            true;
        (Returned =:= false) andalso
        ((NewCountResponses == CountTotal) orelse
         ((CountCorrect +
           (CountTotal - NewCountResponses)) < CountRequired)) ->
            cloudi_service:return_nothrow(Dispatcher, Type,
                                          Name, Pattern,
                                          <<>>, <<>>,
                                          Timeout, TransId, SrcPid),
            true;
        true ->
            Returned
    end,
    NewRequests = if
        NewCountResponses == CountTotal ->
            dict:erase(TransId, Requests);
        NewCountResponses < CountTotal ->
            dict:store(TransId,
                       Request#request{count_responses = NewCountResponses,
                                       returned = NewReturned},
                       Requests)
    end,
    {noreply, State#state{failures_source = NewFailuresSrc,
                          failures_dest = NewFailuresDst,
                          requests = NewRequests,
                          pending = NewPending}};

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

mcast_send(PatternPids, Pending, QuorumName,
           RequestInfo, Request, Timeout, Priority, TransId, Dispatcher) ->
    mcast_send(PatternPids, 0, Pending, QuorumName,
               RequestInfo, Request, Timeout, Priority, TransId, Dispatcher).

mcast_send([], CountSent, Pending, _, _, _, _, _, _, _) ->
    {CountSent, Pending};
mcast_send([{_, DstPid} = PatternPid | PatternPids],
           CountSent, Pending, QuorumName,
           RequestInfo, Request, Timeout, Priority, TransId, Dispatcher) ->
    case cloudi_service:send_async_active(Dispatcher, QuorumName,
                                          RequestInfo, Request,
                                          Timeout, Priority, PatternPid) of
        {ok, QuorumTransId} ->
            mcast_send(PatternPids, CountSent + 1,
                       dict:store(QuorumTransId,
                                  #pending{trans_id = TransId,
                                           dest = DstPid},
                                  Pending),
                       QuorumName,
                       RequestInfo, Request,
                       Timeout, Priority, TransId, Dispatcher);
        {error, timeout} ->
            mcast_send(PatternPids, CountSent,
                       Pending, QuorumName,
                       RequestInfo, Request,
                       Timeout, Priority, TransId, Dispatcher)
    end.

mcast(PatternPids, QuorumName,
      Type, Name, Pattern, RequestInfo, Request,
      Timeout, Priority, TransId, SrcPid, CountRequired,
      #state{requests = Requests,
             pending = Pending} = State, Dispatcher) ->
    {CountSent,
     NewPending} = mcast_send(PatternPids, Pending, QuorumName,
                              RequestInfo, Request,
                              Timeout, Priority, TransId, Dispatcher),
    if
        CountSent >= CountRequired ->
            NewRequests = dict:store(TransId,
                                     #request{type = Type,
                                              name = Name,
                                              pattern = Pattern,
                                              timeout = Timeout,
                                              source = SrcPid,
                                              count_required = CountRequired,
                                              count_total = CountSent,
                                              returned = false},
                                     Requests),
            {noreply, State#state{requests = NewRequests,
                                  pending = NewPending}};
        true ->
            NewRequests = dict:store(TransId,
                                     #request{type = Type,
                                              name = Name,
                                              pattern = Pattern,
                                              timeout = Timeout,
                                              source = SrcPid,
                                              count_required = CountRequired,
                                              count_total = CountSent,
                                              returned = true},
                                     Requests),
            {reply, <<>>, State#state{requests = NewRequests,
                                      pending = NewPending}}
    end.

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
    erlang:exit(Pid, cloudi_service_quorum).
