%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Send Queue==
%%% For internal services that need to have successful service request sends
%%% (i.e., a timeout does not occur because a response is received within the
%%%  timeout period) with each send done asynchronously and all tracking done
%%% with in-memory data.
%%%
%%% The data is not queued within this module's internal data structure,
%%% but like all service requests, the queueing occurs in the destination
%%% service process.  This module's internal data provides tracking based
%%% on the service request transaction id so many asynchronous service
%%% requests may be easily managed concurrently.  The alternative is to rely
%%% on synchronous service requests and increase the sending service's
%%% process count as necessary for concurrency, which is a simpler approach.
%%% So, cloudi_queue usage is necessary when multiple service requests must
%%% be sent concurrently and every request must receive a response
%%% (receiving a response is the only proof the transaction was successful).
%%%
%%% Only one instance of cloudi_queue is necessary within a service's state
%%% due to the transaction id being globally unique.
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

-module(cloudi_queue).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([new/0,
         new/1,
         recv/3,
         send/4,
         send/5,
         send/6,
         send/7,
         send/8,
         timeout/3]).

-ifdef(ERLANG_OTP_VERSION_16).
-type dict_proxy(_Key, _Value) :: dict().
-else.
-type dict_proxy(Key, Value) :: dict:dict(Key, Value).
-endif.

-record(request,
    {
        name :: cloudi_service:service_name(),
        request_info :: cloudi_service:request_info(),
        request :: cloudi_service:request(),
        timeout :: cloudi_service:timeout_milliseconds(),
        priority :: cloudi_service:priority(),
        pattern_pid :: cloudi_service:pattern_pid(),
        retry_count = 0 :: non_neg_integer()
    }).

-record(cloudi_queue,
    {
        retry_max :: non_neg_integer(),
        validate_request_info :: fun((any()) -> boolean()),
        validate_request :: fun((any(), any()) -> boolean()),
        validate_response_info :: fun((any()) -> boolean()),
        validate_response :: fun((any(), any()) -> boolean()),
        failures_source_die :: boolean(),
        failures_source_max_count :: pos_integer(),
        failures_source_max_period :: infinity | pos_integer(),
        failures_source = [] :: list(erlang:timestamp()),
        requests = dict:new() :: dict_proxy(cloudi_service:trans_id(),
                                            #request{})
    }).

-include("cloudi_service.hrl").

-define(DEFAULT_RETRY,                                0). % see below:
        % a retry doesn't count as a failure, until it fails completely
        % (i.e., hit the max retry count or send returns an error)
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
-define(DEFAULT_FAILURES_SOURCE_MAX_COUNT,            1). % see below:
        % (similar to the MaxR configuration value for services)
-define(DEFAULT_FAILURES_SOURCE_MAX_PERIOD,    infinity). % seconds, see below:
        % (similar to the MaxT configuration value for services)
        % If you want the source service to eventually fail,
        % use the service's MaxT/MaxR as the failures_source_max_period value
        % (e.g., 300/5 == 60 seconds).  Can also use the value 'infinity'
        % to accumulate a failure count indefinitely.

-type options() ::
    list({retry, non_neg_integer()} |
         {validate_request_info,
          fun((RequestInfo :: cloudi_service:request_info()) -> boolean()) |
          {Module1 :: module(), Function1 :: atom()}} |
         {validate_request,
          fun((RequestInfo :: cloudi_service:request_info(),
               Request :: cloudi_service:request()) -> boolean()) |
          {Module2 :: module(), Function2 :: atom()}} |
         {validate_response_info,
          fun((ResponseInfo :: cloudi_service:response_info()) -> boolean()) |
          {Module3 :: module(), Function3 :: atom()}} |
         {validate_response,
          fun((ResponseInfo :: cloudi_service:response_info(),
               Response :: cloudi_service:response()) -> boolean()) |
          {Module4 :: module(), Function4 :: atom()}} |
         {failures_source_die, boolean()} |
         {failures_source_max_count, pos_integer()} |
         {failures_source_max_period, infinity | pos_integer()}).
-type state() :: #cloudi_queue{}.
-export_type([options/0,
              state/0]).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a CloudI queue.===
%% @end
%%-------------------------------------------------------------------------

-spec new() ->
    state().

new() ->
    new([]).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a CloudI queue.===
%% @end
%%-------------------------------------------------------------------------

-spec new(Options :: options()) ->
    state().

new(Options)
    when is_list(Options) ->
    Defaults = [
        {retry,                         ?DEFAULT_RETRY},
        {validate_request_info,         ?DEFAULT_VALIDATE_REQUEST_INFO},
        {validate_request,              ?DEFAULT_VALIDATE_REQUEST},
        {validate_response_info,        ?DEFAULT_VALIDATE_RESPONSE_INFO},
        {validate_response,             ?DEFAULT_VALIDATE_RESPONSE},
        {failures_source_die,           ?DEFAULT_FAILURES_SOURCE_DIE},
        {failures_source_max_count,     ?DEFAULT_FAILURES_SOURCE_MAX_COUNT},
        {failures_source_max_period,    ?DEFAULT_FAILURES_SOURCE_MAX_PERIOD}],
    [RetryMax, ValidateRequestInfo0, ValidateRequest0,
     ValidateResponseInfo0, ValidateResponse0,
     FailuresSrcDie, FailuresSrcMaxCount, FailuresSrcMaxPeriod
     ] =
        cloudi_proplists:take_values(Defaults, Options),
    true = is_integer(RetryMax) andalso (RetryMax >= 0),
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
    #cloudi_queue{
        retry_max = RetryMax,
        validate_request_info = ValidateRequestInfo1,
        validate_request = ValidateRequest1,
        validate_response_info = ValidateResponseInfo1,
        validate_response = ValidateResponse1,
        failures_source_die = FailuresSrcDie,
        failures_source_max_count = FailuresSrcMaxCount,
        failures_source_max_period = FailuresSrcMaxPeriod
    }.

%%-------------------------------------------------------------------------
%% @doc
%% ===Receive a service request.===
%% Must be called from the `cloudi_service_handle_info/3' callback function.
%% @end
%%-------------------------------------------------------------------------

-spec recv(Dispatcher :: cloudi_service:dispatcher(),
           #return_async_active{},
           State :: state()) ->
    {ok, NewState :: state()} |
    {{error, Reason :: cloudi_service:error_reason()}, NewState :: state()}.

recv(Dispatcher,
     #return_async_active{response_info = ResponseInfo,
                          response = Response,
                          trans_id = TransId},
     #cloudi_queue{validate_response_info = ResponseInfoF,
                   validate_response = ResponseF,
                   failures_source_die = FailuresSrcDie,
                   failures_source_max_count = FailuresSrcMaxCount,
                   failures_source_max_period = FailuresSrcMaxPeriod,
                   failures_source = FailuresSrc,
                   requests = Requests} = State)
    when is_pid(Dispatcher) ->
    #request{} = dict:fetch(TransId, Requests),
    NewRequests = dict:erase(TransId, Requests),
    case validate(ResponseInfoF, ResponseF,
                  ResponseInfo, Response) of
        true ->
            {ok, State#cloudi_queue{requests = NewRequests}};
        false ->
            NewFailuresSrc = failure(FailuresSrcDie,
                                     FailuresSrcMaxCount,
                                     FailuresSrcMaxPeriod,
                                     FailuresSrc),
            {{error, timeout},
             State#cloudi_queue{failures_source = NewFailuresSrc,
                                requests = NewRequests}}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send(Dispatcher :: cloudi_service:dispatcher(),
           Name :: cloudi_service:service_name(),
           Request :: cloudi_service:request(),
           State :: state()) ->
    {ok, NewState :: state()} |
    {{error, Reason :: cloudi_service:error_reason()}, NewState :: state()}.

send(Dispatcher, Name, Request, State) ->
    send(Dispatcher, Name, <<>>, Request,
         undefined, undefined, undefined, State).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send(Dispatcher :: cloudi_service:dispatcher(),
           Name :: cloudi_service:service_name(),
           Request :: cloudi_service:request(),
           Timeout :: cloudi_service:timeout_milliseconds(),
           State :: state()) ->
    {ok, NewState :: state()} |
    {{error, Reason :: cloudi_service:error_reason()}, NewState :: state()}.

send(Dispatcher, Name, Request, Timeout, State) ->
    send(Dispatcher, Name, <<>>, Request,
         Timeout, undefined, undefined, State).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send(Dispatcher :: cloudi_service:dispatcher(),
           Name :: cloudi_service:service_name(),
           Request :: cloudi_service:request(),
           Timeout :: cloudi_service:timeout_milliseconds(),
           PatternPid :: cloudi_service:pattern_pid(),
           State :: state()) ->
    {ok, NewState :: state()} |
    {{error, Reason :: cloudi_service:error_reason()}, NewState :: state()}.

send(Dispatcher, Name, Request, Timeout, PatternPid, State) ->
    send(Dispatcher, Name, <<>>, Request,
         Timeout, undefined, PatternPid, State).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send(Dispatcher :: cloudi_service:dispatcher(),
           Name :: cloudi_service:service_name(),
           RequestInfo :: cloudi_service:request_info(),
           Request :: cloudi_service:request(),
           Timeout :: cloudi_service:timeout_milliseconds(),
           Priority :: cloudi_service:priority(),
           State :: state()) ->
    {ok, NewState :: state()} |
    {{error, Reason :: cloudi_service:error_reason()}, NewState :: state()}.

send(Dispatcher, Name, RequestInfo, Request, Timeout, Priority, State) ->
    send(Dispatcher, Name, RequestInfo, Request,
         Timeout, Priority, undefined, State).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send(Dispatcher :: cloudi_service:dispatcher(),
           Name :: cloudi_service:service_name(),
           RequestInfo :: cloudi_service:request_info(),
           Request :: cloudi_service:request(),
           Timeout :: cloudi_service:timeout_milliseconds(),
           Priority :: cloudi_service:priority(),
           PatternPid :: cloudi_service:pattern_pid() | undefined,
           State :: state()) ->
    {ok, NewState :: state()} |
    {{error, Reason :: cloudi_service:error_reason()}, NewState :: state()}.

send(Dispatcher, Name, RequestInfo, Request, Timeout, Priority, PatternPid,
     #cloudi_queue{validate_request_info = RequestInfoF,
                   validate_request = RequestF,
                   failures_source_die = FailuresSrcDie,
                   failures_source_max_count = FailuresSrcMaxCount,
                   failures_source_max_period = FailuresSrcMaxPeriod,
                   failures_source = FailuresSrc,
                   requests = Requests} = State)
    when is_pid(Dispatcher) ->
    case validate(RequestInfoF, RequestF,
                  RequestInfo, Request) of
        true ->
            case cloudi_service:send_async_active(Dispatcher, Name,
                                                  RequestInfo, Request,
                                                  Timeout, Priority,
                                                  PatternPid) of
                {ok, TransId} ->
                    RequestState = #request{name = Name,
                                            request_info = RequestInfo,
                                            request = Request,
                                            timeout = Timeout,
                                            priority = Priority,
                                            pattern_pid = PatternPid},
                    {ok, State#cloudi_queue{requests = dict:store(TransId,
                                                                  RequestState,
                                                                  Requests)}};
                {error, _} = Error ->
                    NewFailuresSrc = failure(FailuresSrcDie,
                                             FailuresSrcMaxCount,
                                             FailuresSrcMaxPeriod,
                                             FailuresSrc),
                    {Error,
                     State#cloudi_queue{failures_source = NewFailuresSrc}}
            end;
        false ->
            NewFailuresSrc = failure(FailuresSrcDie,
                                     FailuresSrcMaxCount,
                                     FailuresSrcMaxPeriod,
                                     FailuresSrc),
            {{error, timeout},
             State#cloudi_queue{failures_source = NewFailuresSrc}}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===A service request timeout.===
%% `ok' is returned if a service request is retried. Must be called from the
%% `cloudi_service_handle_info/3' callback function.
%% @end
%%-------------------------------------------------------------------------

-spec timeout(Dispatcher :: cloudi_service:dispatcher(),
              #timeout_async_active{},
              State :: state()) ->
    {ok, NewState :: state()} |
    {{error, Reason :: cloudi_service:error_reason()}, NewState :: state()}.

timeout(Dispatcher,
        #timeout_async_active{trans_id = TransId},
        #cloudi_queue{retry_max = RetryMax,
                      failures_source_die = FailuresSrcDie,
                      failures_source_max_count = FailuresSrcMaxCount,
                      failures_source_max_period = FailuresSrcMaxPeriod,
                      failures_source = FailuresSrc,
                      requests = Requests} = State)
    when is_pid(Dispatcher) ->
    case dict:fetch(TransId, Requests) of
        #request{retry_count = RetryMax} ->
            NewFailuresSrc = failure(FailuresSrcDie,
                                     FailuresSrcMaxCount,
                                     FailuresSrcMaxPeriod,
                                     FailuresSrc),
            {{error, timeout},
             State#cloudi_queue{failures_source = NewFailuresSrc,
                                requests = dict:erase(TransId, Requests)}};
        #request{name = Name,
                 request_info = RequestInfo,
                 request = Request,
                 timeout = Timeout,
                 priority = Priority,
                 pattern_pid = PatternPid,
                 retry_count = I} = RequestState
            when I < RetryMax ->
            case cloudi_service:send_async_active(Dispatcher, Name,
                                                  RequestInfo, Request,
                                                  Timeout, Priority,
                                                  TransId, PatternPid) of
                {ok, TransId} ->
                    NewRequests = dict:store(TransId,
                                             RequestState#request{
                                                 retry_count = I + 1},
                                             Requests),
                    {ok, State#cloudi_queue{requests = NewRequests}};
                {error, _} = Error ->
                    NewFailuresSrc = failure(FailuresSrcDie,
                                             FailuresSrcMaxCount,
                                             FailuresSrcMaxPeriod,
                                             FailuresSrc),
                    {Error,
                     State#cloudi_queue{failures_source = NewFailuresSrc,
                                        requests = dict:erase(TransId,
                                                              Requests)}}
            end
    end.

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

failure(false, _, _, FailureList) ->
    FailureList;
failure(true, MaxCount, MaxPeriod, FailureList) ->
    failure_check(cloudi_timestamp:seconds(), FailureList, MaxCount, MaxPeriod).

failure_store(FailureList, MaxCount) ->
    if
        erlang:length(FailureList) == MaxCount ->
            failure_kill();
        true ->
            ok
    end,
    FailureList.

failure_check(SecondsNow, FailureList, MaxCount, infinity) ->
    failure_store([SecondsNow | FailureList], MaxCount);
failure_check(SecondsNow, FailureList, MaxCount, MaxPeriod) ->
    NewFailureList = cloudi_timestamp:seconds_filter(FailureList,
                                                     SecondsNow, MaxPeriod),
    failure_store([SecondsNow | NewFailureList], MaxCount).

failure_kill() ->
    erlang:exit(cloudi_queue).

