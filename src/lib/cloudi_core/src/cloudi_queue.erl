%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
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
%%% MIT License
%%%
%%% Copyright (c) 2015-2017 Michael Truog <mjtruog at gmail dot com>
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
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2015-2017 Michael Truog
%%% @version 1.7.3 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_queue).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([new/0,
         new/1,
         failures/1,
         recv/3,
         recv_id/3,
         send/4,
         send/5,
         send/6,
         send/7,
         send/8,
         send_id/4,
         send_id/5,
         send_id/6,
         send_id/7,
         send_id/8,
         size/1,
         timeout/3]).

-include("cloudi_core_i_constants.hrl").

-record(request,
    {
        name :: cloudi_service:service_name(),
        request_info :: cloudi_service:request_info(),
        request :: cloudi_service:request(),
        timeout :: cloudi_service:timeout_milliseconds(),
        priority :: cloudi_service:priority(),
        id :: cloudi_service:trans_id(),
        pattern_pid :: cloudi_service:pattern_pid(),
        retry_count = 0 :: non_neg_integer()
    }).

-record(cloudi_queue,
    {
        retry_max :: non_neg_integer(),
        validate_request_info :: undefined | fun((any()) -> boolean()),
        validate_request :: undefined | fun((any(), any()) -> boolean()),
        validate_response_info :: undefined | fun((any()) -> boolean()),
        validate_response :: undefined | fun((any(), any()) -> boolean()),
        failures_source_die :: boolean(),
        failures_source_max_count :: pos_integer(),
        failures_source_max_period :: infinity | pos_integer(),
        failures_source = [] :: list(erlang:timestamp()),
        requests = #{} :: #{cloudi_service:trans_id() := #request{}}
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

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

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
%% ===Return the current number of failures to send and validate.===
%% @end
%%-------------------------------------------------------------------------

-spec failures(State :: state()) ->
    non_neg_integer().

failures(#cloudi_queue{failures_source = FailuresSrc}) ->
    erlang:length(FailuresSrc).

%%-------------------------------------------------------------------------
%% @doc
%% ===Receive a service request.===
%% Must be called from the `cloudi_service_handle_info/3' callback function.
%% @end
%%-------------------------------------------------------------------------

-spec recv(Dispatcher :: cloudi_service:dispatcher(),
           Return :: #return_async_active{},
           State :: state()) ->
    {ok, NewState :: state()} |
    {{error, Reason :: cloudi_service:error_reason()}, NewState :: state()}.

recv(Dispatcher, Return, State) ->
    case recv_id(Dispatcher, Return, State) of
        {ok, _, NewState} ->
            {ok, NewState};
        {{error, _}, _} = Error ->
            Error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Receive a service request and provide the first transaction id.===
%% Must be called from the `cloudi_service_handle_info/3' callback function.
%% @end
%%-------------------------------------------------------------------------

-spec recv_id(Dispatcher :: cloudi_service:dispatcher(),
              #return_async_active{},
              State :: state()) ->
    {ok, Id :: cloudi_service:trans_id(), NewState :: state()} |
    {{error, Reason :: cloudi_service:error_reason()}, NewState :: state()}.

recv_id(Dispatcher,
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
    {#request{id = Id}, NewRequests} = maps:take(TransId, Requests),
    case validate(ResponseInfoF, ResponseF,
                  ResponseInfo, Response) of
        true ->
            {ok, Id, State#cloudi_queue{requests = NewRequests}};
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
           PatternPid :: cloudi_service:pattern_pid() | undefined,
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

send(Dispatcher, Name, RequestInfo, Request,
     Timeout, Priority, PatternPid, State) ->
    case send_id(Dispatcher, Name, RequestInfo, Request,
                 Timeout, Priority, PatternPid, State) of
        {ok, _, NewState} ->
            {ok, NewState};
        {{error, _}, _} = Error ->
            Error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a service request and provide the first transaction id.===
%% @end
%%-------------------------------------------------------------------------

-spec send_id(Dispatcher :: cloudi_service:dispatcher(),
              Name :: cloudi_service:service_name(),
              Request :: cloudi_service:request(),
              State :: state()) ->
    {ok, Id :: cloudi_service:trans_id(), NewState :: state()} |
    {{error, Reason :: cloudi_service:error_reason()}, NewState :: state()}.

send_id(Dispatcher, Name, Request, State) ->
    send_id(Dispatcher, Name, <<>>, Request,
            undefined, undefined, undefined, State).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a service request and provide the first transaction id.===
%% @end
%%-------------------------------------------------------------------------

-spec send_id(Dispatcher :: cloudi_service:dispatcher(),
              Name :: cloudi_service:service_name(),
              Request :: cloudi_service:request(),
              Timeout :: cloudi_service:timeout_milliseconds(),
              State :: state()) ->
    {ok, Id :: cloudi_service:trans_id(), NewState :: state()} |
    {{error, Reason :: cloudi_service:error_reason()}, NewState :: state()}.

send_id(Dispatcher, Name, Request, Timeout, State) ->
    send_id(Dispatcher, Name, <<>>, Request,
            Timeout, undefined, undefined, State).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a service request and provide the first transaction id.===
%% @end
%%-------------------------------------------------------------------------

-spec send_id(Dispatcher :: cloudi_service:dispatcher(),
              Name :: cloudi_service:service_name(),
              Request :: cloudi_service:request(),
              Timeout :: cloudi_service:timeout_milliseconds(),
              PatternPid :: cloudi_service:pattern_pid() | undefined,
              State :: state()) ->
    {ok, Id :: cloudi_service:trans_id(), NewState :: state()} |
    {{error, Reason :: cloudi_service:error_reason()}, NewState :: state()}.

send_id(Dispatcher, Name, Request, Timeout, PatternPid, State) ->
    send_id(Dispatcher, Name, <<>>, Request,
            Timeout, undefined, PatternPid, State).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a service request and provide the first transaction id.===
%% @end
%%-------------------------------------------------------------------------

-spec send_id(Dispatcher :: cloudi_service:dispatcher(),
              Name :: cloudi_service:service_name(),
              RequestInfo :: cloudi_service:request_info(),
              Request :: cloudi_service:request(),
              Timeout :: cloudi_service:timeout_milliseconds(),
              Priority :: cloudi_service:priority(),
              State :: state()) ->
    {ok, Id :: cloudi_service:trans_id(), NewState :: state()} |
    {{error, Reason :: cloudi_service:error_reason()}, NewState :: state()}.

send_id(Dispatcher, Name, RequestInfo, Request, Timeout, Priority, State) ->
    send_id(Dispatcher, Name, RequestInfo, Request,
            Timeout, Priority, undefined, State).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a service request and provide the first transaction id.===
%% @end
%%-------------------------------------------------------------------------

-spec send_id(Dispatcher :: cloudi_service:dispatcher(),
              Name :: cloudi_service:service_name(),
              RequestInfo :: cloudi_service:request_info(),
              Request :: cloudi_service:request(),
              Timeout :: cloudi_service:timeout_milliseconds(),
              Priority :: cloudi_service:priority(),
              PatternPid :: cloudi_service:pattern_pid() | undefined,
              State :: state()) ->
    {ok, Id :: cloudi_service:trans_id(), NewState :: state()} |
    {{error, Reason :: cloudi_service:error_reason()}, NewState :: state()}.

send_id(Dispatcher, Name, RequestInfo, Request, Timeout, Priority, PatternPid,
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
            case send_async(Dispatcher, Name, RequestInfo, Request,
                            Timeout, Priority, PatternPid) of
                {ok, TransId, RequestState} ->
                    {ok, TransId,
                     State#cloudi_queue{requests = maps:put(TransId,
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
%% ===Return the size of the queue.===
%% @end
%%-------------------------------------------------------------------------

-spec size(State :: state()) ->
    non_neg_integer().

size(#cloudi_queue{requests = Requests}) ->
    maps:size(Requests).

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
    case maps:get(TransId, Requests) of
        #request{retry_count = RetryMax} ->
            NewFailuresSrc = failure(FailuresSrcDie,
                                     FailuresSrcMaxCount,
                                     FailuresSrcMaxPeriod,
                                     FailuresSrc),
            {{error, timeout},
             State#cloudi_queue{failures_source = NewFailuresSrc,
                                requests = maps:remove(TransId, Requests)}};
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
                    NewRequests = maps:put(TransId,
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
                                        requests = maps:remove(TransId,
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

send_async(Dispatcher, Name, RequestInfo, Request,
           Timeout, Priority, undefined) ->
    case cloudi_service:get_pid(Dispatcher, Name, Timeout) of
        {ok, PatternPid} ->
            send_async(Dispatcher, Name, RequestInfo, Request,
                       Timeout, Priority, PatternPid);
        {error, _} = Error ->
            Error
    end;
send_async(Dispatcher, Name, RequestInfo, Request,
           Timeout, Priority, PatternPid) ->
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
                                    id = TransId,
                                    pattern_pid = PatternPid},
            {ok, TransId, RequestState};
        {error, _} = Error ->
            Error
    end.

failure(false, _, _, FailureList) ->
    FailureList;
failure(true, MaxCount, MaxPeriod, FailureList) ->
    failure_check(cloudi_timestamp:seconds(), FailureList, MaxCount, MaxPeriod).

failure_store(FailureList, FailureCount, MaxCount) ->
    if
        FailureCount == MaxCount ->
            failure_kill();
        true ->
            ok
    end,
    FailureList.

failure_check(SecondsNow, FailureList, MaxCount, infinity) ->
    NewFailureCount = erlang:length(FailureList),
    failure_store([SecondsNow | FailureList], NewFailureCount + 1,
                  MaxCount);
failure_check(SecondsNow, FailureList, MaxCount, MaxPeriod) ->
    {NewFailureCount,
     NewFailureList} = cloudi_timestamp:seconds_filter(FailureList,
                                                       SecondsNow, MaxPeriod),
    failure_store([SecondsNow | NewFailureList], NewFailureCount + 1,
                  MaxCount).

failure_kill() ->
    erlang:exit(cloudi_queue).

