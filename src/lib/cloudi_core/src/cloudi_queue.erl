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
%%% Copyright (c) 2015-2018 Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2015-2018 Michael Truog
%%% @version 1.7.3 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_queue).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([byte_size/2,
         byte_size/4,
         byte_size/5,
         byte_size/6,
         byte_size/7,
         byte_size/8,
         failures/2,
         handle_info/3,
         mcast/4,
         mcast/5,
         mcast/7,
         new/0,
         new/1,
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
         size/2,
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
        retry_pattern_pid :: boolean(),
        retry_count = 0 :: non_neg_integer(),
        retry_delay = false :: boolean()
    }).

-record(request_ordered_send,
    {
        name :: cloudi_service:service_name(),
        request_info :: cloudi_service:request_info(),
        request :: cloudi_service:request(),
        timeout :: cloudi_service:timeout_milliseconds(),
        priority :: cloudi_service:priority(),
        id :: cloudi_service:trans_id(),
        pattern_pid :: cloudi_service:pattern_pid() | undefined
    }).

-record(request_ordered_mcast,
    {
        name :: cloudi_service:service_name(),
        request_info :: cloudi_service:request_info(),
        request :: cloudi_service:request(),
        timeout :: cloudi_service:timeout_milliseconds(),
        priority :: cloudi_service:priority()
    }).

-type ordered_requests() :: queue:queue(#request_ordered_send{} |
                                        #request_ordered_mcast{}).
-type requests() :: #{cloudi_service:trans_id() := #request{}}.

-record(cloudi_queue,
    {
        retry :: non_neg_integer(),
        retry_delay :: non_neg_integer(),
        ordered :: boolean(),
        word_size :: pos_integer(),
        service = undefined :: undefined | pid(),
        validate_request_info :: undefined | fun((any()) -> boolean()),
        validate_request :: undefined | fun((any(), any()) -> boolean()),
        validate_response_info :: undefined | fun((any()) -> boolean()),
        validate_response :: undefined | fun((any(), any()) -> boolean()),
        failures_source_die :: boolean(),
        failures_source_max_count :: pos_integer(),
        failures_source_max_period :: infinity | pos_integer(),
        failures_source = [] :: list(cloudi_timestamp:seconds_monotonic()),
        ordered_requests = queue:new() :: ordered_requests(),
        ordered_pending = 0 :: non_neg_integer(),
        requests = #{} :: requests()
    }).

-include("cloudi_service.hrl").

-define(DEFAULT_RETRY,                                0). % see below:
        % a retry doesn't count as a failure, until it fails completely
        % (i.e., hit the max retry count or send returns an error)
-define(DEFAULT_RETRY_DELAY,                          0). % milliseconds
-define(DEFAULT_ORDERED,                          false).
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
         {retry_delay, non_neg_integer()} |
         {ordered, boolean()} |
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
%% ===Return the size of the CloudI queue in bytes.===
%% @end
%%-------------------------------------------------------------------------

-spec byte_size(Dispatcher :: cloudi_service:dispatcher(),
                State :: state()) ->
    non_neg_integer().

byte_size(Dispatcher,
          #cloudi_queue{word_size = WordSize} = State)
    when is_pid(Dispatcher) ->
    cloudi_x_erlang_term:byte_size(State, WordSize).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the size of the CloudI queue in bytes with the additional service request added.===
%% @end
%%-------------------------------------------------------------------------

-spec byte_size(Dispatcher :: cloudi_service:dispatcher(),
                Name :: cloudi_service:service_name(),
                Request :: cloudi_service:request(),
                State :: state()) ->
    non_neg_integer().

byte_size(Dispatcher, Name, Request, State) ->
    byte_size(Dispatcher, Name, <<>>, Request,
              undefined, undefined, undefined, State).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the size of the CloudI queue in bytes with the additional service request added.===
%% @end
%%-------------------------------------------------------------------------

-spec byte_size(Dispatcher :: cloudi_service:dispatcher(),
                Name :: cloudi_service:service_name(),
                Request :: cloudi_service:request(),
                Timeout :: cloudi_service:timeout_milliseconds(),
                State :: state()) ->
    non_neg_integer().

byte_size(Dispatcher, Name, Request, Timeout, State) ->
    byte_size(Dispatcher, Name, <<>>, Request,
              Timeout, undefined, undefined, State).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the size of the CloudI queue in bytes with the additional service request added.===
%% @end
%%-------------------------------------------------------------------------

-spec byte_size(Dispatcher :: cloudi_service:dispatcher(),
                Name :: cloudi_service:service_name(),
                Request :: cloudi_service:request(),
                Timeout :: cloudi_service:timeout_milliseconds(),
                PatternPid :: cloudi_service:pattern_pid() | undefined,
                State :: state()) ->
    non_neg_integer().

byte_size(Dispatcher, Name, Request, Timeout, PatternPid, State) ->
    byte_size(Dispatcher, Name, <<>>, Request,
              Timeout, undefined, PatternPid, State).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the size of the CloudI queue in bytes with the additional service request added.===
%% @end
%%-------------------------------------------------------------------------

-spec byte_size(Dispatcher :: cloudi_service:dispatcher(),
                Name :: cloudi_service:service_name(),
                RequestInfo :: cloudi_service:request_info(),
                Request :: cloudi_service:request(),
                Timeout :: cloudi_service:timeout_milliseconds(),
                Priority :: cloudi_service:priority(),
                State :: state()) ->
    non_neg_integer().

byte_size(Dispatcher, Name, RequestInfo, Request, Timeout, Priority, State) ->
    byte_size(Dispatcher, Name, RequestInfo, Request,
              Timeout, Priority, undefined, State).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the size of the CloudI queue in bytes with the additional service request added.===
%% @end
%%-------------------------------------------------------------------------

-spec byte_size(Dispatcher :: cloudi_service:dispatcher(),
                Name :: cloudi_service:service_name(),
                RequestInfo :: cloudi_service:request_info(),
                Request :: cloudi_service:request(),
                Timeout :: cloudi_service:timeout_milliseconds(),
                Priority :: cloudi_service:priority(),
                PatternPid :: cloudi_service:pattern_pid() | undefined,
                State :: state()) ->
    non_neg_integer().
    
byte_size(Dispatcher, Name, RequestInfo, Request,
          Timeout, Priority, PatternPid,
          #cloudi_queue{word_size = WordSize,
                        requests = Requests} = State)
    when is_pid(Dispatcher) ->
    TransIdTest = <<0:128>>,
    PatternPidTest = if
        PatternPid =:= undefined ->
            {Name, self()};
        is_tuple(PatternPid) ->
            PatternPid
    end,
    RequestStateTest = #request{name = Name,
                                request_info = RequestInfo,
                                request = Request,
                                timeout = Timeout,
                                priority = Priority,
                                id = TransIdTest,
                                pattern_pid = PatternPidTest,
                                retry_pattern_pid = false},
    StateTest = State#cloudi_queue{requests = maps:put(TransIdTest,
                                                       RequestStateTest,
                                                       Requests)},
    cloudi_x_erlang_term:byte_size(StateTest, WordSize).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the current number of failures to send and validate.===
%% @end
%%-------------------------------------------------------------------------

-spec failures(Dispatcher :: cloudi_service:dispatcher(),
               State :: state()) ->
    non_neg_integer().

failures(Dispatcher,
         #cloudi_queue{failures_source = FailuresSrc})
    when is_pid(Dispatcher) ->
    erlang:length(FailuresSrc).

%%-------------------------------------------------------------------------
%% @doc
%% ===Handle all info messages related to the CloudI queue.===
%% Must be called from the `cloudi_service_handle_info/3' callback function.
%% @end
%%-------------------------------------------------------------------------

-spec handle_info(any(),
                  State :: state(),
                  Dispatcher :: cloudi_service:dispatcher()) ->
    {ok, StateNew :: state()} |
    {{error, Reason :: cloudi_service:error_reason()}, StateNew :: state()} |
    {ignored, State :: state()}.

handle_info(#return_async_active{} = ReturnAsync, State, Dispatcher) ->
    recv(Dispatcher, ReturnAsync, State);
handle_info(#timeout_async_active{} = TimeoutAsync, State, Dispatcher) ->
    timeout(Dispatcher, TimeoutAsync, State);
handle_info(_, State, _) ->
    {ignored, State}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a multicast service request.===
%% @end
%%-------------------------------------------------------------------------

-spec mcast(Dispatcher :: cloudi_service:dispatcher(),
            Name :: cloudi_service:service_name(),
            Request :: cloudi_service:request(),
            State :: state()) ->
    {ok, State :: state()} |
    {{error, Reason :: cloudi_service:error_reason()}, State :: state()}.

mcast(Dispatcher, Name, Request, State) ->
    mcast(Dispatcher, Name, <<>>, Request, undefined, undefined, State).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a multicast service request.===
%% @end
%%-------------------------------------------------------------------------

-spec mcast(Dispatcher :: cloudi_service:dispatcher(),
            Name :: cloudi_service:service_name(),
            Request :: cloudi_service:request(),
            Timeout :: cloudi_service:timeout_milliseconds(),
            State :: state()) ->
    {ok, State :: state()} |
    {{error, Reason :: cloudi_service:error_reason()}, State :: state()}.

mcast(Dispatcher, Name, Request, Timeout, State) ->
    mcast(Dispatcher, Name, <<>>, Request, Timeout, undefined, State).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a multicast service request.===
%% @end
%%-------------------------------------------------------------------------

-spec mcast(Dispatcher :: cloudi_service:dispatcher(),
            Name :: cloudi_service:service_name(),
            RequestInfo :: cloudi_service:request_info(),
            Request :: cloudi_service:request(),
            Timeout :: cloudi_service:timeout_milliseconds(),
            Priority :: cloudi_service:priority(),
            State :: state()) ->
    {ok, State :: state()} |
    {{error, Reason :: cloudi_service:error_reason()}, State :: state()}.

mcast(Dispatcher, Name, RequestInfo, Request, Timeout, Priority,
      #cloudi_queue{ordered = true,
                    validate_request_info = RequestInfoF,
                    validate_request = RequestF,
                    failures_source_die = FailuresSrcDie,
                    failures_source_max_count = FailuresSrcMaxCount,
                    failures_source_max_period = FailuresSrcMaxPeriod,
                    failures_source = FailuresSrc,
                    ordered_requests = OrderedRequests,
                    ordered_pending = OrderedPending} = State)
    when is_pid(Dispatcher), OrderedPending > 0 ->
    case validate(RequestInfoF, RequestF,
                  RequestInfo, Request) of
        true ->
            OrderedRequest = #request_ordered_mcast{name = Name,
                                                    request_info = RequestInfo,
                                                    request = Request,
                                                    timeout = Timeout,
                                                    priority = Priority},
            OrderedRequestsNew = queue:in(OrderedRequest, OrderedRequests),
            {ok, State#cloudi_queue{ordered_requests = OrderedRequestsNew}};
        false ->
            FailuresSrcNew = failure(FailuresSrcDie,
                                     FailuresSrcMaxCount,
                                     FailuresSrcMaxPeriod,
                                     FailuresSrc),
            {{error, timeout},
             State#cloudi_queue{failures_source = FailuresSrcNew}}
    end;
mcast(Dispatcher, Name, RequestInfo, Request, Timeout, Priority,
      #cloudi_queue{ordered = Ordered,
                    validate_request_info = RequestInfoF,
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
            case queue_mcast(Dispatcher, Name, RequestInfo, Request,
                             Timeout, Priority, Requests, Ordered) of
                {ok, OrderedPending, RequestsNew} ->
                    {ok, State#cloudi_queue{ordered_pending = OrderedPending,
                                            requests = RequestsNew}};
                {{error, _} = Error, OrderedPending, RequestsNew} ->
                    FailuresSrcNew = failure(FailuresSrcDie,
                                             FailuresSrcMaxCount,
                                             FailuresSrcMaxPeriod,
                                             FailuresSrc),
                    {Error,
                     State#cloudi_queue{failures_source = FailuresSrcNew,
                                        ordered_pending = OrderedPending,
                                        requests = RequestsNew}}
            end;
        false ->
            FailuresSrcNew = failure(FailuresSrcDie,
                                     FailuresSrcMaxCount,
                                     FailuresSrcMaxPeriod,
                                     FailuresSrc),
            {{error, timeout},
             State#cloudi_queue{failures_source = FailuresSrcNew}}
    end.

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
        {retry_delay,                   ?DEFAULT_RETRY_DELAY},
        {ordered,                       ?DEFAULT_ORDERED},
        {validate_request_info,         ?DEFAULT_VALIDATE_REQUEST_INFO},
        {validate_request,              ?DEFAULT_VALIDATE_REQUEST},
        {validate_response_info,        ?DEFAULT_VALIDATE_RESPONSE_INFO},
        {validate_response,             ?DEFAULT_VALIDATE_RESPONSE},
        {failures_source_die,           ?DEFAULT_FAILURES_SOURCE_DIE},
        {failures_source_max_count,     ?DEFAULT_FAILURES_SOURCE_MAX_COUNT},
        {failures_source_max_period,    ?DEFAULT_FAILURES_SOURCE_MAX_PERIOD}],
    [Retry, RetryDelay, Ordered, ValidateRequestInfo0, ValidateRequest0,
     ValidateResponseInfo0, ValidateResponse0,
     FailuresSrcDie, FailuresSrcMaxCount, FailuresSrcMaxPeriod
     ] =
        cloudi_proplists:take_values(Defaults, Options),
    true = is_integer(Retry) andalso (Retry >= 0),
    true = is_integer(RetryDelay) andalso
           (RetryDelay >= 0) andalso (RetryDelay =< 4294967295),
    true = is_boolean(Ordered),
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
    WordSize = erlang:system_info(wordsize),
    #cloudi_queue{
        retry = Retry,
        retry_delay = RetryDelay,
        ordered = Ordered,
        word_size = WordSize,
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
           ReturnAsync :: #return_async_active{},
           State :: state()) ->
    {ok, StateNew :: state()} |
    {{error, Reason :: cloudi_service:error_reason()}, StateNew :: state()} |
    {ignored, State :: state()}.

recv(Dispatcher, ReturnAsync, State) ->
    case recv_id(Dispatcher, ReturnAsync, State) of
        {ok, _, StateNew} ->
            {ok, StateNew};
        {{error, _}, _} = Error ->
            Error;
        {ignored, _} = Ignored ->
            Ignored
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
    {ok, Id :: cloudi_service:trans_id(), StateNew :: state()} |
    {{error, Reason :: cloudi_service:error_reason()}, StateNew :: state()} |
    {ignored, State :: state()}.

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
    case maps:take(TransId, Requests) of
        error ->
            {ignored, State};
        {#request{id = Id}, RequestsNew} ->
            case validate(ResponseInfoF, ResponseF,
                          ResponseInfo, Response) of
                true ->
                    {ok, Id,
                     ordered_check(Dispatcher,
                                   State#cloudi_queue{
                                       requests = RequestsNew})};
                false ->
                    FailuresSrcNew = failure(FailuresSrcDie,
                                             FailuresSrcMaxCount,
                                             FailuresSrcMaxPeriod,
                                             FailuresSrc),
                    {{error, timeout},
                     ordered_check(Dispatcher,
                                   State#cloudi_queue{
                                       failures_source = FailuresSrcNew,
                                       requests = RequestsNew})}
            end
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
    {ok, StateNew :: state()} |
    {{error, Reason :: cloudi_service:error_reason()}, StateNew :: state()}.

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
    {ok, StateNew :: state()} |
    {{error, Reason :: cloudi_service:error_reason()}, StateNew :: state()}.

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
    {ok, StateNew :: state()} |
    {{error, Reason :: cloudi_service:error_reason()}, StateNew :: state()}.

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
    {ok, StateNew :: state()} |
    {{error, Reason :: cloudi_service:error_reason()}, StateNew :: state()}.

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
    {ok, StateNew :: state()} |
    {{error, Reason :: cloudi_service:error_reason()}, StateNew :: state()}.

send(Dispatcher, Name, RequestInfo, Request,
     Timeout, Priority, PatternPid, State) ->
    case send_id(Dispatcher, Name, RequestInfo, Request,
                 Timeout, Priority, PatternPid, State) of
        {ok, _, StateNew} ->
            {ok, StateNew};
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
    {ok, Id :: cloudi_service:trans_id(), StateNew :: state()} |
    {{error, Reason :: cloudi_service:error_reason()}, StateNew :: state()}.

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
    {ok, Id :: cloudi_service:trans_id(), StateNew :: state()} |
    {{error, Reason :: cloudi_service:error_reason()}, StateNew :: state()}.

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
    {ok, Id :: cloudi_service:trans_id(), StateNew :: state()} |
    {{error, Reason :: cloudi_service:error_reason()}, StateNew :: state()}.

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
    {ok, Id :: cloudi_service:trans_id(), StateNew :: state()} |
    {{error, Reason :: cloudi_service:error_reason()}, StateNew :: state()}.

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
    {ok, Id :: cloudi_service:trans_id(), StateNew :: state()} |
    {{error, Reason :: cloudi_service:error_reason()}, StateNew :: state()}.

send_id(Dispatcher, Name, RequestInfo, Request, Timeout, Priority, PatternPid,
        #cloudi_queue{ordered = true,
                      validate_request_info = RequestInfoF,
                      validate_request = RequestF,
                      failures_source_die = FailuresSrcDie,
                      failures_source_max_count = FailuresSrcMaxCount,
                      failures_source_max_period = FailuresSrcMaxPeriod,
                      failures_source = FailuresSrc,
                      ordered_requests = OrderedRequests,
                      ordered_pending = OrderedPending} = State)
    when is_pid(Dispatcher), OrderedPending > 0 ->
    case validate(RequestInfoF, RequestF,
                  RequestInfo, Request) of
        true ->
            TransId = cloudi_service:trans_id(Dispatcher),
            OrderedRequest = #request_ordered_send{name = Name,
                                                   request_info = RequestInfo,
                                                   request = Request,
                                                   timeout = Timeout,
                                                   priority = Priority,
                                                   id = TransId,
                                                   pattern_pid = PatternPid},
            OrderedRequestsNew = queue:in(OrderedRequest, OrderedRequests),
            {ok, TransId,
             State#cloudi_queue{ordered_requests = OrderedRequestsNew}};
        false ->
            FailuresSrcNew = failure(FailuresSrcDie,
                                     FailuresSrcMaxCount,
                                     FailuresSrcMaxPeriod,
                                     FailuresSrc),
            {{error, timeout},
             State#cloudi_queue{failures_source = FailuresSrcNew}}
    end;
send_id(Dispatcher, Name, RequestInfo, Request, Timeout, Priority, PatternPid,
        #cloudi_queue{ordered = Ordered,
                      validate_request_info = RequestInfoF,
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
            case queue_send_first(Dispatcher, Name, RequestInfo, Request,
                                  Timeout, Priority, PatternPid) of
                {ok, TransId, RequestState} ->
                    OrderedPending = if
                        Ordered =:= true ->
                            1;
                        Ordered =:= false ->
                            0
                    end,
                    {ok, TransId,
                     State#cloudi_queue{ordered_pending = OrderedPending,
                                        requests = maps:put(TransId,
                                                            RequestState,
                                                            Requests)}};
                {error, _} = Error ->
                    FailuresSrcNew = failure(FailuresSrcDie,
                                             FailuresSrcMaxCount,
                                             FailuresSrcMaxPeriod,
                                             FailuresSrc),
                    {Error,
                     State#cloudi_queue{failures_source = FailuresSrcNew}}
            end;
        false ->
            FailuresSrcNew = failure(FailuresSrcDie,
                                     FailuresSrcMaxCount,
                                     FailuresSrcMaxPeriod,
                                     FailuresSrc),
            {{error, timeout},
             State#cloudi_queue{failures_source = FailuresSrcNew}}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the size of the CloudI queue.===
%% @end
%%-------------------------------------------------------------------------

-spec size(Dispatcher :: cloudi_service:dispatcher(),
           State :: state()) ->
    non_neg_integer().

size(Dispatcher,
     #cloudi_queue{ordered_requests = OrderedRequests,
                   requests = Requests})
    when is_pid(Dispatcher) ->
    queue:len(OrderedRequests) + maps:size(Requests).

%%-------------------------------------------------------------------------
%% @doc
%% ===A service request timeout.===
%% `ok' is returned if a service request is retried. Must be called from the
%% `cloudi_service_handle_info/3' callback function.
%% @end
%%-------------------------------------------------------------------------

-spec timeout(Dispatcher :: cloudi_service:dispatcher(),
              TimeoutAsync :: #timeout_async_active{},
              State :: state()) ->
    {ok, StateNew :: state()} |
    {{error, Reason :: cloudi_service:error_reason()}, StateNew :: state()} |
    {ignored, State :: state()}.

timeout(Dispatcher,
        #timeout_async_active{trans_id = TransId} = TimeoutAsync,
        #cloudi_queue{retry = Retry,
                      retry_delay = RetryDelay,
                      service = ServiceOld,
                      failures_source_die = FailuresSrcDie,
                      failures_source_max_count = FailuresSrcMaxCount,
                      failures_source_max_period = FailuresSrcMaxPeriod,
                      failures_source = FailuresSrc,
                      requests = Requests} = State)
    when is_pid(Dispatcher) ->
    case maps:find(TransId, Requests) of
        error ->
            {ignored, State};
        {ok, #request{retry_count = Retry}} ->
            FailuresSrcNew = failure(FailuresSrcDie,
                                     FailuresSrcMaxCount,
                                     FailuresSrcMaxPeriod,
                                     FailuresSrc),
            {{error, timeout},
             ordered_check(Dispatcher,
                           State#cloudi_queue{
                               failures_source = FailuresSrcNew,
                               requests = maps:remove(TransId, Requests)})};
        {ok, #request{retry_delay = false} = RequestState}
            when RetryDelay > 0 ->
            Service = if
                ServiceOld =:= undefined ->
                    cloudi_service:self(Dispatcher);
                is_pid(ServiceOld) ->
                    ServiceOld
            end,
            erlang:send_after(RetryDelay, Service, TimeoutAsync),
            RequestsNew = maps:put(TransId,
                                   RequestState#request{retry_delay = true},
                                   Requests),
            {ok, State#cloudi_queue{service = Service,
                                    requests = RequestsNew}};
        {ok, #request{retry_count = I} = RequestState}
            when I < Retry ->
            case queue_send_retry(Dispatcher, RequestState) of
                {ok, RequestStateNew} ->
                    RequestsNew = maps:put(TransId,
                                           RequestStateNew#request{
                                               retry_count = I + 1,
                                               retry_delay = false},
                                           Requests),
                    {ok, State#cloudi_queue{requests = RequestsNew}};
                {error, _} = Error ->
                    FailuresSrcNew = failure(FailuresSrcDie,
                                             FailuresSrcMaxCount,
                                             FailuresSrcMaxPeriod,
                                             FailuresSrc),
                    {Error,
                     ordered_check(Dispatcher,
                                   State#cloudi_queue{
                                       failures_source = FailuresSrcNew,
                                       requests = maps:remove(TransId,
                                                              Requests)})}
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

queue_send_first(Dispatcher, Name, RequestInfo, Request,
                 Timeout, Priority, undefined) ->
    case cloudi_service:get_pid(Dispatcher, Name, Timeout) of
        {ok, PatternPid} ->
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
                                            pattern_pid = PatternPid,
                                            retry_pattern_pid = false},
                    {ok, TransId, RequestState};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;
queue_send_first(Dispatcher, Name, RequestInfo, Request,
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
                                    pattern_pid = PatternPid,
                                    retry_pattern_pid = true},
            {ok, TransId, RequestState};
        {error, _} = Error ->
            Error
    end.

queue_send_first(Dispatcher, Name, RequestInfo, Request,
                 Timeout, Priority, TransId, undefined) ->
    case cloudi_service:get_pid(Dispatcher, Name, Timeout) of
        {ok, PatternPid} ->
            case cloudi_service:send_async_active(Dispatcher, Name,
                                                  RequestInfo, Request,
                                                  Timeout, Priority,
                                                  TransId, PatternPid) of
                {ok, TransId} ->
                    RequestState = #request{name = Name,
                                            request_info = RequestInfo,
                                            request = Request,
                                            timeout = Timeout,
                                            priority = Priority,
                                            id = TransId,
                                            pattern_pid = PatternPid,
                                            retry_pattern_pid = false},
                    {ok, RequestState};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;
queue_send_first(Dispatcher, Name, RequestInfo, Request,
                 Timeout, Priority, TransId, PatternPid) ->
    case cloudi_service:send_async_active(Dispatcher, Name,
                                          RequestInfo, Request,
                                          Timeout, Priority,
                                          TransId, PatternPid) of
        {ok, TransId} ->
            RequestState = #request{name = Name,
                                    request_info = RequestInfo,
                                    request = Request,
                                    timeout = Timeout,
                                    priority = Priority,
                                    id = TransId,
                                    pattern_pid = PatternPid,
                                    retry_pattern_pid = true},
            {ok, RequestState};
        {error, _} = Error ->
            Error
    end.

queue_send_retry(Dispatcher,
                 #request{name = Name,
                          request_info = RequestInfo,
                          request = Request,
                          timeout = Timeout,
                          priority = Priority,
                          id = TransId,
                          retry_pattern_pid = false} = RequestState) ->
    case cloudi_service:get_pid(Dispatcher, Name, Timeout) of
        {ok, PatternPidNew} ->
            case cloudi_service:send_async_active(Dispatcher, Name,
                                                  RequestInfo, Request,
                                                  Timeout, Priority,
                                                  TransId, PatternPidNew) of
                {ok, TransId} ->
                    {ok, RequestState#request{pattern_pid = PatternPidNew}};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;
queue_send_retry(Dispatcher,
                 #request{name = Name,
                          request_info = RequestInfo,
                          request = Request,
                          timeout = Timeout,
                          priority = Priority,
                          id = TransId,
                          pattern_pid = PatternPid,
                          retry_pattern_pid = true} = RequestState) ->
    case cloudi_service:send_async_active(Dispatcher, Name,
                                          RequestInfo, Request,
                                          Timeout, Priority,
                                          TransId, PatternPid) of
        {ok, TransId} ->
            {ok, RequestState};
        {error, _} = Error ->
            Error
    end.

queue_mcast(Dispatcher, Name, RequestInfo, Request,
            Timeout, Priority, Requests, Ordered) ->
    case cloudi_service:get_pids(Dispatcher, Name, Timeout) of
        {ok, PatternPids} ->
            queue_mcast(PatternPids, 0,
                        Dispatcher, Name, RequestInfo, Request,
                        Timeout, Priority, Requests, Ordered);
        {error, _} = Error ->
            {Error, 0, Requests}
    end.

queue_mcast([], OrderedPending, _, _, _, _, _, _, Requests, _) ->
    {ok, OrderedPending, Requests};
queue_mcast([PatternPid | PatternPids], OrderedPending,
            Dispatcher, Name, RequestInfo, Request,
            Timeout, Priority, Requests, Ordered) ->
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
                                    pattern_pid = PatternPid,
                                    retry_pattern_pid = true},
            OrderedPendingNew = if
                Ordered =:= true ->
                    OrderedPending + 1;
                Ordered =:= false ->
                    OrderedPending
            end,
            queue_mcast(PatternPids, OrderedPendingNew,
                        Dispatcher, Name, RequestInfo, Request,
                        Timeout, Priority,
                        maps:put(TransId,
                                 RequestState,
                                 Requests), Ordered);
        {error, _} = Error ->
            {Error, OrderedPending, Requests}
    end.

ordered_check(Dispatcher,
              #cloudi_queue{ordered = true,
                            ordered_requests = OrderedRequests,
                            ordered_pending = 1} = State) ->
    {Result, StateNew} = case queue:out(OrderedRequests) of
        {{value, #request_ordered_send{} = OrderedSend},
         OrderedRequestsNew} ->
            ordered_send(Dispatcher,
                         OrderedSend,
                         State#cloudi_queue{
                             ordered_requests = OrderedRequestsNew,
                             ordered_pending = 0});
        {{value, #request_ordered_mcast{} = OrderedMcast},
         OrderedRequestsNew} ->
            ordered_mcast(Dispatcher,
                          OrderedMcast,
                          State#cloudi_queue{
                              ordered_requests = OrderedRequestsNew,
                              ordered_pending = 0});
        {empty, OrderedRequestsNew} ->
            {ok, State#cloudi_queue{ordered_requests = OrderedRequestsNew,
                                    ordered_pending = 0}}
    end,
    #cloudi_queue{ordered_pending = OrderedPendingNew} = StateNew,
    case Result of
        ok ->
            StateNew;
        {error, _} when OrderedPendingNew == 0 ->
            ordered_check(Dispatcher, StateNew);
        {error, _} ->
            StateNew
    end;
ordered_check(_, #cloudi_queue{ordered = true,
                               ordered_pending = OrderedPending} = State)
    when OrderedPending > 1 ->
    State#cloudi_queue{ordered_pending = OrderedPending - 1};
ordered_check(_, #cloudi_queue{ordered = false,
                               ordered_pending = 0} = State) ->
    State.

ordered_send(Dispatcher,
             #request_ordered_send{name = Name,
                                   request_info = RequestInfo,
                                   request = Request,
                                   timeout = Timeout,
                                   priority = Priority,
                                   id = TransId,
                                   pattern_pid = PatternPid},
             #cloudi_queue{ordered = true,
                           failures_source_die = FailuresSrcDie,
                           failures_source_max_count = FailuresSrcMaxCount,
                           failures_source_max_period = FailuresSrcMaxPeriod,
                           failures_source = FailuresSrc,
                           ordered_pending = 0,
                           requests = Requests} = State) ->
    case queue_send_first(Dispatcher, Name, RequestInfo, Request,
                          Timeout, Priority, TransId, PatternPid) of
        {ok, RequestState} ->
            {ok,
             State#cloudi_queue{ordered_pending = 1,
                                requests = maps:put(TransId,
                                                    RequestState,
                                                    Requests)}};
        {error, _} = Error ->
            FailuresSrcNew = failure(FailuresSrcDie,
                                     FailuresSrcMaxCount,
                                     FailuresSrcMaxPeriod,
                                     FailuresSrc),
            {Error,
             State#cloudi_queue{failures_source = FailuresSrcNew}}
    end.

ordered_mcast(Dispatcher,
              #request_ordered_mcast{name = Name,
                                     request_info = RequestInfo,
                                     request = Request,
                                     timeout = Timeout,
                                     priority = Priority},
              #cloudi_queue{ordered = true = Ordered,
                            failures_source_die = FailuresSrcDie,
                            failures_source_max_count = FailuresSrcMaxCount,
                            failures_source_max_period = FailuresSrcMaxPeriod,
                            failures_source = FailuresSrc,
                            ordered_pending = 0,
                            requests = Requests} = State) ->
    case queue_mcast(Dispatcher, Name, RequestInfo, Request,
                     Timeout, Priority, Requests, Ordered) of
        {ok, OrderedPending, RequestsNew} ->
            {ok, State#cloudi_queue{ordered_pending = OrderedPending,
                                    requests = RequestsNew}};
        {{error, _} = Error, OrderedPending, RequestsNew} ->
            FailuresSrcNew = failure(FailuresSrcDie,
                                     FailuresSrcMaxCount,
                                     FailuresSrcMaxPeriod,
                                     FailuresSrc),
            {Error,
             State#cloudi_queue{failures_source = FailuresSrcNew,
                                ordered_pending = OrderedPending,
                                requests = RequestsNew}}
    end.

failure(false, _, _, FailureList) ->
    FailureList;
failure(true, MaxCount, MaxPeriod, FailureList) ->
    failure_check(cloudi_timestamp:seconds_monotonic(), FailureList,
                  MaxCount, MaxPeriod).

failure_store(FailureList, FailureCount, MaxCount) ->
    if
        FailureCount == MaxCount ->
            failure_kill();
        true ->
            ok
    end,
    FailureList.

failure_check(SecondsNow, FailureList, MaxCount, infinity) ->
    FailureCountNew = erlang:length(FailureList),
    failure_store([SecondsNow | FailureList], FailureCountNew + 1,
                  MaxCount);
failure_check(SecondsNow, FailureList, MaxCount, MaxPeriod) ->
    {FailureCountNew,
     FailureListNew} = cloudi_timestamp:seconds_filter_monotonic(FailureList,
                                                                 SecondsNow,
                                                                 MaxPeriod),
    failure_store([SecondsNow | FailureListNew], FailureCountNew + 1,
                  MaxCount).

failure_kill() ->
    erlang:exit(cloudi_queue).

