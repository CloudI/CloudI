%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Service Request Send CloudI Service==
%%% The send service has its lifetime tied to the sending of service requests
%%% provided as initialization arguments.  Use the send service when it is
%%% necessary to initiate processing in other services that doesn't occur
%%% automatically.
%%%
%%% The send service configuration could be added to
%%% cloudi_service_api_batch configuration
%%% (or provided to the cloudi_service_api_batch:services_add function)
%%% for sending service requests to cloudi_service_shell
%%% before other queued service configurations are started by
%%% cloudi_service_api_batch.
%%%
%%% To avoid having the send service restart when 1 or more
%%% service request sends fail, set MaxR to 0 in its service configuration.
%%% A retry argument is provided if service request send retries are necessary.
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2021-2022 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2021-2022 Michael Truog
%%% @version 2.0.5 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_send).
-author('mjtruog at protonmail dot com').

-behaviour(cloudi_service).

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include_lib("cloudi_core/include/cloudi_service.hrl").

-define(DEFAULT_SENDS,                         []).
-define(DEFAULT_ASYNC,                      false).
        % Do not validate the response or wait for the response.
-define(DEFAULT_VALIDATE_RESPONSE,
        {cloudi_service_shell, validate_response}).
-define(DEFAULT_MCAST,                      false).
        % Use mcast for each send.
-define(DEFAULT_ORDERED,                    false).
        % Order all sends.
-define(DEFAULT_RETRY,                          0).
        % Use a max retry count for all sends.
-define(DEFAULT_RETRY_DELAY,                    0). % milliseconds
        % If there is a retry count set,
        % what delay should be used before each retry.
-define(DEFAULT_DEBUG,                      false). % log output for debugging
-define(DEFAULT_DEBUG_LEVEL,                trace).

-record(state,
    {
        async :: boolean(),
        sends_failed :: non_neg_integer(),
        queue :: cloudi_queue:state(),
        validate_response :: fun((any(), any()) -> boolean()),
        debug_level :: off | trace | debug | info | warn | error | fatal
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, _Prefix, _Timeout, Dispatcher) ->
    Defaults = [
        {sends,                    ?DEFAULT_SENDS},
        {async,                    ?DEFAULT_ASYNC},
        {validate_response,        ?DEFAULT_VALIDATE_RESPONSE},
        {mcast,                    ?DEFAULT_MCAST},
        {ordered,                  ?DEFAULT_ORDERED},
        {retry,                    ?DEFAULT_RETRY},
        {retry_delay,              ?DEFAULT_RETRY_DELAY},
        {debug,                    ?DEFAULT_DEBUG},
        {debug_level,              ?DEFAULT_DEBUG_LEVEL}],
    [Sends, Async, ValidateResponse0, Mcast, Ordered, Retry, RetryDelay,
     Debug, DebugLevel] = cloudi_proplists:take_values(Defaults, Args),
    if
        Async =:= true ->
            self() ! async,
            true = Retry == 0,
            true = Ordered =:= false,
            true = Debug =:= false;
        Async =:= false ->
            true
    end,
    true = is_boolean(Mcast),
    {SendsFailed,
     Queue} = sends(Sends,
                    cloudi_queue:new([{retry, Retry},
                                      {retry_delay, RetryDelay},
                                      {ordered, Ordered}]),
                    Mcast,
                    Dispatcher),
    ValidateResponseN = cloudi_args_type:
                        function_required(ValidateResponse0, 2),
    true = is_boolean(Debug),
    true = ((DebugLevel =:= trace) orelse
            (DebugLevel =:= debug) orelse
            (DebugLevel =:= info) orelse
            (DebugLevel =:= warn) orelse
            (DebugLevel =:= error) orelse
            (DebugLevel =:= fatal)),
    DebugLogLevel = if
        Debug =:= false ->
            off;
        Debug =:= true ->
            DebugLevel
    end,
    {ok, #state{async = Async,
                sends_failed = SendsFailed,
                queue = Queue,
                validate_response = ValidateResponseN,
                debug_level = DebugLogLevel}}.

cloudi_service_handle_info(Request,
                           #state{async = true,
                                  sends_failed = SendsFailed} = State, _) ->
    async = Request,
    {stop, stop_reason(SendsFailed), State};
cloudi_service_handle_info(Request, State, Dispatcher) ->
    StateNew = recv(Request, State, Dispatcher),
    #state{sends_failed = SendsFailedNew,
           queue = QueueNew} = StateNew,
    Stop = cloudi_queue:size(Dispatcher, QueueNew) == 0,
    if
        Stop =:= true ->
            {stop, stop_reason(SendsFailedNew), StateNew};
        Stop =:= false ->
            {noreply, StateNew}
    end.

cloudi_service_terminate(_Reason, _Timeout, _State) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

send_args_valid({Name, Request}) ->
    true = cloudi_args_type:service_name(Name),
    {Name, <<>>, Request, undefined, undefined};
send_args_valid({Name, Request, Timeout}) ->
    true = cloudi_args_type:service_name(Name),
    true = cloudi_args_type:timeout_period(Timeout),
    {Name, <<>>, Request, Timeout, undefined};
send_args_valid({Name, RequestInfo, Request, Timeout}) ->
    true = cloudi_args_type:service_name(Name),
    true = cloudi_args_type:timeout_period(Timeout),
    {Name, RequestInfo, Request, Timeout, undefined};
send_args_valid({Name, _RequestInfo, _Request, Timeout, Priority} = Args) ->
    true = cloudi_args_type:service_name(Name),
    true = cloudi_args_type:timeout_period(Timeout),
    true = cloudi_args_type:priority(Priority),
    Args.

sends([], Queue, SendsFailed, _, _) ->
    {SendsFailed, Queue};
sends([Send | Sends], Queue, SendsFailed, Mcast, Dispatcher) ->
    {Name, RequestInfo, Request, Timeout, Priority} = send_args_valid(Send),
    {Result, QueueNew} = if
        Mcast =:= true ->
            cloudi_queue:mcast(Dispatcher, Name, RequestInfo, Request,
                               Timeout, Priority, Queue);
        Mcast =:= false ->
            cloudi_queue:send(Dispatcher, Name, RequestInfo, Request,
                              Timeout, Priority, Queue)
    end,
    SendsFailedNew = case Result of
        ok ->
            SendsFailed;
        {error, timeout} ->
            ?LOG_ERROR("send to \"~s\" failed", [Name]),
            SendsFailed + 1
    end,
    sends(Sends, QueueNew, SendsFailedNew, Mcast, Dispatcher).

sends([_ | _] = Sends, Queue, Mcast, Dispatcher) ->
    sends(Sends, Queue, 0, Mcast, Dispatcher).

recv(Request,
     #state{sends_failed = SendsFailed,
            queue = Queue,
            validate_response = ValidateResponse,
            debug_level = DebugLogLevel} = State, Dispatcher) ->
    {Result,
     QueueNew} = cloudi_queue:handle_info(Request, Queue, Dispatcher),
    SendsFailedNew = case Result of
        ok ->
            #return_async_active{response_info = ResponseInfo,
                                 response = Response} = Request,
            case ValidateResponse(ResponseInfo, Response) of
                true ->
                    ?LOG(DebugLogLevel,
                         "send successful", []),
                    SendsFailed;
                false ->
                    ?LOG_ERROR("send response error", []),
                    SendsFailed + 1
            end;
        {error, timeout} ->
            ?LOG_ERROR("send response timeout", []),
            SendsFailed + 1
    end,
    State#state{sends_failed = SendsFailedNew,
                queue = QueueNew}.

stop_reason(0) ->
    shutdown;
stop_reason(SendsFailed)
    when SendsFailed > 0 ->
    {sends_failed, SendsFailed}.

