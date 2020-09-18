%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Funnel Service==
%%%
%%% WARNING: EXPERIMENTAL
%%%
%%% The funnel service is a way of using duplicate service request sends
%%% (possibly from separate service processes processing the same data)
%%% with the goal of making the sending service fault-tolerant.
%%%
%%% The funnel service is the opposite of cloudi_service_quorum due to
%%% receiving many service requests that may be duplicates and only sending
%%% unique service requests with distributed fault-tolerance.
%%% A duplicate service request has the same service name, request info and
%%% request.  Each duplicate still contains a unique trans_id and is only
%%% a duplicate while the timeout time periods elapse.
%%%
%%% The funnel service creates its own service request for the first
%%% unique service request it receives and any response it receives will be
%%% used for replies to duplicate service requests.  The last recent
%%% timeout time period before the response is received is used for
%%% retaining the response data for future duplicate service requests.
%%%
%%% With the funnel service's sensitivity to a service request's
%%% timeout value, other more robust approaches to fault-tolerance
%%% should be preferred.  The funnel service provides a way to make
%%% a CloudI service fault-tolerant without modifying the service
%%% (assuming the send destinations are easy to point at the funnel service)
%%% but it does add latency to the service request sends and may require
%%% larger timeout values.  If timeout values are too small,
%%% duplicate service requests may not be recognized by the funnel service
%%% and it could send more than a single service request for a group
%%% of duplicate service requests it receives.
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2020 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2020 Michael Truog
%%% @version 2.0.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_funnel).
-author('mjtruog at protonmail dot com').

-behaviour(cloudi_service).

%% cloudi_crdt update functions
-export([request_crdt_merge/2,
         request_send_retry_crdt/1,
         response_store_crdt/2,
         response_timeout_crdt/2]).

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include_lib("cloudi_core/include/cloudi_service.hrl").
-include_lib("cloudi_core/include/cloudi_crdt.hrl").

-define(DEFAULT_NAME,                          "funnel").
        % Funnel name used for incoming service requests
        % (Prefix ++ FunnelName ++ "*")
-define(DEFAULT_NODE_COUNT,                           2).
        % Count of connected nodes using the funnel service
        % with the same name argument.
-define(DEFAULT_RETRY,                                0).
-define(DEFAULT_RETRY_DELAY,                          0). % milliseconds

-type request_key() ::
    {cloudi_service:service_name(),
     cloudi_service:request_info(),
     cloudi_service:request()}.

-type process_index() :: non_neg_integer().
-type sender_id() :: {node(), process_index()}.

-record(request,
    {
        name
            :: cloudi_service:service_name(),
        pattern
            :: cloudi_service:service_name_pattern(),
        timeout_last
            :: cloudi_service:timeout_value_milliseconds(),
        priority_min
            :: cloudi_service:priority_value(),
        trans_id_first
            :: cloudi_service:trans_id(),
        trans_id_last
            :: cloudi_service:trans_id(),
        pending
            :: list({cloudi_service:request_type(),
                     cloudi_service:timeout_value_milliseconds(),
                     cloudi_service:trans_id(),
                     pid()}),
        sender_id
            :: sender_id(),
        response = undefined
            :: undefined |
               {cloudi_service:response_info(),
                cloudi_service:response()},
        retry_count = 0
            :: non_neg_integer()
    }).

-record(state,
    {
        sender_id :: sender_id(),
        service :: cloudi_service:source(),
        retry :: non_neg_integer(),
        retry_delay :: non_neg_integer(),
        crdt :: cloudi_crdt:state(),
        sent = #{} :: #{cloudi_service:trans_id() := request_key()}
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, Prefix, _Timeout, Dispatcher) ->
    Defaults = [
        {name,                          ?DEFAULT_NAME},
        {node_count,                    ?DEFAULT_NODE_COUNT},
        {retry,                         ?DEFAULT_RETRY},
        {retry_delay,                   ?DEFAULT_RETRY_DELAY}],
    [Name, NodeCount,
     Retry, RetryDelay] = cloudi_proplists:take_values(Defaults, Args),
    false = cloudi_service_name:pattern(Prefix),
    true = cloudi_service:destination_refresh_immediate(Dispatcher),
    true = is_list(Name) andalso is_integer(hd(Name)),
    false = lists:member($/, Name),
    false = cloudi_service_name:pattern(Name),
    true = is_integer(NodeCount) andalso (NodeCount > 0),
    true = is_integer(Retry) andalso (Retry >= 0),
    true = is_integer(RetryDelay) andalso
           (RetryDelay >= 0) andalso (RetryDelay =< 4294967295),
    true = ((Retry == 0) andalso (RetryDelay == 0)) orelse
           ((Retry > 0) andalso (RetryDelay >= 0)),
    ProcessIndex = cloudi_service:process_index(Dispatcher),
    SenderId = {node(), ProcessIndex},
    Service = cloudi_service:self(Dispatcher),
    InitialDataF = restart_crdt_data_f(Service),
    CRDT0 = cloudi_crdt:new(Dispatcher,
                            [{service_name, "crdt_" ++ Name},
                             {initial_data_function, InitialDataF},
                             {retry, 20}, % 5 minutes total
                             {retry_delay, 15 * 1000}, % 15 seconds
                             {clean_vclocks_failure, 100.0 / NodeCount},
                             {priority_default_offset, -1}]),
    CRDTN = cloudi_crdt:events_subscriptions(Dispatcher,
                                             [assign, update], CRDT0),
    cloudi_service:subscribe(Dispatcher, Name ++ "*"),
    {ok, #state{sender_id = SenderId,
                service = Service,
                retry = Retry,
                retry_delay = RetryDelay,
                crdt = CRDTN}}.

cloudi_service_handle_request(RequestType, Name, Pattern, RequestInfo, Request,
                              Timeout, Priority, TransId, Pid,
                              #state{crdt = CRDT0} = State, Dispatcher) ->
    case cloudi_service_name:parse(Name, Pattern) of
        [FunnelName] ->
            request(FunnelName,
                    RequestType, Name, Pattern, RequestInfo, Request,
                    Timeout, Priority, TransId, Pid, State, Dispatcher);
        [] ->
            {ok, CRDTN} = cloudi_crdt:
                          handle_request(RequestType, Name, Pattern,
                                         RequestInfo, Request,
                                         Timeout, Priority, TransId, Pid,
                                         CRDT0, Dispatcher),
            {noreply, State#state{crdt = CRDTN}}
    end.

cloudi_service_handle_info({response_return,
                            Name, Pattern, ResponseInfo, Response, Pending},
                           State, Dispatcher) ->
    ok = response_return(Pending,
                         Name, Pattern, ResponseInfo, Response,
                         cloudi_trans_id:microseconds(), Dispatcher),
    {noreply, State};
cloudi_service_handle_info({response_timeout, RequestKey},
                           #state{service = Service,
                                  crdt = CRDT0} = State, Dispatcher) ->
    UpdateF = response_timeout_crdt,
    CRDTN = cloudi_crdt:update_clear_id(Dispatcher,
                                        RequestKey,
                                        ?MODULE,
                                        UpdateF,
                                        [RequestKey, Service],
                                        UpdateF,
                                        CRDT0),
    {noreply, State#state{crdt = CRDTN}};
cloudi_service_handle_info({retry, RequestKey},
                           #state{crdt = CRDT} = State, Dispatcher) ->
    RequestValue = cloudi_crdt:get(Dispatcher, RequestKey, CRDT),
    StateNew = request_send(RequestKey, RequestValue, State, Dispatcher),
    {noreply, StateNew};
cloudi_service_handle_info({restart_crdt_data, Data}, State, Dispatcher) ->
    StateNew = restart_crdt_data(Data, State, Dispatcher),
    {noreply, StateNew};
cloudi_service_handle_info(#crdt_event{type = assign,
                                       id = request_crdt_merge,
                                       key = RequestKey,
                                       new = {value, RequestValue}},
                           State, Dispatcher) ->
    StateNew = request_send(RequestKey, RequestValue, State, Dispatcher),
    {noreply, StateNew};
cloudi_service_handle_info(#crdt_event{type = update,
                                       id = request_send_retry_crdt,
                                       key = RequestKey},
                           #state{service = Service,
                                  retry_delay = RetryDelay} = State,
                           _Dispatcher) ->
    erlang:send_after(RetryDelay, Service, {retry, RequestKey}),
    {noreply, State};
cloudi_service_handle_info(#crdt_event{}, State, _Dispatcher) ->
    {noreply, State};
cloudi_service_handle_info(Request,
                           #state{crdt = CRDT0,
                                  sent = Sent} = State, Dispatcher) ->
    case cloudi_crdt:handle_info(Request, CRDT0, Dispatcher) of
        {ok, CRDTN} ->
            {noreply, State#state{crdt = CRDTN}};
        {ignored, CRDTN} ->
            StateNew = case Request of
                #return_async_active{response_info = ResponseInfo,
                                     response = Response,
                                     trans_id = TransId} ->
                    {RequestKey, SentNew} = maps:take(TransId, Sent),
                    response_store(RequestKey, ResponseInfo, Response,
                                   State#state{crdt = CRDTN,
                                               sent = SentNew}, Dispatcher);
                #timeout_async_active{trans_id = TransId} ->
                    {RequestKey, SentNew} = maps:take(TransId, Sent),
                    request_send_retry(RequestKey,
                                       State#state{sent = SentNew}, Dispatcher)
            end,
            {noreply, StateNew}
    end.

cloudi_service_terminate(_Reason, _Timeout, _State) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

restart_crdt_data_f(Service) ->
    fun(Data) ->
        Service ! {restart_crdt_data, Data}
    end.

restart_crdt_data(Data, State, Dispatcher) ->
    % process data after a restart that will not be present in events
    % is used to create new service requests for this sender,
    % if no response is already present
    maps:fold(fun(RequestKey, RequestValue, StateNext) ->
        request_send(RequestKey, RequestValue, StateNext, Dispatcher)
    end, State, Data).

request(FunnelName,
        RequestType, Name, Pattern, RequestInfo, Request,
        Timeout, Priority, TransId, Pid,
        #state{sender_id = SenderId,
               crdt = CRDT0} = State, Dispatcher) ->
    RequestKey = {FunnelName, RequestInfo, Request},
    case cloudi_crdt:find(Dispatcher, RequestKey, CRDT0) of
        {ok, #request{response = {ResponseInfo, Response}}} ->
            {reply, ResponseInfo, Response, State};
        _ ->
            Pending = [{RequestType, Timeout, TransId, Pid}],
            RequestValue = #request{name = Name,
                                    pattern = Pattern,
                                    timeout_last = Timeout,
                                    priority_min = Priority,
                                    trans_id_first = TransId,
                                    trans_id_last = TransId,
                                    pending = Pending,
                                    sender_id = SenderId},
            UpdateF = request_crdt_merge,
            CRDTN = cloudi_crdt:update_assign_id(Dispatcher,
                                                 RequestKey,
                                                 RequestValue,
                                                 ?MODULE,
                                                 UpdateF,
                                                 RequestValue,
                                                 UpdateF,
                                                 CRDT0),
            {noreply, State#state{crdt = CRDTN}}
    end.

request_crdt_merge(#request{name = Name,
                            pattern = Pattern,
                            timeout_last = Timeout,
                            priority_min = Priority,
                            trans_id_first = TransId,
                            trans_id_last = TransId,
                            pending = [PendingValue]},
                   #request{name = Name,
                            pattern = Pattern,
                            timeout_last = TimeoutLastOld,
                            priority_min = PriorityMinOld,
                            trans_id_first = TransIdFirstOld,
                            trans_id_last = TransIdLastOld,
                            pending = PendingOld} = RequestValueOld) ->
    MicroSeconds = cloudi_trans_id:microseconds(TransId),
    MicroSecondsFirstOld = cloudi_trans_id:microseconds(TransIdFirstOld),
    MicroSecondsLastOld = cloudi_trans_id:microseconds(TransIdLastOld),
    TransIdFirst = if
        MicroSecondsFirstOld < MicroSeconds ->
            TransIdFirstOld;
        true ->
            TransId
    end,
    {TimeoutLast,
     TransIdLast} = if
        MicroSecondsLastOld < MicroSeconds ->
            {Timeout,
             TransId};
        true ->
            {TimeoutLastOld,
             TransIdLastOld}
    end,
    PriorityMin = erlang:min(PriorityMinOld, Priority),
    RequestValueOld#request{timeout_last = TimeoutLast,
                            priority_min = PriorityMin,
                            trans_id_first = TransIdFirst,
                            trans_id_last = TransIdLast,
                            pending = [PendingValue | PendingOld]}.

request_send(RequestKey,
             #request{timeout_last = TimeoutLast,
                      priority_min = PriorityMin,
                      trans_id_last = TransIdLast,
                      sender_id = SenderId,
                      response = undefined} = RequestValue,
             #state{sender_id = SenderId,
                    sent = Sent} = State, Dispatcher) ->
    {FunnelName, RequestInfo, Request} = RequestKey,
    Timeout = timeout_current(TimeoutLast, TransIdLast),
    case cloudi_service:send_async_active(Dispatcher, FunnelName,
                                          RequestInfo, Request,
                                          Timeout, PriorityMin) of
        {ok, TransId} ->
            State#state{sent = maps:put(TransId, RequestKey, Sent)};
        {error, timeout} ->
            request_send_retry(RequestKey, RequestValue, State, Dispatcher)
    end;
request_send(_, _, State, _) ->
    State.

request_send_retry(RequestKey,
                   #state{crdt = CRDT} = State, Dispatcher) ->
    RequestValue = cloudi_crdt:get(Dispatcher, RequestKey, CRDT),
    request_send_retry(RequestKey, RequestValue, State, Dispatcher).

request_send_retry(RequestKey,
                   #request{retry_count = RetryCount},
                   #state{retry = RetryCountMax,
                          crdt = CRDT0} = State, Dispatcher) ->
    if
        RetryCount < RetryCountMax ->
            UpdateF = request_send_retry_crdt,
            CRDTN = cloudi_crdt:update_id(Dispatcher,
                                          RequestKey,
                                          ?MODULE,
                                          UpdateF,
                                          UpdateF,
                                          CRDT0),
            State#state{crdt = CRDTN};
        RetryCount == RetryCountMax ->
            ResponseInfo = <<>>,
            Response = <<>>,
            response_store(RequestKey, ResponseInfo, Response,
                           State, Dispatcher)
    end.

request_send_retry_crdt(#request{retry_count = RetryCount} = RequestValue) ->
    RequestValue#request{retry_count = RetryCount + 1}.

response_store(RequestKey, ResponseInfo, Response,
               #state{service = Service,
                      crdt = CRDT0} = State, Dispatcher) ->
    UpdateF = response_store_crdt,
    ResponseData = {ResponseInfo, Response},
    CRDTN = cloudi_crdt:update_id(Dispatcher,
                                  RequestKey,
                                  ?MODULE,
                                  UpdateF,
                                  [RequestKey, ResponseData, Service],
                                  UpdateF,
                                  CRDT0),
    State#state{crdt = CRDTN}.

response_store_crdt([RequestKey, ResponseData, Service],
                    #request{name = Name,
                             pattern = Pattern,
                             timeout_last = TimeoutLast,
                             trans_id_last = TransIdLast,
                             pending = Pending} = RequestValue) ->
    {ResponseInfo, Response} = ResponseData,
    Service ! {response_return, Name, Pattern, ResponseInfo, Response, Pending},
    ResponseTimeout = timeout_current(TimeoutLast, TransIdLast),
    erlang:send_after(ResponseTimeout, Service,
                      {response_timeout, RequestKey}),
    RequestValue#request{pending = [],
                         response = ResponseData}.

response_return([], _, _, _, _, _, _) ->
    ok;
response_return([{RequestType, Timeout, TransId, Pid} | Pending],
                Name, Pattern, ResponseInfo, Response,
                MicroSecondsNow, Dispatcher) ->
    MilliSecondsElapsed = (MicroSecondsNow -
                           cloudi_trans_id:microseconds(TransId)) div 1000,
    if
        MilliSecondsElapsed =< Timeout ->
            cloudi_service:return_nothrow(Dispatcher, RequestType,
                                          Name, Pattern,
                                          ResponseInfo, Response,
                                          Timeout, TransId, Pid);
        true ->
            ok
    end,
    response_return(Pending, Name, Pattern, ResponseInfo, Response,
                    MicroSecondsNow, Dispatcher).

response_timeout_crdt([RequestKey, Service],
                      #request{timeout_last = TimeoutLast,
                               trans_id_last = TransIdLast} = RequestValue) ->
    case timeout_current(TimeoutLast, TransIdLast) of
        0 ->
            undefined;
        ResponseTimeout ->
            erlang:send_after(ResponseTimeout, Service,
                              {response_timeout, RequestKey}),
            RequestValue
    end.

timeout_current(TimeoutLast, TransIdLast) ->
    MilliSecondsElapsed = (cloudi_trans_id:microseconds() -
                           cloudi_trans_id:microseconds(TransIdLast)) div 1000,
    if
        TimeoutLast < MilliSecondsElapsed ->
            0;
        true ->
            TimeoutLast - MilliSecondsElapsed
    end.

