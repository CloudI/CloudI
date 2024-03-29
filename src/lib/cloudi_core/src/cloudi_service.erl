%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Internal Service Behavior==
%%% The interface which all internal services must implement.
%%% ```
%%% The user module should export:
%%%
%%%   cloudi_service_init(Args, Prefix, Timeout, Dispatcher)
%%%
%%%    ==> {ok, State}
%%%        {stop, Reason}
%%%        {stop, Reason, State}
%%%               State = undefined, if not returned
%%%               Reason = restart | shutdown | Term
%%%
%%%   cloudi_service_handle_request(RequestType, Name, Pattern,
%%%                                 RequestInfo, Request,
%%%                                 Timeout, Priority, TransId, Source,
%%%                                 State, Dispatcher)
%%%
%%%    ==> {reply, Response, StateNew}
%%%        {reply, ResponseInfo, Response, StateNew}
%%%        {forward, NameNext, RequestInfoNext, RequestNext, StateNew}
%%%        {forward, NameNext, RequestInfoNext, RequestNext,
%%%         TimeoutNext, PriorityNext, StateNew}
%%%        {noreply, StateNew}
%%%        {stop, Reason, StateNew}
%%%               Reason = restart | shutdown | Term
%%%
%%%   cloudi_service_handle_info(Request, State, Dispatcher)
%%%
%%%    ==> {noreply, State}
%%%        {stop, Reason, StateNew}
%%%               Reason = restart | shutdown | Term
%%%
%%%   cloudi_service_terminate(Reason, Timeout, State)
%%%
%%%        Always called when the service terminates
%%%        (either due to a stop tuple return value,
%%%         an error/exit/throw exception or an exit signal).
%%%
%%%    ==> ok
%%%
%%%
%%% The work flow (of the service) can be described as follows:
%%%
%%%    User module                                          Generic
%%%    -----------                                          -------
%%%    cloudi_service_init              <-----              .
%%%
%%%                                                         loop
%%%    cloudi_service_handle_request    <-----              .
%%%                                     ----->              reply
%%%
%%%    cloudi_service_handle_info       <-----              .
%%%
%%%    cloudi_service_terminate         <-----              .
%%%
%%% '''
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2011-2022 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2011-2022 Michael Truog
%%% @version 2.0.5 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service).
-author('mjtruog at protonmail dot com').

%% behavior interface
-export([process_index/1,
         process_count/1,
         process_count_max/1,
         process_count_min/1,
         self/1,
         monitor/2,
         demonitor/2,
         demonitor/3,
         dispatcher/1,
         subscribe/2,
         subscribe_count/2,
         unsubscribe/2,
         get_pid/2,
         get_pid/3,
         get_pids/2,
         get_pids/3,
         send_async/3,
         send_async/4,
         send_async/5,
         send_async/6,
         send_async/7,
         send_async_active/3,
         send_async_active/4,
         send_async_active/5,
         send_async_active/6,
         send_async_active/7,
         send_async_active/8,
         send_async_passive/3,
         send_async_passive/4,
         send_async_passive/5,
         send_async_passive/6,
         send_async_passive/7,
         send_sync/3,
         send_sync/4,
         send_sync/5,
         send_sync/6,
         send_sync/7,
         mcast_async/3,
         mcast_async/4,
         mcast_async/6,
         mcast_async_active/3,
         mcast_async_active/4,
         mcast_async_active/6,
         mcast_async_passive/3,
         mcast_async_passive/4,
         mcast_async_passive/6,
         forward/9,
         forward_async/8,
         forward_sync/8,
         return/2,
         return/3,
         return/9,
         return_async/8,
         return_sync/8,
         return_nothrow/9,
         recv_async/1,
         recv_async/2,
         recv_async/3,
         recv_async/4,
         recv_asyncs/2,
         recv_asyncs/3,
         recv_asyncs/4,
         % service configuration
         prefix/1,
         suffix/2,
         timeout_async/1,
         timeout_sync/1,
         timeout_max/1,
         priority_default/1,
         destination_refresh_immediate/1,
         destination_refresh_lazy/1,
         duo_mode/1,
         source_subscriptions/2,
         context_options/1,
         trans_id/1]).

-include("cloudi_core_i_constants.hrl").

-type request_type() :: 'send_async' | 'send_sync'.
-type service_name() :: cloudi:service_name().
-type service_name_pattern() :: cloudi:service_name_pattern().
-type service_name_pattern_suffix() :: cloudi:service_name_pattern_suffix().
-type request_info() :: cloudi:request_info().
-type request() :: cloudi:request().
-type response_info() :: cloudi:response_info().
-type response() :: cloudi:response().
-type timeout_value_milliseconds() :: cloudi:timeout_value_milliseconds().
-type timeout_milliseconds() :: cloudi:timeout_milliseconds().
-type timeout_period() :: cloudi:timeout_period().
-type priority_value() :: cloudi:priority_value().
-type priority() :: cloudi:priority().
-type trans_id() :: cloudi:trans_id(). % version 1 UUID
-type pattern_pid() :: cloudi:pattern_pid().
-export_type([request_type/0,
              service_name/0,
              service_name_pattern/0,
              service_name_pattern_suffix/0,
              request_info/0, request/0,
              response_info/0, response/0,
              timeout_value_milliseconds/0,
              timeout_milliseconds/0,
              timeout_period/0,
              priority_value/0,
              priority/0,
              trans_id/0,
              pattern_pid/0]).

% Dispatcher Erlang Process of a CloudI Service that will send the
% service request.
-type dispatcher() :: pid().
% Source Erlang Process of a CloudI Service that will receive the
% service request response.  The same Erlang process receives the
% CloudI Service's incoming service requests based on its subscriptions.
-type source() :: pid().
-export_type([dispatcher/0,
              source/0]).

-type error_reason() :: timeout.
-export_type([error_reason/0]).

% for cloudi_service_api:aspect_request_after_internal() and
% cloudi_service_api:aspect_request_after_external() and
-type request_result() ::
    {reply, ResponseInfo :: response_info(), Response :: response()} |
    {forward, NameNext :: service_name(),
     RequestInfoNext :: request_info(), RequestNext :: request(),
     TimeoutNext :: timeout_value_milliseconds(),
     PriorityNext :: priority_value()} |
    noreply.
-export_type([request_result/0]).

% used for accessing RequestInfo data
-type key_values(Key, Value) :: cloudi_key_value:key_values(Key, Value).
-type key_values() :: cloudi_key_value:key_values().
-export_type([key_values/2,
              key_values/0]).

%%%------------------------------------------------------------------------
%%% Callback functions for behavior
%%%------------------------------------------------------------------------

-callback cloudi_service_init(Args :: list(),
                              Prefix :: service_name_pattern(),
                              Timeout ::
                                  cloudi_service_api:
                                  timeout_initialize_value_milliseconds(),
                              Dispatcher :: dispatcher()) ->
    {ok, State :: any()} |
    {stop, Reason :: any()} |
    {stop, Reason :: any(), State :: any()}.

-callback cloudi_service_handle_request(RequestType :: request_type(),
                                        Name :: service_name(),
                                        Pattern :: service_name_pattern(),
                                        RequestInfo :: request_info(),
                                        Request :: request(),
                                        Timeout :: timeout_value_milliseconds(),
                                        Priority :: priority_value(),
                                        TransId :: trans_id(),
                                        Source :: source(),
                                        State :: any(),
                                        Dispatcher :: dispatcher()) ->
    {reply, Response :: response(), StateNew :: any()} |
    {reply, ResponseInfo :: response_info(), Response :: response(),
     StateNew :: any()} |
    {forward, NameNext :: service_name(),
     RequestInfoNext :: request_info(), RequestNext :: request(),
     StateNew :: any()} |
    {forward, NameNext :: service_name(),
     RequestInfoNext :: request_info(), RequestNext :: request(),
     TimeoutNext :: timeout_value_milliseconds(),
     PriorityNext :: priority_value(), StateNew :: any()} |
    {noreply, StateNew :: any()} |
    {stop, Reason :: any(), StateNew :: any()}.

-callback cloudi_service_handle_info(Request :: any(),
                                     State :: any(),
                                     Dispatcher :: dispatcher()) ->
    {noreply, StateNew :: any()} |
    {stop, Reason :: any(), StateNew :: any()}.

-callback cloudi_service_terminate(Reason :: any(),
                                   Timeout ::
                                       cloudi_service_api:
                                       timeout_terminate_value_milliseconds(),
                                   State :: any()) ->
    ok.

-optional_callbacks([cloudi_service_handle_request/11,
                     cloudi_service_handle_info/3]).

-include("cloudi_core_i_common_interface.hrl").

%%%------------------------------------------------------------------------
%%% Behavior interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the 0-based index of this instance of the service.===
%% The configuration of the service defined how many instances should exist.
%% @end
%%-------------------------------------------------------------------------

-spec process_index(Dispatcher :: dispatcher()) ->
    ProcessIndex :: non_neg_integer().

process_index(Dispatcher)
    when is_pid(Dispatcher) ->
    gen_server:call(Dispatcher, process_index, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the initial process count of this instance of the service.===
%% The configuration of the service defined how many instances should exist.
%% @end
%%-------------------------------------------------------------------------

-spec process_count(Dispatcher :: dispatcher()) ->
    ProcessCount :: pos_integer().

process_count(Dispatcher)
    when is_pid(Dispatcher) ->
    gen_server:call(Dispatcher, process_count, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the process count maximum of this instance of the service.===
%% This will be the same as the process_count, unless count_process_dynamic
%% configuration provides a maximum that is greater than the process_count.
%% @end
%%-------------------------------------------------------------------------

-spec process_count_max(Dispatcher :: dispatcher()) ->
    ProcessCountMax :: pos_integer().

process_count_max(Dispatcher)
    when is_pid(Dispatcher) ->
    gen_server:call(Dispatcher, process_count_max, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the process count minimum of this instance of the service.===
%% This will be the same as the process_count, unless count_process_dynamic
%% configuration provides a minimum that is less than the process_count.
%% @end
%%-------------------------------------------------------------------------

-spec process_count_min(Dispatcher :: dispatcher()) ->
    ProcessCountMin :: pos_integer().

process_count_min(Dispatcher)
    when is_pid(Dispatcher) ->
    gen_server:call(Dispatcher, process_count_min, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the Erlang pid representing the service.===
%% @end
%%-------------------------------------------------------------------------

-spec self(Dispatcher :: dispatcher()) ->
    Self :: source().

self(Dispatcher)
    when is_pid(Dispatcher) ->
    gen_server:call(Dispatcher, self, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Monitor an Erlang pid.===
%% The function will make sure an Erlang pid is monitored properly so the
%% cloudi_service_handle_info/3 callback receives the monitor result.
%% Since both the request_pid and the info_pid are temporary Erlang processes,
%% no spawned Erlang process should be linked to the request_pid or info_pid.
%% @end
%%-------------------------------------------------------------------------

-spec monitor(Dispatcher :: dispatcher(),
              Pid :: pid()) ->
    MonitorRef :: reference().

monitor(Dispatcher, Pid)
    when is_pid(Dispatcher), is_pid(Pid) ->
    gen_server:call(Dispatcher, {monitor, Pid}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Demonitor an Erlang pid.===
%% The function will make sure an Erlang pid is demonitored properly.
%% @end
%%-------------------------------------------------------------------------

-spec demonitor(Dispatcher :: dispatcher(),
                MonitorRef :: reference()) ->
    true.

demonitor(Dispatcher, MonitorRef)
    when is_pid(Dispatcher), is_reference(MonitorRef) ->
    gen_server:call(Dispatcher, {demonitor, MonitorRef}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Demonitor an Erlang pid with options.===
%% The function will make sure an Erlang pid is demonitored properly.
%% @end
%%-------------------------------------------------------------------------

-spec demonitor(Dispatcher :: dispatcher(),
                MonitorRef :: reference(),
                Options :: list()) ->
    true.

demonitor(Dispatcher, MonitorRef, Options)
    when is_pid(Dispatcher), is_reference(MonitorRef), is_list(Options) ->
    gen_server:call(Dispatcher, {demonitor, MonitorRef, Options}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the Erlang pid representing the service sender.===
%% Use when the Dispatcher is stored and used after the current
%% cloudi_service callback has returned.  This is only necessary when
%% storing the dispatcher within the cloudi_service_init/4 callback,
%% for use in a different callback.
%% @end
%%-------------------------------------------------------------------------

-spec dispatcher(Dispatcher :: dispatcher()) ->
    DispatcherNew :: pid().

dispatcher(Dispatcher)
    when is_pid(Dispatcher) ->
    gen_server:call(Dispatcher, dispatcher, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Subscribe to a service name pattern.===
%% @end
%%-------------------------------------------------------------------------

-spec subscribe(Dispatcher :: dispatcher(),
                Pattern :: service_name_pattern_suffix()) ->
    ok | error.

subscribe(Dispatcher, Pattern)
    when is_pid(Dispatcher), is_list(Pattern) ->
    gen_server:call(Dispatcher, {'subscribe', Pattern}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Determine how may service name pattern subscriptions have occurred.===
%% @end
%%-------------------------------------------------------------------------

-spec subscribe_count(Dispatcher :: dispatcher(),
                      Pattern :: service_name_pattern_suffix()) ->
    non_neg_integer().

subscribe_count(Dispatcher, Pattern)
    when is_pid(Dispatcher), is_list(Pattern) ->
    gen_server:call(Dispatcher, {'subscribe_count', Pattern}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Unsubscribe from a service name pattern.===
%% @end
%%-------------------------------------------------------------------------

-spec unsubscribe(Dispatcher :: dispatcher(),
                  Pattern :: service_name_pattern_suffix()) ->
    ok | error.

unsubscribe(Dispatcher, Pattern)
    when is_pid(Dispatcher), is_list(Pattern) ->
    gen_server:call(Dispatcher, {'unsubscribe', Pattern}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a service destination based on a service name.===
%% @end
%%-------------------------------------------------------------------------

-spec get_pid(Dispatcher :: dispatcher(),
              Name :: service_name()) ->
    {ok, PatternPid :: pattern_pid()} |
    {error, Reason :: error_reason()}.

get_pid(Dispatcher, [NameC | _] = Name)
    when is_pid(Dispatcher), is_integer(NameC) ->
    gen_server:call(Dispatcher, {'get_pid', Name}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a service destination based on a service name.===
%% @end
%%-------------------------------------------------------------------------

-spec get_pid(Dispatcher :: dispatcher(),
              Name :: service_name(),
              Timeout :: timeout_period()) ->
    {ok, PatternPid :: pattern_pid()} |
    {error, Reason :: error_reason()}.

get_pid(Dispatcher, [NameC | _] = Name, undefined)
    when is_pid(Dispatcher), is_integer(NameC) ->
    gen_server:call(Dispatcher, {'get_pid', Name}, infinity);

get_pid(Dispatcher, [NameC | _] = Name, Timeout)
    when is_pid(Dispatcher), is_integer(NameC) ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_GET_PID_MIN,
                                                ?TIMEOUT_GET_PID_MAX),
    gen_server:call(Dispatcher, {'get_pid', Name, TimeoutNew}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get all service destinations based on a service name.===
%% @end
%%-------------------------------------------------------------------------

-spec get_pids(Dispatcher :: dispatcher(),
               Name :: service_name()) ->
    {ok, PatternPids :: nonempty_list(pattern_pid())} |
    {error, Reason :: error_reason()}.

get_pids(Dispatcher, [NameC | _] = Name)
    when is_pid(Dispatcher), is_integer(NameC) ->
    gen_server:call(Dispatcher, {'get_pids', Name}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a service destination based on a service name.===
%% @end
%%-------------------------------------------------------------------------

-spec get_pids(Dispatcher :: dispatcher(),
               Name :: service_name(),
               Timeout :: timeout_period()) ->
    {ok, PatternPids :: nonempty_list(pattern_pid())} |
    {error, Reason :: error_reason()}.

get_pids(Dispatcher, [NameC | _] = Name, undefined)
    when is_pid(Dispatcher), is_integer(NameC) ->
    gen_server:call(Dispatcher, {'get_pids', Name}, infinity);

get_pids(Dispatcher, [NameC | _] = Name, Timeout)
    when is_pid(Dispatcher), is_integer(NameC) ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_GET_PIDS_MIN,
                                                ?TIMEOUT_GET_PIDS_MAX),
    gen_server:call(Dispatcher, {'get_pids', Name, TimeoutNew}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_async(Dispatcher :: dispatcher(),
                 Name :: service_name(),
                 Request :: request()) ->
    {ok, TransId :: trans_id()} |
    {error, Reason :: error_reason()}.

send_async(Dispatcher, [NameC | _] = Name, Request)
    when is_pid(Dispatcher), is_integer(NameC) ->
    gen_server:call(Dispatcher, {'send_async', Name, <<>>, Request,
                                 undefined, undefined}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_async(Dispatcher :: dispatcher(),
                 Name :: service_name(),
                 Request :: request(),
                 Timeout :: timeout_period()) ->
    {ok, TransId :: trans_id()} |
    {error, Reason :: error_reason()}.

send_async(Dispatcher, [NameC | _] = Name, Request, undefined)
    when is_pid(Dispatcher), is_integer(NameC) ->
    gen_server:call(Dispatcher, {'send_async', Name, <<>>, Request,
                                 undefined, undefined}, infinity);

send_async(Dispatcher, [NameC | _] = Name, Request, Timeout)
    when is_pid(Dispatcher), is_integer(NameC) ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_SEND_ASYNC_MIN,
                                                ?TIMEOUT_SEND_ASYNC_MAX),
    gen_server:call(Dispatcher, {'send_async', Name, <<>>, Request,
                                 TimeoutNew, undefined}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_async(Dispatcher :: dispatcher(),
                 Name :: service_name(),
                 Request :: request(),
                 Timeout :: timeout_period(),
                 PatternPid :: pattern_pid() | undefined) ->
    {ok, TransId :: trans_id()} |
    {error, Reason :: error_reason()}.

send_async(Dispatcher, [NameC | _] = Name, Request, undefined, undefined)
    when is_pid(Dispatcher), is_integer(NameC) ->
    gen_server:call(Dispatcher, {'send_async', Name, <<>>, Request,
                                 undefined, undefined}, infinity);

send_async(Dispatcher, [NameC | _] = Name, Request, undefined, PatternPid)
    when is_pid(Dispatcher), is_integer(NameC), is_tuple(PatternPid) ->
    gen_server:call(Dispatcher, {'send_async', Name, <<>>, Request,
                                 undefined, undefined, PatternPid}, infinity);

send_async(Dispatcher, [NameC | _] = Name, Request, Timeout, undefined)
    when is_pid(Dispatcher), is_integer(NameC) ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_SEND_ASYNC_MIN,
                                                ?TIMEOUT_SEND_ASYNC_MAX),
    gen_server:call(Dispatcher, {'send_async', Name, <<>>, Request,
                                 TimeoutNew, undefined}, infinity);

send_async(Dispatcher, [NameC | _] = Name, Request, Timeout, PatternPid)
    when is_pid(Dispatcher), is_integer(NameC), is_tuple(PatternPid) ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_SEND_ASYNC_MIN,
                                                ?TIMEOUT_SEND_ASYNC_MAX),
    gen_server:call(Dispatcher, {'send_async', Name, <<>>, Request,
                                 TimeoutNew, undefined, PatternPid}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_async(Dispatcher :: dispatcher(),
                 Name :: service_name(),
                 RequestInfo :: request_info(),
                 Request :: request(),
                 Timeout :: timeout_period(),
                 Priority :: priority()) ->
    {ok, TransId :: trans_id()} |
    {error, Reason :: error_reason()}.

send_async(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
           undefined, undefined)
    when is_pid(Dispatcher), is_integer(NameC) ->
    gen_server:call(Dispatcher, {'send_async', Name,
                                 RequestInfo, Request,
                                 undefined, undefined}, infinity);

send_async(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
           undefined, Priority)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    gen_server:call(Dispatcher, {'send_async', Name,
                                 RequestInfo, Request,
                                 undefined, Priority}, infinity);

send_async(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
           Timeout, undefined)
    when is_pid(Dispatcher), is_integer(NameC) ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_SEND_ASYNC_MIN,
                                                ?TIMEOUT_SEND_ASYNC_MAX),
    gen_server:call(Dispatcher, {'send_async', Name,
                                 RequestInfo, Request,
                                 TimeoutNew, undefined}, infinity);

send_async(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
           Timeout, Priority)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_SEND_ASYNC_MIN,
                                                ?TIMEOUT_SEND_ASYNC_MAX),
    gen_server:call(Dispatcher, {'send_async', Name,
                                 RequestInfo, Request,
                                 TimeoutNew, Priority}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_async(Dispatcher :: dispatcher(),
                 Name :: service_name(),
                 RequestInfo :: request_info(),
                 Request :: request(),
                 Timeout :: timeout_period(),
                 Priority :: priority(),
                 PatternPid :: pattern_pid() | undefined) ->
    {ok, TransId :: trans_id()} |
    {error, Reason :: error_reason()}.

send_async(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
           undefined, undefined, undefined)
    when is_pid(Dispatcher), is_integer(NameC) ->
    gen_server:call(Dispatcher, {'send_async', Name,
                                 RequestInfo, Request,
                                 undefined, undefined}, infinity);

send_async(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
           undefined, undefined, PatternPid)
    when is_pid(Dispatcher), is_integer(NameC), is_tuple(PatternPid) ->
    gen_server:call(Dispatcher, {'send_async', Name,
                                 RequestInfo, Request,
                                 undefined,
                                 undefined, PatternPid}, infinity);

send_async(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
           undefined, Priority, undefined)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    gen_server:call(Dispatcher, {'send_async', Name,
                                 RequestInfo, Request,
                                 undefined, Priority}, infinity);

send_async(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
           undefined, Priority, PatternPid)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW,
         is_tuple(PatternPid) ->
    gen_server:call(Dispatcher, {'send_async', Name,
                                 RequestInfo, Request,
                                 undefined,
                                 Priority, PatternPid}, infinity);

send_async(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
           Timeout, undefined, undefined)
    when is_pid(Dispatcher), is_integer(NameC) ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_SEND_ASYNC_MIN,
                                                ?TIMEOUT_SEND_ASYNC_MAX),
    gen_server:call(Dispatcher, {'send_async', Name,
                                 RequestInfo, Request,
                                 TimeoutNew, undefined}, infinity);

send_async(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
           Timeout, undefined, PatternPid)
    when is_pid(Dispatcher), is_integer(NameC), is_tuple(PatternPid) ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_SEND_ASYNC_MIN,
                                                ?TIMEOUT_SEND_ASYNC_MAX),
    gen_server:call(Dispatcher, {'send_async', Name,
                                 RequestInfo, Request,
                                 TimeoutNew, undefined, PatternPid}, infinity);

send_async(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
           Timeout, Priority, undefined)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_SEND_ASYNC_MIN,
                                                ?TIMEOUT_SEND_ASYNC_MAX),
    gen_server:call(Dispatcher, {'send_async', Name,
                                 RequestInfo, Request,
                                 TimeoutNew, Priority}, infinity);

send_async(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
           Timeout, Priority, PatternPid)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW,
         is_tuple(PatternPid) ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_SEND_ASYNC_MIN,
                                                ?TIMEOUT_SEND_ASYNC_MAX),
    gen_server:call(Dispatcher, {'send_async', Name,
                                 RequestInfo, Request,
                                 TimeoutNew, Priority, PatternPid}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% The response is sent to the service as an Erlang message which is either:
%% `{return_async_active, Name, Pattern, ResponseInfo, Response, Timeout, TransId}'
%% (or)
%% `{timeout_async_active, TransId}'
%% use `-include_lib("cloudi_core/include/cloudi_service.hrl").' to have:
%% `#return_async_active{}' (or) `#timeout_async_active{}'
%% @end
%%-------------------------------------------------------------------------

-spec send_async_active(Dispatcher :: dispatcher(),
                        Name :: service_name(),
                        Request :: request()) ->
    {ok, TransId :: trans_id()} |
    {error, Reason :: error_reason()}.

send_async_active(Dispatcher, [NameC | _] = Name, Request)
    when is_pid(Dispatcher), is_integer(NameC) ->
    gen_server:call(Dispatcher, {'send_async_active', Name, <<>>, Request,
                                 undefined, undefined}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% The response is sent to the service as an Erlang message which is either:
%% `{return_async_active, Name, Pattern, ResponseInfo, Response, Timeout, TransId}'
%% (or)
%% `{timeout_async_active, TransId}'
%% use `-include_lib("cloudi_core/include/cloudi_service.hrl").' to have:
%% `#return_async_active{}' (or) `#timeout_async_active{}'
%% @end
%%-------------------------------------------------------------------------

-spec send_async_active(Dispatcher :: dispatcher(),
                        Name :: service_name(),
                        Request :: request(),
                        Timeout :: timeout_period()) ->
    {ok, TransId :: trans_id()} |
    {error, Reason :: error_reason()}.

send_async_active(Dispatcher, [NameC | _] = Name, Request, undefined)
    when is_pid(Dispatcher), is_integer(NameC) ->
    gen_server:call(Dispatcher, {'send_async_active', Name, <<>>, Request,
                                 undefined, undefined}, infinity);

send_async_active(Dispatcher, [NameC | _] = Name, Request, Timeout)
    when is_pid(Dispatcher), is_integer(NameC) ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_SEND_ASYNC_MIN,
                                                ?TIMEOUT_SEND_ASYNC_MAX),
    gen_server:call(Dispatcher, {'send_async_active', Name, <<>>, Request,
                                 TimeoutNew, undefined}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% The response is sent to the service as an Erlang message which is either:
%% `{return_async_active, Name, Pattern, ResponseInfo, Response, Timeout, TransId}'
%% (or)
%% `{timeout_async_active, TransId}'
%% use `-include_lib("cloudi_core/include/cloudi_service.hrl").' to have:
%% `#return_async_active{}' (or) `#timeout_async_active{}'
%% @end
%%-------------------------------------------------------------------------

-spec send_async_active(Dispatcher :: dispatcher(),
                        Name :: service_name(),
                        Request :: request(),
                        Timeout :: timeout_period(),
                        PatternPid :: pattern_pid() | undefined) ->
    {ok, TransId :: trans_id()} |
    {error, Reason :: error_reason()}.

send_async_active(Dispatcher, [NameC | _] = Name, Request, undefined, undefined)
    when is_pid(Dispatcher), is_integer(NameC) ->
    gen_server:call(Dispatcher, {'send_async_active', Name, <<>>, Request,
                                 undefined, undefined}, infinity);

send_async_active(Dispatcher, [NameC | _] = Name, Request,
                  undefined, PatternPid)
    when is_pid(Dispatcher), is_integer(NameC), is_tuple(PatternPid) ->
    gen_server:call(Dispatcher, {'send_async_active', Name, <<>>, Request,
                                 undefined, undefined, PatternPid}, infinity);

send_async_active(Dispatcher, [NameC | _] = Name, Request, Timeout, undefined)
    when is_pid(Dispatcher), is_integer(NameC) ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_SEND_ASYNC_MIN,
                                                ?TIMEOUT_SEND_ASYNC_MAX),
    gen_server:call(Dispatcher, {'send_async_active', Name, <<>>, Request,
                                 TimeoutNew, undefined}, infinity);

send_async_active(Dispatcher, [NameC | _] = Name, Request, Timeout, PatternPid)
    when is_pid(Dispatcher), is_integer(NameC), is_tuple(PatternPid) ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_SEND_ASYNC_MIN,
                                                ?TIMEOUT_SEND_ASYNC_MAX),
    gen_server:call(Dispatcher, {'send_async_active', Name, <<>>, Request,
                                 TimeoutNew, undefined, PatternPid}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% The response is sent to the service as an Erlang message which is either:
%% `{return_async_active, Name, Pattern, ResponseInfo, Response, Timeout, TransId}'
%% (or)
%% `{timeout_async_active, TransId}'
%% use `-include_lib("cloudi_core/include/cloudi_service.hrl").' to have:
%% `#return_async_active{}' (or) `#timeout_async_active{}'
%% @end
%%-------------------------------------------------------------------------

-spec send_async_active(Dispatcher :: dispatcher(),
                        Name :: service_name(),
                        RequestInfo :: request_info(),
                        Request :: request(),
                        Timeout :: timeout_period(),
                        Priority :: priority()) ->
    {ok, TransId :: trans_id()} |
    {error, Reason :: error_reason()}.

send_async_active(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
                  undefined, undefined)
    when is_pid(Dispatcher), is_integer(NameC) ->
    gen_server:call(Dispatcher, {'send_async_active', Name,
                                 RequestInfo, Request,
                                 undefined,
                                 undefined}, infinity);

send_async_active(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
                  undefined, Priority)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    gen_server:call(Dispatcher, {'send_async_active', Name,
                                 RequestInfo, Request,
                                 undefined,
                                 Priority}, infinity);

send_async_active(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
                  Timeout, undefined)
    when is_pid(Dispatcher), is_integer(NameC) ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_SEND_ASYNC_MIN,
                                                ?TIMEOUT_SEND_ASYNC_MAX),
    gen_server:call(Dispatcher, {'send_async_active', Name,
                                 RequestInfo, Request,
                                 TimeoutNew, undefined}, infinity);

send_async_active(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
                  Timeout, Priority)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_SEND_ASYNC_MIN,
                                                ?TIMEOUT_SEND_ASYNC_MAX),
    gen_server:call(Dispatcher, {'send_async_active', Name,
                                 RequestInfo, Request,
                                 TimeoutNew, Priority}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% The response is sent to the service as an Erlang message which is either:
%% `{return_async_active, Name, Pattern, ResponseInfo, Response, Timeout, TransId}'
%% (or)
%% `{timeout_async_active, TransId}'
%% use `-include_lib("cloudi_core/include/cloudi_service.hrl").' to have:
%% `#return_async_active{}' (or) `#timeout_async_active{}'
%% @end
%%-------------------------------------------------------------------------

-spec send_async_active(Dispatcher :: dispatcher(),
                        Name :: service_name(),
                        RequestInfo :: request_info(),
                        Request :: request(),
                        Timeout :: timeout_period(),
                        Priority :: priority(),
                        PatternPid :: pattern_pid() | undefined) ->
    {ok, TransId :: trans_id()} |
    {error, Reason :: error_reason()}.

send_async_active(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
                  undefined, undefined, undefined)
    when is_pid(Dispatcher), is_integer(NameC) ->
    gen_server:call(Dispatcher, {'send_async_active', Name,
                                 RequestInfo, Request,
                                 undefined, undefined}, infinity);

send_async_active(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
                  undefined, undefined, PatternPid)
    when is_pid(Dispatcher), is_integer(NameC), is_tuple(PatternPid) ->
    gen_server:call(Dispatcher, {'send_async_active', Name,
                                 RequestInfo, Request,
                                 undefined,
                                 undefined, PatternPid}, infinity);

send_async_active(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
                  undefined, Priority, undefined)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    gen_server:call(Dispatcher, {'send_async_active', Name,
                                 RequestInfo, Request,
                                 undefined, Priority}, infinity);

send_async_active(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
                  undefined, Priority, PatternPid)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW,
         is_tuple(PatternPid) ->
    gen_server:call(Dispatcher, {'send_async_active', Name,
                                 RequestInfo, Request,
                                 undefined,
                                 Priority, PatternPid}, infinity);

send_async_active(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
                  Timeout, undefined, undefined)
    when is_pid(Dispatcher), is_integer(NameC) ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_SEND_ASYNC_MIN,
                                                ?TIMEOUT_SEND_ASYNC_MAX),
    gen_server:call(Dispatcher, {'send_async_active', Name,
                                 RequestInfo, Request,
                                 TimeoutNew, undefined}, infinity);

send_async_active(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
                  Timeout, undefined, PatternPid)
    when is_pid(Dispatcher), is_integer(NameC), is_tuple(PatternPid) ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_SEND_ASYNC_MIN,
                                                ?TIMEOUT_SEND_ASYNC_MAX),
    gen_server:call(Dispatcher, {'send_async_active', Name,
                                 RequestInfo, Request,
                                 TimeoutNew, undefined, PatternPid}, infinity);

send_async_active(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
                  Timeout, Priority, undefined)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_SEND_ASYNC_MIN,
                                                ?TIMEOUT_SEND_ASYNC_MAX),
    gen_server:call(Dispatcher, {'send_async_active', Name,
                                 RequestInfo, Request,
                                 TimeoutNew, Priority}, infinity);

send_async_active(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
                  Timeout, Priority, PatternPid)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW,
         is_tuple(PatternPid) ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_SEND_ASYNC_MIN,
                                                ?TIMEOUT_SEND_ASYNC_MAX),
    gen_server:call(Dispatcher, {'send_async_active', Name,
                                 RequestInfo, Request,
                                 TimeoutNew, Priority, PatternPid}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request with a previously generated transaction id.===
%% Only meant for special transaction handling.
%% The response is sent to the service as an Erlang message which is either:
%% `{return_async_active, Name, Pattern, ResponseInfo, Response, Timeout, TransId}'
%% (or)
%% `{timeout_async_active, TransId}'
%% use `-include_lib("cloudi_core/include/cloudi_service.hrl").' to have:
%% `#return_async_active{}' (or) `#timeout_async_active{}'
%% @end
%%-------------------------------------------------------------------------

-spec send_async_active(Dispatcher :: dispatcher(),
                        Name :: service_name(),
                        RequestInfo :: request_info(),
                        Request :: request(),
                        Timeout :: timeout_period(),
                        Priority :: priority(),
                        TransId :: trans_id(),
                        PatternPid :: pattern_pid()) ->
    {ok, TransId :: trans_id()} |
    {error, Reason :: error_reason()}.

send_async_active(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
                  undefined, undefined, TransId, PatternPid)
    when is_pid(Dispatcher), is_integer(NameC),
         is_binary(TransId), is_tuple(PatternPid) ->
    gen_server:call(Dispatcher, {'send_async_active', Name,
                                 RequestInfo, Request,
                                 undefined, undefined,
                                 TransId, PatternPid}, infinity);

send_async_active(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
                  undefined, Priority, TransId, PatternPid)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW,
         is_binary(TransId), is_tuple(PatternPid) ->
    gen_server:call(Dispatcher, {'send_async_active', Name,
                                 RequestInfo, Request,
                                 undefined, Priority,
                                 TransId, PatternPid}, infinity);

send_async_active(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
                  Timeout, undefined, TransId, PatternPid)
    when is_pid(Dispatcher), is_integer(NameC),
         is_binary(TransId), is_tuple(PatternPid) ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_SEND_ASYNC_MIN,
                                                ?TIMEOUT_SEND_ASYNC_MAX),
    gen_server:call(Dispatcher, {'send_async_active', Name,
                                 RequestInfo, Request,
                                 TimeoutNew, undefined,
                                 TransId, PatternPid}, infinity);

send_async_active(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
                  Timeout, Priority, TransId, PatternPid)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW,
         is_binary(TransId), is_tuple(PatternPid) ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_SEND_ASYNC_MIN,
                                                ?TIMEOUT_SEND_ASYNC_MAX),
    gen_server:call(Dispatcher, {'send_async_active', Name,
                                 RequestInfo, Request,
                                 TimeoutNew, Priority,
                                 TransId, PatternPid}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% An alias for send_async.  The asynchronous service request is returned
%% and handled the same way as within external services.
%% @end
%%-------------------------------------------------------------------------

-spec send_async_passive(Dispatcher :: dispatcher(),
                         Name :: service_name(),
                         Request :: request()) ->
    {ok, TransId :: trans_id()} |
    {error, Reason :: error_reason()}.

send_async_passive(Dispatcher, Name, Request) ->
    send_async(Dispatcher, Name, Request).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% An alias for send_async.  The asynchronous service request is returned
%% and handled the same way as within external services.
%% @end
%%-------------------------------------------------------------------------

-spec send_async_passive(Dispatcher :: dispatcher(),
                         Name :: service_name(),
                         Request :: request(),
                         Timeout :: timeout_period()) ->
    {ok, TransId :: trans_id()} |
    {error, Reason :: error_reason()}.

send_async_passive(Dispatcher, Name, Request, Timeout) ->
    send_async(Dispatcher, Name, Request, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% An alias for send_async.  The asynchronous service request is returned
%% and handled the same way as within external services.
%% @end
%%-------------------------------------------------------------------------

-spec send_async_passive(Dispatcher :: dispatcher(),
                         Name :: service_name(),
                         Request :: request(),
                         Timeout :: timeout_period(),
                         PatternPid :: pattern_pid() | undefined) ->
    {ok, TransId :: trans_id()} |
    {error, Reason :: error_reason()}.

send_async_passive(Dispatcher, Name, Request, Timeout, PatternPid) ->
    send_async(Dispatcher, Name, Request, Timeout, PatternPid).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% An alias for send_async.  The asynchronous service request is returned
%% and handled the same way as within external services.
%% @end
%%-------------------------------------------------------------------------

-spec send_async_passive(Dispatcher :: dispatcher(),
                         Name :: service_name(),
                         RequestInfo :: request_info(),
                         Request :: request(),
                         Timeout :: timeout_period(),
                         Priority :: priority()) ->
    {ok, TransId :: trans_id()} |
    {error, Reason :: error_reason()}.

send_async_passive(Dispatcher, Name, RequestInfo, Request,
                   Timeout, Priority) ->
    send_async(Dispatcher, Name, RequestInfo, Request,
               Timeout, Priority).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% An alias for send_async.  The asynchronous service request is returned
%% and handled the same way as within external services.
%% @end
%%-------------------------------------------------------------------------

-spec send_async_passive(Dispatcher :: dispatcher(),
                         Name :: service_name(),
                         RequestInfo :: request_info(),
                         Request :: request(),
                         Timeout :: timeout_period(),
                         Priority :: priority(),
                         PatternPid :: pattern_pid() | undefined) ->
    {ok, TransId :: trans_id()} |
    {error, Reason :: error_reason()}.

send_async_passive(Dispatcher, Name, RequestInfo, Request,
                   Timeout, Priority, PatternPid) ->
    send_async(Dispatcher, Name, RequestInfo, Request,
               Timeout, Priority, PatternPid).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a synchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_sync(Dispatcher :: dispatcher(),
                Name :: service_name(),
                Request :: request()) ->
    {ok, ResponseInfo :: response_info(), Response :: response()} |
    {ok, Response :: response()} |
    {error, Reason :: error_reason()}.

send_sync(Dispatcher, [NameC | _] = Name, Request)
    when is_pid(Dispatcher), is_integer(NameC) ->
    gen_server:call(Dispatcher, {'send_sync', Name, <<>>, Request,
                                 undefined, undefined}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a synchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_sync(Dispatcher :: dispatcher(),
                Name :: service_name(),
                Request :: request(),
                Timeout :: timeout_period()) ->
    {ok, ResponseInfo :: response_info(), Response :: response()} |
    {ok, Response :: response()} |
    {error, Reason :: error_reason()}.

send_sync(Dispatcher, [NameC | _] = Name, Request, undefined)
    when is_pid(Dispatcher), is_integer(NameC) ->
    gen_server:call(Dispatcher, {'send_sync', Name, <<>>, Request,
                                 undefined, undefined}, infinity);

send_sync(Dispatcher, [NameC | _] = Name, Request, Timeout)
    when is_pid(Dispatcher), is_integer(NameC) ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_SEND_SYNC_MIN,
                                                ?TIMEOUT_SEND_SYNC_MAX),
    gen_server:call(Dispatcher, {'send_sync', Name, <<>>, Request,
                                 TimeoutNew, undefined}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a synchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_sync(Dispatcher :: dispatcher(),
                Name :: service_name(),
                Request :: request(),
                Timeout :: timeout_period(),
                PatternPid :: pattern_pid() | undefined) ->
    {ok, ResponseInfo :: response_info(), Response :: response()} |
    {ok, Response :: response()} |
    {error, Reason :: error_reason()}.

send_sync(Dispatcher, [NameC | _] = Name, Request, undefined, undefined)
    when is_pid(Dispatcher), is_integer(NameC) ->
    gen_server:call(Dispatcher, {'send_sync', Name, <<>>, Request,
                                 undefined, undefined}, infinity);

send_sync(Dispatcher, [NameC | _] = Name, Request, undefined, PatternPid)
    when is_pid(Dispatcher), is_integer(NameC), is_tuple(PatternPid) ->
    gen_server:call(Dispatcher, {'send_sync', Name, <<>>, Request,
                                 undefined, undefined, PatternPid}, infinity);

send_sync(Dispatcher, [NameC | _] = Name, Request, Timeout, undefined)
    when is_pid(Dispatcher), is_integer(NameC) ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_SEND_SYNC_MIN,
                                                ?TIMEOUT_SEND_SYNC_MAX),
    gen_server:call(Dispatcher, {'send_sync', Name, <<>>, Request,
                                 TimeoutNew, undefined}, infinity);

send_sync(Dispatcher, [NameC | _] = Name, Request, Timeout, PatternPid)
    when is_pid(Dispatcher), is_integer(NameC), is_tuple(PatternPid) ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_SEND_SYNC_MIN,
                                                ?TIMEOUT_SEND_SYNC_MAX),
    gen_server:call(Dispatcher, {'send_sync', Name, <<>>, Request,
                                 TimeoutNew, undefined, PatternPid}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a synchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_sync(Dispatcher :: dispatcher(),
                Name :: service_name(),
                RequestInfo :: request_info(),
                Request :: request(),
                Timeout :: timeout_period(),
                Priority :: priority()) ->
    {ok, ResponseInfo :: response_info(), Response :: response()} |
    {ok, Response :: response()} |
    {error, Reason :: error_reason()}.

send_sync(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
          undefined, undefined)
    when is_pid(Dispatcher), is_integer(NameC) ->
    gen_server:call(Dispatcher, {'send_sync', Name,
                                 RequestInfo, Request,
                                 undefined, undefined}, infinity);

send_sync(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
          undefined, Priority)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    gen_server:call(Dispatcher, {'send_sync', Name,
                                 RequestInfo, Request,
                                 undefined, Priority}, infinity);

send_sync(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
          Timeout, undefined)
    when is_pid(Dispatcher), is_integer(NameC) ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_SEND_SYNC_MIN,
                                                ?TIMEOUT_SEND_SYNC_MAX),
    gen_server:call(Dispatcher, {'send_sync', Name,
                                 RequestInfo, Request,
                                 TimeoutNew, undefined}, infinity);

send_sync(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
          Timeout, Priority)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_SEND_SYNC_MIN,
                                                ?TIMEOUT_SEND_SYNC_MAX),
    gen_server:call(Dispatcher, {'send_sync', Name,
                                 RequestInfo, Request,
                                 TimeoutNew, Priority}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a synchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_sync(Dispatcher :: dispatcher(),
                Name :: service_name(),
                RequestInfo :: request_info(),
                Request :: request(),
                Timeout :: timeout_period(),
                Priority :: priority(),
                PatternPid :: pattern_pid() | undefined) ->
    {ok, ResponseInfo :: response_info(), Response :: response()} |
    {ok, Response :: response()} |
    {error, Reason :: error_reason()}.

send_sync(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
          undefined, undefined, undefined)
    when is_pid(Dispatcher), is_integer(NameC) ->
    gen_server:call(Dispatcher, {'send_sync', Name,
                                 RequestInfo, Request,
                                 undefined, undefined}, infinity);

send_sync(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
          undefined, undefined, PatternPid)
    when is_pid(Dispatcher), is_integer(NameC), is_tuple(PatternPid) ->
    gen_server:call(Dispatcher, {'send_sync', Name,
                                 RequestInfo, Request,
                                 undefined,
                                 undefined, PatternPid}, infinity);

send_sync(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
          undefined, Priority, undefined)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    gen_server:call(Dispatcher, {'send_sync', Name,
                                 RequestInfo, Request,
                                 undefined, Priority}, infinity);

send_sync(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
          undefined, Priority, PatternPid)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW,
         is_tuple(PatternPid) ->
    gen_server:call(Dispatcher, {'send_sync', Name,
                                 RequestInfo, Request,
                                 undefined,
                                 Priority, PatternPid}, infinity);

send_sync(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
          Timeout, undefined, undefined)
    when is_pid(Dispatcher), is_integer(NameC) ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_SEND_SYNC_MIN,
                                                ?TIMEOUT_SEND_SYNC_MAX),
    gen_server:call(Dispatcher, {'send_sync', Name,
                                 RequestInfo, Request,
                                 TimeoutNew, undefined}, infinity);

send_sync(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
          Timeout, undefined, PatternPid)
    when is_pid(Dispatcher), is_integer(NameC), is_tuple(PatternPid) ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_SEND_SYNC_MIN,
                                                ?TIMEOUT_SEND_SYNC_MAX),
    gen_server:call(Dispatcher, {'send_sync', Name,
                                 RequestInfo, Request,
                                 TimeoutNew, undefined, PatternPid}, infinity);

send_sync(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
          Timeout, Priority, undefined)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_SEND_SYNC_MIN,
                                                ?TIMEOUT_SEND_SYNC_MAX),
    gen_server:call(Dispatcher, {'send_sync', Name,
                                 RequestInfo, Request,
                                 TimeoutNew, Priority}, infinity);

send_sync(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
          Timeout, Priority, PatternPid)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW,
         is_tuple(PatternPid) ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_SEND_SYNC_MIN,
                                                ?TIMEOUT_SEND_SYNC_MAX),
    gen_server:call(Dispatcher, {'send_sync', Name,
                                 RequestInfo, Request,
                                 TimeoutNew, Priority, PatternPid}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a multicast asynchronous service request.===
%% Asynchronous service requests are sent to all services that have
%% subscribed to the service name pattern that matches the destination.
%% @end
%%-------------------------------------------------------------------------

-spec mcast_async(Dispatcher :: dispatcher(),
                  Name :: service_name(),
                  Request :: request()) ->
    {ok, TransIdList :: list(trans_id())} |
    {error, Reason :: error_reason()}.

mcast_async(Dispatcher, [NameC | _] = Name, Request)
    when is_pid(Dispatcher), is_integer(NameC) ->
    gen_server:call(Dispatcher, {'mcast_async', Name, <<>>, Request,
                                 undefined, undefined}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a multicast asynchronous service request.===
%% Asynchronous service requests are sent to all services that have
%% subscribed to the service name pattern that matches the destination.
%% @end
%%-------------------------------------------------------------------------

-spec mcast_async(Dispatcher :: dispatcher(),
                  Name :: service_name(),
                  Request :: request(),
                  Timeout :: timeout_period()) ->
    {ok, TransIdList :: list(trans_id())} |
    {error, Reason :: error_reason()}.

mcast_async(Dispatcher, [NameC | _] = Name, Request, undefined)
    when is_pid(Dispatcher), is_integer(NameC) ->
    gen_server:call(Dispatcher, {'mcast_async', Name, <<>>, Request,
                                 undefined, undefined}, infinity);

mcast_async(Dispatcher, [NameC | _] = Name, Request, Timeout)
    when is_pid(Dispatcher), is_integer(NameC) ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_MCAST_ASYNC_MIN,
                                                ?TIMEOUT_MCAST_ASYNC_MAX),
    gen_server:call(Dispatcher, {'mcast_async', Name, <<>>, Request,
                                 TimeoutNew, undefined}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a multicast asynchronous service request.===
%% Asynchronous service requests are sent to all services that have
%% subscribed to the service name pattern that matches the destination.
%% @end
%%-------------------------------------------------------------------------

-spec mcast_async(Dispatcher :: dispatcher(),
                  Name :: service_name(),
                  RequestInfo :: request_info(),
                  Request :: request(),
                  Timeout :: timeout_period(),
                  Priority :: priority()) ->
    {ok, TransIdList :: list(trans_id())} |
    {error, Reason :: error_reason()}.

mcast_async(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
            undefined, undefined)
    when is_pid(Dispatcher), is_integer(NameC) ->
    gen_server:call(Dispatcher, {'mcast_async', Name,
                                 RequestInfo, Request,
                                 undefined,
                                 undefined}, infinity);

mcast_async(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
            undefined, Priority)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    gen_server:call(Dispatcher, {'mcast_async', Name,
                                 RequestInfo, Request,
                                 undefined,
                                 Priority}, infinity);

mcast_async(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
            Timeout, undefined)
    when is_pid(Dispatcher), is_integer(NameC) ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_MCAST_ASYNC_MIN,
                                                ?TIMEOUT_MCAST_ASYNC_MAX),
    gen_server:call(Dispatcher, {'mcast_async', Name,
                                 RequestInfo, Request,
                                 TimeoutNew, undefined}, infinity);

mcast_async(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
            Timeout, Priority)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_MCAST_ASYNC_MIN,
                                                ?TIMEOUT_MCAST_ASYNC_MAX),
    gen_server:call(Dispatcher, {'mcast_async', Name,
                                 RequestInfo, Request,
                                 TimeoutNew, Priority}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a multicast asynchronous service request.===
%% Asynchronous service requests are sent to all services that have
%% subscribed to the service name pattern that matches the destination.
%% The responses are sent to the service as Erlang messages that are either:
%% `{return_async_active, Name, Pattern, ResponseInfo, Response, Timeout, TransId}'
%% (or)
%% `{timeout_async_active, TransId}'
%% use `-include_lib("cloudi_core/include/cloudi_service.hrl").' to have:
%% `#return_async_active{}' (or) `#timeout_async_active{}'
%% @end
%%-------------------------------------------------------------------------

-spec mcast_async_active(Dispatcher :: dispatcher(),
                         Name :: service_name(),
                         Request :: request()) ->
    {ok, TransIdList :: list(trans_id())} |
    {error, Reason :: error_reason()}.

mcast_async_active(Dispatcher, [NameC | _] = Name, Request)
    when is_pid(Dispatcher), is_integer(NameC) ->
    gen_server:call(Dispatcher, {'mcast_async_active', Name, <<>>, Request,
                                 undefined, undefined}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a multicast asynchronous service request.===
%% Asynchronous service requests are sent to all services that have
%% subscribed to the service name pattern that matches the destination.
%% The responses are sent to the service as Erlang messages that are either:
%% `{return_async_active, Name, Pattern, ResponseInfo, Response, Timeout, TransId}'
%% (or)
%% `{timeout_async_active, TransId}'
%% use `-include_lib("cloudi_core/include/cloudi_service.hrl").' to have:
%% `#return_async_active{}' (or) `#timeout_async_active{}'
%% @end
%%-------------------------------------------------------------------------

-spec mcast_async_active(Dispatcher :: dispatcher(),
                         Name :: service_name(),
                         Request :: request(),
                         Timeout :: timeout_period()) ->
    {ok, TransIdList :: list(trans_id())} |
    {error, Reason :: error_reason()}.

mcast_async_active(Dispatcher, [NameC | _] = Name, Request, undefined)
    when is_pid(Dispatcher), is_integer(NameC) ->
    gen_server:call(Dispatcher, {'mcast_async_active', Name, <<>>, Request,
                                 undefined, undefined}, infinity);

mcast_async_active(Dispatcher, [NameC | _] = Name, Request, Timeout)
    when is_pid(Dispatcher), is_integer(NameC) ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_MCAST_ASYNC_MIN,
                                                ?TIMEOUT_MCAST_ASYNC_MAX),
    gen_server:call(Dispatcher, {'mcast_async_active', Name, <<>>, Request,
                                 TimeoutNew, undefined}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a multicast asynchronous service request.===
%% Asynchronous service requests are sent to all services that have
%% subscribed to the service name pattern that matches the destination.
%% The responses are sent to the service as Erlang messages that are either:
%% `{return_async_active, Name, Pattern, ResponseInfo, Response, Timeout, TransId}'
%% (or)
%% `{timeout_async_active, TransId}'
%% use `-include_lib("cloudi_core/include/cloudi_service.hrl").' to have:
%% `#return_async_active{}' (or) `#timeout_async_active{}'
%% @end
%%-------------------------------------------------------------------------

-spec mcast_async_active(Dispatcher :: dispatcher(),
                         Name :: service_name(),
                         RequestInfo :: request_info(),
                         Request :: request(),
                         Timeout :: timeout_period(),
                         Priority :: priority()) ->
    {ok, TransIdList :: list(trans_id())} |
    {error, Reason :: error_reason()}.

mcast_async_active(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
                   undefined, undefined)
    when is_pid(Dispatcher), is_integer(NameC) ->
    gen_server:call(Dispatcher, {'mcast_async_active', Name,
                                 RequestInfo, Request,
                                 undefined,
                                 undefined}, infinity);

mcast_async_active(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
                   undefined, Priority)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    gen_server:call(Dispatcher, {'mcast_async_active', Name,
                                 RequestInfo, Request,
                                 undefined,
                                 Priority}, infinity);

mcast_async_active(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
                   Timeout, undefined)
    when is_pid(Dispatcher), is_integer(NameC) ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_MCAST_ASYNC_MIN,
                                                ?TIMEOUT_MCAST_ASYNC_MAX),
    gen_server:call(Dispatcher, {'mcast_async_active', Name,
                                 RequestInfo, Request,
                                 TimeoutNew, undefined}, infinity);

mcast_async_active(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
                   Timeout, Priority)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_MCAST_ASYNC_MIN,
                                                ?TIMEOUT_MCAST_ASYNC_MAX),
    gen_server:call(Dispatcher, {'mcast_async_active', Name,
                                 RequestInfo, Request,
                                 TimeoutNew, Priority}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a multicast asynchronous service request.===
%% An alias for mcast_async.  The asynchronous service requests are returned
%% and handled the same way as within external services.
%% @end
%%-------------------------------------------------------------------------

-spec mcast_async_passive(Dispatcher :: dispatcher(),
                          Name :: service_name(),
                          Request :: request()) ->
    {ok, TransIdList :: list(trans_id())} |
    {error, Reason :: error_reason()}.

mcast_async_passive(Dispatcher, Name, Request) ->
    mcast_async(Dispatcher, Name, Request).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a multicast asynchronous service request.===
%% An alias for mcast_async.  The asynchronous service requests are returned
%% and handled the same way as within external services.
%% @end
%%-------------------------------------------------------------------------

-spec mcast_async_passive(Dispatcher :: dispatcher(),
                          Name :: service_name(),
                          Request :: request(),
                          Timeout :: timeout_period()) ->
    {ok, TransIdList :: list(trans_id())} |
    {error, Reason :: error_reason()}.

mcast_async_passive(Dispatcher, Name, Request, Timeout) ->
    mcast_async(Dispatcher, Name, Request, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a multicast asynchronous service request.===
%% An alias for mcast_async.  The asynchronous service requests are returned
%% and handled the same way as within external services.
%% @end
%%-------------------------------------------------------------------------

-spec mcast_async_passive(Dispatcher :: dispatcher(),
                          Name :: service_name(),
                          RequestInfo :: request_info(),
                          Request :: request(),
                          Timeout :: timeout_period(),
                          Priority :: priority()) ->
    {ok, TransIdList :: list(trans_id())} |
    {error, Reason :: error_reason()}.

mcast_async_passive(Dispatcher, Name, RequestInfo, Request,
                    Timeout, Priority) ->
    mcast_async(Dispatcher, Name, RequestInfo, Request, Timeout, Priority).

%%-------------------------------------------------------------------------
%% @doc
%% ===Forward a service request.===
%% @end
%%-------------------------------------------------------------------------

-spec forward(Dispatcher :: dispatcher(),
              RequestType :: request_type(),
              Name :: service_name(),
              RequestInfo :: request_info(),
              Request :: request(),
              Timeout :: timeout_value_milliseconds(),
              Priority :: priority(),
              TransId :: trans_id(),
              Source :: source()) ->
    no_return().

forward(Dispatcher, 'send_async', Name, RequestInfo, Request,
        Timeout, Priority, TransId, Source) ->
    forward_async(Dispatcher, Name, RequestInfo, Request,
                  Timeout, Priority, TransId, Source);

forward(Dispatcher, 'send_sync', Name, RequestInfo, Request,
        Timeout, Priority, TransId, Source) ->
    forward_sync(Dispatcher, Name, RequestInfo, Request,
                 Timeout, Priority, TransId, Source).

%%-------------------------------------------------------------------------
%% @doc
%% ===Forward an asynchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec forward_async(Dispatcher :: dispatcher(),
                    Name :: service_name(),
                    RequestInfo :: request_info(),
                    Request :: request(),
                    Timeout :: timeout_value_milliseconds(),
                    Priority :: priority(),
                    TransId :: trans_id(),
                    Source :: source()) ->
    no_return().

forward_async(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
              Timeout, Priority, TransId, Source)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         is_binary(TransId), is_pid(Source),
         Timeout >= ?TIMEOUT_FORWARD_ASYNC_MIN,
         Timeout =< ?TIMEOUT_FORWARD_ASYNC_MAX, is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    erlang:throw({cloudi_service_forward,
                  {'cloudi_service_forward_async_retry', Name,
                   RequestInfo, Request,
                   Timeout, Priority, TransId, Source}}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Forward a synchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec forward_sync(Dispatcher :: dispatcher(),
                   Name :: service_name(),
                   RequestInfo :: request_info(),
                   Request :: request(),
                   Timeout :: timeout_value_milliseconds(),
                   Priority :: priority(),
                   TransId :: trans_id(),
                   Source :: source()) ->
    no_return().

forward_sync(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
             Timeout, Priority, TransId, Source)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         is_binary(TransId), is_pid(Source),
         Timeout >= ?TIMEOUT_FORWARD_SYNC_MIN,
         Timeout =< ?TIMEOUT_FORWARD_SYNC_MAX, is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    erlang:throw({cloudi_service_forward,
                  {'cloudi_service_forward_sync_retry', Name,
                   RequestInfo, Request,
                   Timeout, Priority, TransId, Source}}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return a service response.===
%% @end
%%-------------------------------------------------------------------------

-spec return(Dispatcher :: dispatcher(),
             Response :: response()) ->
    no_return().

return(Dispatcher, Response)
    when is_pid(Dispatcher) ->
    erlang:throw({cloudi_service_return, {Response}}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return a service response.===
%% @end
%%-------------------------------------------------------------------------

-spec return(Dispatcher :: dispatcher(),
             ResponseInfo :: response_info(),
             Response :: response()) ->
    no_return().

return(Dispatcher, ResponseInfo, Response)
    when is_pid(Dispatcher) ->
    erlang:throw({cloudi_service_return, {ResponseInfo, Response}}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return a service response.===
%% @end
%%-------------------------------------------------------------------------

-spec return(Dispatcher :: dispatcher(),
             RequestType :: request_type(),
             Name :: service_name(),
             Pattern :: service_name_pattern(),
             ResponseInfo :: response_info(),
             Response :: response(),
             Timeout :: timeout_value_milliseconds(),
             TransId :: trans_id(),
             Source :: source()) ->
    no_return().

return(Dispatcher, 'send_async', Name, Pattern, ResponseInfo, Response,
       Timeout, TransId, Source) ->
    return_async(Dispatcher, Name, Pattern, ResponseInfo, Response,
                 Timeout, TransId, Source);

return(Dispatcher, 'send_sync', Name, Pattern, ResponseInfo, Response,
       Timeout, TransId, Source) ->
    return_sync(Dispatcher, Name, Pattern, ResponseInfo, Response,
                Timeout, TransId, Source).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return an asynchronous service response.===
%% @end
%%-------------------------------------------------------------------------

-spec return_async(Dispatcher :: dispatcher(),
                   Name :: service_name(),
                   Pattern :: service_name_pattern(),
                   ResponseInfo :: response_info(),
                   Response :: response(),
                   Timeout :: timeout_value_milliseconds(),
                   TransId :: trans_id(),
                   Source :: source()) ->
    no_return().

return_async(Dispatcher, [NameC | _] = Name, [PatternC | _] = Pattern,
             ResponseInfo, Response, Timeout, TransId, Source)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(PatternC),
         is_integer(Timeout), is_binary(TransId), is_pid(Source),
         Timeout >= ?TIMEOUT_RETURN_ASYNC_MIN,
         Timeout =< ?TIMEOUT_RETURN_ASYNC_MAX ->
    erlang:throw({cloudi_service_return,
                  {'cloudi_service_return_async', Name, Pattern,
                   ResponseInfo, Response,
                   Timeout, TransId, Source}}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return a synchronous service response.===
%% @end
%%-------------------------------------------------------------------------

-spec return_sync(Dispatcher :: dispatcher(),
                  Name :: service_name(),
                  Pattern :: service_name_pattern(),
                  ResponseInfo :: response_info(),
                  Response :: response(),
                  Timeout :: timeout_value_milliseconds(),
                  TransId :: trans_id(),
                  Source :: source()) ->
    no_return().

return_sync(Dispatcher, [NameC | _] = Name, [PatternC | _] = Pattern,
            ResponseInfo, Response, Timeout, TransId, Source)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(PatternC),
         is_integer(Timeout), is_binary(TransId), is_pid(Source),
         Timeout >= ?TIMEOUT_RETURN_SYNC_MIN,
         Timeout =< ?TIMEOUT_RETURN_SYNC_MAX ->
    erlang:throw({cloudi_service_return,
                  {'cloudi_service_return_sync', Name, Pattern,
                   ResponseInfo, Response,
                   Timeout, TransId, Source}}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return a service response without exiting the request handler.===
%% Should rarely, if ever, be used.  If the service has the option
%% request_timeout_adjustment == true, the adjustment will not occur when
%% this function is used.  Also, the service's
%% response_timeout_immediate_max option will not prevent a null response
%% from being sent when this function is used.
%% @end
%%-------------------------------------------------------------------------

-spec return_nothrow(Dispatcher :: dispatcher(),
                     RequestType :: request_type(),
                     Name :: service_name(),
                     Pattern :: service_name_pattern(),
                     ResponseInfo :: response_info(),
                     Response :: response(),
                     Timeout :: timeout_value_milliseconds(),
                     TransId :: trans_id(),
                     Source :: source()) ->
    ok.

return_nothrow(Dispatcher, 'send_async',
               [NameC | _] = Name, [PatternC | _] = Pattern,
               ResponseInfo, Response, Timeout, TransId, Source)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(PatternC),
         is_integer(Timeout), is_binary(TransId), is_pid(Source),
         Timeout >= ?TIMEOUT_RETURN_ASYNC_MIN,
         Timeout =< ?TIMEOUT_RETURN_ASYNC_MAX ->
    Source ! {'cloudi_service_return_async', Name, Pattern,
              ResponseInfo, Response,
              Timeout, TransId, Source},
    ok;

return_nothrow(Dispatcher, 'send_sync',
               [NameC | _] = Name, [PatternC | _] = Pattern,
               ResponseInfo, Response, Timeout, TransId, Source)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(PatternC),
         is_integer(Timeout), is_binary(TransId), is_pid(Source),
         Timeout >= ?TIMEOUT_RETURN_SYNC_MIN,
         Timeout =< ?TIMEOUT_RETURN_SYNC_MAX ->
    Source ! {'cloudi_service_return_sync', Name, Pattern,
              ResponseInfo, Response,
              Timeout, TransId, Source},
    ok.

%%-------------------------------------------------------------------------
%% @doc
%% ===Receive an asynchronous service request.===
%% Use a null TransId to receive the oldest service request.
%% @end
%%-------------------------------------------------------------------------

-spec recv_async(Dispatcher :: dispatcher()) ->
    {ok, ResponseInfo :: response_info(), Response :: response(),
     TransId :: trans_id()} |
    {error, Reason :: error_reason()}.

recv_async(Dispatcher)
    when is_pid(Dispatcher) ->
    gen_server:call(Dispatcher,
                    {'recv_async', <<0:128>>, true}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Receive an asynchronous service request.===
%% Either use the supplied TransId to receive the specific service request
%% or use a null TransId to receive the oldest service request.
%% @end
%%-------------------------------------------------------------------------

-spec recv_async(Dispatcher :: dispatcher(),
                 timeout_period() | trans_id()) ->
    {ok, ResponseInfo :: response_info(), Response :: response(),
     TransId :: trans_id()} |
    {error, Reason :: error_reason()}.

recv_async(Dispatcher, TransId)
    when is_pid(Dispatcher), is_binary(TransId) ->
    gen_server:call(Dispatcher,
                    {'recv_async', TransId, true}, infinity);

recv_async(Dispatcher, undefined)
    when is_pid(Dispatcher) ->
    gen_server:call(Dispatcher,
                    {'recv_async', <<0:128>>, true}, infinity);

recv_async(Dispatcher, Timeout)
    when is_pid(Dispatcher) ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_RECV_ASYNC_MIN,
                                                ?TIMEOUT_RECV_ASYNC_MAX),
    gen_server:call(Dispatcher,
                    {'recv_async', TimeoutNew, <<0:128>>, true}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Receive an asynchronous service request.===
%% Either use the supplied TransId to receive the specific service request
%% or use a null TransId to receive the oldest service request.
%% @end
%%-------------------------------------------------------------------------

-spec recv_async(Dispatcher :: dispatcher(),
                 timeout_period() | trans_id(),
                 trans_id() | boolean()) ->
    {ok, ResponseInfo :: response_info(), Response :: response(),
     TransId :: trans_id()} |
    {error, Reason :: error_reason()}.

recv_async(Dispatcher, undefined, TransId)
    when is_pid(Dispatcher), is_binary(TransId) ->
    gen_server:call(Dispatcher,
                    {'recv_async', TransId, true}, infinity);

recv_async(Dispatcher, TransId, Consume)
    when is_pid(Dispatcher), is_binary(TransId), is_boolean(Consume) ->
    gen_server:call(Dispatcher,
                    {'recv_async', TransId, Consume}, infinity);

recv_async(Dispatcher, Timeout, TransId)
    when is_pid(Dispatcher), is_binary(TransId) ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_RECV_ASYNC_MIN,
                                                ?TIMEOUT_RECV_ASYNC_MAX),
    gen_server:call(Dispatcher,
                    {'recv_async', TimeoutNew, TransId, true}, infinity);

recv_async(Dispatcher, Timeout, Consume)
    when is_pid(Dispatcher), is_boolean(Consume) ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_RECV_ASYNC_MIN,
                                                ?TIMEOUT_RECV_ASYNC_MAX),
    gen_server:call(Dispatcher,
                    {'recv_async', TimeoutNew, <<0:128>>, Consume}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Receive an asynchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec recv_async(Dispatcher :: dispatcher(),
                 Timeout :: timeout_period(),
                 TransId :: trans_id(),
                 Consume :: boolean()) ->
    {ok, ResponseInfo :: response_info(), Response :: response(),
     TransId :: trans_id()} |
    {error, Reason :: error_reason()}.

recv_async(Dispatcher, undefined, TransId, Consume)
    when is_pid(Dispatcher), is_binary(TransId), is_boolean(Consume) ->
    gen_server:call(Dispatcher,
                    {'recv_async', TransId, Consume}, infinity);

recv_async(Dispatcher, Timeout, TransId, Consume)
    when is_pid(Dispatcher), is_binary(TransId), is_boolean(Consume) ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_RECV_ASYNC_MIN,
                                                ?TIMEOUT_RECV_ASYNC_MAX),
    gen_server:call(Dispatcher,
                    {'recv_async', TimeoutNew, TransId, Consume}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Receive asynchronous service requests.===
%% @end
%%-------------------------------------------------------------------------

-spec recv_asyncs(Dispatcher :: dispatcher(),
                  TransIdList :: list(trans_id())) ->
    {ok, list({ResponseInfo :: response_info(),
               Response :: response(), TransId :: trans_id()})} |
    {error, Reason :: error_reason()}.

recv_asyncs(Dispatcher, [_ | _] = TransIdList)
    when is_pid(Dispatcher), is_list(TransIdList) ->
    gen_server:call(Dispatcher,
                    {'recv_asyncs',
                     [{<<>>, <<>>, TransId} ||
                      TransId <- TransIdList], true}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Receive asynchronous service requests.===
%% @end
%%-------------------------------------------------------------------------

-spec recv_asyncs(Dispatcher :: dispatcher(),
                  Timeout :: timeout_period(),
                  TransIdList :: list(trans_id())) ->
    {ok, list({ResponseInfo :: response_info(), Response :: response(),
               TransId :: trans_id()})} |
    {error, Reason :: error_reason()}.

recv_asyncs(Dispatcher, undefined, [_ | _] = TransIdList)
    when is_pid(Dispatcher), is_list(TransIdList) ->
    gen_server:call(Dispatcher,
                    {'recv_asyncs',
                     [{<<>>, <<>>, TransId} ||
                      TransId <- TransIdList], true}, infinity);

recv_asyncs(Dispatcher, Timeout, [_ | _] = TransIdList)
    when is_pid(Dispatcher), is_list(TransIdList) ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_RECV_ASYNCS_MIN,
                                                ?TIMEOUT_RECV_ASYNCS_MAX),
    gen_server:call(Dispatcher,
                    {'recv_asyncs', TimeoutNew,
                     [{<<>>, <<>>, TransId} ||
                      TransId <- TransIdList], true}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Receive asynchronous service requests.===
%% @end
%%-------------------------------------------------------------------------

-spec recv_asyncs(Dispatcher :: dispatcher(),
                  Timeout :: timeout_period(),
                  TransIdList :: list(trans_id()),
                  Consume :: boolean()) ->
    {ok, list({ResponseInfo :: response_info(), Response :: response(),
               TransId :: trans_id()})} |
    {error, Reason :: error_reason()}.

recv_asyncs(Dispatcher, Timeout, [_ | _] = TransIdList, Consume)
    when is_pid(Dispatcher), is_list(TransIdList), is_boolean(Consume) ->
    TimeoutNew = timeout_period_to_milliseconds(Timeout,
                                                ?TIMEOUT_RECV_ASYNCS_MIN,
                                                ?TIMEOUT_RECV_ASYNCS_MAX),
    gen_server:call(Dispatcher,
                    {'recv_asyncs', TimeoutNew,
                     [{<<>>, <<>>, TransId} ||
                      TransId <- TransIdList], Consume}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Configured service default prefix.===
%% All subscribed/unsubscribed service names use this prefix.  The prefix
%% defines the scope of the service.
%% @end
%%-------------------------------------------------------------------------

-spec prefix(Dispatcher :: dispatcher()) ->
    Prefix :: service_name_pattern().

prefix(Dispatcher) ->
    gen_server:call(Dispatcher, prefix, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Service request suffix from a service name or service name pattern.===
%% @end
%%-------------------------------------------------------------------------

-spec suffix(Dispatcher :: dispatcher(),
             NameOrPattern :: service_name() | service_name_pattern()) ->
    Suffix :: service_name() | service_name_pattern().

suffix(Dispatcher, NameOrPattern) ->
    cloudi_service_name:suffix(prefix(Dispatcher), NameOrPattern).

%%-------------------------------------------------------------------------
%% @doc
%% ===Configured service default asynchronous timeout (in milliseconds).===
%% @end
%%-------------------------------------------------------------------------

-spec timeout_async(Dispatcher :: dispatcher()) ->
    TimeoutAsync :: cloudi_service_api:timeout_send_async_value_milliseconds().

timeout_async(Dispatcher) ->
    gen_server:call(Dispatcher, timeout_async, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Configured service default synchronous timeout (in milliseconds).===
%% @end
%%-------------------------------------------------------------------------

-spec timeout_sync(Dispatcher :: dispatcher()) ->
    TimeoutSync :: cloudi_service_api:timeout_send_sync_value_milliseconds().

timeout_sync(Dispatcher) ->
    gen_server:call(Dispatcher, timeout_sync, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Maximum possible service request timeout (in milliseconds).===
%% Use ?TIMEOUT_MAX_ERLANG directly from cloudi_constants.hrl instead,
%% if possible.
%% @end
%%-------------------------------------------------------------------------

-spec timeout_max(Dispatcher :: dispatcher()) ->
    ?TIMEOUT_MAX_ERLANG.

timeout_max(Dispatcher)
    when is_pid(Dispatcher) ->
    ?TIMEOUT_MAX_ERLANG.

%%-------------------------------------------------------------------------
%% @doc
%% ===Configured service default priority.===
%% @end
%%-------------------------------------------------------------------------

-spec priority_default(Dispatcher :: dispatcher()) ->
    PriorityDefault :: cloudi_service_api:priority().

priority_default(Dispatcher) ->
    gen_server:call(Dispatcher, priority_default, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Configured service destination refresh is immediate.===
%% @end
%%-------------------------------------------------------------------------

-spec destination_refresh_immediate(Dispatcher :: dispatcher()) ->
    boolean().

destination_refresh_immediate(Dispatcher) ->
    gen_server:call(Dispatcher, destination_refresh_immediate, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Configured service destination refresh is lazy.===
%% @end
%%-------------------------------------------------------------------------

-spec destination_refresh_lazy(Dispatcher :: dispatcher()) ->
    boolean().

destination_refresh_lazy(Dispatcher) ->
    gen_server:call(Dispatcher, destination_refresh_lazy, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Determine if duo_mode is enabled.===
%% @end
%%-------------------------------------------------------------------------

-spec duo_mode(Dispatcher :: dispatcher()) ->
    boolean().

duo_mode(Dispatcher) ->
    gen_server:call(Dispatcher, duo_mode, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a list of all service name patterns a service request source is subscribed to.===
%% The source pid can be found at:
%%
%% `cloudi_service_handle_request(_, _, _, _, _, _, _, _, Source, _, _)'
%% @end
%%-------------------------------------------------------------------------

-spec source_subscriptions(Dispatcher :: dispatcher(),
                           Source :: source()) ->
    list(service_name_pattern()).

source_subscriptions(Dispatcher, Source)
    when is_pid(Source) ->
    gen_server:call(Dispatcher, {source_subscriptions, Source}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the context options from the service's configuration.===
%% A service would only use this when delaying the creation of a context
%% for child processes.
%% @end
%%-------------------------------------------------------------------------

-spec context_options(Dispatcher :: dispatcher()) ->
    cloudi:options().

context_options(Dispatcher) ->
    gen_server:call(Dispatcher, context_options, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return a new transaction id.===
%% The same data as used when sending service requests is used.
%% @end
%%-------------------------------------------------------------------------

-spec trans_id(Dispatcher :: dispatcher()) ->
    trans_id().

trans_id(Dispatcher) ->
    gen_server:call(Dispatcher, trans_id, infinity).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

