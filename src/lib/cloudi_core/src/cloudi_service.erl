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
%%%    ==> {ok, State}
%%%        {stop, Reason}
%%%        {stop, Reason, State}
%%%               State = undefined, if not returned
%%%               Reason = restart | shutdown | Term, terminate(State) is called
%%%
%%%   cloudi_service_handle_request(RequestType, Name, Pattern,
%%%                                 RequestInfo, Request, Timeout, Priority,
%%%                                 TransId, Pid, State, Dispatcher)
%%%    ==> {reply, Response, NewState}
%%%        {reply, ResponseInfo, Response, NewState}
%%%        {forward, NextName, NextRequestInfo, NextRequest, NewState}
%%%        {forward, NextName, NextRequestInfo, NextRequest,
%%%         NextTimeout, NextPriority, NewState}
%%%        {noreply, NewState}
%%%        {stop, Reason, NewState}  
%%%               Reason = restart | shutdown | Term, terminate(State) is called
%%%
%%%   cloudi_service_handle_info(Request, State, Dispatcher)
%%%
%%%    ==> {noreply, State}
%%%        {stop, Reason, NewState} 
%%%               Reason = restart | shutdown | Term, terminate(State) is called
%%%
%%%   cloudi_service_terminate(Reason, Timeout,
%%%                            State) Let the user module clean up
%%%        always called when the service terminates
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
%%% Copyright (c) 2011-2020 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2011-2020 Michael Truog
%%% @version 2.0.1 {@date} {@time}
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
              priority_value/0,
              priority/0,
              trans_id/0,
              pattern_pid/0]).

-type dispatcher() :: pid().
-type source() :: pid().
-export_type([dispatcher/0,
              source/0]).

-type error_reason() :: timeout.
-export_type([error_reason/0]).

% for cloudi_service_api:aspect_request_after_internal() and
% cloudi_service_api:aspect_request_after_external() and
-type request_result() ::
    {reply, ResponseInfo :: response_info(), Response :: response()} |
    {forward, NextName :: service_name(),
     NextRequestInfo :: request_info(), NextRequest :: request(),
     NextTimeout :: timeout_value_milliseconds(),
     NextPriority :: priority_value()} |
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
    {reply, Response :: response(), NewState :: any()} |
    {reply, ResponseInfo :: response_info(), Response :: response(),
     NewState :: any()} |
    {forward, NextName :: service_name(),
     NextRequestInfo :: request_info(), NextRequest :: request(),
     NewState :: any()} |
    {forward, NextName :: service_name(),
     NextRequestInfo :: request_info(), NextRequest :: request(),
     NextTimeout :: timeout_value_milliseconds(),
     NextPriority :: priority_value(), NewState :: any()} |
    {noreply, NewState :: any()} |
    {stop, Reason :: any(), NewState :: any()}.

-callback cloudi_service_handle_info(Request :: any(),
                                     State :: any(),
                                     Dispatcher :: dispatcher()) ->
    {noreply, NewState :: any()} |
    {stop, Reason :: any(), NewState :: any()}.

-callback cloudi_service_terminate(Reason :: any(),
                                   Timeout ::
                                       cloudi_service_api:
                                       timeout_terminate_value_milliseconds(),
                                   State :: any()) ->
    ok.

-optional_callbacks([cloudi_service_handle_request/11,
                     cloudi_service_handle_info/3]).

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
    NewDispatcher :: pid().

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
              Timeout :: timeout_milliseconds()) ->
    {ok, PatternPid :: pattern_pid()} |
    {error, Reason :: error_reason()}.

get_pid(Dispatcher, [NameC | _] = Name, undefined)
    when is_pid(Dispatcher), is_integer(NameC) ->
    gen_server:call(Dispatcher, {'get_pid', Name}, infinity);

get_pid(Dispatcher, Name, immediate) ->
    get_pid(Dispatcher, Name, limit_min);

get_pid(Dispatcher, Name, limit_min) ->
    get_pid(Dispatcher, Name, ?TIMEOUT_GET_PID_MIN);

get_pid(Dispatcher, Name, limit_max) ->
    get_pid(Dispatcher, Name, ?TIMEOUT_GET_PID_MAX);

get_pid(Dispatcher, [NameC | _] = Name, Timeout)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         Timeout >= 0, Timeout =< ?TIMEOUT_GET_PID_MAX ->
    gen_server:call(Dispatcher, {'get_pid', Name, Timeout}, infinity).

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
               Timeout :: timeout_milliseconds()) ->
    {ok, PatternPids :: nonempty_list(pattern_pid())} |
    {error, Reason :: error_reason()}.

get_pids(Dispatcher, [NameC | _] = Name, undefined)
    when is_pid(Dispatcher), is_integer(NameC) ->
    gen_server:call(Dispatcher, {'get_pids', Name}, infinity);

get_pids(Dispatcher, Name, immediate) ->
    get_pid(Dispatcher, Name, limit_min);

get_pids(Dispatcher, Name, limit_min) ->
    get_pid(Dispatcher, Name, ?TIMEOUT_GET_PIDS_MIN);

get_pids(Dispatcher, Name, limit_max) ->
    get_pid(Dispatcher, Name, ?TIMEOUT_GET_PIDS_MAX);

get_pids(Dispatcher, [NameC | _] = Name, Timeout)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         Timeout >= 0, Timeout =< ?TIMEOUT_GET_PIDS_MAX ->
    gen_server:call(Dispatcher, {'get_pids', Name, Timeout}, infinity).

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
                 Timeout :: timeout_milliseconds()) ->
    {ok, TransId :: trans_id()} |
    {error, Reason :: error_reason()}.

send_async(Dispatcher, [NameC | _] = Name, Request, undefined)
    when is_pid(Dispatcher), is_integer(NameC) ->
    gen_server:call(Dispatcher, {'send_async', Name, <<>>, Request,
                                 undefined, undefined}, infinity);

send_async(Dispatcher, Name, Request, immediate) ->
    send_async(Dispatcher, Name, Request, limit_min);

send_async(Dispatcher, Name, Request, limit_min) ->
    send_async(Dispatcher, Name, Request, ?TIMEOUT_SEND_ASYNC_MIN);

send_async(Dispatcher, Name, Request, limit_max) ->
    send_async(Dispatcher, Name, Request, ?TIMEOUT_SEND_ASYNC_MAX);

send_async(Dispatcher, [NameC | _] = Name, Request, Timeout)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         Timeout >= 0, Timeout =< ?TIMEOUT_SEND_ASYNC_MAX ->
    gen_server:call(Dispatcher, {'send_async', Name, <<>>, Request,
                                 Timeout, undefined}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_async(Dispatcher :: dispatcher(),
                 Name :: service_name(),
                 Request :: request(),
                 Timeout :: timeout_milliseconds(),
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

send_async(Dispatcher, Name, Request, immediate, PatternPid) ->
    send_async(Dispatcher, Name, Request,
               limit_min, PatternPid);

send_async(Dispatcher, Name, Request, limit_min, PatternPid) ->
    send_async(Dispatcher, Name, Request,
               ?TIMEOUT_SEND_ASYNC_MIN, PatternPid);

send_async(Dispatcher, Name, Request, limit_max, PatternPid) ->
    send_async(Dispatcher, Name, Request,
               ?TIMEOUT_SEND_ASYNC_MAX, PatternPid);

send_async(Dispatcher, [NameC | _] = Name, Request, Timeout, undefined)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         Timeout >= 0, Timeout =< ?TIMEOUT_SEND_ASYNC_MAX ->
    gen_server:call(Dispatcher, {'send_async', Name, <<>>, Request,
                                 Timeout, undefined}, infinity);

send_async(Dispatcher, [NameC | _] = Name, Request, Timeout, PatternPid)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         Timeout >= 0, Timeout =< ?TIMEOUT_SEND_ASYNC_MAX,
         is_tuple(PatternPid) ->
    gen_server:call(Dispatcher, {'send_async', Name, <<>>, Request,
                                 Timeout, undefined, PatternPid}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_async(Dispatcher :: dispatcher(),
                 Name :: service_name(),
                 RequestInfo :: request_info(),
                 Request :: request(),
                 Timeout :: timeout_milliseconds(),
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

send_async(Dispatcher, Name, RequestInfo, Request,
           immediate, Priority) ->
    send_async(Dispatcher, Name, RequestInfo, Request,
               limit_min, Priority);

send_async(Dispatcher, Name, RequestInfo, Request,
           limit_min, Priority) ->
    send_async(Dispatcher, Name, RequestInfo, Request,
               ?TIMEOUT_SEND_ASYNC_MIN, Priority);

send_async(Dispatcher, Name, RequestInfo, Request,
           limit_max, Priority) ->
    send_async(Dispatcher, Name, RequestInfo, Request,
               ?TIMEOUT_SEND_ASYNC_MAX, Priority);

send_async(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
           Timeout, undefined)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         Timeout >= 0, Timeout =< ?TIMEOUT_SEND_ASYNC_MAX ->
    gen_server:call(Dispatcher, {'send_async', Name,
                                 RequestInfo, Request,
                                 Timeout, undefined}, infinity);

send_async(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
           Timeout, Priority)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         Timeout >= 0, Timeout =< ?TIMEOUT_SEND_ASYNC_MAX,
         is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    gen_server:call(Dispatcher, {'send_async', Name,
                                 RequestInfo, Request,
                                 Timeout, Priority}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_async(Dispatcher :: dispatcher(),
                 Name :: service_name(),
                 RequestInfo :: request_info(),
                 Request :: request(),
                 Timeout :: timeout_milliseconds(),
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

send_async(Dispatcher, Name, RequestInfo, Request,
           immediate, Priority, PatternPid) ->
    send_async(Dispatcher, Name, RequestInfo, Request,
               limit_min, Priority, PatternPid);

send_async(Dispatcher, Name, RequestInfo, Request,
           limit_min, Priority, PatternPid) ->
    send_async(Dispatcher, Name, RequestInfo, Request,
               ?TIMEOUT_SEND_ASYNC_MIN, Priority, PatternPid);

send_async(Dispatcher, Name, RequestInfo, Request,
           limit_max, Priority, PatternPid) ->
    send_async(Dispatcher, Name, RequestInfo, Request,
               ?TIMEOUT_SEND_ASYNC_MAX, Priority, PatternPid);

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
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         Timeout >= 0, Timeout =< ?TIMEOUT_SEND_ASYNC_MAX ->
    gen_server:call(Dispatcher, {'send_async', Name,
                                 RequestInfo, Request,
                                 Timeout, undefined}, infinity);

send_async(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
           Timeout, undefined, PatternPid)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         Timeout >= 0, Timeout =< ?TIMEOUT_SEND_ASYNC_MAX,
         is_tuple(PatternPid) ->
    gen_server:call(Dispatcher, {'send_async', Name,
                                 RequestInfo, Request,
                                 Timeout, undefined, PatternPid}, infinity);

send_async(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
           Timeout, Priority, undefined)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         Timeout >= 0, Timeout =< ?TIMEOUT_SEND_ASYNC_MAX,
         is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    gen_server:call(Dispatcher, {'send_async', Name,
                                 RequestInfo, Request,
                                 Timeout, Priority}, infinity);

send_async(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
           Timeout, Priority, PatternPid)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         Timeout >= 0, Timeout =< ?TIMEOUT_SEND_ASYNC_MAX,
         is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW,
         is_tuple(PatternPid) ->
    gen_server:call(Dispatcher, {'send_async', Name,
                                 RequestInfo, Request,
                                 Timeout, Priority, PatternPid}, infinity).

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
                        Timeout :: timeout_milliseconds()) ->
    {ok, TransId :: trans_id()} |
    {error, Reason :: error_reason()}.

send_async_active(Dispatcher, [NameC | _] = Name, Request, undefined)
    when is_pid(Dispatcher), is_integer(NameC) ->
    gen_server:call(Dispatcher, {'send_async_active', Name, <<>>, Request,
                                 undefined, undefined}, infinity);

send_async_active(Dispatcher, Name, Request, immediate) ->
    send_async_active(Dispatcher, Name, Request, limit_min);

send_async_active(Dispatcher, Name, Request, limit_min) ->
    send_async_active(Dispatcher, Name, Request, ?TIMEOUT_SEND_ASYNC_MIN);

send_async_active(Dispatcher, Name, Request, limit_max) ->
    send_async_active(Dispatcher, Name, Request, ?TIMEOUT_SEND_ASYNC_MAX);

send_async_active(Dispatcher, [NameC | _] = Name, Request, Timeout)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         Timeout >= 0, Timeout =< ?TIMEOUT_SEND_ASYNC_MAX ->
    gen_server:call(Dispatcher, {'send_async_active', Name, <<>>, Request,
                                 Timeout, undefined}, infinity).

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
                        Timeout :: timeout_milliseconds(),
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

send_async_active(Dispatcher, Name, Request, immediate, PatternPid) ->
    send_async_active(Dispatcher, Name, Request,
                      limit_min, PatternPid);

send_async_active(Dispatcher, Name, Request, limit_min, PatternPid) ->
    send_async_active(Dispatcher, Name, Request,
                      ?TIMEOUT_SEND_ASYNC_MIN, PatternPid);

send_async_active(Dispatcher, Name, Request, limit_max, PatternPid) ->
    send_async_active(Dispatcher, Name, Request,
                      ?TIMEOUT_SEND_ASYNC_MAX, PatternPid);

send_async_active(Dispatcher, [NameC | _] = Name, Request, Timeout, undefined)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         Timeout >= 0, Timeout =< ?TIMEOUT_SEND_ASYNC_MAX ->
    gen_server:call(Dispatcher, {'send_async_active', Name, <<>>, Request,
                                 Timeout, undefined}, infinity);

send_async_active(Dispatcher, [NameC | _] = Name, Request, Timeout, PatternPid)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         Timeout >= 0, Timeout =< ?TIMEOUT_SEND_ASYNC_MAX,
         is_tuple(PatternPid) ->
    gen_server:call(Dispatcher, {'send_async_active', Name, <<>>, Request,
                                 Timeout, undefined, PatternPid}, infinity).

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
                        Timeout :: timeout_milliseconds(),
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

send_async_active(Dispatcher, Name, RequestInfo, Request,
                  immediate, Priority) ->
    send_async_active(Dispatcher, Name, RequestInfo, Request,
                      limit_min, Priority);

send_async_active(Dispatcher, Name, RequestInfo, Request,
                  limit_min, Priority) ->
    send_async_active(Dispatcher, Name, RequestInfo, Request,
                      ?TIMEOUT_SEND_ASYNC_MIN, Priority);

send_async_active(Dispatcher, Name, RequestInfo, Request,
                  limit_max, Priority) ->
    send_async_active(Dispatcher, Name, RequestInfo, Request,
                      ?TIMEOUT_SEND_ASYNC_MAX, Priority);

send_async_active(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
                  Timeout, undefined)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         Timeout >= 0, Timeout =< ?TIMEOUT_SEND_ASYNC_MAX ->
    gen_server:call(Dispatcher, {'send_async_active', Name,
                                 RequestInfo, Request,
                                 Timeout, undefined}, infinity);

send_async_active(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
                  Timeout, Priority)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         Timeout >= 0, Timeout =< ?TIMEOUT_SEND_ASYNC_MAX,
         is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    gen_server:call(Dispatcher, {'send_async_active', Name,
                                 RequestInfo, Request,
                                 Timeout, Priority}, infinity).

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
                        Timeout :: timeout_milliseconds(),
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

send_async_active(Dispatcher, Name, RequestInfo, Request,
                  immediate, Priority, PatternPid) ->
    send_async_active(Dispatcher, Name, RequestInfo, Request,
                      limit_min, Priority, PatternPid);

send_async_active(Dispatcher, Name, RequestInfo, Request,
                  limit_min, Priority, PatternPid) ->
    send_async_active(Dispatcher, Name, RequestInfo, Request,
                      ?TIMEOUT_SEND_ASYNC_MIN, Priority, PatternPid);

send_async_active(Dispatcher, Name, RequestInfo, Request,
                  limit_max, Priority, PatternPid) ->
    send_async_active(Dispatcher, Name, RequestInfo, Request,
                      ?TIMEOUT_SEND_ASYNC_MAX, Priority, PatternPid);

send_async_active(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
                  Timeout, undefined, undefined)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         Timeout >= 0, Timeout =< ?TIMEOUT_SEND_ASYNC_MAX ->
    gen_server:call(Dispatcher, {'send_async_active', Name,
                                 RequestInfo, Request,
                                 Timeout, undefined}, infinity);

send_async_active(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
                  Timeout, undefined, PatternPid)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         Timeout >= 0, Timeout =< ?TIMEOUT_SEND_ASYNC_MAX,
         is_tuple(PatternPid) ->
    gen_server:call(Dispatcher, {'send_async_active', Name,
                                 RequestInfo, Request,
                                 Timeout, undefined, PatternPid}, infinity);

send_async_active(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
                  Timeout, Priority, undefined)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         Timeout >= 0, Timeout =< ?TIMEOUT_SEND_ASYNC_MAX,
         is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    gen_server:call(Dispatcher, {'send_async_active', Name,
                                 RequestInfo, Request,
                                 Timeout, Priority}, infinity);

send_async_active(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
                  Timeout, Priority, PatternPid)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         Timeout >= 0, Timeout =< ?TIMEOUT_SEND_ASYNC_MAX,
         is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW,
         is_tuple(PatternPid) ->
    gen_server:call(Dispatcher, {'send_async_active', Name,
                                 RequestInfo, Request,
                                 Timeout, Priority, PatternPid}, infinity).

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
                        Timeout :: timeout_milliseconds(),
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

send_async_active(Dispatcher, Name, RequestInfo, Request,
                  immediate, Priority, TransId, PatternPid) ->
    send_async_active(Dispatcher, Name, RequestInfo, Request,
                      limit_min, Priority, TransId, PatternPid);

send_async_active(Dispatcher, Name, RequestInfo, Request,
                  limit_min, Priority, TransId, PatternPid) ->
    send_async_active(Dispatcher, Name, RequestInfo, Request,
                      ?TIMEOUT_SEND_ASYNC_MIN, Priority, TransId, PatternPid);

send_async_active(Dispatcher, Name, RequestInfo, Request,
                  limit_max, Priority, TransId, PatternPid) ->
    send_async_active(Dispatcher, Name, RequestInfo, Request,
                      ?TIMEOUT_SEND_ASYNC_MAX, Priority, TransId, PatternPid);

send_async_active(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
                  Timeout, undefined, TransId, PatternPid)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         Timeout >= 0, Timeout =< ?TIMEOUT_SEND_ASYNC_MAX,
         is_binary(TransId), is_tuple(PatternPid) ->
    gen_server:call(Dispatcher, {'send_async_active', Name,
                                 RequestInfo, Request,
                                 Timeout, undefined,
                                 TransId, PatternPid}, infinity);

send_async_active(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
                  Timeout, Priority, TransId, PatternPid)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         Timeout >= 0, Timeout =< ?TIMEOUT_SEND_ASYNC_MAX,
         is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW,
         is_binary(TransId), is_tuple(PatternPid) ->
    gen_server:call(Dispatcher, {'send_async_active', Name,
                                 RequestInfo, Request,
                                 Timeout, Priority,
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
                         Timeout :: timeout_milliseconds()) ->
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
                         Timeout :: timeout_milliseconds(),
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
                         Timeout :: timeout_milliseconds(),
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
                         Timeout :: timeout_milliseconds(),
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
                Timeout :: timeout_milliseconds()) ->
    {ok, ResponseInfo :: response_info(), Response :: response()} |
    {ok, Response :: response()} |
    {error, Reason :: error_reason()}.

send_sync(Dispatcher, [NameC | _] = Name, Request, undefined)
    when is_pid(Dispatcher), is_integer(NameC) ->
    gen_server:call(Dispatcher, {'send_sync', Name, <<>>, Request,
                                 undefined, undefined}, infinity);

send_sync(Dispatcher, Name, Request, immediate) ->
    send_sync(Dispatcher, Name, Request, limit_min);

send_sync(Dispatcher, Name, Request, limit_min) ->
    send_sync(Dispatcher, Name, Request, ?TIMEOUT_SEND_SYNC_MIN);

send_sync(Dispatcher, Name, Request, limit_max) ->
    send_sync(Dispatcher, Name, Request, ?TIMEOUT_SEND_SYNC_MAX);

send_sync(Dispatcher, [NameC | _] = Name, Request, Timeout)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         Timeout >= 0, Timeout =< ?TIMEOUT_SEND_SYNC_MAX ->
    gen_server:call(Dispatcher, {'send_sync', Name, <<>>, Request,
                                 Timeout, undefined}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a synchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_sync(Dispatcher :: dispatcher(),
                Name :: service_name(),
                Request :: request(),
                Timeout :: timeout_milliseconds(),
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

send_sync(Dispatcher, Name, Request, immediate, PatternPid) ->
    send_sync(Dispatcher, Name, Request,
              limit_min, PatternPid);

send_sync(Dispatcher, Name, Request, limit_min, PatternPid) ->
    send_sync(Dispatcher, Name, Request,
              ?TIMEOUT_SEND_SYNC_MIN, PatternPid);

send_sync(Dispatcher, Name, Request, limit_max, PatternPid) ->
    send_sync(Dispatcher, Name, Request,
              ?TIMEOUT_SEND_SYNC_MAX, PatternPid);

send_sync(Dispatcher, [NameC | _] = Name, Request, Timeout, undefined)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         Timeout >= 0, Timeout =< ?TIMEOUT_SEND_SYNC_MAX ->
    gen_server:call(Dispatcher, {'send_sync', Name, <<>>, Request,
                                 Timeout, undefined}, infinity);

send_sync(Dispatcher, [NameC | _] = Name, Request, Timeout, PatternPid)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         Timeout >= 0, Timeout =< ?TIMEOUT_SEND_SYNC_MAX,
         is_tuple(PatternPid) ->
    gen_server:call(Dispatcher, {'send_sync', Name, <<>>, Request,
                                 Timeout, undefined, PatternPid}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a synchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_sync(Dispatcher :: dispatcher(),
                Name :: service_name(),
                RequestInfo :: request_info(),
                Request :: request(),
                Timeout :: timeout_milliseconds(),
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

send_sync(Dispatcher, Name, RequestInfo, Request,
          immediate, Priority) ->
    send_sync(Dispatcher, Name, RequestInfo, Request,
              limit_min, Priority);

send_sync(Dispatcher, Name, RequestInfo, Request,
          limit_min, Priority) ->
    send_sync(Dispatcher, Name, RequestInfo, Request,
              ?TIMEOUT_SEND_SYNC_MIN, Priority);

send_sync(Dispatcher, Name, RequestInfo, Request,
          limit_max, Priority) ->
    send_sync(Dispatcher, Name, RequestInfo, Request,
              ?TIMEOUT_SEND_SYNC_MAX, Priority);

send_sync(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
          Timeout, undefined)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         Timeout >= 0, Timeout =< ?TIMEOUT_SEND_SYNC_MAX ->
    gen_server:call(Dispatcher, {'send_sync', Name,
                                 RequestInfo, Request,
                                 Timeout, undefined}, infinity);

send_sync(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
          Timeout, Priority)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         Timeout >= 0, Timeout =< ?TIMEOUT_SEND_SYNC_MAX,
         is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    gen_server:call(Dispatcher, {'send_sync', Name,
                                 RequestInfo, Request,
                                 Timeout, Priority}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a synchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_sync(Dispatcher :: dispatcher(),
                Name :: service_name(),
                RequestInfo :: request_info(),
                Request :: request(),
                Timeout :: timeout_milliseconds(),
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

send_sync(Dispatcher, Name, RequestInfo, Request,
          immediate, Priority, PatternPid) ->
    send_sync(Dispatcher, Name, RequestInfo, Request,
              limit_min, Priority, PatternPid);

send_sync(Dispatcher, Name, RequestInfo, Request,
          limit_min, Priority, PatternPid) ->
    send_sync(Dispatcher, Name, RequestInfo, Request,
              ?TIMEOUT_SEND_SYNC_MIN, Priority, PatternPid);

send_sync(Dispatcher, Name, RequestInfo, Request,
          limit_max, Priority, PatternPid) ->
    send_sync(Dispatcher, Name, RequestInfo, Request,
              ?TIMEOUT_SEND_SYNC_MAX, Priority, PatternPid);

send_sync(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
          Timeout, undefined, undefined)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         Timeout >= 0, Timeout =< ?TIMEOUT_SEND_SYNC_MAX ->
    gen_server:call(Dispatcher, {'send_sync', Name,
                                 RequestInfo, Request,
                                 Timeout, undefined}, infinity);

send_sync(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
          Timeout, undefined, PatternPid)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         Timeout >= 0, Timeout =< ?TIMEOUT_SEND_SYNC_MAX,
         is_tuple(PatternPid) ->
    gen_server:call(Dispatcher, {'send_sync', Name,
                                 RequestInfo, Request,
                                 Timeout, undefined, PatternPid}, infinity);

send_sync(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
          Timeout, Priority, undefined)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         Timeout >= 0, Timeout =< ?TIMEOUT_SEND_SYNC_MAX,
         is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    gen_server:call(Dispatcher, {'send_sync', Name,
                                 RequestInfo, Request,
                                 Timeout, Priority}, infinity);

send_sync(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
          Timeout, Priority, PatternPid)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         Timeout >= 0, Timeout =< ?TIMEOUT_SEND_SYNC_MAX,
         is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW,
         is_tuple(PatternPid) ->
    gen_server:call(Dispatcher, {'send_sync', Name,
                                 RequestInfo, Request,
                                 Timeout, Priority, PatternPid}, infinity).

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
                  Timeout :: timeout_milliseconds()) ->
    {ok, TransIdList :: list(trans_id())} |
    {error, Reason :: error_reason()}.

mcast_async(Dispatcher, [NameC | _] = Name, Request, undefined)
    when is_pid(Dispatcher), is_integer(NameC) ->
    gen_server:call(Dispatcher, {'mcast_async', Name, <<>>, Request,
                                 undefined, undefined}, infinity);

mcast_async(Dispatcher, Name, Request, immediate) ->
    mcast_async(Dispatcher, Name, Request, limit_min);

mcast_async(Dispatcher, Name, Request, limit_min) ->
    mcast_async(Dispatcher, Name, Request, ?TIMEOUT_MCAST_ASYNC_MIN);

mcast_async(Dispatcher, Name, Request, limit_max) ->
    mcast_async(Dispatcher, Name, Request, ?TIMEOUT_MCAST_ASYNC_MAX);

mcast_async(Dispatcher, [NameC | _] = Name, Request, Timeout)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         Timeout >= 0, Timeout =< ?TIMEOUT_MCAST_ASYNC_MAX ->
    gen_server:call(Dispatcher, {'mcast_async', Name, <<>>, Request,
                                 Timeout, undefined}, infinity).

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
                  Timeout :: timeout_milliseconds(),
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

mcast_async(Dispatcher, Name, RequestInfo, Request, immediate, Priority) ->
    mcast_async(Dispatcher, Name, RequestInfo, Request,
                limit_min, Priority);

mcast_async(Dispatcher, Name, RequestInfo, Request, limit_min, Priority) ->
    mcast_async(Dispatcher, Name, RequestInfo, Request,
                ?TIMEOUT_MCAST_ASYNC_MIN, Priority);

mcast_async(Dispatcher, Name, RequestInfo, Request, limit_max, Priority) ->
    mcast_async(Dispatcher, Name, RequestInfo, Request,
                ?TIMEOUT_MCAST_ASYNC_MAX, Priority);

mcast_async(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
            Timeout, undefined)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         Timeout >= 0, Timeout =< ?TIMEOUT_MCAST_ASYNC_MAX ->
    gen_server:call(Dispatcher, {'mcast_async', Name,
                                 RequestInfo, Request,
                                 Timeout, undefined}, infinity);

mcast_async(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
            Timeout, Priority)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         Timeout >= 0, Timeout =< ?TIMEOUT_MCAST_ASYNC_MAX,
         is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    gen_server:call(Dispatcher, {'mcast_async', Name,
                                 RequestInfo, Request,
                                 Timeout, Priority}, infinity).

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
                         Timeout :: timeout_milliseconds()) ->
    {ok, TransIdList :: list(trans_id())} |
    {error, Reason :: error_reason()}.

mcast_async_active(Dispatcher, [NameC | _] = Name, Request, undefined)
    when is_pid(Dispatcher), is_integer(NameC) ->
    gen_server:call(Dispatcher, {'mcast_async_active', Name, <<>>, Request,
                                 undefined, undefined}, infinity);

mcast_async_active(Dispatcher, Name, Request, immediate) ->
    mcast_async_active(Dispatcher, Name, Request, limit_min);

mcast_async_active(Dispatcher, Name, Request, limit_min) ->
    mcast_async_active(Dispatcher, Name, Request, ?TIMEOUT_MCAST_ASYNC_MIN);

mcast_async_active(Dispatcher, Name, Request, limit_max) ->
    mcast_async_active(Dispatcher, Name, Request, ?TIMEOUT_MCAST_ASYNC_MAX);

mcast_async_active(Dispatcher, [NameC | _] = Name, Request, Timeout)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         Timeout >= 0, Timeout =< ?TIMEOUT_MCAST_ASYNC_MAX ->
    gen_server:call(Dispatcher, {'mcast_async_active', Name, <<>>, Request,
                                 Timeout, undefined}, infinity).

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
                         Timeout :: timeout_milliseconds(),
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

mcast_async_active(Dispatcher, Name, RequestInfo, Request,
                   immediate, Priority) ->
    mcast_async_active(Dispatcher, Name, RequestInfo, Request,
                       limit_min, Priority);

mcast_async_active(Dispatcher, Name, RequestInfo, Request,
                   limit_min, Priority) ->
    mcast_async_active(Dispatcher, Name, RequestInfo, Request,
                       ?TIMEOUT_MCAST_ASYNC_MIN, Priority);

mcast_async_active(Dispatcher, Name, RequestInfo, Request,
                   limit_max, Priority) ->
    mcast_async_active(Dispatcher, Name, RequestInfo, Request,
                       ?TIMEOUT_MCAST_ASYNC_MAX, Priority);

mcast_async_active(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
                   Timeout, undefined)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         Timeout >= 0, Timeout =< ?TIMEOUT_MCAST_ASYNC_MAX ->
    gen_server:call(Dispatcher, {'mcast_async_active', Name,
                                 RequestInfo, Request,
                                 Timeout, undefined}, infinity);

mcast_async_active(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
                   Timeout, Priority)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         Timeout >= 0, Timeout =< ?TIMEOUT_MCAST_ASYNC_MAX,
         is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    gen_server:call(Dispatcher, {'mcast_async_active', Name,
                                 RequestInfo, Request,
                                 Timeout, Priority}, infinity).

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
                          Timeout :: timeout_milliseconds()) ->
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
                          Timeout :: timeout_milliseconds(),
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
              Pid :: pid()) ->
    no_return().

forward(Dispatcher, 'send_async', Name, RequestInfo, Request,
        Timeout, Priority, TransId, Pid) ->
    forward_async(Dispatcher, Name, RequestInfo, Request,
                  Timeout, Priority, TransId, Pid);

forward(Dispatcher, 'send_sync', Name, RequestInfo, Request,
        Timeout, Priority, TransId, Pid) ->
    forward_sync(Dispatcher, Name, RequestInfo, Request,
                 Timeout, Priority, TransId, Pid).

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
                    Pid :: pid()) ->
    no_return().

forward_async(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
              Timeout, Priority, TransId, Pid)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         is_binary(TransId), is_pid(Pid),
         Timeout >= ?TIMEOUT_FORWARD_ASYNC_MIN,
         Timeout =< ?TIMEOUT_FORWARD_ASYNC_MAX, is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    erlang:throw({cloudi_service_forward,
                  {'cloudi_service_forward_async_retry', Name,
                   RequestInfo, Request,
                   Timeout, Priority, TransId, Pid}}).

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
                   Pid :: pid()) ->
    no_return().

forward_sync(Dispatcher, [NameC | _] = Name, RequestInfo, Request,
             Timeout, Priority, TransId, Pid)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(Timeout),
         is_binary(TransId), is_pid(Pid),
         Timeout >= ?TIMEOUT_FORWARD_SYNC_MIN,
         Timeout =< ?TIMEOUT_FORWARD_SYNC_MAX, is_integer(Priority),
         Priority >= ?PRIORITY_HIGH, Priority =< ?PRIORITY_LOW ->
    erlang:throw({cloudi_service_forward,
                  {'cloudi_service_forward_sync_retry', Name,
                   RequestInfo, Request,
                   Timeout, Priority, TransId, Pid}}).

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
             Pid :: pid()) ->
    no_return().

return(Dispatcher, 'send_async', Name, Pattern, ResponseInfo, Response,
       Timeout, TransId, Pid) ->
    return_async(Dispatcher, Name, Pattern, ResponseInfo, Response,
                 Timeout, TransId, Pid);

return(Dispatcher, 'send_sync', Name, Pattern, ResponseInfo, Response,
       Timeout, TransId, Pid) ->
    return_sync(Dispatcher, Name, Pattern, ResponseInfo, Response,
                Timeout, TransId, Pid).

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
                   Pid :: pid()) ->
    no_return().

return_async(Dispatcher, [NameC | _] = Name, [PatternC | _] = Pattern,
             ResponseInfo, Response, Timeout, TransId, Pid)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(PatternC),
         is_integer(Timeout), is_binary(TransId), is_pid(Pid),
         Timeout >= ?TIMEOUT_RETURN_ASYNC_MIN,
         Timeout =< ?TIMEOUT_RETURN_ASYNC_MAX ->
    erlang:throw({cloudi_service_return,
                  {'cloudi_service_return_async', Name, Pattern,
                   ResponseInfo, Response,
                   Timeout, TransId, Pid}}).

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
                  Pid :: pid()) ->
    no_return().

return_sync(Dispatcher, [NameC | _] = Name, [PatternC | _] = Pattern,
            ResponseInfo, Response, Timeout, TransId, Pid)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(PatternC),
         is_integer(Timeout), is_binary(TransId), is_pid(Pid),
         Timeout >= ?TIMEOUT_RETURN_SYNC_MIN,
         Timeout =< ?TIMEOUT_RETURN_SYNC_MAX ->
    erlang:throw({cloudi_service_return,
                  {'cloudi_service_return_sync', Name, Pattern,
                   ResponseInfo, Response,
                   Timeout, TransId, Pid}}).

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
                     Pid :: pid()) ->
    ok.

return_nothrow(Dispatcher, 'send_async',
               [NameC | _] = Name, [PatternC | _] = Pattern,
               ResponseInfo, Response, Timeout, TransId, Pid)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(PatternC),
         is_integer(Timeout), is_binary(TransId), is_pid(Pid),
         Timeout >= ?TIMEOUT_RETURN_ASYNC_MIN,
         Timeout =< ?TIMEOUT_RETURN_ASYNC_MAX ->
    Pid ! {'cloudi_service_return_async', Name, Pattern,
           ResponseInfo, Response,
           Timeout, TransId, Pid},
    ok;

return_nothrow(Dispatcher, 'send_sync',
               [NameC | _] = Name, [PatternC | _] = Pattern,
               ResponseInfo, Response, Timeout, TransId, Pid)
    when is_pid(Dispatcher), is_integer(NameC), is_integer(PatternC),
         is_integer(Timeout), is_binary(TransId), is_pid(Pid),
         Timeout >= ?TIMEOUT_RETURN_SYNC_MIN,
         Timeout =< ?TIMEOUT_RETURN_SYNC_MAX ->
    Pid ! {'cloudi_service_return_sync', Name, Pattern,
           ResponseInfo, Response,
           Timeout, TransId, Pid},
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
                 timeout_milliseconds() | trans_id()) ->
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

recv_async(Dispatcher, immediate) ->
    recv_async(Dispatcher, limit_min);

recv_async(Dispatcher, limit_min) ->
    recv_async(Dispatcher, ?TIMEOUT_RECV_ASYNC_MIN);

recv_async(Dispatcher, limit_max) ->
    recv_async(Dispatcher, ?TIMEOUT_RECV_ASYNC_MAX);

recv_async(Dispatcher, Timeout)
    when is_pid(Dispatcher), is_integer(Timeout),
         Timeout >= 0, Timeout =< ?TIMEOUT_RECV_ASYNC_MAX ->
    gen_server:call(Dispatcher,
                    {'recv_async', Timeout, <<0:128>>, true}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Receive an asynchronous service request.===
%% Either use the supplied TransId to receive the specific service request
%% or use a null TransId to receive the oldest service request.
%% @end
%%-------------------------------------------------------------------------

-spec recv_async(Dispatcher :: dispatcher(),
                 timeout_milliseconds() | trans_id(),
                 trans_id() | boolean()) ->
    {ok, ResponseInfo :: response_info(), Response :: response(),
     TransId :: trans_id()} |
    {error, Reason :: error_reason()}.

recv_async(Dispatcher, undefined, TransId)
    when is_pid(Dispatcher), is_binary(TransId) ->
    gen_server:call(Dispatcher,
                    {'recv_async', TransId, true}, infinity);

recv_async(Dispatcher, immediate, TransId) ->
    recv_async(Dispatcher, limit_min, TransId);

recv_async(Dispatcher, limit_min, TransId) ->
    recv_async(Dispatcher, ?TIMEOUT_RECV_ASYNC_MIN, TransId);

recv_async(Dispatcher, limit_max, TransId) ->
    recv_async(Dispatcher, ?TIMEOUT_RECV_ASYNC_MAX, TransId);

recv_async(Dispatcher, Timeout, TransId)
    when is_pid(Dispatcher), is_integer(Timeout), is_binary(TransId),
         Timeout >= 0, Timeout =< ?TIMEOUT_RECV_ASYNC_MAX ->
    gen_server:call(Dispatcher,
                    {'recv_async', Timeout, TransId, true}, infinity);

recv_async(Dispatcher, Timeout, Consume)
    when is_pid(Dispatcher), is_integer(Timeout), is_boolean(Consume),
         Timeout >= 0, Timeout =< ?TIMEOUT_RECV_ASYNC_MAX ->
    gen_server:call(Dispatcher,
                    {'recv_async', Timeout, <<0:128>>, Consume}, infinity);

recv_async(Dispatcher, TransId, Consume)
    when is_pid(Dispatcher), is_binary(TransId), is_boolean(Consume) ->
    gen_server:call(Dispatcher,
                    {'recv_async', TransId, Consume}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Receive an asynchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec recv_async(Dispatcher :: dispatcher(),
                 Timeout :: timeout_milliseconds(),
                 TransId :: trans_id(),
                 Consume :: boolean()) ->
    {ok, ResponseInfo :: response_info(), Response :: response(),
     TransId :: trans_id()} |
    {error, Reason :: error_reason()}.

recv_async(Dispatcher, undefined, TransId, Consume)
    when is_pid(Dispatcher), is_binary(TransId), is_boolean(Consume) ->
    gen_server:call(Dispatcher,
                    {'recv_async', TransId, Consume}, infinity);

recv_async(Dispatcher, immediate, TransId, Consume) ->
    recv_async(Dispatcher, limit_min, TransId, Consume);

recv_async(Dispatcher, limit_min, TransId, Consume) ->
    recv_async(Dispatcher, ?TIMEOUT_RECV_ASYNC_MIN, TransId, Consume);

recv_async(Dispatcher, limit_max, TransId, Consume) ->
    recv_async(Dispatcher, ?TIMEOUT_RECV_ASYNC_MAX, TransId, Consume);

recv_async(Dispatcher, Timeout, TransId, Consume)
    when is_pid(Dispatcher), is_integer(Timeout), is_binary(TransId),
         is_boolean(Consume),
         Timeout >= 0, Timeout =< ?TIMEOUT_RECV_ASYNC_MAX ->
    gen_server:call(Dispatcher,
                    {'recv_async', Timeout, TransId, Consume}, infinity).

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
                  Timeout :: timeout_milliseconds(),
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

recv_asyncs(Dispatcher, immediate, TransIdList) ->
    recv_asyncs(Dispatcher, limit_min, TransIdList);

recv_asyncs(Dispatcher, limit_min, TransIdList) ->
    recv_asyncs(Dispatcher, ?TIMEOUT_RECV_ASYNCS_MIN, TransIdList);

recv_asyncs(Dispatcher, limit_max, TransIdList) ->
    recv_asyncs(Dispatcher, ?TIMEOUT_RECV_ASYNCS_MAX, TransIdList);

recv_asyncs(Dispatcher, Timeout, [_ | _] = TransIdList)
    when is_pid(Dispatcher), is_integer(Timeout), is_list(TransIdList),
         Timeout >= 0, Timeout =< ?TIMEOUT_RECV_ASYNCS_MAX ->
    gen_server:call(Dispatcher,
                    {'recv_asyncs', Timeout,
                     [{<<>>, <<>>, TransId} ||
                      TransId <- TransIdList], true}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Receive asynchronous service requests.===
%% @end
%%-------------------------------------------------------------------------

-spec recv_asyncs(Dispatcher :: dispatcher(),
                  Timeout :: timeout_milliseconds(),
                  TransIdList :: list(trans_id()),
                  Consume :: boolean()) ->
    {ok, list({ResponseInfo :: response_info(), Response :: response(),
               TransId :: trans_id()})} |
    {error, Reason :: error_reason()}.

recv_asyncs(Dispatcher, immediate, TransIdList, Consume) ->
    recv_asyncs(Dispatcher, limit_min, TransIdList, Consume);

recv_asyncs(Dispatcher, limit_min, TransIdList, Consume) ->
    recv_asyncs(Dispatcher, ?TIMEOUT_RECV_ASYNCS_MIN, TransIdList, Consume);

recv_asyncs(Dispatcher, limit_max, TransIdList, Consume) ->
    recv_asyncs(Dispatcher, ?TIMEOUT_RECV_ASYNCS_MAX, TransIdList, Consume);

recv_asyncs(Dispatcher, Timeout, [_ | _] = TransIdList, Consume)
    when is_pid(Dispatcher), is_integer(Timeout), is_list(TransIdList),
         is_boolean(Consume),
         Timeout >= 0, Timeout =< ?TIMEOUT_RECV_ASYNCS_MAX ->
    gen_server:call(Dispatcher,
                    {'recv_asyncs', Timeout,
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
%% `cloudi_service_handle_request(_, _, _, _, _, _, _, _, Pid, _, _)'
%% @end
%%-------------------------------------------------------------------------

-spec source_subscriptions(Dispatcher :: dispatcher(),
                           Pid :: source()) ->
    list(service_name_pattern()).

source_subscriptions(Dispatcher, Pid)
    when is_pid(Pid) ->
    gen_server:call(Dispatcher, {source_subscriptions, Pid}, infinity).

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

