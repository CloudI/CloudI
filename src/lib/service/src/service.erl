%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Service Behaviour==
%%% A minimal behaviour for creating CloudI internal services.
%%% ```
%%% The user module should export:
%%%
%%%   service_init(Args, Prefix, Dispatcher)  
%%%    ==> {ok, State}
%%%        {stop, Reason}
%%%        {stop, Reason, State}
%%%
%%%   service_request(ServiceReq, State, Dispatcher)
%%%    ==> {reply, Response, NewState}
%%%        {reply, ResponseInfo, Response, NewState}
%%%        {forward, NextServiceReq, NewState}
%%%        {noreply, NewState}
%%%        {stop, Reason, NewState}  
%%%               Reason = normal | shutdown | Term terminate(State) is called
%%%
%%%   service_info(Request, State, Dispatcher)
%%%
%%%    ==> {noreply, State}
%%%        {stop, Reason, NewState} 
%%%               Reason = normal | shutdown | Term, terminate(State) is called
%%%
%%%   service_terminate(Reason, State) Let the user module clean up
%%%        always called when the service terminates
%%%
%%%    ==> ok
%%%
%%%
%%% The work flow (of the service) can be described as follows:
%%%
%%%    User module                                          Generic
%%%    -----------                                          -------
%%%    service_init                     <-----              .
%%%
%%%                                                         loop
%%%    service_request                  <-----              .
%%%                                     ----->              reply
%%%
%%%    service_info                     <-----              .
%%%
%%%    service_terminate                <-----              .
%%%
%%% '''
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2013-2014, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2013-2014 Michael Truog
%%% @version 1.3.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(service).
-author('mjtruog [at] gmail (dot) com').

%% behavior interface
-export([% cloudi_service_api module helpers
         add/1,
         add/2,
         remove/1,
         remove/2,
         % cloudi_service/cloudi module helpers
         process_index/1,
         self/1,
         subscribe/2,
         unsubscribe/2,
         get_pid/2,
         get_pid/3,
         get_pids/2,
         get_pids/3,
         send/2,
         send_async/2,
         send_async/3,
         send_async/4,
         send_async/5,
         send_async/6,
         send_async/7,
         send_async_active/2,
         send_async_active/3,
         send_async_active/4,
         send_async_active/5,
         send_async_active/6,
         send_async_active/7,
         send_async_passive/2,
         send_async_passive/3,
         send_async_passive/4,
         send_async_passive/5,
         send_async_passive/6,
         send_async_passive/7,
         send_sync/2,
         send_sync/3,
         send_sync/4,
         send_sync/5,
         send_sync/6,
         send_sync/7,
         mcast_async/2,
         mcast_async/3,
         mcast_async/4,
         mcast_async/6,
         mcast_async_active/2,
         mcast_async_active/3,
         mcast_async_active/4,
         mcast_async_active/6,
         mcast_async_passive/2,
         mcast_async_passive/3,
         mcast_async_passive/4,
         mcast_async_passive/6,
         forward/2,
         forward/9,
         forward_async/2,
         forward_async/8,
         forward_sync/2,
         forward_sync/8,
         return/2,
         return/3,
         return/9,
         return_async/8,
         return_sync/8,
         return_nothrow/3,
         return_nothrow/4,
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
         timeout_async/1,
         timeout_sync/1,
         timeout_max/1,
         priority_default/1,
         destination_refresh_immediate/1,
         destination_refresh_lazy/1,
         source_subscriptions/2,
         context_options/1,
         % service request parameter helpers
         service_name_parse/2,
         service_name_parse_with_suffix/2,
         request_http_qs_parse/1,
         request_info_key_value_new/1,
         request_info_key_value_parse/1,
         key_value_erase/2,
         key_value_find/2,
         key_value_store/3,
         trans_id/1,
         trans_id_age/1,
         % functions to trigger edoc, until -callback works with edoc
         'Module:service_init'/3,
         'Module:service_request'/3,
         'Module:service_info'/3,
         'Module:service_terminate'/2]).

-include("service_req.hrl").

-type service_req() :: #service_req{}.
-export_type([service_req/0]).

% cloudi_service module types
-type request_type() :: cloudi_service:request_type().
-type service_name() :: cloudi_service:service_name().
-type service_name_pattern() :: cloudi_service:service_name_pattern().
-type request_info() :: cloudi_service:request_info().
-type request() :: cloudi_service:request().
-type response_info() :: cloudi_service:response_info().
-type response() :: cloudi_service:response().
-type timeout_milliseconds() :: cloudi_service:timeout_milliseconds().
-type priority() :: cloudi_service:priority().
-type trans_id() :: cloudi_service:trans_id().
-type pattern_pid() :: cloudi_service:pattern_pid().
-type dispatcher() :: cloudi_service:dispatcher().
-type source() :: cloudi_service:source().
-type key_values() :: cloudi_service:key_values().
-export_type([request_type/0,
              service_name/0,
              service_name_pattern/0,
              request_info/0, request/0,
              response_info/0, response/0,
              timeout_milliseconds/0,
              priority/0,
              trans_id/0,
              pattern_pid/0,
              dispatcher/0,
              source/0,
              key_values/0]).

% cloudi module types
-type context() :: cloudi:context().
-export_type([context/0]).

%%%------------------------------------------------------------------------
%%% Callback functions from behavior
%%%------------------------------------------------------------------------

-callback service_config() ->
    cloudi_service_api:service_internal() |
    cloudi_service_api:service_proplist().

-callback service_init(Args :: list(),
                       Prefix :: cloudi_service:service_name_pattern(),
                       Dispatcher :: cloudi_service:dispatcher()) ->
    {'ok', State :: any()} |
    {'stop', Reason :: any()} |
    {'stop', Reason :: any(), State :: any()}.

-callback service_request(ServiceReq :: service_req(),
                          State :: any(),
                          Dispatcher :: cloudi_service:dispatcher()) ->
    {'reply', Response :: cloudi_service:response(), NewState :: any()} |
    {'reply', ResponseInfo :: cloudi_service:response_info(),
     Response :: cloudi_service:response(), NewState :: any()} |
    {'forward', NextServiceReq :: service_req(),
     NewState :: any()} |
    {'noreply', NewState :: any()} |
    {'stop', Reason :: any(), NewState :: any()}.

-callback service_info(Request :: any(),
                       State :: any(),
                       Dispatcher :: cloudi_service:dispatcher()) ->
    {'noreply', NewState :: any()} |
    {'stop', Reason :: any(), NewState :: any()}.

-callback service_terminate(Reason :: any(),
                            State :: any()) ->
    'ok'.

%%%------------------------------------------------------------------------
%%% Behavior interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% cloudi_service_api module helpers
%%%------------------------------------------------------------------------

-spec add(Module :: module()) ->
    {ok, ServiceId :: cloudi_service_api:service_id()} |
    {error, any()}.

add(Module)
    when is_atom(Module) ->
    case cloudi_service_api:services_add([Module:service_config()],
                                         infinity) of
        {ok, [ServiceId]} ->
            {ok, ServiceId};
        {error, _} = Error ->
            Error
    end.

-spec add(Module :: module(),
          Timeout :: cloudi_service_api:timeout_milliseconds() | infinity) ->
    {ok, ServiceId :: cloudi_service_api:service_id()} |
    {error, any()}.

add(Module, Timeout)
    when is_atom(Module) ->
    case cloudi_service_api:services_add([Module:service_config()],
                                         Timeout) of
        {ok, [ServiceId]} ->
            {ok, ServiceId};
        {error, _} = Error ->
            Error
    end.

-spec remove(ServiceId :: cloudi_service_api:service_id()) ->
    ok |
    {error, any()}.

remove(ServiceId)
    when is_binary(ServiceId), byte_size(ServiceId) == 16 ->
    cloudi_service_api:services_remove([ServiceId], infinity).

-spec remove(ServiceId :: cloudi_service_api:service_id(),
             Timeout :: cloudi_service_api:timeout_milliseconds() | infinity) ->
    ok |
    {error, any()}.

remove(ServiceId, Timeout)
    when is_binary(ServiceId), byte_size(ServiceId) == 16 ->
    cloudi_service_api:services_remove([ServiceId], Timeout).

%%%------------------------------------------------------------------------
%%% cloudi_service/cloudi module helpers
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the index of this instance of the service.===
%% The configuration of the service defined how many instances should exist.
%% @end
%%-------------------------------------------------------------------------

-spec process_index(Dispatcher :: cloudi_service:dispatcher()) ->
    ProcessIndex :: non_neg_integer().

process_index(Dispatcher) ->
    cloudi_service:process_index(Dispatcher).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the Erlang pid representing the service.===
%% @end
%%-------------------------------------------------------------------------

-spec self(Dispatcher :: cloudi_service:dispatcher()) ->
    Self :: pid().

self(Dispatcher) ->
    cloudi_service:self(Dispatcher).

%%-------------------------------------------------------------------------
%% @doc
%% ===Subscribe to a service name pattern.===
%% @end
%%-------------------------------------------------------------------------

-spec subscribe(Dispatcher :: cloudi_service:dispatcher(),
                Pattern :: cloudi_service:service_name_pattern()) ->
    ok.

subscribe(Dispatcher, Pattern) ->
    cloudi_service:subscribe(Dispatcher, Pattern).

%%-------------------------------------------------------------------------
%% @doc
%% ===Unsubscribe from a service name pattern.===
%% @end
%%-------------------------------------------------------------------------

-spec unsubscribe(Dispatcher :: cloudi_service:dispatcher(),
                  Pattern :: cloudi_service:service_name_pattern()) ->
    ok | error.

unsubscribe(Dispatcher, Pattern) ->
    cloudi_service:unsubscribe(Dispatcher, Pattern).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a service destination based on a service name.===
%% @end
%%-------------------------------------------------------------------------

-spec get_pid(Dispatcher :: cloudi_service:dispatcher() |
                            cloudi:context(),
              Name :: cloudi_service:service_name()) ->
    {'ok', PatternPid :: cloudi_service:pattern_pid()} |
    {'error', Reason :: atom()}.

get_pid(Dispatcher, Name) ->
    cloudi:get_pid(Dispatcher, Name).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a service destination based on a service name.===
%% @end
%%-------------------------------------------------------------------------

-spec get_pid(Dispatcher :: cloudi_service:dispatcher() |
                            cloudi:context(),
              Name :: cloudi_service:service_name(),
              Timeout :: cloudi_service:timeout_milliseconds() |
                         'undefined') ->
    {'ok', PatternPid :: cloudi_service:pattern_pid()} |
    {'error', Reason :: atom()}.

get_pid(Dispatcher, Name, Timeout) ->
    cloudi:get_pid(Dispatcher, Name, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get all service destinations based on a service name.===
%% @end
%%-------------------------------------------------------------------------

-spec get_pids(Dispatcher :: cloudi_service:dispatcher() |
                             cloudi:context(),
               Name :: cloudi_service:service_name()) ->
    {'ok', PatternPids :: list(cloudi_service:pattern_pid())} |
    {'error', Reason :: atom()}.

get_pids(Dispatcher, Name) ->
    cloudi:get_pids(Dispatcher, Name).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a service destination based on a service name.===
%% @end
%%-------------------------------------------------------------------------

-spec get_pids(Dispatcher :: cloudi_service:dispatcher() |
                             cloudi:context(),
               Name :: cloudi_service:service_name(),
               Timeout :: cloudi_service:timeout_milliseconds() |
                          'undefined') ->
    {'ok', PatternPids :: list(cloudi_service:pattern_pid())} |
    {'error', Reason :: atom()}.

get_pids(Dispatcher, Name, Timeout) ->
    cloudi:get_pids(Dispatcher, Name, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send(Dispatcher :: cloudi_service:dispatcher() |
                         cloudi:context(),
           ServiceReq :: service_req()) ->
    {'ok', TransId :: cloudi_service:trans_id()} |
    {'error', Reason :: atom()}.

send(Dispatcher,
     #service_req{type = send_async,
                  name = Name,
                  request_info = RequestInfo,
                  request = Request,
                  timeout = Timeout,
                  priority = Priority,
                  pid = PatternPid}) ->
    NewRequestInfo = if
        RequestInfo =:= undefined ->
            <<>>;
        true ->
            RequestInfo
    end,
    cloudi:send_async(Dispatcher, Name, NewRequestInfo, Request,
                      Timeout, Priority, PatternPid);

send(Dispatcher,
     #service_req{type = send_sync,
                  name = Name,
                  request_info = RequestInfo,
                  request = Request,
                  timeout = Timeout,
                  priority = Priority,
                  pid = PatternPid}) ->
    NewRequestInfo = if
        RequestInfo =:= undefined ->
            <<>>;
        true ->
            RequestInfo
    end,
    cloudi:send_sync(Dispatcher, Name, NewRequestInfo, Request,
                     Timeout, Priority, PatternPid).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_async(Dispatcher :: cloudi_service:dispatcher() |
                               cloudi:context(),
                 ServiceReq :: service_req()) ->
    {'ok', TransId :: cloudi_service:trans_id()} |
    {'error', Reason :: atom()}.

send_async(Dispatcher,
           #service_req{type = Type,
                        name = Name,
                        request_info = RequestInfo,
                        request = Request,
                        timeout = Timeout,
                        priority = Priority,
                        pid = PatternPid})
    when Type =:= undefined; Type =:= send_async ->
    NewRequestInfo = if
        RequestInfo =:= undefined ->
            <<>>;
        true ->
            RequestInfo
    end,
    cloudi:send_async(Dispatcher, Name, NewRequestInfo, Request,
                      Timeout, Priority, PatternPid).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_async(Dispatcher :: cloudi_service:dispatcher() |
                               cloudi:context(),
                 Name :: cloudi_service:service_name(),
                 Request :: cloudi_service:request()) ->
    {'ok', TransId :: cloudi_service:trans_id()} |
    {'error', Reason :: atom()}.

send_async(Dispatcher, Name, Request) ->
    cloudi:send_async(Dispatcher, Name, Request).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_async(Dispatcher :: cloudi_service:dispatcher() |
                               cloudi:context(),
                 Name :: cloudi_service:service_name(),
                 Request :: cloudi_service:request(),
                 Timeout :: cloudi_service:timeout_milliseconds() |
                            'undefined') ->
    {'ok', TransId :: cloudi_service:trans_id()} |
    {'error', Reason :: atom()}.

send_async(Dispatcher, Name, Request, Timeout) ->
    cloudi:send_async(Dispatcher, Name, Request, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_async(Dispatcher :: cloudi_service:dispatcher() |
                               cloudi:context(),
                 Name :: cloudi_service:service_name(),
                 Request :: cloudi_service:request(),
                 Timeout :: cloudi_service:timeout_milliseconds() |
                            'undefined',
                 PatternPid :: cloudi_service:pattern_pid() |
                               'undefined') ->
    {'ok', TransId :: cloudi_service:trans_id()} |
    {'error', Reason :: atom()}.

send_async(Dispatcher, Name, Request, Timeout, PatternPid) ->
    cloudi:send_async(Dispatcher, Name, Request, Timeout, PatternPid).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_async(Dispatcher :: cloudi_service:dispatcher() |
                               cloudi:context(),
                 Name :: cloudi_service:service_name(),
                 RequestInfo :: cloudi_service:request_info(),
                 Request :: cloudi_service:request(),
                 Timeout :: cloudi_service:timeout_milliseconds() |
                            'undefined',
                 Priority :: cloudi_service:priority() |
                             'undefined') ->
    {'ok', TransId :: cloudi_service:trans_id()} |
    {'error', Reason :: atom()}.

send_async(Dispatcher, Name, RequestInfo, Request,
           Timeout, Priority) ->
    cloudi:send_async(Dispatcher, Name, RequestInfo, Request,
                      Timeout, Priority).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_async(Dispatcher :: cloudi_service:dispatcher() |
                               cloudi:context(),
                 Name :: cloudi_service:service_name(),
                 RequestInfo :: cloudi_service:request_info(),
                 Request :: cloudi_service:request(),
                 Timeout :: cloudi_service:timeout_milliseconds() |
                            'undefined',
                 Priority :: cloudi_service:priority() |
                             'undefined',
                 PatternPid :: cloudi_service:pattern_pid() |
                               'undefined') ->
    {'ok', TransId :: cloudi_service:trans_id()} |
    {'error', Reason :: atom()}.

send_async(Dispatcher, Name, RequestInfo, Request,
           Timeout, Priority, PatternPid) ->
    cloudi:send_async(Dispatcher, Name, RequestInfo, Request,
                      Timeout, Priority, PatternPid).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% The response is sent to the service as an Erlang message which is either:
%% `#return_async_active{}' (or) `#timeout_async_active{}'
%% @end
%%-------------------------------------------------------------------------

-spec send_async_active(Dispatcher :: cloudi_service:dispatcher(),
                        ServiceReq :: service_req()) ->
    {'ok', TransId :: cloudi_service:trans_id()} |
    {'error', Reason :: atom()}.

send_async_active(Dispatcher,
                  #service_req{type = Type,
                               name = Name,
                               request_info = RequestInfo,
                               request = Request,
                               timeout = Timeout,
                               priority = Priority,
                               pid = PatternPid})
    when Type =:= undefined; Type =:= send_async ->
    NewRequestInfo = if
        RequestInfo =:= undefined ->
            <<>>;
        true ->
            RequestInfo
    end,
    cloudi_service:send_async_active(Dispatcher, Name, NewRequestInfo, Request,
                                     Timeout, Priority, PatternPid).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% The response is sent to the service as an Erlang message which is either:
%% `#return_async_active{}' (or) `#timeout_async_active{}'
%% @end
%%-------------------------------------------------------------------------

-spec send_async_active(Dispatcher :: cloudi_service:dispatcher(),
                        Name :: cloudi_service:service_name(),
                        Request :: cloudi_service:request()) ->
    {'ok', TransId :: cloudi_service:trans_id()} |
    {'error', Reason :: atom()}.

send_async_active(Dispatcher, Name, Request) ->
    cloudi_service:send_async_active(Dispatcher, Name, Request).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% The response is sent to the service as an Erlang message which is either:
%% `#return_async_active{}' (or) `#timeout_async_active{}'
%% @end
%%-------------------------------------------------------------------------

-spec send_async_active(Dispatcher :: cloudi_service:dispatcher(),
                        Name :: cloudi_service:service_name(),
                        Request :: cloudi_service:request(),
                        Timeout :: cloudi_service:timeout_milliseconds() |
                                   'undefined') ->
    {'ok', TransId :: cloudi_service:trans_id()} |
    {'error', Reason :: atom()}.

send_async_active(Dispatcher, Name, Request, Timeout) ->
    cloudi_service:send_async_active(Dispatcher, Name, Request, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% The response is sent to the service as an Erlang message which is either:
%% `#return_async_active{}' (or) `#timeout_async_active{}'
%% @end
%%-------------------------------------------------------------------------

-spec send_async_active(Dispatcher :: cloudi_service:dispatcher(),
                        Name :: cloudi_service:service_name(),
                        Request :: cloudi_service:request(),
                        Timeout :: cloudi_service:timeout_milliseconds() |
                                   'undefined',
                        PatternPid :: cloudi_service:pattern_pid() |
                                      'undefined') ->
    {'ok', TransId :: cloudi_service:trans_id()} |
    {'error', Reason :: atom()}.

send_async_active(Dispatcher, Name, Request, Timeout, PatternPid) ->
    cloudi_service:send_async_active(Dispatcher, Name, Request,
                                     Timeout, PatternPid).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% The response is sent to the service as an Erlang message which is either:
%% `#return_async_active{}' (or) `#timeout_async_active{}'
%% @end
%%-------------------------------------------------------------------------

-spec send_async_active(Dispatcher :: cloudi_service:dispatcher(),
                        Name :: cloudi_service:service_name(),
                        RequestInfo :: cloudi_service:request_info(),
                        Request :: cloudi_service:request(),
                        Timeout :: cloudi_service:timeout_milliseconds() |
                                   'undefined',
                        Priority :: cloudi_service:priority() |
                                    'undefined') ->
    {'ok', TransId :: cloudi_service:trans_id()} |
    {'error', Reason :: atom()}.

send_async_active(Dispatcher, Name, RequestInfo, Request,
                  Timeout, Priority) ->
    cloudi_service:send_async_active(Dispatcher, Name, RequestInfo, Request,
                                     Timeout, Priority).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% The response is sent to the service as an Erlang message which is either:
%% `#return_async_active{}' (or) `#timeout_async_active{}'
%% @end
%%-------------------------------------------------------------------------

-spec send_async_active(Dispatcher :: cloudi_service:dispatcher(),
                        Name :: cloudi_service:service_name(),
                        RequestInfo :: cloudi_service:request_info(),
                        Request :: cloudi_service:request(),
                        Timeout :: cloudi_service:timeout_milliseconds() |
                                   'undefined',
                        Priority :: cloudi_service:priority() |
                                    'undefined',
                        PatternPid :: cloudi_service:pattern_pid() |
                                      'undefined') ->
    {'ok', TransId :: cloudi_service:trans_id()} |
    {'error', Reason :: atom()}.

send_async_active(Dispatcher, Name, RequestInfo, Request,
                  Timeout, Priority, PatternPid) ->
    cloudi_service:send_async_active(Dispatcher, Name, RequestInfo, Request,
                                     Timeout, Priority, PatternPid).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% An alias for send_async.  The asynchronous service request is returned
%% and handled the same way as within external services.
%% @end
%%-------------------------------------------------------------------------

-spec send_async_passive(Dispatcher :: cloudi_service:dispatcher() |
                                       cloudi:context(),
                         ServiceReq :: service_req()) ->
    {'ok', TransId :: cloudi_service:trans_id()} |
    {'error', Reason :: atom()}.

send_async_passive(Dispatcher, ServiceReq) ->
    send_async(Dispatcher, ServiceReq).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% An alias for send_async.  The asynchronous service request is returned
%% and handled the same way as within external services.
%% @end
%%-------------------------------------------------------------------------

-spec send_async_passive(Dispatcher :: cloudi_service:dispatcher() |
                                       cloudi:context(),
                         Name :: cloudi_service:service_name(),
                         Request :: cloudi_service:request()) ->
    {'ok', TransId :: cloudi_service:trans_id()} |
    {'error', Reason :: atom()}.

send_async_passive(Dispatcher, Name, Request) ->
    send_async(Dispatcher, Name, Request).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% An alias for send_async.  The asynchronous service request is returned
%% and handled the same way as within external services.
%% @end
%%-------------------------------------------------------------------------

-spec send_async_passive(Dispatcher :: cloudi_service:dispatcher() |
                                       cloudi:context(),
                         Name :: cloudi_service:service_name(),
                         Request :: cloudi_service:request(),
                         Timeout :: cloudi_service:timeout_milliseconds() |
                                    'undefined') ->
    {'ok', TransId :: cloudi_service:trans_id()} |
    {'error', Reason :: atom()}.

send_async_passive(Dispatcher, Name, Request, Timeout) ->
    send_async(Dispatcher, Name, Request, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% An alias for send_async.  The asynchronous service request is returned
%% and handled the same way as within external services.
%% @end
%%-------------------------------------------------------------------------

-spec send_async_passive(Dispatcher :: cloudi_service:dispatcher() |
                                       cloudi:context(),
                         Name :: cloudi_service:service_name(),
                         Request :: cloudi_service:request(),
                         Timeout :: cloudi_service:timeout_milliseconds() |
                                    'undefined',
                         PatternPid :: cloudi_service:pattern_pid() |
                                       'undefined') ->
    {'ok', TransId :: cloudi_service:trans_id()} |
    {'error', Reason :: atom()}.

send_async_passive(Dispatcher, Name, Request, Timeout, PatternPid) ->
    send_async(Dispatcher, Name, Request, Timeout, PatternPid).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send an asynchronous service request.===
%% An alias for send_async.  The asynchronous service request is returned
%% and handled the same way as within external services.
%% @end
%%-------------------------------------------------------------------------

-spec send_async_passive(Dispatcher :: cloudi_service:dispatcher() |
                                       cloudi:context(),
                         Name :: cloudi_service:service_name(),
                         RequestInfo :: cloudi_service:request_info(),
                         Request :: cloudi_service:request(),
                         Timeout :: cloudi_service:timeout_milliseconds() |
                                    'undefined',
                         Priority :: cloudi_service:priority() |
                                     'undefined') ->
    {'ok', TransId :: cloudi_service:trans_id()} |
    {'error', Reason :: atom()}.

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

-spec send_async_passive(Dispatcher :: cloudi_service:dispatcher() |
                                       cloudi:context(),
                         Name :: cloudi_service:service_name(),
                         RequestInfo :: cloudi_service:request_info(),
                         Request :: cloudi_service:request(),
                         Timeout :: cloudi_service:timeout_milliseconds() |
                                    'undefined',
                         Priority :: cloudi_service:priority() |
                                     'undefined',
                         PatternPid :: cloudi_service:pattern_pid() |
                                       'undefined') ->
    {'ok', TransId :: cloudi_service:trans_id()} |
    {'error', Reason :: atom()}.

send_async_passive(Dispatcher, Name, RequestInfo, Request,
                   Timeout, Priority, PatternPid) ->
    send_async(Dispatcher, Name, RequestInfo, Request,
               Timeout, Priority, PatternPid).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a synchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_sync(Dispatcher :: cloudi_service:dispatcher() |
                              cloudi:context(),
                ServiceReq :: service_req()) ->
    {'ok', ResponseInfo :: cloudi_service:response_info(),
     Response :: cloudi_service:response()} |
    {'ok', Response :: cloudi_service:response()} |
    {'error', Reason :: atom()}.

send_sync(Dispatcher,
          #service_req{type = Type,
                       name = Name,
                       request_info = RequestInfo,
                       request = Request,
                       timeout = Timeout,
                       priority = Priority,
                       pid = PatternPid})
    when Type =:= undefined; Type =:= send_sync ->
    NewRequestInfo = if
        RequestInfo =:= undefined ->
            <<>>;
        true ->
            RequestInfo
    end,
    cloudi:send_sync(Dispatcher, Name, NewRequestInfo, Request,
                     Timeout, Priority, PatternPid).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a synchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_sync(Dispatcher :: cloudi_service:dispatcher() |
                              cloudi:context(),
                Name :: cloudi_service:service_name(),
                Request :: cloudi_service:request()) ->
    {'ok', ResponseInfo :: cloudi_service:response_info(),
     Response :: cloudi_service:response()} |
    {'ok', Response :: cloudi_service:response()} |
    {'error', Reason :: atom()}.

send_sync(Dispatcher, Name, Request) ->
    cloudi:send_sync(Dispatcher, Name, Request).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a synchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_sync(Dispatcher :: cloudi_service:dispatcher() |
                              cloudi:context(),
                Name :: cloudi_service:service_name(),
                Request :: cloudi_service:request(),
                Timeout :: cloudi_service:timeout_milliseconds() |
                           'undefined') ->
    {'ok', ResponseInfo :: cloudi_service:response_info(),
     Response :: cloudi_service:response()} |
    {'ok', Response :: cloudi_service:response()} |
    {'error', Reason :: atom()}.

send_sync(Dispatcher, Name, Request, Timeout) ->
    cloudi:send_sync(Dispatcher, Name, Request, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a synchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_sync(Dispatcher :: cloudi_service:dispatcher() |
                              cloudi:context(),
                Name :: cloudi_service:service_name(),
                Request :: cloudi_service:request(),
                Timeout :: cloudi_service:timeout_milliseconds() |
                           'undefined',
                PatternPid :: cloudi_service:pattern_pid() |
                              'undefined') ->
    {'ok', ResponseInfo :: cloudi_service:response_info(),
     Response :: cloudi_service:response()} |
    {'ok', Response :: cloudi_service:response()} |
    {'error', Reason :: atom()}.

send_sync(Dispatcher, Name, Request, Timeout, PatternPid) ->
    cloudi:send_sync(Dispatcher, Name, Request, Timeout, PatternPid).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a synchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_sync(Dispatcher :: cloudi_service:dispatcher() |
                              cloudi:context(),
                Name :: cloudi_service:service_name(),
                RequestInfo :: cloudi_service:request_info(),
                Request :: cloudi_service:request(),
                Timeout :: cloudi_service:timeout_milliseconds() |
                           'undefined',
                Priority :: cloudi_service:priority() |
                            'undefined') ->
    {'ok', ResponseInfo :: cloudi_service:response_info(),
     Response :: cloudi_service:response()} |
    {'ok', Response :: cloudi_service:response()} |
    {'error', Reason :: atom()}.

send_sync(Dispatcher, Name, RequestInfo, Request,
          Timeout, Priority) ->
    cloudi:send_sync(Dispatcher, Name, RequestInfo, Request,
                     Timeout, Priority).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a synchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec send_sync(Dispatcher :: cloudi_service:dispatcher() |
                              cloudi:context(),
                Name :: cloudi_service:service_name(),
                RequestInfo :: cloudi_service:request_info(),
                Request :: cloudi_service:request(),
                Timeout :: cloudi_service:timeout_milliseconds() |
                           'undefined',
                Priority :: cloudi_service:priority() |
                            'undefined',
                PatternPid :: cloudi_service:pattern_pid() |
                              'undefined') ->
    {'ok', ResponseInfo :: cloudi_service:response_info(),
     Response :: cloudi_service:response()} |
    {'ok', Response :: cloudi_service:response()} |
    {'error', Reason :: atom()}.

send_sync(Dispatcher, Name, RequestInfo, Request,
          Timeout, Priority, PatternPid) ->
    cloudi:send_sync(Dispatcher, Name, RequestInfo, Request,
                     Timeout, Priority, PatternPid).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a multicast asynchronous service request.===
%% Asynchronous service requests are sent to all services that have
%% subscribed to the service name pattern that matches the destination.
%% @end
%%-------------------------------------------------------------------------

-spec mcast_async(Dispatcher :: cloudi_service:dispatcher() |
                                cloudi:context(),
                  ServiceReq :: service_req()) ->
    {'ok', TransIdList :: list(cloudi_service:trans_id())} |
    {'error', Reason :: atom()}.

mcast_async(Dispatcher,
            #service_req{type = Type,
                         name = Name,
                         request_info = RequestInfo,
                         request = Request,
                         timeout = Timeout,
                         priority = Priority})
    when Type =:= undefined; Type =:= send_async ->
    NewRequestInfo = if
        RequestInfo =:= undefined ->
            <<>>;
        true ->
            RequestInfo
    end,
    cloudi:mcast_async(Dispatcher, Name, NewRequestInfo, Request,
                       Timeout, Priority).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a multicast asynchronous service request.===
%% Asynchronous service requests are sent to all services that have
%% subscribed to the service name pattern that matches the destination.
%% @end
%%-------------------------------------------------------------------------

-spec mcast_async(Dispatcher :: cloudi_service:dispatcher() |
                                cloudi:context(),
                  Name :: cloudi_service:service_name(),
                  Request :: cloudi_service:request()) ->
    {'ok', TransIdList :: list(cloudi_service:trans_id())} |
    {'error', Reason :: atom()}.

mcast_async(Dispatcher, Name, Request) ->
    cloudi:mcast_async(Dispatcher, Name, Request).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a multicast asynchronous service request.===
%% Asynchronous service requests are sent to all services that have
%% subscribed to the service name pattern that matches the destination.
%% @end
%%-------------------------------------------------------------------------

-spec mcast_async(Dispatcher :: cloudi_service:dispatcher() |
                                cloudi:context(),
                  Name :: cloudi_service:service_name(),
                  Request :: cloudi_service:request(),
                  Timeout :: cloudi_service:timeout_milliseconds() |
                             'undefined') ->
    {'ok', TransIdList :: list(cloudi_service:trans_id())} |
    {'error', Reason :: atom()}.

mcast_async(Dispatcher, Name, Request, Timeout) ->
    cloudi:mcast_async(Dispatcher, Name, Request, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a multicast asynchronous service request.===
%% Asynchronous service requests are sent to all services that have
%% subscribed to the service name pattern that matches the destination.
%% @end
%%-------------------------------------------------------------------------

-spec mcast_async(Dispatcher :: cloudi_service:dispatcher() |
                                cloudi:context(),
                  Name :: cloudi_service:service_name(),
                  RequestInfo :: cloudi_service:request_info(),
                  Request :: cloudi_service:request(),
                  Timeout :: cloudi_service:timeout_milliseconds() |
                             'undefined',
                  Priority :: cloudi_service:priority() |
                              'undefined') ->
    {'ok', TransIdList :: list(cloudi_service:trans_id())} |
    {'error', Reason :: atom()}.

mcast_async(Dispatcher, Name, RequestInfo, Request,
            Timeout, Priority) ->
    cloudi:mcast_async(Dispatcher, Name, RequestInfo, Request,
                       Timeout, Priority).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a multicast asynchronous service request.===
%% Asynchronous service requests are sent to all services that have
%% subscribed to the service name pattern that matches the destination.
%% The responses are sent to the service as Erlang messages that are either:
%% `#return_async_active{}' (or) `#timeout_async_active{}'
%% @end
%%-------------------------------------------------------------------------

-spec mcast_async_active(Dispatcher :: cloudi_service:dispatcher(),
                         ServiceReq :: service_req()) ->
    {'ok', TransIdList :: list(cloudi_service:trans_id())} |
    {'error', Reason :: atom()}.

mcast_async_active(Dispatcher,
                   #service_req{type = Type,
                                name = Name,
                                request_info = RequestInfo,
                                request = Request,
                                timeout = Timeout,
                                priority = Priority})
    when Type =:= undefined; Type =:= send_async ->
    NewRequestInfo = if
        RequestInfo =:= undefined ->
            <<>>;
        true ->
            RequestInfo
    end,
    cloudi_service:mcast_async_active(Dispatcher, Name, NewRequestInfo, Request,
                                      Timeout, Priority).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a multicast asynchronous service request.===
%% Asynchronous service requests are sent to all services that have
%% subscribed to the service name pattern that matches the destination.
%% The responses are sent to the service as Erlang messages that are either:
%% `#return_async_active{}' (or) `#timeout_async_active{}'
%% @end
%%-------------------------------------------------------------------------

-spec mcast_async_active(Dispatcher :: cloudi_service:dispatcher(),
                         Name :: cloudi_service:service_name(),
                         Request :: cloudi_service:request()) ->
    {'ok', TransIdList :: list(cloudi_service:trans_id())} |
    {'error', Reason :: atom()}.

mcast_async_active(Dispatcher, Name, Request) ->
    cloudi_service:mcast_async_active(Dispatcher, Name, Request).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a multicast asynchronous service request.===
%% Asynchronous service requests are sent to all services that have
%% subscribed to the service name pattern that matches the destination.
%% The responses are sent to the service as Erlang messages that are either:
%% `#return_async_active{}' (or) `#timeout_async_active{}'
%% @end
%%-------------------------------------------------------------------------

-spec mcast_async_active(Dispatcher :: cloudi_service:dispatcher(),
                         Name :: cloudi_service:service_name(),
                         Request :: cloudi_service:request(),
                         Timeout :: cloudi_service:timeout_milliseconds() |
                                    'undefined') ->
    {'ok', TransIdList :: list(cloudi_service:trans_id())} |
    {'error', Reason :: atom()}.

mcast_async_active(Dispatcher, Name, Request, Timeout) ->
    cloudi_service:mcast_async_active(Dispatcher, Name, Request, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a multicast asynchronous service request.===
%% Asynchronous service requests are sent to all services that have
%% subscribed to the service name pattern that matches the destination.
%% The responses are sent to the service as Erlang messages that are either:
%% `#return_async_active{}' (or) `#timeout_async_active{}'
%% @end
%%-------------------------------------------------------------------------

-spec mcast_async_active(Dispatcher :: cloudi_service:dispatcher(),
                         Name :: cloudi_service:service_name(),
                         RequestInfo :: cloudi_service:request_info(),
                         Request :: cloudi_service:request(),
                         Timeout :: cloudi_service:timeout_milliseconds() |
                                    'undefined',
                         Priority :: cloudi_service:priority() |
                                     'undefined') ->
    {'ok', TransIdList :: list(cloudi_service:trans_id())} |
    {'error', Reason :: atom()}.

mcast_async_active(Dispatcher, Name, RequestInfo, Request,
                   Timeout, Priority) ->
    cloudi_service:mcast_async_active(Dispatcher, Name, RequestInfo, Request,
                                      Timeout, Priority).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a multicast asynchronous service request.===
%% An alias for mcast_async.  The asynchronous service requests are returned
%% and handled the same way as within external services.
%% @end
%%-------------------------------------------------------------------------

-spec mcast_async_passive(Dispatcher :: cloudi_service:dispatcher() |
                                        cloudi:context(),
                          ServiceReq :: service_req()) ->
    {'ok', TransId :: list(cloudi_service:trans_id())} |
    {'error', Reason :: atom()}.

mcast_async_passive(Dispatcher, ServiceReq) ->
    mcast_async(Dispatcher, ServiceReq).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a multicast asynchronous service request.===
%% An alias for mcast_async.  The asynchronous service requests are returned
%% and handled the same way as within external services.
%% @end
%%-------------------------------------------------------------------------

-spec mcast_async_passive(Dispatcher :: cloudi_service:dispatcher() |
                                        cloudi:context(),
                          Name :: cloudi_service:service_name(),
                          Request :: cloudi_service:request()) ->
    {'ok', TransIdList :: list(cloudi_service:trans_id())} |
    {'error', Reason :: atom()}.

mcast_async_passive(Dispatcher, Name, Request) ->
    mcast_async(Dispatcher, Name, Request).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a multicast asynchronous service request.===
%% An alias for mcast_async.  The asynchronous service requests are returned
%% and handled the same way as within external services.
%% @end
%%-------------------------------------------------------------------------

-spec mcast_async_passive(Dispatcher :: cloudi_service:dispatcher() |
                                        cloudi:context(),
                          Name :: cloudi_service:service_name(),
                          Request :: cloudi_service:request(),
                          Timeout :: cloudi_service:timeout_milliseconds() |
                                     'undefined') ->
    {'ok', TransIdList :: list(cloudi_service:trans_id())} |
    {'error', Reason :: atom()}.

mcast_async_passive(Dispatcher, Name, Request, Timeout) ->
    mcast_async(Dispatcher, Name, Request, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Send a multicast asynchronous service request.===
%% An alias for mcast_async.  The asynchronous service requests are returned
%% and handled the same way as within external services.
%% @end
%%-------------------------------------------------------------------------

-spec mcast_async_passive(Dispatcher :: cloudi_service:dispatcher() |
                                        cloudi:context(),
                          Name :: cloudi_service:service_name(),
                          RequestInfo :: cloudi_service:request_info(),
                          Request :: cloudi_service:request(),
                          Timeout :: cloudi_service:timeout_milliseconds() |
                                     'undefined',
                          Priority :: cloudi_service:priority() |
                                      'undefined') ->
    {'ok', TransIdList :: list(cloudi_service:trans_id())} |
    {'error', Reason :: atom()}.

mcast_async_passive(Dispatcher, Name, RequestInfo, Request,
                    Timeout, Priority) ->
    mcast_async(Dispatcher, Name, RequestInfo, Request, Timeout, Priority).

%%-------------------------------------------------------------------------
%% @doc
%% ===Forward a service request.===
%% @end
%%-------------------------------------------------------------------------

-spec forward(Dispatcher :: cloudi_service:dispatcher(),
              ServiceReq :: service_req()) ->
    no_return().

forward(Dispatcher,
        #service_req{type = Type,
                     name = Name,
                     request_info = RequestInfo,
                     request = Request,
                     timeout = Timeout,
                     priority = Priority,
                     trans_id = TransId,
                     pid = Pid})
    when Type =:= send_async; Type =:= send_sync ->
    cloudi_service:forward(Dispatcher, Type, Name, RequestInfo, Request,
                           Timeout, Priority, TransId, Pid).

%%-------------------------------------------------------------------------
%% @doc
%% ===Forward a service request.===
%% @end
%%-------------------------------------------------------------------------

-spec forward(Dispatcher :: cloudi_service:dispatcher(),
              Type :: cloudi_service:request_type(),
              Name :: cloudi_service:service_name(),
              RequestInfo :: cloudi_service:request_info(),
              Request :: cloudi_service:request(),
              Timeout :: cloudi_service:timeout_milliseconds(),
              Priority :: cloudi_service:priority(),
              TransId :: cloudi_service:trans_id(),
              Pid :: pid()) ->
    no_return().

forward(Dispatcher, Type, Name, RequestInfo, Request,
        Timeout, Priority, TransId, Pid) ->
    cloudi_service:forward(Dispatcher, Type, Name, RequestInfo, Request,
                           Timeout, Priority, TransId, Pid).

%%-------------------------------------------------------------------------
%% @doc
%% ===Forward an asynchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec forward_async(Dispatcher :: cloudi_service:dispatcher(),
                    ServiceReq :: service_req()) ->
    no_return().

forward_async(Dispatcher,
              #service_req{type = Type,
                           name = Name,
                           request_info = RequestInfo,
                           request = Request,
                           timeout = Timeout,
                           priority = Priority,
                           trans_id = TransId,
                           pid = Pid})
    when Type =:= send_async ->
    cloudi_service:forward_async(Dispatcher, Name, RequestInfo, Request,
                                 Timeout, Priority, TransId, Pid).

%%-------------------------------------------------------------------------
%% @doc
%% ===Forward an asynchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec forward_async(Dispatcher :: cloudi_service:dispatcher(),
                    Name :: cloudi_service:service_name(),
                    RequestInfo :: cloudi_service:request_info(),
                    Request :: cloudi_service:request(),
                    Timeout :: cloudi_service:timeout_milliseconds(),
                    Priority :: cloudi_service:priority(),
                    TransId :: cloudi_service:trans_id(),
                    Pid :: pid()) ->
    no_return().

forward_async(Dispatcher, Name, RequestInfo, Request,
              Timeout, Priority, TransId, Pid) ->
    cloudi_service:forward_async(Dispatcher, Name, RequestInfo, Request,
                                 Timeout, Priority, TransId, Pid).

%%-------------------------------------------------------------------------
%% @doc
%% ===Forward a synchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec forward_sync(Dispatcher :: cloudi_service:dispatcher(),
                   ServiceReq :: service_req()) ->
    no_return().

forward_sync(Dispatcher,
             #service_req{type = Type,
                          name = Name,
                          request_info = RequestInfo,
                          request = Request,
                          timeout = Timeout,
                          priority = Priority,
                          trans_id = TransId,
                          pid = Pid})
    when Type =:= send_sync ->
    cloudi_service:forward_async(Dispatcher, Name, RequestInfo, Request,
                                 Timeout, Priority, TransId, Pid).

%%-------------------------------------------------------------------------
%% @doc
%% ===Forward a synchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec forward_sync(Dispatcher :: cloudi_service:dispatcher(),
                   Name :: cloudi_service:service_name(),
                   RequestInfo :: cloudi_service:request_info(),
                   Request :: cloudi_service:request(),
                   Timeout :: cloudi_service:timeout_milliseconds(),
                   Priority :: cloudi_service:priority(),
                   TransId :: cloudi_service:trans_id(),
                   Pid :: pid()) ->
    no_return().

forward_sync(Dispatcher, Name, RequestInfo, Request,
             Timeout, Priority, TransId, Pid) ->
    cloudi_service:forward_sync(Dispatcher, Name, RequestInfo, Request,
                                Timeout, Priority, TransId, Pid).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return a service response.===
%% @end
%%-------------------------------------------------------------------------

-spec return(Dispatcher :: cloudi_service:dispatcher(),
             Response :: cloudi_service:response()) ->
    no_return().

return(Dispatcher, Response) ->
    cloudi_service:return(Dispatcher, Response).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return a service response.===
%% @end
%%-------------------------------------------------------------------------

-spec return(Dispatcher :: cloudi_service:dispatcher(),
             ResponseInfo :: cloudi_service:response_info(),
             Response :: cloudi_service:response()) ->
    no_return().

return(Dispatcher, ResponseInfo, Response) ->
    cloudi_service:return(Dispatcher, ResponseInfo, Response).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return a service response.===
%% @end
%%-------------------------------------------------------------------------

-spec return(Dispatcher :: cloudi_service:dispatcher(),
             Type :: cloudi_service:request_type(),
             Name :: cloudi_service:service_name(),
             Pattern :: cloudi_service:service_name_pattern(),
             ResponseInfo :: cloudi_service:response_info(),
             Response :: cloudi_service:response(),
             Timeout :: cloudi_service:timeout_milliseconds(),
             TransId :: cloudi_service:trans_id(),
             Pid :: pid()) ->
    no_return().

return(Dispatcher, Type, Name, Pattern, ResponseInfo, Response,
       Timeout, TransId, Pid) ->
    cloudi_service:return(Dispatcher, Type, Name, Pattern,
                          ResponseInfo, Response, Timeout, TransId, Pid).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return an asynchronous service response.===
%% @end
%%-------------------------------------------------------------------------

-spec return_async(Dispatcher :: cloudi_service:dispatcher(),
                   Name :: cloudi_service:service_name(),
                   Pattern :: cloudi_service:service_name_pattern(),
                   ResponseInfo :: cloudi_service:response_info(),
                   Response :: cloudi_service:response(),
                   Timeout :: cloudi_service:timeout_milliseconds(),
                   TransId :: cloudi_service:trans_id(),
                   Pid :: pid()) ->
    no_return().

return_async(Dispatcher, Name, Pattern, ResponseInfo, Response,
             Timeout, TransId, Pid) ->
    cloudi_service:return_async(Dispatcher, Name, Pattern,
                                ResponseInfo, Response, Timeout, TransId, Pid).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return a synchronous service response.===
%% @end
%%-------------------------------------------------------------------------

-spec return_sync(Dispatcher :: cloudi_service:dispatcher(),
                  Name :: cloudi_service:service_name(),
                  Pattern :: cloudi_service:service_name_pattern(),
                  ResponseInfo :: cloudi_service:response_info(),
                  Response :: cloudi_service:response(),
                  Timeout :: cloudi_service:timeout_milliseconds(),
                  TransId :: cloudi_service:trans_id(),
                  Pid :: pid()) ->
    no_return().

return_sync(Dispatcher, Name, Pattern, ResponseInfo, Response,
            Timeout, TransId, Pid) ->
    cloudi_service:return_sync(Dispatcher, Name, Pattern,
                               ResponseInfo, Response, Timeout, TransId, Pid).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return a service response without exiting the request handler.===
%% Should rarely, if ever, be used.  If the service has the option
%% request_timeout_adjustment == true, the adjustment will not occur when
%% this function is used.
%% @end
%%-------------------------------------------------------------------------

-spec return_nothrow(Dispatcher :: cloudi_service:dispatcher(),
                     Response :: cloudi_service:response(),
                     ServiceReq :: service_req()) -> 'ok'.

return_nothrow(Dispatcher, Response,
               #service_req{type = Type,
                            name = Name,
                            pattern = Pattern,
                            timeout = Timeout,
                            trans_id = TransId,
                            pid = Pid}) ->
    cloudi_service:return_nothrow(Dispatcher, Type, Name, Pattern,
                                  <<>>, Response,
                                  Timeout, TransId, Pid).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return a service response without exiting the request handler.===
%% Should rarely, if ever, be used.  If the service has the option
%% request_timeout_adjustment == true, the adjustment will not occur when
%% this function is used.
%% @end
%%-------------------------------------------------------------------------

-spec return_nothrow(Dispatcher :: cloudi_service:dispatcher(),
                     ResponseInfo :: cloudi_service:response_info(),
                     Response :: cloudi_service:response(),
                     ServiceReq :: service_req()) -> 'ok'.

return_nothrow(Dispatcher, ResponseInfo, Response,
               #service_req{type = Type,
                            name = Name,
                            pattern = Pattern,
                            timeout = Timeout,
                            trans_id = TransId,
                            pid = Pid}) ->
    cloudi_service:return_nothrow(Dispatcher, Type, Name, Pattern,
                                  ResponseInfo, Response,
                                  Timeout, TransId, Pid).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return a service response without exiting the request handler.===
%% Should rarely, if ever, be used.  If the service has the option
%% request_timeout_adjustment == true, the adjustment will not occur when
%% this function is used.
%% @end
%%-------------------------------------------------------------------------

-spec return_nothrow(Dispatcher :: cloudi_service:dispatcher(),
                     Type :: cloudi_service:request_type(),
                     Name :: cloudi_service:service_name(),
                     Pattern :: cloudi_service:service_name_pattern(),
                     ResponseInfo :: cloudi_service:response_info(),
                     Response :: cloudi_service:response(),
                     Timeout :: cloudi_service:timeout_milliseconds(),
                     TransId :: cloudi_service:trans_id(),
                     Pid :: pid()) -> 'ok'.

return_nothrow(Dispatcher, Type, Name, Pattern, ResponseInfo, Response,
               Timeout, TransId, Pid) ->
    cloudi_service:return_nothrow(Dispatcher, Type, Name, Pattern,
                                  ResponseInfo, Response,
                                  Timeout, TransId, Pid).

%%-------------------------------------------------------------------------
%% @doc
%% ===Receive an asynchronous service request.===
%% Use a null TransId to receive the oldest service request.
%% @end
%%-------------------------------------------------------------------------

-spec recv_async(Dispatcher :: cloudi_service:dispatcher() |
                               cloudi:context()) ->
    {'ok', ResponseInfo :: cloudi_service:response_info(),
     Response :: cloudi_service:response(),
     TransId :: cloudi_service:trans_id()} |
    {'error', Reason :: atom()}.

recv_async(Dispatcher) ->
    cloudi:recv_async(Dispatcher).

%%-------------------------------------------------------------------------
%% @doc
%% ===Receive an asynchronous service request.===
%% Either use the supplied TransId to receive the specific service request
%% or use a null TransId to receive the oldest service request.
%% @end
%%-------------------------------------------------------------------------

-spec recv_async(Dispatcher :: cloudi_service:dispatcher() |
                               cloudi:context(),
                 cloudi_service:timeout_milliseconds() |
                 cloudi_service:trans_id()) ->
    {'ok', ResponseInfo :: cloudi_service:response_info(),
     Response :: cloudi_service:response(),
     TransId :: cloudi_service:trans_id()} |
    {'error', Reason :: atom()}.

recv_async(Dispatcher, Param1) ->
    cloudi:recv_async(Dispatcher, Param1).

%%-------------------------------------------------------------------------
%% @doc
%% ===Receive an asynchronous service request.===
%% Either use the supplied TransId to receive the specific service request
%% or use a null TransId to receive the oldest service request.
%% @end
%%-------------------------------------------------------------------------

-spec recv_async(Dispatcher :: cloudi_service:dispatcher() |
                               cloudi:context(),
                 cloudi_service:timeout_milliseconds() |
                 cloudi_service:trans_id(),
                 cloudi_service:trans_id() |
                 boolean()) ->
    {'ok', ResponseInfo :: cloudi_service:response_info(),
     Response :: cloudi_service:response(),
     TransId :: cloudi_service:trans_id()} |
    {'error', Reason :: atom()}.

recv_async(Dispatcher, Param1, Param2) ->
    cloudi:recv_async(Dispatcher, Param1, Param2).

%%-------------------------------------------------------------------------
%% @doc
%% ===Receive an asynchronous service request.===
%% @end
%%-------------------------------------------------------------------------

-spec recv_async(Dispatcher :: cloudi_service:dispatcher(),
                 Timeout :: cloudi_service:timeout_milliseconds(),
                 TransId :: cloudi_service:trans_id(),
                 Consume :: boolean()) ->
    {'ok', ResponseInfo :: cloudi_service:response_info(),
     Response :: cloudi_service:response(),
     TransId :: cloudi_service:trans_id()} |
    {'error', Reason :: atom()}.

recv_async(Dispatcher, Timeout, TransId, Consume) ->
    cloudi_service:recv_async(Dispatcher, Timeout, TransId, Consume).

%%-------------------------------------------------------------------------
%% @doc
%% ===Receive asynchronous service requests.===
%% @end
%%-------------------------------------------------------------------------

-spec recv_asyncs(Dispatcher :: cloudi_service:dispatcher() |
                                cloudi:context(),
                  TransIdList :: list(cloudi_service:trans_id())) ->
    {'ok', list({ResponseInfo :: cloudi_service:response_info(),
                 Response :: cloudi_service:response(),
                 TransId :: cloudi_service:trans_id()})} |
    {'error', Reason :: atom()}.

recv_asyncs(Dispatcher, TransIdList) ->
    cloudi:recv_asyncs(Dispatcher, TransIdList).

%%-------------------------------------------------------------------------
%% @doc
%% ===Receive asynchronous service requests.===
%% @end
%%-------------------------------------------------------------------------

-spec recv_asyncs(Dispatcher :: cloudi_service:dispatcher() |
                                cloudi:context(),
                  Timeout :: cloudi_service:timeout_milliseconds() |
                             'undefined',
                  TransIdList :: list(cloudi_service:trans_id())) ->
    {'ok', list({ResponseInfo :: cloudi_service:response_info(),
                 Response :: cloudi_service:response(),
                 TransId :: cloudi_service:trans_id()})} |
    {'error', Reason :: atom()}.

recv_asyncs(Dispatcher, Timeout, TransIdList) ->
    cloudi:recv_asyncs(Dispatcher, Timeout, TransIdList).

%%-------------------------------------------------------------------------
%% @doc
%% ===Receive asynchronous service requests.===
%% @end
%%-------------------------------------------------------------------------

-spec recv_asyncs(Dispatcher :: cloudi_service:dispatcher(),
                  Timeout :: cloudi_service:timeout_milliseconds(),
                  TransIdList :: list(cloudi_service:trans_id()),
                  Consume :: boolean()) ->
    {'ok', list({ResponseInfo :: cloudi_service:response_info(),
                 Response :: cloudi_service:response(),
                 TransId :: cloudi_service:trans_id()})} |
    {'error', Reason :: atom()}.

recv_asyncs(Dispatcher, Timeout, TransIdList, Consume) ->
    cloudi_service:recv_asyncs(Dispatcher, Timeout, TransIdList, Consume).

%%-------------------------------------------------------------------------
%% @doc
%% ===Configured service default prefix.===
%% All subscribed/unsubscribed service names use this prefix.  The prefix
%% defines the scope of the service.
%% @end
%%-------------------------------------------------------------------------

-spec prefix(Dispatcher :: cloudi_service:dispatcher()) ->
    Prefix :: cloudi_service:service_name_pattern().

prefix(Dispatcher) ->
    cloudi_service:prefix(Dispatcher).

%%-------------------------------------------------------------------------
%% @doc
%% ===Configured service default asynchronous timeout (in milliseconds).===
%% @end
%%-------------------------------------------------------------------------

-spec timeout_async(Dispatcher :: cloudi_service:dispatcher() |
                                  cloudi:context()) ->
    TimeoutAsync :: cloudi_service_api:timeout_milliseconds().

timeout_async(Dispatcher) ->
    cloudi:timeout_async(Dispatcher).

%%-------------------------------------------------------------------------
%% @doc
%% ===Configured service default synchronous timeout (in milliseconds).===
%% @end
%%-------------------------------------------------------------------------

-spec timeout_sync(Dispatcher :: cloudi_service:dispatcher() |
                                 cloudi:context()) ->
    TimeoutSync :: cloudi_service_api:timeout_milliseconds().

timeout_sync(Dispatcher) ->
    cloudi:timeout_sync(Dispatcher).

%%-------------------------------------------------------------------------
%% @doc
%% === Maximum possible service request timeout (in milliseconds).===
%% @end
%%-------------------------------------------------------------------------

-spec timeout_max(Context :: cloudi_service:dispatcher() |
                             cloudi:context()) ->
    TimeoutMax :: cloudi_service_api:timeout_milliseconds().

timeout_max(Dispatcher) ->
    cloudi:timeout_max(Dispatcher).

%%-------------------------------------------------------------------------
%% @doc
%% ===Configured service default priority.===
%% @end
%%-------------------------------------------------------------------------

-spec priority_default(Context :: cloudi_service:dispatcher() |
                                  cloudi:context()) ->
    PriorityDefault :: cloudi_service_api:priority().

priority_default(Dispatcher) ->
    cloudi:priority_default(Dispatcher).

%%-------------------------------------------------------------------------
%% @doc
%% ===Configured service destination refresh is immediate.===
%% @end
%%-------------------------------------------------------------------------

-spec destination_refresh_immediate(Context :: cloudi_service:dispatcher() |
                                               cloudi:context()) ->
    boolean().

destination_refresh_immediate(Dispatcher) ->
    cloudi:destination_refresh_immediate(Dispatcher).

%%-------------------------------------------------------------------------
%% @doc
%% ===Configured service destination refresh is lazy.===
%% @end
%%-------------------------------------------------------------------------

-spec destination_refresh_lazy(Context :: cloudi_service:dispatcher() |
                                          cloudi:context()) ->
    boolean().

destination_refresh_lazy(Dispatcher) ->
    cloudi:destination_refresh_lazy(Dispatcher).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a list of all service name patterns a service request source is subscribed to.===
%% @end
%%-------------------------------------------------------------------------

-spec source_subscriptions(Dispatcher :: cloudi_service:dispatcher(),
                           ServiceReq :: service_req()) ->
    list(cloudi_service:service_name_pattern()).

source_subscriptions(Dispatcher, #service_req{pid = Pid}) ->
    cloudi_service:source_subscriptions(Dispatcher, Pid).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the context options from the service's configuration.===
%% A service would only use this when delaying the creation of a context
%% for child processes.
%% @end
%%-------------------------------------------------------------------------

-spec context_options(Dispatcher :: cloudi_service:dispatcher()) ->
    cloudi:options().

context_options(Dispatcher) ->
    cloudi_service:context_options(Dispatcher).

%%-------------------------------------------------------------------------
%% @doc
%% ===Parse a service name pattern.===
%% @end
%%-------------------------------------------------------------------------

-spec service_name_parse(Name :: string(),
                         Pattern :: string()) ->
    list(string()) | error.

service_name_parse(Name, Pattern) ->
    cloudi_service:service_name_parse(Name, Pattern).

%%-------------------------------------------------------------------------
%% @doc
%% ===Parse a service name pattern and return the common suffix.===
%% @end
%%-------------------------------------------------------------------------

-spec service_name_parse_with_suffix(Name :: string(),
                                     Pattern :: string()) ->
    {list(string()), string()} | error.

service_name_parse_with_suffix(Name, Pattern) ->
    cloudi_service:service_name_parse_with_suffix(Name, Pattern).

%%-------------------------------------------------------------------------
%% @doc
%% ===Parse HTTP Request query string data.===
%% @end
%%-------------------------------------------------------------------------

-spec request_http_qs_parse(Request :: binary()) ->
    Result :: dict().

request_http_qs_parse(Request) ->
    cloudi_service:request_http_qs_parse(Request).

%%-------------------------------------------------------------------------
%% @doc
%% ===New RequestInfo key/value data.===
%% RequestInfo is meant to contain key/value pairs that is request
%% meta-data.  Create the binary RequestInfo data with a list of pairs or
%% a dict data structure.
%% @end
%%-------------------------------------------------------------------------

-spec request_info_key_value_new(RequestInfo :: cloudi_service:key_values()) ->
    Result :: binary().

request_info_key_value_new(RequestInfo) ->
    cloudi_service:request_info_key_value_new(RequestInfo).

%%-------------------------------------------------------------------------
%% @doc
%% ===Parse RequestInfo key/value data.===
%% RequestInfo is meant to contain key/value pairs that is request
%% meta-data.
%% @end
%%-------------------------------------------------------------------------

-spec request_info_key_value_parse(RequestInfo :: binary() | list()) ->
    Result :: dict().

request_info_key_value_parse(RequestInfo) ->
    cloudi_service:request_info_key_value_parse(RequestInfo).

%%-------------------------------------------------------------------------
%% @doc
%% ===Generic key/value erase.===
%% RequestInfo's key/value result from request_info_key_value_parse/1
%% can be used here to erase request meta-data while encapsulating
%% the data structure used for the lookup.
%% @end
%%-------------------------------------------------------------------------

-spec key_value_erase(Key :: any(),
                      KeyValues :: cloudi_service:key_values()) ->
    NewKeyValues :: cloudi_service:key_values().

key_value_erase(Key, KeyValues) ->
    cloudi_service:key_value_erase(Key, KeyValues).

%%-------------------------------------------------------------------------
%% @doc
%% ===Generic key/value find.===
%% RequestInfo's key/value result from request_info_key_value_parse/1
%% can be used here to access the request meta-data while encapsulating
%% the data structure used for the lookup.
%% @end
%%-------------------------------------------------------------------------

-spec key_value_find(Key :: any(),
                     KeyValues :: cloudi_service:key_values()) ->
    {ok, Value :: any()} |
    error.

key_value_find(Key, KeyValues) ->
    cloudi_service:key_value_find(Key, KeyValues).

%%-------------------------------------------------------------------------
%% @doc
%% ===Generic key/value store.===
%% RequestInfo's key/value result from request_info_key_value_parse/1
%% can be used here to store request meta-data while encapsulating
%% the data structure used for the lookup.
%% @end
%%-------------------------------------------------------------------------

-spec key_value_store(Key :: any(),
                      Value :: any(),
                      KeyValues :: cloudi_service:key_values()) ->
    NewKeyValues :: cloudi_service:key_values().

key_value_store(Key, Value, KeyValues) ->
    cloudi_service:key_value_store(Key, Value, KeyValues).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return a new transaction id.===
%% The same data as used when sending service requests is used.
%% @end
%%-------------------------------------------------------------------------

-spec trans_id(Dispatcher :: cloudi_service:dispatcher() |
                             cloudi:context()) ->
    <<_:128>>.

trans_id(Dispatcher) ->
    cloudi:trans_id(Dispatcher).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the age of the transaction id.===
%% The result is microseconds since the Unix epoch 1970-01-01 00:00:00.
%% @end
%%-------------------------------------------------------------------------

-spec trans_id_age(TransId :: <<_:128>>) ->
    non_neg_integer().

trans_id_age(TransId)
    when is_binary(TransId) ->
    cloudi:trans_id_age(TransId).

%%%------------------------------------------------------------------------
%%% edoc functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Initialize the internal service.===
%% Create the internal service state.  Do any initial service subscriptions
%% necessary.  Send service requests, if required for service initialization.
%% @end
%%-------------------------------------------------------------------------

-spec 'Module:service_init'(Args :: list(),
                            Prefix :: cloudi_service:service_name_pattern(),
                            Dispatcher :: cloudi_service:dispatcher()) ->
    {'ok', State :: any()} |
    {'stop', Reason :: any()} |
    {'stop', Reason :: any(), State :: any()}.

'Module:service_init'(_, _, _) ->
    {ok, state}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Handle an incoming service request.===
%% The request_pid_uses and request_pid_options service configuration options
%% control the Erlang process used to call this function.
%% @end
%%-------------------------------------------------------------------------

-spec 'Module:service_request'(ServiceReq :: service_req(),
                               State :: any(),
                               Dispatcher :: cloudi_service:dispatcher()) ->
    {'reply', Response :: cloudi_service:response(), NewState :: any()} |
    {'reply', ResponseInfo :: cloudi_service:response_info(),
     Response :: cloudi_service:response(), NewState :: any()} |
    {'forward', NextServiceReq :: service_req(), NewState :: any()} |
    {'noreply', NewState :: any()} |
    {'stop', Reason :: any(), NewState :: any()}.

'Module:service_request'(_, _, _) ->
    {reply, response, state}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Handle an incoming Erlang message.===
%% The info_pid_uses and info_pid_options service configuration options
%% control the Erlang process used to call this function.
%% @end
%%-------------------------------------------------------------------------

-spec 'Module:service_info'(Request :: any(),
                            State :: any(),
                            Dispatcher :: cloudi_service:dispatcher()) ->
    {'noreply', NewState :: any()} |
    {'stop', Reason :: any(), NewState :: any()}.

'Module:service_info'(_, _, _) ->
    {noreply, state}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Handle service termination.===
%% cloudi_service_terminate/2 is always called, even when cloudi_service_init/3
%% returns a stop tuple.  When State is unset in the stop tuple, the
%% cloudi_service_terminate/2 function is called with State equal to
%% 'undefined'.  Always calling the cloudi_service_terminate/2 function differs
%% from how Erlang/OTP behaviours handle the init/1 function returning a stop
%% tuple, but this approach can help prevent problems managing any global
%% state that might exist that is connected to a service, or simply services
%% that are only partially initialized.
%% @end
%%-------------------------------------------------------------------------

-spec 'Module:service_terminate'(Reason :: any(),
                                 State :: any()) ->
    'ok'.

'Module:service_terminate'(_, _) ->
    'ok'.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

