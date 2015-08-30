%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Basic CloudI TCP Integration==
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
%%% @version 1.5.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_tcp).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_service).

%% external interface

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include_lib("cloudi_core/include/cloudi_service_children.hrl").

-define(DEFAULT_INTERFACE,             {127,0,0,1}). % ip address
-define(DEFAULT_PORT,                         8080).
-define(DEFAULT_DESTINATION,             undefined). % service name
-define(DEFAULT_DESTINATION_CONNECT,     undefined). % service name
-define(DEFAULT_DESTINATION_DISCONNECT,  undefined). % service name
-define(DEFAULT_BACKLOG,                       128).
-define(DEFAULT_NODELAY,                      true).
-define(DEFAULT_KEEPALIVE,                    true).
-define(DEFAULT_RECV_TIMEOUT,            30 * 1000). % milliseconds
-define(DEFAULT_MAX_CONNECTIONS,              4096).
-define(DEFAULT_PACKET_TYPE,                  line). % inet:setopts/2 packet
-define(DEFAULT_PACKET_BUFFER_RECV_SIZE, undefined).
-define(DEFAULT_PACKET_BUFFER_SEND_SIZE, undefined).
-define(DEFAULT_PACKET_BUFFER_SIZE,      undefined). % Erlang driver buffer
-define(DEFAULT_DESTINATION_SET,             false). % see below:
        % Use to set the destination of each socket based on a
        % service request sent as the first service request to
        % the configured destination.  The RequestInfo contains a
        % <<"service_name">> key/value entry with the current
        % destination with may be modified by providing a
        % <<"service_name">> key/value entry in the ResponseInfo.
        % The ResponseInfo may also include a
        % <<"subscribe">> key/value entry to accept incoming service
        % request traffic that is sent to the socket, and/or a
        % <<"lock">> key/value boolean entry to lock the new service
        % name destination to a single remote execution thread
        % (i.e., it ties the lifetime of the socket to the lifetime
        %  of a pattern_pid() by locking onto the destination).
-define(DEFAULT_PROTOCOL_ID_CHECK,       undefined). % see below:
        % To avoid blocking on bidirectional communication requiring an
        % outgoing service request response without a request data identifier,
        % provide a function that provides the protocol's data identifier to be
        % used as a one-to-one mapping with the service request transaction id.
        % The incoming case does not need to produce an identifier, but is
        % provided for completeness (all socket data uses the function).
        % The outgoing case needs to always return a binary to be sent.
        % (incoming/outgoing shows whether it is coming into or out-of CloudI)
        % fun((incoming | outgoing, any()) ->
        %     {ID :: any(), any()} |
        %     {incoming, Request :: any()})
        % can be provided as {Module, FunctionName}
        % e.g.:
        % fun
        % (incoming, SocketBinary) ->
        %     case protocol_rpc:decode(SocketBinary) of
        %         #protocol_request{} = IncomingRequest ->
        %             {incoming, IncomingRequest};
        %         #protocol_response{id = ID} = IncomingResponse ->
        %             {ID, IncomingResponse};
        %         #protocol_error{id = ID} = IncomingResponse ->
        %             {ID, IncomingResponse}
        %     end;
        % (outgoing, #protocol_request{id = ID} = OutgoingRequest) ->
        %     {ID, protocol_rpc:encode(OutgoingRequest)};
        % (outgoing, #protocol_response{id = ID} = OutgoingResponse) ->
        %     {ID, protocol_rpc:encode(OutgoingResponse)};
        % (outgoing, #protocol_error{id = ID} = OutgoingResponse) ->
        %     {ID, protocol_rpc:encode(OutgoingResponse)}
        % end.
-define(DEFAULT_DEBUG,                       false). % log output for debugging
-define(DEFAULT_DEBUG_LEVEL,                 trace).

-ifdef(ERLANG_OTP_VERSION_16).
-type dict_proxy(_Key, _Value) :: dict().
-else.
-type dict_proxy(Key, Value) :: dict:dict(Key, Value).
-endif.

-record(state,
    {
        scope :: atom(),
        listener,
        acceptor,
        timeout_recv :: pos_integer() | infinity,
        socket_options :: list(),
        interface_formatted :: binary(),
        port_formatted :: binary(),
        service :: pid(),
        destination :: cloudi_service:service_name(),
        destination_connect :: cloudi_service:service_name() | undefined,
        destination_disconnect :: cloudi_service:service_name() | undefined,
        connection_count = 0 :: non_neg_integer(),
        connection_max :: pos_integer(),
        requests = dict:new(),
        destination_set :: boolean(),
        protocol_id_check :: undefined |
                             fun((incoming | outgoing, any()) ->
                                 {incoming | any(), any()}),
        debug_level :: off | trace | debug | info | warn | error | fatal
    }).

-record(state_socket,
    {
        socket :: port(),
        timeout_recv :: pos_integer() | infinity,
        service :: pid(),
        dispatcher :: pid(),
        context :: cloudi:context(),
        destination :: cloudi_service:service_name(),
        destination_disconnect :: cloudi_service:service_name() | undefined,
        request_info :: binary(),
        destination_set :: boolean(),
        lock :: cloudi:pattern_pid() | undefined,
        debug_level :: off | trace | debug | info | warn | error | fatal,
        protocol_id_check :: undefined |
                             fun((incoming | outgoing, any()) ->
                                 {incoming | any(), any()}),
        response_pending = false :: boolean(),
        response_timer :: reference(),
        request_pending :: cloudi:message_service_request(),
        response_lookup :: dict_proxy(any(),
                                      {cloudi:message_service_request(),
                                       reference()}) | undefined,
        recv_timeouts :: dict_proxy(cloudi:trans_id(),
                                    reference()) | undefined,
        queued :: cloudi_x_pqueue4:new() | undefined
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, _Prefix, _Timeout, Dispatcher) ->
    Defaults = [
        {ip,                       ?DEFAULT_INTERFACE},
        {port,                     ?DEFAULT_PORT},
        {destination,              ?DEFAULT_DESTINATION},
        {destination_connect,      ?DEFAULT_DESTINATION_CONNECT},
        {destination_disconnect,   ?DEFAULT_DESTINATION_DISCONNECT},
        {backlog,                  ?DEFAULT_BACKLOG},
        {nodelay,                  ?DEFAULT_NODELAY},
        {keepalive,                ?DEFAULT_KEEPALIVE},
        {recv_timeout,             ?DEFAULT_RECV_TIMEOUT},
        {max_connections,          ?DEFAULT_MAX_CONNECTIONS},
        {packet_type,              ?DEFAULT_PACKET_TYPE},
        {packet_buffer_recv_size,  ?DEFAULT_PACKET_BUFFER_RECV_SIZE},
        {packet_buffer_send_size,  ?DEFAULT_PACKET_BUFFER_SEND_SIZE},
        {packet_buffer_size,       ?DEFAULT_PACKET_BUFFER_SIZE},
        {destination_set,          ?DEFAULT_DESTINATION_SET},
        {protocol_id_check,        ?DEFAULT_PROTOCOL_ID_CHECK},
        {debug,                    ?DEFAULT_DEBUG},
        {debug_level,              ?DEFAULT_DEBUG_LEVEL}],
    [Interface, Port, Destination, DestinationConnect, DestinationDisconnect,
     Backlog, NoDelay, KeepAlive, RecvTimeout, MaxConnections, PacketType,
     BufferRecvSize, BufferSendSize, BufferSize, DestinationSet,
     ProtocolIdCheck0, Debug, DebugLevel] =
        cloudi_proplists:take_values(Defaults, Args),
    true = cloudi_service:duo_mode(Dispatcher),
    true = is_integer(Port),
    true = is_list(Destination),
    true = (DestinationConnect =:= undefined) orelse
           is_list(DestinationConnect),
    true = (DestinationDisconnect =:= undefined) orelse
           is_list(DestinationDisconnect),
    true = is_integer(Backlog),
    true = is_boolean(NoDelay),
    true = is_boolean(KeepAlive),
    true = (RecvTimeout =:= infinity) orelse
           (is_integer(RecvTimeout) andalso (RecvTimeout > 0)),
    true = (is_integer(MaxConnections) andalso (MaxConnections > 0)),
    true = lists:member(PacketType,
                        [raw, 0, 1, 2, 4, asn1, cdr, sunrm, fcgi, tpkt, line]),
    true = (BufferRecvSize =:= undefined) orelse is_integer(BufferRecvSize),
    true = (BufferSendSize =:= undefined) orelse is_integer(BufferSendSize),
    true = (BufferSize =:= undefined) orelse is_integer(BufferSize),
    true = is_boolean(DestinationSet),
    ProtocolIdCheck1 = case ProtocolIdCheck0 of
        undefined ->
            undefined;
        {ProtocolIdCheckModule, ProtocolIdCheckFunction}
            when is_atom(ProtocolIdCheckModule),
                 is_atom(ProtocolIdCheckFunction) ->
            true = erlang:function_exported(ProtocolIdCheckModule,
                                            ProtocolIdCheckFunction, 2),
            fun(ProtocolIdCheckArg1, ProtocolIdCheckArg2) ->
                ProtocolIdCheckModule:
                ProtocolIdCheckFunction(ProtocolIdCheckArg1,
                                          ProtocolIdCheckArg2)
            end;
        _ when is_function(ProtocolIdCheck0, 2) ->
            ProtocolIdCheck0
    end,
    true = ((Debug =:= true) orelse
            (Debug =:= false)),
    true = ((DebugLevel =:= trace) orelse
            (DebugLevel =:= debug) orelse
            (DebugLevel =:= info) orelse
            (DebugLevel =:= warn) orelse
            (DebugLevel =:= error) orelse
            (DebugLevel =:= fatal)),
    SocketOptions0 = [binary, {active, false},
                      {nodelay, NoDelay}, {delay_send, false},
                      {keepalive, KeepAlive}, {packet, PacketType}],
    SocketOptions1 = if
        BufferRecvSize =:= undefined ->
            SocketOptions0;
        true ->
            [{recbuf, BufferRecvSize} | SocketOptions0]
    end,
    SocketOptions2 = if
        BufferSendSize =:= undefined ->
            SocketOptions1;
        true ->
            [{sndbuf, BufferSendSize} | SocketOptions1]
    end,
    SocketOptionsN = if
        BufferSize =:= undefined ->
            SocketOptions2;
        true ->
            [{buffer, BufferSize} | SocketOptions2]
    end,
    {_, Scope} = lists:keyfind(groups_scope, 1,
                               cloudi_service:context_options(Dispatcher)),
    case gen_tcp:listen(Port, [{ip, Interface}, {backlog, Backlog},
                               {reuseaddr, true} | SocketOptionsN]) of
        {ok, Listener} ->
            case inet:sockname(Listener) of
                {ok, {InterfaceUsed, PortUsed}} ->
                    InterfaceFormatted =
                        cloudi_ip_address:to_binary(InterfaceUsed),
                    PortFormatted = erlang:integer_to_binary(PortUsed),
                    case prim_inet:async_accept(Listener, -1) of
                        {ok, Acceptor} ->
                            Service = cloudi_service:self(Dispatcher),
                            DebugLogLevel = if
                                Debug =:= false ->
                                    off;
                                Debug =:= true ->
                                    DebugLevel
                            end,
                            {ok, #state{scope = Scope,
                                        listener = Listener,
                                        acceptor = Acceptor,
                                        timeout_recv = RecvTimeout,
                                        socket_options = SocketOptionsN,
                                        interface_formatted =
                                            InterfaceFormatted,
                                        port_formatted =
                                            PortFormatted,
                                        service = Service,
                                        destination = Destination,
                                        destination_connect =
                                            DestinationConnect,
                                        destination_disconnect =
                                            DestinationDisconnect,
                                        connection_max = MaxConnections,
                                        destination_set = DestinationSet,
                                        protocol_id_check = ProtocolIdCheck1,
                                        debug_level = DebugLogLevel}};
                        {error, _} = Error ->
                            {stop, Error, #state{listener = Listener}}
                    end;
                {error, _} = Error ->
                    {stop, Error, #state{listener = Listener}}
            end;
        {error, _} = Error ->
            {stop, Error, #state{}}
    end.

cloudi_service_handle_request(_Type, _Name, _Pattern, _RequestInfo, _Request,
                              _Timeout, _Priority, _TransId, _Pid,
                              State, _Dispatcher) ->
    {reply, <<>>, State}.

cloudi_service_handle_info({inet_async, Listener, Acceptor, {ok, Socket}},
                           #state{listener = Listener,
                                  acceptor = Acceptor,
                                  connection_count = ConnectionCount,
                                  connection_max = ConnectionMax} = State,
                           _Dispatcher)
    when ConnectionCount >= ConnectionMax ->
    true = inet_db:register_socket(Socket, inet_tcp),
    catch gen_tcp:close(Socket),
    ?LOG_WARN("max_connections (~p) reached!", [ConnectionMax]),
    case prim_inet:async_accept(Listener, -1) of
        {ok, NewAcceptor} ->
            {noreply, State#state{acceptor = NewAcceptor}};
        {error, _} = Error ->
            {stop, Error, State}
    end;

cloudi_service_handle_info({inet_async, Listener, Acceptor, {ok, Socket}},
                           #state{scope = Scope,
                                  listener = Listener,
                                  acceptor = Acceptor,
                                  timeout_recv = TimeoutRecv,
                                  socket_options = SocketOptions,
                                  interface_formatted =
                                      DestinationAddressFormatted,
                                  port_formatted =
                                      DestinationPortFormatted,
                                  service = Service,
                                  destination = Destination,
                                  destination_connect =
                                      DestinationConnect,
                                  destination_disconnect =
                                      DestinationDisconnect,
                                  connection_count = ConnectionCount,
                                  destination_set = DestinationSet,
                                  protocol_id_check = ProtocolIdCheck,
                                  debug_level = DebugLogLevel} = State,
                           Dispatcher) ->
    true = inet_db:register_socket(Socket, inet_tcp),
    ok = inet:setopts(Socket, SocketOptions),
    NewConnectionCount = case inet:peername(Socket) of
        {ok, {SourceAddress, SourcePort}} ->
            SourceAddressFormatted = cloudi_ip_address:to_binary(SourceAddress),
            SourcePortFormatted = erlang:integer_to_binary(SourcePort),
            RequestInfo = cloudi_request_info:key_value_new(
                [{<<"source_address">>, SourceAddressFormatted},
                 {<<"source_port">>, SourcePortFormatted},
                 {<<"destination_address">>, DestinationAddressFormatted},
                 {<<"destination_port">>, DestinationPortFormatted}]),
            SocketPid = proc_lib:spawn_opt(fun() ->
                Context = create_context(Dispatcher),
                StateSocket = #state_socket{
                                  socket = Socket,
                                  timeout_recv = TimeoutRecv,
                                  service = Service,
                                  dispatcher = Dispatcher,
                                  context = Context,
                                  destination = Destination,
                                  destination_disconnect =
                                      DestinationDisconnect,
                                  request_info = RequestInfo,
                                  destination_set = DestinationSet,
                                  debug_level = DebugLogLevel,
                                  protocol_id_check = ProtocolIdCheck},
                socket_loop_init(Scope,
                                 DestinationSet,
                                 DestinationConnect,
                                 StateSocket)
            end, [link]),
            case gen_tcp:controlling_process(Socket, SocketPid) of
                ok = InitSuccess ->
                    SocketPid ! {init, InitSuccess};
                {error, _} = InitError ->
                    SocketPid ! {init, InitError}
            end,
            ConnectionCount + 1;
        {error, Reason} ->
            ?LOG_ERROR("socket accept error: ~p", [Reason]),
            catch gen_tcp:close(Socket),
            ConnectionCount
    end,
    case prim_inet:async_accept(Listener, -1) of
        {ok, NewAcceptor} ->
            {noreply, State#state{acceptor = NewAcceptor,
                                  connection_count = NewConnectionCount}};
        {error, _} = Error ->
            {stop, Error, State#state{connection_count = NewConnectionCount}}
    end;

cloudi_service_handle_info({inet_async, _Listener, _Acceptor, Error},
                           State, _Dispatcher) ->
    {stop, Error, State};

cloudi_service_handle_info(socket_closed,
                           #state{connection_count = ConnectionCount} = State,
                           _Dispatcher) ->
    {noreply, State#state{connection_count = ConnectionCount - 1}};

cloudi_service_handle_info(Request, State, _Dispatcher) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(_Reason, _Timeout,
                         #state{listener = Listener}) ->
    catch gen_tcp:close(Listener),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

socket_loop_init_set(true, Scope,
                     #state_socket{
                         socket = Socket,
                         dispatcher = Dispatcher,
                         context = Context0,
                         destination = Destination,
                         request_info = RequestInfo,
                         protocol_id_check = ProtocolIdCheck} = State) ->
    DestinationSetRequestInfo = cloudi_request_info:key_value_append(
        [{<<"service_name">>,
          erlang:list_to_binary(Destination)}], RequestInfo),
    Self = self(),
    case send_sync_minimal(Dispatcher, Context0,
                           Destination, DestinationSetRequestInfo,
                           <<"SET">>, Self) of
        {{ok, ResponseInfo, _Response}, Context1} ->
            KeyValues = cloudi_request_info:key_value_parse(ResponseInfo),
            NewDestination = case cloudi_key_value:find(<<"service_name">>,
                                                        KeyValues) of
                error ->
                    Destination;
                {ok, NextDestination} ->
                    NextDestination
            end,
            Subscribed = case cloudi_key_value:find(<<"subscribe">>,
                                                    KeyValues) of
                error ->
                    false;
                {ok, ServiceNamePattern} when is_binary(ServiceNamePattern) ->
                    ServiceNamePatternValue =
                        erlang:binary_to_list(ServiceNamePattern),
                    ok = cloudi_x_cpg:join(Scope, ServiceNamePatternValue,
                                           Self, infinity),
                    true
            end,
            {Lock,
             ContextN}= case cloudi_key_value:find(<<"lock">>, KeyValues) of
                error ->
                    undefined;
                {ok, <<"false">>} ->
                    undefined;
                {ok, <<"true">>} ->
                    case cloudi:get_pid(Context1, NewDestination) of
                        {{ok, LockValue}, Context2} ->
                            {LockValue, Context2};
                        {{error, timeout}, Context2} ->
                            socket_loop_terminate(lock_timeout,
                                State#state_socket{context = Context2})
                    end
            end,
            ResponseLookup = if
                Subscribed =:= true,
                ProtocolIdCheck /= undefined ->
                    dict:new();
                true ->
                    undefined
            end,
            RecvTimeouts = if
                Subscribed =:= true,
                ProtocolIdCheck =:= undefined ->
                    dict:new();
                true ->
                    undefined
            end,
            Queued = if
                Subscribed =:= true,
                ProtocolIdCheck =:= undefined ->
                    cloudi_x_pqueue4:new();
                true ->
                    undefined
            end,
            socket_terminate_check(Socket, KeyValues),
            State#state_socket{context = ContextN,
                                     destination = NewDestination,
                                     lock = Lock,
                                     response_lookup = ResponseLookup,
                                     recv_timeouts = RecvTimeouts,
                                     queued = Queued};
        {{error, timeout}, ContextN} ->
            socket_loop_terminate(set_timeout,
                State#state_socket{context = ContextN})
    end;
socket_loop_init_set(false, _, #state_socket{} = State) ->
    State#state_socket{lock = undefined}.

socket_loop_init_connect(undefined, #state_socket{} = State) ->
    State;
socket_loop_init_connect(DestinationConnect,
                         #state_socket{
                             socket = Socket,
                             dispatcher = Dispatcher,
                             context = Context,
                             request_info = RequestInfo,
                             lock = Lock} = State)
    when is_list(DestinationConnect) ->
    case send_sync_minimal(Dispatcher, Context,
                           DestinationConnect, RequestInfo,
                           <<"CONNECT">>, Lock, self()) of
        {{ok, ResponseInfo, Response}, NewContext} ->
            socket_terminate_check(Socket, ResponseInfo),
            socket_send(Response, State),
            State#state_socket{context = NewContext};
        {{error, timeout}, NewContext} ->
            socket_send(<<>>, State),
            NewState = State#state_socket{context = NewContext},
            if
                Lock =:= undefined ->
                    NewState;
                true ->
                    socket_loop_terminate(timeout, NewState)
            end
    end.

socket_loop_init(Scope, DestinationSet, DestinationConnect,
                 #state_socket{socket = Socket,
                               debug_level = DebugLevel} = State) ->
    receive
        {init, ok} ->
            NewState = socket_loop_init_connect(DestinationConnect,
                socket_loop_init_set(DestinationSet, Scope, State)),
            socket_debug_log(DebugLevel, "socket ~p opened", [Socket]),
            ok = inet:setopts(Socket, [{active, once}]),
            socket_loop(NewState);
        {init, Error} ->
            socket_loop_terminate(Error, State)
    end.

socket_loop_out_request({Type, Name, Pattern, RequestInfo, Request,
                         Timeout, Priority, TransId, Source},
                        #state_socket{
                            socket = Socket,
                            debug_level = DebugLevel,
                            protocol_id_check = undefined,
                            response_pending = false} = State)
    when is_binary(Request) ->
    ResponseTimer = erlang:send_after(Timeout, self(),
                                      response_timeout),
    T = {Type, Name, Pattern, undefined, undefined,
         Timeout, Priority, TransId, Source},
    socket_terminate_check(Socket, RequestInfo),
    socket_send(Request, State),
    socket_debug_log(DebugLevel,
                    "socket out request ~p", [Request]),
    State#state_socket{
        response_pending = true,
        response_timer = ResponseTimer,
        request_pending = T};
socket_loop_out_request({Type, Name, Pattern, RequestInfo, RequestProtocol,
                         Timeout, Priority, TransId, Source},
                        #state_socket{
                            socket = Socket,
                            debug_level = DebugLevel,
                            protocol_id_check = ProtocolIdCheck,
                            response_pending = false,
                            response_lookup = ResponseLookup} = State) ->
    {ID, Request} = ProtocolIdCheck(outgoing, RequestProtocol),
    ResponseTimer = erlang:send_after(Timeout, self(),
                                      {response_timeout, ID}),
    T = {Type, Name, Pattern, undefined, undefined,
         Timeout, Priority, TransId, Source},
    socket_terminate_check(Socket, RequestInfo),
    socket_send(Request, State),
    socket_debug_log(DebugLevel,
                    "socket out request ~p", [Request]),
    State#state_socket{
        response_lookup = dict:store(ID, {T, ResponseTimer}, ResponseLookup)};
socket_loop_out_request({_, _, _, _, Request,
                         Timeout, Priority, TransId, _} = T,
                        #state_socket{
                            protocol_id_check = undefined,
                            response_pending = true,
                            recv_timeouts = RecvTimeouts,
                            queued = Queue} = State)
    when is_binary(Request), Timeout > 0 ->
    State#state_socket{
        recv_timeouts = dict:store(TransId,
            erlang:send_after(Timeout, self(),
                {'cloudi_service_recv_timeout', Priority, TransId}),
            RecvTimeouts),
        queued = cloudi_x_pqueue4:in(T, Priority, Queue)};
socket_loop_out_request({_, _, _, _, _, _, _, _, _},
                        #state_socket{
                            protocol_id_check = undefined} = State) ->
    % ignoring service request
    State.

socket_loop_in_request(Request, ResponseF,
                       #state_socket{
                           socket = Socket,
                           dispatcher = Dispatcher,
                           context = Context,
                           destination = Destination,
                           request_info = RequestInfo,
                           lock = Lock,
                           debug_level = DebugLevel} = State) ->
    socket_debug_log(DebugLevel,
                     "socket in request ~p", [Request]),
    case send_sync_minimal(Dispatcher, Context, Destination,
                           RequestInfo, Request, Lock, self()) of
        {{ok, ResponseInfo, Response}, NewContext} ->
            socket_terminate_check(Socket, ResponseInfo),
            NewResponse = if
                ResponseF =:= undefined ->
                    Response;
                true ->
                    ResponseF(Response)
            end,
            socket_send(NewResponse, State),
            socket_debug_log(DebugLevel,
                             "socket out response ~p", [NewResponse]),
            State#state_socket{context = NewContext};
        {{error, timeout}, NewContext} ->
            socket_send(<<>>, State),
            if
                Lock =:= undefined ->
                    socket_debug_log(DebugLevel,
                                     "socket out response ~p", [<<>>]),
                    State#state_socket{context = NewContext};
                true ->
                    socket_loop_terminate(timeout, State)
            end
    end.

socket_loop_in_response({SendType, Name, Pattern, _, _,
                         _, _, TransId, Source},
                        ResponseTimer, ResponseInfo, Response,
                        DebugLevel) ->
    socket_debug_log(DebugLevel,
                     "socket in response ~p", [Response]),
    Timeout = case erlang:cancel_timer(ResponseTimer) of
        false ->
            0;
        V ->
            V
    end,
    ReturnType = if
        SendType =:= 'cloudi_service_send_async' ->
            'cloudi_service_return_async';
        SendType =:= 'cloudi_service_send_sync' ->
            'cloudi_service_return_sync'
    end,
    Source ! {ReturnType,
              Name, Pattern, ResponseInfo, Response,
              Timeout, TransId, Source},
    ok.

socket_loop_in_data(Data,
                    #state_socket{
                        request_info = ResponseInfo,
                        debug_level = DebugLevel,
                        protocol_id_check = undefined,
                        response_pending = true,
                        response_timer = ResponseTimer,
                        request_pending = T} = State) ->
    ok = socket_loop_in_response(T, ResponseTimer,
                                 ResponseInfo, Data, DebugLevel),
    State#state_socket{
        response_pending = false,
        response_timer = undefined,
        request_pending = undefined};
socket_loop_in_data(Data,
                    #state_socket{
                        protocol_id_check = undefined,
                        response_pending = false} = State) ->
    socket_loop_in_request(Data, undefined, State);
socket_loop_in_data(Data,
                    #state_socket{
                        request_info = Info,
                        debug_level = DebugLevel,
                        protocol_id_check = ProtocolIdCheck,
                        response_pending = false,
                        response_lookup = ResponseLookup} = State) ->
    {LookupID,
     LookupData, Value} = case ProtocolIdCheck(incoming, Data) of
        {incoming, Request} ->
            {undefined, undefined, Request};
        {ID, Response} ->
            case dict:find(ID, ResponseLookup) of
                {ok, ResponseData} ->
                    {ID, ResponseData, Response};
                error ->
                    {undefined, timeout, undefined}
            end
    end,
    case LookupData of
        undefined ->
            % request
            ResponseF = fun(ProtocolData) ->
                {_, ResponseDataOut} = ProtocolIdCheck(outgoing, ProtocolData),
                ResponseDataOut
            end,
            socket_loop_in_request(Value, ResponseF, State);
        timeout ->
            % a response arrived but has already timed-out
            State;
        {T, ResponseTimer} ->
            % a response to an outgoing service request that has finished
            ok = socket_loop_in_response(T, ResponseTimer,
                                         Info, Value, DebugLevel),
            State#state_socket{
                response_lookup = dict:erase(LookupID, ResponseLookup)}
    end.

socket_loop(#state_socket{
                socket = Socket,
                timeout_recv = TimeoutRecv,
                response_pending = ResponsePending,
                response_lookup = ResponseLookup,
                recv_timeouts = RecvTimeouts,
                queued = Queue} = State) ->
    receive
        {Type, _, _, _, _, _, _, _, _} = T
            when (Type =:= 'cloudi_service_send_async' orelse
                  Type =:= 'cloudi_service_send_sync') ->
            socket_loop(socket_loop_out_request(T, State));
        {tcp, Socket, Data} ->
            NewState = socket_loop_in_data(Data, State),
            ok = inet:setopts(Socket, [{active, once}]),
            socket_loop(NewState);
        {tcp_closed, Socket} ->
            socket_loop_terminate(normal, State);
        {'cloudi_service_recv_timeout', Priority, TransId} ->
            F = fun({_, {_, _, _, _, _, _, _, Id, _}}) -> Id == TransId end,
            {_, NewQueue} = cloudi_x_pqueue4:remove_unique(F, Priority, Queue),
            socket_loop(State#state_socket{
                            recv_timeouts = dict:erase(TransId, RecvTimeouts),
                            queued = NewQueue});
        response_timeout ->
            true = ResponsePending,
            socket_loop(socket_process_queue(
                State#state_socket{
                    response_pending = false,
                    response_timer = undefined,
                    request_pending = undefined}));
        {response_timeout, ID} ->
            false = ResponsePending,
            socket_loop(State#state_socket{
                            response_lookup = dict:erase(ID, ResponseLookup)})
    after
        TimeoutRecv ->
            socket_loop_terminate(normal, State)
    end.

socket_loop_terminate(Reason,
                      #state_socket{
                          socket = Socket,
                          service = Service,
                          dispatcher = Dispatcher,
                          context = Context,
                          destination_disconnect = DestinationDisconnect,
                          request_info = RequestInfo,
                          debug_level = DebugLevel}) ->
    if
        is_list(DestinationDisconnect) ->
            send_async_minimal(Dispatcher, Context,
                               DestinationDisconnect,
                               RequestInfo, <<"DISCONNECT">>, self());
        true ->
            ok
    end,
    if
        Reason =:= normal ->
            socket_debug_log(DebugLevel, "socket ~p closed", [Socket]);
        true ->
            ?LOG_ERROR("socket ~p error: ~p", [Socket, Reason])
    end,
    catch gen_tcp:close(Socket),
    Service ! socket_closed,
    erlang:unlink(Service),
    erlang:exit(Reason).

socket_terminate_check(Socket, Info) ->
    KeyValues = cloudi_request_info:key_value_parse(Info),
    case cloudi_key_value:find(<<"connection">>, KeyValues) of
        {ok, <<"close">>} ->
            self() ! {tcp_closed, Socket},
            ok;
        error ->
            ok
    end.

socket_send(Outgoing,
            #state_socket{socket = Socket} = State) ->
    case gen_tcp:send(Socket, Outgoing) of
        ok ->
            ok;
        {error, Reason} ->
            ?LOG_WARN("socket ~p send error: ~p", [Socket, Reason]),
            socket_loop_terminate(normal, State)
    end.

socket_debug_log(off, _, _) ->
    ok;
socket_debug_log(trace, Message, Args) ->
    ?LOG_TRACE(Message, Args);
socket_debug_log(debug, Message, Args) ->
    ?LOG_DEBUG(Message, Args);
socket_debug_log(info, Message, Args) ->
    ?LOG_INFO(Message, Args);
socket_debug_log(warn, Message, Args) ->
    ?LOG_WARN(Message, Args);
socket_debug_log(error, Message, Args) ->
    ?LOG_ERROR(Message, Args);
socket_debug_log(fatal, Message, Args) ->
    ?LOG_FATAL(Message, Args).

socket_process_queue(#state_socket{recv_timeouts = RecvTimeouts,
                                   queued = Queue} = State) ->
    case cloudi_x_pqueue4:out(Queue) of
        {empty, NewQueue} ->
            State#state_socket{queued = NewQueue};
        {{value, {Type, Name, Pattern, RequestInfo, Request,
                  _, Priority, TransId, Pid}}, NewQueue} ->
            Timeout = case erlang:cancel_timer(dict:fetch(TransId,
                                                          RecvTimeouts)) of
                false ->
                    0;
                V ->
                    V
            end,
            NewRecvTimeouts = dict:erase(TransId, RecvTimeouts),
            socket_loop_out_request({Type, Name, Pattern, RequestInfo, Request,
                                     Timeout, Priority, TransId, Pid},
                                    State#state_socket{
                                        recv_timeouts = NewRecvTimeouts,
                                        queued = NewQueue})
    end.

