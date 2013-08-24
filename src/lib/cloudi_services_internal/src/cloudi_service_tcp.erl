%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Basic CloudI TCP Integration==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2013, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2013 Michael Truog
%%% @version 1.3.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_tcp).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_service).

%% external interface

%% cloudi_service callbacks
-export([cloudi_service_init/3,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/2]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-define(DEFAULT_INTERFACE,            {127,0,0,1}). % ip address
-define(DEFAULT_PORT,                        8080).
-define(DEFAULT_DESTINATION,            undefined). % service name
-define(DEFAULT_BACKLOG,                      128).
-define(DEFAULT_NODELAY,                     true).
-define(DEFAULT_KEEPALIVE,                   true).
-define(DEFAULT_MAX_CONNECTIONS,             4096).
-define(DEFAULT_PACKET_TYPE,                 line). % gen_tcp:listen/2 option

-record(state,
    {
        listener,
        acceptor,
        service,
        timeout_async,
        socket_options,
        interface_formatted,
        port_formatted,
        destination,
        connection_count = 0,
        connection_max,
        requests = dict:new()
    }).

-record(state_socket,
    {
        service,
        socket,
        timeout_async,
        request_info
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, _Prefix, Dispatcher) ->
    Defaults = [
        {ip,                       ?DEFAULT_INTERFACE},
        {port,                     ?DEFAULT_PORT},
        {destination,              ?DEFAULT_DESTINATION},
        {backlog,                  ?DEFAULT_BACKLOG},
        {nodelay,                  ?DEFAULT_NODELAY},
        {keepalive,                ?DEFAULT_KEEPALIVE},
        {max_connections,          ?DEFAULT_MAX_CONNECTIONS},
        {packet_type,              ?DEFAULT_PACKET_TYPE}],
    [Interface, Port, Destination, Backlog, NoDelay, KeepAlive,
     MaxConnections, PacketType] =
        cloudi_proplists:take_values(Defaults, Args),
    true = is_integer(Port),
    true = is_list(Destination),
    true = is_integer(Backlog),
    true = is_boolean(NoDelay),
    true = is_boolean(KeepAlive),
    true = (is_integer(MaxConnections) andalso (MaxConnections > 0)),
    true = lists:member(PacketType,
                        [raw, 0, 1, 2, 4, asn1, cdr, sunrm, fcgi, tpkt, line]),
    Service = cloudi_service:self(Dispatcher),
    TimeoutAsync = cloudi_service:timeout_async(Dispatcher),
    SocketOptions = [{ip, Interface}, {active, false}, binary,
                     {reuseaddr, true}, {backlog, Backlog},
                     {nodelay, NoDelay}, {keepalive, KeepAlive},
                     {packet, PacketType}],
    case gen_tcp:listen(Port, SocketOptions) of
        {ok, Listener} ->
            case inet:sockname(Listener) of
                {ok, {InterfaceUsed, PortUsed}} ->
                    InterfaceFormatted = ip_address_binary(InterfaceUsed),
                    PortFormatted = erlang:integer_to_binary(PortUsed),
                    case prim_inet:async_accept(Listener, -1) of
                        {ok, Acceptor} ->
                            {ok, #state{listener = Listener,
                                        acceptor = Acceptor,
                                        service = Service,
                                        timeout_async = TimeoutAsync,
                                        socket_options = SocketOptions,
                                        interface_formatted =
                                            InterfaceFormatted,
                                        port_formatted =
                                            PortFormatted,
                                        connection_max = MaxConnections,
                                        destination = Destination}};
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

cloudi_service_handle_info({inet_async, _Listener, _Acceptor, {ok, Socket}},
                           #state{connection_count = ConnectionCount,
                                  connection_max = ConnectionMax} = State,
                           _Dispatcher)
    when ConnectionCount >= ConnectionMax ->
    catch gen_tcp:close(Socket),
    ?LOG_WARN("max_connections (~p) reached!", [ConnectionMax]),
    {noreply, State};

cloudi_service_handle_info({inet_async, _Listener, _Acceptor, {ok, Socket}},
                           #state{service = Service,
                                  timeout_async = TimeoutAsync,
                                  socket_options = SocketOptions,
                                  interface_formatted =
                                      DestinationAddressFormatted,
                                  port_formatted =
                                      DestinationPortFormatted,
                                  connection_count = ConnectionCount} = State,
                           _Dispatcher) ->
    true = inet_db:register_socket(Socket, inet_tcp),
    ok = inet:setopts(Socket, SocketOptions),
    case inet:peername(Socket) of
        {ok, {SourceAddress, SourcePort}} ->
            SourceAddressFormatted = ip_address_binary(SourceAddress),
            SourcePortFormatted = erlang:integer_to_binary(SourcePort),
            RequestInfo = cloudi_service:request_info_key_value_new(
                [{<<"source_address">>, SourceAddressFormatted},
                 {<<"source_port">>, SourcePortFormatted},
                 {<<"destination_address">>, DestinationAddressFormatted},
                 {<<"destination_port">>, DestinationPortFormatted}]),
            SocketPid = proc_lib:spawn_opt(fun() ->
                socket_loop_init(#state_socket{service = Service,
                                               socket = Socket,
                                               timeout_async = TimeoutAsync,
                                               request_info = RequestInfo})
            end, [link]),
            case gen_tcp:controlling_process(Socket, SocketPid) of
                ok = Success ->
                    SocketPid ! {init, Success},
                    {noreply,
                     State#state{connection_count = ConnectionCount + 1}};
                {error, _} = Error ->
                    SocketPid ! {init, Error},
                    {noreply, State}
            end;
        {error, Reason} ->
            ?LOG_ERROR("socket accept error: ~p", [Reason]),
            catch gen_tcp:close(Socket),
            {noreply, State}
    end;

cloudi_service_handle_info({inet_async, _Listener, _Acceptor, Error},
                           State, _Dispatcher) ->
    {stop, Error, State};

cloudi_service_handle_info({tcp_request, TcpPid, RequestInfo, Request},
                           #state{destination = Name,
                                  requests = Requests} = State, Dispatcher) ->
    case cloudi_service:send_async_active(Dispatcher, Name,
                                          RequestInfo, Request,
                                          undefined, undefined) of
        {ok, TransId} ->
            {noreply, State#state{requests = dict:store(TransId, TcpPid,
                                                        Requests)}};
        {error, Reason} ->
            ?LOG_ERROR("dropped incoming tcp request: ~p", [Reason]),
            {noreply, State}
    end;

cloudi_service_handle_info({return_async_active, _Name, _Pattern,
                            _ResponseInfo, Response, _Timeout, TransId},
                           #state{requests = Requests} = State, _) ->
    TcpPid = dict:fetch(TransId, Requests),
    TcpPid ! {tcp_response, Response},
    {noreply, State#state{requests = dict:erase(TransId, Requests)}};

cloudi_service_handle_info({timeout_async_active, TransId},
                           #state{requests = Requests} = State, _) ->
    TcpPid = dict:fetch(TransId, Requests),
    TcpPid ! {tcp_response, <<>>},
    {noreply, State#state{requests = dict:erase(TransId, Requests)}};

cloudi_service_handle_info(Request, State, _) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(_, #state{listener = Listener}) ->
    catch gen_tcp:close(Listener),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

ip_address_binary({B1, B2, B3, B4}) ->
    cloudi_string:format_to_binary("~3..0b.~3..0b.~3..0b.~3..0b",
                                   [B1, B2, B3, B4]);
ip_address_binary({S1, S2, S3, S4, S5, S6, S7, S8}) ->
    cloudi_string:format_to_binary("~4.16.0b:~4.16.0b:~4.16.0b:~4.16.0b:"
                                   "~4.16.0b:~4.16.0b:~4.16.0b:~4.16.0b",
                                   [S1, S2, S3, S4, S5, S6, S7, S8]).

socket_loop_init(#state_socket{service = Service,
                               socket = Socket} = StateSocket) ->
    receive
        {init, ok} ->
            ok = inet:setopts(Socket, [{active, once}]),
            socket_loop(StateSocket);
        {init, {error, Reason} = Error} ->
            ?LOG_ERROR("socket creation error: ~p", [Reason]),
            catch gen_tcp:close(Socket),
            erlang:unlink(Service),
            erlang:exit(Error)
    end.

socket_loop(#state_socket{service = Service,
                          socket = Socket,
                          timeout_async = TimeoutAsync,
                          request_info = RequestInfo} = StateSocket) ->
    receive
        {tcp, Socket, Request} ->
            Service ! {tcp_request, self(), RequestInfo, Request},
            receive
                {tcp_response, Response} ->
                    gen_tcp:send(Socket, Response)
            after
                TimeoutAsync ->
                    ok
            end,
            ok = inet:setopts(Socket, [{active, once}]),
            socket_loop(StateSocket);
        {tcp_closed, Socket} ->
            catch gen_tcp:close(Socket),
            erlang:unlink(Service),
            erlang:exit(socket_closed)
    end.

