%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Basic CloudI UDP Integration==
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
%%% @version 1.4.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_udp).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_service).

%% external interface

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-define(DEFAULT_INTERFACE,            {127,0,0,1}). % ip address
-define(DEFAULT_PORT,                        8081).
-define(DEFAULT_DESTINATION,            undefined). % service name

-record(state,
    {
        socket,
        interface,
        interface_formatted,
        port,
        port_formatted,
        destination,
        requests = dict:new()
    }).

-record(request,
    {
        source_address,
        source_port
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, _Prefix, _Timeout, _Dispatcher) ->
    Defaults = [
        {ip,                       ?DEFAULT_INTERFACE},
        {port,                     ?DEFAULT_PORT},
        {destination,              ?DEFAULT_DESTINATION}],
    [Interface, Port, Destination] =
        cloudi_proplists:take_values(Defaults, Args),
    true = is_integer(Port),
    true = is_list(Destination),
    case gen_udp:open(Port, [{ip, Interface}, {active, true}, binary]) of
        {ok, Socket} ->
            case inet:sockname(Socket) of
                {ok, {InterfaceUsed, PortUsed}} ->
                    InterfaceFormatted =
                        cloudi_ip_address:to_binary(InterfaceUsed),
                    PortFormatted = erlang:integer_to_binary(PortUsed),
                    {ok, #state{socket = Socket,
                                interface = InterfaceUsed,
                                interface_formatted = InterfaceFormatted,
                                port = PortUsed,
                                port_formatted = PortFormatted,
                                destination = Destination}};
                {error, _} = Error ->
                    {stop, Error, #state{socket = Socket}}
            end;
        {error, _} = Error ->
            {stop, Error, #state{}}
    end.

cloudi_service_handle_request(_Type, _Name, _Pattern, _RequestInfo, _Request,
                              _Timeout, _Priority, _TransId, _Pid,
                              State, _Dispatcher) ->
    {reply, <<>>, State}.

cloudi_service_handle_info({udp, Socket, SourceAddress, SourcePort, Request},
                           #state{socket = Socket,
                                  interface_formatted =
                                      DestinationAddressFormatted,
                                  port_formatted =
                                      DestinationPortFormatted,
                                  destination = Name,
                                  requests = Requests} = State, Dispatcher) ->
    SourceAddressFormatted = cloudi_ip_address:to_binary(SourceAddress),
    SourcePortFormatted = erlang:integer_to_binary(SourcePort),
    RequestInfo = cloudi_service:request_info_key_value_new(
        [{<<"source_address">>, SourceAddressFormatted},
         {<<"source_port">>, SourcePortFormatted},
         {<<"destination_address">>, DestinationAddressFormatted},
         {<<"destination_port">>, DestinationPortFormatted}]),
    case cloudi_service:send_async_active(Dispatcher, Name,
                                          RequestInfo, Request,
                                          undefined, undefined) of
        {ok, TransId} ->
            RequestData = #request{source_address = SourceAddress,
                                   source_port = SourcePort},
            {noreply, State#state{requests = dict:store(TransId, RequestData,
                                                        Requests)}};
        {error, Reason} ->
            ?LOG_ERROR("dropped incoming udp packet: ~p", [Reason]),
            {noreply, State}
    end;

cloudi_service_handle_info({return_async_active, _Name, _Pattern,
                            _ResponseInfo, Response, _Timeout, TransId},
                           #state{socket = Socket,
                                  requests = Requests} = State, _Dispatcher) ->
    #request{source_address = SourceAddress,
             source_port = SourcePort} = dict:fetch(TransId, Requests),
    send(Socket, SourceAddress, SourcePort, Response),
    {noreply, State#state{requests = dict:erase(TransId, Requests)}};

cloudi_service_handle_info({timeout_async_active, TransId},
                           #state{socket = Socket,
                                  requests = Requests} = State, _Dispatcher) ->
    #request{source_address = SourceAddress,
             source_port = SourcePort} = dict:fetch(TransId, Requests),
    send(Socket, SourceAddress, SourcePort, <<>>),
    {noreply, State#state{requests = dict:erase(TransId, Requests)}};

cloudi_service_handle_info(Request, State, _Dispatcher) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(_Reason, _Timeout,
                         #state{socket = Socket}) ->
    catch gen_udp:close(Socket),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

send(Socket, SourceAddress, SourcePort, Response) ->
    case gen_udp:send(Socket, SourceAddress, SourcePort, Response) of
        ok ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR("dropped outgoing udp packet: ~p", [Reason]),
            ok
    end.

