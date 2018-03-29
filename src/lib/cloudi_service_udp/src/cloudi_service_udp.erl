%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Basic CloudI UDP Integration==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2013-2017 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2013-2017 Michael Truog
%%% @version 1.7.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_udp).
-author('mjtruog at protonmail dot com').

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
        requests = #{}
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

cloudi_service_init(Args, _Prefix, _Timeout, Dispatcher) ->
    Defaults = [
        {ip,                       ?DEFAULT_INTERFACE},
        {port,                     ?DEFAULT_PORT},
        {destination,              ?DEFAULT_DESTINATION}],
    [Interface, Port, Destination] =
        cloudi_proplists:take_values(Defaults, Args),
    1 = cloudi_service:process_count_max(Dispatcher),
    true = cloudi_service:duo_mode(Dispatcher),
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
    RequestInfo = cloudi_request_info:key_value_new(
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
            {noreply, State#state{requests = maps:put(TransId, RequestData,
                                                      Requests)}};
        {error, Reason} ->
            ?LOG_ERROR("dropped incoming udp packet: ~p", [Reason]),
            {noreply, State}
    end;

cloudi_service_handle_info({return_async_active, _Name, _Pattern,
                            _ResponseInfo, Response, _Timeout, TransId},
                           #state{socket = Socket,
                                  requests = Requests} = State, _Dispatcher) ->
    {#request{source_address = SourceAddress,
              source_port = SourcePort},
     NewRequests} = maps:take(TransId, Requests),
    send(Socket, SourceAddress, SourcePort, Response),
    {noreply, State#state{requests = NewRequests}};

cloudi_service_handle_info({timeout_async_active, TransId},
                           #state{socket = Socket,
                                  requests = Requests} = State, _Dispatcher) ->
    {#request{source_address = SourceAddress,
              source_port = SourcePort},
     NewRequests} = maps:take(TransId, Requests),
    send(Socket, SourceAddress, SourcePort, <<>>),
    {noreply, State#state{requests = NewRequests}};

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

