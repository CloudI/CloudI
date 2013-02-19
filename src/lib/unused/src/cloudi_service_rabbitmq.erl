%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI RabbitMQ Integration==
%%% Provide a way of sending requests through RabbitMQ.
%%% This code has not been used yet and is just a decent starting place for
%%% future development.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2011, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011 Michael Truog
%%% @version 0.1.3 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_rabbitmq).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_service).

%% external interface

%% cloudi_service callbacks
-export([cloudi_service_init/3,
         cloudi_service_handle_request/8,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/2]).

-include("cloudi_logger.hrl").
-include("amqp_client.hrl").

-define(DEFAULT_PORT,         ?PROTOCOL_PORT).
-define(DEFAULT_USER_NAME,           "guest").
-define(DEFAULT_PASSWORD,            "guest").
-define(DEFAULT_HOST_NAME,       "localhost").
-define(DEFAULT_VIRTUAL_HOST,            "/").


-record(state,
    {
        connection,
        channel,
        queue_out,
        queue_in,
        exchange,
        replies = dict:new()
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, Prefix, Dispatcher) ->
    Defaults = [
        {port,                      ?DEFAULT_PORT},
        {username,             ?DEFAULT_USER_NAME},
        {password,              ?DEFAULT_PASSWORD},
        {hostname,             ?DEFAULT_HOST_NAME},
        {virtualhost,       ?DEFAULT_VIRTUAL_HOST}],
    [Port, UserName, Password, HostName, VirtualHost | L] =
        proplists2:take_values(Defaults, Args),
    Parameters = #amqp_params{port = Port,
                              username = list_to_binary(UserName),
                              password = list_to_binary(Password),
                              hostname = list_to_binary(HostName),
                              virtualhost = list_to_binary(VirtualHost),
                              node = rabbit_misc:makenode(rabbit)},
    case amqp_connection:start(direct, Parameters) of
        {ok, Connection} ->
            {ok, Channel} = amqp_connection:open_channel(Connection),
            amqp_channel:register_return_handler(Channel, self()),
            {QueueOut, QueueIn} = prefix2queues(Prefix),
            #'queue.declare_ok'{} =
            amqp_channel:call(Channel,
                              #'queue.declare'{queue = QueueOut}),
            #'queue.declare_ok'{} =
            amqp_channel:call(Channel,
                              #'queue.declare'{queue = QueueIn}),
            Exchange = erlang:term_to_binary(self()),
            #'exchange.declare_ok'{} = 
            amqp_channel:call(Channel,
                              #'exchange.declare'{exchange = Exchange}),
            amqp_channel:subscribe(Channel,
                                   #'basic.consume'{queue = QueueIn}, self()),
            lists:foreach(fun({name, Name}) ->
                RoutingKey = name2routingkey(Prefix ++ Name),
                #'queue.bind_ok'{} = 
                amqp_channel:call(Channel,
                                  #'queue.bind'{queue = QueueOut,
                                                exchange = Exchange,
                                                routing_key = RoutingKey}),
                cloudi_service:subscribe(Dispatcher, Name)
            end, L),
            {ok, #state{connection = Connection,
                        channel = Channel,
                        queue_out = QueueOut,
                        queue_in = QueueIn,
                        exchange = Exchange}};
        {error, Reason} ->
            {stop, Reason}
    end.

cloudi_service_handle_request(Type, Name, Request, Timeout, TransId, Pid,
                          #state{channel = Channel,
                                 queue_in = QueueIn,
                                 exchange = Exchange,
                                 replies = Replies} = State, Dispatcher) ->
    true = is_binary(Request),
    RoutingKey = name2routingkey(Name),
    Properties = #'P_basic'{correlation_id = TransId,
                            content_type = <<"application/octet-stream">>,
                            reply_to = QueueIn},
    amqp_channel:call(Channel,
                      #'basic.publish'{exchange = Exchange,
                                       routing_key = RoutingKey,
                                       mandatory = true},
                      #amqp_msg{props = Properties,
                                payload = Request}),
    F = fun(Response) ->
        cloudi_service:return_nothrow(Dispatcher, Type, Name, Response,
                                  Timeout, TransId, Pid)
    end,
    {noreply, State#state{replies = dict:store(TransId, F, Replies)}}.

cloudi_service_handle_info({#'basic.return'{},
                       #amqp_msg{props = #'P_basic'{correlation_id = TransId}}},
                       #state{replies = Replies} = State) ->
    {noreply, State#state{replies = dict:erase(TransId, Replies)}};

cloudi_service_handle_info({#'basic.deliver'{delivery_tag = DeliveryTag},
                        #amqp_msg{props = #'P_basic'{correlation_id = TransId},
                                  payload = Response}},
                       #state{channel = Channel,
                              replies = Replies} = State) ->
    amqp_channel:call(Channel, #'basic.ack'{delivery_tag = DeliveryTag}),
    case dict:find(TransId, Replies) of
        {ok, F} ->
            F(Response),
            {noreply, State#state{replies = dict:erase(TransId, Replies)}};
        error ->
            {noreply, State}
    end;

cloudi_service_handle_info(Request, State, _) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(_, #state{connection = Connection,
                               channel = Channel}) ->
    amqp_channel:close(Channel),
    amqp_connection:close(Connection),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

prefix2queues([$/ | L]) ->
    RabbitMQ = name2rabbitmq(L, "cloudi."),
    {erlang:list_to_binary(RabbitMQ),
     erlang:iolist_to_binary([RabbitMQ, ".reply"])}.

name2routingkey([$/ | L]) ->
    erlang:list_to_binary(name2rabbitmq(L, "")).

name2rabbitmq([], Out) ->
    lists:reverse(Out);
name2rabbitmq([$/], Out) ->
    lists:reverse(Out);
name2rabbitmq([$/ | L], Out) ->
    name2rabbitmq(L, [$. | Out]);
name2rabbitmq([C | L], Out) ->
    name2rabbitmq(L, [C | Out]).

