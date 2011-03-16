%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Nodes==
%%% Manage node connections to provide reliability after network interruptions.
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
%%% @version 0.1.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_nodes).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).

%% external interface
-export([start_link/1,
         reconfigure/2,
         alive/0,
         dead/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("cloudi_logger.hrl").
-include("cloudi_constants.hrl").
-include("cloudi_configuration.hrl").

-record(state,
    {
        nodes_alive = [],
        nodes_dead = [],
        timer_reconnect = undefined
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

reconfigure(Config, Timeout) ->
    gen_server:call(?MODULE, {reconfigure, Config,
                              Timeout - ?TIMEOUT_DELTA}, Timeout).

alive() ->
    gen_server:call(?MODULE, alive).

dead() ->
    gen_server:call(?MODULE, dead).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([Config]) ->
    net_kernel:monitor_nodes(true, [{node_type, visible}, nodedown_reason]),
    self() ! reconnect,
    {ok, #state{nodes_dead = Config#config.nodes}}.

handle_call({reconfigure, Config, _}, _,
            #state{nodes_alive = NodesAlive,
                   timer_reconnect = TimerReconnect} = State) ->
    NewNodesDead = lists:foldl(fun(N, L) ->
        case lists2:delete_checked(N, L) of
            false ->
                % node is alive, but is no longer configured
                net_kernel:disconnect(N),
                L;
            NewL ->
                NewL
        end
    end, Config#config.nodes, NodesAlive),
    NewNodesAlive = lists:filter(fun(N) ->
        not lists:member(N, NewNodesDead)
    end, Config#config.nodes),
    NewTimerReconnect = if
        NewNodesDead /= [], TimerReconnect == undefined ->
            erlang:send_after(?NODE_RECONNECT, self(), reconnect);
        true ->
            TimerReconnect
    end,
    {reply, ok, State#state{nodes_dead = NewNodesDead,
                            nodes_alive = NewNodesAlive,
                            timer_reconnect = NewTimerReconnect}};

handle_call(alive, _,
            #state{nodes_alive = NodesAlive} = State) ->
    {reply, NodesAlive, State};

handle_call(dead, _,
            #state{nodes_dead = NodesDead} = State) ->
    {reply, NodesDead, State};

handle_call(Request, _, State) ->
    ?LOG_WARN("Unknown call \"~p\"", [Request]),
    {stop, string2:format("Unknown call \"~p\"", [Request]), error, State}.

handle_cast(Request, State) ->
    ?LOG_WARN("Unknown cast \"~p\"", [Request]),
    {noreply, State}.

handle_info({'nodeup', Node, InfoList},
            #state{nodes_alive = NodesAlive,
                   nodes_dead = NodesDead} = State) ->
    ?LOG_INFO("nodeup ~p ~p", [Node, InfoList]),
    {noreply, State#state{nodes_alive = [Node | NodesAlive],
                          nodes_dead = lists2:delete_all(Node, NodesDead)}};

handle_info({'nodedown', Node, InfoList},
            #state{nodes_alive = NodesAlive,
                   nodes_dead = NodesDead,
                   timer_reconnect = TimerReconnect} = State) ->
    ?LOG_INFO("nodedown ~p ~p", [Node, InfoList]),
    NewTimerReconnect = if
        TimerReconnect == undefined ->
            erlang:send_after(?NODE_RECONNECT, self(), reconnect);
        true ->
            TimerReconnect
    end,
    {noreply, State#state{nodes_alive = lists:delete(Node, NodesAlive),
                          nodes_dead = [Node | NodesDead],
                          timer_reconnect = NewTimerReconnect}};

handle_info(reconnect, #state{nodes_dead = NodesDead} = State) ->
    if
        NodesDead /= [] ->
            ?LOG_INFO("currently dead nodes ~p", [NodesDead]),
            lists:foreach(fun(Node) ->
                net_kernel:connect_node(Node)
            end, NodesDead),
            {noreply,
             State#state{timer_reconnect = erlang:send_after(?NODE_RECONNECT,
                                                             self(),
                                                             reconnect)}};
        true ->
            {noreply, State}
    end;

handle_info(Request, State) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

