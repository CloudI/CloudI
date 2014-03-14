%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Nodes==
%%% Manage node connections to provide reliability after network interruptions.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2011-2014, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011-2014 Michael Truog
%%% @version 1.3.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_nodes).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).

%% external interface
-export([start_link/1,
         reconfigure/2,
         alive/1,
         dead/1,
         nodes/1,
         logger_redirect/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("cloudi_logger.hrl").
-include("cloudi_constants.hrl").
-include("cloudi_configuration.hrl").

-record(state,
    {
        nodes_alive = [] :: list(node()),
        nodes_dead :: list(node()),
        nodes :: list(node()),
        logger_redirect :: node() | undefined,
        reconnect_interval :: pos_integer(),
        reconnect_timer,
        discovery :: #config_nodes_discovery{} | undefined
    }).

-define(CATCH_EXIT(F),
        try F catch exit:{Reason, _} -> {error, Reason} end).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

reconfigure(Config, Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE, {reconfigure, Config}, Timeout)).

alive(Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE, alive, Timeout)).

dead(Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE, dead, Timeout)).

nodes(Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE, nodes, Timeout)).

logger_redirect(Node) when is_atom(Node) ->
    gen_server:cast(?MODULE, {logger_redirect, Node}).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([#config{logging = #config_logging{redirect = NodeLogger},
              nodes = #config_nodes{nodes = Nodes,
                                    reconnect_start = ReconnectStart,
                                    reconnect_delay = ReconnectDelay,
                                    discovery = Discovery}}]) ->
    net_kernel:monitor_nodes(true, [{node_type, visible}, nodedown_reason]),
    NewNodeLogger = if
        NodeLogger == node(); NodeLogger =:= undefined ->
            undefined;
        true ->
            NodeLogger
    end,
    if
        NewNodeLogger =/= undefined ->
            case lists:member(NewNodeLogger, Nodes) of
                true ->
                    ok;
                false ->
                    ?LOG_WARN("unable to control log output redirection "
                              "to unmonitored node (~p)",
                              [NewNodeLogger])
            end;
        true ->
            ok
    end,
    discovery_start(Discovery),
    ReconnectInterval = ReconnectDelay * 1000,
    ReconnectTimer = erlang:send_after(ReconnectStart * 1000,
                                       self(), reconnect),
    {ok, #state{nodes_dead = Nodes,
                nodes = Nodes,
                logger_redirect = NewNodeLogger,
                reconnect_interval = ReconnectInterval,
                reconnect_timer = ReconnectTimer,
                discovery = Discovery}}.

handle_call({reconfigure,
             #config{logging = #config_logging{redirect = NodeLogger},
                     nodes = #config_nodes{nodes = Nodes,
                                           reconnect_delay = ReconnectDelay,
                                           discovery = Discovery}}}, _,
            #state{nodes_alive = NodesAlive,
                   discovery = OldDiscovery} = State) ->
    NewNodes = lists:usort(Nodes ++ nodes()),
    NewNodesDead = lists:foldl(fun(N, L) ->
        case cloudi_lists:delete_checked(N, L) of
            false ->
                % node is alive, but is no longer configured
                net_kernel:disconnect(N),
                L;
            NewL ->
                NewL
        end
    end, NewNodes, NodesAlive),
    NewNodesAlive = lists:filter(fun(N) ->
        not lists:member(N, NewNodesDead)
    end, NewNodes),
    ReconnectInterval = ReconnectDelay * 1000,
    logger_redirect(NodeLogger),
    discovery_update(OldDiscovery, Discovery),
    {reply, ok, State#state{nodes_alive = NewNodesAlive,
                            nodes_dead = NewNodesDead,
                            nodes = NewNodes,
                            reconnect_interval = ReconnectInterval,
                            discovery = Discovery}};

handle_call(alive, _,
            #state{nodes_alive = NodesAlive} = State) ->
    {reply, {ok, NodesAlive}, State};

handle_call(dead, _,
            #state{nodes_dead = NodesDead} = State) ->
    {reply, {ok, NodesDead}, State};

handle_call(nodes, _,
            #state{nodes = Nodes} = State) ->
    {reply, {ok, Nodes}, State};

handle_call(Request, _, State) ->
    ?LOG_WARN("Unknown call \"~p\"", [Request]),
    {stop, cloudi_string:format("Unknown call \"~p\"", [Request]),
     error, State}.

handle_cast({logger_redirect, NodeLogger},
            #state{nodes_alive = NodesAlive,
                   nodes_dead = NodesDead,
                   logger_redirect = OldNodeLogger} = State) ->
    NewNodeLogger = if
        NodeLogger == node(); NodeLogger =:= undefined ->
            undefined;
        true ->
            NodeLogger
    end,
    if
        NewNodeLogger /= OldNodeLogger ->
            if
                NewNodeLogger =:= undefined ->
                    cloudi_logger:redirect(undefined);
                true ->
                    case lists:member(NewNodeLogger, NodesAlive) of
                        true ->
                            cloudi_logger:redirect(NewNodeLogger);
                        false ->
                            case lists:member(NewNodeLogger, NodesDead) of
                                true ->
                                    ?LOG_INFO("redirecting log output to ~p "
                                              "after it reconnects",
                                              [NewNodeLogger]);
                                false ->
                                    ?LOG_WARN("unable to redirect log output "
                                              "to an unmonitored node (~p)",
                                              [NewNodeLogger])
                            end
                    end
            end,
            {noreply, State#state{logger_redirect = NewNodeLogger}};
        true ->
            {noreply, State}
    end;

handle_cast(Request, State) ->
    ?LOG_WARN("Unknown cast \"~p\"", [Request]),
    {stop, cloudi_string:format("Unknown cast \"~p\"", [Request]), State}.

handle_info({'nodeup', Node, InfoList},
            #state{nodes_alive = NodesAlive,
                   nodes_dead = NodesDead,
                   nodes = Nodes,
                   logger_redirect = NodeLogger} = State) ->
    if
        Node == NodeLogger ->
            cloudi_logger:redirect(NodeLogger);
        true ->
            ok
    end,
    ?LOG_INFO("nodeup ~p ~p", [Node, InfoList]),
    {noreply,
     State#state{nodes_alive = lists:umerge(NodesAlive, [Node]),
                 nodes_dead = lists:delete(Node, NodesDead),
                 nodes = lists:umerge(Nodes, [Node])}};

handle_info({'nodedown', Node, InfoList},
            #state{nodes_alive = NodesAlive,
                   nodes_dead = NodesDead,
                   logger_redirect = NodeLogger} = State) ->
    if
        Node == NodeLogger ->
            cloudi_logger:redirect(undefined);
        true ->
            ok
    end,
    ?LOG_INFO("nodedown ~p ~p", [Node, InfoList]),
    {noreply, State#state{nodes_alive = lists:delete(Node, NodesAlive),
                          nodes_dead = lists:umerge(NodesDead, [Node])}};

handle_info(reconnect,
            #state{nodes_dead = NodesDead,
                   reconnect_interval = ReconnectInterval,
                   discovery = Discovery} = State) ->
    discovery_check(Discovery),
    if
        NodesDead /= [] ->
            ?LOG_INFO("currently dead nodes ~p", [NodesDead]),
            pforeach(fun(Node) ->
                % avoid the possibly long synchronous call here
                net_kernel:connect_node(Node)
            end, NodesDead);
        true ->
            ok
    end,
    ReconnectTimer = erlang:send_after(ReconnectInterval, self(), reconnect),
    {noreply, State#state{reconnect_timer = ReconnectTimer}};

handle_info(Request, State) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {stop, cloudi_string:format("Unknown cast \"~p\"", [Request]), State}.

terminate(_, #state{discovery = Discovery}) ->
    discovery_stop(Discovery),
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

discovery_start(undefined) ->
    ok;
discovery_start(#config_nodes_discovery{module = Module,
                                        start_f = StartF,
                                        start_a = StartA}) ->
    case erlang:apply(Module, StartF, StartA) of
        {ok, _} ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR("~p:~p error: ~p", [Module, StartF, Reason])
    end,
    ok.

discovery_check(undefined) ->
    ok;
discovery_check(#config_nodes_discovery{module = Module,
                                        discover_f = DiscoverF,
                                        discover_a = DiscoverA}) ->
    case erlang:apply(Module, DiscoverF, DiscoverA) of
        ok ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR("~p:~p error: ~p", [Module, DiscoverF, Reason])
    end,
    ok.

discovery_stop(undefined) ->
    ok;
discovery_stop(#config_nodes_discovery{module = Module,
                                       start_f = StopF,
                                       start_a = StopA}) ->
    case erlang:apply(Module, StopF, StopA) of
        ok ->
            ok;
        {error, Reason} ->
            ?LOG_ERROR("~p:~p error: ~p", [Module, StopF, Reason])
    end,
    ok.

discovery_update(undefined, undefined) ->
    ok;
discovery_update(undefined, #config_nodes_discovery{} = NewDiscovery) ->
    discovery_start(NewDiscovery);
discovery_update(#config_nodes_discovery{} = OldDiscovery, undefined) ->
    discovery_stop(OldDiscovery);
discovery_update(#config_nodes_discovery{discover_f = OldF},
                 #config_nodes_discovery{discover_f = OldF}) ->
    ok;
discovery_update(#config_nodes_discovery{} = OldDiscovery,
                 #config_nodes_discovery{} = NewDiscovery) ->
    discovery_stop(OldDiscovery),
    discovery_start(NewDiscovery).

pforeach(_, []) ->
    ok;
pforeach(F, L) ->
    [erlang:spawn_link(fun() -> F(E) end) || E <- L],
    ok.
