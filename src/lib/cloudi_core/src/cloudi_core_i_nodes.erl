%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Nodes==
%%% Manage node connections to provide reliability after network interruptions.
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2011-2018 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2011-2018 Michael Truog
%%% @version 1.7.4 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_core_i_nodes).
-author('mjtruog at protonmail dot com').

-behaviour(gen_server).

%% external interface
-export([start_link/1,
         reconfigure/2,
         alive/1,
         dead/1,
         nodes/1,
         logging_redirect_set/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("cloudi_logger.hrl").
-include("cloudi_core_i_constants.hrl").
-include("cloudi_core_i_configuration.hrl").

-record(state,
    {
        nodes_alive = [] :: list(node()),
        nodes_dead :: list(node()),
        nodes :: list(node()),
        logging_redirect :: node() | undefined,
        reconnect_interval :: pos_integer(),
        reconnect_timer,
        listen :: visible | all,
        connect :: visible | hidden,
        discovery :: #config_nodes_discovery{} | undefined
    }).

-define(CATCH_EXIT(F),
        try F catch exit:{Reason, _} -> {error, Reason} end).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

start_link(#config{nodes = #config_nodes{listen = Listen,
                                         connect = Connect,
                                         timestamp_type = TimestampType}} =
           Config) ->
    applications_set(Listen, Connect, TimestampType),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

reconfigure(Config, Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE, {reconfigure, Config}, Timeout)).

alive(Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE, alive, Timeout)).

dead(Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE, dead, Timeout)).

nodes(Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE, nodes, Timeout)).

logging_redirect_set(Node) when is_atom(Node) ->
    gen_server:cast(?MODULE, {logging_redirect_set, Node}).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([#config{logging = #config_logging{redirect = NodeLogger},
              nodes = #config_nodes{nodes = Nodes,
                                    reconnect_start = ReconnectStart,
                                    reconnect_delay = ReconnectDelay,
                                    listen = Listen,
                                    connect = Connect,
                                    discovery = Discovery}}]) ->
    monitor_nodes(true, Listen),
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
                logging_redirect = NewNodeLogger,
                reconnect_interval = ReconnectInterval,
                reconnect_timer = ReconnectTimer,
                listen = Listen,
                connect = Connect,
                discovery = Discovery}}.

handle_call({reconfigure,
             #config{logging = #config_logging{redirect = NodeLogger},
                     nodes = #config_nodes{nodes = Nodes,
                                           reconnect_delay = ReconnectDelay,
                                           listen = Listen,
                                           connect = Connect,
                                           timestamp_type = TimestampType,
                                           discovery = Discovery}}}, _,
            #state{nodes_alive = NodesAlive,
                   listen = OldListen,
                   connect = OldConnect,
                   discovery = OldDiscovery} = State) ->
    ConnectedNodes = if
        Connect =:= visible ->
            erlang:nodes();
        Connect =:= hidden ->
            erlang:nodes(connected)
    end,
    NewNodes = lists:usort(Nodes ++ ConnectedNodes),
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
    logging_redirect_set(NodeLogger),
    applications_set(Listen, Connect, TimestampType),
    if
        OldListen /= Listen ->
            monitor_nodes(false, OldListen),
            monitor_nodes(true, Listen),
            cpg_scopes_reset();
        true ->
            ok
    end,
    if
        OldConnect /= Connect ->
            discovery_stop(OldDiscovery),
            discovery_start(Discovery);
        true ->
            discovery_update(OldDiscovery, Discovery)
    end,
    {reply, ok, State#state{nodes_alive = NewNodesAlive,
                            nodes_dead = NewNodesDead,
                            nodes = NewNodes,
                            reconnect_interval = ReconnectInterval,
                            connect = Connect,
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
    {stop, cloudi_string:format("Unknown call \"~w\"", [Request]),
     error, State}.

handle_cast({logging_redirect_set, NodeLogger},
            #state{nodes_alive = NodesAlive,
                   nodes_dead = NodesDead,
                   logging_redirect = OldNodeLogger} = State) ->
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
                    cloudi_core_i_logger:redirect_update(undefined);
                true ->
                    case lists:member(NewNodeLogger, NodesAlive) of
                        true ->
                            cloudi_core_i_logger:redirect_update(NewNodeLogger);
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
            {noreply, State#state{logging_redirect = NewNodeLogger}};
        true ->
            {noreply, State}
    end;

handle_cast(Request, State) ->
    {stop, cloudi_string:format("Unknown cast \"~w\"", [Request]), State}.

handle_info({nodeup, Node, InfoList},
            #state{nodes_alive = NodesAlive,
                   nodes_dead = NodesDead,
                   nodes = Nodes,
                   logging_redirect = NodeLogger} = State) ->
    if
        Node == NodeLogger ->
            cloudi_core_i_logger:redirect_update(NodeLogger);
        true ->
            ok
    end,
    ?LOG_INFO("nodeup ~p~n ~p", [Node, InfoList]),
    {noreply,
     State#state{nodes_alive = lists:umerge(NodesAlive, [Node]),
                 nodes_dead = lists:delete(Node, NodesDead),
                 nodes = lists:umerge(Nodes, [Node])}};

handle_info({nodedown, Node, InfoList},
            #state{nodes_alive = NodesAlive,
                   nodes_dead = NodesDead,
                   logging_redirect = NodeLogger} = State) ->
    if
        Node == NodeLogger ->
            cloudi_core_i_logger:redirect_update(undefined);
        true ->
            ok
    end,
    ?LOG_INFO("nodedown ~p~n ~p", [Node, InfoList]),
    {noreply, State#state{nodes_alive = lists:delete(Node, NodesAlive),
                          nodes_dead = lists:umerge(NodesDead, [Node])}};

handle_info(reconnect,
            #state{nodes_dead = NodesDead,
                   reconnect_interval = ReconnectInterval,
                   connect = Connect,
                   discovery = Discovery} = State) ->
    discovery_check(Discovery),
    if
        NodesDead /= [] ->
            ?LOG_INFO("currently dead nodes ~p", [NodesDead]),
            pforeach(fun(Node) ->
                % avoid the possibly long synchronous call here
                connect_node(Connect, Node)
            end, NodesDead);
        true ->
            ok
    end,
    ReconnectTimer = erlang:send_after(ReconnectInterval, self(), reconnect),
    {noreply, State#state{reconnect_timer = ReconnectTimer}};

handle_info({ReplyRef, _}, State) when is_reference(ReplyRef) ->
    % gen_server:call/3 had a timeout exception that was caught but the
    % reply arrived later and must be discarded
    {noreply, State};

handle_info(Request, State) ->
    {stop, cloudi_string:format("Unknown info \"~w\"", [Request]), State}.

terminate(_, #state{discovery = Discovery}) ->
    discovery_stop(Discovery),
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

applications_set(Listen, Connect, TimestampType) ->
    application:set_env(cloudi_x_cpg, node_type, Listen),
    application:set_env(cloudi_x_nodefinder, node_type, Connect),
    application:set_env(cloudi_core, timestamp_type, TimestampType),
    ok.

monitor_nodes(Flag, Listen) ->
    net_kernel:monitor_nodes(Flag, [{node_type, Listen}, nodedown_reason]).

discovery_start_args(ec2_discover, StartA) ->
    [EC2AccessKeyId, EC2SecretAccessKey,
     EC2Host, EC2Groups, EC2Tags] = StartA,
    Lookup = cloudi_environment:lookup(),
    [cloudi_environment:transform(EC2AccessKeyId, Lookup),
     cloudi_environment:transform(EC2SecretAccessKey, Lookup),
     cloudi_environment:transform(EC2Host, Lookup),
     EC2Groups, EC2Tags];
discovery_start_args(_, StartA) ->
    StartA.

discovery_start(undefined) ->
    ok;
discovery_start(#config_nodes_discovery{module = Module,
                                        start_f = StartF,
                                        start_a = StartA}) ->
    case erlang:apply(Module, StartF, discovery_start_args(StartF, StartA)) of
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
                                       stop_f = StopF,
                                       stop_a = StopA}) ->
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
discovery_update(#config_nodes_discovery{start_f = OldStartF,
                                         start_a = OldStartA},
                 #config_nodes_discovery{start_f = OldStartF,
                                         start_a = OldStartA}) ->
    ok;
discovery_update(#config_nodes_discovery{} = OldDiscovery,
                 #config_nodes_discovery{} = NewDiscovery) ->
    discovery_stop(OldDiscovery),
    discovery_start(NewDiscovery).

cpg_scopes() ->
    % due to settings in cloudi_core_i_constants.hrl of
    % SCOPE_CUSTOM_PREFIX and SCOPE_DEFAULT
    CustomScopes = lists:filter(fun(RegisteredName) ->
        lists:prefix(?SCOPE_CUSTOM_PREFIX,
                     erlang:atom_to_list(RegisteredName))
    end, erlang:registered()),
    [?SCOPE_DEFAULT | CustomScopes].

cpg_scopes_reset() ->
    lists:foreach(fun(Scope) ->
        cloudi_x_cpg:reset(Scope)
    end, cpg_scopes()).

connect_node(visible, Node) ->
    net_kernel:connect_node(Node);
connect_node(hidden, Node) ->
    net_kernel:hidden_connect_node(Node).

pforeach(_, []) ->
    ok;
pforeach(F, L) ->
    [erlang:spawn_link(fun() -> F(E) end) || E <- L],
    ok.
