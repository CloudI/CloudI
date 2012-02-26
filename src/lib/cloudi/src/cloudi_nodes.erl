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
%%% Copyright (c) 2011-2012, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011-2012 Michael Truog
%%% @version 0.2.0 {@date} {@time}
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
        nodes_alive = [],
        nodes_dead = [],
        logger_redirect = undefined,
        timer_reconnect = undefined,
        nodes = []
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

start_link(Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

reconfigure(Config, Timeout) ->
    gen_server:call(?MODULE, {reconfigure, Config,
                              Timeout - ?TIMEOUT_DELTA}, Timeout).

alive(Timeout) ->
    gen_server:call(?MODULE, alive, Timeout).

dead(Timeout) ->
    gen_server:call(?MODULE, dead, Timeout).

nodes(Timeout) ->
    gen_server:call(?MODULE, nodes, Timeout).

logger_redirect(Node) when is_atom(Node) ->
    gen_server:cast(?MODULE, {logger_redirect, Node}).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([Config]) ->
    net_kernel:monitor_nodes(true, [{node_type, visible}, nodedown_reason]),
    NodeLogger = (Config#config.logging)#config_logging.redirect,
    NewNodeLogger = if
        NodeLogger == node(); NodeLogger =:= undefined ->
            undefined;
        true ->
            NodeLogger
    end,
    if
        NewNodeLogger =/= undefined ->
            case lists:member(NewNodeLogger, Config#config.nodes) of
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
    {ok, #state{nodes_dead = Config#config.nodes,
                logger_redirect = NewNodeLogger,
                timer_reconnect = erlang:send_after(?NODE_RECONNECT_START,
                                                    self(),
                                                    reconnect),
                nodes = Config#config.nodes}}.

handle_call({reconfigure, Config, _}, _,
            #state{nodes_alive = NodesAlive,
                   timer_reconnect = TimerReconnect} = State) ->
    NewNodesDead = lists:foldl(fun(N, L) ->
        case cloudi_lists:delete_checked(N, L) of
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
    % assume the logger_redirect node does not need to be checked
    % if the node was removed intentionally
    {reply, ok, State#state{nodes_dead = NewNodesDead,
                            nodes_alive = NewNodesAlive,
                            timer_reconnect = NewTimerReconnect,
                            nodes = Config#config.nodes}};

handle_call(alive, _,
            #state{nodes_alive = NodesAlive} = State) ->
    {reply, NodesAlive, State};

handle_call(dead, _,
            #state{nodes_dead = NodesDead} = State) ->
    {reply, NodesDead, State};

handle_call(nodes, _,
            #state{nodes = Nodes} = State) ->
    {reply, Nodes, State};

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
    {noreply, State}.

handle_info({'nodeup', Node, InfoList},
            #state{nodes_alive = NodesAlive,
                   nodes_dead = NodesDead,
                   logger_redirect = NodeLogger} = State) ->
    if
        Node == NodeLogger ->
            cloudi_logger:redirect(NodeLogger);
        true ->
            ok
    end,
    ?LOG_INFO("nodeup ~p ~p", [Node, InfoList]),
    {noreply,
     State#state{nodes_alive = [Node | NodesAlive],
                 nodes_dead = cloudi_lists:delete_all(Node, NodesDead)}};

handle_info({'nodedown', Node, InfoList},
            #state{nodes_alive = NodesAlive,
                   nodes_dead = NodesDead,
                   logger_redirect = NodeLogger,
                   timer_reconnect = TimerReconnect} = State) ->
    if
        Node == NodeLogger ->
            cloudi_logger:redirect(undefined);
        true ->
            ok
    end,
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
            {noreply, State#state{timer_reconnect = undefined}}
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

