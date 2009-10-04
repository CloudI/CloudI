%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Cloudi Worker Process Supervisor==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2009, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2009 Michael Truog
%%% @version 0.0.4 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloud_worker_port_sup).
-author('mjtruog [at] gmail (dot) com').

-behaviour(supervisor).

%% external interface
-export([start_link/2, start_link/3,
         get_worker/1, get_cworker/1,
         get_cworker_name/2,
         get_worker_index/1, get_cworker_index/1,
         restart_port/1, stop_port/1, create_ports/2]).

%% supervisor callbacks
-export([init/1]).

-include("cloud_logger.hrl").

-define(WORKER_NAME(N), "cloud_" N "_port").
% lib/cloud_worker/src/node_connections.cpp currently depends on
% having both the cnode name and the process name only differ by
% "cworker" != "worker"
-define(WORKER_PROCESS_NAME, ?WORKER_NAME("worker")).
-define(WORKER_CNODE_NAME, ?WORKER_NAME("cworker")).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Start the worker process supervisor.===
%% @end
%%-------------------------------------------------------------------------

-spec start_link(OldPid :: pid() | 'undefined',
                 Node :: atom()) ->
    {'ok', pid()} |
    {'error', any()}.

start_link(OldPid, Node) when is_atom(Node) ->
    start_link(OldPid, Node, 5000).

-spec start_link(OldPid :: pid() | 'undefined',
                 Node :: atom(),
                 Timeout :: pos_integer()) -> 
    {'ok', pid()} |
    {'error', any()}.

start_link(OldPid, Node, Timeout) when is_atom(Node), is_integer(Timeout) ->
    monitor_link:call(OldPid, Node, supervisor, start_link,
        [{local, ?MODULE}, ?MODULE, []], Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the worker name given a worker index.===
%% The worker name is the locally registered name of the worker process.
%% @end
%%-------------------------------------------------------------------------

-spec get_worker(Index :: integer()) -> atom().

get_worker(Index) when is_integer(Index) ->
    list_to_atom(?WORKER_PROCESS_NAME ++ integer_to_list(Index)).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the worker cnode name prefix given a worker index.===
%% The worker cnode name prefix identifies the cnode with the suffix '@host'.
%% @end
%%-------------------------------------------------------------------------

-spec get_cworker(Index :: integer()) -> string().

get_cworker(Index) when is_integer(Index) ->
    ?WORKER_CNODE_NAME ++ integer_to_list(Index).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the complete worker cnode name.===
%% @end
%%-------------------------------------------------------------------------

-spec get_cworker_name(Index :: integer(), Node :: atom()) -> atom().

get_cworker_name(Index, Node) when is_integer(Index), is_atom(Node) ->
    list_to_atom(string_extensions:format("~s~p@~s",
        [?WORKER_CNODE_NAME, Index,
         string_extensions:after_character($@, atom_to_list(Node))])).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the worker index, given the worker process name.===
%% @end
%%-------------------------------------------------------------------------

-spec get_worker_index(Worker :: atom()) ->
    integer() |
    'error'.

get_worker_index(Worker) when is_atom(Worker) ->
    try lists:split(length(?WORKER_PROCESS_NAME), atom_to_list(Worker)) of
        {?WORKER_PROCESS_NAME, Index} ->
            list_to_integer(Index);
        _ ->
            error
    catch
        error:badarg ->
            error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the cnode worker index, given the cnode worker name prefix.===
%% @end
%%-------------------------------------------------------------------------

-spec get_cworker_index(Worker :: string()) ->
    integer() |
    'error'.

get_cworker_index(Worker) when is_list(Worker) ->
    try lists:split(length(?WORKER_CNODE_NAME), Worker) of
        {?WORKER_CNODE_NAME, Index} ->
            list_to_integer(Index);
        _ ->
            error
    catch
        error:badarg ->
            error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Restart the worker process.===
%% @end
%%-------------------------------------------------------------------------

-spec restart_port({atom(), atom()}) -> 'ok' | 'error'.

restart_port({Name, Node}) when is_atom(Name), is_atom(Node) ->
    case get_worker_index(Name) of
        error ->
            ?LOG_ERROR("invalid worker name ~p", [Name]);
        Index ->
            CNode = get_cworker_name(Index, Node),
            RestartNow = case lists:member(CNode, nodes(hidden)) of
                true ->
                    case erlang:disconnect_node(CNode) of
                        true ->
                            % let nodedown restart the port by
                            % calling this function a second time
                            % (since cloud_leader is monitoring all nodes).
                            % doing things this way avoids having the new
                            % process unable to publish its name as a
                            % new cnode.
                            false;
                        _ ->
                            true
                    end;
                _ ->
                    true
            end,
            if
                RestartNow ->
                    SupRef = {?MODULE, Node},
                    try supervisor:terminate_child(SupRef, Name) of
                        ok ->
                            try supervisor:restart_child(SupRef, Name) of
                                _ ->
                                    ok
                            catch
                                _:Reason ->
                                    ?LOG_ERROR("failed to restart ~p on ~p: ~P",
                                               [Name, Node, Reason, 3]),
                                    error
                            end;
                        {error, Reason} ->
                            ?LOG_ERROR("failed to restart ~p on ~p: ~p",
                                       [Name, Node, Reason]),
                            error
                    catch
                        _:Reason ->
                            ?LOG_ERROR("failed to restart ~p on ~p: ~P",
                                       [Name, Node, Reason, 3]),
                            error
                    end;
                true ->
                    ok
            end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Stop the worker process and prevent the supervisor from restarting it.===
%% @end
%%-------------------------------------------------------------------------

-spec stop_port({atom(), atom()}) -> 'ok' | 'error'.

stop_port({Name, Node}) when is_atom(Name), is_atom(Node) ->
    try supervisor:terminate_child({?MODULE, Node}, Name) of
        ok ->
            try supervisor:delete_child({?MODULE, Node}, Name) of
                ok ->
                    ok;
                {error, Reason} ->
                    ?LOG_ERROR("failed to delete ~p on ~p: ~p",
                               [Name, Node, Reason]),
                    error
            catch
                _:Reason ->
                    ?LOG_ERROR("failed to delete ~p on ~p: ~P",
                               [Name, Node, Reason, 3]),
                    error
            end;
        {error, Reason} ->
            ?LOG_ERROR("failed to terminate ~p on ~p: ~p",
                       [Name, Node, Reason]),
            error
    catch
        _:Reason ->
            ?LOG_ERROR("failed to terminate ~p on ~p: ~P",
                       [Name, Node, Reason, 3]),
            error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Start worker processes under the supervisor on a given node.===
%% @end
%%-------------------------------------------------------------------------

-spec create_ports(Node :: atom(),
                   NumberOfPorts :: pos_integer()) -> 'ok' | 'error'.

create_ports(Node, NumberOfPorts) when is_integer(NumberOfPorts) ->
    Restart = permanent, % always restarted
    Shutdown = 2000, % milliseconds (2 seconds)
    Type = worker,
    Success = lists:all(fun(Index) ->
        WorkerId = get_worker(Index),
        ChildSpec = {WorkerId,
                     {cloud_worker_port, start_link, [WorkerId, node()]},
                     Restart, Shutdown, Type, [cloud_worker_port]},
        try supervisor:start_child({?MODULE, Node}, ChildSpec) of
            {ok, _} ->
                % cloud_worker_port:init/1 will trigger a "ready" message
                true;
            {ok, _, _} ->
                % cloud_worker_port:init/1 will trigger a "ready" message
                true;
            {error, {already_started, _}} ->
                CNode = get_cworker_name(Index, Node),
                case net_kernel:connect_node(CNode) of
                    true ->
                        % worker needs to have all work assignments verified
                        cloud_worker_port:acquire_as_old_process(
                            {WorkerId, Node}, {cloud_leader, node()});
                    _ ->
                        % worker needs to be reconfigured
                        cloud_worker_port:acquire_as_new_process(
                            {WorkerId, Node}, {cloud_leader, node()})
                end;
            {error, Error} ->
                ?LOG_ERROR("~p failed on ~p: ~p",
                           [WorkerId, Node, Error]),
                false
        catch
            _:Reason ->
                ?LOG_ERROR("~p failed on ~p: ~P",
                           [WorkerId, Node, Reason, 3]),
                false
        end
    end, lists:seq(1, NumberOfPorts)),
    if
        Success -> ok;
        true -> error
    end.

%%%------------------------------------------------------------------------
%%% Callback functions from supervisor
%%%------------------------------------------------------------------------
 
init([]) ->
    MaxRestarts = 5,
    MaxTime = 60, % seconds (1 minute)
    {ok, {{one_for_one, MaxRestarts, MaxTime}, []}}.

