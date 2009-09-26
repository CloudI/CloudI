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
%%% @version 0.0.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloud_worker_port_sup).
-author('mjtruog [at] gmail (dot) com').

-behaviour(supervisor).

%% external interface
-export([start_link/1, start_link/2,
         get_worker/1, get_cworker/1, get_index/1,
         restart_port/1, stop_port/1, create_ports/2]).

%% supervisor callbacks
-export([init/1]).

-include("cloud_logger.hrl").

% lib/cloud_worker/src/node_connections.cpp currently depends on
% having both the cnode name and the process name only differ by
% "cworker" != "worker"
-define(WORKER_PROCESS_NAME, "cloud_worker_port").
-define(WORKER_CNODE_NAME, "cloud_cworker_port").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Start the worker process supervisor.===
%% @end
%%-------------------------------------------------------------------------

-spec start_link(Node :: atom()) ->
    {'ok', pid()} |
    {'error', any()}.

start_link(Node) when is_atom(Node) ->
    start_link(Node, 5000).

-spec start_link(Node :: atom(), Timeout :: pos_integer()) -> 
    {'ok', pid()} |
    {'error', any()}.

start_link(Node, Timeout) when is_atom(Node), is_integer(Timeout) ->
    case node() of
        Node ->
            case supervisor:start_link({local, ?MODULE}, ?MODULE, []) of
                {ok, Pid} ->
                    {ok, Pid};
                {error, {already_started, Pid}} ->
                    {ok, Pid};
                {error, _} = Error ->
                    Error
            end;
        _ ->
            Parent = self(),
            GroupLeader = erlang:group_leader(),
            % link to the remote supervisor if it starts successfully
            % (or if it was started in the past)
            Child = erlang:spawn(Node, fun() ->
                Result = supervisor:start_link({local, ?MODULE},
                                               ?MODULE, [GroupLeader]),
                case Result of
                    {ok, Pid} when is_pid(Pid) ->
                        unlink(Pid);
                    _ ->
                        ok
                end,
                Parent ! {self(), Result}
            end),
            receive
                {Child, {ok, Pid}} when is_pid(Pid) ->
                    link(Pid),
                    {ok, Pid};
                {Child, {error, {already_started, Pid}}} when is_pid(Pid) ->
                    link(Pid),
                    {ok, Pid};
                {Child, {error, _} = Error} ->
                    Error
            after
                Timeout ->
                    {error, timeout}
            end
    end.

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
%% ===Get the worker index, given the worker process name.===
%% @end
%%-------------------------------------------------------------------------

-spec get_index(Worker :: atom()) -> integer().

get_index(Worker) when is_atom(Worker) ->
    {_, Index} = lists:split(
        length(?WORKER_PROCESS_NAME), atom_to_list(Worker)),
    list_to_integer(Index).

%%-------------------------------------------------------------------------
%% @doc
%% ===Restart the worker process.===
%% @end
%%-------------------------------------------------------------------------

-spec restart_port({atom(), atom()}) -> 'ok' | 'error'.

restart_port({Name, Node}) when is_atom(Name), is_atom(Node) ->
    case supervisor:terminate_child({?MODULE, Node}, Name) of
        ok ->
            supervisor:restart_child({?MODULE, Node}, Name),
            ok;
        {error, Reason} ->
            ?LOG_ERROR("failed to restart ~p on ~p: ~p",
                [Name, Node, Reason]),
            error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Stop the worker process and prevent the supervisor from restarting it.===
%% @end
%%-------------------------------------------------------------------------

-spec stop_port({atom(), atom()}) -> 'ok' | 'error'.

stop_port({Name, Node}) when is_atom(Name), is_atom(Node) ->
    case supervisor:terminate_child({?MODULE, Node}, Name) of
        ok ->
            case supervisor:delete_child({?MODULE, Node}, Name) of
                ok ->
                    ok;
                {error, Reason} ->
                    ?LOG_ERROR("failed to delete ~p on ~p: ~p",
                        [Name, Node, Reason]),
                    error
            end;
        {error, Reason} ->
            ?LOG_ERROR("failed to terminate ~p on ~p: ~p",
                [Name, Node, Reason]),
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
        CWorkerId = get_cworker(Index),
        ChildSpec = {WorkerId,
                     {cloud_worker_port, start_link,
                      [WorkerId, CWorkerId, node()]},
                     Restart, Shutdown, Type, [cloud_worker_port]},
        case supervisor:start_child({?MODULE, Node}, ChildSpec) of
            {ok, _} ->
                % cloud_worker_port:init/1 will trigger a "ready" message
                true;
            {ok, _, _} ->
                % cloud_worker_port:init/1 will trigger a "ready" message
                true;
            {error, {already_started, _}} ->
                % trigger a "reset_ready" message
                try gen_server:call({WorkerId, Node}, {reset_ready,
                    {WorkerId, Node}, {cloud_leader, node()}}) of
                    ok -> true
                catch
                    exit:{timeout, _} ->
                        ?LOG_ERROR("~p timeout on ~p", [WorkerId, Node]),
                        false
                end;
            {error, Error} ->
                ?LOG_ERROR("~p failed on ~p: ~p", [WorkerId, Node, Error]),
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
 
init([GroupLeader]) ->
    group_leader(GroupLeader, self()),
    init([]);
init([]) ->
    MaxRestarts = 5,
    MaxTime = 60, % seconds (1 minute)
    {ok, {{one_for_one, MaxRestarts, MaxTime}, []}}.

