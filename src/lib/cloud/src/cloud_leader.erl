%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Cloudi Leader==
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

-module(cloud_leader).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).

%% external interface
-export([start_link/1,
         add_job/1,
         remove_job/1,
         add_machine/1,
         remove_machine/1,
         stop/0,
         stop/1,
         restart/0,
         restart/1,
         work_data_done/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("cloud_logger.hrl").
-include("cloud_configuration.hrl").

-record(state,
    {
    worker_nodes_state = undefined,
    worker_scheduler_state = undefined}).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Start the cloud leader.===
%% The cloud leader coordinates all node discovery and work scheduling
%% for a single instance.  Failover can switch between cloud_leader
%% instances with the cloud_api.
%% @end
%%-------------------------------------------------------------------------

-spec start_link(#config{}) -> {'ok', pid()} | {'error', any()}.

start_link(#config{} = Config) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Config], []).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add a single job to the scheduler.===
%% The new job will be scheduled after being successfully added.
%% @end
%%-------------------------------------------------------------------------

-spec add_job(L :: string()) -> 'ok' | {'error', any()}.

add_job(L) when is_list(L) ->
    cloud_worker_scheduler:add_job_request(?MODULE, L).

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove a job from the cloud.===
%% @end
%%-------------------------------------------------------------------------

-spec remove_job(WorkTitle :: string()) -> 'ok' | 'error'.

remove_job(WorkTitle) when is_list(WorkTitle) ->
    cloud_worker_scheduler:remove_job_request(WorkTitle).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add a single machine to the cloud.===
%% @end
%%-------------------------------------------------------------------------

-spec add_machine(L :: string()) -> 'ok' | {'error', any()}.

add_machine(L) when is_list(L) ->
    cloud_worker_nodes:add_machine_request(?MODULE, L).

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove a machine from the cloud.===
%% @end
%%-------------------------------------------------------------------------

-spec remove_machine(HostName :: string()) -> 'ok' | {'error', any()}.

remove_machine(HostName) when is_list(HostName) ->
    cloud_worker_nodes:remove_machine_request(?MODULE, HostName).

%%-------------------------------------------------------------------------
%% @doc
%% ===Stop the worker nodes server and all the remote Erlang VMs.===
%% @end
%%-------------------------------------------------------------------------

-spec stop() -> 'ok'.

stop() ->
    cloud_worker_nodes:stop_request(?MODULE).

%%-------------------------------------------------------------------------
%% @doc
%% ===Stop a remote Erlang VM that is currently running as part of the cloud.===
%% @end
%%-------------------------------------------------------------------------

-spec stop(Node :: atom()) -> 'ok' | 'error'.

stop(Node) when is_atom(Node) ->
    cloud_worker_nodes:stop_request(?MODULE, Node).

%%-------------------------------------------------------------------------
%% @doc
%% ===Restart the remote Erlang VMs if necessary,===
%% (reconnect if possible).
%% @end
%%-------------------------------------------------------------------------

-spec restart() -> 'ok'.

restart() ->
    cloud_worker_nodes:restart_request(?MODULE).

%%-------------------------------------------------------------------------
%% @doc
%% ===Restart a specific remote Erlang VM if necessary,===
%% (reconnect if possible).
%% @end
%%-------------------------------------------------------------------------

-spec restart(Node :: atom()) -> 'ok' | 'error'.

restart(Node) when is_atom(Node) ->
    cloud_worker_nodes:restart_request(?MODULE, Node).

%%-------------------------------------------------------------------------
%% @doc
%% ===All work data has been collected by the work_manager.===
%% The work type is ready to be removed from the scheduler.
%% @end
%%-------------------------------------------------------------------------

-spec work_data_done(WorkTitle :: string()) -> 'ok'.

work_data_done(WorkTitle) ->
    ok = gen_server:cast(?MODULE, {work_data_done, WorkTitle}),
    ok.

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([Config]) ->
    erlang:process_flag(trap_exit, true),
    {ok, WorkerSchedulerState} =
        cloud_worker_scheduler:initialize_state(Config),
    case cloud_worker_nodes:initialize_state(Config) of
        {ok, WorkerNodesState} ->
            {ok, #state{worker_nodes_state = WorkerNodesState,
                        worker_scheduler_state = WorkerSchedulerState}};
        {error, Reason} ->
            {stop, Reason}
    end.
handle_call({add_work, C}, _,
            #state{worker_scheduler_state = S} = State) ->
    {reply, ok, State#state{worker_scheduler_state =
        cloud_worker_scheduler:add_job(C, S)}};
handle_call({add_machine, C, UseLongName}, _,
            #state{worker_nodes_state = S} = State) ->
    {Result, NewS} = cloud_worker_nodes:add_machine(C, UseLongName, S),
    {reply, Result, State#state{worker_nodes_state = NewS}};
handle_call({remove_machine, HostName}, _,
            #state{worker_nodes_state = S} = State) ->
    {Result, NewS} = cloud_worker_nodes:remove_machine(HostName, S),
    {reply, Result, State#state{worker_nodes_state = NewS}};
handle_call(stop, _,
            #state{worker_nodes_state = S} = State) ->
    {reply, ok, State#state{worker_nodes_state =
        cloud_worker_nodes:stop(S)}};
handle_call({stop, Node}, _,
            #state{worker_nodes_state = S} = State) ->
    {Result, NewS} = cloud_worker_nodes:stop(Node, S),
    {reply, Result, State#state{worker_nodes_state = NewS}};
handle_call(restart, _,
            #state{worker_nodes_state = S} = State) ->
    {reply, ok, State#state{worker_nodes_state =
        cloud_worker_nodes:restart(S)}};
handle_call({restart, Node}, _,
            #state{worker_nodes_state = S} = State) ->
    {Result, NewS} = cloud_worker_nodes:restart(Node, S),
    {reply, Result, State#state{worker_nodes_state = NewS}};
handle_call(Request, _, State) ->
    {stop, string_extensions:format("Unknown call \"~p\"", [Request]),
     error, State}.
handle_cast({ready, Process},
            #state{worker_nodes_state = S} = State) ->
    {noreply, State#state{worker_nodes_state =
        cloud_worker_nodes:worker_node_ready_new(Process, S)}};
handle_cast({reset_ready, Process},
            #state{worker_nodes_state = S} = State) ->
    {noreply, State#state{worker_nodes_state =
        cloud_worker_nodes:worker_node_ready_old(Process, S)}};
handle_cast({work_data_done, WorkTitle},
            #state{worker_nodes_state = NodesState,
                   worker_scheduler_state = SchedulerState} = State) ->
    {NewNodesState, NewSchedulerState} =
        cloud_worker_nodes:work_data_done(WorkTitle,
                                          NodesState, SchedulerState),
    {noreply, State#state{worker_nodes_state = NewNodesState,
                          worker_scheduler_state = NewSchedulerState}};
handle_cast({work_module_done, WorkTitle},
            #state{worker_scheduler_state = S} = State) ->
    {noreply, State#state{worker_scheduler_state =
        cloud_worker_scheduler:work_module_done(WorkTitle, S)}};
handle_cast({work_module_failed, WorkTitle},
            #state{worker_scheduler_state = S} = State) ->
    {noreply, State#state{worker_scheduler_state =
        cloud_worker_scheduler:work_module_failed(WorkTitle, S)}};
handle_cast(Request, State) ->
    ?LOG_WARNING("Unknown cast \"~p\"", [Request]),
    {noreply, State}.
handle_info({'EXIT', Pid, Reason},
            #state{worker_nodes_state = S} = State) ->
    {noreply, State#state{worker_nodes_state = 
        cloud_worker_nodes:workers_died(Pid, Reason, S)}};
handle_info(restart_workers,
            #state{worker_nodes_state = NodesState} = State) ->
    {noreply, State#state{worker_nodes_state =
        cloud_worker_nodes:restart_workers(NodesState)}};
handle_info(allocate_work,
            #state{worker_nodes_state = NodesState,
                   worker_scheduler_state = SchedulerState} = State) ->
    {NewNodesState, NewSchedulerState} =
        cloud_worker_nodes:allocate_work(NodesState, SchedulerState),
    {noreply, State#state{worker_nodes_state = NewNodesState,
                          worker_scheduler_state = NewSchedulerState}};
handle_info(Request, State) ->
    ?LOG_WARNING("Unknown info \"~p\"", [Request]),
    {noreply, State}.
terminate(_, #state{worker_nodes_state = NodesState,
                    worker_scheduler_state = SchedulerState}) ->
    cloud_worker_nodes:deinitialize_state(NodesState),
    cloud_worker_scheduler:deinitialize_state(SchedulerState),
    ok.
code_change(_, State, _) ->
    {ok, State}.

