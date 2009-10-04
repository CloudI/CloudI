%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Cloudi Worker Nodes State==
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

-module(cloud_worker_nodes).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([initialize_state/1,
         deinitialize_state/1,
         add_machine_request/2,
         add_machine/3,
         remove_machine_request/2,
         remove_machine/2,
         stop_request/1,
         stop/1,
         stop_request/2,
         stop/2,
         restart_request/1,
         restart/1,
         restart_request/2,
         restart/2,
         worker_node_ready_new/2,
         worker_node_ready_old/2,
         allocate_work/2,
         restart_workers/1,
         workers_died/3,
         work_data_done/3,
         nodedown_occurred/3]).

-include("cloud_configuration.hrl").
-include("cloud_run_queue.hrl").
-include("cloud_logger.hrl").

% how long to wait for the Erlang VM to start on a single remote node
-define(CLOUD_PEER_START_TIMEOUT, 5000). % 5 seconds
% delay between a worker starting and being ready to accept work
% and the worker's process data being sent to the scheduler for scheduling
-define(WORKER_WORK_ALLOCATION_DELAY, 5000).% 5 seconds
% what interval to use when checking if nodes need to be restarted
% (must be significantly larger than ?WORKER_WORK_ALLOCATION_DELAY
%  to avoid verifying the work assigned to workers, that have not yet
%  started their work... such that their work does not yet reside
%  within the worker)
-define(WORKER_RESTART_INTERVAL, 15000).% 15 seconds

%% server state records

-record(machine_state,
    {
    node = undefined,
    node_state = error,
    node_name_parsed = {},
    worker_port_sup = undefined,
    ports = [],
    processes = 0,
    threads = 0,
    instance = 0}).

                % nodes managed by this process
-record(state,
    {
    machine_states = [],
    % processes accumulated for the scheduler to take
    run_queue_work_state_processes = [],
    % timer for reconnecting/restarting failed nodes
    worker_restart_timer = undefined,
    % timer for scheduling new worker processes
    worker_scheduler_timer = undefined}).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Initialize state local to the module.===
%% @end
%%-------------------------------------------------------------------------

-spec initialize_state(Config :: #config{}) ->
    {'ok', #state{}} |
    {'error', any()}.

initialize_state(Config) when is_record(Config, config) ->
    {Running, MachineStates} = start_machines(get_machine_states(Config)),
    if
        Running /= ok ->
            {error, "cloud_worker_nodes init_state/1 failed"};
        true ->
            Timer = erlang:send_after(?WORKER_RESTART_INTERVAL,
                self(), restart_workers),
            {ok, #state{machine_states = MachineStates,
                        worker_restart_timer = Timer}}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Deinitialize state local to the module.===
%% @end
%%-------------------------------------------------------------------------

-spec deinitialize_state(State :: #state{}) -> 'ok'.

deinitialize_state(#state{worker_restart_timer = WorkerRestartTimer,
                          worker_scheduler_timer = WorkerSchedulerTimer}) ->
    if
        WorkerRestartTimer /= undefined ->
            erlang:cancel_timer(WorkerRestartTimer);
        true ->
            ok
    end,
    if
        WorkerSchedulerTimer /= undefined ->
            erlang:cancel_timer(WorkerSchedulerTimer);
       true ->
            ok
    end,
    ok.

%%-------------------------------------------------------------------------
%% @doc
%% ===Add a single machine to the cloud.===
%% @end
%%-------------------------------------------------------------------------

-spec add_machine_request(Process :: atom(), L :: string()) ->
    'ok' |
    {'error', any()}.

add_machine_request(Process, L)
    when is_atom(Process), is_list(L) ->
    UseLongName = net_kernel:longnames(),
    true = is_boolean(UseLongName), % since the localhost already was started
    try cloud_configuration:parse_machine(L, UseLongName) of
        C when is_record(C, config_machine) ->
            gen_server:call(Process, {add_machine, C, UseLongName},
                ?CLOUD_PEER_START_TIMEOUT + 5000)
    catch
        _:Reason ->
            {error, Reason}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Perform an add of a single machine to the cloud with local state.===
%% @end
%%-------------------------------------------------------------------------

-spec add_machine(C :: #config_machine{},
                  UseLongName :: bool(),
                  State :: #state{}) ->
    {'ok', #state{}} |
    {{'error', any()}, #state{}}.
    

add_machine(C, UseLongName, #state{machine_states = MachineStates} = State) ->
    % bitmask to match tuples representing node name
    Match = if
        UseLongName ->
            2#011;
        true ->
            2#01
    end,
    Result = lists_extensions:iter(
    fun(#machine_state{node_name_parsed = NodeName}, Iter) ->
        case tuple_extensions:match(
                NodeName, C#config_machine.node_name_parsed, Match) of
            true ->
                {error, "machine already exists"};
            false ->
                Iter()
        end
    end, ok, MachineStates),
    if
        Result == ok ->
            PeerProgram = erl_peer_program(),
            PeerArgs = erl_peer_command_line_args(erlang:get_cookie()),
            {ok, HostName} = inet:gethostname(),
            {Running, Machine} = start_machines(get_machine_states(
                [], [C], 1, erlang:get_cookie(),
                UseLongName, HostName, PeerProgram, PeerArgs)),
            if
                Running == ok ->
                    ?LOG_DEBUG("added machine ~p",
                               [C#config_machine.node_name]),
                    {ok,
                     State#state{machine_states = MachineStates ++ Machine}};
                true ->
                    {{error, "unable to start machine"}, State}
            end;
        true ->
            {Result, State}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove a machine from the cloud.===
%% @end
%%-------------------------------------------------------------------------

-spec remove_machine_request(Process :: atom(), HostName :: string()) ->
    'ok' |
    {'error', any()}.

remove_machine_request(Process, HostName)
    when is_atom(Process), is_list(HostName) ->
    gen_server:call(Process, {remove_machine, HostName}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Perform a remove of a single machine on the cloud with local state.===
%% @end
%%-------------------------------------------------------------------------

-spec remove_machine(HostName :: string(), State :: #state{}) ->
    {'ok', #state{}} |
    {{'error', any()}, #state{}}.

remove_machine(HostName, 
               #state{machine_states = MachineStates,
                      run_queue_work_state_processes = Processes} = State) ->
    case find_machine_state(HostName, MachineStates) of
        undefined ->
            {{error, "invalid machine hostname"}, State};
        #machine_state{node_state = error} = M ->
            % sometimes erroneous machines will have process entries
            % (possibly as other machines successfully restart)
            {_, RemainingProcesses} = lists:partition(
            fun(#run_queue_work_state_process{name = {_, Node}}) ->
                Node == M#machine_state.node
            end, Processes),
            {ok, State#state{
                machine_states = 
                    lists:keydelete(M#machine_state.node,
                        #machine_state.node, MachineStates
                    ),
                run_queue_work_state_processes = RemainingProcesses
            }};
        M ->
            % check to determine if all processes are idle
            {MachineProcesses, RemainingProcesses} = lists:partition(
            fun(#run_queue_work_state_process{name = {_, Node}}) ->
                Node == M#machine_state.node
            end, Processes),
            if
                erlang:length(MachineProcesses) == M#machine_state.processes ->
                    ?LOG_DEBUG("removed machine ~p", [HostName]),
                    halt_peer_node(M),
                    {ok, State#state{
                        machine_states = 
                            lists:keydelete(M#machine_state.node,
                                #machine_state.node, MachineStates
                            ),
                        run_queue_work_state_processes = RemainingProcesses
                    }};
                true ->
                    {{error, "machine is not idle"}, State}
            end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Stop the worker nodes server and all the remote Erlang VMs.===
%% @end
%%-------------------------------------------------------------------------

-spec stop_request(Process :: pid() | atom() | {atom(), atom()}) -> 'ok'.

stop_request(Process) ->
    % other timeouts will ensure that the synchronous call terminates
    % however, it could take 5 seconds per process if all the work types
    % are not respecting the stopProcessing boolean flag for their thread.
    gen_server:call(Process, stop, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Perform a stop of the worker nodes server and all the remote Erlang VMs with local state.===
%% @end
%%-------------------------------------------------------------------------

-spec stop(State :: #state{}) -> #state{}.

stop(#state{machine_states = MachineStates,
            worker_restart_timer = WorkerRestartTimer} = State) ->
    if
        WorkerRestartTimer /= undefined ->
            erlang:cancel_timer(WorkerRestartTimer);
        true ->
            ok
    end,
    State#state{
        machine_states = halt_all_nodes(MachineStates),
        worker_restart_timer = undefined
    }.

%%-------------------------------------------------------------------------
%% @doc
%% ===Stop a remote Erlang VM that is currently running as part of the cloud.===
%% @end
%%-------------------------------------------------------------------------

-spec stop_request(Process :: pid() | atom() | {atom(), atom()},
                   Node :: atom()) -> 'ok' | 'error'.

stop_request(Process, Node) when is_atom(Node) ->
    gen_server:call(Process, {stop, Node}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Perform a stop of a remote Erlang VM that is currently running as part of the cloud with local state.===
%% @end
%%-------------------------------------------------------------------------

-spec stop(Node :: atom(), State :: #state{}) ->
    {'ok', #state{}} |
    {'error', #state{}}.

stop(Node, State) when Node == node() ->
    {error, State};
stop(Node, #state{machine_states = MachineStates} = State) ->
    case lists:keyfind(Node, #machine_state.node, MachineStates) of
        false ->
            {error, State};
        Machine when is_record(Machine, machine_state) ->
            {ok, State#state{machine_states = 
                lists:keyreplace(Node, #machine_state.node,
                    MachineStates, halt_peer_node(Machine)
                )
            }}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Restart the remote Erlang VMs if necessary===
%% (reconnect if possible).
%% @end
%%-------------------------------------------------------------------------

-spec restart_request(Process :: pid() | atom() | {atom(), atom()}) -> 'ok'.

restart_request(Process) ->
    gen_server:call(Process, restart, ?CLOUD_PEER_START_TIMEOUT + 5000).

%%-------------------------------------------------------------------------
%% @doc
%% ===Perform a restart the remote VMs if necessary with local state.===
%% @end
%%-------------------------------------------------------------------------

-spec restart(State :: #state{}) -> #state{}.

restart(#state{machine_states = MachineStates,
               worker_restart_timer = WorkerRestartTimer} = State) ->
    NewWorkerRestartTimer = if
        WorkerRestartTimer == undefined ->
            erlang:send_after(?WORKER_RESTART_INTERVAL,
                self(), restart_workers);
        true ->
            WorkerRestartTimer
    end,
    State#state{
        machine_states = restart_peer_nodes(MachineStates),
        worker_restart_timer = NewWorkerRestartTimer
    }.

%%-------------------------------------------------------------------------
%% @doc
%% ===Restart a specific remote Erlang VM if necessary===
%% (reconnect if possible).
%% @end
%%-------------------------------------------------------------------------

-spec restart_request(Process :: pid() | atom() | {atom(), atom()},
                      Node :: atom()) -> 'ok' | 'error'.

restart_request(Process, Node) when is_atom(Node) ->
    gen_server:call(Process, {restart, Node}, ?CLOUD_PEER_START_TIMEOUT + 5000).

%%-------------------------------------------------------------------------
%% @doc
%% ===Perform a restart of a specific remote Erlang VM if necessary with local state===
%% @end
%%-------------------------------------------------------------------------

-spec restart(Node :: atom(), State :: #state{}) ->
    {'ok', #state{}} |
    {'error', #state{}}.

restart(Node, State) when Node == node() ->
    {error, State};
restart(Node, #state{machine_states = MachineStates,
                     worker_restart_timer = WorkerRestartTimer} = State)
    when is_atom(Node) ->
    NewWorkerRestartTimer = if
        WorkerRestartTimer == undefined ->
            erlang:send_after(?WORKER_RESTART_INTERVAL,
               self(), restart_workers);
        true ->
            WorkerRestartTimer
    end,
    case lists:keyfind(Node, #machine_state.node, MachineStates) of
        false ->
            {error, State};
        Machine when is_record(Machine, machine_state) ->
            {ok, State#state{machine_states = 
                lists:keyreplace(Node, #machine_state.node,
                    MachineStates,
                    restart_peer_node(
                        erl_peer_program(),
                        erl_peer_command_line_args(erlang:get_cookie()),
                        Machine
                    )
                ),
                worker_restart_timer = NewWorkerRestartTimer
            }}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===A new worker port process is ready to join the cloud.===
%% @end
%%-------------------------------------------------------------------------

-spec worker_node_ready_new(Process :: pid() | atom() | {atom(), atom()},
                            State :: #state{}) -> #state{}.

worker_node_ready_new(Process, State) ->
    worker_node_ready(Process, false, State).

%%-------------------------------------------------------------------------
%% @doc
%% ===An old worker port process is ready to re-join the cloud.===
%% @end
%%-------------------------------------------------------------------------

-spec worker_node_ready_old(Process :: pid() | atom() | {atom(), atom()},
                            State :: #state{}) -> #state{}.

worker_node_ready_old(Process, State) ->
    worker_node_ready(Process, true, State).

%%-------------------------------------------------------------------------
%% @doc
%% ===Allocate work to idle processes if there is sufficient capacity.===
%% @end
%%-------------------------------------------------------------------------

-spec allocate_work(State :: #state{}, S :: tuple()) -> {#state{}, tuple()}.

allocate_work(#state{run_queue_work_state_processes = Processes} = State, S) ->
    {RunningProcesses, RemainingProcesses, NewS} =
        cloud_worker_scheduler:allocate_work(Processes, S),
    if
        erlang:length(RunningProcesses) > 0 ->
            ok = cloud_work_manager:update(RunningProcesses);
        true ->
            ok
    end,
    {State#state{run_queue_work_state_processes = RemainingProcesses,
                 worker_scheduler_timer = undefined}, NewS}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Restart worker node connections if necessary/possible.===
%% @end
%%-------------------------------------------------------------------------

-spec restart_workers(State :: #state{}) -> #state{}.

restart_workers(#state{machine_states = MachineStates} = State) ->
    ErroneousMachineStates = lists:any(fun(M) ->
        (M#machine_state.node_state == error)
    end, MachineStates),
    NewMachineStates = if
        ErroneousMachineStates ->
            ErroneousNames = lists:foldl(fun(M, Names) ->
                if
                    M#machine_state.node_state == error ->
                        Names ++ [M#machine_state.node];
                    true ->
                        Names
                end
            end, [], MachineStates),
            RestartedMachines = restart_peer_nodes(MachineStates),
            NewErroneousNames = lists:foldl(fun(M, Names) ->
                if
                    M#machine_state.node_state == error ->
                        Names ++ [M#machine_state.node];
                    true ->
                        Names
                end
            end, [], RestartedMachines),
            if
                ErroneousNames == NewErroneousNames ->
                    ?LOG_WARNING("erroneous node(s) unable to restart: ~p",
                                 [ErroneousNames]),
                    RestartedMachines;
                true ->
                    erlang:element(2, start_machines(RestartedMachines))
            end;
        true ->
            MachineStates
    end,
    Timer = erlang:send_after(?WORKER_RESTART_INTERVAL,
        self(), restart_workers),
    State#state{machine_states = NewMachineStates,
                worker_restart_timer = Timer}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Update local state to reflect the death of a worker.===
%% Only a worker port process death will be checked here.
%% @end
%%-------------------------------------------------------------------------

-spec workers_died(Pid :: pid(),
                   Reason :: any(),
                   State :: #state{}) -> #state{}.

workers_died(Pid, Reason, #state{machine_states = MachineStates} = State) ->
    case lists:keyfind(Pid, #machine_state.worker_port_sup, MachineStates) of
        false ->
            % a non-cloud_worker_port_sup process died
            State;
        Machine when is_record(Machine, machine_state) ->
            ?LOG_ERROR("cloud_worker_port_sup ~p died on node ~p: ~p",
                       [Pid, node(Pid), Reason]),
            State#state{machine_states =
                lists:keyreplace(Pid, #machine_state.worker_port_sup,
                    MachineStates, start_machine(Machine)
                )
            }
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===All data has been collected for this work type (i.e., work title).===
%% Any processes which have now become idle will be triggered to schedule
%% new work.
%% @end
%%-------------------------------------------------------------------------

-spec work_data_done(WorkTitle :: string(),
                     State :: #state{},
                     S :: tuple()) -> {#state{}, tuple()}.

work_data_done(WorkTitle,
               #state{run_queue_work_state_processes = Processes} = State, S) ->
    {IdleProcesses, NewS} = cloud_worker_scheduler:work_data_done(WorkTitle, S),
    if
        erlang:length(IdleProcesses) > 0 ->
            ?LOG_INFO("~p workers are now idle",
                      [erlang:length(IdleProcesses)]),
            % immediately try to schedule other work from the waiting queue
            self() ! allocate_work,
            {State#state{run_queue_work_state_processes =
                Processes ++ IdleProcesses}, NewS};
        true ->
            {State, NewS}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Node monitor message handling for nodedown.===
%% @end
%%-------------------------------------------------------------------------

-spec nodedown_occurred(Node :: atom(),
                        InfoList :: list({atom(), atom()}),
                        State :: #state{}) ->
    #state{}.

nodedown_occurred(Node, InfoList,
                  #state{machine_states = MachineStates} = State)
    when is_atom(Node), is_list(InfoList) ->
    {NodeName, HostName} =
        string_extensions:split_on_character($@, atom_to_list(Node)),
    NewMachineStates = update_machine_state_by_hostname(fun(M) ->
        if
            M#machine_state.node_state == error ->
                M;
            M#machine_state.node == Node ->
                % breaking the link to the cloud_worker_port_sup
                % is always detected before a nodedown is sent
                % (so this may rarely, if ever happen)
                restart_peer_node(erl_peer_program(),
                    erl_peer_command_line_args(erlang:get_cookie()), M);
            true ->
                case cloud_worker_port_sup:get_cworker_index(NodeName) of
                    error ->
                        ?LOG_ERROR("unknown node ~p is down: ~p",
                                   [Node, InfoList]);
                    Index ->
                        cloud_worker_port_sup:restart_port(
                            {cloud_worker_port_sup:get_worker(Index),
                             M#machine_state.node})
                end,
                M
        end
    end, HostName, MachineStates),
    if
        NewMachineStates == error ->
            State;
        true ->
            State#state{machine_states = NewMachineStates}
    end.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

%% get the machine states based on the configuration information
%% that specified the machines that are available.
%% make sure the nodes are started, if possible.
get_machine_states(Config) when is_record(Config, config) ->
    PeerProgram = erl_peer_program(),
    PeerArgs = erl_peer_command_line_args(Config#config.cookie),
    {ok, HostName} = inet:gethostname(),
    LongNamesUsed = case net_kernel:longnames() of
        true ->
            true = Config#config.node_longnames,
            true;
        false ->
            false = Config#config.node_longnames,
            false;
        ignored ->
            Config#config.node_longnames
    end,
    get_machine_states([], Config#config.machines, 0,
                       Config#config.cookie,
                       LongNamesUsed,
                       HostName,
                       PeerProgram,
                       PeerArgs).

get_machine_states(MachineStates, [], Entries, _, _, _, _, _)
    when is_list(MachineStates),
         is_integer(Entries), Entries >= 1 ->
    MachineStates;

get_machine_states(MachineStates, [M | MachineConfigs], 0,
                   Cookie, LongNamesUsed, HostName, PeerProgram, PeerArgs)
    when is_list(MachineStates), is_record(M, config_machine),
         is_atom(Cookie), is_boolean(LongNamesUsed),
         is_list(HostName), is_list(PeerProgram), is_list(PeerArgs) ->
    % first entry must be the local host
    true = found_local_host(M#config_machine.node_name_parsed, HostName),
    DistributedModeResult = if
        LongNamesUsed ->
            net_kernel:start([M#config_machine.node_name, longnames]);
        true ->
            net_kernel:start([M#config_machine.node_name, shortnames])
    end,
    case DistributedModeResult of
        {ok, _} ->
            ok;
        {error, {already_started, _}} ->
            ok;
        {error, Reason} ->
            throw(string_extensions:format(
                "distributed mode failed: ~p", [Reason]))

    end,
    erlang:set_cookie(node(), Cookie),
    % -name or -sname get the env value automatically,
    % but not with an explicit call to start/1
    case application:get_env(kernel, net_ticktime) of
        undefined ->
            ok;
        {ok, TickTime} ->
            net_kernel:set_net_ticktime(TickTime)
    end,
    % store the local host
    get_machine_states(MachineStates ++ [
        #machine_state{
            node = node(),
            node_state = ok,
            node_name_parsed = M#config_machine.node_name_parsed,
            ports = M#config_machine.ports,
            processes = M#config_machine.processes,
            threads = M#config_machine.threads
        }], MachineConfigs, 1, Cookie, LongNamesUsed, HostName,
        PeerProgram, PeerArgs);

get_machine_states(MachineStates, [M | MachineConfigs], Entries,
                   Cookie, LongNamesUsed, HostName, PeerProgram, PeerArgs)
    when is_list(MachineStates), is_record(M, config_machine),
         is_atom(Cookie), is_boolean(LongNamesUsed),
         is_list(HostName), is_list(PeerProgram), is_list(PeerArgs) ->
    get_machine_states(MachineStates ++ [
        restart_peer_node(PeerProgram, PeerArgs, #machine_state{
            node = M#config_machine.node_name,
            node_state = error,
            node_name_parsed = M#config_machine.node_name_parsed,
            ports = M#config_machine.ports,
            processes = M#config_machine.processes,
            threads = M#config_machine.threads
        })], MachineConfigs, Entries + 1,
        Cookie, LongNamesUsed, HostName,
        PeerProgram, PeerArgs).

%% search for a node in the machine states when given a hostname
find_machine_state(_, []) ->
    undefined;
find_machine_state(HostName, [#machine_state{node = Node} = M | MachineStates])
    when is_list(HostName) ->
    case string_extensions:after_character($@, atom_to_list(Node)) of
        HostName ->
            M;
        _ ->
            find_machine_state(HostName, MachineStates)
    end.

%% update a machine_state entry based on a hostname
update_machine_state_by_hostname(F, HostName, MachineStates)
    when is_function(F, 1), is_list(HostName), is_list(MachineStates) ->
    update_machine_state_by_hostname(F, HostName, [], MachineStates).
update_machine_state_by_hostname(_, _, _, []) ->
    error;
update_machine_state_by_hostname(F, HostName, CheckedMachineStates,
    [#machine_state{node = Node} = M | RemainingMachineStates]) ->
    case string_extensions:after_character($@, atom_to_list(Node)) of
        HostName ->
            CheckedMachineStates ++ [F(M) | RemainingMachineStates];
        _ ->
            update_machine_state_by_hostname(F, HostName,
                CheckedMachineStates ++ [M], RemainingMachineStates)
    end.

%% cloud_peer:start program to setup the erlang environment
erl_peer_program() ->
    {ok, CurrentWorkingDirectory} = file:get_cwd(),
    EpmdPort = case os:getenv("ERL_EPMD_PORT") of
        false ->
            "0";
        L when is_list(L) ->
            L
    end,
    lists:flatten([
        filename:join([CurrentWorkingDirectory, "bin/execute_erl"]),
        " ", EpmdPort, " ",
        atom_to_list(lib:progname())]).

%% cloud_peer:start arguments to the erl command line
erl_peer_command_line_args(Cookie) when is_atom(Cookie) ->
    {ok, CurrentWorkingDirectory} = file:get_cwd(),
    PathArgs = case init:get_argument(pz) of
        {ok, Paths} when is_list(Paths) ->
            lists:foldl(fun([Path], Arg) ->
                lists:append([Arg, " -pz ",
                    filename:join([CurrentWorkingDirectory, Path])])
            end, "", Paths);
        _ ->
            ""
    end,
    lists:flatten([PathArgs, " -setcookie ", atom_to_list(Cookie)]).

%% match the parsed node name to the parsed local host name
found_local_host({_, Host, _}, Host) when is_list(Host) ->
    true;
found_local_host({_, Host}, Host) when is_list(Host) ->
    true;
found_local_host(_, _) ->
    false.

start_machine(Machine) when is_record(Machine, machine_state) ->
    if
        Machine#machine_state.node_state == ok ->
            % start the remote supervisor for port processes
            case cloud_worker_port_sup:start_link(
                Machine#machine_state.worker_port_sup,
                Machine#machine_state.node) of
                {ok, Pid} when is_pid(Pid) ->
                    % start all the remote port process
                    % under the remote supervisor
                    case cloud_worker_port_sup:create_ports(
                        Machine#machine_state.node,
                        Machine#machine_state.processes) of
                        ok ->
                            Machine#machine_state{worker_port_sup = Pid};
                        error ->
                            Machine#machine_state{node_state = error,
                                                  worker_port_sup = Pid}
                    end;
                {error, Reason} ->
                    ?LOG_ERROR("~p failed to start worker supervisor: ~p",
                               [Machine#machine_state.node, Reason]),
                    Machine#machine_state{node_state = error}
            end;
        true ->
            Machine % ignore dead node
    end.

%% for the nodes that are started, spawn the proper number of worker ports
%% and configure monitoring of the new processes and running nodes.
start_machines(MachineStates) when is_list(MachineStates) ->
    NewMachineStates = lists:map(fun(Machine) ->
        start_machine(Machine)
    end, MachineStates),
    Success = lists:any(fun(Machine) ->
            (Machine#machine_state.node_state == ok) and
            (Machine#machine_state.processes > 0)
        end, NewMachineStates),
    if
        Success ->
            {ok, NewMachineStates};
        true ->
            {error, NewMachineStates}
    end.

%% restart all but the first node
%% (i.e., reconnect and re-spawn the Erlang VM, if necessary)
restart_peer_nodes([MainMachine | MachineStates])
    when is_list(MachineStates) ->
    PeerProgram = erl_peer_program(),
    PeerArgs = erl_peer_command_line_args(erlang:get_cookie()),
    [MainMachine | lists:map(fun(Machine) ->
        restart_peer_node(PeerProgram, PeerArgs, Machine)
    end, MachineStates)].

%% restart a node, if possible
%% (i.e., reconnect and re-spawn the Erlang VM, if necessary)
restart_peer_node(PeerProgram, PeerArgs, Machine)
    when is_record(Machine, machine_state) ->
    case net_kernel:connect(Machine#machine_state.node) of
        true ->
            if
                Machine#machine_state.node_state == error ->
                    ?LOG_ERROR("reconnected ~p node",
                               [Machine#machine_state.node]),
                    Machine#machine_state{node_state = ok};
                true ->
                    Machine
            end;
        _ ->
            PeerStartResult = case Machine#machine_state.node_name_parsed of
                {PeerName, PeerHostName, PeerDomain} ->
                    cloud_peer:start(PeerHostName ++ "." ++ PeerDomain,
                                     PeerName, PeerArgs, PeerProgram,
                                     ?CLOUD_PEER_START_TIMEOUT);
                {PeerName, PeerHostName} ->
                    cloud_peer:start(PeerHostName,
                                     PeerName, PeerArgs, PeerProgram,
                                     ?CLOUD_PEER_START_TIMEOUT)
            end,
            {State, Node} = case PeerStartResult of
                {ok, PeerNode} ->
                    {ok, PeerNode};
                {error, {already_running, PeerNode}} ->
                    {ok, PeerNode};
                {error, Reason} ->
                    ?LOG_ERROR("cloud_peer:start of ~p failed: ~p",
                               [Machine#machine_state.node, Reason]),
                    {error, Machine#machine_state.node}
            end,
            Machine#machine_state{node = Node, node_state = State}
    end.

%% halt all the nodes
halt_all_nodes([MainMachine | PeerNodes] = AllMachines)
    when is_list(AllMachines) ->
    lists:foreach(fun(Machine) ->
        if
            Machine#machine_state.node_state == ok ->
                lists:foreach(fun(Index) ->
                    WorkerId = cloud_worker_port_sup:get_worker(Index),
                    Process = {WorkerId, Machine#machine_state.node},
                    try cloud_worker_port:worker_stop(Process)
                    catch
                        exit:{timeout, _} ->
                            % cloud_worker_port process may not terminate
                            % because it might be ignoring the
                            % abortTask variable
                            cloud_worker_port_sup:stop_port(Process),
                            ?LOG_WARNING("forced exit of ~p on ~p",
                                         [WorkerId, Machine#machine_state.node])
                    end
                end, lists:seq(1, Machine#machine_state.processes));
            true ->
                ok
        end
    end, AllMachines),
    [MainMachine#machine_state{node_state = error} | lists:map(fun(Machine) ->
        halt_peer_node(Machine)
    end, PeerNodes)].

%% halt the node
halt_peer_node(Machine) when is_record(Machine, machine_state) ->
    if
        Machine#machine_state.node_state == ok ->
            spawn(Machine#machine_state.node, fun() -> erlang:halt() end);
        true ->
            ok
    end,
    Machine#machine_state{node_state = error}.
    
%% start a cnode worker whose erlang process has been
%% started by the cloud_worker_port_sup
worker_node_ready(
    {Name, Node}, Reset, #state{machine_states = MachineStates} = State)
    when is_atom(Name), is_atom(Node), is_boolean(Reset) ->
    % start the worker
    if
        Reset ->
            ?LOG_INFO("worker_node_ready ~p on ~p (old)", [Name, Node]);
        true ->
            ?LOG_INFO("worker_node_ready ~p on ~p (new)", [Name, Node])
    end,
    case lists:keyfind(Node, #machine_state.node, MachineStates) of
        false ->
            State;
        Machine when is_record(Machine, machine_state) ->
            Started = if
                Reset ->
                    % old instance going through state verification
                    % (and state will be reset as necessary)
                    true;
                true ->
                    WorkerIndex = cloud_worker_port_sup:get_worker_index(Name),
                    true = is_integer(WorkerIndex),
                    % to make sure this is a new instance, stop before starting
                    cloud_worker_port:worker_stop_proxy({Name, Node}),
                    % iterate over the possible worker port numbers
                    lists_extensions:iter(fun(PortIndex, Itr) ->
                        % net_kernel:connect_node/1 is called
                        % to create the cnode server socket
                        % with a connection from this node,
                        % so that all locally registered names
                        % on this node work properly
                        case cloud_worker_port:worker_start_proxy({Name, Node},
                            cloud_worker_port_sup:get_cworker(WorkerIndex),
                            lists:nth(PortIndex, Machine#machine_state.ports),
                            Machine#machine_state.instance) of
                            true ->
                                true;
                            false ->
                                Itr()
                        end
                    end, false, lists:seq(WorkerIndex,
                        erlang:length(Machine#machine_state.ports),
                        Machine#machine_state.processes))
            end,
            if
                Started ->
                    % update instance for next worker_start
                    NewMachine = if
                        Reset ->
                            Machine;
                        true ->
                            Machine#machine_state{instance = 
                                (1 + Machine#machine_state.instance) rem 32768
                            }
                    end,
                    % store process entry for allocation later
                    allocate_worker_later(Name, Node, not Reset,
                        Machine#machine_state.threads, State#state{
                        machine_states = lists:keyreplace(Node,
                            #machine_state.node,
                            MachineStates, NewMachine
                        )
                    });
                true ->
                    ?LOG_ERROR("unable to start ~p on ~p", [Name, Node]),
                    State
            end
    end.

%% a worker is now ready to accept work, so add the worker to a list
%% of available workers, to that they will have work allocated soon
%% by the cloud_worker_scheduler
allocate_worker_later(Name, Node, NewInstance, Threads,
                      #state{run_queue_work_state_processes = Processes,
                             worker_scheduler_timer = Timer} = State) ->
    % set timer to trigger sending the processes to the scheduler
    NewTimer = if
        Timer == undefined ->
            erlang:send_after(?WORKER_WORK_ALLOCATION_DELAY,
                self(), allocate_work);
        true ->
            Timer
    end,
    % update the processes list to include the new worker
    NewProcesses = case lists:keyfind(
        {Name, Node}, #run_queue_work_state_process.name, Processes) of
        false ->
            % a totally new process or
            % a process with work assignments that disconnected
            Processes ++ [#run_queue_work_state_process{
                name = {Name, Node},
                new_instance = NewInstance,
                threads_unused = Threads
            }];
        % update accumulated process state if necessary
        % (it is likely these are processes the scheduler did not need)
        #run_queue_work_state_process{new_instance = true} ->
            Processes;
        #run_queue_work_state_process{new_instance = false} 
            when NewInstance == false ->
            % reset_ready message, a reconnect
            % (already a valid entry for it)
            Processes;
        #run_queue_work_state_process{new_instance = false} = P
            when NewInstance == true ->
            % ready message, a new process
            lists:keyreplace({Name, Node}, #run_queue_work_state_process.name,
                Processes, P#run_queue_work_state_process{new_instance = true})
    end,
    State#state{
        run_queue_work_state_processes = NewProcesses,
        worker_scheduler_timer = NewTimer
    }.

