%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Cloudi Worker Scheduler State==
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

-module(cloud_worker_scheduler).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([initialize_state/1,
         deinitialize_state/1,
         add_job_request/2,
         add_job/2,
         remove_job_request/1,
         allocate_work/2,
         work_data_done/2,
         work_module_done/2,
         work_module_failed/2]).

-include("cloud_configuration.hrl").
-include("cloud_run_queue.hrl").
-include("cloud_logger.hrl").

-record(state,
    {
    run_queue_work_state_processes = [],
    run_queue = undefined}).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Initialize state local to the module.===
%% @end
%%-------------------------------------------------------------------------

-spec initialize_state(Config :: #config{}) -> {'ok', #state{}}.

initialize_state(Config) when is_record(Config, config) ->
    {ok, #state{run_queue = cloud_run_queue:new(Config)}}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Deinitialize state local to the module.===
%% @end
%%-------------------------------------------------------------------------

-spec deinitialize_state(State :: #state{}) -> 'ok'.

deinitialize_state(#state{} = State) when is_record(State, state) ->
    ok.

%%-------------------------------------------------------------------------
%% @doc
%% ===Add a single job to the scheduler and attempt to schedule it===
%% (which will happen if free process threads exist).
%% @end
%%-------------------------------------------------------------------------

-spec add_job_request(Process :: pid() | atom() | {atom(), atom()},
                      L :: string()) ->
    'ok' |
    {'error', any()}.

add_job_request(Process, L) when is_list(L) ->
    try cloud_configuration:parse_job(L) of
        C when is_record(C, config_work) ->
            ok = gen_server:call(Process, {add_work, C}),
            ok
    catch
        _:Reason ->
            {error, Reason}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Perform an add of a single job to the scheduler and attempt to schedule it with local state.===
%% @end
%%-------------------------------------------------------------------------

-spec add_job(Config :: #config_work{}, State :: #state{}) -> #state{}.

add_job(Config, #state{run_queue = Queue} = State) ->
    ?LOG_DEBUG("added work ~p", [Config#config_work.work_title]),
    self() ! allocate_work,
    State#state{run_queue = cloud_run_queue:update([Config], Queue)}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove a job from the cloud.===
%% @end
%%-------------------------------------------------------------------------

-spec remove_job_request(WorkTitle :: string()) -> 'ok' | 'error'.

remove_job_request(WorkTitle) when is_list(WorkTitle) ->
    ?LOG_DEBUG("removed work ~p", [WorkTitle]),
    cloud_work_interface:stop(WorkTitle).

%%-------------------------------------------------------------------------
%% @doc
%% ===Allocate work to processes.===
%% @end
%%-------------------------------------------------------------------------

-spec allocate_work(NewProcesses :: list(#run_queue_work_state_process{}),
                    State :: #state{}) ->
    {list(#run_queue_work_state_process{}),
     list(#run_queue_work_state_process{}), #state{}}.

allocate_work(NewProcesses,
              #state{run_queue_work_state_processes = OldProcesses,
                     run_queue = Queue} = State) ->
    Lookup = cloud_work_manager:get_task_speed_lookup(),
    % make sure the processes are ready for work
    {Processes, UpdatedQueue} = check_processes_before_work_allocation(
        merge_new_and_old_processes(NewProcesses, OldProcesses), Queue),
    ?LOG_DEBUG("~nprocesses to allocate ~p", [Processes]),
    % put work on processes
    case allocate_work(Processes, Lookup, UpdatedQueue) of
        {[], RemainingProcesses, NewQueue} ->
            ?LOG_INFO("worker scheduler is now idle", []),
            {[], RemainingProcesses, 
             State#state{run_queue_work_state_processes = [],
                         run_queue = NewQueue}};
        {AllocatedProcesses, RemainingProcesses, NewQueue} ->
            % start work on the processes
            ?LOG_DEBUG("~nworkers allocated ~p", [AllocatedProcesses]),
            ?LOG_DEBUG("~nallocated queue~p", [NewQueue]),
            % need to call workers here
            {RunningProcesses, FinalQueue} =
            case start_work(AllocatedProcesses) of
                {SuccessfulProcesses, [], []} ->
                    ?LOG_DEBUG("~nall workers configured ~p",
                        [SuccessfulProcesses]),
                    {SuccessfulProcesses, NewQueue};
                {SuccessfulProcesses, FailedProcesses, FailedAssignments} ->
                    ?LOG_DEBUG("~nworkers configured ~p"
                        "(~p process failures, "
                        "~p work assignment failures)",
                        [SuccessfulProcesses, FailedProcesses,
                         FailedAssignments]),
                    % wipe out any work remaining on the process
                    % by restarting the worker process
                    % (all assignments have failed)
                    lists:foreach(fun(Process) ->
                        cloud_worker_port_sup:restart_port(
                            Process#run_queue_work_state_process.name)
                    end, FailedProcesses),
        
                    % remove references to the failed processes and
                    % failed work assignments
                    CleanQueue = cloud_run_queue:remove_work_assignments(
                        FailedAssignments, 
                        cloud_run_queue:remove_work_assignments(
                            FailedProcesses, NewQueue
                        )
                    ),
                    {SuccessfulProcesses,
                     cloud_run_queue:clear_invalid_work(CleanQueue)}
            end,
            {RunningProcesses, RemainingProcesses, 
             State#state{run_queue_work_state_processes = RunningProcesses,
                         run_queue = FinalQueue}}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===All data has been collected for this work type (i.e., work title).===
%% Remove the idle work from the scheduler.
%% @end
%%-------------------------------------------------------------------------

-spec work_data_done(WorkTitle :: string(), State :: #state{}) ->
    {list(#run_queue_work_state_process{}), #state{}}.

work_data_done(WorkTitle, 
               #state{run_queue_work_state_processes = RunningProcesses,
                      run_queue = Queue} = State) ->
    case cloud_run_queue:remove_idle_work(WorkTitle, Queue) of
        error ->
            ?LOG_ERROR("unable to remove idle work type ~p", [WorkTitle]),
            {[], State};
        {ProcessNames, NewQueue} ->
            {NewRunningProcesses, IdleProcesses} =
                stop_work(ProcessNames, RunningProcesses, WorkTitle),
            ?LOG_INFO("work type ~p purged", [WorkTitle]),
            {IdleProcesses,
             State#state{run_queue_work_state_processes = NewRunningProcesses,
                         run_queue = NewQueue}}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===The work module has finished supplying all work of this work type.===
%% (i.e., for this work title)
%% @end
%%-------------------------------------------------------------------------

-spec work_module_done(WorkTitle :: string(), State :: #state{}) -> #state{}.

work_module_done(WorkTitle, #state{run_queue = Queue} = State) ->
    case cloud_run_queue:put_done(WorkTitle, Queue) of
        error ->
            ?LOG_ERROR("unable to set work type ~p as \"done\"", [WorkTitle]),
            State;
        NewQueue ->
            ?LOG_INFO("work type ~p is \"done\"", [WorkTitle]),
            State#state{run_queue = NewQueue}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===The work module failed so the work title has failed.===
%% @end
%%-------------------------------------------------------------------------

-spec work_module_failed(WorkTitle :: string(), State :: #state{}) -> #state{}.

work_module_failed(WorkTitle, #state{run_queue = Queue} = State) ->
    case cloud_run_queue:put_failed(WorkTitle, Queue) of
        error ->
            ?LOG_ERROR("unable to set work type ~p as \"failed\"", [WorkTitle]),
            State;
        NewQueue ->
            ?LOG_INFO("work type ~p is \"failed\"", [WorkTitle]),
            State#state{run_queue = NewQueue}
    end.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

%% merge the new and old processes by their status as new or old instances
merge_new_and_old_processes([], OldProcesses) when is_list(OldProcesses) ->
    OldProcesses;
merge_new_and_old_processes([#run_queue_work_state_process{
        name = ProcessName
    } = P | NewProcesses], OldProcesses)
    when is_list(NewProcesses), is_list(OldProcesses) ->
    case lists:keyfind(ProcessName, #run_queue_work_state_process.name,
                       OldProcesses) of
        false ->
            merge_new_and_old_processes(NewProcesses, OldProcesses ++ [P]);
        #run_queue_work_state_process{assignments = OldAssignments} = OldP
        when P#run_queue_work_state_process.new_instance == true ->
            if
                % a process crashed, attempt to reassign old work
                erlang:length(OldAssignments) > 0 ->
                    ?LOG_INFO("attempt to verify old work on new process ~p", [
                              ProcessName]),
                    % ignore start_work_assignment/4 outcome and let
                    % check_processes_before_work_allocation/3 handle it later
                    start_work_assignment([], [], OldAssignments, ProcessName),
                    % change new_instance to false so that the process
                    % state goes through the
                    % check_processes_before_work_allocation/3 validation
                    % (assumption here is that start_work was
                    %  already called on this process)
                    merge_new_and_old_processes(NewProcesses, 
                        lists:keyreplace(ProcessName,
                            #run_queue_work_state_process.name, OldProcesses,
                            OldP#run_queue_work_state_process{
                                new_instance = false
                            }
                        )
                    );
                % no old work to reassign
                true ->
                    % infrequent case that might occur as work is finishing
                    % (all data is not yet collected
                    %  but the work module was stopped)
                    merge_new_and_old_processes(NewProcesses, 
                        lists:keyreplace(ProcessName,
                            #run_queue_work_state_process.name, OldProcesses, P
                        )
                    )
            end;
        OldP -> % when P#run_queue_work_state_process.new_instance == false
            ?LOG_INFO("attempt to verify old process ~p", [ProcessName]),
            merge_new_and_old_processes(NewProcesses, 
                lists:keyreplace(P#run_queue_work_state_process.name,
                    #run_queue_work_state_process.name, OldProcesses,
                    OldP#run_queue_work_state_process{new_instance = false}
                )
            )
    end.

%% resolve inconsistent state between the run queue and the worker processes
check_processes_before_work_allocation(Processes, Queue)
    when is_list(Processes) ->
    check_processes_before_work_allocation([], Processes, Queue).

check_processes_before_work_allocation(CheckedProcesses, [], Queue)
    when is_list(CheckedProcesses) ->
    {CheckedProcesses, Queue};

check_processes_before_work_allocation(CheckedProcesses,
    [#run_queue_work_state_process{
        name = ProcessName,
        new_instance = true,
        threads_used = 0
    } = P | Processes], Queue)
    when is_list(CheckedProcesses) ->
    % make sure to remove any invalid references in the run queue
    CleanQueue = cloud_run_queue:remove_work_assignment(ProcessName, Queue),
    % no work assignments now exist for this process
    check_processes_before_work_allocation(CheckedProcesses ++ [
            % assignments should be empty here, but just making sure it is
            P#run_queue_work_state_process{assignments = []}
        ], Processes, CleanQueue);

check_processes_before_work_allocation(CheckedProcesses,
    [#run_queue_work_state_process{
        name = ProcessName,
        new_instance = false,
        assignments = Assignments,
        threads_used = ThreadsUsed,
        threads_unused = ThreadsUnused
    } = P | Processes], Queue)
    when is_list(CheckedProcesses) ->
    % process was restarted, validate the process state
    {Restart, InvalidAssignments} = lists_extensions:itera2(
        fun(#run_queue_work_state_process_assignment{
            work_title = WorkTitle,
            id_offset = IdOffset,
            threads = Threads} = A, R, AList, Itr) ->
        case cloud_worker_port:has_work(
            ProcessName, WorkTitle, IdOffset, Threads) of
            {ok, true} ->
                % do nothing here since the work exists as expected
                Itr(R, AList);
            {ok, false} ->
                cloud_worker_port:remove_work(ProcessName, WorkTitle),
                Itr(R, AList ++ [{ProcessName, A}]);
            {error, _} = Error ->
                ?LOG_ERROR("worker process ~p has_work query failed: ~p", [
                           ProcessName, Error]),
                Itr(true, AList)
        end
    end, false, [], Assignments),
    if
        Restart ->
            % restart the process because a function call failed
            cloud_worker_port_sup:restart_port(ProcessName),
            % remove any old references to the process in the run_queue
            CleanQueue = cloud_run_queue:remove_work_assignment(P, Queue),
            % discard the process,
            % since it will redo the scheduling process after it restarts
            check_processes_before_work_allocation(CheckedProcesses,
                Processes, CleanQueue);
        erlang:length(InvalidAssignments) > 0 ->
            ?LOG_WARNING("~ninvalid work assignments: ~p", [
                         InvalidAssignments]),
            ThreadsFreed = lists:foldl(fun({{_, _}, A}, T) ->
                A#run_queue_work_state_process_assignment.threads + T
            end, 0, InvalidAssignments),
            % remove the invalid assignments from the run queue
            CleanQueue = cloud_run_queue:remove_work_assignments(
                InvalidAssignments, Queue),
            % update the process information
            check_processes_before_work_allocation(CheckedProcesses ++ [
                P#run_queue_work_state_process{
                    new_instance = true, % now a valid process
                    threads_used = ThreadsUsed - ThreadsFreed,
                    threads_unused = ThreadsUnused + ThreadsFreed
                }], Processes, CleanQueue);
        true ->
            check_processes_before_work_allocation(CheckedProcesses ++ [
                P#run_queue_work_state_process{
                    new_instance = true % now a valid process
                }], Processes, Queue)
    end;

check_processes_before_work_allocation(CheckedProcesses,
    [P | Processes], Queue)
    when is_list(CheckedProcesses),
         is_record(P, run_queue_work_state_process) ->
    check_processes_before_work_allocation(CheckedProcesses ++ [P],
        Processes, Queue).

%% start the work module and provide a way for the work module
%% to report when it either finishes or fails
start_work_module(Parent, WorkTitle, WorkModule, WorkInstance, WorkArguments)
    when is_pid(Parent), is_list(WorkTitle),
         is_atom(WorkModule), is_atom(WorkInstance), is_list(WorkArguments) ->
    Child = spawn(fun() ->
        erlang:process_flag(trap_exit, true),
        try erlang:apply(WorkModule, 'start_link',
                         [WorkInstance, WorkArguments]) of
            ok ->
                Parent ! {self(), ok},
                link(Parent),
                receive
                    {'EXIT', Parent, _} ->
                        % our parent process just died
                        ok;
                    {'EXIT', _, ignore} ->
                        % timeout on reply from start_link/2 occurred
                        ok;
                    {'EXIT', _, normal} ->
                        gen_server:cast(Parent, {work_module_done, WorkTitle});
                    {'EXIT', _, Reason} ->
                        ?LOG_ERROR("work type ~p failed: ~p",
                            [WorkTitle, Reason]),
                        gen_server:cast(Parent, {work_module_failed, WorkTitle})
                end;
            {error, _} = Error ->
                Parent ! {self(), Error}
        catch
            _:Reason ->
                Parent ! {self(), {error, Reason}}
        end
    end),
    receive
        {Child, Result} ->
            Result
    after
        5000 ->
            % make sure zombie work module processes are killed
            % so that future start attempts will not be blocked
            erlang:exit(Child, ignore),
            {error, timeout}
    end.

%% allocate as much waiting work from the queue, as possible for the
%% processes that have requested work allocation
allocate_work(Processes, Lookup, Queue)
    when is_list(Processes), is_record(Queue, run_queue) ->
    case cloud_run_queue:get_waiting(Queue) of
        {none, RemainingQueue} ->
            % no work remains to be processed
            % {idle, busy} == {ProcessesIn, ProcessesOut}
            {ProcessesIn, ProcessesOut} = lists:partition(fun(P) ->
                (P#run_queue_work_state_process.threads_used == 0)
            end, Processes),
            {ProcessesOut, ProcessesIn, RemainingQueue};
        {Work, RemainingQueue} ->
            % found work that is waiting to be executed
            WorkTitle = Work#run_queue_work_state.work_title,
            % distinguish between the work module and the work instance
            % (the work instance is the work title,
            %  and includes a ".tag" string as a suffix)
            {WorkModule, WorkInstance} =
            case string_extensions:before_character($., WorkTitle) of
                [] ->
                    A = list_to_atom(WorkTitle),
                    {A, A};
                L when is_list(L) ->
                    {list_to_atom(L), list_to_atom(WorkTitle)}
            end,
            {WorkLoaded, InitialTaskSize} = case c:l(WorkModule) of
                {module, WorkModule} ->
                    StartResult = start_work_module(self(),
                        WorkTitle, WorkModule, WorkInstance,
                        Work#run_queue_work_state.work_arguments),
                    if
                        StartResult /= ok ->
                            ?LOG_ERROR("ignoring work type ~p: ~p",
                                [WorkTitle, StartResult]),
                            {error, 0.0};
                        true ->
                            case cloud_work_interface:get_initial_task_size(
                                WorkTitle) of
                                S when S >= 1.0; S =< 0.0 ->
                                    ?LOG_ERROR("get_initial_task_size/0 "
                                        "is invalid for ~p", [WorkTitle]),
                                    cloud_work_interface:stop(WorkTitle),
                                    {error, 0.0};
                                S ->
                                    {ok, S}
                            end
                    end;          
                _ ->
                    ?LOG_ERROR("error loading work ~p", [WorkTitle]),
                    {error, 0.0}
            end,
            if
                WorkLoaded == error ->
                    % make sure the failed work type is
                    % removed from the run queue
                    gen_server:cast(self(), {work_data_done, WorkTitle}),
                    allocate_work(Processes, Lookup, 
                        cloud_run_queue:put_failed(Work, RemainingQueue));
                true ->
                    ThreadsRequested = 
                        Work#run_queue_work_state.concurrent_tasks_requested,
                    case allocate_work_to_processes(ThreadsRequested, 0,
                        InitialTaskSize, Work, [], Processes, Lookup) of
                        {0, _, ProcessesOut, ProcessesIn, NewWork} ->
                            % continue attempting to allocate processes to tasks
                            % since all threads requested were allocated
                            allocate_work(ProcessesOut ++ ProcessesIn, Lookup, 
                                cloud_run_queue:put_running(
                                    NewWork, RemainingQueue));
                        {all, ThreadsAllocated, ProcessesOut, [], NewWork} ->
                            % it is possible that a static number of threads
                            % were allocated per process, so attempt to
                            % schedule another work type
                            allocate_work(ProcessesOut, Lookup, 
                                cloud_run_queue:put_running(
                                    NewWork, RemainingQueue));
                            
                        {_, ThreadsAllocated, ProcessesOut, [], NewWork}
                            when ThreadsAllocated > 0 ->
                            % not all of the threads requested were allocated
                            % (unless 'all') but some were,
                            % so just return the result
                            {ProcessesOut, [],
                             cloud_run_queue:put_running(
                                NewWork, RemainingQueue)};
                        {_, ThreadsAllocated, ProcessesOut, [], NewWork}
                            when ThreadsAllocated == 0 ->
                            % no threads were allocated,
                            % so just put the task back
                            % on the queue in a waiting state
                            {ProcessesOut, [],
                             cloud_run_queue:put_waiting(
                                NewWork, RemainingQueue)}
                    end
            end
    end.

%% allocate a specific type of work to the processes that have
%% requested work allocation
allocate_work_to_processes(0, ThreadIdOffset,
    _, Work, ProcessesOut, ProcessesIn, _) ->
    % no more threads need to be allocated
    {0, ThreadIdOffset, ProcessesOut, ProcessesIn, Work};

allocate_work_to_processes(ThreadsRequested, ThreadIdOffset,
    _, Work, ProcessesOut, [], _) ->
    % no more processes exist for task allocation
    {ThreadsRequested, ThreadIdOffset, ProcessesOut, [], Work};

allocate_work_to_processes(all, ThreadIdOffset,
    InitialTaskSize, #run_queue_work_state{
        work_title = WorkTitle,
        use_threads = UseThreads,
        process_names = ProcessNames} = Work,
    ProcessesOut, ProcessesIn, Lookup)
    when is_integer(ThreadIdOffset), is_float(InitialTaskSize),
         is_list(ProcessesOut), is_list(ProcessesIn) ->
    % the process (host) that can handle the most work of this type
    {Process, _} = cloud_task_speed_lookup:get_max_tasksize(
        ProcessesIn, WorkTitle, InitialTaskSize, Lookup),
    RemainingProcessesIn = lists:delete(Process, ProcessesIn),
    #run_queue_work_state_process{
        name = ProcessName,
        assignments = Assignments,
        threads_used = ThreadsUsed,
        threads_unused = ThreadsUnused} = Process,
    if 
        ThreadsUnused == 0 ->
            % process is full
            allocate_work_to_processes(all, ThreadIdOffset,
                InitialTaskSize, Work,
                ProcessesOut ++ [Process], RemainingProcessesIn, Lookup);
        true ->
            % use all available threads
            ProcessThreads = if
                is_integer(UseThreads) ->
                    erlang:min(UseThreads, ThreadsUnused);
                UseThreads == false ->
                    1;
                true ->
                    ThreadsUnused
            end,
            allocate_work_to_processes(all,
                ThreadIdOffset + ProcessThreads, InitialTaskSize,
                Work#run_queue_work_state{
                    process_names = ProcessNames ++ [ProcessName]
                },
                ProcessesOut ++ [Process#run_queue_work_state_process{
                    assignments = Assignments ++ [
                        #run_queue_work_state_process_assignment{
                            work_title = WorkTitle,
                            id_offset = ThreadIdOffset,
                            threads = ProcessThreads
                        }
                    ],
                    threads_used = ThreadsUsed + ProcessThreads,
                    threads_unused = ThreadsUnused - ProcessThreads
                }], RemainingProcessesIn, Lookup)
    end;

allocate_work_to_processes(ThreadsRequested, ThreadIdOffset,
    InitialTaskSize, #run_queue_work_state{
        work_title = WorkTitle,
        use_threads = UseThreads,
        process_names = ProcessNames} = Work,
    ProcessesOut, ProcessesIn, Lookup)
    when is_integer(ThreadsRequested), is_integer(ThreadIdOffset),
         is_float(InitialTaskSize), 
         is_list(ProcessesOut), is_list(ProcessesIn) ->
    % the process (host) that can handle the most work of this type
    {Process, _} = cloud_task_speed_lookup:get_max_tasksize(
        ProcessesIn, WorkTitle, InitialTaskSize, Lookup),
    RemainingProcessesIn = lists:delete(Process, ProcessesIn),
    #run_queue_work_state_process{
        name = ProcessName,
        assignments = Assignments,
        threads_used = ThreadsUsed,
        threads_unused = ThreadsUnused} = Process,
    if 
        ThreadsUnused == 0 ->
            % process is full
            allocate_work_to_processes(ThreadsRequested, ThreadIdOffset,
                InitialTaskSize, Work,
                ProcessesOut ++ [Process], RemainingProcessesIn, Lookup);
        true ->
            % assign some of the threads to a work type
            ProcessThreads = if
                is_integer(UseThreads) ->
                    erlang:min(UseThreads,
                        erlang:min(ThreadsRequested, ThreadsUnused));
                UseThreads == false ->
                    1;
                ThreadsRequested < ThreadsUnused ->
                    ThreadsRequested;
                true ->
                    ThreadsUnused
            end,
            allocate_work_to_processes(
                ThreadsRequested - ProcessThreads,
                ThreadIdOffset + ProcessThreads, InitialTaskSize,
                Work#run_queue_work_state{
                    process_names = ProcessNames ++ [ProcessName]
                },
                ProcessesOut ++ [Process#run_queue_work_state_process{
                    assignments = Assignments ++ [
                        #run_queue_work_state_process_assignment{
                            work_title = WorkTitle,
                            id_offset = ThreadIdOffset,
                            threads = ProcessThreads
                        }
                    ],
                    threads_used = ThreadsUsed + ProcessThreads,
                    threads_unused = ThreadsUnused - ProcessThreads
                }], RemainingProcessesIn, Lookup)
    end.

%% assign work to the workers that have already been
%% allocated to tasks
start_work(Processes) ->
    start_work([], [], [], Processes).

start_work(SuccessfulProcesses, FailedProcesses, FailedAssignments, []) ->
    {SuccessfulProcesses, FailedProcesses, FailedAssignments};

start_work(SuccessfulProcesses, FailedProcesses, FailedAssignments,
           [#run_queue_work_state_process{
               name = ProcessName,
               assignments = Assignments,
               threads_used = ThreadsUsed,
               threads_unused = ThreadsUnused} = P | Processes])
        when length(Assignments) > 0 ->
    case start_work_assignment([], [], Assignments, ProcessName) of
        {Assignments, []} ->
            % all assignments succeeded
            start_work(SuccessfulProcesses ++ [P], FailedProcesses,
                FailedAssignments, Processes);
        {[], Assignments} ->
            % all assignments failed, assume the process has failed
            start_work(SuccessfulProcesses, FailedProcesses ++ [P],
                FailedAssignments, Processes);
        {GoodAssignments, BadAssignments} ->
            % decrease the thread count so the failed assignments are
            % no longer counted
            ThreadsFreed = lists:foldl(fun(A, T) ->
                A#run_queue_work_state_process_assignment.threads + T
            end, 0, BadAssignments),
            % save the failed assignments with the process name
            % for future removal from the run_queue
            start_work(SuccessfulProcesses ++ [P#run_queue_work_state_process{
                    assignments = GoodAssignments,
                    threads_used = ThreadsUsed - ThreadsFreed,
                    threads_unused = ThreadsUnused + ThreadsFreed
                }], FailedProcesses, FailedAssignments ++ 
                lists:map(fun(A) -> {ProcessName, A} end, BadAssignments),
                Processes)
    end.

%% assign a specific worker a specific type of work and
%% keep track of the result
start_work_assignment(SuccessfulAssignments, FailedAssignments, [], _) ->
    {SuccessfulAssignments, FailedAssignments};

start_work_assignment(SuccessfulAssignments, FailedAssignments,
                      [#run_queue_work_state_process_assignment{
                          work_title = WorkTitle,
                          id_offset = ThreadIdOffset,
                          threads = ThreadCount} = A | Assignments],
                      ProcessName) ->
    % unable to determine if the work is already assigned or newly assigned,
    % so check before adding work
    case cloud_worker_port:has_work(
        ProcessName, WorkTitle, ThreadIdOffset, ThreadCount) of
        {ok, true} ->
            start_work_assignment(SuccessfulAssignments ++ [A],
                FailedAssignments, Assignments, ProcessName);
        {ok, false} ->
            case cloud_worker_port:add_work(
                ProcessName, WorkTitle, ThreadIdOffset, ThreadCount) of
                {ok, true} ->
                    start_work_assignment(SuccessfulAssignments ++ [A],
                        FailedAssignments, Assignments, ProcessName);
                {ok, false} ->
                    ?LOG_ERROR("worker process ~p will not accept ~p work", [
                        ProcessName, WorkTitle]),
                    start_work_assignment(SuccessfulAssignments,
                        FailedAssignments ++ [A], Assignments, ProcessName);
                {error, Reason} ->
                    ?LOG_ERROR("worker process ~p add_work failed: ~p", [
                        ProcessName, Reason]),
                    start_work_assignment(SuccessfulAssignments,
                        FailedAssignments ++ [A], Assignments, ProcessName)
            end;
        {error, Reason} ->
            ?LOG_ERROR("worker process ~p has_work failed: ~p", [
                ProcessName, Reason]),
            start_work_assignment(SuccessfulAssignments,
                FailedAssignments ++ [A], Assignments, ProcessName)
    end.

% remove work assignments from the processes list
% and the worker Erlang port process
stop_work(ProcessNames, RunningProcesses, WorkTitle) ->
    stop_work(ProcessNames, RunningProcesses, [], WorkTitle).
stop_work([], RunningProcesses, IdleProcesses, _) ->
    {RunningProcesses, IdleProcesses};
stop_work([{Name, Node} = Process | ProcessNames],
          [FirstProcess | _] = RunningProcesses, IdleProcesses, WorkTitle)
    when is_atom(Name), is_atom(Node), 
         is_record(FirstProcess, run_queue_work_state_process),
         is_list(WorkTitle) ->
    try cloud_worker_port:remove_work(Process, WorkTitle) of
        {ok, true} ->
            ok;
        {ok, false} ->
            ?LOG_ERROR("unable to remove work ~p "
                "assigned to worker ~p on ~p",
                [WorkTitle, Name, Node]);
        {error, Reason} ->
            ?LOG_ERROR("unable to remove work ~p "
                "assigned to worker ~p on ~p with port error: ~p",
                [WorkTitle, Name, Node, Reason])
    catch
        _:Reason ->
            ?LOG_ERROR("unable to remove work ~p "
                "assigned to worker ~p on ~p: ~p",
                [WorkTitle, Name, Node, Reason])
    end,
    case lists:keytake(Process, #run_queue_work_state_process.name,
                       RunningProcesses) of
        false ->
            ?LOG_ERROR("unable to find process ~p on ~p", [Name, Node]),
            stop_work(ProcessNames, RunningProcesses,
                IdleProcesses, WorkTitle);
        {value, #run_queue_work_state_process{
            assignments = Assignments,
            threads_used = ThreadsUsed,
            threads_unused = ThreadsUnused
         } = P, RemainingRunningProcesses} ->
            case lists:keytake(WorkTitle,
                #run_queue_work_state_process_assignment.work_title,
                Assignments) of
                false ->
                    ?LOG_ERROR("unable to find assignments "
                        "for process ~p on ~p", [Name, Node]),
                    stop_work(ProcessNames, RunningProcesses ++ [P],
                        IdleProcesses, WorkTitle);
                {value, #run_queue_work_state_process_assignment{
                    threads = FreedThreads
                 }, RemainingAssignments} ->
                    NewThreadsUsed = ThreadsUsed - FreedThreads,
                    NewThreadsUnused = ThreadsUnused + FreedThreads,
                    if 
                        erlang:length(RemainingAssignments) == 0 ->
                            stop_work(ProcessNames,
                                RemainingRunningProcesses,
                                IdleProcesses ++
                                [P#run_queue_work_state_process{
                                    assignments = RemainingAssignments,
                                    threads_used = NewThreadsUsed,
                                    threads_unused = NewThreadsUnused
                                }], WorkTitle);
                        true ->
                            stop_work(ProcessNames,
                                RemainingRunningProcesses ++
                                [P#run_queue_work_state_process{
                                    assignments = RemainingAssignments,
                                    threads_used = NewThreadsUsed,
                                    threads_unused = NewThreadsUnused
                                }], IdleProcesses, WorkTitle)
                    end
            end
    end.

