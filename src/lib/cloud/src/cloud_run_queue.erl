%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Cloudi Run Queue==
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

-module(cloud_run_queue).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([new/1, update/2,
         get_waiting/1, put_waiting/2, put_running/2,
         put_failed/2, put_done/2,
         remove_idle_work/2,
         remove_work_assignment/2,
         remove_work_assignments/2,
         clear_invalid_work/1]).

-include("cloud_run_queue.hrl").
-include("cloud_logger.hrl").
-include("cloud_configuration.hrl").

%%% work_manager_run_queue defines state transitions for work
%%% as it is processed.
%%%
%%% (config) is the starting state obtained from the configuration file parsing
%%%                      work --(config)--> waiting --+--> running ---> done
%%%                                            ^------'       |
%%%                                                           |
%%%                                         failed <----------'

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a new run queue from the configuration.===
%% @end
%%-------------------------------------------------------------------------

-spec new(Config :: #config{}) -> #run_queue{}.

new(Config)
    when is_record(Config, config) ->
    load_work(Config#config.jobs, #run_queue{}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Update an old run queue from the current configuration.===
%% (merging work)
%% @end
%%-------------------------------------------------------------------------

-spec update(Config :: list(#config_work{}),
             Queue :: #run_queue{}) -> #run_queue{}.

update(ConfigJobs, Queue)
    when is_list(ConfigJobs), is_record(Queue, run_queue) ->
    load_work(ConfigJobs, Queue).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get waiting work for assignment.===
%% @end
%%-------------------------------------------------------------------------

-spec get_waiting(Queue :: #run_queue{}) ->
    {'none', #run_queue{}} |
    {#run_queue_work_state{}, #run_queue{}}.

get_waiting(#run_queue{work_waiting = []} = Queue) ->
    {none, Queue};
get_waiting(#run_queue{work_waiting = [WorkState | WorkWaiting]} = Queue)
    when is_record(WorkState, run_queue_work_state) ->
    {WorkState, Queue#run_queue{work_waiting = WorkWaiting}}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Put waiting work back into the queue===
%% (assumes it was from the last get).
%% @end
%%-------------------------------------------------------------------------

-spec put_waiting(WorkState :: #run_queue_work_state{},
                  Queue :: #run_queue{}) ->
    #run_queue{}.

put_waiting(WorkState, #run_queue{work_waiting = WorkWaiting} = Queue)
    when is_record(WorkState, run_queue_work_state) ->
    Queue#run_queue{work_waiting = [WorkState | WorkWaiting]}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Put work in the running queue.===
%% @end
%%-------------------------------------------------------------------------

-spec put_running(WorkState :: #run_queue_work_state{},
                  Queue :: #run_queue{}) ->
    #run_queue{}.

put_running(WorkState, #run_queue{work_running = WorkRunning} = Queue)
    when is_record(WorkState, run_queue_work_state) ->
    Queue#run_queue{work_running = WorkRunning ++ [WorkState]}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Put work from running into the failed queue based on the work title.===
%% @end
%%-------------------------------------------------------------------------

-spec put_failed(WorkTitle :: string() | #run_queue_work_state{},
                 Queue :: #run_queue{}) ->
    'error' |
    #run_queue{}.

put_failed(WorkTitle, #run_queue{work_failed = WorkFailed} = Queue)
    when is_list(WorkTitle) ->
    case lists:keytake(WorkTitle, #run_queue_work_state.work_title,
                       Queue#run_queue.work_running) of
        false ->
            error;
        {value, WorkState, WorkRunning} ->
            Queue#run_queue{work_failed = WorkFailed ++ [WorkState],
                            work_running = WorkRunning}
    end;

%%-------------------------------------------------------------------------
%% @doc
%% ===Put work into the failed queue.===
%% @end
%%-------------------------------------------------------------------------

put_failed(WorkState, #run_queue{work_failed = WorkFailed} = Queue)
    when is_record(WorkState, run_queue_work_state) ->
    Queue#run_queue{work_failed = WorkFailed ++ [WorkState]}.
 
%%-------------------------------------------------------------------------
%% @doc
%% ===Put work in the done queue based on the work title.===
%% @end
%%-------------------------------------------------------------------------

-spec put_done(WorkTitle :: string(), Queue :: #run_queue{}) ->
    'error' |
    #run_queue{}.

put_done(WorkTitle, #run_queue{work_done = WorkDone} = Queue)
    when is_list(WorkTitle) ->
    case lists:keytake(WorkTitle, #run_queue_work_state.work_title,
                       Queue#run_queue.work_running) of
        false ->
            error;
        {value, WorkState, WorkRunning} ->
            Queue#run_queue{work_done = WorkDone ++ [WorkState],
                            work_running = WorkRunning}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove an idle work entry from the run queue.===
%% @end
%%-------------------------------------------------------------------------

-spec remove_idle_work(WorkTitle :: string(), Queue :: #run_queue{}) ->
    {list({atom(), atom()}), #run_queue{}} |
    'error'.

remove_idle_work(WorkTitle, Queue)
    when is_list(WorkTitle), is_record(Queue, run_queue) ->
    case lists:keytake(WorkTitle, #run_queue_work_state.work_title,
                       Queue#run_queue.work_done) of
        false ->
            case lists:keytake(WorkTitle, #run_queue_work_state.work_title,
                               Queue#run_queue.work_failed) of
                false ->
                    error;
                {value, #run_queue_work_state{
                    process_names = ProcessNames
                 }, WorkFailed} ->
                    {ProcessNames, Queue#run_queue{work_failed = WorkFailed}}
            end;
        {value, #run_queue_work_state{
            process_names = ProcessNames
         }, WorkDone} ->
            {ProcessNames, Queue#run_queue{work_done = WorkDone}}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove old mentions of a work assignment in the process_names entry of the run_queue_work_state record.===
%% @end
%%-------------------------------------------------------------------------

-spec remove_work_assignment(ProcessName :: {atom(), atom()} |
                             {{atom(), atom()},
                              #run_queue_work_state_process_assignment{}} |
                             #run_queue_work_state_process{},
                             Queue :: #run_queue{}) ->
    #run_queue{} |
    'error'.

remove_work_assignment({Name, Node} = ProcessName, #run_queue{
        work_running = WorkRunning,
        work_waiting = WorkWaiting} = Queue)
    when is_atom(Name), is_atom(Node) ->
    % remove all mentions of a worker process,
    % more expensive than normal manipulation based on work title
    Clean = fun(List) -> lists:map(
        fun(#run_queue_work_state{process_names = ProcessNames} = WorkState) ->
            WorkState#run_queue_work_state{
                process_names = lists:delete(ProcessName, ProcessNames)
            }
        end, List)
    end,
    Queue#run_queue{
        work_running = Clean(WorkRunning),
        work_waiting = Clean(WorkWaiting)};

remove_work_assignment({{Name, Node} = ProcessName,
    #run_queue_work_state_process_assignment{
        work_title = WorkTitle
    }}, Queue)
    when is_atom(Name), is_atom(Node), is_record(Queue, run_queue) ->
    case lists:keyfind(WorkTitle, #run_queue_work_state.work_title,
                       Queue#run_queue.work_running) of
        false ->
            case lists:keyfind(WorkTitle, #run_queue_work_state.work_title,
                               Queue#run_queue.work_waiting) of
                false ->
                    ?LOG_WARNING("unable to find ~p entry", [WorkTitle]),
                    Queue;
                #run_queue_work_state{
                    process_names = ProcessNames
                } = WorkState ->
                    Queue#run_queue{
                        work_waiting = lists:keyreplace(
                            WorkTitle, #run_queue_work_state.work_title,
                            Queue#run_queue.work_waiting,
                            WorkState#run_queue_work_state{
                                process_names = lists:delete(
                                    ProcessName, ProcessNames
                                )
                            }
                        )
                    }
            end;
        #run_queue_work_state{
            process_names = ProcessNames
        } = WorkState ->
            Queue#run_queue{
                work_running = lists:keyreplace(
                    WorkTitle, #run_queue_work_state.work_title,
                    Queue#run_queue.work_running,
                    WorkState#run_queue_work_state{
                        process_names = lists:delete(
                            ProcessName, ProcessNames
                        )
                    }
                )
            }
    end;

remove_work_assignment(#run_queue_work_state_process{
        name = ProcessName,
        assignments = Assignments
    }, Queue)
    when is_record(Queue, run_queue) ->
    remove_work_assignments(
        lists:map(fun(A) -> {ProcessName, A} end, Assignments), Queue).

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove work assignments from a list of processes.===
%% @end
%%-------------------------------------------------------------------------

-spec remove_work_assignments(list(#run_queue_work_state_process{}) |
                              list({{atom(), atom()},
                                   #run_queue_work_state_process_assignment{}}),
                              Queue :: #run_queue{}) -> #run_queue{}.

remove_work_assignments([], Queue)
    when is_record(Queue, run_queue) ->
    Queue;

remove_work_assignments([Process | RemainingProcesses], Queue)
    when is_record(Process, run_queue_work_state_process),
         is_record(Queue, run_queue) ->
    remove_work_assignments(RemainingProcesses,
        remove_work_assignment(Process, Queue));

remove_work_assignments([{{_, _}, A} = Assignment | RemainingAssignments],
    Queue)
    when is_record(A, run_queue_work_state_process_assignment),
         is_record(Queue, run_queue) ->
    remove_work_assignments(RemainingAssignments, 
        remove_work_assignment(Assignment, Queue)).

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove any work that has failed from the list of running work and add the work to the list of failed work.===
%% @end
%%-------------------------------------------------------------------------

-spec clear_invalid_work(Queue :: #run_queue{}) ->
    #run_queue{}.

clear_invalid_work(#run_queue{work_running = WorkRunning,
                              work_failed = WorkFailed} = Queue) ->
    {NewWorkRunning, Failed} = lists:partition(fun(Work) ->
        erlang:length(Work#run_queue_work_state.process_names) > 0
    end, WorkRunning),
    Queue#run_queue{work_running = NewWorkRunning,
                    work_failed = WorkFailed ++ Failed}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

load_work([], Queue)
    when is_record(Queue, run_queue) ->
    Queue;
load_work([#config_work{work_title = WorkTitle} = Work | RemainingJobs], Queue)
    when is_record(Queue, run_queue) ->
    Done = lists:keymember(
        WorkTitle, #run_queue_work_state.work_title,
        Queue#run_queue.work_done),
    Failed = lists:keymember(
        WorkTitle, #run_queue_work_state.work_title,
        Queue#run_queue.work_failed),
    Running = lists:keymember(
        WorkTitle, #run_queue_work_state.work_title,
        Queue#run_queue.work_running),
    if
        Done ->
            ?LOG_WARNING("ignoring work ~p, "
                "since it is already loaded and \"done\"", [WorkTitle]),
            load_work(RemainingJobs, Queue);
        Failed ->
            ?LOG_WARNING("ignoring work ~p, "
                "since it is already loaded and \"failed\"", [WorkTitle]),
            load_work(RemainingJobs, Queue);
        Running ->
            ?LOG_WARNING("ignoring work ~p, "
                "since it is already loaded and \"running\"", [WorkTitle]),
            load_work(RemainingJobs, Queue);
        true ->
            Waiting = lists:keyfind(
                WorkTitle, #run_queue_work_state.work_title,
                Queue#run_queue.work_waiting),
            % if the entry exists,
            % update it, otherwise add the new entry
            if
                is_record(Waiting, run_queue_work_state) ->
                    % configuration is only updated if
                    % the work is in a waiting state...
                    % otherwise updating the configuration would
                    % disturb the running/dead processes causing
                    % confusion and chaos in the ether.
                    NewWorkState = Waiting#run_queue_work_state{
                        concurrent_tasks_requested = 
                            Work#config_work.concurrent_tasks,
                        use_threads = 
                            Work#config_work.use_threads
                    },
                    load_work(RemainingJobs,
                        Queue#run_queue{work_waiting = 
                            lists:keyreplace(WorkTitle,
                                #run_queue_work_state.work_title,
                                Queue#run_queue.work_waiting,
                                NewWorkState
                            )
                        }
                    );
                true ->
                    % if a job finished and was removed from the done list
                    % it could try to execute a second time if the
                    % configuration was not updated.
                    NewWorkState = #run_queue_work_state{
                        work_title = Work#config_work.work_title,
                        work_arguments = Work#config_work.work_arguments,
                        concurrent_tasks_requested = 
                            Work#config_work.concurrent_tasks,
                        use_threads = 
                            Work#config_work.use_threads
                    },
                    load_work(RemainingJobs,
                        Queue#run_queue{work_waiting = 
                            Queue#run_queue.work_waiting ++ [
                                NewWorkState
                            ]
                        }
                    )
            end
    end.

