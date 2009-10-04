%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Cloudi Work Status==
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

-module(cloud_work_status).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([new/0, update/2,
         has_work/2,
         has_active_work/2, is_inactive_work/2,
         get_sequence_number/4,
         has_cached_task/3,
         get_cached_task/3, set_cached_task/4, 
         output_length/1, store_output/5, drain_output/2]).

-include("cloud_work_status.hrl").
-include("cloud_run_queue.hrl").
-include("cloud_logger.hrl").
-include("rbdict.hrl").

% wraps the sequence number used externally while
% internal sequence numbers do not wrap
-define(WORKER_SEQUENCE_ID_COUNT, 4294967296).

% arbitrary limit to help prevent exhausting all memory
% when drain_output/2 is waiting on a slow node
% (a type of flow control, to limit the number of results coming in).
% the parameter triggers a warning and sets the work to inactive so that
% the slowest worker can send its result before providing more tasks to other
% workers.  the max output queue size is the parameter multiplied by the
% number of workers.
-define(MAX_OUTPUT_PER_WORKER, 15).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a new work status record.===
%% @end
%%-------------------------------------------------------------------------

-spec new() -> rbdict().

new() ->
    rbdict:new().

%%-------------------------------------------------------------------------
%% @doc
%% ===Update the work status with information from new processes.===
%% @end
%%-------------------------------------------------------------------------

-spec update(Processes :: list(#run_queue_work_state_process{}),
             WorkStatus :: rbdict()) -> rbdict().

update([FirstProcess | _] = Processes, WorkStatus)
    when is_record(FirstProcess, run_queue_work_state_process) ->
    % assuming this is all the processes for the work they refer to
    % (not that the processes for the work are split between separate lists)

    % clear the concurrent task counts for only
    % the work mentioned by the processes
    CleanedWorkStatus = lists:foldl(fun(P, D1) ->
        lists:foldl(fun(#run_queue_work_state_process_assignment{
                work_title = WorkTitle}, D2) ->
            case rbdict:find(WorkTitle, D2) of
                error ->
                    rbdict:store(WorkTitle, #work_status{
                        concurrent_tasks = 0}, D2);
                {ok, #work_status{concurrent_tasks = 0}} ->
                    D2;
                {ok, W} when is_record(W, work_status) ->
                    rbdict:store(WorkTitle, W#work_status{
                        concurrent_tasks = 0}, D2)
            end
        end, D1, P#run_queue_work_state_process.assignments)
    end, WorkStatus, Processes),

    % assign the updated task counts for the work
    lists:foldl(fun(P, D3) ->
        lists:foldl(fun(#run_queue_work_state_process_assignment{
                work_title = WorkTitle,
                threads = Threads}, D4) ->
            {ok, #work_status{concurrent_tasks = CurrentTasks} = W} =
                rbdict:find(WorkTitle, D4),
            rbdict:store(WorkTitle, W#work_status{
                % make everything is active again
                % in case processes were restarted
                active = true,
                concurrent_tasks = CurrentTasks + Threads}, D4)
        end, D3, P#run_queue_work_state_process.assignments)
    end, CleanedWorkStatus, Processes).

%%-------------------------------------------------------------------------
%% @doc
%% ===Determine if the work status contains information for a work title.===
%% @end
%%-------------------------------------------------------------------------

-spec has_work(WorkTitle :: string(),
               WorkStatus :: rbdict()) -> bool().

has_work(WorkTitle, WorkStatus) when is_list(WorkTitle) ->
    rbdict:is_key(WorkTitle, WorkStatus).

%%-------------------------------------------------------------------------
%% @doc
%% ===Check if the work title is marked as active within the work status.===
%% Work goes inactive if data modules are unable
%% to consume all the data (to prevent extreme memory consumption from
%% accumulating garbage).
%% @end
%%-------------------------------------------------------------------------

-spec has_active_work(WorkTitle :: string(),
                      WorkStatus :: rbdict()) -> bool().

has_active_work(WorkTitle, WorkStatus) when is_list(WorkTitle) ->
    case rbdict:find(WorkTitle, WorkStatus) of
        {ok, #work_status{active = true}} ->
            true;
        {ok, #work_status{active = false}} ->
            false;
        error ->
            false
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Set the work to inactive because the work type (work title) appears to be done or failed.===
%% Output will still be accepted and successfully
%% draining the output makes the work active, so it is likely this will
%% be called more than once for the same work type.  After all the output
%% is collected, the request to make the work inactive removes the entry
%% from the work status.
%% @end
%%-------------------------------------------------------------------------

-spec is_inactive_work(WorkTitle :: string(),
                       WorkStatus :: rbdict()) ->
    {'ok', rbdict()} |
    'error'.

is_inactive_work(WorkTitle, WorkStatus) when is_list(WorkTitle) ->
    case rbdict:find(WorkTitle, WorkStatus) of
        {ok, #work_status{working = SequenceLookup,
                          output = OutputQueue} = Status} ->
            Idle = rbdict:all(fun(_, #work_status_working{task = Task}) ->
                Task == undefined 
            end, SequenceLookup),
            if
                Idle, OutputQueue == [] ->
                    cloud_leader:work_data_done(WorkTitle),
                    ?LOG_INFO("purging status of work title ~p", [WorkTitle]),
                    {ok, rbdict:erase(WorkTitle, WorkStatus)};
                true ->
                    % do not set active to false here, since
                    % active must be true for another evaluation of this
                    % function to confirm the work is inactive
                    {ok, rbdict:store(WorkTitle, Status, WorkStatus)}
            end;
        error ->
            ?LOG_CRITICAL("unable to find work title ~p", [WorkTitle]),
            error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the external sequence number when given a type of work (work title, or task) and the worker process index.===
%% The sequence number is saved in the work status as a "cached status"
%% so it can be recovered if the worker dies or fails in some way.
%% @end
%%-------------------------------------------------------------------------

-spec get_sequence_number(WorkTitle :: string(),
                          Id :: non_neg_integer(),
                          Node :: atom(),
                          WorkStatus :: rbdict()) ->
    {non_neg_integer(),
     pos_integer(),
     #work_status_working_task{} | 'undefined',
     rbdict()} |
    'error'.

get_sequence_number(WorkTitle, Id, Node, WorkStatus)
    when is_list(WorkTitle), is_integer(Id), is_atom(Node) ->
    case rbdict:find(WorkTitle, WorkStatus) of
        {ok, #work_status{working_sequence_number = NewNumber,
                          concurrent_tasks = TasksCount,
                          working = SequenceLookup} = Status} ->
            case rbdict:find(Id, SequenceLookup) of
                {ok, #work_status_working{
                        sequence = OldNumber,
                        task = OldTask}} ->
                    {OldNumber rem ?WORKER_SEQUENCE_ID_COUNT,
                     TasksCount, OldTask, WorkStatus};
                error ->
                    {NewNumber rem ?WORKER_SEQUENCE_ID_COUNT,
                     TasksCount, undefined,
                     rbdict:store(WorkTitle, Status#work_status{
                        working_sequence_number = NewNumber + 1,
                        working = rbdict:store(Id,
                            #work_status_working{
                                node = Node,
                                sequence = NewNumber
                            }, SequenceLookup)
                        }, WorkStatus)
                    }
            end;
        error ->
            ?LOG_CRITICAL("unable to find work title ~p ", [WorkTitle]),
            error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Check to make sure the cached task exists.===
%% @end
%%-------------------------------------------------------------------------

-spec has_cached_task(WorkTitle :: string(),
                      Id :: non_neg_integer(),
                      WorkStatus :: rbdict()) ->
    bool().

has_cached_task(WorkTitle, Id, WorkStatus)
    when is_list(WorkTitle), is_integer(Id) ->
    case rbdict:find(WorkTitle, WorkStatus) of
        {ok, #work_status{working = SequenceLookup} = Status} ->
            rbdict:is_key(Id, SequenceLookup);
        error ->
            false
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the cached task that was stored when a sequence number was assigned.===
%% @end
%%-------------------------------------------------------------------------

-spec get_cached_task(WorkTitle :: string(),
                      Id :: non_neg_integer(),
                      WorkStatus :: rbdict()) ->
    #work_status_working_task{} |
    'error'.

get_cached_task(WorkTitle, Id, WorkStatus)
    when is_list(WorkTitle), is_integer(Id) ->
    case rbdict:find(WorkTitle, WorkStatus) of
        {ok, #work_status{working = SequenceLookup} = Status} ->
            case rbdict:find(Id, SequenceLookup) of
                {ok, #work_status_working{task = Task}}
                    when is_record(Task, work_status_working_task) ->
                    Task;
                error ->
                    error
            end;
        error ->
            error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Update the cached task stored for a type of work (work title, or task) and the worker process index.===
%% The data defining the task can be recovered if the
%% worker dies or fails in some way.
%% @end
%%-------------------------------------------------------------------------

-spec set_cached_task(WorkTitle :: string(),
                      Id :: non_neg_integer(),
                      Task :: #work_status_working_task{},
                      WorkStatus :: rbdict()) ->
    {non_neg_integer(), rbdict()} |
    'error'.

set_cached_task(WorkTitle, Id, Task, WorkStatus)
    when is_list(WorkTitle), is_integer(Id),
         is_record(Task, work_status_working_task) ->
    case rbdict:find(WorkTitle, WorkStatus) of
        {ok, #work_status{working = SequenceLookup} = Status} ->
            case rbdict:find(Id, SequenceLookup) of
                {ok, #work_status_working{sequence = Number} = CachedStatus} ->
                    {Number rem ?WORKER_SEQUENCE_ID_COUNT,
                     rbdict:store(WorkTitle, Status#work_status{
                        working = rbdict:store(
                            Id, CachedStatus#work_status_working{task = Task},
                            SequenceLookup)
                        }, WorkStatus)
                    };
                error ->
                    ?LOG_CRITICAL("unable to find worker id ~p "
                        "for work title ~p", [Id, WorkTitle]),
                    error
            end;
        error ->
            ?LOG_CRITICAL("unable to find work title ~p", [WorkTitle]),
            error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Determine the length of the output queue for all work present in the work status.===
%% The count returned only reflects the number of tasks
%% that have data stored after completing successfully.  The count
%% does not reflect the number of queries stored, since they are all
%% nested within the output for each task.
%% @end
%%-------------------------------------------------------------------------

-spec output_length(WorkStatus :: rbdict()) -> non_neg_integer().

output_length(WorkStatus) ->
    rbdict:fold(fun(_, Status, Length) ->
        erlang:length(Status#work_status.output) + Length
    end, 0, WorkStatus).

%%-------------------------------------------------------------------------
%% @doc
%% ===Store the output (list of data repository (database) queries) for a successfully completed task with the unwrapped sequence number.===
%% @end
%%-------------------------------------------------------------------------

-spec store_output(WorkTitle :: string(),
                   Id :: non_neg_integer(),
                   SequenceNumber :: non_neg_integer(),
                   Output :: list({atom(), string()}),
                   WorkStatus :: rbdict()) ->
    {'ok', rbdict()} |
    'duplicate' |
    'error'.

store_output(WorkTitle, Id, SequenceNumber, Output, WorkStatus)
    when is_list(WorkTitle), is_integer(Id), is_integer(SequenceNumber) ->
    case rbdict:find(WorkTitle, WorkStatus) of
        {ok, #work_status{active = Active,
                          remove_sequence_number = NextToRemove,
                          working_sequence_number = MaxNumber,
                          working = SequenceLookup,
                          output = OutputQueue} = Status} ->
            RealSequenceNumber = 
                store_output_sequence_number(SequenceNumber, MaxNumber),
            if
                RealSequenceNumber < NextToRemove ->
                    ?LOG_CRITICAL("duplicate sequence number ~p "
                        "for work title ~p", [RealSequenceNumber, WorkTitle]),
                    duplicate;
                true ->
                    Duplicate = 
                        lists:keyfind(RealSequenceNumber, 1, OutputQueue),
                    if
                        Duplicate /= false ->
                            ?LOG_CRITICAL("duplicate sequence number ~p "
                                "for work title ~p",
                                [RealSequenceNumber, WorkTitle]),
                            duplicate;
                        true ->
                            CurrentlyActive = if
                                Active ->
                                    (rbdict:size(SequenceLookup) *
                                        ?MAX_OUTPUT_PER_WORKER) >
                                        (1 + erlang:length(OutputQueue));
                                true ->
                                    Active
                            end,
                            if
                                CurrentlyActive /= Active ->
                                    SlowWorker = rbdict:iter(fun(_,
                                        #work_status_working{
                                            node = Node,
                                            sequence = WorkingSequence
                                        }, Iter) ->
                                        if
                                            WorkingSequence == NextToRemove ->
                                                Node;
                                            true ->
                                                Iter()
                                        end
                                    end, undefined, SequenceLookup),
                                    ?LOG_WARNING("max output per worker "
                                        "of ~p exceeded for work title ~p, "
                                        "waiting on ~p",
                                        [?MAX_OUTPUT_PER_WORKER, WorkTitle,
                                         SlowWorker]);
                                true ->
                                    ok
                            end,
                            {ok,
                             rbdict:store(WorkTitle,
                                increment_sequence_number(Id,
                                Status#work_status{
                                    active = CurrentlyActive,
                                    output = lists:keymerge(1, OutputQueue,
                                        [{RealSequenceNumber, Output}])
                                }), WorkStatus)}
                    end
            end;
        error ->
            ?LOG_CRITICAL("unable to find work title ~p", [WorkTitle]),
            error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Determine all task (work title) output that is ready to be consumed in the order defined by the sequence number===
%% (missing sequence numbers will prevent other data from being consumed).
%% All the output is passed to the data module responsible for it based
%% on the data title ("DATA_MODULE.DATABASE_NAME").  If the data modules
%% fail to process all the output successfully, the associated task will
%% become inactive to prevent excessive memory consumption.
%% @end
%%-------------------------------------------------------------------------

-spec drain_output(WorkStatus :: rbdict(),
                   DataTypeLookup :: any()) ->
    {'ok', rbdict()} |
    {'error', rbdict()}.

drain_output(WorkStatus, DataTypeLookup) ->
    NewWorkStatus = rbdict:map(fun(WorkTitle,
        #work_status{remove_sequence_number = Sequence,
                     output = OutputQueue} = Status) ->
        {NewSequence, WorkResults, NewOutputQueue} = 
            drain_output_entries(Sequence, [], OutputQueue),
        if
            Sequence /= NewSequence ->
                Remaining = rbsets:fold(fun(DataTitle, Results) ->
                    {S, R} = cloud_data_interface:do_queries(
                        DataTitle, Results),
                    if
                        S /= ok ->
                            ?LOG_ERROR("data repository ~p failed for "
                                "work title ~p", [DataTitle, WorkTitle]);
                        true ->
                            ok
                    end,
                    R
                end, WorkResults, DataTypeLookup),
                if
                    Remaining == [] ->
                        Status#work_status{
                            active = true,
                            remove_sequence_number = NewSequence,
                            output = NewOutputQueue};
                    true ->
                        % store the remaining results so that they can
                        % be drained at a later time
                        RemainingLength = erlang:length(Remaining),
                        RemainingSequence = NewSequence - RemainingLength,
                        if
                            Sequence == RemainingSequence ->
                                ?LOG_ERROR("failed to store all results (~w) "
                                    "for work title ~p (at least partially)",
                                    [RemainingLength, WorkTitle]);
                            true ->
                                ?LOG_ERROR("failed to store some results "
                                    "(~w out of ~w) for work title ~p",
                                    [RemainingLength,
                                     erlang:length(WorkResults), WorkTitle])
                        end,
                        RemainingOutputQueue = lists:zip(lists:seq(
                            RemainingSequence, NewSequence - 1),
                            Remaining) ++ NewOutputQueue,
                        Status#work_status{
                            active = false,
                            remove_sequence_number = RemainingSequence,
                            output = RemainingOutputQueue}
                end;
            true ->
                Status
        end
    end, WorkStatus),
    AllActive = rbdict:all(fun(_, #work_status{active = Active}) ->
        Active end, NewWorkStatus),
    if
        AllActive ->
            {ok, NewWorkStatus};
        true ->
            {error, NewWorkStatus}
    end.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

%% if the sequence number has wrapped, determine which interval it is in
%% to obtain the unwrapped sequence number
store_output_sequence_number(Number, MaxNumber)
    when is_integer(Number), is_integer(MaxNumber) ->
    store_output_sequence_number(
        Number, Number + ?WORKER_SEQUENCE_ID_COUNT, MaxNumber).

store_output_sequence_number(PreviousNumber, Number, MaxNumber)
    when is_number(PreviousNumber), is_integer(Number), is_integer(MaxNumber),
         Number < MaxNumber ->
    store_output_sequence_number(
        Number, Number + ?WORKER_SEQUENCE_ID_COUNT, MaxNumber);

store_output_sequence_number(PreviousNumber, _, _)
    when is_number(PreviousNumber) ->
    PreviousNumber.

%% increment the next working sequence number and use the current
%% working sequence number to add an entry that tracks the
%% currently working thread id
increment_sequence_number(Id, #work_status{working_sequence_number = NewNumber,
                                           working = SequenceLookup} = Status)
    when is_integer(Id) ->
    Status#work_status{
        working_sequence_number = NewNumber + 1,
        working = rbdict:update(Id,
            fun(CachedStatus) ->
                CachedStatus#work_status_working{
                    sequence = NewNumber,
                    task = undefined
                }
            end, SequenceLookup
        )
    }.

%% drain only sequential output entries from the OutputQueue
drain_output_entries(Sequence, DataQueue, [])
    when is_integer(Sequence), is_list(DataQueue) ->
    {Sequence, DataQueue, []};

drain_output_entries(Sequence, DataQueue,
                     [{Sequence, OutputData} | OutputQueue])
    when is_integer(Sequence), is_list(DataQueue), is_list(OutputData) ->
    drain_output_entries(Sequence + 1, DataQueue ++ [OutputData], OutputQueue);

drain_output_entries(Sequence, DataQueue, OutputQueue)
    when is_integer(Sequence), is_list(DataQueue), is_list(OutputQueue) ->
    {Sequence, DataQueue, OutputQueue}.

