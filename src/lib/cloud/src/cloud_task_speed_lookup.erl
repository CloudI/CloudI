%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Cloudi Task Speed Lookup==
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
%%% @version 0.0.3 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloud_task_speed_lookup).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([new/0, update/2, add/6, get_max_tasksize/4, get_tasksize/4]).

-include("cloud_logger.hrl").
-include("cloud_run_queue.hrl").
-include("rbdict.hrl").

-record(task_size_state,
    {
    threads = 0,
    task_size = undefined}).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a new task speed lookup.===
%% @end
%%-------------------------------------------------------------------------

-spec new() -> rbdict().

new() ->
    rbdict:new().

%%-------------------------------------------------------------------------
%% @doc
%% ===Update the task speed lookup with information from new processes.===
%% @end
%%-------------------------------------------------------------------------

-spec update(Processes :: list(#run_queue_work_state_process{}),
             State :: rbdict()) -> rbdict().

update([FirstProcess | _] = Processes, State)
    when is_record(FirstProcess, run_queue_work_state_process) ->
    % assuming this is all the processes for the work they refer to
    % (not that the processes for the work are split between separate lists)

    % clear the number of threads used for the work on a host
    % and initialize storage for the task size state
    CleanedState = lists:foldl(fun(#run_queue_work_state_process{
            name = {_, Node},
            assignments = Assignments}, D1) ->
        HostName = get_hostname_from_node(Node),
        lists:foldl(fun(#run_queue_work_state_process_assignment{
                work_title = WorkTitle}, D2) ->
            Value = case rbdict:find(HostName, D2) of
                error ->
                    rbdict:new();
                {ok, V} ->
                    V
            end,
            rbdict:store(HostName, 
                case rbdict:find(WorkTitle, Value) of
                    error ->
                        rbdict:store(WorkTitle,
                            #task_size_state{threads = 0}, Value);
                    {ok, #task_size_state{threads = 0}} ->
                        Value;
                    {ok, S} when is_record(S, task_size_state) ->
                        rbdict:store(WorkTitle,
                            S#task_size_state{threads = 0}, Value)
                end, D2)
        end, D1, Assignments)
    end, State, Processes),
    % update the current threads that will be reporting from the host
    % for a particular type of work
    lists:foldl(fun(#run_queue_work_state_process{
            name = {_, Node},
            assignments = Assignments}, D3) ->
        HostName = get_hostname_from_node(Node),
        lists:foldl(fun(#run_queue_work_state_process_assignment{
                work_title = WorkTitle,
                threads = Threads}, D4) ->
            {ok, Value} = rbdict:find(HostName, D4),
            {ok, #task_size_state{threads = CurrentThreads} = S} =
                rbdict:find(WorkTitle, Value),
            rbdict:store(HostName,
                rbdict:store(WorkTitle, S#task_size_state{
                    threads = (CurrentThreads + Threads)}, Value), D4)
        end, D3, Assignments)
    end, CleanedState, Processes).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add task speed information to the task speed lookup.===
%% Task allocation will gradually improve to match the capacity of the node.
%% The TargetTime and ElapsedTime is provided as a float in hours.
%% @end
%%-------------------------------------------------------------------------

-spec add(Node :: atom(),
          WorkTitle :: string(),
          TaskSize :: float(),
          TargetTime :: float(),
          ElapsedTime :: float(),
          State :: rbdict()) -> rbdict().

add(Node, WorkTitle, TaskSize, TargetTime, ElapsedTime, State)
    when is_atom(Node), is_list(WorkTitle), is_float(TaskSize),
         is_float(TargetTime), is_float(ElapsedTime) ->
    HostName = get_hostname_from_node(Node),
    rbdict:update(HostName, fun(Value) ->
        % amount of work that can be done in an hour
        TaskScaleFactor = TargetTime / ElapsedTime,
        if
            TaskScaleFactor < 0.1 ->
                ?LOG_DEBUG("node ~p is too slow for ~p "
                           "(with the current task_time_target "
                           "of ~p hours)", [Node, WorkTitle, TargetTime]);
            true ->
                ok
        end,
        SmoothTaskSize = case rbdict:find(WorkTitle, Value) of
            {ok, #task_size_state{threads = Threads,
                                  task_size = undefined} = S} ->
                smooth_task_size(TaskSize * TaskScaleFactor, TaskSize,
                    TargetTime, ElapsedTime, Threads);
            {ok, #task_size_state{threads = Threads,
                                  task_size = OldTaskSize} = S} ->
                smooth_task_size(OldTaskSize * TaskScaleFactor, OldTaskSize,
                    TargetTime, ElapsedTime, Threads)
        end,
        %?LOG_DEBUG("task size on ~p off by a factor of ~p (now ~p)",
        %           [Node, TaskScaleFactor, SmoothTaskSize]),
        rbdict:store(WorkTitle, S#task_size_state{
            task_size = SmoothTaskSize}, Value)
    end, State).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the max tasksize and the node that can handle it.===
%% @end
%%-------------------------------------------------------------------------

-spec get_max_tasksize(Processes :: list(#run_queue_work_state_process{}) |
                                    list(atom()),
                       WorkTitle :: string(),
                       InitialTaskSize :: float(),
                       State :: rbdict()) ->
    {#run_queue_work_state_process{}, float()} |
    {atom(), float()}.

get_max_tasksize([H | _] = Processes, WorkTitle, InitialTaskSize, State)
    when is_record(H, run_queue_work_state_process),
         is_list(Processes), is_list(WorkTitle), is_float(InitialTaskSize) ->
    {TaskSize, Process} = lists:max(lists:map(
    fun(#run_queue_work_state_process{name = {_, N}} = P) ->
        {get_tasksize(N, WorkTitle, 0.0, State), P}
    end, Processes)),
    if
        TaskSize =:= 0.0 ->
            {H, InitialTaskSize};
        true ->
            {Process, TaskSize}
    end;

get_max_tasksize(NodeList, WorkTitle, InitialTaskSize, State)
    when is_list(NodeList), is_list(WorkTitle), is_float(InitialTaskSize) ->
    {TaskSize, Node} = lists:max(lists:map(fun(N) ->
        {get_tasksize(N, WorkTitle, 0.0, State), N}
    end, NodeList)),
    if
        TaskSize =:= 0.0 ->
            {hd(NodeList), InitialTaskSize};
        true ->
            {Node, TaskSize}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the tasksize for a particular node.===
%% @end
%%-------------------------------------------------------------------------

-spec get_tasksize(Node :: atom(),
                   WorkTitle :: string(),
                   InitialTaskSize :: float(),
                   State :: rbdict()) -> float().

get_tasksize(Node, WorkTitle, InitialTaskSize, State)
    when is_atom(Node), is_list(WorkTitle), is_float(InitialTaskSize) ->
    HostName = get_hostname_from_node(Node),
    case find_on_find(HostName, WorkTitle, error, State) of
        error ->
            InitialTaskSize;
        #task_size_state{task_size = undefined} ->
            InitialTaskSize;
        #task_size_state{task_size = Value} ->
            erlang:min(Value, 1.0)
    end.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

get_hostname_from_node(Node) when is_atom(Node) ->
    string_extensions:after_character($@, atom_to_list(Node)).

smooth_task_size(NewTaskSize, OldTaskSize, TargetTime, ElapsedTime, Threads)
    when is_float(NewTaskSize), is_float(OldTaskSize),
         is_float(TargetTime), is_float(ElapsedTime), is_integer(Threads) ->
    Difference = erlang:abs((TargetTime - ElapsedTime) / ElapsedTime),
    % determine the truncated moving average period based on the
    % percentage difference between the elapsed time and the target time
    % (empirically found solution that is relatively stable
    %  and provides slow convergance, until a better solution is found)
    SmoothingFactor = if
        Difference =< 1.0 ->
            math_extensions:product(lists:seq(
                math_extensions:floor(math:log(Difference) / math:log(3.0)) *
                -2 + 1,
            1, -2)) * 8 * erlang:float(Threads);
        true ->
            math_extensions:product(lists:seq(
                math_extensions:ceil(math:log(Difference) / math:log(50.0)) *
                2 + 1,
            1, -2)) * 8 * erlang:float(Threads)
    end,
    % smoothing method as separate sequences
    %SmoothingFactor = if
    %    Difference =< 0.0625 ->
    %        erlang:float(Threads) * 7560.0;
    %    Difference =< 0.125 ->
    %        erlang:float(Threads) * 840.0;
    %    Difference =< 0.25 ->
    %        erlang:float(Threads) * 120.0;
    %    Difference =< 0.5 ->
    %        erlang:float(Threads) * 24.0;
    %    Difference =< 1.0 ->
    %        erlang:float(Threads) * 8.0;
    %    Difference =< 50.0 ->                   % = 1.0 * 50
    %        erlang:float(Threads) * 24.0;       % = 8.0 * 3
    %    Difference =< 2500.0 ->                 % = 50.0 * 50
    %        erlang:float(Threads) * 120.0;      % = 24.0 * 5
    %    Difference =< 125000.0 ->               % = 2500.0 * 50
    %        erlang:float(Threads) * 840.0;      % = 120.0 * 7
    %    Difference =< 6250000.0 ->              % = 125000.0 * 50
    %        erlang:float(Threads) * 7560.0;     % = 840.0 * 9
    %    true ->
    %        erlang:float(Threads) * 83160.0     % = 7560.0 * 11
    %end,
    % perform truncated moving average
    OldTaskSize + (NewTaskSize - OldTaskSize) / SmoothingFactor.

find_on_find(Key1, Key2, Error, Dict1) ->
    case rbdict:find(Key1, Dict1) of
        error ->
            Error;
        {ok, Dict2} ->
            case rbdict:find(Key2, Dict2) of
                error ->
                    Error;
                {ok, Value} ->
                    Value
            end
    end.

