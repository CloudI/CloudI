%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Task Size Calculation==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2009-2017 Michael Truog <mjtruog at gmail dot com>
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
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2009-2017 Michael Truog
%%% @version 1.7.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_task_size).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([new/7,
         get/2,
         reduce/3,
         put/4]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-type dict_proxy(Key, Value) :: dict:dict(Key, Value).

-record(node,
    {
        task_size :: number()
    }).

-record(cloudi_task_size,
    {
        task_count :: pos_integer(), % count of concurrent tasks
        task_size_initial :: integer(), % count to control task size
        task_size_min :: integer(),
        task_size_max :: integer(),
        target_time :: float(), % in hours
        target_time_min :: float(), % in hours
        target_time_max :: float(), % in hours
        target_time_incr = 0 :: integer(),
        target_time_decr = 0 :: integer(),
        lookup = dict:new() :: dict_proxy(node(), #node{})
    }).

-type state() :: #cloudi_task_size{}.

-define(TARGET_TIME_ADJUST, 4). % number of consecutive incr/decr to
                                % cause a target time adjustment
-define(TARGET_TIME_ADJUST_FACTOR, 2.0).
-define(TARGET_TIME_USAGE_FACTOR, 2.0). % defines task size tolerance

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a new task size lookup.===
%% @end
%%-------------------------------------------------------------------------

-spec new(TaskCount :: pos_integer(),
          TaskSizeInitial :: integer(),
          TaskSizeMin :: integer(),
          TaskSizeMax :: integer(),
          TargetTimeInitial :: float(),
          TargetTimeMin :: float(),
          TargetTimeMax :: float()) ->
    state().

new(TaskCount,
    TaskSizeInitial, TaskSizeMin, TaskSizeMax,
    TargetTimeInitial, TargetTimeMin, TargetTimeMax)
    when is_integer(TaskCount), TaskCount > 0,
         is_integer(TaskSizeInitial),
         is_integer(TaskSizeMin), is_integer(TaskSizeMax),
         TaskSizeInitial >= TaskSizeMin, TaskSizeInitial =< TaskSizeMax,
         is_float(TargetTimeInitial), TargetTimeInitial > 0.0,
         is_float(TargetTimeMin), is_float(TargetTimeMax),
         TargetTimeInitial >= TargetTimeMin,
         TargetTimeInitial =< TargetTimeMax ->
    #cloudi_task_size{task_count = TaskCount,
                      task_size_initial = TaskSizeInitial,
                      task_size_min = TaskSizeMin,
                      task_size_max = TaskSizeMax,
                      target_time = TargetTimeInitial,
                      target_time_min = TargetTimeMin,
                      target_time_max = TargetTimeMax}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the task size information.===
%% @end
%%-------------------------------------------------------------------------

-spec get(Pid :: pid(),
          State :: state()) ->
    {TaskSize :: integer(),
     Timeout :: cloudi_service:timeout_value_milliseconds()}.

get(Pid,
    #cloudi_task_size{task_size_initial = TaskSizeInitial,
                      target_time = TargetTime,
                      lookup = Lookup})
    when is_pid(Pid) ->
    Timeout = erlang:round(?TARGET_TIME_USAGE_FACTOR *
                           TargetTime * 3600000.0),
    case dict:find(node(Pid), Lookup) of
        {ok, #node{task_size = TaskSize}} ->
            TaskSizeInteger = if
                is_float(TaskSize) ->
                    cloudi_math:floor(TaskSize);
                is_integer(TaskSize) ->
                    TaskSize
            end,
            {TaskSizeInteger, Timeout};
        error ->
            {TaskSizeInitial, Timeout}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Reduce the task size after a timeout.===
%% @end
%%-------------------------------------------------------------------------

-spec reduce(Pid :: pid(),
             Multiplier :: float(),
             State :: state()) ->
    state().

reduce(Pid, Multiplier,
       #cloudi_task_size{task_size_min = TaskSizeMin,
                         task_size_max = TaskSizeMax,
                         lookup = Lookup} = State)
    when is_pid(Pid), is_float(Multiplier),
         Multiplier > 0.0, Multiplier =< 1.0 ->
    Node = node(Pid),
    case dict:find(Node, Lookup) of
        {ok, #node{task_size = TaskSize} = NodeState} ->
            NewTaskSize = task_size_clamp(TaskSize * Multiplier,
                                          TaskSizeMin, TaskSizeMax),
            NewNodeState = NodeState#node{task_size = NewTaskSize},
            State#cloudi_task_size{lookup = dict:store(Node, NewNodeState,
                                                       Lookup)};
        error ->
            State
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Store task size information.===
%% ElapsedTime is in hours.
%% @end
%%-------------------------------------------------------------------------

-spec put(Pid :: pid(),
          TaskSize :: integer(),
          ElapsedTime :: float(),
          State :: state()) ->
    state().

put(Pid, TaskSize, ElapsedTime,
    #cloudi_task_size{task_count = TaskCount,
                      task_size_initial = TaskSizeInitial,
                      task_size_min = TaskSizeMin,
                      task_size_max = TaskSizeMax,
                      target_time = TargetTime0,
                      target_time_incr = TargetTimeIncr,
                      target_time_decr = TargetTimeDecr,
                      target_time_min = TargetTimeMin,
                      target_time_max = TargetTimeMax,
                      lookup = Lookup} = State)
    when is_pid(Pid), is_integer(TaskSize), is_float(ElapsedTime) ->
    Node = node(Pid),
    #node{task_size = OldTaskSize} = NodeState = case dict:find(Node, Lookup) of
        {ok, LookupValue} ->
            LookupValue;
        error ->
            #node{task_size = TaskSizeInitial}
    end,
    TaskSizeSmoothed = task_size_smoothed(TaskSize, OldTaskSize,
                                          TargetTime0, ElapsedTime, TaskCount),
    NewTaskSize = task_size_clamp(TaskSizeSmoothed, TaskSizeMin, TaskSizeMax),
    {NextTargetTimeIncr, TargetTime1} = if
        NewTaskSize < TaskSizeMin + 0.5 ->
            target_time_incr(TargetTimeIncr + 1, TargetTime0, TargetTimeMax);
        true ->
            {0, TargetTime0}
    end,
    {NextTargetTimeDecr, TargetTimeN} = if
        NewTaskSize > TaskSizeMax - 0.5 ->
            target_time_decr(TargetTimeDecr + 1, TargetTime1, TargetTimeMin);
        true ->
            {0, TargetTime1}
    end,
    {NewTargetTimeIncr, NewTargetTimeDecr} = if
        NextTargetTimeIncr > 0, NextTargetTimeDecr > 0 ->
            {0, 0};
        true ->
            {NextTargetTimeIncr, NextTargetTimeDecr}
    end,
    NewLookup = dict:store(Node,
                           NodeState#node{task_size = NewTaskSize},
                           Lookup),
    State#cloudi_task_size{target_time = TargetTimeN,
                           target_time_incr = NewTargetTimeIncr,
                           target_time_decr = NewTargetTimeDecr,
                           lookup = NewLookup}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

task_size_clamp(TaskSize, TaskSizeMin, TaskSizeMax)
    when is_float(TaskSize) ->
    TaskSizeInteger = cloudi_math:floor(TaskSize),
    if
        TaskSizeInteger < TaskSizeMin ->
            TaskSizeMin;
        TaskSizeInteger > TaskSizeMax ->
            TaskSizeMax;
        true ->
            TaskSize
    end.

task_size_smoothed(CurrentTaskSize, OldTaskSize,
                   TargetTimeTotal, ElapsedTime, TaskCount)
    when is_integer(CurrentTaskSize), is_number(OldTaskSize),
         is_float(TargetTimeTotal), is_float(ElapsedTime) ->
    TargetTime = TargetTimeTotal / ?TARGET_TIME_USAGE_FACTOR,
    NextTaskSize = CurrentTaskSize * (TargetTime / ElapsedTime),
    Difference = erlang:abs((TargetTime - ElapsedTime) / ElapsedTime),
    % determine the truncated moving average period based on the
    % percentage difference between the elapsed time and the target time
    % (empirically found solution that is relatively stable
    %  and provides slow convergance, until a better solution is found)
    %product(L) when is_list(L) ->
    %    lists:foldl(fun(X, Y) -> X * Y end, 1, L).
    %ceil(X) ->
    %    T = erlang:trunc(X),
    %    if
    %        X > T ->
    %            T + 1;
    %        true ->
    %            T
    %    end.
    %floor(X) ->
    %   T = erlang:trunc(X),
    %   if
    %       X < T ->
    %           T - 1;
    %       true ->
    %           T
    %   end.
    %SmoothingFactor = if
    %    Difference =< 1.0 ->
    %        product(lists:seq(
    %            floor(math:log(Difference) / math:log(3.0)) * -2 + 1,
    %        1, -2)) * 2 * erlang:float(TaskCount);
    %    true ->
    %        product(lists:seq(
    %            ceil(math:log(Difference) / math:log(50.0)) * 2 + 1,
    %        1, -2)) * 2 * erlang:float(TaskCount)
    %end,
    % smoothing method as separate sequences
    SmoothingFactor = TaskCount * (if
        Difference =< 0.0625 ->
            1890.0;
        Difference =< 0.125 ->
            210.0;
        Difference =< 0.25 ->
            30.0;
        Difference =< 0.5 ->
            6.0;
        Difference =< 1.0 ->
            2.0;
        Difference =< 50.0 ->                   % = 1.0 * 50
            6.0;                                % = 2.0 * 3
        Difference =< 2500.0 ->                 % = 50.0 * 50
            30.0;                               % = 6.0 * 5
        Difference =< 125000.0 ->               % = 2500.0 * 50
            210.0;                              % = 30.0 * 7
        Difference =< 6250000.0 ->              % = 125000.0 * 50
            1890.0;                             % = 210.0 * 9
        true ->
            20790.0                             % = 1890.0 * 11
    end),
    % perform truncated moving average
    Change = (NextTaskSize - OldTaskSize) / SmoothingFactor,
    OldTaskSize + Change.

target_time_incr(?TARGET_TIME_ADJUST, TargetTime, TargetTimeMax) ->
    NewTargetTime = erlang:min(TargetTime * ?TARGET_TIME_ADJUST_FACTOR,
                               TargetTimeMax),
    if
        NewTargetTime == TargetTimeMax ->
            ?LOG_ERROR("target time increase failed (~p, ~p)",
                       [TargetTime, TargetTimeMax]);
        true ->
            ?LOG_WARN("target time increased to ~p hours", [NewTargetTime])
    end,
    {0, NewTargetTime};
target_time_incr(TargetTimeIncr, TargetTime, _) ->
    {TargetTimeIncr, TargetTime}.

target_time_decr(?TARGET_TIME_ADJUST, TargetTime, TargetTimeMin) ->
    NewTargetTime = erlang:max(TargetTime / ?TARGET_TIME_ADJUST_FACTOR,
                               TargetTimeMin),
    if
        NewTargetTime == TargetTimeMin ->
            ?LOG_ERROR("target time decrease failed (~p, ~p)",
                       [TargetTime, TargetTimeMin]);
        true ->
            ?LOG_WARN("target time decreased to ~p hours", [NewTargetTime])
    end,
    {0, NewTargetTime};
target_time_decr(TargetTimeDecr, TargetTime, _) ->
    {TargetTimeDecr, TargetTime}.

