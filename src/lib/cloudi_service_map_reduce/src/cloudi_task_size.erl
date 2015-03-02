%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Task Size Calculation==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2009-2015, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2009-2015 Michael Truog
%%% @version 1.4.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_task_size).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([new/6,
         get/2,
         reduce/3,
         put/4]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-ifdef(ERLANG_OTP_VER_16).
-type dict_proxy(_Key, _Value) :: dict().
-else.
-type dict_proxy(Key, Value) :: dict:dict(Key, Value).
-endif.

-record(node,
    {
        task_size :: number()
    }).

-record(cloudi_task_size,
    {
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

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a new task size lookup.===
%% @end
%%-------------------------------------------------------------------------

-spec new(TaskSizeInitial :: integer(),
          TaskSizeMin :: integer(),
          TaskSizeMax :: integer(),
          TargetTimeInitial :: float(),
          TargetTimeMin :: float(),
          TargetTimeMax :: float()) ->
    state().

new(TaskSizeInitial, TaskSizeMin, TaskSizeMax,
    TargetTimeInitial, TargetTimeMin, TargetTimeMax)
    when is_integer(TaskSizeInitial),
         is_integer(TaskSizeMin), is_integer(TaskSizeMax),
         TaskSizeInitial >= TaskSizeMin, TaskSizeInitial =< TaskSizeMax,
         is_float(TargetTimeInitial), TargetTimeInitial > 0.0,
         is_float(TargetTimeMin), is_float(TargetTimeMax),
         TargetTimeInitial >= TargetTimeMin,
         TargetTimeInitial =< TargetTimeMax ->
    #cloudi_task_size{task_size_initial = TaskSizeInitial,
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
    {TaskSize :: integer(), TargetTime :: float()}.

get(Pid,
    #cloudi_task_size{task_size_initial = TaskSizeInitial,
                      target_time = TargetTime,
                      lookup = Lookup})
    when is_pid(Pid) ->
    case dict:find(node(Pid), Lookup) of
        {ok, #node{task_size = TaskSize}} ->
            TaskSizeInteger = if
                is_float(TaskSize) ->
                    cloudi_math:floor(TaskSize);
                is_integer(TaskSize) ->
                    TaskSize
            end,
            {TaskSizeInteger, TargetTime};
        error ->
            {TaskSizeInitial, TargetTime}
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
    #cloudi_task_size{task_size_initial = TaskSizeInitial,
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
                                          TargetTime0, ElapsedTime),
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

task_size_clamp(TaskSize, TaskSizeMin, TaskSizeMax) ->
    TaskSizeInteger = if
        is_float(TaskSize) ->
            cloudi_math:floor(TaskSize);
        is_integer(TaskSize) ->
            TaskSize
    end,
    if
        TaskSizeInteger < TaskSizeMin ->
            TaskSizeMin;
        TaskSizeInteger > TaskSizeMax ->
            TaskSizeMax;
        true ->
            TaskSize
    end.

task_size_smoothed(CurrentTaskSize, OldTaskSize, TargetTime, ElapsedTime)
    when is_integer(CurrentTaskSize), is_number(OldTaskSize),
         is_float(TargetTime), is_float(ElapsedTime) ->
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
    %        1, -2)) * 8 * erlang:float(Threads);
    %    true ->
    %        product(lists:seq(
    %            ceil(math:log(Difference) / math:log(50.0)) * 2 + 1,
    %        1, -2)) * 8 * erlang:float(Threads)
    %end,
    % smoothing method as separate sequences
    SmoothingFactor = if
        Difference =< 0.0625 ->
            7560.0;
        Difference =< 0.125 ->
            840.0;
        Difference =< 0.25 ->
            120.0;
        Difference =< 0.5 ->
            24.0;
        Difference =< 1.0 ->
            8.0;
        Difference =< 50.0 ->                   % = 1.0 * 50
            24.0;                               % = 8.0 * 3
        Difference =< 2500.0 ->                 % = 50.0 * 50
            120.0;                              % = 24.0 * 5
        Difference =< 125000.0 ->               % = 2500.0 * 50
            840.0;                              % = 120.0 * 7
        Difference =< 6250000.0 ->              % = 125000.0 * 50
            7560.0;                             % = 840.0 * 9
        true ->
            83160.0                             % = 7560.0 * 11
    end,
    % perform truncated moving average
    OldTaskSize +
    ((NextTaskSize / SmoothingFactor) -
     (OldTaskSize / SmoothingFactor)).

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

