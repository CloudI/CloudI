%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Task Scheduler==
%%% CloudI service requests are normally randomly distributed among the
%%% available Erlang processes associated with a service instance.
%%% The CloudI Task Scheduler tracks the TaskCost and the elapsed time
%%% that a service's Erlang process took to process the task to determine
%%% the Erlang process' speed.  The Task Scheduler uses the past performance
%%% of service Erlang processes to balance the TaskCost among the
%%% available Erlang processes.
%%%
%%% The cloudi_task_scheduler source code assumes the TaskCost has a
%%% linear relationship with the elapsed time.
%%%
%%% Using the min_max schedule algorithm instead of the
%%% greedy schedule algorithm is best due to more fully utilizing
%%% each node based on the node's speed.  The performance difference is
%%% subtle but the maximum latency will be minimized
%%% (providing better fault-tolerance) while the average latency is
%%% slightly higher with negative skewness.  The latency kurtosis is lower
%%% and the total TaskCost processed per millisecond is higher due to more
%%% fully utilizing each node.  The total runtime can be the same for both
%%% schedule algorithms when no outages occur but temporary outages can
%%% cause the min_max schedule algorithm to provide a smaller total runtime
%%% (due to better decisions becoming available with each outage).
%%% Smaller TaskCost values should be used in the beginning while
%%% Erlang process speeds are being determined, to avoid causing higher
%%% latency.
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2024-2025 Michael Truog <mjtruog at protonmail dot com>
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
%%% @author Michael Truog <mjtruog at protonmail dot com>
%%% @copyright 2024-2025 Michael Truog
%%% @version 2.0.8 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_task_scheduler).
-author('mjtruog at protonmail dot com').

%% external interface
-export([get_pid/4,
         get_pid/5,
         get_pid_retry/5,
         new/1,
         new/2,
         nodes_speed/1,
         task_done/4]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-type task_id() :: any().
-type task_cost() :: pos_integer().
-export_type([task_id/0,
              task_cost/0]).

-record(request,
    {
        task_cost :: task_cost(),
        timeout :: pos_integer(),
        timeout_source :: timeouts | cloudi_task_size
    }).

-record(destination,
    {
        sort_key :: float() | undefined,
        pattern_pid :: cloudi:pattern_pid(),
        subscribe_count :: pos_integer(),
        task_cost_pending = 0 :: non_neg_integer(),
        speed = 1.0 :: float(),
        speed_count = 0 :: non_neg_integer(),
        alive = true :: boolean(),
        requests = #{} :: #{task_id() := #request{}}
    }).

-type schedule() :: greedy | min_max.

-record(cloudi_task_scheduler,
    {
        timeout_max :: pos_integer(),
        timeout_default :: pos_integer(),
        task_count :: undefined | pos_integer(),
        task_size :: undefined | tuple(),
        schedule :: schedule(),
        task_cost_min = undefined :: task_cost() | undefined,
        task_cost_max = undefined :: task_cost() | undefined,
        timeouts = #{} :: #{node() := pos_integer()},
        destinations = cloudi_x_trie:new() :: cloudi_x_trie:cloudi_x_trie()
    }).

-type state() :: #cloudi_task_scheduler{}.
-export_type([state/0]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a CloudI service destination with the most capacity using a TaskCost from cloudi_task_size.===
%% @end
%%-------------------------------------------------------------------------

-spec get_pid(Dispatcher :: cloudi_service:dispatcher(),
              Name :: cloudi:service_name(),
              TaskId :: task_id(),
              State :: #cloudi_task_scheduler{}) ->
    {ok, cloudi:pattern_pid(), cloudi:timeout_value_milliseconds(),
     task_cost(), #cloudi_task_scheduler{}} |
    {error, any()}.

get_pid(Dispatcher, [_ | _] = Name, TaskId,
        #cloudi_task_scheduler{task_count = TaskCount,
                               task_size = TaskSize,
                               schedule = Schedule,
                               task_cost_min = TaskCostMin,
                               task_cost_max = TaskCostMax,
                               destinations = Destinations} = State)
    when element(1, TaskSize) =:= cloudi_task_size ->
    case cloudi_service:get_pids(Dispatcher, Name) of
        {ok, [{Pattern, _} | _] = PatternPids} ->
            {DestinationList,
             DestinationListSize,
             DestinationListAlive} = get_pid_update(Pattern, PatternPids,
                                                    Destinations),
            {Destination,
             DestinationListNew} = get_pid_schedule(Schedule,
                                                    DestinationList,
                                                    DestinationListSize,
                                                    DestinationListAlive,
                                                    undefined,
                                                    TaskCostMin,
                                                    TaskCostMax,
                                                    TaskCount),
            {PatternPid,
             Timeout,
             TaskCost,
             DestinationsNew} = get_pid_store_task_size(Destination,
                                                        DestinationListNew,
                                                        TaskId, TaskSize,
                                                        Destinations),
            {ok, PatternPid, Timeout, TaskCost,
             State#cloudi_task_scheduler{
                 destinations = DestinationsNew}};
        {error, _} = Error ->
            Error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a CloudI service destination with the most capacity.===
%% @end
%%-------------------------------------------------------------------------

-spec get_pid(Dispatcher :: cloudi_service:dispatcher(),
              Name :: cloudi:service_name(),
              TaskId :: task_id(),
              TaskCost :: task_cost(),
              State :: #cloudi_task_scheduler{}) ->
    {ok, cloudi:pattern_pid(), cloudi:timeout_value_milliseconds(),
     #cloudi_task_scheduler{}} |
    {error, any()}.

get_pid(Dispatcher, [_ | _] = Name, TaskId, TaskCost,
        #cloudi_task_scheduler{timeout_default = TimeoutDefault,
                               task_count = TaskCount,
                               schedule = Schedule,
                               task_cost_min = TaskCostMin,
                               task_cost_max = TaskCostMax,
                               timeouts = Timeouts,
                               destinations = Destinations} = State)
    when is_integer(TaskCost), TaskCost > 0 ->
    case cloudi_service:get_pids(Dispatcher, Name) of
        {ok, [{Pattern, _} | _] = PatternPids} ->
            {DestinationList,
             DestinationListSize,
             DestinationListAlive} = get_pid_update(Pattern, PatternPids,
                                                    Destinations),
            {Destination,
             DestinationListNew} = get_pid_schedule(Schedule,
                                                    DestinationList,
                                                    DestinationListSize,
                                                    DestinationListAlive,
                                                    TaskCost,
                                                    TaskCostMin,
                                                    TaskCostMax,
                                                    TaskCount),
            {PatternPid,
             Timeout,
             TimeoutsNew,
             DestinationsNew} = get_pid_store_task_cost(Destination,
                                                        DestinationListNew,
                                                        TaskId, TaskCost,
                                                        TimeoutDefault,
                                                        Timeouts, Destinations),
            {ok, PatternPid, Timeout,
             State#cloudi_task_scheduler{
                 timeouts = TimeoutsNew,
                 destinations = DestinationsNew}};
        {error, _} = Error ->
            Error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a new CloudI service destination with the most capacity.===
%% @end
%%-------------------------------------------------------------------------

-spec get_pid_retry(Dispatcher :: cloudi_service:dispatcher(),
                    Name :: cloudi:service_name(),
                    PatternPidOld :: cloudi:pattern_pid(),
                    TaskId :: task_id(),
                    State :: #cloudi_task_scheduler{}) ->
    {ok, cloudi:pattern_pid(), cloudi:timeout_value_milliseconds(),
     #cloudi_task_scheduler{}} |
    {error, any()}.

get_pid_retry(Dispatcher, [_ | _] = Name, {_, PidOld} = PatternPidOld, TaskId,
              #cloudi_task_scheduler{timeout_max = TimeoutMax,
                                     task_size = TaskSize,
                                     timeouts = Timeouts,
                                     destinations = Destinations} = State) ->
    case task_remove(undefined, PatternPidOld, TaskId, Destinations) of
        {TaskCost, TimeoutOld, TimeoutSource, undefined, DestinationsNew} ->
            TaskSizeNew = if
                TimeoutSource =:= timeouts ->
                    TaskSize;
                TimeoutSource =:= cloudi_task_size ->
                    cloudi_task_size:reduce(PidOld, TaskSize)
            end,
            TimeoutsNew = increase_timeout(PatternPidOld,
                                           TimeoutOld,
                                           TimeoutMax,
                                           Timeouts),
            get_pid(Dispatcher, Name, TaskId, TaskCost,
                    State#cloudi_task_scheduler{
                        task_size = TaskSizeNew,
                        timeouts = TimeoutsNew,
                        destinations = DestinationsNew});
        error ->
            {error, not_found}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a new task scheduler instance.===
%% @end
%%-------------------------------------------------------------------------

-spec new(Dispatcher :: cloudi_service:dispatcher()) ->
    #cloudi_task_scheduler{}.

new(Dispatcher) ->
    true = undefined > 0.0, % for the sort_key
    TimeoutMax = cloudi_service:timeout_max(Dispatcher),
    TimeoutDefault = cloudi_service:timeout_async(Dispatcher),
    #cloudi_task_scheduler{timeout_max = TimeoutMax,
                           timeout_default = TimeoutDefault,
                           task_count = undefined,
                           task_size = undefined,
                           schedule = greedy}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a new task scheduler instance with usage of cloudi_task_size.===
%% @end
%%-------------------------------------------------------------------------

-spec new(Dispatcher :: cloudi_service:dispatcher(),
          tuple() | pos_integer()) ->
    #cloudi_task_scheduler{}.

new(Dispatcher, TaskSize)
    when element(1, TaskSize) =:= cloudi_task_size ->
    true = undefined > 0.0, % for the sort_key
    TimeoutMax = cloudi_service:timeout_max(Dispatcher),
    TimeoutDefault = cloudi_service:timeout_async(Dispatcher),
    TaskCount = cloudi_task_size:task_count(TaskSize),
    #cloudi_task_scheduler{timeout_max = TimeoutMax,
                           timeout_default = TimeoutDefault,
                           task_count = TaskCount,
                           task_size = TaskSize,
                           schedule = min_max};
new(Dispatcher, Options)
    when is_list(Options) ->
    true = undefined > 0.0, % for the sort_key
    Defaults = [
        {task_count,                   undefined},
        {task_size,                    undefined},
        {schedule,                     min_max}],
    [TaskCount0, TaskSize0,
     Schedule] = cloudi_proplists:take_values(Defaults, Options),
    TaskSizeN = if
        is_tuple(TaskSize0) ->
            true = element(1, TaskSize0) =:= cloudi_task_size,
            TaskSize0;
        TaskSize0 =:= undefined ->
            undefined
    end,
    TaskCountN = if
        is_integer(TaskCount0) andalso TaskCount0 > 0 ->
            TaskCount0;
        is_tuple(TaskSizeN) ->
            cloudi_task_size:task_count(TaskSizeN);
        TaskCount0 =:= undefined ->
            undefined
    end,
    if
        Schedule =:= greedy ->
            ok;
        Schedule =:= min_max ->
            true = is_integer(TaskCountN),
            ok
    end,
    TimeoutMax = cloudi_service:timeout_max(Dispatcher),
    TimeoutDefault = cloudi_service:timeout_async(Dispatcher),
    #cloudi_task_scheduler{timeout_max = TimeoutMax,
                           timeout_default = TimeoutDefault,
                           task_count = TaskCountN,
                           task_size = TaskSizeN,
                           schedule = Schedule}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the total speed per node.===
%% The total speed is the total TaskCost processed per millisecond
%% (based on each service process' average speed).
%% @end
%%-------------------------------------------------------------------------

-spec nodes_speed(#cloudi_task_scheduler{}) ->
    #{node() := float()}.

nodes_speed(#cloudi_task_scheduler{destinations = Destinations}) ->
    PidSpeedN = cloudi_x_trie:fold(fun(_Pattern, DestinationList, PidSpeed0) ->
        lists:foldl(fun(Destination, PidSpeed1) ->
            % if pids appear multiple times (due to separate patterns)
            % the separate speeds are merged below
            #destination{pattern_pid = {_, Pid},
                         speed = SpeedA,
                         speed_count = SpeedCountA} = Destination,
            maps:update_with(Pid, fun({SpeedB, SpeedCountB} = ValueB) ->
                if
                    SpeedCountA == 0 ->
                        ValueB;
                    SpeedCountB == 0 ->
                        {SpeedA, SpeedCountA};
                    true ->
                        SpeedCount = SpeedCountA + SpeedCountB,
                        SpeedDelta = (SpeedA - SpeedB) / SpeedCount,
                        Speed = SpeedB + SpeedCountA * SpeedDelta,
                        {Speed, SpeedCount}
                end
            end, {SpeedA, SpeedCountA}, PidSpeed1)
        end, PidSpeed0, DestinationList)
    end, #{}, Destinations),
    maps:fold(fun(Pid, {Speed, _}, Nodes) ->
        maps:update_with(node(Pid), fun(SpeedSum) ->
            SpeedSum + Speed
        end, Speed, Nodes)
    end, #{}, PidSpeedN).

%%-------------------------------------------------------------------------
%% @doc
%% ===Update the task scheduler after a task is completed.===
%% @end
%%-------------------------------------------------------------------------

-spec task_done(PatternPidOld :: cloudi:pattern_pid(),
                TaskId :: task_id(),
                TimeoutNew :: cloudi:timeout_value_milliseconds(),
                State :: #cloudi_task_scheduler{}) ->
    {ok, cloudi:timeout_value_milliseconds(), #cloudi_task_scheduler{}} |
    {error, any()}.

task_done({_, PidOld} = PatternPidOld, TaskId, TimeoutNew,
          #cloudi_task_scheduler{task_size = TaskSize,
                                 task_cost_min = TaskCostMin,
                                 task_cost_max = TaskCostMax,
                                 destinations = Destinations} = State)
    when is_integer(TimeoutNew), TimeoutNew >= 0 ->
    case task_remove(TimeoutNew, PatternPidOld, TaskId, Destinations) of
        {TaskCost, _, TimeoutSource, Elapsed, DestinationsNew}
            when is_integer(Elapsed) ->
            TaskSizeNew = if
                TimeoutSource =:= timeouts ->
                    TaskSize;
                TimeoutSource =:= cloudi_task_size ->
                    cloudi_task_size:put(PidOld, TaskCost,
                                         max(Elapsed, 1) / 3600000,
                                         TaskSize)
            end,
            {TaskCostMinNew,
             TaskCostMaxNew} = task_done_update(TaskCost,
                                                TaskCostMin,
                                                TaskCostMax),
            {ok, Elapsed,
             State#cloudi_task_scheduler{task_size = TaskSizeNew,
                                         task_cost_min = TaskCostMinNew,
                                         task_cost_max = TaskCostMaxNew,
                                         destinations = DestinationsNew}};
        error ->
            {error, not_found}
    end.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

get_pid_update(Pattern, PatternPids, Destinations) ->
    PatternPidsLookup = get_pid_lookup(PatternPids),
    case cloudi_x_trie:find(Pattern, Destinations) of
        {ok, DestinationList} ->
            get_pid_update_destinations(DestinationList,
                                        PatternPidsLookup);
        error ->
            get_pid_update_destinations([], PatternPidsLookup)
    end.

get_pid_update_destinations([], DestinationList,
                            DestinationListSize, DestinationListAlive,
                            PatternPidsLookup) ->
    PatternPidsLookupSize = map_size(PatternPidsLookup),
    DestinationListSizeNew = DestinationListSize + PatternPidsLookupSize,
    DestinationListAliveNew = DestinationListAlive + PatternPidsLookupSize,
    DestinationListNew = maps:fold(fun(PatternPid, SubscribeCount,
                                       DestinationListNext) ->
         Destination = #destination{sort_key = undefined,
                                    pattern_pid = PatternPid,
                                    subscribe_count = SubscribeCount},
         #destination{task_cost_pending = TaskCostPending,
                      speed = Speed,
                      alive = Alive} = Destination,
         true = Alive,
         SortKey = sort_key(SubscribeCount, TaskCostPending, Speed, Alive),
         lists:keymerge(#destination.sort_key,
                        DestinationListNext,
                        [Destination#destination{sort_key = SortKey}])
    end, DestinationList, PatternPidsLookup),
    {DestinationListNew, DestinationListSizeNew, DestinationListAliveNew};
get_pid_update_destinations([#destination{pattern_pid = PatternPid,
                                          subscribe_count = SubscribeCountOld,
                                          task_cost_pending = TaskCostPending,
                                          speed = Speed,
                                          requests = Requests} = Destination |
                             DestinationListOld],
                            DestinationList,
                            DestinationListSize, DestinationListAlive,
                            PatternPidsLookup) ->
    case maps:take(PatternPid, PatternPidsLookup) of
        {SubscribeCount, PatternPidsLookupNew} ->
            Alive = true,
            SortKeyNew = sort_key(SubscribeCount,
                                  TaskCostPending, Speed, Alive),
            DestinationNew = Destination#destination{
                                 sort_key = SortKeyNew,
                                 subscribe_count = SubscribeCount,
                                 alive = Alive},
            DestinationListNew = lists:keymerge(#destination.sort_key,
                                                DestinationList,
                                                [DestinationNew]),
            get_pid_update_destinations(DestinationListOld,
                                        DestinationListNew,
                                        DestinationListSize + 1,
                                        DestinationListAlive + 1,
                                        PatternPidsLookupNew);
        error when map_size(Requests) > 0 ->
            Alive = false,
            SortKeyNew = sort_key(SubscribeCountOld,
                                  TaskCostPending, Speed, Alive),
            DestinationNew = Destination#destination{
                                 sort_key = SortKeyNew,
                                 alive = Alive},
            DestinationListNew = lists:keymerge(#destination.sort_key,
                                                DestinationList,
                                                [DestinationNew]),
            get_pid_update_destinations(DestinationListOld,
                                        DestinationListNew,
                                        DestinationListSize + 1,
                                        DestinationListAlive,
                                        PatternPidsLookup);
        error ->
            get_pid_update_destinations(DestinationListOld,
                                        DestinationList,
                                        DestinationListSize,
                                        DestinationListAlive,
                                        PatternPidsLookup)
    end.

get_pid_update_destinations(DestinationList, PatternPidsLookup) ->
    get_pid_update_destinations(lists:reverse(DestinationList), [], 0, 0,
                                PatternPidsLookup).

get_pid_schedule(greedy,
                 [Destination | DestinationList],
                 _, _, _, _, _, _) ->
    {Destination, DestinationList};
get_pid_schedule(min_max,
                 [#destination{alive = true,
                               requests = Requests} = Destination |
                  DestinationList],
                 DestinationListSize, DestinationListAlive,
                 TaskCost, TaskCostMin, TaskCostMax, TaskCount) ->
    TaskCostFraction = task_cost_fraction(TaskCost, TaskCostMin, TaskCostMax),
    if
        TaskCostFraction =:= 1.0;
        DestinationListAlive == 1 ->
            {Destination, DestinationList};
        true ->
            % TaskCount is the concurrency as an integer
            TasksPerDestination = TaskCount div DestinationListSize,
            IndexPick = round(DestinationListAlive * (1.0 - TaskCostFraction)),
            Index = 0,
            RequestsSize = map_size(Requests),
            {IndexOffset,
             TasksLimit} = if
                RequestsSize < TasksPerDestination ->
                    {Index - IndexPick,
                     RequestsSize + 1};
                true ->
                    {undefined,
                     TasksPerDestination}
            end,
            get_pid_schedule_min_max(DestinationList, [],
                                     Destination, IndexOffset, Index,
                                     TasksLimit, IndexPick)

    end.

get_pid_schedule_min_max([], DestinationListPrevious,
                         DestinationPrevious, _, _, _, _) ->
    {DestinationPrevious,
     DestinationListPrevious};
get_pid_schedule_min_max([#destination{alive = Alive,
                                       requests = Requests} = Destination |
                          DestinationListNew] = DestinationList,
                         DestinationListPrevious,
                         DestinationPrevious, IndexOffsetPrevious, Index,
                         TasksLimit, IndexPick) ->
    IndexNew = Index + 1,
    IndexOffset = IndexNew - IndexPick,
    if
        Alive =:= false;
        abs(IndexOffset) > abs(IndexOffsetPrevious) ->
            {DestinationPrevious,
             lists:keymerge(#destination.sort_key,
                            DestinationList, DestinationListPrevious)};
        Alive =:= true ->
            RequestsSize = map_size(Requests),
            {DestinationPreviousNew,
             IndexOffsetPreviousNew,
             TasksLimitNew,
             DestinationListPreviousNew} = if
                RequestsSize < TasksLimit ->
                    % Pick this destination because the TaskCost,
                    % in its historical min/max range, is associated
                    % with this destination's current expected time
                    % processing tasks.  That means the largest TaskCost
                    % values are allocated to the destinations that have the
                    % least work to do based on their current tasks and speed.
                    {Destination,
                     IndexOffset,
                     RequestsSize + 1,
                     lists:keymerge(#destination.sort_key,
                                    DestinationListPrevious,
                                    [DestinationPrevious])};
                true ->
                    {DestinationPrevious,
                     IndexOffsetPrevious,
                     TasksLimit,
                     lists:keymerge(#destination.sort_key,
                                    DestinationListPrevious,
                                    [Destination])}
            end,
            get_pid_schedule_min_max(DestinationListNew,
                                     DestinationListPreviousNew,
                                     DestinationPreviousNew,
                                     IndexOffsetPreviousNew, IndexNew,
                                     TasksLimitNew, IndexPick)
    end.

get_pid_store_task_size(#destination{pattern_pid = {_, Pid} = PatternPid,
                                     alive = true} = Destination,
                        DestinationList,
                        TaskId, TaskSize,
                        Destinations) ->
    {TaskCost, Timeout} = cloudi_task_size:get(Pid, TaskSize),
    TimeoutSource = cloudi_task_size,
    DestinationsNew = get_pid_store(Destination, DestinationList,
                                    TaskId, TaskCost,
                                    Timeout, TimeoutSource, Destinations),
    {PatternPid, Timeout, TaskCost, DestinationsNew}.

get_pid_store_task_cost(#destination{pattern_pid = PatternPid,
                                     alive = true} = Destination,
                        DestinationList,
                        TaskId, TaskCost,
                        TimeoutDefault, Timeouts, Destinations) ->
    {Timeout,
     TimeoutsNew} = get_pid_store_timeout(PatternPid, TimeoutDefault, Timeouts),
    TimeoutSource = timeouts,
    DestinationsNew = get_pid_store(Destination, DestinationList,
                                    TaskId, TaskCost,
                                    Timeout, TimeoutSource, Destinations),
    {PatternPid, Timeout, TimeoutsNew, DestinationsNew}.

get_pid_store(#destination{pattern_pid = {Pattern, _},
                           subscribe_count = SubscribeCount,
                           task_cost_pending = TaskCostPending,
                           speed = Speed,
                           alive = Alive,
                           requests = Requests} = Destination,
              DestinationList,
              TaskId, TaskCost,
              Timeout, TimeoutSource, Destinations) ->
    Request = #request{task_cost = TaskCost,
                       timeout = Timeout,
                       timeout_source = TimeoutSource},
    RequestsNew = maps:put(TaskId, Request, Requests),
    TaskCostPendingNew = TaskCostPending + TaskCost,
    SortKeyNew = sort_key(SubscribeCount, TaskCostPendingNew, Speed, Alive),
    DestinationNew = Destination#destination{
                         sort_key = SortKeyNew,
                         task_cost_pending = TaskCostPendingNew,
                         requests = RequestsNew},
    DestinationListNew = lists:keymerge(#destination.sort_key,
                                        DestinationList, [DestinationNew]),
    cloudi_x_trie:store(Pattern, DestinationListNew, Destinations).

get_pid_store_timeout({_, Pid}, TimeoutDefault, Timeouts) ->
    PidNode = node(Pid),
    case maps:find(PidNode, Timeouts) of
        {ok, Timeout} ->
            {Timeout, Timeouts};
        error ->
            TimeoutsNew = maps:put(PidNode, TimeoutDefault, Timeouts),
            {TimeoutDefault, TimeoutsNew}
    end.

get_pid_lookup([], PatternPidsLookup) ->
    true = map_size(PatternPidsLookup) > 0,
    PatternPidsLookup;
get_pid_lookup([PatternPid | PatternPids], PatternPidsLookup) ->
    PatternPidsLookupNew = maps:update_with(PatternPid, fun(SubscribeCount) ->
        SubscribeCount + 1
    end, 1, PatternPidsLookup),
    get_pid_lookup(PatternPids, PatternPidsLookupNew).

get_pid_lookup([_ | _] = PatternPids) ->
    get_pid_lookup(PatternPids, maps:new()).

task_remove(TimeoutNew, {Pattern, _} =  PatternPidOld, TaskId, Destinations) ->
    case cloudi_x_trie:find(Pattern, Destinations) of
        {ok, DestinationList} ->
            case lists:keytake(PatternPidOld, #destination.pattern_pid,
                               DestinationList) of
                {value,
                 #destination{subscribe_count = SubscribeCount,
                              task_cost_pending = TaskCostPending,
                              speed = Speed,
                              speed_count = SpeedCount,
                              alive = Alive,
                              requests = Requests} = Destination,
                 DestinationListNew} ->
                    case maps:take(TaskId, Requests) of
                        {#request{task_cost = TaskCost,
                                  timeout = TimeoutOld,
                                  timeout_source = TimeoutSource},
                         RequestsNew} ->
                            TaskCostPendingNew = TaskCostPending - TaskCost,
                            {Elapsed, SpeedNew, SpeedCountNew} = if
                                TimeoutNew =:= undefined ->
                                    {undefined, Speed, SpeedCount};
                                is_integer(TimeoutNew) ->
                                    speed(TaskCost, TimeoutOld, TimeoutNew,
                                          Speed, SpeedCount)
                            end,
                            SortKeyNew = sort_key(SubscribeCount,
                                                  TaskCostPendingNew,
                                                  SpeedNew, Alive),
                            DestinationNew = Destination#destination{
                                                 sort_key = SortKeyNew,
                                                 task_cost_pending =
                                                     TaskCostPendingNew,
                                                 speed = SpeedNew,
                                                 speed_count = SpeedCountNew,
                                                 requests = RequestsNew},
                            {TaskCost, TimeoutOld, TimeoutSource, Elapsed,
                             cloudi_x_trie:store(Pattern,
                                 lists:keymerge(#destination.sort_key,
                                                DestinationListNew,
                                                [DestinationNew]),
                                 Destinations)};
                        error ->
                            error
                    end;
                false ->
                    error
            end;
        error ->
            error
    end.

task_done_update(TaskCost, TaskCostMin, TaskCostMax) ->
    TaskCostMinNew = if
        TaskCostMin =:= undefined ->
            TaskCost;
        is_integer(TaskCostMin) ->
            min(TaskCost, TaskCostMin)
    end,
    TaskCostMaxNew = if
        TaskCostMax =:= undefined ->
            TaskCost;
        is_integer(TaskCostMax) ->
            max(TaskCost, TaskCostMax)
    end,
    {TaskCostMinNew, TaskCostMaxNew}.

increase_timeout({_, PidOld}, TimeoutOld, TimeoutMax, Timeouts) ->
    PidNode = node(PidOld),
    case maps:find(PidNode, Timeouts) of
        {ok, Timeout} when Timeout > TimeoutOld ->
            Timeouts;
        _ ->
            TimeoutNew = min(TimeoutOld * 2, TimeoutMax),
            maps:put(PidNode, TimeoutNew, Timeouts)
    end.

task_cost_fraction(undefined, _, _) ->
    1.0;
task_cost_fraction(_, undefined, _) ->
    1.0;
task_cost_fraction(_, _, undefined) ->
    1.0;
task_cost_fraction(TaskCost, TaskCostMin, _)
    when TaskCost =< TaskCostMin ->
    0.0;
task_cost_fraction(TaskCost, _, TaskCostMax)
    when TaskCost >= TaskCostMax ->
    1.0;
task_cost_fraction(TaskCost, TaskCostMin, TaskCostMax) ->
    (TaskCost - TaskCostMin) / (TaskCostMax - TaskCostMin).

sort_key(SubscribeCount, TaskCostPending, Speed, Alive) ->
    if
        Alive =:= true ->
            % estimated computing time (in milliseconds) allocated per pid
            (TaskCostPending / Speed) / SubscribeCount;
        Alive =:= false ->
            undefined
    end.

speed(TaskCost, TimeoutOld, TimeoutNew, Speed, SpeedCount) ->
    Elapsed = TimeoutOld - TimeoutNew,
    SpeedSample = if
        Elapsed == 0 ->
            TaskCost;
        Elapsed > 0 ->
            TaskCost / Elapsed
    end,
    if
        SpeedCount == 0 ->
            {Elapsed, SpeedSample, 1};
        SpeedCount > 0 ->
            % speed is an average value
            SpeedCountNew = SpeedCount + 1,
            SpeedNew = Speed + (SpeedSample - Speed) / SpeedCountNew,
            {Elapsed, SpeedNew, SpeedCountNew}
    end.
