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
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2024 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2024 Michael Truog
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
         task_done/5]).

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

-record(cloudi_task_scheduler,
    {
        timeout_max :: pos_integer(),
        timeout_default :: pos_integer(),
        task_size :: undefined | tuple(),
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
        #cloudi_task_scheduler{task_size = TaskSize,
                               destinations = Destinations} = State)
    when element(1, TaskSize) =:= cloudi_task_size ->
    case cloudi_service:get_pids(Dispatcher, Name) of
        {ok, PatternPids} ->
            DestinationList = get_pid_update(Name, PatternPids, Destinations),
            case get_pid_store_task_size(DestinationList,
                                         Name, TaskId,
                                         TaskSize, Destinations) of
                {PatternPid, Timeout, TaskCost, DestinationsNew} ->
                    {ok, PatternPid, Timeout, TaskCost,
                     State#cloudi_task_scheduler{
                         destinations = DestinationsNew}};
                error ->
                    {error, all_dead}
            end;
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
                               timeouts = Timeouts,
                               destinations = Destinations} = State)
    when is_integer(TaskCost), TaskCost > 0 ->
    case cloudi_service:get_pids(Dispatcher, Name) of
        {ok, PatternPids} ->
            DestinationList = get_pid_update(Name, PatternPids, Destinations),
            case get_pid_store_task_cost(DestinationList,
                                         Name, TaskId, TaskCost,
                                         TimeoutDefault,
                                         Timeouts, Destinations) of
                {PatternPid, Timeout, TimeoutsNew, DestinationsNew} ->
                    {ok, PatternPid, Timeout,
                     State#cloudi_task_scheduler{
                         timeouts = TimeoutsNew,
                         destinations = DestinationsNew}};
                error ->
                    {error, all_dead}
            end;
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
    case task_remove(undefined, Name, PatternPidOld, TaskId, Destinations) of
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
                           task_size = undefined}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a new task scheduler instance with usage of cloudi_task_size.===
%% @end
%%-------------------------------------------------------------------------

-spec new(Dispatcher :: cloudi_service:dispatcher(),
          TaskSize :: tuple()) ->
    #cloudi_task_scheduler{}.

new(Dispatcher, TaskSize)
    when element(1, TaskSize) =:= cloudi_task_size ->
    true = undefined > 0.0, % for the sort_key
    TimeoutMax = cloudi_service:timeout_max(Dispatcher),
    TimeoutDefault = cloudi_service:timeout_async(Dispatcher),
    #cloudi_task_scheduler{timeout_max = TimeoutMax,
                           timeout_default = TimeoutDefault,
                           task_size = TaskSize}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Update the task scheduler after a task is completed.===
%% @end
%%-------------------------------------------------------------------------

-spec task_done(Name :: cloudi:service_name(),
                PatternPidOld :: cloudi:pattern_pid(),
                TaskId :: task_id(),
                TimeoutNew :: cloudi:timeout_value_milliseconds(),
                State :: #cloudi_task_scheduler{}) ->
    {ok, cloudi:timeout_value_milliseconds(), #cloudi_task_scheduler{}} |
    {error, any()}.

task_done([_ | _] = Name, {_, PidOld} = PatternPidOld, TaskId, TimeoutNew,
          #cloudi_task_scheduler{task_size = TaskSize,
                                 destinations = Destinations} = State)
    when is_integer(TimeoutNew), TimeoutNew >= 0 ->
    case task_remove(TimeoutNew, Name, PatternPidOld, TaskId, Destinations) of
        {TaskCost, _, TimeoutSource, Elapsed, DestinationsNew}
            when is_integer(Elapsed) ->
            TaskSizeNew = if
                TimeoutSource =:= timeouts ->
                    TaskSize;
                TimeoutSource =:= cloudi_task_size ->
                    cloudi_task_size:put(PidOld, TaskCost,
                                         erlang:max(Elapsed, 1) / 3600000,
                                         TaskSize)
            end,
            {ok, Elapsed,
             State#cloudi_task_scheduler{task_size = TaskSizeNew,
                                         destinations = DestinationsNew}};
        error ->
            {error, not_found}
    end.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

get_pid_update(Name, PatternPids, Destinations) ->
    PatternPidsLookup = get_pid_lookup(PatternPids),
    case cloudi_x_trie:find(Name, Destinations) of
        {ok, DestinationList} ->
            get_pid_update_destinations(DestinationList,
                                        PatternPidsLookup);
        error ->
            get_pid_update_destinations([], PatternPidsLookup)
    end.

get_pid_update_destinations([], DestinationList, PatternPidsLookup) ->
    maps:fold(fun(PatternPid, SubscribeCount, DestinationListNew) ->
         Destination = #destination{sort_key = undefined,
                                    pattern_pid = PatternPid,
                                    subscribe_count = SubscribeCount},
         #destination{task_cost_pending = TaskCostPending,
                      speed = Speed,
                      alive = Alive} = Destination,
         SortKey = sort_key(SubscribeCount, TaskCostPending, Speed, Alive),
         lists:keymerge(#destination.sort_key,
                        DestinationListNew,
                        [Destination#destination{sort_key = SortKey}])
    end, DestinationList, PatternPidsLookup);
get_pid_update_destinations([#destination{pattern_pid = PatternPid,
                             subscribe_count = SubscribeCountOld,
                             task_cost_pending = TaskCostPending,
                             speed = Speed,
                             requests = Requests} = Destination |
                             DestinationListOld],
                            DestinationList, PatternPidsLookup) ->
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
                                        PatternPidsLookup);
        error ->
            get_pid_update_destinations(DestinationListOld,
                                        DestinationList,
                                        PatternPidsLookup)
    end.

get_pid_update_destinations(DestinationList, PatternPidsLookup) ->
    get_pid_update_destinations(lists:reverse(DestinationList), [],
                                PatternPidsLookup).

get_pid_store_task_size([#destination{alive = false} | _], _, _, _, _) ->
    error;
get_pid_store_task_size([#destination{pattern_pid = {_, Pid}} = Destination |
                         DestinationList],
                        Name, TaskId,
                        TaskSize, Destinations) ->
    {TaskCost, Timeout} = cloudi_task_size:get(Pid, TaskSize),
    TimeoutSource = cloudi_task_size,
    {PatternPid,
     DestinationsNew} = get_pid_store(Destination, DestinationList,
                                      Name, TaskId,
                                      TaskCost, Timeout, TimeoutSource,
                                      Destinations),
    {PatternPid, Timeout, TaskCost, DestinationsNew}.

get_pid_store_task_cost([#destination{alive = false} | _], _, _, _, _, _, _) ->
    error;
get_pid_store_task_cost([#destination{pattern_pid = PatternPid} = Destination |
                         DestinationList],
                        Name, TaskId, TaskCost,
                        TimeoutDefault, Timeouts, Destinations) ->
    {Timeout,
     TimeoutsNew} = get_pid_store_timeout(PatternPid, TimeoutDefault, Timeouts),
    TimeoutSource = timeouts,
    {PatternPid,
     DestinationsNew} = get_pid_store(Destination, DestinationList,
                                      Name, TaskId,
                                      TaskCost, Timeout, TimeoutSource,
                                      Destinations),
    {PatternPid, Timeout, TimeoutsNew, DestinationsNew}.

get_pid_store(#destination{pattern_pid = PatternPid,
                           subscribe_count = SubscribeCount,
                           task_cost_pending = TaskCostPending,
                           speed = Speed,
                           alive = Alive,
                           requests = Requests} = Destination,
              DestinationList,
              Name, TaskId, TaskCost, Timeout, TimeoutSource, Destinations) ->
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
    DestinationsNew = cloudi_x_trie:store(Name, DestinationListNew,
                                          Destinations),
    {PatternPid, DestinationsNew}.

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
    PatternPidsLookup;
get_pid_lookup([PatternPid | PatternPids], PatternPidsLookup) ->
    PatternPidsLookupNew = maps:update_with(PatternPid, fun(SubscribeCount) ->
        SubscribeCount + 1
    end, 1, PatternPidsLookup),
    get_pid_lookup(PatternPids, PatternPidsLookupNew).

get_pid_lookup(PatternPids) ->
    get_pid_lookup(PatternPids, maps:new()).

task_remove(TimeoutNew, Name, PatternPidOld, TaskId, Destinations) ->
    case cloudi_x_trie:find(Name, Destinations) of
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
                             cloudi_x_trie:store(Name,
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

increase_timeout({_, PidOld}, TimeoutOld, TimeoutMax, Timeouts) ->
    PidNode = node(PidOld),
    case maps:find(PidNode, Timeouts) of
        {ok, Timeout} when Timeout > TimeoutOld ->
            Timeouts;
        _ ->
            TimeoutNew = erlang:min(TimeoutOld * 2, TimeoutMax),
            maps:put(PidNode, TimeoutNew, Timeouts)
    end.

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
    % speed is an average value, calculated with a moving average
    SpeedCountNew = SpeedCount + 1,
    SpeedNew = Speed + (SpeedSample - Speed) / SpeedCountNew,
    {Elapsed, SpeedNew, SpeedCountNew}.

