%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Erlang Metrics==
%%% All memory values are in bytes (unless "words" is explicitly specified).
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2015-2018 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2015-2018 Michael Truog
%%% @version 1.7.5 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_monitoring_erlang).
-author('mjtruog at protonmail dot com').

%% external interface
-export([basic_update/0,
         memory_init/1,
         memory_update/1,
         system_info_init/1,
         system_info_update/1,
         statistics_init/1,
         statistics_update/1,
         process_info_init/1,
         process_info_update/1,
         port_info_init/1,
         port_info_update/1]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

basic_update() ->
    MetricValues0 = case erlang:whereis(error_logger) of
        undefined ->
            [];
        ErrorLoggerPid ->
            ErrorLoggerLength = element(2,
                erlang:process_info(ErrorLoggerPid, message_queue_len)),
            [metric(gauge, [error_logger, message_queue_len],
                    ErrorLoggerLength)]
    end,
    MetricValues1 = case erlang:whereis(logger_proxy) of
        undefined ->
            MetricValues0;
        LoggerProxyPid ->
            LoggerProxyLength = element(2,
                erlang:process_info(LoggerProxyPid, message_queue_len)),
            [metric(gauge, [logger_proxy, message_queue_len],
                    LoggerProxyLength) | MetricValues0]
    end,
    MetricValuesN = case erlang:whereis(logger) of
        undefined ->
            MetricValues1;
        LoggerPid ->
            LoggerLength = element(2,
                erlang:process_info(LoggerPid, message_queue_len)),
            [metric(gauge, [logger, message_queue_len],
                    LoggerLength) | MetricValues1]
    end,
    [metric(gauge, [code, modules],
            erlang:length(code:all_loaded())),
     metric(gauge, [ets, tables],
            erlang:length(ets:all())) | MetricValuesN].

memory_init(Options) ->
    Options.

memory_update([]) ->
    {[], []};
memory_update(Options) ->
    Memory = erlang:memory(),
    MetricValues = [metric(gauge, memory_value(Option, Memory))
                    || Option <- Options],
    {MetricValues, Options}.

system_info_init(Options) ->
    [system_info_value(Option) || Option <- Options],
    Options.

system_info_update(Options) ->
    MetricValues = [metric(gauge, system_info_value(Option))
                    || Option <- Options],
    {MetricValues, Options}.

statistics_init(Options) ->
    case lists:member(scheduler_wall_time, Options) of
        true ->
            _ = erlang:system_flag(scheduler_wall_time, true);
        false ->
            ok
    end,
    [statistics_value(Option) || Option <- Options].

statistics_update(EntriesOld) ->
    statistics_update(EntriesOld, [], []).

process_info_init(Options) ->
    Options.

process_info_update([]) ->
    {[], []};
process_info_update(Options) ->
    Processes = erlang:processes(),
    MetricValues = [metric(gauge, process_info_value(Option, Processes))
                    || Option <- Options],
    {MetricValues, Options}.

port_info_init(Options) ->
    Options.

port_info_update([]) ->
    {[], []};
port_info_update(Options) ->
    Ports = erlang:ports(),
    MetricValues = [metric(gauge, port_info_value(Option, Ports))
                    || Option <- Options],
    {MetricValues, Options}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

memory_value(Option, Memory) ->
    {_, Value} = lists:keyfind(Option, 1, Memory),
    {Option, Value}.

system_info_value(Option)
    when (Option =:= dirty_cpu_schedulers) orelse
         (Option =:= dirty_cpu_schedulers_online) orelse
         (Option =:= dirty_io_schedulers) ->
    Value = try erlang:system_info(Option)
    catch
        error:badarg ->
            0
    end,
    {Option, Value};
system_info_value(Option)
    when (Option =:= logical_processors) orelse
         (Option =:= logical_processors_available) orelse
         (Option =:= logical_processors_online) ->
    Value = case erlang:system_info(Option) of
        unknown ->
            0;
        Count when is_integer(Count) ->
            Count
    end,
    {Option, Value};
system_info_value(Option) ->
    Value = erlang:system_info(Option),
    true = is_integer(Value),
    {Option, Value}.

statistics_value(context_switches) ->
    {ContextSwitches, _} = erlang:statistics(context_switches),
    {context_switches, ContextSwitches};
statistics_value(garbage_collection) ->
    GC = {_GCs, _WordsReclaimed, _} = erlang:statistics(garbage_collection),
    {garbage_collection, GC};
statistics_value(io) ->
    {{input, TotalBytesIn},
     {output, TotalBytesOut}} = erlang:statistics(io),
    {io, {TotalBytesIn, TotalBytesOut}};
statistics_value(scheduler_wall_time) ->
    {scheduler_wall_time, lists:sort(erlang:statistics(scheduler_wall_time))};
statistics_value(reductions) ->
    {TotalReductions, _} = erlang:statistics(reductions),
    {reductions, TotalReductions};
statistics_value(Option) ->
    Value = erlang:statistics(Option),
    true = is_integer(Value),
    {Option, Value}.

statistics_value_update(context_switches, ContextSwitchesOld) ->
    EntryNew = {_, ContextSwitchesNew} = statistics_value(context_switches),
    {[metric(spiral, [context_switches],
             ContextSwitchesNew - ContextSwitchesOld)],
     EntryNew};
statistics_value_update(garbage_collection,
                        {GCsOld, WordsReclaimedOld, _}) ->
    EntryNew = {_, ValueNew} = statistics_value(garbage_collection),
    {GCsNew, WordsReclaimedNew, _} = ValueNew,
    {[metric(spiral, [garbage_collection, count],
             GCsNew - GCsOld),
      metric(spiral, [garbage_collection, words_reclaimed],
             WordsReclaimedNew - WordsReclaimedOld)],
     EntryNew};
statistics_value_update(io, {TotalBytesInOld, TotalBytesOutOld}) ->
    EntryNew = {_, ValueNew} = statistics_value(io),
    {TotalBytesInNew, TotalBytesOutNew} = ValueNew,
    {[metric(spiral, [io, input], TotalBytesInNew - TotalBytesInOld),
      metric(spiral, [io, output], TotalBytesOutNew - TotalBytesOutOld)],
     EntryNew};
statistics_value_update(scheduler_wall_time, SchedulersOld) ->
    EntryNew = {_, SchedulersNew} = statistics_value(scheduler_wall_time),
    MetricValues = lists:foldl(fun({SchedulerId,
                                    ActiveTimeOld, TotalTimeOld}, L) ->
        case lists:keyfind(SchedulerId, 1, SchedulersNew) of
            {_, ActiveTimeNew, TotalTimeNew} ->
                [metric(histogram,
                        [scheduler_wall_time,
                         erlang:integer_to_list(SchedulerId), active],
                        ActiveTimeNew - ActiveTimeOld),
                 metric(histogram,
                        [scheduler_wall_time,
                         erlang:integer_to_list(SchedulerId), total],
                        TotalTimeNew - TotalTimeOld) | L];
            false ->
                L
        end
    end, [], SchedulersOld),
    {MetricValues, EntryNew};
statistics_value_update(reductions, TotalReductionsOld) ->
    EntryNew = {_, TotalReductionsNew} = statistics_value(reductions),
    {[metric(spiral, [reductions],
             TotalReductionsNew - TotalReductionsOld)], EntryNew};
statistics_value_update(Option, _) ->
    EntryNew = statistics_value(Option),
    {[metric(gauge, EntryNew)], EntryNew}.

statistics_update([], EntriesNew, MetricValues) ->
    {MetricValues, EntriesNew};
statistics_update([{Option, ValueOld} | EntriesOld],
                  EntriesNew, MetricValues) ->
    {MetricValueL, EntryNew} = statistics_value_update(Option, ValueOld),
    statistics_update(EntriesOld,
                      [EntryNew | EntriesNew], MetricValueL ++ MetricValues).

process_info_value(Option, Processes)
    when Option =:= message_queue_len ->
    % if more options are added, provide them as
    % a list to erlang:process_info/2
    TotalCount = lists:foldl(fun(Pid, I) ->
        case erlang:process_info(Pid, Option) of
            {Option, Count} when is_integer(Count) ->
                I + Count;
            undefined ->
                I
        end
    end, 0, Processes),
    {Option, TotalCount}.

port_info_value(Option, Ports)
    when (Option =:= memory) orelse
         (Option =:= queue_size) ->
    TotalCount = lists:foldl(fun(Port, I) ->
        case erlang:port_info(Port, Option) of
            {Option, Count} when is_integer(Count) ->
                I + Count;
            undefined ->
                I
        end
    end, 0, Ports),
    {Option, TotalCount}.

metric(gauge, {Option, Value})
    when is_atom(Option) ->
    {gauge, [Option], Value}.

metric(Type, [_ | _] = Name, Value)
    when (Type =:= gauge) orelse
         (Type =:= spiral) orelse
         (Type =:= histogram) ->
    {Type, Name, Value}.

