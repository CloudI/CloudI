%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Service Concurrency==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2023 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2023 Michael Truog
%%% @version 2.0.6 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_core_i_concurrency).
-author('mjtruog at protonmail dot com').

%% external interface
-export([bind_format/1,
         bind_validate/4,
         bind_assign_process/3,
         bind_assign_process/4,
         bind_increment_thread/1,
         bind_init/1,
         bind_logical_processor/1,
         count/2,
         new/0]).

-type logical_processor() :: non_neg_integer().
-type scheduler_id() :: pos_integer().

-record(concurrency,
    {
        schedulers
            :: pos_integer(),
        scheduler_bindings
            :: tuple() | undefined,
        scheduler_id_lookup
            :: #{logical_processor() := scheduler_id()} | undefined,
        scheduler_id_bind = 1
            :: scheduler_id()
    }).

-record(bind,
    {
        string_exact = undefined
            :: nonempty_string() | undefined,
        thread_count = undefined
            :: pos_integer() | undefined,
        thread_index = 0
            :: non_neg_integer(),
        scheduler_ids_exact = undefined
            :: tuple() | undefined,
        logical_processors_exact = undefined
            :: tuple() | undefined,
        scheduler_ids = undefined
            :: tuple() | undefined,
        logical_processors = undefined
            :: tuple() | undefined
    }).

-type state() :: #concurrency{}.
-export_type([state/0]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Convert internal state to the bind configuration format.===
%% @end
%%-------------------------------------------------------------------------

-spec bind_format(#bind{} | false) ->
    boolean() | nonempty_string().

bind_format(false) ->
    false;
bind_format(#bind{string_exact = undefined}) ->
    true;
bind_format(#bind{string_exact = Input}) ->
    Input.

%%-------------------------------------------------------------------------
%% @doc
%% ===Convert the bind configuration format to internal state.===
%% @end
%%-------------------------------------------------------------------------

-spec bind_validate(Bind :: boolean(),
                    CountProcess :: pos_integer() | float(),
                    CountThread :: pos_integer() | float(),
                    Concurrency :: #concurrency{} | undefined) ->
    {ok, #bind{} | false} |
    {error, {service_options_bind_invalid, any()}}.

bind_validate(false, _, _, _) ->
    {ok, false};
bind_validate(_, _, _, #concurrency{scheduler_bindings = undefined}) ->
    {error, {service_options_bind_invalid, unbound}};
bind_validate(true, _, _, _) ->
    {ok, #bind{}};
bind_validate(String, CountProcess, CountThread,
              #concurrency{
                  scheduler_id_lookup = SchedulerIdLookup} = Concurrency)
    when is_list(String) ->
    ProcessCount = count(CountProcess, Concurrency),
    ThreadCount = count(CountThread, Concurrency),
    bind_string(String, ProcessCount, ThreadCount, SchedulerIdLookup).

%%-------------------------------------------------------------------------
%% @doc
%% ===Assign bind internal service process data.===
%% @end
%%-------------------------------------------------------------------------

-spec bind_assign_process(Bind :: #bind{} | false,
                          ProcessIndex :: non_neg_integer(),
                          Concurrency :: #concurrency{}) ->
    {#bind{} | false, #concurrency{}}.

bind_assign_process(Bind, ProcessIndex, Concurrency) ->
    bind_assign_process(Bind, ProcessIndex, 1, Concurrency).

%%-------------------------------------------------------------------------
%% @doc
%% ===Assign bind external service process data.===
%% @end
%%-------------------------------------------------------------------------

-spec bind_assign_process(Bind :: #bind{} | false,
                          ProcessIndex :: non_neg_integer(),
                          ThreadCount :: pos_integer(),
                          Concurrency :: #concurrency{}) ->
    {#bind{} | false, #concurrency{}}.

bind_assign_process(false, _, _, Concurrency) ->
    {false, Concurrency};
bind_assign_process(#bind{thread_count = undefined,
                          scheduler_ids_exact = undefined,
                          logical_processors_exact = undefined,
                          scheduler_ids = undefined,
                          logical_processors = undefined} = Bind,
                    _, ThreadCount, Concurrency) ->
    bind_assign_process(0, [], [],
                        Bind#bind{thread_count = ThreadCount}, Concurrency);
bind_assign_process(#bind{thread_count = undefined,
                          scheduler_ids_exact = SchedulerIdsExact,
                          logical_processors_exact = LogicalProcessorsExact,
                          scheduler_ids = undefined,
                          logical_processors = undefined} = Bind,
                    ProcessIndex, ThreadCount, Concurrency) ->
    I = ProcessIndex + 1,
    {Bind#bind{thread_count = ThreadCount,
               scheduler_ids = element(I, SchedulerIdsExact),
               logical_processors = element(I, LogicalProcessorsExact)},
     Concurrency}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Increment the bind thread_index.===
%% @end
%%-------------------------------------------------------------------------

-spec bind_increment_thread(Bind :: #bind{} | false) ->
    #bind{} | false.

bind_increment_thread(#bind{thread_count = ThreadCount,
                            thread_index = ThreadIndex} = Bind) ->
    true = ThreadIndex < ThreadCount,
    Bind#bind{thread_index = ThreadIndex + 1};
bind_increment_thread(false) ->
    false.

%%-------------------------------------------------------------------------
%% @doc
%% ===Bind the CloudI service dispatcher process.===
%% The other CloudI service Erlang processes automatically get the
%% same bind when spawn occurs from the dispatcher process.
%% @end
%%-------------------------------------------------------------------------

-spec bind_init(#bind{} | false) ->
    ok.

bind_init(#bind{thread_count = ThreadCount,
                thread_index = ThreadIndex,
                scheduler_ids = SchedulerIds}) ->
    true = ThreadIndex < ThreadCount,
    SchedulerId = element(ThreadIndex + 1, SchedulerIds),
    bind_erlang_process(SchedulerId);
bind_init(false) ->
    ok.

%%-------------------------------------------------------------------------
%% @doc
%% ===Logical processor for binding external service threads.===
%% @end
%%-------------------------------------------------------------------------

-spec bind_logical_processor(#bind{} | false) ->
    non_neg_integer() | -1.

bind_logical_processor(#bind{thread_count = ThreadCount,
                             thread_index = ThreadIndex,
                             logical_processors = LogicalProcessors}) ->
    true = ThreadIndex < ThreadCount,
    LogicalProcessor = element(ThreadIndex + 1, LogicalProcessors),
    LogicalProcessor;
bind_logical_processor(false) ->
    -1.

%%-------------------------------------------------------------------------
%% @doc
%% ===Convert count_process/count_thread to the integer process_count/thread_count values.===
%% @end
%%-------------------------------------------------------------------------

-spec count(I :: pos_integer() | float(),
            #concurrency{}) ->
    pos_integer().

count(I, #concurrency{schedulers = Schedulers}) ->
    cloudi_concurrency:count(I, Schedulers).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create concurrency information.===
%% @end
%%-------------------------------------------------------------------------

-spec new() ->
    #concurrency{}.

new() ->
    Schedulers = erlang:system_info(schedulers),
    SchedulerBindings = case erlang:system_info(scheduler_bind_type) of
        unbound ->
            undefined;
        _ ->
            erlang:system_info(scheduler_bindings)
    end,
    SchedulerIdLookup = if
        SchedulerBindings =:= undefined ->
            undefined;
        is_tuple(SchedulerBindings) ->
            scheduler_id_lookup(SchedulerBindings, Schedulers)
    end,
    #concurrency{schedulers = Schedulers,
                 scheduler_bindings = SchedulerBindings,
                 scheduler_id_lookup = SchedulerIdLookup}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

bind_assign_process(ThreadCount, SchedulerIdsL, LogicalProcessorsL,
                    #bind{thread_count = ThreadCount} = Bind,
                    Concurrency) ->
    SchedulerIds = erlang:list_to_tuple(lists:reverse(SchedulerIdsL)),
    LogicalProcessors = erlang:list_to_tuple(lists:reverse(LogicalProcessorsL)),
    {Bind#bind{scheduler_ids = SchedulerIds,
               logical_processors = LogicalProcessors},
     Concurrency};
bind_assign_process(ThreadIndex, SchedulerIdsL, LogicalProcessorsL, Bind,
                    #concurrency{
                        schedulers = Schedulers,
                        scheduler_bindings = SchedulerBindings,
                        scheduler_id_bind = SchedulerIdBind} = Concurrency) ->
    LogicalProcessor = element(SchedulerIdBind, SchedulerBindings),
    ConcurrencyNew = if
        SchedulerIdBind == Schedulers ->
            Concurrency#concurrency{scheduler_id_bind = 1};
        is_integer(SchedulerIdBind) ->
            Concurrency#concurrency{scheduler_id_bind = SchedulerIdBind + 1}
    end,
    bind_assign_process(ThreadIndex + 1,
                        [SchedulerIdBind | SchedulerIdsL],
                        [LogicalProcessor | LogicalProcessorsL],
                        Bind, ConcurrencyNew).

bind_string(Input, ProcessCount, ThreadCount, SchedulerIdLookup) ->
    true = is_integer(ProcessCount),
    true = is_integer(ThreadCount),
    case bind_string_parse(cloudi_string:split(",", Input), [], [], 0, [], [],
                           ThreadCount, SchedulerIdLookup) of
        {ok, Count, LogicalProcessorsExact, SchedulerIdsExact} ->
            CountRequired = ProcessCount * ThreadCount,
            if
                Count > CountRequired ->
                    {error,
                     {service_options_bind_invalid,
                      {processors_decrease, Count - CountRequired}}};
                Count < CountRequired ->
                    {error,
                     {service_options_bind_invalid,
                      {processors_increase, CountRequired - Count}}};
                Count == CountRequired ->
                    {ok,
                     #bind{string_exact = Input,
                           scheduler_ids_exact = SchedulerIdsExact,
                           logical_processors_exact = LogicalProcessorsExact}}
            end;
        {error, Reason} ->
            {error, {service_options_bind_invalid, Reason}}
    end.

bind_string_parse([], LogicalProcessorsExact, SchedulerIdsExact, Count,
                  _, _, _, _) ->
    {ok, Count,
     erlang:list_to_tuple(lists:reverse(LogicalProcessorsExact)),
     erlang:list_to_tuple(lists:reverse(SchedulerIdsExact))};
bind_string_parse([Segment | SegmentL],
                  LogicalProcessorsExact, SchedulerIdsExact, Count,
                  LogicalProcessorsElement, SchedulerIdsElement,
                  ThreadCount, SchedulerIdLookup) ->
    case cloudi_string:splitl($-, Segment, input) of
        {[_ | _] = Segment, ""} ->
            try erlang:list_to_integer(Segment) of
                Value when Value >= 0 ->
                    bind_string_parse_check([Value],
                                            LogicalProcessorsElement,
                                            SchedulerIdsElement,
                                            LogicalProcessorsExact,
                                            SchedulerIdsExact, Count, SegmentL,
                                            ThreadCount, SchedulerIdLookup);
                _ ->
                    {error, invalid_format}
            catch
                _:_ ->
                    {error, invalid_format}
            end;
        {[_ | _] = RangeMinStr, [_ | _] = RangeMaxStr} ->
            try {erlang:list_to_integer(RangeMinStr),
                 erlang:list_to_integer(RangeMaxStr)} of
                {RangeMin, RangeMax}
                    when RangeMin >= 0, RangeMin < RangeMax ->
                    Values = lists:seq(RangeMin, RangeMax, 1),
                    bind_string_parse_check(Values,
                                            LogicalProcessorsElement,
                                            SchedulerIdsElement,
                                            LogicalProcessorsExact,
                                            SchedulerIdsExact, Count, SegmentL,
                                            ThreadCount, SchedulerIdLookup);
                {_, _} ->
                    {error, invalid_format}
            catch
                _:_ ->
                    {error, invalid_format}
            end;
        {_, _} ->
            {error, invalid_format}
    end.

bind_string_parse_check([], LogicalProcessorsElement, SchedulerIdsElement,
                        LogicalProcessorsExact, SchedulerIdsExact, Count,
                        SegmentL, ThreadCount, SchedulerIdLookup) ->
    bind_string_parse(SegmentL,
                      LogicalProcessorsExact, SchedulerIdsExact, Count,
                      LogicalProcessorsElement, SchedulerIdsElement,
                      ThreadCount, SchedulerIdLookup);
bind_string_parse_check([Value | Values],
                        LogicalProcessorsElement, SchedulerIdsElement,
                        LogicalProcessorsExact, SchedulerIdsExact, Count,
                        SegmentL, ThreadCount, SchedulerIdLookup) ->
    case maps:find(Value, SchedulerIdLookup) of
        {ok, SchedulerId} ->
            CountNew = Count + 1,
            LogicalProcessorsElementNext = [Value | LogicalProcessorsElement],
            SchedulerIdsElementNext = [SchedulerId | SchedulerIdsElement],
            if
                CountNew rem ThreadCount == 0 ->
                    LogicalProcessorsElementNew =
                        erlang:list_to_tuple(lists:reverse(
                            LogicalProcessorsElementNext)),
                    SchedulerIdsElementNew =
                        erlang:list_to_tuple(lists:reverse(
                            SchedulerIdsElementNext)),
                    bind_string_parse_check(Values, [], [],
                                            [LogicalProcessorsElementNew |
                                             LogicalProcessorsExact],
                                            [SchedulerIdsElementNew |
                                             SchedulerIdsExact],
                                            CountNew, SegmentL,
                                            ThreadCount, SchedulerIdLookup);
                true ->
                    bind_string_parse_check(Values,
                                            LogicalProcessorsElementNext,
                                            SchedulerIdsElementNext,
                                            LogicalProcessorsExact,
                                            SchedulerIdsExact,
                                            CountNew, SegmentL,
                                            ThreadCount, SchedulerIdLookup)
            end;
        error ->
            {error, {invalid_processor, Value}}
    end.

scheduler_id_lookup(SchedulerBindings, Schedulers) ->
    scheduler_id_lookup(#{}, 0, SchedulerBindings, Schedulers).

scheduler_id_lookup(Output, Schedulers, _, Schedulers) ->
    Output;
scheduler_id_lookup(Output, SchedulerId, SchedulerBindings, Schedulers) ->
    SchedulerIdNew = SchedulerId + 1,
    scheduler_id_lookup(maps:put(element(SchedulerIdNew, SchedulerBindings),
                                 SchedulerIdNew, Output), SchedulerIdNew,
                        SchedulerBindings, Schedulers).

-dialyzer({no_fail_call,
           bind_erlang_process/1}).

-spec bind_erlang_process(SchedulerId :: pos_integer()) ->
    ok.

bind_erlang_process(SchedulerId) ->
    % undocumented, without a valid spec, :-(
    try erlang:process_flag(scheduler, SchedulerId) of
        0 ->
            ok
    catch
        _:_ ->
            ok
    end.

