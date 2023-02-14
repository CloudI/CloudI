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
         bind_validate/2,
         bind_assign_process/2,
         bind_assign_process/3,
         bind_increment_thread/1,
         bind_init/1,
         bind_logical_processor/1,
         count/2,
         new/0]).

-record(concurrency,
    {
        schedulers :: pos_integer(),
        scheduler_bindings :: tuple() | undefined,
        scheduler_id_bind = 1 :: pos_integer()
    }).

-record(bind,
    {
        thread_count = undefined :: pos_integer() | undefined,
        thread_index = 0 :: non_neg_integer(),
        scheduler_ids = undefined :: tuple() | undefined,
        logical_processors = undefined :: tuple() | undefined
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
    boolean().

bind_format(#bind{}) ->
    true;
bind_format(false) ->
    false.

%%-------------------------------------------------------------------------
%% @doc
%% ===Convert the bind configuration format to internal state.===
%% @end
%%-------------------------------------------------------------------------

-spec bind_validate(Bind :: boolean(),
                    Concurrency :: #concurrency{} | undefined) ->
    {ok, #bind{} | false} |
    {error, {service_options_bind_invalid, any()}}.

bind_validate(true,
              #concurrency{scheduler_bindings = SchedulerBindings}) ->
    if
        is_tuple(SchedulerBindings) ->
            {ok, #bind{}};
        SchedulerBindings =:= undefined ->
            {error, {service_options_bind_invalid, unbound}}
    end;
bind_validate(false, _) ->
    {ok, false}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Assign bind internal service process data.===
%% @end
%%-------------------------------------------------------------------------

-spec bind_assign_process(Bind :: #bind{} | false,
                          Concurrency :: #concurrency{}) ->
    {#bind{} | false, #concurrency{}}.

bind_assign_process(Bind, Concurrency) ->
    bind_assign_process(Bind, 1, Concurrency).

%%-------------------------------------------------------------------------
%% @doc
%% ===Assign bind external service process data.===
%% @end
%%-------------------------------------------------------------------------

-spec bind_assign_process(Bind :: #bind{} | false,
                          ThreadCount :: pos_integer(),
                          Concurrency :: #concurrency{}) ->
    {#bind{} | false, #concurrency{}}.

bind_assign_process(#bind{thread_count = undefined,
                          scheduler_ids = undefined,
                          logical_processors = undefined} = Bind,
                    ThreadCount, Concurrency) ->
    bind_assign_process(0, [], [],
                        Bind#bind{thread_count = ThreadCount}, Concurrency);
bind_assign_process(false, _, Concurrency) ->
    {false, Concurrency}.

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
    #concurrency{schedulers = Schedulers,
                 scheduler_bindings = SchedulerBindings}.

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

