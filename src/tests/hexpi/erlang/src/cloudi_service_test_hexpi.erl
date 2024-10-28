%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Map-Reduce Example For hexpi Test==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2012-2023 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2012-2023 Michael Truog
%%% @version 2.0.6 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_test_hexpi).
-author('mjtruog at protonmail dot com').

-behaviour(cloudi_service_map_reduce).

%% cloudi_service_map_reduce callbacks
-export([cloudi_service_map_reduce_new/5,
         cloudi_service_map_reduce_send/2,
         cloudi_service_map_reduce_resend/2,
         cloudi_service_map_reduce_recv/7,
         cloudi_service_map_reduce_info/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include_lib("cloudi_core/include/cloudi_service.hrl").

-define(NAME_PGSQL,          "/db/pgsql/cloudi_tests").
-define(NAME_MYSQL,          "/db/mysql/cloudi_tests").
-define(NAME_FILESYSTEM,     "/tests/http_req/hexpi.txt/post").

% example runtimes for
% AMD Phenom 9950 Quad-Core, 64bit, linux 2.6.27-14-generic:
% 10^6th digit in  6.5 seconds
% 10^7th digit in  1.2 minutes
% 10^8th digit in 13.2 minutes
% 10^9th digit in  2.4 hours
% (scales linearly)

% 32 max with current piqpr8_gmp.cpp float precision
-define(PI_DIGIT_STEP_SIZE, 32).

-record(state,
    {
        index
            :: pos_integer(),
        index_start
            :: pos_integer(),
        index_end
            :: pos_integer(),
        task_scheduler
            :: cloudi_task_scheduler:state(),
        use_pgsql = false
            :: boolean(),
        use_mysql = false
            :: boolean(),
        use_filesystem = false
            :: boolean(),
        map_done = false
            :: boolean(),
        destination = "/tests/hexpi"
            :: cloudi_service:service_name(),
        step = ?PI_DIGIT_STEP_SIZE,
        queue = cloudi_queue:new([{retry, 3},
                                  {retry_delay, {5, minutes}},
                                  {failures_source_die, true}])
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service_map_reduce
%%%------------------------------------------------------------------------

cloudi_service_map_reduce_new([IndexStart, IndexEnd], ConcurrentTaskCount,
                              _Prefix, _Timeout, Dispatcher)
    when is_integer(IndexStart), is_integer(IndexEnd),
         is_pid(Dispatcher) ->
    IterationsMin = 1,
    IterationsMax = 1000000000,
    TargetTimeMin = 1.0 / 3600.0, % 1 second, in hours
    TargetTimeMax = 6.0, % hours
    TaskSize = cloudi_task_size:new(ConcurrentTaskCount,
                                    IterationsMin,
                                    IterationsMin, IterationsMax,
                                    TargetTimeMin,
                                    TargetTimeMin, TargetTimeMax),
    TaskScheduler = cloudi_task_scheduler:new(Dispatcher, TaskSize),
    {ok, setup(#state{index = IndexStart,
                      index_start = IndexStart,
                      index_end = IndexEnd,
                      task_scheduler = TaskScheduler}, Dispatcher)}.

cloudi_service_map_reduce_send(#state{map_done = true} = State, _) ->
    {done, State};
cloudi_service_map_reduce_send(#state{index = Index,
                                      index_end = IndexEnd,
                                      task_scheduler = TaskScheduler,
                                      destination = Name,
                                      step = Step} = State,
                               Dispatcher)
    when is_pid(Dispatcher) ->
    TaskId = IndexBin = erlang:integer_to_binary(Index),
    case cloudi_task_scheduler:get_pid(Dispatcher, Name, TaskId,
                                       TaskScheduler) of
        {ok, PatternPid, Timeout, TaskCost, TaskSchedulerNew} ->
            Iterations = TaskCost,
            Request = <<Iterations:32/unsigned-integer-native,
                        Step:32/unsigned-integer-native,
                        IndexBin/binary>>,
            SendArgs = [Dispatcher, Name, Request, Timeout, PatternPid],
            ?LOG_INFO("~p iterations starting at digit ~s",
                      [Iterations, IndexBin]),
            IndexNew = Index + Step * Iterations,
            {ok, SendArgs,
             State#state{index = IndexNew,
                         task_scheduler = TaskSchedulerNew,
                         map_done = (IndexNew > IndexEnd)}};
        {error, _} = Error ->
            Error
    end.

cloudi_service_map_reduce_resend([Dispatcher, Name, Request,
                                  Timeout, PatternPidOld],
                                 #state{task_scheduler = TaskScheduler,
                                        destination = Name} = State) ->
    <<_Iterations:32/unsigned-integer-native,
      _Step:32/unsigned-integer-native,
      IndexBin/binary>> = Request,
    TaskId = IndexBin,
    case cloudi_task_scheduler:get_pid_retry(Dispatcher, Name, PatternPidOld,
                                             TaskId, TaskScheduler) of
        {ok, PatternPid, TimeoutNew, TaskSchedulerNew} ->
            ?LOG_INFO("index ~s result timeout (after ~p ms)",
                      [IndexBin, Timeout]),
            {ok, [Dispatcher, Name, Request, TimeoutNew, PatternPid],
             State#state{task_scheduler = TaskSchedulerNew}};
        {error, _} = Error ->
            Error
    end.

cloudi_service_map_reduce_recv([_, Name, Request, _, PatternPidOld],
                               _ResponseInfo, Response,
                               TimeoutNew, _TransId,
                               #state{task_scheduler = TaskScheduler} = State,
                               Dispatcher) ->
    <<_Iterations:32/unsigned-integer-native,
      _Step:32/unsigned-integer-native,
      IndexBin/binary>> = Request,
    TaskId = IndexBin,
    ?LOG_INFO("index ~s result received (~w map-reduce seconds elapsed)",
              [IndexBin, cloudi_service_map_reduce:elapsed_seconds()]),
    <<_ElapsedTime:32/float-native, PiResult/binary>> = Response,
    {ok, _,
     TaskSchedulerNew} = cloudi_task_scheduler:task_done(Name, PatternPidOld,
                                                         TaskId, TimeoutNew,
                                                         TaskScheduler),
    StateNew = reduce_send(IndexBin, PiResult,
                           State#state{task_scheduler = TaskSchedulerNew},
                           Dispatcher),
    reduce_done_check(StateNew, Dispatcher).

cloudi_service_map_reduce_info(#return_async_active{} = Request,
                               #state{queue = Queue0} = State, Dispatcher) ->
    {ok, QueueN} = cloudi_queue:recv(Dispatcher, Request, Queue0),
    reduce_done_check(State#state{queue = QueueN}, Dispatcher);
cloudi_service_map_reduce_info(#timeout_async_active{} = Request,
                               #state{queue = Queue0} = State, Dispatcher) ->
    {ok, QueueN} = cloudi_queue:timeout(Dispatcher, Request, Queue0),
    {ok, State#state{queue = QueueN}};
cloudi_service_map_reduce_info(Request, _, _) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {error, {unknown_info, Request}}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

setup(#state{queue = Queue0} = State, Dispatcher) ->
    TimeoutAsync = cloudi_service:timeout_async(Dispatcher),
    Pgsql = service_name_pattern_pid(?NAME_PGSQL, Dispatcher),
    Mysql = service_name_pattern_pid(?NAME_MYSQL, Dispatcher),
    Filesystem = service_name_pattern_pid(?NAME_FILESYSTEM, Dispatcher),

    SQLDrop = sql_drop(),
    SQLCreate = sql_create(),
    Queue2 = if
        Pgsql /= undefined ->
            {ok, Queue1} = cloudi_queue:send(Dispatcher, ?NAME_PGSQL,
                                             <<SQLDrop/binary,
                                               SQLCreate/binary>>,
                                             TimeoutAsync, Pgsql, Queue0),
            Queue1;
        true ->
            Queue0
    end,
    QueueN = if
        Mysql /= undefined ->
            {ok, Queue3} = cloudi_queue:send(Dispatcher, ?NAME_MYSQL,
                                             <<SQLDrop/binary,
                                               SQLCreate/binary>>,
                                             TimeoutAsync, Mysql, Queue2),
            Queue3;
        true ->
            Queue2
    end,
    State#state{use_pgsql = is_tuple(Pgsql),
                use_mysql = is_tuple(Mysql),
                use_filesystem = is_tuple(Filesystem),
                queue = QueueN}.

reduce_send(DigitIndex, PiResult,
            #state{use_pgsql = UsePgsql,
                   use_mysql = UseMysql,
                   use_filesystem = UseFilesystem,
                   queue = Queue0} = State, Dispatcher)
    when is_binary(DigitIndex), is_binary(PiResult) ->
    Queue2 = if
        UsePgsql == true ->
            {ok, Queue1} = cloudi_queue:send(Dispatcher, ?NAME_PGSQL,
                                             sql_insert(DigitIndex, PiResult),
                                             Queue0),
            Queue1;
        true ->
            Queue0
    end,
    Queue4 = if
        UseMysql == true ->
            {ok, Queue3} = cloudi_queue:send(Dispatcher, ?NAME_MYSQL,
                                             sql_insert(DigitIndex, PiResult),
                                             Queue2),
            Queue3;
        true ->
            Queue2
    end,
    QueueN = if
        UseFilesystem == true ->
            {FilesystemRequestInfo,
             FilesystemRequest} = filesystem(DigitIndex, PiResult),
            {ok, Queue5} = cloudi_queue:send(Dispatcher,
                                             ?NAME_FILESYSTEM,
                                             FilesystemRequestInfo,
                                             FilesystemRequest,
                                             undefined, undefined,
                                             Queue4),
            Queue5;
        true ->
            Queue4
    end,
    State#state{queue = QueueN}.

reduce_done_check(#state{map_done = false} = State, _) ->
    {ok, State};
reduce_done_check(#state{map_done = true,
                         queue = Queue} = State,
                  Dispatcher) ->
    MapSize = cloudi_service_map_reduce:map_size(),
    ReduceSize = cloudi_queue:size(Dispatcher, Queue),
    if
        MapSize == 0, ReduceSize == 0 ->
            {done, State};
        true ->
            {ok, State}
    end.

sql_drop() ->
    <<"DROP TABLE IF EXISTS incoming_results;">>.

sql_create() ->
    <<"CREATE TABLE incoming_results ("
      "digit_index   NUMERIC(30) PRIMARY KEY,"
      "data          TEXT"
      ");">>.

sql_insert(DigitIndex, PiResult) ->
    cloudi_string:format_to_binary("INSERT INTO incoming_results "
                                   "(digit_index, data) "
                                   "VALUES (~s, '~s');",
                                   [DigitIndex, PiResult]).

filesystem(DigitIndex, PiResult) ->
    {[{<<"range">>, <<"bytes=", DigitIndex/binary, "-">>}], PiResult}.

service_name_pattern_pid(Name, Dispatcher) ->
    case cloudi_service:get_pid(Dispatcher, Name, limit_min) of
        {ok, {Name, _} = PatternPid} ->
            PatternPid;
        {ok, _} ->
            undefined;
        {error, _} ->
            undefined
    end.

