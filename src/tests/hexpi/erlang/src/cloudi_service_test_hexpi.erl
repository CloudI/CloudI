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
-define(NAME_MEMCACHED,      "/db/memcached").
-define(NAME_TOKYOTYRANT,    "/db/tokyotyrant").
-define(NAME_COUCHDB,        "/db/couchdb").
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
        task_size
            :: cloudi_task_size:state(),
        use_pgsql = false
            :: boolean(),
        use_mysql = false
            :: boolean(),
        use_memcached = false
            :: boolean(),
        use_tokyotyrant = false
            :: boolean(),
        use_couchdb = false
            :: boolean(),
        use_filesystem = false
            :: boolean(),
        map_done = false
            :: boolean(),
        timeout_max
            :: cloudi_service:timeout_value_milliseconds(),
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
    TimeoutMax = cloudi_service:timeout_max(Dispatcher),
    {ok, setup(#state{index = IndexStart,
                      index_start = IndexStart,
                      index_end = IndexEnd,
                      task_size = TaskSize,
                      timeout_max = TimeoutMax}, Dispatcher)}.

cloudi_service_map_reduce_send(#state{map_done = true} = State, _) ->
    {done, State};
cloudi_service_map_reduce_send(#state{index = Index,
                                      index_end = IndexEnd,
                                      task_size = TaskSize,
                                      destination = Name,
                                      step = Step} = State,
                               Dispatcher)
    when is_pid(Dispatcher) ->
    case cloudi_service:get_pid(Dispatcher, Name) of
        {ok, {_, Pid} = PatternPid} ->
            {Iterations, Timeout} = cloudi_task_size:get(Pid, TaskSize),
            IndexStr = erlang:integer_to_list(Index),
            IndexBin = erlang:list_to_binary(IndexStr),
            Request = <<Iterations:32/unsigned-integer-native,
                        Step:32/unsigned-integer-native,
                        IndexBin/binary>>,
            SendArgs = [Dispatcher, Name, Request, Timeout, PatternPid],
            ?LOG_INFO("~p iterations starting at digit ~p",
                      [Iterations, Index]),
            IndexNew = Index + Step * Iterations,
            {ok, SendArgs,
             State#state{index = IndexNew,
                         map_done = (IndexNew > IndexEnd)}};
        {error, _} = Error ->
            Error
    end.

cloudi_service_map_reduce_resend([Dispatcher, Name, Request,
                                  Timeout, {_, PidOld}],
                                 #state{task_size = TaskSize,
                                        timeout_max = TimeoutMax,
                                        destination = Name} = State) ->

    case cloudi_service:get_pid(Dispatcher, Name) of
        {ok, PatternPid} ->
            <<_Iterations:32/unsigned-integer-native,
              _Step:32/unsigned-integer-native,
              IndexBin/binary>> = Request,
            IndexStr = erlang:binary_to_list(IndexBin),
            ?LOG_INFO("index ~s result timeout (after ~p ms)",
                      [IndexStr, Timeout]),
            TaskSizeNew = cloudi_task_size:reduce(PidOld, 0.9, TaskSize),
            TimeoutNew = erlang:min(Timeout * 2, TimeoutMax),
            {ok, [Dispatcher, Name, Request, TimeoutNew, PatternPid],
             State#state{task_size = TaskSizeNew}};
        {error, _} = Error ->
            Error
    end.

cloudi_service_map_reduce_recv([_, _, Request, _, {_, Pid}],
                               _ResponseInfo, Response,
                               Timeout, TransId,
                               #state{task_size = TaskSize} = State,
                               Dispatcher)
    when is_integer(Timeout), is_binary(TransId) ->
    <<Iterations:32/unsigned-integer-native,
      _Step:32/unsigned-integer-native,
      IndexBin/binary>> = Request,
    ?LOG_INFO("index ~s result received (~w map-reduce seconds elapsed)",
              [IndexBin, cloudi_service_map_reduce:elapsed_seconds()]),
    <<ElapsedTime:32/float-native, PiResult/binary>> = Response,
    TaskSizeNew = cloudi_task_size:put(Pid, Iterations, ElapsedTime, TaskSize),
    StateNew = reduce_send(IndexBin, PiResult, ElapsedTime, Pid,
                           State#state{task_size = TaskSizeNew},
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
    Memcached = service_name_pattern_pid(?NAME_MEMCACHED, Dispatcher),
    Tokyotyrant = service_name_pattern_pid(?NAME_TOKYOTYRANT, Dispatcher),
    Couchdb = service_name_pattern_pid(?NAME_COUCHDB, Dispatcher),
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
                use_memcached = is_tuple(Memcached),
                use_tokyotyrant = is_tuple(Tokyotyrant),
                use_couchdb = is_tuple(Couchdb),
                use_filesystem = is_tuple(Filesystem),
                queue = QueueN}.

reduce_send(DigitIndex, PiResult, ElapsedTime, Pid,
            #state{use_pgsql = UsePgsql,
                   use_mysql = UseMysql,
                   use_memcached = UseMemcached,
                   use_tokyotyrant = UseTokyotyrant,
                   use_couchdb = UseCouchdb,
                   use_filesystem = UseFilesystem,
                   queue = Queue0} = State, Dispatcher)
    when is_binary(DigitIndex), is_binary(PiResult), is_float(ElapsedTime),
         is_pid(Pid) ->
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
    Queue6 = if
        UseMemcached == true ->
            {ok, Queue5} = cloudi_queue:send(Dispatcher, ?NAME_MEMCACHED,
                                             memcached(DigitIndex, PiResult),
                                             Queue4),
            Queue5;
        true ->
            Queue4
    end,
    Queue8 = if
        UseTokyotyrant == true ->
            {ok, Queue7} = cloudi_queue:send(Dispatcher,
                                             ?NAME_TOKYOTYRANT,
                                             tokyotyrant(DigitIndex, PiResult),
                                             Queue6),
            Queue7;
        true ->
            Queue6
    end,
    Queue10 = if
        UseCouchdb == true ->
            {ok, Queue9} = cloudi_queue:send(Dispatcher, ?NAME_COUCHDB,
                                             couchdb(DigitIndex, Pid),
                                             Queue8),
            Queue9;
        true ->
            Queue8
    end,
    QueueN = if
        UseFilesystem == true ->
            {FilesystemRequestInfo,
             FilesystemRequest} = filesystem(DigitIndex, PiResult),
            {ok, Queue11} = cloudi_queue:send(Dispatcher,
                                              ?NAME_FILESYSTEM,
                                              FilesystemRequestInfo,
                                              FilesystemRequest,
                                              undefined, undefined,
                                              Queue10),
            Queue11;
        true ->
            Queue10
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

memcached(DigitIndex, PiResult) ->
    cloudi_string:format_to_list("{set, \"~s\", <<\"~s\">>}",
                                 [DigitIndex, PiResult]).

tokyotyrant(DigitIndex, PiResult) ->
    cloudi_string:format_to_list("{put, \"~s\", <<\"~s\">>}",
                                 [DigitIndex, PiResult]).

couchdb(DigitIndex, Pid) ->
    cloudi_string:format_to_list("{update_document, \"pi_state\","
                                 " [{<<\"~s\", <<\"~s\">>}]}",
                                 [erlang:pid_to_list(Pid), DigitIndex]).

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

