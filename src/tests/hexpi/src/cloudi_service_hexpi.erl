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
%%% Copyright (c) 2012-2017 Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2012-2017 Michael Truog
%%% @version 1.7.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_hexpi).
-author('mjtruog [at] gmail (dot) com').

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
        destination = "/tests/hexpi",
        index,
        index_start,
        index_end,
        done = false,
        step = ?PI_DIGIT_STEP_SIZE,
        task_size,
        queue,
        use_pgsql,
        use_mysql,
        use_memcached,
        use_tokyotyrant,
        use_couchdb,
        use_filesystem
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
    Queue = cloudi_queue:new([{retry, 3},
                              {failures_source_die, true}]),
    {ok, setup(#state{index = IndexStart,
                      index_start = IndexStart,
                      index_end = IndexEnd,
                      task_size = TaskSize,
                      queue = Queue}, Dispatcher)}.

cloudi_service_map_reduce_send(#state{done = true} = State, _) ->
    {done, State};
cloudi_service_map_reduce_send(#state{destination = Name,
                                      index = Index,
                                      index_end = IndexEnd,
                                      step = Step,
                                      task_size = TaskSize} =
                               State,
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
            NewIndex = Index + Step * Iterations,
            {ok, SendArgs,
             State#state{index = NewIndex,
                         done = (NewIndex > IndexEnd)}};
        {error, _} = Error ->
            Error
    end.

cloudi_service_map_reduce_resend([Dispatcher, Name, Request,
                                  Timeout, {_, OldPid}],
                                 #state{destination = Name,
                                        task_size = TaskSize} =
                                 State) ->

    case cloudi_service:get_pid(Dispatcher, Name) of
        {ok, PatternPid} ->
            <<_Iterations:32/unsigned-integer-native,
              _Step:32/unsigned-integer-native,
              IndexBin/binary>> = Request,
            IndexStr = erlang:binary_to_list(IndexBin),
            ?LOG_INFO("index ~s result timeout (after ~p ms)",
                      [IndexStr, Timeout]),
            NewTaskSize = cloudi_task_size:reduce(OldPid, 0.9, TaskSize),
            {ok, [Dispatcher, Name, Request, Timeout * 2, PatternPid],
             State#state{task_size = NewTaskSize}};
        {error, _} = Error ->
            Error
    end.

cloudi_service_map_reduce_recv([_, _, Request, _, {_, Pid}],
                               _ResponseInfo, Response,
                               Timeout, TransId,
                               #state{done = Done,
                                      task_size = TaskSize} = State,
                               Dispatcher)
    when is_integer(Timeout), is_binary(TransId) ->
    <<Iterations:32/unsigned-integer-native,
      _Step:32/unsigned-integer-native,
      IndexBin/binary>> = Request,
    ?LOG_INFO("index ~s result received", [IndexBin]),
    <<ElapsedTime:32/float-native, PiResult/binary>> = Response,
    NewTaskSize = cloudi_task_size:put(Pid, Iterations, ElapsedTime, TaskSize),
    NewState = send_results(IndexBin, PiResult, ElapsedTime, Pid,
                            State#state{task_size = NewTaskSize},
                            Dispatcher),
    if
        Done =:= true ->
            {done, NewState};
        true ->
            {ok, NewState}
    end.

cloudi_service_map_reduce_info(#return_async_active{} = Request,
                               #state{queue = Queue0} = State, Dispatcher) ->
    {ok, QueueN} = cloudi_queue:recv(Dispatcher, Request, Queue0),
    {ok, State#state{queue = QueueN}};
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
    Pgsql = case cloudi_service:get_pid(Dispatcher,
                                        ?NAME_PGSQL,
                                        limit_min) of
        {ok, PatternPid1} ->
            PatternPid1;
        {error, _} ->
            undefined
    end,
    Mysql = case cloudi_service:get_pid(Dispatcher,
                                        ?NAME_MYSQL,
                                        limit_min) of
        {ok, PatternPid2} ->
            PatternPid2;
        {error, _} ->
            undefined
    end,
    Memcached = case cloudi_service:get_pid(Dispatcher,
                                            ?NAME_MEMCACHED,
                                            limit_min) of
        {ok, PatternPid3} ->
            PatternPid3;
        {error, _} ->
            undefined
    end,
    Tokyotyrant = case cloudi_service:get_pid(Dispatcher,
                                              ?NAME_TOKYOTYRANT,
                                              limit_min) of
        {ok, PatternPid4} ->
            PatternPid4;
        {error, _} ->
            undefined
    end,
    Couchdb = case cloudi_service:get_pid(Dispatcher,
                                          ?NAME_COUCHDB,
                                          limit_min) of
        {ok, PatternPid5} ->
            PatternPid5;
        {error, _} ->
            undefined
    end,
    Filesystem = case cloudi_service:get_pid(Dispatcher,
                                             ?NAME_FILESYSTEM,
                                             limit_min) of
        {ok, PatternPid6} ->
            PatternPid6;
        {error, _} ->
            undefined
    end,

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
    State#state{queue = QueueN,
                use_pgsql = is_tuple(Pgsql),
                use_mysql = is_tuple(Mysql),
                use_memcached = is_tuple(Memcached),
                use_tokyotyrant = is_tuple(Tokyotyrant),
                use_couchdb = is_tuple(Couchdb),
                use_filesystem = is_tuple(Filesystem)}.

send_results(DigitIndex, PiResult, ElapsedTime, Pid,
             #state{queue = Queue0,
                    use_pgsql = UsePgsql,
                    use_mysql = UseMysql,
                    use_memcached = UseMemcached,
                    use_tokyotyrant = UseTokyotyrant,
                    use_couchdb = UseCouchdb,
                    use_filesystem = UseFilesystem} = State, Dispatcher)
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

