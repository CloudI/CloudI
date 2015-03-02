%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Map-Reduce Example For hexpi Test==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2012-2015, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2012-2015 Michael Truog
%%% @version 1.4.1 {@date} {@time}
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
    {ok, setup(#state{index = IndexStart,
                      index_start = IndexStart,
                      index_end = IndexEnd,
                      task_size = TaskSize}, Dispatcher)}.

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
            <<Iterations:32/unsigned-integer-native,
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
                                      task_size = TaskSize} =
                               State,
                               Dispatcher)
    when is_integer(Timeout), is_binary(TransId) ->
    <<Iterations:32/unsigned-integer-native,
      _Step:32/unsigned-integer-native,
      IndexBin/binary>> = Request,
    ?LOG_INFO("index ~s result received", [IndexBin]),
    <<ElapsedTime:32/float-native, PiResult/binary>> = Response,
    send_results(IndexBin, PiResult, ElapsedTime, Pid, State, Dispatcher),
    NewTaskSize = cloudi_task_size:put(Pid, Iterations, ElapsedTime, TaskSize),
    NextState = State#state{task_size = NewTaskSize},
    if
        Done =:= true ->
            {done, NextState};
        true ->
            {ok, NextState}
    end.

cloudi_service_map_reduce_info(Request, _, _) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {error, {unknown_info, Request}}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

setup(State, Dispatcher) ->
    TimeoutAsync = cloudi_service:timeout_async(Dispatcher),
    Pgsql = case cloudi_service:get_pid(Dispatcher,
                                        ?NAME_PGSQL,
                                        immediate) of
        {ok, PatternPid1} ->
            PatternPid1;
        {error, _} ->
            undefined
    end,
    Mysql = case cloudi_service:get_pid(Dispatcher,
                                        ?NAME_MYSQL,
                                        immediate) of
        {ok, PatternPid2} ->
            PatternPid2;
        {error, _} ->
            undefined
    end,
    Memcached = case cloudi_service:get_pid(Dispatcher,
                                            ?NAME_MEMCACHED,
                                            immediate) of
        {ok, PatternPid3} ->
            PatternPid3;
        {error, _} ->
            undefined
    end,
    Tokyotyrant = case cloudi_service:get_pid(Dispatcher,
                                              ?NAME_TOKYOTYRANT,
                                              immediate) of
        {ok, PatternPid4} ->
            PatternPid4;
        {error, _} ->
            undefined
    end,
    Couchdb = case cloudi_service:get_pid(Dispatcher,
                                          ?NAME_COUCHDB,
                                          immediate) of
        {ok, PatternPid5} ->
            PatternPid5;
        {error, _} ->
            undefined
    end,
    Filesystem = case cloudi_service:get_pid(Dispatcher,
                                             ?NAME_FILESYSTEM,
                                             immediate) of
        {ok, PatternPid6} ->
            PatternPid6;
        {error, _} ->
            undefined
    end,

    SQLDrop = sql_drop(),
    SQLCreate = sql_create(),
    if
        Pgsql /= undefined ->
            cloudi_service:send_async(Dispatcher, ?NAME_PGSQL, SQLDrop,
                                      TimeoutAsync, Pgsql),
            cloudi_service:send_async(Dispatcher, ?NAME_PGSQL, SQLCreate,
                                      TimeoutAsync, Pgsql);
        true ->
            ok
    end,
    if
        Mysql /= undefined ->
            cloudi_service:send_async(Dispatcher, ?NAME_MYSQL, SQLDrop,
                                      TimeoutAsync, Mysql),
            cloudi_service:send_async(Dispatcher, ?NAME_MYSQL, SQLCreate,
                                      TimeoutAsync, Mysql);
        true ->
            ok
    end,
    State#state{use_pgsql = is_tuple(Pgsql),
                use_mysql = is_tuple(Mysql),
                use_memcached = is_tuple(Memcached),
                use_tokyotyrant = is_tuple(Tokyotyrant),
                use_couchdb = is_tuple(Couchdb),
                use_filesystem = is_tuple(Filesystem)}.

send_results(DigitIndex, PiResult, ElapsedTime, Pid,
             #state{use_pgsql = UsePgsql,
                    use_mysql = UseMysql,
                    use_memcached = UseMemcached,
                    use_tokyotyrant = UseTokyotyrant,
                    use_couchdb = UseCouchdb,
                    use_filesystem = UseFilesystem}, Dispatcher)
    when is_binary(DigitIndex), is_binary(PiResult), is_float(ElapsedTime),
         is_pid(Pid) ->
    SQLInsert = sql_insert(DigitIndex, PiResult),
    if
        UsePgsql == true ->
            cloudi_service:send_async(Dispatcher, ?NAME_PGSQL, SQLInsert);
        true ->
            ok
    end,
    if
        UseMysql == true ->
            cloudi_service:send_async(Dispatcher, ?NAME_MYSQL, SQLInsert);
        true ->
            ok
    end,
    if
        UseMemcached == true ->
            cloudi_service:send_async(Dispatcher, ?NAME_MEMCACHED,
                                      memcached(DigitIndex, PiResult));
        true ->
            ok
    end,
    if
        UseTokyotyrant == true ->
            cloudi_service:send_async(Dispatcher, ?NAME_TOKYOTYRANT,
                                      tokyotyrant(DigitIndex, PiResult));
        true ->
            ok
    end,
    if
        UseCouchdb == true ->
            cloudi_service:send_async(Dispatcher, ?NAME_COUCHDB,
                                      couchdb(DigitIndex, Pid));
        true ->
            ok
    end,
    if
        UseFilesystem == true ->
            {FilesystemRequestInfo,
             FilesystemRequest} = filesystem(DigitIndex, PiResult),
            cloudi_service:send_async(Dispatcher,
                                      ?NAME_FILESYSTEM,
                                      FilesystemRequestInfo,
                                      FilesystemRequest,
                                      undefined, undefined);
        true ->
            ok
    end,
    ok.

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

