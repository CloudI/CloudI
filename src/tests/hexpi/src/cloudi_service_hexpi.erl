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
%%% Copyright (c) 2012-2013, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2012-2013 Michael Truog
%%% @version 1.3.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_hexpi).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_service_map_reduce).

%% cloudi_service_map_reduce callbacks
-export([cloudi_service_map_reduce_new/3,
         cloudi_service_map_reduce_send/2,
         cloudi_service_map_reduce_resend/2,
         cloudi_service_map_reduce_recv/7]).

-include("cloudi_logger.hrl").

-define(NAME_PGSQL,          "/db/pgsql/cloudi_tests_proxy").
-define(NAME_MYSQL,          "/db/mysql/cloudi_tests_proxy").
-define(NAME_MEMCACHED,      "/db/memcached").
-define(NAME_TOKYOTYRANT,    "/db/tokyotyrant").
-define(NAME_COUCHDB,        "/db/couchdb").

% example runtimes for
% AMD Phenom 9950 Quad-Core, 64bit, linux 2.6.27-14-generic:
% 10^6th digit in  6.5 seconds
% 10^7th digit in  1.2 minutes
% 10^8th digit in 13.2 minutes
% 10^9th digit in  2.4 hours
% (scales linearly)

% 32 max with current piqpr8_gmp.cpp float precision
-define(PI_DIGIT_STEP_SIZE, 32).
% the number of iterations that will totally exceed the time target
% on all the machines that will be running tasks
-define(MAX_ITERATIONS, 1000000.0).

-record(state,
    {
        destination = "/tests/hexpi",
        index,
        index_start,
        index_end,
        done = false,
        step = ?PI_DIGIT_STEP_SIZE,
        target_time = (1.0 / 3600.0), % hours
        task_size_initial = (1.0 / ?MAX_ITERATIONS), % percentage
        task_size_lookup = cloudi_task_size:new(),
        timeout_async,
        use_pgsql,
        use_mysql,
        use_memcached,
        use_tokyotyrant,
        use_couchdb
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service_map_reduce
%%%------------------------------------------------------------------------

cloudi_service_map_reduce_new([IndexStart, IndexEnd], _Prefix, Dispatcher)
    when is_integer(IndexStart), is_integer(IndexEnd),
         is_pid(Dispatcher) ->
    {ok, setup(#state{index = IndexStart,
                      index_start = IndexStart,
                      index_end = IndexEnd}, Dispatcher)}.

cloudi_service_map_reduce_send(#state{done = true} = State, _) ->
    {done, State};
cloudi_service_map_reduce_send(#state{destination = Name,
                                      index = Index,
                                      index_end = IndexEnd,
                                      step = Step,
                                      target_time = TargetTime,
                                      task_size_initial = TaskSizeInitial,
                                      task_size_lookup = TaskSizeLookup} =
                               State,
                               Dispatcher)
    when is_pid(Dispatcher) ->
    case cloudi_service:get_pid(Dispatcher, Name) of
        {ok, {_, Pid} = PatternPid} ->
            TaskSize = cloudi_task_size:get(TaskSizeInitial, Pid,
                                            TaskSizeLookup),
            Iterations = erlang:round(0.5 + TaskSize * ?MAX_ITERATIONS),
            % determine the size of the task and take the ceiling of the value
            % (to avoid iterations of 0)
            IndexStr = erlang:integer_to_list(Index),
            IndexBin = erlang:list_to_binary(IndexStr),
            Request = <<Iterations:32/unsigned-integer-native,
                        Step:32/unsigned-integer-native,
                        IndexBin/binary>>,
            % TargetTime is the percentage of an hour
            % (elapsed time is returned this way from the hexpi C++ code)
            Timeout = erlang:round(0.5 + TargetTime * 3600000.0) + 5000,
            SendArgs = [Dispatcher, Name, Request, Timeout, PatternPid],
            ?LOG_INFO("~p iterations starting at digit ~p~n",
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
                                        target_time = TargetTime,
                                        task_size_lookup = TaskSizeLookup} =
                                 State) ->

    case cloudi_service:get_pid(Dispatcher, Name) of
        {ok, PatternPid} ->
            <<Iterations:32/unsigned-integer-native,
              _Step:32/unsigned-integer-native,
              IndexBin/binary>> = Request,
            IndexStr = erlang:binary_to_list(IndexBin),
            TaskSize = Iterations / ?MAX_ITERATIONS,
            ?LOG_INFO("index ~s result timeout (after ~p ms)~n",
                      [IndexStr, Timeout]),
            % a timeout generates a guess at what the
            % elapsed time could have been to reduce the task size
            ElapsedTime = (Timeout / 3600000.0) * 10.0,
            NewTaskSizeLookup = cloudi_task_size:put(TaskSize,
                                                     TargetTime,
                                                     ElapsedTime,
                                                     OldPid,
                                                     TaskSizeLookup),
            {ok, [Dispatcher, Name, Request, Timeout * 2, PatternPid],
             State#state{task_size_lookup = NewTaskSizeLookup}};
        {error, _} = Error ->
            Error
    end.

cloudi_service_map_reduce_recv([_, _, Request, _, {_, Pid}],
                               _ResponseInfo, Response,
                               Timeout, TransId,
                               #state{done = Done,
                                      target_time = TargetTime,
                                      task_size_lookup = TaskSizeLookup} =
                               State,
                               Dispatcher)
    when is_integer(Timeout), is_binary(TransId) ->
    <<Iterations:32/unsigned-integer-native,
      _Step:32/unsigned-integer-native,
      IndexBin/binary>> = Request,
    IndexStr = erlang:binary_to_list(IndexBin),
    ?LOG_INFO("index ~s result received~n", [IndexStr]),
    TaskSize = Iterations / ?MAX_ITERATIONS,
    <<ElapsedTime:32/float-native, PiResult/binary>> = Response,
    send_results(IndexStr, PiResult, Pid, State, Dispatcher),
    NewTaskSizeLookup = cloudi_task_size:put(TaskSize,
                                             TargetTime,
                                             ElapsedTime,
                                             Pid,
                                             TaskSizeLookup),
    NextState = State#state{task_size_lookup = NewTaskSizeLookup},
    if
        Done =:= true ->
            {done, NextState};
        true ->
            {ok, NextState}
    end.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

setup(State, Dispatcher) ->
    TimeoutAsync = cloudi_service:timeout_async(Dispatcher),
    PidPgsql = case cloudi_service:get_pid(Dispatcher,
                                           ?NAME_PGSQL, 200) of
        {ok, Pid1} ->
            Pid1;
        {error, _} ->
            undefined
    end,
    PidMysql = case cloudi_service:get_pid(Dispatcher,
                                           ?NAME_MYSQL, 200) of
        {ok, Pid2} ->
            Pid2;
        {error, _} ->
            undefined
    end,
    PidMemcached = case cloudi_service:get_pid(Dispatcher,
                                               ?NAME_MEMCACHED, 200) of
        {ok, Pid3} ->
            Pid3;
        {error, _} ->
            undefined
    end,
    PidTokyotyrant = case cloudi_service:get_pid(Dispatcher,
                                                 ?NAME_TOKYOTYRANT, 200) of
        {ok, Pid4} ->
            Pid4;
        {error, _} ->
            undefined
    end,
    PidCouchdb = case cloudi_service:get_pid(Dispatcher,
                                             ?NAME_COUCHDB, 200) of
        {ok, Pid5} ->
            Pid5;
        {error, _} ->
            undefined
    end,

    SQLDrop = sql_drop(),
    SQLCreate = sql_create(),
    if
        PidPgsql /= undefined ->
            cloudi_service:send_async(Dispatcher, ?NAME_PGSQL, SQLDrop,
                                      TimeoutAsync, PidPgsql),
            cloudi_service:send_async(Dispatcher, ?NAME_PGSQL, SQLCreate,
                                      TimeoutAsync, PidPgsql);
        true ->
            ok
    end,
    if
        PidMysql /= undefined ->
            cloudi_service:send_async(Dispatcher, ?NAME_MYSQL, SQLDrop,
                                      TimeoutAsync, PidMysql),
            cloudi_service:send_async(Dispatcher, ?NAME_MYSQL, SQLCreate,
                                      TimeoutAsync, PidMysql);
        true ->
            ok
    end,
    State#state{timeout_async = TimeoutAsync,
                use_pgsql = is_pid(PidPgsql),
                use_mysql = is_pid(PidMysql),
                use_memcached = is_pid(PidMemcached),
                use_tokyotyrant = is_pid(PidTokyotyrant),
                use_couchdb = is_pid(PidCouchdb)}.

send_results(DigitIndexStr, PiResult, Pid,
             #state{use_pgsql = UsePgsql,
                    use_mysql = UseMysql,
                    use_memcached = UseMemcached,
                    use_tokyotyrant = UseTokyotyrant,
                    use_couchdb = UseCouchdb}, Dispatcher) ->
    SQLInsert = sql_insert(DigitIndexStr, PiResult),
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
                                      memcached(DigitIndexStr, PiResult));
        true ->
            ok
    end,
    if
        UseTokyotyrant == true ->
            cloudi_service:send_async(Dispatcher, ?NAME_TOKYOTYRANT,
                                      tokyotyrant(DigitIndexStr, PiResult));
        true ->
            ok
    end,
    if
        UseCouchdb == true ->
            cloudi_service:send_async(Dispatcher, ?NAME_COUCHDB,
                                      couchdb(DigitIndexStr, Pid));
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

sql_insert(DigitIndexStr, PiResult) ->
    list_to_binary(cloudi_string:format("INSERT INTO incoming_results "
                                        "(digit_index, data) "
                                        "VALUES (~s, '~s');",
                                        [DigitIndexStr, PiResult])).

memcached(DigitIndexStr, PiResult) ->
    cloudi_string:format("{set, \"~s\", <<\"~s\">>}",
                         [DigitIndexStr, PiResult]).

tokyotyrant(DigitIndexStr, PiResult) ->
    cloudi_string:format("{put, \"~s\", <<\"~s\">>}",
                         [DigitIndexStr, PiResult]).

couchdb(DigitIndexStr, Pid) ->
    cloudi_string:format("{update_document, \"pi_state\","
                         " [{<<\"~s\", <<\"~s\">>}]}",
                         [erlang:pid_to_list(Pid), DigitIndexStr]).

