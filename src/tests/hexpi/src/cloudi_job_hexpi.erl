%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Work Module For hexpi Test==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2009-2012, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2009-2012 Michael Truog
%%% @version 0.2.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_job_hexpi).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_job).

%% cloudi_job callbacks
-export([cloudi_job_init/3,
         cloudi_job_handle_request/10,
         cloudi_job_handle_info/3,
         cloudi_job_terminate/2]).

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

-record(task,
    {
        task_size,
        iterations,
        step,
        index,
        timeout
    }).

-record(state,
    {
        index,
        index_start,
        index_end,
        tasks_failed = [],
        concurrent_tasks,
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
%%% Callback functions from cloudi_job
%%%------------------------------------------------------------------------

cloudi_job_init([IndexStart, IndexEnd, ConcurrentTasks],
                _Prefix, _Dispatcher) ->
    self() ! setup,
    self() ! task,
    {ok, #state{index = IndexStart,
                index_start = IndexStart,
                index_end = IndexEnd,
                concurrent_tasks =
                    cloudi_configurator:concurrency(ConcurrentTasks)}}.

cloudi_job_handle_request(_Type, _Name, _RequestInfo, _Request,
                          _Timeout, _Priority, _TransId, _Pid,
                          State, _Dispatcher) ->
    {reply, <<>>, State}.

cloudi_job_handle_info(setup, State, Dispatcher) ->
    {noreply, setup(State, Dispatcher)};

cloudi_job_handle_info(task,
                       #state{concurrent_tasks = ConcurrentTasks} = State,
                       Dispatcher) ->
    {Requests, Done, NewState} = send_tasks(ConcurrentTasks, [],
                                            State, Dispatcher),
    {noreply, recv_tasks(Requests, Done, NewState, Dispatcher)};

cloudi_job_handle_info(Request, State, _) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_job_terminate(_, #state{}) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

setup(State, Dispatcher) ->
    TimeoutAsync = cloudi_job:timeout_async(Dispatcher),
    PidPgsql = case cloudi_job:get_pid(Dispatcher,
                                       ?NAME_PGSQL, 200) of
        {ok, Pid1} ->
            Pid1;
        {error, _} ->
            undefined
    end,
    PidMysql = case cloudi_job:get_pid(Dispatcher,
                                       ?NAME_MYSQL, 200) of
        {ok, Pid2} ->
            Pid2;
        {error, _} ->
            undefined
    end,
    PidMemcached = case cloudi_job:get_pid(Dispatcher,
                                           ?NAME_MEMCACHED, 200) of
        {ok, Pid3} ->
            Pid3;
        {error, _} ->
            undefined
    end,
    PidTokyotyrant = case cloudi_job:get_pid(Dispatcher,
                                             ?NAME_TOKYOTYRANT, 200) of
        {ok, Pid4} ->
            Pid4;
        {error, _} ->
            undefined
    end,
    PidCouchdb = case cloudi_job:get_pid(Dispatcher,
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
            cloudi_job:send_async(Dispatcher, ?NAME_PGSQL, SQLDrop,
                                  TimeoutAsync, PidPgsql),
            cloudi_job:send_async(Dispatcher, ?NAME_PGSQL, SQLCreate,
                                  TimeoutAsync, PidPgsql);
        true ->
            ok
    end,
    if
        PidMysql /= undefined ->
            cloudi_job:send_async(Dispatcher, ?NAME_MYSQL, SQLDrop,
                                  TimeoutAsync, PidMysql),
            cloudi_job:send_async(Dispatcher, ?NAME_MYSQL, SQLCreate,
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

send_task(Name,
          #state{tasks_failed = [#task{task_size = TaskSize,
                                       iterations = Iterations,
                                       step = Step,
                                       index = Index,
                                       timeout = Timeout} | Failed]} = State,
          Dispatcher) ->
    case cloudi_job:get_pid(Dispatcher, Name) of
        {ok, Pid} ->
            ?LOG_INFO("~p iterations starting at digit ~p (retry)~n",
                      [Iterations, Index]),
            % define the task
            IndexStr = erlang:integer_to_list(Index),
            IndexBin = erlang:list_to_binary(IndexStr),
            Request = <<Iterations:32/unsigned-integer-native,
                        Step:32/unsigned-integer-native,
                        IndexBin/binary, 0>>,
            case cloudi_job:send_async(Dispatcher, Name, Request,
                                       Timeout, Pid) of
                {ok, TransId} ->
                    {next, TaskSize, Iterations, Step, Index,
                     Pid, Timeout, TransId,
                     State#state{tasks_failed = Failed}};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end;

send_task(Name,
          #state{index = Index,
                 index_end = IndexEnd,
                 tasks_failed = [],
                 step = Step,
                 target_time = TargetTime,
                 task_size_initial = TaskSizeInitial,
                 task_size_lookup = TaskSizeLookup} = State,
          Dispatcher) ->
    case cloudi_job:get_pid(Dispatcher, Name) of
        {ok, Pid} ->
            TaskSize = cloudi_task_size:get(TaskSizeInitial, Pid,
                                            TaskSizeLookup),
            Iterations = math2:ceil(TaskSize * ?MAX_ITERATIONS),
            % determine the size of the task and take the ceiling of the value
            % (to avoid iterations of 0)
            ?LOG_INFO("~p iterations starting at digit ~p~n",
                      [Iterations, Index]),
            % define the task
            IndexStr = erlang:integer_to_list(Index),
            IndexBin = erlang:list_to_binary(IndexStr),
            Request = <<Iterations:32/unsigned-integer-native,
                        Step:32/unsigned-integer-native,
                        IndexBin/binary, 0>>,
            Timeout = math2:ceil(TargetTime * 3600000.0) * 10,
            case cloudi_job:send_async(Dispatcher, Name, Request,
                                       Timeout, Pid) of
                {ok, TransId} ->
                    NewIndex = Index + Step * Iterations,
                    if
                        NewIndex =< IndexEnd ->
                            {next, TaskSize, Iterations, Step, Index,
                             Pid, Timeout, TransId,
                             State#state{index = NewIndex}};
                        true ->
                            {done, TaskSize, Iterations, Step, Index,
                             Pid, Timeout, TransId,
                             State}
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

send_tasks(0, L, State, _) ->
    {lists:reverse(L), false, State};

send_tasks(Count, L, State, Dispatcher) ->
    Name = "/tests/hexpi",
    case send_task(Name, State, Dispatcher) of
        {next, TaskSize, Iterations, Step, Index,
         Pid, Timeout, TransId, NewState} ->
            send_tasks(Count - 1,
                       [{TaskSize, Iterations, Step, Index,
                         Pid, Timeout, TransId} | L],
                       NewState, Dispatcher);
        {done, TaskSize, Iterations, Step, Index,
         Pid, Timeout, TransId, NewState} ->
            {[{TaskSize, Iterations, Step, Index,
               Pid, Timeout, TransId} | L], true, NewState};
        {error, Reason} ->
            ?LOG_ERROR("send_task error ~p~n", [Reason]),
            {L, false, State}
    end.

recv_tasks([], Done, State, _) ->
    if
        Done == false ->
            self() ! task;
        true ->
            ok
    end,
    State;

recv_tasks([{TaskSize, Iterations, Step, Index, Pid, Timeout, TransId} | L],
           Done,
           #state{tasks_failed = Failed,
                  target_time = TargetTime,
                  task_size_lookup = TaskSizeLookup} = State, Dispatcher) ->

    case cloudi_job:recv_async(Dispatcher, Timeout, TransId) of
        {ok, Response} ->
            <<ElapsedTime:32/float-native, PiResult/binary>> = Response,
            send_results(Index, PiResult, Pid, State, Dispatcher),
            NewTaskSizeLookup = cloudi_task_size:put(TaskSize,
                                                     TargetTime,
                                                     ElapsedTime,
                                                     Pid,
                                                     TaskSizeLookup),
            NextState = State#state{task_size_lookup = NewTaskSizeLookup},
            if
                Done == false ->
                    {Task, NewDone, FinalState} = send_tasks(1, [], NextState,
                                                             Dispatcher),
                    recv_tasks(L ++ Task, NewDone, FinalState, Dispatcher);
                true ->
                    recv_tasks(L, Done, NextState, Dispatcher)
            end;
        {error, timeout} ->
            ?LOG_ERROR("index ~p result timeout (after ~p ms)~n",
                       [Index, Timeout]),
            % exaggerate the elapsed timeout, to reduce the task size 
            ElapsedTime = (Timeout * 4) / 3600000.0,
            Task = #task{task_size = TaskSize,
                         iterations = Iterations,
                         step = Step,
                         index = Index,
                         timeout = Timeout * 2},
            NewTaskSizeLookup = cloudi_task_size:put(TaskSize,
                                                     TargetTime,
                                                     ElapsedTime,
                                                     Pid,
                                                     TaskSizeLookup),
            recv_tasks(L, Done,
                       State#state{tasks_failed = [Task | Failed],
                                   task_size_lookup = NewTaskSizeLookup},
                       Dispatcher);
        {error, Reason} ->
            ?LOG_ERROR("recv_async error ~p~n", [Reason]),
            recv_tasks(L, Done, State, Dispatcher)
    end.

send_results(DigitIndex, PiResult, Pid,
             #state{use_pgsql = UsePgsql,
                    use_mysql = UseMysql,
                    use_memcached = UseMemcached,
                    use_tokyotyrant = UseTokyotyrant,
                    use_couchdb = UseCouchdb}, Dispatcher) ->
    SQLInsert = sql_insert(DigitIndex, PiResult),
    if
        UsePgsql == true ->
            cloudi_job:send_async(Dispatcher, ?NAME_PGSQL, SQLInsert);
        true ->
            ok
    end,
    if
        UseMysql == true ->
            cloudi_job:send_async(Dispatcher, ?NAME_MYSQL, SQLInsert);
        true ->
            ok
    end,
    if
        UseMemcached == true ->
            cloudi_job:send_async(Dispatcher, ?NAME_MEMCACHED,
                                  memcached(DigitIndex, PiResult));
        true ->
            ok
    end,
    if
        UseTokyotyrant == true ->
            cloudi_job:send_async(Dispatcher, ?NAME_TOKYOTYRANT,
                                  tokyotyrant(DigitIndex, PiResult));
        true ->
            ok
    end,
    if
        UseCouchdb == true ->
            cloudi_job:send_async(Dispatcher, ?NAME_COUCHDB,
                                  couchdb(DigitIndex, Pid));
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
    list_to_binary(string2:format("INSERT INTO incoming_results "
                                  "(digit_index, data) VALUES (~w, '~s');",
                                  [DigitIndex, PiResult])).

memcached(DigitIndex, PiResult) ->
    string2:format("{set, \"~w\", <<\"~s\">>}",
                   [DigitIndex, PiResult]).

tokyotyrant(DigitIndex, PiResult) ->
    string2:format("{put, \"~w\", <<\"~s\">>}",
                   [DigitIndex, PiResult]).

couchdb(DigitIndex, Pid) ->
    string2:format("{update_document, \"pi_state\","
                   " [{<<\"~s\", <<\"~w\">>}]}",
                   [erlang:pid_to_list(Pid), DigitIndex]).

