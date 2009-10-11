%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Cloudi Work Module For Tests==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2009, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2009 Michael Truog
%%% @version 0.0.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloud_job_tests).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloud_work_interface).
-behaviour(gen_server).

%% external interface
-export([clear_all_results/0,
         clear_results/2]).

%% cloud_work_interface callbacks
-export([start_link/2,
         handle_stop/1,
         handle_get_initial_task_size/0,
         handle_get_task_time_target/0,
         handle_get_task/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("cloud_logger.hrl").

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
    index = undefined,
    index_start = undefined,
    index_end = undefined,
    step = ?PI_DIGIT_STEP_SIZE}).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Delete all previous results in the database.===
%% @end
%%-------------------------------------------------------------------------

-spec clear_all_results() -> 'ok'.

clear_all_results() ->
    cloud_data_mysql:squery('cloud_data_mysql.cloudi_tests',
        "DELETE FROM incoming_results_v3;"),
    {ok, _} = cloud_data_pgsql:squery('cloud_data_pgsql.cloudi_tests',
        "DELETE FROM incoming_results_v3;"),
    ok.

%%-------------------------------------------------------------------------
%% @doc
%% ===Delete a range of previous results in the database.===
%% @end
%%-------------------------------------------------------------------------

-spec clear_results(State :: pos_integer(), End :: pos_integer()) -> 'ok'.

clear_results(Start, End) when is_integer(Start), is_integer(End) ->
    cloud_data_mysql:equery('cloud_data_mysql.cloudi_tests',
        "DELETE FROM incoming_results_v3 "
        "WHERE digit_index >= ? AND digit_index <= ?;", [Start, End]),
    {ok, _} = cloud_data_pgsql:equery('cloud_data_pgsql.cloudi_tests',
        "DELETE FROM incoming_results_v3 "
        "WHERE digit_index >= ? AND digit_index <= ?;", [Start, End]),
    ok.

%%%------------------------------------------------------------------------
%%% Callback functions from cloud_work_interface
%%%------------------------------------------------------------------------

start_link(WorkInstance, [IndexStart, IndexEnd])
    when is_atom(WorkInstance), is_integer(IndexStart), is_integer(IndexEnd) ->
    case gen_server:start_link({local, WorkInstance}, ?MODULE,
        [IndexStart, IndexEnd], []) of
        {ok, _} ->
            ok;
        {error, {already_started, _}} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

handle_stop(WorkInstance) when is_atom(WorkInstance) ->
    gen_server:call(WorkInstance, stop).

% static work type parameter, does not use the WorkInstance
handle_get_initial_task_size() ->
    (1.0 / ?MAX_ITERATIONS). % percentage

% static work type parameter, does not use the WorkInstance
handle_get_task_time_target() ->
    (1.0 / 3600.0). % hours

handle_get_task(WorkInstance, SequenceNumber, TaskSize)
    when is_atom(WorkInstance), is_integer(SequenceNumber),
         is_float(TaskSize) ->
    Iterations = math_extensions:ceil(TaskSize * ?MAX_ITERATIONS),
    % allocate the task
    try gen_server:call(WorkInstance, {get_task, Iterations}) of
        {ok, Index, Step} ->
            % determine the size of the task and take the ceiling of the value
            % (to avoid iterations of 0)
            io:format("~p iterations starting at digit ~p~n",
                [Iterations, Index]),
            % define the task
            IndexStr = erlang:integer_to_list(Index),
            IndexBin = erlang:list_to_binary(IndexStr ++ [0]),
            {<<Iterations:32/unsigned-integer-native,
               Step:32/unsigned-integer-native,
               IndexBin/binary>>, []}
    catch
        exit:{noproc, _} ->
            % the work module has already stopped execution
            % but output still needs to be collected, so the work module
            % is still being queried by workers to determine if more work
            % exists
            {<<>>, []}
    end.

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([IndexStart, IndexEnd]) ->
    State = #state{
        index = IndexStart,
        index_start = IndexStart,
        index_end = IndexEnd},
    case setup_database() of
        ok ->
            case update_state_from_database(State) of
                {ok, _} = Result ->
                    cloud_data_mysql:equery('cloud_data_mysql.cloudi_tests',
                        "DELETE FROM incoming_results_v3 "
                        "WHERE digit_index >= ? AND digit_index <= ?;",
                        [State#state.index_start, State#state.index_end]),
                    Result;
                {error, Reason} ->
                    {stop, Reason}
            end;
        error ->
            {stop, "initialization of database failed"}
    end.

handle_call({get_task, Iterations}, _,
            #state{index = Index,
                   index_end = IndexEnd,
                   step = Step} = State) ->
    NewIndex = Index + Step * Iterations,
    if
        NewIndex > IndexEnd ->
            % all our work is done
            {stop, normal, {ok, Index, Step}, State};
        true ->
            {reply, {ok, Index, Step}, State#state{index = NewIndex}}
    end;
handle_call(stop, _, State) ->
    {stop, "stop requested", ok, State};
handle_call(Request, _, State) ->
    ?LOG_WARNING("Unknown call \"~p\"", [Request]),
    {stop, string_extensions:format("Unknown call \"~p\"", [Request]),
     error, State}.
handle_cast(Request, State) ->
    ?LOG_WARNING("Unknown cast \"~p\"", [Request]),
    {noreply, State}.
handle_info(Request, State) ->
    ?LOG_WARNING("Unknown info \"~p\"", [Request]),
    {noreply, State}.
terminate(_, _) ->
    ok.
code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

%% initialize tables in the database
setup_database() ->
    % queries for database initialization
    RequiredDatabaseQueries = [
        "DROP TABLE IF EXISTS incoming_results;",
        "DROP TABLE IF EXISTS incoming_results_v2;"],
    OptionalDatabaseQueries = [
        "CREATE TABLE incoming_results_v3 ("
        "digit_index   NUMERIC(30) PRIMARY KEY,"
        "data          TEXT"
        ");"],
    % execute setup queries
    RequiredResult = lists_extensions:iter(fun(Q, Itr) ->
        % mysql can be used, but is not required
        cloud_data_mysql:squery('cloud_data_mysql.cloudi_tests', Q),
        case cloud_data_pgsql:squery('cloud_data_pgsql.cloudi_tests', Q) of
            {ok, _} ->
                Itr();
            {ok, _, _} ->
                Itr();
            {ok, _, _, _} ->
                Itr();
            {error, _} = Error ->
                Error
        end
    end, ok, RequiredDatabaseQueries),
    if
        RequiredResult == ok ->
            lists:foreach(fun(Q) ->
                % mysql can be used, but is not required
                cloud_data_mysql:squery('cloud_data_mysql.cloudi_tests', Q),
                cloud_data_pgsql:squery('cloud_data_pgsql.cloudi_tests', Q)
            end, OptionalDatabaseQueries),
            ok;
        true ->
            error
    end.

%% recover from a previous crash of the same work title
%% based on the content in the database
update_state_from_database(#state{index_start = IndexStart,
                                  index_end = IndexEnd} = State)
    when is_record(State, state) ->
    % determine if old data can be used from an
    % older run of the same work title
    {ok, _, [ResultLimits]} = 
        cloud_data_pgsql:equery(
            'cloud_data_pgsql.cloudi_tests',
            "SELECT MIN(digit_index), MAX(digit_index) "
            "FROM incoming_results_v3 "
            "WHERE digit_index >= ? AND digit_index <= ?;",
            [IndexStart, IndexEnd]),
    case ResultLimits of
        {null, null} ->
            {ok, State};
        _ ->
            MinIndex = erlang:list_to_integer(
                erlang:binary_to_list(erlang:element(1, ResultLimits))),
            if
                MinIndex == IndexStart ->
                    MaxIndex = erlang:list_to_integer(
                        erlang:binary_to_list(erlang:element(2, ResultLimits))),
                    % assume [MinIndex..MaxIndex] are continuous
                    {ok, _, [Position]} =
                        cloud_data_pgsql:equery(
                            'cloud_data_pgsql.cloudi_tests',
                            "SELECT (? + CHARACTER_LENGTH(data)) "
                            "FROM incoming_results_v3 "
                            "WHERE digit_index = ?;",
                            [MaxIndex, MaxIndex]),
                    NewIndexStart = erlang:element(1, Position),
                    if
                        NewIndexStart >= IndexEnd ->
                            {error, "work is done"};
                        true ->
                            {ok, State#state{index = NewIndexStart,
                                             index_start = NewIndexStart}}
                    end;
                true ->
                    % results do not seem continuous
                    {ok, _} = cloud_data_pgsql:equery(
                        'cloud_data_pgsql.cloudi_tests',
                        "DELETE FROM incoming_results_v3 "
                        "WHERE digit_index >= ? AND digit_index <= ?;",
                        [IndexStart, IndexEnd]),
                    {ok, State}
            end
    end.

