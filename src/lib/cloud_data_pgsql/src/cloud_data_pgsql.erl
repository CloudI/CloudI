%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Cloudi Postgres Data Module==
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

-module(cloud_data_pgsql).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).
-behaviour(cloud_data_interface).

%% external interface
-export([equery/3, squery/2]).

%% cloud_data_interface callbacks
-export([start_link/2, handle_stop/1, handle_do_queries/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("cloud_logger.hrl").
-include("../../epgsql/include/pgsql.hrl").

-define(PGSQL_TIMEOUT, 20000). % 20 seconds

-define(DEFAULT_HOST_NAME, "127.0.0.1").
-define(DEFAULT_USER_NAME, "cloudi").
-define(DEFAULT_PASSWORD,  "").
-define(DEFAULT_PORT,      5432).

-record(state,
    {
    data_title = undefined,
    connection = undefined}).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Perform an extended SQL query that does string substitution with $1, $2, $3, etc.===
%% @end
%%-------------------------------------------------------------------------

-spec equery(DataTitle :: atom(),
             String :: string(),
             Parameters :: list()) ->
    {'ok', list(#column{}), list(tuple())} |
    {'ok', non_neg_integer()} |
    {'ok', non_neg_integer(), list(#column{}), list(tuple())} |
    {'error', any()}.

equery(DataTitle, String, Parameters) ->
    % depend on PGSQL_TIMEOUT for a database communication timeout
    gen_server:call(DataTitle, {equery, String, Parameters}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Perform a simple SQL string query.===
%% @end
%%-------------------------------------------------------------------------

-spec squery(DataTitle :: atom(),
             String :: string()) ->
    {'ok', list(#column{}), list(tuple())} |
    {'ok', non_neg_integer()} |
    {'ok', non_neg_integer(), list(#column{}), list(tuple())} |
    {'error', any()}.

squery(DataTitle, String) ->
    % depend on PGSQL_TIMEOUT for a database communication timeout
    gen_server:call(DataTitle, {squery, String}, infinity).

%%%------------------------------------------------------------------------
%%% Callback functions from cloud_data_interface
%%%------------------------------------------------------------------------

-spec start_link(DataTitle :: atom(),
                 Arguments :: list({atom(), any()})) ->
    {'ok', pid()} |
    {'error', any()}.

start_link(DataTitle, Arguments) when is_atom(DataTitle), is_list(Arguments) ->
    gen_server:start_link({local, DataTitle}, ?MODULE,
        [DataTitle, Arguments], []).

-spec handle_stop(DataTitle :: atom()) -> any().

handle_stop(DataTitle) when is_atom(DataTitle) ->
    gen_server:call(DataTitle, stop).

-spec handle_do_queries(DataTitle :: atom(),
                        QueryList :: list({atom(), string()})) ->
    {'ok', list({atom(), string()})} |
    {'error', list({atom(), string()})}.

handle_do_queries(DataTitle, QueryList)
    when is_atom(DataTitle), is_list(QueryList) ->
    % depend on PGSQL_TIMEOUT for a database communication timeout
    gen_server:call(DataTitle, {do_queries, QueryList}, infinity).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([DataTitle, Arguments]) when is_atom(DataTitle), is_list(Arguments) ->
    init_state(DataTitle, Arguments).
handle_call({equery, String, Parameters}, _,
            #state{connection = Connection} = State) ->
    {reply, pgsql:equery(Connection, String, Parameters), State};
handle_call({squery, String}, _,
            #state{connection = Connection} = State) ->
    {reply, pgsql:squery(Connection, String), State};
handle_call(stop, _,
            #state{data_title = DataTitle,
                   connection = Connection} = State) ->
    pgsql:close(Connection),
    {stop, atom_to_list(DataTitle) ++ " was requested to stop", State};
handle_call({do_queries, QueryList}, _,
            #state{data_title = DataTitle,
                   connection = Connection} = State) ->
    Response = do_queries_group(QueryList, Connection, DataTitle),
    {reply, Response, State, hibernate};
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
terminate(_, #state{connection = Connection}) ->
    pgsql:close(Connection),
    ok.
code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

%% initialize the pgsql client state
init_state(DataTitle, Arguments) when is_atom(DataTitle), is_list(Arguments) ->
    {HostName, Args1} = case lists:keytake(hostname, 1, Arguments) of
        false ->
            {?DEFAULT_HOST_NAME, Arguments};
        {value, {hostname, H}, RemainingArgs1} when is_list(H) ->
            {H, RemainingArgs1}
    end,
    {UserName, Args2} = case lists:keytake(username, 1, Args1) of
        false ->
            {?DEFAULT_USER_NAME, Args1};
        {value, {username, U}, RemainingArgs2} when is_list(U) ->
            {U, RemainingArgs2}
    end,
    {Password, Args3} = case lists:keytake(password, 1, Args2) of
        false ->
            {?DEFAULT_PASSWORD, Args2};
        {value, {password, P1}, RemainingArgs3} when is_list(P1) ->
            {P1, RemainingArgs3}
    end,
    {Port, Args4} = case lists:keytake(port, 1, Args3) of
        false ->
            {?DEFAULT_PORT, Args3};
        {value, {port, P2}, RemainingArgs4} when is_integer(P2) ->
            {P2, RemainingArgs4}
    end,
    Args5 = case lists:keytake(timeout, 1, Args4) of
        false ->
            [{timeout, ?PGSQL_TIMEOUT} | Args4];
        {value, {timeout, Timeout}, RemainingArgs5}
        when Timeout < ?PGSQL_TIMEOUT ->
            % override any user specified timeout smaller than the default
            ?LOG_WARNING("overriding timeout, ~p is now ~p milliseconds",
                         [Timeout, ?PGSQL_TIMEOUT]),
            [{timeout, ?PGSQL_TIMEOUT} | RemainingArgs5];
        {value, _, _} ->
            Args4
    end,
    case pgsql:connect(HostName, UserName, Password, [{port, Port} | Args5]) of
        {ok, Connection} ->
            {ok, #state{data_title = DataTitle,
                        connection = Connection}};
        {error, Reason} ->
            {stop, Reason}
    end.

%% check the result of a query to determine if the query succeeded and
%% return a boolean based on whether or not the query succeeded
is_query_result_valid(_, [], _) ->
    true;

is_query_result_valid(I, [Result | ResultList], Query)
    when is_integer(I), is_tuple(Result) ->
    Success = is_query_result_valid(I, Result, Query),
    if
        Success ->
            is_query_result_valid(I + 1, ResultList, Query);
        true ->
            % cause a transaction rollback
            % (since a partial success can not be tolerated)
            throw("error in compound statement")
    end;

is_query_result_valid(I, Result, Query)
    when is_integer(I), is_tuple(Result) ->
    case Result of
        {ok, _} ->
            true;
        {ok, _, _} ->
            true;
        {ok, _, _, _} ->
            true;
        {error, Reason} ->
            if
                I > 1 ->
                    ?LOG_ERROR("query error: ~p, on command ~w of ~p",
                               [Reason, I, Query]);
                true ->
                    ?LOG_ERROR("query error: ~p, on ~p",
                               [Reason, Query])
            end,
            false
    end.

%% do a single query and return a boolean to determine if the query succeeded
do_query(Query, Connection) ->
    is_query_result_valid(1, pgsql:squery(Connection, Query), Query).

%% do all queries in the list until an error is encountered
%% return the remaining list if there is an error, else an empty list
do_queries_in_transaction(SQLQueryList, Connection)
    when is_list(SQLQueryList) ->
    case pgsql:with_transaction(Connection, fun(C) ->
            lists:dropwhile(fun(Query) -> do_query(Query, C) end, SQLQueryList)
        end) of
        {rollback, Why} ->
            ?LOG_ERROR("exception caused rollback: ~p", [Why]),
            SQLQueryList;
        Remaining when is_list(Remaining) ->
            Remaining
    end.

%% remove queries that were processed from the main QueryList
get_all_remaining_queries(RemainingQueryList,
                          Remaining, Remaining,
                          QueryList, _)
    when is_list(RemainingQueryList), is_list(Remaining), is_list(QueryList) ->
    % all the other processing queries failed
    % (first failure aborts the process of executing remaining queries)
    RemainingQueryList ++ QueryList;
get_all_remaining_queries(RemainingQueryList,
                          [SQLQuery | Processing], Remaining,
                          [{DataTitle, SQLQuery} | QueryList], DataTitle)
    when is_list(RemainingQueryList), is_list(Processing),
         is_list(Remaining), is_list(QueryList), is_atom(DataTitle) ->
    % a query that succeeded
    get_all_remaining_queries(RemainingQueryList,
                              Processing, Remaining,
                              QueryList, DataTitle);
get_all_remaining_queries(RemainingQueryList, Processing, Remaining,
                          [{_, _} = Query | QueryList], DataTitle)
    when is_list(RemainingQueryList), is_list(Processing),
         is_list(Remaining), is_list(QueryList), is_atom(DataTitle) ->
    % a query that is for a different module (different DataTitle)
    get_all_remaining_queries(RemainingQueryList ++ [Query],
                              Processing, Remaining,
                              QueryList, DataTitle).

%% do queries that are grouped based on the DataTitle to be processed
%% (only queries that contain the DataTitle can be processed in this module)
%% all the processed queries are removed from the QueryList
%% so that it may be processed in other data modules, if necessary
do_queries_group(QueryList, Connection, DataTitle)
    when is_list(QueryList), is_atom(DataTitle) ->
    do_queries_group([], QueryList, QueryList, Connection, DataTitle).

do_queries_group([], [], QueryList, _, _) when is_list(QueryList) ->
    {ok, QueryList}; % no queries to handle in this module
do_queries_group(Processing, [], QueryList, Connection, DataTitle)
    when is_list(Processing), is_list(QueryList), is_atom(DataTitle) ->
    case do_queries_in_transaction(Processing, Connection) of
        [] ->
            {ok, get_all_remaining_queries(
                [], Processing, [], QueryList, DataTitle)};
        Remaining when is_list(Remaining) ->
            {error, get_all_remaining_queries(
                [], Processing, Remaining, QueryList, DataTitle)}
    end;
do_queries_group(Processing, [{DataTitle, SQLQuery} | OtherQueries],
                 QueryList, Connection, DataTitle)
    when is_list(Processing), is_list(OtherQueries),
         is_list(QueryList), is_atom(DataTitle) ->
    do_queries_group(Processing ++ [SQLQuery], OtherQueries,
                     QueryList, Connection, DataTitle);
do_queries_group(Processing, [{_, _} | OtherQueries],
                 QueryList, Connection, DataTitle)
    when is_list(Processing), is_list(OtherQueries),
         is_list(QueryList), is_atom(DataTitle) ->
    do_queries_group(Processing, OtherQueries,
                     QueryList, Connection, DataTitle).

