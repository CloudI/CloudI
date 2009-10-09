%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Cloudi MySQL Data Module==
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
%%% @version 0.0.5 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloud_data_mysql).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).
-behaviour(cloud_data_interface).

%% external interface
-export([equery/3, prepare_query/3, execute_query/3, squery/2]).
%% do_queries_group/5 interface
-export([do_queries_in_transaction/2]).

%% cloud_data_interface callbacks
-export([start_link/2, handle_stop/1, handle_do_queries/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("cloud_logger.hrl").
-include("mysql.hrl").

-define(MYSQL_TIMEOUT, 20000). % 20 seconds

-define(DEFAULT_HOST_NAME, "127.0.0.1").
-define(DEFAULT_USER_NAME, "cloudi").
-define(DEFAULT_PASSWORD,  "").
-define(DEFAULT_PORT,      3306).
-define(DEFAULT_ENCODING,  utf8).

-record(state,
    {
    data_title = undefined,
    process = undefined,
    prepared_queries = rbdict:new(),
    timeout = undefined}).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Perform an extended SQL query that does string substitution with "?"s.===
%% @end
%%-------------------------------------------------------------------------

-spec equery(DataTitle :: atom(),
             String :: string(),
             Parameters :: list()) ->
    {'ok', #mysql_result{}} |
    {'error', any()}.

equery(DataTitle, String, Parameters)
    when is_atom(DataTitle), is_list(String), is_list(Parameters) ->
    % depend on MYSQL_TIMEOUT for a database communication timeout
    gen_server:call(DataTitle, {equery, String, Parameters}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Prepare a query with "?"s for dynamic values.===
%% The query can be used in the future with the supplied Identifier atom.
%% @end
%%-------------------------------------------------------------------------

-spec prepare_query(DataTitle :: atom(),
                    Identifier :: atom(),
                    String :: string()) ->
    {'ok', #mysql_result{}} |
    {'error', any()}.

prepare_query(DataTitle, Identifier, String)
    when is_atom(DataTitle), is_atom(Identifier), is_list(String) ->
    % depend on MYSQL_TIMEOUT for a database communication timeout
    gen_server:call(DataTitle, {prepare, Identifier, String}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Execute a query that has already been prepared.===
%% @end
%%-------------------------------------------------------------------------

-spec execute_query(DataTitle :: atom(),
                    Identifier :: atom(),
                    Arguments :: list()) ->
    {'ok', #mysql_result{}} |
    {'error', any()}.

execute_query(DataTitle, Identifier, Arguments)
    when is_atom(DataTitle), is_atom(Identifier), is_list(Arguments) ->
    % depend on MYSQL_TIMEOUT for a database communication timeout
    gen_server:call(DataTitle, {execute, Identifier, Arguments}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Perform a simple SQL string query.===
%% @end
%%-------------------------------------------------------------------------

-spec squery(DataTitle :: atom(),
             String :: string()) ->
    {'ok', #mysql_result{}} |
    {'error', any()}.

squery(DataTitle, String)
    when is_atom(DataTitle), is_list(String) ->
    % depend on MYSQL_TIMEOUT for a database communication timeout
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
    % depend on MYSQL_TIMEOUT for a database communication timeout
    gen_server:call(DataTitle, {do_queries, QueryList}, infinity).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([DataTitle, Arguments]) when is_atom(DataTitle), is_list(Arguments) ->
    init_state(DataTitle, Arguments).
handle_call({equery, String, Arguments}, _,
            #state{process = Process,
                   timeout = Timeout} = State) ->
    {reply,
     case mysql_conn:execute(Process, none, 1, Arguments,
                             self(), list_to_binary(String), Timeout) of
        {data, Result} ->
            {ok, Result};
        {updated, Result} ->
            {ok, Result};
        {error, _} = Error ->
            Error
     end, State};
handle_call({prepare, Identifier, String}, _,
            #state{prepared_queries = Queries} = State) ->
    {reply, ok, State#state{
        prepared_queries = rbdict:update(Identifier, fun({Version, _}) ->
            {Version + 1, String}
        end, {1, String}, Queries)}};
handle_call({execute, Identifier, Arguments}, _,
            #state{process = Process,
                   prepared_queries = Queries,
                   timeout = Timeout} = State) ->
    case rbdict:find(Identifier, Queries) of
        error ->
            {reply, {error, "not prepared"}, State};
        {ok, {Version, String}} ->
            {reply,
             case mysql_conn:execute(Process, Identifier, Version, Arguments,
                                     self(), list_to_binary(String), Timeout) of
                {data, Result} ->
                    {ok, Result};
                {updated, Result} ->
                    {ok, Result};
                {error, _} = Error ->
                    Error
             end, State}
    end;
handle_call({squery, String}, _,
            #state{process = Process,
                   timeout = Timeout} = State) ->
    {reply,
     case mysql_conn:fetch(Process, list_to_binary(String), self(), Timeout) of
        {data, Result} ->
            {ok, Result};
        {updated, Result} ->
            {ok, Result};
        {error, _} = Error ->
            Error
     end, State};
handle_call(stop, _,
            #state{data_title = DataTitle,
                   process = Process} = State) ->
    mysql_conn:close_socket(Process),
    {stop, atom_to_list(DataTitle) ++ " was requested to stop", State};
handle_call({do_queries, QueryList}, _,
            #state{data_title = DataTitle} = State) ->
    {reply, 
     cloud_data_interface:do_queries_group(QueryList,
        cloud_data_mysql, do_queries_in_transaction, State, DataTitle),
     State, hibernate};
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
terminate(_, #state{process = Process}) ->
    mysql_conn:close_socket(Process),
    ok.
code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

%% initialize the mysql client state
init_state(DataTitle, Args) when is_atom(DataTitle), is_list(Args) ->
    Defaults = [
        {hostname, ?DEFAULT_HOST_NAME},
        {username, ?DEFAULT_USER_NAME},
        {password, ?DEFAULT_PASSWORD},
        {port,     ?DEFAULT_PORT},
        {database, "will_not_be_used"},
        {encoding, ?DEFAULT_ENCODING},
        {timeout,  ?MYSQL_TIMEOUT}],
    [HostName, UserName, Password, Port, Database, Encoding, Timeout, []] =
        proplists_extensions:take_values(Defaults, Args),
    try mysql_conn:start(HostName, Port, UserName, Password,
                         Database, Encoding, DataTitle) of
        {ok, Process} ->
            {ok, #state{data_title = DataTitle,
                        process = Process,
                        timeout = Timeout}};
        {error, Reason} ->
            {stop, Reason}
    catch
        _:Reason ->
            {stop, Reason}
    end.

%% wrap the mysql string query interface
mysql_complete_query(Pid, Queries, From, Timeout) ->
    case mysql_conn:fetch(Pid, Queries, From, Timeout) of
        {data, Result} ->
            {ok, Result};
        {updated, Result} ->
            {ok, Result};
        {error, Result} ->
            throw(Result)
    end.

with_transaction(Pid, Queries, From, Timeout)
    when is_pid(Pid), is_list(Queries), is_pid(From), is_integer(Timeout) ->
    try {ok, _} = mysql_complete_query(Pid, <<"BEGIN">>, From, Timeout),
        {ok, _} = mysql_complete_query(Pid, Queries, From, Timeout),
        {ok, _} = mysql_complete_query(Pid, <<"COMMIT">>, From, Timeout),
        ok
    catch
        _:Why ->
            mysql_complete_query(Pid, <<"ROLLBACK">>, From, Timeout),
            {rollback, Why}
    end.

%% do all queries in the list and only succeed if all queries succeed
do_queries_in_transaction(SQLQueryList,
                          #state{process = Process,
                                 timeout = Timeout})
    when is_list(SQLQueryList) ->
    case with_transaction(Process, SQLQueryList, self(), Timeout) of
        {rollback, Why} ->
            ?LOG_ERROR("exception caused rollback: ~p", [Why]),
            SQLQueryList;
        ok ->
            []
    end.

