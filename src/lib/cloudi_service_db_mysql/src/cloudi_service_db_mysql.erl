%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI MySQL Data Module==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2009-2013, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2009-2013 Michael Truog
%%% @version 1.3.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_db_mysql).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_service).

%% external interface
-export([equery/4, prepare_query/4, execute_query/4, squery/3]).

%% cloudi_service callbacks
-export([cloudi_service_init/3,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/2]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-define(DEFAULT_HOST_NAME,       "127.0.0.1").
-define(DEFAULT_USER_NAME,          "cloudi").
-define(DEFAULT_PASSWORD,                 "").
-define(DEFAULT_PORT,                   3306).
-define(DEFAULT_ENCODING,               utf8).
-define(DEFAULT_TIMEOUT,               20000). % ms

-record(state,
    {
        process,
        prepared_queries = dict:new()
    }).

-record(mysql_result, {fieldinfo = [], rows = [], affectedrows = 0,
                       insert_id = 0, server_status = 0, warning_count = 0,
                       message = "", error = "" }).

-type dispatcher() :: cloudi_service:dispatcher() | cloudi:context().

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Perform an extended SQL query that does string substitution with '?'s.===
%% @end
%%-------------------------------------------------------------------------

-spec equery(Dispatcher :: dispatcher(),
             Name :: string(),
             String :: string(),
             Parameters :: list()) ->
    {'ok', #mysql_result{}} |
    {'error', any()}.

equery(Dispatcher, Name, String, Parameters)
    when is_list(Name),
         is_list(String), is_list(Parameters) ->
    cloudi:send_sync(Dispatcher, Name,
                     {equery, String, Parameters}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Prepare a query with '?'s for dynamic values.===
%% The query can be used in the future with the supplied Identifier atom.
%% @end
%%-------------------------------------------------------------------------

-spec prepare_query(Dispatcher :: dispatcher(),
                    Name :: string(),
                    Identifier :: atom(),
                    String :: string()) ->
    {'ok', #mysql_result{}} |
    {'error', any()}.

prepare_query(Dispatcher, Name, Identifier, String)
    when is_list(Name),
         is_atom(Identifier), is_list(String) ->
    cloudi:send_sync(Dispatcher, Name,
                     {prepare, Identifier, String}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Execute a query that has already been prepared.===
%% @end
%%-------------------------------------------------------------------------

-spec execute_query(Dispatcher :: dispatcher(),
                    Name :: string(),
                    Identifier :: atom(),
                    Arguments :: list()) ->
    {'ok', #mysql_result{}} |
    {'error', any()}.

execute_query(Dispatcher, Name, Identifier, Arguments)
    when is_list(Name),
         is_atom(Identifier), is_list(Arguments) ->
    cloudi:send_sync(Dispatcher, Name,
                     {execute, Identifier, Arguments}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Perform a simple SQL string query.===
%% @end
%%-------------------------------------------------------------------------

-spec squery(Dispatcher :: dispatcher(),
             Name :: string(),
             String :: string()) ->
    {'ok', #mysql_result{}} |
    {'error', any()}.

squery(Dispatcher, Name, String)
    when is_list(Name), is_list(String) ->
    cloudi:send_sync(Dispatcher, Name,
                     {squery, String}).

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, _Prefix, Dispatcher) ->
    Defaults = [
        {hostname, ?DEFAULT_HOST_NAME},
        {username, ?DEFAULT_USER_NAME},
        {password, ?DEFAULT_PASSWORD},
        {port,     ?DEFAULT_PORT},
        {encoding, ?DEFAULT_ENCODING},
        {database, undefined}],
    [HostName, UserName, Password, Port, Encoding, Database] =
        cloudi_proplists:take_values(Defaults, Args),
    true = is_list(Database),
    try cloudi_x_mysql_conn:start(HostName, Port, UserName, Password,
                         Database, Encoding, undefined) of
        {ok, Process} ->
            cloudi_service:subscribe(Dispatcher, Database),
            {ok, #state{process = Process}};
        {error, Reason} ->
            {stop, Reason}
    catch
        _:Reason ->
            {stop, Reason}
    end.

cloudi_service_handle_request(_Type, _Name, _Pattern, _RequestInfo, Request,
                          Timeout, _Priority, _TransId, _Pid,
                          #state{process = Process,
                                 prepared_queries = Queries} = State,
                          _Dispatcher) ->
    case Request of
        {equery, String, Parameters} ->
            Result = cloudi_x_mysql_conn:execute(Process, none, 1, Parameters, self(),
                                        list_to_binary(String), Timeout),
            {reply, response_internal(Result, Request), State};
        {prepare, Identifier, String} ->
            {reply, ok,
             State#state{prepared_queries = dict:update(Identifier,
                fun({Version, _}) ->
                    {Version + 1, String}
                end, {1, String}, Queries)}};
        {execute, Identifier, Arguments} ->
            case dict:find(Identifier, Queries) of
                error ->
                    {reply, {error, "not prepared"}, State};
                {ok, {Version, String}} ->
                    Result = cloudi_x_mysql_conn:execute(Process, Identifier, Version,
                                                Arguments, self(),
                                                list_to_binary(String),
                                                Timeout),
                    {reply, response_internal(Result, Request), State}
            end;
        {squery, String} ->
            Result = cloudi_x_mysql_conn:fetch(Process, list_to_binary(String),
                                      self(), Timeout),
            {reply, response_internal(Result, Request), State};
        [String | _] = QueryList when is_list(String) ->
            {reply, do_queries_in_transaction(QueryList, Timeout,
                                              Process), State};
        String when is_list(String) ->
            Result = cloudi_x_mysql_conn:fetch(Process, list_to_binary(String),
                                      self(), Timeout),
            {reply, response_internal(Result, Request), State};
        String when is_binary(String) ->
            Result = cloudi_x_mysql_conn:fetch(Process, String, self(), Timeout),
            {reply, response_external(Result, Request), State}
    end.

cloudi_service_handle_info(Request, State, _) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(_, undefined) ->
    ok;
cloudi_service_terminate(_, #state{process = Process}) ->
    cloudi_x_mysql_conn:close_socket(Process),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

response_internal({data, Result}, _) ->
    {ok, Result};
response_internal({update, Result}, _) ->
    {ok, Result};
response_internal({error, _} = Error, _) ->
    Error.

response_external({data, Result}, Input) ->
    cloudi_response:new(Input, cloudi_string:term_to_binary(Result));
response_external({update, Result}, Input) ->
    cloudi_response:new(Input, cloudi_string:term_to_binary(Result));
response_external({error, _} = Error, Input) ->
    cloudi_response:new(Input, Error).

%% wrap the cloudi_x_mysql string query interface
mysql_complete_query(Pid, Queries, From, Timeout) ->
    case cloudi_x_mysql_conn:fetch(Pid, Queries, From, Timeout) of
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
do_queries_in_transaction(QueryList, Timeout, Process)
    when is_list(QueryList) ->
    case with_transaction(Process, QueryList, self(), Timeout) of
        {rollback, Why} ->
            ?LOG_ERROR("exception caused rollback: ~p", [Why]),
            {error, Why};
        ok ->
            ok
    end.

