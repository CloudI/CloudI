%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI PostgreSQL Data Module==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2009-2011, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2009-2011 Michael Truog
%%% @version 0.1.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_job_db_pgsql).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_job).

%% external interface
-export([equery/4, equery/5,
         squery/3, squery/4]).

%% cloudi_job callbacks
-export([cloudi_job_init/2,
         cloudi_job_handle_request/8,
         cloudi_job_handle_info/3,
         cloudi_job_terminate/2]).

-include("cloudi_logger.hrl").

-define(DEFAULT_HOST_NAME, "127.0.0.1").
-define(DEFAULT_USER_NAME, "cloudi").
-define(DEFAULT_PASSWORD,  "").
-define(DEFAULT_PORT,      5432).
-define(DEFAULT_TIMEOUT,   20000). % 20 seconds

-record(state,
    {
        connection
    }).

% from external/epgsql/include/pgsql.hrl 
%-record(column,    {name, type, size, modifier, format}).
%-record(statement, {name, columns, types}).
-record(error,     {severity, code, message, extra}).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Form an internal request for an extended SQL query that does string substitution with "?"s.===
%% @end
%%-------------------------------------------------------------------------

equery(Dispatcher, Name, String, Parameters)
    when is_pid(Dispatcher), is_list(Name),
         is_list(String), is_list(Parameters) ->
    cloudi_job:send_sync(Dispatcher, Name,
                         {equery_argument_parse(String), Parameters}).

equery(Dispatcher, Name, String, Parameters, Timeout)
    when is_pid(Dispatcher), is_list(Name),
         is_list(String), is_list(Parameters), is_integer(Timeout) ->
    cloudi_job:send_sync(Dispatcher, Name,
                         {equery_argument_parse(String), Parameters}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Form an internal request for a simple SQL string query.===
%% @end
%%-------------------------------------------------------------------------

squery(Dispatcher, Name, String)
    when is_pid(Dispatcher), is_list(Name),
         is_list(String) ->
    cloudi_job:send_sync(Dispatcher, Name, String).

squery(Dispatcher, Name, String, Timeout)
    when is_pid(Dispatcher), is_list(Name),
         is_list(String), is_integer(Timeout) ->
    cloudi_job:send_sync(Dispatcher, Name, String, Timeout).

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_job
%%%------------------------------------------------------------------------

cloudi_job_init(Args, Dispatcher) ->
    Defaults = [
        {hostname, ?DEFAULT_HOST_NAME},
        {username, ?DEFAULT_USER_NAME},
        {password, ?DEFAULT_PASSWORD},
        {port, ?DEFAULT_PORT},
        {timeout, ?DEFAULT_TIMEOUT},
        {database, undefined}],
    [HostName, UserName, Password, Port, Timeout, Database | NewArgs] =
        proplists2:take_values(Defaults, Args),
    true = is_list(Database),
    FinalArgs = [{port, Port},
                 {timeout, Timeout},
                 {database, Database} | NewArgs],
    case pgsql:connect(HostName, UserName, Password, FinalArgs) of
        {ok, Connection} ->
            cloudi_job:subscribe(Dispatcher, Database),
            {ok, #state{connection = Connection}};
        {error, Reason} ->
            {stop, Reason}
    end.

cloudi_job_handle_request(_Type, _Name, Request, _Timeout, TransId, _Pid,
                          #state{connection = Connection} = State,
                          _Dispatcher) ->
    case Request of
        {String, Parameters} when is_list(String), is_list(Parameters) ->
            case pgsql:equery(Connection, String, Parameters) of
                {ok, _} = Response ->
                    {reply, response_internal(Response), State};
                {ok, _, _} = Response ->
                    {reply, response_internal(Response), State};
                {ok, _, _, _} = Response ->
                    {reply, response_internal(Response), State};
                {error, _} = Response ->
                    {reply, response_internal(Response), State}
            end;
        [String | _] = QueryList when is_list(String) ->
            case do_queries_in_transaction(QueryList, Connection) of
                ok ->
                    {reply, ok, State};
                {error, _} = Error ->
                    {reply, Error, State}
            end;
        String when is_binary(String) ->
            case pgsql:squery(Connection, String) of
                {ok, _} = Response ->
                    {reply, response_external(Response), State};
                {ok, _, _} = Response ->
                    {reply, response_external(Response), State};
                {ok, _, _, _} = Response ->
                    {reply, response_external(Response), State};
                {error, Reason} = Response ->
                    ?LOG_ERROR("request ~p error ~p",
                               [TransId, Reason#error.message]),
                    {reply, response_external(Response), State}
            end;
        String when is_list(String) ->
            case pgsql:squery(Connection, String) of
                {ok, _} = Response ->
                    {reply, response_internal(Response), State};
                {ok, _, _} = Response ->
                    {reply, response_internal(Response), State};
                {ok, _, _, _} = Response ->
                    {reply, response_internal(Response), State};
                {error, _} = Response ->
                    {reply, response_internal(Response), State}
            end
    end.

cloudi_job_handle_info(Request, State, _) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_job_terminate(_, #state{connection = Connection}) ->
    pgsql:close(Connection),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

response_internal({ok, _I} = Response) ->
    Response;
response_internal({ok, _Columns, _Data} = Response) ->
    Response;
response_internal({ok, _I, _Columns, _Data} = Response) ->
    Response;
response_internal({error, Reason}) ->
    {error, Reason#error.message}.

% XXX change the response format for external/binary

response_external({ok, I}) ->
    % SQL UPDATE
    <<I:32/unsigned-integer-native>>;
response_external({ok, _Columns, _Data}) ->
    % SQL SELECT
    <<0:32>>;
response_external({ok, I, _Columns, _Data}) ->
    % SQL INSERT
    <<I:32/unsigned-integer-native>>;
response_external({error, _}) ->
    <<>>.

%% do a single query and return a boolean to determine if the query succeeded
do_query(Query, Connection) when is_list(Query) ->
    do_query(list_to_binary(Query), Connection);

do_query(Query, Connection) ->
    case pgsql:squery(Connection, Query) of
        {ok, _} ->
            true;
        {ok, _, _} ->
            true;
        {ok, _, _, _} ->
            true;
        {error, Reason} ->
            % cause a transaction rollback
            % (since a partial success can not be tolerated)
            throw(Reason#error.message)
    end.

%% do all queries in the list until an error is encountered
%% return the remaining list if there is an error, else an empty list
do_queries_in_transaction(SQLQueryList, Connection)
    when is_list(SQLQueryList) ->
    case pgsql:with_transaction(Connection, fun(C) ->
            lists:dropwhile(fun(Query) -> do_query(Query, C) end, SQLQueryList)
        end) of
        [] ->
            ok;
        {rollback, Reason} ->
            {error, Reason}
    end.

%% provide the "?"s parameter syntax externally like mysql, but provide the
%% $1, $2, $3, etc. PostgreSQL parameter syntax internally to epgsql,
%% as required.
equery_argument_parse(String) ->
    equery_argument_parse_get([], 1, String).

-define(SPACE,   32).
-define(QUOTE,   39). % '
-define(PERCENT, 37). % %
-define(SYMMETRIC_OPERATOR_GUARD(P),
        P == $=; P == $>; P == $<;
        P == $+; P == $-; P == $*;
        P == $/; P == ?PERCENT).

%% handle spaces and normal punctuation when performing
%% parameter syntax substitution
equery_argument_parse_put(NewString, ?QUOTE, $?, ?QUOTE,
                          false, Index, Remaining) ->
    equery_argument_parse_get(
        NewString ++ "'$" ++ integer_to_list(Index) ++ "'",
        Index + 1, Remaining);
equery_argument_parse_put(NewString, ?SPACE, $?, ?SPACE,
                          false, Index, Remaining) ->
    equery_argument_parse_get(
        NewString ++ " $" ++ integer_to_list(Index) ++ " ",
        Index + 1, Remaining);
equery_argument_parse_put(NewString, ?SPACE, $?, $),
                          false, Index, Remaining) ->
    equery_argument_parse_get(
        NewString ++ " $" ++ integer_to_list(Index) ++ ")",
        Index + 1, Remaining);
equery_argument_parse_put(NewString, ?SPACE, $?, $,,
                          false, Index, Remaining) ->
    equery_argument_parse_get(
        NewString ++ " $" ++ integer_to_list(Index) ++ ",",
        Index + 1, Remaining);
equery_argument_parse_put(NewString, ?SPACE, $?, $;,
                          false, Index, Remaining) ->
    equery_argument_parse_get(
        NewString ++ " $" ++ integer_to_list(Index) ++ ";",
        Index + 1, Remaining);
equery_argument_parse_put(NewString, $,, $?, ?SPACE,
                          false, Index, Remaining) ->
    equery_argument_parse_get(
        NewString ++ ",$" ++ integer_to_list(Index) ++ " ",
        Index + 1, Remaining);
equery_argument_parse_put(NewString, $(, $?, ?SPACE,
                          false, Index, Remaining) ->
    equery_argument_parse_get(
        NewString ++ "($" ++ integer_to_list(Index) ++ " ",
        Index + 1, Remaining);
%% handle expression operators lacking separation spaces when
%% performing parameter syntax substitution
equery_argument_parse_put(NewString, Left, $?, ?SPACE,
                          false, Index, Remaining)
    when ?SYMMETRIC_OPERATOR_GUARD(Left);
         Left == $D; Left == $R ->
    equery_argument_parse_get(
        NewString ++ Left ++ "$" ++ integer_to_list(Index) ++ " ",
        Index + 1, Remaining);
equery_argument_parse_put(NewString, Left, $?, $),
                          false, Index, Remaining)
    when ?SYMMETRIC_OPERATOR_GUARD(Left);
         Left == $D; Left == $R ->
    equery_argument_parse_get(
        NewString ++ Left ++ "$" ++ integer_to_list(Index) ++ ")",
        Index + 1, Remaining);
equery_argument_parse_put(NewString, ?SPACE, $?, Right,
                          false, Index, Remaining)
    when ?SYMMETRIC_OPERATOR_GUARD(Right);
         Right == $A; Right == $O; Right == $! ->
    equery_argument_parse_get(
        NewString ++ " $" ++ integer_to_list(Index) ++ Right,
        Index + 1, Remaining);
equery_argument_parse_put(NewString, $(, $?, Right,
                          false, Index, Remaining)
    when ?SYMMETRIC_OPERATOR_GUARD(Right);
         Right == $A; Right == $O; Right == $! ->
    equery_argument_parse_get(
        NewString ++ "($" ++ integer_to_list(Index) ++ Right,
        Index + 1, Remaining);
%% tail recursion termination case
equery_argument_parse_put(NewString, C1, C2, C3,
                          _, _, []) ->
    NewString ++ [C1, C2, C3];
%% keep track of quoted strings, so that they are not parsed
%% for parameter syntax substitution
equery_argument_parse_put(NewString, C1, C2, C3,
                          Quoted, Index, [C4 | Remaining]) ->
    NewQuoted = if
        C1 == ?QUOTE, Quoted ->
            false;
        C1 == ?QUOTE ->
            true;
        true ->
            Quoted
    end,
    equery_argument_parse_put(
        NewString ++ [C1], C2, C3, C4, NewQuoted, Index, Remaining).
%% get more characters for parameter syntax substitution parsing
equery_argument_parse_get(NewString, _, []) ->
    NewString;
equery_argument_parse_get(NewString, Index, Remaining) ->
    [C1 | Remaining1] = Remaining,
    if
        Remaining1 == [] ->
            NewString ++ [C1];
        true ->
            [C2 | Remaining2] = Remaining1,
            if
                Remaining2 == [] ->
                    NewString ++ [C1, C2];
                true ->
                    [C3 | Remaining3] = Remaining2,
                    equery_argument_parse_put(
                        NewString, C1, C2, C3, false, Index, Remaining3)
            end
    end.

