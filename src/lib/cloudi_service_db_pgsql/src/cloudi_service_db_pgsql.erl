%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI PostgreSQL Data Module==
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
%%% @version 1.3.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_db_pgsql).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_service).

%% external interface
-export([equery/4, equery/5,
         squery/3, squery/4]).

%% cloudi_service callbacks
-export([cloudi_service_init/3,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/2]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-define(DEFAULT_HOST_NAME,            "127.0.0.1").
-define(DEFAULT_USER_NAME,               "cloudi").
-define(DEFAULT_PASSWORD,                      "").
-define(DEFAULT_PORT,                        5432).
-define(DEFAULT_LISTEN,                     false). % async epgsql option
                                                    % (LISTEN/NOTIFY)
-define(DEFAULT_TIMEOUT,                    20000). % 20 seconds
-define(DEFAULT_ENDIAN,                    native). % response binary
                                                    % integer sizes

-record(state,
    {
        connection,
        endian :: big | little | native
    }).

% from external/epgsql/include/cloudi_x_pgsql.hrl 
%-record(column,    {name, type, size, modifier, format}).
%-record(statement, {name, columns, types}).
-record(error,     {severity, code, message, extra}).

%-type dispatcher() :: cloudi_service:dispatcher() | cloudi:context().

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Form an internal request for an extended SQL query that does string substitution with '?'s.===
%% @end
%%-------------------------------------------------------------------------

equery(Dispatcher, Name, String, Parameters)
    when is_list(Name), (is_binary(String) orelse is_list(String)),
         is_list(Parameters) ->
    cloudi:send_sync(Dispatcher, Name,
                     {equery_argument_parse(String), Parameters}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Form an internal request for an extended SQL query that does string substitution with '?'s.===
%% @end
%%-------------------------------------------------------------------------

equery(Dispatcher, Name, String, Parameters, Timeout)
    when is_list(Name), (is_binary(String) orelse is_list(String)),
         is_list(Parameters), is_integer(Timeout) ->
    cloudi:send_sync(Dispatcher, Name,
                     {equery_argument_parse(String), Parameters}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Form an internal request for a simple SQL string query.===
%% @end
%%-------------------------------------------------------------------------

squery(Dispatcher, Name, String)
    when is_list(Name), (is_binary(String) orelse is_list(String)) ->
    cloudi:send_sync(Dispatcher, Name,
                     {String, []}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Form an internal request for a simple SQL string query.===
%% @end
%%-------------------------------------------------------------------------

squery(Dispatcher, Name, String, Timeout)
    when is_list(Name), (is_binary(String) orelse is_list(String)),
         is_integer(Timeout) ->
    cloudi:send_sync(Dispatcher, Name,
                     {String, []}, Timeout).

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, _Prefix, Dispatcher) ->
    Defaults = [
        {hostname,                 ?DEFAULT_HOST_NAME},
        {username,                 ?DEFAULT_USER_NAME},
        {password,                 ?DEFAULT_PASSWORD},
        {port,                     ?DEFAULT_PORT},
        {listen,                   ?DEFAULT_LISTEN},
        {timeout,                  ?DEFAULT_TIMEOUT},
        {endian,                   ?DEFAULT_ENDIAN},
        {database,                 undefined}],
    [HostName, UserName, Password, Port,
     Listen, Timeout, Endian, Database | NewArgs] =
        cloudi_proplists:take_values(Defaults, Args),
    true = (Endian =:= big orelse
            Endian =:= little orelse
            Endian =:= native),
    true = is_list(Database),
    AsyncArg = if
        Listen =:= true ->
            [{async, self()}];
        Listen =:= false ->
            []
    end,
    FinalArgs = AsyncArg ++
                [{port, Port},
                 {timeout, Timeout},
                 {database, Database} | NewArgs],
    case cloudi_x_pgsql:connect(HostName, UserName, Password, FinalArgs) of
        {ok, Connection} ->
            cloudi_service:subscribe(Dispatcher, Database),
            {ok, #state{connection = Connection,
                        endian = Endian}};
        {error, Reason} ->
            {stop, Reason, #state{}}
    end.

cloudi_service_handle_request(_Type, _Name, _Pattern, _RequestInfo, Request,
                              _Timeout, _Priority, TransId, _Pid,
                              #state{connection = Connection,
                                     endian = Endian} = State,
                              _Dispatcher) ->
    case Request of
        {String, []}
            when (is_binary(String) orelse is_list(String)) ->
            case cloudi_x_pgsql:squery(Connection, String) of
                {ok, _} = Response ->
                    {reply, response_internal(Response, Request), State};
                {ok, _, _} = Response ->
                    {reply, response_internal(Response, Request), State};
                {ok, _, _, _} = Response ->
                    {reply, response_internal(Response, Request), State};
                {error, _} = Response ->
                    {reply, response_internal(Response, Request), State};
                Response when is_list(Response) ->
                    {reply, response_internal(Response, Request), State}
            end;
        {String, [_ | _] = Parameters}
            when (is_binary(String) orelse is_list(String)) ->
            case cloudi_x_pgsql:equery(Connection, String, Parameters) of
                {ok, _} = Response ->
                    {reply, response_internal(Response, Request), State};
                {ok, _, _} = Response ->
                    {reply, response_internal(Response, Request), State};
                {ok, _, _, _} = Response ->
                    {reply, response_internal(Response, Request), State};
                {error, _} = Response ->
                    {reply, response_internal(Response, Request), State};
                Response when is_list(Response) ->
                    {reply, response_internal(Response, Request), State}
            end;
        [String | _] = QueryList when is_list(String) ->
            case do_queries_in_transaction(QueryList, Connection) of
                ok ->
                    {reply, response_internal(ok, Request), State};
                {error, _} = Error ->
                    {reply, response_internal(Error, Request), State}
            end;
        String when is_binary(String) ->
            case cloudi_x_pgsql:squery(Connection, String) of
                {ok, _} = Response ->
                    {reply, response_external(Response, Request, Endian),
                     State};
                {ok, _, _} = Response ->
                    {reply, response_external(Response, Request, Endian),
                     State};
                {ok, _, _, _} = Response ->
                    {reply, response_external(Response, Request, Endian),
                     State};
                {error, Reason} = Response ->
                    ?LOG_ERROR("request ~p error ~p",
                               [TransId, Reason#error.message]),
                    {reply, response_external(Response, Request, Endian),
                     State};
                Response when is_list(Response) ->
                    {reply, response_external(Response, Request, Endian),
                     State}
            end;
        String when is_list(String) ->
            case cloudi_x_pgsql:squery(Connection, String) of
                {ok, _} = Response ->
                    {reply, response_internal(Response, Request), State};
                {ok, _, _} = Response ->
                    {reply, response_internal(Response, Request), State};
                {ok, _, _, _} = Response ->
                    {reply, response_internal(Response, Request), State};
                {error, _} = Response ->
                    {reply, response_internal(Response, Request), State};
                Response when is_list(Response) ->
                    {reply, response_internal(Response, Request), State}
            end
    end.

cloudi_service_handle_info(Request, State, _) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(_, #state{connection = undefined}) ->
    ok;
cloudi_service_terminate(_, #state{connection = Connection}) ->
    cloudi_x_pgsql:close(Connection),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

response_internal(ok, Request) when is_list(Request) ->
    ok;
response_internal({ok, _I} = Response, _) ->
    Response;
response_internal({ok, _Columns, _Data} = Response, _) ->
    Response;
response_internal({ok, _I, _Columns, _Data} = Response, _) ->
    Response;
response_internal({error, Reason}, _) ->
    {error, Reason#error.message};
response_internal(Response, _) when is_list(Response) ->
    Response.

response_external({ok, I}, Input, Endian) ->
    % SQL UPDATE
    IBin = if
        Endian =:= big ->
            <<I:32/unsigned-integer-big>>;
        Endian =:= little ->
            <<I:32/unsigned-integer-little>>;
        Endian =:= native ->
            <<I:32/unsigned-integer-native>>
    end,
    cloudi_response:new(Input, IBin, Endian);
response_external({ok, _Columns, Rows}, Input, Endian) ->
    % SQL SELECT
    I = erlang:length(Rows),
    IBin = if
        Endian =:= big ->
            <<I:32/unsigned-integer-big>>;
        Endian =:= little ->
            <<I:32/unsigned-integer-little>>;
        Endian =:= native ->
            <<I:32/unsigned-integer-native>>
    end,
    Output = erlang:iolist_to_binary([IBin |
    lists:map(fun(T) ->
        cloudi_response:new(
            Input, cloudi_string:term_to_list(erlang:tuple_to_list(T)), Endian
        )
    end, Rows)]),
    cloudi_response:new(Input, Output);
response_external({ok, I, _Columns, _Rows}, Input, Endian) ->
    % SQL INSERT
    IBin = if
        Endian =:= big ->
            <<I:32/unsigned-integer-big>>;
        Endian =:= little ->
            <<I:32/unsigned-integer-little>>;
        Endian =:= native ->
            <<I:32/unsigned-integer-native>>
    end,
    cloudi_response:new(Input, IBin, Endian);
response_external({error, _} = Error, Input, Endian) ->
    cloudi_response:new(Input, Error, Endian);
response_external(Response, Input, Endian) when is_list(Response) ->
    erlang:iolist_to_binary(lists:map(fun(T) ->
        cloudi_response:new(Input, response_external(T, Input, Endian), Endian)
    end, Response)).

%% do a single query and return a boolean to determine if the query succeeded
do_query(Query, Connection) when is_list(Query) ->
    do_query(list_to_binary(Query), Connection);

do_query(Query, Connection) ->
    case cloudi_x_pgsql:squery(Connection, Query) of
        {ok, _} ->
            true;
        {ok, _, _} ->
            true;
        {ok, _, _, _} ->
            true;
        {error, Reason} ->
            % cause a transaction rollback
            % (since a partial success can not be tolerated)
            throw(Reason#error.message);
        Result when is_list(Result) ->
            lists:map(fun(T) ->
                if
                    erlang:element(1, T) =:= error ->
                        Reason = erlang:element(2, T),
                        throw(Reason#error.message);
                    true ->
                        true
                end
            end, Result),
            true
    end.

%% do all queries in the list until an error is encountered
%% return the remaining list if there is an error, else an empty list
do_queries_in_transaction(SQLQueryList, Connection)
    when is_list(SQLQueryList) ->
    case cloudi_x_pgsql:with_transaction(Connection, fun(C) ->
            lists:dropwhile(fun(Query) -> do_query(Query, C) end, SQLQueryList)
        end) of
        [] ->
            ok;
        {rollback, Reason} ->
            {error, Reason}
    end.

%% provide the "?"s parameter syntax externally like cloudi_x_mysql,
%% but provide the $1, $2, $3, etc. PostgreSQL parameter syntax internally
%% to cloudi_x_epgsql, as required.
equery_argument_parse(String) ->
    if
        is_list(String) ->
            equery_argument_parse_get([], 1, String);
        is_binary(String) ->
            equery_argument_parse_get([], 1, erlang:binary_to_list(String))
    end.

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

