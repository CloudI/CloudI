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
%%% Copyright (c) 2009-2014, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2009-2014 Michael Truog
%%% @version 1.3.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_db_pgsql).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_service).

%% external interface
-export([equery/4, equery/5,
         squery/3, squery/4,
         transaction/3, transaction/4,
         binary_to_bytea/1]).

%% cloudi_service callbacks
-export([cloudi_service_init/3,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/2]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include_lib("cloudi_x_epgsql_wg/include/cloudi_x_epgsql_wg.hrl").

-define(DEFAULT_DRIVER,                        wg).
-define(DEFAULT_HOST_NAME,            "127.0.0.1").
-define(DEFAULT_USER_NAME,               "cloudi").
-define(DEFAULT_PASSWORD,                      "").
-define(DEFAULT_PORT,                        5432).
-define(DEFAULT_SSL,                        false).
-define(DEFAULT_LISTEN,                 undefined). % LISTEN/NOTIFY destination
-define(DEFAULT_TIMEOUT,                    20000). % 20 seconds
-define(DEFAULT_ENDIAN,                    native). % response binary sizes
-define(DEFAULT_OUTPUT,                      both).
-define(DEFAULT_INTERNAL_INTERFACE,        native).
% currently only handling equery "?" -> "$N" conversion
-define(DEFAULT_MYSQL_COMPATIBILITY,        false).
-define(DEFAULT_DEBUG,                      false). % log output for debugging
-define(DEFAULT_DEBUG_LEVEL,                trace).

% supported drivers
-define(MODULE_WG, cloudi_x_epgsql_wg).
-define(MODULE_SEMIOCAST, cloudi_x_pgsql_connection).

-type endian() :: big | little | native.

-record(state,
    {
        module :: ?MODULE_WG | ?MODULE_SEMIOCAST,
        connection :: any(),
        listen :: cloudi_service:service_name() | undefined,
        timeout :: pos_integer(),
        endian :: endian(),
        output :: both | external | internal,
        interface :: native | common,
        mysql :: boolean(),
        debug_level :: off | trace | debug | info | warn | error | fatal
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-type count() :: non_neg_integer().
-type rows() :: list(tuple()).
-type common_result() :: {updated, count()} |
                         {updated, count(), rows()} |
                         {selected, rows()} |
                         {error, any()}.
-export_type([common_result/0]).
-type dispatcher() :: cloudi:context() | cloudi_service:dispatcher().

%%-------------------------------------------------------------------------
%% @doc
%% ===An extended SQL query.===
%% @end
%%-------------------------------------------------------------------------

-spec equery(Dispatcher :: dispatcher(),
             Name :: cloudi_service:service_name(),
             Query :: string() | binary(),
             Parameters :: list()) ->
    {ok, any()} |
    {error, any()}.

equery(Dispatcher, Name, Query, Parameters)
    when is_list(Name), (is_binary(Query) orelse is_list(Query)),
         is_list(Parameters) ->
    cloudi:send_sync(Dispatcher, Name,
                     {Query, Parameters}).

%%-------------------------------------------------------------------------
%% @doc
%% ===An extended SQL query with a timeout.===
%% @end
%%-------------------------------------------------------------------------

-spec equery(Dispatcher :: dispatcher(),
             Name :: cloudi_service:service_name(),
             Query :: string() | binary(),
             Parameters :: list(),
             Timeout :: cloudi_service:timeout_milliseconds()) ->
    {ok, any()} |
    {error, any()}.

equery(Dispatcher, Name, Query, Parameters, Timeout)
    when is_list(Name), (is_binary(Query) orelse is_list(Query)),
         is_list(Parameters), is_integer(Timeout) ->
    cloudi:send_sync(Dispatcher, Name,
                     {Query, Parameters}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===A SQL simple query.===
%% @end
%%-------------------------------------------------------------------------

-spec squery(Dispatcher :: dispatcher(),
             Name :: cloudi_service:service_name(),
             Query :: string() | binary()) ->
    {ok, any()} |
    {error, any()}.

squery(Dispatcher, Name, Query)
    when is_list(Name), (is_binary(Query) orelse is_list(Query)) ->
    cloudi:send_sync(Dispatcher, Name,
                     {Query, []}).

%%-------------------------------------------------------------------------
%% @doc
%% ===A SQL simple query with a timeout.===
%% @end
%%-------------------------------------------------------------------------

-spec squery(Dispatcher :: dispatcher(),
             Name :: cloudi_service:service_name(),
             Query :: string() | binary(),
             Timeout :: cloudi_service:timeout_milliseconds()) ->
    {ok, any()} |
    {error, any()}.

squery(Dispatcher, Name, Query, Timeout)
    when is_list(Name), (is_binary(Query) orelse is_list(Query)),
         is_integer(Timeout) ->
    cloudi:send_sync(Dispatcher, Name,
                     {Query, []}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===A list of SQL simple query for a transaction.===
%% @end
%%-------------------------------------------------------------------------

-spec transaction(Dispatcher :: dispatcher(),
                  Name :: cloudi_service:service_name(),
                  QueryList :: list(string() | binary())) ->
    {ok, any()} |
    {error, any()}.

transaction(Dispatcher, Name, [Query | _] = QueryList)
    when is_list(Name), (is_binary(Query) orelse is_list(Query)) ->
    cloudi:send_sync(Dispatcher, Name,
                     QueryList).

%%-------------------------------------------------------------------------
%% @doc
%% ===A list of SQL simple query for a transaction with a timeout.===
%% @end
%%-------------------------------------------------------------------------

-spec transaction(Dispatcher :: dispatcher(),
                  Name :: cloudi_service:service_name(),
                  QueryList :: list(string() | binary()),
                  Timeout :: cloudi_service:timeout_milliseconds()) ->
    {ok, any()} |
    {error, any()}.

transaction(Dispatcher, Name, [Query | _] = QueryList, Timeout)
    when is_list(Name), (is_binary(Query) orelse is_list(Query)),
         is_integer(Timeout) ->
    cloudi:send_sync(Dispatcher, Name,
                     QueryList, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Postgres string encoding of binary data.===
%% Better to use equery.
%% @end
%%-------------------------------------------------------------------------

-spec binary_to_bytea(B :: binary()) ->
    string().

binary_to_bytea(B) when is_binary(B) ->
    binary_to_bytea(B, []).

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, _Prefix, Dispatcher) ->
    Defaults = [
        {driver,                   ?DEFAULT_DRIVER},
        {hostname,                 ?DEFAULT_HOST_NAME},
        {username,                 ?DEFAULT_USER_NAME},
        {password,                 ?DEFAULT_PASSWORD},
        {port,                     ?DEFAULT_PORT},
        {ssl,                      ?DEFAULT_SSL},
        {listen,                   ?DEFAULT_LISTEN},
        {timeout,                  ?DEFAULT_TIMEOUT},
        {endian,                   ?DEFAULT_ENDIAN},
        {output,                   ?DEFAULT_OUTPUT},
        {internal_interface,       ?DEFAULT_INTERNAL_INTERFACE},
        {mysql_compatibility,      ?DEFAULT_MYSQL_COMPATIBILITY},
        {debug,                    ?DEFAULT_DEBUG},
        {debug_level,              ?DEFAULT_DEBUG_LEVEL},
        {database,                 undefined}],
    [Driver, HostName, UserName, Password, Port, SSL, Listen, Timeout, Endian,
     Output, Interface, MysqlCompatibility, Debug, DebugLevel,
     Database | NewArgs] =
        cloudi_proplists:take_values(Defaults, Args),
    Module = if
        Driver =:= wg; Driver =:= epgsql_wg; Driver =:= cloudi_x_epgsql_wg ->
            % NewArgs is passed to ssl application
            ?MODULE_WG;
        Driver =:= semiocast ->
            [] = NewArgs,
            ?MODULE_SEMIOCAST
    end,
    true = (is_list(HostName) andalso is_integer(hd(HostName))),
    true = (is_list(UserName) andalso is_integer(hd(UserName))),
    true = (is_list(Password) andalso is_integer(hd(Password))),
    true = (is_integer(Port) andalso (Port > 0)),
    true = ((SSL =:= true) orelse
            (SSL =:= false)),
    AsyncArg = if
        is_list(Listen), is_integer(hd(Listen)) ->
            [{async, cloudi_service:self(Dispatcher)}];
        Listen =:= undefined ->
            []
    end,
    true = (is_integer(Timeout) andalso (Timeout > 0)),
    true = ((Endian =:= big) orelse
            (Endian =:= little) orelse
            (Endian =:= native)),
    true = ((Output =:= both) orelse
            (Output =:= external) orelse
            (Output =:= internal)),
    true = ((Interface =:= native) orelse
            (Interface =:= common)),
    true = ((MysqlCompatibility =:= true) orelse
            (MysqlCompatibility =:= false)),
    true = ((Debug =:= true) orelse
            (Debug =:= false)),
    true = ((DebugLevel =:= trace) orelse
            (DebugLevel =:= debug) orelse
            (DebugLevel =:= info) orelse
            (DebugLevel =:= warn) orelse
            (DebugLevel =:= error) orelse
            (DebugLevel =:= fatal)),
    true = (is_list(Database) andalso is_integer(hd(Database))),
    FinalArgs = AsyncArg ++
                [{port, Port},
                 {timeout, Timeout},
                 {database, Database},
                 {ssl, SSL} | NewArgs],
    case driver_open(Module, HostName, UserName, Password, FinalArgs) of
        {ok, Connection} ->
            cloudi_service:subscribe(Dispatcher, Database),
            DebugLogLevel = if
                Debug =:= false ->
                    off;
                Debug =:= true ->
                    DebugLevel
            end,
            {ok, #state{module = Module,
                        connection = Connection,
                        listen = Listen,
                        timeout = Timeout,
                        endian = Endian,
                        output = Output,
                        interface = Interface,
                        mysql = MysqlCompatibility,
                        debug_level = DebugLogLevel}};
        {error, Reason} ->
            {stop, Reason, undefined}
    end.

cloudi_service_handle_request(_Type, _Name, _Pattern, _RequestInfo, Request,
                              _Timeout, _Priority, _TransId, _Pid,
                              #state{output = Output,
                                     mysql = MysqlCompatibility} = State,
                              _Dispatcher) ->
    case Request of
        {Query, []}
            when (Output =/= external),
                 (is_binary(Query) orelse is_list(Query)) ->
            case driver_squery(internal, Query, State) of
                {error, closed} ->
                    {stop, closed, State};
                Response ->
                    {reply, Response, State}
            end;
        {Query, [_ | _] = Parameters}
            when (Output =/= external),
                 (is_binary(Query) orelse is_list(Query)) ->
            NewQuery = if
                MysqlCompatibility =:= true ->
                    mysql_query_transform(Query);
                MysqlCompatibility =:= false ->
                    Query
            end,
            case driver_equery(NewQuery, Parameters, State) of
                {error, closed} ->
                    {stop, closed, State};
                Response ->
                    {reply, Response, State}
            end;
        [Query | _] = QueryList
            when (Output =/= external),
                 (is_binary(Query) orelse is_list(Query)) ->
            case driver_with_transaction(QueryList, State) of
                {error, closed} ->
                    {stop, closed, State};
                Response ->
                    {reply, Response, State}
            end;
        Query
            when is_binary(Query) ->
            ResponseOutput = if
                Output =:= internal ->
                    internal;
                Output =:= external; Output =:= both ->
                    external
            end,
            case driver_squery(ResponseOutput, Query, State) of
                {error, closed} ->
                    {stop, closed, State};
                Response ->
                    {reply, Response, State}
            end;
        Query
            when (Output =/= external),
                 is_list(Query) ->
            case driver_squery(internal, Query, State) of
                {error, closed} ->
                    {stop, closed, State};
                Response ->
                    {reply, Response, State}
            end
    end.

cloudi_service_handle_info({pgsql, Connection, Async},
                           #state{connection = Connection,
                                  listen = Listen} = State, Dispatcher) ->
    {ok, _} = cloudi_service:send_async(Dispatcher, Listen, Async),
    {noreply, State};
cloudi_service_handle_info(Request, State, _) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(_, undefined) ->
    ok;
cloudi_service_terminate(_, #state{module = Module,
                                   connection = Connection}) ->
    driver_close(Module, Connection),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

-spec response_external(common_result(),
                        Input :: any(),
                        Endian :: endian()) ->
    binary().

% rely on an interface result format
response_external({updated, Count}, Input, Endian) ->
    % SQL UPDATE and various commands
    CountBin = if
        Endian =:= big ->
            <<Count:32/unsigned-integer-big>>;
        Endian =:= little ->
            <<Count:32/unsigned-integer-little>>;
        Endian =:= native ->
            <<Count:32/unsigned-integer-native>>
    end,
    cloudi_response:new(Input, CountBin, Endian);
response_external({updated, Count, _Rows}, Input, Endian) ->
    % SQL INSERT/UPDATE/DELETE
    CountBin = if
        Endian =:= big ->
            <<Count:32/unsigned-integer-big>>;
        Endian =:= little ->
            <<Count:32/unsigned-integer-little>>;
        Endian =:= native ->
            <<Count:32/unsigned-integer-native>>
    end,
    cloudi_response:new(Input, CountBin, Endian);
response_external({selected, Rows}, Input, Endian) ->
    % SQL SELECT
    Count = erlang:length(Rows),
    CountBin = if
        Endian =:= big ->
            <<Count:32/unsigned-integer-big>>;
        Endian =:= little ->
            <<Count:32/unsigned-integer-little>>;
        Endian =:= native ->
            <<Count:32/unsigned-integer-native>>
    end,
    Output = erlang:iolist_to_binary([CountBin |
        lists:map(fun(T) ->
            Row = cloudi_string:term_to_list(erlang:tuple_to_list(T)),
            cloudi_response:new(Input, Row, Endian)
        end, Rows)]),
    cloudi_response:new(Input, Output);
response_external({error, _} = Error, Input, Endian) ->
    cloudi_response:new(Input, Error, Endian).

%% provide the "?"s parameter syntax externally like cloudi_x_emysql,
%% but provide the $1, $2, $3, etc. PostgreSQL parameter syntax internally
%% to cloudi_x_epgsql, as required.
mysql_query_transform(Query) ->
    if
        is_list(Query) ->
            mysql_query_transform_get([], 1, Query);
        is_binary(Query) ->
            mysql_query_transform_get([], 1, erlang:binary_to_list(Query))
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
mysql_query_transform_put(NewString, ?QUOTE, $?, ?QUOTE,
                          false, Index, Remaining) ->
    mysql_query_transform_get(
        NewString ++ "'$" ++ integer_to_list(Index) ++ "'",
        Index + 1, Remaining);
mysql_query_transform_put(NewString, ?SPACE, $?, ?SPACE,
                          false, Index, Remaining) ->
    mysql_query_transform_get(
        NewString ++ " $" ++ integer_to_list(Index) ++ " ",
        Index + 1, Remaining);
mysql_query_transform_put(NewString, ?SPACE, $?, $),
                          false, Index, Remaining) ->
    mysql_query_transform_get(
        NewString ++ " $" ++ integer_to_list(Index) ++ ")",
        Index + 1, Remaining);
mysql_query_transform_put(NewString, ?SPACE, $?, $,,
                          false, Index, Remaining) ->
    mysql_query_transform_get(
        NewString ++ " $" ++ integer_to_list(Index) ++ ",",
        Index + 1, Remaining);
mysql_query_transform_put(NewString, ?SPACE, $?, $;,
                          false, Index, Remaining) ->
    mysql_query_transform_get(
        NewString ++ " $" ++ integer_to_list(Index) ++ ";",
        Index + 1, Remaining);
mysql_query_transform_put(NewString, $,, $?, ?SPACE,
                          false, Index, Remaining) ->
    mysql_query_transform_get(
        NewString ++ ",$" ++ integer_to_list(Index) ++ " ",
        Index + 1, Remaining);
mysql_query_transform_put(NewString, $(, $?, ?SPACE,
                          false, Index, Remaining) ->
    mysql_query_transform_get(
        NewString ++ "($" ++ integer_to_list(Index) ++ " ",
        Index + 1, Remaining);
%% handle expression operators lacking separation spaces when
%% performing parameter syntax substitution
mysql_query_transform_put(NewString, Left, $?, ?SPACE,
                          false, Index, Remaining)
    when ?SYMMETRIC_OPERATOR_GUARD(Left);
         Left == $D; Left == $R ->
    mysql_query_transform_get(
        NewString ++ Left ++ "$" ++ integer_to_list(Index) ++ " ",
        Index + 1, Remaining);
mysql_query_transform_put(NewString, Left, $?, $),
                          false, Index, Remaining)
    when ?SYMMETRIC_OPERATOR_GUARD(Left);
         Left == $D; Left == $R ->
    mysql_query_transform_get(
        NewString ++ Left ++ "$" ++ integer_to_list(Index) ++ ")",
        Index + 1, Remaining);
mysql_query_transform_put(NewString, ?SPACE, $?, Right,
                          false, Index, Remaining)
    when ?SYMMETRIC_OPERATOR_GUARD(Right);
         Right == $A; Right == $O; Right == $! ->
    mysql_query_transform_get(
        NewString ++ " $" ++ integer_to_list(Index) ++ Right,
        Index + 1, Remaining);
mysql_query_transform_put(NewString, $(, $?, Right,
                          false, Index, Remaining)
    when ?SYMMETRIC_OPERATOR_GUARD(Right);
         Right == $A; Right == $O; Right == $! ->
    mysql_query_transform_get(
        NewString ++ "($" ++ integer_to_list(Index) ++ Right,
        Index + 1, Remaining);
%% tail recursion termination case
mysql_query_transform_put(NewString, C1, C2, C3,
                          _, _, []) ->
    NewString ++ [C1, C2, C3];
%% keep track of quoted strings, so that they are not parsed
%% for parameter syntax substitution
mysql_query_transform_put(NewString, C1, C2, C3,
                          Quoted, Index, [C4 | Remaining]) ->
    NewQuoted = if
        C1 == ?QUOTE, Quoted ->
            false;
        C1 == ?QUOTE ->
            true;
        true ->
            Quoted
    end,
    mysql_query_transform_put(
        NewString ++ [C1], C2, C3, C4, NewQuoted, Index, Remaining).
%% get more characters for parameter syntax substitution parsing
mysql_query_transform_get(NewString, _, []) ->
    NewString;
mysql_query_transform_get(NewString, Index, Remaining) ->
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
                    mysql_query_transform_put(
                        NewString, C1, C2, C3, false, Index, Remaining3)
            end
    end.

%% interface adapter

driver_open(?MODULE_WG, HostName, UserName, Password, Args) ->
    ?MODULE_WG:connect(HostName, UserName, Password, Args);
driver_open(?MODULE_SEMIOCAST, HostName, UserName, Password, Args0) ->
    {value, {_, Database}, Args1} = lists:keytake(database, 1, Args0),
    % timeout is per function call (not open though)
    {value, {_, _}, ArgsN} = lists:keytake(timeout, 1, Args1),
    try ?MODULE_SEMIOCAST:open(HostName, Database,
                               UserName, Password, ArgsN) of
        Connection ->
            {ok, Connection}
    catch
        throw:Reason ->
            {error, Reason}
    end.

driver_close(?MODULE_WG, Connection) ->
    ?MODULE_WG:close(Connection);
driver_close(?MODULE_SEMIOCAST, Connection) ->
    ?MODULE_SEMIOCAST:close(Connection).

with_transaction([Query], State) ->
    case driver_squery(internal, Query, State) of
        {error, _} = Error ->
            Error;
        Last ->
            case driver_squery(internal, <<"COMMIT">>, State) of
                {updated, 0} ->
                    Last;
                {error, _} = Error ->
                    Error
            end
    end;
with_transaction([Query | L], State) ->
    case driver_squery(internal, Query, State) of
        {error, _} = Error ->
            Error;
        _ ->
            with_transaction(L, State)
    end.

% only internal usage, due to relying on an erlang list
driver_with_transaction(L, State) ->
    case driver_squery(internal, <<"BEGIN">>, State) of
        {updated, 0} ->
            case with_transaction(L, State) of
                {error, _} = Error ->
                    driver_squery(internal, <<"ROLLBACK">>, State),
                    Error;
                Last ->
                    Last
            end;
        {error, _} = Error ->
            Error
    end.

% only internal usage, due to relying on an erlang list
driver_equery(Query, Parameters,
              #state{module = ?MODULE_WG,
                     connection = Connection,
                     interface = Interface,
                     debug_level = DebugLevel}) ->
    Native = ?MODULE_WG:equery(Connection, Query, Parameters),
    if
        DebugLevel =:= off ->
            ok;
        true ->
            driver_debug(DebugLevel, Query, Parameters, Native)
    end,
    if
        Interface =:= common ->
            wg_to_common(Native);
        Interface =:= native ->
            Native
    end;
driver_equery(Query, Parameters,
              #state{module = ?MODULE_SEMIOCAST,
                     connection = Connection,
                     timeout = Timeout,
                     interface = Interface,
                     debug_level = DebugLevel}) ->
    Native = ?MODULE_SEMIOCAST:extended_query(Query, Parameters, [],
                                              Timeout, Connection),
    if
        DebugLevel =:= off ->
            ok;
        true ->
            driver_debug(DebugLevel, Query, Parameters, Native)
    end,
    if
        Interface =:= common ->
            semiocast_to_common(Native);
        Interface =:= native ->
            Native
    end.

% internal or external
driver_squery(internal, Query,
              #state{module = ?MODULE_WG,
                     connection = Connection,
                     interface = Interface,
                     debug_level = DebugLevel}) ->
    Native = ?MODULE_WG:squery(Connection, Query),
    if
        DebugLevel =:= off ->
            ok;
        true ->
            driver_debug(DebugLevel, Query, Native)
    end,
    if
        Interface =:= common ->
            wg_to_common(Native);
        Interface =:= native ->
            Native
    end;
driver_squery(external, Query,
              #state{module = ?MODULE_WG,
                     connection = Connection,
                     endian = Endian,
                     debug_level = DebugLevel}) ->
    Native = ?MODULE_WG:squery(Connection, Query),
    if
        DebugLevel =:= off ->
            ok;
        true ->
            driver_debug(DebugLevel, Query, Native)
    end,
    response_external(wg_to_common(Native), Query, Endian);
driver_squery(internal, Query,
              #state{module = ?MODULE_SEMIOCAST,
                     connection = Connection,
                     timeout = Timeout,
                     interface = Interface,
                     debug_level = DebugLevel}) ->
    Native = ?MODULE_SEMIOCAST:simple_query(Query, [], Timeout, Connection),
    if
        DebugLevel =:= off ->
            ok;
        true ->
            driver_debug(DebugLevel, Query, Native)
    end,
    if
        Interface =:= common ->
            semiocast_to_common(Native);
        Interface =:= native ->
            Native
    end;
driver_squery(external, Query,
              #state{module = ?MODULE_SEMIOCAST,
                     connection = Connection,
                     timeout = Timeout,
                     endian = Endian,
                     debug_level = DebugLevel}) ->
    Native = ?MODULE_SEMIOCAST:simple_query(Query, [], Timeout, Connection),
    if
        DebugLevel =:= off ->
            ok;
        true ->
            driver_debug(DebugLevel, Query, Native)
    end,
    response_external(semiocast_to_common(Native), Query, Endian).

driver_debug_log(trace, Message, Args) ->
    ?LOG_TRACE(Message, Args);
driver_debug_log(debug, Message, Args) ->
    ?LOG_DEBUG(Message, Args);
driver_debug_log(info, Message, Args) ->
    ?LOG_INFO(Message, Args);
driver_debug_log(warn, Message, Args) ->
    ?LOG_WARN(Message, Args);
driver_debug_log(error, Message, Args) ->
    ?LOG_ERROR(Message, Args);
driver_debug_log(fatal, Message, Args) ->
    ?LOG_FATAL(Message, Args).

driver_debug(Level, Query, Parameters, Result) ->
    driver_debug_log(Level,
                     "SQL(equery): ~p~n"
                     "             ~p~n"
                     "           = ~p",
                     [Query, Parameters, Result]).

driver_debug(Level, Query, Result) ->
    driver_debug_log(Level,
                     "SQL(equery): ~p~n"
                     "           = ~p",
                     [Query, Result]).

% Rows in the wg format only use binary strings for data
wg_to_common({ok, I}) ->
    {updated, I};
wg_to_common({ok, [], []}) ->
    {updated, 0};
wg_to_common({ok, _Columns, Rows}) ->
    {selected, Rows};
wg_to_common({ok, I, _Columns, Rows}) ->
    {updated, I, Rows};
wg_to_common({error, #epgsql_wg_error{message = Message}}) ->
    {error, Message};
wg_to_common([_ | _] = L) ->
    check_list(L, fun wg_to_common/1).

% based on the semiocast driver's attempt to fake odbc results
% (doesn't match odbc)
-spec semiocast_to_common(?MODULE_SEMIOCAST:result_tuple() |
                          {error, any()}) ->
    common_result().
    
% Rows in the semiocast format use Erlang types for data
semiocast_to_common({error, {cloudi_x_pgsql_error, L}}) ->
    {_, Message} = lists:keyfind(message, 1, L),
    {error, Message};
semiocast_to_common({error, _} = Error) ->
    Error;
semiocast_to_common({{insert, _TableOID, Count}, []}) ->
    {updated, Count};
semiocast_to_common({{insert, _TableOID, Count}, Rows}) ->
    {updated, Count, Rows};
semiocast_to_common({{copy, Count}, []}) ->
    {updated, Count};
semiocast_to_common({{delete, Count}, []}) ->
    {updated, Count};
semiocast_to_common({{delete, Count}, Rows}) ->
    {updated, Count, Rows};
semiocast_to_common({{fetch, Count}, []}) ->
    {updated, Count};
semiocast_to_common({{move, Count}, []}) ->
    {updated, Count};
semiocast_to_common({{select, _Count}, Rows}) ->
    {selected, Rows}; % not odbc-like
semiocast_to_common({{update, Count}, []}) ->
    {updated, Count};
semiocast_to_common({{update, Count}, Rows}) ->
    {updated, Count, Rows};
semiocast_to_common({{alter, _What}, []}) ->
    {updated, 0};
semiocast_to_common({{create, _What}, []}) ->
    {updated, 0};
semiocast_to_common({{drop, _What}, []}) ->
    {updated, 0};
semiocast_to_common({{start, transaction}, []}) ->
    {updated, 0};
semiocast_to_common({'begin', []}) ->
    {updated, 0};
semiocast_to_common({commit, []}) ->
    {updated, 0};
semiocast_to_common({'do', []}) ->
    {updated, 0};
semiocast_to_common({listen, []}) ->
    {updated, 0};
semiocast_to_common({notify, []}) ->
    {updated, 0};
semiocast_to_common({rollback, []}) ->
    {error, rollback};
semiocast_to_common({set, []}) ->
    {updated, 0};
semiocast_to_common({{declare, cursor}, []}) ->
    {updated, 0};
semiocast_to_common({{lock, table}, []}) ->
    {updated, 0};
semiocast_to_common({Command, []}) ->
    {error, {invalid_command, Command}};
semiocast_to_common([_ | _] = L) ->
    check_list(L, fun semiocast_to_common/1).

check_list([E], F) ->
    F(E);
check_list([E | L], F) ->
    case F(E) of
        {error, _} = Error ->
            Error;
        _ ->
            % ignore all but the last element, unless there is an error
            check_list(L, F)
    end.

binary_to_bytea(<<>>, L) ->
    "E'\\\\x" ++ lists:reverse([$' | L]);
binary_to_bytea(<<N1:4, N2:4, Rest/binary>>, L) ->
    binary_to_bytea(Rest, [int_to_hex(N2), int_to_hex(N1) | L]).

-compile({inline, [{int_to_hex,1}]}).

int_to_hex(I) when 16#0 =< I, I =< 16#9 ->
    I + $0;
int_to_hex(I) when 16#A =< I, I =< 16#F ->
    (I - 16#A) + $A.

