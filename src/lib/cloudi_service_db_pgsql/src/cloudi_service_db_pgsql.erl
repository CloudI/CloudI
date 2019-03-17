%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI PostgreSQL Data Module==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2009-2019 Michael Truog <mjtruog at protonmail dot com>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a
%%% copy of this software and associated documentation files (the "Software"),
%%% to deal in the Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%% and/or sell copies of the Software, and to permit persons to whom the
%%% Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%%% DEALINGS IN THE SOFTWARE.
%%%
%%% @author Michael Truog <mjtruog at protonmail dot com>
%%% @copyright 2009-2019 Michael Truog
%%% @version 1.8.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_db_pgsql).
-author('mjtruog at protonmail dot com').

-behaviour(cloudi_service).

%% external interface
-export([equery/4, equery/5,
         squery/3, squery/4,
         transaction/3, transaction/4,
         binary_to_bytea/1]).

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include_lib("cloudi_x_epgsql/include/cloudi_x_epgsql.hrl").

-define(DEFAULT_DATABASE,               undefined). % required argument, string
-define(DEFAULT_DRIVER,                 semiocast).
-define(DEFAULT_HOST_NAME,            "127.0.0.1").
-define(DEFAULT_USER_NAME,               "cloudi").
-define(DEFAULT_PASSWORD,                      "").
-define(DEFAULT_PORT,                        5432).
-define(DEFAULT_SSL,                        false).
-define(DEFAULT_LISTEN,                 undefined). % LISTEN/NOTIFY destination
-define(DEFAULT_TIMEOUT,                    20000). % ms (connect-only)
-define(DEFAULT_OUTPUT,                      both).
-define(DEFAULT_EXTERNAL_FORMAT,    erlang_string).
-define(DEFAULT_INTERNAL_INTERFACE,        native).
-define(DEFAULT_MYSQL_COMPATIBILITY,        false). % see below:
        % currently only handling equery "?" -> "$N" conversion
-define(DEFAULT_DEBUG,                      false). % log output for debugging
-define(DEFAULT_DEBUG_LEVEL,                trace).


% supported drivers
-define(MODULE_EPGSQL, cloudi_x_epgsql).
-define(MODULE_SEMIOCAST, cloudi_x_pgsql_connection).

-define(DRIVER_DEBUG(Level, Query, Parameters, Result),
    ?LOG(Level,
         "SQL(equery):~n"
         " ~p~n"
         " ~p~n"
         " = ~p",
         [Query, Parameters, Result])).

-define(DRIVER_DEBUG(Level, Query, Result),
    ?LOG(Level,
         "SQL(squery):~n"
         " ~p~n"
         " = ~p",
         [Query, Result])).

-record(state,
    {
        module :: ?MODULE_EPGSQL | ?MODULE_SEMIOCAST,
        connection :: any(),
        listen :: cloudi_service:service_name() | undefined,
        output_type :: both | internal | external,
        external_format :: cloudi_request:external_format(),
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

-type agent() :: cloudi:agent().
-type service_name() :: cloudi:service_name().
-type timeout_milliseconds() :: cloudi:timeout_milliseconds().
-type module_response(Result) ::
    {{ok, Result}, NewAgent :: agent()} |
    {{error, cloudi:error_reason_sync()}, NewAgent :: agent()}.

%%-------------------------------------------------------------------------
%% @doc
%% ===An extended SQL query.===
%% @end
%%-------------------------------------------------------------------------

-spec equery(Agent :: agent(),
             Name :: service_name(),
             Query :: string() | binary(),
             Parameters :: list()) ->
    module_response(any()).

equery(Agent, Name, Query, Parameters)
    when (is_binary(Query) orelse is_list(Query)),
         is_list(Parameters) ->
    cloudi:send_sync(Agent, Name,
                     {Query, Parameters}).

%%-------------------------------------------------------------------------
%% @doc
%% ===An extended SQL query with a timeout.===
%% @end
%%-------------------------------------------------------------------------

-spec equery(Agent :: agent(),
             Name :: service_name(),
             Query :: string() | binary(),
             Parameters :: list(),
             Timeout :: timeout_milliseconds()) ->
    module_response(any()).

equery(Agent, Name, Query, Parameters, Timeout)
    when (is_binary(Query) orelse is_list(Query)), is_list(Parameters) ->
    cloudi:send_sync(Agent, Name,
                     {Query, Parameters}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===A SQL simple query.===
%% @end
%%-------------------------------------------------------------------------

-spec squery(Agent :: agent(),
             Name :: service_name(),
             Query :: string() | binary()) ->
    module_response(any()).

squery(Agent, Name, Query)
    when (is_binary(Query) orelse is_list(Query)) ->
    cloudi:send_sync(Agent, Name,
                     {Query, []}).

%%-------------------------------------------------------------------------
%% @doc
%% ===A SQL simple query with a timeout.===
%% @end
%%-------------------------------------------------------------------------

-spec squery(Agent :: agent(),
             Name :: service_name(),
             Query :: string() | binary(),
             Timeout :: timeout_milliseconds()) ->
    module_response(any()).

squery(Agent, Name, Query, Timeout)
    when (is_binary(Query) orelse is_list(Query)) ->
    cloudi:send_sync(Agent, Name,
                     {Query, []}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===A list of SQL simple query for a transaction.===
%% @end
%%-------------------------------------------------------------------------

-spec transaction(Agent :: agent(),
                  Name :: service_name(),
                  QueryList :: list(string() | binary())) ->
    module_response(ok | {error, any()}).

transaction(Agent, Name, [Query | _] = QueryList)
    when (is_binary(Query) orelse is_list(Query)) ->
    cloudi:send_sync(Agent, Name,
                     QueryList).

%%-------------------------------------------------------------------------
%% @doc
%% ===A list of SQL simple query for a transaction with a timeout.===
%% @end
%%-------------------------------------------------------------------------

-spec transaction(Agent :: agent(),
                  Name :: service_name(),
                  QueryList :: list(string() | binary()),
                  Timeout :: timeout_milliseconds()) ->
    module_response(ok | {error, any()}).

transaction(Agent, Name, [Query | _] = QueryList, Timeout)
    when (is_binary(Query) orelse is_list(Query)) ->
    cloudi:send_sync(Agent, Name,
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

cloudi_service_init(Args, _Prefix, Timeout, Dispatcher) ->
    Defaults = [
        {database,                 ?DEFAULT_DATABASE},
        {driver,                   ?DEFAULT_DRIVER},
        {hostname,                 ?DEFAULT_HOST_NAME},
        {username,                 ?DEFAULT_USER_NAME},
        {password,                 ?DEFAULT_PASSWORD},
        {port,                     ?DEFAULT_PORT},
        {ssl,                      ?DEFAULT_SSL},
        {listen,                   ?DEFAULT_LISTEN},
        {timeout,                  ?DEFAULT_TIMEOUT},
        {output,                   ?DEFAULT_OUTPUT},
        {external_format,          ?DEFAULT_EXTERNAL_FORMAT},
        {internal_interface,       ?DEFAULT_INTERNAL_INTERFACE},
        {mysql_compatibility,      ?DEFAULT_MYSQL_COMPATIBILITY},
        {debug,                    ?DEFAULT_DEBUG},
        {debug_level,              ?DEFAULT_DEBUG_LEVEL}],
    [Database, Driver, HostName, UserName, Password, Port, SSL, Listen,
     TimeoutConnect, OutputType, ExternalFormat, Interface, MysqlCompatibility,
     Debug, DebugLevel | NewArgs] =
        cloudi_proplists:take_values(Defaults, Args),
    true = (is_list(Database) andalso is_integer(hd(Database))),
    Module = if
        Driver =:= epgsql ->
            ?MODULE_EPGSQL;
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
    true = (is_integer(TimeoutConnect) andalso (TimeoutConnect > 0)),
    true = ((OutputType =:= both) orelse
            (OutputType =:= external) orelse
            (OutputType =:= internal)),
    true = ((ExternalFormat =:= erlang_string) orelse
            (ExternalFormat =:= erlang_term) orelse
            (ExternalFormat =:= msgpack)),
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
    FinalArgs = AsyncArg ++
                [{port, Port},
                 {timeout, erlang:min(TimeoutConnect, Timeout)},
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
                        output_type = OutputType,
                        external_format = ExternalFormat,
                        interface = Interface,
                        mysql = MysqlCompatibility,
                        debug_level = DebugLogLevel}};
        {error, Reason} ->
            {stop, Reason, undefined}
    end.

cloudi_service_handle_request(_RequestType, _Name, _Pattern,
                              _RequestInfo, Request,
                              Timeout, _Priority, _TransId, _Pid,
                              #state{output_type = OutputType,
                                     external_format = ExternalFormat} = State,
                              _Dispatcher) ->
    if
        is_binary(Request) ->
            ResponseOutputType = if
                OutputType =:= internal ->
                    internal;
                OutputType =:= external; OutputType =:= both ->
                    external
            end,
            if
                OutputType =:= internal; ExternalFormat =:= erlang_string ->
                    request_internal(Request, Timeout,
                                     ResponseOutputType, State);
                true ->
                    RequestInternal = case cloudi_request:
                                           external_format(Request,
                                                           ExternalFormat) of
                        [T1, T2] ->
                            {T1, T2};
                        RequestInternalValue ->
                            RequestInternalValue
                    end,
                    request_internal(RequestInternal, Timeout,
                                     ResponseOutputType, State)
            end;
        true ->
            request_internal(Request, Timeout,
                             internal, State)
    end.

cloudi_service_handle_info({AsyncTag, Connection, Async},
                           #state{module = Module,
                                  connection = Connection,
                                  listen = Listen,
                                  interface = Interface} = State, Dispatcher)
    when AsyncTag =:= pgsql;
         AsyncTag =:= epgsql ->
    Request = if
        Interface =:= native ->
            Async;
        Interface =:= common ->
            driver_async_to_common(Module, Async)
    end,
    {ok, _} = cloudi_service:send_async(Dispatcher, Listen, Request),
    {noreply, State};

cloudi_service_handle_info(Request, State, _Dispatcher) ->
    {stop, cloudi_string:format("Unknown info \"~w\"", [Request]), State}.

cloudi_service_terminate(_Reason, _Timeout, undefined) ->
    ok;
cloudi_service_terminate(_Reason, _Timeout,
                         #state{module = Module,
                                connection = Connection}) ->
    driver_close(Module, Connection),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

request_internal({Query, []}, Timeout,
                 ResponseOutputType, State)
    when is_binary(Query); is_list(Query) ->
    Response = driver_squery(Query, Timeout,
                             ResponseOutputType, State),
    {reply, Response, State};
request_internal({Query, [_ | _] = Parameters}, Timeout,
                 ResponseOutputType,
                 #state{mysql = MysqlCompatibility} = State)
    when is_binary(Query); is_list(Query) ->
    NewQuery = if
        MysqlCompatibility =:= true ->
            mysql_query_transform(Query);
        MysqlCompatibility =:= false ->
            Query
    end,
    Response = driver_equery(NewQuery, Parameters, Timeout,
                             ResponseOutputType, State),
    {reply, Response, State};
request_internal([Query | _] = QueryList, Timeout,
                 ResponseOutputType, State)
    when is_binary(Query); is_list(Query) ->
    Response = driver_with_transaction(QueryList, Timeout,
                                       ResponseOutputType, State),
    {reply, Response, State};
request_internal(Query, Timeout,
                 ResponseOutputType, State)
    when is_binary(Query); is_list(Query) ->
    Response = driver_squery(Query, Timeout,
                             ResponseOutputType, State),
    {reply, Response, State}.

-spec response_external(common_result(),
                        ExternalFormat :: cloudi_request:external_format()) ->
    binary().

% rely on an interface result format
response_external({updated, _Count} = Response, ExternalFormat) ->
    % SQL UPDATE and various commands
    cloudi_response:external_format(Response, ExternalFormat);
response_external({updated, Count, _Rows}, ExternalFormat) ->
    % SQL INSERT/UPDATE/DELETE
    cloudi_response:external_format({updated, Count}, ExternalFormat);
response_external({selected, _Rows} = Response, ExternalFormat) ->
    % SQL SELECT
    cloudi_response:external_format(Response, ExternalFormat);
response_external({error, _} = Response, ExternalFormat) ->
    cloudi_response:external_format(Response, ExternalFormat).

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

driver_open(?MODULE_EPGSQL, HostName, UserName, Password, Args) ->
    ?MODULE_EPGSQL:connect(HostName, UserName, Password, Args);
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

driver_close(?MODULE_EPGSQL, Connection) ->
    ?MODULE_EPGSQL:close(Connection);
driver_close(?MODULE_SEMIOCAST, Connection) ->
    ?MODULE_SEMIOCAST:close(Connection).

driver_with_transaction(L, Timeout, ResponseOutputType,
                        #state{external_format = ExternalFormat} = State) ->
    Response = case driver_squery([<<"BEGIN;">> | L] ++
                                  [<<"COMMIT;">>], Timeout, internal,
                                  State#state{interface = common}) of
        {updated, 0} ->
            ok;
        {error, _} = Error ->
            driver_squery(<<"ROLLBACK;">>, Timeout,
                          internal, State),
            Error
    end,
    if
        ResponseOutputType =:= internal ->
            Response;
        ResponseOutputType =:= external ->
            if
                Response =:= ok ->
                    response_external({updated, 0}, ExternalFormat);
                true ->
                    response_external(Response, ExternalFormat)
            end
    end.

driver_equery(Query, Parameters, _Timeout, ResponseOutputType,
              #state{module = ?MODULE_EPGSQL,
                     connection = Connection,
                     external_format = ExternalFormat,
                     interface = Interface,
                     debug_level = DebugLevel}) ->
    Native = ?MODULE_EPGSQL:equery(Connection, Query, Parameters),
    ?DRIVER_DEBUG(DebugLevel, Query, Parameters, Native),
    if
        Native == {error, closed} ->
            erlang:exit(closed);
        true ->
            ok
    end,
    Response = if
        Interface =:= common; ResponseOutputType =:= external ->
            epgsql_to_common(Native);
        Interface =:= native ->
            Native
    end,
    if
        ResponseOutputType =:= internal ->
            Response;
        ResponseOutputType =:= external ->
            response_external(Response, ExternalFormat)
    end;
driver_equery(Query, Parameters, Timeout, ResponseOutputType,
              #state{module = ?MODULE_SEMIOCAST,
                     connection = Connection,
                     external_format = ExternalFormat,
                     interface = Interface,
                     debug_level = DebugLevel}) ->
    Native = ?MODULE_SEMIOCAST:extended_query(Query, Parameters, [],
                                              Timeout, Connection),
    ?DRIVER_DEBUG(DebugLevel, Query, Parameters, Native),
    if
        Native == {error, closed} ->
            erlang:exit(closed);
        true ->
            ok
    end,
    Response = if
        Interface =:= common; ResponseOutputType =:= external ->
            semiocast_to_common(Native);
        Interface =:= native ->
            Native
    end,
    if
        ResponseOutputType =:= internal ->
            Response;
        ResponseOutputType =:= external ->
            response_external(Response, ExternalFormat)
    end.

driver_squery(Query, _Timeout, ResponseOutputType,
              #state{module = ?MODULE_EPGSQL,
                     connection = Connection,
                     external_format = ExternalFormat,
                     interface = Interface,
                     debug_level = DebugLevel}) ->
    Native = ?MODULE_EPGSQL:squery(Connection, Query),
    ?DRIVER_DEBUG(DebugLevel, Query, Native),
    if
        Native == {error, closed} ->
            erlang:exit(closed);
        true ->
            ok
    end,
    Response = if
        Interface =:= common; ResponseOutputType =:= external ->
            epgsql_to_common(Native);
        Interface =:= native ->
            Native
    end,
    if
        ResponseOutputType =:= internal ->
            Response;
        ResponseOutputType =:= external ->
            response_external(Response, ExternalFormat)
    end;
driver_squery(Query, Timeout, ResponseOutputType,
              #state{module = ?MODULE_SEMIOCAST,
                     connection = Connection,
                     external_format = ExternalFormat,
                     interface = Interface,
                     debug_level = DebugLevel}) ->
    Native = ?MODULE_SEMIOCAST:simple_query(Query, [], Timeout, Connection),
    ?DRIVER_DEBUG(DebugLevel, Query, Native),
    if
        Native == {error, closed} ->
            erlang:exit(closed);
        true ->
            ok
    end,
    Response = if
        Interface =:= common; ResponseOutputType =:= external ->
            semiocast_to_common(Native);
        Interface =:= native ->
            Native
    end,
    if
        ResponseOutputType =:= internal ->
            Response;
        ResponseOutputType =:= external ->
            response_external(Response, ExternalFormat)
    end.

epgsql_to_common({ok, I}) ->
    {updated, I};
epgsql_to_common({ok, [], []}) ->
    {updated, 0};
epgsql_to_common({ok, _Columns, Rows}) ->
    {selected, Rows};
epgsql_to_common({ok, I, _Columns, Rows}) ->
    {updated, I, Rows};
epgsql_to_common({error, #error{message = Message}}) ->
    {error, Message};
epgsql_to_common([_ | _] = L) ->
    check_list(L, fun epgsql_to_common/1).

semiocast_to_common_rows([] = Rows) ->
    Rows;
semiocast_to_common_rows([Row | _] = Rows) ->
    case lists:any(fun erlang:is_tuple/1, erlang:tuple_to_list(Row)) of
        true ->
            % crash on any unsupported types
            erlang:exit({unsupported_type, Row});
        false ->
            Rows
    end.

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
    {updated, Count, semiocast_to_common_rows(Rows)};
semiocast_to_common({{copy, Count}, []}) ->
    {updated, Count};
semiocast_to_common({{delete, Count}, []}) ->
    {updated, Count};
semiocast_to_common({{delete, Count}, Rows}) ->
    {updated, Count, semiocast_to_common_rows(Rows)};
semiocast_to_common({{fetch, Count}, []}) ->
    {updated, Count};
semiocast_to_common({{move, Count}, []}) ->
    {updated, Count};
semiocast_to_common({{select, _Count}, Rows}) ->
    {selected, semiocast_to_common_rows(Rows)}; % not odbc-like
semiocast_to_common({{update, Count}, []}) ->
    {updated, Count};
semiocast_to_common({{update, Count}, Rows}) ->
    {updated, Count, semiocast_to_common_rows(Rows)};
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
semiocast_to_common({unlisten, []}) ->
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
    % list is reversed, odd
    check_list(lists:reverse(L), fun semiocast_to_common/1).

driver_async_to_common(?MODULE_EPGSQL,
                       {notification, Channel, Pid, Payload}) ->
    {notify_ok, Pid, Channel, Payload};
driver_async_to_common(?MODULE_EPGSQL,
                       {notice, #error{message = Message}}) ->
    {notify_error, Message};
driver_async_to_common(?MODULE_SEMIOCAST,
                       {notification, Pid, Channel, Payload}) ->
    {notify_ok, Pid, Channel, Payload};
driver_async_to_common(?MODULE_SEMIOCAST,
                       {notice, Fields}) ->
    Message = case lists:keyfind(message, 1, Fields) of
        false ->
            <<>>;
        {message, MessageValue} ->
            MessageValue
    end,
    {notify_error, Message}.

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

