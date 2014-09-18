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
%%% @version 1.3.3 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_db_mysql).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_service).

%% external interface
-export([equery/4, equery/5,
         prepare_query/4, prepare_query/5,
         execute_query/4, execute_query/5,
         squery/3, squery/4,
         transaction/3, transaction/4]).

%% cloudi_service callbacks
-export([cloudi_service_init/3,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/2]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-define(DEFAULT_DATABASE,               undefined). % required argument, string
-define(DEFAULT_DRIVER,                  eonblast).
-define(DEFAULT_HOST_NAME,            "127.0.0.1").
-define(DEFAULT_USER_NAME,               "cloudi").
-define(DEFAULT_PASSWORD,                      "").
-define(DEFAULT_PORT,                        3306).
-define(DEFAULT_PING,                   undefined). % ms
-define(DEFAULT_TIMEOUT,                    20000). % ms (connect-only)
-define(DEFAULT_ENCODING,                    utf8).
-define(DEFAULT_ENDIAN,                    native). % response binary sizes
-define(DEFAULT_OUTPUT,                      both).
-define(DEFAULT_INTERNAL_INTERFACE,        native).
-define(DEFAULT_START_COMMANDS,                []). % list(SQL :: binary())
-define(DEFAULT_DEBUG,                      false). % log output for debugging
-define(DEFAULT_DEBUG_LEVEL,                trace).

% supported drivers
-define(MODULE_EONBLAST, cloudi_x_emysql_conn). % default

-type endian() :: big | little | native.

-record(state,
    {
        module :: ?MODULE_EONBLAST,
        connection :: any(),
        endian :: endian(),
        output :: both | external | internal,
        interface :: native | common,
        debug_level :: off | trace | debug | info | warn | error | fatal
    }).

% required for ?MODULE_EONBLAST
-record(pool,
    {
        pool_id,
        size,
        user,
        password,
        host,
        port,
        database,
        encoding,
        available = queue:new(),
        locked = gb_trees:empty(),
        waiting = queue:new(),
        start_cmds = [],
        conn_test_period = 0,
        connect_timeout = infinity
    }).
-record(field,
    {
        seq_num :: pos_integer(),
        catalog :: binary(),
        db :: binary(),
        table :: binary(),
        org_table :: binary(),
        name :: binary(),
        org_name :: binary(),
        type :: non_neg_integer(),
        default :: binary(),
        charset_nr :: non_neg_integer(),
        length :: non_neg_integer(),
        flags :: non_neg_integer(),
        decimals :: non_neg_integer(),
        % if row data is already decoded, why provide the decoder?
        decoder :: fun((binary()) -> any())
    }).
-record(ok_packet,
    {
        seq_num :: pos_integer(),
        affected_rows :: non_neg_integer(),
        insert_id :: integer() | binary(),
        status :: non_neg_integer(),
        warning_count :: non_neg_integer(),
        msg :: list() |
               {error, list(), any()} | {incomplete, list(), binary()}
    }).
-record(error_packet,
    {
        seq_num :: pos_integer(),
        code :: non_neg_integer(),
        status :: 0 | binary(),
        msg :: list()
    }).
-record(eof_packet,
    {
        seq_num,
        status :: non_neg_integer(),
        warning_count :: non_neg_integer()
    }).
-record(result_packet,
    {
        seq_num,
        field_list :: list(#field{}),
        rows :: list(list(any())),
        extra :: binary()
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-type count() :: non_neg_integer().
-type rows() :: list(tuple()).
-type common_result() :: {updated, count()} |
                         %{updated, count(), rows()} |
                         {selected, rows()} |
                         {error, any()}.
-export_type([common_result/0]).
-type dispatcher() :: cloudi_service:dispatcher() | cloudi:context().

%%-------------------------------------------------------------------------
%% @doc
%% ===Perform an extended SQL query that does string substitution with '?'s.===
%% @end
%%-------------------------------------------------------------------------

-spec equery(Dispatcher :: dispatcher(),
             Name :: cloudi_service:service_name(),
             Query :: string() | binary(),
             Parameters :: list()) ->
    {'ok', any()} |
    {'error', any()}.

equery(Dispatcher, Name, Query, Parameters)
    when is_list(Name), (is_binary(Query) orelse is_list(Query)),
         is_list(Parameters) ->
    cloudi:send_sync(Dispatcher, Name,
                     {Query, Parameters}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Perform an extended SQL query that does string substitution with '?'s using a timeout.===
%% @end
%%-------------------------------------------------------------------------

-spec equery(Dispatcher :: dispatcher(),
             Name :: cloudi_service:service_name(),
             Query :: string() | binary(),
             Parameters :: list(),
             Timeout :: cloudi_service:timeout_milliseconds()) ->
    {'ok', any()} |
    {'error', any()}.

equery(Dispatcher, Name, Query, Parameters, Timeout)
    when is_list(Name), (is_binary(Query) orelse is_list(Query)),
         is_list(Parameters), is_integer(Timeout) ->
    cloudi:send_sync(Dispatcher, Name,
                     {Query, Parameters}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Prepare a query with '?'s for dynamic values.===
%% The query can be used in the future with the supplied Identifier atom.
%% @end
%%-------------------------------------------------------------------------

-spec prepare_query(Dispatcher :: dispatcher(),
                    Name :: cloudi_service:service_name(),
                    Identifier :: atom(),
                    Query :: string() | binary()) ->
    {'ok', ok} |
    {'error', any()}.

prepare_query(Dispatcher, Name, Identifier, Query)
    when is_list(Name), is_atom(Identifier),
         (is_binary(Query) orelse is_list(Query)) ->
    cloudi:send_sync(Dispatcher, Name,
                     {prepare, Identifier, Query}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Prepare a query with '?'s for dynamic values using a timeout.===
%% The query can be used in the future with the supplied Identifier atom.
%% @end
%%-------------------------------------------------------------------------

-spec prepare_query(Dispatcher :: dispatcher(),
                    Name :: cloudi_service:service_name(),
                    Identifier :: atom(),
                    Query :: string() | binary(),
                    Timeout :: cloudi_service:timeout_milliseconds()) ->
    {'ok', ok} |
    {'error', any()}.

prepare_query(Dispatcher, Name, Identifier, Query, Timeout)
    when is_list(Name), is_atom(Identifier),
         (is_binary(Query) orelse is_list(Query)), is_integer(Timeout) ->
    cloudi:send_sync(Dispatcher, Name,
                     {prepare, Identifier, Query}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Execute a query that has already been prepared.===
%% @end
%%-------------------------------------------------------------------------

-spec execute_query(Dispatcher :: dispatcher(),
                    Name :: cloudi_service:service_name(),
                    Identifier :: atom(),
                    Arguments :: list()) ->
    {'ok', any()} |
    {'error', any()}.

execute_query(Dispatcher, Name, Identifier, Arguments)
    when is_list(Name), is_atom(Identifier),
         is_list(Arguments) ->
    cloudi:send_sync(Dispatcher, Name,
                     {execute, Identifier, Arguments}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Execute a query that has already been prepared with a timeout.===
%% @end
%%-------------------------------------------------------------------------

-spec execute_query(Dispatcher :: dispatcher(),
                    Name :: cloudi_service:service_name(),
                    Identifier :: atom(),
                    Arguments :: list(),
                    Timeout :: cloudi_service:timeout_milliseconds()) ->
    {'ok', any()} |
    {'error', any()}.

execute_query(Dispatcher, Name, Identifier, Arguments, Timeout)
    when is_list(Name), is_atom(Identifier),
         is_list(Arguments), is_integer(Timeout) ->
    cloudi:send_sync(Dispatcher, Name,
                     {execute, Identifier, Arguments}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Perform a simple SQL string query.===
%% @end
%%-------------------------------------------------------------------------

-spec squery(Dispatcher :: dispatcher(),
             Name :: cloudi_service:service_name(),
             Query :: string() | binary()) ->
    {'ok', any()} |
    {'error', any()}.

squery(Dispatcher, Name, Query)
    when is_list(Name), (is_binary(Query) orelse is_list(Query)) ->
    cloudi:send_sync(Dispatcher, Name,
                     Query).

%%-------------------------------------------------------------------------
%% @doc
%% ===Perform a simple SQL string query with a timeout.===
%% @end
%%-------------------------------------------------------------------------

-spec squery(Dispatcher :: dispatcher(),
             Name :: cloudi_service:service_name(),
             Query :: string() | binary(),
             Timeout :: cloudi_service:timeout_milliseconds()) ->
    {'ok', any()} |
    {'error', any()}.

squery(Dispatcher, Name, Query, Timeout)
    when is_list(Name), (is_binary(Query) orelse is_list(Query)),
         is_integer(Timeout) ->
    cloudi:send_sync(Dispatcher, Name,
                     Query, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===A list of SQL simple query for a transaction.===
%% @end
%%-------------------------------------------------------------------------

-spec transaction(Dispatcher :: dispatcher(),
                  Name :: cloudi_service:service_name(),
                  QueryList :: list(string() | binary())) ->
    {ok, ok | {error, any()}} |
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
    {ok, ok | {error, any()}} |
    {error, any()}.

transaction(Dispatcher, Name, [Query | _] = QueryList, Timeout)
    when is_list(Name), (is_binary(Query) orelse is_list(Query)),
         is_integer(Timeout) ->
    cloudi:send_sync(Dispatcher, Name,
                     QueryList, Timeout).

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, _Prefix, Dispatcher) ->
    Defaults = [
        {database,                 ?DEFAULT_DATABASE},
        {driver,                   ?DEFAULT_DRIVER},
        {hostname,                 ?DEFAULT_HOST_NAME},
        {username,                 ?DEFAULT_USER_NAME},
        {password,                 ?DEFAULT_PASSWORD},
        {port,                     ?DEFAULT_PORT},
        {ping,                     ?DEFAULT_PING},
        {timeout,                  ?DEFAULT_TIMEOUT},
        {encoding,                 ?DEFAULT_ENCODING},
        {endian,                   ?DEFAULT_ENDIAN},
        {output,                   ?DEFAULT_OUTPUT},
        {internal_interface,       ?DEFAULT_INTERNAL_INTERFACE},
        {start_commands,           ?DEFAULT_START_COMMANDS},
        {debug,                    ?DEFAULT_DEBUG},
        {debug_level,              ?DEFAULT_DEBUG_LEVEL}],
    [Database, Driver, HostName, UserName, Password, Port, Ping,
     TimeoutConnect, Encoding, Endian, Output, Interface,
     StartCommands, Debug, DebugLevel] =
        cloudi_proplists:take_values(Defaults, Args),
    true = (is_list(Database) andalso is_integer(hd(Database))),
    Module = if
        Driver =:= eonblast ->
            ?MODULE_EONBLAST
    end,
    true = (is_list(HostName) andalso is_integer(hd(HostName))),
    true = (is_list(UserName) andalso is_integer(hd(UserName))),
    true = (is_list(Password) andalso is_integer(hd(Password))),
    true = (is_integer(Port) andalso (Port > 0)),
    TimeoutMax = cloudi_service:timeout_max(Dispatcher),
    true = (Ping =:= undefined) orelse
           (is_integer(Ping) andalso (Ping > 0) andalso (Ping =< TimeoutMax)),
    true = (is_integer(TimeoutConnect) andalso (TimeoutConnect > 0)),
    true = (Encoding =:= utf8) orelse (Encoding =:= latin1),
    true = ((Endian =:= big) orelse
            (Endian =:= little) orelse
            (Endian =:= native)),
    true = ((Output =:= both) orelse
            (Output =:= external) orelse
            (Output =:= internal)),
    true = ((Interface =:= native) orelse
            (Interface =:= common)),
    true = ((Debug =:= true) orelse
            (Debug =:= false)),
    true = ((DebugLevel =:= trace) orelse
            (DebugLevel =:= debug) orelse
            (DebugLevel =:= info) orelse
            (DebugLevel =:= warn) orelse
            (DebugLevel =:= error) orelse
            (DebugLevel =:= fatal)),
    case driver_open(Module, HostName, UserName, Password,
                     Port, Database, TimeoutConnect, Encoding,
                     StartCommands) of
        {ok, Connection} ->
            cloudi_service:subscribe(Dispatcher, Database),
            DebugLogLevel = if
                Debug =:= false ->
                    off;
                Debug =:= true ->
                    DebugLevel
            end,
            if
                Ping =:= undefined ->
                    ok;
                is_integer(Ping) ->
                    erlang:send_after(Ping, cloudi_service:self(Dispatcher),
                                      {ping, Ping})
            end,
            {ok, #state{module = Module,
                        connection = Connection,
                        endian = Endian,
                        output = Output,
                        interface = Interface,
                        debug_level = DebugLogLevel}};
        {error, Reason} ->
            {stop, Reason}
    end.

cloudi_service_handle_request(_Type, _Name, _Pattern, _RequestInfo, Request,
                              Timeout, _Priority, _TransId, _Pid,
                              #state{output = Output} = State,
                              _Dispatcher) ->
    case Request of
        {Query, []}
            when (Output =/= external),
                 (is_binary(Query) orelse is_list(Query)) ->
            Response = driver_squery(internal,
                                     Query, Timeout, State),
            {reply, Response, State};
        {Query, Parameters}
            when (Output =/= external),
                 (is_binary(Query) orelse is_list(Query)) ->
            Response = driver_equery(Query, Parameters, Timeout, State),
            {reply, Response, State};
        [Query | _] = QueryList
            when (Output =/= external),
                 (is_binary(Query) orelse is_list(Query)) ->
            Response = driver_with_transaction(QueryList, Timeout, State),
            {reply, Response, State};
        Query
            when is_binary(Query) ->
            ResponseOutput = if
                Output =:= internal ->
                    internal;
                Output =:= external; Output =:= both ->
                    external
            end,
            Response = driver_squery(ResponseOutput,
                                     Query, Timeout, State),
            {reply, Response, State};
        Query
            when (Output =/= external),
                 is_list(Query) ->
            Response = driver_squery(internal,
                                     Query, Timeout, State),
            {reply, Response, State};

        % older requests not meant to be compatible with other
        % CloudI database services

        {prepare, Identifier, Query}
            when (Output =/= external),
                 (is_binary(Query) orelse is_list(Query)) ->
            Response = driver_prepare(Identifier, Query, Timeout, State),
            {reply, Response, State};
        {execute, Identifier, Parameters}
            when (Output =/= external) ->
            Response = driver_equery(Identifier, Parameters, Timeout, State),
            {reply, Response, State};
        {equery, Query, []}
            when (Output =/= external),
                 (is_binary(Query) orelse is_list(Query)) ->
            Response = driver_squery(internal,
                                     Query, Timeout, State),
            {reply, Response, State};
        {equery, Query, Parameters}
            when (Output =/= external),
                 (is_binary(Query) orelse is_list(Query)) ->
            Response = driver_equery(Query, Parameters, Timeout, State),
            {reply, Response, State};
        {squery, Query}
            when (Output =/= external),
                 (is_binary(Query) orelse is_list(Query)) ->
            Response = driver_squery(internal,
                                     Query, Timeout, State),
            {reply, Response, State}
    end.

cloudi_service_handle_info({ping, Ping} = Request, State, Dispatcher) ->
    case driver_ping(Ping, State) of
        ok ->
            erlang:send_after(Ping, cloudi_service:self(Dispatcher), Request),
            {noreply, State};
        error ->
            {stop, ping_failed, State}
    end;
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

%% interface adapter

driver_open(?MODULE_EONBLAST, HostName, UserName, Password,
            Port, Database, TimeoutConnect, Encoding,
            StartCommands) ->
    Pool = #pool{pool_id = ?MODULE,
                 size = 1,
                 host = HostName,
                 user = UserName,
                 password = Password,
                 port = Port,
                 database = Database,
                 encoding = Encoding,
                 start_cmds = StartCommands,
                 connect_timeout = TimeoutConnect},
    try ?MODULE_EONBLAST:open_connection(Pool) of
        Connection ->
            % do not bother to add to Pool#pool.available
            % (not using the ?MODULE_EONBLAST connection manager process)
            {ok, Connection}
    catch
        ErrorType:Error ->
            ?LOG_ERROR("start failed ~p ~p~n~p",
                       [ErrorType, Error, erlang:get_stacktrace()]),
            {error, Error}
    end.

driver_close(?MODULE_EONBLAST, Connection) ->
    ok = ?MODULE_EONBLAST:close_connection(Connection).

% only internal usage, due to relying on an erlang list
driver_with_transaction(L, Timeout, State) ->
    case driver_squery(internal,
                       [<<"BEGIN;">> | L] ++ [<<"COMMIT;">>], Timeout,
                       State#state{interface = common}) of
        {updated, 0} ->
            ok;
        {error, _} = Error ->
            driver_squery(internal, <<"ROLLBACK;">>,
                          Timeout, State),
            Error
    end.

% only internal usage, due to relying on an erlang list
driver_equery(Query, Parameters, Timeout,
              #state{module = ?MODULE_EONBLAST,
                     connection = Connection,
                     interface = Interface,
                     debug_level = DebugLevel}) ->
    NewQuery = if
        is_atom(Query) ->
            Query;
        is_list(Query) ->
            erlang:iolist_to_binary(Query);
        is_binary(Query) ->
            Query
    end,
    Native = ?MODULE_EONBLAST:execute(Connection,
                                      NewQuery, Parameters, Timeout),
    if
        DebugLevel =:= off ->
            ok;
        true ->
            driver_debug(DebugLevel, Query, Parameters, Native)
    end,
    if
        Interface =:= common ->
            eonblast_to_common(Native);
        Interface =:= native ->
            Native
    end.

% internal or external
driver_squery(internal, Query, Timeout,
              #state{module = ?MODULE_EONBLAST,
                     connection = Connection,
                     interface = Interface,
                     debug_level = DebugLevel}) ->
    QueryBinary = if
        is_list(Query) ->
            erlang:iolist_to_binary(Query);
        is_binary(Query) ->
            Query
    end,
    Native = ?MODULE_EONBLAST:execute(Connection,
                                      QueryBinary, [], Timeout),
    if
        DebugLevel =:= off ->
            ok;
        true ->
            driver_debug(DebugLevel, Query, Native)
    end,
    if
        Interface =:= common ->
            eonblast_to_common(Native);
        Interface =:= native ->
            Native
    end;
driver_squery(external, Query, Timeout,
              #state{module = ?MODULE_EONBLAST,
                     connection = Connection,
                     endian = Endian,
                     debug_level = DebugLevel}) ->
    QueryBinary = if
        is_list(Query) ->
            erlang:iolist_to_binary(Query);
        is_binary(Query) ->
            Query
    end,
    Native = ?MODULE_EONBLAST:execute(Connection,
                                      QueryBinary, [], Timeout),
    if
        DebugLevel =:= off ->
            ok;
        true ->
            driver_debug(DebugLevel, Query, Native)
    end,
    response_external(eonblast_to_common(Native), Query, Endian).

driver_prepare(Identifier, Query, Timeout,
               #state{module = ?MODULE_EONBLAST,
                      connection = Connection,
                      debug_level = DebugLevel}) ->
    QueryBinary = if
        is_list(Query) ->
            erlang:iolist_to_binary(Query);
        is_binary(Query) ->
            Query
    end,
    Result = try ?MODULE_EONBLAST:preprepare(Connection, Identifier,
                                             QueryBinary, Timeout) of
        ok ->
            ok
    catch
        _:Reason ->
            {error, Reason}
    end,
    if
        DebugLevel =:= off ->
            ok;
        true ->
            driver_debug_log(DebugLevel,
                             "prepare:~n"
                             " ~p~n"
                             " ~p~n"
                             " = ~p",
                             [Identifier, Query, Result])
    end,
    Result.

driver_ping(Timeout,
            #state{module = ?MODULE_EONBLAST,
                   connection = Connection,
                   debug_level = DebugLevel}) ->
    Result = ?MODULE_EONBLAST:ping_connection(Connection, Timeout),
    if
        DebugLevel =:= off ->
            ok;
        true ->
            driver_debug_log(DebugLevel,
                             "ping:~n"
                             " = ~p",
                             [Result])
    end,
    Result.

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
                     "SQL(equery):~n"
                     " ~p~n"
                     " ~p~n"
                     " = ~p",
                     [Query, Parameters, Result]).

driver_debug(Level, Query, Result) ->
    driver_debug_log(Level,
                     "SQL(squery):~n"
                     " ~p~n"
                     " = ~p",
                     [Query, Result]).

eonblast_to_common(L) when is_list(L) ->
    [H | _] = lists:reverse(L),
    eonblast_to_common(H);
eonblast_to_common(#ok_packet{affected_rows = I}) ->
    {updated, I};
eonblast_to_common(#result_packet{rows = Rows}) ->
    NewRows = [erlang:list_to_tuple(L) || L <- Rows],
    {selected, NewRows};
eonblast_to_common(#eof_packet{status = Status}) ->
    {error, erlang:list_to_binary(?MODULE_EONBLAST:hstate(Status))};
eonblast_to_common(#error_packet{status = Status,
                                 msg = Message}) ->
    Reason = if
        is_integer(Status) ->
            erlang:iolist_to_binary([erlang:integer_to_list(Status), ": ",
                                     Message]);
        is_binary(Status) ->
            erlang:iolist_to_binary([Status, ": ",
                                     Message])
    end,
    {error, Reason}.

