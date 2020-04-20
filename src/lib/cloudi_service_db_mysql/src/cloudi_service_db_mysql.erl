%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI MySQL Data Module==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2009-2020 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2009-2020 Michael Truog
%%% @version 1.8.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_db_mysql).
-author('mjtruog at protonmail dot com').

-behaviour(cloudi_service).

%% external interface
-export([equery/4, equery/5,
         prepare_query/4, prepare_query/5,
         execute_query/4, execute_query/5,
         squery/3, squery/4,
         transaction/3, transaction/4]).

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/3]).

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
-define(DEFAULT_EXTERNAL_FORMAT,    erlang_string).
-define(DEFAULT_INTERNAL_INTERFACE,        native).
-define(DEFAULT_START_COMMANDS,                []). % list(SQL :: binary())
-define(DEFAULT_DEBUG,                      false). % log output for debugging
-define(DEFAULT_DEBUG_LEVEL,                trace).

% supported drivers
-define(MODULE_EONBLAST, cloudi_x_emysql_conn). % default

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
        module :: ?MODULE_EONBLAST,
        connection :: any(),
        output_type :: both | external | internal,
        external_format :: cloudi_request:external_format(),
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

% for features specific to Erlang/OTP version 21.x (and later versions)
-ifdef(ERLANG_OTP_VERSION_19).
-else.
-ifdef(ERLANG_OTP_VERSION_20).
-else.
-ifdef(OTP_RELEASE).
-define(ERLANG_OTP_VERSION_21_FEATURES, true).
-else.
-error("Erlang/OTP version invalid").
-endif.
-endif.
-endif.

% Get the stacktrace in a way that is backwards compatible
-ifdef(ERLANG_OTP_VERSION_21_FEATURES).
-define(STACKTRACE(ErrorType, Error, ErrorStackTrace),
        ErrorType:Error:ErrorStackTrace ->).
-else.
-define(STACKTRACE(ErrorType, Error, ErrorStackTrace),
        ErrorType:Error ->
            ErrorStackTrace = erlang:get_stacktrace(),).
-endif.

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

-type agent() :: cloudi:agent().
-type service_name() :: cloudi:service_name().
-type timeout_milliseconds() :: cloudi:timeout_milliseconds().
-type module_response(Result) ::
    {{ok, Result}, NewAgent :: agent()} |
    {{error, cloudi:error_reason()}, NewAgent :: agent()}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Perform an extended SQL query that does string substitution with '?'s.===
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
%% ===Perform an extended SQL query that does string substitution with '?'s using a timeout.===
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
%% ===Prepare a query with '?'s for dynamic values.===
%% The query can be used in the future with the supplied Identifier atom.
%% @end
%%-------------------------------------------------------------------------

-spec prepare_query(Agent :: agent(),
                    Name :: service_name(),
                    Identifier :: atom(),
                    Query :: string() | binary()) ->
    module_response(ok).

prepare_query(Agent, Name, Identifier, Query)
    when is_atom(Identifier), (is_binary(Query) orelse is_list(Query)) ->
    cloudi:send_sync(Agent, Name,
                     {prepare, Identifier, Query}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Prepare a query with '?'s for dynamic values using a timeout.===
%% The query can be used in the future with the supplied Identifier atom.
%% @end
%%-------------------------------------------------------------------------

-spec prepare_query(Agent :: agent(),
                    Name :: service_name(),
                    Identifier :: atom(),
                    Query :: string() | binary(),
                    Timeout :: timeout_milliseconds()) ->
    module_response(ok).

prepare_query(Agent, Name, Identifier, Query, Timeout)
    when is_atom(Identifier),
         (is_binary(Query) orelse is_list(Query)) ->
    cloudi:send_sync(Agent, Name,
                     {prepare, Identifier, Query}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Execute a query that has already been prepared.===
%% @end
%%-------------------------------------------------------------------------

-spec execute_query(Agent :: agent(),
                    Name :: service_name(),
                    Identifier :: atom(),
                    Arguments :: list()) ->
    module_response(any()).

execute_query(Agent, Name, Identifier, Arguments)
    when is_atom(Identifier), is_list(Arguments) ->
    cloudi:send_sync(Agent, Name,
                     {execute, Identifier, Arguments}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Execute a query that has already been prepared with a timeout.===
%% @end
%%-------------------------------------------------------------------------

-spec execute_query(Agent :: agent(),
                    Name :: service_name(),
                    Identifier :: atom(),
                    Arguments :: list(),
                    Timeout :: timeout_milliseconds()) ->
    module_response(any()).

execute_query(Agent, Name, Identifier, Arguments, Timeout)
    when is_atom(Identifier), is_list(Arguments) ->
    cloudi:send_sync(Agent, Name,
                     {execute, Identifier, Arguments}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Perform a simple SQL string query.===
%% @end
%%-------------------------------------------------------------------------

-spec squery(Agent :: agent(),
             Name :: service_name(),
             Query :: string() | binary()) ->
    module_response(any()).

squery(Agent, Name, Query)
    when (is_binary(Query) orelse is_list(Query)) ->
    cloudi:send_sync(Agent, Name,
                     Query).

%%-------------------------------------------------------------------------
%% @doc
%% ===Perform a simple SQL string query with a timeout.===
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
                     Query, Timeout).

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
        {ping,                     ?DEFAULT_PING},
        {timeout,                  ?DEFAULT_TIMEOUT},
        {encoding,                 ?DEFAULT_ENCODING},
        {output,                   ?DEFAULT_OUTPUT},
        {external_format,          ?DEFAULT_EXTERNAL_FORMAT},
        {internal_interface,       ?DEFAULT_INTERNAL_INTERFACE},
        {start_commands,           ?DEFAULT_START_COMMANDS},
        {debug,                    ?DEFAULT_DEBUG},
        {debug_level,              ?DEFAULT_DEBUG_LEVEL}],
    [Database, Driver, HostName, UserName, Password, Port, Ping,
     TimeoutConnect, Encoding, OutputType, ExternalFormat, Interface,
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
    true = ((OutputType =:= both) orelse
            (OutputType =:= external) orelse
            (OutputType =:= internal)),
    true = ((ExternalFormat =:= erlang_string) orelse
            (ExternalFormat =:= erlang_term) orelse
            (ExternalFormat =:= msgpack)),
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
                     Port, Database, erlang:min(TimeoutConnect, Timeout),
                     Encoding, StartCommands) of
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
                        output_type = OutputType,
                        external_format = ExternalFormat,
                        interface = Interface,
                        debug_level = DebugLogLevel}};
        {error, Reason} ->
            {stop, Reason}
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
                        [T1, T2, T3]
                            when (T1 =:= <<"prepare">>) orelse
                                 (T1 =:= <<"execute">>) orelse 
                                 (T1 =:= <<"equery">>) ->
                            T1New = erlang:binary_to_atom(T1, utf8),
                            T2New = if
                                T1New =:= execute; T1New =:= prepare ->
                                    erlang:binary_to_atom(T2, utf8);
                                true ->
                                    T2
                            end,
                            {T1New, T2New, T3};
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

cloudi_service_handle_info({ping, Ping} = Request, State, Dispatcher) ->
    case driver_ping(Ping, State) of
        ok ->
            erlang:send_after(Ping, cloudi_service:self(Dispatcher), Request),
            {noreply, State};
        error ->
            {stop, ping_failed, State}
    end;
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
request_internal({Query, Parameters}, Timeout,
                 ResponseOutputType, State)
    when is_binary(Query); is_list(Query) ->
    Response = driver_equery(Query, Parameters, Timeout,
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
    {reply, Response, State};
% external interface not meant to be compatible with other
% CloudI database services:
request_internal({prepare, Identifier, Query}, Timeout,
                 ResponseOutputType, State)
    when is_binary(Query); is_list(Query) ->
    Response = driver_prepare(Identifier, Query, Timeout,
                              ResponseOutputType, State),
    {reply, Response, State};
request_internal({execute, Identifier, Parameters}, Timeout,
                 ResponseOutputType, State) ->
    Response = driver_equery(Identifier, Parameters, Timeout,
                             ResponseOutputType, State),
    {reply, Response, State};
request_internal({equery, Query, []}, Timeout,
                 ResponseOutputType, State)
    when is_binary(Query); is_list(Query) ->
    Response = driver_squery(Query, Timeout,
                             ResponseOutputType, State),
    {reply, Response, State};
request_internal({equery, Query, Parameters}, Timeout,
                 ResponseOutputType, State)
    when is_binary(Query); is_list(Query) ->
    Response = driver_equery(Query, Parameters, Timeout,
                             ResponseOutputType, State),
    {reply, Response, State};
request_internal({squery, Query}, Timeout,
                 ResponseOutputType, State)
    when is_binary(Query); is_list(Query) ->
    Response = driver_squery(Query, Timeout,
                             ResponseOutputType, State),
    {reply, Response, State}.

-spec response_external(common_result(),
                        ExternalFormat :: cloudi_request:external_format()) ->
    binary().

% use the common interface format
response_external({updated, _Count} = Response, ExternalFormat) ->
    cloudi_response:external_format(Response, ExternalFormat);
response_external({selected, _Rows} = Response, ExternalFormat) ->
    cloudi_response:external_format(Response, ExternalFormat);
response_external({error, _} = Response, ExternalFormat) ->
    cloudi_response:external_format(Response, ExternalFormat).

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
        ?STACKTRACE(ErrorType, Error, ErrorStackTrace)
            ?LOG_ERROR("start failed ~p ~p~n~p",
                       [ErrorType, Error, ErrorStackTrace]),
            {error, Error}
    end.

driver_close(?MODULE_EONBLAST, Connection) ->
    ok = ?MODULE_EONBLAST:close_connection(Connection).

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

driver_equery(Query, Parameters, Timeout, ResponseOutputType,
              #state{module = ?MODULE_EONBLAST,
                     connection = Connection,
                     external_format = ExternalFormat,
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
    ?DRIVER_DEBUG(DebugLevel, Query, Parameters, Native),
    Response = if
        Interface =:= common; ResponseOutputType =:= external ->
            eonblast_to_common(Native);
        Interface =:= native ->
            Native
    end,
    if
        ResponseOutputType =:= internal ->
            Response;
        ResponseOutputType =:= external ->
            response_external(Response, ExternalFormat)
    end.

driver_squery(Query, Timeout, ResponseOutputType,
              #state{module = ?MODULE_EONBLAST,
                     connection = Connection,
                     external_format = ExternalFormat,
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
    ?DRIVER_DEBUG(DebugLevel, Query, Native),
    Response = if
        Interface =:= common; ResponseOutputType =:= external ->
            eonblast_to_common(Native);
        Interface =:= native ->
            Native
    end,
    if
        ResponseOutputType =:= internal ->
            Response;
        ResponseOutputType =:= external ->
            response_external(Response, ExternalFormat)
    end.

driver_prepare(Identifier, Query, Timeout, ResponseOutputType,
               #state{module = ?MODULE_EONBLAST,
                      connection = Connection,
                      external_format = ExternalFormat,
                      debug_level = DebugLevel}) ->
    QueryBinary = if
        is_list(Query) ->
            erlang:iolist_to_binary(Query);
        is_binary(Query) ->
            Query
    end,
    Response = try ?MODULE_EONBLAST:preprepare(Connection, Identifier,
                                               QueryBinary, Timeout) of
        ok ->
            ok
    catch
        _:Reason ->
            {error, Reason}
    end,
    ?LOG(DebugLevel,
         "prepare:~n"
         " ~p~n"
         " ~p~n"
         " = ~p",
         [Identifier, Query, Response]),
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

driver_ping(Timeout,
            #state{module = ?MODULE_EONBLAST,
                   connection = Connection,
                   debug_level = DebugLevel}) ->
    Result = ?MODULE_EONBLAST:ping_connection(Connection, Timeout),
    ?LOG(DebugLevel,
         "ping:~n"
         " = ~p",
         [Result]),
    Result.

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

