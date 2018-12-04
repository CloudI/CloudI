%% @doc PostgreSQL connection (high level functions).
-module(pgsql_connection).
-vsn("9").
-behaviour(gen_server).
-include("pgsql_internal.hrl").

-export([
    % API
    open/1,
    open/2,
    open/3,
    open/4,
    open/5,

    close/1,

    % Native API
    simple_query/2,
    simple_query/3,
    simple_query/4,

    extended_query/3,
    extended_query/4,
    extended_query/5,

    batch_query/3,
    batch_query/4,
    batch_query/5,

    fold/4,
    fold/5,
    fold/6,
    fold/7,

    map/3,
    map/4,
    map/5,
    map/6,

    foreach/3,
    foreach/4,
    foreach/5,
    foreach/6,
 
    send_copy_data/2,
    send_copy_end/1,
    
    % Cancel current query
    cancel/1,

    % Subscribe to notifications.
    subscribe/2,
    unsubscribe/2,

    % Compatibility (deprecated) API
    sql_query/2,
    sql_query/3,
    sql_query/4,

    param_query/3,
    param_query/4,
    param_query/5,
    
    convert_statement/1,

    % supervisor API
    start_link/1,

    % gen_server API
    init/1,
    handle_call/3,
    handle_cast/2,
    code_change/3,
    handle_info/2,
    terminate/2
    ]).

-export_type([
    row/0,
    rows/0,
    result_tuple/0,
    pgsql_connection/0]).

%%--------------------------------------------------------------------
%% Default settings
%%--------------------------------------------------------------------

-define(REQUEST_TIMEOUT, infinity). 
-define(DEFAULT_HOST, "127.0.0.1").
-define(DEFAULT_PORT, 5432).
-define(DEFAULT_USER, "storage").
-define(DEFAULT_PASSWORD, "").
-define(DEFAULT_MAX_ROWS_STEP, 1000).

-define(TIMEOUT_GEN_SERVER_CALL_DELTA, 5000).

%% ========================================================================= %%
%% Types
%% ========================================================================= %%

-type pgsql_connection() :: {pgsql_connection, pid()}.

-type n_rows() :: integer().
-type row() :: tuple().
-type rows() :: [row()].
-type odbc_result_tuple() :: {updated, n_rows()} | {updated, n_rows(), rows()} | {selected, rows()}.
% Column descriptions are returned with return_descriptions query option, an
% experimental API. Column name (unicode:unicode_binary()) is the second element
% of the tuple.
-type column_description() :: #row_description_field{}.
-type column_descriptions() :: [column_description()].

-type result_tuple() ::
    {'begin' | commit | 'do' | listen | unlisten | notify | rollback | set | {declare, cursor} | {lock, table} | comment, []}
    | {{insert, integer(), integer()}, rows()}
    | {{copy | delete | fetch | move | select | update, integer()}, rows()}
    | {{insert, integer(), integer()}, column_descriptions(), rows()}
    | {{copy | delete | fetch | move | select | update, integer()}, column_descriptions(), rows()}
    | {{alter | create | drop, atom()} | {start, transaction}, []}
    | {copy_in, [pgsql_format()]}.

% A gen_tcp or SSL socket.
-type prim_socket() :: port() | tuple().
-type socket_module() :: gen_tcp | ssl.
-type socket() :: {socket_module(), prim_socket()}.

% driver options.
-type open_option() ::
        {host, inet:ip_address() | inet:hostname()} % default: ?DEFAULT_HOST
    |   {port, integer()}                       % default: ?DEFAULT_PORT
    |   {database, iodata()}                    % default: user
    |   {user, iodata()}                        % default: ?DEFAULT_USER
    |   {password, iodata()}                    % default: none
    |   {fetch_oid_map, boolean()}              % default: true
    |   {ssl, boolean()}                        % default: false
    |   {ssl_options, [ssl:ssl_option()]}       % default: []
    |   {reconnect, boolean()}                  % default: true
    |   {application_name, atom() | iodata()}   % default: node()
    |   {timezone, iodata() | undefined}        % default: undefined (not set)
    |   {async, pid()}                          % subscribe to notifications (default: no)
    |   proplists:property().                   % undocumented !
-type open_options() :: [open_option()].
-type query_option() ::
        {max_rows_step, non_neg_integer()}      % default: ?DEFAULT_MAX_ROWS_STEP
    |   {retry, boolean()}                      % default: false
    |   {return_descriptions, boolean()}        % default: false
    |   {datetime_float_seconds, round | always | as_available} % default: as_available
    |   proplists:property().                   % undocumented.
-type query_options() :: [query_option()].

% gen_server:call From tag.
-type from() :: {pid(), term()}.

-record(state, {
      options           :: open_options(),
      socket            :: socket() | closed,   %% gen_tcp or ssl socket
      subscribers       :: [{pid(), reference()}],
      backend_procid    :: integer() | undefined,
      backend_secret    :: integer() | undefined,
      integer_datetimes :: boolean() | undefined,
      oidmap            :: pgsql_oid_map(),
      current           :: {tuple(), reference(), from()} | undefined | {tuple(), from()},
      pending           :: [{tuple(), reference(), from()}] | [{tuple(), from()}],
      statement_timeout :: non_neg_integer() | undefined    %% to pipeline statements with timeouts, currently unused
     }).

-define(MESSAGE_HEADER_SIZE, 5).

% pgsql extended query states.
-type extended_query_mode() :: all | batch | {cursor, non_neg_integer()}.
-type extended_query_loop_state() ::
        % expect parse_complete message
        parse_complete
    |   {parse_complete_with_params, extended_query_mode(), [any()]}
        % expect parameter_description
    |   {parameter_description_with_params, extended_query_mode(), [any()]}
        % expect row_description or no_data
    |   pre_bind_row_description
        % expect bind_complete
    |   bind_complete
        % expect row_description or no_data
    |   row_description
        % expect data_row or command_complete
    |   {rows, [#row_description_field{}]}
        % expect command_complete
    |   no_data
        % expect ready_for_query
    |   {result, any()}
        % expect copy_data or copy_done
    |   {copy, [pgsql_format()]}.

-define(binary_to_integer(Bin), list_to_integer(binary_to_list(Bin))).

%%--------------------------------------------------------------------
%% @doc Open a connection to a database, throws an error if it failed.
%% 
-spec open(iodata() | open_options()) -> pgsql_connection().
open([Option | _OptionsT] = Options) when is_tuple(Option) orelse is_atom(Option) ->
    open0(Options);
open(Database) ->
    open(Database, ?DEFAULT_USER).

%%--------------------------------------------------------------------
%% @doc Open a connection to a database, throws an error if it failed.
%% 
-spec open(iodata(), iodata()) -> pgsql_connection().
open(Database, User) ->
    open(Database, User, ?DEFAULT_PASSWORD).

%%--------------------------------------------------------------------
%% @doc Open a connection to a database, throws an error if it failed.
%% 
-spec open(iodata(), iodata(), iodata()) -> pgsql_connection().
open(Database, User, Password) ->
    open(?DEFAULT_HOST, Database, User, Password).

%%--------------------------------------------------------------------
%% @doc Open a connection to a database, throws an error if it failed.
%% 
-spec open(string(), string(), string(), string()) -> pgsql_connection().
open(Host, Database, User, Password) ->
    open(Host, Database, User, Password, []).

%%--------------------------------------------------------------------
%% @doc Open a connection to a database, throws an error if it failed.
%% 
-spec open(string(), string(), string(), string(), open_options()) -> pgsql_connection().
open(Host, Database, User, Password, Options0) ->
    Options = [{host, Host}, {database, Database}, {user, User}, {password, Password} | Options0],
    open0(Options).

open0(Options) ->
    case pgsql_connection_sup:start_child(Options) of
        {ok, Pid} ->
            {pgsql_connection, Pid};
        {error, Error} -> throw(Error)
    end.

%%--------------------------------------------------------------------
%% @doc Close a connection.
%% 
-spec close(pgsql_connection()) -> ok.
close({pgsql_connection, Pid}) ->
    MonitorRef = erlang:monitor(process, Pid),
    exit(Pid, shutdown),
    receive {'DOWN', MonitorRef, process, Pid, _Info} -> ok end.

%%--------------------------------------------------------------------
%% @doc Perform a query.
%% This function creates a statement and runs step as many times as
%% required. The result is:
%% <ul>
%% <li>``{selected, Rows}'' if the query was a SELECT query.</li>
%% <li>``{updated, NbRows}'' if the query was not a SELECT query.</li>
%% </ul>
%% (the return types are compatible with ODBC's sql_query function).
%% 
-spec sql_query(iodata(), pgsql_connection()) -> odbc_result_tuple() | {error, any()}.
sql_query(Query, Connection) ->
    sql_query(Query, [], Connection).

-spec sql_query(iodata(), query_options(), pgsql_connection()) -> odbc_result_tuple() | {error, any()}.
sql_query(Query, QueryOptions, Connection) ->
    sql_query(Query, QueryOptions, ?REQUEST_TIMEOUT, Connection).

-spec sql_query(iodata(), query_options(), timeout(), pgsql_connection()) -> odbc_result_tuple() | {error, any()}.
sql_query(Query, QueryOptions, Timeout, Connection) ->
    Result = simple_query(Query, QueryOptions, Timeout, Connection),
    native_to_odbc(Result).

%%--------------------------------------------------------------------
%% @doc Perform a query with parameters.
%%
-spec param_query(iodata(), [any()], pgsql_connection()) -> odbc_result_tuple() | {error, any()}.
param_query(Query, Parameters, Connection) ->
    param_query(Query, Parameters, [], Connection).

-spec param_query(iodata(), [any()], query_options(), pgsql_connection()) -> odbc_result_tuple() | {error, any()}.
param_query(Query, Parameters, QueryOptions, Connection) ->
    param_query(Query, Parameters, QueryOptions, ?REQUEST_TIMEOUT, Connection).

-spec param_query(iodata(), [any()], query_options(), timeout(), pgsql_connection()) -> odbc_result_tuple() | {error, any()}.
param_query(Query, Parameters, QueryOptions, Timeout, Connection) ->
    ConvertedQuery = convert_statement(Query),
    Result = extended_query(ConvertedQuery, Parameters, QueryOptions, Timeout, Connection),
    native_to_odbc(Result).

%%--------------------------------------------------------------------
%% @doc Perform a simple query.
%%
-spec simple_query(iodata(), pgsql_connection()) ->
    result_tuple() | {error, any()} | [result_tuple() | {error, any()}].
simple_query(Query, Connection) ->
    simple_query(Query, [], Connection).

-spec simple_query(iodata(), query_options(), pgsql_connection()) ->
    result_tuple() | {error, any()} | [result_tuple() | {error, any()}].
simple_query(Query, QueryOptions, Connection) ->
    simple_query(Query, QueryOptions, ?REQUEST_TIMEOUT, Connection).

%% @doc Perform a simple query with query options and a timeout.
%% Issuing SET statement_timeout or altering default in postgresql.conf
%% will confuse timeout logic and such manual handling of statement_timeout
%% should not be mixed with calls to simple_query/4.
%%
-spec simple_query(iodata(), query_options(), timeout(), pgsql_connection()) ->
    result_tuple() | {error, any()} | [result_tuple() | {error, any()}].
simple_query(Query, QueryOptions, Timeout, {pgsql_connection, ConnectionPid}) ->
    call_and_retry(ConnectionPid, {simple_query, Query, QueryOptions, Timeout}, proplists:get_bool(retry, QueryOptions), adjust_timeout(Timeout)).

%%--------------------------------------------------------------------
%% @doc Perform a query with parameters.
%%
-spec extended_query(iodata(), [any()], pgsql_connection()) -> result_tuple() | {error, any()}.
extended_query(Query, Parameters, Connection) ->
    extended_query(Query, Parameters, [], Connection).

-spec extended_query(iodata(), [any()], query_options(), pgsql_connection()) -> result_tuple() | {error, any()}.
extended_query(Query, Parameters, QueryOptions, Connection) ->
    extended_query(Query, Parameters, QueryOptions, ?REQUEST_TIMEOUT, Connection).

%% @doc Perform an extended query with query options and a timeout.
%% See discussion of simple_query/4 about timeout values.
%%
-spec extended_query(iodata(), [any()], query_options(), timeout(), pgsql_connection()) -> result_tuple() | {error, any()}.
extended_query(Query, Parameters, QueryOptions, Timeout, {pgsql_connection, ConnectionPid}) ->
    call_and_retry(ConnectionPid, {extended_query, Query, Parameters, QueryOptions, Timeout}, proplists:get_bool(retry, QueryOptions), adjust_timeout(Timeout)).

%%--------------------------------------------------------------------
%% @doc Perform a query several times with parameters.
%%
-spec batch_query(iodata(), [any()], pgsql_connection()) ->
    [result_tuple()] | {error, any()} | [result_tuple() | {error, any()}].
batch_query(Query, Parameters, Connection) ->
    batch_query(Query, Parameters, [], Connection).

-spec batch_query(iodata(), [any()], query_options(), pgsql_connection()) ->
    [result_tuple()] | {error, any()} | [result_tuple() | {error, any()}].
batch_query(Query, Parameters, QueryOptions, Connection) ->
    batch_query(Query, Parameters, QueryOptions, ?REQUEST_TIMEOUT, Connection).

-spec batch_query(iodata(), [any()], query_options(), timeout(), pgsql_connection()) ->
    [result_tuple()] | {error, any()} | [result_tuple() | {error, any()}].
batch_query(Query, Parameters, QueryOptions, Timeout, {pgsql_connection, ConnectionPid}) ->
    call_and_retry(ConnectionPid, {batch_query, Query, Parameters, QueryOptions, Timeout}, proplists:get_bool(retry, QueryOptions), adjust_timeout(Timeout)).

%%--------------------------------------------------------------------
%% @doc Fold over results of a given query.
%% The function is evaluated within the connection's process.
%%
-spec fold(fun((tuple(), Acc) -> Acc), Acc, iodata(), pgsql_connection()) -> {ok, Acc} | {error, any()}.
fold(Function, Acc0, Query, Connection) ->
    fold(Function, Acc0, Query, [], Connection).

-spec fold(fun((tuple(), Acc) -> Acc), Acc, iodata(), [any()], pgsql_connection()) -> {ok, Acc} | {error, any()}.
fold(Function, Acc0, Query, Parameters, Connection) ->
    fold(Function, Acc0, Query, Parameters, [], Connection).

-spec fold(fun((tuple(), Acc) -> Acc), Acc, iodata(), [any()], query_options(), pgsql_connection()) -> {ok, Acc} | {error, any()}.
fold(Function, Acc0, Query, Parameters, QueryOptions, Connection) ->
    fold(Function, Acc0, Query, Parameters, QueryOptions, ?REQUEST_TIMEOUT, Connection).

-spec fold(fun((tuple(), Acc) -> Acc), Acc, iodata(), [any()], query_options(), timeout(), pgsql_connection()) -> {ok, Acc} | {error, any()}.
fold(Function, Acc0, Query, Parameters, QueryOptions, Timeout, {pgsql_connection, ConnectionPid}) ->
    call_and_retry(ConnectionPid, {fold, Query, Parameters, Function, Acc0, QueryOptions, Timeout}, proplists:get_bool(retry, QueryOptions), adjust_timeout(Timeout)).

%%--------------------------------------------------------------------
%% @doc Map results of a given query.
%% The function is evaluated within the connection's process.
%%
-spec map(fun((tuple()) -> Any), iodata(), pgsql_connection()) -> {ok, [Any]} | {error, any()}.
map(Function, Query, Connection) ->
    map(Function, Query, [], Connection).

-spec map(fun((tuple()) -> Any), iodata(), [any()], pgsql_connection()) -> {ok, [Any]} | {error, any()}.
map(Function, Query, Parameters, Connection) ->
    map(Function, Query, Parameters, [], Connection).

-spec map(fun((tuple()) -> Any), iodata(), [any()], query_options(), pgsql_connection()) -> {ok, [Any]} | {error, any()}.
map(Function, Query, Parameters, QueryOptions, Connection) ->
    map(Function, Query, Parameters, QueryOptions, ?REQUEST_TIMEOUT, Connection).

-spec map(fun((tuple()) -> Any), iodata(), [any()], query_options(), timeout(), pgsql_connection()) -> {ok, [Any]} | {error, any()}.
map(Function, Query, Parameters, QueryOptions, Timeout, {pgsql_connection, ConnectionPid}) ->
    call_and_retry(ConnectionPid, {map, Query, Parameters, Function, QueryOptions, Timeout}, proplists:get_bool(retry, QueryOptions), adjust_timeout(Timeout)).

%%--------------------------------------------------------------------
%% @doc Iterate on results of a given query.
%% The function is evaluated within the connection's process.
%%
-spec foreach(fun((tuple()) -> any()), iodata(), pgsql_connection()) -> ok | {error, any()}.
foreach(Function, Query, Connection) ->
    foreach(Function, Query, [], Connection).

-spec foreach(fun((tuple()) -> any()), iodata(), [any()], pgsql_connection()) -> ok | {error, any()}.
foreach(Function, Query, Parameters, Connection) ->
    foreach(Function, Query, Parameters, [], Connection).

-spec foreach(fun((tuple()) -> any()), iodata(), [any()], query_options(), pgsql_connection()) -> ok | {error, any()}.
foreach(Function, Query, Parameters, QueryOptions, Connection) ->
    foreach(Function, Query, Parameters, QueryOptions, ?REQUEST_TIMEOUT, Connection).

-spec foreach(fun((tuple()) -> any()), iodata(), [any()], query_options(), timeout(), pgsql_connection()) -> ok | {error, any()}.
foreach(Function, Query, Parameters, QueryOptions, Timeout, {pgsql_connection, ConnectionPid}) ->
    call_and_retry(ConnectionPid, {foreach, Query, Parameters, Function, QueryOptions, Timeout}, proplists:get_bool(retry, QueryOptions), adjust_timeout(Timeout)).

%%--------------------------------------------------------------------
%% @doc Send some binary data after starting a COPY
%%
-spec send_copy_data(iodata(), pgsql_connection()) -> ok | {error, any()}.
send_copy_data(Data, {pgsql_connection, ConnectionPid}) ->
    call_and_retry(ConnectionPid, {send_copy_data, Data}, false, infinity).

%%--------------------------------------------------------------------
%% @doc Finish a COPY
%%
-spec send_copy_end(pgsql_connection()) -> {copy, integer()} | {error, any()}.
send_copy_end({pgsql_connection, ConnectionPid}) ->
    call_and_retry(ConnectionPid, {send_copy_end}, false, infinity).

%%--------------------------------------------------------------------
%% @doc Cancel the current query.
%%
-spec cancel(pgsql_connection()) -> ok | {error, any()}.
cancel({pgsql_connection, ConnectionPid}) ->
    gen_server:call(ConnectionPid, cancel, ?REQUEST_TIMEOUT).

%%--------------------------------------------------------------------
%% @doc Subscribe to notifications. Subscribers get notifications as
%% <code>{pgsql, Connection, {notification, ProcID, Channel, Payload}}</code>
%%
-spec subscribe(pid(), pgsql_connection()) -> ok | {error, any()}.
subscribe(Pid, {pgsql_connection, ConnectionPid}) ->
    gen_server:cast(ConnectionPid, {subscribe, Pid}).

%%--------------------------------------------------------------------
%% @doc Unsubscribe to notifications.
%%
-spec unsubscribe(pid(), pgsql_connection()) -> ok | {error, any()}.
unsubscribe(Pid, {pgsql_connection, ConnectionPid}) ->
    gen_server:cast(ConnectionPid, {unsubscribe, Pid}).

%%====================================================================
%% Supervisor API
%%====================================================================

%%--------------------------------------------------------------------
%% Starts a pgsql_connection process.
%%
-spec start_link(open_options()) -> {ok, pid()} | {error, any()}.
start_link(Options) ->
    gen_server:start_link(?MODULE, Options, []).

%% ========================================================================= %%
%% gen_server API
%% ========================================================================= %%

%%--------------------------------------------------------------------
%% @doc gen_server's init callback.
%%
-spec init(open_options()) -> {ok, #state{}} | {stop, any()}.
init(Options) ->
    process_flag(trap_exit, true),
    Subscribers = case lists:keyfind(async, 1, Options) of
        false -> [];
        {async, SubscriberPid} -> do_subscribe(SubscriberPid, [])
    end,
    State0 = #state{
        options = Options,
        socket = closed,
        subscribers = Subscribers,
        oidmap = gb_trees:from_orddict(orddict:from_list(?PG_TYPE_H_TYPES_DICT)),
        pending = []
    },
    case pgsql_open(State0) of
        {ok, State1} ->
            set_active_once(State1),
            {ok, State1};
        {error, OpenErrorReason} -> {stop, OpenErrorReason}
    end.

%%--------------------------------------------------------------------
%% @doc Handle a synchronous message.
%%
-spec handle_call(any(), from(), #state{}) -> {noreply, #state{}} | {reply, any(), #state{}}.
handle_call({do_query, Command}, From, #state{} = State0) ->
    State1 = do_query(Command, From, State0),
    {noreply, State1};
handle_call(cancel, _From, #state{socket = closed} = State0) ->
    {reply, {error, closed}, State0};
handle_call(cancel, _From, #state{} = State0) ->
    Result = oob_cancel(State0),
    {reply, Result, State0}.

%%--------------------------------------------------------------------
%% @doc Handle an asynchronous message.
%%
-spec handle_cast(any(), #state{}) -> {noreply, #state{}}.
handle_cast({set_parameter, Key, Value}, State0) ->
    State1 = handle_parameter(Key, Value, sync, State0),
    {noreply, State1};
handle_cast({socket_closed, Socket}, #state{socket = Socket} = State) ->
    {noreply, State#state{socket = closed}};
handle_cast({socket_closed, _ClosedSocket}, #state{socket = _OtherSocket} = State) ->
    {noreply, State};
handle_cast({command_completed, CurrentCommand}, #state{} = State0) ->
    State1 = command_completed(CurrentCommand, State0),
    {noreply, State1};
handle_cast({subscribe, Pid}, #state{subscribers = Subscribers0} = State0) ->
    Subscribers1 = do_subscribe(Pid, Subscribers0),
    State1 = State0#state{subscribers = Subscribers1},
    {noreply, State1};
handle_cast({unsubscribe, Pid}, #state{subscribers = Subscribers0} = State0) ->
    Subscribers1 = do_unsubscribe(Pid, Subscribers0),
    State1 = State0#state{subscribers = Subscribers1},
    {noreply, State1}.

%%--------------------------------------------------------------------
%% @doc handle system messages.
%%
-spec handle_info(any(), #state{}) -> {noreply, #state{}} | {stop, any(), #state{}}.
handle_info({'EXIT', _From, normal}, State) ->
    {noreply, State};
handle_info({'EXIT', _From, Reason}, State) ->
    {stop, Reason, State};
handle_info({'DOWN', MonitorRef, process, _Pid, _Info}, #state{subscribers = Subscribers0} = State0) ->
    Subscribers1 = lists:keydelete(MonitorRef, 2, Subscribers0),
    State1 = State0#state{subscribers = Subscribers1},
    {noreply, State1};
handle_info({_Tag, Socket, Data}, #state{socket = {_SocketModule, Socket}} = State0) ->
    State1 = process_active_data(Data, State0),
    set_active_once(State1),
    {noreply, State1};
handle_info({ClosedTag, Socket}, #state{socket = {_SocketModule, Socket}} = State0) when ClosedTag =:= tcp_closed orelse ClosedTag =:= ssl_closed ->
    State1 = State0#state{socket = closed},
    {noreply, State1};
handle_info({ErrorTag, Socket, _SocketError}, #state{socket = {SocketModule, Socket}} = State0) when ErrorTag =:= tcp_error orelse ErrorTag =:= ssl_error ->
    _ = SocketModule:close(Socket),
    State1 = State0#state{socket = closed},
    {noreply, State1};
handle_info({Tag, _OtherSocket, _Data}, State0) when Tag =:= tcp orelse Tag =:= ssl ->
    {noreply, State0};
handle_info({ClosedTag, _OtherSocket}, State0) when ClosedTag =:= tcp_closed orelse ClosedTag =:= ssl_closed ->
    {noreply, State0};
handle_info({ErrorTag, _OtherSocket, _SocketError}, State0) when ErrorTag =:= tcp_error orelse ErrorTag =:= ssl_error ->
    {noreply, State0}.

%%--------------------------------------------------------------------
%% @doc handle code change.
%%
-spec code_change(string() | {down, string()}, any(), any()) -> {ok, #state{}}.
code_change(Vsn, State, Extra) ->
    error_logger:info_msg("~p: unknown code_change (~p, ~p, ~p)~n", [?MODULE, Vsn, State, Extra]),
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc handle termination.
%%
-spec terminate(any(), #state{}) -> ok.
terminate(_Reason, #state{socket = closed}) ->
    ok;
terminate(_Reason, #state{socket = {SocketModule, Socket}}) ->
    SocketModule:close(Socket),
    ok.

%%====================================================================
%% Private functions
%%====================================================================

%%--------------------------------------------------------------------
%% @doc Actually open (or re-open) the connection.
%%
pgsql_open(#state{options = Options} = State0) ->
    Host = proplists:get_value(host, Options, ?DEFAULT_HOST),
    Port = proplists:get_value(port, Options, ?DEFAULT_PORT),
    % First open a TCP connection
    case gen_tcp:connect(Host, Port, [binary, {packet, raw}, {active, false}]) of
        {ok, Sock} ->
            case pgsql_setup(Sock, State0) of
                {ok, State1} ->
                    case proplists:get_value(fetch_oid_map, Options, true) of
                        true ->
                            State2 = update_oid_map(State1),
                            {ok, State2};
                        false ->
                            {ok, State1}
                    end;
                {error, _} = SetupError -> SetupError
            end;
        {error, _} = ConnectError -> ConnectError
    end.

%%--------------------------------------------------------------------
%% @doc Setup the connection, handling the authentication handshake.
%%
-spec pgsql_setup(port(), #state{}) -> {ok, #state{}} | {error, any()}.
pgsql_setup(Sock, #state{options = Options} = State0) ->
    case proplists:get_bool(ssl, Options) of
        false ->
            pgsql_setup_startup(State0#state{socket = {gen_tcp, Sock}});
        true ->
            pgsql_setup_ssl(Sock, State0)
    end.

pgsql_setup_ssl(Sock, #state{options = Options} = State0) ->
    SSLRequestMessage = pgsql_protocol:encode_ssl_request_message(),
    case gen_tcp:send(Sock, SSLRequestMessage) of
        ok ->
            case gen_tcp:recv(Sock, 1) of
                {ok, <<$S>>} ->
                    % upgrade socket.
                    SSLOptions = proplists:get_value(ssl_options, Options, []),
                    case ssl:connect(Sock, [binary, {packet, raw}, {active, false}] ++ SSLOptions) of
                        {ok, SSLSocket} ->
                            pgsql_setup_startup(State0#state{socket = {ssl, SSLSocket}});
                        {error, _} = SSLConnectErr -> SSLConnectErr
                    end;
                {ok, <<$N>>} ->
                    % server is unwilling
                    {error, ssl_refused}
            end;
        {error, _} = SendSSLRequestError -> SendSSLRequestError
    end.

pgsql_setup_startup(#state{socket = {SockModule, Sock} = Socket, options = Options, subscribers = Subscribers} = State0) ->
    % Send startup packet connection packet.
    User = proplists:get_value(user, Options, ?DEFAULT_USER),
    Database = proplists:get_value(database, Options, User),
    ApplicationName = case proplists:get_value(application_name, Options, node()) of
        ApplicationNameAtom when is_atom(ApplicationNameAtom) -> atom_to_binary(ApplicationNameAtom, utf8);
        ApplicationNameString -> ApplicationNameString
    end,
    TZOpt = case proplists:get_value(timezone, Options, undefined) of
        undefined -> [];
        Timezone -> [{<<"timezone">>, Timezone}]
    end,
    StartupMessage = pgsql_protocol:encode_startup_message([{<<"user">>, User}, {<<"database">>, Database}, {<<"application_name">>, ApplicationName} | TZOpt]),
    case SockModule:send(Sock, StartupMessage) of
        ok ->
            case receive_message(Socket, sync, Subscribers) of
                {ok, #error_response{fields = Fields}} ->
                    {error, {pgsql_error, Fields}};
                {ok, #authentication_ok{}} ->
                    pgsql_setup_finish(Socket, State0);
                {ok, #authentication_kerberos_v5{}} ->
                    {error, {unimplemented, authentication_kerberos_v5}};
                {ok, #authentication_cleartext_password{}} ->
                    pgsql_setup_authenticate_cleartext_password(Socket, State0);
                {ok, #authentication_md5_password{salt = Salt}} ->
                    pgsql_setup_authenticate_md5_password(Socket, Salt, State0);
                {ok, #authentication_scm_credential{}} ->
                    {error, {unimplemented, authentication_scm}};
                {ok, #authentication_gss{}} ->
                    {error, {unimplemented, authentication_gss}};
                {ok, #authentication_sspi{}} ->
                    {error, {unimplemented, authentication_sspi}};
                {ok, #authentication_gss_continue{}} ->
                    {error, {unimplemented, authentication_sspi}};
                {ok, Message} ->
                    {error, {unexpected_message, Message}};
                {error, _} = ReceiveError -> ReceiveError
            end;
        {error, _} = SendError -> SendError
    end.

pgsql_setup_authenticate_cleartext_password(Socket, #state{options = Options} = State0) ->
    Password = proplists:get_value(password, Options, ?DEFAULT_PASSWORD),
    pgsql_setup_authenticate_password(Socket, Password, State0).

-ifndef(old_hash).
pgsql_setup_authenticate_md5_password(Socket, Salt, #state{options = Options} = State0) ->
    User = proplists:get_value(user, Options, ?DEFAULT_USER),
    Password = proplists:get_value(password, Options, ?DEFAULT_PASSWORD),
    % concat('md5', md5(concat(md5(concat(password, username)), random-salt)))
    <<MD51Int:128>> = crypto:hash(md5, [Password, User]),
    MD51Hex = io_lib:format("~32.16.0b", [MD51Int]),
    <<MD52Int:128>> = crypto:hash(md5, [MD51Hex, Salt]),
    MD52Hex = io_lib:format("~32.16.0b", [MD52Int]),
    MD5ChallengeResponse = ["md5", MD52Hex],
    pgsql_setup_authenticate_password(Socket, MD5ChallengeResponse, State0).
-else.
pgsql_setup_authenticate_md5_password(Socket, Salt, #state{options = Options} = State0) ->
    User = proplists:get_value(user, Options, ?DEFAULT_USER),
    Password = proplists:get_value(password, Options, ?DEFAULT_PASSWORD),
    % concat('md5', md5(concat(md5(concat(password, username)), random-salt)))
    <<MD51Int:128>> = crypto:md5([Password, User]),
    MD51Hex = io_lib:format("~32.16.0b", [MD51Int]),
    <<MD52Int:128>> = crypto:md5([MD51Hex, Salt]),
    MD52Hex = io_lib:format("~32.16.0b", [MD52Int]),
    MD5ChallengeResponse = ["md5", MD52Hex],
    pgsql_setup_authenticate_password(Socket, MD5ChallengeResponse, State0).
-endif.

pgsql_setup_authenticate_password({SockModule, Sock} = Socket, Password, #state{subscribers = Subscribers} = State0) ->
    Message = pgsql_protocol:encode_password_message(Password),
    case SockModule:send(Sock, Message) of
        ok ->
            case receive_message(Socket, sync, Subscribers) of
                {ok, #error_response{fields = Fields}} ->
                    {error, {pgsql_error, Fields}};
                {ok, #authentication_ok{}} ->
                    pgsql_setup_finish(Socket, State0);
                {ok, UnexpectedMessage} ->
                    {error, {unexpected_message, UnexpectedMessage}};
                {error, _} = ReceiveError -> ReceiveError
            end;
        {error, _} = SendError -> SendError
    end.

pgsql_setup_finish(Socket, #state{subscribers = Subscribers} = State0) ->
    case receive_message(Socket, sync, Subscribers) of
        {ok, #parameter_status{name = Name, value = Value}} ->
            State1 = handle_parameter(Name, Value, sync, State0),
            pgsql_setup_finish(Socket, State1);
        {ok, #backend_key_data{procid = ProcID, secret = Secret}} ->
            pgsql_setup_finish(Socket, State0#state{backend_procid = ProcID, backend_secret = Secret});
        {ok, #ready_for_query{}} ->
            {ok, State0};
        {ok, #error_response{fields = Fields}} ->
            {error, {pgsql_error, Fields}};
        {ok, Message} ->
            {error, {unexpected_message, Message}};
        {error, _} = ReceiveError -> ReceiveError
    end.

pgsql_simple_query(Query, QueryOptions, Timeout, From, #state{socket = {SockModule, Sock}} = State0) ->
    % If timeout is not infinity, change the parameter before and after the
    % query. While we could catenate the query, it seems easier to send
    % separate query messages, as we don't have to deal with errors.
    ConnPid = self(),
    CurrentCommand = State0#state.current,
    case Timeout of
        infinity ->
            spawn_link(fun() ->
                pgsql_simple_query0(Query, {async, ConnPid, fun(Result) ->
                    gen_server:reply(From, Result),
                    gen_server:cast(ConnPid, {command_completed, CurrentCommand})
                end}, QueryOptions, State0)
            end),
            State0;
        Value ->
            Queries = [
                io_lib:format("set statement_timeout = ~B", [Value]),
                Query,
                "set statement_timeout to default"],
            SinglePacket = [pgsql_protocol:encode_query_message(AQuery) || AQuery <- Queries],
            case SockModule:send(Sock, SinglePacket) of
                ok ->
                    {SetResult, State1} = pgsql_simple_query_loop([], [], sync, QueryOptions, State0),
                    true = set_succeeded_or_within_failed_transaction(SetResult),
                    spawn_link(fun() ->
                        pgsql_simple_query_loop([], [], {async, ConnPid, fun(QueryResult) ->
                            pgsql_simple_query_loop([], [], {async, ConnPid, fun(ResetResult) ->
                                true = set_succeeded_or_within_failed_transaction(ResetResult),
                                gen_server:reply(From, QueryResult),
                                gen_server:cast(ConnPid, {command_completed, CurrentCommand})
                            end}, QueryOptions, State1)
                        end}, QueryOptions, State1)
                    end),
                    State1;
                {error, closed} = SendQueryError ->
                    gen_server:reply(From, SendQueryError),
                    State1 = State0#state{socket = closed},
                    command_completed(CurrentCommand, State1);
                {error, _} = SendQueryError ->
                    gen_server:reply(From, SendQueryError),
                    command_completed(CurrentCommand, State0)
            end
    end.

% This function should always return true as set or reset may only fail because
% we are within a failed transaction.
% If set failed because the transaction was aborted, the query will fail
% (unless it is a rollback).
% If set succeeded within a transaction, but the query failed, the reset may
% fail but set only applies to the transaction anyway.
-spec set_succeeded_or_within_failed_transaction({set, []} | {error, pgsql_error:pgsql_error()}) -> boolean().
set_succeeded_or_within_failed_transaction({set, []}) -> true;
set_succeeded_or_within_failed_transaction({error, {pgsql_error, _} = Error}) ->
    pgsql_error:is_in_failed_sql_transaction(Error).

-spec pgsql_simple_query0(iodata(), sync, query_options(), #state{}) -> {tuple(), #state{}};
                         (iodata(), {async, pid(), fun((any()) -> ok)}, query_options(), #state{}) -> ok.
pgsql_simple_query0(Query, AsyncT, QueryOptions, #state{socket = {SockModule, Sock}} = State) ->
    QueryMessage = pgsql_protocol:encode_query_message(Query),
    case SockModule:send(Sock, QueryMessage) of
        ok -> pgsql_simple_query_loop([], [], AsyncT, QueryOptions, State);
        {error, _} = SendQueryError ->
            return_async(SendQueryError, AsyncT, State)
    end.

pgsql_simple_query_loop(Result0, Acc, AsyncT, QueryOptions, #state{socket = Socket, subscribers = Subscribers} = State0) ->
    case receive_message(Socket, AsyncT, Subscribers) of
        {ok, #parameter_status{name = Name, value = Value}} ->
            State1 = handle_parameter(Name, Value, AsyncT, State0),
            pgsql_simple_query_loop(Result0, Acc, AsyncT, QueryOptions, State1);
        {ok, #row_description{fields = Fields}} when Result0 =:= [] ->
            State1 = oob_update_oid_map_from_fields_if_required(Fields, State0),
            pgsql_simple_query_loop({rows, Fields, []}, Acc, AsyncT, QueryOptions, State1);
        {ok, #data_row{values = Values}} when is_tuple(Result0) andalso element(1, Result0) =:= rows ->
            {rows, Fields, AccRows0} = Result0,
            DecodedRow = pgsql_protocol:decode_row(Fields, Values, State0#state.oidmap, [{integer_datetimes, State0#state.integer_datetimes} | QueryOptions]),
            AccRows1 = [DecodedRow | AccRows0],
            pgsql_simple_query_loop({rows, Fields, AccRows1}, Acc, AsyncT, QueryOptions, State0);
        {ok, #copy_out_response{format = Format}} when Result0 =:= [] ->
            Fields = [Format],
            pgsql_simple_query_loop({copy, Fields, []}, Acc, AsyncT, QueryOptions, State0);
        {ok, #copy_data{data = Data}} when is_tuple(Result0) andalso element(1, Result0) =:= copy ->
            {copy, Fields, AccData0} = Result0,
            AccData1 = [Data | AccData0],
            pgsql_simple_query_loop({copy, Fields, AccData1}, Acc, AsyncT, QueryOptions, State0);
        {ok, #copy_done{}} ->
            pgsql_simple_query_loop(Result0, Acc, AsyncT, QueryOptions, State0);
        {ok, #copy_in_response{format = Format}} when Result0 =:= [] ->
            Fields = [Format],
            return_async({copy_in, Fields}, AsyncT, State0);
        {ok, #command_complete{command_tag = Tag}} ->
            ResultRows = case Result0 of
                {rows, _Descs, AccRows} -> lists:reverse(AccRows);
                {copy, _Descs, AccData} -> lists:reverse(AccData);
                [] -> []
            end,
            DecodedTag = decode_tag(Tag),
            Result = case proplists:get_bool(return_descriptions, QueryOptions) of
                true when is_tuple(Result0) -> {DecodedTag, element(2, Result0), ResultRows};
                true when Result0 =:= [] -> {DecodedTag, [], []};
                false -> {DecodedTag, ResultRows}
            end,
            Acc1 = [Result | Acc],
            pgsql_simple_query_loop([], Acc1, AsyncT, QueryOptions, State0);
        {ok, #empty_query_response{}} ->
            pgsql_simple_query_loop(Result0, Acc, AsyncT, QueryOptions, State0);
        {ok, #error_response{fields = Fields}} ->
            Error = {error, {pgsql_error, Fields}},
            Acc1 = [Error | Acc],
            pgsql_simple_query_loop([], Acc1, AsyncT, QueryOptions, State0);
        {ok, #ready_for_query{}} ->
            Result = case Acc of
                [SingleResult] -> SingleResult;
                MultipleResults -> MultipleResults
            end,
            return_async(Result, AsyncT, State0);
        {ok, Message} ->
            Result = {error, {unexpected_message, Message}},
            return_async(Result, AsyncT, State0);
        {error, _} = ReceiveError ->
            return_async(ReceiveError, AsyncT, State0)
    end.

pgsql_extended_query(Query, Parameters, Fun, Acc0, FinalizeFun, Mode, QueryOptions, Timeout, From, State0) ->
    % If timeout is not infinity, change the parameter before and after the
    % query. While we could catenate the query, it seems easier to send
    % separate query messages, as we don't have to deal with errors.
    ConnPid = self(),
    CurrentCommand = State0#state.current,
    case Timeout of
        infinity ->
            spawn_link(fun() ->
                pgsql_extended_query0(Query, Parameters, Fun, Acc0, FinalizeFun, Mode, {async, ConnPid, fun(Result) ->
                    gen_server:reply(From, Result),
                    gen_server:cast(ConnPid, {command_completed, CurrentCommand})
                end}, QueryOptions, State0)
            end),
            State0;
        Value ->
            {SetResult, State1} = pgsql_simple_query0(io_lib:format("set statement_timeout = ~B", [Value]), sync, [], State0),
            true = set_succeeded_or_within_failed_transaction(SetResult),
            spawn_link(fun() ->
                pgsql_extended_query0(Query, Parameters, Fun, Acc0, FinalizeFun, Mode, {async, ConnPid, fun(QueryResult) ->
                    pgsql_simple_query0("set statement_timeout to default", {async, ConnPid, fun(ResetResult) ->
                        true = set_succeeded_or_within_failed_transaction(ResetResult),
                        gen_server:reply(From, QueryResult),
                        gen_server:cast(ConnPid, {command_completed, CurrentCommand})
                    end}, [], State1)
                end}, QueryOptions, State1)
            end),
            State1
    end.

-spec pgsql_extended_query0(iodata(), [any()], fun(), any(), fun(), all | batch | {cursor, non_neg_integer()}, sync, query_options(), #state{}) -> {any(), #state{}};
                           (iodata(), [any()], fun(), any(), fun(), all | batch | {cursor, non_neg_integer()}, {async, pid(), fun((any()) -> ok)}, query_options(), #state{}) -> ok.
pgsql_extended_query0(Query, Parameters, Fun, Acc0, FinalizeFun, Mode, AsyncT, QueryOptions, #state{socket = {SockModule, Sock}, oidmap = OIDMap, integer_datetimes = IntegerDateTimes} = State) ->
    ParseMessage = pgsql_protocol:encode_parse_message("", Query, []),
    % We ask for a description of parameters only if required.
    NeedStatementDescription = requires_statement_description(Mode, Parameters),
    PacketT = case NeedStatementDescription of
        true ->
            DescribeStatementMessage = pgsql_protocol:encode_describe_message(statement, ""),
            FlushMessage = pgsql_protocol:encode_flush_message(),
            LoopState0 = {parse_complete_with_params, Mode, Parameters},
            {ok, [ParseMessage, DescribeStatementMessage, FlushMessage], LoopState0};
        false ->
            case encode_bind_describe_execute(Mode, Parameters, [], OIDMap, IntegerDateTimes) of
                {ok, BindExecute} ->
                    {ok, [ParseMessage, BindExecute], parse_complete};
                {error, _} = Error -> Error
            end
    end,
    case PacketT of
        {ok, SinglePacket, LoopState} ->
            case SockModule:send(Sock, SinglePacket) of
                ok ->
                    case Mode of
                        batch ->
                            {_, ResultRL, FinalState} = lists:foldl(fun(_ParametersBatch, {AccLoopState, AccResults, AccState}) ->
                                        {Result, AccState1} = pgsql_extended_query_receive_loop(AccLoopState, Fun, Acc0, FinalizeFun, 0, sync, QueryOptions, AccState),
                                        {bind_complete, [Result | AccResults], AccState1}
                                end, {LoopState, [], State}, Parameters),
                            Result = lists:reverse(ResultRL),
                            return_async(Result, AsyncT, FinalState);
                        all ->
                            pgsql_extended_query_receive_loop(LoopState, Fun, Acc0, FinalizeFun, 0, AsyncT, QueryOptions, State);
                        {cursor, MaxRowsStep} ->
                            pgsql_extended_query_receive_loop(LoopState, Fun, Acc0, FinalizeFun, MaxRowsStep, AsyncT, QueryOptions, State)
                    end;
                {error, _} = SendSinglePacketError ->
                    return_async(SendSinglePacketError, AsyncT, State)
            end;
        {error, _} ->
            return_async(PacketT, AsyncT, State)
    end.

-spec encode_bind_describe_execute(all | {cursor, non_neg_integer()}, [any()], [pgsql_oid()], pgsql_oid_map(), boolean()) -> {ok, iodata()} | {error, any()};
                                  (batch, [[any()]], [pgsql_oid()], pgsql_oid_map(), boolean()) -> {ok, iodata()} | {error, any()}.
encode_bind_describe_execute(Mode, Parameters, ParameterDataTypes, OIDMap, IntegerDateTimes) ->
    DescribeMessage = pgsql_protocol:encode_describe_message(portal, ""),
    MaxRowsStep = case Mode of
        all -> 0;
        batch -> 0;
        {cursor, MaxRowsStep0} -> MaxRowsStep0
    end,
    ExecuteMessage = pgsql_protocol:encode_execute_message("", MaxRowsStep),
    SyncOrFlushMessage = if
        MaxRowsStep > 0 -> pgsql_protocol:encode_flush_message();
        true -> pgsql_protocol:encode_sync_message()
    end,
    try
        SinglePacket = case Mode of
            batch ->
                [
                    [pgsql_protocol:encode_bind_message("", "", ParametersBatch, ParameterDataTypes, OIDMap, IntegerDateTimes),
                    DescribeMessage, ExecuteMessage, SyncOrFlushMessage] || ParametersBatch <- Parameters];
            _ ->
                BindMessage = pgsql_protocol:encode_bind_message("", "", Parameters, ParameterDataTypes, OIDMap, IntegerDateTimes),
                [BindMessage, DescribeMessage, ExecuteMessage, SyncOrFlushMessage]
        end,
        {ok, SinglePacket}
    catch throw:Exception ->
        {error, Exception}
    end.

requires_statement_description(batch, ParametersL) ->
    lists:any(fun pgsql_protocol:bind_requires_statement_description/1, ParametersL);
requires_statement_description(_Mode, Parameters) ->
    pgsql_protocol:bind_requires_statement_description(Parameters).

-spec pgsql_extended_query_receive_loop(extended_query_loop_state(), fun(), any(), fun(), non_neg_integer(), sync, query_options(), #state{}) -> {any(), #state{}};
                                       (extended_query_loop_state(), fun(), any(), fun(), non_neg_integer(), {async, pid(), fun((any()) -> ok)}, query_options(), #state{}) -> ok.
pgsql_extended_query_receive_loop(_LoopState, _Fun, _Acc, _FinalizeFun, _MaxRowsStep, AsyncT, _QueryOptions, #state{socket = closed} = State0) ->
    return_async({error, closed}, AsyncT, State0);
pgsql_extended_query_receive_loop(LoopState, Fun, Acc0, FinalizeFun, MaxRowsStep, AsyncT, QueryOptions, #state{socket = Socket, subscribers = Subscribers} = State0) ->
    case receive_message(Socket, AsyncT, Subscribers) of
        {ok, Message} ->
            pgsql_extended_query_receive_loop0(Message, LoopState, Fun, Acc0, FinalizeFun, MaxRowsStep, AsyncT, QueryOptions, State0);
        {error, _} = ReceiveError ->
            return_async(ReceiveError, AsyncT, State0)
    end.

-spec pgsql_extended_query_receive_loop0(pgsql_backend_message(), extended_query_loop_state(), fun(), any(), fun(), non_neg_integer(), sync, query_options(), #state{}) -> {any(), query_options(), #state{}};
                                        (pgsql_backend_message(), extended_query_loop_state(), fun(), any(), fun(), non_neg_integer(), {async, pid(), fun((any()) -> ok)}, query_options(), #state{}) -> ok.
pgsql_extended_query_receive_loop0(#parameter_status{name = Name, value = Value}, LoopState, Fun, Acc0, FinalizeFun, MaxRowsStep, AsyncT, QueryOptions, State0) ->
    State1 = handle_parameter(Name, Value, AsyncT, State0),
    pgsql_extended_query_receive_loop(LoopState, Fun, Acc0, FinalizeFun, MaxRowsStep, AsyncT, QueryOptions, State1);
pgsql_extended_query_receive_loop0(#parse_complete{}, parse_complete, Fun, Acc0, FinalizeFun, MaxRowsStep, AsyncT, QueryOptions, State0) ->
    pgsql_extended_query_receive_loop(bind_complete, Fun, Acc0, FinalizeFun, MaxRowsStep, AsyncT, QueryOptions, State0);

% Path where we ask the backend about what it expects.
% We ignore row descriptions sent before bind as the format codes are null.
pgsql_extended_query_receive_loop0(#parse_complete{}, {parse_complete_with_params, Mode, Parameters}, Fun, Acc0, FinalizeFun, MaxRowsStep, AsyncT, QueryOptions, State0) ->
    pgsql_extended_query_receive_loop({parameter_description_with_params, Mode, Parameters}, Fun, Acc0, FinalizeFun, MaxRowsStep, AsyncT, QueryOptions, State0);
pgsql_extended_query_receive_loop0(#parameter_description{data_types = ParameterDataTypes}, {parameter_description_with_params, Mode, Parameters}, Fun, Acc0, FinalizeFun, MaxRowsStep, AsyncT, QueryOptions, #state{socket = {SockModule, Sock}} = State0) ->
    State1 = oob_update_oid_map_if_required(ParameterDataTypes, State0),
    PacketT = encode_bind_describe_execute(Mode, Parameters, ParameterDataTypes, State1#state.oidmap, State1#state.integer_datetimes),
    case PacketT of
        {ok, SinglePacket} ->
            case SockModule:send(Sock, SinglePacket) of
                ok ->
                    pgsql_extended_query_receive_loop(pre_bind_row_description, Fun, Acc0, FinalizeFun, MaxRowsStep, AsyncT, QueryOptions, State1);
                {error, _} = SendError ->
                    return_async(SendError, AsyncT, State1)
            end;
        {error, _} = Error ->
            case SockModule:send(Sock, pgsql_protocol:encode_sync_message()) of
                ok -> flush_until_ready_for_query(Error, AsyncT, State1);
                {error, _} = SendSyncPacketError -> return_async(SendSyncPacketError, AsyncT, State1)
            end
    end;
pgsql_extended_query_receive_loop0(#row_description{}, pre_bind_row_description, Fun, Acc0, FinalizeFun, MaxRowsStep, AsyncT, QueryOptions, State0) ->
    pgsql_extended_query_receive_loop(bind_complete, Fun, Acc0, FinalizeFun, MaxRowsStep, AsyncT, QueryOptions, State0);
pgsql_extended_query_receive_loop0(#no_data{}, pre_bind_row_description, Fun, Acc0, FinalizeFun, MaxRowsStep, AsyncT, QueryOptions, State0) ->
    pgsql_extended_query_receive_loop(bind_complete, Fun, Acc0, FinalizeFun, MaxRowsStep, AsyncT, QueryOptions, State0);

% Common paths after bind.
pgsql_extended_query_receive_loop0(#bind_complete{}, bind_complete, Fun, Acc0, FinalizeFun, MaxRowsStep, AsyncT, QueryOptions, State0) ->
    pgsql_extended_query_receive_loop(row_description, Fun, Acc0, FinalizeFun, MaxRowsStep, AsyncT, QueryOptions, State0);
pgsql_extended_query_receive_loop0(#no_data{}, row_description, Fun, Acc0, FinalizeFun, MaxRowsStep, AsyncT, QueryOptions, State0) ->
    pgsql_extended_query_receive_loop(no_data, Fun, Acc0, FinalizeFun, MaxRowsStep, AsyncT, QueryOptions, State0);
pgsql_extended_query_receive_loop0(#row_description{fields = Fields}, row_description, Fun, Acc0, FinalizeFun, MaxRowsStep, AsyncT, QueryOptions, State0) ->
    State1 = oob_update_oid_map_from_fields_if_required(Fields, State0),
    pgsql_extended_query_receive_loop({rows, Fields}, Fun, Acc0, FinalizeFun, MaxRowsStep, AsyncT, QueryOptions, State1);
pgsql_extended_query_receive_loop0(#data_row{values = Values}, {rows, Fields} = LoopState, Fun, Acc0, FinalizeFun, MaxRowsStep, AsyncT, QueryOptions, State0) ->
    DecodedRow = pgsql_protocol:decode_row(Fields, Values, State0#state.oidmap, [{integer_datetimes, State0#state.integer_datetimes} | QueryOptions]),
    Acc1 = Fun(DecodedRow, Fields, QueryOptions, Acc0),
    pgsql_extended_query_receive_loop(LoopState, Fun, Acc1, FinalizeFun, MaxRowsStep, AsyncT, QueryOptions, State0);
pgsql_extended_query_receive_loop0(#copy_out_response{format = Format}, _LoopState, Fun, Acc0, FinalizeFun, MaxRowsStep, AsyncT, QueryOptions, State0) ->
    Fields = [Format],
    pgsql_extended_query_receive_loop({copy, Fields}, Fun, Acc0, FinalizeFun, MaxRowsStep, AsyncT, QueryOptions, State0);
pgsql_extended_query_receive_loop0(#copy_data{data = Data}, {copy, Fields} = LoopState, Fun, Acc0, FinalizeFun, MaxRowsStep, AsyncT, QueryOptions, State0) ->
    Acc1 = Fun(Data, Fields, QueryOptions, Acc0),
    pgsql_extended_query_receive_loop(LoopState, Fun, Acc1, FinalizeFun, MaxRowsStep, AsyncT, QueryOptions, State0);
pgsql_extended_query_receive_loop0(#copy_done{}, LoopState, Fun, Acc0, FinalizeFun, MaxRowsStep, AsyncT, QueryOptions, State0) ->
    pgsql_extended_query_receive_loop(LoopState, Fun, Acc0, FinalizeFun, MaxRowsStep, AsyncT, QueryOptions, State0);
pgsql_extended_query_receive_loop0(#command_complete{command_tag = Tag}, _LoopState, Fun, Acc0, FinalizeFun, MaxRowsStep, AsyncT, QueryOptions, #state{socket = {SockModule, Sock}} = State0) ->
    Result = FinalizeFun(Tag, QueryOptions, Acc0),
    if  MaxRowsStep > 0 ->
            case SockModule:send(Sock, pgsql_protocol:encode_sync_message()) of
                ok -> pgsql_extended_query_receive_loop({result, Result}, Fun, Acc0, FinalizeFun, MaxRowsStep, AsyncT, QueryOptions, State0);
                {error, _} = SendSyncPacketError -> return_async(SendSyncPacketError, AsyncT, State0)
            end;
        true -> pgsql_extended_query_receive_loop({result, Result}, Fun, Acc0, FinalizeFun, MaxRowsStep, AsyncT, QueryOptions, State0)
    end;
pgsql_extended_query_receive_loop0(#portal_suspended{}, LoopState, Fun, Acc0, FinalizeFun, MaxRowsStep, AsyncT, QueryOptions, #state{socket = {SockModule, Sock}} = State0) ->
    ExecuteMessage = pgsql_protocol:encode_execute_message("", MaxRowsStep),
    FlushMessage = pgsql_protocol:encode_flush_message(),
    SinglePacket = [ExecuteMessage, FlushMessage],
    case SockModule:send(Sock, SinglePacket) of
        ok -> pgsql_extended_query_receive_loop(LoopState, Fun, Acc0, FinalizeFun, MaxRowsStep, AsyncT, QueryOptions, State0);
        {error, _} = SendSinglePacketError ->
            return_async(SendSinglePacketError, AsyncT, State0)
    end;
pgsql_extended_query_receive_loop0(#ready_for_query{}, {result, Result}, _Fun, _Acc0, _FinalizeFun, _MaxRowsStep, AsyncT, _QueryOptions, State0) ->
    return_async(Result, AsyncT, State0);
pgsql_extended_query_receive_loop0(#copy_in_response{}, LoopState, Fun, Acc0, FinalizeFun, MaxRowsStep, AsyncT, QueryOptions, #state{socket={SockModule,Sock}}=State0) ->
    ErrorMessage = <<"Cannot use COPY with extended_query">>,
    Packet = [pgsql_protocol:encode_copy_fail(ErrorMessage),pgsql_protocol:encode_sync_message()],
    Res= SockModule:send(Sock, Packet),
    case Res of
        ok -> pgsql_extended_query_receive_loop(LoopState, Fun, Acc0, FinalizeFun, MaxRowsStep, AsyncT, QueryOptions, State0);
        {error,_} = SendError -> return_async(SendError, AsyncT, State0)
    end;
pgsql_extended_query_receive_loop0(#error_response{fields = Fields}, LoopState, _Fun, _Acc0, _FinalizeFun, MaxRowsStep, AsyncT, _QueryOptions, #state{socket = {SockModule, Sock}} = State0) ->
    Error = {error, {pgsql_error, Fields}},
    % We already sent a Sync except when we sent a Flush :-)
    % - when we asked for the statement description
    % - when MaxRowsStep > 0
    NeedSync = case LoopState of
        {parse_complete_with_params, _Mode, _Args} -> true;
        {parameter_description_with_params, _Mode, _Parameters} -> true;
        _ when MaxRowsStep > 0 -> true;
        _ -> false
    end,
    case NeedSync of
        true ->
            case SockModule:send(Sock, pgsql_protocol:encode_sync_message()) of
                ok -> flush_until_ready_for_query(Error, AsyncT, State0);
                {error, _} = SendSyncPacketError -> return_async(SendSyncPacketError, AsyncT, State0)
            end;
        false ->
            flush_until_ready_for_query(Error, AsyncT, State0)
    end;
pgsql_extended_query_receive_loop0(#ready_for_query{} = Message, _LoopState, _Fun, _Acc0, _FinalizeFun, _MaxRowsStep, AsyncT, _QueryOptions, State0) ->
    Result = {error, {unexpected_message, Message}},
    return_async(Result, AsyncT, State0);
pgsql_extended_query_receive_loop0(Message, _LoopState, _Fun, _Acc0, _FinalizeFun, _MaxRowsStep, AsyncT, _QueryOptions, State0) ->
    Error = {error, {unexpected_message, Message}},
    flush_until_ready_for_query(Error, AsyncT, State0).

pgsql_send_copy_data(Data, From, #state{socket = {SockModule, Sock}} = State0) ->
    Message = pgsql_protocol:encode_copy_data_message(Data),
    Result = SockModule:send(Sock, Message),
    gen_server:reply(From, Result),
    State0#state{current = undefined}.

pgsql_send_copy_end(From, #state{socket = {SockModule, Sock}} = State0) ->
    Message = pgsql_protocol:encode_copy_done(),
    Result0 = SockModule:send(Sock, Message),
    {Result1, State1} = pgsql_send_copy_end_flush(Result0, State0),
    gen_server:reply(From, Result1),
    State1.
pgsql_send_copy_end_flush(Result0, #state{socket = Socket, subscribers = Subscribers} = State0) ->
    case receive_message(Socket, sync, Subscribers) of
        {ok, {command_complete, <<"COPY ",CopyCount/binary>>}} ->
            CopyCountNum = ?binary_to_integer(CopyCount),
            pgsql_send_copy_end_flush({copy,CopyCountNum}, State0);
        {ok, {ready_for_query, _}} ->
            {Result0, State0#state{current = undefined}}
    end.

flush_until_ready_for_query(Result, AsyncT, #state{socket = Socket, subscribers = Subscribers} = State0) ->
    case receive_message(Socket, AsyncT, Subscribers) of
        {ok, #parameter_status{name = Name, value = Value}} ->
            State1 = handle_parameter(Name, Value, AsyncT, State0),
            flush_until_ready_for_query(Result, AsyncT, State1);
        {ok, #ready_for_query{}} ->
            return_async(Result, AsyncT, State0);
        {ok, _OtherMessage} ->
            flush_until_ready_for_query(Result, AsyncT, State0);
        {error, _} = ReceiveError ->
            return_async(ReceiveError, AsyncT, State0)
    end.

-spec return_async(any(), sync, #state{}) -> {any(), #state{}};
                  (any(), {async, pid(), fun((any()) -> ok)}, #state{}) -> ok.
return_async({error, closed} = Error, sync, #state{} = State) ->
    {Error, State#state{current = undefined, socket = closed}};
return_async({error, closed} = Error, {async, ConnPid, Callback}, #state{socket = Socket}) ->
    ok = Callback(Error),
    gen_server:cast(ConnPid, {socket_closed, Socket});
return_async(Result, sync, #state{} = State) ->
    {Result, State};
return_async(Result, {async, _ConnPid, Callback}, #state{}) ->
    Callback(Result).

extended_query_fn(Row, RowDescs, _QueryOptions, {_PreviousDescs, AccRows}) ->
    {RowDescs, [Row | AccRows]}.

extended_query_finalize(Tag, QueryOptions, {RowDescs, AccRows}) ->
    DecodedTag = decode_tag(Tag),
    Rows = lists:reverse(AccRows),
    case proplists:get_bool(return_descriptions, QueryOptions) of
        true ->
            {DecodedTag, RowDescs, Rows};
        false ->
            {DecodedTag, Rows}
    end.

fold_finalize(_Tag, _QueryOptions, Acc) ->
    {ok, Acc}.

map_fn(Row, _RowDesc, _QueryOptions, {Function, Acc}) -> {Function, [Function(Row) | Acc]}.
    
map_finalize(_Tag, _QueryOptions, {_Function, Acc}) ->
    {ok, lists:reverse(Acc)}.

foreach_fn(Row, _RowDesc, _QueryOptions, Function) ->
    Function(Row),
    Function.

foreach_finalize(_Tag, _QueryOptions, _Function) ->
    ok.

%%--------------------------------------------------------------------
%% @doc Handle parameter status messages. These can happen anytime.
%% 
handle_parameter(<<"integer_datetimes">> = Key, <<"on">> = Value, AsyncT, State0) ->
    set_parameter_async(Key, Value, AsyncT),
    State0#state{integer_datetimes = true};
handle_parameter(<<"integer_datetimes">> = Key, <<"off">> = Value, AsyncT, State0) ->
    set_parameter_async(Key, Value, AsyncT),
    State0#state{integer_datetimes = false};
handle_parameter(_Key, _Value, _AsyncT, State0) -> State0.

set_parameter_async(_Key, _Value, sync) -> ok;
set_parameter_async(Key, Value, {async, ConnPid, _}) ->
    gen_server:cast(ConnPid, {set_parameter, Key, Value}).

%%--------------------------------------------------------------------
%% @doc Convert a statement from the ? placeholder syntax to the $x placeholder
%% syntax.
%%
-spec convert_statement(binary() | string()) -> string().
convert_statement(StatementStr) when is_list(StatementStr) ->
    convert_statement_0(StatementStr, false, 1, []);
convert_statement(StatementStr) when is_binary(StatementStr) ->
    convert_statement(binary_to_list(StatementStr)).

convert_statement_0([], _InString, _PlaceholderIndex, Acc) -> lists:reverse(Acc);
convert_statement_0([$? | Tail], false, PlaceholderIndex, Acc) ->
    convert_statement_0(Tail, false, PlaceholderIndex + 1, lists:reverse([$$ | integer_to_list(PlaceholderIndex)]) ++ Acc);
convert_statement_0([$' | Tail], InString, PlaceholderIndex, Acc) ->
    convert_statement_0(Tail, not InString, PlaceholderIndex, [$' | Acc]);
convert_statement_0([H | Tail], InString, PlaceholderIndex, Acc) ->
    convert_statement_0(Tail, InString, PlaceholderIndex, [H | Acc]).

%%--------------------------------------------------------------------
%% @doc Receive a single packet (in passive mode). Notifications and
%% notices are broadcast to subscribers.
%%
-spec receive_message(socket(), sync | {async, pid(), fun((any()) -> ok)}, [{pid(), reference()}]) -> {ok, pgsql_backend_message()} | {error, any()}.
receive_message({SockModule, Sock}, AsyncT, Subscribers) ->
    Result0 = case SockModule:recv(Sock, ?MESSAGE_HEADER_SIZE) of
        {ok, <<Code:8/integer, Size:32/integer>>} ->
            Payload = Size - 4,
            case Payload of
                0 ->
                    pgsql_protocol:decode_message(Code, <<>>);
                _ ->
                    case SockModule:recv(Sock, Payload) of
                        {ok, Rest} ->
                            pgsql_protocol:decode_message(Code, Rest);
                        {error, _} = ErrorRecvPacket -> ErrorRecvPacket
                    end
            end;
        {error, _} = ErrorRecvPacketHeader -> ErrorRecvPacketHeader
    end,
    case Result0 of
        {ok, #notification_response{} = Notification} ->
            broadcast_to_subscribers(Notification, AsyncT, Subscribers),
            receive_message({SockModule, Sock}, AsyncT, Subscribers);
        {ok, #notice_response{} = Notice} ->
            broadcast_to_subscribers(Notice, AsyncT, Subscribers),
            receive_message({SockModule, Sock}, AsyncT, Subscribers);
        _ -> Result0
    end.

-spec broadcast_to_subscribers(
            #notification_response{} | #notice_response{},
            sync | {async, pid(), fun((any()) -> ok)},
            [{pid(), reference()}]) -> ok.
broadcast_to_subscribers(Packet, AsyncT, Subscribers) ->
    ConnPid = case AsyncT of
        sync -> self();
        {async, Pid, _Fun} -> Pid
    end,
    Connection = {?MODULE, ConnPid},
    What = case Packet of
        #notification_response{procid = ProcID, channel = Channel, payload = Payload} -> {notification, ProcID, Channel, Payload};
        #notice_response{fields = Fields} -> {notice, Fields}
    end,
    Message = {pgsql, Connection, What},
    lists:foreach(fun({Subscriber, _Ref}) ->
        Subscriber ! Message
    end, Subscribers).

%%--------------------------------------------------------------------
%% @doc Decode a command complete tag and result rows and form a result
%% according to the current API.
%%
decode_tag(Tag) ->
    case binary:split(Tag, <<" ">>) of
        [Verb, Object] ->
            VerbDecoded = decode_verb(Verb),
            ObjectL = decode_object(Object),
            list_to_tuple([VerbDecoded | ObjectL]);
        [Verb] -> decode_verb(Verb)
    end.

decode_verb(Verb) ->
    VerbStr = binary_to_list(Verb),
    VerbLC = string:to_lower(VerbStr),
    list_to_atom(VerbLC).

decode_object(<<FirstByte, _/binary>> = Object) when FirstByte =< $9 andalso FirstByte >= $0 ->
    Words = binary:split(Object, <<" ">>, [global]),
    [list_to_integer(binary_to_list(Word)) || Word <- Words];
decode_object(Object) ->
    ObjectUStr = re:replace(Object, <<" ">>, <<"_">>, [global, {return, list}]),
    ObjectULC = string:to_lower(ObjectUStr),
    [list_to_atom(ObjectULC)].
    
%%--------------------------------------------------------------------
%% @doc Convert a native result to an odbc result.
%%
-spec native_to_odbc(result_tuple()) -> odbc_result_tuple() | {error, any()}.
native_to_odbc({error, _} = Error) -> Error;
native_to_odbc({{insert, _TableOID, Count}, []}) -> {updated, Count};
native_to_odbc({{delete, Count}, []}) -> {updated, Count};
native_to_odbc({{update, Count}, []}) -> {updated, Count};
native_to_odbc({{move, Count}, []}) -> {updated, Count};
native_to_odbc({{fetch, _Count}, []}) -> {updated, 0};
native_to_odbc({{copy, Count}, []}) -> {updated, Count};
native_to_odbc({{insert, _TableOID, Count}, Rows}) -> {updated, Count, Rows};
native_to_odbc({{delete, Count}, Rows}) -> {updated, Count, Rows};
native_to_odbc({{update, Count}, Rows}) -> {updated, Count, Rows};
native_to_odbc({{select, _Count}, Rows}) -> {selected, Rows};
native_to_odbc({{create, _What}, []}) -> {updated, 1};
native_to_odbc({{drop, _What}, []}) -> {updated, 1};
native_to_odbc({{alter, _What}, []}) -> {updated, 1};
native_to_odbc({'begin', []}) -> {updated, 0};
native_to_odbc({commit, []}) -> {updated, 0};
%native_to_odbc({rollback, []}) -> {updated, 0};    -- make sure rollback fails.
native_to_odbc({set, []}) -> {updated, 0};
native_to_odbc({listen, []}) -> {updated, 0};
native_to_odbc({notify, []}) -> {updated, 0};
native_to_odbc({'do', []}) -> {updated, 0};
native_to_odbc({Other, []}) -> {error, {pgsql_error, {unknown_command, Other}}}.

adjust_timeout(infinity) -> infinity;
adjust_timeout(Timeout) -> Timeout + ?TIMEOUT_GEN_SERVER_CALL_DELTA.

%%--------------------------------------------------------------------
%% @doc Cancel using a new connection.
%%
oob_cancel(#state{options = Options, backend_procid = ProcID, backend_secret = Secret}) ->
    Host = proplists:get_value(host, Options, ?DEFAULT_HOST),
    Port = proplists:get_value(port, Options, ?DEFAULT_PORT),
    % First open a TCP connection
    case gen_tcp:connect(Host, Port, [binary, {packet, raw}, {active, false}]) of
        {ok, Sock} ->
            Message = pgsql_protocol:encode_cancel_message(ProcID, Secret),
            case gen_tcp:send(Sock, Message) of
                ok ->
                    gen_tcp:close(Sock);
                {error, _} = SendError -> SendError
            end;
        {error, _} = ConnectError -> ConnectError
    end.

%%--------------------------------------------------------------------
%% @doc Update the OID Map out of band, opening a new connection.
%%
-spec oob_update_oid_map_from_fields_if_required([#row_description_field{}], #state{}) -> #state{}.
oob_update_oid_map_from_fields_if_required(Fields, State0) ->
    OIDs = [OID || #row_description_field{data_type_oid = OID} <- Fields],
    oob_update_oid_map_if_required(OIDs, State0).

-spec oob_update_oid_map_if_required([pgsql_oid()], #state{}) -> #state{}.
oob_update_oid_map_if_required(OIDs, #state{oidmap = OIDMap} = State0) ->
    Required = lists:any(fun(OID) ->
        not gb_trees:is_defined(OID, OIDMap)
    end, OIDs),
    case Required of
        true -> oob_update_oid_map(State0);
        false -> State0
    end.

oob_update_oid_map(#state{options = Options0} = State0) ->
    OOBOptions =  lists:keystore(fetch_oid_map, 1, Options0, {fetch_oid_map, false}),
    {ok, Pid} = pgsql_connection_sup:start_child(OOBOptions),
    SubConnection = {pgsql_connection, Pid},
    {ok, NewOIDMap} = fold(fun({Oid, Typename}, AccTypes) ->
        gb_trees:enter(Oid, binary_to_atom(Typename, utf8), AccTypes)
    end, State0#state.oidmap, "SELECT oid, typname FROM pg_type", SubConnection),
    close(SubConnection),
    State0#state{oidmap = NewOIDMap}.

%%--------------------------------------------------------------------
%% @doc Update the OID Map inline (at setup).
%%
update_oid_map(#state{} = State0) ->
    {{ok, NewOIDMap}, State1} = pgsql_extended_query0(<<"SELECT oid, typname FROM pg_type">>, [], fun({Oid, Typename}, _RowDesc, _QueryOptions, AccTypes) ->
        gb_trees:enter(Oid, binary_to_atom(Typename, utf8), AccTypes)
    end, State0#state.oidmap, fun fold_finalize/3, all, sync, [], State0),
    State1#state{oidmap = NewOIDMap}.

%%--------------------------------------------------------------------
%% @doc Prepare socket for sending query: set it in passive mode or
%% reconnect if it was closed and options allow it.
%%
-spec set_passive_or_reconnect_if_required(#state{}) -> #state{}.
set_passive_or_reconnect_if_required(#state{socket = closed, options = Options} = State0) ->
    case proplists:get_value(reconnect, Options, true) of
        true ->
            case pgsql_open(State0) of
                {ok, State1} -> State1;
                {error, _} -> State0
            end;
        false -> State0
    end;
set_passive_or_reconnect_if_required(#state{socket = {gen_tcp, Socket}} = State0) ->
    _ = inet:setopts(Socket, [{active, false}]),
    State0;
set_passive_or_reconnect_if_required(#state{socket = {ssl, Socket}} = State0) ->
    _ = ssl:setopts(Socket, [{active, false}]),
    State0.

%%--------------------------------------------------------------------
%% @doc Set the socket in active mode for a single packet (a notification).
%%
-spec set_active_once(#state{}) -> ok.
set_active_once(#state{socket = closed}) -> ok;
set_active_once(#state{socket = {gen_tcp, Socket}}) ->
    _ = inet:setopts(Socket, [{active, once}]),
    ok;
set_active_once(#state{socket = {ssl, Socket}}) ->
    _ = ssl:setopts(Socket, [{active, once}]),
    ok.

%%--------------------------------------------------------------------
%% @doc Process some active data.
%%
-spec process_active_data(binary(), #state{}) -> #state{}.
process_active_data(<<Code:8/integer, Size:32/integer, Tail/binary>>, #state{socket = {SockModule, Sock}, subscribers = Subscribers} = State0) ->
    TailSize = byte_size(Tail),
    Payload = Size - 4,
    DecodeT = case Payload of
        0 ->
            {pgsql_protocol:decode_message(Code, <<>>), Tail};
        _ when Payload =< TailSize ->
            {PayloadBin, Rest0} = split_binary(Tail, Payload),
            {pgsql_protocol:decode_message(Code, PayloadBin), Rest0};
        _ when Payload > TailSize ->
            case SockModule:recv(Sock, Payload - TailSize) of
                {ok, Missing} ->
                    {pgsql_protocol:decode_message(Code, list_to_binary([Tail, Missing])), <<>>};
                {error, _} = ErrorRecvPacket ->
                    {ErrorRecvPacket, <<>>}
            end
    end,
    case DecodeT of
        {{ok, #notification_response{} = Notification}, Rest} ->
            broadcast_to_subscribers(Notification, sync, Subscribers),
            process_active_data(Rest, State0);
        {{ok, #notice_response{} = Notice}, Rest} ->
            broadcast_to_subscribers(Notice, sync, Subscribers),
            process_active_data(Rest, State0);
        {{ok, #parameter_status{name = Name, value = Value}}, Rest} ->
            State1 = handle_parameter(Name, Value, sync, State0),
            process_active_data(Rest, State1);
        {{ok, Message}, Rest} ->
            error_logger:warning_msg("Unexpected asynchronous message\n~p\n", [Message]),
            process_active_data(Rest, State0);
        {{error, _} = Error, _Rest} ->
            error_logger:error_msg("Unexpected asynchronous error\n~p\n", [Error]),
            SockModule:close(Sock),
            State0#state{socket = closed}
    end;
process_active_data(<<>>, State0) -> State0;
process_active_data(PartialHeader, #state{socket = {SockModule, Sock}} = State0) ->
    PartialHeaderSize = byte_size(PartialHeader),
    case SockModule:recv(Sock, ?MESSAGE_HEADER_SIZE - PartialHeaderSize) of
        {ok, Rest} ->
            process_active_data(list_to_binary([PartialHeader, Rest]), State0);
        {error, _} = Error ->
            error_logger:error_msg("Unexpected asynchronous error\n~p\n", [Error]),
            SockModule:close(Sock),
            State0#state{socket = closed}
    end.    

%%--------------------------------------------------------------------
%% @doc Subscribe to notifications. We setup a monitor to clean the list up.
%%
do_subscribe(Pid, List) ->
    MonitorRef = erlang:monitor(process, Pid),
    [{Pid, MonitorRef} | List].

%%--------------------------------------------------------------------
%% @doc Unsubscribe to notifications. Clear the monitor.
%%
do_unsubscribe(Pid, List) ->
    case lists:keyfind(Pid, 1, List) of
        {Pid, MonitorRef} ->
            erlang:demonitor(MonitorRef),
            lists:keydelete(Pid, 1, List);
        false -> List
    end.

%%--------------------------------------------------------------------
%% @doc Send a call message to the gen server, retrying if the result is
%% {error, closed} and the option retry is set to true.
%%
call_and_retry(ConnPid, Command, Retry, Timeout) ->
    case gen_server:call(ConnPid, {do_query, Command}, Timeout) of
        {error, closed} when Retry -> 
            call_and_retry(ConnPid, Command, Retry, Timeout);
        Other -> 
            Other
    end.

%%--------------------------------------------------------------------
%% @doc Perform a query.
%%
do_query(Command, From, #state{current = undefined} = State0) ->
    State1 = State0#state{current = {Command, From}},
    State2 = set_passive_or_reconnect_if_required(State1),
    case State2#state.socket of
        closed ->
            gen_server:reply(From, {error, closed}),
            command_completed({Command, From}, State2);
        _ ->
            do_query0(Command, From, State2)
    end;
do_query(Command, From, #state{pending = Pending} = State0) ->
    State0#state{pending = [{Command, From} | Pending]}.

do_query0({simple_query, Query, QueryOptions, Timeout}, From, State0) ->
    pgsql_simple_query(Query, QueryOptions, Timeout, From, State0);
do_query0({extended_query, Query, Parameters, QueryOptions, Timeout}, From, State0) ->
    pgsql_extended_query(Query, Parameters, fun extended_query_fn/4, {[], []}, fun extended_query_finalize/3, all, QueryOptions, Timeout, From, State0);
do_query0({batch_query, Query, ParametersList, QueryOptions, Timeout}, From, State0) ->
    pgsql_extended_query(Query, ParametersList, fun extended_query_fn/4, {[], []}, fun extended_query_finalize/3, batch, QueryOptions, Timeout, From, State0);
do_query0({fold, Query, Parameters, Function, Acc0, QueryOptions, Timeout}, From, #state{} = State0) ->
    MaxRowsStep = proplists:get_value(max_rows_step, QueryOptions, ?DEFAULT_MAX_ROWS_STEP),
    pgsql_extended_query(Query, Parameters, fun(Row, _RowDesc, _QueryOptions, AccIn) -> Function(Row, AccIn) end, Acc0, fun fold_finalize/3, {cursor, MaxRowsStep}, QueryOptions, Timeout, From, State0);
do_query0({map, Query, Parameters, Function, QueryOptions, Timeout}, From, #state{} = State0) ->
    MaxRowsStep = proplists:get_value(max_rows_step, QueryOptions, ?DEFAULT_MAX_ROWS_STEP),
    pgsql_extended_query(Query, Parameters, fun map_fn/4, {Function, []}, fun map_finalize/3, {cursor, MaxRowsStep}, QueryOptions, Timeout, From, State0);
do_query0({foreach, Query, Parameters, Function, QueryOptions, Timeout}, From, #state{} = State0) ->
    MaxRowsStep = proplists:get_value(max_rows_step, QueryOptions, ?DEFAULT_MAX_ROWS_STEP),
    pgsql_extended_query(Query, Parameters, fun foreach_fn/4, Function, fun foreach_finalize/3, {cursor, MaxRowsStep}, QueryOptions, Timeout, From, State0);
do_query0({send_copy_data, Data}, From, State0) ->
    pgsql_send_copy_data(Data, From, State0);
do_query0({send_copy_end}, From, State0) ->
    pgsql_send_copy_end(From, State0).


command_completed(Command, #state{current = Command, pending = []} = State) ->
    set_active_once(State),
    State#state{current = undefined};
command_completed(Command, #state{current = Command, pending = [{PendingCommand, PendingFrom} | PendingT]} = State0) ->
    State1 = State0#state{current = undefined, pending = PendingT},
    do_query(PendingCommand, PendingFrom, State1).
