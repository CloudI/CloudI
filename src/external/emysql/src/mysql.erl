%%% Encoding: latin-1
%%% File    : mysql.erl
%%% Author  : Magnus Ahltorp <ahltorp@nada.kth.se>
%%% Descrip.: MySQL client.
%%%
%%% Created :  4 Aug 2005 by Magnus Ahltorp <ahltorp@nada.kth.se>
%%%
%%% Copyright (c) 2001-2004 Kungliga Tekniska Högskolan
%%% See the file COPYING
%%%
%%% Modified: 9/12/2006 by Yariv Sadan <yarivvv@gmail.com>
%%% Note: Added support for prepared statements,
%%% transactions, better connection pooling, more efficient logging
%%% and made other internal enhancements.
%%%
%%% Modified: 9/23/2006 Rewrote the transaction handling code to
%%% provide a simpler, Mnesia-style transaction interface. Also,
%%% moved much of the prepared statement handling code to mysql_conn.erl
%%% and added versioning to prepared statements.
%%% 
%%%
%%% Usage:
%%%
%%%
%%% Call one of the start-functions before any call to fetch/2
%%%
%%%   start_link(PoolId, Host, User, Password, Database)
%%%   start_link(PoolId, Host, Port, User, Password, Database)
%%%
%%% (These functions also have non-linking coutnerparts.)
%%%
%%% PoolId is a connection pool identifier. If you want to have more
%%% than one connection to a server (or a set of MySQL replicas),
%%% add more with
%%%
%%%   connect(PoolId, Host, Port, User, Password, Database, Reconnect)
%%%
%%% use 'undefined' as Port to get default MySQL port number (3306).
%%% MySQL querys will be sent in a per-PoolId round-robin fashion.
%%% Set Reconnect to 'true' if you want the dispatcher to try and
%%% open a new connection, should this one die.
%%%
%%% When you have a mysql_dispatcher running, this is how you make a
%%% query :
%%%
%%%   fetch(PoolId, "select * from hello") -> Result
%%%     Result = {data, MySQLRes} | {updated, MySQLRes} |
%%%              {error, MySQLRes}
%%%
%%% Actual data can be extracted from MySQLRes by calling the following API
%%% functions:
%%%     - on data received:
%%%          FieldInfo = mysql:get_result_field_info(MysqlRes)
%%%          AllRows   = mysql:get_result_rows(MysqlRes)
%%%         with FieldInfo = list() of {Table, Field, Length, Name}
%%%          and AllRows   = list() of list() representing records
%%%     - on update:
%%%          Affected  = mysql:get_result_affected_rows(MysqlRes)
%%%         with Affected  = integer()
%%%     - on error:
%%%          Reason    = mysql:get_result_reason(MysqlRes)
%%%         with Reason    = string()
%%% 
%%% If you just want a single MySQL connection, or want to manage your
%%% connections yourself, you can use the mysql_conn module as a
%%% stand-alone single MySQL connection. See the comment at the top of
%%% mysql_conn.erl.

-module(mysql).
-behaviour(gen_server).

-export([start_link/7,
         connect/7,
         fetch/2,
         fetch/3,
         prepare/2,
         execute/4,
         get_result_field_info/1,
         get_result_rows/1,
         get_result_affected_rows/1,
         get_result_insert_id/1,
         get_result_reason/1,
         call_server/1,
         encode/1,
         encode/2,
         two_digits/1,
         quote/1,
         quote/2,
         asciz_binary/2]).

%% Internal exports - gen_server callbacks
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

%% Records
-include("mysql.hrl").

-record(conn, {pool_name, pid, reconnect, host, port, user, password, database, encoding}).
-record(state, {connections = gb_trees:empty(), pools = [], prepares = gb_trees:empty()}).
-record(pool, {name, members = []}).

%% Macros
-define(SERVER, mysql_dispatcher).
-define(CONNECT_TIMEOUT, 5000).
-define(LOCAL_FILES, 128).
-define(PORT, 3306).

%% @spec start_link(PoolId, Host, Port, User, Password, Database, Encoding) -> Result
%%       PoolId = atom()
%%       Host = string()
%%       Port = undefined | integer()
%%       User = string()
%%       Password = string()
%%       Database = string()
%%       Encoding = atom
%%       Result = {ok, pid()} | ignore | {error, any()}
%% @doc Starts the MySQL client gen_server process.
start_link(PoolId, Host, undefined, User, Password, Database, Encoding) ->
    start_link(PoolId, Host, ?PORT, User, Password, Database, Encoding);
start_link(PoolId, Host, Port, User, Password, Database, Encoding) ->
    gen_server:start_link(
        {local, ?SERVER},
        ?MODULE,
        [PoolId, Host, Port, User, Password, Database, Encoding],
        []
    ).

%% @spec: connect(PoolId::atom(), Host::string(), Port::integer() | undefined,
%%    User::string(), Password::string(), Database::string(),
%%    Encoding::string(), Reconnect::bool(), LinkConnection::bool()) ->
%%      {ok, ConnPid} | {error, Reason}
%% @doc Add an additional MySQL connection to a given pool.
%% @spec connect(PoolId, Host, Port, User, Password, Database, Encoding) -> Result
connect(PoolName, Host, undefined, User, Password, Database, Encoding) ->
    connect(PoolName, Host, ?PORT, User, Password, Database, Encoding);
connect(PoolName, Host, Port, User, Password, Database, Encoding) ->
    gen_server:call(?SERVER, {add_connection, #conn{
        pool_name = PoolName, reconnect = true, host = Host,
        port = Port, user = User, password = Password, database = Database,
        encoding = Encoding
    }}).

%% @doc Send a query to a connection from the connection pool and wait
%%   for the result. If this function is called inside a transaction,
%%   the PoolId parameter is ignored.
%%
%% @spec fetch(PoolId::atom(), Query::iolist(), Timeout::integer()) ->
%%   query_result()
fetch(PoolId, Query) ->
    fetch(PoolId, Query, undefined).

fetch(PoolId, Query, Timeout) -> 
    call_server({fetch, PoolId, Query, Timeout}).

%% @doc Register a prepared statement with the dispatcher. This call does not
%% prepare the statement in any connections. The statement is prepared
%% lazily in each connection when it is told to execute the statement.
%% If the Name parameter matches the name of a statement that has
%% already been registered, the version of the statement is incremented
%% and all connections that have already prepared the statement will
%% prepare it again with the newest version.
%%
%% @spec prepare(Name::atom(), Query::iolist()) -> ok
prepare(Name, Query) ->
    gen_server:cast(?SERVER, {prepare, Name, Query}).

%% @spec execute(PoolId, Name, Params, Timeout) -> Result
%%       PoolId = atom()
%%       Name = atom()
%%       Params = [term()]
%%       Timeout = undefined | integer()
%%       Result = {data, any()} | {updated, any()} | {error, any()}
%% @doc Execute a query in the connection pool identified by
%% PoolId. This function optionally accepts a list of parameters to pass
%% to the prepared statement and a Timeout parameter.
%% If this function is called inside a transaction, the PoolId paramter is
%% ignored.
execute(PoolId, Name, Params, Timeout) ->
    call_server({execute, PoolId, Name, Params, Timeout}).

%% @spec get_result_field_info(mysql_result()) -> fieldinfo()
%% @doc Extract the FieldInfo from MySQL Result on data received.
get_result_field_info(#mysql_result{fieldinfo = FieldInfo}) -> FieldInfo.

%% @doc Extract the Rows from MySQL Result on data received
%% 
%% @spec get_result_rows(MySQLRes::mysql_result()) -> [Row::list()]
get_result_rows(#mysql_result{rows = AllRows}) -> AllRows.

%% @doc Extract the Rows from MySQL Result on update
%%
%% @spec get_result_affected_rows(MySQLRes::mysql_result()) ->
%%           AffectedRows::integer()
get_result_affected_rows(#mysql_result{affectedrows = AffectedRows}) -> AffectedRows.

%% @doc Extract the insert id from MySQL Result on insert
%%
%% @spec get_result_insert_id(MySQLRes::mysql_result()) ->
%%           InsertID::integer()
get_result_insert_id(#mysql_result{insert_id = InsertID}) -> InsertID.

%% @doc Extract the error Reason from MySQL Result on error
%%
%% @spec get_result_reason(MySQLRes::mysql_result()) ->
%%    Reason::string()
get_result_reason(#mysql_result{error = Reason}) -> Reason.

init([PoolName, Host, Port, User, Password, Database, Encoding]) ->
    case mysql_conn:start(Host, Port, User, Password, Database, Encoding, PoolName) of
        {ok, ConnPid} ->
            State = add_connection(
                #state{ },
                #conn{
                    pool_name = PoolName, pid = ConnPid, reconnect = true, host = Host,
                    port = Port, user = User, password = Password, database = Database,
                    encoding = Encoding
                }
            ),
            {ok, State};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call({fetch, PoolId, Query, Timeout}, From, State) ->
    do_work(
        State, PoolId,
        fun(Conn, NextState) ->
            Pid = Conn#conn.pid,
            Response = mysql_conn:fetch(Pid, Query, From, Timeout),
            case Response of
                {error, Reason} ->
                    {reply, {error, Reason}, handle_reset(Conn#conn.pid, NextState)};
                _ ->
                    {reply, Response, NextState}
            end
        end
    );

handle_call({execute, Pool, StatementName, Params, Timeout}, From, State) ->
    do_work(
        State, Pool,
        fun (Conn, NextState) ->
            {StatementName1, StatementSQL1, StatementVersion1} = case gb_trees:lookup(StatementName, NextState#state.prepares) of
                none -> {none, StatementName, 1};
                {value, {StatementSQL, StatementVersion}} -> {StatementName, StatementSQL, StatementVersion}
            end,
            Response = mysql_conn:execute(
                Conn#conn.pid,
                StatementName1,
                StatementVersion1,
                Params,
                From,
                StatementSQL1,
                Timeout
            ),
            case Response of
                {error, Reason} ->
                    {reply, {error, Reason}, handle_reset(Conn#conn.pid, NextState)};
                _ ->
                    {reply, Response, NextState}
            end
        end
    );

handle_call({add_connection, Conn}, _From, State) ->
    {Resp, NewState} = case mysql_conn:start(Conn#conn.host, Conn#conn.port, Conn#conn.user, Conn#conn.password, Conn#conn.database, Conn#conn.encoding, Conn#conn.pool_name) of
        {ok, ConnPid} ->
            {ok, add_connection(State, Conn#conn{ pid = ConnPid })};
        Err ->
            {Err, State}
    end,
    {reply, Resp, NewState}.

handle_cast({prepare, Name, Stmt}, State) ->
    Version1 = case gb_trees:lookup(Name, State#state.prepares) of
        {value, {_Stmt, Version}} -> Version + 1;
        none -> 1
    end,
    {noreply, State#state{prepares = gb_trees:enter(Name, {Stmt, Version1}, State#state.prepares)}}.

handle_reset(Pid, State) ->
    case gb_trees:lookup(Pid, State#state.connections) of
        {value, #conn{reconnect=true} = Conn0} ->
            State1 = remove_connection(State, Pid),
            case reset_connection(Conn0) of
                undefined -> State1;
                Conn -> add_connection(State1, Conn)
            end;
        _ -> State
    end.

handle_info(_MSG, State) ->
    {noreply, State}.

terminate(Reason, _State) -> 
    Reason.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

init_pool(State, Name) ->
    case lists:keymember(Name, 2, State#state.pools) of
        true -> {error, pool_exists};
        false ->
            State#state{ pools = [ #pool{ name = Name } | State#state.pools]}
    end.

has_pool(State, PoolName) ->
    lists:keymember(PoolName, 2, State#state.pools).

%% @spec add_member(State, PoolName, Value) -> Result
add_member(State, PoolName, Value) ->
    case lists:keyfind(PoolName, 2, State#state.pools) of
        false -> {error, pool_doesnt_exist};
        Pool ->
            case lists:member(Value, Pool#pool.members) of
                true -> {error, already_member};
                false ->
                    NewPool = Pool#pool{ members = [Value | Pool#pool.members] },
                    State#state{
                        pools = lists:keyreplace(PoolName, 2, State#state.pools, NewPool)
                    }
            end
    end.

%% @spec next_member(State, PoolName) -> Result
next_member(State, PoolName) ->
    case lists:keyfind(PoolName, 2, State#state.pools) of
        false -> {error, pool_doesnt_exist};
        Pool ->
            case Pool#pool.members of
                [] ->
                    exit(pool_is_empty);
                [NextMember | Others] ->
                    NewPool = Pool#pool{ members = Others ++ [NextMember] },
                    NewState = State#state{
                        pools = lists:keyreplace(PoolName, 2, State#state.pools, NewPool)
                    },
                    {ok, NextMember, NewState}
            end
    end.

%% @spec remove_member(State, PoolName, Member) -> Result
remove_member(State, PoolName, Member) ->
    case lists:keyfind(PoolName, 2, State#state.pools) of
        false -> {error, pool_doesnt_exist};
        Pool ->
            NewPool = Pool#pool{ members = lists:delete(Member, Pool#pool.members) },
            NewState = State#state{
                pools = lists:keyreplace(PoolName, 2, State#state.pools, NewPool)
            },
            {ok, NewState}
    end.

do_work(State, PoolName, Fun) ->
    {ok, Pid, NewState} = next_member(State, PoolName),
    {value, Connection} = gb_trees:lookup(Pid, State#state.connections),
    Fun(Connection, NewState).

call_server(Msg) ->
    gen_server:call(?SERVER, Msg, 1000 * 60 * 10).

add_connection(State, Connection) ->
    State1 = case has_pool(State, Connection#conn.pool_name) of
        true -> State;
        false ->
            init_pool(State, Connection#conn.pool_name)
    end,
    State2 = add_member(State1, Connection#conn.pool_name, Connection#conn.pid),
    State2#state{
        connections = gb_trees:enter(Connection#conn.pid, Connection, State2#state.connections)
    }.

remove_connection(State, Pid) ->
    {value, Connection} = gb_trees:lookup(Pid, State#state.connections),
    {ok, State1} = remove_member(State, Connection#conn.pool_name, Pid),
    State1#state{
        connections = gb_trees:delete(Pid, State1#state.connections)
    }.

reset_connection(#conn{pid=Pid}=Conn) when Pid =/= undefined ->
    mysql_conn:close_socket(Pid),
    reset_connection(Conn#conn{pid = undefined});
reset_connection(Conn) ->
    case mysql_conn:start(Conn#conn.host, Conn#conn.port, Conn#conn.user, Conn#conn.password, Conn#conn.database, Conn#conn.encoding, Conn#conn.pool_name) of
        {ok, ConnPid} ->
            Conn#conn{ pid = ConnPid };
        _ -> undefined
    end.

%% @doc Encode a value so that it can be included safely in a MySQL query.
%%
%% @spec encode(Val::term(), AsBinary::bool()) ->
%%   string() | binary() | {error, Error}
encode(Val) ->
    encode(Val, false).
encode(Val, false) when Val == undefined; Val == null ->
    "null";
encode(Val, true) when Val == undefined; Val == null ->
    <<"null">>;
encode(Val, false) when is_binary(Val) ->
    binary_to_list(quote(Val));
encode(Val, true) when is_binary(Val) ->
    quote(Val);
encode(Val, true) ->
    list_to_binary(encode(Val,false));
encode(Val, false) when is_atom(Val) ->
    quote(atom_to_list(Val));
encode(Val, false) when is_list(Val) ->
    quote(Val);
encode(Val, false) when is_integer(Val) ->
    integer_to_list(Val);
encode(Val, false) when is_float(Val) ->
    [Res] = io_lib:format("~w", [Val]),
    Res;
encode({datetime, Val}, AsBinary) ->
    encode(Val, AsBinary);
encode({{Year, Month, Day}, {Hour, Minute, Second}}, false) ->
    Res = two_digits([Year, Month, Day, Hour, Minute, Second]),
    lists:flatten(Res);
encode({TimeType, Val}, AsBinary)
  when TimeType == 'date';
       TimeType == 'time' ->
    encode(Val, AsBinary);
encode({Time1, Time2, Time3}, false) ->
    Res = two_digits([Time1, Time2, Time3]),
    lists:flatten(Res);
encode(Val, _AsBinary) ->
    {error, {unrecognized_value, Val}}.

%% @private
two_digits(Nums) when is_list(Nums) ->
    [two_digits(Num) || Num <- Nums];
two_digits(Num) ->
    [Str] = io_lib:format("~b", [Num]),
    case length(Str) of
        1 -> [$0 | Str];
        _ -> Str
    end.

%%  Quote a string or binary value so that it can be included safely in a
%%  MySQL query.
quote(String) when is_list(String) ->
    [39 | lists:reverse([39 | quote(String, [])])];	%% 39 is $'
quote(Bin) when is_binary(Bin) ->
    list_to_binary(quote(binary_to_list(Bin))).

%% @private
quote([], Acc) ->
    Acc;
quote([0 | Rest], Acc) ->
    quote(Rest, [$0, $\\ | Acc]);
quote([10 | Rest], Acc) ->
    quote(Rest, [$n, $\\ | Acc]);
quote([13 | Rest], Acc) ->
    quote(Rest, [$r, $\\ | Acc]);
quote([$\\ | Rest], Acc) ->
    quote(Rest, [$\\ , $\\ | Acc]);
quote([39 | Rest], Acc) ->		%% 39 is $'
    quote(Rest, [39, $\\ | Acc]);	%% 39 is $'
quote([34 | Rest], Acc) ->		%% 34 is $"
    quote(Rest, [34, $\\ | Acc]);	%% 34 is $"
quote([26 | Rest], Acc) ->
    quote(Rest, [$Z, $\\ | Acc]);
quote([C | Rest], Acc) ->
    quote(Rest, [C | Acc]).


%% @doc Find the first zero-byte in Data and add everything before it
%%   to Acc, as a string.
%%
%% @spec asciz_binary(Data::binary(), Acc::list()) ->
%%   {NewList::list(), Rest::binary()}
asciz_binary(<<>>, Acc) ->
    {lists:reverse(Acc), <<>>};
asciz_binary(<<0:8, Rest/binary>>, Acc) ->
    {lists:reverse(Acc), Rest};
asciz_binary(<<C:8, Rest/binary>>, Acc) ->
    asciz_binary(Rest, [C | Acc]).
