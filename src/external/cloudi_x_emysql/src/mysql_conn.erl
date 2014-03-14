%%%-------------------------------------------------------------------
%%% Encoding: latin-1
%%% File    : mysql_conn.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: MySQL connection handler, handles de-framing of messages
%%%           received by the MySQL receiver process.
%%% Created :  5 Aug 2005 by Fredrik Thulin <ft@it.su.se>
%%% Modified: 11 Jan 2006 by Mickael Remond <mickael.remond@process-one.net>
%%%
%%% Note    : All MySQL code was written by Magnus Ahltorp, originally
%%%           in the file mysql.erl - I just moved it here.
%%%
%%% Modified: 12 Sep 2006 by Yariv Sadan <yarivvv@gmail.com>
%%% Added automatic type conversion between MySQL types and Erlang types
%%% and different logging style.
%%%
%%% Modified: 23 Sep 2006 by Yariv Sadan <yarivvv@gmail.com>
%%% Added transaction handling and prepared statement execution.
%%%
%%% Copyright (c) 2001-2004 Kungliga Tekniska Högskolan
%%% See the file COPYING

%% @type mysql_result = {list(), list(), integer(), integer(), integer(), integer(), string(), string()}
%% @type query_result = {data, mysql_result()} | {updated, mysql_result()} | {error, mysql_result()}
%% @type fieldinfo =  {term(), term(), term(), term()}
%% @todo Gut all of the transaction functionality.
%% @todo Enhance the data receive loops and protect against bad results.
%% @author Fredrik Thulin <ft@it.su.se>
%% @author Kungliga Tekniska Högskolan
%% @author Yariv Sadan <yarivvv@gmail.com>
%% @author Nick Gerakines <nick@gerakines.net>
%% @doc A module that directly creates and manages MySQL connections.
-module(mysql_conn).

-export([start/7, fetch/3, fetch/4, execute/6, execute/7, close_socket/1]).
-export([init/8, do_recv/3]).

-include("mysql.hrl").
-record(state,
    {
    init_arguments,
    mysql_version,
    recv_pid,
    socket,
    data,
    prepares = gb_trees:empty(),
    pool_id,
    last}).

-define(SECURE_CONNECTION, 32768).
-define(MYSQL_QUERY_OP, 3).
-define(DEFAULT_STANDALONE_TIMEOUT, 5000).
-define(TIMEOUT, 8000).
-define(MYSQL_4_0, 40). %% Support for MySQL 4.0.x
-define(MYSQL_4_1, 41). %% Support for MySQL 4.1.x et 5.0.x

%% @spec start(Host, Port, User, Password, Database, Encoding, PoolId) -> Result
%%       Host = string()
%%       Port = integer()
%%       User = string()
%%       Password = string()
%%       Database = string()
%%       Encoding = atom()
%%       PoolId = atom()
%%       Result = {ok, pid()} | {error, any()}
start(Host, Port, User, Password, Database, Encoding, PoolId) ->
    Pid = proc_lib:spawn_link(?MODULE, init, [Host, Port, User, Password, Database, Encoding, PoolId, self()]),
    get_ack(Pid).

%% @private
get_ack(Pid) ->
    receive
        Pid -> {ok, Pid};
        {error, Error} ->  {error, Error};
        _ -> get_ack(Pid)
    after 1000 * 5 ->
        {error, failed_to_open_connection}
    end.

%% @private
init(Host, Port, User, Password, Database, Encoding, PoolId, Parent) ->
    case mysql_recv:start_link(Host, Port, self()) of
        {ok, RecvPid, Sock} ->
            case mysql_init(Sock, RecvPid, User, Password) of
                {ok, Version} ->
                    case set_database(Sock, RecvPid, Version, Database) of
                        {error, _} = E -> Parent ! E;
                        ok ->
                            set_encoding(Sock, RecvPid, Version, Encoding),
                            Parent ! self(),
                            loop(#state{
                                init_arguments = {Host, Port, User, Password,
                                                  Database, Encoding, PoolId,
                                                  {undefined, undefined}},
                                mysql_version = Version,
                                recv_pid = RecvPid,
                                socket   = Sock,
                                pool_id  = PoolId,
                                data     = <<>>
                            })
                    end;
                {error, _} = E -> Parent ! E
            end;
        E -> Parent ! E
    end.

%% @equiv fetch(Pid, Queries, From, ?DEFAULT_STANDALONE_TIMEOUT)
fetch(Pid, Queries, From) ->
    fetch(Pid, Queries, From, ?TIMEOUT).

%% @spec fetch(Pid, Queries, From, Timeout) -> Result
%%       Pid = pid()
%%       Queries = [binary()]
%%       From = pid()
%%       Timeout = integer()
%%       Result = ok | {data, any()} | {updated, any()} | {error, any()}
%% @doc Send a query or a list of queries and wait for the result if running
%% stand-alone (From = self()), but don't block the caller if we are not
%% running stand-alone.
%% @todo Update this description, it's wrong.
fetch(Pid, Queries, From, undefined) ->
    fetch(Pid, Queries, From, ?TIMEOUT);
fetch(Pid, Queries, From, Timeout)  ->
    do_fetch(Pid, Queries, From, Timeout).

execute(Pid, Name, Version, Params, From, Stmt) ->
    execute(Pid, Name, Version, Params, From, Stmt, ?TIMEOUT).

execute(Pid, Name, Version, Params, From, Stmt, undefined) -> 
    execute(Pid, Name, Version, Params, From, Stmt, ?TIMEOUT);
execute(Pid, Name, Version, Params, From, Stmt, Timeout) ->
    send_msg(Pid, {execute, Name, Version, Params, From, Stmt, Timeout}, From).

close_socket(Pid) ->
    send_msg(Pid, close_socket, fuck_off).

set_database(_, _, _, undefined) -> ok;
set_database(Sock, RecvPid, Version, Database) ->
    Db = iolist_to_binary(Database),
    case do_query(Sock, RecvPid, <<"use ", Db/binary>>, Version, ?TIMEOUT) of
        {error, _MySQLRes} -> {error, failed_changing_database};
        _ -> ok
    end.

set_encoding(_, _, _, undefined) -> ok;
set_encoding(Sock, RecvPid, Version, Encoding) when is_atom(Encoding) ->
    EncodingBinary = erlang:atom_to_binary(Encoding, utf8),
    do_query(Sock, RecvPid, <<"set names '", EncodingBinary/binary, "'">>, Version, ?TIMEOUT);
set_encoding(_, _, _, _) -> ok.

%% @private
loop(State) ->
    RecvPid = State#state.recv_pid,
    receive
        {'$mysql_conn_loop', {From, Mref}, {fetch, Queries, Timeout}} ->
            gen:reply({From, Mref}, (catch do_queries(State, Queries, Timeout))),
            loop(State);
        {'$mysql_conn_loop', {From, Mref}, {execute, Name, Version, Params, _From1, Stmt, Timeout}} ->
            NewState = case do_execute(State, Name, Params, Version, Stmt, Timeout) of    
                {error, _} = Err ->
                    gen:reply({From, Mref}, Err),
                    State;
                {ok, Result, OutState} ->
                    gen:reply({From, Mref}, Result),
                    OutState
            end,
            loop(NewState);
        {'$mysql_conn_loop', {From, Mref}, close_socket} ->
            gen_tcp:close(State#state.socket),
            gen:reply({From, Mref}, ok),
            ok;
        {'$mysql_conn_loop', {_From, _Mref}, {mysql_recv, RecvPid, data, Packet, Num}} ->
            error_logger:error_report([?MODULE, ?LINE, unexpected_packet, {num, Num}, {packet, Packet}]),
            loop(State);
        {'$mysql_conn_loop', {_From, _Mref}, Unknown} ->
            error_logger:error_report([?MODULE, ?LINE, {unsuppoted_message, Unknown}]),
            loop(State);
        % reconnect when the socket magically closes for no reason
        {mysql_recv, _, closed, normal} ->
            gen_tcp:close(State#state.socket),
            {Host, Port, User, Password,
             Database, Encoding, PoolId, _} =
                State#state.init_arguments,
            Parent = self(),
            Child = erlang:spawn_link(fun() ->
                case get_ack(Parent) of
                    {ok, _} ->
                        ok;
                    {error, Reason} ->
                        erlang:exit(Parent, Reason)
                end
            end),
            init(Host, Port, User, Password,
                 Database, Encoding, PoolId, Child);
        Unknown ->
            error_logger:error_report([?MODULE, ?LINE, {unsuppoted_message, Unknown}]),
            loop(State)
    end.

%% @private
do_query(State, Query, Timeout) ->
    do_query(
        State#state.socket,
        State#state.recv_pid,
        Query,
        State#state.mysql_version,
        Timeout
    ).

%% @private
do_query(Sock, RecvPid, Query, Version, Timeout) ->
    Query1 = iolist_to_binary(Query),
    Packet =  <<?MYSQL_QUERY_OP, Query1/binary>>,
    case do_send(Sock, Packet, 0) of
        ok ->
            get_query_response(RecvPid, Version, Timeout);
        {error, Reason} ->
            Msg = io_lib:format("Failed sending data on socket : ~p", [Reason]),
            {error, Msg}
    end.

%% @private
do_queries(State, Queries, Timeout) when not is_list(Queries) ->
    do_query(State, Queries, Timeout);
do_queries(State, Queries, Timeout) ->
    do_queries(
        State#state.socket,
        State#state.recv_pid,
        Queries,
        State#state.mysql_version,
        Timeout
    ).

%% @doc Execute a list of queries, returning the response for the last query.
%% If a query returns an error before the last query is executed, the
%% loop is aborted and the error is returned. 
%% @private
do_queries(Sock, RecvPid, Queries, Version, Timeout) ->
    catch(
        lists:foldl(
            fun(Query, _LastResponse) ->
                case do_query(Sock, RecvPid, Query, Version, Timeout) of
                    {error, _} = Err -> exit(Err);
                    Res -> Res
                end
            end,
            ok,
            Queries
        )
    ).

%% @private
do_execute(State, Name, Params, _ExpectedVersion, Stmt, Timeout) ->
    case State#state.last of
        {Name, Stmt} -> {ok, do_execute1(State, Name, Params, Timeout), State};
        {OldName, _} ->
            OldNameBin = erlang:atom_to_binary(OldName, utf8),
            case do_query(State#state.socket, State#state.recv_pid, <<"DEALLOCATE PREPARE ", OldNameBin/binary>>, State#state.mysql_version, 1000) of
                {updated, _} -> prepare_and_execute(State, Name, Stmt, Params, Timeout);
                {error, Err} -> {error, Err};
                Other -> {error, {unexpected_result, Other}}
            end;
        undefined ->
            prepare_and_execute(State, Name, Stmt, Params, Timeout)
    end.

%% @private
prepare_and_execute(State, Name, Stmt, Params, Timeout) ->
    NameBin = erlang:atom_to_binary(Name, utf8),
    StmtBin = <<"PREPARE ", NameBin/binary, " FROM '", Stmt/binary, "'">>,
    case do_query(State, StmtBin, Timeout) of
        {updated, _} ->
            State1 = State#state{
                last = {Name, Stmt}
            },
            {ok, do_execute1(State1, Name, Params, Timeout), State1};
        {error, _} = Err ->
            Err;
        Other ->
            {error, {unexpected_result, Other}}
    end.

%% @private
do_execute1(State, Name, Params, Timeout) ->
    Stmts = make_statements_for_execute(Name, Params),
    do_queries(State, Stmts, Timeout).

%% @private
make_statements_for_execute(Name, []) ->
    NameBin = erlang:atom_to_binary(Name, utf8),
    [<<"EXECUTE ", NameBin/binary>>];
make_statements_for_execute(Name, Params) ->
    NumParams = length(Params),
    ParamNums = lists:seq(1, NumParams),

    NameBin = erlang:atom_to_binary(Name, utf8),
    
    ParamNames = lists:foldl(
        fun(Num, Acc) ->
            ParamName = [$@ | integer_to_list(Num)],
            if
                Num == 1 -> ParamName ++ Acc;
                true -> [$, | ParamName] ++ Acc
            end
        end,
        [], lists:reverse(ParamNums)
    ),
    ParamNamesBin = list_to_binary(ParamNames),

    ExecStmt = <<"EXECUTE ", NameBin/binary, " USING ", ParamNamesBin/binary>>,

    ParamVals = lists:zip(ParamNums, Params),
    Stmts = lists:foldl(
        fun({Num, Val}, Acc) ->
            NumBin = mysql:encode(Num, true),
            ValBin = mysql:encode(Val, true),
            [<<"SET @", NumBin/binary, "=", ValBin/binary>> | Acc]
        end,
        [ExecStmt], lists:reverse(ParamVals)
    ),
    Stmts.

%% @private
do_recv(RecvPid, SeqNum, Timeout) when SeqNum == undefined, is_integer(Timeout) ->
    receive
        {mysql_recv, RecvPid, data, Packet, Num} ->
            {ok, Packet, Num};
        {mysql_recv, RecvPid, closed, _E} ->
            {error, "mysql_recv: socket was closed"}
    after Timeout ->
        exit(timeout)
    end;
do_recv(RecvPid, SeqNum, Timeout) when is_integer(SeqNum), is_integer(Timeout) ->
    ResponseNum = SeqNum + 1,
    receive
        {mysql_recv, RecvPid, data, Packet, ResponseNum} ->
            {ok, Packet, ResponseNum};
        {mysql_recv, RecvPid, closed, _E} ->
            {error, "mysql_recv: socket was closed"}
    after Timeout ->
        exit(timeout)
    end.

%% @private
do_fetch(Pid, Queries, From, Timeout) ->
    send_msg(Pid, {fetch, Queries, Timeout}, From).

%% @private
send_msg(Pid, Msg, _From) ->
    {ok, Response} = gen:call(Pid, '$mysql_conn_loop', Msg, 1000 * 60 * 10),
    case Response of
        {fetch_result, _Pid, Result} -> 
            Result;
        {'EXIT', Error} ->
            %% error_logger:error_report({?MODULE, send_msg, Error}),
            %% whereis(mysql_dispatcher) ! {remove_connection, Pid},
            {error, #mysql_result{error=lists:flatten(io_lib:format("~p", [Error]))}};
        Other -> 
            Other
    end.

%% @private
mysql_init(Sock, RecvPid, User, Password) ->
    receive
        {mysql_recv, RecvPid, data, Packet, InitSeqNum} ->
            {Version, Salt1, Salt2, Caps} = greeting(Packet),
            AuthRes = case Caps band ?SECURE_CONNECTION of
                ?SECURE_CONNECTION ->
                    mysql_auth:do_new_auth(Sock, RecvPid, InitSeqNum + 1, User, Password, Salt1, Salt2);
                _ ->
                    mysql_auth:do_old_auth(Sock, RecvPid, InitSeqNum + 1, User, Password, Salt1)
            end,
            case AuthRes of
                {ok, <<0:8, _Rest/binary>>, _RecvNum} ->
                    {ok,Version};
                {ok, <<255:8, _Code:16/little, Message/binary>>, _RecvNum} ->
                    {error, binary_to_list(Message)};
                {ok, RecvPacket, _RecvNum} ->
                    {error, binary_to_list(RecvPacket)};
                {error, Reason} ->
                    {error, Reason}
            end;
        {mysql_recv, RecvPid, closed, _E} ->
            {error, "mysql_recv: socket was closed"}
    end.

%% @private
greeting(Packet) ->
    <<_Protocol:8, Rest/binary>> = Packet,
    {Version, Rest2} = asciz(Rest),
    <<_TreadID:32/little, Rest3/binary>> = Rest2,
    {Salt, Rest4} = asciz(Rest3),
    <<Caps:16/little, Rest5/binary>> = Rest4,
    <<_ServerChar:16/binary-unit:8, Rest6/binary>> = Rest5,
    {Salt2, _Rest7} = asciz(Rest6),
    {normalize_version(Version), Salt, Salt2, Caps}.

%% @private
asciz(Data) when is_binary(Data) ->
    mysql:asciz_binary(Data, []).%;
%asciz(Data) when is_list(Data) ->
%    {String, [0 | Rest]} = lists:splitwith(fun (C) -> C /= 0 end, Data),
%    {String, Rest}.

%% @private
%% @todo This really needs to be cleaned up.
get_query_response(RecvPid, Version, Timeout) ->
    case do_recv(RecvPid, undefined, Timeout) of
        {ok, <<Fieldcount:8, Rest/binary>>, _} ->
            case Fieldcount of
                0 ->
                    {ok, AffectedRows, Rest1} = get_length_coded_binary(Rest),
                    {ok, InsertID, Rest2} = get_length_coded_binary(Rest1),
                    <<ServerStatus:16/integer, Rest3/binary>> = Rest2,
                    <<WarningCount:16/integer, Message0/binary>> = Rest3,
                    {ok, Message, _} = get_length_coded_string(Message0),
                    Res = #mysql_result{
                        affectedrows = AffectedRows, 
                        insert_id = InsertID,
                        server_status = ServerStatus,
                        warning_count = WarningCount,
                        message = Message
                    },
                    {updated, Res};
                255 ->
                    <<_Code:16/little, Message/binary>>  = Rest,
                    {error, #mysql_result{error=Message}};
                _ ->
                    %% Tabular data received
                    case get_fields(RecvPid, [], Version, Timeout) of
                        {ok, Fields} ->
                            case get_rows(Fields, RecvPid, [], Timeout) of
                                {ok, Rows} -> {data, #mysql_result{fieldinfo=Fields, rows=Rows}};
                                {error, Reason} ->{error, #mysql_result{error=Reason}}
                            end;
                        {error, Reason} ->
                            {error, #mysql_result{error=Reason}}
                    end
            end;
        {error, Reason} ->
            {error, #mysql_result{error=Reason}}
    end.

%% @private
get_length_coded_binary(<<FirstByte:8, Rest/binary>>) ->
    if
        FirstByte =< 250 -> {ok, FirstByte, Rest};
        FirstByte == 251 -> {ok, FirstByte, Rest};
        FirstByte == 252 ->
            case Rest of
                <<FollowingBytes:16/integer, Rest1/binary>> -> {ok, FollowingBytes, Rest1};
                _ -> {ok, bad_packet, <<>>}
            end;
        FirstByte == 253 ->
            case Rest of
                <<FollowingBytes:24/integer, Rest1/binary>> -> {ok, FollowingBytes, Rest1};
                _ -> {ok, bad_packet, <<>>}
            end;
        FirstByte == 254 ->
            case Rest of
                <<FollowingBytes:64/integer, Rest1/binary>> -> {ok, FollowingBytes, Rest1};
                _ -> {ok, bad_packet, <<>>}
            end;
        true ->
            {ok, bad_packet, <<>>}
    end;
get_length_coded_binary(_) ->
    {ok, bad_packet, <<>>}.

%% @private
get_length_coded_string(<<>>) -> {ok, "", <<>>};
get_length_coded_string(Bin) ->
    {ok, Length, Rest} = get_length_coded_binary(Bin),
    case Rest of
        <<String:Length/binary, Rest1/binary>> -> {ok, binary_to_list(String), Rest1};
        _ -> {ok, bad_packet, <<>>}
    end.

%% @private
get_fields(RecvPid, Res, ?MYSQL_4_0, Timeout) ->
    case do_recv(RecvPid, undefined, Timeout) of
        {ok, Packet, _Num} ->
            case Packet of
                <<254:8>> -> {ok, lists:reverse(Res)};
                <<254:8, Rest/binary>> when size(Rest) < 8 -> {ok, lists:reverse(Res)};
                _ ->
                    {Table, Rest} = get_with_length(Packet),
                    {Field, Rest2} = get_with_length(Rest),
                    {LengthB, Rest3} = get_with_length(Rest2),
                    LengthL = size(LengthB) * 8,
                    <<Length:LengthL/little>> = LengthB,
                    {Type, Rest4} = get_with_length(Rest3),
                    {_Flags, _Rest5} = get_with_length(Rest4),
                    This = {Table,
                        Field,
                        Length,
                        %% TODO: Check on MySQL 4.0 if types are specified
                        %%       using the same 4.1 formalism and could 
                        %%       be expanded to atoms:
                        Type},
                    get_fields(RecvPid, [This | Res], ?MYSQL_4_0, Timeout)
            end;
        {error, Reason} ->
            {error, Reason}
    end;
%% Support for MySQL 4.1.x and 5.x:
get_fields(RecvPid, Res, ?MYSQL_4_1, Timeout) ->
    case do_recv(RecvPid, undefined, Timeout) of
        {ok, Packet, _Num} ->
            case Packet of
                <<254:8>> -> {ok, lists:reverse(Res)};
                <<254:8, Rest/binary>> when size(Rest) < 8 -> {ok, lists:reverse(Res)};
                _ ->
                    {_Catalog, Rest} = get_with_length(Packet),
                    {_Database, Rest2} = get_with_length(Rest),
                    {Table, Rest3} = get_with_length(Rest2),
                    %% OrgTable is the real table name if Table is an alias
                    {_OrgTable, Rest4} = get_with_length(Rest3),
                    {Field, Rest5} = get_with_length(Rest4),
                    %% OrgField is the real field name if Field is an alias
                    {_OrgField, Rest6} = get_with_length(Rest5),

                    %% <<Metadata:8/little, Charset:16/little, Length:32/little, Type:8/little, Flags:16/little, Decimals:8:litle, .../binary>>
                    <<_:8/little, _:16/little, Length:32/little, Type:8/little, _:16/little, _:8/little, _Rest7/binary>> = Rest6,

                    This = {Table, Field, Length, get_field_datatype(Type)},
                    get_fields(RecvPid, [This | Res], ?MYSQL_4_1, Timeout)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @private
get_rows(Fields, RecvPid, Res, Timeout) ->
    case do_recv(RecvPid, undefined, Timeout) of
        {ok, Packet, _Num} ->
            case Packet of
                <<254:8, Rest/binary>> when size(Rest) < 8 -> {ok, lists:reverse(Res)};
                _ ->
                    {ok, This} = get_row(Fields, Packet, []),
                    get_rows(Fields, RecvPid, [This | Res], Timeout)
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @private
get_row([], _Data, Res) ->
    {ok, lists:reverse(Res)};
get_row([Field | OtherFields], Data, Res) ->
    {Col, Rest} = get_with_length(Data),
    This = case Col of
        null -> undefined;
        _ -> convert_type(Col, element(4, Field))
    end,
    get_row(OtherFields, Rest, [This | Res]).

%% @private
get_with_length(<<251:8, Rest/binary>>) ->
    {null, Rest};
get_with_length(<<252:8, Length:16/little, Rest/binary>>) ->
    split_binary(Rest, Length);
get_with_length(<<253:8, Length:24/little, Rest/binary>>) ->
    split_binary(Rest, Length);
get_with_length(<<254:8, Length:64/little, Rest/binary>>) ->
    split_binary(Rest, Length);
get_with_length(<<Length:8, Rest/binary>>) when Length < 251 ->
    split_binary(Rest, Length).


%% @private
do_send(Sock, Packet, SeqNum) when is_binary(Packet), is_integer(SeqNum) ->
    Data = <<(size(Packet)):24/little, SeqNum:8, Packet/binary>>,
    gen_tcp:send(Sock, Data).

%% @private
normalize_version([$4,$.,$0|_T]) ->
    ?MYSQL_4_0;
normalize_version([$4,$.,$1|_T]) ->
    ?MYSQL_4_1;
normalize_version([$5|_T]) ->
    %% MySQL version 5.x protocol is compliant with MySQL 4.1.x:
    ?MYSQL_4_1; 
normalize_version(_Other) ->
    %% Error, but trying the oldest protocol anyway:
    ?MYSQL_4_0.

%% @private
get_field_datatype(0) ->   'DECIMAL';
get_field_datatype(1) ->   'TINY';
get_field_datatype(2) ->   'SHORT';
get_field_datatype(3) ->   'LONG';
get_field_datatype(4) ->   'FLOAT';
get_field_datatype(5) ->   'DOUBLE';
get_field_datatype(6) ->   'NULL';
get_field_datatype(7) ->   'TIMESTAMP';
get_field_datatype(8) ->   'LONGLONG';
get_field_datatype(9) ->   'INT24';
get_field_datatype(10) ->  'DATE';
get_field_datatype(11) ->  'TIME';
get_field_datatype(12) ->  'DATETIME';
get_field_datatype(13) ->  'YEAR';
get_field_datatype(14) ->  'NEWDATE';
get_field_datatype(246) -> 'NEWDECIMAL';
get_field_datatype(247) -> 'ENUM';
get_field_datatype(248) -> 'SET';
get_field_datatype(249) -> 'TINYBLOB';
get_field_datatype(250) -> 'MEDIUM_BLOG';
get_field_datatype(251) -> 'LONG_BLOG';
get_field_datatype(252) -> 'BLOB';
get_field_datatype(253) -> 'VAR_STRING';
get_field_datatype(254) -> 'STRING';
get_field_datatype(255) -> 'GEOMETRY'.

%% @todo Handle leftovers.
convert_type(Val, ColType) ->
    case ColType of
        T when T == 'TINY'; T == 'SHORT'; T == 'LONG'; T == 'LONGLONG'; T == 'INT24'; T == 'YEAR' ->
            list_to_integer(binary_to_list(Val));
        T when T == 'TIMESTAMP'; T == 'DATETIME' ->
            {ok, [Year, Month, Day, Hour, Minute, Second], _Leftovers} =
            io_lib:fread("~d-~d-~d ~d:~d:~d", binary_to_list(Val)),
            {datetime, {{Year, Month, Day}, {Hour, Minute, Second}}};
        'TIME' ->
            {ok, [Hour, Minute, Second], _Leftovers} =
            io_lib:fread("~d:~d:~d", binary_to_list(Val)),
            {time, {Hour, Minute, Second}};
        'DATE' ->
            {ok, [Year, Month, Day], _Leftovers} =
            io_lib:fread("~d-~d-~d", binary_to_list(Val)),
            {date, {Year, Month, Day}};
        T when T == 'DECIMAL'; T == 'NEWDECIMAL'; T == 'FLOAT'; T == 'DOUBLE' ->
            {ok, [Num], _Leftovers} = case io_lib:fread("~f", binary_to_list(Val)) of
                {error, _} -> io_lib:fread("~d", binary_to_list(Val));
                Res -> Res
            end,
            Num;
        _Other ->
            Val
    end.
