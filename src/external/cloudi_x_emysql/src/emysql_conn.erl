%% Copyright (c) 2009-2012
%% Bill Warnecke <bill@rupture.com>,
%% Jacob Vorreuter <jacob.vorreuter@gmail.com>,
%% Henning Diedrich <hd2010@eonblast.com>,
%% Eonblast Corporation <http://www.eonblast.com>
%%
%% Permission is  hereby  granted,  free of charge,  to any person
%% obtaining  a copy of this software and associated documentation
%% files (the "Software"),to deal in the Software without restric-
%% tion,  including  without  limitation the rights to use,  copy,
%% modify, merge,  publish,  distribute,  sublicense,  and/or sell
%% copies  of the  Software,  and to  permit  persons to  whom the
%% Software  is  furnished  to do  so,  subject  to the  following
%% conditions:
%%
%% The above  copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF  MERCHANTABILITY,  FITNESS  FOR  A  PARTICULAR  PURPOSE  AND
%% NONINFRINGEMENT. IN  NO  EVENT  SHALL  THE AUTHORS OR COPYRIGHT
%% HOLDERS  BE  LIABLE FOR  ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT,  TORT  OR OTHERWISE,  ARISING
%% FROM,  OUT OF OR IN CONNECTION WITH THE SOFTWARE  OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%% @private
-module(emysql_conn).
-export([set_database/2, set_encoding/2,
        execute/3, prepare/3, unprepare/2,
        open_connections/1, open_connection/1,
        reset_connection/3, close_connection/1,
        open_n_connections/2, hstate/1,
        test_connection/2, need_test_connection/1
]).

-include("emysql.hrl").

set_database(_, undefined) -> ok;
set_database(_, Empty) when Empty == ""; Empty == <<>> -> ok;
set_database(Connection, Database) ->
    Packet = <<?COM_QUERY, "use `", (iolist_to_binary(Database))/binary, "`">>,  % todo: utf8?
    emysql_tcp:send_and_recv_packet(Connection#emysql_connection.socket, Packet, 0).

set_encoding(_, undefined) -> ok;
set_encoding(Connection, Encoding) ->
    Packet = <<?COM_QUERY, "set names '", (erlang:atom_to_binary(Encoding, utf8))/binary, "'">>,
    emysql_tcp:send_and_recv_packet(Connection#emysql_connection.socket, Packet, 0).

%% @todo This can go away once the underlying socket accepts IOData
canonicalize_query(Q) when is_binary(Q) -> Q;
canonicalize_query(QL) when is_list(QL) -> iolist_to_binary(QL).

execute(Connection, StmtName, []) when is_atom(StmtName) ->
    prepare_statement(Connection, StmtName),
    StmtNameBin = atom_to_binary(StmtName, utf8),
    Packet = <<?COM_QUERY, "EXECUTE ", StmtNameBin/binary>>,
    emysql_tcp:send_and_recv_packet(Connection#emysql_connection.socket, Packet, 0);
execute(#emysql_connection { socket = Sock }, Query, []) ->
    QB = canonicalize_query(Query),
    Packet = <<?COM_QUERY, QB/binary>>,
    emysql_tcp:send_and_recv_packet(Sock, Packet, 0);
execute(Connection, Query, Args) when (is_list(Query) orelse is_binary(Query)) andalso is_list(Args) ->
    StmtName = "stmt_"++integer_to_list(erlang:phash2(Query)),
    ok = prepare(Connection, StmtName, Query),
    Ret =
    case set_params(Connection, 1, Args, undefined) of
        OK when is_record(OK, ok_packet) ->
            ParamNamesBin = list_to_binary(string:join([[$@ | integer_to_list(I)] || I <- lists:seq(1, length(Args))], ", ")),  % todo: utf8?
            Packet = <<?COM_QUERY, "EXECUTE ", (list_to_binary(StmtName))/binary, " USING ", ParamNamesBin/binary>>,  % todo: utf8?
            emysql_tcp:send_and_recv_packet(Connection#emysql_connection.socket, Packet, 0);
        Error ->
            Error
    end,
    unprepare(Connection, StmtName),
    Ret;

execute(Connection, StmtName, Args) when is_atom(StmtName), is_list(Args) ->
    prepare_statement(Connection, StmtName),
    case set_params(Connection, 1, Args, undefined) of
        OK when is_record(OK, ok_packet) ->
            ParamNamesBin = list_to_binary(string:join([[$@ | integer_to_list(I)] || I <- lists:seq(1, length(Args))], ", ")),  % todo: utf8?
            StmtNameBin = atom_to_binary(StmtName, utf8),
            Packet = <<?COM_QUERY, "EXECUTE ", StmtNameBin/binary, " USING ", ParamNamesBin/binary>>,
            emysql_tcp:send_and_recv_packet(Connection#emysql_connection.socket, Packet, 0);
        Error ->
            Error
    end.

prepare(Connection, Name, Statement) when is_atom(Name) ->
    prepare(Connection, atom_to_list(Name), Statement);
prepare(Connection, Name, Statement) ->
    StatementBin = encode(Statement, binary),
    Packet = <<?COM_QUERY, "PREPARE ", (list_to_binary(Name))/binary, " FROM ", StatementBin/binary>>,  % todo: utf8?
    case emysql_tcp:send_and_recv_packet(Connection#emysql_connection.socket, Packet, 0) of
        OK when is_record(OK, ok_packet) ->
            ok;
        Err when is_record(Err, error_packet) ->
            exit({failed_to_prepare_statement, Err#error_packet.msg})
    end.

unprepare(Connection, Name) when is_atom(Name)->
    unprepare(Connection, atom_to_list(Name));
unprepare(Connection, Name) ->
    Packet = <<?COM_QUERY, "DEALLOCATE PREPARE ", (list_to_binary(Name))/binary>>,  % todo: utf8?
    emysql_tcp:send_and_recv_packet(Connection#emysql_connection.socket, Packet, 0).

open_n_connections(PoolId, N) ->
    case emysql_conn_mgr:find_pool(PoolId, emysql_conn_mgr:pools()) of
        {Pool, _} ->
            lists:foldl(fun(_, {Conns, Reasons}) ->
                %% Catch {'EXIT',_} errors so newly opened connections are not orphaned.
                %% We do not want to close all the connections here like in
                %% open_connections/2. Struggle to keep working.
                case catch open_connection(Pool) of
                    #emysql_connection{} = Connection ->
                        {[Connection | Conns], Reasons};
                    {'EXIT', Reason} ->
                        {Conns, [Reason | Reasons]}
                end
            end, {[], []}, lists:seq(1, N));
        _ ->
            exit(pool_not_found)
    end.

%% @doc Opens connections for the necessary pool.
%%
%% If connection opening fails, removes all connections from the pool
%% Does not remove pool from emysql_conn_mgr due to a possible deadlock.
%% Caller must do it by itself.
open_connections(Pool) ->
     %-% io:format("open connections loop: .. "),
    case (queue:len(Pool#pool.available) + gb_trees:size(Pool#pool.locked)) < Pool#pool.size of
        true ->
            case catch open_connection(Pool) of
                #emysql_connection{} = Conn ->
                    open_connections(Pool#pool{available = queue:in(Conn, Pool#pool.available)});
                {'EXIT', Reason} ->
                    AllConns = lists:append(
                        queue:to_list(Pool#pool.available),
                        gb_trees:values(Pool#pool.locked)
                    ),
                    lists:foreach(fun emysql_conn:close_connection/1, AllConns),
                    {error, Reason}
			end;
        false ->
            {ok, Pool}
    end.

open_connection(#pool{pool_id=PoolId, host=Host, port=Port, user=User,
        password=Password, database=Database, encoding=Encoding,
                      start_cmds=StartCmds, connect_timeout=ConnectTimeout} = Pool) ->
     %-% io:format("~p open connection for pool ~p host ~p port ~p user ~p base ~p~n", [self(), PoolId, Host, Port, User, Database]),
     %-% io:format("~p open connection: ... connect ... ~n", [self()]),
    case gen_tcp:connect(Host, Port, [binary, {packet, raw}, {active, false}, {recbuf, ?TCP_RECV_BUFFER}], ConnectTimeout) of
        {ok, Sock} ->
            #greeting {
               server_version = Version,
               thread_id = ThreadId,
               caps = Caps,
               language = Language
              } = handshake(Sock, User, Password),
            Connection = #emysql_connection{
                            id = erlang:port_to_list(Sock),
                            pool_id = PoolId,
                            encoding = Encoding,
                            socket = Sock,
                            version = Version,
                            thread_id = ThreadId,
                            caps = Caps,
                            language = Language,
                            test_period = Pool#pool.conn_test_period,
                            last_test_time = now_seconds()
                           },
            %%-% io:format("~p open connection: ... set db ...~n", [self()]),
            ok = set_database_or_die(Connection, Database),
            ok = set_encoding_or_die(Connection, Encoding),
            ok = run_startcmds_or_die(Connection, StartCmds),
            ok = give_manager_control(Sock),
            Connection;
        {error, Reason} ->
             %-% io:format("~p open connection: ... ERROR ~p~n", [self(), Reason]),
             %-% io:format("~p open connection: ... exit with failed_to_connect_to_database~n", [self()]),
            exit({failed_to_connect_to_database, Reason})
    end.

handshake(Sock, User, Password) ->
   case emysql_auth:handshake(Sock, User, Password) of
       {ok, #greeting{} = G} -> G;
       {error, Reason} ->
           gen_tcp:close(Sock),
           exit(Reason)
   end.

give_manager_control(Socket) ->
    case emysql_conn_mgr:give_manager_control(Socket) of
        {error, Reason} ->
            gen_tcp:close(Socket),
            exit({Reason,
                 "Failed to find conn mgr when opening connection. Make sure crypto is started and emysql.app is in the Erlang path."});
        ok -> ok
   end.

set_database_or_die(#emysql_connection { socket = Socket } = Connection, Database) ->
    case set_database(Connection, Database) of
        ok -> ok;
        OK1 when is_record(OK1, ok_packet) -> ok;
        Err1 when is_record(Err1, error_packet) ->
             gen_tcp:close(Socket),
             exit({failed_to_set_database, Err1#error_packet.msg})
    end.

run_startcmds_or_die(#emysql_connection{socket=Socket}, StartCmds) ->
    lists:foreach(
        fun(Cmd) ->
                Packet = <<?COM_QUERY, Cmd/binary>>,
                case emysql_tcp:send_and_recv_packet(Socket, Packet, 0) of
                    OK when OK =:= ok orelse is_record(OK, ok_packet) ->
                        ok;
                    #error_packet{msg=Msg} ->
                        gen_tcp:close(Socket),
                        exit({failed_to_run_cmd, Msg})
                end
        end,
        StartCmds
    ).
 
set_encoding_or_die(#emysql_connection { socket = Socket } = Connection, Encoding) ->
    case set_encoding(Connection, Encoding) of
        ok -> ok;
        OK2 when is_record(OK2, ok_packet) -> ok;
        Err2 when is_record(Err2, error_packet) ->
            gen_tcp:close(Socket),
            exit({failed_to_set_encoding, Err2#error_packet.msg})
    end.
 
reset_connection(Pools, Conn, StayLocked) ->
    %% if a process dies or times out while doing work
    %% the socket must be closed and the connection reset
    %% in the conn_mgr state. Also a new connection needs
    %% to be opened to replace the old one. If that fails,
    %% we queue the old as available for the next try
    %% by the next caller process coming along. So the
    %% pool can't run dry, even though it can freeze.
    %-% io:format("resetting connection~n"),
    MonitorRef = Conn#emysql_connection.monitor_ref,
    close_connection(Conn),
    %% OPEN NEW SOCKET
    case emysql_conn_mgr:find_pool(Conn#emysql_connection.pool_id, Pools) of
        {Pool, _} ->
            case catch open_connection(Pool) of
                #emysql_connection{} = NewConn when StayLocked == pass ->
                    NewConn2 = add_monitor_ref(NewConn, MonitorRef),
                    ok = emysql_conn_mgr:replace_connection_as_available(Conn, NewConn2),
                    NewConn2;
                #emysql_connection{} = NewConn when StayLocked == keep ->
                    NewConn2 = add_monitor_ref(NewConn, MonitorRef),
                    ok = emysql_conn_mgr:replace_connection_as_locked(Conn, NewConn2),
                    NewConn2;
                {'EXIT', Reason} ->
                    DeadConn = Conn#emysql_connection { alive = false, last_test_time = 0 },
                    emysql_conn_mgr:replace_connection_as_available(Conn, DeadConn),
                    {error, {cannot_reopen_in_reset, Reason}}
            end;
        undefined ->
            exit(pool_not_found)
    end.

add_monitor_ref(Conn, MonitorRef) ->
    Conn#emysql_connection{monitor_ref = MonitorRef}.

close_connection(Conn) ->
	%% garbage collect statements
	emysql_statements:remove(Conn#emysql_connection.id),
	ok = gen_tcp:close(Conn#emysql_connection.socket).

test_connection(Conn, StayLocked) ->
  case catch emysql_tcp:send_and_recv_packet(Conn#emysql_connection.socket, <<?COM_PING>>, 0) of
    {'EXIT', _} ->
      case reset_connection(emysql_conn_mgr:pools(), Conn, StayLocked) of
        NewConn when is_record(NewConn, emysql_connection) ->
          NewConn;
        {error, FailedReset} ->
          exit({connection_down, {and_conn_reset_failed, FailedReset}})
      end;
    _ ->
       NewConn = Conn#emysql_connection{last_test_time = now_seconds()},
       case StayLocked of
         pass -> emysql_conn_mgr:replace_connection_as_available(Conn, NewConn);
         keep -> emysql_conn_mgr:replace_connection_as_locked(Conn, NewConn)
       end,
       NewConn
  end.

need_test_connection(Conn) ->
   (Conn#emysql_connection.test_period =:= 0) orelse
     (Conn#emysql_connection.last_test_time =:= 0) orelse
     (Conn#emysql_connection.last_test_time + Conn#emysql_connection.test_period < now_seconds()).

now_seconds() ->
   {M, S, _} = erlang:now(),
   M * 1000000 + S.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
set_params(_, _, [], Result) -> Result;
set_params(Connection, Num, Values, _) ->
	Packet = set_params_packet(Num, Values),
	emysql_tcp:send_and_recv_packet(Connection#emysql_connection.socket, Packet, 0).

set_params_packet(NumStart, Values) ->
	BinValues = [encode(Val, binary) || Val <- Values],
	BinNums = [encode(Num, binary) || Num <- lists:seq(NumStart, NumStart + length(Values) - 1)],
	BinPairs = lists:zip(BinNums, BinValues),
	Parts = [<<"@", NumBin/binary, "=", ValBin/binary>> || {NumBin, ValBin} <- BinPairs], 
	Sets = list_to_binary(join(Parts, <<",">>)),
	<<?COM_QUERY, "SET ", Sets/binary>>.

%% @doc Join elements of list with Sep
%%
%% 1> join([1,2,3], 0).
%% [1,0,2,0,3]

join([], _Sep) -> [];
join(L, Sep) -> join(L, Sep, []).

join([H], _Sep, Acc)  -> lists:reverse([H|Acc]);
join([H|T], Sep, Acc) -> join(T, Sep, [Sep, H|Acc]).

prepare_statement(Connection, StmtName) ->
    case emysql_statements:fetch(StmtName) of
        undefined ->
            exit(statement_has_not_been_prepared);
        {Version, Statement} ->
            case emysql_statements:version(Connection#emysql_connection.id, StmtName) of
                Version ->
                    ok;
                _ ->
                    ok = prepare(Connection, StmtName, Statement),
                    emysql_statements:prepare(Connection#emysql_connection.id, StmtName, Version)
            end
    end.

% human readable string rep of the server state flag
%% @private
hstate(State) ->
       case (State band ?SERVER_STATUS_AUTOCOMMIT)   of 0 -> ""; _-> "AUTOCOMMIT " end
    ++ case (State band ?SERVER_MORE_RESULTS_EXIST)  of 0 -> ""; _-> "MORE_RESULTS_EXIST " end
    ++ case (State band ?SERVER_QUERY_NO_INDEX_USED) of 0 -> ""; _-> "NO_INDEX_USED " end.

%% @doc Encode a value so that it can be included safely in a MySQL query.
%% @spec encode(term(), list | binary) -> string() | binary() | {error, Error}
encode(null, list) ->
    "null";
encode(undefined, list) ->
    "null";
encode(null, binary)  ->
    <<"null">>;
encode(undefined, binary)  ->
    <<"null">>;
encode(Val, list) when is_binary(Val) ->
    quote(binary_to_list(Val));
encode(Val, binary) when is_atom(Val) ->
    encode(atom_to_list(Val), binary);
encode(Val, binary) when is_list(Val) ->
    list_to_binary(quote(Val));
encode(Val, binary) when is_binary(Val) ->
    list_to_binary(quote(binary_to_list(Val)));
encode(Val, list) when is_list(Val) ->
    quote(Val);
encode(Val, list) when is_integer(Val) ->
    integer_to_list(Val);
encode(Val, binary) when is_integer(Val) ->
    list_to_binary(integer_to_list(Val));
encode(Val, list) when is_float(Val) ->
    [Res] = io_lib:format("~w", [Val]),
    Res;
encode(Val, binary) when is_float(Val) ->
    iolist_to_binary(io_lib:format("~w", [Val]));
encode({datetime, Val}, ReturnType) ->
    encode(Val, ReturnType);
encode({date, Val}, ReturnType) ->
    encode(Val, ReturnType);
encode({time, Val}, ReturnType) ->
    encode(Val, ReturnType);
encode({{Year, Month, Day}, {Hour, Minute, Second}}, list) ->
    Res = io_lib:format("'~4.4.0w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w'",
                        [Year, Month, Day, Hour, Minute, Second]),
    lists:flatten(Res);
encode({{_Year, _Month, _Day}, {_Hour, _Minute, _Second}}=Val, binary) ->
    list_to_binary(encode(Val, list));
encode({Time1, Time2, Time3}, list) ->
    Res = two_digits([Time1, Time2, Time3]),
    lists:flatten(Res);
encode({_Time1, _Time2, _Time3}=Val, binary) ->
    list_to_binary(encode(Val, list));
encode(Val, _) ->
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

%% @doc Quote a string or binary value so that it can be included safely in a
%% MySQL query. For the quoting, a binary is converted to a list and back.
%% For this, it's necessary to know the encoding of the binary.
%% @spec quote(x()) -> x()
%%       x() = list() | binary()
%% @end
%% hd/11,12
quote(String) when is_list(String) ->
    [39 | lists:reverse([39 | quote_loop(String)])]. %% 39 is $'

%% @doc  Make MySQL-safe backslash escapes before 10, 13, \, 26, 34, 39.
%% @spec quote_loop(list()) -> list()
%% @private
%% @end
%% hd/11,12
quote_loop(List) ->
    quote_loop(List, []).

quote_loop([], Acc) ->
    Acc;

quote_loop([0 | Rest], Acc) ->
    quote_loop(Rest, [$0, $\\ | Acc]);

quote_loop([10 | Rest], Acc) ->
    quote_loop(Rest, [$n, $\\ | Acc]);

quote_loop([13 | Rest], Acc) ->
    quote_loop(Rest, [$r, $\\ | Acc]);

quote_loop([$\\ | Rest], Acc) ->
    quote_loop(Rest, [$\\ , $\\ | Acc]);

quote_loop([39 | Rest], Acc) -> %% 39 is $'
    quote_loop(Rest, [39, $\\ | Acc]); %% 39 is $'

quote_loop([34 | Rest], Acc) -> %% 34 is $"
    quote_loop(Rest, [34, $\\ | Acc]); %% 34 is $"

quote_loop([26 | Rest], Acc) ->
    quote_loop(Rest, [$Z, $\\ | Acc]);

quote_loop([C | Rest], Acc) ->
    quote_loop(Rest, [C | Acc]).
