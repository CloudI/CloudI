-module(erlcql_test).

-compile(export_all).

-define(CQL_VERSION, <<"3.0.0">>).
-define(OPTS, [{cql_version, ?CQL_VERSION}]).
-define(CONSISTENCY, quorum).

-spec create_keyspace() -> Keyspace :: bitstring().
create_keyspace() ->
    _ = random:seed(now()),
    N = integer_to_list(random:uniform(1000000000)),
    Keyspace = [<<"erlcql_tests_">>, N],
    Query = [<<"CREATE KEYSPACE IF NOT EXISTS ">>, Keyspace,
             <<" WITH replication = {",
               "'class': 'SimpleStrategy', ",
               "'replication_factor': 1}">>],
    single_query(Query),
    iolist_to_binary(Keyspace).

-spec keyspace_exists(bitstring()) -> boolean().
keyspace_exists(Keyspace) ->
    Query = <<"SELECT keyspace_name FROM system.schema_keyspaces">>,
    {Keyspaces, _} = single_query(Query),
    lists:member([Keyspace], Keyspaces).

-spec drop_keyspace(bitstring()) -> ok.
drop_keyspace(Keyspace) ->
    Query = [<<"DROP KEYSPACE IF EXISTS ">>, Keyspace],
    single_query(Query),
    ok.

-spec gen_table_name() -> Table :: bitstring().
gen_table_name() ->
    _ = random:seed(now()),
    N = integer_to_list(random:uniform(1000000000)),
    iolist_to_binary([<<"table_">>, N]).

-spec start_client(bitstring() | proplists:proplist()) -> Pid :: pid().
start_client(Opts) when is_list(Opts) ->
    {ok, Pid} = erlcql_client:start_link(Opts ++ ?OPTS),
    Pid;
start_client(Keyspace) ->
    Opts = [{use, Keyspace} | ?OPTS],
    {ok, Pid} = erlcql_client:start_link(Opts),
    Pid.

-spec single_query(iodata()) -> {ok, any()} | {error, any()}.
single_query(Query) ->
    {ok, Pid} = erlcql_client:start_link(?OPTS),
    {ok, Response} = erlcql_client:'query'(Pid, Query, ?CONSISTENCY),
    stop_client(Pid),
    Response.

-spec 'query'(pid(), iodata()) -> any().
'query'(Pid, Query) ->
    {ok, Response} = erlcql_client:'query'(Pid, Query, ?CONSISTENCY),
    Response.

-spec execute(pid(), atom(), [any()]) -> any().
execute(Pid, Name, Values) ->
    {ok, Response} = erlcql_client:execute(Pid, Name, Values, ?CONSISTENCY),
    Response.

-spec stop_client(pid()) -> ok.
stop_client(Pid) ->
    true = unlink(Pid),
    true = exit(Pid, kill),
    ok.

%% bstr:hexencode/1 -----------------------------------------------------------

hexencode(Str) ->
    hexencode(Str, <<>>).

hexencode(<<Hi:4, Lo:4, Tail/binary>>, Acc) ->
    HiChar = integer_to_hex_char(Hi, lower),
    LoChar = integer_to_hex_char(Lo, lower),
    hexencode(Tail, <<Acc/binary, HiChar, LoChar>>);
hexencode(<<>>, Acc) ->
    Acc.

integer_to_hex_char(N) when N >= 0 ->
    if
        N =< 9 ->
            $0 + N;
        N =< 15 ->
            $A - 10 + N;
        true ->
            erlang:error(badarg)
    end.

integer_to_hex_char(N, lower) when N >= 0 ->
    if
        N =< 9 ->
            $0 + N;
        N =< 15 ->
            $a - 10 + N;
        true ->
            erlang:error(badarg)
    end;
integer_to_hex_char(N, upper) ->
    integer_to_hex_char(N).

%% inet:ntoa/1 ----------------------------------------------------------------

ntoa({A,B,C,D}) ->
    integer_to_list(A) ++ "." ++ integer_to_list(B) ++ "." ++
        integer_to_list(C) ++ "." ++ integer_to_list(D);
ntoa({0,0,0,0,0,0,0,0}) -> "::";
ntoa({0,0,0,0,0,0,0,1}) -> "::1";
ntoa({0,0,0,0,0,0,A,B}) -> "::" ++ dig_to_dec(A) ++ "." ++ dig_to_dec(B);
ntoa({0,0,0,0,0,16#ffff,A,B}) ->
    "::FFFF:" ++ dig_to_dec(A) ++ "." ++ dig_to_dec(B);
ntoa({_,_,_,_,_,_,_,_}=T) ->
    ntoa(tuple_to_list(T), []);
ntoa(_) ->
    {error, einval}.

ntoa([], R) ->
    ntoa_done(R);
ntoa([0,0|T], R) ->
    ntoa(T, R, 2);
ntoa([D|T], R) ->
    ntoa(T, [D|R]).

ntoa([], R, _) ->
    ntoa_done(R, []);
ntoa([0|T], R, N) ->
    ntoa(T, R, N+1);
ntoa([D|T], R, N) ->
    ntoa(T, R, N, [D]).

ntoa([], R1, _N1, R2) ->
    ntoa_done(R1, R2);
ntoa([0,0|T], R1, N1, R2) ->
    ntoa(T, R1, N1, R2, 2);
ntoa([D|T], R1, N1, R2) ->
    ntoa(T, R1, N1, [D|R2]).

ntoa(T, R1, N1, R2, N2) when N2 > N1 ->
    ntoa(T, R2++dup(N1, 0, R1), N2);
ntoa([], R1, _N1, R2, N2) ->
    ntoa_done(R1, dup(N2, 0, R2));
ntoa([0|T], R1, N1, R2, N2) ->
    ntoa(T, R1, N1, R2, N2+1);
ntoa([D|T], R1, N1, R2, N2) ->
    ntoa(T, R1, N1, [D|dup(N2, 0, R2)]).

ntoa_done(R1, R2) ->
    lists:append(
      separate(":", lists:map(fun dig_to_hex/1, lists:reverse(R1)))++
      ["::"|separate(":", lists:map(fun dig_to_hex/1, lists:reverse(R2)))]).

ntoa_done(R) ->
    lists:append(separate(":", lists:map(fun dig_to_hex/1, lists:reverse(R)))).

separate(_E, []) ->
    [];
separate(E, [_|_]=L) ->
    separate(E, L, []).

separate(E, [H|[_|_]=T], R) ->
    separate(E, T, [E,H|R]);
separate(_E, [H], R) ->
    lists:reverse(R, [H]).

dup(0, _, L) ->
    L;
dup(N, E, L) when is_integer(N), N >= 1 ->
    dup(N-1, E, [E|L]).

dig_to_dec(0) -> "0.0";
dig_to_dec(X) ->
    integer_to_list((X bsr 8) band 16#ff) ++ "." ++
        integer_to_list(X band 16#ff).

dig_to_hex(X) ->
    erlang:integer_to_list(X, 16).
