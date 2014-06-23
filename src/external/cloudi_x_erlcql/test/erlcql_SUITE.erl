-module(erlcql_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include_lib("proper/include/proper.hrl").

-define(OPTS, [{cql_version, <<"3.0.0">>}]).
-define(KEYSPACE, <<"erlcql_tests">>).

-define(CREATE_KEYSPACE, <<"CREATE KEYSPACE IF NOT EXISTS erlcql_tests ",
                           "WITH replication = {'class': 'SimpleStrategy', ",
                           "'replication_factor': 1}">>).
-define(DROP_KEYSPACE, <<"DROP KEYSPACE IF EXISTS erlcql_tests">>).
-define(USE_KEYSPACE, <<"USE erlcql_tests">>).
-define(CREATE_TABLE, <<"CREATE TABLE IF NOT EXISTS t ",
                        "(k int PRIMARY KEY, v text)">>).
-define(DROP_TABLE, <<"DROP TABLE IF EXISTS t">>).

-define(PROPTEST(A), true = proper:quickcheck(A())).
-define(PROPTEST(A, Args), true = proper:quickcheck(A(Args), {numtests, 1000})).

-import(erlcql, [q/2, q/3]).

%% Fixtures -------------------------------------------------------------------

init_per_suite(Config) ->
    {ok, Pid} = erlcql:start_link("localhost", ?OPTS),
    unlink(Pid),
    q(Pid, ?DROP_KEYSPACE),
    [{pid, Pid} | Config].

end_per_suite(Config) ->
    Pid = get_pid(Config),
    q(Pid, ?DROP_KEYSPACE),
    exit(Pid, kill).

init_per_group(types, Config) ->
    Pid = get_pid(Config),
    q(Pid, ?CREATE_KEYSPACE),
    Config;
init_per_group(data_manipulation, Config) ->
    Pid = get_pid(Config),
    q(Pid, ?CREATE_KEYSPACE),
    q(Pid, ?USE_KEYSPACE),
    q(Pid, ?CREATE_TABLE),
    Config;
init_per_group(client, Config) ->
    Pid = get_pid(Config),
    q(Pid, ?CREATE_KEYSPACE),
    Config;
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(types, Config) ->
    Pid = get_pid(Config),
    q(Pid, ?DROP_KEYSPACE);
end_per_group(data_manipulation, Config) ->
    Pid = get_pid(Config),
    q(Pid, ?DROP_KEYSPACE);
end_per_group(client, Config) ->
    Pid = get_pid(Config),
    q(Pid, ?DROP_KEYSPACE);
end_per_group(_GroupName, _Config) ->
    ok.

init_per_testcase(create_keyspace, Config) ->
    Pid = get_pid(Config),
    q(Pid, ?DROP_KEYSPACE),
    Config;
init_per_testcase(drop_keyspace, Config) ->
    Pid = get_pid(Config),
    q(Pid, ?CREATE_KEYSPACE),
    Config;
init_per_testcase(TestCase, Config) ->
    clean_type_table(TestCase, Config),
    Config.

end_per_testcase(create_keyspace, Config) ->
    Pid = get_pid(Config),
    q(Pid, ?DROP_KEYSPACE);
end_per_testcase(TestCase, Config) ->
    clean_type_table(TestCase, Config),
    ok.

groups() ->
    [{keyspaces, [],
      [create_keyspace,
       drop_keyspace]},
     {types, [],
      [{collections, [],
        [list_of_ints,
         list_of_varints,
         set_of_floats,
         map_of_strings_to_boolean]}]},
     {data_manipulation, [],
      [insert]},
     {client, [],
      [start_without_use,
       start_with_use]}].

all() ->
    [{group, keyspaces},
     {group, types},
     {group, data_manipulation},
     {group, client}].

%% Tests ----------------------------------------------------------------------

create_keyspace(Config) ->
    Pid = get_pid(Config),
    {ok, created} = q(Pid, ?CREATE_KEYSPACE),
    Keyspaces = get_keyspaces(Pid),
    true == lists:member([?KEYSPACE], Keyspaces).

drop_keyspace(Config) ->
    Pid = get_pid(Config),
    {ok, dropped} = q(Pid, ?DROP_KEYSPACE),
    Keyspaces = get_keyspaces(Pid),
    false == lists:member([?KEYSPACE], Keyspaces).

%% Type tests

list_of_ints(Config) ->
    Pid = get_pid(Config),
    create_type_table(Pid, <<"list<int>">>),
    ?PROPTEST(prop_list_of_ints, Pid).

prop_list_of_ints(Pid) ->
    ?FORALL(L, list(integer()),
            begin
                SL = [integer_to_list(X) || X <- L],
                V = iolist_to_binary([$[, string:join(SL, ", "), $]]),
                insert_type(Pid, V),
                L == get_value(Pid, <<"list<int>">>)
            end).

list_of_varints(Config) ->
    Pid = get_pid(Config),
    create_type_table(Pid, <<"list<varint>">>),
    ?PROPTEST(prop_list_of_varints, Pid).

prop_list_of_varints(Pid) ->
    ?FORALL(L, list(integer()),
            begin
                SL = [integer_to_list(X) || X <- L],
                V = iolist_to_binary([$[, string:join(SL, ", "), $]]),
                insert_type(Pid, V),
                L == get_value(Pid, <<"list<varint>">>)
            end).

set_of_floats(Config) ->
    Pid = get_pid(Config),
    check_type(Pid, <<"set<double>">>, <<"{0.1, 1.2, 2.3}">>, [0.1, 1.2, 2.3]).

map_of_strings_to_boolean(Config) ->
    Pid = get_pid(Config),
    create_type_table(Pid, <<"map<varchar, boolean>">>),
    ?PROPTEST(prop_map_of_strings_to_boolean, Pid).

b2l(false) ->
    "false";
b2l(true) ->
    "true".

prop_map_of_strings_to_boolean(Pid) ->
    ?FORALL(L, list({erlcql_type_SUITE:varchar_string(), boolean()}),
            begin
                L2 = [{erlcql_type_SUITE:utf8_to_binary(U), B} || {U, B} <- L],
                ML = [[$', erlcql_type_SUITE:escape_string(U), "': ", b2l(B)]
                      || {U, B} <- L],
                V = iolist_to_binary([${, string:join(ML, ", "), $}]),
                insert_type(Pid, V),
                Z = get_value(Pid, <<"map<varchar, boolean>">>),
                [] == Z -- L2
            end).

insert(Config) ->
    Pid = get_pid(Config),
    {ok, void} = q(Pid, <<"INSERT INTO t (k, v) VALUES (1, 'one')">>),
    Rows = q(Pid, <<"SELECT * FROM t">>, one),
    {ok, {[[1, <<"one">>]], [{<<"k">>, int}, {<<"v">>, varchar}]}} = Rows.

%% Client tests

start_without_use(_Config) ->
    {ok, Pid} = erlcql:start_link("localhost", ?OPTS),
    unlink(Pid),
    Msg = <<"no keyspace has been specified">>,
    {error, {invalid, Msg, _}} = q(Pid, ?CREATE_TABLE),
    exit(Pid, kill).

start_with_use(_Config) ->
    Opts = [{use, ?KEYSPACE} | ?OPTS],
    {ok, Pid} = erlcql:start_link("localhost", Opts),
    unlink(Pid),
    {ok, _} = q(Pid, ?CREATE_TABLE),
    exit(Pid, kill).

%% Helpers --------------------------------------------------------------------

get_pid(Config) ->
    proplists:get_value(pid, Config).

get_keyspaces(Pid) ->
    Check = <<"SELECT keyspace_name FROM system.schema_keyspaces">>,
    {ok, {Keyspaces, _}} = q(Pid, Check, one),
    Keyspaces.

check_type(Pid, Type, Value, Expected) ->
    create_type_table(Pid, Type),
    insert_type(Pid, Value),
    Expected = get_value(Pid, Type).

create_type_table(Pid, Type) when is_atom(Type) ->
    TypeBin = atom_to_binary(Type, utf8),
    create_type_table(Pid, TypeBin);
create_type_table(Pid, Type) ->
    {ok, created} = q(Pid, [<<"CREATE TABLE IF NOT EXISTS erlcql_tests.t ",
                              "(k varchar PRIMARY KEY, v ">>, Type, <<")">>]).

insert_type(Pid, Value) ->
    {ok, void} = q(Pid, [<<"INSERT INTO erlcql_tests.t ",
                           "(k, v) VALUES ('key', ">>, Value, <<")">>]).

get_value(Pid, <<"list<int>">>) ->
    get_value(Pid, {list, int});
get_value(Pid, <<"list<varint>">>) ->
    get_value(Pid, {list, varint});
get_value(Pid, <<"set<double>">>) ->
    get_value(Pid, {set, double});
get_value(Pid, <<"map<varchar, boolean>">>) ->
    get_value(Pid, {map, varchar, boolean});
get_value(Pid, Type) ->
    Res = q(Pid, <<"SELECT v FROM erlcql_tests.t">>, one),
    {ok, {[[Value]], [{<<"v">>, Type}]}} = Res,
    Value.

clean_type_table(TestCase, Config) ->
    {_, [], Tests} = lists:keyfind(types, 1, groups()),
    {_, [], Collections} = lists:keyfind(collections, 1, Tests),
    Types = Collections,
    case lists:member(TestCase, Types) of
        true ->
            Pid = get_pid(Config),
            q(Pid, <<"DROP TABLE IF EXISTS erlcql_tests.t">>);
        false ->
            ok
    end.
