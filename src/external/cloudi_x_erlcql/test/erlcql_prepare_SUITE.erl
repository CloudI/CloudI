-module(erlcql_prepare_SUITE).

-compile(export_all).

-include_lib("common_test/include/ct.hrl").
-include("erlcql_test.hrl").

%% Suite ----------------------------------------------------------------------

all() ->
    [prepare_execute].

%% Fixtures -------------------------------------------------------------------

init_per_suite(Config) ->
    Keyspace = create_keyspace(),
    [{keyspace, Keyspace} | Config].

end_per_suite(Config) ->
    Keyspace = ?c(keyspace, Config),
    ok = drop_keyspace(Keyspace).

%% Tests ----------------------------------------------------------------------

prepare_execute(Config) ->
    Keyspace = ?c(keyspace, Config),
    Table = gen_table_name(),
    Create = [<<"CREATE TABLE ">>, Keyspace, <<".">>, Table,
              <<" (id int PRIMARY KEY,"
                " ascii ascii,"
                " bigint bigint,"
                " blob blob,"
                " boolean boolean,"
                " inet inet,"
                " timestamp timestamp,"
                " timeuuid timeuuid,"
                " uuid uuid,"
                " varchar varchar,"
                " varint varint,"
                " list list<varchar>,"
                " set_ set<varint>,"
                " map map<uuid, boolean>)">>],
    single_query(Create),

    Insert = [<<"INSERT INTO ">>, Table,
              <<" (id, ascii, bigint, blob, boolean, inet, timestamp,"
                " timeuuid, uuid, varchar, varint, list, set_, map) VALUES"
                " (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)">>],
    Select = [<<"SELECT id, ascii, bigint, blob, boolean, inet, timestamp,"
                " timeuuid, uuid, varchar, varint, list, set_, map"
                " FROM ">>, Table, <<" WHERE id = ?">>],
    Prepare = [{insert, Insert},
               {select, Select}],
    Opts = [{use, Keyspace},
            {prepare, Prepare}],
    Client = start_client(Opts),

    List = [<<"a">>, <<"b">>, <<"c">>, <<"d">>, <<"e">>, <<"f">>],
    Set = [1, 6, 3, 5, 4, 2],
    Map = [{<<"bab12c79-35f7-4638-94bb-ec93408a0d4c">>, true},
           {<<"3ec62590-d910-412d-a4f2-30ff2ac7dc44">>, false}],
    Values = [1, <<"ascii">>, 123456, <<"0xA1B2C3D4E5F6">>,
              true, {192, 168, 12, 1}, 1392207804464,
              <<"85445cf8-93e0-11e3-bca1-425861b86ab6">>,
              <<"92cf200b-672a-4c37-884d-b17206dcb096">>, <<"varchar">>,
              123456, List, lists:usort(Set), lists:keysort(1, Map)],
    execute(Client, insert, Values),

    {[Row], _} = execute(Client, select, [hd(Values)]),
    Row = Values.
