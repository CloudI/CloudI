#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../emysql ./ebin ./t -boot start_sasl -sasl sasl_error_logger false

-include_lib("emysql/include/emysql.hrl").
-include_lib("emysql/t/mysql_test.hrl").

-record(bee, {name, type, size}).

main(_) ->
    etap:plan(unknown),
    error_logger:tty(false),
    application:start(crypto),
    application:start(emysql),
    emysql:add_pool(master, 4, "test", "test", "localhost", 3306, "testdatabase", 'utf8'),
    ?DROP_TABLES(master),

    CreateTable = emysql:execute(master, "CREATE TABLE hive (name VARCHAR(32), type INT(11), size INT(3), PRIMARY KEY (name))"),
    etap:is(is_record(CreateTable, ok_packet), true, "create table returned ok packet"),

    {ok, HoneyBeePid} = emysql_worker:start(honeybee),
    etap:ok(is_process_alive(HoneyBeePid), "HoneyBeePid is alive"),

    A = honeybee:create(#bee{ name = "A11", type = 1, size = 3}),
    etap:is(is_record(A, ok_packet), true, "Bee created"),

    B = honeybee:get_by_bid("A11"),
    etap:is(B, [{bee,<<"A11">>,1,3}], "Bee retrieved"),

    C = honeybee:get_all(),
    etap:is(C, [{bee,<<"A11">>,1,3}], "Bees retrieved"),

    D = honeybee:update_type("A11", 2),
    etap:is(is_record(D, ok_packet), true, "Bee updated"),

    E = honeybee:get_by_type(2),
    etap:is(E:zip(), [[{<<"name">>,<<"A11">>},{<<"type">>,2},{<<"size">>,3}]], "Bee retrieved"),
    etap:is(E:rows(), [[<<"A11">>,2,3]], "Bee retrieved"),

    etap:end_tests().
