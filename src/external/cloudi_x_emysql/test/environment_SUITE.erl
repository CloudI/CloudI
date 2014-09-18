%%%-------------------------------------------------------------------
%%% File     : Emysql/test/environment_SUITE.erl
%%% Descr    : Suite #1 - testing the test setup, db and pathes =
%%%            availability of crypto app, emysql app and test db. 
%%% Author   : H. Diedrich
%%% Created  : 12/13/2011 hd
%%% Requires : Erlang 14B (prior may not have ct_run)
%%%-------------------------------------------------------------------
%%%
%%% THIS SUITE DOES NO ACTUAL TESTS BUT CHECKS THE TEST DATABASE ETC.
%%% Test Cases are in this high granularity for clear failure reports.
%%%
%%% Run from Emysql/: 
%%%     make test
%%%
%%% Results see:
%%%     test/index.html
%%%
%%%-------------------------------------------------------------------

-define(POOL, environment_test_pool).

-module(environment_SUITE).
-include_lib("common_test/include/ct.hrl").

-include("../include/emysql.hrl").

-export([
        all/0,
        init_per_testcase/2,
        end_per_testcase/2,

        initializing_crypto_app/1,

        initializing_emysql_app/1,
        accessing_emysql_module/1,

        connecting_to_db_and_creating_a_pool_transition/1,
        insert_a_record/1,
        select_a_record/1,

        add_pool_utf8/1,
        add_pool_latin1/1,
        add_pool_latin1_compatible/1,
        add_pool_time_zone/1,
        add_pool_wrong_db/1,
        add_pool_wrong_cmd/1,

        add_pool_env_defaults/1,
        add_pool_env_all/1

    ]).

% List of test cases.
%%--------------------------------------------------------------------
all() -> 
    [
        initializing_crypto_app,
        initializing_emysql_app,
        accessing_emysql_module,
        connecting_to_db_and_creating_a_pool_transition,
        insert_a_record,
        select_a_record,

        add_pool_utf8,
        add_pool_latin1,
        add_pool_latin1_compatible,
        add_pool_time_zone,
        add_pool_wrong_db,
        add_pool_wrong_cmd,

        add_pool_env_defaults,
        add_pool_env_all
    ].

init_per_testcase(add_pool_env_defaults, Config) ->
    ok = application:stop(emysql),
    ok = application:set_env(emysql, pools, [{?POOL, [
                    {user, test_helper:test_u()},
                    {password, test_helper:test_p()},
                    {host, "localhost"},
                    {port, 3306}
                ]}]
    ),
    ok = application:start(emysql),
    Config;

init_per_testcase(add_pool_env_all, Config) ->
    ok = application:stop(emysql),
    ok = application:set_env(emysql, pools, [{?POOL, [
                    {size, 10},
                    {user, test_helper:test_u()},
                    {password, test_helper:test_p()},
                    {host, "localhost"},
                    {port, 3306},
                    {database, "hello_database"},
                    {encoding, utf8},
                    {start_cmds, [<<"SET TIME_ZONE='+00:00'">>]}
                ]}]
    ),
    ok = application:start(emysql),
    Config;

init_per_testcase(T, Config) when
        T == connecting_to_db_and_creating_a_pool_transition orelse
        T == insert_a_record orelse
        T == select_a_record ->
    emysql:add_pool(?POOL, 10, test_helper:test_u(),
        test_helper:test_p(), "localhost", 3306, "hello_database", utf8),
    Config;

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(T, _) when
        T == connecting_to_db_and_creating_a_pool_transition orelse
        T == insert_a_record orelse
        T == select_a_record orelse
        T == add_pool_utf8 orelse
        T == add_pool_latin1 orelse
        T == add_pool_latin1_compatible orelse
        T == add_pool_time_zone ->
	emysql:remove_pool(?POOL);

end_per_testcase(T, _) when
        T == add_pool_env_defaults orelse
        T == add_pool_env_all ->
    application:unset_env(emysql, pools),
    emysql:remove_pool(?POOL);

end_per_testcase(_, _) ->
    ok.

% Test Case: Test if the crypt app is available. This detects a path error.
%%--------------------------------------------------------------------
initializing_crypto_app(_) ->
    crypto:start(),
    ok.

% Test Case: Test if the emysql app is available. This detects a path error.
%%--------------------------------------------------------------------
initializing_emysql_app(_) ->
    application:start(emysql),
    ok.

% Test Case: Test if the emysql module is available. This detects a path error.
%%--------------------------------------------------------------------
accessing_emysql_module(_) ->
    emysql:modules(),
    ok.
%% Test case: test obsolete transitional API
%%--------------------------------------------------------------------
connecting_to_db_and_creating_a_pool_transition(_) ->
    #result_packet{rows=[[<<"hello_database">>]]} =
    emysql:execute(?POOL, <<"SELECT DATABASE();">>),
    #result_packet{rows=[[<<"utf8">>]]} =
    emysql:execute(?POOL, <<"SELECT @@character_set_connection;">>).

% Test Case: Test if we can insert a record.
%%--------------------------------------------------------------------
insert_a_record(_) ->
    #ok_packet{} = emysql:execute(?POOL, <<"DELETE FROM hello_table">>),
    #ok_packet{} = emysql:execute(?POOL,
        <<"INSERT INTO hello_table SET hello_text = 'Hello World!'">>).

% Test Case: Test if we can select records.
%%--------------------------------------------------------------------
select_a_record(_) ->
    #result_packet{rows=[[<<"Hello World!">>]]} =
    emysql:execute(?POOL, <<"select hello_text from hello_table">>).

add_pool_utf8(_) ->
    emysql:add_pool(?POOL, 10, test_helper:test_u(), test_helper:test_p(),
        "localhost", 3306, undefined, utf8),
    #result_packet{rows=[[<<"utf8">>]]} =
    emysql:execute(?POOL, <<"SELECT @@character_set_connection;">>).

add_pool_latin1(_) ->
    emysql:add_pool(?POOL, 10, test_helper:test_u(), test_helper:test_p(),
        "localhost", 3306, undefined, latin1),
    #result_packet{rows=[[<<"latin1">>]]} =
    emysql:execute(?POOL, <<"SELECT @@character_set_connection;">>).

add_pool_latin1_compatible(_) ->
    emysql:add_pool(?POOL, 10, test_helper:test_u(), test_helper:test_p(),
        "localhost", 3306, undefined, undefined),
    #result_packet{rows=[[<<"latin1">>]]} =
    emysql:execute(?POOL, <<"SELECT @@character_set_connection;">>).

add_pool_time_zone(_) ->
    emysql:add_pool(?POOL, 10, test_helper:test_u(), test_helper:test_p(),
        "localhost", 3306, undefined, utf8, [<<"SET time_zone='+00:00'">>]),
    #result_packet{rows=[[<<"+00:00">>]]} =
    emysql:execute(?POOL, <<"SELECT @@time_zone;">>).

add_pool_env_defaults(_) ->
    #result_packet{rows=[[undefined]]} =
    emysql:execute(?POOL, <<"SELECT DATABASE();">>),
    #result_packet{rows=[[<<"latin1">>]]} =
    emysql:execute(?POOL, <<"SELECT @@character_set_connection;">>),
    #result_packet{rows=[[<<"SYSTEM">>]]} =
    emysql:execute(?POOL, <<"SELECT @@time_zone;">>).

add_pool_env_all(_) ->
    #result_packet{rows=[[<<"hello_database">>]]} =
    emysql:execute(?POOL, <<"SELECT DATABASE();">>),
    #result_packet{rows=[[<<"utf8">>]]} =
    emysql:execute(?POOL, <<"SELECT @@character_set_connection;">>),
    #result_packet{rows=[[<<"+00:00">>]]} =
    emysql:execute(?POOL, <<"SELECT @@time_zone;">>).

add_pool_wrong_db(_) ->
    {Pid, Mref} = spawn_monitor(fun() ->
                emysql:add_pool(?POOL, 10, test_helper:test_u(),
                    test_helper:test_p(), "localhost", 3306,
                    "this-database-does-not-exist", utf8
                )
        end
    ),
    receive
        {'DOWN', Mref, process, Pid,
            {{nocatch, {failed_to_set_database, _}}, _}} ->
            ok
    after 100 ->
            exit(should_have_failed)
    end,
    % Verify there are no connections added for real
    [] = emysql_conn_mgr:pools().

add_pool_wrong_cmd(_) ->
    {Pid, Mref} = spawn_monitor(fun() ->
                emysql:add_pool(?POOL, 10, test_helper:test_u(),
                    test_helper:test_p(), "localhost", 3306, undefined, utf8,
                    [<<"syntax error">>])
        end
    ),
    receive
        {'DOWN', Mref, process, Pid, {{nocatch, {failed_to_run_cmd, _}}, _}} ->
            ok
    after 100 ->
            exit(should_have_failed)
    end,
    % Verify there are no connections added for real
    [] = emysql_conn_mgr:pools().
