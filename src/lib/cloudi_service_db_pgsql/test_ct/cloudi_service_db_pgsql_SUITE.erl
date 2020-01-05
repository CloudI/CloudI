%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
-module(cloudi_service_db_pgsql_SUITE).

%% CT callbacks
-export([all/0,
         groups/0,
         suite/0,
         init_per_suite/1,
         end_per_suite/1,
         group/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% test cases
-export([t_table_create_1/1,
         t_table_create_2/1,
         t_table_query_1/1,
         t_table_query_2/1,
         t_table_query_3/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("cloudi_core/include/cloudi_logger.hrl").

-ifndef(CLOUDI_LONG_TEST_TIMEOUT).
-define(CLOUDI_LONG_TEST_TIMEOUT, 60). % minutes
-endif.
-define(DEFAULT_PGSQL_HOST, "127.0.0.1").
-define(DEFAULT_PGSQL_PORT, 5432).

% different driver service names
-define(DB_EPGSQL, "/db/pgsql/epgsql/cloudi_tests").
-define(DB_SEMIOCAST, "/db/pgsql/semiocast/cloudi_tests").

%%%------------------------------------------------------------------------
%%% Callback functions from CT
%%%------------------------------------------------------------------------

all() ->
    test_condition([{group, table_create},
                    {group, table_query}]).

groups() ->
    [{table_create, [],
      [t_table_create_1,
       t_table_create_2]},
     {table_query, [],
      [t_table_query_1,
       t_table_query_2,
       t_table_query_3]}].

suite() ->
    [{ct_hooks, [cth_surefire]},
     {timetrap, {minutes, ?CLOUDI_LONG_TEST_TIMEOUT}}].

init_per_suite(Config) ->
    Path = [_ | _] = os:getenv("TEST_DIR"),
    CloudIConfigPath = filename:join(Path,
                                     erlang:atom_to_list(?MODULE) ++ ".conf"),
    ok = cloudi_x_reltool_util:application_start(cloudi_core,
                                                 [{configuration,
                                                   CloudIConfigPath}],
                                                 infinity),
    Config.

end_per_suite(_Config) ->
    ok = cloudi_x_reltool_util:application_stop(cloudi_core),
    ok.

group(_GroupName) ->
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%%%------------------------------------------------------------------------
%%% test cases
%%%------------------------------------------------------------------------

t_table_create_1(_Config) ->
    Context0 = cloudi:new(),
    % (last SQL command result is returned with the common interface,
    %  not the whole list)
    {{ok, {updated, 0}},
     Context1} = cloudi_service_db_pgsql:squery(Context0,
        ?DB_EPGSQL,
        <<"DROP TABLE IF EXISTS incoming_results; "
          "CREATE TABLE incoming_results ("
          "digit_index   NUMERIC(30) PRIMARY KEY,"
          "data          TEXT"
          ");">>),
    {{ok, {updated, 0}},
     Context2} = cloudi_service_db_pgsql:squery(Context1,
        ?DB_SEMIOCAST,
        <<"DROP TABLE IF EXISTS incoming_results; "
          "CREATE TABLE incoming_results ("
          "digit_index   NUMERIC(30) PRIMARY KEY,"
          "data          TEXT"
          ");">>),
    {{ok, {updated, 0}},
     Context3} = cloudi_service_db_pgsql:squery(Context2,
        ?DB_SEMIOCAST,
        <<"DROP TABLE IF EXISTS incoming_results; "
          "CREATE TABLE incoming_results ("
          "digit_index   NUMERIC(30) PRIMARY KEY,"
          "data          TEXT"
          ");">>),
    {{ok, {updated, 0}},
     Context4} = cloudi_service_db_pgsql:squery(Context3,
        ?DB_EPGSQL,
        "DROP TABLE IF EXISTS incoming_results; "
        "CREATE TABLE incoming_results ("
        "digit_index   NUMERIC(30) PRIMARY KEY,"
        "data          TEXT"
        ");"),
    {{ok, {updated, 0}},
     Context5} = cloudi_service_db_pgsql:squery(Context4,
        ?DB_SEMIOCAST,
        "DROP TABLE IF EXISTS incoming_results; "
        "CREATE TABLE incoming_results ("
        "digit_index   NUMERIC(30) PRIMARY KEY,"
        "data          TEXT"
        ");"),
    {{ok, {updated, 0}},
     Context6} = cloudi_service_db_pgsql:squery(Context5,
        ?DB_SEMIOCAST,
        "DROP TABLE IF EXISTS incoming_results; "
        "CREATE TABLE incoming_results ("
        "digit_index   NUMERIC(30) PRIMARY KEY,"
        "data          TEXT"
        ");"),
    {{ok, ok},
     Context7} = cloudi_service_db_pgsql:transaction(Context6,
        ?DB_EPGSQL,
        [<<"DROP TABLE IF EXISTS incoming_results;">>,
         <<"CREATE TABLE incoming_results ("
           "digit_index   NUMERIC(30) PRIMARY KEY,"
           "data          TEXT"
           ");">>]),
    {{ok, ok},
     Context8} = cloudi_service_db_pgsql:transaction(Context7,
        ?DB_SEMIOCAST,
        [<<"DROP TABLE IF EXISTS incoming_results;">>,
         <<"CREATE TABLE incoming_results ("
           "digit_index   NUMERIC(30) PRIMARY KEY,"
           "data          TEXT"
           ");">>]),
    {{ok, ok},
     Context9} = cloudi_service_db_pgsql:transaction(Context8,
        ?DB_SEMIOCAST,
        [<<"DROP TABLE IF EXISTS incoming_results;">>,
         <<"CREATE TABLE incoming_results ("
           "digit_index   NUMERIC(30) PRIMARY KEY,"
           "data          TEXT"
           ");">>]),
    {{ok, ok},
     Context10} = cloudi_service_db_pgsql:transaction(Context9,
        ?DB_EPGSQL,
        [<<"INSERT INTO incoming_results (digit_index, data) "
           "VALUES (1, 'one');">>,
         <<"INSERT INTO incoming_results (digit_index, data) "
           "VALUES (2, 'two');">>]),
    {{ok, ok},
     Context11} = cloudi_service_db_pgsql:transaction(Context10,
        ?DB_SEMIOCAST,
        [<<"INSERT INTO incoming_results (digit_index, data) "
           "VALUES (3, 'three');">>,
         <<"INSERT INTO incoming_results (digit_index, data) "
           "VALUES (4, 'four');">>]),
    {{ok, ok},
     Context12} = cloudi_service_db_pgsql:transaction(Context11,
        ?DB_SEMIOCAST,
        [<<"INSERT INTO incoming_results (digit_index, data) "
           "VALUES (5, 'five');">>,
         <<"INSERT INTO incoming_results (digit_index, data) "
           "VALUES (6, 'six');">>]),
    {{ok, {selected, RowsEPGSQL}},
     Context13} = cloudi_service_db_pgsql:squery(Context12,
        ?DB_EPGSQL, <<"SELECT * FROM incoming_results">>),
    % epgsql driver returns row data as binaries
    [{<<"1">>,<<"one">>},
     {<<"2">>,<<"two">>},
     {<<"3">>,<<"three">>},
     {<<"4">>,<<"four">>},
     {<<"5">>,<<"five">>},
     {<<"6">>,<<"six">>}] = RowsEPGSQL,
    {{ok, {selected, RowsSEMIOCAST}},
     Context14} = cloudi_service_db_pgsql:squery(Context13,
        ?DB_SEMIOCAST, <<"SELECT * FROM incoming_results">>),
    % semiocast driver returns row data as erlang types
    [{1,<<"one">>},
     {2,<<"two">>},
     {3,<<"three">>},
     {4,<<"four">>},
     {5,<<"five">>},
     {6,<<"six">>}] = RowsSEMIOCAST,
    {{ok, {updated, 0}},
     _} = cloudi_service_db_pgsql:squery(Context14,
        ?DB_SEMIOCAST, "DROP TABLE incoming_results"),
    ok.

t_table_create_2(_Config) ->
    Context0 = cloudi:new(),
    {{ok, {error, Message1}},
     Context1} = cloudi_service_db_pgsql:transaction(Context0,
        ?DB_EPGSQL,
        [<<"CREATE TABLE incoming_results ("
           "digit_index   NUMERIC(30) PRIMARY KEY,"
           "data          TEXT"
           ");">>,
         <<"INSERT INTO incoming_results (digit_index, data) "
           "VALUES (1, 'one');">>,
         <<"INSERT INTO incoming_results (digit_index, data) "
           "VALUES (invalid, 'two');">>]),
    true = is_binary(Message1),
    {{ok, {error, Message1}},
     Context2} = cloudi_service_db_pgsql:transaction(Context1,
        ?DB_SEMIOCAST,
        [<<"CREATE TABLE incoming_results ("
           "digit_index   NUMERIC(30) PRIMARY KEY,"
           "data          TEXT"
           ");">>,
         <<"INSERT INTO incoming_results (digit_index, data) "
           "VALUES (1, 'one');">>,
         <<"INSERT INTO incoming_results (digit_index, data) "
           "VALUES (invalid, 'two');">>]),
    true = is_binary(Message1),
    {{ok, {error, Message1}},
     Context3} = cloudi_service_db_pgsql:transaction(Context2,
        ?DB_SEMIOCAST,
        [<<"CREATE TABLE incoming_results ("
           "digit_index   NUMERIC(30) PRIMARY KEY,"
           "data          TEXT"
           ");">>,
         <<"INSERT INTO incoming_results (digit_index, data) "
           "VALUES (1, 'one');">>,
         <<"INSERT INTO incoming_results (digit_index, data) "
           "VALUES (invalid, 'two');">>]),
    {{ok, {error, Message2}},
     _} = cloudi_service_db_pgsql:squery(Context3,
        ?DB_SEMIOCAST, <<"DROP TABLE incoming_results">>),
    true = is_binary(Message2),
    ok.

t_table_query_1(_Config) ->
    Context0 = cloudi:new(),
    {{ok, ok},
     Context1} = cloudi_service_db_pgsql:transaction(Context0,
        ?DB_SEMIOCAST,
        [<<"CREATE TABLE incoming_results ("
           "digit_index   NUMERIC(30) PRIMARY KEY,"
           "data          TEXT"
           ");">>,
         <<"INSERT INTO incoming_results (digit_index, data) "
           "VALUES (1, 'one');">>,
         <<"INSERT INTO incoming_results (digit_index, data) "
           "VALUES (2, 'two');">>,
         <<"INSERT INTO incoming_results (digit_index, data) "
           "VALUES (3, 'three');">>,
         <<"INSERT INTO incoming_results (digit_index, data) "
           "VALUES (4, 'four');">>]),
    {{ok, {selected, RowsEPGSQL}},
     Context2} = cloudi_service_db_pgsql:equery(Context1,
        ?DB_EPGSQL, <<"SELECT * FROM incoming_results "
                      "WHERE digit_index = $1">>, [2]),
    [{<<"2">>,<<"two">>}] = RowsEPGSQL,
    {{ok, {selected, RowsSEMIOCAST}},
     Context3} = cloudi_service_db_pgsql:equery(Context2,
        ?DB_SEMIOCAST, <<"SELECT * FROM incoming_results "
                         "WHERE digit_index = $1">>, [2]),
    [{2,<<"two">>}] = RowsSEMIOCAST,
    {{ok, {updated, 0}},
     _} = cloudi_service_db_pgsql:squery(Context3,
        ?DB_SEMIOCAST, <<"DROP TABLE incoming_results">>),
    ok.

t_table_query_2(_Config) ->
    Context0 = cloudi:new(),
    {{ok, {updated, 0}},
     Context1} = cloudi_service_db_pgsql:squery(Context0,
        ?DB_SEMIOCAST,
        <<"DROP TABLE IF EXISTS binary_results; "
          "CREATE TABLE binary_results ("
          "digit_index   NUMERIC(30) PRIMARY KEY,"
          "data          BYTEA"
          ");">>),
    Binary = <<16#DE, 16#AD, 16#BE, 16#EF>>,
    {{ok, {updated, 1}},
     Context2} = cloudi_service_db_pgsql:equery(Context1,
        ?DB_EPGSQL, <<"INSERT INTO binary_results (digit_index, data) "
                      "VALUES (1, $1)">>, [Binary]),
    {{ok, {updated, 1}},
     Context3} = cloudi_service_db_pgsql:equery(Context2,
        ?DB_SEMIOCAST, <<"INSERT INTO binary_results (digit_index, data) "
                  "VALUES (2, $1)">>, [Binary]),
    {{ok, {updated, 1}},
     Context4} = cloudi_service_db_pgsql:equery(Context3,
        ?DB_SEMIOCAST, <<"INSERT INTO binary_results (digit_index, data) "
                         "VALUES (3, $1)">>, [Binary]),
    {{ok, {updated, 1}},
     Context5} = cloudi_service_db_pgsql:squery(Context4,
        ?DB_EPGSQL, <<"INSERT INTO binary_results (digit_index, data) "
                      "VALUES (4, E'\\\\xDEADBEEF')">>),
    {{ok, {updated, 1}},
     Context6} = cloudi_service_db_pgsql:squery(Context5,
        ?DB_SEMIOCAST, <<"INSERT INTO binary_results (digit_index, data) "
                  "VALUES (5, E'\\\\xDEADBEEF')">>),
    {{ok, {updated, 1}},
     Context7} = cloudi_service_db_pgsql:squery(Context6,
        ?DB_SEMIOCAST, <<"INSERT INTO binary_results (digit_index, data) "
                         "VALUES (6, E'\\\\xDEADBEEF')">>),
    {{ok, {selected, RowsEPGSQL}},
     Context8} = cloudi_service_db_pgsql:squery(Context7,
        ?DB_EPGSQL, <<"SELECT * FROM binary_results">>),
    BinaryEPGSQL = <<"\\xdeadbeef">>, % epgsql doesn't really support BYTEA
    [{<<"1">>,BinaryEPGSQL},
     {<<"2">>,BinaryEPGSQL},
     {<<"3">>,BinaryEPGSQL},
     {<<"4">>,BinaryEPGSQL},
     {<<"5">>,BinaryEPGSQL},
     {<<"6">>,BinaryEPGSQL}] = RowsEPGSQL,
    {{ok, {selected, RowsSEMIOCAST}},
     Context9} = cloudi_service_db_pgsql:squery(Context8,
        ?DB_SEMIOCAST, <<"SELECT * FROM binary_results">>),
    [{1,Binary},
     {2,Binary},
     {3,Binary},
     {4,Binary},
     {5,Binary},
     {6,Binary}] = RowsSEMIOCAST,
    {{ok, {updated, 0}},
     _} = cloudi_service_db_pgsql:squery(Context9,
        ?DB_SEMIOCAST, <<"DROP TABLE binary_results">>),
    ok.

t_table_query_3(_Config) ->
    Context0 = cloudi:new(),
    {{ok, {updated, 0}},
     Context1} = cloudi_service_db_pgsql:squery(Context0,
        ?DB_SEMIOCAST,
        <<"DROP TABLE IF EXISTS incoming_results; "
          "CREATE TABLE incoming_results ("
          "digit_index   NUMERIC(30) PRIMARY KEY,"
          "data          TEXT NULL"
          ");">>),
    {{ok, {updated, 1}},
     Context2} = cloudi_service_db_pgsql:equery(Context1,
        ?DB_EPGSQL, <<"INSERT INTO incoming_results (digit_index, data) "
                      "VALUES (1, $1)">>, [null]),
    {{ok, {updated, 1}},
     Context3} = cloudi_service_db_pgsql:equery(Context2,
        ?DB_SEMIOCAST, <<"INSERT INTO incoming_results (digit_index, data) "
                  "VALUES (2, $1)">>, [null]),
    {{ok, {updated, 1}},
     Context4} = cloudi_service_db_pgsql:equery(Context3,
        ?DB_SEMIOCAST, <<"INSERT INTO incoming_results (digit_index, data) "
                         "VALUES (3, $1)">>, [null]),
    {{ok, {selected, RowsEPGSQL}},
     Context5} = cloudi_service_db_pgsql:squery(Context4,
        ?DB_EPGSQL, <<"SELECT * FROM incoming_results">>),
    [{<<"1">>,null},
     {<<"2">>,null},
     {<<"3">>,null}] = RowsEPGSQL,
    {{ok, {selected, RowsSEMIOCAST}},
     Context6} = cloudi_service_db_pgsql:squery(Context5,
        ?DB_SEMIOCAST, <<"SELECT * FROM incoming_results">>),
    [{1,null},
     {2,null},
     {3,null}] = RowsSEMIOCAST,
    {{ok, {updated, 0}},
     _} = cloudi_service_db_pgsql:squery(Context6,
        ?DB_SEMIOCAST, <<"DROP TABLE incoming_results">>),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

test_condition(L) ->
    if
        ?CLOUDI_LONG_TEST_TIMEOUT > 0 ->
            case gen_tcp:connect(?DEFAULT_PGSQL_HOST,
                                 ?DEFAULT_PGSQL_PORT, []) of
                {ok, Socket} ->
                    catch gen_tcp:close(Socket),
                    L;
                {error, econnrefused} ->
                    error_logger:error_msg("unable to test ~p",
                                           [{?DEFAULT_PGSQL_HOST,
                                             ?DEFAULT_PGSQL_PORT}]),
                    {skip, pgsql_dead};
                {error, Reason} ->
                    error_logger:error_msg("unable to test ~p: ~p",
                                           [{?DEFAULT_PGSQL_HOST,
                                             ?DEFAULT_PGSQL_PORT}, Reason]),
                    {skip, pgsql_dead}
            end;
        ?CLOUDI_LONG_TEST_TIMEOUT =:= 0 ->
            {skip, long_tests_disabled}
    end.

