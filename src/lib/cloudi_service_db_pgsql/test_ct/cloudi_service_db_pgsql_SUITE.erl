%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
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

-define(DEFAULT_PGSQL_HOST, "127.0.0.1").
-define(DEFAULT_PGSQL_PORT, 5432).

-define(DB_WG, "/db/pgsql/wg/cloudi_tests"). % service name
-define(DB_SEMIOCAST, "/db/pgsql/semiocast/cloudi_tests"). % service name

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
     {timetrap, 5100}].

init_per_suite(Config) ->
    Path = [_ | _] = os:getenv("TEST_DIR"),
    CloudIConfigPath = filename:join(Path,
                                     erlang:atom_to_list(?MODULE) ++ ".conf"),
    ok = cloudi_x_reltool_util:application_start(cloudi_core,
                                                 [{configuration,
                                                   CloudIConfigPath}],
                                                 1000),
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
    Context = cloudi:new(),
    % (last SQL command result is returned with the common interface,
    %  not the whole list)
    {ok, {updated, 0}} = cloudi_service_db_pgsql:squery(Context,
        ?DB_WG,
        <<"DROP TABLE IF EXISTS incoming_results; "
          "CREATE TABLE incoming_results ("
          "digit_index   NUMERIC(30) PRIMARY KEY,"
          "data          TEXT"
          ");">>),
    {ok, {updated, 0}} = cloudi_service_db_pgsql:squery(Context,
        ?DB_SEMIOCAST,
        <<"DROP TABLE IF EXISTS incoming_results; "
          "CREATE TABLE incoming_results ("
          "digit_index   NUMERIC(30) PRIMARY KEY,"
          "data          TEXT"
          ");">>),
    {ok, {updated, 0}} = cloudi_service_db_pgsql:squery(Context,
        ?DB_WG,
        "DROP TABLE IF EXISTS incoming_results; "
        "CREATE TABLE incoming_results ("
        "digit_index   NUMERIC(30) PRIMARY KEY,"
        "data          TEXT"
        ");"),
    {ok, {updated, 0}} = cloudi_service_db_pgsql:squery(Context,
        ?DB_SEMIOCAST,
        "DROP TABLE IF EXISTS incoming_results; "
        "CREATE TABLE incoming_results ("
        "digit_index   NUMERIC(30) PRIMARY KEY,"
        "data          TEXT"
        ");"),
    {ok, ok} = cloudi_service_db_pgsql:transaction(Context,
        ?DB_WG,
        [<<"DROP TABLE IF EXISTS incoming_results;">>,
         <<"CREATE TABLE incoming_results ("
           "digit_index   NUMERIC(30) PRIMARY KEY,"
           "data          TEXT"
           ");">>]),
    {ok, ok} = cloudi_service_db_pgsql:transaction(Context,
        ?DB_SEMIOCAST,
        [<<"DROP TABLE IF EXISTS incoming_results;">>,
         <<"CREATE TABLE incoming_results ("
           "digit_index   NUMERIC(30) PRIMARY KEY,"
           "data          TEXT"
           ");">>]),
    {ok, ok} = cloudi_service_db_pgsql:transaction(Context,
        ?DB_WG,
        [<<"INSERT INTO incoming_results (digit_index, data) "
           "VALUES (1, 'one');">>,
         <<"INSERT INTO incoming_results (digit_index, data) "
           "VALUES (2, 'two');">>]),
    {ok, ok} = cloudi_service_db_pgsql:transaction(Context,
        ?DB_SEMIOCAST,
        [<<"INSERT INTO incoming_results (digit_index, data) "
           "VALUES (3, 'three');">>,
         <<"INSERT INTO incoming_results (digit_index, data) "
           "VALUES (4, 'four');">>]),
    {ok, {selected, RowsWG}} = cloudi_service_db_pgsql:squery(Context,
        ?DB_WG, "SELECT * FROM incoming_results"),
    % wg driver returns row data as binaries
    [{<<"1">>,<<"one">>},
     {<<"2">>,<<"two">>},
     {<<"3">>,<<"three">>},
     {<<"4">>,<<"four">>}] = RowsWG,
    {ok, {selected, RowsSEMIOCAST}} = cloudi_service_db_pgsql:squery(Context,
        ?DB_SEMIOCAST, <<"SELECT * FROM incoming_results">>),
    % semiocast driver returns row data as erlang types
    [{1,<<"one">>},
     {2,<<"two">>},
     {3,<<"three">>},
     {4,<<"four">>}] = RowsSEMIOCAST,
    {ok, {updated, 0}} = cloudi_service_db_pgsql:squery(Context,
        ?DB_WG, "DROP TABLE incoming_results"),
    ok.

t_table_create_2(_Config) ->
    Context = cloudi:new(),
    {ok, {error, Message1}} = cloudi_service_db_pgsql:transaction(Context,
        ?DB_WG,
        [<<"CREATE TABLE incoming_results ("
           "digit_index   NUMERIC(30) PRIMARY KEY,"
           "data          TEXT"
           ");">>,
         <<"INSERT INTO incoming_results (digit_index, data) "
           "VALUES (1, 'one');">>,
         <<"INSERT INTO incoming_results (digit_index, data) "
           "VALUES (invalid, 'two');">>]),
    true = is_binary(Message1),
    {ok, {error, Message1}} = cloudi_service_db_pgsql:transaction(
        Context,
        ?DB_SEMIOCAST,
        [<<"CREATE TABLE incoming_results ("
           "digit_index   NUMERIC(30) PRIMARY KEY,"
           "data          TEXT"
           ");">>,
         <<"INSERT INTO incoming_results (digit_index, data) "
           "VALUES (1, 'one');">>,
         <<"INSERT INTO incoming_results (digit_index, data) "
           "VALUES (invalid, 'two');">>]),
    {ok, {error, Message2}} = cloudi_service_db_pgsql:squery(Context,
        ?DB_SEMIOCAST, <<"DROP TABLE incoming_results">>),
    true = is_binary(Message2),
    ok.

t_table_query_1(_Config) ->
    Context = cloudi:new(),
    {ok, ok} = cloudi_service_db_pgsql:transaction(Context,
        ?DB_WG,
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
    {ok, {selected, RowsWG}} = cloudi_service_db_pgsql:equery(Context,
        ?DB_WG, <<"SELECT * FROM incoming_results "
                  "WHERE digit_index = $1">>, [2]),
    [{<<"2">>,<<"two">>}] = RowsWG,
    {ok, {selected, RowsSEMIOCAST}} = cloudi_service_db_pgsql:equery(Context,
        ?DB_SEMIOCAST, <<"SELECT * FROM incoming_results "
                         "WHERE digit_index = $1">>, [2]),
    [{2,<<"two">>}] = RowsSEMIOCAST,
    {ok, {updated, 0}} = cloudi_service_db_pgsql:squery(Context,
        ?DB_SEMIOCAST, <<"DROP TABLE incoming_results">>),
    ok.

t_table_query_2(_Config) ->
    Context = cloudi:new(),
    {ok, {updated, 0}} = cloudi_service_db_pgsql:squery(Context,
        ?DB_WG,
        <<"DROP TABLE IF EXISTS binary_results; "
          "CREATE TABLE binary_results ("
          "digit_index   NUMERIC(30) PRIMARY KEY,"
          "data          BYTEA"
          ");">>),
    Binary = <<16#DE, 16#AD, 16#BE, 16#EF>>,
    {ok, {updated, 1}} = cloudi_service_db_pgsql:equery(Context,
        ?DB_WG, <<"INSERT INTO binary_results (digit_index, data) "
                  "VALUES (1, $1)">>, [Binary]),
    {ok, {updated, 1}} = cloudi_service_db_pgsql:equery(Context,
        ?DB_SEMIOCAST, <<"INSERT INTO binary_results (digit_index, data) "
                         "VALUES (2, $1)">>, [Binary]),
    {ok, {updated, 1}} = cloudi_service_db_pgsql:squery(Context,
        ?DB_WG, <<"INSERT INTO binary_results (digit_index, data) "
                  "VALUES (3, E'\\\\xDEADBEEF')">>),
    {ok, {updated, 1}} = cloudi_service_db_pgsql:squery(Context,
        ?DB_SEMIOCAST, <<"INSERT INTO binary_results (digit_index, data) "
                         "VALUES (4, E'\\\\xDEADBEEF')">>),
    {ok, {selected, RowsWG}} = cloudi_service_db_pgsql:squery(Context,
        ?DB_WG, <<"SELECT * FROM binary_results">>),
    BinaryWG = <<"\\xdeadbeef">>, % wg doesn't really support BYTEA
    [{<<"1">>,BinaryWG},
     {<<"2">>,BinaryWG},
     {<<"3">>,BinaryWG},
     {<<"4">>,BinaryWG}] = RowsWG,
    {ok, {selected, RowsSEMIOCAST}} = cloudi_service_db_pgsql:squery(Context,
        ?DB_SEMIOCAST, <<"SELECT * FROM binary_results">>),
    [{1,Binary},
     {2,Binary},
     {3,Binary},
     {4,Binary}] = RowsSEMIOCAST,
    {ok, {updated, 0}} = cloudi_service_db_pgsql:squery(Context,
        ?DB_SEMIOCAST, <<"DROP TABLE binary_results">>),
    ok.

t_table_query_3(_Config) ->
    Context = cloudi:new(),
    {ok, {updated, 0}} = cloudi_service_db_pgsql:squery(Context,
        ?DB_WG,
        <<"DROP TABLE IF EXISTS incoming_results; "
          "CREATE TABLE incoming_results ("
          "digit_index   NUMERIC(30) PRIMARY KEY,"
          "data          TEXT NULL"
          ");">>),
    {ok, {updated, 1}} = cloudi_service_db_pgsql:equery(Context,
        ?DB_WG, <<"INSERT INTO incoming_results (digit_index, data) "
                  "VALUES (1, $1)">>, [null]),
    {ok, {updated, 1}} = cloudi_service_db_pgsql:equery(Context,
        ?DB_SEMIOCAST, <<"INSERT INTO incoming_results (digit_index, data) "
                         "VALUES (2, $1)">>, [null]),
    {ok, {selected, RowsWG}} = cloudi_service_db_pgsql:squery(Context,
        ?DB_WG, <<"SELECT * FROM incoming_results">>),
    [{<<"1">>,null},
     {<<"2">>,null}] = RowsWG,
    {ok, {selected, RowsSEMIOCAST}} = cloudi_service_db_pgsql:squery(Context,
        ?DB_SEMIOCAST, <<"SELECT * FROM incoming_results">>),
    [{1,null},
     {2,null}] = RowsSEMIOCAST,
    {ok, {updated, 0}} = cloudi_service_db_pgsql:squery(Context,
        ?DB_SEMIOCAST, <<"DROP TABLE incoming_results">>),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

test_condition(L) ->
    case gen_tcp:connect(?DEFAULT_PGSQL_HOST, ?DEFAULT_PGSQL_PORT, []) of
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
    end.

