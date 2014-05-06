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
         t_table_query_1/1]).

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
      [t_table_query_1]}].

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
    {ok, {updated, 0}} = cloudi_service_db_pgsql:transaction(Context,
        ?DB_WG,
        [<<"DROP TABLE IF EXISTS incoming_results">>,
         <<"CREATE TABLE incoming_results ("
           "digit_index   NUMERIC(30) PRIMARY KEY,"
           "data          TEXT"
           ")">>]),
    {ok, {updated, 0}} = cloudi_service_db_pgsql:transaction(Context,
        ?DB_SEMIOCAST,
        [<<"DROP TABLE IF EXISTS incoming_results">>,
         <<"CREATE TABLE incoming_results ("
           "digit_index   NUMERIC(30) PRIMARY KEY,"
           "data          TEXT"
           ")">>]),
    {ok, {updated, 1}} = cloudi_service_db_pgsql:transaction(Context,
        ?DB_WG,
        [<<"INSERT INTO incoming_results (digit_index, data) "
           "values (1, 'one')">>,
         <<"INSERT INTO incoming_results (digit_index, data) "
           "values (2, 'two')">>]),
    {ok, {updated, 1}} = cloudi_service_db_pgsql:transaction(Context,
        ?DB_SEMIOCAST,
        [<<"INSERT INTO incoming_results (digit_index, data) "
           "values (3, 'three')">>,
         <<"INSERT INTO incoming_results (digit_index, data) "
           "values (4, 'four')">>]),
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
    % XXX DB_SEMIOCAST can't handle this yet, it crashes the driver
    {ok, {error, MessageWG}} = cloudi_service_db_pgsql:transaction(Context,
        ?DB_WG,
        [<<"CREATE TABLE incoming_results ("
           "digit_index   NUMERIC(30) PRIMARY KEY,"
           "data          TEXT"
           ")">>,
         <<"INSERT INTO incoming_results (digit_index, data) "
           "values (1, 'one')">>,
         <<"INSERT INTO incoming_results (digit_index, data) "
           "values (invalid, 'two')">>]),
    true = is_binary(MessageWG),
    % XXX same problem is here
    %{ok, {error, MessageWG}} = cloudi_service_db_pgsql:squery(Context,
    %    ?DB_SEMIOCAST,
    %    <<"BEGIN;"
    %      "CREATE TABLE incoming_results ("
    %      "digit_index   NUMERIC(30) PRIMARY KEY,"
    %      "data          TEXT"
    %      ");"
    %      "INSERT INTO incoming_results (digit_index, data) "
    %      "values (1, 'one');"
    %      "INSERT INTO incoming_results (digit_index, data) "
    %      "values (invalid, 'two');"
    %      "COMMIT;">>),
    {ok, {error, MessageSEMIOCAST}} = cloudi_service_db_pgsql:squery(Context,
        ?DB_SEMIOCAST, <<"DROP TABLE incoming_results">>),
    true = is_binary(MessageSEMIOCAST),
    {ok, {error, MessageSEMIOCAST}} = cloudi_service_db_pgsql:squery(Context,
        ?DB_WG, <<"DROP TABLE incoming_results">>),
    ok.

t_table_query_1(_Config) ->
    Context = cloudi:new(),
    {ok, {updated, 1}} = cloudi_service_db_pgsql:transaction(Context,
        ?DB_WG,
        [<<"CREATE TABLE incoming_results ("
           "digit_index   NUMERIC(30) PRIMARY KEY,"
           "data          TEXT"
           ")">>,
         <<"INSERT INTO incoming_results (digit_index, data) "
           "values (1, 'one')">>,
         <<"INSERT INTO incoming_results (digit_index, data) "
           "values (2, 'two')">>,
         <<"INSERT INTO incoming_results (digit_index, data) "
           "values (3, 'three')">>,
         <<"INSERT INTO incoming_results (digit_index, data) "
           "values (4, 'four')">>]),
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

