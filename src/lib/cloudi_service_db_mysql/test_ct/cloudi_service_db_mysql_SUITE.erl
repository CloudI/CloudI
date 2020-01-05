%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
-module(cloudi_service_db_mysql_SUITE).

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

-ifndef(CLOUDI_TEST_TIMEOUT).
-define(CLOUDI_TEST_TIMEOUT, 10). % seconds
-endif.
-define(DEFAULT_MYSQL_HOST, "127.0.0.1").
-define(DEFAULT_MYSQL_PORT, 3306).

% different driver service names
-define(DB_EONBLAST, "/db/mysql/eonblast/cloudi_tests").

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
     {timetrap, ?CLOUDI_TEST_TIMEOUT * 1000 + 100}].

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
     Context1} = cloudi_service_db_mysql:squery(Context0,
        ?DB_EONBLAST,
        <<"DROP TABLE IF EXISTS incoming_results; "
          "CREATE TABLE incoming_results ("
          "digit_index   NUMERIC(30) PRIMARY KEY,"
          "data          TEXT"
          ");">>),
    {{ok, {updated, 0}},
     Context2} = cloudi_service_db_mysql:squery(Context1,
        ?DB_EONBLAST,
        "DROP TABLE IF EXISTS incoming_results; "
        "CREATE TABLE incoming_results ("
        "digit_index   NUMERIC(30) PRIMARY KEY,"
        "data          TEXT"
        ");"),
    {{ok, ok},
     Context3} = cloudi_service_db_mysql:transaction(Context2,
        ?DB_EONBLAST,
        [<<"DROP TABLE IF EXISTS incoming_results;">>,
         <<"CREATE TABLE incoming_results ("
           "digit_index   NUMERIC(30) PRIMARY KEY,"
           "data          TEXT"
           ");">>]),
    {{ok, ok},
     Context4} = cloudi_service_db_mysql:transaction(Context3,
        ?DB_EONBLAST,
        [<<"INSERT INTO incoming_results (digit_index, data) "
           "VALUES (1, 'one');">>,
         <<"INSERT INTO incoming_results (digit_index, data) "
           "VALUES (2, 'two');">>]),
    {{ok, {selected, RowsEONBLAST}},
     Context5} = cloudi_service_db_mysql:squery(Context4,
        ?DB_EONBLAST, <<"SELECT * FROM incoming_results">>),
    [{1,<<"one">>},
     {2,<<"two">>}] = RowsEONBLAST,
    {{ok, {updated, 0}},
     _} = cloudi_service_db_mysql:squery(Context5,
        ?DB_EONBLAST, "DROP TABLE incoming_results"),
    ok.

t_table_create_2(_Config) ->
    Context0 = cloudi:new(),
    {{ok, {error, Message1}},
     _} = cloudi_service_db_mysql:transaction(Context0,
        ?DB_EONBLAST,
        [<<"CREATE TABLE incoming_results ("
           "digit_index   NUMERIC(30) PRIMARY KEY,"
           "data          TEXT"
           ");">>,
         <<"INSERT INTO incoming_results (digit_index, data) "
           "VALUES (1, 'one');">>,
         <<"INSERT INTO incoming_results (digit_index, data) "
           "VALUES (invalid, 'two');">>]),
    true = is_binary(Message1),
    ok.

t_table_query_1(_Config) ->
    Context0 = cloudi:new(),
    {{ok, ok},
     Context1} = cloudi_service_db_mysql:transaction(Context0,
        ?DB_EONBLAST,
        [<<"DROP TABLE IF EXISTS incoming_results;">>,
         <<"CREATE TABLE incoming_results ("
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
    {{ok, {selected, RowsEONBLAST}},
     Context2} = cloudi_service_db_mysql:equery(Context1,
        ?DB_EONBLAST, <<"SELECT * FROM incoming_results "
                        "WHERE digit_index = ?">>, [2]),
    [{2,<<"two">>}] = RowsEONBLAST,
    {{ok, {updated, 0}},
     _} = cloudi_service_db_mysql:squery(Context2,
        ?DB_EONBLAST, <<"DROP TABLE incoming_results">>),
    ok.

t_table_query_2(_Config) ->
    Context0 = cloudi:new(),
    {{ok, {updated, 0}},
     Context1} = cloudi_service_db_mysql:squery(Context0,
        ?DB_EONBLAST,
        <<"DROP TABLE IF EXISTS binary_results; "
          "CREATE TABLE binary_results ("
          "digit_index   NUMERIC(30) PRIMARY KEY,"
          "data          BLOB"
          ");">>),
    Binary = <<16#DE, 16#AD, 16#BE, 16#EF>>,
    {{ok, {updated, 1}},
     Context2} = cloudi_service_db_mysql:equery(Context1,
        ?DB_EONBLAST, <<"INSERT INTO binary_results (digit_index, data) "
                        "VALUES (1, ?)">>, [Binary]),
    {{ok, {updated, 1}},
     Context3} = cloudi_service_db_mysql:squery(Context2,
        ?DB_EONBLAST, <<"INSERT INTO binary_results (digit_index, data) "
                        "VALUES (2, X'DEADBEEF')">>),
    {{ok, {selected, RowsEONBLAST}},
     Context4} = cloudi_service_db_mysql:squery(Context3,
        ?DB_EONBLAST, <<"SELECT * FROM binary_results">>),
    BinaryEONBLAST = Binary,
    [{1,BinaryEONBLAST},
     {2,BinaryEONBLAST}] = RowsEONBLAST,
    {{ok, {updated, 0}},
     _} = cloudi_service_db_mysql:squery(Context4,
        ?DB_EONBLAST, <<"DROP TABLE binary_results">>),
    ok.

t_table_query_3(_Config) ->
    Context0 = cloudi:new(),
    {{ok, {updated, 0}},
     Context1} = cloudi_service_db_mysql:squery(Context0,
        ?DB_EONBLAST,
        <<"DROP TABLE IF EXISTS incoming_results; "
          "CREATE TABLE incoming_results ("
          "digit_index   NUMERIC(30) PRIMARY KEY,"
          "data          TEXT NULL"
          ");">>),
    {{ok, {updated, 1}},
     Context2} = cloudi_service_db_mysql:equery(Context1,
        ?DB_EONBLAST, <<"INSERT INTO incoming_results (digit_index, data) "
                        "VALUES (1, ?)">>, [null]),
    {{ok, {selected, RowsEONBLAST}},
     Context3} = cloudi_service_db_mysql:squery(Context2,
        ?DB_EONBLAST, <<"SELECT * FROM incoming_results">>),
    [{1,undefined}] = RowsEONBLAST,
    {{ok, {updated, 0}},
     _} = cloudi_service_db_mysql:squery(Context3,
        ?DB_EONBLAST, <<"DROP TABLE incoming_results">>),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

test_condition(L) ->
    case gen_tcp:connect(?DEFAULT_MYSQL_HOST, ?DEFAULT_MYSQL_PORT, []) of
        {ok, Socket} ->
            catch gen_tcp:close(Socket),
            L;
        {error, econnrefused} ->
            error_logger:error_msg("unable to test ~p",
                                   [{?DEFAULT_MYSQL_HOST,
                                     ?DEFAULT_MYSQL_PORT}]),
            {skip, mysql_dead};
        {error, Reason} ->
            error_logger:error_msg("unable to test ~p: ~p",
                                   [{?DEFAULT_MYSQL_HOST,
                                     ?DEFAULT_MYSQL_PORT}, Reason]),
            {skip, mysql_dead}
    end.

