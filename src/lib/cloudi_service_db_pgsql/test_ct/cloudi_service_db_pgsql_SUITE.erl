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
-export([t_create_table_1/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("cloudi_core/include/cloudi_logger.hrl").

-define(DEFAULT_PGSQL_HOST, "127.0.0.1").
-define(DEFAULT_PGSQL_PORT, 5432).

-define(DB, "/db/pgsql/cloudi_tests"). % service name

%%%------------------------------------------------------------------------
%%% Callback functions from CT
%%%------------------------------------------------------------------------

all() ->
    test_condition([{group, create_table_1}]).

groups() ->
    [{create_table_1, [],
      [t_create_table_1]}].

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

t_create_table_1(_Config) ->
    Context = cloudi:new(),
    {ok, _Response} = cloudi:send_sync(Context, ?DB,
        % from hexpi test
        <<"DROP TABLE IF EXISTS incoming_results; "
          "CREATE TABLE incoming_results ("
          "digit_index   NUMERIC(30) PRIMARY KEY,"
          "data          TEXT"
          ");">>),
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

