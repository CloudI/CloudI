%%%-------------------------------------------------------------------
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright (C) 2011-2012 Mahesh Paolini-Subramanya
%%% @doc Erlastic_search tests
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(test_SUITE).
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').

-include_lib("common_test/include/ct.hrl").

-compile(export_all).

-define(CLOUDI_CONF, "cloudi.conf").

suite() ->
    [{ct_hooks,[cth_surefire]}, {timetrap,{seconds,320}}].

init_per_suite(Config) ->


    % General startup
    Response = cloudi_x_reltool_util:ensure_application_loaded(cloudi_core),
    ct:pal("Response:~p~n", [Response]),
    DataDir = ?config(data_dir, Config),
    ConfFile = DataDir ++ ?CLOUDI_CONF,
    ct:pal("ConfFile:~p~n", [ConfFile]),
    application:set_env(cloudi_core, configuration, ConfFile),
    cloudi_x_reltool_util:application_start(cloudi_core),
    Config.


end_per_suite(_Config) ->
    ok.


init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
    ok.


init_per_testcase(_TestCase, Config) ->
    [{context, cloudi:new()} | Config].

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [{test, [],
      [
         t_test
      ]}
    ].

all() ->
    [
        {group, test}
    ].
t_test(_Config) ->
    true = true.

