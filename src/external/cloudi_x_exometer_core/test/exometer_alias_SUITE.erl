-module(exometer_alias_SUITE).

%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------
%% common_test exports
-export(
   [
    all/0, groups/0, suite/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
   ]).

%% test case exports
-export(
   [
    test_register_alias/1,
    test_load_map/1,
    test_unload_map/1,
    test_map_duplicates/1,
    test_map_badargs/1,
    test_map_existing/1,
    test_map_multiple_errors/1
   ]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% common_test API
%%%===================================================================

all() ->
    [
     {group, test_alias},
     {group, test_map}
    ].

groups() ->
    [
     {test_alias, [shuffle],
      [
       test_register_alias
      ]},
     {test_map, [shuffle],
      [
       test_load_map,
       test_unload_map,
       test_map_duplicates,
       test_map_badargs,
       test_map_existing,
       test_map_multiple_errors
      ]}
    ].

suite() ->
    [].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(Case, Config) ->
    {ok, StartedApps} = exometer_test_util:ensure_all_started(exometer_core),
    [{started_apps, StartedApps} | Config].

end_per_testcase(_Case, Config) ->
    [application:stop(App) || App <- ?config(started_apps, Config)],
    ok.

%%%===================================================================
%%% Test Cases
%%%===================================================================
test_register_alias(_Config) ->
    M = [?MODULE, ?LINE],
    ok = exometer_alias:new(t1, M, value),
    {M, value} = exometer_alias:resolve(t1),
    ok = exometer_alias:delete(t1),
    error = exometer_alias:resolve(t1),
    ok.

test_load_map(_Config) ->
    ok = exometer_alias:load(fun map1/0),
    {[map1,1], a} = exometer_alias:resolve(m11a),
    {[map1,1], b} = exometer_alias:resolve(m11b),
    {[map1,2], a} = exometer_alias:resolve(m12a),
    {[map1,2], b} = exometer_alias:resolve(m12b),
    ok.

test_unload_map(Config) ->
    ok = test_load_map(Config),
    ok = exometer_alias:unload(fun map1/0),
    error = exometer_alias:resolve(m11a),
    error = exometer_alias:resolve(m11b),
    error = exometer_alias:resolve(m12a),
    error = exometer_alias:resolve(m12b),
    ok.

test_map_duplicates(_Config) ->
    {error, {map_error, [{duplicate_aliases,
			  [{m21a, [{[map2,1], a},
				   {[map2,1], b}]}]
			 }]}} =
	exometer_alias:load(fun map2/0),
    ok.

test_map_badargs(_Config) ->
    {error, {map_error, [{invalid,
			  [{m31a,[{{map3,1},a}]}]
			 }]}} =
	exometer_alias:load(fun map3/0),
    ok.

test_map_existing(_Config) ->
    exometer_alias:new(m11a, [some,metric], a),
    {error, {map_error, [{existing_aliases,
			  [{m11a, [{[some,metric], a}]}]}
			]}} =
	exometer_alias:load(fun map1/0),
    ok.

test_map_multiple_errors(_Config) ->
    exometer_alias:new(m11a, [some,other,metric], a),
    Map = fun() ->
		  map1() ++ map2() ++ map3()
	  end,
    {error, {map_error, Errors}} = exometer_alias:load(Map),
    [
     {duplicate_aliases, [
			  {m21a,[{[map2,1],a},{[map2,1], b}]}
			 ]},
     {existing_aliases, [
			 {m11a, [{[some,other,metric], a}]}
			]},
     {invalid, [
		{m31a, [{{map3,1},a}]}
	       ]}
    ] = lists:sort(Errors),
    ok.

map1() ->
    [
     {[map1, 1], [{a, m11a},
		  {b, m11b}]},
     {[map1, 2], [{a, m12a},
		  {b, m12b}]}
    ].

%% contains error: duplicate aliases
map2() ->
    [
     {[map2, 1], [{a, m21a},
		  {b, m21a}]},
     {[map2, 2], [{a, m22a},
		  {b, m22b}]}
    ].

map3() ->
    [
     {{map3, 1}, [{a, m31a}]}
    ].
