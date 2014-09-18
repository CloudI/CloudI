%%%-------------------------------------------------------------------
%%% File     : Emysql/test/pool_SUITE.erl
%%% Descr    : Suite #4 - testing connection pools. 
%%% Author   : H. Diedrich
%%% Created  : 01/02/2012 hd
%%% Requires : Erlang 14B (prior may not have ct_run)
%%%-------------------------------------------------------------------
%%%
%%% Run from Emysql/: 
%%%     make test2
%%%
%%% Results see:
%%%     test/index.html
%%%
%%%-------------------------------------------------------------------

-module(pool_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").
-include_lib("../include/emysql.hrl").

% List of test cases.
% Test cases have self explanatory names.
%%--------------------------------------------------------------------
all() -> 
    [three_pools].


%%--------------------------------------------------------------------
init_per_suite(Config) ->

	% if this fails, focus on environment_SUITE to fix test setup.
    crypto:start(),
    application:start(emysql),
    Config.
    
% clean up
%%--------------------------------------------------------------------
end_per_suite(_) ->
	ok.

%% Test Case: Test if three pools work ok.
%% Test for Issue 20, Bart van Deenen, 1 Feb 2012.
%% The bug was a missing check of what pool a queued execution was waiting for.
%% The fix included introducing wait queues per pool, instead of a global one.
%%--------------------------------------------------------------------
three_pools(_) ->

    try emysql:remove_pool(test1) catch _:_ -> ok end,
    try emysql:remove_pool(test2) catch _:_ -> ok end,
    try emysql:remove_pool(test3) catch _:_ -> ok end,

    emysql:add_pool(test1, 1, "test", "test", "localhost", 3306, "test1", utf8),
    emysql:add_pool(test2, 1, "test", "test", "localhost", 3306, "test2", utf8),
    emysql:add_pool(test3, 1, "test", "test", "localhost", 3306, "test1", utf8),

    #result_packet{} = emysql:execute(test1, "select a from test"),
    #result_packet{} = emysql:execute(test2, "select b from test"),
    #result_packet{} = emysql:execute(test3, "select A from test"),

    F=fun() ->
        timer:sleep(100+random:uniform(500)),
            #result_packet{} = emysql:execute(test2, "select b from test"),
            #result_packet{} = emysql:execute(test3, "select A from test"),
        ok
    end,

    [spawn(F) || _ <- lists:seq(1,5)],

	emysql:remove_pool(test1),
	emysql:remove_pool(test2),
	emysql:remove_pool(test3),

    ok.