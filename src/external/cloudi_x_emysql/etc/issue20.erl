%% Test for Issue 20, courtesy Bart van Deenen, 1 Feb 2012.
%% The bug was a missing check of what pool a queued execution was waiting for.
%% The fix included introducing wait queues per pool.
%% To run this test, make a folder test, move this file into it,
%% move emysql into test/emysql, clone eunit into test/eunit:
%% $ git clone https://github.com/abhay/eunit.git # then do
%% $ cd emysql && make && cd .. && cd eunit && make && cd ..
%% $ erlc issue20.erl && erl -pa emysql/ebin -s issue20 test -s init stop -noshell
%% /hd 2 1 12

-module(issue20).

-include_lib("eunit/include/eunit.hrl").
-include_lib("emysql/include/emysql.hrl").

-compile(export_all).

test() ->

    crypto:start(),
    application:start(emysql),

    try emysql:remove_pool(test1) catch _:_ -> ok end,
    try emysql:remove_pool(test2) catch _:_ -> ok end,
    try emysql:remove_pool(test3) catch _:_ -> ok end,

    emysql:add_pool(test1, 1, "demo", "demo", "localhost", 3306, "emysql1", utf8),
    emysql:add_pool(test2, 1, "demo", "demo", "localhost", 3306, "emysql2", utf8),
    emysql:add_pool(test3, 1, "demo", "demo", "localhost", 3306, "emysql1", utf8),

    ?assertMatch( #result_packet{}, emysql:execute(test1, "select a from test")),

    ?assertMatch( #result_packet{}, emysql:execute(test2, "select b from test")),

    ?assertMatch( #result_packet{}, emysql:execute(test3, "select A from test")),

    F=fun() ->
        timer:sleep(100+random:uniform(500)),
        ?assertMatch( #result_packet{}, emysql:execute(test2, "select b from test")),
        ?assertMatch( #result_packet{}, emysql:execute(test3, "select A from test")),
        ok
    end,
    [spawn(F) || _ <- lists:seq(1,5)],
    timer:sleep(1000),
    ok.