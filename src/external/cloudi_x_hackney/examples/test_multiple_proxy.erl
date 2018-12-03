#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../_build/default/lib/hackney/ebin  -pa ../_build/default/lib/*/ebin  -pa ../_build/default/lib/certifi/ebin  -pa ../_build/default/lib/idna/ebin  -pa ../_build/default/lib/metrics/ebin -pa ../_build/default/lib/mimerl/ebin   -pa ../_build/default/lib/ssl_verify_fun/ebin -pa ../_build/default/lib/unicode_util_compat/ebin
%%
%%
%%
%%
-define(MAX, 100).
-define(PROXY_OPTS, {"127.0.0.1", 8888}).

wait_request() ->
    receive
        start -> ok
    end.

request(WaitPid) ->
    wait_request(),
    Method = get,
    Url = <<"https://httparrot.herokuapp.com/get">>,
    Headers = [{<<"Connection">>, <<"keep-alive">>}],
    Options = [{pool, default}, {proxy, ?PROXY_OPTS}],
    {ok, _, _Headers, Ref} = hackney:request(Method, Url, Headers, <<>>, Options),
    {ok, _} = hackney:body(Ref),
    WaitPid ! ack.

wait(100) ->
    io:format("~n", []),
    ok;
wait(N) ->
    receive
        ack ->
            io:format(".", []),
            wait(N+1)
    after 15000 ->
            PoolSize = hackney_pool:count(default),
            io:format("~ntimeout ~p/~p done. ~p in the pool.~n",
                      [N, ?MAX, PoolSize])
    end.


main(_) ->
    application:ensure_all_started(hackney),
    hackney_pool:start_pool(default, []),
    io:format("processing ", []),
    Self = self(),
    Pids = lists:foldr(fun(_, Acc) ->
                    Pid = spawn(fun() -> timer:sleep(100), request(Self) end),
                    [Pid | Acc]
            end, [], lists:seq(1, ?MAX)),

    [Pid ! start || Pid <- Pids],
    wait(0).
