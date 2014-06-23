#!/usr/bin/env escript
%%! -pa ../ebin ../deps/backoff/ebin

%% Usage: ./clean.escript
main(_) ->
    {ok, C} = erlcql_client:start_link([]),
    Q = <<"SELECT keyspace_name FROM system.schema_keyspaces">>,
    {ok, {Rs, _}} = erlcql_client:'query'(C, Q, one),
    F = fun([<<"erlcql_tests_", _/binary>>]) -> true;
           (_) -> false end,
    Ks = lists:filter(F, Rs),
    D = fun(K) -> [<<"DROP KEYSPACE ">>, K] end,
    G = fun(K) -> io:format("Dropping keyspace `~s`~n", [K]),
                  erlcql_client:'query'(C, D(K), any) end,
    lists:foreach(G, Ks).
