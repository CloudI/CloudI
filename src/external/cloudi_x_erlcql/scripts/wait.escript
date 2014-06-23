#!/usr/bin/env escript

%% Usage: ./wait.escript <host> <port> <timeout>
main([H, P, T]) when is_list(T) ->
    wait(H, list_to_integer(P), timer:seconds(list_to_integer(T))).

wait(H, P, 0) ->
    io:format("No local Cassandra running (~s:~p)~n", [H, P]),
    halt(2);
wait(H, P, T) ->
    case gen_tcp:connect(H, P, []) of
        {ok, _} ->
            io:format("Connected to local Cassandra (~s:~p)~n", [H, P]);
        {error, _} ->
            timer:sleep(timer:seconds(1)),
            wait(H, P, T - timer:seconds(1))
    end.
