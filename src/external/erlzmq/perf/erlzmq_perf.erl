-module(erlzmq_perf).
-export([recv_loop/2, send_loop/3]).


recv_loop(0, _) ->
    ok;
recv_loop(N, S) ->
    erlzmq:recv(S),
    recv_loop(N-1, S).

send_loop(0, _, _) ->
    ok;
send_loop(N, S, M) ->
    erlzmq:send(S, M),
    send_loop(N-1, S, M).
