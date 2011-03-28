-module(erlzmq_perf).
-export([local_lat_loop/3, local_lat_loop/4,
         remote_lat_loop/3, remote_lat_loop/4,
         recv_loop/2, recv_loop/3, send_loop/3]).

local_lat_loop(N, S, M) ->
    local_lat_loop(N, S, M, passive).

local_lat_loop(0, _, _, _) ->
    ok;
local_lat_loop(N, S, M, active) ->
    ok = receive
        {zmq, S, M} ->
            ok
    end,
    ok = erlzmq:send(S, M),
    local_lat_loop(N-1, S, M, active);
local_lat_loop(N, S, M, passive) ->
    {ok, M} = erlzmq:recv(S),
    ok = erlzmq:send(S, M),
    local_lat_loop(N-1, S, M, passive).

remote_lat_loop(N, S, M) ->
    remote_lat_loop(N, S, M, passive).

remote_lat_loop(0, _, _, _) ->
    ok;
remote_lat_loop(N, S, M, active) ->
    ok = erlzmq:send(S, M),
    ok = receive
        {zmq, S, M} ->
            ok
    end,
    remote_lat_loop(N-1, S, M, active);

remote_lat_loop(N, S, M, passive) ->
    ok = erlzmq:send(S, M),
    {ok, M} = erlzmq:recv(S),
    remote_lat_loop(N-1, S, M, passive).

recv_loop(N, S) ->
    recv_loop(N, S, passive).

recv_loop(0, _, _) ->
    ok;
recv_loop(N, S, passive) ->
    erlzmq:recv(S),
    recv_loop(N-1, S, passive);
recv_loop(N, S, active) ->
    receive
        {zmq, S, _Msg} ->
            ok
    end,
    recv_loop(N-1, S, active).

send_loop(0, _, _) ->
    ok;
send_loop(N, S, M) ->
    erlzmq:send(S, M),
    send_loop(N-1, S, M).


