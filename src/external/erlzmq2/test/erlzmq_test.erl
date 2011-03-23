-module(erlzmq_test).
-include_lib("eunit/include/eunit.hrl").
-export([worker/2]).

hwm_test() ->
    {ok, C} = erlzmq:context(),
    {ok, S1} = erlzmq:socket(C, pull),
    {ok, S2} = erlzmq:socket(C, push),

    ok = erlzmq:setsockopt(S2, linger, 0),
    ok = erlzmq:setsockopt(S2, hwm, 5),

    ok = erlzmq:bind(S1, "tcp://127.0.0.1:5858"),
    ok = erlzmq:connect(S2, "tcp://127.0.0.1:5858"),

    ok = hwm_loop(10, S2),

    ?assertMatch({ok, <<"test">>}, erlzmq:recv(S1)),
    ?assertMatch(ok, erlzmq:send(S2, <<"test">>)).

hwm_loop(0, _S) ->
    ok;
hwm_loop(N, S) when N > 5 ->
    ?assertMatch(ok, erlzmq:send(S, <<"test">>, [noblock])),
    hwm_loop(N-1, S);
hwm_loop(N, S) ->
    ?assertMatch({error, _} ,erlzmq:send(S, <<"test">>, [noblock])),
    hwm_loop(N-1, S).


pair_inproc_test() ->
    basic_tests("inproc://tester", pair, pair).

pair_ipc_test() ->
    basic_tests("ipc:///tmp/tester", pair, pair).

pair_tcp_test() ->
    basic_tests("tcp://127.0.0.1:5555", pair, pair).

reqrep_inproc_test() ->
    basic_tests("inproc://test", req, rep).

reqrep_ipc_test() ->
    basic_tests("ipc:///tmp/tester", req, rep).

reqrep_tcp_test() ->
    basic_tests("tcp://127.0.0.1:5556", req, rep).

shutdown_stress_test() ->
    ?assertMatch(ok, shutdown_stress_loop(10)).

shutdown_stress_loop(0) ->
    ok;
shutdown_stress_loop(N) ->
    {ok, C} = erlzmq:context(7),
    {ok, S1} = erlzmq:socket(C, rep),
    ?assertMatch(ok, shutdown_stress_worker_loop(100, C)),
    ?assertMatch(ok, join_procs(100)),
    ?assertMatch(ok, erlzmq:close(S1)),
    ?assertMatch(ok, erlzmq:term(C)),
    shutdown_stress_loop(N-1).

shutdown_no_blocking_test() ->
    {ok, C} = erlzmq:context(),
    {ok, S} = erlzmq:socket(C, pub),
    erlzmq:close(S),
    ?assertEqual(ok, erlzmq:term(C, 500)).

shutdown_blocking_test() ->
    {ok, C} = erlzmq:context(),
    {ok, _S} = erlzmq:socket(C, pub),
    ?assertMatch({error, timeout, _}, erlzmq:term(C, 500)).

shutdown_blocking_unblocking_test() ->
    {ok, C} = erlzmq:context(),
    {ok, S} = erlzmq:socket(C, pub),
    V = erlzmq:term(C, 500),
    ?assertMatch({error, timeout, _}, V),
    {error, timeout, Ref} = V,
    erlzmq:close(S),
    receive 
        {Ref, ok} ->
            ok
    end.

join_procs(0) ->
    ok;
join_procs(N) ->
    receive
        proc_end ->
            join_procs(N-1)
    end.

shutdown_stress_worker_loop(0, _) ->
    ok;
shutdown_stress_worker_loop(N, C) ->
    {ok, S2} = erlzmq:socket(C, sub),
    spawn(?MODULE, worker, [self(), S2]),
    shutdown_stress_worker_loop(N-1, C).

worker(Pid, S) ->
    ?assertMatch(ok, erlzmq:connect(S, "tcp://127.0.0.1:5557")),
    ?assertMatch(ok, erlzmq:close(S)),
    Pid ! proc_end.

create_bound_pair(Ctx, Type1, Type2, Transport) ->
    {ok, S1} = erlzmq:socket(Ctx, Type1),
    {ok, S2} = erlzmq:socket(Ctx, Type2),
    ok = erlzmq:bind(S1, Transport),
    ok = erlzmq:connect(S2, Transport),
    {S1, S2}.

ping_pong({S1, S2}, Msg) ->
    ok = erlzmq:send(S1, Msg),
    ?assertMatch({ok, Msg}, erlzmq:recv(S2)),
    ok = erlzmq:send(S2, Msg).

basic_tests(Transport, Type1, Type2) ->
    {ok, C} = erlzmq:context(1),
    {S1, S2} = create_bound_pair(C, Type1, Type2, Transport),
    ping_pong({S1, S2}, <<"XXX">>).

