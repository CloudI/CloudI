-module(erlzmq_test).
-include_lib("eunit/include/eunit.hrl").
-export([worker/2]).

% provides some context for failures only viewable within the C code
%-define(PRINT_DEBUG, true).

-ifdef(PRINT_DEBUG).
% use stderr while bypassing the io server to avoid buffering
-define(PRINT_START,
        PRINT_PORT = open_port({fd, 0, 2}, [out, {line, 256}]),
        port_command(PRINT_PORT,
                     io_lib:format("~w:~w start~n", [?MODULE, ?LINE]))).
-define(PRINT_CHECK(ANY),
        port_command(PRINT_PORT,
                     io_lib:format("~w:~w ~p~n", [?MODULE, ?LINE, ANY]))).
-define(PRINT_END, 
        port_command(PRINT_PORT,
                     io_lib:format("~w:~w end~n", [?MODULE, ?LINE])),
        port_close(PRINT_PORT),
        ok).
-else.
-define(PRINT_START, ok).
-define(PRINT_CHECK(_), ok).
-define(PRINT_END, ok).
-endif.

hwm_test() ->
    ?PRINT_START,
    ?PRINT_CHECK(lists:flatten(
        io_lib:format("executing as os pid ~s", [os:getpid()]))),
    {ok, C} = erlzmq:context(),
    {ok, S1} = erlzmq:socket(C, [pull, {active, false}]),
    {ok, S2} = erlzmq:socket(C, [push, {active, false}]),

    ok = erlzmq:setsockopt(S1, rcvhwm, 2),
    ok = erlzmq:setsockopt(S2, sndhwm, 2),


    ok = erlzmq:bind(S1, "inproc://a"),
    ok = erlzmq:connect(S2, "inproc://a"),

    ok = hwm_loop(10, S2),

    ?assertMatch({ok, <<"test">>}, erlzmq:recv(S1)),
    ?assertMatch({ok, <<"test">>}, erlzmq:recv(S1)),
    ?assertMatch({ok, <<"test">>}, erlzmq:recv(S1)),
    ?assertMatch({ok, <<"test">>}, erlzmq:recv(S1)),

    ?assertMatch(ok, erlzmq:send(S2, <<"test">>)),

    ?assertMatch({ok, <<"test">>}, erlzmq:recv(S1)),

    ok = erlzmq:close(S1),
    ok = erlzmq:close(S2),
    ok = erlzmq:term(C),
    ?PRINT_END.

hwm_loop(0, _S) ->
    ok;
hwm_loop(N, S) when N > 6 ->
    ?assertMatch(ok, erlzmq:send(S, <<"test">>, [dontwait])),
    hwm_loop(N-1, S);
hwm_loop(N, S) ->
    ?assertMatch({error, _} ,erlzmq:send(S, <<"test">>, [dontwait])),
    hwm_loop(N-1, S).

invalid_rep_test() ->
    ?PRINT_START,
    {ok, Ctx} = erlzmq:context(),

    {ok, XrepSocket} = erlzmq:socket(Ctx, [xrep, {active, false}]),
    {ok, ReqSocket} = erlzmq:socket(Ctx, [req, {active, false}]),

    ok = erlzmq:setsockopt(XrepSocket, linger, 0),
    ok = erlzmq:setsockopt(ReqSocket, linger, 0),
    ok = erlzmq:bind(XrepSocket, "inproc://hi"),
    ok = erlzmq:connect(ReqSocket, "inproc://hi"),

    %%  Initial request.
    ok = erlzmq:send(ReqSocket, <<"r">>),

    %%  Receive the request.
    {ok, Addr} = erlzmq:recv(XrepSocket),
    {ok, Bottom} = erlzmq:recv(XrepSocket),
    {ok, _Body} = erlzmq:recv(XrepSocket),

    %%  Send invalid reply.
    ok = erlzmq:send(XrepSocket, Addr),

    %%  Send valid reply.
    ok = erlzmq:send(XrepSocket, Addr, [sndmore]),
    ok = erlzmq:send(XrepSocket, Bottom, [sndmore]),
    ok = erlzmq:send(XrepSocket, <<"b">>),

    %%  Check whether we've got the valid reply.
    {ok, <<"b">>} = erlzmq:recv(ReqSocket),

    %%  Tear down the wiring.
    ok = erlzmq:close(XrepSocket),
    ok = erlzmq:close(ReqSocket),
    ok = erlzmq:term(Ctx),
    ?PRINT_END.

pair_inproc_test() ->
    ?PRINT_START,
    basic_tests("inproc://tester", pair, pair, active),
    basic_tests("inproc://tester", pair, pair, passive),
    ?PRINT_END.

pair_ipc_test() ->
    ?PRINT_START,
    basic_tests("ipc:///tmp/tester", pair, pair, active),
    basic_tests("ipc:///tmp/tester", pair, pair, passive),
    ?PRINT_END.

pair_tcp_test() ->
    ?PRINT_START,
    basic_tests("tcp://127.0.0.1:5554", pair, pair, active),
    basic_tests("tcp://127.0.0.1:5555", pair, pair, passive),
    ?PRINT_END.

reqrep_device_test() ->
    ?PRINT_START,
    {ok, Ctx} = erlzmq:context(),

    %%  Create a req/rep device.
    {ok, Xreq} = erlzmq:socket(Ctx, [xreq, {active, false}]),
    ok = erlzmq:bind(Xreq, "tcp://127.0.0.1:5560"),
    {ok, Xrep} = erlzmq:socket(Ctx, [xrep, {active, false}]),
    ok = erlzmq:bind(Xrep, "tcp://127.0.0.1:5561"),

    %%  Create a worker.
    {ok, Rep} = erlzmq:socket(Ctx, [rep, {active, false}]),
    ok= erlzmq:connect(Rep, "tcp://127.0.0.1:5560"),

    %%  Create a client.
    {ok, Req} = erlzmq:socket(Ctx, [req, {active, false}]),
    ok = erlzmq:connect(Req, "tcp://127.0.0.1:5561"),

    %%  Send a request.
    ok = erlzmq:send(Req, <<"ABC">>, [sndmore]),
    ok = erlzmq:send(Req, <<"DEF">>),


    %%  Pass the request through the device.
    lists:foreach(fun(_) ->
                          {ok, Msg} = erlzmq:recv(Xrep),
                          {ok, RcvMore}= erlzmq:getsockopt(Xrep, rcvmore),
                          case RcvMore of
                              0 ->
                                  ok = erlzmq:send(Xreq, Msg);
                              _ ->
                                  ok = erlzmq:send(Xreq, Msg, [sndmore])
                          end
                  end,
                  lists:seq(1, 4)),

    %%  Receive the request.
    {ok, Buff0} = erlzmq:recv(Rep),
    ?assertMatch(<<"ABC">>, Buff0),
    {ok, RcvMore1} = erlzmq:getsockopt(Rep, rcvmore),
    ?assert(RcvMore1 > 0),
    {ok, Buff1} = erlzmq:recv(Rep),
    ?assertMatch(<<"DEF">>, Buff1),
    {ok, RcvMore2} = erlzmq:getsockopt(Rep, rcvmore),
    ?assertMatch(0, RcvMore2),

    %%  Send the reply.
    ok = erlzmq:send(Rep, <<"GHI">>, [sndmore]),
    ok = erlzmq:send (Rep, <<"JKL">>),

    %%  Pass the reply through the device.
    lists:foreach(fun(_) ->
                          {ok, Msg} = erlzmq:recv(Xreq),
                          {ok,RcvMore3} = erlzmq:getsockopt(Xreq, rcvmore),
                          case RcvMore3 of
                              0 ->
                                  ok = erlzmq:send(Xrep, Msg);
                              _ ->
                                  ok = erlzmq:send(Xrep, Msg, [sndmore])
                          end
                  end, lists:seq(1, 4)),

    %%  Receive the reply.
    {ok, Buff2} = erlzmq:recv(Req),
    ?assertMatch(<<"GHI">>, Buff2),
    {ok, RcvMore4} = erlzmq:getsockopt(Req, rcvmore),
    ?assert(RcvMore4 > 0),
    {ok, Buff3} = erlzmq:recv(Req),
    ?assertMatch(<<"JKL">>, Buff3),
    {ok, RcvMore5} = erlzmq:getsockopt(Req, rcvmore),
    ?assertMatch(0, RcvMore5),

    %%  Clean up.
    ok = erlzmq:close(Req),
    ok = erlzmq:close(Rep),
    ok = erlzmq:close(Xrep),
    ok = erlzmq:close(Xreq),
    ok = erlzmq:term(Ctx),
    ?PRINT_END.


reqrep_inproc_test() ->
    ?PRINT_START,
    basic_tests("inproc://test", req, rep, active),
    basic_tests("inproc://test", req, rep, passive),
    ?PRINT_END.

reqrep_ipc_test() ->
    ?PRINT_START,
    basic_tests("ipc:///tmp/tester", req, rep, active),
    basic_tests("ipc:///tmp/tester", req, rep, passive),
    ?PRINT_END.

reqrep_tcp_test() ->
    ?PRINT_START,
    basic_tests("tcp://127.0.0.1:5556", req, rep, active),
    basic_tests("tcp://127.0.0.1:5557", req, rep, passive),
    ?PRINT_END.


sub_forward_test() ->
    ?PRINT_START,
    {ok, Ctx} = erlzmq:context(),

    %%  First, create an intermediate device.
    {ok, Xpub} = erlzmq:socket(Ctx, [xpub, {active, false}]),

    ok = erlzmq:bind(Xpub, "tcp://127.0.0.1:5560"),

    {ok, Xsub} = erlzmq:socket(Ctx, [xsub, {active, false}]),

    ok = erlzmq:bind(Xsub, "tcp://127.0.0.1:5561"),

    %%  Create a publisher.
    {ok, Pub} = erlzmq:socket(Ctx, [pub, {active, false}]),

    ok = erlzmq:connect(Pub, "tcp://127.0.0.1:5561"),

    %%  Create a subscriber.
    {ok, Sub} = erlzmq:socket(Ctx, [sub, {active, false}]),

    ok = erlzmq:connect(Sub, "tcp://127.0.0.1:5560"),

    %%  Subscribe for all messages.
    ok = erlzmq:setsockopt(Sub, subscribe, <<"">>),

    %%  Pass the subscription upstream through the device.
    {ok, Buff0} = erlzmq:recv(Xpub),
    ok = erlzmq:send(Xsub, Buff0),

    %%  Wait a bit till the subscription gets to the publisher.
    timer:sleep(1000),

    %%  Send an empty message.
    ok = erlzmq:send(Pub, <<>>),

    %%  Pass the message downstream through the device.
    {ok, Buff} = erlzmq:recv(Xsub),

    ok = erlzmq:send(Xpub, Buff),

    %%  Receive the message in the subscriber.
    {ok, Buff} = erlzmq:recv(Sub),

    %%  Clean up.
    ok = erlzmq:close(Xpub),
    ok = erlzmq:close(Xsub),
    ok = erlzmq:close(Pub),
    ok = erlzmq:close(Sub),
    ok = erlzmq:term(Ctx),
    ?PRINT_END.

timeo() ->
    ?PRINT_START,
    {ok, Ctx} = erlzmq:context(),
    %%  Create a disconnected socket.
    {ok, Sb} = erlzmq:socket(Ctx, [pull, {active, false}]),
    ok = erlzmq:bind(Sb, "inproc://timeout_test"),
    %%  Check whether non-blocking recv returns immediately.
    {error, eagain} = erlzmq:recv(Sb, [dontwait]),
    %%  Check whether recv timeout is honoured.
    Timeout0 = 500,
    ok = erlzmq:setsockopt(Sb, rcvtimeo, Timeout0),
    {Elapsed0, _} =
        timer:tc(fun() ->
                         ?assertMatch({error, eagain}, erlzmq:recv(Sb))
                 end),
    ?assert(Elapsed0 > 440000 andalso Elapsed0 < 550000),

    %%  Check whether connection during the wait doesn't distort the timeout.
    Timeout1 = 2000,
    ok = erlzmq:setsockopt(Sb, rcvtimeo, Timeout1),
    proc_lib:spawn(fun() ->
                           timer:sleep(1000),
                           {ok, Sc} = erlzmq:socket(Ctx, [push, {active, false}]),
                           ok = erlzmq:connect(Sc, "inproc://timeout_test"),
                           timer:sleep(1000),
                           ok = erlzmq:close(Sc)
                   end),
    {Elapsed1, _} = timer:tc(fun() ->
                                     ?assertMatch({error, eagain}, erlzmq:recv(Sb))
                             end),
    ?assert(Elapsed1 > 1900000 andalso Elapsed1 < 2100000),

    %%  Check that timeouts don't break normal message transfer.
    {ok, Sc} = erlzmq:socket(Ctx, [push, {active, false}]),
    ok = erlzmq:setsockopt(Sb, rcvtimeo, Timeout1),
    ok = erlzmq:setsockopt(Sb, sndtimeo, Timeout1),
    ok = erlzmq:connect(Sc, "inproc://timeout_test"),

    Buff = <<"12345678ABCDEFGH12345678abcdefgh">>,
    ok = erlzmq:send(Sc, Buff),
    case erlzmq:recv(Sb) of
        {ok, Buff} ->
            ok;
        {error, eagain} ->
            timeout
    end,
    %%  Clean-up.
    ok = erlzmq:close(Sc),
    ok = erlzmq:close(Sb),
    ok = erlzmq:term (Ctx),
    ok,
    ?PRINT_END.

timeo_test_() ->
    % sometimes this test can timeout with the default timeout
    {timeout, 10, [
        ?_assert(timeo() =:= ok)
    ]}.

bad_init_test() ->
    ?PRINT_START,
    ?assertEqual({error, einval}, erlzmq:context(-1)),
    ?PRINT_END.

shutdown_stress_test() ->
    ?PRINT_START,
    ?assertMatch(ok, shutdown_stress_loop(10)),
    ?PRINT_END.

version_test() ->
    ?PRINT_START,
    {Major, Minor, Patch} = erlzmq:version(),
    ?assert(is_integer(Major) andalso is_integer(Minor) andalso is_integer(Patch)),
    ?PRINT_END.

shutdown_stress_loop(0) ->
    ok;
shutdown_stress_loop(N) ->
    {ok, C} = erlzmq:context(7),
    {ok, S1} = erlzmq:socket(C, [rep, {active, false}]),
    ?assertMatch(ok, shutdown_stress_worker_loop(100, C)),
    ?assertMatch(ok, join_procs(100)),
    ?assertMatch(ok, erlzmq:close(S1)),
    ?assertMatch(ok, erlzmq:term(C)),
    shutdown_stress_loop(N-1).

shutdown_no_blocking_test() ->
    ?PRINT_START,
    {ok, C} = erlzmq:context(),
    {ok, S} = erlzmq:socket(C, [pub, {active, false}]),
    erlzmq:close(S),
    ?assertEqual(ok, erlzmq:term(C, 500)),
    ?PRINT_END.

shutdown_blocking_test() ->
    ?PRINT_START,
    {ok, C} = erlzmq:context(),
    {ok, _S} = erlzmq:socket(C, [pub, {active, false}]),
    case erlzmq:term(C, 0) of
        {error, {timeout, _}} ->
            % typical
            ok;
        ok ->
            % very infrequent
            ok
    end,
    ?PRINT_END.

shutdown_blocking_unblocking_test() ->
    ?PRINT_START,
    {ok, C} = erlzmq:context(),
    {ok, _} = erlzmq:socket(C, [pub, {active, false}]),
    V = erlzmq:term(C, 0),
    ?assertMatch({error, {timeout, _}}, V),
    {error, {timeout, Ref}} = V,
    % all remaining sockets are automatically closed by term (i.e., zmq_term)
    receive
        {Ref, ok} ->
            ok
    end,
    ?PRINT_END.

join_procs(0) ->
    ok;
join_procs(N) ->
    receive
        proc_end ->
            join_procs(N-1)
    after
        2000 ->
            throw(stuck)
    end.

shutdown_stress_worker_loop(0, _) ->
    ok;
shutdown_stress_worker_loop(N, C) ->
    {ok, S2} = erlzmq:socket(C, [sub, {active, false}]),
    spawn(?MODULE, worker, [self(), S2]),
    shutdown_stress_worker_loop(N-1, C).

worker(Pid, S) ->
    ?assertMatch(ok, erlzmq:connect(S, "tcp://127.0.0.1:5558")),
    ?assertMatch(ok, erlzmq:close(S)),
    Pid ! proc_end.

create_bound_pair(Ctx, Type1, Type2, Mode, Transport) ->
    Active = if
        Mode =:= active ->
            true;
        Mode =:= passive ->
            false
    end,
    {ok, S1} = erlzmq:socket(Ctx, [Type1, {active, Active}]),
    {ok, S2} = erlzmq:socket(Ctx, [Type2, {active, Active}]),
    ok = erlzmq:bind(S1, Transport),
    ok = erlzmq:connect(S2, Transport),
    {S1, S2}.

ping_pong({S1, S2}, Msg, active) ->
    ok = erlzmq:send(S1, Msg, [sndmore]),
    ok = erlzmq:send(S1, Msg),
    receive
        {zmq, S2, Msg, [rcvmore]} ->
            ok
    after
        1000 ->
            ?assertMatch({ok, Msg}, timeout)
    end,
    receive
        {zmq, S2, Msg, []} ->
            ok
    after
        1000 ->
            ?assertMatch({ok, Msg}, timeout)
    end,
    ok = erlzmq:send(S2, Msg),
    receive
        {zmq, S1, Msg, []} ->
            ok
    after
        1000 ->
            ?assertMatch({ok, Msg}, timeout)
    end,
    ok = erlzmq:send(S1, Msg),
    receive
        {zmq, S2, Msg, []} ->
            ok
    after
        1000 ->
            ?assertMatch({ok, Msg}, timeout)
    end,
    ok = erlzmq:send(S2, Msg),
    receive
        {zmq, S1, Msg, []} ->
            ok
    after
        1000 ->
            ?assertMatch({ok, Msg}, timeout)
    end,
    ok;

ping_pong({S1, S2}, Msg, passive) ->
    ok = erlzmq:send(S1, Msg),
    ?assertMatch({ok, Msg}, erlzmq:recv(S2)),
    ok = erlzmq:send(S2, Msg),
    ?assertMatch({ok, Msg}, erlzmq:recv(S1)),
    ok = erlzmq:send(S1, Msg, [sndmore]),
    ok = erlzmq:send(S1, Msg),
    ?assertMatch({ok, Msg}, erlzmq:recv(S2)),
    ?assertMatch({ok, Msg}, erlzmq:recv(S2)),
    ok.

basic_tests(Transport, Type1, Type2, Mode) ->
    {ok, C} = erlzmq:context(1),
    {S1, S2} = create_bound_pair(C, Type1, Type2, Mode, Transport),
    ping_pong({S1, S2}, <<"XXX">>, Mode),
    ok = erlzmq:close(S1),
    ok = erlzmq:close(S2),
    ok = erlzmq:term(C).

