#! /usr/bin/env escript
%%! -smp enable -pa ebin -pa perf

main([ConnectTo,MessageSizeStr,RoundtripCountStr]) ->
    {MessageSize, _} = string:to_integer(MessageSizeStr),
    {RoundtripCount, _} = string:to_integer(RoundtripCountStr),
    {ok, Context} = erlzmq:context(),
    {ok, Socket} = erlzmq:socket(Context, [req, {active, false}]),
    ok = erlzmq:connect(Socket, ConnectTo),
    Msg = list_to_binary(lists:duplicate(MessageSize, 0)),

    Start = now(),
    erlzmq_perf:remote_lat_loop(RoundtripCount, Socket, Msg),
    Elapsed = timer:now_diff(now(), Start),

    Latency = Elapsed / (RoundtripCount * 2),

    io:format("message size: ~p [B]~n"
              "roundtrip count: ~p~n"
              "average latency: ~p [us]~n",
              [MessageSize, RoundtripCount, Latency]),
   erlzmq:close(Socket),
   erlzmq:term(Context).

