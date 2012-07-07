#! /usr/bin/env escript
%%! -smp enable -pa ebin -pa perf
%-mode(compile).

main([BindTo,MessageSizeStr,MessageCountStr]) ->
    {MessageSize, _} = string:to_integer(MessageSizeStr),
    {MessageCount, _} = string:to_integer(MessageCountStr),
    {ok, Context} = erlzmq:context(),
    {ok, Socket} = erlzmq:socket(Context, [sub, {active, true}]),
    ok = erlzmq:setsockopt(Socket,subscribe, <<>>),
    ok = erlzmq:bind(Socket, BindTo),
    receive
        _ ->
            ok
    end,
    Start = now(),
    erlzmq_perf:recv_loop(MessageCount-1, Socket, active),
    Elapsed = timer:now_diff(now(), Start),

    Throughput = MessageCount / Elapsed * 1000000,
    Megabits = Throughput * MessageSize * 8 / 1000000,

    io:format("message size: ~p [B]~n"
              "message count: ~p~n"
              "mean throughput: ~p [msg/s]~n"
              "mean throughput: ~p [Mb/s]~n",
              [MessageSize, MessageCount, Throughput, Megabits]),
   
    erlzmq:close(Socket),
    erlzmq:term(Context).

