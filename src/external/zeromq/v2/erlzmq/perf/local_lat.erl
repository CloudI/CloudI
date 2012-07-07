#! /usr/bin/env escript
%%! -smp enable -pa ebin -pa perf

main([BindTo,MessageSizeStr,RoundtripCountStr]) ->
    {MessageSize, _} = string:to_integer(MessageSizeStr),
    {RoundtripCount, _} = string:to_integer(RoundtripCountStr),
    {ok, Context} = erlzmq:context(),
    {ok, Socket} = erlzmq:socket(Context, [rep, {active, false}]),
    ok = erlzmq:bind(Socket, BindTo),
    Msg = list_to_binary(lists:duplicate(MessageSize, 0)),
    ok = erlzmq_perf:local_lat_loop(RoundtripCount, Socket, Msg),
    erlzmq:close(Socket),
    erlzmq:term(Context).

