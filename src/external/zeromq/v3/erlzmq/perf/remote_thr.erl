#! /usr/bin/env escript
%%! -smp enable -pa ebin -pa perf

main([ConnectTo,MessageSizeStr,MessageCountStr]) ->
    {MessageSize, _} = string:to_integer(MessageSizeStr),
    {MessageCount, _} = string:to_integer(MessageCountStr),
    {ok, Context} = erlzmq:context(1),
    {ok, Socket} = erlzmq:socket(Context, [pub, {active, false}]),
    erlzmq:connect(Socket, ConnectTo),
    Msg = list_to_binary(lists:duplicate(MessageSize, 0)),
    erlzmq_perf:send_loop(MessageCount, Socket, Msg),
    erlzmq:close(Socket),
    erlzmq:term(Context).
