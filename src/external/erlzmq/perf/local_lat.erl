#! /usr/bin/env escript
%%! -smp enable -pa ebin

main([BindTo,MessageSizeStr,RoundtripCountStr]) ->
    {MessageSize, _} = string:to_integer(MessageSizeStr),
    {RoundtripCount, _} = string:to_integer(RoundtripCountStr),
    {ok, Context} = erlzmq:context(),
    {ok, Socket} = erlzmq:socket(Context, [rep, {active, false}]),
    ok = erlzmq:bind(Socket, BindTo),
    Msg = list_to_binary(lists:duplicate(MessageSize, 0)),
    Do = fun() ->
            {ok, RMsg} = erlzmq:brecv(Socket),
            RMsg = Msg,
            erlzmq:send(Socket, Msg)
        end,
    [ Do() || _I <- lists:seq(1,RoundtripCount) ],
    erlzmq:close(Socket),
    erlzmq:term(Context).

