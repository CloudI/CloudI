%% Basic PUB/SUB server for testing.
%% @hidden

-module(zmq_pubserver).
-export([run/0, run/2]).

run() ->
    run(50, 1000).
run(N, Delay) ->
    spawn(fun() ->
        case zmq:socket(pub, []) of
        {ok, Socket} -> 
            zmq:bind(Socket, "tcp://127.0.0.1:5550"),
            send(Socket, Delay, N);
        Other -> 
            io:format("~p error creating socket: ~p\n", [self(), Other])
        end
    end).

send(_Socket, _Delay, 0) ->
    ok;
send(Socket, Delay, MsgIndex) ->
    Data = {msg, MsgIndex},
    case zmq:send(Socket, term_to_binary(Data)) of 
    ok -> 
        io:format("~p sent ~p\n", [self(), Data]),
        timer:sleep(Delay),
        send(Socket, Delay, MsgIndex-1);
    Other -> 
        io:format("~p unexpected error in zmq:send(): ~p\n", [self(), Other])
    end.
