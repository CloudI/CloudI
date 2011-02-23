%% Basic REQ/REP client for testing.
%% @hidden

-module(zmq_reqclient).
-export([run/0, run/3]).

run()                   -> do_run(false, 50, 1000).
run(active,  N, Delay)  -> do_run(true,  N, Delay);
run(passive, N, Delay)  -> do_run(false, N, Delay).

do_run(Mode, N, Delay) when is_boolean(Mode), is_integer(N), is_integer(Delay) ->
    spawn(fun() ->
        case zmq:socket(req, [{active, Mode}]) of
        {ok, Socket} -> 
            zmq:connect(Socket, "tcp://127.0.0.1:5550"),
            reqrep(Mode, Socket, Delay, N);
        Other -> 
            io:format("~p error creating socket: ~p\n", [self(), Other])
        end
    end).

reqrep(_Mode, _Socket, _Delay, 0) ->
    ok;
reqrep(Mode, Socket, Delay, MsgIndex) ->
    Data = {msg, MsgIndex},
    case zmq:send(Socket, term_to_binary(Data)) of 
    ok -> 
        io:format("~p ~w sent ~p\n", [self(), ?MODULE, Data]),
        recv(Socket, Mode, MsgIndex),
        timer:sleep(Delay),
        reqrep(Mode, Socket, Delay, MsgIndex-1);
    Other -> 
        io:format("~p unexpected error in zmq:send(): ~p\n", [self(), Other])
    end.
        
recv(Socket, Mode, N) ->
    case zmq_subclient:recv(Socket, Mode) of
    {ok, {msg, N}} ->
        io:format("~p -> subclient received ~p\n", [self(), N]);
    Other ->
        io:format("~p unexpected receive error: ~p\n", [self(), Other]),
        throw(Other)
    end.
