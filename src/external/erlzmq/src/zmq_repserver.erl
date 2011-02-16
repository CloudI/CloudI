%% Basic REQ/REP server for testing.
%% @hidden

-module(zmq_repserver).
-export([run/0, run/1]).

run()        -> run(passive).
run(active)  -> do_run(true, 1000);
run(passive) -> do_run(false, 1000).

do_run(Mode, Delay) when is_boolean(Mode), is_integer(Delay) ->
    spawn(fun() ->
        case zmq:socket(rep, [{active, Mode}]) of
        {ok, Socket} -> 
            zmq:bind(Socket, "tcp://127.0.0.1:5550"),
            reqrep(Socket, Delay, fun() -> zmq_subclient:recv(Socket, Mode) end);
        Other -> 
            io:format("~p error creating socket: ~p\n", [self(), Other])
        end
    end).

reqrep(Socket, Delay, F) ->
    case catch F() of
    {ok, stop} ->
        ok;
    {ok, {msg, N} = Msg} ->
        io:format("~p -> received ~p\n", [self(), N]),
        Res = zmq:send(Socket, term_to_binary(Msg)),
        io:format("~p <- replied  ~w (~p)\n", [self(), N, Res]),
        timer:sleep(Delay),
        reqrep(Socket, Delay, F);
    Other ->
        io:format("~p unexpected error in zmq:recv(): ~p\n", [self(), Other])
    end.
