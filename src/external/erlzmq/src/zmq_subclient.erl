%% Basic PUB/SUB client for testing.
%% @hidden

-module(zmq_subclient).
-export([run/0, run/1, recv/2]).

run()        -> run(passive).
run(active)  -> do_run(true);
run(passive) -> do_run(false).

do_run(Mode) ->
    spawn(fun() ->
        case zmq:socket(sub, [{active, Mode}, {subscribe, ""}]) of
        {ok, Socket} -> 
            zmq:connect(Socket, "tcp://127.0.0.1:5550"),
            loop(Socket, fun() -> recv(Socket, Mode) end);
        Other ->
            io:format("~p error creating socket: ~p\n", [self(), Other])
        end
    end). 

loop(Socket, F) ->
    case F() of
    {ok, {msg, 1}} ->
        io:format("~p subclient is done.\n", [self()]);
    {ok, {msg, N}} ->
        io:format("~p -> subclient received ~p\n", [self(), N]),
        loop(Socket, F);
    Other ->
        io:format("~p unexpected receive error: ~p\n", [self(), Other])
    end.

recv(_Socket, true) ->
    receive 
    {zmq, _S, Msg} when is_binary(Msg) -> 
        {ok, binary_to_term(Msg)};
    {zmq, _S, Other} ->
        Other
    end;
recv(Socket, false) ->
    case zmq:recv(Socket) of
    {ok, Msg} ->
        {ok, binary_to_term(Msg)};
    Other ->
        Other
    end.

