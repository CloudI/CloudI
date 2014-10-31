%%% -*- coding: latin-1 -*-
%%% ----------------------------------------------------------------------------
%%% Copyright (c) 2009, Erlang Training and Consulting Ltd.
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%%    * Redistributions of source code must retain the above copyright
%%%      notice, this list of conditions and the following disclaimer.
%%%    * Redistributions in binary form must reproduce the above copyright
%%%      notice, this list of conditions and the following disclaimer in the
%%%      documentation and/or other materials provided with the distribution.
%%%    * Neither the name of Erlang Training and Consulting Ltd. nor the
%%%      names of its contributors may be used to endorse or promote products
%%%      derived from this software without specific prior written permission.
%%% 
%%% THIS SOFTWARE IS PROVIDED BY Erlang Training and Consulting Ltd. ''AS IS''
%%% AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED. IN NO EVENT SHALL Erlang Training and Consulting Ltd. BE
%%% LIABLE SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
%%% BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR
%%% OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
%%% ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%% ----------------------------------------------------------------------------

%%% @author Oscar Hellström <oscar@hellstrom.st>
%%% @author Filipe David Manana <fdmanana@apache.org>
-module(lhttpc_manager_tests).

-include_lib("eunit/include/eunit.hrl").

-define(HOST, "www.example.com").
-define(SSL, false).

%%% Eunit setup stuff

start_app() ->
    application:start(public_key),
    ok = application:start(ssl),
    _ = application:load(lhttpc),
    ok = application:set_env(lhttpc, pool_size, 3),
    ok = application:start(lhttpc).

stop_app(_) ->
    ok = application:stop(lhttpc),
    ok = application:stop(ssl).

manager_test_() ->
    {inorder,
        {setup, fun start_app/0, fun stop_app/1, [
                ?_test(empty_manager()),
                ?_test(one_socket()),
                {timeout, 60, ?_test(connection_timeout())},
                {timeout, 60, ?_test(many_sockets())},
                {timeout, 60, ?_test(closed_race_cond())}
            ]}
    }.

%%% Tests

empty_manager() ->
    LS = socket_server:listen(),
    link(whereis(lhttpc_manager)), % want to make sure it doesn't crash
    ?assertEqual(0, lhttpc_manager:connection_count(lhttpc_manager)),

    Client = spawn_client(),
    ?assertEqual(ok, ping_client(Client)),

    ?assertEqual(no_socket, client_peek_socket(Client)),
    ?assertEqual(0, lhttpc_manager:connection_count(lhttpc_manager)),

    ?assertEqual(ok, stop_client(Client)),
    ?assertEqual(0, lhttpc_manager:connection_count(lhttpc_manager)),

    catch gen_tcp:close(LS),
    unlink(whereis(lhttpc_manager)),
    ok.

one_socket() ->
    LS = socket_server:listen(),
    link(whereis(lhttpc_manager)), % want to make sure it doesn't crash
    ?assertEqual(0, lhttpc_manager:connection_count(lhttpc_manager)),

    Client1 = spawn_client(),
    ?assertEqual(ok, ping_client(Client1)),
    Client2 = spawn_client(),
    ?assertEqual(ok, ping_client(Client2)),

    ?assertEqual(0, lhttpc_manager:connection_count(lhttpc_manager)),
    Result1 = connect_client(Client1),
    ?assertMatch({ok, _}, Result1),
    {ok, Socket} = Result1,
    ?assertEqual(ok, ping_client(Client1)),

    ?assertEqual(0, lhttpc_manager:connection_count(lhttpc_manager)),
    ?assertEqual(ok, disconnect_client(Client1)),
    ?assertEqual(ok, ping_client(Client1)),
    ?assertEqual(1, lhttpc_manager:connection_count(lhttpc_manager)),

    Result2 = connect_client(Client2),
    ?assertEqual({ok, Socket}, Result2),
    ?assertEqual(ok, ping_client(Client2)),
    ?assertEqual(0, lhttpc_manager:connection_count(lhttpc_manager)),

    ?assertEqual(ok, stop_client(Client1)),
    ?assertEqual(ok, stop_client(Client2)),
    catch gen_tcp:close(LS),
    unlink(whereis(lhttpc_manager)),
    ok.

connection_timeout() ->
    LS = socket_server:listen(),
    link(whereis(lhttpc_manager)), % want to make sure it doesn't crash
    ok = lhttpc_manager:update_connection_timeout(lhttpc_manager, 3000),
    erlang:yield(), % make sure lhttpc_manager processes the message
    ?assertEqual(0, lhttpc_manager:connection_count(lhttpc_manager)),

    Client = spawn_client(),
    ?assertEqual(ok, ping_client(Client)),

    Result1 = connect_client(Client),
    ?assertMatch({ok, _}, Result1),
    {ok, Socket} = Result1,
    ?assertEqual(ok, ping_client(Client)),
    ?assertEqual(0, lhttpc_manager:connection_count(lhttpc_manager)),

    ?assertEqual(ok, disconnect_client(Client)),
    ?assertEqual(ok, ping_client(Client)),
    ?assertEqual(1, lhttpc_manager:connection_count(lhttpc_manager)),

    % sleep a while and verify the socket was closed by lhttpc_manager
    ok = timer:sleep(3100),
    ?assertEqual(0, lhttpc_manager:connection_count(lhttpc_manager)),
    Result2 = connect_client(Client),
    ?assertMatch({ok, _}, Result2),
    {ok, Socket2} = Result2,
    ?assertEqual(ok, ping_client(Client)),
    ?assertEqual(0, lhttpc_manager:connection_count(lhttpc_manager)),
    ?assert(Socket2 =/= Socket),

    ?assertEqual(ok, disconnect_client(Client)),
    ?assertEqual(ok, ping_client(Client)),
    ?assertEqual(1, lhttpc_manager:connection_count(lhttpc_manager)),

    catch gen_tcp:close(LS),
    ?assertEqual(ok, stop_client(Client)),
    unlink(whereis(lhttpc_manager)),
    ok.

many_sockets() ->
    link(whereis(lhttpc_manager)), % want to make sure it doesn't crash
    LS = socket_server:listen(),
    Client1 = spawn_client(),
    Client2 = spawn_client(),
    Client3 = spawn_client(),
    ?assertEqual(ok, ping_client(Client1)),
    ?assertEqual(ok, ping_client(Client2)),
    ?assertEqual(ok, ping_client(Client3)),

    _Acceptor1 = socket_server:accept(LS),
    Result1 = connect_client(Client1),
    ?assertMatch({ok, _}, Result1),
    {ok, Socket1} = Result1,
    ?assertEqual(ok, ping_client(Client1)),

    ?assertEqual(ok, disconnect_client(Client1)),
    ?assertEqual(ok, ping_client(Client1)),
    ?assertEqual(1, lhttpc_manager:connection_count(lhttpc_manager)),

    Result2 = connect_client(Client2),
    ?assertMatch({ok, Socket1}, Result2),
    ?assertEqual(ok, ping_client(Client2)),
    ?assertEqual(0, lhttpc_manager:connection_count(lhttpc_manager)),

    ?assertEqual(ok, disconnect_client(Client2)),
    ?assertEqual(ok, ping_client(Client2)),
    ?assertEqual(1, lhttpc_manager:connection_count(lhttpc_manager)),

    lhttpc_manager ! {tcp_closed, Socket1},
    _Acceptor2 = socket_server:accept(LS),
    Result3 = connect_client(Client1),
    ?assertMatch({ok, _}, Result3),
    {ok, Socket2} = Result3,
    ?assertEqual(ok, ping_client(Client1)),
    ?assertNot(lists:member(Socket2, [Socket1])),

    Result4 = connect_client(Client2),
    ?assertMatch({ok, _}, Result4),
    {ok, Socket3} = Result4,
    ?assertEqual(ok, ping_client(Client2)),
    ?assertNot(lists:member(Socket3, [Socket1, Socket2])),

    Result5 = connect_client(Client3),
    ?assertMatch({ok, _}, Result5),
    {ok, Socket4} = Result5,
    ?assertEqual(ok, ping_client(Client3)),
    ?assertNot(lists:member(Socket4, [Socket1, Socket2, Socket3])),

    Client4 = spawn_client(),
    ?assertEqual(ok, ping_client(Client4)),
    Result6 = connect_client(Client4),
    ?assertMatch(timeout, Result6),
    ?assertEqual(timeout, ping_client(Client4)),

    ?assertEqual(ok, disconnect_client(Client1)),
    ?assertEqual(ok, ping_client(Client1)),
    % 0 because the connection should be delivered to blocked client Client4
    ?assertEqual(0, lhttpc_manager:connection_count(lhttpc_manager)),

    Result7 = get_client_socket(Client4),
    ?assertMatch({ok, _}, Result7),
    {ok, Socket5} = Result7,
    ?assertEqual(ok, ping_client(Client4)),
    ?assertEqual(Socket2, Socket5),

    % If a blocked client dies, verify that the pool doesn't
    % send a socket to it.
    Client5 = spawn_client(),
    Client6 = spawn_client(),
    ?assertEqual(ok, ping_client(Client5)),
    ?assertEqual(ok, ping_client(Client6)),
    ?assertEqual(timeout, connect_client(Client5)),
    ?assertEqual(timeout, connect_client(Client6)),
    ?assertEqual(timeout, ping_client(Client5)),
    ?assertEqual(timeout, ping_client(Client6)),

    exit(Client5, kill),

    ?assertEqual(ok, disconnect_client(Client4)),
    ?assertEqual(ok, ping_client(Client4)),
    % 0 because the connection should be delivered to blocked client Client6
    ?assertEqual(0, lhttpc_manager:connection_count(lhttpc_manager)),

    Result8 = get_client_socket(Client6),
    ?assertMatch({ok, _}, Result8),
    {ok, Socket6} = Result8,
    ?assertEqual(ok, ping_client(Client6)),
    ?assertEqual(Socket6, Socket5),

    % If a client holding a socket dies, without returning it to the pool,
    % a blocked client will be unblocked
    Client7 = spawn_client(),
    ?assertEqual(ok, ping_client(Client7)),
    ?assertEqual(timeout, connect_client(Client7)),
    ?assertEqual(timeout, ping_client(Client7)),

    exit(Client6, kill),
    Result9 = get_client_socket(Client7),
    ?assertMatch({ok, _}, Result9),
    {ok, Socket7} = Result9,
    ?assertEqual(ok, ping_client(Client7)),
    ?assertNot(lists:member(
        Socket7, [Socket1, Socket2, Socket3, Socket4, Socket5, Socket6])),

    ?assertEqual(0, lhttpc_manager:connection_count(lhttpc_manager)),
    ?assertEqual(ok, disconnect_client(Client2)),
    ?assertEqual(1, lhttpc_manager:connection_count(lhttpc_manager)),
    ?assertEqual(ok, disconnect_client(Client3)),
    ?assertEqual(2, lhttpc_manager:connection_count(lhttpc_manager)),
    ?assertEqual(ok, disconnect_client(Client7)),
    ?assertEqual(3, lhttpc_manager:connection_count(lhttpc_manager)),

    lhttpc_manager ! {tcp_closed, Socket7},
    ?assertEqual(2, lhttpc_manager:connection_count(lhttpc_manager)),
    lhttpc_manager ! {tcp_closed, Socket4},
    ?assertEqual(1, lhttpc_manager:connection_count(lhttpc_manager)),
    lhttpc_manager ! {tcp_closed, Socket3},
    ?assertEqual(0, lhttpc_manager:connection_count(lhttpc_manager)),

    catch gen_tcp:close(LS),
    ?assertEqual(ok, stop_client(Client1)),
    ?assertEqual(ok, stop_client(Client2)),
    ?assertEqual(ok, stop_client(Client3)),
    ?assertEqual(ok, stop_client(Client4)),
    ?assertEqual(ok, stop_client(Client7)),
    unlink(whereis(lhttpc_manager)),
    ok.

closed_race_cond() ->
    LS = socket_server:listen(),
    link(whereis(lhttpc_manager)), % want to make sure it doesn't crash
    ?assertEqual(0, lhttpc_manager:connection_count(lhttpc_manager)),

    Client = spawn_client(),
    ?assertEqual(ok, ping_client(Client)),

    Result1 = connect_client(Client),
    ?assertMatch({ok, _}, Result1),
    {ok, Socket} = Result1,
    ?assertEqual(ok, ping_client(Client)),

    ?assertEqual(0, lhttpc_manager:connection_count(lhttpc_manager)),
    ?assertEqual(ok, disconnect_client(Client)),
    ?assertEqual(ok, ping_client(Client)),
    ?assertEqual(1, lhttpc_manager:connection_count(lhttpc_manager)),

    ManagerPid = whereis(lhttpc_manager),
    true = erlang:suspend_process(ManagerPid),

    Pid = self(),
    spawn_link(fun() ->
        Pid ! {result, client_peek_socket(Client)}
    end),

    erlang:yield(), % make sure that the spawned process has run
    gen_tcp:close(Socket), % a closed message should be sent to the manager
    true = erlang:resume_process(ManagerPid),

    Result2 = receive
        {result, R} -> R
        after 5000 -> erlang:error("Timeout receiving result from child process")
    end,

    ?assertMatch(no_socket, Result2),
    ?assertEqual(0, lhttpc_manager:connection_count(lhttpc_manager)),

    ?assertEqual(ok, stop_client(Client)),
    catch gen_tcp:close(LS),
    unlink(whereis(lhttpc_manager)),
    ok.

%%% Helpers functions

spawn_client() ->
    Parent = self(),
    spawn(fun() -> client_loop(Parent, nil) end).

client_loop(Parent, Socket) ->
    receive
        stop ->
            catch gen_tcp:close(Socket),
            Parent ! stopped;
        {get_socket, Ref} ->
            Parent ! {socket, Ref, Socket},
            client_loop(Parent, Socket);
        {ping, Ref} ->
            Parent ! {pong, Ref},
            client_loop(Parent, Socket);
        {peek_socket, Ref, To} ->
            Args = {socket, self(), ?HOST, get_port(), ?SSL},
            Result = gen_server:call(lhttpc_manager, Args, infinity),
            To ! {result, Ref, Result},
            client_loop(Parent, Socket);
        {connect, Ref} ->
            Args = {socket, self(), ?HOST, get_port(), ?SSL},
            NewSocket = case gen_server:call(lhttpc_manager, Args, infinity) of
                no_socket ->
                    socket_server:connect(get_port());
                {ok, S} ->
                    S
            end,
            Parent ! {connected, Ref, NewSocket},
            client_loop(Parent, NewSocket);
        {return_socket, Ref} ->
            gen_tcp:controlling_process(Socket, whereis(lhttpc_manager)),
            ok = gen_server:call(lhttpc_manager, {done, ?HOST, get_port(), ?SSL, Socket}),
            Parent ! {returned, Ref},
            client_loop(Parent, nil)
    end.

ping_client(Client) ->
    Ref = make_ref(),
    Client ! {ping, Ref},
    receive
        {pong, Ref} ->
            ok
    after 2000 ->
            timeout
    end.

connect_client(Client) ->
    Ref = make_ref(),
    Client ! {connect, Ref},
    receive
        {connected, Ref, Socket} ->
            {ok, Socket}
    after 2000 ->
            timeout
    end.

disconnect_client(Client) ->
    Ref = make_ref(),
    Client ! {return_socket, Ref},
    receive
        {returned, Ref} ->
            ok
    after 2000 ->
            timeout
    end.

get_client_socket(Client) ->
    Ref = make_ref(),
    Client ! {get_socket, Ref},
    receive
        {socket, Ref, Socket} ->
            {ok, Socket}
    after 2000 ->
            timeout
    end.

client_peek_socket(Client) ->
    Ref = make_ref(),
    Client ! {peek_socket, Ref, self()},
    receive
        {result, Ref, Result} ->
            Result
    after 5000 ->
            timeout
    end.

stop_client(Client) ->
    Client ! stop,
    receive
        stopped ->
            ok
    after 2000 ->
            timeout
    end.

get_port() ->
    {ok, P} = application:get_env(lhttpc, test_port),
    P.
