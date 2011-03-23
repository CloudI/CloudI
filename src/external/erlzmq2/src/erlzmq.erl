-module(erlzmq).
%% @headerfile "erlzmq.hrl"
-include_lib("erlzmq.hrl").
-export([context/0, context/1, socket/2, bind/2, connect/2, send/2, send/3,
         recv/1, recv/2, setsockopt/3, getsockopt/2, close/1, term/1, term/2]).
-export_type([erlzmq_socket/0, erlzmq_context/0]).

%% @equiv context(1)
%% @spec context() -> {ok, erlzmq_context()} | erlzmq_error()
-spec context() -> {ok, erlzmq_context()} | erlzmq_error().
context() ->
    context(1).

%% @doc Create a new erlzmq context with the specified number of io threads.
%% <br />
%% If the context can be created an 'ok' tuple containing an
%% {@type erlzmq_context()} handle to the created context is returned;
%% if not, it returns an 'error' tuple with an {@type erlzmq_type_error()}
%% describing the error.
%% <br />
%% The context must be later cleaned up calling {@link erlzmq:term/1. term/1}
%% <br />
%% <i>For more information see
%% <a href="http://api.zeromq.org/master:zmq-init">zmq_init</a></i>
%% @end
%% @spec context(pos_integer()) -> {ok, erlzmq_context()} | erlzmq_error()
-spec context(Threads :: pos_integer()) -> {ok, erlzmq_context()} | erlzmq_error().

context(Threads) when is_integer(Threads) ->
    erlzmq_nif:context(Threads).


%% @doc Create a socket.
%% <br />
%% This functions creates a socket of the given
%% {@link erlzmq_socket_type(). type} and associates it with the given
%% {@link erlzmq_context(). context}.
%% <br />
%% If the socket can be created an 'ok' tuple containing a
%% {@type erlzmq_socket()} handle to the created socket is returned;
%% if not, it returns an {@type erlzmq_error()} describing the error.<br />
%% <i>For more information see
%% <a href="http://api.zeromq.org/master:zmq_socket">zmq_socket</a>.</i>
%% @end
%% @spec socket(erlzmq_context(), erlzmq_socket_type()) -> {ok, erlzmq_socket()} | erlzmq_error()
-spec socket(Context :: erlzmq_context(), Type :: erlzmq_socket_type()) -> {ok, erlzmq_socket()} | erlzmq_error().

socket(Context, Type) ->
    erlzmq_nif:socket(Context, socket_type(Type)).

%% @doc Accept connections on a socket.
%% <br />
%% <i>For more information see
%% <a href="http://api.zeromq.org/master:zmq_bind">zmq_bind</a>.</i>
%% @end
%% @spec bind(erlzmq_socket(), erlzmq_endpoint()) -> ok | erlzmq_error()
-spec bind(Socket :: erlzmq_socket(), Endpoint :: erlzmq_endpoint()) -> ok | erlzmq_error().

bind(Socket, Endpoint) ->
    erlzmq_result(erlzmq_nif:bind(Socket, Endpoint)).

%% @doc Connect a socket.
%% <br />
%% <i>For more information see
%% <a href="http://api.zeromq.org/master:zmq_connect">zmq_connect</a>.</i>
%% @end
%% @spec connect(erlzmq_socket(), erlzmq_endpoint()) -> ok | erlzmq_error()
-spec connect(Socket :: erlzmq_socket(), Endpoint :: erlzmq_endpoint()) -> ok | erlzmq_error().

connect(Socket, Endpoint) ->
    erlzmq_result(erlzmq_nif:connect(Socket, Endpoint)).

%% @equiv send(Socket, Msg, [])
%% @spec send(erlzmq_socket(), erlzmq_data()) -> ok | erlzmq_error()
-spec send(Socket :: erlzmq_socket(), Data :: erlzmq_data()) -> ok | erlzmq_error().

send(Socket, Binary) ->
    send(Socket, Binary, []).

%% @doc Send a message on a socket.
%% <br />
%% <i>For more information see
%% <a href="http://api.zeromq.org/master:zmq_send">zmq_send</a>.</i>
%% @end
%% @spec send(ezma_socket(), erlzmq_data(), erlzmq_send_recv_flags()) -> ok | erlzmq_error()
-spec send(Socket :: erlzmq_socket(), Data :: erlzmq_data(), Flags :: erlzmq_send_recv_flags()) -> ok | erlzmq_error().

send(Socket, Binary, Flags) when is_list(Flags) ->
    case erlzmq_nif:send(Socket, Binary, sendrecv_flags(Flags)) of
        Ref when is_reference(Ref) ->
            receive
                {Ref, ok} ->
                    ok;
                {Ref, error, Error} ->
                    {error, Error}
            end;
        Result ->
            erlzmq_result(Result)
    end.

%% @equiv recv(Socket, 0)
%% @spec recv(erlzmq_socket()) -> {ok, erlzmq_data()} | erlzmq_error()
-spec recv(Socket :: erlzmq_socket()) -> {ok, erlzmq_data()} | erlzmq_error().

recv(Socket) ->
    recv(Socket, []).

%% @doc Receive a message from a socket.
%% <br />
%% <i>For more information see
%% <a href="http://api.zeromq.org/master:zmq_recv">zmq_recv</a>.</i>
%% @end
%% @spec recv(erlzmq_socket(), erlzmq_send_recv_flags()) -> {ok, erlzmq_data()} | erlzmq_error()
-spec recv(Socket :: erlzmq_socket(), Flags :: erlzmq_send_recv_flags()) -> {ok, erlzmq_data()} | erlzmq_error() | {error, timeout, reference()}.

recv(Socket, Flags) when is_list(Flags) ->
    case erlzmq_nif:recv(Socket, sendrecv_flags(Flags)) of
        Ref when is_reference(Ref) ->
            Timeout = proplists:get_value(timeout, Flags, infinity),
            receive
                {Ref, Result} ->
                    {ok, Result}
            after Timeout ->
                    {error, timeout, Ref}
            end;
        Result ->
            erlzmq_result(Result)
    end.

%% @doc Set an {@link erlzmq_sockopt(). option} associated with a socket.
%% <br />
%% <i>For more information see
%% <a href="http://api.zeromq.org/master:zmq_setsockopt">zmq_setsockopt</a>.</i>
%% @end
%% @spec setsockopt(erlzmq_socket(), erlzmq_sockopt(), erlzmq_sockopt_value()) -> ok | erlzmq_error()
-spec setsockopt(Socket :: erlzmq_socket(), Name :: erlzmq_sockopt(), erlzmq_sockopt_value()) -> ok | erlzmq_error().

setsockopt(Socket, Name, Value) ->
    erlzmq_result(erlzmq_nif:setsockopt(Socket, option_name(Name), Value)).

%% @doc Get an {@link erlzmq_sockopt(). option} associated with a socket.
%% <br />
%% <i>For more information see
%% <a href="http://api.zeromq.org/master:zmq_getsockopt">zmq_getsockopt</a>.</i>
%% @end
%% @spec getsockopt(erlzmq_socket(), erlzmq_sockopt()) -> {ok, erlzmq_sockopt_value()} | erlzmq_error()
-spec getsockopt(Socket :: erlzmq_socket(), Name :: erlzmq_sockopt()) -> {ok, erlzmq_sockopt_value()} | erlzmq_error().

getsockopt(Socket, Name) ->
    erlzmq_result(erlzmq_nif:getsockopt(Socket, option_name(Name))).


%% @doc Close the given socket.
%% <br />
%% <i>For more information see
%% <a href="http://api.zeromq.org/master:zmq_close">zmq_close</a>.</i>
%% @end
%% @spec close(erlzmq_socket()) -> ok | erlzmq_error()
-spec close(Socket :: erlzmq_socket()) -> ok | erlzmq_error().

close(Socket) ->
    erlzmq_result(erlzmq_nif:close(Socket)).

%% @equiv term(Context, infinity)
%% @spec term(erlzmq_context()) -> ok | erlzmq_error()
-spec term(Context :: erlzmq_context()) -> ok | erlzmq_error().

term(Context) ->
    term(Context, infinity).


%% @doc Terminate the given context waiting up to Timeout ms.
%% <br />
%% This function should be called after all sockets associated with
%% the given context have been closed.<br />
%% If not it will block the given Timeout amount of time.
%% <i>For more information see
%% <a href="http://api.zeromq.org/master:zmq_term">zmq_term</a>.</i>
%% @end
%% @spec term(erlzmq_context(), timeout()) -> ok | erlzmq_error()
-spec term(Context :: erlzmq_context(), Timeout :: timeout()) -> ok | erlzmq_error() | {error, timeout, reference()}.

term(Context, Timeout) ->
    case erlzmq_nif:term(Context) of
        Ref when is_reference(Ref) ->
            receive
                {Ref, Result} ->
                    Result
            after Timeout ->
                    {error, timeout, Ref}
            end;
        Result ->
            erlzmq_result(Result)
    end.


%% Private

-spec socket_type(Type :: erlzmq_socket_type()) -> integer().

socket_type(pair) ->
    ?'ZMQ_PAIR';
socket_type(pub) ->
    ?'ZMQ_PUB';
socket_type(sub) ->
    ?'ZMQ_SUB';
socket_type(req) ->
    ?'ZMQ_REQ';
socket_type(rep) ->
    ?'ZMQ_REP';
socket_type(xreq) ->
    ?'ZMQ_XREQ';
socket_type(xrep) ->
    ?'ZMQ_XREP';
socket_type(pull) ->
    ?'ZMQ_PULL';
socket_type(push) ->
    ?'ZMQ_PUSH';
socket_type(xpub) ->
    ?'ZMQ_XPUB';
socket_type(xsub) ->
    ?'ZMQ_XSUB'.

-spec sendrecv_flags(Flags :: erlzmq_send_recv_flags()) -> integer().

sendrecv_flags([]) ->
    0;
sendrecv_flags([{timeout,_}]) ->
    0;
sendrecv_flags([noblock|Rest]) ->
    ?'ZMQ_NOBLOCK' bor sendrecv_flags(Rest);
sendrecv_flags([sndmore|Rest]) ->
    ?'ZMQ_SNDMORE' bor sendrecv_flags(Rest).

-spec option_name(Name :: erlzmq_sockopt()) -> integer().

option_name(hwm) ->
    ?'ZMQ_HWM';
option_name(swap) ->
    ?'ZMQ_SWAP';
option_name(affinity) ->
    ?'ZMQ_AFFINITY';
option_name(identity) ->
    ?'ZMQ_IDENTITY';
option_name(subscribe) ->
    ?'ZMQ_SUBSCRIBE';
option_name(unsubscribe) ->
    ?'ZMQ_UNSUBSCRIBE';
option_name(rate) ->
    ?'ZMQ_RATE';
option_name(recovery_ivl) ->
    ?'ZMQ_RECOVERY_IVL';
option_name(mcast_loop) ->
    ?'ZMQ_MCAST_LOOP';
option_name(sndbuf) ->
    ?'ZMQ_SNDBUF';
option_name(rcvbuf) ->
    ?'ZMQ_RCVBUF';
option_name(rcvmore) ->
    ?'ZMQ_RCVMORE';
option_name(fd) ->
    ?'ZMQ_FD';
option_name(events) ->
    ?'ZMQ_EVENTS';
option_name(linger) ->
    ?'ZMQ_LINGER';
option_name(reconnect_ivl) ->
    ?'ZMQ_RECONNECT_IVL';
option_name(backlog) ->
    ?'ZMQ_BACKLOG';
option_name(recovery_ivl_msec) ->
    ?'ZMQ_RECOVERY_IVL_MSEC';
option_name(reconnect_ivl_max) ->
    ?'ZMQ_RECONNECT_IVL_MAX'.


-spec erlzmq_result(ok) -> ok;
                 ({ok, Value :: term()}) -> Value :: term();
                 ({error, Value :: atom()}) -> Value :: atom();
                 ({error, integer()}) -> {error, erlzmq_error_type()};
                 ({error, erlzmq, integer()}) -> {error, erlzmq_error_type()}.

erlzmq_result(ok) ->
    ok;
erlzmq_result({ok, _} = Result) ->
    Result;
erlzmq_result({error, Code} = Error) when is_atom(Code) ->
    Error;
erlzmq_result({error, Code}) when is_integer(Code) andalso Code > 156384712 ->
    erlzmq_result({error, erlzmq, Code - 156384712});
erlzmq_result({error, erlzmq, 1}) ->
    {error, enotsup};
erlzmq_result({error, erlzmq, 2}) ->
    {error, eprotonosupport};
erlzmq_result({error, erlzmq, 3}) ->
    {error, enobufs};
erlzmq_result({error, erlzmq, 4}) ->
    {error, enetdown};
erlzmq_result({error, erlzmq, 5}) ->
    {error, eaddrinuse};
erlzmq_result({error, erlzmq, 6}) ->
    {error, eaddrnotavail};
erlzmq_result({error, erlzmq, 7}) ->
    {error, econnrefused};
erlzmq_result({error, erlzmq, 8}) ->
    {error, einprogress};
erlzmq_result({error, erlzmq, 51}) ->
    {error, efsm};
erlzmq_result({error, erlzmq, 52}) ->
    {error, enocompatproto};
erlzmq_result({error, erlzmq, 53}) ->
    {error, eterm};
erlzmq_result({error, erlzmq, 54}) ->
    {error, emthread};

%% errno
erlzmq_result({error, 1}) ->
    {error, eperm};
erlzmq_result({error, 2}) ->
    {error, enoent};
erlzmq_result({error, 3}) ->
    {error, esrch};
erlzmq_result({error, 4}) ->
    {error, eintr};
erlzmq_result({error, 5}) ->
    {error, eio};
erlzmq_result({error, 7}) ->
    {error, enxio};
erlzmq_result({error, 8}) ->
    {error, eperm};
erlzmq_result({error, 9}) ->
    {error, ebadf};
erlzmq_result({error, 10}) ->
    {error, echild};
erlzmq_result({error, 11}) ->
    {error, edeadlk};
erlzmq_result({error, 12}) ->
    {error, enomem};
erlzmq_result({error, 13}) ->
    {error, eacces};
erlzmq_result({error, 14}) ->
    {error, efault};
erlzmq_result({error, 15}) ->
    {error, enotblk};
erlzmq_result({error, 16}) ->
    {error, ebusy};
erlzmq_result({error, 17}) ->
    {error, eexist};
erlzmq_result({error, 18}) ->
    {error, exdev};
erlzmq_result({error, 19}) ->
    {error, enodev};
erlzmq_result({error, 20}) ->
    {error, enotdir};
erlzmq_result({error, 21}) ->
    {error, eisdir};
erlzmq_result({error, 22}) ->
    {error, einval};
erlzmq_result({error, 23}) ->
    {error, enfile};
erlzmq_result({error, 24}) ->
    {error, emfile};
erlzmq_result({error, 25}) ->
    {error, enotty};
erlzmq_result({error, 26}) ->
    {error, etxtbsy};
erlzmq_result({error, 27}) ->
    {error, efbig};
erlzmq_result({error, 28}) ->
    {error, enospc};
erlzmq_result({error, 29}) ->
    {error, espipe};
erlzmq_result({error, 30}) ->
    {error, erofs};
erlzmq_result({error, 31}) ->
    {error, emlink};
erlzmq_result({error, 32}) ->
    {error, epipe};
erlzmq_result({error, 35}) ->
    {error, eagain};
erlzmq_result({error, 36}) ->
    {error, einprogress};
erlzmq_result({error, 37}) ->
    {error, ealready};
erlzmq_result({error, 38}) ->
    {error, enotsock};
erlzmq_result({error, 39}) ->
    {error, edestaddrreq};
erlzmq_result({error, 40}) ->
    {error, emsgsize};
erlzmq_result({error, 41}) ->
    {error, eprototype};
erlzmq_result({error, 42}) ->
    {error, enoprotoopt};
erlzmq_result({error, 43}) ->
    {error, eprotonosupport};
erlzmq_result({error, 44}) ->
    {error, esocktnosupport};
erlzmq_result({error, 45}) ->
    {error, enotsup};
erlzmq_result({error, 46}) ->
    {error, epfnosupport};
erlzmq_result({error, 47}) ->
    {error, eafnosupport};
erlzmq_result({error, 48}) ->
    {error, eaddrinuse};
erlzmq_result({error, 49}) ->
    {error, eaddrnotavail};
erlzmq_result({error, 50}) ->
    {error, enetdown};
erlzmq_result({error, 51}) ->
    {error, enetunreach};
erlzmq_result({error, 52}) ->
    {error, enetreset};
erlzmq_result({error, 53}) ->
    {error, econnaborted};
erlzmq_result({error, 54}) ->
    {error, econnreset};
erlzmq_result({error, 55}) ->
    {error, enobufs};
erlzmq_result({error, 56}) ->
    {error, eisconn};
erlzmq_result({error, 57}) ->
    {error, enotconn};
erlzmq_result({error, 58}) ->
    {error, eshutdown};
erlzmq_result({error, 59}) ->
    {error, etoomanyrefs};
erlzmq_result({error, 60}) ->
    {error, etimedout};
erlzmq_result({error, 61}) ->
    {error, econnrefused};
erlzmq_result({error, 62}) ->
    {error, eloop};
erlzmq_result({error, 63}) ->
    {error, enametoolong};

erlzmq_result({error, N}) ->
    {error, {unknown, N}}.
