%%%-------------------------------------------------------------------
%%% File: $Id$
%%%-------------------------------------------------------------------
%%% @doc Erlang bindings for ZeroMQ.
%%%
%%% @author Dhammika Pathirana <dhammika at gmail dot com>
%%% @author Serge Aleynikov <saleyn at gmail dot com>.
%%% @copyright 2010 Dhammika Pathirana and Serge Aleynikov
%%% @version {@version}
%%% @end
%%%-------------------------------------------------------------------
%%% @type zmq_socket().  Opaque 0MQ socket type.
%%% @type zmq_sockopt() = {hwm, integer()}
%%%                     | {lwm, integer()}
%%%                     | {swap, integer()}
%%%                     | {affinity, integer()}
%%%                     | {identity, string()}
%%%                     | {subscribe, string()}
%%%                     | {unsubscibe, string()}
%%%                     | {rate, integer()}
%%%                     | {recovery_ivl, integer()}
%%%                     | {mcast_loop, boolean()}
%%%                     | {sndbuf, integer()}
%%%                     | {rcvbuf, integer()}
%%%                     | {rcvmore, boolean()}
%%%                     | {active, boolean()}.     
%%%           0MQ socket options. See 0MQ man pages for details.
%%%           One additional options `active' indicates to the driver
%%%           that incoming messages must be automatically delivered 
%%%           to the process owner's mailbox instead of explicitely
%%%           requiring recv/1 call.
%%% @end
%%% @type zmq_sendopt() = sndmore.
%%%           Send options. See 0MQ man pages for details.
%%% @end
%%%-------------------------------------------------------------------
-module(zmq).
-author("dhammika@gmail.com").
-author("saleyn@gmail.com").
-id("$Id$").

-behaviour(gen_server).

%% ZMQ API
-export([start_link/0, start_link/1,
         socket/1, socket/2, close/1, setsockopt/2, getsockopt/2,
         bind/2, connect/2, send/2, send/3, recv/1, format_error/1]).

-export([port/0]).

%% gen_server callbacks.
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("zmq.hrl").

-record(state, {port}).

%%%===================================================================
%%% ZMQ API
%%%===================================================================

%%--------------------------------------------------------------------
%% @equiv start_link(1)
%% @end
%%--------------------------------------------------------------------
start_link() ->
    start_link(1).

%%--------------------------------------------------------------------
%% @doc Start the server.
%% @spec (IoThreads) -> {ok, Pid} | {error, Error} | ignore
%% @end
%%--------------------------------------------------------------------
start_link(IoThreads) when is_integer(IoThreads) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [IoThreads], []).

socket(Type) when is_atom(Type) ->
    socket(Type, []).

%%--------------------------------------------------------------------
%% @doc Create a 0MQ socket.
%% @spec (Type, Options) -> {ok, Socket::zmq_socket()} | {error, Reason}
%%          Type = pair | pub | sub | req | rep | 
%%                 xreq | xrep | upstream | downstream
%%          Options = [Option]
%%          Option  = {active, boolean()}
%%                  | {zmq_sockopt(), Value}
%% @end
%%--------------------------------------------------------------------
socket(Type, Options) when is_atom(Type), is_list(Options) ->
%    gen_server:call(?MODULE, {socket, Type, Options}).
    % We are using direct call to the driver to create the socket,
    % because we need the driver to know the socket owner's pid, so
    % that it can deliver messages to its mailbox in the passive mode
    try gen_server:call(?MODULE, port) of
    Port when is_port(Port) ->
        [check_sockopt({O, V}) || {O,V} <- Options],
        Msg     = encode_msg_socket(Type),
        {ok, S} = driver(Port, Msg),
        case driver(Port, encode_sock_opts(S, Options)) of
        ok -> 
            {ok, {Port, S}};
        {error, Why} ->
            driver(Port, encode_close(S)),
            throw(Why)
        end
    catch _:Error ->
        {error, Error}
    end.

%%--------------------------------------------------------------------
%% @doc Close a 0MQ socket.
%% @spec (Socket::zmq_socket()) -> ok | {error, Reason}
%% @end
%%--------------------------------------------------------------------
close({Port, Socket}) when is_integer(Socket) ->
    Msg = encode_close(Socket),
    driver(Port, Msg);
close(Socket) when is_integer(Socket) ->
    gen_server:call(?MODULE, {close, Socket}).

%%--------------------------------------------------------------------
%% @doc Set socket options.
%% @spec (Socket::zmq_socket(), Options) -> ok | {error, Reason}
%%          Options = [{zmq_sockopt(), Value}]
%% @end
%%--------------------------------------------------------------------
setsockopt(Socket, Opts) when is_integer(Socket), is_list(Opts) ->
    gen_server:call(?MODULE, {setsockopt, Socket, Opts}).

%%--------------------------------------------------------------------
%% @doc Get socket option.
%% @spec (Socket::zmq_socket(), Option) -> {ok, Value} | {error, Reason}
%%          Option = zmq_sockopt()
%% @end
%%--------------------------------------------------------------------
getsockopt(Socket, Option) when is_integer(Socket), is_atom(Option) ->
    gen_server:call(?MODULE, {getsockopt, Socket, Option});
% Experimantal support of direct port communication
getsockopt({Port, S}, Option) when is_atom(Option)->
    Msg = encode_getsockopt(S, Option),
    driver(Port, Msg).

%%--------------------------------------------------------------------
%% @doc Bind a 0MQ socket to address.
%% @spec (Socket::zmq_socket(), Address) -> ok | {error, Reason}
%%          Address = string() | binary()
%% @end
%%--------------------------------------------------------------------
bind(Socket, Address) when is_integer(Socket), is_list(Address) ->
    bind(Socket, list_to_binary(Address));
bind(Socket, Address) when is_integer(Socket), is_binary(Address) ->
    gen_server:call(?MODULE, {bind, Socket, Address});
bind({Port, S}, Address) when is_integer(S), is_list(Address) ->
    bind({Port, S}, list_to_binary(Address));
bind({Port, S}, Address) when is_integer(S), is_binary(Address) ->
    Msg = encode_bind(S, Address),
    driver(Port, Msg).

%%--------------------------------------------------------------------
%% @doc Connect a 0MQ socket to address.
%% @spec (Socket::zmq_socket(), Address) -> ok | {error, Reason}
%%          Address = string() | binary()
%% @end
%%--------------------------------------------------------------------
connect(Socket, Address) when is_integer(Socket), is_list(Address) ->
    connect(Socket, list_to_binary(Address));
connect(Socket, Address) when is_integer(Socket), is_binary(Address) ->
    gen_server:call(?MODULE, {connect, Socket, Address});
% Experimantal support of direct port communication
connect({Port, S}, Address) when is_list(Address)->
    connect({Port, S}, list_to_binary(Address));
connect({Port, S}, Address) when is_binary(Address) ->
    Msg = encode_connect(S, Address),
    driver(Port, Msg).

%%--------------------------------------------------------------------
%% @equiv send(S, Msg, [])
%% @end
%%--------------------------------------------------------------------
send(Socket, Data) ->
    send(Socket, Data, []).

%%--------------------------------------------------------------------
%% @doc Send a message to a given 0MQ socket.
%% @spec (Socket::zmq_socket(), Msg::binary(), Flags) -> ok | {error, Reason}
%%          Flags = [zmq_sendopt()]
%% @end
%%--------------------------------------------------------------------
send(Socket, Data, Flags)
        when is_integer(Socket), is_binary(Data), is_list(Flags) ->
    gen_server:call(?MODULE, {send, Socket, Data, Flags});
% Experimantal support of direct port communication
send({Port, S}, Data, Flags) ->
    Msg = encode_msg_send(S, Data, Flags),
    driver(Port, Msg).

%%--------------------------------------------------------------------
%% @doc Receive a message from a given 0MQ socket.
%% @spec (Socket::zmq_socket()) -> {ok, binary()} | {error, Reason}
%% @end
%%--------------------------------------------------------------------
recv(Socket) when is_integer(Socket) ->
    gen_server:call(?MODULE, {recv, Socket});
% Experimantal support of direct port communication
recv({Port, S}) ->
    Msg = encode_msg_recv(S),
    driver(Port, Msg).

%% Experimental functions for direct communications with port 
%% bypassing serialization through ?MODULE server.

port() ->
    gen_server:call(?MODULE, port).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Handle start.
%%
%% @spec init(Args) ->
%%          {ok, State} |
%%          {ok, State, Timeout} |
%%          {ok, State, hibernate} |
%%          {stop, Reason} |
%%          ignore
%% @end
%%--------------------------------------------------------------------
init([IoThreads]) ->
    process_flag(trap_exit, true),
    DirName = re:replace(filename:dirname(code:which(?MODULE)), 
                         "/?[^/]+/\\.\\.", "", [{return,list}]),
    SearchDir = filename:join(filename:dirname(DirName), "priv"),
    ?log("init, lib path: ~s", [SearchDir]),
    try erl_ddll:load(SearchDir, ?DRIVER_NAME) of
    ok ->
        Port = open_port({spawn_driver, ?DRIVER_NAME}, [binary]),
        init_context(Port, IoThreads),
        {ok, #state{port=Port}};
    {error, Reason} ->
        throw(erl_ddll:format_error(Reason))
    catch _:Error ->
        {stop, Error}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc Handle synchronous call.
%%
%% @spec handle_call(Request, From, State) ->
%%          {reply, Reply, NewState} |
%%          {reply, Reply, NewState, Timeout} |
%%          {reply, Reply, NewState, hibernate} |
%%          {noreply, NewState} |
%%          {noreply, NewState, Timeout} |
%%          {noreply, NewState, hibernate} |
%%          {stop, Reason, Reply, NewState} |
%%          {stop, Reason, NewState}
%% @end
%%-------------------------------------------------------------------

% No need to support context termination - context allocation
% is handled on port creation, and the resource will be 
% automatically reclaimed upon death of the port driver.  
%handle_call({term}, _From, State) ->
%    ?log("~p", [term]),
%    Message = <<(?ZMQ_TERM):8>>,
%    Reply = driver(State#state.port, Message),
%    {reply, Reply, State};

handle_call({socket, Type, Options}, _From, #state{port=Port} = State) ->
    ?log("~p, type:~s options:~p", [socket, Type, Options]),
    try
        [check_sockopt({O, V}) || {O,V} <- Options],
        Msg     = encode_msg_socket(Type),
        {ok, S} = driver(Port, Msg),
        case driver(Port, encode_sock_opts(S, Options)) of
        ok -> 
            ok;
        {error, Why} ->
            driver(Port, encode_close(S)),
            throw(Why)
        end,
        {reply, {ok, S}, State}
    catch _:Error ->
        {reply, {error, Error}, State}
    end;

handle_call({close, Socket}, _From, State) ->
    ?log("~p", [close]),
    do_call(State, encode_close(Socket));

handle_call({setsockopt, Socket, Options}, _From, State) ->
    ?log("~p", [socketopt]),
    do_call(State, encode_sock_opts(Socket, Options));

handle_call({getsockopt, Socket, Option}, _From, State) ->
    ?log("~p", [getsockopt]),
    do_call(State, encode_getsockopt(Socket, Option));

handle_call({bind, Socket, Address}, _From, State) ->
    ?log("~p addr:~s", [bind, binary_to_list(Address)]),
    do_call(State, encode_bind(Socket, Address));

handle_call({connect, Socket, Address}, _From, State) ->
    ?log("~p addr:~s", [connect, binary_to_list(Address)]),
    do_call(State, encode_connect(Socket, Address));

handle_call({send, Socket, Data, Flags}, _From, State) ->
    ?log("~p", [send]),
    do_call(State, encode_msg_send(Socket, Data, Flags));

handle_call({recv, Socket}, _From, State) ->
    ?log("~p", [recv]),
    do_call(State, encode_msg_recv(Socket));

handle_call(port, _From, #state{port = Port} = State) ->
    {reply, Port, State};

handle_call(Request, _From, State) ->
    {stop, {unknown_call, Request}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handle asynchronous call.
%%
%% @spec handle_cast(Msg, State) ->
%%          {noreply, NewState} |
%%          {noreply, NewState, Timeout} |
%%          {noreply, NewState, hibernate} |
%%          {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_cast(Msg, State) ->
    {stop, {unknown_cast, Msg}, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handle message.
%%
%% @spec handle_info(Info, State) ->
%%          {noreply, NewState} |
%%          {noreply, NewState, Timeout} |
%%          {noreply, NewState, hibernate} |
%%          {stop, Reason, NewState}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    ?log("unhandled message: ~p\n", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc Handle termination/shutdown.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    port_close(State#state.port),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc Handle code change.
%%
%% @spec code_change(OldVsn, State, Extra) -> 
%%          {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%% @doc Format error atom returned by the driver.
%% @spec (Code::atom()) -> string()
%% @end
%%--------------------------------------------------------------------
format_error(enotsup)           -> "Not supported";
format_error(eprotonosupport)   -> "Protocol not supported";
format_error(enobufs)           -> "No buffer space available";
format_error(enetdown)          -> "Network is down";
format_error(eaddrinuse)        -> "Address in use";
format_error(eaddrnotavail)     -> "Address not available";
format_error(efsm)              -> "Operation cannot be accomplished in current state";
format_error(enocompatproto)    -> "The protocol is not compatible with the socket type";
format_error(E) when is_atom(E) -> inet:format_error(E);
format_error(E) when is_list(E) -> E;
format_error(E) when is_tuple(E)-> io_lib:format("~p", [E]).

%%%===================================================================
%%% Internal functions
%%%===================================================================

init_context(Port, IoThreads) ->
    ?log("~p, io threads:~B", [init, IoThreads]),
    Message = <<(?ZMQ_INIT):8, IoThreads:32>>,
    case driver(Port, Message) of
        ok              -> ok;
        {error, Error}  -> throw(format_error(Error))
    end.

encode_msg_socket(Type) ->
    case Type of
        pair        -> <<(?ZMQ_SOCKET):8, (?ZMQ_PAIR):8>>;
        pub         -> <<(?ZMQ_SOCKET):8, (?ZMQ_PUB):8>>;
        push        -> <<(?ZMQ_SOCKET):8, (?ZMQ_PUSH):8>>;
        pull        -> <<(?ZMQ_SOCKET):8, (?ZMQ_PULL):8>>;
        sub         -> <<(?ZMQ_SOCKET):8, (?ZMQ_SUB):8>>;
        req         -> <<(?ZMQ_SOCKET):8, (?ZMQ_REQ):8>>;
        rep         -> <<(?ZMQ_SOCKET):8, (?ZMQ_REP):8>>;
        xreq        -> <<(?ZMQ_SOCKET):8, (?ZMQ_XREQ):8>>;
        xrep        -> <<(?ZMQ_SOCKET):8, (?ZMQ_XREP):8>>;
        upstream    -> <<(?ZMQ_SOCKET):8, (?ZMQ_UPSTREAM):8>>;
        downstream  -> <<(?ZMQ_SOCKET):8, (?ZMQ_DOWNSTREAM):8>>;
        _           -> throw({unknown_sock_type, Type})
    end.

encode_close(Socket) ->
    <<(?ZMQ_CLOSE):8, Socket:32>>.

encode_sock_opts(Socket, Options) when length(Options) =< 255 ->
    Opts = lists:map(fun({O, Value}) ->
        V = check_sockopt({O, Value}),
        case O of 
            hwm         -> <<?ZMQ_HWM,          8, V:64/native>>;
            swap        -> <<?ZMQ_SWAP,         8, V:64/native>>;
            affinity    -> <<?ZMQ_AFFINITY,     8, V:64/native>>;
            identity    -> <<?ZMQ_IDENTITY,     (byte_size(V)):8, V/binary>>;
            subscribe   -> <<?ZMQ_SUBSCRIBE,    (byte_size(V)):8, V/binary>>;
            unsubscribe -> <<?ZMQ_UNSUBSCRIBE,  (byte_size(V)):8, V/binary>>;
            rate        -> <<?ZMQ_RATE,         8, V:64/native>>;
            recovery_ivl-> <<?ZMQ_RECOVERY_IVL, 8, V:64/native>>;
            mcast_loop  -> <<?ZMQ_MCAST_LOOP,   8, V:64/native>>;
            sndbuf      -> <<?ZMQ_SNDBUF,       8, V:64/native>>;
            rcvbuf      -> <<?ZMQ_RCVBUF,       8, V:64/native>>;
            % Driver's socket options
            active      -> <<?ZMQ_ACTIVE,       1, V>>;
            _           -> throw({unknown_sock_option, O})
        end
    end, Options),
    <<(?ZMQ_SETSOCKOPT):8, Socket:32, (length(Opts)):8, (list_to_binary(Opts))/binary>>.

check_sockopt({hwm,           V}) when is_integer(V) -> V;
check_sockopt({swap,          V}) when is_integer(V) -> V;
check_sockopt({affinity,      V}) when is_integer(V) -> V;
check_sockopt({identity,      V}) when is_list(V),   length(V)    =< 255 -> list_to_binary(V);
check_sockopt({identity,      V}) when is_binary(V), byte_size(V) =< 255 -> V;
% Note that 0MQ doesn't limit the size of subscribe/unsubscribe options,
% but we do for simplicity.
check_sockopt({subscribe,     V}) when is_list(V),   length(V)    =< 255 -> list_to_binary(V);
check_sockopt({subscribe,     V}) when is_binary(V), byte_size(V) =< 255 -> V;
check_sockopt({unsubscribe,   V}) when is_list(V),   length(V)    =< 255 -> list_to_binary(V);
check_sockopt({unsubscribe,   V}) when is_binary(V), byte_size(V) =< 255 -> V;
check_sockopt({rate,          V}) when is_integer(V) -> V;
check_sockopt({recovery_ivl,  V}) when is_integer(V) -> V;
check_sockopt({mcast_loop, true})                    -> 1;
check_sockopt({mcast_loop,false})                    -> 0;
check_sockopt({sndbuf,        V}) when is_integer(V) -> V;
check_sockopt({rcvbuf,        V}) when is_integer(V) -> V;
check_sockopt({active,     true})                    -> 1;
check_sockopt({active,    false})                    -> 0;
check_sockopt(Option) -> throw({unknown_option, Option}).

sockopt_to_int(Option) ->
    case Option of 
        hwm         -> ?ZMQ_HWM;
        swap        -> ?ZMQ_SWAP;
        affinity    -> ?ZMQ_AFFINITY;
        identity    -> ?ZMQ_IDENTITY;
        subscribe   -> ?ZMQ_SUBSCRIBE;
        unsubscribe -> ?ZMQ_UNSUBSCRIBE;
        rate        -> ?ZMQ_RATE;
        recovery_ivl-> ?ZMQ_RECOVERY_IVL;
        mcast_loop  -> ?ZMQ_MCAST_LOOP;
        sndbuf      -> ?ZMQ_SNDBUF;
        rcvbuf      -> ?ZMQ_RCVBUF;
        rcvmore     -> ?ZMQ_RCVMORE;
        _           -> throw({unknown_sock_option, Option})
    end.

encode_getsockopt(Socket, Option) ->
    O = sockopt_to_int(Option),
    <<(?ZMQ_GETSOCKOPT):8, Socket:32, O:32>>.

encode_bind(Socket, Address) ->
    <<(?ZMQ_BIND):8, Socket:32, Address/binary>>.
encode_connect(Socket, Address) ->
    <<(?ZMQ_CONNECT):8, Socket:32, Address/binary>>.

send_flags_to_int([]) -> 0;
send_flags_to_int([H|T]) ->
    send_flags_to_int(T) bor
    case H of
        sndmore -> ?ZMQ_SNDMORE;
        _ -> throw({unknown_send_option, 0})
    end.

encode_msg_send(Socket, Data, Flags) ->
    F = send_flags_to_int(Flags),
    <<(?ZMQ_SEND):8, Socket:32, F:32, Data/binary>>.
encode_msg_recv(Socket) ->
    <<(?ZMQ_RECV):8, Socket:32>>.

do_call(#state{} = State, Message) ->
    Reply = driver(State#state.port, Message),
    {reply, Reply, State}.

driver(Port, Message) ->
    ?log("port command ~p", [Message]),
    port_command(Port, Message),
    receive
        zok ->
            ok;
        {zok, Term} ->
            {ok, Term};
        Err = {error, _} ->
            Err
    end.
