%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%% @doc Multicast Erlang node discovery protocol.
%% Listens on a multicast channel for node discovery requests and 
%% responds by connecting to the node.
%% @end

-module(nodefinder_multicast).

-behaviour(gen_server).

%% external interface
-export([start_link/5,
         discover/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,
        {
            socket_send :: gen_udp:socket(),
            socket_recv :: gen_udp:socket(),
            address :: inet:ip_address(),
            port :: inet:port_number(),
            timeout :: pos_integer(), % seconds
            connect :: visible | hidden,
            node :: binary(),
            key_v4 :: binary(),
            key_v3 :: binary(),
            key_v2 :: binary()
        }).

-include("nodefinder.hrl").
-include("nodefinder_logging.hrl").

-define(MULTICAST_MESSAGE_NAME, "ERLANG/NODEFINDER").
-define(MULTICAST_MESSAGE_PROTOCOL_VERSION, "4").

% how much time synchronization error to handle between nodes
% (need to have ntpd running when using this source code)
-define(MULTICAST_MESSAGE_VALID_SECONDS, 300). % 5 minutes

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

start_link(Interface, Address, Port, TTL, TimeoutSeconds) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE,
                          [Interface, Address, Port, TTL, TimeoutSeconds], []).

discover(Timeout) ->
    try gen_server:call(?MODULE, discover, Timeout)
    catch
        exit:{Reason, _} ->
            {error, Reason}
    end.

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([Interface, Address, Port, TTL, TimeoutSeconds]) ->
    Opts = [{active, true},
            {ip, Address},
            {multicast_if, Interface},
            {add_membership, {Address, Interface}},
            {multicast_loop, true},
            {reuseaddr, true},
            binary],
    {ok, SocketRecv} = gen_udp:open(Port, Opts),
    Connect = nodefinder_app:connect_type(),
    State = #state{socket_recv = SocketRecv,
                   socket_send = send_socket(Interface, TTL),
                   address = Address,
                   port = Port,
                   timeout = TimeoutSeconds,
                   connect = Connect,
                   node = erlang:atom_to_binary(node(), utf8),
                   key_v4 = key_v4(),
                   key_v3 = key_v3(),
                   key_v2 = key_v2()},
    ok = send_discover(State),
    {ok, State}.

handle_call(discover, _From, State) ->
    ok = send_discover(State),
    {reply, ok, State};
handle_call(Request, _From, State) ->
    {stop, lists:flatten(io_lib:format("Unknown call \"~w\"", [Request])),
     error, State}.

handle_cast(Request, State) ->
    {stop, lists:flatten(io_lib:format("Unknown cast \"~w\"", [Request])),
     State}.

handle_info({udp, SocketRecv, IP, _InPortNo, Packet},
            #state{socket_recv = SocketRecv} = State) ->
    ok = process_packet(Packet, IP, State),
    {noreply, State};

handle_info(Request, State) ->
    {stop, lists:flatten(io_lib:format("Unknown info \"~w\"", [Request])),
     State}.

terminate(_Reason,
          #state{socket_recv = SocketRecv,
                 socket_send = SocketSend}) ->
    gen_udp:close(SocketRecv),
    gen_udp:close(SocketSend),
    ok.

code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

send_discover(#state{socket_send = SocketSend,
                     address = Address,
                     port = Port,
                     node = NodeBin,
                     key_v4 = KeyV4}) ->
    Seconds = seconds_v4(),
    SecondsBin = <<Seconds:64>>,
    Identifier = identifier_v4([SecondsBin, NodeBin], KeyV4),
    Message = <<?MULTICAST_MESSAGE_NAME " "
                ?MULTICAST_MESSAGE_PROTOCOL_VERSION " ",
                Identifier:32/binary, " ",
                SecondsBin:8/binary, " ",
                NodeBin/binary>>,
    case gen_udp:send(SocketSend, Address, Port, Message) of
        ok ->
            ok;
        {error, Reason} ->
            % enetunreach can occur here
            ?LOG_WARN("udp error: ~p", [Reason]),
            ok
    end.

process_packet(<<?MULTICAST_MESSAGE_NAME " "
                 ?MULTICAST_MESSAGE_PROTOCOL_VERSION " ",
                 Identifier:32/binary, " ", 
                 SecondsBin:8/binary, " ",
                 NodeBin/binary>>, IP,
               #state{timeout = Timeout,
                      connect = Connect,
                      key_v4 = KeyV4}) ->
    IdentifierExpected = identifier_v4([SecondsBin, NodeBin], KeyV4),
    if
        Identifier /= IdentifierExpected ->
            ok; % ignored, different cookie
        true ->
            <<Seconds:64>> = SecondsBin,
            Delta = seconds_v4() - Seconds,
            if
                Delta >= (-1 * ?MULTICAST_MESSAGE_VALID_SECONDS),
                Delta < Timeout ->
                    Node = erlang:binary_to_atom(NodeBin, utf8),
                    connect_node(Connect, Node);
                true ->
                    ?LOG_WARN("expired multicast from ~s (~p)",
                              [NodeBin, IP])
            end
    end,
    ok;
process_packet(<<"ERLANG/NODEFINDER 3 ",
                 Identifier:32/binary, " ",
                 SecondsBin:8/binary, " ",
                 NodeBin/binary>>, IP,
               #state{timeout = Timeout,
                      connect = Connect,
                      key_v3 = KeyV3}) ->
    % nodefinder 1.7.3 support
    IdentifierExpected = identifier_v3([SecondsBin, NodeBin], KeyV3),
    if
        Identifier /= IdentifierExpected ->
            ok; % ignored, different cookie
        true ->
            <<Seconds:64>> = SecondsBin,
            Delta = seconds_v3() - Seconds,
            if
                Delta >= (-1 * ?MULTICAST_MESSAGE_VALID_SECONDS),
                Delta < Timeout ->
                    Node = erlang:binary_to_atom(NodeBin, utf8),
                    connect_node(Connect, Node);
                true ->
                    ?LOG_WARN("expired multicast from ~s (~p)",
                              [NodeBin, IP])
            end
    end,
    ok;
process_packet(<<"DISCOVERV2 ",
                 Identifier:20/binary, " ", 
                 SecondsBin:8/binary, " ",
                 NodeBin/binary>>, IP,
               #state{timeout = Timeout,
                      connect = Connect,
                      key_v2 = KeyV2}) ->
    % nodefinder =< 1.7.2 support
    IdentifierExpected = identifier_v2([SecondsBin, NodeBin], KeyV2),
    if
        Identifier /= IdentifierExpected ->
            ok; % ignored, different cookie
        true ->
            <<Seconds:64>> = SecondsBin,
            Delta = seconds_v2() - Seconds,
            if
                Delta >= (-1 * ?MULTICAST_MESSAGE_VALID_SECONDS),
                Delta < Timeout ->
                    Node = erlang:binary_to_atom(NodeBin, latin1),
                    connect_node(Connect, Node);
                true ->
                    ?LOG_WARN("expired multicast from ~s (~p)",
                              [NodeBin, IP])
            end
    end,
    ok;
process_packet(_Packet, _IP, _State) -> 
    ok.

identifier_v4(Message, KeyV4) ->
    crypto:hmac(sha256, KeyV4, Message).

identifier_v3(Message, KeyV3) ->
    crypto:hmac(sha256, KeyV3, Message).

identifier_v2(Message, KeyV2) ->
    crypto:hmac(sha, KeyV2, Message).

key_v4() ->
    crypto:hash(sha256, erlang:atom_to_binary(erlang:get_cookie(), utf8)).

key_v3() ->
    crypto:hash(sha256, erlang:term_to_binary(erlang:get_cookie())).

key_v2() ->
    crypto:hash(sha, erlang:term_to_binary(erlang:get_cookie())).

seconds_v4() ->
    seconds_v3().

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
seconds_v3() ->
    erlang:system_time(second).
-else.
seconds_v3() ->
    erlang:system_time(seconds).
-endif.

seconds_v2() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

send_socket(Interface, TTL) ->
    SendOpts = [{ip, Interface},
                {multicast_ttl, TTL}, 
                {multicast_loop, true}],
    {ok, SocketSend} = gen_udp:open(0, SendOpts),
    SocketSend.

connect_node(_, Node)
    when Node =:= node() ->
    ok;
connect_node(visible, Node) ->
    net_kernel:connect_node(Node);
connect_node(hidden, Node) ->
    net_kernel:hidden_connect_node(Node).

