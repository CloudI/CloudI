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
        sendsock,
        recvsock,
        address,
        port,
        timeout :: pos_integer(), % seconds
        connect :: visible | hidden
    }).

% how much time synchronization error to handle between nodes
-define(SECONDS_DELTA, 300).

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
            list],
    {ok, RecvSocket} = gen_udp:open(Port, Opts),
    Connect = nodefinder_app:connect_type(),
    {ok, send_discover(#state{recvsock = RecvSocket,
                              sendsock = send_socket(Interface, TTL),
                              address = Address,
                              port = Port,
                              timeout = TimeoutSeconds,
                              connect = Connect})}.

handle_call(discover, _From, State) ->
    {reply, ok, send_discover(State)};
handle_call(Request, _From, State) ->
    {stop, lists:flatten(io_lib:format("Unknown call \"~p\"", [Request])),
     error, State}.

handle_cast(Request, State) ->
    {stop, lists:flatten(io_lib:format("Unknown cast \"~p\"", [Request])),
     State}.

handle_info({udp, Socket, IP, InPortNo, Packet},
            #state{recvsock = Socket} = State) ->
    {noreply, process_packet(Packet, IP, InPortNo, State)};

handle_info(Request, State) ->
    {stop, lists:flatten(io_lib:format("Unknown info \"~p\"", [Request])),
     State}.

terminate(_Reason,
          #state{recvsock = RecvSock,
                 sendsock = SendSock}) ->
    gen_udp:close(RecvSock),
    gen_udp:close(SendSock),
    ok.

code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

send_discover(#state{sendsock = SendSock,
                     address = Address,
                     port = Port} = State) ->
    NodeString = erlang:atom_to_list(erlang:node()),
    Time = seconds(),
    Identifier = identifier([<<Time:64>>, NodeString]),
    Message = ["DISCOVERV2 ", Identifier, " ", <<Time:64>>, " ", NodeString],
    ok = gen_udp:send(SendSock, Address, Port, Message),
    State.

identifier(Message) ->
    % Don't use cookie directly
    Key = crypto:hash(sha, erlang:term_to_binary(erlang:get_cookie())),
    crypto:hmac(sha, Key, Message).

process_packet("DISCOVERV2 " ++ Rest, IP, InPortNo,
               #state{timeout = Timeout,
                      connect = Connect} = State) -> 
    case erlang:list_to_binary(Rest) of
        <<Identifier:20/binary, " ", 
          Time:64, " ",
          NodeString/binary>> ->
            IdentifierExpected = identifier([<<Time:64>>, NodeString]),
            Delta = seconds() - Time,
            if
                Identifier /= IdentifierExpected ->
                    ok; % ignored, different cookie
                Delta >= (-1 * ?SECONDS_DELTA), Delta < Timeout ->
                    Node = erlang:list_to_atom(
                        erlang:binary_to_list(NodeString)),
                    connect_node(Connect, Node);
                true ->
                    error_logger:warning_msg("expired DISCOVERV2 (~p) "
                                             "from ~p:~p~n",
                                             [Delta, IP, InPortNo])
            end;
        _ ->
            error_logger:warning_msg("bad DISCOVERV2 from ~p:~p~n", 
                                     [IP, InPortNo])
    end,
    State;
process_packet(_Packet, _IP, _InPortNo, State) -> 
    State.

seconds() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

send_socket(Interface, TTL) ->
    SendOpts = [{ip, Interface},
                {multicast_ttl, TTL}, 
                {multicast_loop, true}],
    {ok, SendSocket} = gen_udp:open(0, SendOpts),
    SendSocket.

connect_node(visible, Node) ->
    net_kernel:connect_node(Node);
connect_node(hidden, Node) ->
    net_kernel:hidden_connect_node(Node).

