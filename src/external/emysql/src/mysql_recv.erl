%%%-------------------------------------------------------------------
%%% Encoding: latin-1
%%% File    : mysql_recv.erl
%%% Author  : Fredrik Thulin <ft@it.su.se>
%%% Descrip.: Handles data being received on a MySQL socket. Decodes
%%%           per-row framing and sends each row to parent.
%%%
%%% Created :  4 Aug 2005 by Fredrik Thulin <ft@it.su.se>
%%%
%%% Note    : All MySQL code was written by Magnus Ahltorp, originally
%%%           in the file mysql.erl - I just moved it here.
%%%
%%% Copyright (c) 2001-2004 Kungliga Tekniska Högskolan
%%% See the file COPYING
%%%
%%%           Signals this receiver process can send to it's parent
%%%             (the parent is a mysql_conn connection handler) :
%%%
%%%             {mysql_recv, self(), data, Packet, Num}
%%%             {mysql_recv, self(), closed, {error, Reason}}
%%%             {mysql_recv, self(), closed, normal}
%%%
%%%           Internally (from inside init/4 to start_link/4) the
%%%           following signals may be sent to the parent process :
%%%
%%%             {mysql_recv, self(), init, {ok, Sock}}
%%%             {mysql_recv, self(), init, {error, E}}
%%%
%%%-------------------------------------------------------------------
-module(mysql_recv).

-export([start_link/3,
         init/3,
         loop/1,
         sendpacket/2]).

-record(state, {socket, parent, data}).
-define(SECURE_CONNECTION, 32768).
-define(CONNECT_TIMEOUT, 5000). %% 5 seconds should be enough

%% @spec start_link(Host, Port, Parent) -> Result
%%       Host = string()
%%       Port = integer()
%%       Parent = pid()
%%       Result = {ok, pid(), term()} | {error, Reason}
%%       Reason = atom() | string()
%% @doc Start a process that connects to Host:Port and waits for data. When
%% it has received a MySQL frame, it sends it to Parent and waits for the
%% next frame.
start_link(Host, Port, Parent) when is_list(Host), is_integer(Port) ->
    Pid = spawn_link(?MODULE, init, [Host, Port, Parent]),
    receive
        {error, _} = E -> E;
        {Pid, Sock} -> {ok, Pid, Sock}
    after 1000 * 5 ->
        {error, failed_to_open_socket}
    end.

%% @private
init(Host, Port, Parent) ->
    case gen_tcp:connect(Host, Port, [binary, {packet, 0}]) of
        {ok, Sock} ->
            Parent ! {self(), Sock},
            State = #state{
                socket  = Sock,
                parent  = Parent,
                data    = <<>>
            },
            loop(State);
        E ->
            Parent ! E
    end.

%% @private
loop(State) ->
    Sock = State#state.socket,
    receive
        {tcp, Sock, InData} ->
            NewData = list_to_binary([State#state.data, InData]),
            Rest = sendpacket(State#state.parent, NewData),
            loop(State#state{data = Rest});
        {tcp_error, Sock, Reason} ->
            State#state.parent ! {mysql_recv, self(), closed, {error, Reason}},
            error;
        {tcp_closed, Sock} ->
            State#state.parent ! {mysql_recv, self(), closed, normal},
            error
    end.

%% @private
sendpacket(Parent, Data) ->
    case Data of
        <<Length:24/little, Num:8, D/binary>> when Length =< size(D)->
            {Packet, Rest} = split_binary(D, Length),
            Parent ! {mysql_recv, self(), data, Packet, Num},
            sendpacket(Parent, Rest);
        _ -> Data
    end.
