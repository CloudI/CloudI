%%% File    : pgsql_tcp.erl
%%% Author  : Blah <cos@local>
%%% Description : Unwrapping of TCP line protocol packages to postgres messages.
%%% Created : 22 Jul 2005

-module(pgsql_tcp).

-behaviour(gen_server).

-export([start/2, start_link/2]).

%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 code_change/3,
	 handle_info/2,
	 terminate/2]).

-record(state, {socket, protopid, buffer}).

start(Sock, ProtoPid) ->
    gen_server:start(?MODULE, [Sock, ProtoPid], []).

start_link(Sock, ProtoPid) ->
    gen_server:start_link(?MODULE, [Sock, ProtoPid], []).

init([Sock, ProtoPid]) ->
    inet:setopts(Sock, [{active, once}]),
    {ok, #state{socket = Sock, protopid = ProtoPid, buffer = <<>>}}.

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_info({tcp, Sock, Bin},
	    #state{socket = Sock,
		   protopid = ProtoPid,
		   buffer = Buffer} = State) ->
    {ok, Rest} = process_buffer(ProtoPid, <<Buffer/binary, Bin/binary>>),
    inet:setopts(Sock, [{active, once}]),
    {noreply, State#state{buffer = Rest}};
handle_info({tcp_closed, Sock},
	    #state{socket = Sock,
		   protopid = ProtoPid} = State) ->
    io:format("Sock closed~n", []),
    ProtoPid ! {socket, Sock, closed},
    {stop, tcp_close, State};
handle_info({tcp_error, Sock, Reason},
	    #state{socket = Sock,
		   protopid = ProtoPid} = State) ->
    io:format("Sock error~n", []),
    ProtoPid ! {socket, Sock, {error, Reason}},
    {stop, tcp_error, State};
handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


%% Given a binary that begins with a proper message header the binary
%% will be processed for each full message it contains, and it will
%% return any trailing incomplete messages.
process_buffer(ProtoPid, Bin = <<Code:8/integer, Size:4/integer-unit:8, Rest/binary>>) ->
    Payload = Size - 4,
    if
	size(Rest) >= Payload ->
	    <<Packet:Payload/binary, Rest1/binary>> = Rest,
	    {ok, Message} = pgsql_proto:decode_packet(Code, Packet),
	    ProtoPid ! {pgsql, Message},
	    process_buffer(ProtoPid, Rest1);
	true ->
	    {ok, Bin}
    end;
process_buffer(_ProtoPid, Bin) when is_binary(Bin) ->
    {ok, Bin}.

