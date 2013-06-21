%% Feel free to use, reuse and abuse the code in this file.

-module(ws_send_many).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init(_Any, _Req, _Opts) ->
	{upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, Sequence) ->
	Req2 = cowboy_req:compact(Req),
	erlang:send_after(10, self(), send_many),
	{ok, Req2, Sequence}.

websocket_handle(_Frame, Req, State) ->
	{ok, Req, State}.

websocket_info(send_many, Req, State = [{sequence, Sequence}]) ->
	{reply, Sequence, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.
