%% Feel free to use, reuse and abuse the code in this file.

-module(ws_echo).
-behaviour(cowboy1_websocket_handler).
-export([init/3]).
-export([websocket_init/3, websocket_handle/3,
	websocket_info/3, websocket_terminate/3]).

init(_Any, _Req, _Opts) ->
	{upgrade, protocol, cowboy1_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
	Req2 = cowboy1_req:compact(Req),
	{ok, Req2, undefined}.

websocket_handle({text, Data}, Req, State) ->
	{reply, {text, Data}, Req, State};
websocket_handle({binary, Data}, Req, State) ->
	{reply, {binary, Data}, Req, State};
websocket_handle(_Frame, Req, State) ->
	{ok, Req, State}.

websocket_info(_Info, Req, State) ->
	{ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	ok.
