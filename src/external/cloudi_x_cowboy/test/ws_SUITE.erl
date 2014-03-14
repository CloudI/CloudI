%% Copyright (c) 2011-2013, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(ws_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct.
-export([all/0]).
-export([groups/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_group/2]).
-export([end_per_group/2]).

%% Tests.
-export([ws0/1]).
-export([ws8/1]).
-export([ws8_init_shutdown/1]).
-export([ws8_single_bytes/1]).
-export([ws13/1]).
-export([ws_deflate/1]).
-export([ws_deflate_chunks/1]).
-export([ws_deflate_fragments/1]).
-export([ws_send_close/1]).
-export([ws_send_close_payload/1]).
-export([ws_send_many/1]).
-export([ws_text_fragments/1]).
-export([ws_timeout_hibernate/1]).
-export([ws_timeout_cancel/1]).
-export([ws_timeout_reset/1]).
-export([ws_upgrade_with_opts/1]).

%% ct.

all() ->
	[{group, ws}].

groups() ->
	BaseTests = [
		ws0,
		ws8,
		ws8_init_shutdown,
		ws8_single_bytes,
		ws13,
		ws_deflate,
		ws_deflate_chunks,
		ws_deflate_fragments,
		ws_send_close,
		ws_send_close_payload,
		ws_send_many,
		ws_text_fragments,
		ws_timeout_hibernate,
		ws_timeout_cancel,
		ws_timeout_reset,
		ws_upgrade_with_opts
	],
	[{ws, [parallel], BaseTests}].

init_per_suite(Config) ->
	application:start(crypto),
	application:start(cowlib),
	application:start(ranch),
	application:start(cowboy),
	Config.

end_per_suite(_Config) ->
	application:stop(cowboy),
	application:stop(ranch),
	application:stop(cowlib),
	application:stop(crypto),
	ok.

init_per_group(ws, Config) ->
	cowboy:start_http(ws, 100, [{port, 0}], [
		{env, [{dispatch, init_dispatch()}]},
		{compress, true}
	]),
	Port = ranch:get_port(ws),
	[{port, Port}|Config].

end_per_group(Listener, _Config) ->
	cowboy:stop_listener(Listener),
	ok.

%% Dispatch configuration.

init_dispatch() ->
	cowboy_router:compile([
		{"localhost", [
			{"/ws_echo_timer", ws_echo_timer, []},
			{"/ws_echo", ws_echo, []},
			{"/ws_init_shutdown", ws_init_shutdown, []},
			{"/ws_send_many", ws_send_many, [
				{sequence, [
					{text, <<"one">>},
					{text, <<"two">>},
					{text, <<"seven!">>}]}
			]},
			{"/ws_send_close", ws_send_many, [
				{sequence, [
					{text, <<"send">>},
					close,
					{text, <<"won't be received">>}]}
			]},
			{"/ws_send_close_payload", ws_send_many, [
				{sequence, [
					{text, <<"send">>},
					{close, 1001, <<"some text!">>},
					{text, <<"won't be received">>}]}
			]},
			{"/ws_timeout_hibernate", ws_timeout_hibernate, []},
			{"/ws_timeout_cancel", ws_timeout_cancel, []},
			{"/ws_upgrade_with_opts", ws_upgrade_with_opts,
				<<"failure">>}
		]}
	]).

%% ws and wss.

%% We do not support hixie76 anymore.
ws0(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket,
		"GET /ws_echo_timer HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade\r\n"
		"Upgrade: WebSocket\r\n"
		"Origin: http://localhost\r\n"
		"Sec-Websocket-Key1: Y\" 4 1Lj!957b8@0H756!i\r\n"
		"Sec-Websocket-Key2: 1711 M;4\\74  80<6\r\n"
		"\r\n"),
	{ok, Handshake} = gen_tcp:recv(Socket, 0, 6000),
	{ok, {http_response, {1, 1}, 400, _}, _}
		= erlang:decode_packet(http, Handshake, []).

ws8(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, [
		"GET /ws_echo_timer HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade\r\n"
		"Upgrade: websocket\r\n"
		"Sec-WebSocket-Origin: http://localhost\r\n"
		"Sec-WebSocket-Version: 8\r\n"
		"Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n"
		"\r\n"]),
	{ok, Handshake} = gen_tcp:recv(Socket, 0, 6000),
	{ok, {http_response, {1, 1}, 101, "Switching Protocols"}, Rest}
		= erlang:decode_packet(http, Handshake, []),
	[Headers, <<>>] = websocket_headers(
		erlang:decode_packet(httph, Rest, []), []),
	{'Connection', "Upgrade"} = lists:keyfind('Connection', 1, Headers),
	{'Upgrade', "websocket"} = lists:keyfind('Upgrade', 1, Headers),
	{"sec-websocket-accept", "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="}
		= lists:keyfind("sec-websocket-accept", 1, Headers),
	ok = gen_tcp:send(Socket, << 16#81, 16#85, 16#37, 16#fa, 16#21, 16#3d,
		16#7f, 16#9f, 16#4d, 16#51, 16#58 >>),
	{ok, << 1:1, 0:3, 1:4, 0:1, 5:7, "Hello" >>}
		= gen_tcp:recv(Socket, 0, 6000),
	{ok, << 1:1, 0:3, 1:4, 0:1, 14:7, "websocket_init" >>}
		= gen_tcp:recv(Socket, 0, 6000),
	{ok, << 1:1, 0:3, 1:4, 0:1, 16:7, "websocket_handle" >>}
		= gen_tcp:recv(Socket, 0, 6000),
	{ok, << 1:1, 0:3, 1:4, 0:1, 16:7, "websocket_handle" >>}
		= gen_tcp:recv(Socket, 0, 6000),
	{ok, << 1:1, 0:3, 1:4, 0:1, 16:7, "websocket_handle" >>}
		= gen_tcp:recv(Socket, 0, 6000),
	ok = gen_tcp:send(Socket, << 1:1, 0:3, 9:4, 1:1, 0:7, 0:32 >>), %% ping
	{ok, << 1:1, 0:3, 10:4, 0:8 >>} = gen_tcp:recv(Socket, 0, 6000), %% pong
	ok = gen_tcp:send(Socket, << 1:1, 0:3, 8:4, 1:1, 0:7, 0:32 >>), %% close
	{ok, << 1:1, 0:3, 8:4, 0:8 >>} = gen_tcp:recv(Socket, 0, 6000),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws8_init_shutdown(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, [
		"GET /ws_init_shutdown HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade\r\n"
		"Upgrade: websocket\r\n"
		"Sec-WebSocket-Origin: http://localhost\r\n"
		"Sec-WebSocket-Version: 8\r\n"
		"Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n"
		"\r\n"]),
	{ok, Handshake} = gen_tcp:recv(Socket, 0, 6000),
	{ok, {http_response, {1, 1}, 403, "Forbidden"}, _Rest}
		= erlang:decode_packet(http, Handshake, []),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws8_single_bytes(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, [
		"GET /ws_echo_timer HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade\r\n"
		"Upgrade: websocket\r\n"
		"Sec-WebSocket-Origin: http://localhost\r\n"
		"Sec-WebSocket-Version: 8\r\n"
		"Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n"
		"\r\n"]),
	{ok, Handshake} = gen_tcp:recv(Socket, 0, 6000),
	{ok, {http_response, {1, 1}, 101, "Switching Protocols"}, Rest}
		= erlang:decode_packet(http, Handshake, []),
	[Headers, <<>>] = websocket_headers(
		erlang:decode_packet(httph, Rest, []), []),
	{'Connection', "Upgrade"} = lists:keyfind('Connection', 1, Headers),
	{'Upgrade', "websocket"} = lists:keyfind('Upgrade', 1, Headers),
	{"sec-websocket-accept", "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="}
		= lists:keyfind("sec-websocket-accept", 1, Headers),
	ok = gen_tcp:send(Socket, << 16#81 >>), %% send one byte
	ok = timer:sleep(100), %% sleep for a period
	ok = gen_tcp:send(Socket, << 16#85 >>), %% send another and so on
	ok = timer:sleep(100),
	ok = gen_tcp:send(Socket, << 16#37 >>),
	ok = timer:sleep(100),
	ok = gen_tcp:send(Socket, << 16#fa >>),
	ok = timer:sleep(100),
	ok = gen_tcp:send(Socket, << 16#21 >>),
	ok = timer:sleep(100),
	ok = gen_tcp:send(Socket, << 16#3d >>),
	ok = timer:sleep(100),
	ok = gen_tcp:send(Socket, << 16#7f >>),
	ok = timer:sleep(100),
	ok = gen_tcp:send(Socket, << 16#9f >>),
	ok = timer:sleep(100),
	ok = gen_tcp:send(Socket, << 16#4d >>),
	ok = timer:sleep(100),
	ok = gen_tcp:send(Socket, << 16#51 >>),
	ok = timer:sleep(100),
	ok = gen_tcp:send(Socket, << 16#58 >>),
	{ok, << 1:1, 0:3, 1:4, 0:1, 14:7, "websocket_init" >>}
		= gen_tcp:recv(Socket, 0, 6000),
	{ok, << 1:1, 0:3, 1:4, 0:1, 5:7, "Hello" >>}
		= gen_tcp:recv(Socket, 0, 6000),
	{ok, << 1:1, 0:3, 1:4, 0:1, 16:7, "websocket_handle" >>}
		= gen_tcp:recv(Socket, 0, 6000),
	{ok, << 1:1, 0:3, 1:4, 0:1, 16:7, "websocket_handle" >>}
		= gen_tcp:recv(Socket, 0, 6000),
	{ok, << 1:1, 0:3, 1:4, 0:1, 16:7, "websocket_handle" >>}
		= gen_tcp:recv(Socket, 0, 6000),
	ok = gen_tcp:send(Socket, << 1:1, 0:3, 9:4, 1:1, 0:7, 0:32 >>), %% ping
	{ok, << 1:1, 0:3, 10:4, 0:8 >>} = gen_tcp:recv(Socket, 0, 6000), %% pong
	ok = gen_tcp:send(Socket, << 1:1, 0:3, 8:4, 1:1, 0:7, 0:32 >>), %% close
	{ok, << 1:1, 0:3, 8:4, 0:8 >>} = gen_tcp:recv(Socket, 0, 6000),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws13(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, [
		"GET /ws_echo_timer HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade\r\n"
		"Origin: http://localhost\r\n"
		"Sec-WebSocket-Version: 13\r\n"
		"Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n"
		"Upgrade: websocket\r\n"
		"\r\n"]),
	{ok, Handshake} = gen_tcp:recv(Socket, 0, 6000),
	{ok, {http_response, {1, 1}, 101, "Switching Protocols"}, Rest}
		= erlang:decode_packet(http, Handshake, []),
	[Headers, <<>>] = websocket_headers(
		erlang:decode_packet(httph, Rest, []), []),
	{'Connection', "Upgrade"} = lists:keyfind('Connection', 1, Headers),
	{'Upgrade', "websocket"} = lists:keyfind('Upgrade', 1, Headers),
	{"sec-websocket-accept", "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="}
		= lists:keyfind("sec-websocket-accept", 1, Headers),
	%% text
	ok = gen_tcp:send(Socket, << 16#81, 16#85, 16#37, 16#fa, 16#21, 16#3d,
		16#7f, 16#9f, 16#4d, 16#51, 16#58 >>),
	{ok, << 1:1, 0:3, 1:4, 0:1, 5:7, "Hello" >>}
		= gen_tcp:recv(Socket, 0, 6000),
	%% binary (empty)
	ok = gen_tcp:send(Socket, << 1:1, 0:3, 2:4, 1:1, 0:7, 0:32 >>),
	{ok, << 1:1, 0:3, 2:4, 0:8 >>} = gen_tcp:recv(Socket, 0, 6000),
	%% binary
	ok = gen_tcp:send(Socket, << 16#82, 16#85, 16#37, 16#fa, 16#21, 16#3d,
		16#7f, 16#9f, 16#4d, 16#51, 16#58 >>),
	{ok, << 1:1, 0:3, 2:4, 0:1, 5:7, "Hello" >>}
		= gen_tcp:recv(Socket, 0, 6000),
	%% Receives.
	{ok, << 1:1, 0:3, 1:4, 0:1, 14:7, "websocket_init" >>}
		= gen_tcp:recv(Socket, 0, 6000),
	{ok, << 1:1, 0:3, 1:4, 0:1, 16:7, "websocket_handle" >>}
		= gen_tcp:recv(Socket, 0, 6000),
	{ok, << 1:1, 0:3, 1:4, 0:1, 16:7, "websocket_handle" >>}
		= gen_tcp:recv(Socket, 0, 6000),
	{ok, << 1:1, 0:3, 1:4, 0:1, 16:7, "websocket_handle" >>}
		= gen_tcp:recv(Socket, 0, 6000),
	ok = gen_tcp:send(Socket, << 1:1, 0:3, 9:4, 1:1, 0:7, 0:32 >>), %% ping
	{ok, << 1:1, 0:3, 10:4, 0:8 >>} = gen_tcp:recv(Socket, 0, 6000), %% pong
	ok = gen_tcp:send(Socket, << 1:1, 0:3, 8:4, 1:1, 0:7, 0:32 >>), %% close
	{ok, << 1:1, 0:3, 8:4, 0:8 >>} = gen_tcp:recv(Socket, 0, 6000),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws_deflate(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}, {nodelay, true}]),
	ok = gen_tcp:send(Socket, [
		"GET /ws_echo HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade\r\n"
		"Upgrade: websocket\r\n"
		"Sec-WebSocket-Origin: http://localhost\r\n"
		"Sec-WebSocket-Version: 8\r\n"
		"Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n"
		"Sec-WebSocket-Extensions: x-webkit-deflate-frame\r\n"
		"\r\n"]),
	{ok, Handshake} = gen_tcp:recv(Socket, 0, 6000),
	{ok, {http_response, {1, 1}, 101, "Switching Protocols"}, Rest}
		= erlang:decode_packet(http, Handshake, []),
	[Headers, <<>>] = websocket_headers(
		erlang:decode_packet(httph, Rest, []), []),
	{'Connection', "Upgrade"} = lists:keyfind('Connection', 1, Headers),
	{'Upgrade', "websocket"} = lists:keyfind('Upgrade', 1, Headers),
	{"sec-websocket-accept", "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="}
		= lists:keyfind("sec-websocket-accept", 1, Headers),
	{"sec-websocket-extensions", "x-webkit-deflate-frame"}
		= lists:keyfind("sec-websocket-extensions", 1, Headers),

	Mask = 16#11223344,
	Hello = << 242, 72, 205, 201, 201, 7, 0 >>,
	MaskedHello = websocket_mask(Hello, Mask, <<>>),

	% send compressed text frame containing the Hello string
	ok = gen_tcp:send(Socket, << 1:1, 1:1, 0:2, 1:4, 1:1, 7:7, Mask:32,
		MaskedHello/binary >>),
	% receive compressed text frame containing the Hello string
	{ok, << 1:1, 1:1, 0:2, 1:4, 0:1, 7:7, Hello/binary >>}
		= gen_tcp:recv(Socket, 0, 6000),

	ok = gen_tcp:send(Socket, << 1:1, 0:3, 8:4, 1:1, 0:7, 0:32 >>), %% close
	{ok, << 1:1, 0:3, 8:4, 0:8 >>} = gen_tcp:recv(Socket, 0, 6000),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws_deflate_chunks(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}, {nodelay, true}]),
	ok = gen_tcp:send(Socket, [
		"GET /ws_echo HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade\r\n"
		"Upgrade: websocket\r\n"
		"Sec-WebSocket-Origin: http://localhost\r\n"
		"Sec-WebSocket-Version: 8\r\n"
		"Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n"
		"Sec-WebSocket-Extensions: x-webkit-deflate-frame\r\n"
		"\r\n"]),
	{ok, Handshake} = gen_tcp:recv(Socket, 0, 6000),
	{ok, {http_response, {1, 1}, 101, "Switching Protocols"}, Rest}
		= erlang:decode_packet(http, Handshake, []),
	[Headers, <<>>] = websocket_headers(
		erlang:decode_packet(httph, Rest, []), []),
	{'Connection', "Upgrade"} = lists:keyfind('Connection', 1, Headers),
	{'Upgrade', "websocket"} = lists:keyfind('Upgrade', 1, Headers),
	{"sec-websocket-accept", "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="}
		= lists:keyfind("sec-websocket-accept", 1, Headers),
	{"sec-websocket-extensions", "x-webkit-deflate-frame"}
		= lists:keyfind("sec-websocket-extensions", 1, Headers),

	Mask = 16#11223344,
	Hello = << 242, 72, 205, 201, 201, 7, 0 >>,
	MaskedHello = websocket_mask(Hello, Mask, <<>>),

	% send compressed text frame containing the Hello string
	ok = gen_tcp:send(Socket, << 1:1, 1:1, 0:2, 1:4, 1:1, 7:7, Mask:32,
		(binary:part(MaskedHello, 0, 4))/binary >>),
	ok = timer:sleep(100),
	ok = gen_tcp:send(Socket, binary:part(MaskedHello, 4, 3)),

	% receive compressed text frame containing the Hello string
	{ok, << 1:1, 1:1, 0:2, 1:4, 0:1, 7:7, Hello/binary >>}
		= gen_tcp:recv(Socket, 0, 6000),

	ok = gen_tcp:send(Socket, << 1:1, 0:3, 8:4, 1:1, 0:7, 0:32 >>), %% close
	{ok, << 1:1, 0:3, 8:4, 0:8 >>} = gen_tcp:recv(Socket, 0, 6000),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws_deflate_fragments(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}, {nodelay, true}]),
	ok = gen_tcp:send(Socket, [
		"GET /ws_echo HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade\r\n"
		"Upgrade: websocket\r\n"
		"Sec-WebSocket-Origin: http://localhost\r\n"
		"Sec-WebSocket-Version: 8\r\n"
		"Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n"
		"Sec-WebSocket-Extensions: x-webkit-deflate-frame\r\n"
		"\r\n"]),
	{ok, Handshake} = gen_tcp:recv(Socket, 0, 6000),
	{ok, {http_response, {1, 1}, 101, "Switching Protocols"}, Rest}
		= erlang:decode_packet(http, Handshake, []),
	[Headers, <<>>] = websocket_headers(
		erlang:decode_packet(httph, Rest, []), []),
	{'Connection', "Upgrade"} = lists:keyfind('Connection', 1, Headers),
	{'Upgrade', "websocket"} = lists:keyfind('Upgrade', 1, Headers),
	{"sec-websocket-accept", "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="}
		= lists:keyfind("sec-websocket-accept", 1, Headers),
	{"sec-websocket-extensions", "x-webkit-deflate-frame"}
		= lists:keyfind("sec-websocket-extensions", 1, Headers),

	Mask = 16#11223344,
	Hello = << 242, 72, 205, 201, 201, 7, 0 >>,

	% send compressed text frame containing the Hello string
	% as 2 separate fragments
	ok = gen_tcp:send(Socket, << 0:1, 1:1, 0:2, 1:4, 1:1, 4:7, Mask:32,
		(websocket_mask(binary:part(Hello, 0, 4), Mask, <<>>))/binary >>),
	ok = gen_tcp:send(Socket, << 1:1, 1:1, 0:2, 0:4, 1:1, 3:7, Mask:32,
		(websocket_mask(binary:part(Hello, 4, 3), Mask, <<>>))/binary >>),
	% receive compressed text frame containing the Hello string
	{ok, << 1:1, 1:1, 0:2, 1:4, 0:1, 7:7, Hello/binary >>}
		= gen_tcp:recv(Socket, 0, 6000),

	ok = gen_tcp:send(Socket, << 1:1, 0:3, 8:4, 1:1, 0:7, 0:32 >>), %% close
	{ok, << 1:1, 0:3, 8:4, 0:8 >>} = gen_tcp:recv(Socket, 0, 6000),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws_send_close(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, [
		"GET /ws_send_close HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade\r\n"
		"Upgrade: websocket\r\n"
		"Sec-WebSocket-Origin: http://localhost\r\n"
		"Sec-WebSocket-Version: 8\r\n"
		"Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n"
		"\r\n"]),
	{ok, Handshake} = gen_tcp:recv(Socket, 0, 6000),
	{ok, {http_response, {1, 1}, 101, "Switching Protocols"}, Rest}
		= erlang:decode_packet(http, Handshake, []),
	[Headers, <<>>] = websocket_headers(
		erlang:decode_packet(httph, Rest, []), []),
	{'Connection', "Upgrade"} = lists:keyfind('Connection', 1, Headers),
	{'Upgrade', "websocket"} = lists:keyfind('Upgrade', 1, Headers),
	{"sec-websocket-accept", "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="}
		= lists:keyfind("sec-websocket-accept", 1, Headers),
	%% We catch all frames at once and check them directly.
	{ok, Many} = gen_tcp:recv(Socket, 8, 6000),
	<< 1:1, 0:3, 1:4, 0:1, 4:7, "send",
		1:1, 0:3, 8:4, 0:8 >> = Many,
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws_send_close_payload(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, [
		"GET /ws_send_close_payload HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade\r\n"
		"Upgrade: websocket\r\n"
		"Sec-WebSocket-Origin: http://localhost\r\n"
		"Sec-WebSocket-Version: 8\r\n"
		"Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n"
		"\r\n"]),
	{ok, Handshake} = gen_tcp:recv(Socket, 0, 6000),
	{ok, {http_response, {1, 1}, 101, "Switching Protocols"}, Rest}
		= erlang:decode_packet(http, Handshake, []),
	[Headers, <<>>] = websocket_headers(
		erlang:decode_packet(httph, Rest, []), []),
	{'Connection', "Upgrade"} = lists:keyfind('Connection', 1, Headers),
	{'Upgrade', "websocket"} = lists:keyfind('Upgrade', 1, Headers),
	{"sec-websocket-accept", "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="}
		= lists:keyfind("sec-websocket-accept", 1, Headers),
	%% We catch all frames at once and check them directly.
	{ok, Many} = gen_tcp:recv(Socket, 20, 6000),
	<< 1:1, 0:3, 1:4, 0:1, 4:7, "send",
		1:1, 0:3, 8:4, 0:1, 12:7, 1001:16, "some text!" >> = Many,
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws_send_many(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, [
		"GET /ws_send_many HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade\r\n"
		"Upgrade: websocket\r\n"
		"Sec-WebSocket-Origin: http://localhost\r\n"
		"Sec-WebSocket-Version: 8\r\n"
		"Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n"
		"\r\n"]),
	{ok, Handshake} = gen_tcp:recv(Socket, 0, 6000),
	{ok, {http_response, {1, 1}, 101, "Switching Protocols"}, Rest}
		= erlang:decode_packet(http, Handshake, []),
	[Headers, <<>>] = websocket_headers(
		erlang:decode_packet(httph, Rest, []), []),
	{'Connection', "Upgrade"} = lists:keyfind('Connection', 1, Headers),
	{'Upgrade', "websocket"} = lists:keyfind('Upgrade', 1, Headers),
	{"sec-websocket-accept", "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="}
		= lists:keyfind("sec-websocket-accept", 1, Headers),
	%% We catch all frames at once and check them directly.
	{ok, Many} = gen_tcp:recv(Socket, 18, 6000),
	<< 1:1, 0:3, 1:4, 0:1, 3:7, "one",
		1:1, 0:3, 1:4, 0:1, 3:7, "two",
		1:1, 0:3, 1:4, 0:1, 6:7, "seven!" >> = Many,
	ok = gen_tcp:send(Socket, << 1:1, 0:3, 8:4, 1:1, 0:7, 0:32 >>), %% close
	{ok, << 1:1, 0:3, 8:4, 0:8 >>} = gen_tcp:recv(Socket, 0, 6000),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws_text_fragments(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, [
		"GET /ws_echo HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade\r\n"
		"Upgrade: websocket\r\n"
		"Sec-WebSocket-Origin: http://localhost\r\n"
		"Sec-WebSocket-Version: 8\r\n"
		"Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n"
		"\r\n"]),
	{ok, Handshake} = gen_tcp:recv(Socket, 0, 6000),
	{ok, {http_response, {1, 1}, 101, "Switching Protocols"}, Rest}
		= erlang:decode_packet(http, Handshake, []),
	[Headers, <<>>] = websocket_headers(
		erlang:decode_packet(httph, Rest, []), []),
	{'Connection', "Upgrade"} = lists:keyfind('Connection', 1, Headers),
	{'Upgrade', "websocket"} = lists:keyfind('Upgrade', 1, Headers),
	{"sec-websocket-accept", "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="}
		= lists:keyfind("sec-websocket-accept", 1, Headers),

	ok = gen_tcp:send(Socket, [
		<< 0:1, 0:3, 1:4, 1:1, 5:7 >>,
		<< 16#37 >>, << 16#fa >>, << 16#21 >>, << 16#3d >>, << 16#7f >>,
		<< 16#9f >>, << 16#4d >>, << 16#51 >>, << 16#58 >>]),
	ok = gen_tcp:send(Socket, [
		<< 1:1, 0:3, 0:4, 1:1, 5:7 >>,
		<< 16#37 >>, << 16#fa >>, << 16#21 >>, << 16#3d >>, << 16#7f >>,
		<< 16#9f >>, << 16#4d >>, << 16#51 >>, << 16#58 >>]),
	{ok, << 1:1, 0:3, 1:4, 0:1, 10:7, "HelloHello" >>}
		= gen_tcp:recv(Socket, 0, 6000),

	ok = gen_tcp:send(Socket, [
		%% #1
		<< 0:1, 0:3, 1:4, 1:1, 5:7 >>,
		<< 16#37 >>, << 16#fa >>, << 16#21 >>, << 16#3d >>, << 16#7f >>,
		<< 16#9f >>, << 16#4d >>, << 16#51 >>, << 16#58 >>,
		%% #2
		<< 0:1, 0:3, 0:4, 1:1, 5:7 >>,
		<< 16#37 >>, << 16#fa >>, << 16#21 >>, << 16#3d >>, << 16#7f >>,
		<< 16#9f >>, << 16#4d >>, << 16#51 >>, << 16#58 >>,
		%% #3
		<< 1:1, 0:3, 0:4, 1:1, 5:7 >>,
		<< 16#37 >>, << 16#fa >>, << 16#21 >>, << 16#3d >>, << 16#7f >>,
		<< 16#9f >>, << 16#4d >>, << 16#51 >>, << 16#58 >>]),
	{ok, << 1:1, 0:3, 1:4, 0:1, 15:7, "HelloHelloHello" >>}
		= gen_tcp:recv(Socket, 0, 6000),
	ok = gen_tcp:send(Socket, << 1:1, 0:3, 8:4, 1:1, 0:7, 0:32 >>), %% close
	{ok, << 1:1, 0:3, 8:4, 0:8 >>} = gen_tcp:recv(Socket, 0, 6000),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws_timeout_hibernate(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, [
		"GET /ws_timeout_hibernate HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade\r\n"
		"Upgrade: websocket\r\n"
		"Sec-WebSocket-Origin: http://localhost\r\n"
		"Sec-WebSocket-Version: 8\r\n"
		"Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n"
		"\r\n"]),
	{ok, Handshake} = gen_tcp:recv(Socket, 0, 6000),
	{ok, {http_response, {1, 1}, 101, "Switching Protocols"}, Rest}
		= erlang:decode_packet(http, Handshake, []),
	[Headers, <<>>] = websocket_headers(
		erlang:decode_packet(httph, Rest, []), []),
	{'Connection', "Upgrade"} = lists:keyfind('Connection', 1, Headers),
	{'Upgrade', "websocket"} = lists:keyfind('Upgrade', 1, Headers),
	{"sec-websocket-accept", "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="}
		= lists:keyfind("sec-websocket-accept", 1, Headers),
	{ok, << 1:1, 0:3, 8:4, 0:1, 2:7, 1000:16 >>} = gen_tcp:recv(Socket, 0, 6000),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws_timeout_cancel(Config) ->
	%% Erlang messages to a socket should not cancel the timeout	
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, [
		"GET /ws_timeout_cancel HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade\r\n"
		"Upgrade: websocket\r\n"
		"Sec-WebSocket-Origin: http://localhost\r\n"
		"Sec-WebSocket-Version: 8\r\n"
		"Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n"
		"\r\n"]),
	{ok, Handshake} = gen_tcp:recv(Socket, 0, 6000),
	{ok, {http_response, {1, 1}, 101, "Switching Protocols"}, Rest}
		= erlang:decode_packet(http, Handshake, []),
	[Headers, <<>>] = websocket_headers(
		erlang:decode_packet(httph, Rest, []), []),
	{'Connection', "Upgrade"} = lists:keyfind('Connection', 1, Headers),
	{'Upgrade', "websocket"} = lists:keyfind('Upgrade', 1, Headers),
	{"sec-websocket-accept", "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="}
		= lists:keyfind("sec-websocket-accept", 1, Headers),
	{ok, << 1:1, 0:3, 8:4, 0:1, 2:7, 1000:16 >>} = gen_tcp:recv(Socket, 0, 6000),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws_timeout_reset(Config) ->
	%% Erlang messages across a socket should reset the timeout	
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, [
		"GET /ws_timeout_cancel HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade\r\n"
		"Upgrade: websocket\r\n"
		"Sec-WebSocket-Origin: http://localhost\r\n"
		"Sec-Websocket-Version: 13\r\n"
		"Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n"
		"\r\n"]),
	{ok, Handshake} = gen_tcp:recv(Socket, 0, 6000),
	{ok, {http_response, {1, 1}, 101, "Switching Protocols"}, Rest}
		= erlang:decode_packet(http, Handshake, []),
	[Headers, <<>>] = websocket_headers(
		erlang:decode_packet(httph, Rest, []), []),
	{'Connection', "Upgrade"} = lists:keyfind('Connection', 1, Headers),
	{'Upgrade', "websocket"} = lists:keyfind('Upgrade', 1, Headers),
	{"sec-websocket-accept", "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="}
		= lists:keyfind("sec-websocket-accept", 1, Headers),
	[begin
		ok = gen_tcp:send(Socket, << 16#81, 16#85, 16#37, 16#fa, 16#21, 16#3d,
			16#7f, 16#9f, 16#4d, 16#51, 16#58 >>),
		{ok, << 1:1, 0:3, 1:4, 0:1, 5:7, "Hello" >>}
			= gen_tcp:recv(Socket, 0, 6000),
		ok = timer:sleep(500)
	end || _ <- [1, 2, 3, 4]],
	{ok, << 1:1, 0:3, 8:4, 0:1, 2:7, 1000:16 >>} = gen_tcp:recv(Socket, 0, 6000),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

ws_upgrade_with_opts(Config) ->
	{port, Port} = lists:keyfind(port, 1, Config),
	{ok, Socket} = gen_tcp:connect("localhost", Port,
		[binary, {active, false}, {packet, raw}]),
	ok = gen_tcp:send(Socket, [
		"GET /ws_upgrade_with_opts HTTP/1.1\r\n"
		"Host: localhost\r\n"
		"Connection: Upgrade\r\n"
		"Upgrade: websocket\r\n"
		"Sec-WebSocket-Origin: http://localhost\r\n"
		"Sec-WebSocket-Version: 8\r\n"
		"Sec-WebSocket-Key: dGhlIHNhbXBsZSBub25jZQ==\r\n"
		"\r\n"]),
	{ok, Handshake} = gen_tcp:recv(Socket, 0, 6000),
	{ok, {http_response, {1, 1}, 101, "Switching Protocols"}, Rest}
		= erlang:decode_packet(http, Handshake, []),
	[Headers, <<>>] = websocket_headers(
		erlang:decode_packet(httph, Rest, []), []),
	{'Connection', "Upgrade"} = lists:keyfind('Connection', 1, Headers),
	{'Upgrade', "websocket"} = lists:keyfind('Upgrade', 1, Headers),
	{"sec-websocket-accept", "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="}
		= lists:keyfind("sec-websocket-accept", 1, Headers),
	{ok, Response} = gen_tcp:recv(Socket, 9, 6000),
	<< 1:1, 0:3, 1:4, 0:1, 7:7, "success" >> = Response,
	ok = gen_tcp:send(Socket, << 1:1, 0:3, 8:4, 1:1, 0:7, 0:32 >>), %% close
	{ok, << 1:1, 0:3, 8:4, 0:8 >>} = gen_tcp:recv(Socket, 0, 6000),
	{error, closed} = gen_tcp:recv(Socket, 0, 6000),
	ok.

%% Internal.

websocket_headers({ok, http_eoh, Rest}, Acc) ->
	[Acc, Rest];
websocket_headers({ok, {http_header, _I, Key, _R, Value}, Rest}, Acc) ->
	F = fun(S) when is_atom(S) -> S; (S) -> string:to_lower(S) end,
	websocket_headers(erlang:decode_packet(httph, Rest, []),
		[{F(Key), Value}|Acc]).

websocket_mask(<<>>, _, Unmasked) ->
	Unmasked;
websocket_mask(<< O:32, Rest/bits >>, MaskKey, Acc) ->
	T = O bxor MaskKey,
	websocket_mask(Rest, MaskKey, << Acc/binary, T:32 >>);
websocket_mask(<< O:24 >>, MaskKey, Acc) ->
	<< MaskKey2:24, _:8 >> = << MaskKey:32 >>,
	T = O bxor MaskKey2,
	<< Acc/binary, T:24 >>;
websocket_mask(<< O:16 >>, MaskKey, Acc) ->
	<< MaskKey2:16, _:16 >> = << MaskKey:32 >>,
	T = O bxor MaskKey2,
	<< Acc/binary, T:16 >>;
websocket_mask(<< O:8 >>, MaskKey, Acc) ->
	<< MaskKey2:8, _:24 >> = << MaskKey:32 >>,
	T = O bxor MaskKey2,
	<< Acc/binary, T:8 >>.
