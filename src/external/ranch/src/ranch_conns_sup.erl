%% Copyright (c) 2011-2012, Loïc Hoguin <essen@ninenines.eu>
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

%% @private
-module(ranch_conns_sup).
-behaviour(supervisor).

%% API.
-export([start_link/1]).
-export([start_protocol/5]).

%% supervisor.
-export([init/1]).

%% API.

-spec start_link(any()) -> {ok, pid()}.
start_link(Ref) ->
	supervisor:start_link(?MODULE, Ref).

-spec start_protocol(pid(), inet:socket(), module(), module(), any())
	-> {ok, pid()}.
start_protocol(ListenerPid, Socket, Transport, Protocol, Opts) ->
	Protocol:start_link(ListenerPid, Socket, Transport, Opts).

%% supervisor.

init(Ref) ->
	ok = ranch_server:set_connections_sup(Ref, self()),
	{ok, {{simple_one_for_one, 0, 1}, [{?MODULE, {?MODULE, start_protocol, []},
		temporary, brutal_kill, worker, [?MODULE]}]}}.
