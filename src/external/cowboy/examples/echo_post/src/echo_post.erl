%% Feel free to use, reuse and abuse the code in this file.

-module(echo_post).

%% API.
-export([start/0]).

%% API.

start() ->
	ok = application:start(cowboy),
	ok = application:start(echo_post).
