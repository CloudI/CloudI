%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(chunked_hello_world_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", toppage_h, []}
		]}
	]),
	{ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
		env => #{dispatch => Dispatch}
	}),
	chunked_hello_world_sup:start_link().

stop(_State) ->
	ok = cowboy:stop_listener(http).
