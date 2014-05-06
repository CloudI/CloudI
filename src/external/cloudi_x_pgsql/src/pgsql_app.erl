%% @doc Application for the pgsql application.

-module(pgsql_app).
-vsn(1).
-behaviour(application).

%% application API
-export([start/2, stop/1]).

%% @doc Start the application.
-spec start(normal | {takeover, node()} | {failover, node()}, any()) -> {ok, pid()} | {ok, pid(), any()} | {error, any()}.
start(_Type, _StartArgs) ->
    pgsql_sup:start_link().

%% @doc Stop the application.
-spec stop(any()) -> stop.
stop(_State) ->
    stop.
