%% @doc Supervisor for the pgsql application.
-module(pgsql_sup).
-vsn("1").
-behaviour(supervisor).

%% Access from supervisor.
-export([start_link/0]).

%% supervisor API.
-export([init/1]).

-define(SHUTDOWN_DELAY, 5000).
% no more than 5 restarts per second.
-define(MAX_RESTARTS, 5).
-define(MAX_RESTARTS_PERIOD, 1).

%% @doc Start the supervisor.
%%
-spec start_link() -> {ok, pid()} | {error, any()}.
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Supervisor init callback.
%%
-spec init(any()) -> {ok, {{supervisor:strategy(), non_neg_integer(), non_neg_integer()}, [supervisor:child_spec()]}}.
init(_Args) ->
    PostgreSQLChildSpec = {pgsql_connection_sup, % id
        {pgsql_connection_sup, start_link, []}, % init function
        transient, % restart children that crash
        ?SHUTDOWN_DELAY, supervisor,
        [pgsql_connection_sup] % module
        },
    RestartStrategy = {one_for_one, ?MAX_RESTARTS, ?MAX_RESTARTS_PERIOD},
    {ok, {RestartStrategy, [PostgreSQLChildSpec]}}.
