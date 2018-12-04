%% @doc Supervisor for PostgreSQL connections.
-module(pgsql_connection_sup).
-vsn("1").
-behaviour(supervisor).

-export([
         start_link/0,
         start_child/1,
         init/1
        ]).

-define(SHUTDOWN_DELAY, 5000).
% no more than 5 restarts per second.
-define(MAX_RESTARTS, 5).
-define(MAX_RESTARTS_PERIOD, 1).

%%--------------------------------------------------------------------
%% @doc Start the supervisor.
%%
%% @spec(start() -> {ok, pid()} | {error, tuple()})
-spec start_link() -> {ok, pid()} | {error, tuple()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%--------------------------------------------------------------------
%% @doc supervisor init callback.
%%
-spec init(any()) -> {ok, {{supervisor:strategy(), non_neg_integer(), pos_integer()}, [supervisor:child_spec()]}}.
init(_Args) ->
    ChildSpec = {pgsql_connection, % id (ignored).
                 {pgsql_connection, start_link, []}, % init function
                 temporary, % do not restart children that crash
                 ?SHUTDOWN_DELAY, worker,
                 [pgsql_proto] % modules
                },
    RestartStrategy = {simple_one_for_one, ?MAX_RESTARTS, ?MAX_RESTARTS_PERIOD},
    {ok, {RestartStrategy, [ChildSpec]}}.

%%----------------------------------------------------------------
%% @doc Start a server.
%%
-spec start_child([{atom(), string()}]) -> {ok, pid()} | {error, any()}.
start_child(Options) ->
    supervisor:start_child(?MODULE, [Options]).
