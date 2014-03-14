%%%-------------------------------------------------------------------
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright (C) 2013 Mahesh Paolini-Subramanya
%%% @doc Root Supervisor for erlang_cassandra
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(erlang_cassandra_poolboy_sup).
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').

-behaviour(supervisor).

-include("erlang_cassandra.hrl").

%% API
-export([start_link/0]).
-export([start_pool/3]).
-export([stop_pool/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-define(WORKER(Restart, Module, Args), {Module, {Module, start_link, Args}, Restart, 5000, worker, [Module]}).
-define(SUPERVISOR(Restart, Module, Args), {Module, {Module, start_link, Args}, Restart, 5000, supervisor, [Module]}).

-type startlink_err() :: {'already_started', pid()} | 'shutdown' | term().
-type startlink_ret() :: {'ok', pid()} | 'ignore' | {'error', startlink_err()}.

-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_pool(pool_name(), params(), params()) -> supervisor:startchild_ret().
start_pool(PoolName, PoolOptions, ConnectionOptions) when is_list(PoolOptions),
                                                          is_list(ConnectionOptions) ->
    PoolSpec = pool_spec(PoolName, PoolOptions, ConnectionOptions),
    supervisor:start_child(?SERVER, PoolSpec).


-spec stop_pool(pool_name()) -> ok | error().
stop_pool(PoolName) ->
    PoolId = erlang_cassandra:registered_pool_name(PoolName),
    supervisor:terminate_child(?SERVER, PoolId),
    supervisor:delete_child(?SERVER, PoolId).


-spec init(Args :: term()) -> {ok, {{RestartStrategy :: supervisor:strategy(), MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
                                    [ChildSpec :: supervisor:child_spec()]}}.
init([]) ->
    RestartStrategy = one_for_one,
    MaxRestarts = 10,
    MaxSecondsBetweenRestarts = 10,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    Keyspace = application:get_env(erlang_cassandra, keyspace, ?DUMMY_STARTUP_POOL),
    % We need this to be a one_for_one supervisor, because of the way the 
    % connection_options trickle through to the workers (and hence, our
    % gen-server).  To simplify things, I start a default pool of size 0. This
    % ensures that even if Cassandra is not up, the application still starts
    % up.
    % That said, max_overflow is setup so that we can add connections
    % NOTE: You can have the pool actually connect and do something by passing
    % in pool_name / pool_options / connection_options in the environment (or by
    % setting it in your app.config)
    PoolOptions = application:get_env(erlang_cassandra, pool_options, [{size, 0},
                                                        {max_overflow, 10}]),
    ConnectionOptions = application:get_env(erlang_cassandra, connection_options, []),
    PoolSpecs = pool_spec({undefined, undefined, Keyspace}, PoolOptions, [{set_keyspace, false} | ConnectionOptions]),
    {ok, {SupFlags, [PoolSpecs]}}.


-spec pool_spec(pool_name(), params(), params()) -> supervisor:child_spec().
pool_spec({Host, Port, Keyspace} = PoolName, PoolOptions, ConnectionOptions) ->
    PoolId = erlang_cassandra:registered_pool_name(PoolName),
    PoolArgs = [{name, {local, PoolId}},
                {worker_module, erlang_cassandra_poolboy_worker}] ++ PoolOptions,
    ConnectionArgs = keyspace(Keyspace) ++ sanitize_connection_options(thrift_host(Host),
                                                 thrift_port(Port),
                                                 ConnectionOptions),
 
    % Pass in pool_name to the connection_options since this is also the
    %   keyspace, and is needed by init/1
    poolboy:child_spec(PoolId, PoolArgs, ConnectionArgs).

keyspace(Keyspace) when is_binary(Keyspace) -> [{keyspace, Keyspace}].
thrift_host(undefined) -> [];
thrift_host(Host) when is_list(Host) -> [{thrift_host, Host}].
thrift_port(undefined) -> [];
thrift_port(Port) when is_integer(Port) -> [{thrift_port, Port}].

sanitize_connection_options([], [], Options) ->
    Options;
sanitize_connection_options(HostArgs, [], Options) ->
    HostArgs ++ lists:keydelete(thrift_host, 1, Options);
sanitize_connection_options([], PortArgs, Options) ->
    PortArgs ++ lists:keydelete(thrift_port, 1, Options);
sanitize_connection_options(HostArgs, PortArgs, Options) ->
    HostArgs ++ 
    PortArgs ++ 
    lists:keydelete(thrift_host, 1, lists:keydelete(thrift_port, 1, Options)).
