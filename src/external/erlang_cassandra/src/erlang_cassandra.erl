%%%-------------------------------------------------------------------
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright (C) 2013 Mahesh Paolini-Subramanya
%%% @doc Thrift based cassandra client
%%%       - Keyspaces map to ServerRef - each keyspace has a
%%%         pool associated with it
%%%       - Only binaries as input. No strings
%%%         
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------

-module(erlang_cassandra).
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').

-behaviour(gen_server).

-include("erlang_cassandra.hrl").

%% API
-export([start/0, start/1]).
-export([stop/0, stop/1]).
-export([start_link/1]).
-export([stop_pool/1]).
-export([start_pool/1, start_pool/2, start_pool/3]).
-export([start_cql_pool/1, start_cql_pool/2, start_cql_pool/3]).
-export([stop_cql_pool/1]).
-export([registered_pool_name/1]).

-export([set_keyspace/1, set_keyspace/2]).
-export([describe_keyspace/1, describe_keyspace/2]).
-export([system_add_keyspace/1, system_add_keyspace/2]).
-export([system_update_keyspace/1, system_update_keyspace/2]).
-export([system_drop_keyspace/1, system_drop_keyspace/2]).

-export([insert/5]).
-export([get/4]).
-export([remove/5]).

-export([system_add_column_family/1, system_add_column_family/2]).
-export([system_describe_column_family/2]).
-export([system_drop_column_family/2]).
-export([system_update_column_family/1, system_update_column_family/2]).
-export([truncate/2]).

-export([add/5]).
-export([remove_counter/4]).

-export([get_slice/5]).
-export([multiget_slice/5]).
-export([get_count/5]).
-export([multiget_count/5]).
-export([get_range_slices/5]).
-export([get_indexed_slices/5]).

-export([execute_cql_query/3]).
-export([prepare_cql_query/3]).
-export([execute_prepared_cql_query/3]).

-export([describe_version/0, describe_version/1]).
-export([describe_snitch/0, describe_snitch/1]).
-export([describe_partitioner/0, describe_partitioner/1]).
-export([describe_schema_versions/0, describe_schema_versions/1]).
-export([describe_keyspaces/0, describe_keyspaces/1]).
-export([describe_cluster_name/0, describe_cluster_name/1]).
-export([describe_ring/1, describe_ring/2]).

%% Helpers
-export([timestamp/0]).
%% Cassandra Records
-export([column_path/3]).
-export([column_parent/1, column_parent/2]).
-export([column/4]).
-export([counter_column/2]).
-export([column_family_definition/2, column_family_definition/3]).
-export([keyspace_definition/1, keyspace_definition/2]).
-export([slice_predicate/3, slice_predicate/4]).
-export([key_range/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(APP, ?MODULE).

-record(state, {
        keyspace                            :: keyspace(),
        set_keyspace                        :: boolean(),
        connection_options                  :: params(),
        connection                          :: connection()}).

%% ------------------------------------------------------------------
%% API
%% ------------------------------------------------------------------

%% @doc Start the application and all its dependencies.
-spec start() -> ok.
start() ->
    reltool_util:application_start(?APP).

%% @doc To start up a 'simple' client
-spec start(params()) -> {ok, pid()}.
start(Options) when is_list(Options) ->
    gen_server:start(?MODULE, [Options], []).

%% @doc Stop the application and all its dependencies.
-spec stop() -> ok.
stop() ->
    reltool_util:application_stop(?APP).

%% @doc Stop this gen_server
-spec stop(server_ref()) -> ok | error().
stop(ServerRef) ->
    gen_server:call(ServerRef, {stop}, ?POOL_TIMEOUT).


%% @doc Used by Poolboy, to start 'unregistered' gen_servers
start_link(ConnectionOptions) ->
    gen_server:start_link(?MODULE, [ConnectionOptions], []).

%% @doc Name used to register the pool server
-spec registered_pool_name(pool_name()) -> registered_pool_name().
registered_pool_name(PoolName) when is_binary(PoolName) ->
    binary_to_atom(<<?REGISTERED_NAME_PREFIX, PoolName/binary, ".pool">>, utf8);
registered_pool_name({Host, Port, PoolName}) ->
    BHost = binary_host(Host),
    BPort = binary_port(Port),
    binary_to_atom(<<?REGISTERED_NAME_PREFIX, BHost/binary, "_", BPort/binary, "_", PoolName/binary, ".pool">>, utf8).


%% @doc Start a poolboy instance
-spec start_pool(pool_name()) -> supervisor:startchild_ret().
start_pool(PoolName) ->
    PoolOptions = application:get_env(erlang_cassandra, pool_options, ?DEFAULT_POOL_OPTIONS),
    ConnectionOptions = application:get_env(erlang_cassandra, connection_options, ?DEFAULT_CONNECTION_OPTIONS),
    start_pool(PoolName, PoolOptions, ConnectionOptions).

%% @doc Start a poolboy instance
-spec start_pool(pool_name(), params()) -> supervisor:startchild_ret().
start_pool(PoolName, PoolOptions) when is_list(PoolOptions) ->
    ConnectionOptions = application:get_env(erlang_cassandra, connection_options, ?DEFAULT_CONNECTION_OPTIONS),
    start_pool(PoolName, PoolOptions, ConnectionOptions).

%% @doc Start a poolboy instance with appropriate Pool & Conn settings
-spec start_pool(pool_name(), params(), params()) -> supervisor:startchild_ret().
start_pool(PoolName, PoolOptions, ConnectionOptions) when is_list(PoolOptions),
                                                          is_list(ConnectionOptions) ->
    erlang_cassandra_poolboy_sup:start_pool(fq_server_ref(PoolName), PoolOptions, ConnectionOptions).


%% @doc Start a pool for cql queries.
%%      Its, by default, of size 1 in case you plan on using prepared queries
%%          (they are saved on a per connection basis)
-spec start_cql_pool(pool_name()) -> supervisor:startchild_ret().
start_cql_pool(PoolName) ->
    PoolOptions = application:get_env(erlang_cassandra, cql_pool_options, ?CQL_POOL_OPTIONS),
    ConnectionOptions = application:get_env(erlang_cassandra, connection_options, ?DEFAULT_CONNECTION_OPTIONS),
    start_cql_pool(PoolName, PoolOptions, 
               [{set_keyspace, false} | ConnectionOptions]).

%% @doc Start a poolboy instance
-spec start_cql_pool(pool_name(), params()) -> supervisor:startchild_ret().
start_cql_pool(PoolName, PoolOptions) when is_list(PoolOptions) ->
    ConnectionOptions = application:get_env(erlang_cassandra, connection_options, ?DEFAULT_CONNECTION_OPTIONS),
    start_cql_pool(PoolName, PoolOptions, 
               [{set_keyspace, false} | ConnectionOptions]).

%% @doc Start a poolboy instance with appropriate Pool & Conn settings
-spec start_cql_pool(pool_name(), params(), params()) -> supervisor:startchild_ret().
start_cql_pool(PoolName, PoolOptions, ConnectionOptions) when is_list(PoolOptions),
                                                                 is_list(ConnectionOptions) ->
    erlang_cassandra_poolboy_sup:start_pool(fq_server_ref(PoolName), PoolOptions, 
                                            [{set_keyspace, false} | ConnectionOptions]).


%% @doc Stop a poolboy instance
-spec stop_pool(pool_name()) -> ok | error().
stop_pool(PoolName) ->
    erlang_cassandra_poolboy_sup:stop_pool(fq_server_ref(PoolName)).


%% @doc Stop a poolboy instance
-spec stop_cql_pool(pool_name()) -> ok | error().
stop_cql_pool(PoolName) ->
    erlang_cassandra_poolboy_sup:stop_pool(fq_server_ref(PoolName)).


%% @doc Set the keyspace to be used by the connection
-spec set_keyspace(destination()) -> response().
set_keyspace(Keyspace) when is_binary(Keyspace) ->
    set_keyspace(Keyspace, Keyspace);
set_keyspace({_Host, _Port, Keyspace} = Destination) ->
    set_keyspace(Destination, Keyspace).

-spec set_keyspace(destination(), keyspace()) -> response().
set_keyspace(Destination, Keyspace) ->
    route_call(Destination, {set_keyspace, [Keyspace]}, ?POOL_TIMEOUT).

%% @doc Describe the keyspace used by the connection
-spec describe_keyspace(destination()) -> response().
describe_keyspace(Keyspace) when is_binary(Keyspace) ->
    describe_keyspace(Keyspace, Keyspace);
describe_keyspace({_Host, _Port, Keyspace} = Destination) ->
    describe_keyspace(Destination, Keyspace).

-spec describe_keyspace(destination(), keyspace()) -> response().
describe_keyspace(Destination, Keyspace) ->
    route_call(Destination, {describe_keyspace, [Keyspace]}, ?POOL_TIMEOUT).

%% @doc Add a keyspace
-spec system_add_keyspace(keyspace_definition()) -> response().
system_add_keyspace(KeyspaceDefinition) ->
    system_add_keyspace(?DEFAULT_KEYSPACE_OPS_POOL, KeyspaceDefinition).

-spec system_add_keyspace(destination(), keyspace_definition()) -> response().
system_add_keyspace(Destination, KeyspaceDefinition) ->
    route_call(Destination, {system_add_keyspace, KeyspaceDefinition}, ?POOL_TIMEOUT).

%% @doc Update a keyspace
-spec system_update_keyspace(keyspace_definition()) -> response().
system_update_keyspace(KeyspaceDefinition) ->
    Keyspace = KeyspaceDefinition#ksDef.name,
    system_update_keyspace(Keyspace, KeyspaceDefinition).

-spec system_update_keyspace(destination(), keyspace_definition()) -> response().
system_update_keyspace(Destination, KeyspaceDefinition) ->
    route_call(Destination, {system_update_keyspace, KeyspaceDefinition}, ?POOL_TIMEOUT).

%% @doc Remove a keyspace
-spec system_drop_keyspace(destination()) -> response().
system_drop_keyspace(Keyspace) when is_binary(Keyspace) ->
    system_drop_keyspace(?DEFAULT_KEYSPACE_OPS_POOL, Keyspace);
system_drop_keyspace({Host, Port, Keyspace} = _Destination) ->
    system_drop_keyspace({Host, Port, ?DEFAULT_KEYSPACE_OPS_POOL}, Keyspace).

-spec system_drop_keyspace(destination(), keyspace()) -> response().
system_drop_keyspace(Destination, Keyspace) ->
    Response = route_call(Destination, {system_drop_keyspace, Keyspace}, ?POOL_TIMEOUT),
    stop_pool(Keyspace),
    Response.

%% @doc Insert a column
-spec insert(destination(), row_key(), column_parent(), column(), consistency_level()) -> response().
insert(Destination, RowKey, ColumnParent, Column, ConsistencyLevel) ->
    route_call(Destination, {insert, RowKey, ColumnParent, Column, ConsistencyLevel}, ?POOL_TIMEOUT).

%% @doc Get a column
-spec get(destination(), row_key(), column_path(), consistency_level()) -> response().
get(Destination, RowKey, ColumnPath, ConsistencyLevel) ->
    route_call(Destination, {get, RowKey, ColumnPath, ConsistencyLevel}, ?POOL_TIMEOUT).

%% @doc Remove data from the row specified by key at the granularity 
%%      specified by column_path, and the given timestamp
-spec remove(destination(), row_key(), column_path(), column_timestamp(), 
                           consistency_level()) -> response().
remove(Destination, RowKey, ColumnPath, ColumnTimestamp, ConsistencyLevel) ->
    route_call(Destination, {remove, RowKey, ColumnPath, ColumnTimestamp, 
                           ConsistencyLevel}, ?POOL_TIMEOUT).

%% @doc Add a column family
-spec system_add_column_family(column_family_definition()) -> response().
system_add_column_family(ColumnFamilyDefinition) ->
    Keyspace = ColumnFamilyDefinition#cfDef.keyspace,
    route_call(Keyspace, {system_add_column_family, ColumnFamilyDefinition}, ?POOL_TIMEOUT).

-spec system_add_column_family(destination(), column_family_definition()) -> response().
system_add_column_family(Destination, ColumnFamilyDefinition) ->
    route_call(Destination, {system_add_column_family, ColumnFamilyDefinition}, ?POOL_TIMEOUT).

%% @doc Get the column family definition
-spec system_describe_column_family(destination(), column_family()) -> response().
system_describe_column_family(Destination, ColumnFamily) ->
    {ok, KeyspaceDefinition} = describe_keyspace(Destination),
    Result = 
    case lists:keyfind(ColumnFamily, #cfDef.name, KeyspaceDefinition#ksDef.cf_defs) of
        false -> undefined;
        Value -> Value
    end,
    {ok, Result}.


%% @doc Drop a column family
-spec system_drop_column_family(destination(), column_family()) -> response().
system_drop_column_family(Destination, ColumnFamily) ->
    route_call(Destination, {system_drop_column_family, ColumnFamily}, ?POOL_TIMEOUT).

%% @doc Update a column family
-spec system_update_column_family(column_family_definition()) -> response().
system_update_column_family(ColumnFamilyDefinition) ->
    Keyspace = ColumnFamilyDefinition#cfDef.keyspace,
    route_call(Keyspace, {system_update_column_family, ColumnFamilyDefinition}, ?POOL_TIMEOUT).

-spec system_update_column_family(destination(), column_family_definition()) -> response().
system_update_column_family(Destination, ColumnFamilyDefinition) ->
    route_call(Destination, {system_update_column_family, ColumnFamilyDefinition}, ?POOL_TIMEOUT).

%% @doc Remove all rows from a column family
-spec truncate(destination(), column_family()) -> response().
truncate(Destination, ColumnFamily) ->
    route_call(Destination, {truncate, ColumnFamily}, ?POOL_TIMEOUT).

%% @doc Increment a counter column
-spec add(destination(), row_key(), column_parent(), counter_column(), consistency_level()) -> response().
add(Destination, RowKey, ColumnParent, CounterColumn, ConsistencyLevel) ->
    route_call(Destination, {add, RowKey, ColumnParent, CounterColumn,
                           ConsistencyLevel}, ?POOL_TIMEOUT).

%% @doc Remove a counter
-spec remove_counter(destination(), row_key(), column_path(), consistency_level()) -> response().
remove_counter(Destination, RowKey, ColumnPath, ConsistencyLevel) ->
    route_call(Destination, {remove_counter, RowKey, ColumnPath, ConsistencyLevel}, ?POOL_TIMEOUT).

%% @doc Get a group of columns based on a slice
-spec get_slice(destination(), row_key(), column_parent(), slice_predicate(), consistency_level()) -> response().
get_slice(Destination, RowKey, ColumnParent, SlicePredicate, ConsistencyLevel) ->
    route_call(Destination, {get_slice, RowKey, ColumnParent, SlicePredicate,
                           ConsistencyLevel}, ?POOL_TIMEOUT).

%% @doc Get a group of columns based on a slice and a list of rows
-spec multiget_slice(destination(), [row_key()], column_parent(), slice_predicate(), consistency_level()) -> response().
multiget_slice(Destination, RowKeys, ColumnParent, SlicePredicate, ConsistencyLevel) when is_list(RowKeys) ->
    route_call(Destination, {multiget_slice, RowKeys, ColumnParent, SlicePredicate,
                           ConsistencyLevel}, ?POOL_TIMEOUT).

%% @doc Count columns based on a slice
%%      WARNING: NOT O(1)
-spec get_count(destination(), row_key(), column_parent(), slice_predicate(), consistency_level()) -> response().
get_count(Destination, RowKey, ColumnParent, SlicePredicate, ConsistencyLevel) ->
    route_call(Destination, {get_count, RowKey, ColumnParent, SlicePredicate,
                           ConsistencyLevel}, ?POOL_TIMEOUT).

%% @doc Count columns based on a slice and a list of rows
%%      WARNING: NOT O(1)
-spec multiget_count(destination(), [row_key()], column_parent(), slice_predicate(), consistency_level()) -> response().
multiget_count(Destination, RowKeys, ColumnParent, SlicePredicate, ConsistencyLevel) when is_list(RowKeys) ->
    route_call(Destination, {multiget_count, RowKeys, ColumnParent, SlicePredicate,
                           ConsistencyLevel}, ?POOL_TIMEOUT).

%% @doc Get a list of slices for the keys within the specified KeyRange
-spec get_range_slices(destination(),  column_parent(), slice_predicate(), key_range(), consistency_level()) -> response().
get_range_slices(Destination, ColumnParent, SlicePredicate, KeyRange, ConsistencyLevel) ->
    route_call(Destination, {get_range_slices, ColumnParent, SlicePredicate, KeyRange,
                           ConsistencyLevel}, ?POOL_TIMEOUT).

%% @doc Get a list of slices using IndexRange
-spec get_indexed_slices(destination(),  column_parent(), slice_predicate(), key_range(), consistency_level()) -> response().
get_indexed_slices(Destination, ColumnParent, IndexClause, SlicePredicate, ConsistencyLevel) ->
    route_call(Destination, {get_indexed_slices, ColumnParent, IndexClause, SlicePredicate,
                           ConsistencyLevel}, ?POOL_TIMEOUT).

%% @doc Execute a CQL query
-spec execute_cql_query(pool_name(), cql_query(), compression()) -> response().
execute_cql_query(CqlPool, CqlQuery, Compression) when is_binary(CqlQuery) ->
    route_call(CqlPool, {execute_cql_query, CqlQuery, Compression}, ?POOL_TIMEOUT).

%% @doc Prepare a CQL query
-spec prepare_cql_query(pool_name(), cql_query(), compression()) -> response().
prepare_cql_query(CqlPool, CqlQuery, Compression) when is_binary(CqlQuery) ->
    route_call(CqlPool, {prepare_cql_query, CqlQuery, Compression}, ?POOL_TIMEOUT).

%% @doc Execute a prepared a CQL query
-spec execute_prepared_cql_query(pool_name(), cql_query_id(), list()) -> response().
execute_prepared_cql_query(CqlPool, CqlQuery, Values) when is_integer(CqlQuery),
                                                           is_list(Values) ->
    route_call(CqlPool, {execute_prepared_cql_query, CqlQuery, Values}, ?POOL_TIMEOUT).

%% @doc Get the Thrift API version
-spec describe_version() -> response().
describe_version() ->
    describe_version(?DEFAULT_KEYSPACE_OPS_POOL).

%% @doc Get the Thrift API version
-spec describe_version(destination()) -> response().
describe_version(Destination) ->
    route_call(Destination, {describe_version}, ?POOL_TIMEOUT).

%% @doc Get the snitch used for the cluster
-spec describe_snitch() -> response().
describe_snitch() ->
    describe_snitch(?DEFAULT_KEYSPACE_OPS_POOL).

%% @doc Get the Thrift API snitch
-spec describe_snitch(destination()) -> response().
describe_snitch(Destination) ->
    route_call(Destination, {describe_snitch}, ?POOL_TIMEOUT).

%% @doc Get the partitioner used for the cluster
-spec describe_partitioner() -> response().
describe_partitioner() ->
    describe_partitioner(?DEFAULT_KEYSPACE_OPS_POOL).

%% @doc Get the Thrift API partitioner
-spec describe_partitioner(destination()) -> response().
describe_partitioner(Destination) ->
    route_call(Destination, {describe_partitioner}, ?POOL_TIMEOUT).

%% @doc Get the schema_versions used for the cluster
-spec describe_schema_versions() -> response().
describe_schema_versions() ->
    describe_schema_versions(?DEFAULT_KEYSPACE_OPS_POOL).

%% @doc Get the Thrift API schema_versions
-spec describe_schema_versions(destination()) -> response().
describe_schema_versions(Destination) ->
    route_call(Destination, {describe_schema_versions}, ?POOL_TIMEOUT).

%% @doc Get the cluster_name 
-spec describe_cluster_name() -> response().
describe_cluster_name() ->
    describe_cluster_name(?DEFAULT_KEYSPACE_OPS_POOL).

%% @doc Get the Thrift API cluster_name
-spec describe_cluster_name(destination()) -> response().
describe_cluster_name(Destination) ->
    route_call(Destination, {describe_cluster_name}, ?POOL_TIMEOUT).


%% @doc Get the list of all the keyspaces
-spec describe_keyspaces() -> response().
describe_keyspaces() ->
    describe_keyspaces(?DEFAULT_KEYSPACE_OPS_POOL).

%% @doc Get the Thrift API keyspaces
-spec describe_keyspaces(destination()) -> response().
describe_keyspaces(Destination) ->
    route_call(Destination, {describe_keyspaces}, ?POOL_TIMEOUT).

%% @doc Gets the token ring; a map of ranges to host addresses
-spec describe_ring(destination()) -> response().
describe_ring(Keyspace) when is_binary(Keyspace) ->
    describe_ring(Keyspace, Keyspace);
describe_ring({_Host, _Port, Keyspace} = Destination) ->
    describe_ring(Destination, Keyspace).

-spec describe_ring(destination(), keyspace()) -> response().
describe_ring(Destination, Keyspace) ->
    route_call(Destination, {describe_ring, [Keyspace]}, ?POOL_TIMEOUT).

-spec timestamp() -> integer().
timestamp() ->
    {Megasecs, Secs, Microsecs} = os:timestamp(),
    Megasecs * 1000000000 + Secs * 1000 + Microsecs div 1000.

%% Helpful Record Functions
-spec column_path(column_family(), super_column(), column_name()) -> column_path().
column_path(ColumnFamily, SuperColumn, ColumnName) when is_binary(ColumnFamily),
                                                        is_binary(ColumnName) ->
    #columnPath{column_family = ColumnFamily,
                 super_column = SuperColumn,
                 column = ColumnName}.

-spec column_parent(column_family()) -> column_parent().
column_parent(ColumnFamily) when is_binary(ColumnFamily) ->
    column_parent(ColumnFamily, undefined).

-spec column_parent(column_family(), super_column()) -> column_parent().
column_parent(ColumnFamily, SuperColumn) when is_binary(ColumnFamily) ->
    #columnParent{column_family = ColumnFamily,
                  super_column = SuperColumn}.

-spec column(column_name(), column_value(), column_timestamp(), column_ttl()) -> column().
column(Name, Value, Timestamp, Ttl) when is_binary(Name),
                                         is_binary(Value),
                                         is_integer(Timestamp) ->
    #column{name = Name,
            value = Value,
            timestamp = Timestamp,
            ttl = Ttl}.

-spec counter_column(counter_column_name(), counter_column_value()) -> counter_column().
counter_column(Name, Value) when is_binary(Name),
                                                 is_integer(Value) ->
    #counterColumn{name = Name,
                   value = Value}.

-spec column_family_definition(destination(), column_family()) -> column_family_definition().
column_family_definition({_, _, Keyspace} = Destination, ColumnFamily) when is_binary(Keyspace),
                                                                            is_binary(ColumnFamily) ->
    column_family_definition(Destination, ColumnFamily, false);
column_family_definition(Keyspace, ColumnFamily) when is_binary(Keyspace),
                                                      is_binary(ColumnFamily) ->
    column_family_definition(Keyspace, ColumnFamily, false).

-spec column_family_definition(destination(), column_family(), is_counter_column()) -> column_family_definition().
column_family_definition({_, _, Keyspace}, ColumnFamily, Bool) when is_binary(Keyspace),
                                                                    is_binary(ColumnFamily),
                                                                    is_boolean(Bool) ->
    column_family_definition(Keyspace, ColumnFamily, Bool);
column_family_definition(Keyspace, ColumnFamily, false) when is_binary(Keyspace),
                                                      is_binary(ColumnFamily) ->
    #cfDef{keyspace = Keyspace,
           name = ColumnFamily,
           default_validation_class = <<"UTF8Type">>
          };
column_family_definition(Keyspace, ColumnFamily, true) when is_binary(Keyspace),
                                                      is_binary(ColumnFamily) ->
    #cfDef{keyspace = Keyspace,
           name = ColumnFamily,
           default_validation_class = <<"CounterColumnType">>
          }.

-spec keyspace_definition(destination()) -> keyspace_definition().
keyspace_definition({_, _, Keyspace} = Destination) when is_binary(Keyspace) ->
    keyspace_definition(Destination, 1);
keyspace_definition(Keyspace) when is_binary(Keyspace) ->
    keyspace_definition(Keyspace, 1).

-spec keyspace_definition(destination(), replication_factor()) -> keyspace_definition().
keyspace_definition({_, _, Keyspace}, ReplicationFactor) when is_binary(Keyspace),
                                                              is_integer(ReplicationFactor) ->
    keyspace_definition(Keyspace, ReplicationFactor);
keyspace_definition(Keyspace, ReplicationFactor) when is_binary(Keyspace),
                                                      is_integer(ReplicationFactor) ->
    #ksDef{name=Keyspace, 
           strategy_class="org.apache.cassandra.locator.SimpleStrategy",
           strategy_options = dict:store("replication_factor", integer_to_list(ReplicationFactor), dict:new())}.

-spec slice_predicate(undefined | [column_name()], slice_start(), slice_end()) -> slice_predicate().
slice_predicate(ColumnNames, Start, End) ->
    slice_predicate(ColumnNames, Start, End, ?DEFAULT_SLICE_COUNT).
-spec slice_predicate(undefined | [column_name()], slice_start(), slice_end(), slice_count()) -> slice_predicate().
slice_predicate(ColumnNames, Start, End, Count) ->
    Range = #sliceRange{start = Start, 
                        finish = End, 
                        count = Count},
    #slicePredicate{column_names = ColumnNames,
                    slice_range = Range}.

-spec key_range(row_key(), row_key()) -> key_range().
key_range(StartKey, EndKey) ->
    #keyRange{start_key = StartKey,
               end_key = EndKey}.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([ConnectionOptions0]) ->
    {Keyspace1, ConnectionOptions2} = 
    case lists:keytake(keyspace, 1, ConnectionOptions0) of
        {value, {keyspace, Keyspace0}, ConnectionOptions1} -> 
            {Keyspace0, ConnectionOptions1};
        false ->
            {?DEFAULT_KEYSPACE_OPS_POOL, ConnectionOptions0}
    end,
    {SetKeyspace, ConnectionOptions4} = 
    case lists:keytake(set_keyspace, 1, ConnectionOptions2) of
        {value, {set_keyspace, Bool}, ConnectionOptions3} -> 
            {Bool, ConnectionOptions3};
        false ->
            % By default, set the keyspace
            {true, ConnectionOptions2}
    end,
    Connection0 = connection(ConnectionOptions4),
    State0 = #state{keyspace = Keyspace1, 
                    set_keyspace = SetKeyspace,
                    connection_options = ConnectionOptions4,
                    connection = Connection0},
    {Connection1, _Response} = 
    case SetKeyspace of 
        true ->
            % On startup, set the keyspace for this connection
            process_request(Connection0, {set_keyspace, [Keyspace1]}, State0);
        false ->
            {Connection0, ok}
    end,
    {ok, State0#state{connection = Connection1}}.

handle_call({stop}, _From, State) ->
    thrift_client:close(State#state.connection),
    {stop, normal, ok, State};

handle_call(Command, _From, State = #state{connection = Connection0}) ->
    Request = request(Command),
    {Connection1, Response} = process_request(Connection0, Request, State),
    {reply, Response, State#state{connection = Connection1}};

handle_call(_Request, _From, State) ->
    thrift_client:close(State#state.connection),
    {stop, unhandled_call, State}.

handle_cast(_Request, State) ->
    thrift_client:close(State#state.connection),
    {stop, unhandled_info, State}.

handle_info(_Info, State) ->
    thrift_client:close(State#state.connection),
    {stop, unhandled_info, State}.

terminate(_Reason, State) ->
    thrift_client:close(State#state.connection),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
%% @doc Build a new connection
-spec connection(params()) -> connection().
connection(ConnectionOptions) ->
    ThriftHost = proplists:get_value(thrift_host, ConnectionOptions, ?DEFAULT_THRIFT_HOST),
    ThriftPort = proplists:get_value(thrift_port, ConnectionOptions, ?DEFAULT_THRIFT_PORT),
    ThriftOptions1 = case lists:keyfind(thrift_options, 1, ConnectionOptions) of
        {thrift_options, Options} -> Options;
        false -> ?DEFAULT_THRIFT_OPTIONS
    end,
    % Require framing to be true
    ThriftOptions2 = lists:keydelete(framed, 1, ThriftOptions1),
    ThriftOptions3 = [{framed, true} | ThriftOptions2],
    try
        {ok, Connection} = thrift_client_util:new(ThriftHost, ThriftPort, cassandra_thrift, ThriftOptions3),
        Connection
    catch
        _:_ -> undefined
    end.

%% @doc Create a Thrift command based on the function and arguments
-spec request(request()) -> request().

request({F}) ->
    % No arguments is actually an empty list
    {F, []};
request({F, A1}) ->
    {F, [A1]};
request({F, A1, A2}) ->
    {F, [A1, A2]};
request({F, A1, A2, A3}) ->
    {F, [A1, A2, A3]};
request({F, A1, A2, A3, A4}) ->
    {F, [A1, A2, A3, A4]}.

%% @doc Process the request over thrift
%%      In case the network blipped and the thrift connection vanishes,
%%      this will retry the request (w/ a new thrift connection)
%%      before choking
-spec process_request(connection(), request(), #state{}) -> {connection(), response()}.
process_request(Connection, {Function, Args}, State) ->
    try process_request_1(_Retry = true, Connection, {Function, Args}, State)
    catch
        Exception:Reason ->
            case {Exception, Reason} of
                {throw, {retry_request, true}} ->
                    process_request_1(false, undefined, {Function, Args}, State);
                _ ->
                    {undefined, ?CONNECTION_REFUSED}
            end
    end.

%% @doc Actually perform the request
-spec process_request_1(boolean(), connection(), request(), #state{}) -> {connection(), response()}.
process_request_1(Retry, undefined, {Function, Args}, State = #state{connection_options = ConnectionOptions}) ->
    Connection = connection(ConnectionOptions),
    do_request(Retry, Connection, {Function, Args}, State);
process_request_1(Retry, Connection, {Function, Args}, State) ->
    do_request(Retry, Connection, {Function, Args}, State).

-spec do_request(boolean(), connection(), request(), #state{}) -> {connection(),  response()}.
do_request(Retry, Connection, {Function, Args}, _State) ->
    try thrift_client:call(Connection, Function, Args) of
        {Connection1, Response = {ok, _}} ->
            {Connection1, Response};
        {Connection1, Response = {error, _}} ->
            {Connection1, Response}
    catch
        Exception:Reason ->
            case {Exception, Reason} of
                {throw, {Connection1, Response = {exception, _}}} ->
                    {Connection1, Response};
                % Thrift client closes the connection
                {error, {case_clause, {error, closed}}} ->
                    if Retry =:= false -> {undefined, ?CONNECTION_REFUSED};
                        true -> throw({retry_request, Retry})
                    end;
                {error, {case_clause,{error, econnrefused}}} ->
                    if Retry =:= false -> {undefined, ?CONNECTION_REFUSED};
                        true -> throw({retry_request, Retry})
                    end;
                {error, badarg} ->
                    {Connection, {error, badarg}}

            end
    end.

%% @doc Send the request to the gen_server
-spec route_call(destination(), tuple(), timeout()) -> response().
% When this comes back from Poolboy, ServerRef is a pid
%       optionally, atom for internal testing
route_call(ServerRef, Command = {_Function}, Timeout) when is_atom(ServerRef); is_pid(ServerRef) ->
    gen_server:call(ServerRef, Command, Timeout);
route_call(ServerRef, {Function, [[A1]]}, Timeout) when is_atom(ServerRef); is_pid(ServerRef) ->
    route_call(ServerRef, {Function, [A1]}, Timeout);
route_call(ServerRef, {Function, [A1]}, Timeout) when is_atom(ServerRef); is_pid(ServerRef) ->
    route_call(ServerRef, {Function, A1}, Timeout);
route_call(ServerRef, Command = {_Function, _A1}, Timeout) when is_atom(ServerRef); is_pid(ServerRef) ->
    gen_server:call(ServerRef, Command, Timeout);
route_call(ServerRef, Command = {_Function, _A1, _A2}, Timeout) when is_atom(ServerRef); is_pid(ServerRef) ->
    gen_server:call(ServerRef, Command, Timeout);
route_call(ServerRef, Command = {_Function, _A1, _A2, _A3}, Timeout) when is_atom(ServerRef); is_pid(ServerRef) ->
    gen_server:call(ServerRef, Command, Timeout);
route_call(ServerRef, Command = {_Function, _A1, _A2, _A3, _A4}, Timeout) when is_atom(ServerRef); is_pid(ServerRef) ->
    gen_server:call(ServerRef, Command, Timeout);
%% @doc Send the request to poolboy
route_call(Destination, Command = {_Function}, Timeout) ->
    pool_call(fq_server_ref(Destination), Command, Timeout);
route_call(Destination, Command = {_Function, _A1}, Timeout) ->
    pool_call(fq_server_ref(Destination), Command, Timeout);
route_call(Destination, Command = {_Function, _A1, _A2}, Timeout) ->
    pool_call(fq_server_ref(Destination), Command, Timeout);
route_call(Destination, Command = {_Function, _A1, _A2, _A3}, Timeout) ->
    pool_call(fq_server_ref(Destination), Command, Timeout);
route_call(Destination, Command = {_Function, _A1, _A2, _A3, _A4}, Timeout) ->
    pool_call(fq_server_ref(Destination), Command, Timeout).


-spec pool_call(fq_server_ref(), tuple(), timeout()) ->response().
pool_call(FqServerRef, Command, Timeout) ->
    PoolId = registered_pool_name(FqServerRef),
    TransactionFun = fun() ->
            poolboy:transaction(PoolId, fun(Worker) ->
                        gen_server:call(Worker, Command, Timeout)
                end) end,
    try
        TransactionFun()
        % If the pool doesnt' exist, the keyspace has not been set before
        % Check to make sure that the keyspace exists before starting the 
        % pool
    catch
        exit:{noproc, _Other} ->
            case keyspace_exists(FqServerRef) of
                true ->
                    start_pool(FqServerRef),
                    TransactionFun();
                false -> 
                    {_, _, Keyspace} = FqServerRef,
                    {error, {?INVALID_KEYSPACE, Keyspace}}
            end
    end.

keyspace_exists(Destination) ->
    Keyspace = keyspace_from_destination(Destination),
    FqDefaultServerRef = fq_default_server_ref(Destination),
    % Make sure that the default pool exists. Should usually do so, 
    % but if alternate thrift hosts/ports are specified, need
    % to make sure that the correct pool is used
    start_pool(FqDefaultServerRef),
    case safe_describe_keyspace(FqDefaultServerRef, Keyspace) of
        {ok, _} -> true;
        _Other -> 
            false
    end.

% Don't want to describe the default keyspace - it doesn't exist
safe_describe_keyspace(_, ?DEFAULT_KEYSPACE_OPS_POOL) -> {ok, ok};
safe_describe_keyspace(FqDefaultServerRef, Keyspace) ->
    describe_keyspace(FqDefaultServerRef, Keyspace).

%% @doc Fully qualify a server ref w/ a thrift host/port
-spec fq_server_ref(destination()) -> fq_server_ref().
fq_server_ref({Host, Port, Name}) when is_list(Name) -> {Host, Port, list_to_binary(Name)};
fq_server_ref({Host, Port, Name}) when is_binary(Name) -> {Host, Port, Name};
fq_server_ref(Destination) when is_list(Destination) -> {undefined, undefined, list_to_binary(Destination)};
fq_server_ref(Destination) when is_binary(Destination) -> {undefined, undefined, Destination}.

%% @doc Fully qualify the default server ref w/ a thrift host/port
-spec fq_default_server_ref(destination()) -> fq_server_ref().
fq_default_server_ref({Host, Port, _}) -> {Host, Port, ?DEFAULT_KEYSPACE_OPS_POOL}.

%% @doc Get the keyspace from a Destination
-spec keyspace_from_destination(destination()) -> keyspace().
keyspace_from_destination({_, _, Keyspace}) -> Keyspace.

%% If thrift host is passed in, use it
binary_host(Host) when is_list(Host) -> list_to_binary(Host);
binary_host(undefined) -> <<"">>.

%% If thrift port is passed in, use it
binary_port(Port) when is_integer(Port) -> list_to_binary(integer_to_list(Port));
binary_port(undefined) -> <<"">>.
