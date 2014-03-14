%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Cassandra Data Module==
%%%
%%%     This ia a _very_ thin shim on top of erlang_cassandra from 
%%%         https://github.com/dieswaytoofast/erlang_cassandra`
%%%     Note that your results are "double ok"'d.  i.e., whatever
%%%     erlang_cassandra sends back is sent back from send_sync.
%%%     As a result, all successful responses will be of the form
%%%         {ok, {ok, Response}}
%%%     All failures from erlang_cassandra will be of the form
%%%         {ok, {error, Reason}}
%%%     send_sync errors will be standard 'cloudi' errors
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2013, Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%% 
%%%     * Redistributions of source code must retain the above copyright
%%%       notice, this list of conditions and the following disclaimer.
%%%     * Redistributions in binary form must reproduce the above copyright
%%%       notice, this list of conditions and the following disclaimer in
%%%       the documentation and/or other materials provided with the
%%%       distribution.
%%%     * All advertising materials mentioning features or use of this
%%%       software must display the following acknowledgment:
%%%         This product includes
%%%         software developed by Mahesh Paolini-Subramanya
%%%     * The name of the author may not be used to endorse or promote
%%%       products derived from this software without specific prior
%%%       written permission
%%% 
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
%%% DAMAGE.
%%%
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright 2013 Mahesh Paolini-Subramanya
%%% @version 1.3.0 {@date} {@time}
%%%------------------------------------------------------------------------
-module(cloudi_service_db_cassandra).
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').

-behaviour(cloudi_service).

%% external interface

%% cassandra API
-export([set_keyspace/3]).
-export([describe_keyspace/3]).
-export([system_add_keyspace/3]).
-export([system_update_keyspace/3]).
-export([system_drop_keyspace/3]).

-export([insert/7]).
-export([get/6]).
-export([remove/7]).

-export([system_add_column_family/3]).
-export([system_describe_column_family/4]).
-export([system_drop_column_family/4]).
-export([system_update_column_family/3]).
-export([truncate/4]).

-export([add/7]).
-export([remove_counter/6]).

-export([get_slice/7]).
-export([multiget_slice/7]).
-export([get_count/7]).
-export([multiget_count/7]).
-export([get_range_slices/7]).
-export([get_indexed_slices/7]).

-export([execute_cql_query/4]).
-export([prepare_cql_query/4]).
-export([execute_prepared_cql_query/4]).

-export([describe_version/2]).
-export([describe_snitch/2]).
-export([describe_partitioner/2]).
-export([describe_schema_versions/2]).
-export([describe_keyspaces/2]).
-export([describe_cluster_name/2]).
-export([describe_ring/3]).

%% cloudi_service callbacks
-export([cloudi_service_init/3,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/2]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include_lib("cloudi_x_erlang_cassandra/include/cloudi_x_erlang_cassandra_types.hrl").

-define(DEFAULT_DATABASE, undefined).
-define(DEFAULT_THRIFT_HOST, "localhost").
-define(DEFAULT_THRIFT_PORT, 9160).
-define(DEFAULT_CONNECTION_NAME, <<"cloudi_default_connection_name">>).
-define(DEFAULT_POOL_OPTIONS, [{size, 5},
                               {max_overflow, 10}
                              ]).

-define(DEFAULT_CONNECTION_OPTIONS, [{thrift_host, ?DEFAULT_THRIFT_HOST},
                                     {thrift_port, ?DEFAULT_THRIFT_PORT}
                                    ]).




-type error()           :: {error, Reason :: term()}.
-type connection_name() :: {any(), any(), string() | binary()} |
                           string() |
                           binary().
-type dispatcher()      :: cloudi_service:dispatcher() | cloudi:context().
-type name()            :: cloudi_service:service_name().
-type thrift_host()     :: undefined | string().
-type thrift_port()     :: undefined | integer().
-type client_name()     :: binary().
-type server_ref()      :: atom() | pid() | client_name().
-type fq_server_ref()   :: {thrift_host(), thrift_port(), server_ref()}.
-type destination()     :: server_ref() | fq_server_ref().


-type keyspace_definition()      ::  #ksDef{}.
-type row_key()         ::  binary().
-type key_range()       ::  #keyRange{}.
-type column_family()   ::  binary().
-type column_timestamp()  :: non_neg_integer().
-type consistency_level() :: non_neg_integer().
-type column()          :: #column{}.
-type column_parent()   :: #columnParent{}.
-type column_path()     :: #columnPath{}.
-type column_family_definition() :: #cfDef{}.
-type counter_column()  :: #counterColumn{}.
-type slice_predicate() :: #slicePredicate{}.
-type cql_query()       :: binary().
-type cql_query_id()    :: integer().
-type compression()     :: binary().

-record(state, {
        connection_name     :: connection_name()
        }).

-type response()        :: [tuple()] | error().


%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%% @doc Set the keyspace to be used by the connection
-spec set_keyspace(dispatcher(), name(), destination()) ->
    response().
set_keyspace(Dispatcher, Name, Destination)
    when is_list(Name) ->
    cloudi:send_sync(Dispatcher, Name,
                     {set_keyspace, Destination}).

%% @doc Describe the keyspace used by the connection
-spec describe_keyspace(dispatcher(), name(), destination()) ->
    response().
describe_keyspace(Dispatcher, Name, Destination)
    when is_list(Name) ->
    cloudi:send_sync(Dispatcher, Name,
                     {describe_keyspace, Destination}).

%% @doc Add a keyspace
-spec system_add_keyspace(dispatcher(), name(), keyspace_definition()) ->
    response().
system_add_keyspace(Dispatcher, Name, KeyspaceDefinition)
    when is_list(Name) ->
    cloudi:send_sync(Dispatcher, Name,
                     {system_add_keyspace, KeyspaceDefinition}).

%% @doc Update a keyspace
-spec system_update_keyspace(dispatcher(), name(), keyspace_definition()) ->
    response().
system_update_keyspace(Dispatcher, Name, KeyspaceDefinition)
    when is_list(Name) ->
    cloudi:send_sync(Dispatcher, Name,
                     {system_update_keyspace, KeyspaceDefinition}).

%% @doc Remove a keyspace
-spec system_drop_keyspace(dispatcher(), name(), destination()) ->
    response().
system_drop_keyspace(Dispatcher, Name, Destination)
    when is_list(Name) ->
    cloudi:send_sync(Dispatcher, Name,
                     {system_drop_keyspace, Destination}).

%% @doc Insert a column
-spec insert(dispatcher(), name(), destination(),
             row_key(), column_parent(), column(), consistency_level()) ->
    response().
insert(Dispatcher, Name, Destination,
       RowKey, ColumnParent, Column, ConsistencyLevel) ->
    cloudi:send_sync(Dispatcher, Name,
                     {insert, Destination, RowKey,
                      ColumnParent, Column, ConsistencyLevel}).

%% @doc Get a column
-spec get(dispatcher(), name(), destination(),
          row_key(), column_path(), consistency_level()) ->
    response().
get(Dispatcher, Name, Destination, RowKey, ColumnPath, ConsistencyLevel) ->
    cloudi:send_sync(Dispatcher, Name,
                     {get, Destination, RowKey, ColumnPath, ConsistencyLevel}).

%% @doc Remove data from the row specified by key at the granularity 
%%      specified by column_path, and the given timestamp
-spec remove(dispatcher(), name(), destination(),
             row_key(), column_path(), column_timestamp(),
             consistency_level()) ->
    response().
remove(Dispatcher, Name, Destination,
       RowKey, ColumnPath, ColumnTimestamp, ConsistencyLevel) ->
    cloudi:send_sync(Dispatcher, Name,
                     {remove, Destination, RowKey, ColumnPath,
                      ColumnTimestamp, ConsistencyLevel}).

%% @doc Add a column family
-spec system_add_column_family(dispatcher(), name(),
                               column_family_definition()) ->
    response().
system_add_column_family(Dispatcher, Name, ColumnFamilyDefinition) ->
    cloudi:send_sync(Dispatcher, Name,
                     {system_add_column_family, ColumnFamilyDefinition}).

%% @doc Get the column family definition
-spec system_describe_column_family(dispatcher(), name(), destination(),
                                    column_family()) ->
    response().
system_describe_column_family(Dispatcher, Name, Destination, ColumnFamily) ->
    cloudi:send_sync(Dispatcher, Name,
                     {system_describe_column_family, Destination,
                      ColumnFamily}).


%% @doc Drop a column family
-spec system_drop_column_family(dispatcher(), name(), destination(),
                                column_family()) ->
    response().
system_drop_column_family(Dispatcher, Name, Destination, ColumnFamily) ->
    cloudi:send_sync(Dispatcher, Name,
                     {system_drop_column_family, Destination, ColumnFamily}).

%% @doc Update a column family
-spec system_update_column_family(dispatcher(), name(),
                                  column_family_definition()) ->
    response().
system_update_column_family(Dispatcher, Name, ColumnFamilyDefinition) ->
    cloudi:send_sync(Dispatcher, Name,
                     {system_update_column_family, ColumnFamilyDefinition}).

%% @doc Remove all rows from a column family
-spec truncate(dispatcher(), name(), destination(), column_family()) ->
    response().
truncate(Dispatcher, Name, Destination, ColumnFamily) ->
    cloudi:send_sync(Dispatcher, Name,
                     {truncate, Destination, ColumnFamily}).

%% @doc Increment a counter column
-spec add(dispatcher(), name(), destination(),
          row_key(), column_parent(), counter_column(), consistency_level()) ->
    response().
add(Dispatcher, Name, Destination,
    RowKey, ColumnParent, CounterColumn, ConsistencyLevel) ->
    cloudi:send_sync(Dispatcher, Name,
                     {add, Destination, RowKey, ColumnParent, CounterColumn,
                      ConsistencyLevel}).

%% @doc Remove a counter
-spec remove_counter(dispatcher(), name(), destination(),
                     row_key(), column_path(), consistency_level()) ->
    response().
remove_counter(Dispatcher, Name, Destination,
               RowKey, ColumnPath, ConsistencyLevel) ->
    cloudi:send_sync(Dispatcher, Name,
                     {remove_counter, Destination,
                      RowKey, ColumnPath, ConsistencyLevel}).

%% @doc Get a group of columns based on a slice
-spec get_slice(dispatcher(), name(), destination(),
                row_key(), column_parent(), slice_predicate(),
                consistency_level()) ->
    response().
get_slice(Dispatcher, Name, Destination,
          RowKey, ColumnParent, SlicePredicate, ConsistencyLevel) ->
    cloudi:send_sync(Dispatcher, Name,
                     {get_slice, Destination,
                      RowKey, ColumnParent, SlicePredicate, ConsistencyLevel}).

%% @doc Get a group of columns based on a slice and a list of rows
-spec multiget_slice(dispatcher(), name(), destination(),
                     [row_key()], column_parent(), slice_predicate(),
                     consistency_level()) ->
    response().
multiget_slice(Dispatcher, Name, Destination,
               RowKeys, ColumnParent, SlicePredicate, ConsistencyLevel)
    when is_list(RowKeys) ->
    cloudi:send_sync(Dispatcher, Name,
                     {multiget_slice, Destination,
                      RowKeys, ColumnParent, SlicePredicate, ConsistencyLevel}).

%% @doc Count columns based on a slice
%%      WARNING: NOT O(1)
-spec get_count(dispatcher(), name(), destination(),
                row_key(), column_parent(), slice_predicate(),
                consistency_level()) ->
    response().
get_count(Dispatcher, Name, Destination,
          RowKey, ColumnParent, SlicePredicate, ConsistencyLevel) ->
    cloudi:send_sync(Dispatcher, Name,
                     {get_count, Destination,
                      RowKey, ColumnParent, SlicePredicate, ConsistencyLevel}).

%% @doc Count columns based on a slice and a list of rows
%%      WARNING: NOT O(1)
-spec multiget_count(dispatcher(), name(), destination(),
                     [row_key()], column_parent(), slice_predicate(),
                     consistency_level()) ->
    response().
multiget_count(Dispatcher, Name, Destination,
               RowKeys, ColumnParent, SlicePredicate, ConsistencyLevel)
    when is_list(RowKeys) ->
    cloudi:send_sync(Dispatcher, Name,
                     {multiget_count, Destination,
                      RowKeys, ColumnParent, SlicePredicate, ConsistencyLevel}).

%% @doc Get a list of slices for the keys within the specified KeyRange
-spec get_range_slices(dispatcher(), name(), destination(),
                       column_parent(), slice_predicate(), key_range(),
                       consistency_level()) ->
    response().
get_range_slices(Dispatcher, Name, Destination,
                 ColumnParent, SlicePredicate, KeyRange, ConsistencyLevel) ->
    cloudi:send_sync(Dispatcher, Name,
                     {get_range_slices, Destination, ColumnParent,
                      SlicePredicate, KeyRange, ConsistencyLevel}).

%% @doc Get a list of slices using IndexRange
-spec get_indexed_slices(dispatcher(), name(), destination(),
                         column_parent(), slice_predicate(), key_range(),
                         consistency_level()) ->
    response().
get_indexed_slices(Dispatcher, Name, Destination,
                   ColumnParent, IndexClause, SlicePredicate,
                   ConsistencyLevel) ->
    cloudi:send_sync(Dispatcher, Name,
                     {get_indexed_slices, Destination,
                      ColumnParent, IndexClause, SlicePredicate,
                      ConsistencyLevel}).

%% @doc Execute a CQL query
-spec execute_cql_query(dispatcher(), name(),
                        cql_query(), compression()) ->
    response().
execute_cql_query(Dispatcher, Name, CqlQuery, Compression)
    when is_binary(CqlQuery) ->
    cloudi:send_sync(Dispatcher, Name,
                     {execute_cql_query, CqlQuery, Compression}).

%% @doc Prepare a CQL query
-spec prepare_cql_query(dispatcher(), name(),
                        cql_query(), compression()) ->
    response().
prepare_cql_query(Dispatcher, Name, CqlQuery, Compression)
    when is_binary(CqlQuery) ->
    cloudi:send_sync(Dispatcher, Name,
                     {prepare_cql_query, CqlQuery, Compression}).

%% @doc Execute a prepared a CQL query
-spec execute_prepared_cql_query(dispatcher(), name(),
                                 cql_query_id(), list()) ->
    response().
execute_prepared_cql_query(Dispatcher, Name, CqlQuery, Values)
    when is_integer(CqlQuery), is_list(Values) ->
    cloudi:send_sync(Dispatcher, Name,
                     {execute_prepared_cql_query, CqlQuery, Values}).

%% @doc Get the Thrift API version
-spec describe_version(dispatcher(), name()) ->
    response().
describe_version(Dispatcher, Name) ->
    cloudi:send_sync(Dispatcher, Name,
                     {describe_version}).

%% @doc Get the snitch used for the cluster
-spec describe_snitch(dispatcher(), name()) ->
    response().
describe_snitch(Dispatcher, Name) ->
    cloudi:send_sync(Dispatcher, Name,
                     {describe_snitch}).

%% @doc Get the partitioner used for the cluster
-spec describe_partitioner(dispatcher(), name()) ->
    response().
describe_partitioner(Dispatcher, Name) ->
    cloudi:send_sync(Dispatcher, Name,
                     {describe_partitioner}).

%% @doc Get the schema_versions used for the cluster
-spec describe_schema_versions(dispatcher(), name()) ->
    response().
describe_schema_versions(Dispatcher, Name) ->
    cloudi:send_sync(Dispatcher, Name,
                     {describe_schema_versions}).

%% @doc Get the cluster_name 
-spec describe_cluster_name(dispatcher(), name()) ->
    response().
describe_cluster_name(Dispatcher, Name) ->
    cloudi:send_sync(Dispatcher, Name,
                     {describe_cluster_name}).

%% @doc Get the list of all the keyspaces
-spec describe_keyspaces(dispatcher(), name()) ->
    response().
describe_keyspaces(Dispatcher, Name) ->
    cloudi:send_sync(Dispatcher, Name,
                     {describe_keyspaces}).

%% @doc Gets the token ring; a map of ranges to host addresses
-spec describe_ring(dispatcher(), name(), destination()) ->
    response().
describe_ring(Dispatcher, Name, Destination) ->
    cloudi:send_sync(Dispatcher, Name,
                     {describe_ring, Destination}).

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, _Prefix, Dispatcher) ->
    Defaults = [{connection_name, ?DEFAULT_CONNECTION_NAME},
                {pool_options, ?DEFAULT_POOL_OPTIONS},
                {connection_options, ?DEFAULT_CONNECTION_OPTIONS}],
    [ConnectionName, PoolOptions, ConnectionOptions] =
        cloudi_proplists:take_values(Defaults, Args),

    % Use the connection_name to create a CQL connection pool. Note that 
    % if thisin this case, pool_options should be '1', unless no
    % prepared statements are going to be used
    case cloudi_x_erlang_cassandra:start_cql_pool(binary_name(ConnectionName),
                                                  PoolOptions,
                                                  ConnectionOptions) of
        {ok, _} ->
            cloudi_service:subscribe(Dispatcher,
                                     subscription_name(ConnectionName)),
            {ok, #state{connection_name = ConnectionName}};
        {error, Reason} -> 
             {stop, Reason}
    end.

cloudi_service_handle_request(_Type, _Name, _Pattern, _RequestInfo, Request,
                              Timeout, _Priority, _TransId, _Pid,
                              #state{connection_name = ConnectionName
                                    } = State,
                              _Dispatcher) ->
    case Request of
        Command when is_binary(Command) ->
            {reply, do_query(Command, ConnectionName, Timeout), State};
        Command ->
            {reply, process_query(ConnectionName, Command), State}
    end.

cloudi_service_handle_info(Request, State, _) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(_, undefined) ->
    ok;
cloudi_service_terminate(_, #state{connection_name = ConnectionName}) ->
    cloudi_x_erlang_cassandra:stop_cql_pool(ConnectionName),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

%% do a single query and return a boolean to determine if the query succeeded
do_query(Query, ConnectionName, _Timeout) ->
    try 
        Command =  cloudi_string:binary_to_term(Query),
        process_query(ConnectionName, Command)
    of
        {error, invalid_call} ->
            ?LOG_DEBUG("Invalid erlang_cassandra command tuple ~p",
                       [binary_to_list(Query)]),
            <<>>;
        Response ->
            cloudi_string:term_to_binary(Response)

    catch
        _:Reason ->
            ?LOG_DEBUG("exception when processing "
                       "erlang_cassandra command tuple ~p: ~p",
                       [binary_to_list(Query), Reason]),
            <<>>
    end.

process_query(_ConnectionName, {'set_keyspace', Destination}) ->
    cloudi_x_erlang_cassandra:set_keyspace(Destination);
process_query(_ConnectionName, {'describe_keyspace', Destination}) ->
    cloudi_x_erlang_cassandra:describe_keyspace(Destination);
process_query(_ConnectionName, {'system_add_keyspace', KeyspaceDefinition}) ->
    cloudi_x_erlang_cassandra:system_add_keyspace(KeyspaceDefinition);
process_query(_ConnectionName, {'system_update_keyspace', KeyspaceDefinition}) ->
    cloudi_x_erlang_cassandra:system_update_keyspace(KeyspaceDefinition);
process_query(_ConnectionName, {'system_drop_keyspace', Destination}) ->
    cloudi_x_erlang_cassandra:system_drop_keyspace(Destination);
process_query(_ConnectionName, {'insert', Destination, RowKey, ColumnParent, Column, ConsistencyLevel}) ->
    cloudi_x_erlang_cassandra:insert(Destination, RowKey, ColumnParent, Column, ConsistencyLevel);
process_query(_ConnectionName, {'get', Destination, RowKey, ColumnPath, ConsistencyLevel}) ->
    cloudi_x_erlang_cassandra:get(Destination, RowKey, ColumnPath, ConsistencyLevel);
process_query(_ConnectionName, {'remove', Destination, RowKey, ColumnPath, ColumnTimestamp, ConsistencyLevel}) ->
    cloudi_x_erlang_cassandra:remove(Destination, RowKey, ColumnPath, ColumnTimestamp, ConsistencyLevel);
process_query(_ConnectionName, {'system_add_column_family', ColumnFamilyDefinition}) ->
    cloudi_x_erlang_cassandra:system_add_column_family(ColumnFamilyDefinition);
process_query(_ConnectionName, {'system_describe_column_family', Destination, ColumnFamily}) ->
    cloudi_x_erlang_cassandra:system_describe_column_family(Destination, ColumnFamily);
process_query(_ConnectionName, {'system_drop_column_family', Destination, ColumnFamily}) ->
    cloudi_x_erlang_cassandra:system_drop_column_family(Destination, ColumnFamily);
process_query(_ConnectionName, {'system_update_column_family', ColumnFamilyDefinition}) ->
    cloudi_x_erlang_cassandra:system_update_column_family(ColumnFamilyDefinition);
process_query(_ConnectionName, {'truncate', Destination, ColumnFamily}) ->
    cloudi_x_erlang_cassandra:truncate(Destination, ColumnFamily);
process_query(_ConnectionName, {'add', Destination, RowKey, ColumnParent, CounterColumn, ConsistencyLevel}) ->
    cloudi_x_erlang_cassandra:add(Destination, RowKey, ColumnParent, CounterColumn, ConsistencyLevel);
process_query(_ConnectionName, {'remove_counter', Destination, RowKey, ColumnPath, ConsistencyLevel}) ->
    cloudi_x_erlang_cassandra:remove_counter(Destination, RowKey, ColumnPath, ConsistencyLevel);
process_query(_ConnectionName, {'get_slice', Destination, RowKey, ColumnParent, SlicePredicate, ConsistencyLevel}) ->
    cloudi_x_erlang_cassandra:get_slice(Destination, RowKey, ColumnParent, SlicePredicate, ConsistencyLevel);
process_query(_ConnectionName, {'multiget_slice', Destination, RowKeys, ColumnParent, SlicePredicate, ConsistencyLevel}) ->
    cloudi_x_erlang_cassandra:multiget_slice(Destination, RowKeys, ColumnParent, SlicePredicate, ConsistencyLevel);
process_query(_ConnectionName, {'get_count', Destination, RowKey, ColumnParent, SlicePredicate, ConsistencyLevel}) ->
    cloudi_x_erlang_cassandra:get_count(Destination, RowKey, ColumnParent, SlicePredicate, ConsistencyLevel);
process_query(_ConnectionName, {'multiget_count', Destination, RowKeys, ColumnParent, SlicePredicate, ConsistencyLevel}) ->
    cloudi_x_erlang_cassandra:multiget_count(Destination, RowKeys, ColumnParent, SlicePredicate, ConsistencyLevel);
process_query(_ConnectionName, {'get_range_slices', Destination, ColumnParent, SlicePredicate, KeyRange, ConsistencyLevel}) ->
    cloudi_x_erlang_cassandra:get_range_slices(Destination, ColumnParent, SlicePredicate, KeyRange, ConsistencyLevel);
process_query(_ConnectionName, {'get_indexed_slices', Destination, ColumnParent, IndexClause, SlicePredicate, ConsistencyLevel}) ->
    cloudi_x_erlang_cassandra:get_indexed_slices(Destination, ColumnParent, IndexClause, SlicePredicate, ConsistencyLevel);
process_query(ConnectionName, {'execute_cql_query', CqlQuery, Compression}) ->
    cloudi_x_erlang_cassandra:execute_cql_query(ConnectionName, CqlQuery, Compression);
process_query(ConnectionName, {'prepare_cql_query', CqlQuery, Compression}) ->
    cloudi_x_erlang_cassandra:prepare_cql_query(ConnectionName, CqlQuery, Compression);
process_query(ConnectionName, {'execute_prepared_cql_query', CqlQuery, Values}) ->
    cloudi_x_erlang_cassandra:execute_prepared_cql_query(ConnectionName, CqlQuery, Values);
process_query(_ConnectionName, {'describe_version'}) ->
    cloudi_x_erlang_cassandra:describe_version();
process_query(_ConnectionName, {'describe_snitch'}) ->
    cloudi_x_erlang_cassandra:describe_snitch();
process_query(_ConnectionName, {'describe_partitioner'}) ->
    cloudi_x_erlang_cassandra:describe_partitioner();
process_query(_ConnectionName, {'describe_schema_versions'}) ->
    cloudi_x_erlang_cassandra:describe_schema_versions();
process_query(_ConnectionName, {'describe_cluster_name'}) ->
    cloudi_x_erlang_cassandra:describe_cluster_name();
process_query(_ConnectionName, {'describe_keyspaces'}) ->
    cloudi_x_erlang_cassandra:describe_keyspaces();
process_query(_ConnectionName, {'describe_ring', Destination}) ->
    cloudi_x_erlang_cassandra:describe_keyspaces(Destination);
process_query(_ConnectionName, _) ->
    {error, invalid_call}.

binary_name({Host, Port, Name}) when is_list(Name) ->
    {Host, Port, erlang:list_to_binary(Name)};
binary_name({Host, Port, Name}) when is_binary(Name) ->
    {Host, Port, Name};
binary_name(Name) when is_list(Name) ->
    erlang:list_to_binary(Name);
binary_name(Name) when is_binary(Name) ->
    Name.

subscription_name({_, _, Name}) when is_list(Name) -> 
    Name;
subscription_name({_, _, Name}) when is_binary(Name) -> 
    erlang:binary_to_list(Name);
subscription_name(Name) when is_list(Name) -> 
    Name;
subscription_name(Name) when is_binary(Name) -> 
    erlang:binary_to_list(Name).

