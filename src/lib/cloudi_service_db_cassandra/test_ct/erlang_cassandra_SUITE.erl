%%%-------------------------------------------------------------------
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright (C) 2011-2012 Mahesh Paolini-Subramanya
%%% @doc Erlastic_search tests
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(erlang_cassandra_SUITE).
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').

-include_lib("proper/include/proper.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("erlang_cassandra/include/cloudi_x_erlang_cassandra_types.hrl").
-include_lib("cloudi_core/include/cloudi_logger.hrl").

-compile(export_all).

-define(CHECKSPEC(M,F,N), true = proper:check_spec({M,F,N})).
-define(PROPTEST(A), true = proper:quickcheck(A())).
-define(PROPTEST(A, B), true = proper:quickcheck(A(B))).

-define(NUMTESTS, 10).

% Cloudi
-define(DB_PREFIX, "/dbpopulator/cloudi_x_erlang_cassandra/").
-define(DB_TARGET, "testdb").
-define(POOL_OPTIONS, [{size, 1}, {max_overflow, 0}]).
-define(TIMEOUT, 1000).

% Cassandra
-define(KEYSPACE_PREFIX, "cloudi_test_keyspace").
-define(COLUMN_FAMILY_PREFIX, "cloudi_column_family").
-define(SUPER_COLUMN_PREFIX, "cloudi_super_column").
-define(COLUMN_NAME_PREFIX, "cloudi_name").
-define(COLUMN_VALUE_PREFIX, "cloudi_value").
-define(ROW_KEY_PREFIX, "cloudi_row_key").
-define(CONSISTENCY_LEVEL, 1).
-define(MAX_COLUMNS, 100).
-define(MAX_ROWS, 10).

-define(DEFAULT_THRIFT_HOST, "localhost").
-define(DEFAULT_THRIFT_PORT, 9160).
-define(EC(Prefix, Target, PoolOptions), {internal, Prefix, cloudi_service_db_cassandra, [{connection_name, Target}, {pool_options, PoolOptions}, {connection_options, [{thrift_host, ?DEFAULT_THRIFT_HOST}, {thrift_port, ?DEFAULT_THRIFT_PORT}]}], immediate_closest, 5000, 5000, 5000, undefined, undefined, 1, 5, 300, []}).

-record(internal,
    {
        prefix             :: string(),
        module             :: atom() | file:filename(),
        args               :: list(),
        dest_refresh       :: cloudi_service_api:dest_refresh(),
        timeout_init       :: cloudi_service_api:timeout_milliseconds(),
        timeout_async      :: cloudi_service_api:timeout_milliseconds(),
        timeout_sync       :: cloudi_service_api:timeout_milliseconds(),
        dest_list_deny     :: cloudi_service_api:dest_list(),
        dest_list_allow    :: cloudi_service_api:dest_list(),
        count_process      :: pos_integer(),
        max_r              :: non_neg_integer(),
        max_t              :: cloudi_service_api:seconds(),
        options            :: cloudi_service_api:service_options_internal()
    }).
    

suite() ->
    [{ct_hooks,[cth_surefire]}, {timetrap,{seconds,320}}].

init_per_suite(Config0) ->
    setup_environment(),

    % General startup
    Config1 = start(Config0),

    % Cloudi config
    Config2 = setup_cloudi_services(Config1),
    Config2.


end_per_suite(Config) ->
    stop(Config),
    ok.


init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, _Config) ->
%    Config1 = [{context, cloudi:new()} | Config],
    ok.


init_per_testcase(_TestCase, Config) ->
    [{context, cloudi:new()} | Config].

end_per_testcase(_TestCase, _Config) ->
    ok.

groups_condition(Groups) ->
    case gen_tcp:connect(?DEFAULT_THRIFT_HOST, ?DEFAULT_THRIFT_PORT, []) of
        {ok, Socket} ->
            catch gen_tcp:close(Socket),
            Groups;
        {error, econnrefused} ->
            error_logger:error_msg("unable to test ~p",
                                   [{?DEFAULT_THRIFT_HOST,
                                     ?DEFAULT_THRIFT_PORT}]),
            [];
        {error, Reason} ->
            error_logger:error_msg("unable to test ~p: ~p",
                                   [{?DEFAULT_THRIFT_HOST,
                                     ?DEFAULT_THRIFT_PORT}, Reason]),
            []
    end.

groups() ->
    groups_condition([
        {keyspace_crud, [{repeat, 1}],
         [
                t_update_keyspace,
                t_add_drop_keyspace,
                t_set_keyspace,
                t_describe_keyspace
         ]},
        {column_family_crud, [{repeat, 1}],
         [
                t_add_drop_column_family,
                t_update_column_family,
                t_truncate_column_family
         ]},
        {column_crud, [{repeat, 1}],
         [
                t_insert_column,
                t_get_column,
                t_remove_column
         ]},
        {column_slice, [{repeat, 1}],
         [
                t_get_slice,
                t_get_range_slices,
                t_multiget_slice
         ]},
        {count, [{repeat, 1}],
         [
                t_get_count,
                t_multiget_count
         ]},
        {counter_crud, [{repeat, 1}],
         [
                t_add_counter,
                t_remove_counter
         ]},
        {cql, [{repeat, 1}],
         [
                t_execute_cql_query,
                t_prepare_and_execute_cql_query
         ]},
        {test, [],
         [
         t_add_drop_keyspace,
                t_update_keyspace
         ]}

    ]).

all() ->
    [
        {group, keyspace_crud},
        {group, column_family_crud},
        {group, column_crud},
        {group, column_slice},
        {group, count},
        {group, counter_crud},
        {group, cql}
    ].

t_add_drop_keyspace(Config) ->
    ?PROPTEST(prop_add_drop_keyspace, Config).

prop_add_drop_keyspace(Config) ->
    numtests(?NUMTESTS,
             ?FORALL(Keyspace, keyspace_word(), validate_add_drop_keyspace(Config, Keyspace))).

t_set_keyspace(Config) ->
    ?PROPTEST(prop_set_keyspace, Config).

prop_set_keyspace(Config) ->
    numtests(?NUMTESTS,
             ?FORALL(Keyspace, keyspace_word(), validate_set_keyspace(Config, Keyspace))).

t_describe_keyspace(Config) ->
    ?PROPTEST(prop_describe_keyspace, Config).

prop_describe_keyspace(Config) ->
    numtests(?NUMTESTS,
             ?FORALL(Keyspace, keyspace_word(), validate_describe_keyspace(Config, Keyspace))).

t_update_keyspace(Config) ->
    ?PROPTEST(prop_update_keyspace, Config).

prop_update_keyspace(Config) ->
    numtests(?NUMTESTS,
             ?FORALL(Keyspace, keyspace_word(), validate_update_keyspace(Config, Keyspace))).

t_add_drop_column_family(Config) ->
    ?PROPTEST(prop_add_drop_column_family, Config).

prop_add_drop_column_family(Config) ->
    numtests(?NUMTESTS,
             ?FORALL({Keyspace, ColumnFamilyDefinition}, keyspace_and_column_family_definition_item(), validate_add_drop_column_family(Config, Keyspace, ColumnFamilyDefinition))).

t_update_column_family(Config) ->
    ?PROPTEST(prop_update_column_family, Config).

prop_update_column_family(Config) ->
    numtests(?NUMTESTS,
             ?FORALL({Keyspace, ColumnFamilyDefinition}, keyspace_and_column_family_definition_item(), validate_update_column_family(Config, Keyspace, ColumnFamilyDefinition))).

t_truncate_column_family(Config) ->
    ?PROPTEST(prop_truncate_column_family, Config).

prop_truncate_column_family(Config) ->
    numtests(?NUMTESTS,
             ?FORALL({Keyspace, RowKey, ColumnParent, ColumnList}, {keyspace_word(), row_key_word(), column_parent_item(), column_item_list()}, validate_truncate_column_family(Config, Keyspace, RowKey, ColumnParent, ColumnList))).

t_insert_column(Config) ->
    ?PROPTEST(prop_insert_column, Config).

prop_insert_column(Config) ->
    numtests(?NUMTESTS,
             ?FORALL({Keyspace, RowKey, ColumnParent, Column}, {keyspace_word(), row_key_word(), column_parent_item(), column_item()}, validate_insert_column(Config, Keyspace, RowKey, ColumnParent, Column))).

t_get_column(Config) ->
    ?PROPTEST(prop_get_column, Config).

prop_get_column(Config) ->
    numtests(?NUMTESTS,
             ?FORALL({Keyspace, RowKey, ColumnParent, Column}, {keyspace_word(), row_key_word(), column_parent_item(), column_item()}, validate_get_column(Config, Keyspace, RowKey, ColumnParent, Column))).

t_get_slice(Config) ->
    ?PROPTEST(prop_get_slice, Config).

prop_get_slice(Config) ->
    numtests(?NUMTESTS,
             ?FORALL({Keyspace, RowKey, ColumnParent, ColumnList}, {keyspace_word(), row_key_word(), column_parent_item(), column_item_list()}, validate_get_slice(Config, Keyspace, RowKey, ColumnParent, ColumnList))).

t_get_range_slices(Config) ->
    ?PROPTEST(prop_get_range_slices, Config).

prop_get_range_slices(Config) ->
    numtests(?NUMTESTS,
             ?FORALL({Keyspace, RowKeyList, ColumnParent, ColumnList}, {keyspace_word(), row_key_list(), column_parent_item(), column_item_list()}, validate_get_range_slices(Config, Keyspace, RowKeyList, ColumnParent, ColumnList))).

t_multiget_slice(Config) ->
    ?PROPTEST(prop_multiget_slice, Config).

prop_multiget_slice(Config) ->
    numtests(?NUMTESTS,
             ?FORALL({Keyspace, RowKeyList, ColumnParent, ColumnList}, {keyspace_word(), row_key_list(), column_parent_item(), column_item_list()}, validate_multiget_slice(Config, Keyspace, RowKeyList, ColumnParent, ColumnList))).

t_get_count(Config) ->
    ?PROPTEST(prop_get_count, Config).

prop_get_count(Config) ->
    numtests(?NUMTESTS,
             ?FORALL({Keyspace, RowKey, ColumnParent, ColumnList}, {keyspace_word(), row_key_word(), column_parent_item(), column_item_list()}, validate_get_count(Config, Keyspace, RowKey, ColumnParent, ColumnList))).

t_multiget_count(Config) ->
    ?PROPTEST(prop_multiget_count, Config).

prop_multiget_count(Config) ->
    numtests(?NUMTESTS,
             ?FORALL({Keyspace, RowKeyList, ColumnParent, ColumnList}, {keyspace_word(), row_key_list(), column_parent_item(), column_item_list()}, validate_multiget_count(Config, Keyspace, RowKeyList, ColumnParent, ColumnList))).

t_remove_column(Config) ->
    ?PROPTEST(prop_remove_column, Config).

prop_remove_column(Config) ->
    numtests(?NUMTESTS,
             ?FORALL({Keyspace, RowKey, ColumnParent, Column}, {keyspace_word(), row_key_word(), column_parent_item(), column_item()}, validate_remove_column(Config, Keyspace, RowKey, ColumnParent, Column))).

t_add_counter(Config) ->
    ?PROPTEST(prop_add_counter, Config).

prop_add_counter(Config) ->
    numtests(?NUMTESTS,
             ?FORALL({Keyspace, RowKey, ColumnParent, CounterColumn}, {keyspace_word(), row_key_word(), column_parent_item(), counter_column_item()}, validate_add_counter(Config, Keyspace, RowKey, ColumnParent, CounterColumn))).

t_remove_counter(Config) ->
    ?PROPTEST(prop_remove_counter, Config).

prop_remove_counter(Config) ->
    numtests(?NUMTESTS,
             ?FORALL({Keyspace, RowKey, ColumnParent, CounterColumn}, {keyspace_word(), row_key_word(), column_parent_item(), counter_column_item()}, validate_remove_counter(Config, Keyspace, RowKey, ColumnParent, CounterColumn))).

t_execute_cql_query(Config) ->
    ?PROPTEST(prop_execute_cql_query, Config).

prop_execute_cql_query(Config) ->
    numtests(?NUMTESTS,
             ?FORALL(Keyspace, keyspace_word(), validate_execute_cql_query(Config, Keyspace))).

t_prepare_and_execute_cql_query(Config) ->
    ?PROPTEST(prop_prepare_and_execute_cql_query, Config).

prop_prepare_and_execute_cql_query(Config) ->
    numtests(?NUMTESTS,
             ?FORALL(Keyspace, keyspace_word(), validate_prepare_and_execute_cql_query(Config, Keyspace))).

t_describe_cluster_name(Config) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    {ok, {ok, _Response}} = cloudi:send_sync(Context, Target, {describe_cluster_name}).


validate_add_drop_keyspace(Config, Keyspace) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    {ok, {ok, _}} = create_keyspace(Config, Keyspace),
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_drop_keyspace, Keyspace}),
    true.

validate_set_keyspace(Config, Keyspace) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    {ok, {ok, _}} = create_keyspace(Config, Keyspace),
    {ok, {ok, ok}} = cloudi:send_sync(Context, Target, {set_keyspace, Keyspace}),
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_drop_keyspace, Keyspace}),
    true.

validate_describe_keyspace(Config, Keyspace) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    {ok, {ok, _}} = create_keyspace(Config, Keyspace),
    {ok, {ok, KeyspaceDefinition}} = cloudi:send_sync(Context, Target, {describe_keyspace, Keyspace}),
    Keyspace = KeyspaceDefinition#ksDef.name,
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_drop_keyspace, Keyspace}),
    true.

validate_update_keyspace(Config, Keyspace) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    {ok, {ok, _}} = create_keyspace(Config, Keyspace, 1),
    % Create a keyspace with replication_factor 1
    {ok, {ok, KeyspaceDefinition1}} = cloudi:send_sync(Context, Target, {describe_keyspace, Keyspace}),
    <<"1">> = keyspace_replication_factor(KeyspaceDefinition1),
    % Change it to 2
    KeyspaceDefinition2 = cloudi_x_erlang_cassandra:keyspace_definition(Keyspace, 2),
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_update_keyspace, KeyspaceDefinition2}),
    % Validate
    {ok, {ok, KeyspaceDefinition3}} = cloudi:send_sync(Context, Target, {describe_keyspace, Keyspace}),
    <<"2">> = keyspace_replication_factor(KeyspaceDefinition3),
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_drop_keyspace, Keyspace}),
    true.

validate_add_drop_column_family(Config, Keyspace, ColumnFamilyDefinition) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    % set up the keyspace
    {ok, {ok, _}} = create_keyspace(Config, Keyspace),
    {ok, {ok, ok}} = cloudi:send_sync(Context, Target, {set_keyspace, Keyspace}),
    % set up the column family
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_add_column_family, ColumnFamilyDefinition}),
    ColumnFamily = ColumnFamilyDefinition#cfDef.name,
    % cleanup
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_drop_column_family, Keyspace, ColumnFamily}),
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_drop_keyspace, Keyspace}),
    true.

validate_update_column_family(Config, Keyspace, ColumnFamilyDefinition) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    % set up the keyspace
    {ok, {ok, _}} = create_keyspace(Config, Keyspace),
    {ok, {ok, ok}} = cloudi:send_sync(Context, Target, {set_keyspace, Keyspace}),
    % set up the column family
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_add_column_family, ColumnFamilyDefinition}),
    ColumnFamily = ColumnFamilyDefinition#cfDef.name,
    % update the column family
    ColumnFamilyDefinition2 = ColumnFamilyDefinition#cfDef{gc_grace_seconds = 1000},
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_update_column_family, ColumnFamilyDefinition2}),
    {ok, {ok, ColumnFamilyDefinition3}} = cloudi:send_sync(Context, Target, {system_describe_column_family, Keyspace, ColumnFamily}),
    1000 = ColumnFamilyDefinition3#cfDef.gc_grace_seconds,
    % cleanup
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_drop_column_family, Keyspace, ColumnFamily}),
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_drop_keyspace, Keyspace}),
    true.

validate_truncate_column_family(Config, Keyspace, RowKey, ColumnParent, [FirstColumn | _] = ColumnList) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    {ok, {ok, _}} = create_keyspace(Config, Keyspace),
    {ok, {ok, ok}} = cloudi:send_sync(Context, Target, {set_keyspace, Keyspace}),
    % set up the column family
    ColumnFamily = ColumnParent#columnParent.column_family,
    ColumnFamilyDefinition = cloudi_x_erlang_cassandra:column_family_definition(Keyspace, ColumnFamily),
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_add_column_family, ColumnFamilyDefinition}),
    % insert
    lists:foreach(fun(Column) ->
                {ok, {ok, ok}} = cloudi:send_sync(Context, Target, {insert, Keyspace, RowKey, ColumnParent, Column, ?CONSISTENCY_LEVEL})
        end, ColumnList),
    % truncate
    {ok, {ok, ok}} = cloudi:send_sync(Context, Target, {truncate, Keyspace, ColumnFamily}),
    % Validate
    LastColumn = lists:last(ColumnList),
    SlicePredicate = cloudi_x_erlang_cassandra:slice_predicate(undefined, FirstColumn#column.name, LastColumn#column.name),
    {ok, {ok, []}} = cloudi:send_sync(Context, Target, {get_slice, Keyspace, RowKey, ColumnParent, SlicePredicate, ?CONSISTENCY_LEVEL}),
    % cleanup
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_drop_column_family, Keyspace, ColumnFamily}),
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_drop_keyspace, Keyspace}),
    true.

validate_insert_column(Config, Keyspace, RowKey, ColumnParent, Column) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    {ok, {ok, _}} = create_keyspace(Config, Keyspace),
    {ok, {ok, ok}} = cloudi:send_sync(Context, Target, {set_keyspace, Keyspace}),
    % set up the column family
    ColumnFamily = ColumnParent#columnParent.column_family,
    ColumnFamilyDefinition = cloudi_x_erlang_cassandra:column_family_definition(Keyspace, ColumnFamily),
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_add_column_family, ColumnFamilyDefinition}),
    % insert
    {ok, {ok, ok}} = cloudi:send_sync(Context, Target, {insert, Keyspace, RowKey, ColumnParent, Column, ?CONSISTENCY_LEVEL}),
    % cleanup
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_drop_column_family, Keyspace, ColumnFamily}),
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_drop_keyspace, Keyspace}),
    true.

validate_get_column(Config, Keyspace, RowKey, ColumnParent, Column) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    {ok, {ok, _}} = create_keyspace(Config, Keyspace),
    {ok, {ok, ok}} = cloudi:send_sync(Context, Target, {set_keyspace, Keyspace}),
    % set up the column family
    ColumnFamily = ColumnParent#columnParent.column_family,
    ColumnFamilyDefinition = cloudi_x_erlang_cassandra:column_family_definition(Keyspace, ColumnFamily),
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_add_column_family, ColumnFamilyDefinition}),
    % insert
    {ok, {ok, ok}} = cloudi:send_sync(Context, Target, {insert, Keyspace, RowKey, ColumnParent, Column, ?CONSISTENCY_LEVEL}),
    % get
    ColumnPath = cloudi_x_erlang_cassandra:column_path(ColumnParent#columnParent.column_family,
                                              ColumnParent#columnParent.super_column ,
                                              Column#column.name),
    {ok, {ok, Response}} = cloudi:send_sync(Context, Target, {get, Keyspace, RowKey, ColumnPath, ?CONSISTENCY_LEVEL}),
    % validate
    Column = Response#columnOrSuperColumn.column,
    % cleanup
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_drop_column_family, Keyspace, ColumnFamily}),
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_drop_keyspace, Keyspace}),
    true.

validate_get_slice(Config, Keyspace, RowKey, ColumnParent, [FirstColumn | _] = ColumnList) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    {ok, {ok, _}} = create_keyspace(Config, Keyspace),
    {ok, {ok, ok}} = cloudi:send_sync(Context, Target, {set_keyspace, Keyspace}),
    % set up the column family
    ColumnFamily = ColumnParent#columnParent.column_family,
    ColumnFamilyDefinition = cloudi_x_erlang_cassandra:column_family_definition(Keyspace, ColumnFamily),
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_add_column_family, ColumnFamilyDefinition}),
    % insert
    lists:foreach(fun(Column) ->
                {ok, {ok, ok}} = cloudi:send_sync(Context, Target, {insert, Keyspace, RowKey, ColumnParent, Column, ?CONSISTENCY_LEVEL})
        end, ColumnList),
    % get_slice
    LastColumn = lists:last(ColumnList),
    SlicePredicate = cloudi_x_erlang_cassandra:slice_predicate(undefined, FirstColumn#column.name, LastColumn#column.name),
    {ok, {ok, Response}} = cloudi:send_sync(Context, Target, {get_slice, Keyspace, RowKey, ColumnParent, SlicePredicate, ?CONSISTENCY_LEVEL}),
    % validate
    ResponseLength = length(Response), 
    ResponseLength = length(ColumnList),
    % cleanup
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_drop_column_family, Keyspace, ColumnFamily}),
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_drop_keyspace, Keyspace}),
    true.

validate_get_range_slices(Config, Keyspace, [FirstRow | _] = RowKeyList, ColumnParent, [FirstColumn | _] = ColumnList) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    {ok, {ok, _}} = create_keyspace(Config, Keyspace),
    {ok, {ok, ok}} = cloudi:send_sync(Context, Target, {set_keyspace, Keyspace}),
    % set up the column family
    ColumnFamily = ColumnParent#columnParent.column_family,
    ColumnFamilyDefinition = cloudi_x_erlang_cassandra:column_family_definition(Keyspace, ColumnFamily),
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_add_column_family, ColumnFamilyDefinition}),
    % insert
    lists:foreach(fun(RowKey) ->
                lists:foreach(fun(Column) ->
                            {ok, {ok, ok}} = cloudi:send_sync(Context, Target, {insert, Keyspace, RowKey, ColumnParent, Column, ?CONSISTENCY_LEVEL})
                    end, ColumnList)
        end, RowKeyList),
    % get_slice
    % Not sorted, so, just compare the data in one row
    KeyRange = cloudi_x_erlang_cassandra:key_range(FirstRow, FirstRow),
    LastColumn = lists:last(ColumnList),
    SlicePredicate = cloudi_x_erlang_cassandra:slice_predicate(undefined, FirstColumn#column.name, LastColumn#column.name),
    {ok, {ok, Response}} = cloudi:send_sync(Context, Target, {get_range_slices, Keyspace, ColumnParent, SlicePredicate, KeyRange, ?CONSISTENCY_LEVEL}),
    % validate
    KeySlice = lists:keyfind(FirstRow, #keySlice.key, Response),
    ResponseLength = length(KeySlice#keySlice.columns),
    ResponseLength = length(ColumnList),
    % cleanup
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_drop_column_family, Keyspace, ColumnFamily}),
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_drop_keyspace, Keyspace}),
    true.

validate_multiget_slice(Config, Keyspace, RowKeyList, ColumnParent, [FirstColumn | _] = ColumnList) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    {ok, {ok, _}} = create_keyspace(Config, Keyspace),
    {ok, {ok, ok}} = cloudi:send_sync(Context, Target, {set_keyspace, Keyspace}),
    % set up the column family
    ColumnFamily = ColumnParent#columnParent.column_family,
    ColumnFamilyDefinition = cloudi_x_erlang_cassandra:column_family_definition(Keyspace, ColumnFamily),
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_add_column_family, ColumnFamilyDefinition}),
    % insert
    lists:foreach(fun(RowKey) ->
                lists:foreach(fun(Column) ->
                            {ok, {ok, ok}} = cloudi:send_sync(Context, Target, {insert, Keyspace, RowKey, ColumnParent, Column, ?CONSISTENCY_LEVEL})
                    end, ColumnList)
        end, RowKeyList),
    % get_slice
    LastColumn = lists:last(ColumnList),
    SlicePredicate = cloudi_x_erlang_cassandra:slice_predicate(undefined, FirstColumn#column.name, LastColumn#column.name),
    {ok, {ok, Response}} = cloudi:send_sync(Context, Target, {multiget_slice, Keyspace, RowKeyList, ColumnParent, SlicePredicate, ?CONSISTENCY_LEVEL}),
    ResponseLength = dict:fold(fun(_Key, Value, Acc) ->
                length(Value) + Acc
        end, 0, Response),

    % validate
    ResponseLength = length(RowKeyList) * length(ColumnList),
    % cleanup
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_drop_column_family, Keyspace, ColumnFamily}),
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_drop_keyspace, Keyspace}),
    true.

validate_get_count(Config, Keyspace, RowKey, ColumnParent, [FirstColumn | _] = ColumnList) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    {ok, {ok, _}} = create_keyspace(Config, Keyspace),
    {ok, {ok, ok}} = cloudi:send_sync(Context, Target, {set_keyspace, Keyspace}),
    % set up the column family
    ColumnFamily = ColumnParent#columnParent.column_family,
    ColumnFamilyDefinition = cloudi_x_erlang_cassandra:column_family_definition(Keyspace, ColumnFamily),
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_add_column_family, ColumnFamilyDefinition}),
    % insert
     lists:foreach(fun(Column) ->
                {ok, {ok, ok}} = cloudi:send_sync(Context, Target, {insert, Keyspace, RowKey, ColumnParent, Column, ?CONSISTENCY_LEVEL})
        end, ColumnList),
    % get_slice
    LastColumn = lists:last(ColumnList),
    SlicePredicate = cloudi_x_erlang_cassandra:slice_predicate(undefined, FirstColumn#column.name, LastColumn#column.name),
    {ok, {ok, Response}} = cloudi:send_sync(Context, Target, {get_count, Keyspace, RowKey, ColumnParent, SlicePredicate, ?CONSISTENCY_LEVEL}),
    Response = length(ColumnList),
    % cleanup
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_drop_column_family, Keyspace, ColumnFamily}),
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_drop_keyspace, Keyspace}),
    true.

validate_multiget_count(Config, Keyspace, RowKeyList, ColumnParent, [FirstColumn | _] = ColumnList) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    {ok, {ok, _}} = create_keyspace(Config, Keyspace),
    {ok, {ok, ok}} = cloudi:send_sync(Context, Target, {set_keyspace, Keyspace}),
    % set up the column family
    ColumnFamily = ColumnParent#columnParent.column_family,
    ColumnFamilyDefinition = cloudi_x_erlang_cassandra:column_family_definition(Keyspace, ColumnFamily),
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_add_column_family, ColumnFamilyDefinition}),
    % insert
    lists:foreach(fun(RowKey) ->
                lists:foreach(fun(Column) ->
                            {ok, {ok, ok}} = cloudi:send_sync(Context, Target, {insert, Keyspace, RowKey, ColumnParent, Column, ?CONSISTENCY_LEVEL})
                    end, ColumnList)
        end, RowKeyList),
    % get_slice
    LastColumn = lists:last(ColumnList),
    SlicePredicate = cloudi_x_erlang_cassandra:slice_predicate(undefined, FirstColumn#column.name, LastColumn#column.name),
    {ok, {ok, Response}} = cloudi:send_sync(Context, Target, {multiget_count, Keyspace, RowKeyList, ColumnParent, SlicePredicate, ?CONSISTENCY_LEVEL}),
    ResponseLength = dict:fold(fun(_Key, Value, Acc) ->
                Value + Acc
        end, 0, Response),

    % validate
    ResponseLength = length(RowKeyList) * length(ColumnList),
    % cleanup
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_drop_column_family, Keyspace, ColumnFamily}),
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_drop_keyspace, Keyspace}),
    true.


validate_remove_column(Config, Keyspace, RowKey, ColumnParent, Column) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    {ok, {ok, _}} = create_keyspace(Config, Keyspace),
    {ok, {ok, ok}} = cloudi:send_sync(Context, Target, {set_keyspace, Keyspace}),
    % set up the column family
    ColumnFamily = ColumnParent#columnParent.column_family,
    ColumnFamilyDefinition = cloudi_x_erlang_cassandra:column_family_definition(Keyspace, ColumnFamily),
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_add_column_family, ColumnFamilyDefinition}),
    % insert
    {ok, {ok, ok}} = cloudi:send_sync(Context, Target, {insert, Keyspace, RowKey, ColumnParent, Column, ?CONSISTENCY_LEVEL}),
    % remove
    ColumnPath = cloudi_x_erlang_cassandra:column_path(ColumnParent#columnParent.column_family,
                                              ColumnParent#columnParent.super_column ,
                                              Column#column.name),
    ColumnTimestamp = Column#column.timestamp,
    {ok, {ok, ok}} = cloudi:send_sync(Context, Target, {remove, Keyspace, RowKey, ColumnPath, ColumnTimestamp, ?CONSISTENCY_LEVEL}),
    % cleanup
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_drop_column_family, Keyspace, ColumnFamily}),
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_drop_keyspace, Keyspace}),
    true.

validate_add_counter(Config, Keyspace, RowKey, ColumnParent, CounterColumn) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    {ok, {ok, _}} = create_keyspace(Config, Keyspace),
    {ok, {ok, ok}} = cloudi:send_sync(Context, Target, {set_keyspace, Keyspace}),
    % set up the column family
    ColumnFamily = ColumnParent#columnParent.column_family,
    % create a counter column family
    ColumnFamilyDefinition = cloudi_x_erlang_cassandra:column_family_definition(Keyspace, ColumnFamily, true),
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_add_column_family, ColumnFamilyDefinition}),
    % create the counter, and double it
    {ok, {ok, ok}} = cloudi:send_sync(Context, Target, {add, Keyspace, RowKey, ColumnParent, CounterColumn, ?CONSISTENCY_LEVEL}),
    {ok, {ok, ok}} = cloudi:send_sync(Context, Target, {add, Keyspace, RowKey, ColumnParent, CounterColumn, ?CONSISTENCY_LEVEL}),
    % Validate value
    ColumnPath = cloudi_x_erlang_cassandra:column_path(ColumnParent#columnParent.column_family,
                                              ColumnParent#columnParent.super_column ,
                                              CounterColumn#counterColumn.name),
    {ok, {ok, Response}} = cloudi:send_sync(Context, Target, {get, Keyspace, RowKey, ColumnPath, ?CONSISTENCY_LEVEL}),
    RCounterColumn = Response#columnOrSuperColumn.counter_column,
    NewCount = RCounterColumn#counterColumn.value,
    NewCount = CounterColumn#counterColumn.value * 2,
    % cleanup
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_drop_column_family, Keyspace, ColumnFamily}),
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_drop_keyspace, Keyspace}),
    true.

validate_remove_counter(Config, Keyspace, RowKey, ColumnParent, CounterColumn) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    {ok, {ok, _}} = create_keyspace(Config, Keyspace),
    {ok, {ok, ok}} = cloudi:send_sync(Context, Target, {set_keyspace, Keyspace}),
    % set up the column family
    ColumnFamily = ColumnParent#columnParent.column_family,
    % create a counter column family
    ColumnFamilyDefinition = cloudi_x_erlang_cassandra:column_family_definition(Keyspace, ColumnFamily, true),
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_add_column_family, ColumnFamilyDefinition}),
    % create the counter
    {ok, {ok, ok}} = cloudi:send_sync(Context, Target, {add, Keyspace, RowKey, ColumnParent, CounterColumn, ?CONSISTENCY_LEVEL}),
    % Remove the counter
    ColumnPath = cloudi_x_erlang_cassandra:column_path(ColumnParent#columnParent.column_family,
                                              ColumnParent#columnParent.super_column ,
                                              CounterColumn#counterColumn.name),
    {ok, {ok, ok}} = cloudi:send_sync(Context, Target, {remove_counter, Keyspace, RowKey, ColumnPath, ?CONSISTENCY_LEVEL}),
    % cleanup
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_drop_column_family, Keyspace, ColumnFamily}),
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_drop_keyspace, Keyspace}),
    true.

validate_execute_cql_query(Config, Keyspace) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    {ok, {ok, _}} = create_keyspace(Config, Keyspace),
    Query = <<"use ", Keyspace/binary, ";">>,
    {ok, {ok, Response}} = cloudi:send_sync(Context, Target, {execute_cql_query, Query, 2}),
    true = is_record(Response, cqlResult),
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_drop_keyspace, Keyspace}),
    true.

validate_prepare_and_execute_cql_query(Config, Keyspace) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    {ok, {ok, _}} = create_keyspace(Config, Keyspace),
    Query1 = <<"use ", Keyspace/binary, ";">>,
    {ok, {ok, _Response1}} = cloudi:send_sync(Context, Target, {execute_cql_query, Query1, 2}),
    Query2 = <<"CREATE COLUMNFAMILY test (KEY int PRIMARY KEY, text_field text);">>,
    {ok, {ok, _Response2}} = cloudi:send_sync(Context, Target, {execute_cql_query, Query2, 2}),
    Query3 = <<"insert into test (KEY, text_field) values (?, ?);">>,
    {ok, {ok, Response3}} = cloudi:send_sync(Context, Target, {prepare_cql_query, Query3, 2}),
    {ok, {ok, Response4}} = cloudi:send_sync(Context, Target, {execute_prepared_cql_query, Response3#cqlPreparedResult.itemId, [<<"123">>, <<"text">>]}),
    true = is_record(Response4, cqlResult),
    {ok, {ok, _}} = cloudi:send_sync(Context, Target, {system_drop_keyspace, Keyspace}),
    true.

random_name(Name) when is_list(Name) ->
    binary_to_list(random_name(list_to_binary(Name)));
random_name(Name) ->
    random:seed(erlang:now()),
    Id = list_to_binary(integer_to_list(random:uniform(999999999))),
    <<Name/binary, Id/binary>>.

create_keyspace(Config, Keyspace) ->
    create_keyspace(Config, Keyspace, 1).

create_keyspace(Config, Keyspace, ReplicationFactor) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    KeyspaceDefinition = cloudi_x_erlang_cassandra:keyspace_definition(Keyspace, ReplicationFactor),
    cloudi:send_sync(Context, Target, {system_add_keyspace, KeyspaceDefinition}).

delete_keyspace(Config, Keyspace) ->
    Context  = ?config(context, Config),
    Target  = ?config(target, Config),
    cloudi:send_sync(Context, Target, {system_drop_keyspace, Keyspace}).

keyspace_replication_factor(KeyspaceDefinition) ->
    dict:fetch(<<"replication_factor">>, KeyspaceDefinition#ksDef.strategy_options).

% types
keyspace_word() ->
    ?LET(X, non_empty(limited_word(?KEYSPACE_PREFIX)), list_to_binary(X)).

column_family_word() ->
    ?LET(X, non_empty(limited_word(?COLUMN_FAMILY_PREFIX)), list_to_binary(X)).

super_column_word() ->
    ?LET(X, non_empty(limited_word(?SUPER_COLUMN_PREFIX)), list_to_binary(X)).

column_name_word() ->
    ?LET(X, non_empty(limited_word(?COLUMN_NAME_PREFIX)), list_to_binary(X)).

column_value_word() ->
    ?LET(X, non_empty(limited_word(?COLUMN_VALUE_PREFIX)), list_to_binary(X)).

row_key_word() ->
    ?LET(X, non_empty(limited_word(?ROW_KEY_PREFIX)), list_to_binary(X)).

row_key_list() ->
    ?LET(X, non_empty(limited_word(?ROW_KEY_PREFIX)), row_list(list_to_binary(X))).

keyspace_and_column_family_definition_item() ->
    ?LET({Keyspace, ColumnFamily}, 
         {keyspace_word(), column_family_word()},
         {Keyspace, cloudi_x_erlang_cassandra:column_family_definition(Keyspace, ColumnFamily)}).

column_parent_item() ->
    ?LET({ColumnFamily, _SuperColumn}, {column_family_word(),
                                       super_column_word()},
%         cloudi_x_erlang_cassandra:column_parent(ColumnFamily, SuperColumn)).
         cloudi_x_erlang_cassandra:column_parent(ColumnFamily, undefined)).

column_item() ->
    ?LET({ColumnName, ColumnValue},
         {column_name_word(),
          column_value_word()},
         cloudi_x_erlang_cassandra:column(ColumnName, ColumnValue, cloudi_x_erlang_cassandra:timestamp(), undefined)).

column_item_list() ->
    ?LET({ColumnName, ColumnValue},
         {column_name_word(),
          column_value_word()},
          column_list(ColumnName, ColumnValue, cloudi_x_erlang_cassandra:timestamp(), undefined)).

counter_column_item() ->
    ?LET({ColumnName, ColumnValue},
         {column_name_word(),
          non_neg_integer()},
         cloudi_x_erlang_cassandra:counter_column(ColumnName, ColumnValue)).

row_list(RowKey) ->
    Count = random:uniform(?MAX_ROWS),
    List = lists:map(fun(I) -> 
                    BI = list_to_binary(integer_to_list(I)),
                    <<RowKey/binary, "_", BI/binary>>
        end, lists:seq(1, Count)),
    lists:usort(List).

column_list(ColumnName, ColumnValue, Timestamp, Ttl) ->
    Count = random:uniform(?MAX_COLUMNS),
    List = lists:map(fun(I) -> 
                BI = list_to_binary(integer_to_list(I)),
                #column{name = <<ColumnName/binary, "_", BI/binary>>,
                        value = <<ColumnValue/binary, "_", BI/binary>>,
                        timestamp = Timestamp + I,
                        ttl = Ttl}
        end, lists:seq(1, Count)),
    lists:usort(List).


limited_word(Prefix) ->
    ?SUCHTHAT(X, word(Prefix), length(X) < 48).

word(Prefix) -> 
    random_name(Prefix) ++ list(oneof([integer($a, $z), integer($A, $Z), integer($0, $9)])).




setup_environment() ->
    random:seed(erlang:now()).

setup_cloudi_services(Config) ->
    Prefix = random_name(?DB_PREFIX ++ "foo"),
    Db = random_name(?DB_TARGET),
    Target = Prefix ++ Db,
    timer:sleep(?TIMEOUT),
    cloudi_service_api:services_add([?EC(Prefix, Db, ?POOL_OPTIONS)], ?TIMEOUT),
    timer:sleep(?TIMEOUT),
    [{db_prefix, Prefix}, {target, Target} | Config].


start(Config) ->
    setup_cloudi(Config),
    Config.


stop(Config) ->
    Prefix = ?config(db_prefix, Config),
    unload_cloudi_service([Prefix]),
    teardown_cloudi(Config),
    ok.


%% Need to fix with a correct record format
cloudi_services() ->
    {ok, Services} = cloudi_service_api:services(?TIMEOUT),
    [{Uuid, Internal#internal.prefix} || {Uuid, Internal} <- Services].

unload_cloudi_service(Prefixes) ->
    lists:foreach(fun(Prefix) ->
                lists:foreach(fun({Uuid, X}) ->
                            case Prefix of
                                X -> 
                                    cloudi_service_api:services_remove([Uuid], ?TIMEOUT);
                                _ -> ok
                            end end, cloudi_services()) end, Prefixes).

setup_cloudi(_Config) ->
    CloudIConfig = [{acl, []}, {services, []}, {nodes, []},
                    {logging, [{file, "cloudi.log"}]}],
    ok = cloudi_x_reltool_util:application_start(cloudi_core,
                                        [{configuration, CloudIConfig}],1000).


teardown_cloudi(_Config) ->
    cloudi_x_reltool_util:application_stop(cloudi_core).

