%%%-------------------------------------------------------------------
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright (C) 2013 Mahesh Paolini-Subramanya
%%% @doc cassandra tests
%%%      - Each group test creates and deletes a random keyspace 
%%%         (available as ?config(keyspace, Config)
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
-include_lib("erlang_cassandra/include/erlang_cassandra_types.hrl").

-compile(export_all).

-define(CHECKSPEC(M,F,N), true = proper:check_spec({M,F,N})).
-define(PROPTEST(A), true = proper:quickcheck(A())).

-define(NUMTESTS, 10).
-define(KEYSPACE_PREFIX, "test_keyspace").
-define(COLUMN_FAMILY_PREFIX, "column_family").
-define(SUPER_COLUMN_PREFIX, "super_column").
-define(COLUMN_NAME_PREFIX, "name").
-define(COLUMN_VALUE_PREFIX, "value").
-define(ROW_KEY_PREFIX, "row_key").
-define(CONSISTENCY_LEVEL, 1).
-define(MAX_COLUMNS, 100).
-define(MAX_ROWS, 10).
% Thrift
-define(THRIFT_HOST, "localhost").
-define(THRIFT_PORT, 9160).


suite() ->
    [{ct_hooks,[cth_surefire]}, {timetrap,{seconds,320}}].

init_per_suite(Config) ->
    setup_lager(),
    setup_environment(),
    Config.

end_per_suite(_Config) ->
    ok.

keyspace(Name) ->
    case random:uniform(2) of
        1 -> {?THRIFT_HOST, ?THRIFT_PORT, Name};
        2 -> Name
    end.

connection_options() ->
    [{thrift_host, ?THRIFT_HOST},
     {thrift_port, ?THRIFT_PORT},
     {thrift_options, [{framed, true}]}].

pool_options(1) ->
    [];
pool_options(2) ->
    [{size, 7}];
pool_options(_) ->
    [{size, 7},
     {max_overflow, 14}].

retry_options(1) ->
    [];
retry_options(2) ->
    [{retry_interval, 500}];
retry_options(_) ->
    [{retry_interval, 500},
     {retry_amount, 5}].

update_config(Config) ->
    Version = cassandra_test_version(Config),
    Config1 = lists:foldl(fun(X, Acc) -> 
                    proplists:delete(X, Acc)
            end, Config, [cassandra_test_version,
                          pool_options,
                          retry_options,
                          keyspace]),
    [{cassandra_test_version, Version + 1} | Config1].

cassandra_test_version(Config) ->
    case proplists:get_value(cassandra_test_version, Config) of
        undefined -> 1;
        Val -> Val
    end.


init_per_group(_GroupName, Config) ->
    Keyspace = keyspace(random_name(<<"keyspace">>)),

    Config1 = 
    case ?config(saved_config, Config) of
        {_, Config0} -> Config0;
        undefined -> Config
    end,
    Version = cassandra_test_version(Config1),
    PoolOptions = pool_options(Version),
    ConnectionOptions = connection_options(),
    RetryOptions = retry_options(Version),

    Config2 = [{keyspace, Keyspace},
               {pool_options, PoolOptions},
               {connection_options, ConnectionOptions ++ RetryOptions} 
               | Config1],
    start(Config2),
    Config2.

end_per_group(_GroupName, Config) ->
    stop(Config),
    Config1 = update_config(Config),
    {save_config, Config1}.


init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [
        {keyspace_crud, [{repeat, 3}],
         [
                t_update_keyspace,
                t_add_drop_keyspace,
                t_set_keyspace,
                t_describe_keyspace
         ]},
        {column_family_crud, [{repeat, 3}],
         [
                t_add_drop_column_family,
                t_update_column_family,
                t_truncate_column_family
         ]},
        {column_crud, [{repeat, 3}],
         [
                t_insert_column,
                t_get_column,
                t_remove_column
         ]},
        {column_slice, [{repeat, 3}],
         [
                t_get_slice,
                t_get_range_slices,
                t_multiget_slice
         ]},
        {count, [{repeat, 3}],
         [
                t_get_count,
                t_multiget_count
         ]},
        {counter_crud, [{repeat, 3}],
         [
                t_add_counter,
                t_remove_counter
         ]},
        {test, [],
         [      
                t_describe_keyspace
         ]},
        {cql, [{repeat, 3}],
         [
                t_execute_cql_query,
                t_prepare_and_execute_cql_query
         ]}

    ].

all() ->
    [
        {group, keyspace_crud},
        {group, column_crud},
        {group, column_family_crud},
        {group, counter_crud},
        {group, column_slice},
        {group, count},
        {group, cql}
%         {group, test}
    ].

t_add_drop_keyspace(_) ->
    ?PROPTEST(prop_add_drop_keyspace).

prop_add_drop_keyspace() ->
    numtests(?NUMTESTS,
             ?FORALL(Keyspace, keyspace_word(), validate_add_drop_keyspace(Keyspace))).

t_set_keyspace(_) ->
    ?PROPTEST(prop_set_keyspace).

prop_set_keyspace() ->
    numtests(?NUMTESTS,
             ?FORALL(Keyspace, keyspace_word(), validate_set_keyspace(Keyspace))).

t_describe_keyspace(_) ->
    ?PROPTEST(prop_describe_keyspace).

prop_describe_keyspace() ->
    numtests(?NUMTESTS,
             ?FORALL(Keyspace, keyspace_word(), validate_describe_keyspace(Keyspace))).

t_update_keyspace(_) ->
    ?PROPTEST(prop_update_keyspace).

prop_update_keyspace() ->
    numtests(?NUMTESTS,
             ?FORALL(Keyspace, keyspace_word(), validate_update_keyspace(Keyspace))).

t_add_drop_column_family(_) ->
    ?PROPTEST(prop_add_drop_column_family).

prop_add_drop_column_family() ->
    numtests(?NUMTESTS,
             ?FORALL({Keyspace, ColumnFamilyDefinition}, keyspace_and_column_family_definition_item(), validate_add_drop_column_family(Keyspace, ColumnFamilyDefinition))).

t_update_column_family(_) ->
    ?PROPTEST(prop_update_column_family).

prop_update_column_family() ->
    numtests(?NUMTESTS,
             ?FORALL({Keyspace, ColumnFamilyDefinition}, keyspace_and_column_family_definition_item(), validate_update_column_family(Keyspace, ColumnFamilyDefinition))).

t_truncate_column_family(_) ->
    ?PROPTEST(prop_truncate_column_family).

prop_truncate_column_family() ->
    numtests(?NUMTESTS,
             ?FORALL({Keyspace, RowKey, ColumnParent, ColumnList}, {keyspace_word(), row_key_word(), column_parent_item(), column_item_list()}, validate_truncate_column_family(Keyspace, RowKey, ColumnParent, ColumnList))).

t_insert_column(_) ->
    ?PROPTEST(prop_insert_column).

prop_insert_column() ->
    numtests(?NUMTESTS,
             ?FORALL({Keyspace, RowKey, ColumnParent, Column}, {keyspace_word(), row_key_word(), column_parent_item(), column_item()}, validate_insert_column(Keyspace, RowKey, ColumnParent, Column))).

t_get_column(_) ->
    ?PROPTEST(prop_get_column).

prop_get_column() ->
    numtests(?NUMTESTS,
             ?FORALL({Keyspace, RowKey, ColumnParent, Column}, {keyspace_word(), row_key_word(), column_parent_item(), column_item()}, validate_get_column(Keyspace, RowKey, ColumnParent, Column))).

t_get_slice(_) ->
    ?PROPTEST(prop_get_slice).

prop_get_slice() ->
    numtests(?NUMTESTS,
             ?FORALL({Keyspace, RowKey, ColumnParent, ColumnList}, {keyspace_word(), row_key_word(), column_parent_item(), column_item_list()}, validate_get_slice(Keyspace, RowKey, ColumnParent, ColumnList))).

t_get_range_slices(_) ->
    ?PROPTEST(prop_get_range_slices).

prop_get_range_slices() ->
    numtests(?NUMTESTS,
             ?FORALL({Keyspace, RowKeyList, ColumnParent, ColumnList}, {keyspace_word(), row_key_list(), column_parent_item(), column_item_list()}, validate_get_range_slices(Keyspace, RowKeyList, ColumnParent, ColumnList))).

t_multiget_slice(_) ->
    ?PROPTEST(prop_multiget_slice).

prop_multiget_slice() ->
    numtests(?NUMTESTS,
             ?FORALL({Keyspace, RowKeyList, ColumnParent, ColumnList}, {keyspace_word(), row_key_list(), column_parent_item(), column_item_list()}, validate_multiget_slice(Keyspace, RowKeyList, ColumnParent, ColumnList))).

t_get_count(_) ->
    ?PROPTEST(prop_get_count).

prop_get_count() ->
    numtests(?NUMTESTS,
             ?FORALL({Keyspace, RowKey, ColumnParent, ColumnList}, {keyspace_word(), row_key_word(), column_parent_item(), column_item_list()}, validate_get_count(Keyspace, RowKey, ColumnParent, ColumnList))).

t_multiget_count(_) ->
    ?PROPTEST(prop_multiget_count).

prop_multiget_count() ->
    numtests(?NUMTESTS,
             ?FORALL({Keyspace, RowKeyList, ColumnParent, ColumnList}, {keyspace_word(), row_key_list(), column_parent_item(), column_item_list()}, validate_multiget_count(Keyspace, RowKeyList, ColumnParent, ColumnList))).

t_remove_column(_) ->
    ?PROPTEST(prop_remove_column).

prop_remove_column() ->
    numtests(?NUMTESTS,
             ?FORALL({Keyspace, RowKey, ColumnParent, Column}, {keyspace_word(), row_key_word(), column_parent_item(), column_item()}, validate_remove_column(Keyspace, RowKey, ColumnParent, Column))).

t_add_counter(_) ->
    ?PROPTEST(prop_add_counter).

prop_add_counter() ->
    numtests(?NUMTESTS,
             ?FORALL({Keyspace, RowKey, ColumnParent, CounterColumn}, {keyspace_word(), row_key_word(), column_parent_item(), counter_column_item()}, validate_add_counter(Keyspace, RowKey, ColumnParent, CounterColumn))).

t_remove_counter(_) ->
    ?PROPTEST(prop_remove_counter).

prop_remove_counter() ->
    numtests(?NUMTESTS,
             ?FORALL({Keyspace, RowKey, ColumnParent, CounterColumn}, {keyspace_word(), row_key_word(), column_parent_item(), counter_column_item()}, validate_remove_counter(Keyspace, RowKey, ColumnParent, CounterColumn))).

t_execute_cql_query(_) ->
    ?PROPTEST(prop_execute_cql_query).

prop_execute_cql_query() ->
    numtests(?NUMTESTS,
             ?FORALL(Keyspace, keyspace_word(), validate_execute_cql_query(Keyspace))).

t_prepare_and_execute_cql_query(_) ->
    ?PROPTEST(prop_prepare_and_execute_cql_query).

prop_prepare_and_execute_cql_query() ->
    numtests(?NUMTESTS,
             ?FORALL({CqlPool, Keyspace}, {keyspace_word(), keyspace_word()}, validate_prepare_and_execute_cql_query(CqlPool, Keyspace))).


validate_add_drop_keyspace(Keyspace) ->
    {ok, _} = create_keyspace(Keyspace),
    {ok, _} = erlang_cassandra:system_drop_keyspace(Keyspace),
    true.

validate_set_keyspace(Keyspace) ->
    {ok, _} = create_keyspace(Keyspace),
    {ok, ok} = erlang_cassandra:set_keyspace(Keyspace),
    {ok, _} = erlang_cassandra:system_drop_keyspace(Keyspace),
    true.

validate_describe_keyspace(Keyspace) ->
    {ok, _} = create_keyspace(Keyspace),
    {ok, KeyspaceDefinition} = erlang_cassandra:describe_keyspace(Keyspace),
    ActualKeyspace = actual_keyspace(Keyspace),
    ActualKeyspace = KeyspaceDefinition#ksDef.name,
    {ok, _} = erlang_cassandra:system_drop_keyspace(Keyspace),
    true.

validate_update_keyspace(Keyspace) ->
    {ok, _} = create_keyspace(Keyspace, 1),
    % Create a keyspace with replication_factor 1
    {ok, KeyspaceDefinition1} = erlang_cassandra:describe_keyspace(Keyspace),
    <<"1">> = keyspace_replication_factor(KeyspaceDefinition1),
    % Change it to 2
    KeyspaceDefinition2 = erlang_cassandra:keyspace_definition(Keyspace, 2),
    {ok, _} = erlang_cassandra:system_update_keyspace(KeyspaceDefinition2),
    % Validate
    {ok, KeyspaceDefinition3} = erlang_cassandra:describe_keyspace(Keyspace),
    <<"2">> = keyspace_replication_factor(KeyspaceDefinition3),
    {ok, _} = erlang_cassandra:system_drop_keyspace(Keyspace),
    true.

validate_add_drop_column_family(Keyspace, ColumnFamilyDefinition) ->
    % set up the keyspace
    {ok, _} = create_keyspace(Keyspace),
    {ok, ok} = erlang_cassandra:set_keyspace(Keyspace),
    % set up the column family
    {ok, _} = erlang_cassandra:system_add_column_family(ColumnFamilyDefinition),
    ColumnFamily = ColumnFamilyDefinition#cfDef.name,
    % cleanup
    {ok, _} = erlang_cassandra:system_drop_column_family(Keyspace, ColumnFamily),
    {ok, _} = erlang_cassandra:system_drop_keyspace(Keyspace),
    true.

validate_update_column_family(Keyspace, ColumnFamilyDefinition) ->
    % set up the keyspace
    {ok, _} = create_keyspace(Keyspace),
    {ok, ok} = erlang_cassandra:set_keyspace(Keyspace),
    % set up the column family
    {ok, _} = erlang_cassandra:system_add_column_family(ColumnFamilyDefinition),
    ColumnFamily = ColumnFamilyDefinition#cfDef.name,
    % update the column family
    ColumnFamilyDefinition2 = ColumnFamilyDefinition#cfDef{gc_grace_seconds = 1000},
    {ok, _} = erlang_cassandra:system_update_column_family(ColumnFamilyDefinition2),
    {ok, ColumnFamilyDefinition3} = erlang_cassandra:system_describe_column_family(Keyspace, ColumnFamily),
    1000 = ColumnFamilyDefinition3#cfDef.gc_grace_seconds,
    % cleanup
    {ok, _} = erlang_cassandra:system_drop_column_family(Keyspace, ColumnFamily),
    {ok, _} = erlang_cassandra:system_drop_keyspace(Keyspace),
    true.

validate_truncate_column_family(Keyspace, RowKey, ColumnParent, [FirstColumn | _] = ColumnList) ->
    {ok, _} = create_keyspace(Keyspace),
    {ok, ok} = erlang_cassandra:set_keyspace(Keyspace),
    % set up the column family
    ColumnFamily = ColumnParent#columnParent.column_family,
    ColumnFamilyDefinition = erlang_cassandra:column_family_definition(Keyspace, ColumnFamily),
    {ok, _} = erlang_cassandra:system_add_column_family(ColumnFamilyDefinition),
    % insert
    lists:foreach(fun(Column) ->
                {ok, ok} = erlang_cassandra:insert(Keyspace, RowKey, ColumnParent, Column, ?CONSISTENCY_LEVEL)
        end, ColumnList),
    % truncate
    {ok, ok} = erlang_cassandra:truncate(Keyspace, ColumnFamily),
    % Validate
    LastColumn = lists:last(ColumnList),
    SlicePredicate = erlang_cassandra:slice_predicate(undefined, FirstColumn#column.name, LastColumn#column.name),
    {ok, []} = erlang_cassandra:get_slice(Keyspace, RowKey, ColumnParent, SlicePredicate, ?CONSISTENCY_LEVEL),
    % cleanup
    {ok, _} = erlang_cassandra:system_drop_column_family(Keyspace, ColumnFamily),
    {ok, _} = erlang_cassandra:system_drop_keyspace(Keyspace),
    true.

validate_insert_column(Keyspace, RowKey, ColumnParent, Column) ->
    {ok, _} = create_keyspace(Keyspace),
    {ok, ok} = erlang_cassandra:set_keyspace(Keyspace),
    % set up the column family
    ColumnFamily = ColumnParent#columnParent.column_family,
    ColumnFamilyDefinition = erlang_cassandra:column_family_definition(Keyspace, ColumnFamily),
    {ok, _} = erlang_cassandra:system_add_column_family(ColumnFamilyDefinition),
    % insert
    {ok, ok} = erlang_cassandra:insert(Keyspace, RowKey, ColumnParent, Column, ?CONSISTENCY_LEVEL),
    % cleanup
    {ok, _} = erlang_cassandra:system_drop_column_family(Keyspace, ColumnFamily),
    {ok, _} = erlang_cassandra:system_drop_keyspace(Keyspace),
    true.

validate_get_column(Keyspace, RowKey, ColumnParent, Column) ->
    {ok, _} = create_keyspace(Keyspace),
    {ok, ok} = erlang_cassandra:set_keyspace(Keyspace),
    % set up the column family
    ColumnFamily = ColumnParent#columnParent.column_family,
    ColumnFamilyDefinition = erlang_cassandra:column_family_definition(Keyspace, ColumnFamily),
    {ok, _} = erlang_cassandra:system_add_column_family(ColumnFamilyDefinition),
    % insert
    {ok, ok} = erlang_cassandra:insert(Keyspace, RowKey, ColumnParent, Column, ?CONSISTENCY_LEVEL),
    % get
    ColumnPath = erlang_cassandra:column_path(ColumnParent#columnParent.column_family,
                                              ColumnParent#columnParent.super_column ,
                                              Column#column.name),
    {ok, Response} = erlang_cassandra:get(Keyspace, RowKey, ColumnPath, ?CONSISTENCY_LEVEL),
    % validate
    Column = Response#columnOrSuperColumn.column,
    % cleanup
    {ok, _} = erlang_cassandra:system_drop_column_family(Keyspace, ColumnFamily),
    {ok, _} = erlang_cassandra:system_drop_keyspace(Keyspace),
    true.

validate_get_slice(Keyspace, RowKey, ColumnParent, [FirstColumn | _] = ColumnList) ->
    {ok, _} = create_keyspace(Keyspace),
    {ok, ok} = erlang_cassandra:set_keyspace(Keyspace),
    % set up the column family
    ColumnFamily = ColumnParent#columnParent.column_family,
    ColumnFamilyDefinition = erlang_cassandra:column_family_definition(Keyspace, ColumnFamily),
    {ok, _} = erlang_cassandra:system_add_column_family(ColumnFamilyDefinition),
    % insert
    lists:foreach(fun(Column) ->
                {ok, ok} = erlang_cassandra:insert(Keyspace, RowKey, ColumnParent, Column, ?CONSISTENCY_LEVEL)
        end, ColumnList),
    % get_slice
    LastColumn = lists:last(ColumnList),
    SlicePredicate = erlang_cassandra:slice_predicate(undefined, FirstColumn#column.name, LastColumn#column.name),
    {ok, Response} = erlang_cassandra:get_slice(Keyspace, RowKey, ColumnParent, SlicePredicate, ?CONSISTENCY_LEVEL),
    % validate
    ResponseLength = length(Response), 
    ResponseLength = length(ColumnList),
    % cleanup
    {ok, _} = erlang_cassandra:system_drop_column_family(Keyspace, ColumnFamily),
    {ok, _} = erlang_cassandra:system_drop_keyspace(Keyspace),
    true.

validate_get_range_slices(Keyspace, [FirstRow | _] = RowKeyList, ColumnParent, [FirstColumn | _] = ColumnList) ->
    {ok, _} = create_keyspace(Keyspace),
    {ok, ok} = erlang_cassandra:set_keyspace(Keyspace),
    % set up the column family
    ColumnFamily = ColumnParent#columnParent.column_family,
    ColumnFamilyDefinition = erlang_cassandra:column_family_definition(Keyspace, ColumnFamily),
    {ok, _} = erlang_cassandra:system_add_column_family(ColumnFamilyDefinition),
    % insert
    lists:foreach(fun(RowKey) ->
                lists:foreach(fun(Column) ->
                            {ok, ok} = erlang_cassandra:insert(Keyspace, RowKey, ColumnParent, Column, ?CONSISTENCY_LEVEL)
                    end, ColumnList)
        end, RowKeyList),
    % get_slice
    % Not sorted, so, just compare the data in one row
    KeyRange = erlang_cassandra:key_range(FirstRow, FirstRow),
    LastColumn = lists:last(ColumnList),
    SlicePredicate = erlang_cassandra:slice_predicate(undefined, FirstColumn#column.name, LastColumn#column.name),
    {ok, Response} = erlang_cassandra:get_range_slices(Keyspace, ColumnParent, SlicePredicate, KeyRange, ?CONSISTENCY_LEVEL),
    % validate
    KeySlice = lists:keyfind(FirstRow, #keySlice.key, Response),
    ResponseLength = length(KeySlice#keySlice.columns),
    ResponseLength = length(ColumnList),
    % cleanup
    {ok, _} = erlang_cassandra:system_drop_column_family(Keyspace, ColumnFamily),
    {ok, _} = erlang_cassandra:system_drop_keyspace(Keyspace),
    true.

validate_multiget_slice(Keyspace, RowKeyList, ColumnParent, [FirstColumn | _] = ColumnList) ->
    {ok, _} = create_keyspace(Keyspace),
    {ok, ok} = erlang_cassandra:set_keyspace(Keyspace),
    % set up the column family
    ColumnFamily = ColumnParent#columnParent.column_family,
    ColumnFamilyDefinition = erlang_cassandra:column_family_definition(Keyspace, ColumnFamily),
    {ok, _} = erlang_cassandra:system_add_column_family(ColumnFamilyDefinition),
    % insert
    lists:foreach(fun(RowKey) ->
                lists:foreach(fun(Column) ->
                            {ok, ok} = erlang_cassandra:insert(Keyspace, RowKey, ColumnParent, Column, ?CONSISTENCY_LEVEL)
                    end, ColumnList)
        end, RowKeyList),
    % get_slice
    LastColumn = lists:last(ColumnList),
    SlicePredicate = erlang_cassandra:slice_predicate(undefined, FirstColumn#column.name, LastColumn#column.name),
    {ok, Response} = erlang_cassandra:multiget_slice(Keyspace, RowKeyList, ColumnParent, SlicePredicate, ?CONSISTENCY_LEVEL),
    ResponseLength = dict:fold(fun(_Key, Value, Acc) ->
                length(Value) + Acc
        end, 0, Response),

    % validate
    ResponseLength = length(RowKeyList) * length(ColumnList),
    % cleanup
    {ok, _} = erlang_cassandra:system_drop_column_family(Keyspace, ColumnFamily),
    {ok, _} = erlang_cassandra:system_drop_keyspace(Keyspace),
    true.

validate_get_count(Keyspace, RowKey, ColumnParent, [FirstColumn | _] = ColumnList) ->
    {ok, _} = create_keyspace(Keyspace),
    {ok, ok} = erlang_cassandra:set_keyspace(Keyspace),
    % set up the column family
    ColumnFamily = ColumnParent#columnParent.column_family,
    ColumnFamilyDefinition = erlang_cassandra:column_family_definition(Keyspace, ColumnFamily),
    {ok, _} = erlang_cassandra:system_add_column_family(ColumnFamilyDefinition),
    % insert
     lists:foreach(fun(Column) ->
                {ok, ok} = erlang_cassandra:insert(Keyspace, RowKey, ColumnParent, Column, ?CONSISTENCY_LEVEL)
        end, ColumnList),
    % get_slice
    LastColumn = lists:last(ColumnList),
    SlicePredicate = erlang_cassandra:slice_predicate(undefined, FirstColumn#column.name, LastColumn#column.name),
    {ok, Response} = erlang_cassandra:get_count(Keyspace, RowKey, ColumnParent, SlicePredicate, ?CONSISTENCY_LEVEL),
    Response = length(ColumnList),
    % cleanup
    {ok, _} = erlang_cassandra:system_drop_column_family(Keyspace, ColumnFamily),
    {ok, _} = erlang_cassandra:system_drop_keyspace(Keyspace),
    true.

validate_multiget_count(Keyspace, RowKeyList, ColumnParent, [FirstColumn | _] = ColumnList) ->
    {ok, _} = create_keyspace(Keyspace),
    {ok, ok} = erlang_cassandra:set_keyspace(Keyspace),
    % set up the column family
    ColumnFamily = ColumnParent#columnParent.column_family,
    ColumnFamilyDefinition = erlang_cassandra:column_family_definition(Keyspace, ColumnFamily),
    {ok, _} = erlang_cassandra:system_add_column_family(ColumnFamilyDefinition),
    % insert
    lists:foreach(fun(RowKey) ->
                lists:foreach(fun(Column) ->
                            {ok, ok} = erlang_cassandra:insert(Keyspace, RowKey, ColumnParent, Column, ?CONSISTENCY_LEVEL)
                    end, ColumnList)
        end, RowKeyList),
    % get_slice
    LastColumn = lists:last(ColumnList),
    SlicePredicate = erlang_cassandra:slice_predicate(undefined, FirstColumn#column.name, LastColumn#column.name),
    {ok, Response} = erlang_cassandra:multiget_count(Keyspace, RowKeyList, ColumnParent, SlicePredicate, ?CONSISTENCY_LEVEL),
    ResponseLength = dict:fold(fun(_Key, Value, Acc) ->
                Value + Acc
        end, 0, Response),

    % validate
    ResponseLength = length(RowKeyList) * length(ColumnList),
    % cleanup
    {ok, _} = erlang_cassandra:system_drop_column_family(Keyspace, ColumnFamily),
    {ok, _} = erlang_cassandra:system_drop_keyspace(Keyspace),
    true.


validate_remove_column(Keyspace, RowKey, ColumnParent, Column) ->
    {ok, _} = create_keyspace(Keyspace),
    {ok, ok} = erlang_cassandra:set_keyspace(Keyspace),
    % set up the column family
    ColumnFamily = ColumnParent#columnParent.column_family,
    ColumnFamilyDefinition = erlang_cassandra:column_family_definition(Keyspace, ColumnFamily),
    {ok, _} = erlang_cassandra:system_add_column_family(ColumnFamilyDefinition),
    % insert
    {ok, ok} = erlang_cassandra:insert(Keyspace, RowKey, ColumnParent, Column, ?CONSISTENCY_LEVEL),
    % remove
    ColumnPath = erlang_cassandra:column_path(ColumnParent#columnParent.column_family,
                                              ColumnParent#columnParent.super_column ,
                                              Column#column.name),
    ColumnTimestamp = Column#column.timestamp,
    {ok, ok} = erlang_cassandra:remove(Keyspace, RowKey, ColumnPath, ColumnTimestamp, ?CONSISTENCY_LEVEL),
    % cleanup
    {ok, _} = erlang_cassandra:system_drop_column_family(Keyspace, ColumnFamily),
    {ok, _} = erlang_cassandra:system_drop_keyspace(Keyspace),
    true.

validate_add_counter(Keyspace, RowKey, ColumnParent, CounterColumn) ->
    {ok, _} = create_keyspace(Keyspace),
    {ok, ok} = erlang_cassandra:set_keyspace(Keyspace),
    % set up the column family
    ColumnFamily = ColumnParent#columnParent.column_family,
    % create a counter column family
    ColumnFamilyDefinition = erlang_cassandra:column_family_definition(Keyspace, ColumnFamily, true),
    {ok, _} = erlang_cassandra:system_add_column_family(ColumnFamilyDefinition),
    % create the counter, and double it
    {ok, ok} = erlang_cassandra:add(Keyspace, RowKey, ColumnParent, CounterColumn, ?CONSISTENCY_LEVEL),
    {ok, ok} = erlang_cassandra:add(Keyspace, RowKey, ColumnParent, CounterColumn, ?CONSISTENCY_LEVEL),
    % Validate value
    ColumnPath = erlang_cassandra:column_path(ColumnParent#columnParent.column_family,
                                              ColumnParent#columnParent.super_column ,
                                              CounterColumn#counterColumn.name),
    {ok, Response} = erlang_cassandra:get(Keyspace, RowKey, ColumnPath, ?CONSISTENCY_LEVEL),
    RCounterColumn = Response#columnOrSuperColumn.counter_column,
    NewCount = RCounterColumn#counterColumn.value,
    NewCount = CounterColumn#counterColumn.value * 2,
    % cleanup
    {ok, _} = erlang_cassandra:system_drop_column_family(Keyspace, ColumnFamily),
    {ok, _} = erlang_cassandra:system_drop_keyspace(Keyspace),
    true.

validate_remove_counter(Keyspace, RowKey, ColumnParent, CounterColumn) ->
    {ok, _} = create_keyspace(Keyspace),
    {ok, ok} = erlang_cassandra:set_keyspace(Keyspace),
    % set up the column family
    ColumnFamily = ColumnParent#columnParent.column_family,
    % create a counter column family
    ColumnFamilyDefinition = erlang_cassandra:column_family_definition(Keyspace, ColumnFamily, true),
    {ok, _} = erlang_cassandra:system_add_column_family(ColumnFamilyDefinition),
    % create the counter
    {ok, ok} = erlang_cassandra:add(Keyspace, RowKey, ColumnParent, CounterColumn, ?CONSISTENCY_LEVEL),
    % Remove the counter
    ColumnPath = erlang_cassandra:column_path(ColumnParent#columnParent.column_family,
                                              ColumnParent#columnParent.super_column ,
                                              CounterColumn#counterColumn.name),
    {ok, ok} = erlang_cassandra:remove_counter(Keyspace, RowKey, ColumnPath, ?CONSISTENCY_LEVEL),
    % cleanup
    {ok, _} = erlang_cassandra:system_drop_column_family(Keyspace, ColumnFamily),
    {ok, _} = erlang_cassandra:system_drop_keyspace(Keyspace),
    true.

validate_execute_cql_query(Keyspace) ->
    {ok, _} = create_keyspace(Keyspace),
    ActualKeyspace = actual_keyspace(Keyspace),
    Query = <<"use ", ActualKeyspace/binary, ";">>,
    {ok, Response} = erlang_cassandra:execute_cql_query(Keyspace, Query, 2),
    true = is_record(Response, cqlResult),
    {ok, _} = erlang_cassandra:system_drop_keyspace(Keyspace),
    true.

validate_prepare_and_execute_cql_query(CqlPool, Keyspace) ->
    {ok, _} = erlang_cassandra:start_cql_pool(CqlPool),
    {ok, _} = create_keyspace(Keyspace),
    ActualKeyspace = actual_keyspace(Keyspace),
    Query1 = <<"use ", ActualKeyspace/binary, ";">>,
    {ok, _Response1} = erlang_cassandra:execute_cql_query(CqlPool, Query1, 2),
    Query2 = <<"CREATE COLUMNFAMILY test (KEY int PRIMARY KEY, text_field text);">>,
    {ok, _Response2} = erlang_cassandra:execute_cql_query(CqlPool, Query2, 2),
    Query3 = <<"insert into test (KEY, text_field) values (?, ?);">>,
    {ok, Response3} = erlang_cassandra:prepare_cql_query(CqlPool, Query3, 2),
    {ok, Response4} = erlang_cassandra:execute_prepared_cql_query(CqlPool, Response3#cqlPreparedResult.itemId, [<<"123">>, <<"text">>]),
    true = is_record(Response4, cqlResult),
    {ok, _} = erlang_cassandra:system_drop_keyspace(Keyspace),
    ok = erlang_cassandra:stop_cql_pool(CqlPool),
    true.

random_name(Name) when is_list(Name) ->
    binary_to_list(random_name(list_to_binary(Name)));
random_name(Name) ->
    random:seed(erlang:now()),
    Id = list_to_binary(integer_to_list(random:uniform(999999999))),
    <<Name/binary, Id/binary>>.

create_keyspace(Keyspace) ->
    create_keyspace(Keyspace, 1).

create_keyspace(Keyspace, ReplicationFactor) ->
    KeyspaceDefinition = erlang_cassandra:keyspace_definition(Keyspace, ReplicationFactor),
    erlang_cassandra:system_add_keyspace(KeyspaceDefinition).

delete_keyspace(Keyspace) ->
    erlang_cassandra:system_drop_keyspace(Keyspace).

keyspace_replication_factor(KeyspaceDefinition) ->
    dict:fetch(<<"replication_factor">>, KeyspaceDefinition#ksDef.strategy_options).

actual_keyspace({_, _, Keyspace}) -> Keyspace;
actual_keyspace(Keyspace) -> Keyspace.

% types
keyspace_word() ->
    ?LET(X, non_empty(limited_word(?KEYSPACE_PREFIX)), keyspace(list_to_binary(X))).

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
         {Keyspace, erlang_cassandra:column_family_definition(Keyspace, ColumnFamily)}).

column_parent_item() ->
    ?LET({ColumnFamily, _SuperColumn}, {column_family_word(),
                                       super_column_word()},
%         erlang_cassandra:column_parent(ColumnFamily, SuperColumn)).
         erlang_cassandra:column_parent(ColumnFamily, undefined)).

column_item() ->
    ?LET({ColumnName, ColumnValue},
         {column_name_word(),
          column_value_word()},
         erlang_cassandra:column(ColumnName, ColumnValue, erlang_cassandra:timestamp(), undefined)).

column_item_list() ->
    ?LET({ColumnName, ColumnValue},
         {column_name_word(),
          column_value_word()},
          column_list(ColumnName, ColumnValue, erlang_cassandra:timestamp(), undefined)).

counter_column_item() ->
    ?LET({ColumnName, ColumnValue},
         {column_name_word(),
          non_neg_integer()},
         erlang_cassandra:counter_column(ColumnName, ColumnValue)).

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


cleanup_keyspaces() ->
    {ok, KeyspaceDefinitions} = erlang_cassandra:describe_keyspaces(),
    KeyspaceNames = [L#ksDef.name || L <- KeyspaceDefinitions], 
    lists:foreach(fun(Keyspace) -> 
                case binary:match(Keyspace, list_to_binary(?KEYSPACE_PREFIX)) of
                    {0, _} ->
                        erlang_cassandra:system_drop_keyspace(Keyspace);
                    _ ->
                        ok
                end
        end, KeyspaceNames).



setup_environment() ->
    random:seed(erlang:now()).

setup_lager() ->
    reltool_util:application_start(lager),
    lager:set_loglevel(lager_console_backend, debug),
    lager:set_loglevel(lager_file_backend, "console.log", debug).

start(Config) ->
    ConnectionOptions = ?config(connection_options, Config),
    PoolOptions = ?config(pool_options, Config),
    application:set_env(erlang_cassandra, pool_options, PoolOptions),
    application:set_env(erlang_cassandra, connection_options, ConnectionOptions),
    reltool_util:application_start(erlang_cassandra),
    % Eliminate the test keyspaces at start
    cleanup_keyspaces(),
    Config.

stop(_Config) ->
    cleanup_keyspaces(),
    reltool_util:application_stop(erlang_cassandra),
    ok.
