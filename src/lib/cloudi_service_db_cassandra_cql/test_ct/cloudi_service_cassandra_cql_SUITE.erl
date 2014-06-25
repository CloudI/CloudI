%%% BSD LICENSE
%%%
%%% Copyright (c) 2014, Irina Guberman <irina.guberman@gmail.com>
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
%%%         software developed by Irina Guberman
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
%%% @author Irina Guberman <irina.guberman@gmail.com>
%%% @copyright 2014 Irina Guberman
%%% @version 1.0.0 06/12/2014
%%%------------------------------------------------------------------------

-module(cloudi_service_cassandra_cql_SUITE).
-author("irinaguberman").


%% Args default definitions
-define(DEFAULT_KEYSPACE, undefined).
-define(DEFAULT_QUERIES, []).
-define(DEFAULT_CQL_HOST, "localhost").
-define(DEFAULT_CQL_PORT, 9042).

-define(DEFAULT_SERVICE_NAME, "CQL").
-define(DEFAULT_CONSISTENCY, quorum).


-define(DEFAULT_CONNECTION_OPTIONS,
    [{host, ?DEFAULT_CQL_HOST},
        {port, ?DEFAULT_CQL_PORT},
        {keepalive, true},
        {use, ?TEST_KEYSPACE},
        {cql_version, <<"3.1.5">>},
        {auto_reconnect, true}
    ]).

-define(DEFAULTS, [{service_name, ?DEFAULT_SERVICE_NAME},
    {connection_options, ?DEFAULT_CONNECTION_OPTIONS},
    {consistency, ?DEFAULT_CONSISTENCY}]).

-define(DEFAULT_TIMEOUT_INIT, 70000).
-define(DEFAULT_TIMEOUT_SYNC, 70000).
-define(DEFAULT_TIMEOUT_ASYNC, 70000).
-define(DEFAULT_TIMEOUT_REMOVE, 70000).

%% API
-export([]).

%% CT callbacks
-export([all/0,
    init_per_suite/1,
    end_per_suite/1]).

%% test callbacks
-export([test_cloudi_service_cassandra_cql/1]).

-include_lib("common_test/include/ct.hrl").

-define(TEST_PREFIX, "/test/cassandra/cql/").
-define(TEST_SERVICE_NAME, "test_cassandra_target").
-define(TEST_KEYSPACE, <<"test_keyspace">>).

-define(TEST_POOL_SIZE, 5).

-define(CREATE_TEST_KEYSPACE,
    "CREATE KEYSPACE IF NOT EXISTS test_keyspace WITH replication = {
    'class': 'SimpleStrategy',
    'replication_factor': '1'};").

-define(CREATE_TEST_TABLE,
    "CREATE TABLE IF NOT EXISTS test_keyspace.test_cql(id1 text, id2 int, payload blob, PRIMARY KEY(id1, id2));").


all() -> [test_cloudi_service_cassandra_cql].

init_per_suite(Config) ->
    ok = create_test_schema(),
    setup_cloudi(Config),
    ct:log("Staring cloudi_service_cassandra_cql..."),
    Config2 = setup_cloudi_service(Config),
    _C = cloudi:new(),
    Config2.


create_test_schema() ->
    {ok, ClientPid} = cloudi_x_erlcql_client:start_link([]),

    Res = cloudi_x_erlcql_client:async_query(ClientPid, ?CREATE_TEST_KEYSPACE, quorum),

    case Res of
        {ok, QueryRef} ->
            cloudi_x_erlcql_client:await(QueryRef),
            create_test_table(ClientPid);
        {error, _Reason} = Error ->
            ct:log("Error creating test keyspace: ~p", [Error]),
            error
    end.

create_test_table(ClientPid) ->
    Res = cloudi_x_erlcql_client:async_query(ClientPid, ?CREATE_TEST_TABLE, quorum),
    case Res of
        {ok, QueryRef} ->
            Resp = cloudi_x_erlcql_client:await(QueryRef),
            ct:log("Created test table with Resp ~p", [Resp]),
            ok;
        {error, _Reason} = Error ->
            ct:log("Error creating test table: ~p", [Error]),
            error
    end.


end_per_suite(Config) ->
    ok = cloudi_service_api:services_remove(?config(serviceIDs, Config), ?DEFAULT_TIMEOUT_REMOVE),
    cloudi_x_reltool_util:application_stop(cloudi_core),
    cloudi_x_reltool_util:application_stop(cloudi_service_db_cassandra_cql).


setup_cloudi(_Config) ->
    CloudIConfig = [{acl, []}, {services, []}, {nodes, []},
        {logging, [{file, "cloudi.log"}]}],
    ok = cloudi_x_reltool_util:application_start(cloudi_core,
        [{configuration, CloudIConfig}], 1000),
    ok = cloudi_x_reltool_util:application_start(cloudi_service_db_cassandra_cql).


setup_cloudi_service(Config) ->
    ServiceConfig = [{type, internal},
        {prefix, ?TEST_PREFIX},
        {module, cloudi_service_db_cassandra_cql},
        {args, [{service_name, ?TEST_SERVICE_NAME},
            {connection_options, [
                {host, ?DEFAULT_CQL_HOST},
                {port, ?DEFAULT_CQL_PORT},
                {use, ?TEST_KEYSPACE},
                {prepare, test_queries()},
                {cql_version, <<"3.1.5">>},
                {auto_reconnect, true},
                {keepalive, true}]},
            {consistency, quorum}
        ]},
        {dest_refresh, immediate_closest},
        {timeout_init, ?DEFAULT_TIMEOUT_INIT},
        {timeout_async, ?DEFAULT_TIMEOUT_ASYNC},
        {timeout_sync, ?DEFAULT_TIMEOUT_SYNC},
        {count_process, ?TEST_POOL_SIZE},
        {max_r, 5},
        {max_t, 300},
        {options, [{automatic_loading, false}]}],
    {ok, ServiceIDs} = cloudi_service_api:services_add([ServiceConfig], ?DEFAULT_TIMEOUT_INIT),
    [{serviceIDs, ServiceIDs} | Config].

test_cloudi_service_cassandra_cql(_Config) ->
    Ctx = cloudi:new([{dest_refresh, immediate_closest}]),

    Name = ?TEST_PREFIX ++ ?TEST_SERVICE_NAME,

    {ok, Resp} = cloudi_service_db_cassandra_cql:execute_prepared_query(
        Ctx, Name, insert_into_test_table, [<<"test1">>, 1, <<"TEST BLOB1_1">>]),
    {ok, Resp} = cloudi_service_db_cassandra_cql:execute_prepared_query(
        Ctx, Name, insert_into_test_table, [<<"test1">>, 2, <<"TEST BLOB1_2">>], local_quorum),
    {ok, Resp} = cloudi_service_db_cassandra_cql:execute_prepared_query(
        Ctx, Name, insert_into_test_table, [<<"test1">>, 3, <<"TEST BLOB1_3">>]),
    {ok, Resp} = cloudi_service_db_cassandra_cql:execute_prepared_query(
        Ctx, Name, insert_into_test_table, [<<"test2">>, 1, <<"TEST BLOB2_1">>]),
    {ok, Resp} = cloudi_service_db_cassandra_cql:execute_prepared_query(
        Ctx, Name, insert_into_test_table, [<<"test2">>, 2, <<"TEST BLOB2_2">>], local_quorum),
    {ok, Resp} = cloudi_service_db_cassandra_cql:execute_prepared_query(
        Ctx, Name, insert_into_test_table, [<<"test2">>, 3, <<"TEST BLOB2_3">>]),
    timer:sleep(1000),
    {ok, {[[<<"test2">>, 1, <<"TEST BLOB2_1">>],
        [<<"test2">>, 2, <<"TEST BLOB2_2">>],
        [<<"test2">>, 3, <<"TEST BLOB2_3">>],
        [<<"test1">>, 1, <<"TEST BLOB1_1">>],
        [<<"test1">>, 2, <<"TEST BLOB1_2">>],
        [<<"test1">>, 3, <<"TEST BLOB1_3">>]],
        [{<<"id1">>, varchar},
            {<<"id2">>, int},
            {<<"payload">>, blob}]}} = cloudi_service_db_cassandra_cql:execute_prepared_query(
        Ctx, Name, select_all_from_test_table, []),

    {ok,
        {[[6]],
            [{<<"count">>,
                bigint}]}} = cloudi_service_db_cassandra_cql:execute_prepared_query(
        Ctx, Name, select_count_all_from_test_table, [], local_quorum),


    {ok, {[
        [<<"test1">>, 1, <<"TEST BLOB1_1">>],
        [<<"test1">>, 2, <<"TEST BLOB1_2">>],
        [<<"test1">>, 3, <<"TEST BLOB1_3">>]],
        [{<<"id1">>, varchar},
            {<<"id2">>, int},
            {<<"payload">>, blob}]}} = cloudi_service_db_cassandra_cql:execute_prepared_query(
        Ctx, Name, select_by_first_key_from_test_table, [<<"test1">>]),

    {ok, Resp} = cloudi_service_db_cassandra_cql:execute_prepared_query(
        Ctx, Name, delete_by_first_key_from_test_table, [<<"test1">>]),
    timer:sleep(200),

    %% prepared
    {ok,
        {[[3]],
            [{<<"count">>,
                bigint}]}} = cloudi_service_db_cassandra_cql:execute_prepared_query(
        Ctx, Name, select_count_all_from_test_table, []),

    %% test unprepared query sent as string()
    {ok,
        {[[3]],
            [{<<"count">>,
                bigint}]}} = cloudi_service_db_cassandra_cql:execute_query(
        Ctx, Name, "select COUNT(*) from test_cql;"),

    {ok, Resp} = cloudi_service_db_cassandra_cql:execute_prepared_query(
        Ctx, Name, delete_by_first_key_from_test_table, [<<"test2">>], local_quorum),

    timer:sleep(200),
    {ok,
        {[[0]],
            [{<<"count">>,
                bigint}]}} = cloudi_service_db_cassandra_cql:execute_prepared_query(
        Ctx, Name, select_count_all_from_test_table, []),

    %% test unprepared query sent as binary() and with request specific consistency
   {ok,
        {[[0]],
            [{<<"count">>,
                bigint}]}} = cloudi_service_db_cassandra_cql:execute_query(
        Ctx, Name, select_count_all_from_test_table(), all),

    {ok,dropped} = cloudi_service_db_cassandra_cql:execute_query(Ctx, Name, "drop keyspace test_keyspace;").


test_queries() ->
    [{insert_into_test_table, insert_into_test_table()},
        {select_all_from_test_table, select_all_from_test_table()},
        {select_count_all_from_test_table, select_count_all_from_test_table()},
        {select_count_by_first_key_from_test_table, select_count_by_first_key_from_test_table()},
        {select_by_first_key_from_test_table, select_by_first_key_from_test_table()},
        {delete_by_first_key_from_test_table, delete_by_first_key_from_test_table()}].


insert_into_test_table() ->
    <<"INSERT into test_cql(id1, id2, payload) values (?, ?, ?);">>.

select_all_from_test_table() ->
    <<"SELECT * from test_cql;">>.

select_by_first_key_from_test_table() ->
    <<"SELECT * from test_cql where id1 = ?;">>.

delete_by_first_key_from_test_table() ->
    <<"DELETE from test_cql where id1 = ?;">>.

select_count_by_first_key_from_test_table() ->
    <<"select COUNT(*) from test_cql where id1 = ?;">>.

select_count_all_from_test_table() ->
    <<"select COUNT(*) from test_cql;">>.
