#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../emysql ./ebin -sasl sasl_error_logger false

-include_lib("emysql/include/emysql.hrl").

main(_) ->
    etap:plan(unknown),
	
	etap_application:load_ok(crypto, "Application 'crypto' loaded"),
	etap_application:load_ok(emysql, "Application 'emysql' loaded"),
	
	[etap_can:loaded_ok(Module, lists:concat(["Module '", Module, "' loaded."])) || Module <- emysql:modules()],

	application:set_env(emysql, pools, [
		{test1, [
			{size, 2},
			{user, "test"},
			{password, "test"},
			{host, "localhost"},
			{port, 3306},
			{database, "testdatabase"},
			{encoding, 'utf8'}
		]},
		{test2, [
			{size, 3},
			{user, "test"},
			{password, "test"},
			{host, "localhost"},
			{port, 3306},
			{database, "testdatabase"},
			{encoding, 'utf8'}
		]}
	]),

	etap_application:start_ok(crypto, "Application 'crypto' started"),
	etap_application:start_ok(emysql, "Application 'emysql' started"),
	
	%% CHECK INITIAL STATE FOR CORRECT POOL AND CONNECTION RECORDS
	(fun() ->
		Pools = emysql_conn_mgr:pools(),
		etap:is(length(Pools), 2, "state contains correct number of pools"),
		{value, Pool1} = lists:keysearch(test1, 2, Pools),
		{value, Pool2} = lists:keysearch(test2, 2, Pools),
		etap:is(is_record(Pool1, pool) andalso is_record(Pool2, pool), true, "state contains pool records"),
		etap:is(queue:len(Pool1#pool.available), 2, "pool1 contains correct number of connections"),
		etap:is(queue:len(Pool2#pool.available), 3, "pool2 contains correct number of connections"),
		ok
	 end)(),
	
	(fun() ->
		Conn1 = emysql_conn_mgr:lock_connection(test1),
		Pools = emysql_conn_mgr:pools(),
		{value, Pool1} = lists:keysearch(test1, 2, Pools),
		etap:is(gb_trees:values(Pool1#pool.locked), [Conn1], "locked connection is locked"),
		etap:is(lists:filter(fun(C) -> C#connection.id == Conn1#connection.id end, queue:to_list(Pool1#pool.available)), [], "connection is not available"),
		ok
	 end)(),
	
	(fun() ->
		Conn2 = emysql_conn_mgr:lock_connection(test1),
		Pools = emysql_conn_mgr:pools(),
		{value, Pool1} = lists:keysearch(test1, 2, Pools),
		etap:is(gb_trees:size(Pool1#pool.locked), 2, "both connections locked"),
		etap:is(Pool1#pool.available, queue:new(), "no connections available"),
		ok
	 end)(),

	etap:is((catch emysql_conn_mgr:lock_connection(undefined)), {'EXIT', pool_not_found}, "pool_not_found error returned successfully"),
	etap:is((catch emysql_conn_mgr:unlock_connection(#connection{pool_id=test1})), {'EXIT', connection_not_found}, "connection_not_found error returned successfully"),
		
	application:stop(emysql),
	application:load(emysql),
	application:set_env(emysql, pools, [
		{test1, [
			{size, 0},
			{user, "test"},
			{password, "test"},
			{host, "localhost"},
			{port, 3306},
			{database, "testdatabase"},
			{encoding, 'utf8'}
		]}
	]),	
	application:start(emysql),
	etap:is((catch emysql_conn_mgr:lock_connection(test1)), unavailable, "connection_pool_is_empty error returned successfully"),
		
	(fun() ->
		etap:is(emysql:increment_pool_size(test1, 5), ok, "increment pool size"),
		etap:is(queue:len((hd(emysql_conn_mgr:pools()))#pool.available), 5, "correct number of connections are open"),
		etap:is(emysql:decrement_pool_size(test1, 3), ok, "decrement pool size"),
		etap:is(queue:len((hd(emysql_conn_mgr:pools()))#pool.available), 2, "correct number of connections are open"),
		etap:is(emysql:decrement_pool_size(test1, 100), ok, "decrement pool size"),
		etap:is(queue:len((hd(emysql_conn_mgr:pools()))#pool.available), 0, "correct number of connections are open"),
		ok
	 end)(),
	
	application:stop(emysql),
	application:unload(emysql),
	application:load(emysql),
	application:start(emysql),	
	
	(fun() ->
		etap:is(emysql_conn_mgr:pools(), [], "pools empty"),
		etap:is(emysql:add_pool(test2, 1, "test", "test", "localhost", 3306, "testdatabase", 'utf8'), ok, "added pool"),
		etap:is((catch emysql:add_pool(test2, 1, "test", "test", "localhost", 3306, "testdatabase", 'utf8')), {'EXIT', pool_already_exists}, "pool exists"),
		Conn = (catch emysql_conn_mgr:lock_connection(test2)),
		etap:is(is_record(Conn, connection), true, "returned valid connection"),
		etap:is(is_list(erlang:port_info(Conn#connection.socket)), true, "socket is open"),
		etap:is(emysql:remove_pool(test2), ok, "removed pool successfully"),
		etap:is(erlang:port_info(Conn#connection.socket), undefined, "socket has been closed"),
		ok
	 end)(),
	
    etap:end_tests().
