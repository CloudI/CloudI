#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../emysql ./ebin -sasl sasl_error_logger false

-include_lib("emysql/include/emysql.hrl").
-include_lib("emysql/t/mysql_test.hrl").

main(_) ->
    etap:plan(16),
	error_logger:tty(false),
	application:start(crypto),
	application:start(emysql),
	emysql:add_pool(test1, 2, "test", "test", "localhost", 3306, "testdatabase", 'utf8'),
	?DROP_TABLES(test1),
	
	%% LOCK BOTH CONNECTIONS FOR 1 SEC
	spawn(fun() -> emysql:execute(test1, "SELECT SLEEP(1)"), etap:is(1,1,"pid1 finished sleeping") end),
	spawn(fun() -> emysql:execute(test1, "SELECT SLEEP(1)"), etap:is(1,1,"pid2 finished sleeping") end),
	
	timer:sleep(10),
	
	etap:is((emysql:execute(test1, "show tables"))#result_packet.rows, [], "fetched empty table list"),
	
	timer:sleep(1000),
	
	%% AFTER 1 SEC ALL CONNECTIONS ARE UNLOCKED
	etap:is(gb_trees:size((hd(emysql_conn_mgr:pools()))#pool.locked), 0, "connections unlocked"),
	
	etap:is(emysql_conn_mgr:waiting(), queue:new(), "waiting queue is empty"),
	
	ConnIDs = [Conn#connection.id || Conn <- queue:to_list((hd(emysql_conn_mgr:pools()))#pool.available)],
	
	spawn(fun() -> etap:is((catch emysql:execute(test1, "SELECT SLEEP(10)")), {'EXIT',mysql_timeout}, "timeout ok") end),
	spawn(fun() -> etap:is((catch emysql:execute(test1, "SELECT SLEEP(10)")), {'EXIT',mysql_timeout}, "timeout ok") end),
	
	timer:sleep(10),
	
	etap:is((catch emysql:execute(test1, "show tables")), {'EXIT',connection_lock_timeout}, "timed out waiting for connection"),
		
	etap:is((hd(emysql_conn_mgr:pools()))#pool.available, queue:new(), "all connections locked"),
	etap:is(queue:len(emysql_conn_mgr:waiting()), 1, "waiting queue is empty"),
		
	timer:sleep(5000),
	
	etap:is(gb_trees:size((hd(emysql_conn_mgr:pools()))#pool.locked), 0, "connections unlocked"),
	
	[etap:is(lists:member(Conn#connection.id, ConnIDs), false, "connection has been replaced") || Conn <- queue:to_list((hd(emysql_conn_mgr:pools()))#pool.available)],
	
	etap:end_tests().