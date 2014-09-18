#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../emysql ./ebin -sasl sasl_error_logger false

-include_lib("emysql/include/emysql.hrl").
-include_lib("emysql/t/mysql_test.hrl").

main(_) ->
    etap:plan(unknown),
	error_logger:tty(false),
	application:start(crypto),
	application:start(emysql),
	emysql:add_pool(test1, 4, "test", "test", "localhost", 3306, "testdatabase", 'utf8'),
	?DROP_TABLES(test1),
		
	Foo = emysql:execute(test1, "CREATE TABLE foo (
									id int(32) NOT NULL AUTO_INCREMENT, 
									name varchar(50) NOT NULL, 
									PRIMARY KEY (id)
								)"),
	etap:is(is_record(Foo, ok_packet), true, "create table returned ok packet"),
	
	[Pool] = emysql_conn_mgr:pools(),
		
	Conn = hd(queue:to_list(Pool#pool.available)),
	etap:is(emysql_statements:all(), {state, gb_trees:empty(), gb_trees:empty()}, "statements are empty"),
	etap:is(emysql_statements:fetch(foo_billy), undefined, "statement not prepared yet"),
	etap:is(emysql_statements:add(foo_billy, "SELECT * FROM foo"), ok, "added statement to state"),
	etap:is(emysql_statements:fetch(foo_billy), {1, "SELECT * FROM foo"}, "fetched statement and version from state"),
	etap:is(emysql_statements:version(Conn#connection.id, foo_billy), undefined, "statement not prepared for conneciton"),
	etap:is(emysql_statements:prepare(Conn#connection.id, foo_billy, 1), ok, "prepare statement for connection"),
	etap:is(emysql_statements:version(Conn#connection.id, foo_billy), 1, "statement prepared for conneciton"),
	etap:is(emysql_statements:add(foo_billy, "SELECT name FROM foo"), ok, "added statement to state"),
	etap:is(emysql_statements:fetch(foo_billy), {2, "SELECT name FROM foo"}, "fetched statement and version from state"),
	
	etap:is(emysql:prepare(create_foo, "INSERT INTO foo (name) VALUES (?)"), ok, "prepared statements"),
	etap:is(emysql_statements:fetch(create_foo), {1, "INSERT INTO foo (name) VALUES (?)"}, "fetched statement and version from state"),
	etap:is(emysql_statements:version(Conn#connection.id, create_foo), undefined, "statement not prepared for conneciton"),
		
	[begin
		etap:is(is_record(emysql_conn:execute(Conn, create_foo, ["conn " ++ Conn#connection.id]), ok_packet), true, "execute prepared stmt ok")
	 end || Conn <- queue:to_list(Pool#pool.available)],
	
	etap:is(emysql_statements:version(Conn#connection.id, create_foo), 1, "statement prepared for conneciton"),
	
	etap:is(length((emysql:execute(test1, "SELECT * FROM foo"))#result_packet.rows), 4, "correct number of rows were inserted"),
	
	etap:is(emysql:prepare(foo_all, "SELECT * FROM foo"), ok, "prepared statement"),
	etap:is(emysql:prepare(foo_by_id_name, "SELECT * FROM foo WHERE id = ? and name like ?"), ok, "prepared statement"),
	
	etap:is(emysql:execute(test1, foo_all), emysql:execute(test1, foo_all, []), "statements with and without empty args list matches"),
	etap:is(emysql:execute(test1, "SELECT * FROM foo"), emysql:execute(test1, "SELECT * FROM foo", []), "queries with and without empty args list matches"),
	
	etap:is(
		emysql:execute(test1, foo_by_id_name, [1, "conn%"]), 
		emysql:execute(test1, "SELECT * FROM foo WHERE id = 1 and name like 'conn%'"),
		"statement and query with same args return matching results"
	),
	
	etap:end_tests().
	