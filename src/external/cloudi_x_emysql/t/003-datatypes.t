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
	emysql:add_pool(test1, 1, "test", "test", "localhost", 3306, "testdatabase", 'utf8'),
	?DROP_TABLES(test1),

	TblDef = "CREATE TABLE foo (
		foo_dec DECIMAL,
		foo_tiny TINYINT,
		foo_long LONG,
		foo_float FLOAT,
		foo_double DOUBLE,
		foo_timestamp TIMESTAMP,
		foo_int INT,
		foo_date DATE,
		foo_time TIME,
		foo_datetime DATETIME,
		foo_year YEAR,
		foo_varchar VARCHAR(255),
		foo_bit BIT,
		foo_blob BLOB )",
	Foo = emysql:execute(test1, TblDef),
	etap:is(is_record(Foo, ok_packet), true, "create table ok"),
	
	FooInsert = "INSERT INTO foo VALUES (
		1.0,
		2,
		999999999,
		(1/3),
		(1/3),
		'2009-01-01 12:12:12',
		100,
		'2009-01-01',
		'12:12:12',
		'2009-01-01 12:12:12',
		2009,
		'asdf',
		true,
		'asdf'
	)",
	Insert = emysql:execute(test1, FooInsert),
	etap:is(is_record(Insert, ok_packet), true, "insert data ok"),

	Select = emysql:execute(test1, "SELECT * FROM foo"),
	[Row] = Select#result_packet.rows,
	
	etap:is(lists:nth(1, Row), 1, "decimal matches"),
	etap:is(lists:nth(2, Row), 2, "tinyint matches"),
	%etap:is(lists:nth(3, Row), 999999999, "long matches"),
	etap:is(lists:nth(4, Row), 0.333333, "float matches"),
	etap:is(lists:nth(5, Row), 0.333333333, "double matches"),
	etap:is(lists:nth(6, Row), {datetime,{{2009,1,1},{12,12,12}}}, "timestamp matches"),
	etap:is(lists:nth(7, Row), 100, "int matches"),
	etap:is(lists:nth(8, Row), {date,{2009,1,1}}, "date matches"),
	etap:is(lists:nth(9, Row), {time,{12,12,12}}, "time matches"),
	etap:is(lists:nth(10, Row), {datetime,{{2009,1,1},{12,12,12}}}, "datetime matches"),
	etap:is(lists:nth(11, Row), 2009, "year matches"),
	etap:is(lists:nth(12, Row), <<"asdf">>, "varchar matches"),
	etap:is(lists:nth(13, Row), 1, "bit matches"),
	etap:is(lists:nth(14, Row), <<"asdf">>, "blob matches"),
		
    etap:end_tests().
