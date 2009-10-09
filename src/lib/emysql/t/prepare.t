#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -boot start_sasl -sasl sasl_error_logger false

main(_) ->
    etap:plan(unknown),
    crypto:start(),
    {Host, User, Pass, Name} = {"localhost", "test", "test", "testdatabase"},
    {ok, Pid} = mysql:start_link(test1, "localhost", 3306, User, Pass, Name, 'utf8'),

    mysql:prepare(create_foo, <<"CREATE TABLE foo (id int(11));">>),
    mysql:prepare(insert_foo, <<"INSERT INTO foo SET id = ?">>),
    mysql:prepare(delete_foo, <<"DELETE FROM foo WHERE id = ?">>),
    mysql:prepare(select_foo, <<"SELECT * FROM foo WHERE id = ?">>),
    mysql:prepare(drop_foo, <<"DROP TABLE foo">>),

    (fun() ->
        {updated, MySQLRes} = mysql:execute(test1, create_foo, [], 8000),
        etap:is(mysql:get_result_affected_rows(MySQLRes), 0, "Creating table"),
        ok
    end)(),

    (fun() ->
        {updated, MySQLRes} = mysql:execute(test1, insert_foo, [1], 8000),
        etap:is(mysql:get_result_affected_rows(MySQLRes), 1, "Creating row"),
        ok
    end)(),
    
    (fun() ->
        lists:foreach(
            fun(Data) ->
                {updated, MySQLRes} = mysql:execute(test1, insert_foo, [Data], 8000),
                etap:is(mysql:get_result_affected_rows(MySQLRes), 1, "Creating row"),
                ok
            end,
            lists:seq(2, 1000)
        ),
        ok
    end)(),
    
    (fun() ->
        {data, MySQLRes} = mysql:fetch(test1, <<"SELECT * FROM foo WHERE id = 1">>),
        etap:is(mysql:get_result_rows(MySQLRes), [[1]], "Selecting row"),
        ok
    end)(),
    
    (fun() ->
        {data, MySQLRes} = mysql:fetch(test1, [<<"SELECT * FROM foo WHERE id = 1;">>, <<"SELECT * FROM foo WHERE id = 2;">>]),
        etap:is(mysql:get_result_rows(MySQLRes), [[2]], "Selecting row"),
        ok
    end)(),

    (fun() ->
        lists:foreach(
            fun(Data) ->
                {data, MySQLRes} = mysql:execute(test1, select_foo, [Data], 8000),
                etap:is(mysql:get_result_rows(MySQLRes), [[Data]], "Selecting row"),
                ok
            end,
            lists:seq(2, 1000)
        ),
        ok
    end)(),

    (fun() ->
        {updated, MySQLRes} = mysql:execute(test1, delete_foo, [1], 8000),
        etap:is(mysql:get_result_affected_rows(MySQLRes), 1, "Deleting row"),
        ok
    end)(),

    (fun() ->
        {updated, MySQLRes} = mysql:execute(test1, drop_foo, [], 8000),
        etap:is(mysql:get_result_affected_rows(MySQLRes), 0, "Dropping table"),
        ok
    end)(),

    etap:end_tests().
