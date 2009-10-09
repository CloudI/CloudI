#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -boot start_sasl -sasl sasl_error_logger false

main(_) ->
    etap:plan(unknown),
    crypto:start(),
    {Host, User, Pass, Name} = {"localhost", "test", "test", "testdatabase"},
    {ok, Pid} = mysql:start_link(test1, Host, undefined, User, Pass, Name, 'utf8'),
    ok = mysql:connect(test1, Host, 3306, User, Pass, Name, 'utf8'),
    ok = mysql:connect(test1, Host, undefined, User, Pass, Name, 'utf8'),

    etap:is((catch mysql:connect(test, Host, 3305, User, Pass, Name, 'utf8')), {error, econnrefused}, "invalid server"),

    mysql:prepare(create_foo, <<"CREATE TABLE bar (id int(11));">>),
    mysql:prepare(insert_foo, <<"INSERT INTO bar SET id = ?">>),
    mysql:prepare(delete_foo, <<"DELETE FROM bar WHERE id = ?">>),
    mysql:prepare(select_foo, <<"SELECT * FROM bar WHERE id = ?">>),
    mysql:prepare(drop_foo, <<"DROP TABLE bar">>),

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
        Parent = self(),
        lists:foreach(
            fun(Data) -> spawn_link(fun() ->
                {updated, MySQLRes} = mysql:execute(test1, insert_foo, [Data], 8000),
                Parent ! mysql:get_result_affected_rows(MySQLRes)
            end) end,
            lists:seq(2, 10)
        ),
        lists:foreach(fun(_Data) ->
            etap:is(receive X -> X end, 1, "Row created")
        end, lists:seq(2, 10)),
        ok
    end)(),

    (fun() ->
        Parent = self(),
        lists:foreach(
            fun(Data) -> spawn_link(fun() ->
                {data, MySQLRes} = mysql:execute(test1, select_foo, [Data], 8000),
                Parent ! mysql:get_result_rows(MySQLRes)
            end) end,
            lists:seq(2, 10)
        ),
        Collected = lists:sort([receive X -> X end || _Y <- lists:seq(2, 10)]),
        CollectedCompare = [[[Y]] || Y <- lists:seq(2, 10)],
        etap:is(Collected, CollectedCompare, "selected comparison works"),
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
