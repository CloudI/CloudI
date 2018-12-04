-module(pgsql_connection_test).
-include_lib("eunit/include/eunit.hrl").

%%%% CREATE ROLE test LOGIN;
%%%% ALTER USER test WITH SUPERUSER
%%%%
%%%% CREATE DATABASE test WITH OWNER=test;
%%%%

kill_sup(SupPid) ->
    OldTrapExit = process_flag(trap_exit, true),
    exit(SupPid, kill),
    receive {'EXIT', SupPid, _Reason} -> ok after 5000 -> throw({error, timeout}) end,
    process_flag(trap_exit, OldTrapExit).


open_close_test_() ->
    {setup,
    fun() ->
        {ok, Pid} = pgsql_connection_sup:start_link(),
        Pid
    end,
    fun(SupPid) ->
        kill_sup(SupPid)
    end,
    [
        {"Open connection to test database with test account",
        ?_test(begin
            R = pgsql_connection:open("test", "test"),
            pgsql_connection:close(R)
        end)},
        {"Open connection to test database with test account, expliciting empty password",
        ?_test(begin
            R = pgsql_connection:open("test", "test", ""),
            pgsql_connection:close(R)
        end)},
        {"Open connection to test database with test account, expliciting host",
        ?_test(begin
            R = pgsql_connection:open("0.0.0.0", "test", "test", ""),
            pgsql_connection:close(R)
        end)},
        {"Open connection to test database with test account, expliciting host, using IP for host and binaries for account/database/password",
        ?_test(begin
            R = pgsql_connection:open({0,0,0,0}, <<"test">>, <<"test">>, <<>>),
            pgsql_connection:close(R)
        end)},
        {"Open connection to test database with test account, expliciting host and options",
        ?_test(begin
            R = pgsql_connection:open("0.0.0.0", "test", "test", "", [{application_name, eunit_tests}]),
            pgsql_connection:close(R)
        end)},
        {"Open connection to test database with options as list",
        ?_test(begin
            R = pgsql_connection:open([{host, "0.0.0.0"}, {database, "test"}, {user, "test"}, {password, ""}]),
            pgsql_connection:close(R)
        end)},
        {"Bad user throws",
        ?_test(begin
            try
                R = pgsql_connection:open("test", "bad_user"),
                pgsql_connection:close(R),
                ?assert(false)
            catch throw:{pgsql_error, _Error} ->
                ok
            end
        end)}
    ]}.

reconnect_proxy_loop() ->
    {ok, LSock} = gen_tcp:listen(35432, [{active, true}, binary, {reuseaddr, true}]),    
    reconnect_proxy_loop0(LSock, undefined, undefined).

reconnect_proxy_loop0(LSock, undefined, undefined) ->
    {ok, CSock} = gen_tcp:accept(LSock),
    {ok, PSock} = gen_tcp:connect({0, 0, 0, 0}, 5432, [{active, true}, binary]),
    reconnect_proxy_loop0(LSock, CSock, PSock);
reconnect_proxy_loop0(LSock, CSock, PSock) ->
    receive
        {TestClient, close} ->
            ok = gen_tcp:close(CSock),
            ok = gen_tcp:close(PSock),
            TestClient ! {self(), closed},
            reconnect_proxy_loop0(LSock, undefined, undefined);
        {_TestClient, close_during_xfer} ->
            receive {tcp, CSock, _} -> ok end,
            ok = gen_tcp:close(CSock),
            ok = gen_tcp:close(PSock),
            reconnect_proxy_loop0(LSock, undefined, undefined);
        {tcp, CSock, Data} ->
            ok = gen_tcp:send(PSock, Data),
            reconnect_proxy_loop0(LSock, CSock, PSock);
        {tcp, PSock, Data} ->
            ok = gen_tcp:send(CSock, Data),
            reconnect_proxy_loop0(LSock, CSock, PSock);
        {tcp_closed, CSock} ->
            ok = gen_tcp:close(PSock),
            reconnect_proxy_loop0(LSock, undefined, undefined);
        {tcp_closed, PSock} ->
            ok = gen_tcp:close(CSock),
            reconnect_proxy_loop0(LSock, undefined, undefined);
        Message ->
            ?debugVal(Message),
            ?assert(false)
    end.

reconnect_test_() ->
    {setup,
    fun() ->
        {ok, SupPid} = pgsql_connection_sup:start_link(),
        ProxyPid = spawn_link(fun reconnect_proxy_loop/0),
        {SupPid, ProxyPid}
    end,
    fun({SupPid, ProxyPid}) ->
        unlink(ProxyPid),
        exit(ProxyPid, normal),
        kill_sup(SupPid)
    end,
    fun({_SupPid, ProxyPid}) ->
        [
            {"Reconnect after close",
            ?_test(begin
                Conn = pgsql_connection:open([{host, "0.0.0.0"}, {port, 35432}, {database, "test"}, {user, "test"}, {password, ""}, reconnect]),
                ?assertEqual({{select, 1}, [{null}]}, pgsql_connection:simple_query("select null", Conn)),
                ProxyPid ! {self(), close},
                receive {ProxyPid, closed} -> ok end,
                timer:sleep(100),   % make sure the driver got the tcp closed notice.
                ?assertEqual({{select, 1}, [{null}]}, pgsql_connection:simple_query("select null", Conn)),
                ?assertEqual({{select, 1}, [{null}]}, pgsql_connection:simple_query("select null", Conn)),
                ok = pgsql_connection:close(Conn)
            end)},
            {"Socket is closed during transfer, driver returns {error, closed} even with reconnect",
            ?_test(begin
                Conn = pgsql_connection:open([{host, "0.0.0.0"}, {port, 35432}, {database, "test"}, {user, "test"}, {password, ""}, reconnect]),
                ?assertEqual({{select, 1}, [{null}]}, pgsql_connection:simple_query("select null", Conn)),
                ProxyPid ! {self(), close_during_xfer},
                ?assertEqual({error, closed}, pgsql_connection:simple_query("select null", Conn)),
                ?assertEqual({{select, 1}, [{null}]}, pgsql_connection:simple_query("select null", Conn)),
                ok = pgsql_connection:close(Conn)
            end)},
            {"Socket is closed during transfer, driver does not return {error, closed} with retry",
            ?_test(begin
                Conn = pgsql_connection:open([{host, "0.0.0.0"}, {port, 35432}, {database, "test"}, {user, "test"}, {password, ""}, reconnect]),
                ?assertEqual({{select, 1}, [{null}]}, pgsql_connection:simple_query("select null", Conn)),
                ProxyPid ! {self(), close_during_xfer},
                ?assertEqual({{select, 1}, [{null}]}, pgsql_connection:simple_query("select null", [retry], Conn)),
                ok = pgsql_connection:close(Conn)
            end)},
            {"Do not reconnect at all without reconnect",
            ?_test(begin
                Conn = pgsql_connection:open([{host, "0.0.0.0"}, {port, 35432}, {database, "test"}, {user, "test"}, {password, ""}, {reconnect, false}]),
                ?assertEqual({{select, 1}, [{null}]}, pgsql_connection:simple_query("select null", Conn)),
                ProxyPid ! {self(), close},
                receive {ProxyPid, closed} -> ok end,
                timer:sleep(100),   % make sure the driver got the tcp closed notice.
                ?assertEqual({error, closed}, pgsql_connection:simple_query("select null", Conn)),
                ?assertEqual({error, closed}, pgsql_connection:simple_query("select null", Conn)),
                ok = pgsql_connection:close(Conn)
            end)}
        ]
    end
    }.

select_null_test_() ->
    {setup,
    fun() ->
        {ok, SupPid} = pgsql_connection_sup:start_link(),
        Conn = pgsql_connection:open("test", "test"),
        {SupPid, Conn}
    end,
    fun({SupPid, Conn}) ->
        pgsql_connection:close(Conn),
        kill_sup(SupPid)
    end,
    fun({_SupPid, Conn}) ->
    [
        ?_assertEqual({selected, [{null}]}, pgsql_connection:sql_query("select null", Conn)),
        ?_assertEqual({selected, [{null}]}, pgsql_connection:param_query("select null", [], Conn)),
        ?_assertEqual({{select, 1}, [{null}]}, pgsql_connection:simple_query("select null", Conn)),
        ?_assertEqual({{select, 1}, [{null}]}, pgsql_connection:extended_query("select null", [], Conn))
    ]
    end}.

sql_query_test_() ->
    {setup,
    fun() ->
        {ok, SupPid} = pgsql_connection_sup:start_link(),
        Conn = pgsql_connection:open("test", "test"),
        {SupPid, Conn}
    end,
    fun({SupPid, Conn}) ->
        pgsql_connection:close(Conn),
        kill_sup(SupPid)
    end,
    fun({_SupPid, Conn}) ->
    [
        {"Create temporary table",
            ?_assertEqual({updated, 1}, pgsql_connection:sql_query("create temporary table foo (id integer primary key, some_text text)", Conn))
        },
        {"Insert into",
            ?_assertEqual({updated, 1}, pgsql_connection:sql_query("insert into foo (id, some_text) values (1, 'hello')", Conn))
        },
        {"Update",
            ?_assertEqual({updated, 1}, pgsql_connection:sql_query("update foo set some_text = 'hello world'", Conn))
        },
        {"Insert into",
            ?_assertEqual({updated, 1}, pgsql_connection:sql_query("insert into foo (id, some_text) values (2, 'hello again')", Conn))
        },
        {"Update on matching condition",
            ?_assertEqual({updated, 1}, pgsql_connection:sql_query("update foo set some_text = 'hello world' where id = 1", Conn))
        },
        {"Update on non-matching condition",
            ?_assertEqual({updated, 0}, pgsql_connection:sql_query("update foo set some_text = 'goodbye, all' where id = 3", Conn))
        },
        {"Select *",
            ?_assertEqual({selected, [{1, <<"hello world">>}, {2, <<"hello again">>}]}, pgsql_connection:sql_query("select * from foo order by id asc", Conn))
        },
        {"Select with named columns",
            ?_assertEqual({selected, [{1, <<"hello world">>}, {2, <<"hello again">>}]}, pgsql_connection:sql_query("select id as the_id, some_text as the_text from foo order by id asc", Conn))
        },
        {"Select with inverted columns",
            ?_assertEqual({selected, [{<<"hello world">>, 1}, {<<"hello again">>, 2}]}, pgsql_connection:sql_query("select some_text, id from foo order by id asc", Conn))
        },
        {"Select with matching condition",
            ?_assertEqual({selected, [{<<"hello again">>}]}, pgsql_connection:sql_query("select some_text from foo where id = 2", Conn))
        },
        {"Select with non-matching condition",
            ?_assertEqual({selected, []}, pgsql_connection:sql_query("select * from foo where id = 3", Conn))
        }
    ]
    end}.

copy_test_() ->
    {setup,
     fun() ->
	     {ok, SupPid} = pgsql_connection_sup:start_link(),
	     Conn = pgsql_connection:open("test", "test"),
	     {SupPid, Conn}
     end,
     fun({SupPid, Conn}) ->
	     pgsql_connection:close(Conn),
	     kill_sup(SupPid)
     end,
     fun({_SupPid, Conn}) ->
     [
      {"Create temporary table for copies",
       ?_assertEqual({{create,table},[]}, pgsql_connection:simple_query("create temporary table copy_data (foo integer, bar text)", Conn))
      },
      {"Insert data for copy out",
       ?_assertEqual({{insert,0,5},[]}, pgsql_connection:simple_query("insert into copy_data values (0,'hello'),(1,'world'),(2,'shoe'),(10,'hen'),(42,'so long')", Conn))
      },
      {"Copy out",
       ?_assertMatch({{copy, 5},_}, pgsql_connection:simple_query("copy copy_data to stdout", Conn))
      },
      {"Begin copy in",
       ?_assertEqual({copy_in,[text]}, pgsql_connection:simple_query("copy copy_data from stdin", Conn))
      },
      {"Send copy data",
       ?_assertEqual(ok, pgsql_connection:send_copy_data(<<"100\tcentury\n">>, Conn))
      },
      {"End copy",
       ?_assertEqual({copy,1}, pgsql_connection:send_copy_end(Conn))
      },
      {"Check copy data",
       ?_assertEqual({{select, 1},[{100,<<"century">>}]}, pgsql_connection:simple_query("select * from copy_data where foo = 100", Conn))
      },
      {"Copy out with extended query",
       ?_assertMatch({{copy,6},_}, pgsql_connection:simple_query("copy copy_data to stdout", [], Conn))
      },
      {"Begin copy in (2)",
       ?_assertEqual({copy_in,[text]}, pgsql_connection:simple_query("copy copy_data from stdin", [], Conn))
      },
      {"Send copy data (2)",
       ?_assertEqual(ok, pgsql_connection:send_copy_data(<<"101\tx\n102\ty\n">>, Conn))
      },
      {"Send copy data (2b)",
       ?_assertEqual(ok, pgsql_connection:send_copy_data(<<"103\tz\n">>, Conn))
      },
      {"End copy (2)",
       ?_assertEqual({copy,3}, pgsql_connection:send_copy_end(Conn))
      },
      {"Check copy data (2)",
       ?_assertMatch({{select,3},_}, pgsql_connection:extended_query("select * from copy_data where foo > 100", [], Conn))
      },
      {"Can't copy in using extended_query",
       ?_assertMatch({error, {pgsql_error, _}}, pgsql_connection:extended_query("copy copy_data from stdin", [], Conn))
      }
     ]
     end}.

types_test_() ->
    {setup,
    fun() ->
        {ok, SupPid} = pgsql_connection_sup:start_link(),
        Conn = pgsql_connection:open("test", "test"),
        {SupPid, Conn}
    end,
    fun({SupPid, Conn}) ->
        pgsql_connection:close(Conn),
        kill_sup(SupPid)
    end,
    fun({_SupPid, Conn}) ->
    [
        {"Create temporary table for the types",
            ?_assertEqual({updated, 1}, pgsql_connection:sql_query("create temporary table types (id integer primary key, an_integer integer, a_bigint bigint, a_text text, a_uuid uuid, a_bytea bytea, a_real real)", Conn))
        },
        {"Insert nulls (literal)",
            ?_assertEqual({updated, 1}, pgsql_connection:sql_query("insert into types (id, an_integer, a_bigint, a_text, a_uuid, a_bytea, a_real) values (1, null, null, null, null, null, null)", Conn))
        },
        {"Select nulls (1)",
            ?_assertMatch({selected, [{1, null, null, null, null, null, null}]}, pgsql_connection:sql_query("select * from types where id = 1", Conn))
        },
        {"Insert nulls (params)",
            ?_assertEqual({updated, 1}, pgsql_connection:param_query("insert into types (id, an_integer, a_bigint, a_text, a_uuid, a_bytea, a_real) values (?, ?, ?, ?, ?, ?, ?)", [2, null, null, null, null, null, null], Conn))
        },
        {"Select nulls (2)",
            ?_assertMatch({selected, [{2, null, null, null, null, null, null}]}, pgsql_connection:sql_query("select * from types where id = 2", Conn))
        },
        {"Insert integer",
            ?_assertEqual({updated, 1}, pgsql_connection:param_query("insert into types (id, an_integer, a_bigint, a_text, a_uuid, a_bytea, a_real) values (?, ?, ?, ?, ?, ?, ?)",
                [3, 42, null, null, null, null, null], Conn))
        },
        {"Insert bigint",
            ?_assertEqual({updated, 1}, pgsql_connection:param_query("insert into types (id, an_integer, a_bigint, a_text, a_uuid, a_bytea, a_real) values (?, ?, ?, ?, ?, ?, ?)",
                [4, null, 1099511627776, null, null, null, null], Conn))
        },
        {"Insert text (list)",
            ?_assertEqual({updated, 1}, pgsql_connection:param_query("insert into types (id, an_integer, a_bigint, a_text, a_uuid, a_bytea, a_real) values (?, ?, ?, ?, ?, ?, ?)",
                [5, null, null, "And in the end, the love you take is equal to the love you make", null, null, null], Conn))
        },
        {"Insert text (binary)",
            ?_assertEqual({updated, 1}, pgsql_connection:param_query("insert into types (id, an_integer, a_bigint, a_text, a_uuid, a_bytea, a_real) values (?, ?, ?, ?, ?, ?, ?)",
                [6, null, null, <<"And in the end, the love you take is equal to the love you make">>, null, null, null], Conn))
        },
        {"Insert uuid",
            ?_assertEqual({updated, 1}, pgsql_connection:param_query("insert into types (id, an_integer, a_bigint, a_text, a_uuid, a_bytea, a_real) values (?, ?, ?, ?, ?, ?, ?)",
                [7, null, null, null, <<"727F42A6-E6A0-4223-9B72-6A5EB7436AB5">>, null, null], Conn))
        },
        {"Insert bytea",
            ?_assertEqual({updated, 1}, pgsql_connection:param_query("insert into types (id, an_integer, a_bigint, a_text, a_uuid, a_bytea, a_real) values (?, ?, ?, ?, ?, ?, ?)",
                [8, null, null, null, null, <<"deadbeef">>, null], Conn))
        },
        {"Insert float",
            ?_assertEqual({updated, 1}, pgsql_connection:param_query("insert into types (id, an_integer, a_bigint, a_text, a_uuid, a_bytea, a_real) values (?, ?, ?, ?, ?, ?, ?)",
                [9, null, null, null, null, null, 3.1415], Conn))
        },
        {"Insert float",
            ?_assertEqual({updated, 1}, pgsql_connection:param_query("insert into types (id, an_integer, a_bigint, a_text, a_uuid, a_bytea, a_real) values (?, ?, ?, ?, ?, ?, ?)",
                [19, null, null, null, null, null, 3.0], Conn))
        },
        {"Insert all",
            ?_assertEqual({updated, 1}, pgsql_connection:param_query("insert into types (id, an_integer, a_bigint, a_text, a_uuid, a_bytea, a_real) values (?, ?, ?, ?, ?, ?, ?)",
                [10, 42, 1099511627776, "And in the end, the love you take is equal to the love you make", <<"727F42A6-E6A0-4223-9B72-6A5EB7436AB5">>, <<"deadbeef">>, 3.1415], Conn))
        },
        {"Select values (10)",
            ?_test(begin
                R = pgsql_connection:sql_query("select * from types where id = 10", Conn),
                ?assertMatch({selected, [_Row]}, R),
                {selected, [Row]} = R,
                ?assertMatch({10, 42, 1099511627776, <<"And in the end, the love you take is equal to the love you make">>, _UUID, <<"deadbeef">>, _Float}, Row),
                {10, 42, 1099511627776, <<"And in the end, the love you take is equal to the love you make">>, UUID, <<"deadbeef">>, Float} = Row,
                ?assertEqual(<<"727f42a6-e6a0-4223-9b72-6a5eb7436ab5">>, UUID),
                ?assert(Float > 3.1413),
                ?assert(Float < 3.1416)
            end)
        },
        {"Select values (10) (with bind)",
            ?_test(begin
                R = pgsql_connection:param_query("select * from types where id = ?", [10], Conn),
                ?assertMatch({selected, [_Row]}, R),
                {selected, [Row]} = R,
                ?assertMatch({10, 42, 1099511627776, <<"And in the end, the love you take is equal to the love you make">>, _UUID, <<"deadbeef">>, _Float}, Row),
                {10, 42, 1099511627776, <<"And in the end, the love you take is equal to the love you make">>, UUID, <<"deadbeef">>, Float} = Row,
                ?assertEqual(<<"727f42a6-e6a0-4223-9b72-6a5eb7436ab5">>, UUID),
                ?assert(Float > 3.1413),
                ?assert(Float < 3.1416)
            end)
        },
        {"Insert bytea",
            ?_assertEqual({updated, 1}, pgsql_connection:param_query("insert into types (id, an_integer, a_bigint, a_text, a_uuid, a_bytea, a_real) values (?, ?, ?, ?, ?, ?, ?)",
                [11, null, null, null, null, <<"deadbeef">>, null], Conn))
        },
        {"Insert with returning",
            ?_assertEqual({updated, 1, [{15}]}, pgsql_connection:param_query("insert into types (id, an_integer, a_bigint, a_text, a_uuid, a_bytea, a_real) values (?, ?, ?, ?, ?, ?, ?) RETURNING id",
                [15, null, null, null, null, <<"deadbeef">>, null], Conn))
        },
        {"Select values (11)",
            ?_test(begin
                R = pgsql_connection:param_query("select * from types where id = ?", [11], Conn),
                ?assertMatch({selected, [_Row]}, R),
                {selected, [Row]} = R,
                ?assertEqual({11, null, null, null, null, <<"deadbeef">>, null}, Row)
            end)
        },
        {"Insert uuid in lowercase",
            ?_assertEqual({updated, 1}, pgsql_connection:param_query("insert into types (id, an_integer, a_bigint, a_text, a_uuid, a_bytea, a_real) values (?, ?, ?, ?, ?, ?, ?)",
                [16, null, null, null, <<"727f42a6-e6a0-4223-9b72-6a5eb7436ab5">>, null, null], Conn))
        },
        {"Insert uc uuid in text column",
            ?_assertEqual({updated, 1}, pgsql_connection:param_query("insert into types (id, an_integer, a_bigint, a_text, a_uuid, a_bytea, a_real) values (?, ?, ?, ?, ?, ?, ?)",
                [17, null, null, <<"727F42A6-E6A0-4223-9B72-6A5EB7436AB5">>, null, null, null], Conn))
        },
        {"Insert lc uuid in text column",
            ?_assertEqual({updated, 1}, pgsql_connection:param_query("insert into types (id, an_integer, a_bigint, a_text, a_uuid, a_bytea, a_real) values (?, ?, ?, ?, ?, ?, ?)",
                [18, null, null, <<"727f42a6-e6a0-4223-9b72-6a5eb7436ab5">>, null, null, null], Conn))
        },
        {"Select text uuid (17 \& 18)",
            ?_test(begin
                R = pgsql_connection:param_query("select a_text from types where id IN ($1, $2) order by id", [17, 18], Conn),
                ?assertMatch({selected, [_Row17, _Row18]}, R),
                {selected, [Row17, Row18]} = R,
                ?assertEqual({<<"727F42A6-E6A0-4223-9B72-6A5EB7436AB5">>}, Row17),
                ?assertEqual({<<"727f42a6-e6a0-4223-9b72-6a5eb7436ab5">>}, Row18)
            end)
        }
        ]
    end}.

text_types_test_() ->
    {setup,
        fun() ->
                {ok, SupPid} = pgsql_connection_sup:start_link(),
                Conn = pgsql_connection:open("test", "test"),
                {SupPid, Conn}
        end,
        fun({SupPid, Conn}) ->
                pgsql_connection:close(Conn),
                kill_sup(SupPid)
        end,
        fun({_SupPid, Conn}) ->
                [
                    ?_assertEqual({{select,1},[{<<"foo">>}]}, pgsql_connection:simple_query("select 'foo'::text", Conn)),
                    ?_assertEqual({{select,1},[{<<"foo">>}]}, pgsql_connection:extended_query("select $1::text", [<<"foo">>], Conn)),
                    ?_assertEqual({{select,1},[{<<"foo         ">>}]}, pgsql_connection:simple_query("select 'foo'::char(12)", Conn)),
                    ?_assertEqual({{select,1},[{<<"foo         ">>}]}, pgsql_connection:extended_query("select $1::char(12)", [<<"foo">>], Conn)),
                    ?_assertEqual({{select,1},[{<<"foo">>}]}, pgsql_connection:simple_query("select 'foo'::varchar(12)", Conn)),
                    ?_assertEqual({{select,1},[{<<"foo">>}]}, pgsql_connection:extended_query("select $1::varchar(12)", [<<"foo">>], Conn)),
                    ?_assertEqual({{select,1},[{<<"foo">>}]}, pgsql_connection:simple_query("select 'foobar'::char(3)", Conn)),
                    ?_assertEqual({{select,1},[{<<"foo">>}]}, pgsql_connection:extended_query("select $1::char(3)", [<<"foobar">>], Conn))
                ]
        end
    }.


array_types_test_() ->
    {setup,
        fun() ->
                {ok, SupPid} = pgsql_connection_sup:start_link(),
                Conn = pgsql_connection:open("test", "test"),
                {SupPid, Conn}
        end,
        fun({SupPid, Conn}) ->
                pgsql_connection:close(Conn),
                kill_sup(SupPid)
        end,
        fun({_SupPid, Conn}) ->
                [
                    ?_assertEqual({{select,1},[{{array,[<<"2">>,<<"3">>]}}]}, pgsql_connection:simple_query("select '{2,3}'::text[]", Conn)),
                    ?_assertEqual({{select,1},[{{array,[2,3]}}]}, pgsql_connection:simple_query("select '{2,3}'::int[]", Conn)),
                    ?_assertEqual({{select,1},[{{array,[]}}]}, pgsql_connection:simple_query("select '{}'::text[]", Conn)),
                    ?_assertEqual({{select,1},[{{array,[]}}]}, pgsql_connection:simple_query("select '{}'::int[]", Conn)),
                    ?_assertEqual({{select,1},[{{array,[]}}]}, pgsql_connection:simple_query("select ARRAY[]::text[]", Conn)),
                    ?_assertEqual({{select,1},[{{array,[<<"2">>,<<"3">>]}}]}, pgsql_connection:extended_query("select $1::text[]", ["{\"2\", \"3\"}"], Conn)),
                    ?_assertEqual({{select,1},[{{array,[<<"2">>,<<"3">>]}}]}, pgsql_connection:extended_query("select $1::text[]", [{array, ["2", "3"]}], Conn)),
                    ?_assertEqual({{select,1},[{{array,[<<"2">>,<<"3">>]}}]}, pgsql_connection:extended_query("select $1::text[]", [{array, [<<"2">>, <<"3">>]}], Conn)),
                    ?_assertEqual({{select,1},[{{array,[<<"2,3">>,<<"4">>]}}]}, pgsql_connection:extended_query("select $1::text[]", [{array, [<<"2,3">>, <<"4">>]}], Conn)),
                    ?_assertEqual({{select,1},[{{array,[<<"2,,3">>,<<"4">>]}}]}, pgsql_connection:extended_query("select $1::text[]", [{array, [<<"2,,3">>, <<"4">>]}], Conn)),
                    ?_assertEqual({{select,1},[{{array,[<<"2\"3">>,<<"4">>]}}]}, pgsql_connection:extended_query("select $1::text[]", [{array, [<<"2\"3">>, <<"4">>]}], Conn)),
                    ?_assertEqual({{select,1},[{{array,[<<"2\",,\"3">>,<<"4">>]}}]}, pgsql_connection:extended_query("select $1::text[]", [{array, [<<"2\",,\"3">>, <<"4">>]}], Conn)),
                    ?_assertEqual({{select,1},[{{array,[<<"2'3">>,<<"4">>]}}]}, pgsql_connection:extended_query("select $1::text[]", [{array, [<<"2'3">>, <<"4">>]}], Conn)),
                    ?_assertEqual({{select,1},[{{array,[<<"2\\3">>,<<"4">>]}}]}, pgsql_connection:extended_query("select $1::text[]", [{array, [<<"2\\3">>, <<"4">>]}], Conn)),
                    ?_assertEqual({{select,1},[{{array,[<<"2">>,<<"3">>]}}]}, pgsql_connection:extended_query("select $1::bytea[]", [{array, [<<"2">>, <<"3">>]}], Conn)),
                    ?_assertEqual({{select,1},[{{array,[<<"2  ">>,<<"3  ">>]}}]}, pgsql_connection:extended_query("select $1::char(3)[]", [{array, [<<"2">>, <<"3">>]}], Conn)),
                    ?_assertEqual({{select,1},[{{array,[<<"2">>,<<"3">>]}}]}, pgsql_connection:extended_query("select $1::varchar(3)[]", [{array, [<<"2">>, <<"3">>]}], Conn)),
                    ?_assertEqual({{select,1},[{{array,[{array,[<<"2">>]},{array, [<<"3">>]}]}}]}, pgsql_connection:extended_query("select $1::text[]", [{array, [{array, [<<"2">>]}, {array, [<<"3">>]}]}], Conn)),
                    ?_assertEqual({{select,1},[{{array,[{array,[1,2]},{array, [3,4]}]}}]}, pgsql_connection:extended_query("select $1::int[]", [{array, [{array, [1,2]}, {array, [3,4]}]}], Conn)),
                    ?_assertEqual({{select,1},[{{array,[]}}]}, pgsql_connection:extended_query("select '{}'::text[]", [], Conn)),
                    ?_assertEqual({{select,1},[{{array,[]}}]}, pgsql_connection:extended_query("select '{}'::int[]", [], Conn)),
                    ?_assertEqual({{select,1},[{{array,[]}}]}, pgsql_connection:extended_query("select ARRAY[]::text[]", [], Conn)),
                    
                    ?_assertEqual({{select,1},[{{array,[{array,[<<"2">>]},{array, [<<"3">>]}]}}]}, pgsql_connection:simple_query("select '{{\"2\"}, {\"3\"}}'::text[][]", Conn)),
                    ?_assertEqual({{select,1},[{{array,[{array,[1,2]}, {array, [3,4]}]}}]}, pgsql_connection:simple_query("select ARRAY[ARRAY[1,2], ARRAY[3,4]]", Conn)),
                    ?_assertEqual({{select,1},[{{array,[]}}]}, pgsql_connection:extended_query("select $1::bytea[]", [{array, []}], Conn)),
                    ?_assertEqual({{select,1},[{{array,[]},{array,[<<"foo">>]}}]}, pgsql_connection:extended_query("select $1::bytea[], $2::bytea[]", [{array, []}, {array, [<<"foo">>]}], Conn)),

                    ?_assertEqual({{select,1},[{{array,[1,2]}}]}, pgsql_connection:simple_query("select ARRAY[1,2]::int[]", Conn)),
                    ?_assertEqual({{select,1},[{{array,[1,null,3]}}]}, pgsql_connection:simple_query("select ARRAY[1,NULL,3]::int[]", Conn)),       % text format
                    ?_assertEqual({{select,1},[{{array,[1,null,3]}}]}, pgsql_connection:extended_query("select ARRAY[1,NULL,3]::int[]", [], Conn)), % binary format
                    {timeout, 20, ?_test(
                        begin
                                {{create, table}, []} = pgsql_connection:simple_query("create temporary table tmp (id integer primary key, ints integer[])", Conn),
                                Array = lists:seq(1,1000000),
                                R = pgsql_connection:extended_query("insert into tmp(id, ints) values($1, $2)", [1, {array, Array}], Conn),
                                ?assertEqual({{insert, 0, 1}, []}, R)
                        end)},
                    ?_test(
                        begin
                                {{create, table}, []} = pgsql_connection:simple_query("create temporary table tmp2 (id integer primary key, bins bytea[])", Conn),
                                R = pgsql_connection:extended_query("insert into tmp2(id, bins) values($1, $2)", [1, {array, [<<2>>, <<3>>]}], Conn),
                                ?assertEqual({{insert, 0, 1}, []}, R),
                                R2 = pgsql_connection:extended_query("insert into tmp2(id, bins) values($1, $2)", [2, {array, [<<16#C2,1>>]}], Conn),
                                ?assertEqual({{insert, 0, 1}, []}, R2),
                                R3 = pgsql_connection:extended_query("insert into tmp2(id, bins) values($1, $2)", [3, {array, [<<2,0,3>>, <<4>>]}], Conn),
                                ?assertEqual({{insert, 0, 1}, []}, R3)
                        end)
                ]
        end
    }.

% https://github.com/semiocast/pgsql/issues/28
quoted_array_values_test_() ->
    {setup,
        fun() ->
                {ok, SupPid} = pgsql_connection_sup:start_link(),
                Conn = pgsql_connection:open("test", "test"),
                {SupPid, Conn}
        end,
        fun({SupPid, Conn}) ->
                pgsql_connection:close(Conn),
                kill_sup(SupPid)
        end,
        fun({_SupPid, Conn}) ->
                [
                    ?_assertEqual({{select,1},[{{array,[<<"foo bar">>]}}]}, pgsql_connection:simple_query("select ARRAY['foo bar'];", Conn)),
                    ?_assertEqual({{select,1},[{{array,[<<"foo, bar">>]}}]}, pgsql_connection:simple_query("select ARRAY['foo, bar'];", Conn)),
                    ?_assertEqual({{select,1},[{{array,[<<"foo} bar">>]}}]}, pgsql_connection:simple_query("select ARRAY['foo} bar'];", Conn)),
                    ?_assertEqual({{select,1},[{{array,[<<"foo \" bar">>]}}]}, pgsql_connection:simple_query("select ARRAY['foo \" bar'];", Conn)),
                    ?_assertEqual({{select,1},[{{array,[{{2014,1,1},{12,12,12}}]}}]}, pgsql_connection:simple_query("select ARRAY['2014-01-01T12:12:12Z'::timestamp];", Conn))
                ]
        end
    }.

geometric_types_test_() ->
    [{setup,
      fun() ->
              {ok, SupPid} = pgsql_connection_sup:start_link(),
              Conn = pgsql_connection:open("test", "test"),
              {SupPid, Conn}
      end,
      fun({SupPid, Conn}) ->
              pgsql_connection:close(Conn),
              kill_sup(SupPid)
      end,
      fun({_SupPid, Conn}) ->
              [
               ?_assertEqual({{select,1},[{{point,{2.0,-3.0}}}]}, pgsql_connection:simple_query("select '(2,-3)'::point", Conn)),
               ?_assertEqual({{select,1},[{{point,{2.0,1.45648}}}]}, pgsql_connection:simple_query("select '(2,1.45648)'::point", Conn)),
               ?_assertEqual({{select,1},[{{point,{-3.154548,-3.0}}}]}, pgsql_connection:simple_query("select '(-3.154548,-3)'::point", Conn)),
               ?_assertEqual({{select,1},[{{point,{-3.154548,1.45648}}}]}, pgsql_connection:simple_query("select '(-3.154548,1.45648)'::point", Conn)),
               ?_assertEqual({{select,1},[{{point,{2.0,-3.0}}}]}, pgsql_connection:extended_query("select '(2,-3)'::point", [], Conn)),
               ?_assertEqual({{select,1},[{{point,{2.0,1.45648}}}]}, pgsql_connection:extended_query("select '(2,1.45648)'::point", [], Conn)),
               ?_assertEqual({{select,1},[{{point,{-3.154548,-3.0}}}]}, pgsql_connection:extended_query("select '(-3.154548,-3)'::point", [], Conn)),
               ?_assertEqual({{select,1},[{{point,{-3.154548,1.45648}}}]}, pgsql_connection:extended_query("select '(-3.154548,1.45648)'::point", [], Conn)),

               ?_assertEqual({{select,1},[{{lseg,{2.0,1.45648},{-3.154548,-3.0}}}]}, pgsql_connection:simple_query("select '[(2,1.45648),(-3.154548,-3)]'::lseg", Conn)),
               ?_assertEqual({{select,1},[{{lseg,{2.0,1.45648},{-3.154548,-3.0}}}]}, pgsql_connection:extended_query("select '[(2,1.45648),(-3.154548,-3))'::lseg", [], Conn)),

               ?_assertEqual({{select,1},[{{box,{2.0,1.45648},{-3.154548,-3.0}}}]}, pgsql_connection:simple_query("select '((-3.154548,-3),(2,1.45648))'::box", Conn)),
               ?_assertEqual({{select,1},[{{box,{2.0,1.45648},{-3.154548,-3.0}}}]}, pgsql_connection:extended_query("select '((-3.154548,-3),(2,1.45648))'::box", [], Conn)),

               ?_assertEqual({{select,1},[{{polygon,[{-3.154548,-3.0},{2.0,1.45648}]}}]}, pgsql_connection:simple_query("select '((-3.154548,-3),(2,1.45648))'::polygon", Conn)),
               ?_assertEqual({{select,1},[{{polygon,[{-3.154548,-3.0},{2.0,1.45648}]}}]}, pgsql_connection:extended_query("select '((-3.154548,-3),(2,1.45648))'::polygon", [], Conn)),

               ?_assertEqual({{select,1},[{{path,closed,[{-3.154548,-3.0},{2.0,1.45648}]}}]}, pgsql_connection:simple_query("select '((-3.154548,-3),(2,1.45648))'::path", Conn)),
               ?_assertEqual({{select,1},[{{path,closed,[{-3.154548,-3.0},{2.0,1.45648}]}}]}, pgsql_connection:extended_query("select '((-3.154548,-3),(2,1.45648))'::path", [], Conn)),

               ?_assertEqual({{select,1},[{{path,open,[{-3.154548,-3.0},{2.0,1.45648}]}}]}, pgsql_connection:simple_query("select '[(-3.154548,-3),(2,1.45648)]'::path", Conn)),
               ?_assertEqual({{select,1},[{{path,open,[{-3.154548,-3.0},{2.0,1.45648}]}}]}, pgsql_connection:extended_query("select '[(-3.154548,-3),(2,1.45648)]'::path", [], Conn)),

               {setup,
                fun() ->
                        {updated, 1} = pgsql_connection:sql_query("create temporary table tmp (id integer primary key, mypoint point, mylseg lseg, mybox box, mypath path, mypolygon polygon)", Conn),
                        ok
                end,
                fun(_) ->
                        ok
                end,
                fun(_) ->
                        [
                         ?_assertEqual(
                            {{insert, 0, 1}, [{1, {point,{2.0,3.0}}}]},
                            pgsql_connection:extended_query("insert into tmp(id, mypoint) values($1, $2) returning id, mypoint", [1, {point,{2,3}}], Conn)
                           ),
                         ?_assertEqual(
                            {{insert, 0, 1}, [{2, {point,{-10.0,3.254}}}]},
                            pgsql_connection:extended_query("insert into tmp(id, mypoint) values($1, $2) returning id, mypoint", [2, {point,{-10,3.254}}], Conn)
                           ),
                         ?_assertEqual(
                            {{insert, 0, 1}, [{3, {point,{-10.0,-3.5015}}}]},
                            pgsql_connection:extended_query("insert into tmp(id, mypoint) values($1, $2) returning id, mypoint", [3, {point,{-10,-3.5015}}], Conn)
                           ),
                         ?_assertEqual(
                            {{insert, 0, 1}, [{4, {point,{2.25,-3.59}}}]},
                            pgsql_connection:extended_query("insert into tmp(id, mypoint) values($1, $2) returning id, mypoint", [4, {point,{2.25,-3.59}}], Conn)
                           ),

                         ?_assertEqual(
                            {{insert, 0, 1}, [{101, {lseg,{2.54,3.14},{-10.0,-3.5015}}}]},
                            pgsql_connection:extended_query("insert into tmp(id, mylseg) values($1, $2) returning id, mylseg", [101, {lseg,{2.54,3.14},{-10,-3.5015}}], Conn)
                           ),

                         ?_assertEqual(
                            {{insert, 0, 1}, [{201, {box,{2.0,3.0},{-10.14,-3.5015}}}]},
                            pgsql_connection:extended_query("insert into tmp(id, mybox) values($1, $2) returning id, mybox", [201, {box,{2,3},{-10.14,-3.5015}}], Conn)
                           ),
                         ?_assertEqual(
                            {{insert, 0, 1}, [{202, {box,{2.0,3.0},{-10.14,-3.5015}}}]},
                            pgsql_connection:extended_query("insert into tmp(id, mybox) values($1, $2) returning id, mybox", [202, {box,{-10.14,3},{2,-3.5015}}], Conn)
                           ),
                         ?_assertEqual(
                            {{insert, 0, 1}, [{203, {box,{2.0,3.0},{-10.14,-3.5015}}}]},
                            pgsql_connection:extended_query("insert into tmp(id, mybox) values($1, $2) returning id, mybox", [203, {box,{2,-3.5015},{-10.14,3}}], Conn)
                           ),
                         ?_assertEqual(
                            {{insert, 0, 1}, [{204, {box,{2.0,3.0},{-10.14,-3.5015}}}]},
                            pgsql_connection:extended_query("insert into tmp(id, mybox) values($1, $2) returning id, mybox", [204, {box,{-10.14,-3.5015},{2,3}}], Conn)
                           ),

                         ?_assertEqual(
                            {{insert, 0, 1}, [{301, {path,open,[{-10.85,-3.5015}]}}]},
                            pgsql_connection:extended_query("insert into tmp(id, mypath) values($1, $2) returning id, mypath", [301, {path,open,[{-10.85,-3.5015}]}], Conn)
                           ),
                         ?_assertEqual(
                            {{insert, 0, 1}, [{302, {path,open,[{-10.85,-3.5015},{2.0,3.0}]}}]},
                            pgsql_connection:extended_query("insert into tmp(id, mypath) values($1, $2) returning id, mypath", [302, {path,open,[{-10.85,-3.5015},{2,3}]}], Conn)
                           ),

                         ?_assertEqual(
                            {{insert, 0, 1}, [{351, {path,closed,[{-10.85,-3.5015}]}}]},
                            pgsql_connection:extended_query("insert into tmp(id, mypath) values($1, $2) returning id, mypath", [351, {path,closed,[{-10.85,-3.5015}]}], Conn)
                           ),
                         ?_assertEqual(
                            {{insert, 0, 1}, [{352, {path,closed,[{-10.85,-3.5015},{2.0,3.0}]}}]},
                            pgsql_connection:extended_query("insert into tmp(id, mypath) values($1, $2) returning id, mypath", [352, {path,closed,[{-10.85,-3.5015},{2,3}]}], Conn)
                           ),

                         ?_assertEqual(
                            {{insert, 0, 1}, [{401, {polygon,[{-10.85,-3.5015}]}}]},
                            pgsql_connection:extended_query("insert into tmp(id, mypolygon) values($1, $2) returning id, mypolygon", [401, {polygon,[{-10.85,-3.5015}]}], Conn)
                           ),
                         ?_assertEqual(
                            {{insert, 0, 1}, [{402, {polygon,[{-10.85,-3.5015},{2.0,3.0}]}}]},
                            pgsql_connection:extended_query("insert into tmp(id, mypolygon) values($1, $2) returning id, mypolygon", [402, {polygon,[{-10.85,-3.5015},{2,3}]}], Conn)
                           )
                        ]
                end
               }
              ]
      end
     },
     {setup,
      fun() ->
              {ok, SupPid} = pgsql_connection_sup:start_link(),
              Conn = pgsql_connection:open("test", "test"),
              {updated, 1} = pgsql_connection:sql_query("create temporary table tmp (id integer primary key, mypath path)", Conn),
              {SupPid, Conn}
      end,
      fun({SupPid, Conn}) ->
              pgsql_connection:close(Conn),
              kill_sup(SupPid)
      end,
      fun({_SupPid, Conn}) ->
              ?_assertMatch(
                  {error, {badarg, {path,open,[]}}},
                  pgsql_connection:extended_query("insert into tmp(id, mypath) values($1, $2) returning id, mypath", [300, {path,open,[]}], Conn)
              )
      end},
     {setup,
      fun() ->
              {ok, SupPid} = pgsql_connection_sup:start_link(),
              Conn = pgsql_connection:open("test", "test"),
              {updated, 1} = pgsql_connection:sql_query("create temporary table tmp (id integer primary key, mypath path)", Conn),
              {SupPid, Conn}
      end,
      fun({SupPid, Conn}) ->
              pgsql_connection:close(Conn),
              kill_sup(SupPid)
      end,
      fun({_SupPid, Conn}) ->
              ?_assertMatch(
                  {error, {badarg, {path,closed,[]}}},
                  pgsql_connection:extended_query("insert into tmp(id, mypath) values($1, $2) returning id, mypath", [350, {path,closed,[]}], Conn)
              )
      end},
     {setup,
      fun() ->
              {ok, SupPid} = pgsql_connection_sup:start_link(),
              Conn = pgsql_connection:open("test", "test"),
              {updated, 1} = pgsql_connection:sql_query("create temporary table tmp (id integer primary key, mypolygon polygon)", Conn),
              {SupPid, Conn}
      end,
      fun({SupPid, Conn}) ->
              pgsql_connection:close(Conn),
              kill_sup(SupPid)
      end,
      fun({_SupPid, Conn}) ->
              ?_assertMatch(
                  {error, {badarg, {polygon, []}}},
                  pgsql_connection:extended_query("insert into tmp(id, mypolygon) values($1, $2) returning id, mypolygon", [400, {polygon,[]}], Conn)
              )
      end}
    ].

float_types_test_() ->
    {setup,
    fun() ->
        {ok, SupPid} = pgsql_connection_sup:start_link(),
        Conn = pgsql_connection:open("test", "test"),
        {SupPid, Conn}
    end,
    fun({SupPid, Conn}) ->
        pgsql_connection:close(Conn),
        kill_sup(SupPid)
    end,
    fun({_SupPid, Conn}) ->
    [
        ?_assertEqual({selected, [{1.0}]}, pgsql_connection:sql_query("select 1.0::float4", Conn)),
        ?_assertEqual({selected, [{1.0}]}, pgsql_connection:sql_query("select 1.0::float8", Conn)),
        ?_assertEqual({selected, [{1.0}]}, pgsql_connection:param_query("select 1.0::float4", [], Conn)),
        ?_assertEqual({selected, [{1.0}]}, pgsql_connection:param_query("select 1.0::float8", [], Conn)),

        ?_assertEqual({selected, [{3.14159}]}, pgsql_connection:sql_query("select 3.141592653589793::float4", Conn)),
        ?_assertEqual({selected, [{3.14159265358979}]}, pgsql_connection:sql_query("select 3.141592653589793::float8", Conn)),
        ?_assertEqual({selected, [{3.1415927410125732}]}, pgsql_connection:param_query("select 3.141592653589793::float4", [], Conn)),
        ?_assertEqual({selected, [{3.141592653589793}]}, pgsql_connection:param_query("select 3.141592653589793::float8", [], Conn)),

        ?_assertEqual({selected, [{'NaN'}]}, pgsql_connection:sql_query("select 'NaN'::float4", Conn)),
        ?_assertEqual({selected, [{'NaN'}]}, pgsql_connection:sql_query("select 'NaN'::float8", Conn)),
        ?_assertEqual({selected, [{'NaN'}]}, pgsql_connection:param_query("select 'NaN'::float4", [], Conn)),
        ?_assertEqual({selected, [{'NaN'}]}, pgsql_connection:param_query("select 'NaN'::float8", [], Conn)),

        ?_assertEqual({selected, [{'Infinity'}]}, pgsql_connection:sql_query("select 'Infinity'::float4", Conn)),
        ?_assertEqual({selected, [{'Infinity'}]}, pgsql_connection:sql_query("select 'Infinity'::float8", Conn)),
        ?_assertEqual({selected, [{'Infinity'}]}, pgsql_connection:param_query("select 'Infinity'::float4", [], Conn)),
        ?_assertEqual({selected, [{'Infinity'}]}, pgsql_connection:param_query("select 'Infinity'::float8", [], Conn)),

        ?_assertEqual({selected, [{'-Infinity'}]}, pgsql_connection:sql_query("select '-Infinity'::float4", Conn)),
        ?_assertEqual({selected, [{'-Infinity'}]}, pgsql_connection:sql_query("select '-Infinity'::float8", Conn)),
        ?_assertEqual({selected, [{'-Infinity'}]}, pgsql_connection:param_query("select '-Infinity'::float4", [], Conn)),
        ?_assertEqual({selected, [{'-Infinity'}]}, pgsql_connection:param_query("select '-Infinity'::float8", [], Conn))
    ]
    end}.

boolean_type_test_() ->
    {setup,
    fun() ->
        {ok, SupPid} = pgsql_connection_sup:start_link(),
        Conn = pgsql_connection:open("test", "test"),
        {SupPid, Conn}
    end,
    fun({SupPid, Conn}) ->
        pgsql_connection:close(Conn),
        kill_sup(SupPid)
    end,
    fun({_SupPid, Conn}) ->
    [
        ?_assertEqual({selected, [{true}]}, pgsql_connection:sql_query("select true::boolean", Conn)),
        ?_assertEqual({selected, [{false}]}, pgsql_connection:sql_query("select false::boolean", Conn)),
        ?_assertEqual({selected, [{true}]}, pgsql_connection:param_query("select true::boolean", [], Conn)),
        ?_assertEqual({selected, [{false}]}, pgsql_connection:param_query("select false::boolean", [], Conn))
    ]
    end}.

null_test_() ->
    {setup,
    fun() ->
        {ok, SupPid} = pgsql_connection_sup:start_link(),
        Conn = pgsql_connection:open("test", "test"),
        {SupPid, Conn}
    end,
    fun({SupPid, Conn}) ->
        pgsql_connection:close(Conn),
        kill_sup(SupPid)
    end,
    fun({_SupPid, Conn}) ->
    [
        ?_assertEqual({selected, [{null}]}, pgsql_connection:sql_query("select null", Conn)),
        ?_assertEqual({selected, [{null}]}, pgsql_connection:param_query("select null", [], Conn)),
        ?_assertEqual({selected, [{null}]}, pgsql_connection:sql_query("select null::int2", Conn)),
        ?_assertEqual({selected, [{null}]}, pgsql_connection:param_query("select null::int2", [], Conn))
    ]
    end}.

integer_types_test_() ->
    {setup,
    fun() ->
        {ok, SupPid} = pgsql_connection_sup:start_link(),
        Conn = pgsql_connection:open("test", "test"),
        {SupPid, Conn}
    end,
    fun({SupPid, Conn}) ->
        pgsql_connection:close(Conn),
        kill_sup(SupPid)
    end,
    fun({_SupPid, Conn}) ->
    [
        ?_assertEqual({selected, [{127}]}, pgsql_connection:sql_query("select 127::int2", Conn)),
        ?_assertEqual({selected, [{-126}]}, pgsql_connection:sql_query("select -126::int2", Conn)),
        ?_assertEqual({selected, [{127}]}, pgsql_connection:sql_query("select 127::int4", Conn)),
        ?_assertEqual({selected, [{-126}]}, pgsql_connection:sql_query("select -126::int4", Conn)),
        ?_assertEqual({selected, [{127}]}, pgsql_connection:sql_query("select 127::int8", Conn)),
        ?_assertEqual({selected, [{-126}]}, pgsql_connection:sql_query("select -126::int8", Conn)),
        ?_assertEqual({selected, [{127}]}, pgsql_connection:param_query("select 127::int2", [], Conn)),
        ?_assertEqual({selected, [{-126}]}, pgsql_connection:param_query("select -126::int2", [], Conn)),
        ?_assertEqual({selected, [{127}]}, pgsql_connection:param_query("select 127::int4", [], Conn)),
        ?_assertEqual({selected, [{-126}]}, pgsql_connection:param_query("select -126::int4", [], Conn)),
        ?_assertEqual({selected, [{127}]}, pgsql_connection:param_query("select 127::int8", [], Conn)),
        ?_assertEqual({selected, [{-126}]}, pgsql_connection:param_query("select -126::int8", [], Conn))
    ]
    end}.

% Numerics can be either integers or floats.
numeric_types_test_() ->
    {setup,
    fun() ->
        {ok, SupPid} = pgsql_connection_sup:start_link(),
        Conn = pgsql_connection:open("test", "test"),
        {SupPid, Conn}
    end,
    fun({SupPid, Conn}) ->
        pgsql_connection:close(Conn),
        kill_sup(SupPid)
    end,
    fun({_SupPid, Conn}) ->
    [
        % text values (simple_query)
        ?_assertEqual({{select, 1}, [{127}]}, pgsql_connection:simple_query("select 127::numeric", Conn)),
        ?_assertEqual({{select, 1}, [{-126}]}, pgsql_connection:simple_query("select -126::numeric", Conn)),
        ?_assertEqual({{select, 1}, [{123456789012345678901234567890}]}, pgsql_connection:simple_query("select 123456789012345678901234567890::numeric", Conn)),
        ?_assertEqual({{select, 1}, [{-123456789012345678901234567890}]}, pgsql_connection:simple_query("select -123456789012345678901234567890::numeric", Conn)),
        ?_assertEqual({{select, 1}, [{'NaN'}]}, pgsql_connection:simple_query("select 'NaN'::numeric", Conn)),
        ?_assertEqual({{select, 1}, [{123456789012345678901234.567890}]}, pgsql_connection:simple_query("select 123456789012345678901234.567890::numeric", Conn)),
        ?_assertEqual({{select, 1}, [{-123456789012345678901234.567890}]}, pgsql_connection:simple_query("select -123456789012345678901234.567890::numeric", Conn)),
        ?_assertEqual({{select, 1}, [{1000000.0}]}, pgsql_connection:simple_query("select 1000000.0::numeric", [], Conn)),
        ?_assertEqual({{select, 1}, [{10000.0}]}, pgsql_connection:simple_query("select 10000.0::numeric", [], Conn)),
        ?_assertEqual({{select, 1}, [{100.0}]}, pgsql_connection:simple_query("select 100.0::numeric", [], Conn)),
        ?_assertEqual({{select, 1}, [{1.0}]}, pgsql_connection:simple_query("select 1.0::numeric", [], Conn)),
        ?_assertEqual({{select, 1}, [{0.0}]}, pgsql_connection:simple_query("select 0.0::numeric", [], Conn)),
        ?_assertEqual({{select, 1}, [{0.1}]}, pgsql_connection:simple_query("select 0.1::numeric", [], Conn)),
        ?_assertEqual({{select, 1}, [{0.00001}]}, pgsql_connection:simple_query("select 0.00001::numeric", [], Conn)),
        ?_assertEqual({{select, 1}, [{0.0000001}]}, pgsql_connection:simple_query("select 0.0000001::numeric", [], Conn)),

        % binary values (extended_query)
        ?_assertEqual({{select, 1}, [{127}]}, pgsql_connection:extended_query("select 127::numeric", [], Conn)),
        ?_assertEqual({{select, 1}, [{-126}]}, pgsql_connection:extended_query("select -126::numeric", [], Conn)),
        ?_assertEqual({{select, 1}, [{123456789012345678901234567890}]}, pgsql_connection:extended_query("select 123456789012345678901234567890::numeric", [], Conn)),
        ?_assertEqual({{select, 1}, [{-123456789012345678901234567890}]}, pgsql_connection:extended_query("select -123456789012345678901234567890::numeric", [], Conn)),
        ?_assertEqual({{select, 1}, [{'NaN'}]}, pgsql_connection:extended_query("select 'NaN'::numeric", [], Conn)),
        ?_test(begin
            {{select, 1}, [{Val}]} = pgsql_connection:extended_query("select 123456789012345678901234.567890::numeric", [], Conn),
            ?assert(Val > 123456789012345500000000.0),
            ?assert(Val < 123456789012345700000000.0)
        end),
        ?_test(begin
            {{select, 1}, [{Val}]} = pgsql_connection:extended_query("select -123456789012345678901234.567890::numeric", [], Conn),
            ?assert(Val > -123456789012345700000000.0),
            ?assert(Val < -123456789012345500000000.0)
        end),
        ?_assertEqual({{select, 1}, [{1000000.0}]}, pgsql_connection:extended_query("select 1000000.0::numeric", [], Conn)),
        ?_assertEqual({{select, 1}, [{10000.0}]}, pgsql_connection:extended_query("select 10000.0::numeric", [], Conn)),
        ?_assertEqual({{select, 1}, [{100.0}]}, pgsql_connection:extended_query("select 100.0::numeric", [], Conn)),
        ?_assertEqual({{select, 1}, [{1.0}]}, pgsql_connection:extended_query("select 1.0::numeric", [], Conn)),
        ?_assertEqual({{select, 1}, [{0.0}]}, pgsql_connection:extended_query("select 0.0::numeric", [], Conn)),
        ?_assertEqual({{select, 1}, [{0.1}]}, pgsql_connection:extended_query("select 0.1::numeric", [], Conn)),
        ?_assertEqual({{select, 1}, [{0.00001}]}, pgsql_connection:extended_query("select 0.00001::numeric", [], Conn)),
        ?_assertEqual({{select, 1}, [{0.0000001}]}, pgsql_connection:extended_query("select 0.0000001::numeric", [], Conn))
    ]
    end}.

datetime_types_test_() ->
    {setup,
    fun() ->
        {ok, SupPid} = pgsql_connection_sup:start_link(),
        Conn = pgsql_connection:open("127.0.0.1", "test", "test", "", [{timezone, "UTC"}]),
        {SupPid, Conn}
    end,
    fun({SupPid, Conn}) ->
        pgsql_connection:close(Conn),
        kill_sup(SupPid)
    end,
    fun({_SupPid, Conn}) ->
    [
        ?_assertEqual({selected, [{{2012,1,17}}]},    pgsql_connection:sql_query("select '2012-01-17 10:54:03.45'::date", Conn)),
        ?_assertEqual({selected, [{{10,54,3}}]},   pgsql_connection:sql_query("select '2012-01-17 10:54:03'::time", Conn)),
        ?_assertEqual({selected, [{{10,54,3.45}}]},   pgsql_connection:sql_query("select '2012-01-17 10:54:03.45'::time", Conn)),
        ?_assertEqual({selected, [{{10,54,3.45}}]},   pgsql_connection:sql_query("select '2012-01-17 10:54:03.45'::timetz", Conn)),
        ?_assertEqual({selected, [{{{2012,1,17},{10,54,3}}}]},   pgsql_connection:sql_query("select '2012-01-17 10:54:03'::timestamp", Conn)),
        ?_assertEqual({selected, [{{{2012,1,17},{10,54,3.45}}}]},   pgsql_connection:sql_query("select '2012-01-17 10:54:03.45'::timestamp", Conn)),
        ?_assertEqual({selected, [{{{2012,1,17},{10,54,3.45}}}]},   pgsql_connection:sql_query("select '2012-01-17 10:54:03.45'::timestamptz", Conn)),
        ?_assertEqual({selected, [{{{1972,1,17},{10,54,3.45}}}]},   pgsql_connection:sql_query("select '1972-01-17 10:54:03.45'::timestamp", Conn)),
        ?_assertEqual({selected, [{{{1972,1,17},{10,54,3.45}}}]},   pgsql_connection:sql_query("select '1972-01-17 10:54:03.45'::timestamptz", Conn)),
        ?_assertEqual({selected, [{{1970,1,1}}]},   pgsql_connection:sql_query("select 'epoch'::date", Conn)),
        ?_assertEqual({selected, [{{0,0,0}}]},   pgsql_connection:sql_query("select 'allballs'::time", Conn)),
        ?_assertEqual({selected, [{infinity}]},   pgsql_connection:sql_query("select 'infinity'::timestamp", Conn)),
        ?_assertEqual({selected, [{'-infinity'}]},   pgsql_connection:sql_query("select '-infinity'::timestamp", Conn)),
        ?_assertEqual({selected, [{infinity}]},   pgsql_connection:sql_query("select 'infinity'::timestamptz", Conn)),
        ?_assertEqual({selected, [{'-infinity'}]},   pgsql_connection:sql_query("select '-infinity'::timestamptz", Conn)),

        ?_assertEqual({selected, [{{2012,1,17}}]},    pgsql_connection:param_query("select '2012-01-17 10:54:03.45'::date", [], Conn)),
        ?_assertEqual({selected, [{{10,54,3}}]},   pgsql_connection:param_query("select '2012-01-17 10:54:03'::time", [], Conn)),
        ?_assertEqual({selected, [{{10,54,3.45}}]},   pgsql_connection:param_query("select '2012-01-17 10:54:03.45'::time", [], Conn)),
        ?_assertEqual({selected, [{{10,54,3.45}}]},   pgsql_connection:param_query("select '2012-01-17 10:54:03.45'::timetz", [], Conn)),
        ?_assertEqual({selected, [{{{2012,1,17},{10,54,3}}}]},   pgsql_connection:param_query("select '2012-01-17 10:54:03'::timestamp", [], Conn)),
        ?_assertEqual({selected, [{{{2012,1,17},{10,54,3.45}}}]},   pgsql_connection:param_query("select '2012-01-17 10:54:03.45'::timestamp", [], Conn)),
        ?_assertEqual({selected, [{{{2012,1,17},{10,54,3.45}}}]},   pgsql_connection:param_query("select '2012-01-17 10:54:03.45'::timestamptz", [], Conn)),
        ?_assertEqual({selected, [{{{1972,1,17},{10,54,3.45}}}]},   pgsql_connection:param_query("select '1972-01-17 10:54:03.45'::timestamp", [], Conn)),
        ?_assertEqual({selected, [{{{1972,1,17},{10,54,3.45}}}]},   pgsql_connection:param_query("select '1972-01-17 10:54:03.45'::timestamptz", [], Conn)),
        ?_assertEqual({selected, [{{1970,1,1}}]},   pgsql_connection:param_query("select 'epoch'::date", [], Conn)),
        ?_assertEqual({selected, [{{0,0,0}}]},   pgsql_connection:param_query("select 'allballs'::time", [], Conn)),
        ?_assertEqual({selected, [{infinity}]},   pgsql_connection:param_query("select 'infinity'::timestamp", [], Conn)),
        ?_assertEqual({selected, [{'-infinity'}]},   pgsql_connection:param_query("select '-infinity'::timestamp", [], Conn)),
        ?_assertEqual({selected, [{infinity}]},   pgsql_connection:param_query("select 'infinity'::timestamptz", [], Conn)),
        ?_assertEqual({selected, [{'-infinity'}]},   pgsql_connection:param_query("select '-infinity'::timestamptz", [], Conn)),

        ?_assertEqual({{select, 1}, [{{{2012,1,17},{10,54,3}}}]},   pgsql_connection:extended_query("select $1::timestamptz", [{{2012,1,17},{10,54,3}}], Conn)),
        ?_assertEqual({{select, 1}, [{{2012,1,17}}]},   pgsql_connection:extended_query("select $1::date", [{2012,1,17}], Conn)),
        ?_assertEqual({{select, 1}, [{{10,54,3}}]},   pgsql_connection:extended_query("select $1::time", [{10,54,3}], Conn)),

        {"Create temporary table for the times", ?_assertEqual({updated, 1}, pgsql_connection:sql_query("create temporary table times (a_timestamp timestamp, a_time time)", Conn))},
        {"Insert timestamp with micro second resolution",
            ?_assertEqual({{insert, 0, 1}, []}, pgsql_connection:extended_query("insert into times (a_timestamp, a_time) values ($1, $2)", [{{2014, 5, 15}, {12, 12, 12.999999}}, null], Conn))
        },
        {"Insert timestamp without micro second resolution",
            ?_assertEqual({{insert, 0, 1}, []}, pgsql_connection:extended_query("insert into times (a_timestamp, a_time) values ($1, $2)", [{{2014, 5, 15}, {12, 12, 12}}, null], Conn))
        },
        {"Insert a time with micro second resolution",
            ?_assertEqual({{insert, 0, 1}, []}, pgsql_connection:extended_query("insert into times (a_timestamp, a_time) values ($1, $2)", [null, {12, 12, 12.999999}], Conn))
        },
        {"Insert a time without micro second resolution",
            ?_assertEqual({{insert, 0, 1}, []}, pgsql_connection:extended_query("insert into times (a_timestamp, a_time) values ($1, $2)", [null, {12, 12, 12}], Conn))
        }
    ]
    end}.

subsecond_datetime_test_() ->
    {setup,
    fun() ->
        {ok, SupPid} = pgsql_connection_sup:start_link(),
        Conn = pgsql_connection:open("127.0.0.1", "test", "test", "", [{timezone, "UTC"}]),
        {SupPid, Conn}
    end,
    fun({SupPid, Conn}) ->
        pgsql_connection:close(Conn),
        kill_sup(SupPid)
    end,
    fun({_SupPid, Conn}) ->
    [
        ?_assertEqual({{select, 1}, [{{{2012,1,17},{10,54,3.52}}}]}, pgsql_connection:simple_query("select '2012-01-17T10:54:03.52'::timestamptz", Conn)),
        ?_assertEqual({{select, 1}, [{{{2012,1,17},{10,54,4}}}]}, pgsql_connection:simple_query("select '2012-01-17T10:54:03.52'::timestamptz", [{datetime_float_seconds, round}], Conn)),
        ?_assertEqual({{select, 1}, [{{{2012,1,17},{10,54,3.52}}}]}, pgsql_connection:simple_query("select '2012-01-17T10:54:03.52'::timestamptz", [{datetime_float_seconds, as_available}], Conn)),
        ?_assertEqual({{select, 1}, [{{{2012,1,17},{10,54,3.52}}}]}, pgsql_connection:simple_query("select '2012-01-17T10:54:03.52'::timestamptz", [{datetime_float_seconds, always}], Conn)),
        ?_assertEqual({{select, 1}, [{{{2012,1,17},{10,54,3}}}]}, pgsql_connection:simple_query("select '2012-01-17T10:54:03'::timestamptz", [{datetime_float_seconds, round}], Conn)),
        ?_assertEqual({{select, 1}, [{{{2012,1,17},{10,54,3}}}]}, pgsql_connection:simple_query("select '2012-01-17T10:54:03'::timestamptz", [{datetime_float_seconds, as_available}], Conn)),
        ?_assertEqual({{select, 1}, [{{{2012,1,17},{10,54,3.0}}}]}, pgsql_connection:simple_query("select '2012-01-17T10:54:03'::timestamptz", [{datetime_float_seconds, always}], Conn)),

        ?_assertEqual({{select, 1}, [{{{2012,1,17},{10,54,3.52}}}]}, pgsql_connection:extended_query("select '2012-01-17T10:54:03.52'::timestamptz", [], Conn)),
        ?_assertEqual({{select, 1}, [{{{2012,1,17},{10,54,4}}}]}, pgsql_connection:extended_query("select '2012-01-17T10:54:03.52'::timestamptz", [], [{datetime_float_seconds, round}], Conn)),
        ?_assertEqual({{select, 1}, [{{{2012,1,17},{10,54,3.52}}}]}, pgsql_connection:extended_query("select '2012-01-17T10:54:03.52'::timestamptz", [], [{datetime_float_seconds, as_available}], Conn)),
        ?_assertEqual({{select, 1}, [{{{2012,1,17},{10,54,3.52}}}]}, pgsql_connection:extended_query("select '2012-01-17T10:54:03.52'::timestamptz", [], [{datetime_float_seconds, always}], Conn)),
        ?_assertEqual({{select, 1}, [{{{2012,1,17},{10,54,3}}}]}, pgsql_connection:extended_query("select '2012-01-17T10:54:03'::timestamptz", [], [{datetime_float_seconds, round}], Conn)),
        ?_assertEqual({{select, 1}, [{{{2012,1,17},{10,54,3}}}]}, pgsql_connection:extended_query("select '2012-01-17T10:54:03'::timestamptz", [], [{datetime_float_seconds, as_available}], Conn)),
        ?_assertEqual({{select, 1}, [{{{2012,1,17},{10,54,3.0}}}]}, pgsql_connection:extended_query("select '2012-01-17T10:54:03'::timestamptz", [], [{datetime_float_seconds, always}], Conn)),

        ?_assertEqual({{select, 1}, [{{10,54,3.52}}]}, pgsql_connection:simple_query("select '10:54:03.52'::time", Conn)),
        ?_assertEqual({{select, 1}, [{{10,54,4}}]}, pgsql_connection:simple_query("select '10:54:03.52'::time", [{datetime_float_seconds, round}], Conn)),
        ?_assertEqual({{select, 1}, [{{10,54,3.52}}]}, pgsql_connection:simple_query("select '10:54:03.52'::time", [{datetime_float_seconds, as_available}], Conn)),
        ?_assertEqual({{select, 1}, [{{10,54,3.52}}]}, pgsql_connection:simple_query("select '10:54:03.52'::time", [{datetime_float_seconds, always}], Conn)),
        ?_assertEqual({{select, 1}, [{{10,54,3}}]}, pgsql_connection:simple_query("select '10:54:03'::time", [{datetime_float_seconds, round}], Conn)),
        ?_assertEqual({{select, 1}, [{{10,54,3}}]}, pgsql_connection:simple_query("select '10:54:03'::time", [{datetime_float_seconds, as_available}], Conn)),
        ?_assertEqual({{select, 1}, [{{10,54,3.0}}]}, pgsql_connection:simple_query("select '10:54:03'::time", [{datetime_float_seconds, always}], Conn))
    ]
    end}.

tz_test_() ->
    {setup,
    fun() ->
        {ok, SupPid} = pgsql_connection_sup:start_link(),
        PosTZConn = pgsql_connection:open("127.0.0.1", "test", "test", "", [{timezone, "UTC+2"}]),
        NegTZConn = pgsql_connection:open("127.0.0.1", "test", "test", "", [{timezone, "UTC-2"}]),
        {SupPid, PosTZConn, NegTZConn}
    end,
    fun({SupPid, PosTZConn, NegTZConn}) ->
        pgsql_connection:close(PosTZConn),
        pgsql_connection:close(NegTZConn),
        kill_sup(SupPid)
    end,
    fun({_SupPid, PosTZConn, NegTZConn}) ->
    [
        ?_assertEqual({{select,1},[{{11,4,3}}]},   pgsql_connection:simple_query("select '2015-01-03 11:04:03'::time", PosTZConn)),
        ?_assertEqual({{select,1},[{{13,4,3}}]},   pgsql_connection:simple_query("select '2015-01-03 11:04:03'::timetz", PosTZConn)),
        ?_assertEqual({{select,1},[{{11,4,3}}]},    pgsql_connection:extended_query("select '2015-01-03 11:04:03'::time", [], PosTZConn)),
        ?_assertEqual({{select,1},[{{13,4,3}}]},   pgsql_connection:extended_query("select '2015-01-03 11:04:03'::timetz", [], PosTZConn)),
        ?_assertEqual({{select,1},[{{{2015,1,3},{11,4,3}}}]},   pgsql_connection:simple_query("select '2015-01-03 11:04:03'::timestamp", PosTZConn)),
        ?_assertEqual({{select,1},[{{{2015,1,3},{13,4,3}}}]},   pgsql_connection:simple_query("select '2015-01-03 11:04:03'::timestamptz", PosTZConn)),
        ?_assertEqual({{select,1},[{{{2015,1,3},{11,4,3}}}]},    pgsql_connection:extended_query("select '2015-01-03 11:04:03'::timestamp", [], PosTZConn)),
        ?_assertEqual({{select,1},[{{{2015,1,3},{13,4,3}}}]},   pgsql_connection:extended_query("select '2015-01-03 11:04:03'::timestamptz", [], PosTZConn)),

        ?_assertEqual({{select,1},[{{8,4,3}}]},   pgsql_connection:simple_query("select '2015-01-03 11:04:03+0300'::timetz", PosTZConn)),
        ?_assertEqual({{select,1},[{{8,4,3}}]},   pgsql_connection:extended_query("select '2015-01-03 11:04:03+0300'::timetz", [], PosTZConn)),
        ?_assertEqual({{select,1},[{{{2015,1,3},{8,4,3}}}]},   pgsql_connection:simple_query("select '2015-01-03 11:04:03+0300'::timestamptz", PosTZConn)),
        ?_assertEqual({{select,1},[{{{2015,1,3},{8,4,3}}}]},   pgsql_connection:extended_query("select '2015-01-03 11:04:03+0300'::timestamptz", [], PosTZConn)),
        ?_assertEqual({{select,1},[{{14,4,3}}]},   pgsql_connection:simple_query("select '2015-01-03 11:04:03-0300'::timetz", PosTZConn)),
        ?_assertEqual({{select,1},[{{14,4,3}}]},   pgsql_connection:extended_query("select '2015-01-03 11:04:03-0300'::timetz", [], PosTZConn)),
        ?_assertEqual({{select,1},[{{{2015,1,3},{14,4,3}}}]},   pgsql_connection:simple_query("select '2015-01-03 11:04:03-0300'::timestamptz", PosTZConn)),
        ?_assertEqual({{select,1},[{{{2015,1,3},{14,4,3}}}]},   pgsql_connection:extended_query("select '2015-01-03 11:04:03-0300'::timestamptz", [], PosTZConn)),
        
        
        ?_assertEqual({{select,1},[{{11,4,3}}]},   pgsql_connection:simple_query("select '2015-01-03 11:04:03'::time", NegTZConn)),
        ?_assertEqual({{select,1},[{{9,4,3}}]},   pgsql_connection:simple_query("select '2015-01-03 11:04:03'::timetz", NegTZConn)),
        ?_assertEqual({{select,1},[{{11,4,3}}]},    pgsql_connection:extended_query("select '2015-01-03 11:04:03'::time", [], NegTZConn)),
        ?_assertEqual({{select,1},[{{9,4,3}}]},   pgsql_connection:extended_query("select '2015-01-03 11:04:03'::timetz", [], NegTZConn)),
        ?_assertEqual({{select,1},[{{{2015,1,3},{11,4,3}}}]},   pgsql_connection:simple_query("select '2015-01-03 11:04:03'::timestamp", NegTZConn)),
        ?_assertEqual({{select,1},[{{{2015,1,3},{9,4,3}}}]},   pgsql_connection:simple_query("select '2015-01-03 11:04:03'::timestamptz", NegTZConn)),
        ?_assertEqual({{select,1},[{{{2015,1,3},{11,4,3}}}]},    pgsql_connection:extended_query("select '2015-01-03 11:04:03'::timestamp", [], NegTZConn)),
        ?_assertEqual({{select,1},[{{{2015,1,3},{9,4,3}}}]},   pgsql_connection:extended_query("select '2015-01-03 11:04:03'::timestamptz", [], NegTZConn)),

        ?_assertEqual({{select,1},[{{8,4,3}}]},   pgsql_connection:simple_query("select '2015-01-03 11:04:03+0300'::timetz", NegTZConn)),
        ?_assertEqual({{select,1},[{{8,4,3}}]},   pgsql_connection:extended_query("select '2015-01-03 11:04:03+0300'::timetz", [], NegTZConn)),
        ?_assertEqual({{select,1},[{{{2015,1,3},{8,4,3}}}]},   pgsql_connection:simple_query("select '2015-01-03 11:04:03+0300'::timestamptz", NegTZConn)),
        ?_assertEqual({{select,1},[{{{2015,1,3},{8,4,3}}}]},   pgsql_connection:extended_query("select '2015-01-03 11:04:03+0300'::timestamptz", [], NegTZConn)),
        ?_assertEqual({{select,1},[{{14,4,3}}]},   pgsql_connection:simple_query("select '2015-01-03 11:04:03-0300'::timetz", NegTZConn)),
        ?_assertEqual({{select,1},[{{14,4,3}}]},   pgsql_connection:extended_query("select '2015-01-03 11:04:03-0300'::timetz", [], NegTZConn)),
        ?_assertEqual({{select,1},[{{{2015,1,3},{14,4,3}}}]},   pgsql_connection:simple_query("select '2015-01-03 11:04:03-0300'::timestamptz", NegTZConn)),
        ?_assertEqual({{select,1},[{{{2015,1,3},{14,4,3}}}]},   pgsql_connection:extended_query("select '2015-01-03 11:04:03-0300'::timestamptz", [], NegTZConn))
    ]
    end}.

return_descriptions_test_() ->
    {setup,
        fun() ->
                {ok, SupPid} = pgsql_connection_sup:start_link(),
                Conn = pgsql_connection:open("test", "test"),
                {SupPid, Conn}
        end,
        fun({SupPid, Conn}) ->
                pgsql_connection:close(Conn),
                kill_sup(SupPid)
        end,
        fun({_SupPid, Conn}) ->
                [
                    ?_test(
                        begin
                            R = pgsql_connection:simple_query("select 'foo'::text as a", [{return_descriptions, true}], Conn),
                            ?assertMatch({{select,1},[_], [{<<"foo">>}]}, R),
                            {{select,1},[D], [{<<"foo">>}]} = R,
                            ?assert(is_tuple(D)),
                            ?assertEqual(element(2, D), <<"a">>)
                        end
                    ),
                    ?_test(
                        begin
                            R = pgsql_connection:extended_query("select $1::text as a", [<<"foo">>], [{return_descriptions, true}], Conn),
                            ?assertMatch({{select,1},[_], [{<<"foo">>}]}, R),
                            {{select,1},[D], [{<<"foo">>}]} = R,
                            ?assert(is_tuple(D)),
                            ?assertEqual(element(2, D), <<"a">>)
                        end
                    )
                ]
        end
    }.

fold_test_() ->
    {setup,
    fun() ->
        {ok, SupPid} = pgsql_connection_sup:start_link(),
        Conn = pgsql_connection:open("test", "test"),
        {SupPid, Conn}
    end,
    fun({SupPid, Conn}) ->
        pgsql_connection:close(Conn),
        kill_sup(SupPid)
    end,
    fun({_SupPid, Conn}) ->
    [
        {timeout, 20,
        ?_test(begin
            {updated, 1} = pgsql_connection:sql_query("create temporary table tmp (id integer primary key, a_text text)", Conn),
            {updated, 0} = pgsql_connection:sql_query("BEGIN", Conn),
            Val = lists:foldl(fun(I, Acc) ->
                Str = "foobar " ++ integer_to_list(I * 42),
                {updated, 1} = pgsql_connection:param_query("insert into tmp (id, a_text) values (?, ?)", [I, Str], Conn),
                Acc + length(Str)
            end, 0, lists:seq(1, 3742)),
            {updated, 0} = pgsql_connection:sql_query("COMMIT", Conn),
            R = pgsql_connection:fold(fun({Text}, Acc) ->
                Acc + byte_size(Text)
            end, 0, "select a_text from tmp", Conn),
            ?assertEqual({ok, Val}, R)
        end)
        }
    ]
    end}.

map_test_() ->
    {setup,
    fun() ->
        {ok, SupPid} = pgsql_connection_sup:start_link(),
        Conn = pgsql_connection:open("test", "test"),
        {SupPid, Conn}
    end,
    fun({SupPid, Conn}) ->
        pgsql_connection:close(Conn),
        kill_sup(SupPid)
    end,
    fun({_SupPid, Conn}) ->
    [
        {timeout, 20,
        ?_test(begin
            {updated, 1} = pgsql_connection:sql_query("create temporary table tmp (id integer primary key, a_text text)", Conn),
            {updated, 0} = pgsql_connection:sql_query("BEGIN", Conn),
            ValR = lists:foldl(fun(I, Acc) ->
                Str = "foobar " ++ integer_to_list(I * 42),
                {updated, 1} = pgsql_connection:param_query("insert into tmp (id, a_text) values (?, ?)", [I, Str], Conn),
                [length(Str) | Acc]
            end, [], lists:seq(1, 3742)),
            Val = lists:reverse(ValR),
            {updated, 0} = pgsql_connection:sql_query("COMMIT", Conn),
            R = pgsql_connection:map(fun({Text}) ->
                byte_size(Text)
            end, "select a_text from tmp", Conn),
            ?assertEqual({ok, Val}, R)
        end)
        }
    ]
    end}.

map_fold_foreach_should_return_when_query_is_invalid_test_() ->
   {setup,
    fun() ->
        {ok, SupPid} = pgsql_connection_sup:start_link(),
        Conn = pgsql_connection:open("test", "test"),
        {SupPid, Conn}
    end,
    fun({SupPid, Conn}) ->
        pgsql_connection:close(Conn),
        kill_sup(SupPid)
    end,
    fun({_SupPid, Conn}) ->
    [
        ?_test(begin
            R = pgsql_connection:extended_query("select toto", [], Conn),
            ?assertMatch({error, _}, R)
        end),
        ?_test(begin
            R = pgsql_connection:map(fun(_) -> ok end, "select toto", Conn),
            ?assertMatch({error, _}, R)
        end),
        ?_test(begin
            R = pgsql_connection:fold(fun(_,_) -> ok end, ok, "select toto", Conn),
            ?assertMatch({error, _}, R)
        end),
        ?_test(begin
            R = pgsql_connection:foreach(fun(_) -> ok end, "select toto", Conn),
            ?assertMatch({error, _}, R)
        end)
    ]
    end}.

foreach_test_() ->
    {setup,
    fun() ->
        {ok, SupPid} = pgsql_connection_sup:start_link(),
        Conn = pgsql_connection:open("test", "test"),
        {SupPid, Conn}
    end,
    fun({SupPid, Conn}) ->
        pgsql_connection:close(Conn),
        kill_sup(SupPid)
    end,
    fun({_SupPid, Conn}) ->
    [
        {timeout, 20,
        ?_test(begin
            {updated, 1} = pgsql_connection:sql_query("create temporary table tmp (id integer primary key, a_text text)", Conn),
            {updated, 0} = pgsql_connection:sql_query("BEGIN", Conn),
            ValR = lists:foldl(fun(I, Acc) ->
                Str = "foobar " ++ integer_to_list(I * 42),
                {updated, 1} = pgsql_connection:param_query("insert into tmp (id, a_text) values (?, ?)", [I, Str], Conn),
                [length(Str) | Acc]
            end, [], lists:seq(1, 3742)),
            Val = lists:reverse(ValR),
            {updated, 0} = pgsql_connection:sql_query("COMMIT", Conn),
            Self = self(),
            R = pgsql_connection:foreach(fun({Text}) ->
                Self ! {foreach_inner, byte_size(Text)}
            end, "select a_text from tmp", Conn),
            ?assertEqual(ok, R),
            lists:foreach(fun(AVal) ->
                receive {foreach_inner, AVal} -> ok end
            end, Val)
        end)
        }
    ]
    end}.

timeout_test_() ->
    {setup,
    fun() ->
        {ok, SupPid} = pgsql_connection_sup:start_link(),
        Conn = pgsql_connection:open("test", "test"),
        {SupPid, Conn}
    end,
    fun({SupPid, Conn}) ->
        pgsql_connection:close(Conn),
        kill_sup(SupPid)
    end,
    fun({_SupPid, Conn}) ->
    [
        ?_assertEqual({selected, [{null}]}, pgsql_connection:sql_query("select pg_sleep(2)", Conn)),
        ?_assertEqual({selected, [{null}]}, pgsql_connection:param_query("select pg_sleep(2)", [], Conn)),
        ?_assertEqual({selected, [{null}]}, pgsql_connection:sql_query("select pg_sleep(2)", [], infinity, Conn)),
        ?_assertEqual({selected, [{null}]}, pgsql_connection:param_query("select pg_sleep(2)", [], [], infinity, Conn)),
        ?_assertEqual({selected, [{null}]}, pgsql_connection:sql_query("select pg_sleep(2)", [], 2500, Conn)),
        ?_assertEqual({selected, [{null}]}, pgsql_connection:param_query("select pg_sleep(2)", [], [], 2500, Conn)),
        ?_assertMatch({error, {pgsql_error, _}}, pgsql_connection:sql_query("select pg_sleep(2)", [], 1500, Conn)),
        ?_assertMatch({error, {pgsql_error, _}}, pgsql_connection:param_query("select pg_sleep(2)", [], [], 1500, Conn)),
        ?_assertEqual({selected, [{null}]}, pgsql_connection:sql_query("select pg_sleep(2)", Conn)),
        ?_assertEqual({selected, [{null}]}, pgsql_connection:param_query("select pg_sleep(2)", [], Conn)),
        ?_test(begin
            ShowResult1 = pgsql_connection:simple_query("show statement_timeout", Conn),
            ?assertMatch({show, [{_}]}, ShowResult1),
            {show, [{Value1}]} = ShowResult1,
            ?assertEqual({{select, 1}, [{1}]}, pgsql_connection:simple_query("select 1", [], 2500, Conn)),
            ?assertEqual({{select, 1}, [{1}]}, pgsql_connection:simple_query("select 1", [], Conn)),
            ShowResult2 = pgsql_connection:simple_query("show statement_timeout", Conn),
            ?assertMatch({show, [{_}]}, ShowResult2),
            {show, [{Value2}]} = ShowResult2,
            ?assertEqual({set, []}, pgsql_connection:simple_query("set statement_timeout to 2500", Conn)),
            ?assertEqual({{select, 1}, [{1}]}, pgsql_connection:simple_query("select 1", [], 2500, Conn)),
            ?assertEqual({{select, 1}, [{1}]}, pgsql_connection:simple_query("select 1", [], Conn)),
            ShowResult3 = pgsql_connection:simple_query("show statement_timeout", Conn),
            ?assertMatch({show, [{_}]}, ShowResult3),
            
            % Only guarantee is that if the default was 0 (infinity), it is maintained
            % after a query with a default (infinity) timeout.
            if
                Value1 =:= <<"0">> -> ?assertEqual(Value1, Value2);
                true -> ok
            end
        end)
    ]
    end}.

json_types_test_() ->
    {setup,
        fun() ->
                {ok, SupPid} = pgsql_connection_sup:start_link(),
                Conn = pgsql_connection:open("test", "test"),
                {SupPid, Conn}
        end,
        fun({SupPid, Conn}) ->
                pgsql_connection:close(Conn),
                kill_sup(SupPid)
        end,
        fun({_SupPid, Conn}) ->
                [?_test(begin
                    {updated, 1} = pgsql_connection:sql_query("create temporary table tmp (id integer primary key, a_json json, b_json json)", Conn),
                    {{insert,0,1}, []} = pgsql_connection:extended_query("insert into tmp (id, b_json) values ($1, $2)", [2, {json, <<"[{\"a\":\"foo\"},{\"b\":\"bar\"},{\"c\":\"baz\"}]">>}], Conn),
                    ?assertEqual({{select,1},[{{json,<<"[{\"a\":\"foo\"},{\"b\":\"bar\"},{\"c\":\"baz\"}]">>}}]}, pgsql_connection:simple_query("select '[{\"a\":\"foo\"},{\"b\":\"bar\"},{\"c\":\"baz\"}]'::json", Conn)),
                    ?assertEqual({{select,1},[{{json,<<"[{\"a\":\"foo\"},{\"b\":\"bar\"},{\"c\":\"baz\"}]">>}}]}, pgsql_connection:simple_query("select b_json from tmp where id = 2", Conn)),
                    ?assertEqual({{select,1},[{{json,<<"[{\"a\":\"foo\"},{\"b\":\"bar\"},{\"c\":\"baz\"}]">>}}]}, pgsql_connection:extended_query("select b_json from tmp where id = $1", [2], Conn))
                end),
                ?_test(begin
                    %% only run jsonb tests if type exists.
                    case pgsql_connection:simple_query("SELECT 1 FROM pg_type WHERE typname = 'jsonb'", Conn) of
                        {{select, 1}, [{1}]} ->
                            {updated, 1} = pgsql_connection:sql_query("create temporary table tmp_b (id integer primary key, a_json jsonb, b_json json)", Conn),
                            ?assertEqual({{select,1},[{{jsonb,<<"[{\"a\": \"foo\"}, {\"b\": \"bar\"}, {\"c\": \"baz\"}]">>}}]}, pgsql_connection:simple_query("select '[{\"a\":\"foo\"},{\"b\":\"bar\"},{\"c\":\"baz\"}]'::jsonb", Conn)),
                            {{insert,0,1}, []} = pgsql_connection:extended_query("insert into tmp_b (id, a_json) values ($1, $2)", [1, {jsonb, <<"[{\"a\":\"foo\"},{\"b\":\"bar\"},{\"c\":\"baz\"}]">>}], Conn),
                            ?assertEqual({{select,1},[{{jsonb,<<"[{\"a\": \"foo\"}, {\"b\": \"bar\"}, {\"c\": \"baz\"}]">>}}]}, pgsql_connection:simple_query("select a_json from tmp_b where id = 1", Conn)),
                            ?assertEqual({{select,1},[{{jsonb,<<"[{\"a\": \"foo\"}, {\"b\": \"bar\"}, {\"c\": \"baz\"}]">>}}]}, pgsql_connection:extended_query("select a_json from tmp_b where id = $1", [1], Conn));
                        {{select, 0}, []} ->
                            ok
                    end
                end)
                ]
        end
    }.


constraint_violation_test_() ->
    {setup,
    fun() ->
        {ok, SupPid} = pgsql_connection_sup:start_link(),
        Conn = pgsql_connection:open("test", "test"),
        {SupPid, Conn}
    end,
    fun({SupPid, Conn}) ->
        pgsql_connection:close(Conn),
        kill_sup(SupPid)
    end,
    fun({_SupPid, Conn}) ->
    [
        ?_test(begin
            {updated, 1} = pgsql_connection:sql_query("create temporary table tmp (id integer primary key, a_text text)", Conn),
            {updated, 1} = pgsql_connection:param_query("insert into tmp (id, a_text) values (?, ?)", [1, <<"hello">>], Conn),
            E = pgsql_connection:param_query("insert into tmp (id, a_text) values (?, ?)", [1, <<"world">>], Conn),
            ?assertMatch({error, {pgsql_error, _}}, E),
            {error, Err} = E,
            ?assert(pgsql_error:is_integrity_constraint_violation(Err))
        end)
    ]
    end}.

custom_enum_test_() ->
    {setup,
    fun() ->
        {ok, SupPid} = pgsql_connection_sup:start_link(),
        Conn = pgsql_connection:open("test", "test"),
        pgsql_connection:sql_query("DROP TYPE IF EXISTS mood;", Conn),
        {SupPid, Conn}
    end,
    fun({SupPid, Conn}) ->
        pgsql_connection:sql_query("DROP TYPE mood;", Conn),
        pgsql_connection:close(Conn),
        kill_sup(SupPid)
    end,
    fun({_SupPid, Conn}) ->
    [
        ?_test(begin
            {updated, 0} = pgsql_connection:sql_query("BEGIN", Conn),
            {updated, 1} = pgsql_connection:sql_query("CREATE TYPE mood AS ENUM ('sad', 'ok', 'happy');", Conn),
            ?assertMatch({selected, [{{MoodOID, <<"sad">>}}]} when is_integer(MoodOID), pgsql_connection:sql_query("select 'sad'::mood;", Conn)),
            ?assertMatch({selected, [{{MoodOID, <<"sad">>}}]} when is_integer(MoodOID), pgsql_connection:param_query("select 'sad'::mood;", [], Conn)),
            {updated, 0} = pgsql_connection:sql_query("COMMIT", Conn),
            ?assertMatch({selected, [{{mood, <<"sad">>}}]}, pgsql_connection:sql_query("select 'sad'::mood;", Conn)),
            ?assertMatch({selected, [{{mood, <<"sad">>}}]}, pgsql_connection:param_query("select 'sad'::mood;", [], Conn)),
            ?assertMatch({selected, [{{mood, <<"sad">>}}]}, pgsql_connection:param_query("select ?::mood;", [<<"sad">>], Conn)),
            ?assertMatch({selected, [{{array, [{mood, <<"sad">>}]}}]}, pgsql_connection:sql_query("select '{sad}'::mood[];", Conn)),
            ?assertMatch({selected, [{{array, [{mood, <<"sad">>}]}}]}, pgsql_connection:param_query("select ?::mood[];", [{array, [<<"sad">>]}], Conn))
        end)
    ]
    end}.

custom_enum_native_test_() ->
    {setup,
    fun() ->
        {ok, SupPid} = pgsql_connection_sup:start_link(),
        Conn = pgsql_connection:open("test", "test"),
        pgsql_connection:simple_query("DROP TYPE IF EXISTS mood;", Conn),
        {SupPid, Conn}
    end,
    fun({SupPid, Conn}) ->
        {{drop, type}, []} = pgsql_connection:simple_query("DROP TYPE mood;", Conn),
        pgsql_connection:close(Conn),
        kill_sup(SupPid)
    end,
    fun({_SupPid, Conn}) ->
    [
        ?_test(begin
            {'begin', []} = pgsql_connection:simple_query("BEGIN", Conn),
            {{create, type}, []} = pgsql_connection:simple_query("CREATE TYPE mood AS ENUM ('sad', 'ok', 'happy');", Conn),
            ?assertMatch({{select, 1}, [{{MoodOID, <<"sad">>}}]} when is_integer(MoodOID), pgsql_connection:simple_query("select 'sad'::mood;", Conn)),
            ?assertMatch({{select, 1}, [{{MoodOID, <<"sad">>}}]} when is_integer(MoodOID), pgsql_connection:extended_query("select 'sad'::mood;", [], Conn)),
            {'commit', []} = pgsql_connection:simple_query("COMMIT", Conn),
            ?assertMatch({{select, 1}, [{{mood, <<"sad">>}}]}, pgsql_connection:simple_query("select 'sad'::mood;", Conn)),
            ?assertMatch({{select, 1}, [{{mood, <<"sad">>}}]}, pgsql_connection:extended_query("select 'sad'::mood;", [], Conn)),
            ?assertMatch({{select, 1}, [{{mood, <<"sad">>}}]}, pgsql_connection:extended_query("select $1::mood;", [<<"sad">>], Conn)),
            ?assertMatch({{select, 1}, [{{array, [{mood, <<"sad">>}]}}]}, pgsql_connection:simple_query("select '{sad}'::mood[];", Conn)),
            ?assertMatch({{select, 1}, [{{array, [{mood, <<"sad">>}]}}]}, pgsql_connection:extended_query("select $1::mood[];", [{array, [<<"sad">>]}], Conn))
        end)
    ]
    end}.

invalid_query_test_() ->
    {setup,
        fun() ->
                {ok, SupPid} = pgsql_connection_sup:start_link(),
                Conn = pgsql_connection:open("test", "test"),
                {{create, table}, []} = pgsql_connection:simple_query("CREATE TEMPORARY TABLE tmp(id integer primary key, other text)", Conn),
                {SupPid, Conn}
        end,
        fun({SupPid, Conn}) ->
                pgsql_connection:close(Conn),
                kill_sup(SupPid)
        end,
        fun({_SupPid, Conn}) ->
                [
                    ?_test(begin
                                ?assertEqual({error, {badarg, toto}}, pgsql_connection:extended_query("insert into tmp(id, other) values (1, $1)", [toto], Conn)),
                                % connection still usable
                                R = pgsql_connection:extended_query("insert into tmp(id, other) values (1, $1)", ["toto"], Conn),
                                ?assertEqual({{insert, 0, 1}, []}, R)
                        end),
                    ?_test(begin
                                ?assertMatch({error, {pgsql_error, _Error}}, pgsql_connection:simple_query("FOO", Conn)),
                                ?assertMatch({error, {pgsql_error, _Error}}, pgsql_connection:simple_query("FOO", [], Conn)),
                                ?assertMatch({error, {pgsql_error, _Error}}, pgsql_connection:simple_query("FOO", [], 5000, Conn)),
                                % connection still usable
                                R = pgsql_connection:extended_query("insert into tmp(id, other) values (2, $1)", ["toto"], Conn),
                                ?assertEqual({{insert, 0, 1}, []}, R)
                        end),
                    ?_test(begin
                                {'begin',[]} = pgsql_connection:simple_query("BEGIN", Conn),
                                R1 = pgsql_connection:extended_query("insert into tmp(id, other) values (3, $1)", ["toto"], Conn),
                                ?assertEqual({{insert, 0, 1}, []}, R1),
                                ?assertMatch({error, {pgsql_error, _Error}}, pgsql_connection:simple_query("FOO", [], 5000, Conn)),
                                ?assertMatch({error, {pgsql_error, _Error}}, pgsql_connection:simple_query("FOO", [], 5000, Conn)),
                                {'rollback',[]} = pgsql_connection:simple_query("COMMIT", Conn),
                                % row 3 was not inserted.
                                R1 = pgsql_connection:extended_query("insert into tmp(id, other) values (3, $1)", ["toto"], Conn),
                                ?assertEqual({{insert, 0, 1}, []}, R1)
                        end),
                    ?_test(begin
                                {'begin',[]} = pgsql_connection:simple_query("BEGIN", Conn),
                                R1 = pgsql_connection:extended_query("insert into tmp(id, other) values (4, $1)", ["toto"], Conn),
                                ?assertEqual({{insert, 0, 1}, []}, R1),
                                ?assertMatch({error, {pgsql_error, _Error}}, pgsql_connection:extended_query("FOO", [], [], 5000, Conn)),
                                ?assertMatch({error, {pgsql_error, _Error}}, pgsql_connection:extended_query("FOO", [], [], 5000, Conn)),
                                {'rollback',[]} = pgsql_connection:simple_query("COMMIT", Conn),
                                R1 = pgsql_connection:extended_query("insert into tmp(id, other) values (4, $1)", ["toto"], Conn),
                                ?assertEqual({{insert, 0, 1}, []}, R1)
                        end),
                    ?_test(begin
                                {'begin',[]} = pgsql_connection:simple_query("BEGIN", Conn),
                                R1 = pgsql_connection:extended_query("insert into tmp(id, other) values (5, $1)", ["toto"], Conn),
                                ?assertEqual({{insert, 0, 1}, []}, R1),
                                ?assertMatch({error, {pgsql_error, _Error}}, pgsql_connection:extended_query("FOO", [], [], 5000, Conn)),
                                {'rollback',[]} = pgsql_connection:simple_query("ROLLBACK", Conn),
                                R1 = pgsql_connection:extended_query("insert into tmp(id, other) values (5, $1)", ["toto"], Conn),
                                ?assertEqual({{insert, 0, 1}, []}, R1)
                        end),
                    ?_test(begin
                                {'begin',[]} = pgsql_connection:simple_query("BEGIN", Conn),
                                R1 = pgsql_connection:extended_query("insert into tmp(id, other) values (6, $1)", ["toto"], Conn),
                                ?assertEqual({{insert, 0, 1}, []}, R1),
                                ?assertMatch({error, {pgsql_error, _Error}}, pgsql_connection:extended_query("FOO", [], [], 5000, Conn)),
                                {'rollback',[]} = pgsql_connection:simple_query("ROLLBACK", [], 5000, Conn),
                                R1 = pgsql_connection:extended_query("insert into tmp(id, other) values (6, $1)", ["toto"], Conn),
                                ?assertEqual({{insert, 0, 1}, []}, R1)
                        end),
                    ?_test(begin
                                ?assertMatch({error, {pgsql_error, _Error}}, pgsql_connection:extended_query("FOO", [], Conn)),
                                % Empty array forces a Describe command, thus we end the normal sequence with Flush and not with Sync
                                % Error recovery therefore requires a Sync to get the ReadyForQuery message.
                                ?assertMatch({error, {pgsql_error, _Error}}, pgsql_connection:extended_query("FOO", [{array, [<<>>]}], Conn)),
                                % Likewise, cursor mode does send a Flush instead of a Sync after Bind
                                ?assertMatch({error, {pgsql_error, _Error}}, pgsql_connection:foreach(fun(_Row) -> ok end, "FOO", Conn)),
                                % connection still usable
                                R = pgsql_connection:extended_query("insert into tmp(id, other) values (7, $1)", ["toto"], Conn),
                                ?assertEqual({{insert, 0, 1}, []}, R)
                        end)
                ]
        end
    }.


cancel_test_() ->
    {setup,
    fun() ->
        {ok, SupPid} = pgsql_connection_sup:start_link(),
        Conn = pgsql_connection:open("test", "test"),
        {SupPid, Conn}
    end,
    fun({SupPid, Conn}) ->
        pgsql_connection:close(Conn),
        kill_sup(SupPid)
    end,
    fun({_SupPid, Conn}) ->
    [
        ?_test(begin
            Self = self(),
            spawn_link(fun() ->
                SleepResult = pgsql_connection:simple_query("select pg_sleep(2)", Conn),
                Self ! {async_result, SleepResult}
            end),
            timer:sleep(100),
            ?assertEqual(ok, pgsql_connection:cancel(Conn)),
            receive
                {async_result, R} ->
                    ?assertMatch({error, {pgsql_error, _}}, R),
                    {error, {pgsql_error, F}} = R,
                    {code, Code} = lists:keyfind(code, 1, F),
                    ?assertEqual(Code, <<"57014">>)
            end,
            {{select, 1}, [{true}]} = pgsql_connection:simple_query("select true", Conn)
        end)
    ]
    end}.

sql_cancel_test_() ->
    {setup,
    fun() ->
        {ok, SupPid} = pgsql_connection_sup:start_link(),
        Conn1 = pgsql_connection:open("test", "test"),
        Conn2 = pgsql_connection:open("test", "test"),
        {SupPid, Conn1, Conn2}
    end,
    fun({SupPid, Conn1, Conn2}) ->
        pgsql_connection:close(Conn1),
        pgsql_connection:close(Conn2),
        kill_sup(SupPid)
    end,
    fun({_SupPid, Conn1, Conn2}) ->
    [
        ?_test(begin
            {{select, 1}, [{Conn1BackendPid}]} = pgsql_connection:simple_query("SELECT pg_backend_pid()", Conn1),
            Self = self(),
            spawn_link(fun() ->
                SleepResult = pgsql_connection:simple_query("select pg_sleep(2)", Conn1),
                Self ! {async_result, SleepResult}
            end),
            timer:sleep(100),
            {{select, 1}, [{true}]} = pgsql_connection:extended_query("SELECT pg_cancel_backend($1)", [Conn1BackendPid], Conn2),
            receive
                {async_result, R} ->
                    ?assertMatch({error, {pgsql_error, _}}, R),
                    {error, {pgsql_error, F}} = R,
                    {code, Code} = lists:keyfind(code, 1, F),
                    ?assertEqual(Code, <<"57014">>)
            end,
            {{select, 1}, [{NewConn1BackendPid}]} = pgsql_connection:simple_query("SELECT pg_backend_pid()", Conn1),
            ?assertEqual(NewConn1BackendPid, Conn1BackendPid)
        end)
    ]
    end}.

sql_terminate_test_() ->
    {setup,
    fun() ->
        {ok, SupPid} = pgsql_connection_sup:start_link(),
        Conn1 = pgsql_connection:open("test", "test"),
        Conn2 = pgsql_connection:open("test", "test"),
        {SupPid, Conn1, Conn2}
    end,
    fun({SupPid, Conn1, Conn2}) ->
        pgsql_connection:close(Conn1),
        pgsql_connection:close(Conn2),
        kill_sup(SupPid)
    end,
    fun({_SupPid, Conn1, Conn2}) ->
    [
        ?_test(begin
            {{select, 1}, [{Conn1BackendPid}]} = pgsql_connection:simple_query("SELECT pg_backend_pid()", Conn1),
            Self = self(),
            spawn_link(fun() ->
                SleepResult = pgsql_connection:simple_query("select pg_sleep(2)", Conn1),
                Self ! {async_result, SleepResult}
            end),
            timer:sleep(100),
            {{select, 1}, [{true}]} = pgsql_connection:extended_query("SELECT pg_terminate_backend($1)", [Conn1BackendPid], Conn2),
            receive
                {async_result, R} ->
                    ?assertEqual({error, closed}, R)
            end,
            {{select, 1}, [{NewConn1BackendPid}]} = pgsql_connection:simple_query("SELECT pg_backend_pid()", Conn1),
            ?assertNotEqual(NewConn1BackendPid, Conn1BackendPid)
        end)
    ]
    end}.

pending_test_() ->
    {setup,
    fun() ->
        {ok, SupPid} = pgsql_connection_sup:start_link(),
        Conn = pgsql_connection:open("test", "test"),
        {SupPid, Conn}
    end,
    fun({SupPid, Conn}) ->
        pgsql_connection:close(Conn),
        kill_sup(SupPid)
    end,
    fun({_SupPid, Conn}) ->
    [
        {timeout, 10, ?_test(begin
            {{create, table}, []} = pgsql_connection:simple_query("CREATE TEMPORARY TABLE tmp(id integer primary key, other text)", Conn),
            Parent = self(),
            WorkerA = spawn(fun() ->
                R0 = pgsql_connection:simple_query("SELECT COUNT(*) FROM tmp", Conn),
                Parent ! {r0, R0},
                receive continue -> ok end,
                R2 = pgsql_connection:simple_query("SELECT pg_sleep(1), COUNT(*) FROM tmp", Conn),
                Parent ! {r2, R2},
                R4 = pgsql_connection:simple_query("SELECT pg_sleep(1), COUNT(*) FROM tmp", Conn),
                Parent ! {r4, R4},
                R6 = pgsql_connection:simple_query("SELECT COUNT(*) FROM tmp", Conn),
                Parent ! {r6, R6}
            end),
            spawn(fun() ->
                R1 = pgsql_connection:simple_query("INSERT INTO tmp (id) VALUES (1)", Conn),
                Parent ! {r1, R1},
                WorkerA ! continue,
                loop_until_process_is_waiting(WorkerA), % make sure command 2 was sent.
                R3 = pgsql_connection:simple_query("INSERT INTO tmp SELECT 2 AS id, CAST (pg_sleep(0.5) AS text) AS other", Conn),
                Parent ! {r3, R3},
                R5 = pgsql_connection:simple_query("INSERT INTO tmp (id) VALUES (3)", Conn),
                Parent ! {r5, R5}
            end),
            receive {RT0, R0} -> ?assertEqual(r0, RT0), ?assertEqual({{select, 1}, [{0}]}, R0) end,
            receive {RT1, R1} -> ?assertEqual(r1, RT1), ?assertEqual({{insert, 0, 1}, []}, R1) end,
            receive {RT2, R2} -> ?assertEqual(r2, RT2), ?assertEqual({{select, 1}, [{null, 1}]}, R2) end,
            receive {RT3, R3} -> ?assertEqual(r3, RT3), ?assertEqual({{insert, 0, 1}, []}, R3) end,
            receive {RT4, R4} -> ?assertEqual(r4, RT4), ?assertEqual({{select, 1}, [{null, 2}]}, R4) end,
            receive {RT5, R5} -> ?assertEqual(r5, RT5), ?assertEqual({{insert, 0, 1}, []}, R5) end,
            receive {RT6, R6} -> ?assertEqual(r6, RT6), ?assertEqual({{select, 1}, [{3}]}, R6) end
        end)}
    ]
    end}.

loop_until_process_is_waiting(Pid) ->
    case process_info(Pid, status) of
        {status, waiting} -> ok;
        _ -> loop_until_process_is_waiting(Pid)
    end.

batch_test_() ->
    {setup,
    fun() ->
        {ok, SupPid} = pgsql_connection_sup:start_link(),
        Conn = pgsql_connection:open("test", "test"),
        {SupPid, Conn}
    end,
    fun({SupPid, Conn}) ->
        pgsql_connection:close(Conn),
        kill_sup(SupPid)
    end,
    fun({_SupPid, Conn}) ->
    [
        ?_assertEqual([{{select, 1}, [{1}]},{{select, 1}, [{2}]},{{select, 1}, [{3}]}], pgsql_connection:batch_query("select $1::int", [[1], [2], [3]], Conn)),
        ?_assertEqual([{{select, 1}, [{<<"bar">>}]},{{select, 1}, [{<<"foo">>}]},{{select, 1}, [{null}]}], pgsql_connection:batch_query("select $1::bytea", [[<<"bar">>], [<<"foo">>], [null]], Conn))
    ]
    end}.

async_process_loop(TestProcess) ->
    receive
        {set_test_process, Pid} ->
            async_process_loop(Pid);
        OtherMessage ->
            ?assert(is_pid(TestProcess)),
            TestProcess ! {self(), OtherMessage},
            async_process_loop(TestProcess)
    end.
        
notify_test_() ->
    {setup,
    fun() ->
        {ok, SupPid} = pgsql_connection_sup:start_link(),
        AsyncProcess = spawn_link(fun() ->
            async_process_loop(undefined)
        end),
        Conn1 = pgsql_connection:open([{database, "test"}, {user, "test"}, {async, AsyncProcess}]),
        Conn2 = pgsql_connection:open("test", "test"),
        {SupPid, Conn1, Conn2, AsyncProcess}
    end,
    fun({SupPid, Conn1, Conn2, AsyncProcess}) ->
        pgsql_connection:close(Conn1),
        pgsql_connection:close(Conn2),
        unlink(AsyncProcess),
        exit(AsyncProcess, normal),
        kill_sup(SupPid)
    end,
    fun({_SupPid, Conn1, Conn2, AsyncProcess}) ->
    [
        ?_test(begin
            R = pgsql_connection:simple_query("LISTEN test_channel", Conn1),
            ?assertEqual({listen, []}, R)
        end),
        {"Notifications are received while idle",
        ?_test(begin
            AsyncProcess ! {set_test_process, self()},
            R = pgsql_connection:simple_query("NOTIFY test_channel", Conn2),
            ?assertEqual({notify, []}, R),
            receive {AsyncProcess, NotifyMessage} ->
                ?assertMatch({pgsql, Conn1, {notification, _PID, <<"test_channel">>, <<>>}}, NotifyMessage)
            after 1000 -> ?assert(false)
            end
        end)
        },
        {"Notifications are received with payload",
        ?_test(begin
            AsyncProcess ! {set_test_process, self()},
            R = pgsql_connection:simple_query("NOTIFY test_channel, 'payload string'", Conn2),
            ?assertEqual({notify, []}, R),
            receive {AsyncProcess, NotifyMessage} ->
                ?assertMatch({pgsql, Conn1, {notification, _PID, <<"test_channel">>, <<"payload string">>}}, NotifyMessage)
            after 1000 -> ?assert(false)
            end
        end)
        },
        {"Notifications are received with a busy connection executing several requests",
        ?_test(begin
            Parent = self(),
            AsyncProcess ! {set_test_process, Parent},
            spawn_link(fun() ->
                R = pgsql_connection:simple_query("SELECT pg_sleep(0.5)", Conn1),
                ?assertEqual({{select, 1}, [{null}]}, R),
                AsyncProcess ! sleep_1
            end),
            timer:sleep(100),
            spawn_link(fun() ->
                R = pgsql_connection:simple_query("SELECT pg_sleep(0.5)", Conn1),
                ?assertEqual({{select, 1}, [{null}]}, R),
                AsyncProcess ! sleep_2
            end),
            R = pgsql_connection:simple_query("NOTIFY test_channel", Conn2),
            ?assertEqual({notify, []}, R),
            % Acceptable orders are : sleep_1, notification, sleep_2 or notification, sleep_1, sleep_2.
            % PostgreSQL currently (9.2) sends notification after sleep_1 is completed, once the transaction is finished.
            % See note at http://www.postgresql.org/docs/9.2/static/protocol-flow.html#PROTOCOL-ASYNC
            Message0 = receive {AsyncProcess, Msg0} -> Msg0 after 1500 -> ?assert(false) end,
            Message1 = receive {AsyncProcess, Msg1} -> Msg1 after 1500 -> ?assert(false) end,
            Message2 = receive {AsyncProcess, Msg2} -> Msg2 after 1500 -> ?assert(false) end,
            ?assertEqual(sleep_2, Message2),
            case Message0 of
                sleep_1 ->
                    ?assertMatch({pgsql, Conn1, {notification, _PID, <<"test_channel">>, <<>>}}, Message1);
                {pgsql, Conn1, {notification, _PID, <<"test_channel">>, <<>>}} ->
                    ?assertEqual(sleep_1, Message1)
            end
        end)
        },
        {"Subscribe for notifications",
        ?_test(begin
            pgsql_connection:subscribe(self(), Conn1),
            AsyncProcess ! {set_test_process, self()},
            R = pgsql_connection:simple_query("NOTIFY test_channel, '1'", Conn2),
            ?assertEqual({notify, []}, R),
            receive {AsyncProcess, {pgsql, Conn1, {notification, _PID1, <<"test_channel">>, <<"1">>}}} -> ok
            after 1000 -> ?assert(false)
            end,
            receive {pgsql, Conn1, {notification, _PID2, <<"test_channel">>, <<"1">>}} -> ok
            after 1000 -> ?assert(false)
            end,
            pgsql_connection:unsubscribe(self(), Conn1),
            R = pgsql_connection:simple_query("NOTIFY test_channel, '2'", Conn2),
            ?assertEqual({notify, []}, R),
            receive {AsyncProcess, {pgsql, Conn1, {notification, _PID3, <<"test_channel">>, <<"2">>}}} -> ok
            after 1000 -> ?assert(false)
            end,
            receive {pgsql, Conn1, {notification, _PID4, <<"test_channel">>, <<"2">>}} -> ?assert(false)
            after 1000 -> ok
            end
        end)
        }
    ]
    end}.

notice_test_() ->
    {setup,
    fun() ->
        {ok, SupPid} = pgsql_connection_sup:start_link(),
        NoticeProcess = spawn_link(fun() ->
            async_process_loop(undefined)
        end),
        Conn1 = pgsql_connection:open([{database, "test"}, {user, "test"}, {async, NoticeProcess}]),
        {SupPid, Conn1, NoticeProcess}
    end,
    fun({SupPid, Conn1, NoticeProcess}) ->
        pgsql_connection:close(Conn1),
        unlink(NoticeProcess),
        exit(NoticeProcess, normal),
        kill_sup(SupPid)
    end,
    fun({_SupPid, Conn1, AsyncProcess}) ->
    [
        ?_test(begin
            AsyncProcess ! {set_test_process, self()},
            % Set client_min_messages to NOTICE. This is the default, but some environment (e.g. Travis) may have it configured otherwise.
            R1 = pgsql_connection:simple_query("SET client_min_messages=NOTICE;", Conn1),
            ?assertEqual({'set', []}, R1),
            R2 = pgsql_connection:simple_query("DO $$ BEGIN RAISE NOTICE 'test notice'; END $$;", Conn1),
            ?assertEqual({'do', []}, R2),
            receive {AsyncProcess, NoticeMessage} ->
                ?assertMatch({pgsql, Conn1, {notice, _Fields}}, NoticeMessage),
                {pgsql, Conn1, {notice, Fields}} = NoticeMessage,
                ?assertEqual({severity, <<"NOTICE">>}, lists:keyfind(severity, 1, Fields)),
                ?assertEqual({message, <<"test notice">>}, lists:keyfind(message, 1, Fields))
            after 1000 -> ?assert(false)
            end
        end)
    ]
    end}.
