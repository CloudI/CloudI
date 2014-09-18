% Use: erlc hello.erl && erl -pa ./ebin -s hello run -s init stop -noshell

-module(hello).
-export([run/0]).

run() ->

    crypto:start(),
    application:start(emysql),

    emysql:add_pool(hello_pool, 1,
        "hello_username", "hello_password", "localhost", 3306,
        "hello_database", latin1),

    emysql:execute(hello_pool,
        <<"INSERT INTO hello_table SET hello_text = 'Hello World!'">>),

    Result = emysql:execute(hello_pool,
        <<"select hello_text from hello_table">>),

    io:format("~n~p~n", [Result]).
