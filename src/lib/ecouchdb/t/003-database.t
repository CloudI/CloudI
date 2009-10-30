#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

main(_) ->
    etap:plan(14),
    pre_run(),
    test(),
    etap:end_tests(),
    ok.

pre_run() ->
    {A1, A2, A3} = now(),
    random:seed(A1, A2, A3),
    ok.

database() ->
    lists:flatten([
        [[random:uniform(25) + 96] || _ <-lists:seq(1,5)],
        [[random:uniform(9) + 47] || _ <-lists:seq(1,3)]
    ]).

test() ->
    Database = database(),

    (fun() ->
        etap:is(ecouchdb:create_database("localhost", 5984, Database, 5000), {ok, ok}, "tmp database created"),
        {ok, DatabaseProps} = ecouchdb:database_info("localhost", 5984, Database, 5000),
        etap:is(proplists:get_value(<<"db_name">>, DatabaseProps), list_to_binary(Database), "name ok"),
        etap:is(proplists:get_value(<<"doc_count">>, DatabaseProps), 0, "document count ok"),
        etap:is(proplists:get_value(<<"doc_del_count">>, DatabaseProps), 0, "document delete count ok"),
        etap:is(proplists:get_value(<<"update_seq">>, DatabaseProps), 0, "update count ok"),
        etap:is(proplists:get_value(<<"purge_seq">>, DatabaseProps), 0, "purge count ok"),
        etap:is(proplists:get_value(<<"compact_running">>, DatabaseProps), false, "compaction status ok"),
        ok
    end)(),
    
    (fun() ->
        {ok, Databases} = ecouchdb:retrieve_all_dbs("localhost", 5984, 5000),
        etap:any(list_to_binary(Database), Databases, "tmp database listed"),
        ok
    end)(),
    
    (fun() ->
        etap:fun_is(fun ({error, _}) -> true; (_) -> false end, ecouchdb:retrieve_all_dbs("example.com", 80, 5000), "Triggering server 'other' response"),
        ok
    end)(),
    
    (fun() ->
        etap:fun_is(fun ({error, _}) -> true; (_) -> false end, ecouchdb:database_info("example.com", 80, "asdasdasd", 5000), "Triggering server 'other' response"),
        ok
    end)(),

    (fun() ->
        etap:fun_is(fun ({error, {json, {struct, [{<<"error">>, _} | _]}}}) -> true; (_) -> false end, ecouchdb:database_info("localhost", 5984, "hahahahano", 5000), "database_info/2 on non-existing db."),
        ok
    end)(),

    (fun() ->
        Error = {error,{json,{struct,[{<<"error">>,<<"file_exists">>}, {<<"reason">>, <<"The database could not be created, the file already exists.">>}]}}},
        etap:is(ecouchdb:create_database("localhost", 5984, Database, 5000), Error, "tmp database created"),
        ok
    end)(),

    (fun() ->
        etap:is(ecouchdb:delete_database("localhost", 5984, Database, 5000), {ok, ok}, "tmp database created"),
        ok
    end)(),

    (fun() ->
        etap:fun_is(fun ({error, {json, {struct, [{<<"error">>, _} | _]}}}) -> true; (_) -> false end, ecouchdb:delete_database("localhost", 5984, Database, 5000), "tmp database created"),
        ok
    end)(),

    ok.
