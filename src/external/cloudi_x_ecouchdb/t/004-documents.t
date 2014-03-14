#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

main(_) ->
    etap:plan(26),
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
        ok
    end)(),
    
    (fun() ->
        {ok, Databases} = ecouchdb:retrieve_all_dbs("localhost", 5984, 5000),
        etap:any(list_to_binary(Database), Databases, "tmp database listed"),
        ok
    end)(),
    
    %% Create a document
    (fun() ->
        etap:fun_is(
            fun ({ok, {json,{struct,[{<<"ok">>,true}, {<<"id">>,<<"FooDocument">>}, {<<"rev">>, FooRev}]}}}) ->
                    put(foo_rev, FooRev),
                    true;
                (_) -> false
            end,
            ecouchdb:create_document("localhost", 5984, Database, "FooDocument", [{<<"foo">>, <<"bar">>}], 5000),
            "Creating document"
        ),
        ok
    end)(),

    %% Fetch back that document
    (fun() ->
        etap:fun_is(
            fun ({ok, {json, {struct, Keys}}}) ->
                    etap:is(proplists:get_value(<<"_id">>, Keys), <<"FooDocument">>, "_id ok"),
                    etap:is(proplists:get_value(<<"foo">>, Keys), <<"bar">>, "foo ok"),
                    true;
                (_) -> false
            end,
            ecouchdb:retrieve_document("localhost", 5984, Database, "FooDocument", 5000),
            "Fetching document"
        ),
        ok
    end)(),
    
    %% Create a document
    (fun() ->
        etap:fun_is(
            fun ({ok, {json,{struct,[{<<"ok">>,true}, {<<"id">>, DocID}, {<<"rev">>, DocRev}]}}}) ->
                    put(doc_id_1, DocID),
                    put(doc_rev_1, DocRev),
                    true;
                (_) ->
                    false
            end,
            ecouchdb:create_document("localhost", 5984, Database, [{<<"bar">>, <<"baz">>}], 5000),
            "Creating document"
        ),
        ok
    end)(),

    (fun() ->
        etap:fun_is(
            fun ({ok, {json, {struct, Keys}}}) ->
                    etap:is(proplists:get_value(<<"_id">>, Keys), get(doc_id_1), "_id ok"),
                    etap:is(proplists:get_value(<<"bar">>, Keys), <<"baz">>, "bar ok"),
                    true;
                (_) -> false
            end,
            ecouchdb:retrieve_document("localhost", 5984, Database, binary_to_list(get(doc_id_1)), 5000),
            "Fetching document"
        ),
        ok
    end)(),
    
    %% Create a document
    (fun() ->
        etap:fun_is(
            fun ({ok, {json,{struct,[{<<"ok">>,true}, {<<"id">>, _}, {<<"rev">>, _}]}}}) ->
                    true;
                (_) ->
                    false
            end,
            ecouchdb:create_document("localhost", 5984, Database, {struct, [{<<"bar">>, <<"baz">>}]}, 5000),
            "Creating document"
        ),
        ok
    end)(),
    
    %% Create a document
    (fun() ->
        Documents = [
            [{<<"username">>, <<"Nick">>}],
            [{<<"username">>, <<"Tom">>}],
            [{<<"username">>, <<"Jan">>}]
        ],
        etap:fun_is(
            fun ({ok, {json, [{struct,[{<<"id">>, ID1}, {<<"rev">>, _}]}, {struct,[{<<"id">>, ID2}, {<<"rev">>, _}]}, {struct,[{<<"id">>, ID3}, {<<"rev">>, _}]}]}}) -> 
                    put(doc_id_2, ID1),
                    put(doc_id_3, ID2),
                    put(doc_id_4, ID3),                    
                    true;
                (_) ->
                    false
            end,
            ecouchdb:create_documents("localhost", 5984, Database, Documents, 5000),
            "Creating documents"
        ),
        ok
    end)(),
    
    %% Create a document
    (fun() ->
        etap:fun_is(
            fun ({ok, {json,{struct,[{<<"ok">>,true}, {<<"id">>, DocID}, {<<"rev">>, _}]}}}) ->
                    put(doc_id_5, DocID),
                    true;
                (Other) ->
                    io:format("Other ~p~n", [Other]),
                    false
            end,
            ecouchdb:create_attachment(
                "localhost", 5984,
                Database,
                "DocWithAttachment",
                "README.markdown",
                "text/plain",
                5000
            ),
            "Creating document attachment"
        ),
        ok
    end)(),
    
    (fun() ->
        RevTuple = {ok, {get(doc_id_1), get(doc_rev_1)}},
        etap:is(
            RevTuple,
            ecouchdb:document_revision("localhost", 5984, Database, binary_to_list(get(doc_id_1)), 5000),
            "Fetching rev"
        ),
        etap:fun_is(
            fun ({error,{json,{struct,[{<<"error">>,_}|_]}}}) ->
                true;
                (_) ->
                false
            end,
            ecouchdb:document_revision("localhost", 5984, Database, "NotARealDoc", 5000),
            "Fetching rev"
        ),
        etap:fun_is(
            fun ({error, _}) -> true; (Other) -> io:format("Other ~p~n", [Other]), false end,
            ecouchdb:document_revision("google.com", 80, Database, "NotARealDoc", 5000),
            "Fetching rev"
        ),
        ok
    end)(),

    (fun() ->
        etap:fun_is(
            fun ({ok, {json,{struct,[{<<"ok">>,true}, {<<"id">>, DocID}, {<<"rev">>, NewRev}]}}}) ->
                    DocID = get(doc_id_1),
                    put(doc_rev_1, NewRev),
                    true;
                (_) -> false
            end,
            ecouchdb:update_document("localhost", 5984, Database, binary_to_list(get(doc_id_1)), [{<<"foo">>, <<"biz">>}, {<<"_rev">>, get(doc_rev_1)}], 5000),
            "Creating document"
        ),
        ok
    end)(),
    
    (fun() ->
        DocID = get(doc_id_1),
        etap:fun_is(
            fun ({ok, {json, {struct, Keys}}}) ->
                    etap:is(proplists:get_value(<<"_id">>, Keys), DocID, "_id ok"),
                    etap:is(proplists:get_value(<<"foo">>, Keys), <<"biz">>, "foo ok"),
                    true;
                (_) -> false
            end,
            ecouchdb:retrieve_document("localhost", 5984, Database, binary_to_list(get(doc_id_1)), 5000),
            "Fetching document"
        ),
        ok
    end)(),

    (fun() ->
        etap:fun_is(
            fun ({ok, {json,{struct,[{<<"ok">>,true}, {<<"id">>, DocID}, {<<"rev">>, NewRev}]}}}) ->
                    DocID = get(doc_id_1),
                    put(doc_rev_1, NewRev),
                    true;
                (_) -> false
            end,
            ecouchdb:update_document("localhost", 5984, Database, binary_to_list(get(doc_id_1)), {struct, [{<<"foo">>, <<"buz">>}, {<<"_rev">>, get(doc_rev_1)}]}, 5000),
            "Creating document"
        ),
        ok
    end)(),
    
    (fun() ->
        etap:fun_is(
            fun ({ok, {json,{struct,[{<<"ok">>, true}, {<<"id">>, <<"FooDocument">>}, {<<"rev">>, _}]}}}) -> true;
                (_) -> false
            end,
            ecouchdb:delete_document("localhost", 5984, Database, "FooDocument", binary_to_list(get(foo_rev)), 5000),
            "document deleted"
        ),
        ok
    end)(),
    
    (fun() ->
        etap:is(ecouchdb:delete_documents("localhost", 5984, Database, [get(doc_id_1), get(doc_id_2), get(doc_id_3)], 5000), {ok, {json, []}}, "documents deleted"),
        ok
    end)(),

    (fun() ->
        etap:is(ecouchdb:delete_database("localhost", 5984, Database, 5000), {ok, ok}, "tmp database deleted"),
        ok
    end)(),

    ok.
