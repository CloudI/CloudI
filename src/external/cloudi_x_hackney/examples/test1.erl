#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ../_build/default/lib/hackney/ebin  -pa ../_build/default/lib/*/ebin  -pa ../_build/default/lib/certifi/ebin  -pa ../_build/default/lib/idna/ebin  -pa ../_build/default/lib/metrics/ebin -pa ../_build/default/lib/mimerl/ebin   -pa ../_build/default/lib/ssl_verify_fun/ebin -pa ../_build/default/lib/unicode_util_compat/ebin

-module(test1).



main(_) ->
  application:ensure_all_started(hackney),

  io:format("step 1~n", []),
    {ok, _, _, Ref} = hackney:request(get, <<"https://friendpaste.com">>,
                                         [], <<>>, [{pool, default}]),
    {ok, Body} = hackney:body(Ref),
    io:format("body: ~p~n~n", [Body]),

    io:format("step 1~n", []),
    {ok, _, _, Ref2} = hackney:request(get,
                                       <<"https://friendpaste.com/_all_languages">>,
                                       [],
                                       <<>>),
    {ok, Body1} = hackney:body(Ref2),
    io:format("body: ~p~n~n", [Body1]),

    io:format("step 3~n", []),
    ReqBody = << "{
         \"id\": \"some_paste_id\",
         \"rev\": \"some_revision_id\",
         \"changeset\": \"changeset in unidiff format\"
    }" >>,
    ReqHeaders = [{<<"Content-Type">>, <<"application/json">>}],
    {ok, _, _, Ref3} = hackney:request(post, <<"https://friendpaste.com">>,
                                    ReqHeaders, ReqBody),
    {ok, Body2} = hackney:body(Ref3),
    io:format("body: ~p~n~n", [Body2]),

    io:format("step 4~n", []),
    ReqBody1 = {file, "./examples/test.json"},
    {ok, _, _, Ref4} = hackney:request(post, <<"https://friendpaste.com">>,
                                       ReqHeaders, ReqBody1),
    {ok, Body3} = hackney:body(Ref4),
    io:format("body: ~p~n~n", [Body3]),

    IsClosed = hackney_manager:get_state(Ref4) =:= req_not_found,
    io:format("has been closed: ~p~n", [IsClosed]).
