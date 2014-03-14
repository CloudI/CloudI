#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin

main(_) ->
    etap:plan(4),
    case (catch test()) of
        {'EXIT', Err} ->
            io:format("# ~p~n", [Err]),
            etap:bail();
        _ ->
            etap:end_tests()
    end,
    ok.

test() ->
    {ok, Data} = ecouchdb:server_info("localhost", 5984, 5000),
    etap:is(proplists:get_value(<<"couchdb">>, Data), <<"Welcome">>, "message ok"),
    etap:is(proplists:get_value(<<"version">>, Data), <<"0.10.0">>, "version ok"),
    etap:fun_is(fun ({error, _}) -> true; (_) -> false end, ecouchdb:server_info("localhost", 5985, 5000), "server_info/1 nok"),
    etap:fun_is(fun ({error,{invalid_json, _}}) -> true; ({error, timeout}) -> true; (_) -> false end, ecouchdb:server_info("example.com", 80, 5000), "Triggering server 'other' response"),
    ok.
