#!/usr/bin/env escript
%% -*- erlang -*-
%%! -pa ./ebin -sasl errlog_type error -boot start_sasl -noshell

main(_) ->
    etap:plan(unknown),
    case (catch start()) of
        {'EXIT', Err} ->
            io:format("Err ~p~n", [Err]),
            etap:bail();
        _ ->
            etap:end_tests()
    end,
    ok.
    
start() ->
    {ok, _} = mcerlang:start_link([{"127.0.0.1", 11211, 1}]),

    etap:is(mcerlang:set("Hello", <<"World">>), <<>>, "set ok"),
    etap:is(mcerlang:add("Hello", <<"Fail">>), <<"Data exists for key.">>, "add ok"),
    etap:is(mcerlang:get("Hello"), <<"World">>, "get ok"),
    etap:is(mcerlang:delete("Hello"), <<>>, "delete ok"),
    etap:is(mcerlang:add("Hello", <<"World2">>), <<>>, "add ok"),
    etap:is(mcerlang:get("Hello"), <<"World2">>, "get ok"),
    etap:is(mcerlang:append("Hello", <<"!!!">>), <<>>, "append ok"),
    etap:is(mcerlang:get("Hello"), <<"World2!!!">>, "get ok"),
    etap:is(mcerlang:prepend("Hello", <<"$$$">>), <<>>, "prepend ok"),
    etap:is(mcerlang:get("Hello"), <<"$$$World2!!!">>, "get ok"),
    etap:is(mcerlang:delete("Hello"), <<>>, "delete ok"),
    etap:is(mcerlang:get("Hello"), <<>>, "get ok"),
    
    mcerlang:set("One", <<"A">>),
    mcerlang:set("Two", <<"B">>),
    mcerlang:set("Three", <<"C">>),
    
    io:format("stats ~p~n", [mcerlang:stats()]),
    
    etap:is(mcerlang:get_many(["One", "Two", "Two-and-a-half", "Three"]), [{"One",<<"A">>},{"Two",<<"B">>},{"Two-and-a-half",<<>>},{"Three",<<"C">>}], "get_many ok"),
    
    etap:is(mcerlang:flush(0), [{{"127.0.0.1",11211},<<>>}], "flush ok"),
    
	etap:is(mcerlang:quit(), [{{"127.0.0.1",11211},<<>>}], "quit ok"),
	
    ok.