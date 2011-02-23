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
    {ok, Pid} = ememcached:start_link([{"127.0.0.1", 11211, 1}]),

    etap:is(ememcached:set(Pid, "Hello", <<"World">>), <<>>, "set ok"),
    etap:is(ememcached:add(Pid, "Hello", <<"Fail">>), <<"Data exists for key.">>, "add ok"),
    etap:is(ememcached:get(Pid, "Hello"), <<"World">>, "get ok"),
    etap:is(ememcached:delete(Pid, "Hello"), <<>>, "delete ok"),
    etap:is(ememcached:add(Pid, "Hello", <<"World2">>), <<>>, "add ok"),
    etap:is(ememcached:get(Pid, "Hello"), <<"World2">>, "get ok"),
    etap:is(ememcached:append(Pid, "Hello", <<"!!!">>), <<>>, "append ok"),
    etap:is(ememcached:get(Pid, "Hello"), <<"World2!!!">>, "get ok"),
    etap:is(ememcached:prepend(Pid, "Hello", <<"$$$">>), <<>>, "prepend ok"),
    etap:is(ememcached:get(Pid, "Hello"), <<"$$$World2!!!">>, "get ok"),
    etap:is(ememcached:delete(Pid, "Hello"), <<>>, "delete ok"),
    etap:is(ememcached:get(Pid, "Hello"), <<>>, "get ok"),
    
    ememcached:set(Pid, "One", <<"A">>),
    ememcached:set(Pid, "Two", <<"B">>),
    ememcached:set(Pid, "Three", <<"C">>),
    
    io:format("stats ~p~n", [ememcached:stats()]),
    
    etap:is(ememcached:get_many(Pid, ["One", "Two", "Two-and-a-half", "Three"]), [{"One",<<"A">>},{"Two",<<"B">>},{"Two-and-a-half",<<>>},{"Three",<<"C">>}], "get_many ok"),
    
    etap:is(ememcached:flush(Pid, 0), [{{"127.0.0.1",11211},<<>>}], "flush ok"),
    
	etap:is(ememcached:quit(Pid), [{{"127.0.0.1",11211},<<>>}], "quit ok"),
	
    ok.
