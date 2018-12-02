-module(elli_handover_tests).
-include_lib("eunit/include/eunit.hrl").
-include("elli_test.hrl").

elli_test_() ->
    {setup,
     fun setup/0, fun teardown/1,
     [
      ?_test(hello_world()),
      ?_test(echo())
     ]}.



setup() ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    {ok, _} = application:ensure_all_started(hackney),

    Config = [
              {mods, [
                      {elli_example_callback_handover, []}
                     ]}
             ],

    {ok, P} = elli:start_link([{callback, elli_middleware},
                               {callback_args, Config},
                               {port, 3003}]),
    unlink(P),
    [P].

teardown(Pids) ->
    [elli:stop(P) || P <- Pids].


%%
%% INTEGRATION TESTS
%% Uses hackney to actually call Elli over the network
%%

hello_world() ->
    Response = hackney:get("http://localhost:3003/hello/world"),
    ?assertMatch(200, status(Response)),
    ?assertMatch([{<<"Connection">>, <<"close">>},
                  {<<"Content-Length">>, <<"12">>}], headers(Response)),
    ?assertMatch(<<"Hello World!">>, body(Response)).

echo() ->
    Response = hackney:get("http://localhost:3003/hello?name=knut"),
    ?assertMatch(200, status(Response)),
    ?assertMatch(<<"Hello knut">>, body(Response)).
