-module(elli_middleware_tests).
-include_lib("eunit/include/eunit.hrl").
-include("elli_test.hrl").

elli_test_() ->
    {setup,
     fun setup/0, fun teardown/1,
     [
      ?_test(hello_world()),
      ?_test(short_circuit()),
      ?_test(compress()),
      ?_test(no_callbacks())
     ]}.


%%
%% TESTS
%%

short_circuit() ->
    URL      = "http://localhost:3002/middleware/short-circuit",
    Response = hackney:get(URL),
    ?assertMatch(<<"short circuit!">>, body(Response)).

hello_world() ->
    URL      = "http://localhost:3002/hello/world",
    Response = hackney:get(URL),
    ?assertMatch(<<"Hello World!">>, body(Response)).

compress() ->
    Url      = "http://localhost:3002/compressed",
    Headers  = [{<<"Accept-Encoding">>, <<"gzip">>}],
    Response = hackney:get(Url, Headers),
    ?assertMatch([{<<"Connection">>, <<"Keep-Alive">>},
                  {<<"Content-Encoding">>, <<"gzip">>},
                  {<<"Content-Length">>, <<"41">>}],
                 headers(Response)),
    ?assertEqual(binary:copy(<<"Hello World!">>, 86),
                 zlib:gunzip(body(Response))),
    Response1 = hackney:get("http://localhost:3002/compressed"),
    ?assertMatch([{<<"Connection">>, <<"Keep-Alive">>},
                  {<<"Content-Length">>, <<"1032">>}],
                 headers(Response1)),
    ?assertEqual(iolist_to_binary(lists:duplicate(86, "Hello World!")),
                 body(Response1)),
    Url2      = "http://localhost:3002/compressed-io_list",
    Headers2  = [{<<"Accept-Encoding">>, <<"gzip">>}],
    Response2 = hackney:get(Url2, Headers2),
    ?assertMatch(200, status(Response2)),
    ?assertMatch([{<<"Connection">>, <<"Keep-Alive">>},
                  {<<"Content-Encoding">>, <<"gzip">>},
                  {<<"Content-Length">>, <<"41">>}],
                 headers(Response2)),
    ?assertEqual(binary:copy(<<"Hello World!">>, 86),
                 zlib:gunzip(body(Response2))),
    Response3 = hackney:request("http://localhost:3002/compressed-io_list"),
    ?assertMatch(200, status(Response3)),
    ?assertMatch([{<<"Connection">>, <<"Keep-Alive">>},
                  {<<"Content-Length">>, <<"1032">>}],
                 headers(Response3)),
    ?assertEqual(iolist_to_binary(lists:duplicate(86, "Hello World!")),
                 body(Response3)).

no_callbacks() ->
    Response = hackney:get("http://localhost:3004/whatever"),
    ?assertMatch(404, status(Response)),
    ?assertMatch(<<"Not Found">>, body(Response)).


%%
%% HELPERS
%%

setup() ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    {ok, _} = application:ensure_all_started(hackney),

    Config = [
              {mods, [
                      {elli_access_log, [{name, elli_syslog},
                                         {ip, "127.0.0.1"},
                                         {port, 514}]},
                      {elli_example_middleware, []},
                      {elli_middleware_compress, []},
                      {elli_example_callback, []}
                     ]}
             ],

    {ok, P1} = elli:start_link([{callback, elli_middleware},
                                {callback_args, Config},
                                {port, 3002}]),
    unlink(P1),
    {ok, P2} = elli:start_link([{callback, elli_middleware},
                                {callback_args, [{mods, []}]},
                                {port, 3004}]),
    unlink(P2),
    [P1, P2].

teardown(Pids) ->
    [elli:stop(P) || P <- Pids].
