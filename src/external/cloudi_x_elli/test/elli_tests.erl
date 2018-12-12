-module(elli_tests).
-include_lib("eunit/include/eunit.hrl").
-include("elli.hrl").
-include("elli_test.hrl").

-define(I2L(I), integer_to_list(I)).
-define(README, "README.md").
-define(VTB(T1, T2, LB, UB),
        time_diff_to_micro_seconds(T1, T2) >= LB andalso
        time_diff_to_micro_seconds(T1, T2) =< UB).
-ifdef(OTP_RELEASE).
-include_lib("kernel/include/logger.hrl").
-else.
-define(LOG_ERROR(Str), error_logger:error_msg(Str)).
-define(LOG_ERROR(Format,Data), error_logger:error_msg(Format, Data)).
-define(LOG_INFO(Format,Data), error_logger:info_msg(Format, Data)).
-endif.

time_diff_to_micro_seconds(T1, T2) ->
    erlang:convert_time_unit(
      get_timing_value(T2) -
          get_timing_value(T1),
      native,
      micro_seconds).

elli_test_() ->
    {setup,
     fun setup/0, fun teardown/1,
     [{foreach,
       fun init_stats/0, fun clear_stats/1,
       [?_test(hello_world()),
        ?_test(keep_alive_timings()),
        ?_test(not_found()),
        ?_test(crash()),
        ?_test(invalid_return()),
        ?_test(no_compress()),
        ?_test(gzip()),
        ?_test(deflate()),
        ?_test(exception_flow()),
        ?_test(hello_iolist()),
        ?_test(accept_content_type()),
        ?_test(user_connection()),
        ?_test(get_args()),
        ?_test(decoded_get_args()),
        ?_test(decoded_get_args_list()),
        ?_test(post_args()),
        ?_test(shorthand()),
        ?_test(ip()),
        ?_test(found()),
        ?_test(too_many_headers()),
        ?_test(too_big_body()),
        ?_test(way_too_big_body()),
        ?_test(bad_request_line()),
        ?_test(content_length()),
        ?_test(user_content_length()),
        ?_test(headers()),
        ?_test(chunked()),
        ?_test(sendfile()),
        ?_test(send_no_file()),
        ?_test(sendfile_error()),
        ?_test(sendfile_range()),
        ?_test(slow_client()),
        ?_test(post_pipeline()),
        ?_test(get_pipeline()),
        ?_test(head()),
        ?_test(no_body()),
        ?_test(sends_continue())
       ]}
     ]}.

get_timing_value(Key) ->
    [{timings, Timings}] = ets:lookup(elli_stat_table, timings),
    proplists:get_value(Key, Timings).

get_size_value(Key) ->
    [{sizes, Sizes}] = ets:lookup(elli_stat_table, sizes),
    proplists:get_value(Key, Sizes).

setup() ->
    application:start(crypto),
    application:start(public_key),
    application:start(ssl),
    {ok, _} = application:ensure_all_started(hackney),

    Config = [
              {mods, [
                      {elli_metrics_middleware, []},
                      {elli_middleware_compress, []},
                      {elli_example_callback, []}
                     ]}
             ],
    {ok, P} = elli:start_link([{callback, elli_middleware},
                               {callback_args, Config},
                               {port, 3001}]),
    unlink(P),
    [P].

teardown(Pids) ->
    [elli:stop(P) || P <- Pids].

init_stats() ->
    ets:new(elli_stat_table, [set, named_table, public]).

clear_stats(_) ->
    ets:delete(elli_stat_table).


accessors_test_() ->
    RawPath = <<"/foo/bar">>,
    Headers = [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}],
    Method = 'POST',
    Body = <<"name=knut%3D">>,
    Name = <<"knut=">>,
    Req1 = #req{raw_path = RawPath,
                headers = Headers,
                method = Method,
                body = Body},
    Args = [{<<"name">>, Name}],
    Req2 = #req{headers = Headers, args = Args, body = <<>>},

    [
     %% POST /foo/bar
     ?_assertMatch(RawPath, elli_request:raw_path(Req1)),
     ?_assertMatch(Headers, elli_request:headers(Req1)),
     ?_assertMatch(Method, elli_request:method(Req1)),
     ?_assertMatch(Body, elli_request:body(Req1)),
     ?_assertMatch(Args, elli_request:post_args_decoded(Req1)),
     ?_assertMatch(undefined, elli_request:post_arg(<<"foo">>, Req1)),
     ?_assertMatch(undefined, elli_request:post_arg_decoded(<<"foo">>, Req1)),
     ?_assertMatch(Name, elli_request:post_arg_decoded(<<"name">>, Req1)),
     %% GET /foo/bar
     ?_assertMatch(Headers, elli_request:headers(Req2)),

     ?_assertMatch(Args, elli_request:get_args(Req2)),
     ?_assertMatch(undefined, elli_request:get_arg_decoded(<<"foo">>, Req2)),
     ?_assertMatch(Name, elli_request:get_arg_decoded(<<"name">>, Req2)),
     ?_assertMatch([], elli_request:post_args(Req2)),

     ?_assertMatch({error, not_supported}, elli_request:chunk_ref(#req{}))
    ].


%%% Integration tests
%%%   Use hackney to actually call Elli over the network.

hello_world() ->
    Response = hackney:get("http://localhost:3001/hello/world"),
    ?assertMatch(200, status(Response)),
    ?assertMatch([{<<"Connection">>, <<"Keep-Alive">>},
                  {<<"Content-Length">>, <<"12">>}], headers(Response)),
    ?assertMatch(<<"Hello World!">>, body(Response)),
    %% sizes
    ?assertMatch(63, get_size_value(resp_headers)),
    ?assertMatch(12, get_size_value(resp_body)),
    ?assertMatch(undefined, get_size_value(req_body)),

    %% timings
    ?assertNotMatch(undefined, get_timing_value(request_start)),
    ?assertNotMatch(undefined, get_timing_value(headers_start)),
    ?assertNotMatch(undefined, get_timing_value(headers_end)),
    ?assertNotMatch(undefined, get_timing_value(body_start)),
    ?assertNotMatch(undefined, get_timing_value(body_end)),
    ?assertNotMatch(undefined, get_timing_value(user_start)),
    ?assertNotMatch(undefined, get_timing_value(user_end)),
    ?assertNotMatch(undefined, get_timing_value(send_start)),
    ?assertNotMatch(undefined, get_timing_value(send_end)),
    ?assertNotMatch(undefined, get_timing_value(request_end)),
    %% check timings
    ?assert(?VTB(request_start, request_end, 1000000, 1200000)),
    ?assert(?VTB(headers_start, headers_end, 1, 100)),
    ?assert(?VTB(body_start, body_end, 1, 100)),
    ?assert(?VTB(user_start, user_end, 1000000, 1200000)),
    ?assert(?VTB(send_start, send_end, 1, 200)).


keep_alive_timings() ->

    Transport = hackney_tcp,
    Host = <<"localhost">>,
    Port = 3001,
    Options = [],
    {ok, ConnRef} = hackney:connect(Transport, Host, Port, Options),

    ReqBody = <<>>,
    ReqHeaders = [],
    ReqPath = <<"/hello/world">>,
    ReqMethod = get,
    Req = {ReqMethod, ReqPath, ReqHeaders, ReqBody},

    {ok, Status, Headers, HCRef} = hackney:send_request(ConnRef, Req),
    keep_alive_timings(Status, Headers, HCRef),

    %% pause between keep-alive requests,
    %% request_start is a timestamp of
    %% the first bytes of the second request
    timer:sleep(1000),

    {ok, Status, Headers, HCRef} = hackney:send_request(ConnRef, Req),
    keep_alive_timings(Status, Headers, HCRef),

    hackney:close(ConnRef).

keep_alive_timings(Status, Headers, HCRef) ->
    ?assertMatch(200, Status),
    ?assertMatch([{<<"Connection">>,<<"Keep-Alive">>},
                  {<<"Content-Length">>,<<"12">>}], Headers),
    ?assertMatch({ok, <<"Hello World!">>}, hackney:body(HCRef)),
    %% sizes
    ?assertMatch(63, get_size_value(resp_headers)),
    ?assertMatch(12, get_size_value(resp_body)),
    %% timings
    ?assertNotMatch(undefined, get_timing_value(request_start)),
    ?assertNotMatch(undefined, get_timing_value(headers_start)),
    ?assertNotMatch(undefined, get_timing_value(headers_end)),
    ?assertNotMatch(undefined, get_timing_value(body_start)),
    ?assertNotMatch(undefined, get_timing_value(body_end)),
    ?assertNotMatch(undefined, get_timing_value(user_start)),
    ?assertNotMatch(undefined, get_timing_value(user_end)),
    ?assertNotMatch(undefined, get_timing_value(send_start)),
    ?assertNotMatch(undefined, get_timing_value(send_end)),
    ?assertNotMatch(undefined, get_timing_value(request_end)),
    %% check timings
    ?assert(?VTB(request_start, request_end, 1000000, 1200000)),
    ?assert(?VTB(headers_start, headers_end, 1, 100)),
    ?assert(?VTB(body_start, body_end, 1, 100)),
    ?assert(?VTB(user_start, user_end, 1000000, 1200000)),
    ?assert(?VTB(send_start, send_end, 1, 200)).

not_found() ->
    Response = hackney:get("http://localhost:3001/foobarbaz"),
    ?assertMatch(404, status(Response)),
    ?assertMatch([{<<"Connection">>, <<"Keep-Alive">>},
                  {<<"Content-Length">>, <<"9">>}], headers(Response)),
    ?assertMatch(<<"Not Found">>, body(Response)).

crash() ->
    Response = hackney:get("http://localhost:3001/crash"),
    ?assertMatch(500, status(Response)),
    ?assertMatch([{<<"Connection">>, <<"Keep-Alive">>},
                  {<<"Content-Length">>, <<"21">>}], headers(Response)),
    ?assertMatch(<<"Internal server error">>, body(Response)).

invalid_return() ->
    %% Elli should return 500 for handlers returning bogus responses.
    Response = hackney:get("http://localhost:3001/invalid_return"),
    ?assertMatch(500, status(Response)),
    ?assertMatch([{<<"Connection">>, <<"Keep-Alive">>},
                  {<<"Content-Length">>, <<"21">>}], headers(Response)),
    ?assertMatch(<<"Internal server error">>, body(Response)).

no_compress() ->
    Response = hackney:get("http://localhost:3001/compressed"),
    ?assertMatch(200, status(Response)),
    ?assertMatch([{<<"Connection">>, <<"Keep-Alive">>},
                  {<<"Content-Length">>, <<"1032">>}], headers(Response)),
    ?assertEqual(binary:copy(<<"Hello World!">>, 86),
                 body(Response)).

compress(Encoding, Length) ->
    Response = hackney:get("http://localhost:3001/compressed",
                           [{<<"Accept-Encoding">>, Encoding}]),
    ?assertMatch(200, status(Response)),
    ?assertMatch([{<<"Connection">>, <<"Keep-Alive">>},
                  {<<"Content-Encoding">>, Encoding},
                  {<<"Content-Length">>, Length}], headers(Response)),
    ?assertEqual(binary:copy(<<"Hello World!">>, 86),
                 uncompress(Encoding, body(Response))).

uncompress(<<"gzip">>,    Data) -> zlib:gunzip(Data);
uncompress(<<"deflate">>, Data) -> zlib:uncompress(Data).

gzip() -> compress(<<"gzip">>, <<"41">>).

deflate() -> compress(<<"deflate">>, <<"29">>).

exception_flow() ->
    Response = hackney:get("http://localhost:3001/403"),
    ?assertMatch(403, status(Response)),
    ?assertMatch([{<<"Connection">>, <<"Keep-Alive">>},
                  {<<"Content-Length">>, <<"9">>}], headers(Response)),
    ?assertMatch(<<"Forbidden">>, body(Response)).

hello_iolist() ->
    Url      = "http://localhost:3001/hello/iolist?name=knut",
    Response = hackney:get(Url),
    ?assertMatch(<<"Hello knut">>, body(Response)).

accept_content_type() ->
    Json = hackney:get("http://localhost:3001/type?name=knut",
                       [{"Accept", "application/json"}]),
    ?assertMatch(<<"{\"name\" : \"knut\"}">>, body(Json)),
    Text = hackney:get("http://localhost:3001/type?name=knut",
                       [{"Accept", "text/plain"}]),
    ?assertMatch(<<"name: knut">>, body(Text)).

user_connection() ->
    Url      = "http://localhost:3001/user/defined/behaviour",
    Response = hackney:get(Url),
    ?assertMatch(304, status(Response)),
    ?assertMatch([{<<"Connection">>, <<"close">>},
                  {<<"Content-Length">>, <<"123">>}], headers(Response)),
    ?assertMatch(<<>>, body(Response)).


get_args() ->
    Response = hackney:get("http://localhost:3001/hello?name=knut"),
    ?assertMatch(<<"Hello knut">>, body(Response)).

decoded_get_args() ->
    Url      = "http://localhost:3001/decoded-hello?name=knut%3D",
    Response = hackney:get(Url),
    ?assertMatch(<<"Hello knut=">>, body(Response)).

decoded_get_args_list() ->
    Url      = "http://localhost:3001/decoded-list?name=knut%3D&foo",
    Response = hackney:get(Url),
    ?assertMatch(<<"Hello knut=">>, body(Response)).

post_args() ->
    Body        = <<"name=foo&city=New%20York">>,
    ContentType = <<"application/x-www-form-urlencoded">>,

    Response    = hackney:post("http://localhost:3001/hello",
                               [{<<"Content-Type">>, ContentType}],
                               Body),
    ?assertMatch(200, status(Response)),
    ?assertMatch(<<"Hello foo of New York">>, body(Response)).

shorthand() ->
    Response = hackney:get("http://localhost:3001/shorthand"),
    ?assertMatch(200, status(Response)),
    ?assertMatch([{<<"Connection">>, <<"Keep-Alive">>},
                  {<<"Content-Length">>, <<"5">>}], headers(Response)),
    ?assertMatch(<<"hello">>, body(Response)).

ip() ->
    Response = hackney:get("http://localhost:3001/ip"),
    ?assertMatch(200, status(Response)),
    ?assertMatch(<<"127.0.0.1">>, body(Response)).

found() ->
    Response = hackney:get("http://localhost:3001/302"),
    ?assertMatch(302, status(Response)),
    ?assertMatch([{<<"Connection">>, <<"Keep-Alive">>},
                  {<<"Content-Length">>, <<"0">>},
                  {<<"Location">>, <<"/hello/world">>}], headers(Response)),
    ?assertMatch(<<>>, body(Response)).

too_many_headers() ->
    Headers = lists:duplicate(100, {<<"X-Foo">>, <<"Bar">>}),
    Response = hackney:get("http://localhost:3001/foo", Headers),
    ?assertMatch(400, status(Response)).

too_big_body() ->
    Body = binary:copy(<<"x">>, (1024 * 1000) + 1),
    Response = hackney:post("http://localhost:3001/foo", [], Body),
    ?assertMatch(413, status(Response)).

way_too_big_body() ->
    Body = binary:copy(<<"x">>, (1024 * 2000) + 1),
    ?assertMatch({error, closed},
                 hackney:post("http://localhost:3001/foo", [], Body)).


bad_request_line() ->
    {ok, Socket} = gen_tcp:connect("127.0.0.1", 3001,
                                   [{active, false}, binary]),

    Req = <<"FOO BAR /hello HTTP/1.1\r\n">>,
    gen_tcp:send(Socket, <<Req/binary, Req/binary>>),
    ?assertMatch({ok, <<"HTTP/1.1 400 Bad Request\r\n"
                        "Content-Length: 11\r\n\r\nBad Request">>},
                 gen_tcp:recv(Socket, 0)).


content_length() ->
    Response = hackney:get("http://localhost:3001/304"),

    ?assertMatch(304, status(Response)),
    ?assertMatch([{<<"Connection">>, <<"Keep-Alive">>},
                  {<<"Content-Length">>, <<"7">>},
                  {<<"Etag">>, <<"foobar">>}], headers(Response)),
    ?assertMatch(<<>>, body(Response)).

user_content_length() ->
    Headers = <<"Foo: bar\n\n">>,
    Client  = start_slow_client(3001, "/user/content-length"),
    send(Client, Headers, 128),
    ?assertMatch({ok, <<"HTTP/1.1 200 OK\r\n"
                        "Connection: Keep-Alive\r\n"
                        "Content-Length: 123\r\n"
                        "\r\n"
                        "foobar">>},
                 gen_tcp:recv(Client, 0)).

headers() ->
    Response = hackney:get("http://localhost:3001/headers.html"),
    Headers = headers(Response),

    ?assert(proplists:is_defined(<<"X-Custom">>, Headers)),
    ?assertMatch(<<"foobar">>, proplists:get_value(<<"X-Custom">>, Headers)).

chunked() ->
    Expected = <<"chunk10chunk9chunk8chunk7chunk6chunk5chunk4chunk3chunk2chunk1">>,

    Response = hackney:get("http://localhost:3001/chunked"),

    ?assertMatch(200, status(Response)),
    ?assertEqual([{<<"Connection">>, <<"Keep-Alive">>},
                  {<<"Content-Type">>, <<"text/event-stream">>},
                  {<<"Transfer-Encoding">>, <<"chunked">>}], headers(Response)),
    ?assertMatch(Expected, body(Response)),
    %% sizes
    ?assertMatch(104, get_size_value(resp_headers)),
    ?assertMatch(111, get_size_value(chunks)),
    %% timings
    ?assertNotMatch(undefined, get_timing_value(request_start)),
    ?assertNotMatch(undefined, get_timing_value(headers_start)),
    ?assertNotMatch(undefined, get_timing_value(headers_end)),
    ?assertNotMatch(undefined, get_timing_value(body_start)),
    ?assertNotMatch(undefined, get_timing_value(body_end)),
    ?assertNotMatch(undefined, get_timing_value(user_start)),
    ?assertNotMatch(undefined, get_timing_value(user_end)),
    ?assertNotMatch(undefined, get_timing_value(send_start)),
    ?assertNotMatch(undefined, get_timing_value(send_end)),
    ?assertNotMatch(undefined, get_timing_value(request_end)).

sendfile() ->
    Response = hackney:get("http://localhost:3001/sendfile"),
    F              = ?README,
    {ok, Expected} = file:read_file(F),

    ?assertMatch(200, status(Response)),
    ?assertEqual([{<<"Connection">>, <<"Keep-Alive">>},
                  {<<"Content-Length">>, ?I2B(size(Expected))}],
                 headers(Response)),
    ?assertEqual(Expected, body(Response)),
    %% sizes
    ?assertEqual(size(Expected), get_size_value(file)),
    ?assertMatch(65, get_size_value(resp_headers)),
    %% timings
    ?assertNotMatch(undefined, get_timing_value(request_start)),
    ?assertNotMatch(undefined, get_timing_value(headers_start)),
    ?assertNotMatch(undefined, get_timing_value(headers_end)),
    ?assertNotMatch(undefined, get_timing_value(body_start)),
    ?assertNotMatch(undefined, get_timing_value(body_end)),
    ?assertNotMatch(undefined, get_timing_value(user_start)),
    ?assertNotMatch(undefined, get_timing_value(user_end)),
    ?assertNotMatch(undefined, get_timing_value(send_start)),
    ?assertNotMatch(undefined, get_timing_value(send_end)),
    ?assertNotMatch(undefined, get_timing_value(request_end)).

send_no_file() ->
    Response = hackney:get("http://localhost:3001/send_no_file"),

    ?assertMatch(500, status(Response)),
    ?assertMatch([{<<"Content-Length">>, <<"12">>}],
                 headers(Response)),
    ?assertMatch(<<"Server Error">>, body(Response)).

sendfile_error() ->
    Response = hackney:get("http://localhost:3001/sendfile/error"),

    ?assertMatch(500, status(Response)),
    ?assertMatch([{<<"Content-Length">>, <<"12">>}],
                 headers(Response)),
    ?assertMatch(<<"Server Error">>, body(Response)).

sendfile_range() ->
    Url            = "http://localhost:3001/sendfile/range",
    Headers        = [{"Range", "bytes=300-699"}],
    Response       = hackney:get(Url, Headers),
    F              = ?README,
    {ok, Fd}       = file:open(F, [read, raw, binary]),
    {ok, Expected} = file:pread(Fd, 300, 400),
    file:close(Fd),
    Size = elli_util:file_size(F),
    ?assertMatch(206, status(Response)),
    ?assertEqual([{<<"Connection">>, <<"Keep-Alive">>},
                  {<<"Content-Length">>, <<"400">>},
                  {<<"Content-Range">>, iolist_to_binary(["bytes 300-699/", ?I2L(Size)])}],
                 headers(Response)),
    ?assertEqual(Expected, body(Response)).

slow_client() ->
    Body    = <<"name=foobarbaz">>,
    Headers = <<"Content-Length: ", (?I2B(size(Body)))/binary, "\r\n\r\n">>,
    Client  = start_slow_client(3001, "/hello"),

    send(Client, Headers, 1),
    send(Client, Body, size(Body)),

    ?assertMatch({ok, <<"HTTP/1.1 200 OK\r\n"
                        "Connection: Keep-Alive\r\n"
                        "Content-Length: 15\r\n"
                        "\r\n"
                        "Hello undefined">>},
                 gen_tcp:recv(Client, 0)),
    %% check timings
    ?assert(?VTB(request_start, request_end, 30000, 70000)),
    ?assert(?VTB(headers_start, headers_end, 30000, 70000)),
    ?assert(?VTB(body_start, body_end, 1, 3000)),
    ?assert(?VTB(user_start, user_end, 1, 100)),
    ?assert(?VTB(send_start, send_end, 1, 200)).


post_pipeline() ->
    Body         = <<"name=elli&city=New%20York">>,
    Headers      = <<"Content-Length: ", (?I2B(size(Body)))/binary, "\r\n",
                     "Content-Type: application/x-www-form-urlencoded", "\r\n",
                     "\r\n">>,

    {ok, Socket} = gen_tcp:connect("127.0.0.1", 3001,
                                   [{active, false}, binary]),

    Req          = <<"POST /hello HTTP/1.1\r\n",
                     Headers/binary,
                     Body/binary>>,
    gen_tcp:send(Socket, <<Req/binary, Req/binary>>),

    ExpectedResponse = <<"HTTP/1.1 200 OK\r\n"
                         "Connection: Keep-Alive\r\n"
                         "Content-Length: 22\r\n"
                         "\r\n"
                         "Hello elli of New York">>,

    {ok, Res} = gen_tcp:recv(Socket, size(ExpectedResponse) * 2),

    Size = size(Body),
    ?assertMatch(Size, get_size_value(req_body)),
    ?assertEqual(binary:copy(ExpectedResponse, 2),
                 Res).

get_pipeline() ->
    Headers      = <<"User-Agent: sloow\r\n\r\n">>,
    Req          = <<"GET /hello?name=elli HTTP/1.1\r\n",
                     Headers/binary>>,
    {ok, Socket} = gen_tcp:connect("127.0.0.1", 3001,
                                   [{active, false}, binary]),
    gen_tcp:send(Socket, <<Req/binary, Req/binary>>),
    ExpectedResponse = <<"HTTP/1.1 200 OK\r\n"
                         "Connection: Keep-Alive\r\n"
                         "Content-Length: 10\r\n"
                         "\r\n"
                         "Hello elli">>,
    {ok, Res} = gen_tcp:recv(Socket, size(ExpectedResponse) * 2),
    case binary:copy(ExpectedResponse, 2) =:= Res of
        true ->
            ok;
        false ->
            ?LOG_INFO("Expected: ~p~nResult: ~p~n",
                      [binary:copy(ExpectedResponse, 2), Res])
    end,

    ?assertEqual(binary:copy(ExpectedResponse, 2),
                 Res).

head() ->
    Response = hackney:head("http://localhost:3001/head"),
    ?assertMatch(200, status(Response)),
    ?assertMatch([{<<"Connection">>, <<"Keep-Alive">>},
                  {<<"Content-Length">>, <<"20">>}], headers(Response)).

no_body() ->
    Response = hackney:get("http://localhost:3001/304"),
    ?assertMatch(304, status(Response)),
    ?assertMatch([{<<"Connection">>, <<"Keep-Alive">>},
                  {<<"Content-Length">>, <<"7">>},
                  {<<"Etag">>, <<"foobar">>}], headers(Response)),
    ?assertMatch(<<>>, body(Response)).

sends_continue() ->
    {ok, Socket} = gen_tcp:connect("127.0.0.1", 3001, [{active, false}, binary]),

    Body = <<"name=elli&city=New%20York">>,
    Length = ?I2B(size(Body)),

    Req = <<"POST /hello HTTP/1.1\r\n",
            "Host: localhost\r\n",
            "Content-Type: application/x-www-form-urlencoded\r\n",
            "Content-Length: ", Length/binary, "\r\n",
            "Expect: 100-continue\r\n\r\n">>,

    gen_tcp:send(Socket, Req),
    ?assertMatch({ok, <<"HTTP/1.1 100 Continue\r\n"
                        "Content-Length: 0\r\n\r\n">>},
                 gen_tcp:recv(Socket, 0)),
    % Send Result of the body
    gen_tcp:send(Socket, Body),
    ExpectedResponse = <<"HTTP/1.1 200 OK\r\n"
                         "Connection: Keep-Alive\r\n"
                         "Content-Length: 22\r\n"
                         "\r\n"
                         "Hello elli of New York">>,
    ?assertMatch({ok, ExpectedResponse},
                 gen_tcp:recv(Socket, size(ExpectedResponse))).

%%% Slow client, sending only the specified byte size every millisecond

start_slow_client(Port, Url) ->
    case gen_tcp:connect("127.0.0.1", Port, [{active, false}, binary]) of
        {ok, Socket} ->
            gen_tcp:send(Socket, "GET " ++ Url ++ " HTTP/1.1\r\n"),
            Socket;
        {error, Reason} ->
            throw({slow_client_error, Reason})
    end.

send(_Socket, <<>>, _) ->
    ok;
send(Socket, B, ChunkSize) ->
    {Part, Rest} = case B of
                       <<P:ChunkSize/binary, R/binary>> -> {P, R};
                       P -> {P, <<>>}
                   end,
    %%?LOG_INFO("~p~n", [Part]),
    gen_tcp:send(Socket, Part),
    timer:sleep(1),
    send(Socket, Rest, ChunkSize).

%%% Unit tests

body_qs_test() ->
    Expected = [{<<"foo">>, <<"bar">>},
                {<<"baz">>, <<"bang">>},
                {<<"found">>, true}],
    Body     = <<"foo=bar&baz=bang&found">>,
    Headers  = [{<<"Content-Type">>, <<"application/x-www-form-urlencoded">>}],
    ?assertMatch(Expected, elli_request:body_qs(#req{body = Body,
                                                     headers = Headers})).

to_proplist_test() ->
    Req  = #req{method   = 'GET',
                path     = [<<"crash">>],
                args     = [],
                version  = {1, 1},
                raw_path = <<"/crash">>,
                headers  = [{<<"Host">>, <<"localhost:3001">>}],
                body     = <<>>,
                pid      = self(),
                socket   = socket,
                callback = {mod, []}},

    Prop = [{method,   'GET'},
            {scheme,   undefined},
            {host,     undefined},
            {port,     undefined},
            {path,     [<<"crash">>]},
            {args,     []},
            {raw_path, <<"/crash">>},
            {version,  {1, 1}},
            {headers,  [{<<"Host">>, <<"localhost:3001">>}]},
            {body,     <<>>},
            {pid,      self()},
            {socket,   socket},
            {callback, {mod, []}}],
    ?assertEqual(Prop, elli_request:to_proplist(Req)).

is_request_test() ->
    ?assert(elli_request:is_request(#req{})),
    ?assertNot(elli_request:is_request({req, foobar})).


query_str_test_() ->
    MakeReq = fun(Path) -> #req{raw_path = Path} end,
    [
     %% For empty query strings, expect `query_str` to return an empty binary.
     ?_assertMatch(<<>>, elli_request:query_str(MakeReq(<<"/foo">>))),
     ?_assertMatch(<<>>, elli_request:query_str(MakeReq(<<"/foo?">>))),
     %% Otherwise it should return everything to the right hand side of `?`.
     ?_assertMatch(<<"bar=baz&baz=bang">>,
                   elli_request:query_str(MakeReq(<<"/foo?bar=baz&baz=bang">>)))
    ].


get_range_test_() ->
    Req       = #req{headers = [{<<"Range">>,
                                 <<"bytes=0-99 ,500-999 , -800">>}]},
    OffsetReq = #req{headers = [{<<"Range">>, <<"bytes=200-">>}]},
    UndefReq  = #req{headers = []},
    BadReq    = #req{headers = [{<<"Range">>, <<"bytes=--99,hallo-world">>}]},

    ByteRangeSet = [{bytes, 0, 99}, {bytes, 500, 999}, {suffix, 800}],

    [?_assertMatch(ByteRangeSet,    elli_request:get_range(Req)),
     ?_assertMatch([{offset, 200}], elli_request:get_range(OffsetReq)),
     ?_assertMatch([],              elli_request:get_range(UndefReq)),
     ?_assertMatch(parse_error,     elli_request:get_range(BadReq))].

normalize_range_test_() ->
    Size     = 1000,

    Bytes1   = {bytes, 200, 400},
    Bytes2   = {bytes, 0, 1000000},
    Suffix   = {suffix, 303},
    Offset   = {offset, 42},
    Normal   = {200, 400},
    Set      = [{bytes, 0, 999}],
    EmptySet = [],
    Invalid1 = {bytes, 400, 200},
    Invalid2 = {bytes, 1200, 2000},
    Invalid3 = {offset, -10},
    Invalid4 = {offset, 2000},
    Invalid5 = parse_error,
    Invalid6 = [{bytes, 0, 100}, {suffix, 42}],

    [?_assertMatch({200, 201},        elli_util:normalize_range(Bytes1, Size)),
     ?_assertMatch({0, Size},         elli_util:normalize_range(Bytes2, Size)),
     ?_assertEqual({Size - 303, 303}, elli_util:normalize_range(Suffix, Size)),
     ?_assertEqual({42, Size - 42},   elli_util:normalize_range(Offset, Size)),
     ?_assertMatch({200, 400},        elli_util:normalize_range(Normal, Size)),
     ?_assertMatch({0, 1000},         elli_util:normalize_range(Set, Size)),
     ?_assertMatch(undefined,     elli_util:normalize_range(EmptySet, Size)),
     ?_assertMatch(invalid_range, elli_util:normalize_range(Invalid1, Size)),
     ?_assertMatch(invalid_range, elli_util:normalize_range(Invalid2, Size)),
     ?_assertMatch(invalid_range, elli_util:normalize_range(Invalid3, Size)),
     ?_assertMatch(invalid_range, elli_util:normalize_range(Invalid4, Size)),
     ?_assertMatch(invalid_range, elli_util:normalize_range(Invalid5, Size)),
     ?_assertMatch(invalid_range, elli_util:normalize_range(Invalid6, Size))].


encode_range_test() ->
    Expected = [<<"bytes ">>,<<"*">>,<<"/">>,"42"],
    ?assertMatch(Expected, elli_util:encode_range(invalid_range, 42)).

register_test() ->
    ?assertMatch(undefined, whereis(elli)),
    Config = [
              {name, {local, elli}},
              {callback, elli_middleware},
              {callback_args, [{mods, [
                                       elli_example_callback,
                                       elli_metrics_middleware
                                      ]}]}
             ],
    {ok, Pid} = elli:start_link(Config),
    ?assertMatch(Pid, whereis(elli)),
    ok.

invalid_callback_test() ->
    case catch elli:start_link([{callback, elli}]) of
        E ->
            ?assertMatch(invalid_callback, E)
    end.
