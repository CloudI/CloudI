%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
-module(cloudi_service_oauth1_SUITE).
-behaviour(cloudi_service).

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/3]).

%% CT callbacks
-export([all/0,
         groups/0,
         suite/0,
         init_per_suite/1,
         end_per_suite/1,
         group/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% test callbacks
-export([t_example_without_db_1/1,
         t_example_without_db_2/1,
         t_example_with_db_1/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("cloudi_core/include/cloudi_logger.hrl").

-ifndef(CLOUDI_LONG_TEST_TIMEOUT).
-define(CLOUDI_LONG_TEST_TIMEOUT, 60). % minutes
-endif.
-define(DEFAULT_PGSQL_HOST, "127.0.0.1").
-define(DEFAULT_PGSQL_PORT, 5432).
-define(DEFAULT_RIAK_HOST, "127.0.0.1").
-define(DEFAULT_RIAK_PORT, 8087).
-define(TIMEOUT, (?CLOUDI_LONG_TEST_TIMEOUT * 60000)). % milliseconds

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, _Prefix, _Timeout, Dispatcher) ->
    Defaults = [
        {mode,                             undefined}],
    [Mode] = cloudi_proplists:take_values(Defaults, Args),
    if
        Mode =:= example ->
            cloudi_service:subscribe(Dispatcher, "photos/get")
    end,
    {ok, Mode}.

cloudi_service_handle_request(_Type, _Name, _Pattern, _RequestInfo, _Request,
                              _Timeout, _Priority, _TransId, _Pid,
                              example = State, _Dispatcher) ->
    ResponseInfo = [
        {<<"content-type">>, <<"image/jpeg">>},
        {<<"content-disposition">>,
         <<"attachment; filename=\"vacation.jpg\"">>}],
    Response = <<"PHOTO_DATA">>,
    {reply, ResponseInfo, Response, State}.

cloudi_service_handle_info(Request, State, _Dispatcher) ->
    {stop, cloudi_string:format("Unknown info \"~w\"", [Request]), State}.

cloudi_service_terminate(_Reason, _Timeout, _State) ->
    ok.

%%%------------------------------------------------------------------------
%%% Callback functions from CT
%%%------------------------------------------------------------------------

all() ->
    test_conditions_merge([%test_condition_riak([{group, riak}]),
                           test_condition_pgsql([{group, pgsql}])]).

groups() ->
    [{pgsql, [],
      [{group, example_without_db},
       {group, example_with_db}]},
     %{riak, [],
     % [{group, example_without_db},
     %  {group, example_with_db}]},
     {example_without_db, [],
      [t_example_without_db_1,
       t_example_without_db_2]},
     {example_with_db, [],
      [t_example_with_db_1]}].

suite() ->
    [{ct_hooks, [cth_surefire]},
     {timetrap, {minutes, ?CLOUDI_LONG_TEST_TIMEOUT}}].

init_per_suite(Config) ->
    ok = cloudi_x_reltool_util:application_start(cloudi_core, [], infinity),
    Config.

end_per_suite(_Config) ->
    ok = cloudi_x_reltool_util:application_stop(cloudi_core),
    ok.

group(_GroupName) ->
    [].

init_per_group(pgsql, Config) ->
    [{db, pgsql} | lists:keydelete(db, 1, Config)];
init_per_group(riak, Config) ->
    [{db, riak} | lists:keydelete(db, 1, Config)];
init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(t_example_without_db_1, Config) ->
    DBType = db_type(Config),
    {ok, ServiceIds} = cloudi_service_api:services_add([
        db_service(DBType),
        {internal,
            "/",
            ?MODULE,
            [{mode, example}],
            immediate_closest,
            ?TIMEOUT, ?TIMEOUT, ?TIMEOUT,
            undefined, undefined, 1, 5, 300,
            [{automatic_loading, false}]},
        {internal,
            "/",
            cloudi_service_oauth1,
            [{database_type, DBType},
             {database, db_service_name(DBType)},
             {url_host, "https://photos.example.net"},
             {debug, true}],
            immediate_closest,
            ?TIMEOUT, ?TIMEOUT, ?TIMEOUT,
            undefined, undefined, 1, 5, 300, []}
        ], infinity),
    [{service_ids, ServiceIds} | Config];
init_per_testcase(t_example_without_db_2, Config) ->
    DBType = db_type(Config),
    {ok, ServiceIds} = cloudi_service_api:services_add([
        db_service(DBType),
        {internal,
            "/",
            ?MODULE,
            [{mode, example}],
            immediate_closest,
            ?TIMEOUT, ?TIMEOUT, ?TIMEOUT,
            undefined, undefined, 1, 5, 300,
            [{automatic_loading, false}]},
        {internal,
            "/",
            cloudi_service_oauth1,
            [{database_type, DBType},
             {database, db_service_name(DBType)},
             {url_host, "http://photos.example.net"},
             {debug, true}],
            immediate_closest,
            ?TIMEOUT, ?TIMEOUT, ?TIMEOUT,
            undefined, undefined, 1, 5, 300, []}
        ], infinity),
    [{service_ids, ServiceIds} | Config];
init_per_testcase(t_example_with_db_1, Config) ->
    DBType = db_type(Config),
    {ok, ServiceIds} = cloudi_service_api:services_add([
        db_service(DBType),
        {internal,
            "/",
            ?MODULE,
            [{mode, example}],
            immediate_closest,
            ?TIMEOUT, ?TIMEOUT, ?TIMEOUT,
            undefined, undefined, 1, 5, 300,
            [{automatic_loading, false}]},
        {internal,
            "/",
            cloudi_service_oauth1,
            [{database_type, DBType},
             {database, db_service_name(DBType)},
             {url_host, "https://photos.example.net"},
             {tokens_clean, 1},
             {token_request_expiration, 1},
             {token_access_expiration, 1},
             {debug_db, true}],
            immediate_closest,
            ?TIMEOUT, ?TIMEOUT, ?TIMEOUT,
            undefined, undefined, 1, 5, 300, []}
        ], infinity),
    [{service_ids, ServiceIds} | Config].

end_per_testcase(_TestCase, Config) ->
    {value, {_, ServiceIds}, NewConfig} = lists:keytake(service_ids, 1, Config),
    ok = cloudi_service_api:services_remove(ServiceIds, infinity),
    NewConfig.

%%%------------------------------------------------------------------------
%%% test cases
%%%------------------------------------------------------------------------

t_example_without_db_1(_Config) ->
    % OAuth requests based on http://tools.ietf.org/html/rfc5849#section-1.2
    Context0 = cloudi:new(),

    % (HTTPS from http://tools.ietf.org/html/rfc5849#section-1.2 #1)
    % POST /initiate HTTP/1.1
    % Host: photos.example.net
    % Authorization: OAuth realm="Photos",
    %    oauth_consumer_key="dpf43f3p2l4k3l03",
    %    oauth_signature_method="HMAC-SHA1",
    %    oauth_timestamp="137131200",
    %    oauth_nonce="wIjqoS",
    %    oauth_callback="http%3A%2F%2Fprinter.example.com%2Fready",
    %    oauth_signature="74KNZJeDHnMBp0EMJ9ZHt%2FXKycU%3D"
    Name1 = "/initiate/post",
    RequestInfo1 = [
        {<<"url-path">>, <<"/initiate">>}, % <- from cloudi_service_http_cowboy1
        {<<"host">>, <<"photos.example.net">>},
        {<<"authorization">>,
         <<"OAuth realm=\"Photos\","
               "oauth_consumer_key=\"dpf43f3p2l4k3l03\","
               "oauth_signature_method=\"HMAC-SHA1\","
               "oauth_timestamp=\"137131200\","
               "oauth_nonce=\"wIjqoS\","
               "oauth_callback=\"http%3A%2F%2Fprinter.example.com%2Fready\","
               "oauth_signature=\"74KNZJeDHnMBp0EMJ9ZHt%2FXKycU%3D\"">>}],
    Request1 = <<>>,

    % HTTP/1.1 200 OK
    % Content-Type: application/x-www-form-urlencoded
    %
    % oauth_token=hh5s93j4hdidpola&oauth_token_secret=hdhd0244k9j7ao03&
    % oauth_callback_confirmed=true
    ResponseInfo1 = [
        {<<"content-type">>, <<"application/x-www-form-urlencoded">>}],
    Response1 = <<"oauth_token=hh5s93j4hdidpola&"
                  "oauth_token_secret=hdhd0244k9j7ao03&"
                  "oauth_callback_confirmed=true">>,
    {{ok, ResponseInfo1, Response1},
     Context1} = cloudi:send_sync(Context0, Name1,
                                  RequestInfo1, Request1,
                                  undefined, undefined),

    % (HTTPS from http://tools.ietf.org/html/rfc5849#section-1.2 #2)
    % https://photos.example.net/authorize?oauth_token=hh5s93j4hdidpola
    Name2 = "/authorize/get",
    RequestInfo2 = [
        {<<"url-path">>, <<"/authorize">>}, % <- from cloudi_service_http_cowboy1
        {<<"host">>, <<"photos.example.net">>}],
    Request2 = [{<<"oauth_token">>, <<"hh5s93j4hdidpola">>}],

    % http://printer.example.com/ready?
    % oauth_token=hh5s93j4hdidpola&oauth_verifier=hfdp7dh39dks9884
    ResponseInfo2 = [
        {<<"status">>, <<"302">>},
        {<<"location">>,
         <<"http://printer.example.com/ready?"
           "oauth_token=hh5s93j4hdidpola&oauth_verifier=hfdp7dh39dks9884">>}],
    Response2 = <<>>,
    {{ok, ResponseInfo2, Response2},
     Context2} = cloudi:send_sync(Context1, Name2,
                                  RequestInfo2, Request2,
                                  undefined, undefined),

    % (HTTPS from http://tools.ietf.org/html/rfc5849#section-1.2 #3)
    % POST /token HTTP/1.1
    % Host: photos.example.net
    % Authorization: OAuth realm="Photos",
    %    oauth_consumer_key="dpf43f3p2l4k3l03",
    %    oauth_token="hh5s93j4hdidpola",
    %    oauth_signature_method="HMAC-SHA1",
    %    oauth_timestamp="137131201",
    %    oauth_nonce="walatlh",
    %    oauth_verifier="hfdp7dh39dks9884",
    %    oauth_signature="gKgrFCywp7rO0OXSjdot%2FIHF7IU%3D"
    Name3 = "/token/post",
    RequestInfo3 = [
        {<<"url-path">>, <<"/token">>}, % <- from cloudi_service_http_cowboy1
        {<<"host">>, <<"photos.example.net">>},
        {<<"authorization">>,
         <<"OAuth realm=\"Photos\","
               "oauth_consumer_key=\"dpf43f3p2l4k3l03\","
               "oauth_token=\"hh5s93j4hdidpola\","
               "oauth_signature_method=\"HMAC-SHA1\","
               "oauth_timestamp=\"137131201\","
               "oauth_nonce=\"walatlh\","
               "oauth_verifier=\"hfdp7dh39dks9884\","
               "oauth_signature=\"gKgrFCywp7rO0OXSjdot%2FIHF7IU%3D\"">>}],
    Request3 = <<>>,

    % HTTP/1.1 200 OK
    % Content-Type: application/x-www-form-urlencoded
    %
    % oauth_token=nnch734d00sl2jdk&oauth_token_secret=pfkkdhi9sl3r4s00
    ResponseInfo3 = [
        {<<"content-type">>, <<"application/x-www-form-urlencoded">>}],
    Response3 = <<"oauth_token=nnch734d00sl2jdk&"
                  "oauth_token_secret=pfkkdhi9sl3r4s00">>,
    {{ok, ResponseInfo3, Response3},
     _} = cloudi:send_sync(Context2, Name3,
                           RequestInfo3, Request3,
                           undefined, undefined),
    ok.

t_example_without_db_2(_Config) ->
    % OAuth requests based on http://tools.ietf.org/html/rfc5849#section-1.2
    Context0 = cloudi:new(),

    % (HTTP from http://tools.ietf.org/html/rfc5849#section-1.2 #4)
    % GET /photos?file=vacation.jpg&size=original HTTP/1.1
    % Host: photos.example.net
    % Authorization: OAuth realm="Photos",
    %    oauth_consumer_key="dpf43f3p2l4k3l03",
    %    oauth_token="nnch734d00sl2jdk",
    %    oauth_signature_method="HMAC-SHA1",
    %    oauth_timestamp="137131202",
    %    oauth_nonce="chapoH",
    %    oauth_signature="MdpQcU8iPSUjWoN%2FUDMsK2sui9I%3D"
    Name4 = "/verify/photos/get",
    RequestInfo4 = [
        {<<"url-path">>, <<"/photos">>}, % <- matches example URL and signature
        {<<"host">>, <<"photos.example.net">>},
        {<<"authorization">>,
         <<"OAuth realm=\"Photos\","
               "oauth_consumer_key=\"dpf43f3p2l4k3l03\","
               "oauth_token=\"nnch734d00sl2jdk\","
               "oauth_signature_method=\"HMAC-SHA1\","
               "oauth_timestamp=\"137131202\","
               "oauth_nonce=\"chapoH\","
               "oauth_signature=\"MdpQcU8iPSUjWoN%2FUDMsK2sui9I%3D\"">>}],
    Request4 = [{<<"file">>, <<"vacation.jpg">>},
                {<<"size">>, <<"original">>}],
    ResponseInfo4 = [
        {<<"content-type">>, <<"image/jpeg">>},
        {<<"content-disposition">>,
         <<"attachment; filename=\"vacation.jpg\"">>}],
    Response4 = <<"PHOTO_DATA">>,
    {{ok, ResponseInfo4, Response4},
     _} = cloudi:send_sync(Context0, Name4,
                           RequestInfo4, Request4,
                           undefined, undefined),
    ok.

t_example_with_db_1(_Config) ->
    % OAuth requests based on http://tools.ietf.org/html/rfc5849#section-1.2
    Context0 = cloudi:new(),

    Consumer = {"dpf43f3p2l4k3l03", "kd94hf93k423kf44", hmac_sha1},

    % (HTTPS from http://tools.ietf.org/html/rfc5849#section-1.2 #1)
    % POST /initiate HTTP/1.1
    % Host: photos.example.net
    % Authorization: OAuth realm="Photos",
    %    oauth_consumer_key="dpf43f3p2l4k3l03",
    %    oauth_signature_method="HMAC-SHA1",
    %    oauth_timestamp="137131200",
    %    oauth_nonce="wIjqoS",
    %    oauth_callback="http%3A%2F%2Fprinter.example.com%2Fready",
    %    oauth_signature="74KNZJeDHnMBp0EMJ9ZHt%2FXKycU%3D"
    Name1 = "/initiate/post",
    RequestInfo1 = [
        {<<"url-path">>, <<"/initiate">>}, % <- from cloudi_service_http_cowboy1
        {<<"host">>, <<"photos.example.net">>},
        {<<"authorization">>,
         <<"OAuth realm=\"Photos\","
               "oauth_consumer_key=\"dpf43f3p2l4k3l03\","
               "oauth_signature_method=\"HMAC-SHA1\","
               "oauth_timestamp=\"137131200\","
               "oauth_nonce=\"wIjqoS\","
               "oauth_callback=\"http%3A%2F%2Fprinter.example.com%2Fready\","
               "oauth_signature=\"74KNZJeDHnMBp0EMJ9ZHt%2FXKycU%3D\"">>}],
    Request1 = <<>>,

    % HTTP/1.1 200 OK
    % Content-Type: application/x-www-form-urlencoded
    %
    % oauth_token=hh5s93j4hdidpola&oauth_token_secret=hdhd0244k9j7ao03&
    % oauth_callback_confirmed=true
    ResponseInfo1 = [
        {<<"content-type">>, <<"application/x-www-form-urlencoded">>}],
    {{ok, ResponseInfo1, Response1},
     Context1} = cloudi:send_sync(Context0, Name1,
                                  RequestInfo1, Request1,
                                  undefined, undefined),
    [TokenRequest, TokenRequestSecret] = token_data(Response1),

    % (HTTPS from http://tools.ietf.org/html/rfc5849#section-1.2 #2)
    % https://photos.example.net/authorize?oauth_token=hh5s93j4hdidpola
    Name2 = "/authorize/get",
    RequestInfo2 = [
        {<<"url-path">>, <<"/authorize">>}, % <- from cloudi_service_http_cowboy1
        {<<"host">>, <<"photos.example.net">>}],
    Request2 = [{<<"oauth_token">>, TokenRequest}],

    % http://printer.example.com/ready?
    % oauth_token=hh5s93j4hdidpola&oauth_verifier=hfdp7dh39dks9884
    Response2 = <<>>,
    {{ok, ResponseInfo2, Response2},
     Context2} = cloudi:send_sync(Context1, Name2,
                                  RequestInfo2, Request2,
                                  undefined, undefined),
    [{<<"status">>, <<"302">>},
     {<<"location">>, AuthorizeLocation}] = ResponseInfo2,
    Verifier = location_verifier(AuthorizeLocation,
                                 <<"http://printer.example.com/ready">>,
                                 TokenRequest),

    Signature3 = erlang:list_to_binary(cloudi_service_oauth1_data:signature(
        "POST", "https://photos.example.net/token",
        [{"oauth_consumer_key", "dpf43f3p2l4k3l03"},
         {"oauth_token", erlang:binary_to_list(TokenRequest)},
         {"oauth_signature_method", "HMAC-SHA1"},
         {"oauth_timestamp", "137131201"},
         {"oauth_nonce", "walatlh"},
         {"oauth_verifier", erlang:binary_to_list(Verifier)}],
        Consumer, erlang:binary_to_list(TokenRequestSecret))),
    ?LOG_INFO("token (signature=~s,token_request=~s,token_request_secret=~s,"
              "verifier=~s)",
              [Signature3, TokenRequest, TokenRequestSecret, Verifier]),

    % (HTTPS from http://tools.ietf.org/html/rfc5849#section-1.2 #3)
    % POST /token HTTP/1.1
    % Host: photos.example.net
    % Authorization: OAuth realm="Photos",
    %    oauth_consumer_key="dpf43f3p2l4k3l03",
    %    oauth_token="hh5s93j4hdidpola",
    %    oauth_signature_method="HMAC-SHA1",
    %    oauth_timestamp="137131201",
    %    oauth_nonce="walatlh",
    %    oauth_verifier="hfdp7dh39dks9884",
    %    oauth_signature="gKgrFCywp7rO0OXSjdot%2FIHF7IU%3D"
    Name3 = "/token/post",
    RequestInfo3 = [
        {<<"url-path">>, <<"/token">>}, % <- from cloudi_service_http_cowboy1
        {<<"host">>, <<"photos.example.net">>},
        {<<"authorization">>,
         <<(<<"OAuth realm=\"Photos\","
               "oauth_consumer_key=\"dpf43f3p2l4k3l03\","
               "oauth_token=\"">>)/binary, TokenRequest/binary, (<<"\","
               "oauth_signature_method=\"HMAC-SHA1\","
               "oauth_timestamp=\"137131201\","
               "oauth_nonce=\"walatlh\","
               "oauth_verifier=\"">>)/binary, Verifier/binary, (<<"\","
               "oauth_signature=\"">>)/binary, Signature3/binary, (<<"\""
              "">>)/binary>>}],
    Request3 = <<>>,

    % HTTP/1.1 200 OK
    % Content-Type: application/x-www-form-urlencoded
    %
    % oauth_token=nnch734d00sl2jdk&oauth_token_secret=pfkkdhi9sl3r4s00
    ResponseInfo3 = [
        {<<"content-type">>, <<"application/x-www-form-urlencoded">>}],
    {{ok, ResponseInfo3, Response3},
     Context3} = cloudi:send_sync(Context2, Name3,
                                  RequestInfo3, Request3,
                                  undefined, undefined),
    [TokenAccess, TokenAccessSecret] = token_data(Response3),

    Signature4 = erlang:list_to_binary(cloudi_service_oauth1_data:signature(
        "GET",
        "https://photos.example.net/verify/photos"
        "?file=vacation.jpg&size=original",
        [{"file", "vacation.jpg"},
         {"size", "original"},
         {"oauth_consumer_key", "dpf43f3p2l4k3l03"},
         {"oauth_token", erlang:binary_to_list(TokenAccess)},
         {"oauth_signature_method", "HMAC-SHA1"},
         {"oauth_timestamp", "137131202"},
         {"oauth_nonce", "chapoH"}],
        Consumer, erlang:binary_to_list(TokenAccessSecret))),
    ?LOG_INFO("verify* (signature=~s,token_access=~s,token_access_secret=~s)",
              [Signature4, TokenAccess, TokenAccessSecret]),

    % (HTTPS instead of HTTP
    %  from http://tools.ietf.org/html/rfc5849#section-1.2 #4)
    % GET /photos?file=vacation.jpg&size=original HTTP/1.1
    % Host: photos.example.net
    % Authorization: OAuth realm="Photos",
    %    oauth_consumer_key="dpf43f3p2l4k3l03",
    %    oauth_token="nnch734d00sl2jdk",
    %    oauth_signature_method="HMAC-SHA1",
    %    oauth_timestamp="137131202",
    %    oauth_nonce="chapoH",
    %    oauth_signature="MdpQcU8iPSUjWoN%2FUDMsK2sui9I%3D"
    Name4 = "/verify/photos/get",
    RequestInfo4 = [
        {<<"url-path">>,
         <<"/verify/photos">>}, % <- from cloudi_service_http_cowboy1
        {<<"host">>, <<"photos.example.net">>},
        {<<"authorization">>,
         <<(<<"OAuth realm=\"Photos\","
               "oauth_consumer_key=\"dpf43f3p2l4k3l03\","
               "oauth_token=\"">>)/binary, TokenAccess/binary, (<<"\","
               "oauth_signature_method=\"HMAC-SHA1\","
               "oauth_timestamp=\"137131202\","
               "oauth_nonce=\"chapoH\","
               "oauth_signature=\"">>)/binary, Signature4/binary, (<<"\""
              "">>)/binary>>}],
    Request4 = [{<<"file">>, <<"vacation.jpg">>},
                {<<"size">>, <<"original">>}],
    ResponseInfo4 = [
        {<<"content-type">>, <<"image/jpeg">>},
        {<<"content-disposition">>,
         <<"attachment; filename=\"vacation.jpg\"">>}],
    Response4 = <<"PHOTO_DATA">>,
    {{ok, ResponseInfo4, Response4},
     Context4} = cloudi:send_sync(Context3, Name4,
                                  RequestInfo4, Request4,
                                  undefined, undefined),

    timer:sleep(3000), % wait for the access token to expire
    ResponseInfo5 = [{<<"status">>,<<"401">>},
                     {<<"www-authenticate">>,<<"OAuth">>}],
    Response5 = <<>>,
    {{ok, ResponseInfo5, Response5},
     _} = cloudi:send_sync(Context4, Name4,
                           RequestInfo4, Request4,
                           undefined, undefined),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

db_type(Config) ->
    {db, DB} = lists:keyfind(db, 1, Config),
    DB.

db_service_name(pgsql) ->
    "/oauth_db/cloudi_tests";
db_service_name(riak) ->
    "/oauth_db/cloudi_tests/".

db_service(pgsql) ->
    {internal,
        "/oauth_db/",
        cloudi_service_db_pgsql,
        [{driver, semiocast}, % semiocast | wg
         {output, internal},
         {internal_interface, common},
         {hostname, "127.0.0.1"},
         {port, 5432},
         {username, "cloudi_tests"},
         {password, "cloudi_tests"},
         {database, "cloudi_tests"},
         {timeout, 30000}, % milliseconds
         {debug, true}],
        none,
        5000, 5000, 5000, undefined, undefined, 1, 5, 300, []};
db_service(riak) ->
    {internal,
        "/oauth_db/cloudi_tests/",
        cloudi_service_db_riak,
        [{debug, true}],
        none,
        5000, 5000, 5000, undefined, undefined, 1, 5, 300, []}.

token_data(Response) ->
    [<<"oauth_token=", Token/binary>>,
     <<"oauth_token_secret=", TokenSecret/binary>> |
     Rest] = binary:split(Response, <<"&">>, [global]),
    case Rest of
        [] ->
            ok;
        [<<"oauth_callback_confirmed=true">>] ->
            ok
    end,
    [Token, TokenSecret].

location_verifier(Location, Callback, TokenRequest) ->
    CallbackString = erlang:iolist_to_binary([Callback, "?oauth_token=",
                                              TokenRequest]),
    [CallbackString,
     <<"oauth_verifier=", Verifier/binary>>] = binary:split(Location, <<"&">>),
    Verifier.

test_conditions_merge(L) ->
    case test_conditions_merge(L, []) of
        [] ->
            {skip, L};
        [_ | _] = NewL ->
            NewL
    end.

test_conditions_merge([], Output) ->
    lists:reverse(Output);
test_conditions_merge([{skip, _} | L], Output) ->
    test_conditions_merge(L, Output);
test_conditions_merge([Group | L], Output) ->
    test_conditions_merge(L, [Group | Output]).

test_condition_db(_, _, _, _, 0) ->
    {skip, long_tests_disabled};
test_condition_db(L, Host, Port, ErrorReason, LongTestTimeout)
    when LongTestTimeout > 0 ->
    case gen_tcp:connect(Host, Port, []) of
        {ok, Socket} ->
            catch gen_tcp:close(Socket),
            L;
        {error, econnrefused} ->
            error_logger:error_msg("unable to test ~p",
                                   [{Host, Port}]),
            {skip, ErrorReason};
        {error, Reason} ->
            error_logger:error_msg("unable to test ~p: ~p",
                                   [{Host, Port}, Reason]),
            {skip, ErrorReason}
    end.

test_condition_pgsql(L) ->
    test_condition_db(L, ?DEFAULT_PGSQL_HOST, ?DEFAULT_PGSQL_PORT,
                      pgsql_dead, ?CLOUDI_LONG_TEST_TIMEOUT).

test_condition_riak(L) ->
    test_condition_db(L, ?DEFAULT_RIAK_HOST, ?DEFAULT_RIAK_PORT,
                      riak_dead, ?CLOUDI_LONG_TEST_TIMEOUT).

