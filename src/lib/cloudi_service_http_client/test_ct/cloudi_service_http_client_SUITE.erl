%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
-module(cloudi_service_http_client_SUITE).

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
-export([t_cowboy1_client_basic_1/1,
         t_cowboy2_client_basic_1/1,
         t_elli_client_basic_1/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("cloudi_core/include/cloudi_logger.hrl").

-ifndef(CLOUDI_TEST_TIMEOUT).
-define(CLOUDI_TEST_TIMEOUT, 10). % seconds
-endif.
-define(SERVICE_PREFIX1, "/client/").

%%%------------------------------------------------------------------------
%%% Callback functions from CT
%%%------------------------------------------------------------------------

all() ->
    [{group, inets},
     {group, hackney}].

groups() ->
    [{inets, [xequence],
      [t_cowboy1_client_basic_1,
       t_cowboy2_client_basic_1,
       t_elli_client_basic_1]},
     {hackney, [sequence],
      [t_cowboy1_client_basic_1,
       t_cowboy2_client_basic_1,
       t_elli_client_basic_1]}].

suite() ->
    [{ct_hooks, [cth_surefire]},
     {timetrap, {seconds, ?CLOUDI_TEST_TIMEOUT}}].

init_per_suite(Config) ->
    ok = cloudi_x_reltool_util:application_start(cloudi_core,
                                                 [{configuration,
                                                   config_path()}],
                                                 infinity),
    Config.

end_per_suite(_Config) ->
    ok = cloudi_x_reltool_util:application_stop(cloudi_core),
    ok.

group(_GroupName) ->
    [].

init_per_group(GroupName, Config) ->
    Client = GroupName,
    {ok, ServiceIds} = cloudi_service_api:services_add([
        % using proplist configuration format, not the tuple/record format
        [{prefix, ?SERVICE_PREFIX1},
         {module, cloudi_service_http_client},
         {args,
          [{client, Client},
           {debug, true}]}]
        ], infinity),
    [{service_ids, ServiceIds} | Config].

end_per_group(_GroupName, Config0) ->
    {value, {_, ServiceIds}, ConfigN} = lists:keytake(service_ids, 1, Config0),
    ok = cloudi_service_api:services_remove(ServiceIds, infinity),
    ConfigN.

init_per_testcase(TestCase, Config0)
    when TestCase =:= t_cowboy1_client_basic_1;
         TestCase =:= t_cowboy2_client_basic_1;
         TestCase =:= t_elli_client_basic_1 ->
    ConfigN = case cloudi_string:split("_", erlang:atom_to_list(TestCase)) of
        [_, "cowboy2", _, _, _] ->
            [{port, 6464} | Config0];
        [_, "cowboy1", _, _, _] ->
            [{port, 6465} | Config0];
        [_, "elli", _, _, _] ->
            [{port, 6466} | Config0]
    end,
    ConfigN.

end_per_testcase(_TestCase, Config) ->
    lists:keydelete(port, 1, Config).

%%%------------------------------------------------------------------------
%%% test cases
%%%------------------------------------------------------------------------

t_cowboy1_client_basic_1(Config) ->
    cowboy_client_basic_1(Config).

t_cowboy2_client_basic_1(Config) ->
    cowboy_client_basic_1(Config).

t_elli_client_basic_1(Config) ->
    elli_client_basic_1(Config).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

config_path() ->
    Path = [_ | _] = os:getenv("TEST_DIR"),
    filename:join(Path, erlang:atom_to_list(?MODULE) ++ ".config").

cowboy_client_basic_1(Config) ->
    {port, Port} = lists:keyfind(port, 1, Config),
    Context0 = cloudi:new(),
    CloudIConfigPath = config_path(),
    {ok, CloudIConfigData} = file:read_file(CloudIConfigPath),
    ContentLength0 = erlang:integer_to_binary(byte_size(CloudIConfigData)),
    Host = cloudi_string:format_to_binary("127.0.0.1:~w", [Port]),
    URLPath0 = <<"/ct_test_files/cloudi_service_http_client_SUITE.config">>,
    {{ok,
      [{<<"accept-ranges">>, <<"bytes">>},
       {<<"content-length">>, ContentLength0},
       {<<"content-type">>, <<"application/x-erlang">>},
       {<<"date">>, _},
       {<<"etag">>, _},
       {<<"last-modified">>, _},
       {<<"server">>, <<"Cowboy">>},
       {<<"status">>, <<"200">>}],
      Response0},
     Context1} = cloudi_service_http_client:get(Context0,
                                                ?SERVICE_PREFIX1,
                                                [{<<"host">>, Host},
                                                 {<<"url-path">>, URLPath0}],
                                                <<>>),
    CloudIConfigData = Response0,
    {ok, CloudIConfig} = file:consult(CloudIConfigPath),
    {_, ServicesConfigIn} = lists:keyfind(services, 1, CloudIConfig),
    URLPath1 = <<"/cloudi/api/rpc/services.erl">>,
    {{ok,
      [{<<"content-length">>, ContentLength1},
       {<<"content-type">>, <<"application/x-erlang">>},
       {<<"date">>, _},
       {<<"server">>, <<"Cowboy">>},
       {<<"status">>, <<"200">>}],
      Response1},
     _} = cloudi_service_http_client:get(Context1,
                                         ?SERVICE_PREFIX1,
                                         [{<<"host">>, Host},
                                          {<<"url-path">>, URLPath1}],
                                         <<>>),
    true = (erlang:binary_to_integer(ContentLength1) ==
            erlang:byte_size(Response1)),
    ServicesConfigOut = [S ||
                         {_, S} <- cloudi_string:binary_to_term(Response1)],
    5 = erlang:length(ServicesConfigIn),
    6 = erlang:length(ServicesConfigOut),
    ok.

elli_client_basic_1(Config) ->
    {port, Port} = lists:keyfind(port, 1, Config),
    Context0 = cloudi:new(),
    CloudIConfigPath = config_path(),
    {ok, CloudIConfigData} = file:read_file(CloudIConfigPath),
    ContentLength0 = erlang:integer_to_binary(byte_size(CloudIConfigData)),
    Host = cloudi_string:format_to_binary("127.0.0.1:~w", [Port]),
    URLPath0 = <<"/ct_test_files/cloudi_service_http_client_SUITE.config">>,
    {{ok,
      [{<<"accept-ranges">>, <<"bytes">>},
       {<<"connection">>, <<"Keep-Alive">>},
       {<<"content-length">>, ContentLength0},
       {<<"content-type">>, <<"application/x-erlang">>},
       {<<"date">>, _},
       {<<"etag">>, _},
       {<<"last-modified">>, _},
       {<<"status">>, <<"200">>}],
      Response0},
     Context1} = cloudi_service_http_client:get(Context0,
                                                ?SERVICE_PREFIX1,
                                                [{<<"host">>, Host},
                                                 {<<"url-path">>, URLPath0}],
                                                <<>>),
    CloudIConfigData = Response0,
    {ok, CloudIConfig} = file:consult(CloudIConfigPath),
    {_, ServicesConfigIn} = lists:keyfind(services, 1, CloudIConfig),
    URLPath1 = <<"/cloudi/api/rpc/services.erl">>,
    {{ok,
      [{<<"connection">>, <<"Keep-Alive">>},
       {<<"content-length">>, ContentLength1},
       {<<"content-type">>, <<"application/x-erlang">>},
       {<<"status">>, <<"200">>}],
      Response1},
     _} = cloudi_service_http_client:get(Context1,
                                         ?SERVICE_PREFIX1,
                                         [{<<"host">>, Host},
                                          {<<"url-path">>, URLPath1}],
                                         <<>>),
    true = (erlang:binary_to_integer(ContentLength1) ==
            erlang:byte_size(Response1)),
    ServicesConfigOut = [S ||
                         {_, S} <- cloudi_string:binary_to_term(Response1)],
    5 = erlang:length(ServicesConfigIn),
    6 = erlang:length(ServicesConfigOut),
    ok.

