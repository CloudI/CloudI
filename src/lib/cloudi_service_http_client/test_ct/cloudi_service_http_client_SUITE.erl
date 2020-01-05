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
-export([t_client_basic_1/1]).

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
    [{group,
      client_basic}].

groups() ->
    [{client_basic, [],
      [t_client_basic_1]}].

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

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(TestCase, Config)
    when TestCase =:= t_client_basic_1 ->
    {ok, ServiceIds} = cloudi_service_api:services_add([
        % using proplist configuration format, not the tuple/record format
        [{prefix, ?SERVICE_PREFIX1},
         {module, cloudi_service_http_client},
         {args,
          [{client, inets},
           {debug, true}]}]
        ], infinity),
    [{service_ids, ServiceIds} | Config].

end_per_testcase(_TestCase, Config) ->
    {value, {_, ServiceIds}, NewConfig} = lists:keytake(service_ids, 1, Config),
    ok = cloudi_service_api:services_remove(ServiceIds, infinity),
    NewConfig.

%%%------------------------------------------------------------------------
%%% test cases
%%%------------------------------------------------------------------------

t_client_basic_1(_Config) ->
    Context0 = cloudi:new(),
    CloudIConfigPath = config_path(),
    {ok, ConfigData} = file:read_file(CloudIConfigPath),
    ContentLength0 = erlang:integer_to_binary(erlang:byte_size(ConfigData)),
    Host = <<"127.0.0.1:6464">>,
    URLPath0 = <<"/ct_test_files/cloudi_service_http_client_SUITE.config">>,
    {{ok,
      [{<<"status">>, <<"200">>},
       {<<"date">>, _},
       {<<"accept-ranges">>, <<"bytes">>},
       {<<"etag">>, _},
       {<<"server">>, <<"Cowboy">>},
       {<<"content-length">>, ContentLength0},
       {<<"content-type">>, <<"application/x-erlang">>},
       {<<"last-modified">>, _}],
      Response0},
     Context1} = cloudi_service_http_client:get(Context0,
                                                ?SERVICE_PREFIX1,
                                                [{<<"host">>, Host},
                                                 {<<"url-path">>, URLPath0}],
                                                <<>>),
    ConfigData = Response0,
    {ok, Config} = file:consult(CloudIConfigPath),
    {_, ServicesConfigIn} = lists:keyfind(services, 1, Config),
    URLPath1 = <<"/cloudi/api/rpc/services.erl">>,
    {{ok,
      [{<<"status">>, <<"200">>},
       {<<"date">>, _},
       {<<"server">>, <<"Cowboy">>},
       {<<"content-length">>, ContentLength1},
       {<<"content-type">>, <<"application/x-erlang">>}],
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
    3 = erlang:length(ServicesConfigIn),
    4 = erlang:length(ServicesConfigOut),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

config_path() ->
    Path = [_ | _] = os:getenv("TEST_DIR"),
    filename:join(Path, erlang:atom_to_list(?MODULE) ++ ".config").

