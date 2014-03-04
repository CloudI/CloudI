%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
-module(cloudi_service_queue_SUITE).
-behaviour(cloudi_service).

%% cloudi_service callbacks
-export([cloudi_service_init/3,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/2]).

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
-export([t_queue_destination_1/1,
         t_queue_both_1/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("cloudi_core/include/cloudi_logger.hrl").

-define(REQUEST1, "Hello World!").
-define(RESPONSE1, "REQUEST " ?REQUEST1).
-define(PREFIX_QUEUE, "/queue").
-define(PREFIX_TEST, "/test/").

-record(state,
    {
        mode :: request | response,
        prefix :: string(),
        trans_ids = [] :: list(cloudi_service:trans_id()),
        count = 0 :: non_neg_integer()
    }).

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, Prefix, Dispatcher) ->
    Defaults = [
        {mode,                             undefined},
        {test,                             undefined}],
    [Mode, Test] = cloudi_proplists:take_values(Defaults, Args),
    if
        Mode =:= request ->
            cloudi_service:subscribe(Dispatcher, "request");
        Mode =:= response ->
            cloudi_service:subscribe(Dispatcher, "response"),
            cloudi_service:self(Dispatcher) ! {test, Test}
    end,
    {ok, #state{mode = Mode,
                prefix = Prefix}}.

cloudi_service_handle_request(_Type, _Name, _Pattern, _RequestInfo, count,
                              _Timeout, _Priority, _TransId, _Pid,
                              #state{count = Count} = State, _Dispatcher) ->
    {reply, Count, State};
cloudi_service_handle_request(_Type, _Name, _Pattern, _RequestInfo, Request,
                              _Timeout, _Priority, _TransId, _Pid,
                              #state{mode = request,
                                     count = I} = State, _Dispatcher) ->
    {reply, <<(<<"REQUEST ">>)/binary, Request/binary>>,
     State#state{count = I + 1}};
cloudi_service_handle_request(_Type, _Name, _Pattern, _RequestInfo, Request,
                              _Timeout, _Priority, TransId, _Pid,
                              #state{mode = response,
                                     count = I,
                                     trans_ids = TransIds0} = State,
                              _Dispatcher) ->
    <<?RESPONSE1>> = Request,
    TransIdsN = lists:delete(TransId, TransIds0),
    true = (TransIdsN /= TransIds0),
    {reply, <<"RESPONSE_RECEIVED">>,
     State#state{count = I + 1,
                 trans_ids = TransIdsN}}.

cloudi_service_handle_info({test, 1},
                           #state{mode = response,
                                  prefix = Prefix,
                                  trans_ids = TransIds} = State,
                           Dispatcher) ->
    % used in 'both' mode to get the response back without using an
    % Erlang pid ('destination' mode depends on the source pid) so a
    % service request can survive the source failure (including
    % an Erlang VM restart)
    Name = ?PREFIX_QUEUE ++ Prefix ++ "request",
    RequestInfo = cloudi_service:request_info_key_value_new([
        {<<"service_name">>, erlang:list_to_binary(Prefix ++ "response")}]),
    {ok, Id0} = cloudi_service:send_sync(Dispatcher, Name, RequestInfo,
                                         <<?REQUEST1>>, undefined, undefined),
    {ok, Id1} = cloudi_service:send_sync(Dispatcher, Name, RequestInfo,
                                         <<?REQUEST1>>, undefined, undefined),
    {noreply, State#state{trans_ids = [Id0, Id1 | TransIds]}};
cloudi_service_handle_info(Request, State, _) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(_, _) ->
    ok.

%%%------------------------------------------------------------------------
%%% Callback functions from CT
%%%------------------------------------------------------------------------

all() ->
    [{group, queue_destination},
     {group, queue_both}].

groups() ->
    [{queue_destination, [],
      [t_queue_destination_1]},
     {queue_both, [],
      [t_queue_both_1]}].

suite() ->
    [{ct_hooks, [cth_surefire]},
     {timetrap, 11000}].

init_per_suite(Config) ->
    ok = cloudi_x_reltool_util:application_start(cloudi_core, [], infinity),
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

init_per_testcase(_TestCase, Config) ->
    {ok, Services} = cloudi_service_api:services(infinity),
    lists:foreach(fun({ServiceId, _}) ->
        cloudi_service_api:services_remove([ServiceId], infinity)
    end, Services),
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%%%------------------------------------------------------------------------
%%% test cases
%%%------------------------------------------------------------------------

% Test1 uses the two cloudi_service_queue modes (destination and both)
% to send 2 synchronous service requests through cloudi_service_queue to
% an unstable test service.  With cloudi_service_queue retries both
% service requests should complete for both modes.

t_queue_destination_1(_Config) ->
    Context = cloudi:new(),
    {ok, ServiceIds} = cloudi_service_api:services_add([
        {internal,
         ?PREFIX_TEST,
         ?MODULE,
         [{mode, request}],
         immediate_closest,
         5000, 5000, 5000, undefined, undefined, 8, 5, 300,
         [{monkey_chaos,
           [{probability_request, 0.5}]}]},
        {internal,
         ?PREFIX_QUEUE,
         cloudi_service_queue,
         [{retry, 4},
          {fault_isolation, destination},
          {file, "queue_destination_1_${I}.log"}],
         immediate_closest,
         5000, 5000, 5000, undefined, undefined, 4, 0, 0,
         [{request_timeout_immediate_max, 0},
          {response_timeout_immediate_max, 0}]}],
        infinity),
    {ok, <<?RESPONSE1>>} = cloudi:send_sync(Context,
                                            ?PREFIX_QUEUE ?PREFIX_TEST
                                            "request", <<?REQUEST1>>),
    {ok, <<?RESPONSE1>>} = cloudi:send_sync(Context,
                                            ?PREFIX_QUEUE ?PREFIX_TEST
                                            "request", <<?REQUEST1>>),
    [cloudi_service_api:services_remove([ServiceId], infinity) ||
     ServiceId <- ServiceIds],
    ok.

t_queue_both_1(_Config) ->
    Context = cloudi:new(),
    {ok, ServiceIds} = cloudi_service_api:services_add([
        {internal,
         ?PREFIX_TEST,
         ?MODULE,
         [{mode, request}],
         immediate_closest,
         5000, 5000, 5000, undefined, undefined, 8, 5, 300,
         [{monkey_chaos,
           [{probability_request, 0.5}]}]},
        {internal,
         ?PREFIX_QUEUE,
         cloudi_service_queue,
         [{retry, 4},
          {fault_isolation, both},
          {file, "queue_both_1_${I}.log"}],
         immediate_closest,
         5000, 5000, 5000, undefined, undefined, 4, 0, 0,
         [{request_timeout_immediate_max, 0},
          {response_timeout_immediate_max, 0}]},
        {internal,
         ?PREFIX_TEST,
         ?MODULE,
         [{mode, response},
          {test, 1}],
         immediate_closest,
         5000, 5000, 5000, undefined, undefined, 1, 5, 300, []}],
        infinity),
    receive after 10000 -> ok end,
    {ok, 2} = cloudi:send_sync(Context, ?PREFIX_TEST "response", count),
    [cloudi_service_api:services_remove([ServiceId], infinity) ||
     ServiceId <- ServiceIds],
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

