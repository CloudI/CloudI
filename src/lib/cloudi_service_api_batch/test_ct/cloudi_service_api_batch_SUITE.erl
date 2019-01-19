%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
-module(cloudi_service_api_batch_SUITE).
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
-export([t_batch_1/1,
         t_batch_2/1,
         t_batch_3/1,
         t_batch_4/1,
         t_stop_when_done_1/1,
         t_stop_when_done_2/1,
         t_batch_add_error_1/1]).

-record(state,
    {
        mode :: atom()
    }).

-include_lib("common_test/include/ct.hrl").
-include_lib("cloudi_core/include/cloudi_logger.hrl").

-define(SERVICE_PREFIX, "/cloudi/api/").
-define(QUEUE0, "QUEUE0").
-define(QUEUE1, "QUEUE1").
-define(QUEUE2, "QUEUE2").

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, _Prefix, _Timeout, _Dispatcher) ->
    Defaults = [
        {mode,                             undefined},
        {parent,                           undefined},
        {value,                            undefined}],
    [Mode, Parent, Value] = cloudi_proplists:take_values(Defaults, Args),
    true = is_pid(Parent),
    ?LOG_INFO("~p ~p", [Mode, Value]),
    % cloudi_service:self(Dispatcher) =:= self() in cloudi_service_init/4
    NewMode = if
        Mode =:= send_parent_value_1 ->
            Parent ! Value,
            erlang:send_after(500, self(), send_parent_value_1),
            send_parent_value_1;
        Mode =:= send_parent_value_2 ->
            Parent ! Value,
            erlang:send_after(500, self(), send_parent_value_2),
            send_parent_value_2;
        Mode =:= send_parent_value_3 ->
            Parent ! Value,
            erlang:send_after(500, self(), send_parent_value_3),
            send_parent_value_3;
        Mode =:= send_parent_value_4 ->
            Parent ! Value,
            erlang:send_after(500, self(), send_parent_value_4),
            send_parent_value_4
    end,
    {ok, #state{mode = NewMode}}.

cloudi_service_handle_request(_Type, _Name, _Pattern,
                              _RequestInfo, _Request,
                              _Timeout, _Priority, _TransId, _Pid,
                              #state{} = State,
                              _Dispatcher) ->
    erlang:exit(crash),
    {noreply, State}.

cloudi_service_handle_info(send_parent_value_1, State, _Dispatcher) ->
    {stop, shutdown, State};
cloudi_service_handle_info(send_parent_value_2, State, _Dispatcher) ->
    {stop, {shutdown, send_parent_value_2}, State};
cloudi_service_handle_info(send_parent_value_3, State, _Dispatcher) ->
    {stop, anything_else_is_an_error, State};
cloudi_service_handle_info(send_parent_value_4, State, _Dispatcher) ->
    % normal is an error stop reason for internal CloudI services
    % (normal causes the service to do a restart)
    {stop, normal, State}.

cloudi_service_terminate(_Reason, _Timeout, _State) ->
    ok.

%%%------------------------------------------------------------------------
%%% Callback functions from CT
%%%------------------------------------------------------------------------

all() ->
    [{group, batch_basic_1}].

groups() ->
    [{batch_basic_1, [sequence],
      [t_batch_1,
       t_batch_2,
       t_batch_3,
       t_batch_4,
       t_stop_when_done_1,
       t_stop_when_done_2,
       t_batch_add_error_1]}].

suite() ->
    [{ct_hooks, [cth_surefire]},
     {timetrap, 10100}].

init_per_suite(Config) ->
    ok = cloudi_x_reltool_util:application_start(sasl,
                                                 [{sasl_error_logger, false}],
                                                 infinity),
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

init_per_testcase(TestCase) ->
    error_logger:info_msg("~p init~n", [TestCase]),
    error_logger:tty(false),
    ?LOG_INFO("~p init", [TestCase]),
    ok.

end_per_testcase(TestCase) ->
    error_logger:tty(true),
    error_logger:info_msg("~p end~n", [TestCase]),
    ?LOG_INFO("~p end", [TestCase]),
    ok.

init_per_testcase(TestCase, Config)
    when TestCase =:= t_stop_when_done_1 ->
    init_per_testcase(TestCase),
    Self = self(),
    {ok, ServiceIds} = cloudi_service_api:services_add([
        % using proplist configuration format, not the tuple/record format
        [{prefix, ?SERVICE_PREFIX},
         {module, cloudi_service_api_batch},
         {args,
          [{stop_when_done, true},
           {queues_static, true},
           {queues,
            [{?QUEUE0,
              [[{module, ?MODULE},
                {args,
                 [{mode, send_parent_value_1},
                  {parent, Self},
                  {value, Value}]},
                {timeout_init, limit_min},
                {max_t, 1},
                {options,
                 [{automatic_loading, false}]}]
               || Value <- [987453, 12, 467]]}]}]}]
        ], infinity),
    [{service_ids, ServiceIds} | Config];
init_per_testcase(TestCase, Config)
    when TestCase =:= t_stop_when_done_2 ->
    init_per_testcase(TestCase),
    Self = self(),
    {ok, ServiceIds} = cloudi_service_api:services_add([
        % using proplist configuration format, not the tuple/record format
        [{prefix, ?SERVICE_PREFIX},
         {module, cloudi_service_api_batch},
         {args,
          [{stop_when_done, true},
           {queues_static, true},
           {queues,
            [{?QUEUE1,
              [[{module, ?MODULE},
                {args,
                 [{mode, send_parent_value_2},
                  {parent, Self},
                  {value, Value}]},
                {count_process, 3},
                {timeout_init, limit_min},
                {max_t, 1},
                {options,
                 [{automatic_loading, false}]}]
               || Value <- [987453, 12, 467]]}]}]}]
        ], infinity),
    [{service_ids, ServiceIds} | Config];
init_per_testcase(TestCase, Config) ->
    init_per_testcase(TestCase),
    {ok, ServiceIds} = cloudi_service_api:services_add([
        % using proplist configuration format, not the tuple/record format
        [{prefix, ?SERVICE_PREFIX},
         {module, cloudi_service_api_batch}]
        ], infinity),
    [{service_ids, ServiceIds} | Config].

end_per_testcase(TestCase, Config)
    when TestCase =:= t_stop_when_done_1;
         TestCase =:= t_stop_when_done_2 ->
    end_per_testcase(TestCase),
    case lists:keytake(service_ids, 1, Config) of
        {value, {_, ServiceIds}, NewConfig} ->
            _ = cloudi_service_api:services_remove(ServiceIds, infinity),
            NewConfig;
        false ->
            Config
    end;
end_per_testcase(TestCase, Config) ->
    end_per_testcase(TestCase),
    case lists:keytake(service_ids, 1, Config) of
        {value, {_, ServiceIds}, NewConfig} ->
            ok = cloudi_service_api:services_remove(ServiceIds, infinity),
            NewConfig;
        false ->
            Config
    end.

%%%------------------------------------------------------------------------
%%% test cases
%%%------------------------------------------------------------------------

t_batch_1(_Config) ->
    Context0 = cloudi:new(),
    ValuesCount0 = 10,
    Values0 = lists:seq(1, ValuesCount0),
    Configs0 = [[{module, ?MODULE},
                 {args,
                  [{mode, send_parent_value_1},
                   {parent, self()},
                   {value, Value0}]},
                 {timeout_init, limit_min},
                 {max_t, 1},
                 {options,
                  [{automatic_loading, false}]}] || Value0 <- Values0],
    QueuedCount0 = ValuesCount0 - 1, % service configurations queued count
    {{ok, QueuedCount0},
     _Context1} = cloudi_service_api_batch:services_add(Context0,
                                                        ?SERVICE_PREFIX,
                                                        ?QUEUE0,
                                                        Configs0),
    {ok, Received0} = receive_messages(ValuesCount0),
    true = Received0 == Values0,
    ok.

t_batch_2(_Config) ->
    Context0 = cloudi:new(),
    ValuesCount0 = 3,
    Values0 = [987453, 12, 467],
    Configs0 = [[{module, ?MODULE},
                 {args,
                  [{mode, send_parent_value_2},
                   {parent, self()},
                   {value, Value0}]},
                 {count_process, 3},
                 {timeout_init, limit_min},
                 {max_t, 1},
                 {options,
                  [{automatic_loading, false}]}] || Value0 <- Values0],
    QueuedCount0 = ValuesCount0 - 1, % service configurations queued count
    {{ok, QueuedCount0},
     _Context1} = cloudi_service_api_batch:services_add(Context0,
                                                        ?SERVICE_PREFIX,
                                                        ?QUEUE1,
                                                        Configs0),
    {ok, Received0} = receive_messages(9), % 3 messages from 3 processes
    true = Received0 == [987453, 987453, 987453, 12, 12, 12, 467, 467, 467],
    ok.

t_batch_3(_Config) ->
    Context0 = cloudi:new(),
    ValuesCount0 = 3,
    Values0 = [987453, 12, 467],
    Configs0 = [[{module, ?MODULE},
                 {args,
                  [{mode, send_parent_value_3},
                   {parent, self()},
                   {value, Value0}]},
                 {timeout_init, limit_min},
                 {max_t, 10},
                 {options,
                  [{automatic_loading, false}]}] || Value0 <- Values0],
    QueuedCount0 = ValuesCount0 - 1, % service configurations queued count
    {{ok, QueuedCount0},
     _Context1} = cloudi_service_api_batch:services_add(Context0,
                                                        ?SERVICE_PREFIX,
                                                        ?QUEUE2,
                                                        Configs0),
    {ok, Received0} = receive_messages(6), % 5 restarts on error
    true = Received0 == [987453, 987453, 987453, 987453, 987453, 987453],
    nothing = receive Something -> Something after 1000 -> nothing end,
    ok.

t_batch_4(_Config) ->
    Context0 = cloudi:new(),
    ValuesCount0 = 3,
    Values0 = [987453, 12, 467],
    Configs0 = [[{module, ?MODULE},
                 {args,
                  [{mode, send_parent_value_4},
                   {parent, self()},
                   {value, Value0}]},
                 {count_process, 2},
                 {timeout_init, limit_min},
                 {max_t, 10},
                 {options,
                  [{automatic_loading, false}]}] || Value0 <- Values0],
    QueuedCount0 = ValuesCount0 - 1, % service configurations queued count
    {{ok, QueuedCount0},
     _Context1} = cloudi_service_api_batch:services_add(Context0,
                                                        ?SERVICE_PREFIX,
                                                        ?QUEUE0,
                                                        Configs0),
    {ok, Received0} = receive_messages(12), % 2 processes with 5 restarts
    true = Received0 == [987453, 987453, 987453, 987453, 987453, 987453,
                         987453, 987453, 987453, 987453, 987453, 987453],
    nothing = receive Something -> Something after 1000 -> nothing end,
    ok.

t_stop_when_done_1(Config) ->
    {ok, Received0} = receive_messages(3), % 1 message from 3 processes
    true = Received0 == [987453, 12, 467],
    nothing = receive Something -> Something after 1000 -> nothing end,
    {_, [ServiceId]} = lists:keyfind(service_ids, 1, Config),
    {error, not_found} = cloudi_service_api:
                         service_subscriptions(ServiceId, infinity),
    ok.

t_stop_when_done_2(Config) ->
    {ok, Received0} = receive_messages(9), % 3 messages from 3 processes
    true = Received0 == [987453, 987453, 987453, 12, 12, 12, 467, 467, 467],
    nothing = receive Something -> Something after 1000 -> nothing end,
    {_, [ServiceId]} = lists:keyfind(service_ids, 1, Config),
    {error, not_found} = cloudi_service_api:
                         service_subscriptions(ServiceId, infinity),
    ok.

t_batch_add_error_1(_Config) ->
    Context0 = cloudi:new(),
    ValuesCount0 = 10,
    Values0 = lists:seq(1, ValuesCount0),
    Configs0 = [[{invalid_configuration, true},
                 {module, ?MODULE},
                 {args,
                  [{mode, send_parent_value_1},
                   {parent, self()},
                   {value, Value0}]},
                 {timeout_init, limit_min},
                 {max_t, 1},
                 {options,
                  [{automatic_loading, false}]}] || Value0 <- Values0],
    {{ok, {error, purged}},
     _Context1} = cloudi_service_api_batch:services_add(Context0,
                                                        ?SERVICE_PREFIX,
                                                        ?QUEUE0,
                                                        Configs0),
    nothing = receive Something -> Something after 1000 -> nothing end,
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

receive_messages(Count) ->
    receive_messages(Count, []).

receive_messages(0, L) ->
    {ok, lists:reverse(L)};
receive_messages(Count, L) ->
    receive
        Message ->
            receive_messages(Count - 1, [Message | L])
    after
        10000 ->
            {error, timeout}
    end.
