%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% Fuctions Common to Both Internal and External Services
%%%
%%% MIT License
%%%
%%% Copyright (c) 2013-2020 Michael Truog <mjtruog at protonmail dot com>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a
%%% copy of this software and associated documentation files (the "Software"),
%%% to deal in the Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%% and/or sell copies of the Software, and to permit persons to whom the
%%% Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%%% DEALINGS IN THE SOFTWARE.
%%%
%%%------------------------------------------------------------------------

-include("cloudi_core_i_common.hrl").
-include("cloudi_core_i_services_common_init.hrl").

% When using the state record within this file, only the state elements
% that are common among cloudi_core_i_services_internal.erl and
% cloudi_core_i_services_external.erl may be used

-compile({nowarn_unused_function,
          [{recv_async_select_random, 1},
           {recv_async_select_oldest, 1},
           {return_null_response, 6}]}).
-compile({inline,
          [{request_timeout_adjustment_f, 1},
           {return_null_response, 6},
           {return_null_response, 7}]}).

uptime(TimeStart, TimeRestart, Restarts) ->
    TimeSystemStart = erlang:system_info(start_time),
    NanoSecondsStart = cloudi_timestamp:convert(TimeStart - TimeSystemStart,
                                                native, nanosecond),
    NanoSecondsRestart = if
        TimeRestart =:= undefined ->
            undefined;
        is_integer(TimeRestart) ->
            cloudi_timestamp:convert(TimeRestart - TimeSystemStart,
                                     native, nanosecond)
    end,
    {NanoSecondsStart, NanoSecondsRestart, Restarts}.

destination_allowed([], _, _) ->
    false;

destination_allowed(_, undefined, undefined) ->
    true;

destination_allowed(Name, undefined, DestAllow) ->
    case cloudi_x_trie:find_match2(Name, DestAllow) of
        {ok, _, _} ->
            true;
        error ->
            false
    end;

destination_allowed(Name, DestDeny, undefined) ->
    case cloudi_x_trie:find_match2(Name, DestDeny) of
        {ok, _, _} ->
            false;
        error ->
            true
    end;

destination_allowed(Name, DestDeny, DestAllow) ->
    case cloudi_x_trie:find_match2(Name, DestDeny) of
        {ok, _, _} ->
            false;
        error ->
            case cloudi_x_trie:find_match2(Name, DestAllow) of
                {ok, _, _} ->
                    true;
                error ->
                    false
            end
    end.

destination_get(lazy_closest, _, Name, Pid, Groups, _) ->
    cloudi_x_cpg_data:get_closest_pid(Name, Pid, Groups);

destination_get(lazy_furthest, _, Name, Pid, Groups, _) ->
    cloudi_x_cpg_data:get_furthest_pid(Name, Pid, Groups);

destination_get(lazy_random, _, Name, Pid, Groups, _) ->
    cloudi_x_cpg_data:get_random_pid(Name, Pid, Groups);

destination_get(lazy_local, _, Name, Pid, Groups, _) ->
    cloudi_x_cpg_data:get_local_pid(Name, Pid, Groups);

destination_get(lazy_remote, _, Name, Pid, Groups, _) ->
    cloudi_x_cpg_data:get_remote_pid(Name, Pid, Groups);

destination_get(lazy_newest, _, Name, Pid, Groups, _) ->
    cloudi_x_cpg_data:get_newest_pid(Name, Pid, Groups);

destination_get(lazy_oldest, _, Name, Pid, Groups, _) ->
    cloudi_x_cpg_data:get_oldest_pid(Name, Pid, Groups);

destination_get(DestRefresh, _, _, _, _, Timeout)
    when (Timeout < ?TIMEOUT_DELTA),
         (DestRefresh =:= immediate_closest orelse
          DestRefresh =:= immediate_furthest orelse
          DestRefresh =:= immediate_random orelse
          DestRefresh =:= immediate_local orelse
          DestRefresh =:= immediate_remote orelse
          DestRefresh =:= immediate_newest orelse
          DestRefresh =:= immediate_oldest) ->
    {error, timeout};

destination_get(immediate_closest, Scope, Name, Pid, _, Timeout) ->
    ?CATCH_EXIT(cloudi_x_cpg:get_closest_pid(Scope, Name, Pid, Timeout));

destination_get(immediate_furthest, Scope, Name, Pid, _, Timeout) ->
    ?CATCH_EXIT(cloudi_x_cpg:get_furthest_pid(Scope, Name, Pid, Timeout));

destination_get(immediate_random, Scope, Name, Pid, _, Timeout) ->
    ?CATCH_EXIT(cloudi_x_cpg:get_random_pid(Scope, Name, Pid, Timeout));

destination_get(immediate_local, Scope, Name, Pid, _, Timeout) ->
    ?CATCH_EXIT(cloudi_x_cpg:get_local_pid(Scope, Name, Pid, Timeout));

destination_get(immediate_remote, Scope, Name, Pid, _, Timeout) ->
    ?CATCH_EXIT(cloudi_x_cpg:get_remote_pid(Scope, Name, Pid, Timeout));

destination_get(immediate_newest, Scope, Name, Pid, _, Timeout) ->
    ?CATCH_EXIT(cloudi_x_cpg:get_newest_pid(Scope, Name, Pid, Timeout));

destination_get(immediate_oldest, Scope, Name, Pid, _, Timeout) ->
    ?CATCH_EXIT(cloudi_x_cpg:get_oldest_pid(Scope, Name, Pid, Timeout));

destination_get(DestRefresh, _, _, _, _, _) ->
    ?LOG_ERROR("unable to send with invalid destination refresh: ~s",
               [DestRefresh]),
    erlang:exit(badarg).

destination_all(DestRefresh, _, Name, Pid, Groups, _)
    when (DestRefresh =:= lazy_closest orelse
          DestRefresh =:= lazy_furthest orelse
          DestRefresh =:= lazy_random orelse
          DestRefresh =:= lazy_newest orelse
          DestRefresh =:= lazy_oldest) ->
    cloudi_x_cpg_data:get_members(Name, Pid, Groups);

destination_all(lazy_local, _, Name, Pid, Groups, _) ->
    cloudi_x_cpg_data:get_local_members(Name, Pid, Groups);

destination_all(lazy_remote, _, Name, Pid, Groups, _) ->
    cloudi_x_cpg_data:get_remote_members(Name, Pid, Groups);

destination_all(DestRefresh, _, _, _, _, Timeout)
    when (Timeout < ?TIMEOUT_DELTA),
         (DestRefresh =:= immediate_closest orelse
          DestRefresh =:= immediate_furthest orelse
          DestRefresh =:= immediate_random orelse
          DestRefresh =:= immediate_local orelse
          DestRefresh =:= immediate_remote orelse
          DestRefresh =:= immediate_newest orelse
          DestRefresh =:= immediate_oldest) ->
    {error, timeout};

destination_all(DestRefresh, Scope, Name, Pid, _, Timeout)
    when (DestRefresh =:= immediate_closest orelse
          DestRefresh =:= immediate_furthest orelse
          DestRefresh =:= immediate_random orelse
          DestRefresh =:= immediate_newest orelse
          DestRefresh =:= immediate_oldest) ->
    ?CATCH_EXIT(cloudi_x_cpg:get_members(Scope, Name, Pid, Timeout));

destination_all(immediate_local, Scope, Name, Pid, _, Timeout) ->
    ?CATCH_EXIT(cloudi_x_cpg:get_local_members(Scope, Name, Pid, Timeout));

destination_all(immediate_remote, Scope, Name, Pid, _, Timeout) ->
    ?CATCH_EXIT(cloudi_x_cpg:get_remote_members(Scope, Name, Pid, Timeout));

destination_all(DestRefresh, _, _, _, _, _) ->
    ?LOG_ERROR("unable to send with invalid destination refresh: ~s",
               [DestRefresh]),
    erlang:exit(badarg).

send_async_timeout_start(Timeout, TransId, Pid,
                         #state{dispatcher = Dispatcher,
                                send_timeouts = SendTimeouts,
                                send_timeout_monitors =
                                    SendTimeoutMonitors,
                                options = #config_service_options{
                                    request_timeout_immediate_max =
                                        RequestTimeoutImmediateMax}} = State)
    when is_integer(Timeout), is_binary(TransId), is_pid(Pid),
         Timeout >= RequestTimeoutImmediateMax ->
    SendTimeoutMonitorsNew = case maps:find(Pid, SendTimeoutMonitors) of
        {ok, {MonitorRef, TransIdList}} ->
            maps:put(Pid,
                     {MonitorRef,
                      lists:umerge(TransIdList, [TransId])},
                     SendTimeoutMonitors);
        error ->
            MonitorRef = erlang:monitor(process, Pid),
            maps:put(Pid, {MonitorRef, [TransId]}, SendTimeoutMonitors)
    end,
    State#state{
        send_timeouts = maps:put(TransId,
            {passive, Pid,
             erlang:send_after(Timeout, Dispatcher,
                               {'cloudi_service_send_async_timeout', TransId})},
            SendTimeouts),
        send_timeout_monitors = SendTimeoutMonitorsNew};

send_async_timeout_start(Timeout, TransId, _Pid,
                         #state{dispatcher = Dispatcher,
                                send_timeouts = SendTimeouts} = State)
    when is_integer(Timeout), is_binary(TransId) ->
    State#state{
        send_timeouts = maps:put(TransId,
            {passive, undefined,
             erlang:send_after(Timeout, Dispatcher,
                               {'cloudi_service_send_async_timeout', TransId})},
            SendTimeouts)}.

send_sync_timeout_start(Timeout, TransId, Pid, Client,
                        #state{dispatcher = Dispatcher,
                               send_timeouts = SendTimeouts,
                               send_timeout_monitors =
                                   SendTimeoutMonitors,
                               options = #config_service_options{
                                   request_timeout_immediate_max =
                                       RequestTimeoutImmediateMax}} = State)
    when is_integer(Timeout), is_binary(TransId), is_pid(Pid),
         Timeout >= RequestTimeoutImmediateMax ->
    SendTimeoutMonitorsNew = case maps:find(Pid, SendTimeoutMonitors) of
        {ok, {MonitorRef, TransIdList}} ->
            maps:put(Pid,
                     {MonitorRef,
                      lists:umerge(TransIdList, [TransId])},
                     SendTimeoutMonitors);
        error ->
            MonitorRef = erlang:monitor(process, Pid),
            maps:put(Pid, {MonitorRef, [TransId]}, SendTimeoutMonitors)
    end,
    State#state{
        send_timeouts = maps:put(TransId,
            {Client, Pid,
             erlang:send_after(Timeout, Dispatcher,
                               {'cloudi_service_send_sync_timeout', TransId})},
            SendTimeouts),
        send_timeout_monitors = SendTimeoutMonitorsNew};

send_sync_timeout_start(Timeout, TransId, _Pid, Client,
                        #state{dispatcher = Dispatcher,
                               send_timeouts = SendTimeouts} = State)
    when is_integer(Timeout), is_binary(TransId) ->
    State#state{
        send_timeouts = maps:put(TransId,
            {Client, undefined,
             erlang:send_after(Timeout, Dispatcher,
                               {'cloudi_service_send_sync_timeout', TransId})},
            SendTimeouts)}.

send_timeout_end(TransId, Pid,
                 #state{send_timeouts = SendTimeouts,
                        send_timeout_monitors = SendTimeoutMonitors} = State)
    when is_binary(TransId) ->
    SendTimeoutMonitorsNew = if
        is_pid(Pid) ->
            case maps:find(Pid, SendTimeoutMonitors) of
                {ok, {MonitorRef, [TransId]}} ->
                    erlang:demonitor(MonitorRef, [flush]),
                    maps:remove(Pid, SendTimeoutMonitors);
                {ok, {MonitorRef, TransIdList}} ->
                    maps:put(Pid,
                             {MonitorRef,
                              lists:delete(TransId, TransIdList)},
                             SendTimeoutMonitors);
                error ->
                    SendTimeoutMonitors
            end;
        Pid =:= undefined ->
            SendTimeoutMonitors
    end,
    State#state{send_timeouts = maps:remove(TransId, SendTimeouts),
                send_timeout_monitors = SendTimeoutMonitorsNew}.

send_timeout_dead(Pid,
                  #state{dispatcher = Dispatcher,
                         send_timeouts = SendTimeouts,
                         send_timeout_monitors =
                             SendTimeoutMonitors} = State)
    when is_pid(Pid) ->
    case maps:find(Pid, SendTimeoutMonitors) of
        {ok, {_MonitorRef, TransIdList}} ->
            SendTimeoutsNew = lists:foldl(fun(TransId, D) ->
                case maps:find(TransId, D) of
                    {ok, {Type, _, Tref}}
                    when Type =:= active; Type =:= passive ->
                        case erlang:cancel_timer(Tref) of
                            false ->
                                ok;
                            _ ->
                                Dispatcher !
                                    {'cloudi_service_send_async_timeout',
                                     TransId},
                                ok
                        end,
                        maps:put(TransId, {Type, undefined, Tref}, D);
                    {ok, {Client, _, Tref}} ->
                        case erlang:cancel_timer(Tref) of
                            false ->
                                ok;
                            _ ->
                                Dispatcher !
                                    {'cloudi_service_send_sync_timeout',
                                     TransId},
                                ok
                        end,
                        maps:put(TransId, {Client, undefined, Tref}, D);
                    error ->
                        D
                end
            end, SendTimeouts, TransIdList),
            SendTimeoutMonitorsNew = maps:remove(Pid, SendTimeoutMonitors),
            {true,
             State#state{send_timeouts = SendTimeoutsNew,
                         send_timeout_monitors = SendTimeoutMonitorsNew}};
        error ->
            {false, State}
    end.

async_response_timeout_start(_, _, 0, _, State) ->
    State;

async_response_timeout_start(ResponseInfo, Response, Timeout, TransId,
                             #state{dispatcher = Dispatcher,
                                    async_responses = AsyncResponses} = State)
    when is_integer(Timeout), is_binary(TransId) ->
    erlang:send_after(Timeout, Dispatcher,
                      {'cloudi_service_recv_async_timeout', TransId}),
    State#state{async_responses = maps:put(TransId,
                                           {ResponseInfo, Response},
                                           AsyncResponses)}.

recv_async_select_random([{TransId, _} | _]) ->
    TransId.

recv_async_select_oldest([{TransId, _} | L]) ->
    recv_async_select_oldest(L, cloudi_x_uuid:get_v1_time(TransId), TransId).

recv_async_select_oldest([], _, TransIdCurrent) ->
    TransIdCurrent;

recv_async_select_oldest([{TransId, _} | L], Time0, TransIdCurrent) ->
    Time1 = cloudi_x_uuid:get_v1_time(TransId),
    if
        Time1 < Time0 ->
            recv_async_select_oldest(L, Time1, TransId);
        true ->
            recv_async_select_oldest(L, Time0, TransIdCurrent)
    end.

check_init_send(#config_service_options{
                    monkey_latency = false,
                    monkey_chaos = false} = ConfigOptions) ->
    ConfigOptions;
check_init_send(#config_service_options{
                    monkey_latency = MonkeyLatency,
                    monkey_chaos = MonkeyChaos} = ConfigOptions) ->
    MonkeyLatencyNew = if
        MonkeyLatency =/= false ->
            cloudi_core_i_runtime_testing:
            monkey_latency_init(MonkeyLatency);
        true ->
            MonkeyLatency
    end,
    MonkeyChaosNew = if
        MonkeyChaos =/= false ->
            cloudi_core_i_runtime_testing:
            monkey_chaos_init(MonkeyChaos);
        true ->
            MonkeyChaos
    end,
    ConfigOptions#config_service_options{
        monkey_latency = MonkeyLatencyNew,
        monkey_chaos = MonkeyChaosNew}.

check_init_receive(#config_service_options{
                       rate_request_max = undefined,
                       count_process_dynamic = false,
                       hibernate = Hibernate} = ConfigOptions)
    when is_boolean(Hibernate) ->
    ConfigOptions;
check_init_receive(#config_service_options{
                       rate_request_max = RateRequest,
                       count_process_dynamic = CountProcessDynamic,
                       hibernate = Hibernate} = ConfigOptions) ->
    RateRequestNew = if
        RateRequest =/= undefined ->
            cloudi_core_i_rate_based_configuration:
            rate_request_init(RateRequest);
        true ->
            RateRequest
    end,
    CountProcessDynamicNew = if
        CountProcessDynamic =/= false ->
            cloudi_core_i_rate_based_configuration:
            count_process_dynamic_init(CountProcessDynamic);
        true ->
            CountProcessDynamic
    end,
    HibernateNew = if
        not is_boolean(Hibernate) ->
            cloudi_core_i_rate_based_configuration:
            hibernate_init(Hibernate);
        true ->
            Hibernate
    end,
    ConfigOptions#config_service_options{
        rate_request_max = RateRequestNew,
        count_process_dynamic = CountProcessDynamicNew,
        hibernate = HibernateNew}.

check_incoming(_ServiceRequest,
               #config_service_options{
                   count_process_dynamic = false,
                   monkey_latency = false,
                   monkey_chaos = false,
                   hibernate = Hibernate} = ConfigOptions)
    when is_boolean(Hibernate) ->
    ConfigOptions;
check_incoming(ServiceRequest,
               #config_service_options{
                   count_process_dynamic = CountProcessDynamic,
                   monkey_latency = MonkeyLatency,
                   monkey_chaos = MonkeyChaos,
                   hibernate = Hibernate} = ConfigOptions) ->
    CountProcessDynamicNew = if
        (CountProcessDynamic =/= false), ServiceRequest ->
            cloudi_core_i_rate_based_configuration:
            count_process_dynamic_request(CountProcessDynamic);
        true ->
            CountProcessDynamic
    end,
    MonkeyLatencyNew = if
        MonkeyLatency =/= false ->
            cloudi_core_i_runtime_testing:
            monkey_latency_check(MonkeyLatency);
        true ->
            MonkeyLatency
    end,
    MonkeyChaosNew = if
        MonkeyChaos =/= false ->
            cloudi_core_i_runtime_testing:
            monkey_chaos_check(MonkeyChaos);
        true ->
            MonkeyChaos
    end,
    HibernateNew = if
        (not is_boolean(Hibernate)), ServiceRequest ->
            cloudi_core_i_rate_based_configuration:
            hibernate_request(Hibernate);
        true ->
            Hibernate
    end,
    ConfigOptions#config_service_options{
        count_process_dynamic = CountProcessDynamicNew,
        monkey_latency = MonkeyLatencyNew,
        monkey_chaos = MonkeyChaosNew,
        hibernate = HibernateNew}.

request_timeout_adjustment_f(true) ->
    RequestTimeStart = cloudi_timestamp:milliseconds_monotonic(),
    fun(T) ->
        Delta = cloudi_timestamp:milliseconds_monotonic() - RequestTimeStart,
        if
            Delta >= T ->
                0;
            true ->
                T - Delta
        end
    end;
request_timeout_adjustment_f(false) ->
    fun(T) -> T end.

aspects_terminate_before([], _, _, ServiceState) ->
    {ok, ServiceState};
aspects_terminate_before([{M, F} = Aspect | L],
                         Reason, TimeoutTerm, ServiceState) ->
    try {ok, _} = M:F(Reason, TimeoutTerm, ServiceState) of
        {ok, ServiceStateNew} ->
            aspects_terminate_before(L, Reason, TimeoutTerm, ServiceStateNew)
    catch
        ?STACKTRACE(ErrorType, Error, ErrorStackTrace)
            ?LOG_ERROR("aspect ~tp ~tp ~tp~n~tp",
                       [Aspect, ErrorType, Error, ErrorStackTrace]),
            {ok, ServiceState}
    end;
aspects_terminate_before([F | L],
                         Reason, TimeoutTerm, ServiceState) ->
    try {ok, _} = F(Reason, TimeoutTerm, ServiceState) of
        {ok, ServiceStateNew} ->
            aspects_terminate_before(L, Reason, TimeoutTerm, ServiceStateNew)
    catch
        ?STACKTRACE(ErrorType, Error, ErrorStackTrace)
            ?LOG_ERROR("aspect ~tp ~tp ~tp~n~tp",
                       [F, ErrorType, Error, ErrorStackTrace]),
            {ok, ServiceState}
    end.

return_null_response('cloudi_service_send_async',
                     Name, Pattern, Timeout, TransId, Source) ->
    Source ! {'cloudi_service_return_async',
              Name, Pattern, <<>>, <<>>,
              Timeout, TransId, Source},
    ok;
return_null_response('cloudi_service_send_sync',
                     Name, Pattern, Timeout, TransId, Source) ->
    Source ! {'cloudi_service_return_sync',
              Name, Pattern, <<>>, <<>>,
              Timeout, TransId, Source},
    ok.

return_null_response(_, _, _, Timeout, _, _, ResponseTimeoutImmediateMax)
    when Timeout < ResponseTimeoutImmediateMax ->
    ok;
return_null_response('cloudi_service_send_async',
                     Name, Pattern, Timeout, TransId, Source, _) ->
    Source ! {'cloudi_service_return_async',
              Name, Pattern, <<>>, <<>>,
              Timeout, TransId, Source},
    ok;
return_null_response('cloudi_service_send_sync',
                     Name, Pattern, Timeout, TransId, Source, _) ->
    Source ! {'cloudi_service_return_sync',
              Name, Pattern, <<>>, <<>>,
              Timeout, TransId, Source},
    ok.

