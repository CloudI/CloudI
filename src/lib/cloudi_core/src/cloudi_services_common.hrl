%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% CloudI Services Fuctions Common to Both Internal and External Services
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2013, Michael Truog <mjtruog at gmail dot com>
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%% 
%%%     * Redistributions of source code must retain the above copyright
%%%       notice, this list of conditions and the following disclaimer.
%%%     * Redistributions in binary form must reproduce the above copyright
%%%       notice, this list of conditions and the following disclaimer in
%%%       the documentation and/or other materials provided with the
%%%       distribution.
%%%     * All advertising materials mentioning features or use of this
%%%       software must display the following acknowledgment:
%%%         This product includes software developed by Michael Truog
%%%     * The name of the author may not be used to endorse or promote
%%%       products derived from this software without specific prior
%%%       written permission
%%% 
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
%%% DAMAGE.
%%%
%%%------------------------------------------------------------------------

% When using the state record within this file, only the state elements
% that are common among cloudi_services_internal.erl and
% cloudi_services_external.erl may be used (to avoid GC delays)

-compile({nowarn_unused_function,
          [{duo_recv_timeout_start, 5},
           {recv_async_select_random, 1},
           {recv_async_select_oldest, 1}]}).

-define(CATCH_EXIT(F),
        try F catch exit:{Reason, _} -> {error, Reason} end).

destination_allowed([], _, _) ->
    false;

destination_allowed(_, undefined, undefined) ->
    true;

destination_allowed(Name, undefined, DestAllow) ->
    case cloudi_x_trie:find_match(Name, DestAllow) of
        {ok, _, _} ->
            true;
        error ->
            false
    end;

destination_allowed(Name, DestDeny, undefined) ->
    case cloudi_x_trie:find_match(Name, DestDeny) of
        {ok, _, _} ->
            false;
        error ->
            true
    end;

destination_allowed(Name, DestDeny, DestAllow) ->
    case cloudi_x_trie:find_match(Name, DestDeny) of
        {ok, _, _} ->
            false;
        error ->
            case cloudi_x_trie:find_match(Name, DestAllow) of
                {ok, _, _} ->
                    true;
                error ->
                    false
            end
    end.

destination_refresh_first(DestRefresh,
                          #config_service_options{
                              dest_refresh_start = Delay,
                              scope = Scope})
    when (DestRefresh =:= lazy_closest orelse
          DestRefresh =:= lazy_furthest orelse
          DestRefresh =:= lazy_random orelse
          DestRefresh =:= lazy_local orelse
          DestRefresh =:= lazy_remote orelse
          DestRefresh =:= lazy_newest orelse
          DestRefresh =:= lazy_oldest) ->
    cloudi_x_cpg_data:get_groups(Scope, Delay);

destination_refresh_first(DestRefresh, _)
    when (DestRefresh =:= immediate_closest orelse
          DestRefresh =:= immediate_furthest orelse
          DestRefresh =:= immediate_random orelse
          DestRefresh =:= immediate_local orelse
          DestRefresh =:= immediate_remote orelse
          DestRefresh =:= immediate_newest orelse
          DestRefresh =:= immediate_oldest) ->
    ok;

destination_refresh_first(none, _) ->
    ok.

destination_refresh_start(DestRefresh,
                          #config_service_options{
                              dest_refresh_delay = Delay,
                              scope = Scope})
    when (DestRefresh =:= lazy_closest orelse
          DestRefresh =:= lazy_furthest orelse
          DestRefresh =:= lazy_random orelse
          DestRefresh =:= lazy_local orelse
          DestRefresh =:= lazy_remote orelse
          DestRefresh =:= lazy_newest orelse
          DestRefresh =:= lazy_oldest) ->
    cloudi_x_cpg_data:get_groups(Scope, Delay);

destination_refresh_start(DestRefresh, _)
    when (DestRefresh =:= immediate_closest orelse
          DestRefresh =:= immediate_furthest orelse
          DestRefresh =:= immediate_random orelse
          DestRefresh =:= immediate_local orelse
          DestRefresh =:= immediate_remote orelse
          DestRefresh =:= immediate_newest orelse
          DestRefresh =:= immediate_oldest) ->
    ok;

destination_refresh_start(none, _) ->
    ok.

destination_get(lazy_closest, _, Name, Pid, Groups, _)
    when is_list(Name) ->
    cloudi_x_cpg_data:get_closest_pid(Name, Pid, Groups);

destination_get(lazy_furthest, _, Name, Pid, Groups, _)
    when is_list(Name) ->
    cloudi_x_cpg_data:get_furthest_pid(Name, Pid, Groups);

destination_get(lazy_random, _, Name, Pid, Groups, _)
    when is_list(Name) ->
    cloudi_x_cpg_data:get_random_pid(Name, Pid, Groups);

destination_get(lazy_local, _, Name, Pid, Groups, _)
    when is_list(Name) ->
    cloudi_x_cpg_data:get_local_pid(Name, Pid, Groups);

destination_get(lazy_remote, _, Name, Pid, Groups, _)
    when is_list(Name) ->
    cloudi_x_cpg_data:get_remote_pid(Name, Pid, Groups);

destination_get(lazy_newest, _, Name, Pid, Groups, _)
    when is_list(Name) ->
    cloudi_x_cpg_data:get_newest_pid(Name, Pid, Groups);

destination_get(lazy_oldest, _, Name, Pid, Groups, _)
    when is_list(Name) ->
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

destination_get(immediate_closest, Scope, Name, Pid, _, Timeout)
    when is_list(Name) ->
    ?CATCH_EXIT(cloudi_x_cpg:get_closest_pid(Scope, Name, Pid, Timeout));

destination_get(immediate_furthest, Scope, Name, Pid, _, Timeout)
    when is_list(Name) ->
    ?CATCH_EXIT(cloudi_x_cpg:get_furthest_pid(Scope, Name, Pid, Timeout));

destination_get(immediate_random, Scope, Name, Pid, _, Timeout)
    when is_list(Name) ->
    ?CATCH_EXIT(cloudi_x_cpg:get_random_pid(Scope, Name, Pid, Timeout));

destination_get(immediate_local, Scope, Name, Pid, _, Timeout)
    when is_list(Name) ->
    ?CATCH_EXIT(cloudi_x_cpg:get_local_pid(Scope, Name, Pid, Timeout));

destination_get(immediate_remote, Scope, Name, Pid, _, Timeout)
    when is_list(Name) ->
    ?CATCH_EXIT(cloudi_x_cpg:get_remote_pid(Scope, Name, Pid, Timeout));

destination_get(immediate_newest, Scope, Name, Pid, _, Timeout)
    when is_list(Name) ->
    ?CATCH_EXIT(cloudi_x_cpg:get_newest_pid(Scope, Name, Pid, Timeout));

destination_get(immediate_oldest, Scope, Name, Pid, _, Timeout)
    when is_list(Name) ->
    ?CATCH_EXIT(cloudi_x_cpg:get_oldest_pid(Scope, Name, Pid, Timeout));

destination_get(DestRefresh, _, _, _, _, _) ->
    ?LOG_ERROR("unable to send with invalid destination refresh: ~p",
               [DestRefresh]),
    erlang:exit(badarg).

destination_all(DestRefresh, _, Name, Pid, Groups, _)
    when is_list(Name),
         (DestRefresh =:= lazy_closest orelse
          DestRefresh =:= lazy_furthest orelse
          DestRefresh =:= lazy_random orelse
          DestRefresh =:= lazy_newest orelse
          DestRefresh =:= lazy_oldest) ->
    cloudi_x_cpg_data:get_members(Name, Pid, Groups);

destination_all(lazy_local, _, Name, Pid, Groups, _)
    when is_list(Name) ->
    cloudi_x_cpg_data:get_local_members(Name, Pid, Groups);

destination_all(lazy_remote, _, Name, Pid, Groups, _)
    when is_list(Name) ->
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
    when is_list(Name),
         (DestRefresh =:= immediate_closest orelse
          DestRefresh =:= immediate_furthest orelse
          DestRefresh =:= immediate_random orelse
          DestRefresh =:= immediate_newest orelse
          DestRefresh =:= immediate_oldest) ->
    ?CATCH_EXIT(cloudi_x_cpg:get_members(Scope, Name, Pid, Timeout));

destination_all(immediate_local, Scope, Name, Pid, _, Timeout)
    when is_list(Name) ->
    ?CATCH_EXIT(cloudi_x_cpg:get_local_members(Scope, Name, Pid, Timeout));

destination_all(immediate_remote, Scope, Name, Pid, _, Timeout)
    when is_list(Name) ->
    ?CATCH_EXIT(cloudi_x_cpg:get_remote_members(Scope, Name, Pid, Timeout));

destination_all(DestRefresh, _, _, _, _, _) ->
    ?LOG_ERROR("unable to send with invalid destination refresh: ~p",
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
         Timeout > RequestTimeoutImmediateMax ->
    NewSendTimeoutMonitors = case dict:find(Pid, SendTimeoutMonitors) of
        {ok, {MonitorRef, TransIdList}} ->
            dict:store(Pid,
                       {MonitorRef,
                        lists:umerge(TransIdList, [TransId])},
                       SendTimeoutMonitors);
        error ->
            MonitorRef = erlang:monitor(process, Pid),
            dict:store(Pid, {MonitorRef, [TransId]}, SendTimeoutMonitors)
    end,
    State#state{
        send_timeouts = dict:store(TransId,
            {passive, Pid,
             erlang:send_after(Timeout, Dispatcher,
                               {'cloudi_service_send_async_timeout', TransId})},
            SendTimeouts),
        send_timeout_monitors = NewSendTimeoutMonitors};

send_async_timeout_start(Timeout, TransId, _Pid,
                         #state{dispatcher = Dispatcher,
                                send_timeouts = SendTimeouts} = State)
    when is_integer(Timeout), is_binary(TransId) ->
    State#state{
        send_timeouts = dict:store(TransId,
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
         Timeout > RequestTimeoutImmediateMax ->
    NewSendTimeoutMonitors = case dict:find(Pid, SendTimeoutMonitors) of
        {ok, {MonitorRef, TransIdList}} ->
            dict:store(Pid,
                       {MonitorRef,
                        lists:umerge(TransIdList, [TransId])},
                       SendTimeoutMonitors);
        error ->
            MonitorRef = erlang:monitor(process, Pid),
            dict:store(Pid, {MonitorRef, [TransId]}, SendTimeoutMonitors)
    end,
    State#state{
        send_timeouts = dict:store(TransId,
            {Client, Pid,
             erlang:send_after(Timeout, Dispatcher,
                               {'cloudi_service_send_sync_timeout', TransId})},
            SendTimeouts),
        send_timeout_monitors = NewSendTimeoutMonitors};

send_sync_timeout_start(Timeout, TransId, _Pid, Client,
                        #state{dispatcher = Dispatcher,
                               send_timeouts = SendTimeouts} = State)
    when is_integer(Timeout), is_binary(TransId) ->
    State#state{
        send_timeouts = dict:store(TransId,
            {Client, undefined,
             erlang:send_after(Timeout, Dispatcher,
                               {'cloudi_service_send_sync_timeout', TransId})},
            SendTimeouts)}.

send_timeout_end(TransId, Pid,
                 #state{send_timeouts = SendTimeouts,
                        send_timeout_monitors = SendTimeoutMonitors} = State)
    when is_binary(TransId) ->
    NewSendTimeoutMonitors = if
        is_pid(Pid) ->
            case dict:find(Pid, SendTimeoutMonitors) of
                {ok, {MonitorRef, [TransId]}} ->
                    erlang:demonitor(MonitorRef),
                    dict:erase(Pid, SendTimeoutMonitors);
                {ok, {MonitorRef, TransIdList}} ->
                    dict:store(Pid,
                               {MonitorRef,
                                lists:delete(TransId, TransIdList)},
                               SendTimeoutMonitors);
                error ->
                    SendTimeoutMonitors
            end;
        Pid =:= undefined ->
            SendTimeoutMonitors
    end,
    State#state{send_timeouts = dict:erase(TransId, SendTimeouts),
                send_timeout_monitors = NewSendTimeoutMonitors}.

send_timeout_dead(Pid,
                  #state{dispatcher = Dispatcher,
                         send_timeouts = SendTimeouts,
                         send_timeout_monitors =
                             SendTimeoutMonitors} = State)
    when is_pid(Pid) ->
    NewSendTimeouts = case dict:find(Pid, SendTimeoutMonitors) of
        {ok, {_MonitorRef, TransIdList}} ->
            lists:foldl(fun(TransId, D) ->
                case dict:find(TransId, D) of
                    {ok, {Type, _, Tref}}
                    when Type =:= active; Type =:= passive ->
                        case erlang:cancel_timer(Tref) of
                            false ->
                                ok;
                            _ ->
                                Dispatcher !
                                    {'cloudi_service_send_async_timeout',
                                     TransId}
                        end,
                        dict:store(TransId, {Type, undefined, Tref}, D);
                    {ok, {Client, _, Tref}} ->
                        case erlang:cancel_timer(Tref) of
                            false ->
                                ok;
                            _ ->
                                Dispatcher !
                                    {'cloudi_service_send_sync_timeout',
                                     TransId}
                        end,
                        dict:store(TransId, {Client, undefined, Tref}, D);
                    error ->
                        D
                end
            end, SendTimeouts, TransIdList);
        error ->
            SendTimeouts
    end,
    State#state{send_timeouts = NewSendTimeouts,
                send_timeout_monitors = dict:erase(Pid, SendTimeoutMonitors)}.

recv_timeout_start(Timeout, Priority, TransId, T,
                   #state{dispatcher = Dispatcher,
                          recv_timeouts = RecvTimeouts,
                          queued = Queue} = State)
    when is_integer(Timeout), is_integer(Priority), is_binary(TransId) ->
    State#state{
        recv_timeouts = dict:store(TransId,
            erlang:send_after(Timeout, Dispatcher,
                {'cloudi_service_recv_timeout', Priority, TransId}),
            RecvTimeouts),
        queued = cloudi_x_pqueue4:in(T, Priority, Queue)}.

duo_recv_timeout_start(Timeout, Priority, TransId, T,
                       #state_duo{duo_mode_pid = DuoModePid,
                                  recv_timeouts = RecvTimeouts,
                                  queued = Queue} = State)
    when is_integer(Timeout), is_integer(Priority), is_binary(TransId) ->
    State#state_duo{
        recv_timeouts = dict:store(TransId,
            erlang:send_after(Timeout, DuoModePid,
                {'cloudi_service_recv_timeout', Priority, TransId}),
            RecvTimeouts),
        queued = cloudi_x_pqueue4:in(T, Priority, Queue)}.

async_response_timeout_start(_, _, 0, _, State) ->
    State;

async_response_timeout_start(ResponseInfo, Response, Timeout, TransId,
                             #state{dispatcher = Dispatcher,
                                    async_responses = AsyncResponses} = State)
    when is_integer(Timeout), is_binary(TransId) ->
    erlang:send_after(Timeout, Dispatcher,
                      {'cloudi_service_recv_async_timeout', TransId}),
    State#state{async_responses = dict:store(TransId,
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

check_init(#config_service_options{
               monkey_latency = false,
               monkey_chaos = false} = ConfigOptions) ->
    ConfigOptions;
check_init(#config_service_options{
               monkey_latency = MonkeyLatency,
               monkey_chaos = MonkeyChaos} = ConfigOptions) ->
    NewMonkeyLatency = if
        MonkeyLatency =/= false ->
            cloudi_runtime_testing:monkey_latency_init(MonkeyLatency);
        true ->
            MonkeyLatency
    end,
    NewMonkeyChaos = if
        MonkeyChaos =/= false ->
            cloudi_runtime_testing:monkey_chaos_init(MonkeyChaos);
        true ->
            MonkeyChaos
    end,
    ConfigOptions#config_service_options{
        monkey_latency = NewMonkeyLatency,
        monkey_chaos = NewMonkeyChaos}.

check_incoming(#config_service_options{
                   monkey_latency = false,
                   monkey_chaos = false} = ConfigOptions) ->
    ConfigOptions;
check_incoming(#config_service_options{
                   monkey_latency = MonkeyLatency,
                   monkey_chaos = MonkeyChaos} = ConfigOptions) ->
    NewMonkeyLatency = if
        MonkeyLatency =/= false ->
            cloudi_runtime_testing:monkey_latency_check(MonkeyLatency);
        true ->
            MonkeyLatency
    end,
    NewMonkeyChaos = if
        MonkeyChaos =/= false ->
            cloudi_runtime_testing:monkey_chaos_check(MonkeyChaos);
        true ->
            MonkeyChaos
    end,
    ConfigOptions#config_service_options{
        monkey_latency = NewMonkeyLatency,
        monkey_chaos = NewMonkeyChaos}.

