%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Monitoring Metrics==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2015-2016, Michael Truog <mjtruog at gmail dot com>
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
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2015-2016 Michael Truog
%%% @version 1.5.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_monitoring_cloudi).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([update_or_create/5,
         aspect_init_after_internal/0,
         aspect_init_after_external/0,
         aspect_request_before_internal/0,
         aspect_request_before_external/0,
         aspect_request_after_internal/0,
         aspect_request_after_external/0,
         aspect_info_before_internal/0,
         aspect_info_after_internal/0,
         aspect_terminate_before_internal/0,
         aspect_terminate_before_external/0,
         services_state/1,
         basic_update/1,
         services_init/6,
         services_terminate/1,
         services_update/8,
         nodes_update/3]).

-include("cloudi_service_monitoring.hrl").
-include("cloudi_service_monitoring_cloudi.hrl").

-type pid_object() :: {pid(), metric_name(), module()}.

% monitoring config for aspects_init_after
-define(ETS_CONFIG, cloudi_service_monitoring_cloudi).
% service pid to pid_object() global lookup
-define(ETS_PID2METRIC, cloudi_service_monitoring_cloudi_pids).
% aspect function ref to pid_object() global lookup
-define(ETS_REF2METRIC, cloudi_service_monitoring_cloudi_refs).

% timeout for getting state from a service process
-define(SERVICE_PROCESS_TIMEOUT, 250). % milliseconds

-type metric_name() :: cloudi_service_monitoring:metric_name().
-type metric_list() :: cloudi_service_monitoring:metric_list().

-record(scope_data,
    {
        count_internal = 0 :: non_neg_integer(),
        count_external = 0 :: non_neg_integer(),
        concurrency_internal = 0 :: non_neg_integer(),
        concurrency_external = 0 :: non_neg_integer()
    }).

-record(service_data,
    {
        process_info :: dict_proxy(pid(), #process_info{}),
        % modifications to ?ETS_PID2METRIC
        ets_insert = [] :: list(pid_object()),
        ets_delete = [] :: list(pid()),
        % metrics data
        count_internal = 0 :: non_neg_integer(),
        count_external = 0 :: non_neg_integer(),
        concurrency_internal = 0 :: non_neg_integer(),
        concurrency_external = 0 :: non_neg_integer(),
        scopes = dict:new() :: dict_proxy(atom(), #scope_data{}),
        metrics = [] :: metric_list()
    }).

% cloudi_core_i_service_internal and cloudi_core_i_service_external
% use maps when available/stable
-ifdef(ERLANG_OTP_VERSION_16).
-else.
-ifdef(ERLANG_OTP_VERSION_17).
-else.
-define(ERLANG_OTP_VERSION_18_FEATURES, true).
-endif.
-endif.
-ifdef(ERLANG_OTP_VERSION_18_FEATURES).
-define(MAP_SIZE(M),         maps:size(M)).
-else.
-define(MAP_SIZE(M),         dict:size(M)).
-endif.

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

update_or_create(undefined, Type, Name, Value, []) ->
    try ets:lookup(?ETS_CONFIG, init) of
        [] ->
            {error, invalid_state};
        [{init, _, Driver}] ->
            cloudi_service_monitoring:update(Type,
                                             Name,
                                             Value, Driver)
    catch
        error:badarg ->
            {error, invalid_state}
    end;
update_or_create(Service, Type, Name, Value, Options) ->
    try ets:lookup(?ETS_PID2METRIC, Service) of
        [] ->
            {error, not_service};
        [{_, MetricPrefix, Driver}] ->
            [ServiceMetric] = cloudi_proplists:
                              take_values([{service_metric, false}],
                                          Options),
            if
                ServiceMetric =:= true ->
                    cloudi_service_monitoring:update(Type,
                                                     MetricPrefix ++ Name,
                                                     Value, Driver);
                ServiceMetric =:= false ->
                    cloudi_service_monitoring:update(Type,
                                                     Name,
                                                     Value, Driver)
            end
    catch
        error:badarg ->
            {error, invalid_state}
    end.

aspect_init_after_internal() ->
    aspect_init_after_internal_f().

aspect_init_after_external() ->
    aspect_init_after_external_f().

aspect_request_before_internal() ->
    aspect_request_before_internal_f(erlang:make_ref()).

aspect_request_before_external() ->
    aspect_request_before_external_f().

aspect_request_after_internal() ->
    aspect_request_after_internal_f(erlang:make_ref()).

aspect_request_after_external() ->
    aspect_request_after_external_f().

aspect_info_before_internal() ->
    aspect_info_before_internal_f(erlang:make_ref()).

aspect_info_after_internal() ->
    aspect_info_after_internal_f(erlang:make_ref()).

aspect_terminate_before_internal() ->
    aspect_terminate_before_internal_f().

aspect_terminate_before_external() ->
    aspect_terminate_before_external_f().

basic_update(ProcessInfo0) ->
    LoggingPid = whereis(cloudi_core_i_logger),
    {Logging,
     ProcessInfo1} = process_info_update(LoggingPid, ProcessInfo0),
    ConfiguratorPid = whereis(cloudi_core_i_configurator),
    {Configurator,
     ProcessInfo2} = process_info_update(ConfiguratorPid, ProcessInfo1),
    ServicesMonitorPid = whereis(cloudi_core_i_services_monitor),
    {ServicesMonitor,
     ProcessInfoN} = process_info_update(ServicesMonitorPid, ProcessInfo2),
    {process_info_metrics(Logging,
                          [logging]) ++
     process_info_metrics(Configurator,
                          [configurator]) ++
     process_info_metrics(ServicesMonitor,
                          [services, monitor]),
     ProcessInfoN}.

services_state(Timeout) ->
    try sys:get_state(cloudi_core_i_services_monitor, Timeout) of
        State ->
            3 = erlang:tuple_size(State),
            state = erlang:element(1, State),
            {ok, erlang:element(2, State)}
    catch
        exit:{Reason, _} ->
            {error, Reason}
    end.

services_init(undefined, ProcessInfo0, _, _, _, _) ->
    ProcessInfo0;
services_init(Interval, ProcessInfo0,
              MetricPrefix, UseAspectsOnly, Driver, EnvironmentLookup) ->
    {ok, Services} = services_state(Interval * 1000),
    ets:new(?ETS_CONFIG,
            [set, public, named_table,
             {read_concurrency, true}]),
    true = ets:insert(?ETS_CONFIG, [{init, MetricPrefix, Driver}]),
    ets:new(?ETS_PID2METRIC,
            [set, public, named_table,
             {read_concurrency, true}]),
    ets:new(?ETS_REF2METRIC,
            [set, public, named_table,
             {read_concurrency, true}]),
    {InsertsN,
     ProcessInfoN} = cloudi_x_key2value:fold1(fun(_ID, Pids,
                                                  #service{} = Service, A) ->
        ServiceMetricId = service_metric_id_from_service(Service,
                                                         EnvironmentLookup),
        lists:foldl(fun(Pid, {Inserts1, ProcessInfo1}) ->
            Inserts2 = if
                UseAspectsOnly =:= true ->
                    Inserts1;
                UseAspectsOnly =:= false ->
                    [{Pid, MetricPrefix ++ ServiceMetricId, Driver} | Inserts1]
            end,
            {Inserts2, process_info_store(Pid, ProcessInfo1)}
        end, A, Pids)
    end, {[], ProcessInfo0}, Services),
    if
        UseAspectsOnly =:= true ->
            % rely completely on aspects_init_after to add the
            % service pid object to be used for service metrics
            ok;
        UseAspectsOnly =:= false ->
            true = ets:insert(?ETS_PID2METRIC, InsertsN)
    end,
    ProcessInfoN.

services_terminate(undefined) ->
    ok;
services_terminate(_) ->
    true = ets:delete(?ETS_CONFIG),
    true = ets:delete(?ETS_PID2METRIC),
    true = ets:delete(?ETS_REF2METRIC),
    ok.

services_update(undefined, ServicesNew, ProcessInfo0, QueuedEmptySize,
                MetricPrefix, UseAspectsOnly, Driver, EnvironmentLookup) ->
    ChangesN = cloudi_x_key2value:
               fold1(fun(_ID, PidsNew,
                         #service{} = Service,
                         #service_data{process_info = ProcessInfo1,
                                       ets_insert = Inserts0,
                                       metrics = Metrics0} = Changes1) ->
            ServiceMetricId = service_metric_id_from_service(Service,
                                                             EnvironmentLookup),
            {Inserts3,
             ProcessInfo3} = lists:foldl(fun(PidNew, {Inserts1, ProcessInfo2}) ->
                Inserts2 = if
                    UseAspectsOnly =:= true ->
                        Inserts1;
                    UseAspectsOnly =:= false ->
                        [{PidNew, MetricPrefix ++ ServiceMetricId, Driver} |
                         Inserts1]
                end,
                {Inserts2, process_info_store(PidNew, ProcessInfo2)}
            end, {Inserts0, ProcessInfo1}, PidsNew),
            {Metrics1,
             ProcessInfo4} = service_metrics(PidsNew, ProcessInfo3, Service,
                                             QueuedEmptySize,
                                             ServicesNew, ServiceMetricId),
            services_accumulate(Service,
                                Changes1#service_data{process_info = ProcessInfo4,
                                                      ets_insert = Inserts3,
                                                      metrics = Metrics1 ++
                                                                Metrics0})
        end, #service_data{process_info = ProcessInfo0}, ServicesNew),
        #service_data{process_info = ProcessInfoN,
                      ets_insert = InsertsN,
                      count_internal = CountInternal,
                      count_external = CountExternal,
                      concurrency_internal = ConcurrencyInternal,
                      concurrency_external = ConcurrencyExternal,
                      scopes = Scopes,
                      metrics = MetricsN} = ChangesN,
        if
            UseAspectsOnly =:= true ->
                ok;
            UseAspectsOnly =:= false ->
                true = ets:delete_all_objects(?ETS_PID2METRIC),
                true = ets:insert(?ETS_PID2METRIC, InsertsN)
        end,
        {services_metrics(CountInternal, CountExternal,
                          ConcurrencyInternal, ConcurrencyExternal,
                          Scopes) ++ MetricsN,
         ProcessInfoN};
    services_update(ServicesOld, ServicesNew, ProcessInfo0, QueuedEmptySize,
                    MetricPrefix, UseAspectsOnly, Driver, EnvironmentLookup) ->
        ChangesN = cloudi_x_key2value:
                   fold1(fun(ID, PidsNew,
                             #service{} = Service,
                             #service_data{process_info = ProcessInfo1,
                                           ets_insert = Inserts0,
                                           ets_delete = Deletes0,
                                           metrics = Metrics0} = Changes1) ->
            ServiceMetricId = service_metric_id_from_service(Service,
                                                         EnvironmentLookup),
        Changes2 = case cloudi_x_key2value:find1(ID, ServicesOld) of
            {ok, {PidsNew, #service{}}} ->
                ProcessInfo3 = lists:foldl(fun(PidNew, ProcessInfo2) ->
                    process_info_store(PidNew, ProcessInfo2)
                end, ProcessInfo1, PidsNew),
                Changes1#service_data{process_info = ProcessInfo3};
            {ok, {PidsOld, #service{}}} ->
                {Inserts3,
                 ProcessInfo3} = lists:foldl(fun(PidNew,
                                                 {Inserts1, ProcessInfo2}) ->
                    Inserts2 = case lists:member(PidNew, PidsOld) of
                        true ->
                            Inserts1;
                        false ->
                            if
                                UseAspectsOnly =:= true ->
                                    Inserts1;
                                UseAspectsOnly =:= false ->
                                    [{PidNew, MetricPrefix ++ ServiceMetricId,
                                      Driver} | Inserts1]
                            end
                    end,
                    {Inserts2, process_info_store(PidNew, ProcessInfo2)}
                end, {Inserts0, ProcessInfo1}, PidsNew),
                {Deletes3,
                 ProcessInfo5} = lists:foldl(fun(PidOld,
                                                 {Deletes1, ProcessInfo4}) ->
                    case lists:member(PidOld, PidsNew) of
                        true ->
                            {Deletes1, ProcessInfo4};
                        false ->
                            Deletes2 = if
                                UseAspectsOnly =:= true ->
                                    Deletes1;
                                UseAspectsOnly =:= false ->
                                    [PidOld | Deletes1]
                            end,
                            {Deletes2,
                             process_info_erase(PidOld, ProcessInfo4)}
                    end
                end, {Deletes0, ProcessInfo3}, PidsOld),
                Changes1#service_data{process_info = ProcessInfo5,
                                      ets_insert = Inserts3,
                                      ets_delete = Deletes3};
            error ->
                {Inserts3,
                 ProcessInfo3} = lists:foldl(fun(PidNew,
                                                 {Inserts1, ProcessInfo2}) ->
                    Inserts2 = if
                        UseAspectsOnly =:= true ->
                            Inserts1;
                        UseAspectsOnly =:= false ->
                            [{PidNew, MetricPrefix ++ ServiceMetricId,
                              Driver} | Inserts1]
                    end,
                    {Inserts2, process_info_store(PidNew, ProcessInfo2)}
                end, {Inserts0, ProcessInfo1}, PidsNew),
                Changes1#service_data{process_info = ProcessInfo3,
                                      ets_insert = Inserts3}
        end,
        #service_data{process_info = ProcessInfo6} = Changes2,
        {Metrics1,
         ProcessInfo7} = service_metrics(PidsNew, ProcessInfo6, Service,
                                         QueuedEmptySize,
                                         ServicesNew, ServiceMetricId),
        services_accumulate(Service,
                            Changes2#service_data{process_info = ProcessInfo7,
                                                  metrics = Metrics1 ++
                                                            Metrics0})
    end, #service_data{process_info = ProcessInfo0}, ServicesNew),
    #service_data{process_info = ProcessInfoN,
                  ets_insert = InsertsN,
                  ets_delete = DeletesN,
                  count_internal = CountInternal,
                  count_external = CountExternal,
                  concurrency_internal = ConcurrencyInternal,
                  concurrency_external = ConcurrencyExternal,
                  scopes = Scopes,
                  metrics = MetricsN} = ChangesN,
    if
        UseAspectsOnly =:= true ->
            ok;
        UseAspectsOnly =:= false ->
            true = ets:insert(?ETS_PID2METRIC, InsertsN),
            _ = ets:select_delete(?ETS_PID2METRIC,
                                  [{{PidOld, '_', '_'},[],[true]}
                                   || PidOld <- DeletesN])
    end,
    {services_metrics(CountInternal, CountExternal,
                      ConcurrencyInternal, ConcurrencyExternal,
                      Scopes) ++ MetricsN,
     ProcessInfoN}.

nodes_update(NodesVisible, NodesHidden, NodesAll) ->
    [metric(gauge, [visible], NodesVisible),
     metric(gauge, [hidden], NodesHidden),
     metric(gauge, [all], NodesAll)].

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

scopes_accumulate_internal(Scope, Concurrency, Scopes) ->
    dict:update(Scope,
                fun(#scope_data{count_internal = Count,
                                concurrency_internal = ConcurrencySum} = V) ->
        V#scope_data{count_internal = Count + 1,
                     concurrency_internal = ConcurrencySum + Concurrency}
    end, #scope_data{count_internal = 1,
                     concurrency_internal = Concurrency}, Scopes).

scopes_accumulate_external(Scope, Concurrency, Scopes) ->
    dict:update(Scope,
                fun(#scope_data{count_external = Count,
                                concurrency_external = ConcurrencySum} = V) ->
        V#scope_data{count_external = Count + 1,
                     concurrency_external = ConcurrencySum + Concurrency}
    end, #scope_data{count_external = 1,
                     concurrency_external = Concurrency}, Scopes).

services_accumulate(#service{service_f = ServiceF,
                             count_process = CountProcess,
                             count_thread = CountThread,
                             scope = Scope},
                    #service_data{count_internal = CountInternal,
                                  count_external = CountExternal,
                                  concurrency_internal = ConcurrencyInternal,
                                  concurrency_external = ConcurrencyExternal,
                                  scopes = Scopes} = Changes) ->
    Concurrency = CountProcess * CountThread,
    case service_type(ServiceF) of
        internal ->
            NewScopes = scopes_accumulate_internal(Scope, Concurrency, Scopes),
            Changes#service_data{count_internal = CountInternal + 1,
                                 concurrency_internal = ConcurrencyInternal +
                                                        Concurrency,
                                 scopes = NewScopes};
        external ->
            NewScopes = scopes_accumulate_external(Scope, Concurrency, Scopes),
            Changes#service_data{count_external = CountExternal + 1,
                                 concurrency_external = ConcurrencyExternal +
                                                        Concurrency,
                                 scopes = NewScopes}
    end.

service_process_metrics({undefined, _, _}, _, ProcessInfo0, _, _, _) ->
    {[], ProcessInfo0};
service_process_metrics({ServiceMemory, ServiceMessages, ServiceReductionsNow},
                        internal, ProcessInfo0, Pid,
                        QueuedEmptySize, MetricPrefix) ->
    case service_state(Pid) of
        {ok, State} -> % gen_server/proc_lib
            {Outgoing,
             QueuedRequests,
             QueuedRequestsSize0,
             WordSize,
             QueuedInfo,
             Memory,
             Messages,
             ReductionsNow,
             ProcessInfoN} = case erlang:tuple_size(State) of
                30 -> % duo_mode == false
                    state = erlang:element(1, State),
                    {?MAP_SIZE(erlang:element(3, State)),  % send_timeouts
                     erlang:element(9, State),             % queued
                     erlang:element(10, State),            % queued_size
                     erlang:element(11, State),            % queued_word_size
                     erlang:element(12, State),            % queued_info
                     ServiceMemory,
                     ServiceMessages,
                     ServiceReductionsNow,
                     ProcessInfo0};
                15 -> % duo_mode == true
                    state_duo = erlang:element(1, State),
                    Dispatcher =  erlang:element(13, State),
                    DispatcherOutgoing = case service_state(Dispatcher) of
                        {ok, DispatcherState} -> % gen_server/proc_lib
                            30 = erlang:tuple_size(DispatcherState),
                            state = erlang:element(1, DispatcherState),
                            ?MAP_SIZE(erlang:element(3, DispatcherState));
                        {error, _} ->
                            undefined
                    end,
                    {MemoryValue,
                     MessagesValue,
                     ReductionsNowValue,
                     ProcessInfo2} = case process_info_update(Dispatcher,
                                                              ProcessInfo0) of
                        {{undefined, _, _}, ProcessInfo1} ->
                            {ServiceMemory,
                             ServiceMessages,
                             ServiceReductionsNow,
                             ProcessInfo1};
                        {{DispatcherMemory,
                          DispatcherMessages,
                          DispatcherReductionsNow}, ProcessInfo1}
                        when ServiceReductionsNow =:= undefined;
                             DispatcherReductionsNow =:= undefined ->
                            {ServiceMemory + DispatcherMemory,
                             ServiceMessages + DispatcherMessages,
                             undefined,
                             ProcessInfo1};
                        {{DispatcherMemory,
                          DispatcherMessages,
                          DispatcherReductionsNow}, ProcessInfo1} ->
                            {ServiceMemory + DispatcherMemory,
                             ServiceMessages + DispatcherMessages,
                             ServiceReductionsNow + DispatcherReductionsNow,
                             ProcessInfo1}
                    end,
                    {DispatcherOutgoing,
                     erlang:element(6, State),   % queued
                     erlang:element(7, State),   % queued_size
                     erlang:element(8, State),   % queued_word_size
                     erlang:element(9, State),   % queued_info
                     MemoryValue,
                     MessagesValue,
                     ReductionsNowValue,
                     ProcessInfo2}
            end,
            QueuedRequestsLength = cloudi_x_pqueue4:len(QueuedRequests),
            QueuedRequestsSizeN = if
                QueuedRequestsLength > 0, QueuedRequestsSize0 == 0 ->
                    cloudi_x_erlang_term:byte_size(QueuedRequests,
                                                   WordSize) -
                    QueuedEmptySize * WordSize;
                true ->
                    QueuedRequestsSize0
            end,
            QueuedInfoLength = queue:len(QueuedInfo),
            Metrics0 = if
                Outgoing =:= undefined ->
                    [];
                is_integer(Outgoing) ->
                    [metric(gauge, MetricPrefix ++ [outgoing_requests],
                            Outgoing)]
            end,
            MetricsN = if
                ReductionsNow =:= undefined ->
                    Metrics0;
                is_integer(ReductionsNow) ->
                    [metric(spiral, MetricPrefix ++ [reductions],
                            ReductionsNow) | Metrics0]
            end,
            {[metric(gauge, MetricPrefix ++ [memory],
                     Memory),
              metric(gauge, MetricPrefix ++ [message_queue_len],
                     Messages),
              metric(gauge, MetricPrefix ++ [incoming_requests],
                     QueuedRequestsLength),
              metric(gauge, MetricPrefix ++ [incoming_requests_size],
                     QueuedRequestsSizeN),
              metric(gauge, MetricPrefix ++ [incoming_info],
                     QueuedInfoLength) | MetricsN],
             ProcessInfoN};
        {error, _} ->
            {[], ProcessInfo0}
    end;
service_process_metrics({ServiceMemory, ServiceMessages, ServiceReductionsNow},
                        external, ProcessInfo0, Pid,
                        QueuedEmptySize, MetricPrefix) ->
    case service_state(Pid) of
        {ok, {_, State}} -> % gen_fsm
            39 = erlang:tuple_size(State),
            state = erlang:element(1, State),
            Outgoing = ?MAP_SIZE(erlang:element(3, State)),  % send_timeouts
            QueuedRequests = erlang:element(9, State),       % queued
            QueuedRequestsSize0 = erlang:element(10, State), % queued_size
            WordSize = erlang:element(11, State),            % queued_word_size
            QueuedRequestsLength = cloudi_x_pqueue4:len(QueuedRequests),
            QueuedRequestsSizeN = if
                QueuedRequestsLength > 0, QueuedRequestsSize0 == 0 ->
                    cloudi_x_erlang_term:byte_size(QueuedRequests,
                                                   WordSize) -
                    QueuedEmptySize * WordSize;
                true ->
                    QueuedRequestsSize0
            end,
            MetricsN = if
                ServiceReductionsNow =:= undefined ->
                    [];
                is_integer(ServiceReductionsNow) ->
                    [metric(spiral, MetricPrefix ++ [reductions],
                            ServiceReductionsNow)]
            end,
            {[metric(gauge, MetricPrefix ++ [memory],
                     ServiceMemory),
              metric(gauge, MetricPrefix ++ [message_queue_len],
                     ServiceMessages),
              metric(gauge, MetricPrefix ++ [outgoing_requests],
                     Outgoing),
              metric(gauge, MetricPrefix ++ [incoming_requests],
                     QueuedRequestsLength),
              metric(gauge, MetricPrefix ++ [incoming_requests_size],
                     QueuedRequestsSizeN) | MetricsN],
             ProcessInfo0};
        {error, _} ->
            {[], ProcessInfo0}
    end.

service_metrics_pid_internal([], Metrics, ProcessInfo0, _, _, _) ->
    {Metrics, ProcessInfo0};
service_metrics_pid_internal([Pid | Pids], Metrics, ProcessInfo0,
                             QueuedEmptySize,
                             Services, MetricPrefix) ->
    {[_],
     #service{process_index = ProcessIndex}} =
        cloudi_x_key2value:fetch2(Pid, Services),
    ProcessMetricPrefix = MetricPrefix ++
                          [process, erlang:integer_to_list(ProcessIndex)],
    {MetricsNew,
     ProcessInfoN} = service_process_metrics(process_info_find(Pid,
                                                               ProcessInfo0),
                                             internal, ProcessInfo0, Pid,
                                             QueuedEmptySize,
                                             ProcessMetricPrefix),
    service_metrics_pid_internal(Pids, MetricsNew ++ Metrics, ProcessInfoN,
                                 QueuedEmptySize,
                                 Services, MetricPrefix).

service_metrics_pid_external([], Metrics, ProcessInfo0, _, _, _, _) ->
    {Metrics, ProcessInfo0};
service_metrics_pid_external([Pid | Pids], Metrics, ProcessInfo0,
                             ThreadIndexLookup, QueuedEmptySize,
                             Services, MetricPrefix) ->
    {[_],
     #service{process_index = ProcessIndex}} =
        cloudi_x_key2value:fetch2(Pid, Services),
    ThreadIndex = case dict:find(ProcessIndex, ThreadIndexLookup) of
        {ok, ThreadIndexNext} ->
            ThreadIndexNext;
        error ->
            0
    end,
    ThreadMetricPrefix = MetricPrefix ++
                         [process, erlang:integer_to_list(ProcessIndex),
                          thread, erlang:integer_to_list(ThreadIndex)],
    {MetricsNew,
     ProcessInfoN} = service_process_metrics(process_info_find(Pid,
                                                               ProcessInfo0),
                                             external, ProcessInfo0, Pid,
                                             QueuedEmptySize,
                                             ThreadMetricPrefix),
    service_metrics_pid_external(Pids, MetricsNew ++ Metrics, ProcessInfoN,
                                 dict:store(ProcessIndex,
                                            ThreadIndex + 1, ThreadIndexLookup),
                                 QueuedEmptySize,
                                 Services, MetricPrefix).

service_metrics_pid(internal, Pids, ProcessInfo,
                    QueuedEmptySize, Services, MetricPrefix) ->
    service_metrics_pid_internal(Pids, [], ProcessInfo,
                                 QueuedEmptySize, Services, MetricPrefix);
service_metrics_pid(external, Pids, ProcessInfo,
                    QueuedEmptySize, Services, MetricPrefix) ->
    service_metrics_pid_external(Pids, [], ProcessInfo, dict:new(),
                                 QueuedEmptySize, Services, MetricPrefix).

service_metrics(Pids, ProcessInfo0,
                #service{service_f = ServiceF,
                         count_process = CountProcess,
                         count_thread = CountThread},
                QueuedEmptySize, Services, MetricPrefix) ->
    Metrics0 = [metric(gauge, MetricPrefix ++ [concurrency],
                       CountProcess * CountThread)],
    {Metrics1,
     ProcessInfoN} = service_metrics_pid(service_type(ServiceF),
                                         Pids, ProcessInfo0, QueuedEmptySize,
                                         Services, MetricPrefix),
    {Metrics0 ++ Metrics1, ProcessInfoN}.

service_state(Pid) ->
    try sys:get_state(Pid, ?SERVICE_PROCESS_TIMEOUT) of
        State ->
            {ok, State}
    catch
        exit:{Reason, _} ->
            {error, Reason}
    end.

services_metrics(CountInternal, CountExternal,
                 ConcurrencyInternal, ConcurrencyExternal, Scopes) ->
    dict:fold(fun(Scope, 
                  #scope_data{
                      count_internal = ScopeCountInternal,
                      count_external = ScopeCountExternal,
                      concurrency_internal = ScopeConcurrencyInternal,
                      concurrency_external = ScopeConcurrencyExternal},
                  ScopeMetrics) ->
        ScopeName = ?SCOPE_FORMAT(Scope),
        [metric(gauge, [scopes, ScopeName, concurrency],
                ScopeConcurrencyInternal + ScopeConcurrencyExternal),
         metric(gauge, [scopes, ScopeName, count],
                ScopeCountInternal + ScopeCountExternal),
         metric(gauge, [scopes, ScopeName, internal, concurrency],
                ScopeConcurrencyInternal),
         metric(gauge, [scopes, ScopeName, internal, count],
                ScopeCountInternal),
         metric(gauge, [scopes, ScopeName, external, concurrency],
                ScopeConcurrencyExternal),
         metric(gauge, [scopes, ScopeName, external, count],
                ScopeCountExternal) | ScopeMetrics]
    end,
    [metric(gauge, [scopes, count],
            dict:size(Scopes)),
     metric(gauge, [concurrency],
            ConcurrencyInternal + ConcurrencyExternal),
     metric(gauge, [count],
            CountInternal + CountExternal),
     metric(gauge, [internal, concurrency],
            ConcurrencyInternal),
     metric(gauge, [internal, count],
            CountInternal),
     metric(gauge, [external, concurrency],
            ConcurrencyExternal),
     metric(gauge, [external, count],
            CountExternal)],
    Scopes).

process_info_store(Pid, ProcessInfo) ->
    case erlang:process_info(Pid, [memory, message_queue_len, reductions]) of
        [{memory, MemoryNew},
         {message_queue_len, MessagesNew},
         {reductions, ReductionsNew}] ->
            case dict:find(Pid, ProcessInfo) of
                {ok, #process_info{reductions = ReductionsOld}} ->
                    ReductionsNow = if
                        ReductionsOld =:= undefined ->
                            undefined;
                        is_integer(ReductionsOld) ->
                            ReductionsNew - ReductionsOld
                    end,
                    InfoNew = #process_info{memory = MemoryNew,
                                            message_queue_len = MessagesNew,
                                            reductions = ReductionsNew,
                                            reductions_now = ReductionsNow},
                    dict:store(Pid, InfoNew, ProcessInfo);
                error ->
                    InfoNew = #process_info{memory = MemoryNew,
                                            message_queue_len = MessagesNew,
                                            reductions = ReductionsNew},
                    dict:store(Pid, InfoNew, ProcessInfo)
            end;
        undefined ->
            dict:store(Pid, #process_info{}, ProcessInfo)
    end.

process_info_update(Pid, ProcessInfo) ->
    case erlang:process_info(Pid, [memory, message_queue_len, reductions]) of
        [{memory, MemoryNew},
         {message_queue_len, MessagesNew},
         {reductions, ReductionsNew}] ->
            case dict:find(Pid, ProcessInfo) of
                {ok, #process_info{reductions = ReductionsOld}} ->
                    ReductionsNow = if
                        ReductionsOld =:= undefined ->
                            undefined;
                        is_integer(ReductionsOld) ->
                            ReductionsNew - ReductionsOld
                    end,
                    InfoNew = #process_info{memory = MemoryNew,
                                            message_queue_len = MessagesNew,
                                            reductions = ReductionsNew,
                                            reductions_now = ReductionsNow},
                    {{MemoryNew, MessagesNew, ReductionsNow},
                     dict:store(Pid, InfoNew, ProcessInfo)};
                error ->
                    InfoNew = #process_info{memory = MemoryNew,
                                            message_queue_len = MessagesNew,
                                            reductions = ReductionsNew},
                    {{MemoryNew, MessagesNew, undefined},
                     dict:store(Pid, InfoNew, ProcessInfo)}
            end;
        undefined ->
            {{undefined, undefined, undefined},
             dict:store(Pid, #process_info{}, ProcessInfo)}
    end.

process_info_find(Pid, ProcessInfo) ->
    case dict:find(Pid, ProcessInfo) of
        {ok, #process_info{memory = Memory,
                           message_queue_len = Messages,
                           reductions_now = ReductionsNow}} ->
            {Memory, Messages, ReductionsNow};
        error ->
            {undefined, undefined, undefined}
    end.

process_info_erase(Pid, ProcessInfo) ->
    dict:erase(Pid, ProcessInfo).

process_info_metrics({Memory, Messages, ReductionsNow}, MetricPrefix) ->
    L0 = [],
    L1 = if
        Memory =:= undefined ->
            L0;
        is_integer(Memory) ->
            [metric(gauge, MetricPrefix ++ [memory],
                    Memory) | L0]
    end,
    L2 = if
        Messages =:= undefined ->
            L1;
        is_integer(Messages) ->
            [metric(gauge, MetricPrefix ++ [message_queue_len],
                    Messages) | L1]
    end,
    LN = if
        ReductionsNow =:= undefined ->
            L2;
        is_integer(ReductionsNow) ->
            [metric(spiral, MetricPrefix ++ [reductions],
                    ReductionsNow) | L2]
    end,
    LN.

service_type(start_internal) ->
    internal;
service_type(start_external) ->
    external.

metric(spiral, [_ | _] = Name, Value) ->
    {spiral, Name, Value};
metric(gauge, [_ | _] = Name, Value) ->
    {gauge, Name, Value}.

aspect_init() ->
    try ets:lookup(?ETS_CONFIG, init) of
        [] ->
            undefined;
        [{init, MetricPrefix, Driver}] ->
            Pid = self(),
            case service_metric_id_from_pid(Pid) of
                undefined ->
                    undefined;
                ServiceMetricId ->
                    PidObject = {Pid, MetricPrefix ++ ServiceMetricId, Driver},
                    true = ets:insert(?ETS_PID2METRIC, PidObject),
                    PidObject
            end
    catch
        error:badarg ->
            undefined
    end.

aspect_pid_to_service_id(Pid) ->
    try ets:lookup(?ETS_PID2METRIC, Pid) of
        [] ->
            undefined;
        [{_, MetricPrefix, _}] ->
            service_metric_id_from_metric_prefix(MetricPrefix)
    catch
        error:badarg ->
            undefined
    end.

aspect_pid_to_object() ->
    try ets:lookup(?ETS_PID2METRIC, self()) of
        [] ->
            undefined;
        [PidObject] ->
            PidObject
    catch
        error:badarg ->
            undefined
    end.

aspect_ref_to_object(Ref, Dispatcher)
    when is_reference(Ref) ->
    try ets:lookup(?ETS_REF2METRIC, Ref) of
        [] ->
            case ets:lookup(?ETS_PID2METRIC, cloudi_service:self(Dispatcher)) of
                [] ->
                    undefined;
                [PidObject] ->
                    RefObject = {Ref, PidObject},
                    true = ets:insert(?ETS_REF2METRIC, RefObject),
                    PidObject
            end;
        [{Ref, PidObject}] ->
            PidObject
    catch
        error:badarg ->
            undefined
    end.

aspect_init_after_internal_f() ->
    fun(_Args, _Prefix, _Timeout, State, _Dispatcher) ->
        case aspect_init() of
            {_, MetricPrefix, Driver} ->
                update(spiral, MetricPrefix ++ [init], 1, Driver);
            undefined ->
                ok
        end,
        {ok, State}
    end.

aspect_init_after_external_f() ->
    fun(_CommandLine, _Prefix, _Timeout, State) ->
        case aspect_init() of
            {_, MetricPrefix, Driver} ->
                update(spiral, MetricPrefix ++ [init], 1, Driver);
            undefined ->
                ok
        end,
        {ok, State}
    end.

aspect_request_before_internal_f(Ref) ->
    fun(_Type, _Name, _Pattern, _RequestInfo, _Request,
        _Timeout, _Priority, _TransId, Source, State, Dispatcher) ->
        case aspect_ref_to_object(Ref, Dispatcher) of
            {_, MetricPrefix, Driver} ->
                case aspect_pid_to_service_id(Source) of
                    undefined ->
                        update(spiral, MetricPrefix ++ [request, nonservice],
                               1, Driver);
                    ServiceMetricId ->
                        update(spiral, MetricPrefix ++ [request |
                                                        ServiceMetricId],
                               1, Driver)
                end;
            undefined ->
                ok
        end,
        {ok, State}
    end.

aspect_request_before_external_f() ->
    fun(_Type, _Name, _Pattern, _RequestInfo, _Request,
        _Timeout, _Priority, _TransId, Source, State) ->
        case aspect_pid_to_object() of
            {_, MetricPrefix, Driver} ->
                case aspect_pid_to_service_id(Source) of
                    undefined ->
                        update(spiral, MetricPrefix ++ [request, nonservice],
                               1, Driver);
                    ServiceMetricId ->
                        update(spiral, MetricPrefix ++ [request |
                                                        ServiceMetricId],
                               1, Driver)
                end;
            undefined ->
                ok
        end,
        {ok, State}
    end.

aspect_request_after_internal_f(Ref) ->
    fun(_Type, _Name, _Pattern, _RequestInfo, _Request,
        Timeout, _Priority, _TransId, _Source, _Result, State, Dispatcher) ->
        case aspect_ref_to_object(Ref, Dispatcher) of
            {_, MetricPrefix, Driver} ->
                update(histogram, MetricPrefix ++ [request, timeout],
                       Timeout, Driver);
            undefined ->
                ok
        end,
        {ok, State}
    end.

aspect_request_after_external_f() ->
    fun(_Type, _Name, _Pattern, _RequestInfo, _Request,
        Timeout, _Priority, _TransId, _Source, _Result, State) ->
        case aspect_pid_to_object() of
            {_, MetricPrefix, Driver} ->
                update(histogram, MetricPrefix ++ [request, timeout],
                       Timeout, Driver);
            undefined ->
                ok
        end,
        {ok, State}
    end.

aspect_info_before_internal_f(Ref) ->
    fun(_Request, State, Dispatcher) ->
        case aspect_ref_to_object(Ref, Dispatcher) of
            {_, MetricPrefix, Driver} ->
                update(spiral, MetricPrefix ++ [info], 1, Driver);
            undefined ->
                ok
        end,
        {ok, State}
    end.

aspect_info_after_internal_f(_Ref) ->
    fun(_Request, State, _Dispatcher) ->
        %case aspect_ref_to_object(Ref, Dispatcher) of
        %    {_, MetricPrefix, Driver} ->
        %        ok;
        %    undefined ->
        %        ok
        %end,
        {ok, State}
    end.

aspect_terminate_before_internal_f() ->
    fun(_Reason, _Timeout, State) ->
        case aspect_pid_to_object() of
            {_, MetricPrefix, Driver} ->
                update(counter, MetricPrefix ++ [terminate], 1, Driver);
            undefined ->
                ok
        end,
        {ok, State}
    end.

aspect_terminate_before_external_f() ->
    fun(_Reason, _Timeout, State) ->
        case aspect_pid_to_object() of
            {_, MetricPrefix, Driver} ->
                update(counter, MetricPrefix ++ [terminate], 1, Driver);
            undefined ->
                ok
        end,
        {ok, State}
    end.

service_metric_id_from_service(#service{service_m = cloudi_core_i_spawn,
                                        service_f = start_internal,
                                        service_a = [_, Module,
                                                     _, _, _, _, _, _,
                                                     _, _, _, _, ID]},
                               _) ->
    service_metric_id(Module, ID);
service_metric_id_from_service(#service{service_m = cloudi_core_i_spawn,
                                        service_f = start_external,
                                        service_a = [_, FileNameEnv,
                                                     _, _, _, _, _, _, _, _,
                                                     _, _, _, _, _, ID]},
                               EnvironmentLookup) ->
    FileName = cloudi_environment:transform(FileNameEnv, EnvironmentLookup),
    service_metric_id(FileName, ID).

service_metric_id_from_pid(Pid) ->
    case erlang:process_info(Pid, dictionary) of
        {dictionary, Dictionary} ->
            case lists:keyfind(?SERVICE_ID_PDICT_KEY, 1,
                               Dictionary) of
                false ->
                    undefined;
                {_, ID} ->
                    case lists:keyfind(?SERVICE_FILE_PDICT_KEY, 1,
                                       Dictionary) of
                        false ->
                            undefined;
                        {_, FileName} ->
                            service_metric_id(FileName, ID)
                    end
            end;
        undefined ->
            undefined
    end.

service_metric_id_from_metric_prefix(MetricPrefix) ->
    [Index, MetricIdName, TypeChar | _] = lists:reverse(MetricPrefix),
    [TypeChar, MetricIdName, Index].

service_metric_id(FileName, ID)
    when is_atom(FileName) ->
    ["i",
     service_metric_id_name(FileName),
     service_metric_id_index(internal, FileName, ID)];
service_metric_id(FileName, ID)
    when is_list(FileName) ->
    ["e",
     service_metric_id_name(FileName),
     service_metric_id_index(external, FileName, ID)].

service_metric_id_name(FileName)
    when is_atom(FileName) ->
    service_metric_id_name_sanitize(erlang:atom_to_list(FileName));
service_metric_id_name(FileName)
    when is_list(FileName) ->
    service_metric_id_name_sanitize(filename:basename(FileName)).

service_metric_id_name_sanitize(Name) ->
    service_metric_id_name_sanitize(Name, []).

service_metric_id_name_sanitize([], Name) ->
    lists:reverse(Name);
service_metric_id_name_sanitize([H | T], Name) ->
    if
        (H >= $a andalso H =< $z) orelse
        (H >= $A andalso H =< $Z) orelse
        (H >= $0 andalso H =< $9) orelse
        (H == $_) ->
            service_metric_id_name_sanitize(T, [H | Name]);
        true ->
            service_metric_id_name_sanitize(T, Name)
    end.

service_metric_id_index(Type, FileName, ID) ->
    Key = {Type, FileName},
    Count = try ets:lookup(?ETS_CONFIG, Key) of
        [] ->
            true = ets:insert(?ETS_CONFIG, {Key, [ID]}),
            1;
        [{_, IDList}] ->
            case cloudi_lists:index(ID, IDList) of
                undefined ->
                    IDListNew = lists:umerge(IDList, [ID]),
                    true = ets:insert(?ETS_CONFIG, {Key, IDListNew}),
                    cloudi_lists:index(ID, IDListNew);
                CountValue ->
                    CountValue
            end
                
    catch
        error:badarg ->
            1
    end,
    erlang:integer_to_list(Count - 1).

update(Type, Name, Value, Driver) ->
    cloudi_service_monitoring:update(Type, Name, Value, Driver).

