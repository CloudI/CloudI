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
%%% Copyright (c) 2015, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2015 Michael Truog
%%% @version 1.5.1 {@date} {@time}
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
         services_init/5,
         services_terminate/1,
         services_update/6,
         nodes_update/3]).

-include("cloudi_service_monitoring.hrl").
-include("cloudi_service_monitoring_cloudi.hrl").

% monitoring config for aspects_init_after
-define(ETS_CONFIG, cloudi_service_monitoring_cloudi).
% service pid to service_id global lookup
-define(ETS_PID2ID, cloudi_service_monitoring_cloudi_pids).
% aspect function ref to service_id global lookup
-define(ETS_REF2ID, cloudi_service_monitoring_cloudi_refs).

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
        % modifications to ?ETS_PID2ID
        ets_insert = [] :: list({pid(), metric_name(), module()}),
        ets_delete = [] :: list(pid()),
        % metrics data
        count_internal = 0 :: non_neg_integer(),
        count_external = 0 :: non_neg_integer(),
        concurrency_internal = 0 :: non_neg_integer(),
        concurrency_external = 0 :: non_neg_integer(),
        scopes = dict:new() :: dict_proxy(atom(), #scope_data{}),
        metrics = [] :: metric_list()
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

update_or_create(Service, Type, Name, Value, Options) ->
    try ets:lookup(?ETS_PID2ID, Service) of
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

services_init(undefined, ProcessInfo0, _, _, _) ->
    ProcessInfo0;
services_init(Interval, ProcessInfo0, MetricPrefix, UseAspectsOnly, Driver) ->
    {ok, Services} = services_state(Interval * 1000),
    ets:new(?ETS_CONFIG,
            [set, protected, named_table,
             {read_concurrency, true}]),
    true = ets:insert(?ETS_CONFIG, [{init, MetricPrefix, Driver}]),
    ets:new(?ETS_PID2ID,
            [set, public, named_table,
             {read_concurrency, true}]),
    ets:new(?ETS_REF2ID,
            [set, public, named_table,
             {read_concurrency, true}]),
    {InsertsN,
     ProcessInfoN} = cloudi_x_key2value:fold1(fun(ID, Pids, #service{}, A) ->
        ServiceId = service_id(ID),
        lists:foldl(fun(Pid, {Inserts1, ProcessInfo1}) ->
            Inserts2 = if
                UseAspectsOnly =:= true ->
                    Inserts1;
                UseAspectsOnly =:= false ->
                    [{Pid, MetricPrefix ++ [ServiceId], Driver} | Inserts1]
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
            true = ets:insert(?ETS_PID2ID, InsertsN)
    end,
    ProcessInfoN.

services_terminate(undefined) ->
    ok;
services_terminate(_) ->
    true = ets:delete(?ETS_CONFIG),
    true = ets:delete(?ETS_PID2ID),
    true = ets:delete(?ETS_REF2ID),
    ok.

services_update(undefined, ServicesNew, ProcessInfo0,
                MetricPrefix, UseAspectsOnly, Driver) ->
    ChangesN = cloudi_x_key2value:
               fold1(fun(ID, PidsNew,
                         #service{} = Service,
                         #service_data{process_info = ProcessInfo1,
                                       ets_insert = Inserts0,
                                       metrics = Metrics0} = Changes1) ->
        ServiceId = service_id(ID),
        {Inserts3,
         ProcessInfo3} = lists:foldl(fun(PidNew, {Inserts1, ProcessInfo2}) ->
            Inserts2 = if
                UseAspectsOnly =:= true ->
                    Inserts1;
                UseAspectsOnly =:= false ->
                    [{PidNew, MetricPrefix ++ [ServiceId], Driver} | Inserts1]
            end,
            {Inserts2, process_info_store(PidNew, ProcessInfo2)}
        end, {Inserts0, ProcessInfo1}, PidsNew),
        {Metrics1,
         ProcessInfo4} = service_metrics(PidsNew, ProcessInfo3, Service,
                                         ServicesNew, [ServiceId]),
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
            true = ets:delete_all_objects(?ETS_PID2ID),
            true = ets:insert(?ETS_PID2ID, InsertsN)
    end,
    {services_metrics(CountInternal, CountExternal,
                      ConcurrencyInternal, ConcurrencyExternal,
                      Scopes) ++ MetricsN,
     ProcessInfoN};
services_update(ServicesOld, ServicesNew, ProcessInfo0,
                MetricPrefix, UseAspectsOnly, Driver) ->
    ChangesN = cloudi_x_key2value:
               fold1(fun(ID, PidsNew,
                         #service{} = Service,
                         #service_data{process_info = ProcessInfo1,
                                       ets_insert = Inserts0,
                                       ets_delete = Deletes0,
                                       metrics = Metrics0} = Changes1) ->
        ServiceId = service_id(ID),
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
                                    [{PidNew, MetricPrefix ++ [ServiceId],
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
                            [{PidNew, MetricPrefix ++ [ServiceId],
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
                                         ServicesNew, [ServiceId]),
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
            true = ets:insert(?ETS_PID2ID, InsertsN),
            _ = ets:select_delete(?ETS_PID2ID,
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

service_process_metrics({undefined, _, _}, _, ProcessInfo0, _, _) ->
    {[], ProcessInfo0};
service_process_metrics({ServiceMemory, ServiceMessages, ServiceReductionsNow},
                        internal, ProcessInfo0, Pid, MetricPrefix) ->
    try sys:get_state(Pid, ?SERVICE_PROCESS_TIMEOUT) of
        State -> % gen_server/proc_lib
            {QueuedRequests,
             QueuedRequestsSize0,
             WordSize,
             QueuedInfo,
             Memory,
             Messages,
             ReductionsNow,
             ProcessInfoN} = case erlang:tuple_size(State) of
                29 -> % duo_mode == false
                    state = erlang:element(1, State),
                    {erlang:element(8, State),   % queued
                     erlang:element(9, State),   % queued_size
                     erlang:element(10, State),  % queued_word_size
                     erlang:element(11, State),  % queued_info
                     ServiceMemory,
                     ServiceMessages,
                     ServiceReductionsNow,
                     ProcessInfo0};
                14 -> % duo_mode == true
                    state_duo = erlang:element(1, State),
                    Dispatcher =  erlang:element(12, State),
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
                    {erlang:element(5, State),   % queued
                     erlang:element(6, State),   % queued_size
                     erlang:element(7, State),   % queued_word_size
                     erlang:element(8, State),   % queued_info
                     MemoryValue,
                     MessagesValue,
                     ReductionsNowValue,
                     ProcessInfo2}
            end,
            QueuedRequestsLength = cloudi_x_pqueue4:len(QueuedRequests),
            QueuedRequestsSizeN = if
                QueuedRequestsLength > 0, QueuedRequestsSize0 == 0 ->
                    cloudi_x_erlang_term:byte_size(QueuedRequests,
                                                   WordSize);
                true ->
                    QueuedRequestsSize0
            end,
            QueuedInfoLength = queue:len(QueuedInfo),
            Metrics = if
                ReductionsNow =:= undefined ->
                    [];
                is_integer(ReductionsNow) ->
                    [metric(spiral, MetricPrefix ++ [reductions],
                            ReductionsNow)]
            end,
            {[metric(gauge, MetricPrefix ++ [memory],
                     Memory),
              metric(gauge, MetricPrefix ++ [message_queue_len],
                     Messages),
              metric(gauge, MetricPrefix ++ [queued_requests],
                     QueuedRequestsLength),
              metric(gauge, MetricPrefix ++ [queued_requests_size],
                     QueuedRequestsSizeN),
              metric(gauge, MetricPrefix ++ [queued_info],
                     QueuedInfoLength) | Metrics],
             ProcessInfoN}
    catch
        exit:{_, _} ->
            {[], ProcessInfo0}
    end;
service_process_metrics({ServiceMemory, ServiceMessages, ServiceReductionsNow},
                        external, ProcessInfo0, Pid, MetricPrefix) ->
    try sys:get_state(Pid, ?SERVICE_PROCESS_TIMEOUT) of
        {_, State} -> % gen_fsm
            38 = erlang:tuple_size(State),
            state = erlang:element(1, State),
            QueuedRequests = erlang:element(8, State),       % queued
            QueuedRequestsSize0 = erlang:element(9, State),  % queued_size
            WordSize = erlang:element(10, State),            % queued_word_size
            QueuedRequestsLength = cloudi_x_pqueue4:len(QueuedRequests),
            QueuedRequestsSizeN = if
                QueuedRequestsLength > 0, QueuedRequestsSize0 == 0 ->
                    cloudi_x_erlang_term:byte_size(QueuedRequests,
                                                   WordSize);
                true ->
                    QueuedRequestsSize0
            end,
            Metrics = if
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
              metric(gauge, MetricPrefix ++ [queued_requests],
                     QueuedRequestsLength),
              metric(gauge, MetricPrefix ++ [queued_requests_size],
                     QueuedRequestsSizeN) | Metrics],
             ProcessInfo0}
    catch
        exit:{_, _} ->
            {[], ProcessInfo0}
    end.

service_metrics_pid_internal([], Metrics, ProcessInfo0, _, _) ->
    {Metrics, ProcessInfo0};
service_metrics_pid_internal([Pid | Pids], Metrics, ProcessInfo0,
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
                                             ProcessMetricPrefix),
    service_metrics_pid_internal(Pids, MetricsNew ++ Metrics, ProcessInfoN,
                                 Services, MetricPrefix).

service_metrics_pid_external([], Metrics, ProcessInfo0, _, _, _) ->
    {Metrics, ProcessInfo0};
service_metrics_pid_external([Pid | Pids], Metrics, ProcessInfo0,
                             ThreadIndexLookup, Services, MetricPrefix) ->
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
                                             ThreadMetricPrefix),
    service_metrics_pid_external(Pids, MetricsNew ++ Metrics, ProcessInfoN,
                                 dict:store(ProcessIndex,
                                            ThreadIndex + 1, ThreadIndexLookup),
                                 Services, MetricPrefix).

service_metrics_pid(internal, Pids, ProcessInfo,
                    Services, MetricPrefix) ->
    service_metrics_pid_internal(Pids, [], ProcessInfo,
                                 Services, MetricPrefix);
service_metrics_pid(external, Pids, ProcessInfo,
                    Services, MetricPrefix) ->
    service_metrics_pid_external(Pids, [], ProcessInfo, dict:new(),
                                 Services, MetricPrefix).

service_metrics(Pids, ProcessInfo0,
                #service{service_f = ServiceF,
                         count_process = CountProcess,
                         count_thread = CountThread},
                Services, MetricPrefix) ->
    Metrics0 = [metric(gauge, MetricPrefix ++ [concurrency],
                       CountProcess * CountThread)],
    {Metrics1,
     ProcessInfoN} = service_metrics_pid(service_type(ServiceF),
                                         Pids, ProcessInfo0,
                                         Services, MetricPrefix),
    {Metrics0 ++ Metrics1, ProcessInfoN}.

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
            case erlang:process_info(Pid, dictionary) of
                {dictionary, Dictionary} ->
                    case lists:keyfind(?SERVICE_ID_PDICT_KEY, 1, Dictionary) of
                        false ->
                            undefined;
                        {_, ID} ->
                            ServiceId = service_id(ID),
                            PidObject = {Pid,
                                         MetricPrefix ++ [ServiceId], Driver},
                            true = ets:insert(?ETS_PID2ID, PidObject),
                            PidObject
                    end;
                undefined ->
                    undefined
            end
    catch
        error:badarg ->
            undefined
    end.

aspect_pid_to_service_id(Pid) ->
    try ets:lookup(?ETS_PID2ID, Pid) of
        [] ->
            undefined;
        [{_, MetricPrefix, _}] ->
            lists:last(MetricPrefix)
    catch
        error:badarg ->
            undefined
    end.

aspect_pid_to_object() ->
    try ets:lookup(?ETS_PID2ID, self()) of
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
    try ets:lookup(?ETS_REF2ID, Ref) of
        [] ->
            case ets:lookup(?ETS_PID2ID, cloudi_service:self(Dispatcher)) of
                [] ->
                    undefined;
                [PidObject] ->
                    RefObject = {Ref, PidObject},
                    true = ets:insert(?ETS_REF2ID, RefObject),
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
                        ok;
                    ServiceId ->
                        update(spiral, MetricPrefix ++ [request, ServiceId],
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
                        ok;
                    ServiceId ->
                        update(spiral, MetricPrefix ++ [request, ServiceId],
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

service_id(ID) ->
    cloudi_x_uuid:uuid_to_string(ID, list_nodash).

update(Type, Name, Value, Driver) ->
    cloudi_service_monitoring:update(Type, Name, Value, Driver).

