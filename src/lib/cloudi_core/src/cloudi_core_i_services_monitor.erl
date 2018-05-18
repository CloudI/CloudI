%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Services==
%%% Manage all cloudi_core_i_spawn processes with monitors and their
%%% configuration.  Perform process restarts but do not escalate failures
%%% (only log failures).
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2011-2018 Michael Truog <mjtruog at protonmail dot com>
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
%%% @author Michael Truog <mjtruog at protonmail dot com>
%%% @copyright 2011-2018 Michael Truog
%%% @version 1.7.4 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_core_i_services_monitor).
-author('mjtruog at protonmail dot com').

-behaviour(gen_server).

%% external interface
-export([start_link/0,
         monitor/13,
         initialize/1,
         shutdown/2,
         restart/2,
         update/2,
         search/2,
         status/2,
         pids/2,
         increase/5,
         decrease/5,
         terminate_kill/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("cloudi_logger.hrl").
-include("cloudi_core_i_constants.hrl").
-include("cloudi_core_i_configuration.hrl").

-define(CATCH_EXIT(F),
        try F catch exit:{Reason, _} -> {error, Reason} end).

-record(service,
    {
        service_m :: cloudi_core_i_spawn,
        service_f :: start_internal | start_external,
        service_a :: list(),
        process_index :: non_neg_integer(),
        count_process :: pos_integer(),
        count_thread :: pos_integer(),
        scope :: atom(),
        % pids is only accurate (in this record) on the pid lookup (find2)
        % due to the overwrite of #service{} for the key1 ServiceId value
        pids :: list(pid()),
        monitor :: undefined | reference(),
        time_start :: integer(),
        time_restart :: undefined | integer(),
        restart_count_total :: non_neg_integer(),
        restart_count = 0 :: non_neg_integer(),
        restart_times = [] :: list(cloudi_timestamp:seconds_monotonic()),
        timeout_term :: cloudi_service_api:timeout_terminate_milliseconds(),
        restart_delay :: tuple() | false,
        % from the supervisor behavior documentation:
        % If more than MaxR restarts occur within MaxT seconds,
        % the supervisor terminates all child processes...
        max_r :: non_neg_integer(),
        max_t :: non_neg_integer()
    }).

-record(pids_change,
    {
        count_process :: pos_integer(),
        count_increase = 0 :: non_neg_integer(),
        count_decrease = 0 :: non_neg_integer(),
        increase = 0 :: non_neg_integer(),
        decrease = 0 :: non_neg_integer(),
        rate = 0.0 :: float()
    }).

-record(state,
    {
        services = cloudi_x_key2value:new(maps) ::
            cloudi_x_key2value:
            cloudi_x_key2value(cloudi_service_api:service_id(),
                               pid(), #service{}),
        changes = #{} ::
            #{cloudi_service_api:service_id() :=
              list({increase | decrease, number(), number(), number()})}
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec monitor(M :: cloudi_core_i_spawn,
              F :: start_internal | start_external,
              A :: list(),
              ProcessIndex :: non_neg_integer(),
              CountProcess :: pos_integer(),
              CountThread :: pos_integer(),
              Scope :: atom(),
              TimeoutTerm :: cloudi_service_api:
                             timeout_terminate_value_milliseconds(),
              RestartDelay :: tuple() | false,
              MaxR :: non_neg_integer(),
              MaxT :: non_neg_integer(),
              ServiceId :: cloudi_x_uuid:cloudi_x_uuid(),
              Timeout :: infinity | pos_integer()) ->
    {ok, list(pid())} |
    {error, any()}.

monitor(M, F, A, ProcessIndex, CountProcess, CountThread, Scope,
        TimeoutTerm, RestartDelay, MaxR, MaxT, ServiceId, Timeout)
    when is_atom(M), is_atom(F), is_list(A),
         is_integer(ProcessIndex), ProcessIndex >= 0,
         is_integer(CountProcess), CountProcess > 0,
         is_integer(CountThread), CountThread > 0, is_atom(Scope),
         is_integer(TimeoutTerm),
         TimeoutTerm >= ?TIMEOUT_TERMINATE_MIN,
         TimeoutTerm =< ?TIMEOUT_TERMINATE_MAX,
         is_integer(MaxR), MaxR >= 0, is_integer(MaxT), MaxT >= 0,
         is_binary(ServiceId), byte_size(ServiceId) == 16 ->
    ?CATCH_EXIT(gen_server:call(?MODULE,
                                {monitor, M, F, A,
                                 ProcessIndex, CountProcess,
                                 CountThread, Scope,
                                 TimeoutTerm, RestartDelay, MaxR, MaxT,
                                 ServiceId},
                                Timeout)).

-spec initialize(Pids :: list(pid())) ->
    ok.

initialize([]) ->
    ok;
initialize([Pid | Pids])
    when is_pid(Pid) ->
    Pid ! initialize,
    initialize(Pids).

shutdown(ServiceId, Timeout)
    when is_binary(ServiceId), byte_size(ServiceId) == 16 ->
    ?CATCH_EXIT(gen_server:call(?MODULE,
                                {shutdown, ServiceId},
                                Timeout)).

restart(ServiceId, Timeout)
    when is_binary(ServiceId), byte_size(ServiceId) == 16 ->
    ?CATCH_EXIT(gen_server:call(?MODULE,
                                {restart, ServiceId},
                                Timeout)).

update(UpdatePlan, Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE,
                                {update, UpdatePlan},
                                Timeout)).

search([_ | _] = PidList, Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE,
                                {search, PidList},
                                Timeout)).

status([_ | _] = ServiceIdList, Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE,
                                {status, ServiceIdList},
                                Timeout)).

pids(ServiceId, Timeout)
    when is_binary(ServiceId), byte_size(ServiceId) == 16 ->
    ?CATCH_EXIT(gen_server:call(?MODULE,
                                {pids, ServiceId},
                                Timeout)).

increase(Pid, Period, RateCurrent, RateMax, CountProcessMax)
    when is_pid(Pid), is_integer(Period), is_number(RateCurrent),
         is_number(RateMax), is_number(CountProcessMax) ->
    ?CATCH_EXIT(gen_server:cast(?MODULE,
                                {increase,
                                 Pid, Period, RateCurrent,
                                 RateMax, CountProcessMax})).

decrease(Pid, Period, RateCurrent, RateMin, CountProcessMin)
    when is_pid(Pid), is_integer(Period), is_number(RateCurrent),
         is_number(RateMin), is_number(CountProcessMin) ->
    ?CATCH_EXIT(gen_server:cast(?MODULE,
                                {decrease,
                                 Pid, Period, RateCurrent,
                                 RateMin, CountProcessMin})).

terminate_kill(Pid, Reason)
    when is_pid(Pid) ->
    ?CATCH_EXIT(gen_server:cast(?MODULE,
                                {terminate_kill, Pid, Reason})).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([]) ->
    {ok, #state{}}.

handle_call({monitor, M, F, A, ProcessIndex, CountProcess, CountThread, Scope,
             TimeoutTerm, RestartDelay, MaxR, MaxT, ServiceId}, _,
            #state{services = Services} = State) ->
    TimeStart = erlang:monotonic_time(),
    TimeRestart = undefined,
    Restarts = 0,
    case erlang:apply(M, F, [ProcessIndex, CountProcess,
                             TimeStart, TimeRestart,
                             Restarts | A]) of
        {ok, Pid} when is_pid(Pid) ->
            Pids = [Pid],
            NewServices = cloudi_x_key2value:store(ServiceId, Pid,
                new_service_process(M, F, A,
                                    ProcessIndex, CountProcess,
                                    CountThread, Scope, Pids,
                                    erlang:monitor(process, Pid),
                                    TimeStart, TimeRestart, Restarts,
                                    TimeoutTerm, RestartDelay,
                                    MaxR, MaxT), Services),
            {reply, {ok, Pids}, State#state{services = NewServices}};
        {ok, [Pid | _] = Pids} = Success when is_pid(Pid) ->
            NewServices = lists:foldl(fun(P, D) ->
                cloudi_x_key2value:store(ServiceId, P,
                    new_service_process(M, F, A,
                                        ProcessIndex, CountProcess,
                                        CountThread, Scope, Pids,
                                        erlang:monitor(process, P),
                                        TimeStart, TimeRestart, Restarts,
                                        TimeoutTerm, RestartDelay,
                                        MaxR, MaxT), D)
            end, Services, Pids),
            {reply, Success, State#state{services = NewServices}};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({shutdown, ServiceId}, _,
            #state{services = Services} = State) ->
    case cloudi_x_key2value:find1(ServiceId, Services) of
        {ok, {Pids, #service{} = Service}} ->
            NewServices = terminate_service(ServiceId, Pids, undefined,
                                            Service, Services, undefined),
            {reply, {ok, Pids}, State#state{services = NewServices}};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({restart, ServiceId}, _,
            #state{services = Services} = State) ->
    case cloudi_x_key2value:find1(ServiceId, Services) of
        {ok, {Pids, _}} ->
            lists:foreach(fun(P) ->
                erlang:exit(P, restart)
            end, Pids),
            {reply, ok, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({update,
             #config_service_update{
                 uuids = ServiceIdList} = UpdatePlan}, _,
            #state{services = Services} = State) ->
    case service_ids_pids(ServiceIdList, Services) of
        {ok, PidList0} ->
            case update_start(PidList0, UpdatePlan) of
                {[], PidListN} ->
                    Results0 = update_before(UpdatePlan),
                    ResultsN = update_now(PidListN, Results0, true),
                    {ResultsSuccess,
                     ResultsError} = update_results(ResultsN),
                    UpdateSuccess = (ResultsError == []),
                    NewServices = update_after(UpdateSuccess,
                                               PidListN, ResultsSuccess,
                                               UpdatePlan, Services),
                    if
                        UpdateSuccess =:= true ->
                            {reply, ok, State#state{services = NewServices}};
                        UpdateSuccess =:= false ->
                            {reply, {error, ResultsError}, State}
                    end;
                {Results0, PidListN} ->
                    ResultsN = update_now(PidListN, Results0, false),
                    {[], ResultsError} = update_results(ResultsN),
                    {reply, {error, ResultsError}, State}
            end;
        {error, Reason} ->
            {reply, {error, [Reason]}, State}
    end;

handle_call({search, PidList}, _,
            #state{services = Services} = State) ->
    ServiceIdList = lists:foldl(fun(Pid, L) ->
        case cloudi_x_key2value:find2(Pid, Services) of
            {ok, {[ServiceId], _}} ->
                case lists:member(ServiceId, L) of
                    true ->
                        L;
                    false ->
                        [ServiceId | L]
                end;
            error ->
                L
        end
    end, [], PidList),
    {reply, {ok, lists:reverse(ServiceIdList)}, State};

handle_call({status, ServiceIdList}, _,
            #state{services = Services} = State) ->
    TimeNow = erlang:monotonic_time(),
    {reply, service_ids_status(ServiceIdList, TimeNow, Services), State};

handle_call({pids, ServiceId}, _,
            #state{services = Services} = State) ->
    case cloudi_x_key2value:find1(ServiceId, Services) of
        {ok, {PidList, #service{scope = Scope}}} ->
            {reply, {ok, Scope, PidList}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(Request, _, State) ->
    {stop, cloudi_string:format("Unknown call \"~w\"", [Request]),
     error, State}.

handle_cast({Direction,
             Pid, Period, RateCurrent,
             RateLimit, CountProcessLimit},
            #state{services = Services,
                   changes = Changes} = State)
    when (Direction =:= increase);
         (Direction =:= decrease) ->
    case cloudi_x_key2value:find2(Pid, Services) of
        {ok, {[ServiceId], _}} ->
            Entry = {Direction, RateCurrent, RateLimit, CountProcessLimit},
            NewChangeList = case maps:find(ServiceId, Changes) of
                {ok, ChangeList} ->
                    [Entry | ChangeList];
                error ->
                    erlang:send_after(Period * 1000, self(),
                                      {changes, ServiceId}),
                    [Entry]
            end,
            {noreply, State#state{changes = maps:put(ServiceId,
                                                     NewChangeList,
                                                     Changes)}};
        error ->
            % discard old change
            {noreply, State}
    end;

handle_cast({terminate_kill, Pid, Reason},
            #state{services = Services} = State) ->
    case cloudi_x_key2value:find2(Pid, Services) of
        {ok, {[ServiceId], #service{} = Service}} ->
            ok = terminate_kill_enforce(self(), Pid, Reason,
                                        ServiceId, Service),
            {noreply, State};
        error ->
            {noreply, State}
    end;

handle_cast(Request, State) ->
    {stop, cloudi_string:format("Unknown cast \"~w\"", [Request]), State}.

handle_info({changes, ServiceId},
            #state{services = Services,
                   changes = Changes} = State) ->
    NewChanges = maps:remove(ServiceId, Changes),
    case cloudi_x_key2value:find1(ServiceId, Services) of
        {ok, {Pids, #service{count_thread = CountThread}}} ->
            ChangeList = maps:get(ServiceId, Changes),
            CountProcessCurrent = erlang:round(erlang:length(Pids) /
                                               CountThread),
            {I, Rate} = pids_change(ChangeList, CountProcessCurrent),
            NewServices = if
                I == 0 ->
                    ?LOG_TRACE("count_process_dynamic(~p):~n "
                               "constant ~p for ~p requests/second",
                               [service_id(ServiceId), CountProcessCurrent,
                                erlang:round(Rate * 10) / 10]),
                    Services;
                I > 0 ->
                    pids_increase(I, Pids, CountProcessCurrent, Rate,
                                  ServiceId, Services);
                I < 0 ->
                    pids_decrease(I, Pids, CountProcessCurrent, Rate,
                                  ServiceId, Services)
            end,
            {noreply, State#state{services = NewServices,
                                  changes = NewChanges}};
        error ->
            {noreply, State#state{changes = NewChanges}}
    end;

handle_info({'DOWN', _MonitorRef, 'process', Pid, shutdown},
            #state{services = Services} = State) ->
    case cloudi_x_key2value:find2(Pid, Services) of
        {ok, {[ServiceId], #service{pids = Pids} = Service}} ->
            ?LOG_INFO_SYNC("Service pid ~p shutdown~n ~p",
                           [Pid, service_id(ServiceId)]),
            NewServices = terminate_service(ServiceId, Pids, undefined,
                                            Service, Services, Pid),
            cloudi_core_i_configurator:service_dead(ServiceId),
            {noreply, State#state{services = NewServices}};
        error ->
            % Service pid has already terminated
            {noreply, State}
    end;

handle_info({'DOWN', _MonitorRef, 'process', Pid,
             {shutdown, cloudi_count_process_dynamic_terminate}},
            #state{services = Services} = State) ->
    case cloudi_x_key2value:find2(Pid, Services) of
        {ok, {[ServiceId], #service{}}} ->
            ?LOG_INFO("Service pid ~p terminated (count_process_dynamic)~n ~p",
                      [Pid, service_id(ServiceId)]),
            NewServices = cloudi_x_key2value:erase(ServiceId, Pid, Services),
            {noreply, State#state{services = NewServices}};
        error ->
            % Service pid has already terminated
            {noreply, State}
    end;

handle_info({'DOWN', _MonitorRef, 'process', Pid, {shutdown, Reason}},
            #state{services = Services} = State) ->
    case cloudi_x_key2value:find2(Pid, Services) of
        {ok, {[ServiceId], #service{pids = Pids} = Service}} ->
            ?LOG_INFO_SYNC("Service pid ~p shutdown (~p)~n ~p",
                           [Pid, Reason, service_id(ServiceId)]),
            NewServices = terminate_service(ServiceId, Pids, Reason,
                                            Service, Services, Pid),
            cloudi_core_i_configurator:service_dead(ServiceId),
            {noreply, State#state{services = NewServices}};
        error ->
            % Service pid has already terminated
            {noreply, State}
    end;

handle_info({'DOWN', _MonitorRef, 'process', Pid, Info},
            #state{services = Services} = State) ->
    case cloudi_x_key2value:find2(Pid, Services) of
        {ok, {[ServiceId], #service{} = Service}} ->
            ?LOG_WARN("Service pid ~p error: ~p~n ~p",
                      [Pid, Info, service_id(ServiceId)]),
            case restart(Service, Services, State, ServiceId, Pid) of
                {true, NewState} ->
                    {noreply, NewState};
                {false, NewState} ->
                    cloudi_core_i_configurator:service_dead(ServiceId),
                    {noreply, NewState}
            end;
        error ->
            % Service pid has already terminated
            {noreply, State}
    end;

handle_info({restart_stage2, Service, ServiceId, OldPid},
            #state{services = Services} = State) ->
    case restart_stage2(Service, Services, State, ServiceId, OldPid) of
        {true, NewState} ->
            {noreply, NewState};
        {false, NewState} ->
            cloudi_core_i_configurator:service_dead(ServiceId),
            {noreply, NewState}
    end;

handle_info({restart_stage3, Service, ServiceId, OldPid},
            #state{services = Services} = State) ->
    case restart_stage3(Service, Services, State, ServiceId, OldPid) of
        {true, NewState} ->
            {noreply, NewState};
        {false, NewState} ->
            cloudi_core_i_configurator:service_dead(ServiceId),
            {noreply, NewState}
    end;

handle_info({kill, Pid, Reason, ServiceId, Service}, State) ->
    ok = terminate_kill_enforce_now(Pid, Reason, ServiceId, Service),
    {noreply, State};

handle_info({ReplyRef, _}, State) when is_reference(ReplyRef) ->
    % gen_server:call/3 had a timeout exception that was caught but the
    % reply arrived later and must be discarded
    {noreply, State};

handle_info(Request, State) ->
    {stop, cloudi_string:format("Unknown info \"~w\"", [Request]), State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

restart(Service, Services, State, ServiceId, OldPid) ->
    restart_stage1(Service, Services, State, ServiceId, OldPid).

restart_stage1(#service{pids = Pids} = Service,
               Services, State, ServiceId, OldPid) ->
    NewServices = terminate_service(ServiceId, Pids, undefined,
                                    Service, Services, OldPid),
    restart_stage2(Service#service{pids = [],
                                   monitor = undefined},
                   NewServices, State, ServiceId, OldPid).

restart_stage2_async(Service, ServiceId, OldPid) ->
    self() ! {restart_stage2, Service, ServiceId, OldPid},
    ok.

restart_stage3_async(Delay, Service, ServiceId, OldPid) ->
    erlang:send_after(Delay, self(),
                      {restart_stage3, Service, ServiceId, OldPid}),
    ok.

restart_stage2(#service{restart_count = 0,
                        max_r = 0},
               Services, State, ServiceId, OldPid) ->
    % no restarts allowed
    ?LOG_WARN_SYNC("max restarts (MaxR = 0) ~p~n ~p",
                   [OldPid, service_id(ServiceId)]),
    {false, State#state{services = Services}};

restart_stage2(#service{restart_times = RestartTimes,
                        restart_delay = RestartDelay,
                        max_t = MaxT} = Service,
               Services, State, ServiceId, OldPid) ->
    case cloudi_core_i_rate_based_configuration:
         restart_delay_value(RestartTimes, MaxT, RestartDelay) of
        false ->
            restart_stage3(Service, Services, State, ServiceId, OldPid);
        {NewRestartCount,
         NewRestartTimes,
         0} ->
            restart_stage3(Service#service{restart_count = NewRestartCount,
                                           restart_times = NewRestartTimes},
                           Services, State, ServiceId, OldPid);
        {NewRestartCount,
         NewRestartTimes,
         Delay} when Delay > 0 andalso Delay =< ?TIMEOUT_MAX_ERLANG ->
            restart_stage3_async(Delay,
                                 Service#service{
                                     restart_count = NewRestartCount,
                                     restart_times = NewRestartTimes},
                                 ServiceId, OldPid),
            {true, State#state{services = Services}}
    end.

restart_stage3(#service{service_m = M,
                        service_f = F,
                        service_a = A,
                        process_index = ProcessIndex,
                        count_process = CountProcess,
                        time_start = TimeStart,
                        restart_count_total = Restarts,
                        restart_count = 0,
                        restart_times = []} = Service,
               Services, State, ServiceId, OldPid) ->
    % first restart
    TimeRestart = erlang:monotonic_time(),
    SecondsNow = cloudi_timestamp:convert(TimeRestart, native, second),
    NewRestarts = Restarts + 1,
    RestartCount = 1,
    NewServices = case erlang:apply(M, F, [ProcessIndex, CountProcess,
                                           TimeStart, TimeRestart,
                                           NewRestarts | A]) of
        {ok, Pid} when is_pid(Pid) ->
            ?LOG_WARN_SYNC("successful restart (R = 1)~n"
                           "                   (~p is now ~p)~n"
                           " ~p", [OldPid, Pid, service_id(ServiceId)]),
            Pids = [Pid],
            NextServices = cloudi_x_key2value:store(ServiceId, Pid,
                Service#service{pids = Pids,
                                monitor = erlang:monitor(process, Pid),
                                time_restart = TimeRestart,
                                restart_count_total = NewRestarts,
                                restart_count = RestartCount,
                                restart_times = [SecondsNow]}, Services),
            ok = initialize(Pids),
            NextServices;
        {ok, [Pid | _] = Pids} when is_pid(Pid) ->
            ?LOG_WARN_SYNC("successful restart (R = 1)~n"
                           "                   (~p is now one of ~p)~n"
                           " ~p", [OldPid, Pids, service_id(ServiceId)]),
            NextServices = lists:foldl(fun(P, D) ->
                cloudi_x_key2value:store(ServiceId, P,
                    Service#service{pids = Pids,
                                    monitor = erlang:monitor(process, P),
                                    time_restart = TimeRestart,
                                    restart_count_total = NewRestarts,
                                    restart_count = RestartCount,
                                    restart_times = [SecondsNow]}, D)
            end, Services, Pids),
            ok = initialize(Pids),
            NextServices;
        {error, _} = Error ->
            ?LOG_ERROR_SYNC("failed ~p restart~n ~p",
                            [Error, service_id(ServiceId)]),
            restart_stage2_async(Service#service{
                                     time_restart = TimeRestart,
                                     restart_count_total = NewRestarts,
                                     restart_count = RestartCount,
                                     restart_times = [SecondsNow]},
                                 ServiceId, OldPid),
            Services
    end,
    {true, State#state{services = NewServices}};

restart_stage3(#service{restart_count = RestartCount,
                        restart_times = RestartTimes,
                        max_r = MaxR,
                        max_t = MaxT} = Service,
               Services, State, ServiceId, OldPid)
    when MaxR == RestartCount ->
    % last restart?
    SecondsNow = cloudi_timestamp:seconds_monotonic(),
    {NewRestartCount,
     NewRestartTimes} = cloudi_timestamp:seconds_filter_monotonic(RestartTimes,
                                                                  SecondsNow,
                                                                  MaxT),
    if
        NewRestartCount < RestartCount ->
            restart_stage3(Service#service{
                               restart_count = NewRestartCount,
                               restart_times = NewRestartTimes},
                           Services, State, ServiceId, OldPid);
        true ->
            ?LOG_WARN_SYNC("max restarts (MaxR = ~p, MaxT = ~p seconds) ~p~n"
                           " ~p", [MaxR, MaxT, OldPid, service_id(ServiceId)]),
            {false, State#state{services = Services}}
    end;

restart_stage3(#service{service_m = M,
                        service_f = F,
                        service_a = A,
                        process_index = ProcessIndex,
                        count_process = CountProcess,
                        time_start = TimeStart,
                        restart_count_total = Restarts,
                        restart_count = RestartCount,
                        restart_times = RestartTimes} = Service,
               Services, State, ServiceId, OldPid) ->
    % typical restart scenario
    TimeRestart = erlang:monotonic_time(),
    SecondsNow = cloudi_timestamp:convert(TimeRestart, native, second),
    NewRestarts = Restarts + 1,
    R = RestartCount + 1,
    T = SecondsNow - lists:min(RestartTimes),
    NewServices = case erlang:apply(M, F, [ProcessIndex, CountProcess,
                                           TimeStart, TimeRestart,
                                           NewRestarts | A]) of
        {ok, Pid} when is_pid(Pid) ->
            ?LOG_WARN_SYNC("successful restart "
                           "(R = ~p, T = ~p elapsed seconds)~n"
                           "                   (~p is now ~p)~n"
                           " ~p", [R, T, OldPid, Pid,
                                   service_id(ServiceId)]),
            Pids = [Pid],
            NextServices = cloudi_x_key2value:store(ServiceId, Pid,
                Service#service{pids = Pids,
                                monitor = erlang:monitor(process, Pid),
                                time_restart = TimeRestart,
                                restart_count_total = NewRestarts,
                                restart_count = R,
                                restart_times = [SecondsNow |
                                                 RestartTimes]},
                Services),
            ok = initialize(Pids),
            NextServices;
        {ok, [Pid | _] = Pids} when is_pid(Pid) ->
            ?LOG_WARN_SYNC("successful restart "
                           "(R = ~p, T = ~p elapsed seconds)~n"
                           "                   (~p is now one of ~p)~n"
                           " ~p", [R, T, OldPid, Pids,
                                   service_id(ServiceId)]),
            NextServices = lists:foldl(fun(P, D) ->
                cloudi_x_key2value:store(ServiceId, P,
                    Service#service{pids = Pids,
                                    monitor = erlang:monitor(process, P),
                                    time_restart = TimeRestart,
                                    restart_count_total = NewRestarts,
                                    restart_count = R,
                                    restart_times = [SecondsNow |
                                                     RestartTimes]}, D)
            end, Services, Pids),
            ok = initialize(Pids),
            NextServices;
        {error, _} = Error ->
            ?LOG_ERROR_SYNC("failed ~p restart~n ~p",
                            [Error, service_id(ServiceId)]),
            restart_stage2_async(Service#service{
                                     time_restart = TimeRestart,
                                     restart_count_total = NewRestarts,
                                     restart_count = R,
                                     restart_times = [SecondsNow |
                                                      RestartTimes]},
                                 ServiceId,
                                 OldPid),
            Services
    end,
    {true, State#state{services = NewServices}}.

pids_change(ChangeList, CountProcessCurrent) ->
    pids_change_loop(ChangeList,
                     #pids_change{count_process = CountProcessCurrent}).

pids_change_loop([],
                 #pids_change{count_process = CountProcess,
                              count_increase = CountIncrease,
                              count_decrease = CountDecrease,
                              increase = Increase,
                              decrease = Decrease,
                              rate = Rate}) ->
    Change = erlang:round(if
        CountIncrease == CountDecrease ->
            CountBoth = CountIncrease + CountDecrease,
            ((Increase / CountIncrease) *
             (CountIncrease / CountBoth) +
             (Decrease / CountDecrease) *
             (CountDecrease / CountBoth)) - CountProcess;
        CountIncrease > CountDecrease ->
            (Increase / CountIncrease) - CountProcess;
        CountIncrease < CountDecrease ->
            (Decrease / CountDecrease) - CountProcess
    end),
    {Change, Rate};
pids_change_loop([{increase, RateCurrent,
                   RateMax, CountProcessMax} | ChangeList],
                 #pids_change{count_process = CountProcess,
                              count_increase = CountIncrease,
                              increase = Increase,
                              rate = Rate} = State)
    when RateCurrent > RateMax ->
    NewIncrease = Increase + (if
        CountProcessMax =< CountProcess ->
            % if floating point CountProcess was specified in the configuration
            % and the number of schedulers changed, it would be possible to
            % have: CountProcessMax < CountProcess
            CountProcess;
        CountProcessMax > CountProcess ->
            erlang:min(cloudi_math:ceil((CountProcess * RateCurrent) /
                                        RateMax),
                       CountProcessMax)
    end),
    pids_change_loop(ChangeList,
                     State#pids_change{count_increase = (CountIncrease + 1),
                                       increase = NewIncrease,
                                       rate = (Rate + RateCurrent)});
pids_change_loop([{decrease, RateCurrent,
                   RateMin, CountProcessMin} | ChangeList],
                 #pids_change{count_process = CountProcess,
                              count_decrease = CountDecrease,
                              decrease = Decrease,
                              rate = Rate} = State)
    when RateCurrent < RateMin ->
    NewDecrease = Decrease + (if
        CountProcessMin >= CountProcess ->
            % if floating point CountProcess was specified in the configuration
            % and the number of schedulers changed, it would be possible to
            % have: CountProcessMin > CountProcess
            CountProcess;
        CountProcessMin < CountProcess ->
            erlang:max(cloudi_math:floor((CountProcess * RateCurrent) /
                                         RateMin),
                       CountProcessMin)
    end),
    pids_change_loop(ChangeList,
                     State#pids_change{count_decrease = (CountDecrease + 1),
                                       decrease = NewDecrease,
                                       rate = (Rate + RateCurrent)}).

service_instance(Pids, ServiceId, Services) ->
    service_instance(Pids, [], ServiceId, Services).

service_instance([], Results, _, _) ->
    Results;
service_instance([Pid | Pids], Results, ServiceId, Services) ->
    case cloudi_x_key2value:find2(Pid, Services) of
        {ok, {[ServiceId], #service{pids = [Pid]} = Service}} ->
            service_instance(Pids,
                             lists:keymerge(#service.process_index,
                                            Results, [Service]),
                             ServiceId, Services);
        {ok, {[ServiceId], #service{pids = ThreadPids} = Service}} ->
            service_instance(Pids -- ThreadPids,
                             lists:keymerge(#service.process_index,
                                            Results, [Service]),
                             ServiceId, Services)
    end.

pids_update(ServiceL, CountProcess, ServiceId, Services) ->
    pids_update(ServiceL, [], CountProcess, ServiceId, Services).

pids_update([], Output, _, _, Services) ->
    {Output, Services};
pids_update([#service{pids = Pids} = Service | ServiceL], Output,
            CountProcess, ServiceId, Services) ->
    NewService = Service#service{count_process = CountProcess},
    NewServices = lists:foldl(fun(P, D) ->
        cloudi_core_i_rate_based_configuration:
        count_process_dynamic_update(P, CountProcess),
        cloudi_x_key2value:store(ServiceId, P, NewService, D)
    end, Services, Pids),
    pids_update(ServiceL, [NewService | Output],
                CountProcess, ServiceId, NewServices).

pids_increase_loop(0, _, _, _, Services) ->
    Services;
pids_increase_loop(Count, ProcessIndex,
                   #service{service_m = M,
                            service_f = F,
                            service_a = A,
                            count_process = CountProcess,
                            count_thread = CountThread,
                            scope = Scope,
                            time_start = TimeStart,
                            time_restart = TimeRestart,
                            restart_count_total = Restarts,
                            timeout_term = TimeoutTerm,
                            restart_delay = RestartDelay,
                            max_r = MaxR,
                            max_t = MaxT} = Service, ServiceId, Services) ->
    NewServices = case erlang:apply(M, F, [ProcessIndex, CountProcess,
                                           TimeStart, TimeRestart,
                                           Restarts | A]) of
        {ok, Pid} when is_pid(Pid) ->
            ?LOG_INFO("~p -> ~p (count_process_dynamic)",
                      [service_id(ServiceId), Pid]),
            Pids = [Pid],
            NextServices = cloudi_x_key2value:store(ServiceId, Pid,
                new_service_process(M, F, A,
                                    ProcessIndex, CountProcess,
                                    CountThread, Scope, Pids,
                                    erlang:monitor(process, Pid),
                                    TimeStart, TimeRestart, Restarts,
                                    TimeoutTerm, RestartDelay,
                                    MaxR, MaxT), Services),
            ok = initialize(Pids),
            NextServices;
        {ok, [Pid | _] = Pids} when is_pid(Pid) ->
            ?LOG_INFO("~p -> ~p (count_process_dynamic)",
                      [service_id(ServiceId), Pids]),
            NextServices = lists:foldl(fun(P, D) ->
                cloudi_x_key2value:store(ServiceId, P,
                    new_service_process(M, F, A,
                                        ProcessIndex, CountProcess,
                                        CountThread, Scope, Pids,
                                        erlang:monitor(process, P),
                                        TimeStart, TimeRestart, Restarts,
                                        TimeoutTerm, RestartDelay,
                                        MaxR, MaxT), D)
            end, Services, Pids),
            ok = initialize(Pids),
            NextServices;
        {error, _} = Error ->
            ?LOG_ERROR("failed ~p increase (count_process_dynamic)~n ~p",
                       [Error, service_id(ServiceId)]),
            Services
    end,
    pids_increase_loop(Count - 1, ProcessIndex + 1,
                       Service, ServiceId, NewServices).

pids_increase(Count, OldPids, CountProcessCurrent, Rate,
              ServiceId, Services) ->
    ServiceL = service_instance(OldPids, ServiceId, Services),
    ?LOG_INFO("count_process_dynamic(~p):~n "
              "increasing ~p with ~p for ~p requests/second~n~p",
              [service_id(ServiceId), CountProcessCurrent, Count,
               erlang:round(Rate * 10) / 10,
               [P || #service{pids = P} <- ServiceL]]),
    CountProcess = CountProcessCurrent + Count,
    {NewServiceL, % reversed
     NewServices} = pids_update(ServiceL, CountProcess, ServiceId, Services),
    [#service{process_index = ProcessIndex} = Service | _] = NewServiceL,
    pids_increase_loop(Count, ProcessIndex + 1, Service,
                       ServiceId, NewServices).

pids_decrease_loop(0, ServiceL) ->
    ServiceL;
pids_decrease_loop(Count, [#service{pids = Pids} | ServiceL]) ->
    lists:foreach(fun(P) ->
        cloudi_core_i_rate_based_configuration:
        count_process_dynamic_terminate(P)
    end, Pids),
    pids_decrease_loop(Count + 1, ServiceL).

pids_decrease(Count, OldPids, CountProcessCurrent, Rate,
              ServiceId, Services) ->
    ServiceL = service_instance(OldPids, ServiceId, Services),
    ?LOG_INFO("count_process_dynamic(~p):~n "
              "decreasing ~p with ~p for ~p requests/second~n~p",
              [service_id(ServiceId), CountProcessCurrent, Count,
               erlang:round(Rate * 10) / 10,
               [P || #service{pids = P} <- ServiceL]]),
    CountProcess = CountProcessCurrent + Count,
    NewServiceL = pids_decrease_loop(Count, lists:reverse(ServiceL)),
    {_,
     NewServices} = pids_update(NewServiceL, CountProcess, ServiceId, Services),
    NewServices.

terminate_kill_enforce(Self, Pid, Reason, ServiceId,
                       #service{timeout_term = TimeoutTerm} = Service) ->
    erlang:send_after(TimeoutTerm, Self,
                      {kill, Pid, Reason, ServiceId, Service}),
    ok.

terminate_kill_enforce_now(Pid, Reason, ServiceId,
                           #service{timeout_term = TimeoutTerm}) ->
    case erlang:is_process_alive(Pid) of
        true ->
            ?LOG_ERROR_SYNC("Service pid ~p brutal_kill (~p)~n"
                            " ~p after ~p ms (MaxT/MaxR)",
                            [Pid, Reason, service_id(ServiceId), TimeoutTerm]),
            erlang:exit(Pid, kill);
        false ->
            ok
    end,
    ok.

terminate_service(ServiceId, Pids, Reason, Service, Services, OldPid) ->
    ShutdownExit = if
        Reason =:= undefined ->
            shutdown;
        true ->
            {shutdown, Reason}
    end,
    NewServices = terminate_service_pids(Pids, Services, self(), ShutdownExit,
                                         ServiceId, Service, OldPid),
    ok = terminate_service_wait(Pids, OldPid),
    NewServices.

terminate_service_pids([], Services, _, _, _, _, _) ->
    Services;
terminate_service_pids([OldPid | Pids], Services, Self, ShutdownExit,
                       ServiceId, Service, OldPid) ->
    NewServices = cloudi_x_key2value:erase(ServiceId, OldPid, Services),
    terminate_service_pids(Pids, NewServices, Self, ShutdownExit,
                           ServiceId, Service, OldPid);
terminate_service_pids([Pid | Pids], Services, Self, ShutdownExit,
                       ServiceId, Service, OldPid) ->
    erlang:exit(Pid, ShutdownExit),
    ok = terminate_kill_enforce(Self, Pid, ShutdownExit, ServiceId, Service),
    NewServices = cloudi_x_key2value:erase(ServiceId, Pid, Services),
    terminate_service_pids(Pids, NewServices, Self, ShutdownExit,
                           ServiceId, Service, OldPid).

terminate_service_wait([], _) ->
    ok;
terminate_service_wait([OldPid | Pids], OldPid) ->
    terminate_service_wait(Pids, OldPid);
terminate_service_wait([Pid | Pids], OldPid) ->
    % ensure each service process has executed its termination source code
    % (or has died due to a termination timeout)
    receive
        {'DOWN', _MonitorRef, 'process', Pid, _} ->
            terminate_service_wait(Pids, OldPid);
        {kill, Pid, ShutdownExit, ServiceId, Service} ->
            ok = terminate_kill_enforce_now(Pid, ShutdownExit,
                                            ServiceId, Service),
            receive
                {'DOWN', _MonitorRef, 'process', Pid, _} ->
                    terminate_service_wait(Pids, OldPid)
            end
    end.

service_ids_pids(ServiceIdList, Services) ->
    service_ids_pids(ServiceIdList, [], Services).

service_ids_pids([], Pids, _) ->
    {ok, Pids};
service_ids_pids([ServiceId | ServiceIdList], Pids, Services) ->
    case service_id_pids(ServiceId, Services) of
        {ok, PidList} ->
            service_ids_pids(ServiceIdList, Pids ++ PidList, Services);
        {error, _} = Error ->
            Error
    end.

service_id_pids(ServiceId, Services) ->
    case cloudi_x_key2value:find1(ServiceId, Services) of
        {ok, {Pids, #service{}}} ->
            % all pids must be in process_index order
            PidsOrdered1 = lists:foldl(fun(Pid, PidsOrdered0) ->
                {_, Service} = cloudi_x_key2value:fetch2(Pid, Services),
                #service{process_index = ProcessIndex,
                         pids = ProcessPids} = Service,
                lists:umerge(PidsOrdered0, [{ProcessIndex, ProcessPids}])
            end, [], Pids),
            PidsOrderedN = lists:flatmap(fun({_, PidList}) ->
                PidList
            end, PidsOrdered1),
            {ok, PidsOrderedN};
        error ->
            {error, not_found}
    end.

service_ids_status(ServiceIdList, TimeNow, Services) ->
    service_ids_status(ServiceIdList, [], TimeNow, Services).

service_ids_status([], StatusList, _, _) ->
    {ok, lists:reverse(StatusList)};
service_ids_status([ServiceId | ServiceIdList], StatusList,
                   TimeNow, Services) ->
    case service_id_status(ServiceId, TimeNow, Services) of
        {ok, Status} ->
            service_ids_status(ServiceIdList,
                               [{ServiceId, Status} | StatusList],
                               TimeNow, Services);
        {error, _} = Error ->
            Error
    end.

service_id_status(ServiceId, TimeNow, Services) ->
    case cloudi_x_key2value:find1(ServiceId, Services) of
        {ok, {_, #service{service_f = StartType,
                          count_process = CountProcess,
                          count_thread = CountThread,
                          time_start = TimeStart,
                          time_restart = TimeRestart,
                          restart_count_total = Restarts}}} ->
            TimeDiffTotal = TimeNow - TimeStart,
            TimeDiffRunning = if
                TimeRestart =:= undefined ->
                    TimeDiffTotal;
                is_integer(TimeRestart) ->
                    TimeNow - TimeRestart
            end,
            NanoSecondsTotal = cloudi_timestamp:
                               convert(TimeDiffTotal, native, nanosecond),
            NanoSecondsRunning = cloudi_timestamp:
                                 convert(TimeDiffRunning, native, nanosecond),
            UptimeTotal = nanoseconds_to_string(NanoSecondsTotal),
            UptimeRunning = nanoseconds_to_string(NanoSecondsRunning),
            AvailabilityDay =
                nanoseconds_to_availability_day(NanoSecondsRunning),
            AvailabilityWeek =
                nanoseconds_to_availability_week(NanoSecondsRunning),
            AvailabilityMonth =
                nanoseconds_to_availability_month(NanoSecondsRunning),
            AvailabilityYear =
                nanoseconds_to_availability_year(NanoSecondsRunning),
            Status0 = [{uptime_total, UptimeTotal},
                       {uptime_running, UptimeRunning},
                       {uptime_restarts, erlang:integer_to_list(Restarts)},
                       {availability_day, AvailabilityDay},
                       {availability_week, AvailabilityWeek},
                       {availability_month, AvailabilityMonth},
                       {availability_year, AvailabilityYear}],
            StatusN = if
                StartType =:= start_internal ->
                    [{count_process, CountProcess} | Status0];
                StartType =:= start_external ->
                    [{count_process, CountProcess},
                     {count_thread, CountThread} | Status0]
            end,
            {ok, StatusN};
        error ->
            {error, {service_not_found, ServiceId}}
    end.

update_start(PidList, UpdatePlan) ->
    Self = self(),
    [Pid ! {'cloudi_service_update', Self, UpdatePlan} || Pid <- PidList],
    update_start_recv(PidList, [], []).

update_start_recv([], NewPidList, UpdateResults) ->
    {UpdateResults, lists:reverse(NewPidList)};
update_start_recv([Pid | PidList], NewPidList, UpdateResults)
    when is_pid(Pid) ->
    receive
        {'DOWN', _, 'process', Pid, _} = DOWN ->
            self() ! DOWN,
            update_start_recv(PidList, [undefined | NewPidList],
                              [{error, pid_died} | UpdateResults]);
        {'cloudi_service_update', Pid} ->
            update_start_recv(PidList, [Pid | NewPidList],
                              UpdateResults);
        {'cloudi_service_update', Pid, {error, _} = UpdateResult} ->
            update_start_recv(PidList, [undefined | NewPidList],
                              [UpdateResult | UpdateResults])
    end.

update_now(PidList, UpdateResults, UpdateStart) ->
    Self = self(),
    lists:foreach(fun(Pid) ->
        if
            Pid =:= undefined ->
                ok;
            is_pid(Pid) ->
                Pid ! {'cloudi_service_update_now', Self, UpdateStart}
        end
    end, PidList),
    update_now_recv(PidList, UpdateResults).

update_now_recv([], UpdateResults) ->
    lists:reverse(UpdateResults);
update_now_recv([Pid | PidList], UpdateResults) ->
    if
        Pid =:= undefined ->
            update_now_recv(PidList, UpdateResults);
        is_pid(Pid) ->
            UpdateResult = receive
                {'DOWN', _, 'process', Pid, _} = DOWN ->
                    self() ! DOWN,
                    {error, pid_died};
                {'cloudi_service_update_now', Pid, Result} ->
                    Result
            end,
            update_now_recv(PidList, [UpdateResult | UpdateResults])
    end.

update_results(Results) ->
    update_results(Results, [], []).

update_results([], ResultsSuccess, ResultsError) ->
    {lists:reverse(ResultsSuccess), lists:usort(ResultsError)};
update_results([ok | Results], ResultsSuccess, ResultsError) ->
    update_results(Results, ResultsSuccess, ResultsError);
update_results([{ok, Result} | Results], ResultsSuccess, ResultsError) ->
    update_results(Results, [Result | ResultsSuccess], ResultsError);
update_results([{error, Reason} | Results], ResultsSuccess, ResultsError) ->
    update_results(Results, ResultsSuccess, [Reason | ResultsError]).

update_load_module([]) ->
    ok;
update_load_module([Module | ModulesLoad]) ->
    case cloudi_x_reltool_util:module_reload(Module) of
        ok ->
            update_load_module(ModulesLoad);
        {error, _} = Error ->
            Error
    end.

update_load_module([], ModulesLoad) ->
    update_load_module(ModulesLoad);
update_load_module([CodePathAdd | CodePathsAdd], ModulesLoad) ->
    case code:add_patha(CodePathAdd) of
        true ->
            update_load_module(CodePathsAdd, ModulesLoad);
        {error, _} = Error ->
            Error
    end.

update_reload_stop(#config_service_update{
                       type = internal,
                       module = Module,
                       reload_stop = ReloadStop}) ->
    if
        ReloadStop =:= true ->
            ok = cloudi_core_i_services_internal_reload:
                 service_remove(Module);
        ReloadStop =:= false ->
            ok
    end;
update_reload_stop(#config_service_update{
                       type = external}) ->
    ok.

update_reload_start(#config_service_update{
                        type = internal,
                        module = Module,
                        options_keys = OptionsKeys,
                        options = #config_service_options{
                            reload = ReloadStart},
                        reload_stop = ReloadStop}) ->
    Reload = case lists:member(reload, OptionsKeys) of
        true ->
            ReloadStart;
        false ->
            ReloadStop
    end,
    if
        Reload =:= true ->
            ok = cloudi_core_i_services_internal_reload:
                 service_add(Module);
        Reload =:= false ->
            ok
    end;
update_reload_start(#config_service_update{
                        type = external}) ->
    ok.

update_before(#config_service_update{
                  modules_load = ModulesLoad,
                  code_paths_add = CodePathsAdd} = UpdatePlan) ->
    ok = update_reload_stop(UpdatePlan),
    UpdateModuleResult = update_load_module(CodePathsAdd, ModulesLoad),
    [UpdateModuleResult].

update_unload_module([]) ->
    ok;
update_unload_module([CodePathRemove | CodePathsRemove]) ->
    code:del_path(CodePathRemove),
    update_load_module(CodePathsRemove).

update_unload_module([], CodePathsRemove) ->
    update_unload_module(CodePathsRemove);
update_unload_module([Module | ModulesUnload], CodePathsRemove) ->
    cloudi_x_reltool_util:module_unload(Module),
    update_unload_module(ModulesUnload, CodePathsRemove).

update_service(_, _,
               #config_service_update{
                   type = internal,
                   dest_refresh = DestRefresh,
                   timeout_init = TimeoutInit,
                   timeout_async = TimeoutAsync,
                   timeout_sync = TimeoutSync,
                   dest_list_deny = DestListDeny,
                   dest_list_allow = DestListAllow,
                   options_keys = OptionsKeys,
                   options = Options,
                   uuids = ServiceIds}, Services0) ->
    ServicesN = lists:foldl(fun(ServiceId, Services1) ->
        cloudi_x_key2value:update1(ServiceId, fun(OldService) ->
            #service{service_m = Module,
                     service_a = Arguments} = OldService,
            cloudi_core_i_spawn = Module,
            NewArguments = Module:update_internal_f(DestRefresh,
                                                    TimeoutInit,
                                                    TimeoutAsync,
                                                    TimeoutSync,
                                                    DestListDeny,
                                                    DestListAllow,
                                                    OptionsKeys,
                                                    Options,
                                                    Arguments),
            OldService#service{service_a = NewArguments}
        end, Services1)
    end, Services0, ServiceIds),
    ServicesN;
update_service(Pids, Ports,
               #config_service_update{
                   type = external,
                   file_path = FilePath,
                   args = Args,
                   env = Env,
                   dest_refresh = DestRefresh,
                   timeout_init = TimeoutInit,
                   timeout_async = TimeoutAsync,
                   timeout_sync = TimeoutSync,
                   dest_list_deny = DestListDeny,
                   dest_list_allow = DestListAllow,
                   options_keys = OptionsKeys,
                   options = Options,
                   uuids = [ServiceId]}, Services0) ->
    ServicesN = cloudi_x_key2value:update1(ServiceId, fun(OldService) ->
        #service{service_m = Module,
                 service_a = Arguments} = OldService,
        cloudi_core_i_spawn = Module,
        NextArguments = Module:update_external_f(FilePath,
                                                 Args,
                                                 Env,
                                                 DestRefresh,
                                                 TimeoutInit,
                                                 TimeoutAsync,
                                                 TimeoutSync,
                                                 DestListDeny,
                                                 DestListAllow,
                                                 OptionsKeys,
                                                 Options,
                                                 Arguments),
        OldService#service{service_a = NextArguments}
    end, Services0),
    if
        Ports == [] ->
            ok;
        length(Pids) == length(Ports) ->
            {_, NewService} = cloudi_x_key2value:fetch1(ServiceId,
                                                        ServicesN),
            #service{service_a = NewArguments,
                     count_process = CountProcess,
                     count_thread = CountThread} = NewService,
            Result = case cloudi_core_i_configurator:
                          service_update_external(Pids, Ports, NewArguments,
                                                  CountThread, CountProcess) of
                ok ->
                    ok;
                {error, _} = Error ->
                    ?LOG_ERROR("failed ~p service update~n ~p",
                               [Error, service_id(ServiceId)]),
                    error
            end,
            [Pid ! {'cloudi_service_update_after', Result} || Pid <- Pids]
    end,
    ServicesN.

update_after(UpdateSuccess, PidList, ResultsSuccess,
             #config_service_update{
                 modules_unload = ModulesUnload,
                 code_paths_remove = CodePathsRemove} = UpdatePlan, Services) ->
    NewServices = if
        UpdateSuccess =:= true ->
            update_service(PidList, ResultsSuccess, UpdatePlan, Services);
        UpdateSuccess =:= false ->
            Services
    end,
    update_unload_module(ModulesUnload, CodePathsRemove),
    ok = update_reload_start(UpdatePlan),
    NewServices.

nanoseconds_to_string(TotalNanoSeconds) ->
    TotalSeconds = TotalNanoSeconds div ?NANOSECONDS_IN_SECOND,
    NanoSeconds = TotalNanoSeconds rem ?NANOSECONDS_IN_SECOND,
    TotalHours = TotalSeconds div ?SECONDS_IN_HOUR,
    Seconds = TotalSeconds rem ?SECONDS_IN_HOUR,
    lists:flatten([erlang:integer_to_list(TotalHours), " hours ",
                   erlang:integer_to_list(Seconds), " seconds ",
                   erlang:integer_to_list(NanoSeconds), " nanoseconds"]).

nanoseconds_to_availability_day(TotalNanoSeconds) ->
    AvailabilityDay = TotalNanoSeconds /
        (?NANOSECONDS_IN_SECOND * ?SECONDS_IN_HOUR *
         ?HOURS_IN_DAY),
    availability_to_string(AvailabilityDay).

nanoseconds_to_availability_week(TotalNanoSeconds) ->
    AvailabilityWeek = TotalNanoSeconds /
        (?NANOSECONDS_IN_SECOND * ?SECONDS_IN_HOUR *
         ?HOURS_IN_DAY * ?DAYS_IN_WEEK),
    availability_to_string(AvailabilityWeek).

nanoseconds_to_availability_month(TotalNanoSeconds) ->
    AvailabilityMonth = TotalNanoSeconds /
        (?NANOSECONDS_IN_SECOND * ?SECONDS_IN_HOUR *
         ?HOURS_IN_DAY * ?DAYS_IN_MONTH),
    availability_to_string(AvailabilityMonth).

nanoseconds_to_availability_year(TotalNanoSeconds) ->
    AvailabilityYear = TotalNanoSeconds /
        (?NANOSECONDS_IN_SECOND * ?SECONDS_IN_HOUR *
         ?HOURS_IN_DAY * ?DAYS_IN_YEAR),
    availability_to_string(AvailabilityYear).

% avoid erlang:float_to_list precision problems
% and keep the formatting efficient
availability_to_string(Availability)
    when Availability < 0.25 ->
    "0 %";
availability_to_string(Availability)
    when Availability < 1 / 3 ->
    "25 %";
availability_to_string(Availability)
    when Availability < 0.5 ->
    "33.3 %";
availability_to_string(Availability)
    when Availability < 2 / 3 ->
    "50 %";
availability_to_string(Availability)
    when Availability < 0.75 ->
    "66.6 %";
availability_to_string(Availability)
    when Availability < 0.9 ->
    "75 %";
availability_to_string(Availability)
    when Availability < 0.99 ->
    if % 1 nine
        Availability < 0.91 ->
            "90 %";
        Availability < 0.92 ->
            "91 %";
        Availability < 0.93 ->
            "92 %";
        Availability < 0.94 ->
            "93 %";
        Availability < 0.95 ->
            "94 %";
        Availability < 0.96 ->
            "95 %";
        Availability < 0.97 ->
            "96 %";
        Availability < 0.98 ->
            "97 %";
        true ->
            "98 %"
    end;
availability_to_string(Availability)
    when Availability < 0.999 ->
    if % 2 nines
        Availability < 0.991 ->
            "99.0 %";
        Availability < 0.992 ->
            "99.1 %";
        Availability < 0.993 ->
            "99.2 %";
        Availability < 0.994 ->
            "99.3 %";
        Availability < 0.995 ->
            "99.4 %";
        Availability < 0.996 ->
            "99.5 %";
        Availability < 0.997 ->
            "99.6 %";
        Availability < 0.998 ->
            "99.7 %";
        true ->
            "99.8 %"
    end;
availability_to_string(Availability)
    when Availability < 0.9999 ->
    if % 3 nines
        Availability < 0.9991 ->
            "99.9 %";
        Availability < 0.9992 ->
            "99.91 %";
        Availability < 0.9993 ->
            "99.92 %";
        Availability < 0.9994 ->
            "99.93 %";
        Availability < 0.9995 ->
            "99.94 %";
        Availability < 0.9996 ->
            "99.95 %";
        Availability < 0.9997 ->
            "99.96 %";
        Availability < 0.9998 ->
            "99.97 %";
        true ->
            "99.98 %"
    end;
availability_to_string(Availability)
    when Availability < 0.99999 ->
    if % 4 nines
        Availability < 0.99991 ->
            "99.99 %";
        Availability < 0.99992 ->
            "99.991 %";
        Availability < 0.99993 ->
            "99.992 %";
        Availability < 0.99994 ->
            "99.993 %";
        Availability < 0.99995 ->
            "99.994 %";
        Availability < 0.99996 ->
            "99.995 %";
        Availability < 0.99997 ->
            "99.996 %";
        Availability < 0.99998 ->
            "99.997 %";
        true ->
            "99.998 %"
    end;
availability_to_string(Availability)
    when Availability < 0.999999 ->
    if % 5 nines
        Availability < 0.999991 ->
            "99.999 %";
        Availability < 0.999992 ->
            "99.9991 %";
        Availability < 0.999993 ->
            "99.9992 %";
        Availability < 0.999994 ->
            "99.9993 %";
        Availability < 0.999995 ->
            "99.9994 %";
        Availability < 0.999996 ->
            "99.9995 %";
        Availability < 0.999997 ->
            "99.9996 %";
        Availability < 0.999998 ->
            "99.9997 %";
        true ->
            "99.9998 %"
    end;
availability_to_string(Availability)
    when Availability < 0.9999999 ->
    if % 6 nines
        Availability < 0.9999991 ->
            "99.9999 %";
        Availability < 0.9999992 ->
            "99.99991 %";
        Availability < 0.9999993 ->
            "99.99992 %";
        Availability < 0.9999994 ->
            "99.99993 %";
        Availability < 0.9999995 ->
            "99.99994 %";
        Availability < 0.9999996 ->
            "99.99995 %";
        Availability < 0.9999997 ->
            "99.99996 %";
        Availability < 0.9999998 ->
            "99.99997 %";
        true ->
            "99.99998 %"
    end;
availability_to_string(Availability)
    when Availability < 0.99999999 ->
    if % 7 nines
        Availability < 0.99999991 ->
            "99.99999 %";
        Availability < 0.99999992 ->
            "99.999991 %";
        Availability < 0.99999993 ->
            "99.999992 %";
        Availability < 0.99999994 ->
            "99.999993 %";
        Availability < 0.99999995 ->
            "99.999994 %";
        Availability < 0.99999996 ->
            "99.999995 %";
        Availability < 0.99999997 ->
            "99.999996 %";
        Availability < 0.99999998 ->
            "99.999997 %";
        true ->
            "99.999998 %"
    end;
availability_to_string(Availability)
    when Availability < 0.999999999 ->
    if % 8 nines
        Availability < 0.999999991 ->
            "99.999999 %";
        Availability < 0.999999992 ->
            "99.9999991 %";
        Availability < 0.999999993 ->
            "99.9999992 %";
        Availability < 0.999999994 ->
            "99.9999993 %";
        Availability < 0.999999995 ->
            "99.9999994 %";
        Availability < 0.999999996 ->
            "99.9999995 %";
        Availability < 0.999999997 ->
            "99.9999996 %";
        Availability < 0.999999998 ->
            "99.9999997 %";
        true ->
            "99.9999998 %"
    end;
availability_to_string(_) ->
    "99.9999999 %". % 9 nines

service_id(ID) ->
    cloudi_x_uuid:uuid_to_string(ID, list_nodash).

new_service_process(M, F, A, ProcessIndex, CountProcess, CountThread,
                    Scope, Pids, MonitorRef, TimeStart, TimeRestart, Restarts,
                    TimeoutTerm, RestartDelay, MaxR, MaxT) ->
    #service{service_m = M,
             service_f = F,
             service_a = A,
             process_index = ProcessIndex,
             count_process = CountProcess,
             count_thread = CountThread,
             scope = Scope,
             pids = Pids,
             monitor = MonitorRef,
             time_start = TimeStart,
             time_restart = TimeRestart,
             restart_count_total = Restarts,
             timeout_term = TimeoutTerm,
             restart_delay = RestartDelay,
             max_r = MaxR,
             max_t = MaxT}.

