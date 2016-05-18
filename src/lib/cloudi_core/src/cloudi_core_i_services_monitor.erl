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
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2011-2016, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011-2016 Michael Truog
%%% @version 1.5.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_core_i_services_monitor).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).

%% external interface
-export([start_link/0,
         monitor/12,
         initialize/1,
         shutdown/2,
         restart/2,
         search/2,
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
        % pids is only accurate on the pid lookup (find2)
        % due to the overwrite of #service{}
        pids :: list(pid()),
        monitor :: undefined | reference(),
        restart_count = 0 :: non_neg_integer(),
        restart_times = [] :: list(integer()),
        timeout_term :: cloudi_service_api:timeout_terminate_milliseconds(),
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
        services = cloudi_x_key2value:new() ::
            cloudi_x_key2value:
            cloudi_x_key2value(cloudi_service_api:service_id(),
                               pid(), #service{}),
        changes = dict:new() ::
            dict_proxy(cloudi_service_api:service_id(),
                       list({increase | decrease,
                             number(), number(), number()}))
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
                             timeout_terminate_milliseconds(),
              MaxR :: non_neg_integer(),
              MaxT :: non_neg_integer(),
              ServiceId :: cloudi_x_uuid:cloudi_x_uuid(),
              Timeout :: infinity | pos_integer()) ->
    {ok, list(pid())} |
    {error, any()}.

monitor(M, F, A, ProcessIndex, CountProcess, CountThread, Scope,
        TimeoutTerm, MaxR, MaxT, ServiceId, Timeout)
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
                                 ProcessIndex, CountProcess, CountThread, Scope,
                                 TimeoutTerm, MaxR, MaxT, ServiceId},
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

search([_ | _] = PidList, Timeout) ->
    ?CATCH_EXIT(gen_server:call(?MODULE,
                                {search, PidList},
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
             TimeoutTerm, MaxR, MaxT, ServiceId}, _,
            #state{services = Services} = State) ->
    case erlang:apply(M, F, [ProcessIndex, CountProcess | A]) of
        {ok, Pid} when is_pid(Pid) ->
            Pids = [Pid],
            NewServices = cloudi_x_key2value:store(ServiceId, Pid,
                new_service_process(M, F, A,
                                    ProcessIndex, CountProcess, CountThread,
                                    Scope, Pids, erlang:monitor(process, Pid),
                                    TimeoutTerm, MaxR, MaxT), Services),
            {reply, {ok, Pids}, State#state{services = NewServices}};
        {ok, [Pid | _] = Pids} when is_pid(Pid) ->
            NewServices = lists:foldl(fun(P, D) ->
                cloudi_x_key2value:store(ServiceId, P,
                    new_service_process(M, F, A,
                                        ProcessIndex, CountProcess, CountThread,
                                        Scope, Pids, erlang:monitor(process, P),
                                        TimeoutTerm, MaxR, MaxT), D)
            end, Services, Pids),
            {reply, {ok, Pids}, State#state{services = NewServices}};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({shutdown, ServiceId}, _,
            #state{services = Services} = State) ->
    case cloudi_x_key2value:find1(ServiceId, Services) of
        {ok, {Pids, #service{} = Service}} ->
            NewServices = terminate_service(ServiceId, Pids, Service, Services),
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

handle_call({pids, ServiceId}, _,
            #state{services = Services} = State) ->
    case cloudi_x_key2value:find1(ServiceId, Services) of
        {ok, {PidList, #service{scope = Scope}}} ->
            {reply, {ok, Scope, PidList}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(Request, _, State) ->
    ?LOG_WARN("Unknown call \"~p\"", [Request]),
    {stop, cloudi_string:format("Unknown call \"~p\"", [Request]),
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
            NewChangeList = case dict:find(ServiceId, Changes) of
                {ok, ChangeList} ->
                    [Entry | ChangeList];
                error ->
                    erlang:send_after(Period * 1000, self(),
                                      {changes, ServiceId}),
                    [Entry]
            end,
            {noreply, State#state{changes = dict:store(ServiceId,
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
            terminate_kill_enforce(ServiceId, Pid, Reason, Service),
            {noreply, State};
        error ->
            {noreply, State}
    end;

handle_cast(Request, State) ->
    ?LOG_WARN("Unknown cast \"~p\"", [Request]),
    {noreply, State}.

handle_info({changes, ServiceId},
            #state{services = Services,
                   changes = Changes} = State) ->
    NewChanges = dict:erase(ServiceId, Changes),
    case cloudi_x_key2value:find1(ServiceId, Services) of
        {ok, {Pids, #service{count_thread = CountThread}}} ->
            ChangeList = dict:fetch(ServiceId, Changes),
            CountProcessCurrent = erlang:round(erlang:length(Pids) /
                                               CountThread),
            {I, Rate} = pids_change(ChangeList, CountProcessCurrent),
            NewServices = if
                I == 0 ->
                    ?LOG_TRACE("count_process_dynamic: "
                               "constant ~p for ~p requests/second",
                               [CountProcessCurrent,
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
            NewServices = terminate_service(ServiceId, Pids, Service, Services),
            cloudi_core_i_configurator:service_dead(ServiceId),
            {noreply, State#state{services = NewServices}};
        error ->
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
            {noreply, State}
    end;

handle_info({'DOWN', _MonitorRef, 'process', Pid, {shutdown, Reason}},
            #state{services = Services} = State) ->
    case cloudi_x_key2value:find2(Pid, Services) of
        {ok, {[ServiceId], #service{pids = Pids} = Service}} ->
            ?LOG_INFO_SYNC("Service pid ~p shutdown (~p)~n ~p",
                           [Pid, Reason, service_id(ServiceId)]),
            NewServices = terminate_service(ServiceId, Pids, Reason,
                                            Service, Services),
            cloudi_core_i_configurator:service_dead(ServiceId),
            {noreply, State#state{services = NewServices}};
        error ->
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
            % Pids started together as threads for one OS process may
            % have died together.  The first death triggers the restart and
            % removes all other pids from the Services data structure.
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

handle_info({kill, TimeoutTerm, Pid, Reason, ServiceId, #service{}}, State) ->
    case erlang:is_process_alive(Pid) of
        true ->
            ?LOG_ERROR_SYNC("Service pid ~p brutal_kill (~p)~n"
                            " ~p after ~p ms (MaxT/MaxR)",
                            [Pid, Reason, service_id(ServiceId), TimeoutTerm]),
            erlang:exit(Pid, kill);
        false ->
            ok
    end,
    {noreply, State};

handle_info({ReplyRef, _}, State) when is_reference(ReplyRef) ->
    % gen_server:call/3 had a timeout exception that was caught but the
    % reply arrived later and must be discarded
    {noreply, State};

handle_info(Request, State) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

restart(Service, Services, State, ServiceId, OldPid) ->
    restart_stage1(Service, Services, State, ServiceId, OldPid).

restart_stage1(#service{pids = Pids} = Service, Services,
               State, ServiceId, OldPid) ->
    NewServices = terminate_service(ServiceId, Pids, Service, Services),
    restart_stage2(Service#service{pids = [],
                                   monitor = undefined},
                   NewServices, State, ServiceId, OldPid).

restart_stage2(#service{restart_count = 0,
                        max_r = 0},
               Services, State, ServiceId, OldPid) ->
    % no restarts allowed
    ?LOG_WARN_SYNC("max restarts (MaxR = 0) ~p~n ~p",
                   [OldPid, service_id(ServiceId)]),
    {false, State#state{services = Services}};

restart_stage2(#service{service_m = M,
                        service_f = F,
                        service_a = A,
                        process_index = ProcessIndex,
                        count_process = CountProcess,
                        restart_count = 0,
                        restart_times = []} = Service,
               Services, State, ServiceId, OldPid) ->
    % first restart
    SecondsNow = cloudi_timestamp:seconds(),
    NewServices = case erlang:apply(M, F, [ProcessIndex, CountProcess | A]) of
        {ok, Pid} when is_pid(Pid) ->
            ?LOG_WARN_SYNC("successful restart (R = 1)~n"
                           "                   (~p is now ~p)~n"
                           " ~p", [OldPid, Pid, service_id(ServiceId)]),
            Pids = [Pid],
            NextServices = cloudi_x_key2value:store(ServiceId, Pid,
                Service#service{pids = Pids,
                                monitor = erlang:monitor(process, Pid),
                                restart_count = 1,
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
                                    restart_count = 1,
                                    restart_times = [SecondsNow]}, D)
            end, Services, Pids),
            ok = initialize(Pids),
            NextServices;
        {error, _} = Error ->
            ?LOG_ERROR_SYNC("failed ~p restart~n ~p",
                            [Error, service_id(ServiceId)]),
            self() ! {restart_stage2,
                      Service#service{restart_count = 1,
                                      restart_times = [SecondsNow]},
                      ServiceId,
                      OldPid},
            Services
    end,
    {true, State#state{services = NewServices}};
    
restart_stage2(#service{restart_count = RestartCount,
                        restart_times = RestartTimes,
                        max_r = MaxR,
                        max_t = MaxT} = Service,
               Services, State, ServiceId, OldPid)
    when MaxR == RestartCount ->
    % last restart?
    SecondsNow = cloudi_timestamp:seconds(),
    NewRestartTimes = cloudi_timestamp:seconds_filter(RestartTimes,
                                                      SecondsNow, MaxT),
    NewRestartCount = erlang:length(NewRestartTimes),
    if
        NewRestartCount < RestartCount ->
            restart_stage2(Service#service{restart_count = NewRestartCount,
                                           restart_times = NewRestartTimes},
                           Services, State, ServiceId, OldPid);
        true ->
            ?LOG_WARN_SYNC("max restarts (MaxR = ~p, MaxT = ~p seconds) ~p~n"
                           " ~p", [MaxR, MaxT, OldPid, service_id(ServiceId)]),
            {false, State#state{services = Services}}
    end;

restart_stage2(#service{service_m = M,
                        service_f = F,
                        service_a = A,
                        process_index = ProcessIndex,
                        count_process = CountProcess,
                        restart_count = RestartCount,
                        restart_times = RestartTimes} = Service,
               Services, State, ServiceId, OldPid) ->
    % typical restart scenario
    SecondsNow = cloudi_timestamp:seconds(),
    R = RestartCount + 1,
    T = erlang:max(SecondsNow - lists:min(RestartTimes), 0),
    NewServices = case erlang:apply(M, F, [ProcessIndex, CountProcess | A]) of
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
                                    restart_count = R,
                                    restart_times = [SecondsNow |
                                                     RestartTimes]}, D)
            end, Services, Pids),
            ok = initialize(Pids),
            NextServices;
        {error, _} = Error ->
            ?LOG_ERROR_SYNC("failed ~p restart~n ~p",
                            [Error, service_id(ServiceId)]),
            self() ! {restart_stage2,
                      Service#service{restart_count = R,
                                      restart_times = [SecondsNow |
                                                       RestartTimes]},
                      ServiceId,
                      OldPid},
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
                            timeout_term = TimeoutTerm,
                            max_r = MaxR,
                            max_t = MaxT} = Service, ServiceId, Services) ->
    NewServices = case erlang:apply(M, F, [ProcessIndex, CountProcess | A]) of
        {ok, Pid} when is_pid(Pid) ->
            ?LOG_INFO("~p -> ~p (count_process_dynamic)",
                      [service_id(ServiceId), Pid]),
            Pids = [Pid],
            NextServices = cloudi_x_key2value:store(ServiceId, Pid,
                new_service_process(M, F, A,
                                    ProcessIndex, CountProcess, CountThread,
                                    Scope, Pids, erlang:monitor(process, Pid),
                                    TimeoutTerm, MaxR, MaxT), Services),
            ok = initialize(Pids),
            NextServices;
        {ok, [Pid | _] = Pids} when is_pid(Pid) ->
            ?LOG_INFO("~p -> ~p (count_process_dynamic)",
                      [service_id(ServiceId), Pids]),
            NextServices = lists:foldl(fun(P, D) ->
                cloudi_x_key2value:store(ServiceId, P,
                    new_service_process(M, F, A,
                                        ProcessIndex, CountProcess, CountThread,
                                        Scope, Pids, erlang:monitor(process, P),
                                        TimeoutTerm, MaxR, MaxT), D)
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
    ?LOG_INFO("count_process_dynamic: "
              "increasing ~p with ~p for ~p requests/second~n~p",
              [CountProcessCurrent, Count,
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
    ?LOG_INFO("count_process_dynamic: "
              "decreasing ~p with ~p for ~p requests/second~n~p",
              [CountProcessCurrent, Count,
               erlang:round(Rate * 10) / 10,
               [P || #service{pids = P} <- ServiceL]]),
    CountProcess = CountProcessCurrent + Count,
    NewServiceL = pids_decrease_loop(Count, lists:reverse(ServiceL)),
    {_,
     NewServices} = pids_update(NewServiceL, CountProcess, ServiceId, Services),
    NewServices.

terminate_kill_enforce(ServiceId, Pid, Reason,
                       #service{timeout_term = TimeoutTerm} = Service) ->
    erlang:send_after(TimeoutTerm, self(),
                      {kill, TimeoutTerm, Pid, Reason,
                       ServiceId, Service}),
    ok.

terminate_service(ServiceId, Pids, Service, Services) ->
    terminate_service(ServiceId, Pids, undefined, Service, Services).

terminate_service(ServiceId, Pids, Reason,
                  #service{timeout_term = TimeoutTerm} = Service, Services) ->
    Self = self(),
    ShutdownExit = if
        Reason =:= undefined ->
            shutdown;
        true ->
            {shutdown, Reason}
    end,
    NewServices = lists:foldl(fun(P, D) ->
        erlang:exit(P, ShutdownExit),
        erlang:send_after(TimeoutTerm, Self,
                          {kill, TimeoutTerm, P, ShutdownExit,
                           ServiceId, Service}),
        cloudi_x_key2value:erase(ServiceId, P, D)
    end, Services, Pids),
    NewServices.

service_id(ID) ->
    cloudi_x_uuid:uuid_to_string(ID, list_nodash).

new_service_process(M, F, A, ProcessIndex, CountProcess, CountThread,
                    Scope, Pids, MonitorRef, TimeoutTerm, MaxR, MaxT) ->
    #service{service_m = M,
             service_f = F,
             service_a = A,
             process_index = ProcessIndex,
             count_process = CountProcess,
             count_thread = CountThread,
             scope = Scope,
             pids = Pids,
             monitor = MonitorRef,
             timeout_term = TimeoutTerm,
             max_r = MaxR,
             max_t = MaxT}.

