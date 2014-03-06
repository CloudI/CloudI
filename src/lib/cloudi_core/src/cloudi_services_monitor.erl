%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Services==
%%% Manage all cloudi_spawn processes with monitors and their configuration.
%%% Perform process restarts but do not escalate failures (only log failures).
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2011-2014, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011-2014 Michael Truog
%%% @version 1.3.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_services_monitor).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).

%% external interface
-export([start_link/0,
         monitor/9,
         shutdown/2,
         restart/2,
         search/2,
         pids/2,
         increase/5,
         decrease/5]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("cloudi_logger.hrl").

-define(CATCH_EXIT(F),
        try F catch exit:{Reason, _} -> {error, Reason} end).
-define(TERMINATE_DELAY_MIN,  1000). % milliseconds
-define(TERMINATE_DELAY_MAX, 60000). % milliseconds

-record(state,
    {
        services = cloudi_x_key2value:new(dict), % {cloudi_x_uuid, pid} ->
                                                 %     configuration
        changes = dict:new() :: dict() % cloudi_x_uuid -> list()
    }).

-record(service,
    {
        service_m :: atom(),
        service_f :: fun(),
        service_a :: list(),
        process_index :: non_neg_integer(),
        count_thread :: pos_integer(),
        % pids is only accurate on the pid lookup (find2)
        % due to the overwrite of #service{}
        pids :: list(pid()),
        monitor :: reference(),
        restart_count = 0 :: non_neg_integer(),
        restart_times = [] :: list(erlang:timestamp()),
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

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

monitor(M, F, A, ProcessIndex, CountThread, MaxR, MaxT, ServiceId, Timeout)
    when is_atom(M), is_atom(F), is_list(A),
         is_integer(ProcessIndex), ProcessIndex >= 0,
         is_integer(CountThread), CountThread > 0,
         is_integer(MaxR), MaxR >= 0, is_integer(MaxT), MaxT >= 0,
         is_binary(ServiceId), byte_size(ServiceId) == 16 ->
    ?CATCH_EXIT(gen_server:call(?MODULE,
                                {monitor, M, F, A,
                                 ProcessIndex, CountThread, 
                                 MaxR, MaxT, ServiceId},
                                Timeout)).

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

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([]) ->
    {ok, #state{}}.

handle_call({monitor, M, F, A, ProcessIndex, CountThread,
             MaxR, MaxT, ServiceId}, _,
            #state{services = Services} = State) ->
    case erlang:apply(M, F, [ProcessIndex | A]) of
        {ok, Pid} when is_pid(Pid) ->
            NewServices =
                cloudi_x_key2value:store(ServiceId, Pid,
                    #service{service_m = M,
                             service_f = F,
                             service_a = A,
                             process_index = ProcessIndex,
                             count_thread = CountThread,
                             pids = [Pid],
                             monitor = erlang:monitor(process, Pid),
                             max_r = MaxR,
                             max_t = MaxT}, Services),
            {reply, {ok, Pid}, State#state{services = NewServices}};
        {ok, [Pid | _] = Pids} when is_pid(Pid) ->
            NewServices = lists:foldl(fun(P, D) ->
                cloudi_x_key2value:store(ServiceId, P,
                    #service{service_m = M,
                             service_f = F,
                             service_a = A,
                             process_index = ProcessIndex,
                             count_thread = CountThread,
                             pids = Pids,
                             monitor = erlang:monitor(process, P),
                             max_r = MaxR,
                             max_t = MaxT}, D)
            end, Services, Pids),
            {reply, {ok, Pids}, State#state{services = NewServices}};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({shutdown, ServiceId}, _,
            #state{services = Services} = State) ->
    case cloudi_x_key2value:find1(ServiceId, Services) of
        {ok, {Pids, #service{max_r = MaxR,
                             max_t = MaxT} = Service}} ->
            Self = self(),
            Shutdown = terminate_delay(MaxT, MaxR),
            NewServices = lists:foldl(fun(P, D) ->
                erlang:exit(P, shutdown),
                erlang:send_after(Shutdown, Self,
                                  {kill, Shutdown, P, ServiceId, Service}),
                cloudi_x_key2value:erase(ServiceId, P, D)
            end, Services, Pids),
            {reply, ok, State#state{services = NewServices}};
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
        {ok, {PidList, _}} ->
            {reply, {ok, PidList}, State};
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
                    Services;
                I > 0 ->
                    ?LOG_INFO("count_process_dynamic: "
                              "increasing ~p with ~p for ~p requests/second",
                              [CountProcessCurrent, I,
                               erlang:round(Rate * 10) / 10]),
                    pids_increase(I, Pids, ServiceId, Services);
                I < 0 ->
                    ?LOG_INFO("count_process_dynamic: "
                              "decreasing ~p with ~p for ~p requests/second",
                              [CountProcessCurrent, I,
                               erlang:round(Rate * 10) / 10]),
                    pids_decrease(I, Pids, ServiceId, Services)
            end,
            {noreply, State#state{services = NewServices,
                                  changes = NewChanges}};
        error ->
            {noreply, State#state{changes = NewChanges}}
    end;

handle_info({'DOWN', _MonitorRef, 'process', Pid, shutdown},
            #state{services = Services} = State) ->
    case cloudi_x_key2value:find2(Pid, Services) of
        {ok, {[ServiceId], #service{pids = Pids}}} ->
            ?LOG_INFO("Service pid ~p shutdown~n ~p",
                      [Pid, cloudi_x_uuid:uuid_to_string(ServiceId)]),
            NewServices = lists:foldl(fun(P, D) ->
                erlang:exit(P, shutdown),
                cloudi_x_key2value:erase(ServiceId, P, D)
            end, Services, Pids),
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
                      [Pid, cloudi_x_uuid:uuid_to_string(ServiceId)]),
            NewServices = cloudi_x_key2value:erase(ServiceId, Pid, Services),
            {noreply, State#state{services = NewServices}};
        error ->
            {noreply, State}
    end;

handle_info({'DOWN', _MonitorRef, 'process', Pid, Info},
            #state{services = Services} = State) ->
    case cloudi_x_key2value:find2(Pid, Services) of
        {ok, {[ServiceId], #service{} = Service}} ->
            ?LOG_WARN("Service pid ~p error: ~p~n ~p",
                      [Pid, Info, cloudi_x_uuid:uuid_to_string(ServiceId)]),
            case restart(Service, Services, State, ServiceId, Pid) of
                {true, NewState} ->
                    {noreply, NewState};
                {false, NewState} ->
                    cloudi_configurator:service_dead(ServiceId),
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
            cloudi_configurator:service_dead(ServiceId),
            {noreply, NewState}
    end;

handle_info({kill, Shutdown, Pid, ServiceId, #service{}}, State) ->
    case erlang:is_process_alive(Pid) of
        true ->
            ?LOG_ERROR("Service pid ~p brutal_kill after ~p ms (MaxT/MaxR)~n"
                       " ~p",[Pid, Shutdown,
                              cloudi_x_uuid:uuid_to_string(ServiceId)]),
            erlang:exit(Pid, kill);
        false ->
            ok
    end,
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

restart_stage1(#service{pids = Pids,
                        max_r = MaxR,
                        max_t = MaxT} = Service, Services,
               State, ServiceId, OldPid) ->
    Self = self(),
    Shutdown = terminate_delay(MaxT, MaxR),
    NewServices = lists:foldl(fun(P, D) ->
        erlang:exit(P, shutdown),
        erlang:send_after(Shutdown, Self,
                          {kill, Shutdown, P, ServiceId, Service}),
        cloudi_x_key2value:erase(ServiceId, P, D)
    end, Services, Pids),
    restart_stage2(Service#service{pids = [],
                                   monitor = undefined},
                   NewServices, State, ServiceId, OldPid).

restart_stage2(#service{restart_count = 0,
                        max_r = 0},
               Services, State, ServiceId, OldPid) ->
    % no restarts allowed
    ?LOG_WARN("max restarts (MaxR = 0) ~p~n ~p",
              [OldPid, cloudi_x_uuid:uuid_to_string(ServiceId)]),
    {false, State#state{services = Services}};

restart_stage2(#service{service_m = M,
                        service_f = F,
                        service_a = A,
                        process_index = ProcessIndex,
                        restart_count = 0,
                        restart_times = []} = Service,
               Services, State, ServiceId, OldPid) ->
    % first restart
    Now = erlang:now(),
    NewServices = case erlang:apply(M, F, [ProcessIndex | A]) of
        {ok, Pid} when is_pid(Pid) ->
            ?LOG_WARN("successful restart (R = 1)~n"
                      "                   (~p is now ~p)~n"
                      " ~p", [OldPid, Pid,
                              cloudi_x_uuid:uuid_to_string(ServiceId)]),
            Monitor = erlang:monitor(process, Pid),
            cloudi_x_key2value:store(ServiceId, Pid,
                Service#service{pids = [Pid],
                                monitor = Monitor,
                                restart_count = 1,
                                restart_times = [Now]}, Services);
        {ok, [Pid | _] = Pids} when is_pid(Pid) ->
            ?LOG_WARN("successful restart (R = 1)~n"
                      "                   (~p is now one of ~p)~n"
                      " ~p", [OldPid, Pids,
                              cloudi_x_uuid:uuid_to_string(ServiceId)]),
            lists:foldl(fun(P, D) ->
                Monitor = erlang:monitor(process, P),
                cloudi_x_key2value:store(ServiceId, P,
                    Service#service{pids = Pids,
                                    monitor = Monitor,
                                    restart_count = 1,
                                    restart_times = [Now]}, D)
            end, Services, Pids);
        {error, _} = Error ->
            ?LOG_ERROR("failed ~p restart~n ~p",
                       [Error, cloudi_x_uuid:uuid_to_string(ServiceId)]),
            self() ! {restart_stage2,
                      Service#service{restart_count = 1,
                                      restart_times = [Now]},
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
    Now = erlang:now(),
    NewRestartTimes = lists:reverse(lists:dropwhile(fun(T) ->
        erlang:trunc(timer:now_diff(Now, T) * 1.0e-6) > MaxT
    end, lists:reverse(RestartTimes))),
    NewRestartCount = erlang:length(NewRestartTimes),
    if
        NewRestartCount < RestartCount ->
            restart_stage2(Service#service{restart_count = NewRestartCount,
                                           restart_times = NewRestartTimes},
                           Services, State, ServiceId, OldPid);
        true ->
            ?LOG_WARN("max restarts (MaxR = ~p, MaxT = ~p seconds) ~p~n"
                      " ~p", [MaxR, MaxT, OldPid,
                              cloudi_x_uuid:uuid_to_string(ServiceId)]),
            {false, State#state{services = Services}}
    end;

restart_stage2(#service{service_m = M,
                        service_f = F,
                        service_a = A,
                        process_index = ProcessIndex,
                        restart_count = RestartCount,
                        restart_times = RestartTimes} = Service,
               Services, State, ServiceId, OldPid) ->
    % typical restart scenario
    Now = erlang:now(),
    R = RestartCount + 1,
    T = erlang:trunc(timer:now_diff(Now, lists:last(RestartTimes)) * 1.0e-6),
    NewServices = case erlang:apply(M, F, [ProcessIndex | A]) of
        {ok, Pid} when is_pid(Pid) ->
            ?LOG_WARN("successful restart (R = ~p, T = ~p elapsed seconds)~n"
                      "                   (~p is now ~p)~n"
                      " ~p", [R, T, OldPid, Pid,
                              cloudi_x_uuid:uuid_to_string(ServiceId)]),
            Monitor = erlang:monitor(process, Pid),
            cloudi_x_key2value:store(ServiceId, Pid,
                Service#service{pids = [Pid],
                                monitor = Monitor,
                                restart_count = R,
                                restart_times = [Now | RestartTimes]},
                Services);
        {ok, [Pid | _] = Pids} when is_pid(Pid) ->
            ?LOG_WARN("successful restart (R = ~p, T = ~p elapsed seconds)~n"
                      "                   (~p is now one of ~p)~n"
                      " ~p", [R, T, OldPid, Pids,
                              cloudi_x_uuid:uuid_to_string(ServiceId)]),
            lists:foldl(fun(P, D) ->
                Monitor = erlang:monitor(process, P),
                cloudi_x_key2value:store(ServiceId, P,
                    Service#service{pids = Pids,
                                    monitor = Monitor,
                                    restart_count = R,
                                    restart_times = [Now | RestartTimes]}, D)
            end, Services, Pids);
        {error, _} = Error ->
            ?LOG_ERROR("failed ~p restart~n ~p",
                       [Error, cloudi_x_uuid:uuid_to_string(ServiceId)]),
            self() ! {restart_stage2,
                      Service#service{restart_count = R,
                                      restart_times = [Now | RestartTimes]},
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
            NewPids = lists:foldl(fun lists:delete/2, Pids, ThreadPids),
            service_instance(NewPids,
                             lists:keymerge(#service.process_index,
                                            Results, [Service]),
                             ServiceId, Services)
    end.

pids_increase_loop(0, _, _, _, Services) ->
    Services;
pids_increase_loop(Count, ProcessIndex,
                   #service{service_m = M,
                            service_f = F,
                            service_a = A,
                            count_thread = CountThread,
                            max_r = MaxR,
                            max_t = MaxT} = Service, ServiceId, Services) ->
    NewServices = case erlang:apply(M, F, [ProcessIndex | A]) of
        {ok, Pid} when is_pid(Pid) ->
            ?LOG_INFO("~p -> ~p (count_process_dynamic)",
                      [cloudi_x_uuid:uuid_to_string(ServiceId), Pid]),
            cloudi_x_key2value:store(ServiceId, Pid,
                #service{service_m = M,
                         service_f = F,
                         service_a = A,
                         process_index = ProcessIndex,
                         count_thread = CountThread,
                         pids = [Pid],
                         monitor = erlang:monitor(process, Pid),
                         max_r = MaxR,
                         max_t = MaxT}, Services);
        {ok, [Pid | _] = Pids} when is_pid(Pid) ->
            ?LOG_INFO("~p -> ~p (count_process_dynamic)",
                      [cloudi_x_uuid:uuid_to_string(ServiceId), Pids]),
            lists:foldl(fun(P, D) ->
                cloudi_x_key2value:store(ServiceId, P,
                    #service{service_m = M,
                             service_f = F,
                             service_a = A,
                             process_index = ProcessIndex,
                             count_thread = CountThread,
                             pids = Pids,
                             monitor = erlang:monitor(process, P),
                             max_r = MaxR,
                             max_t = MaxT}, D)
            end, Services, Pids);
        {error, _} = Error ->
            ?LOG_ERROR("failed ~p increase (count_process_dynamic)~n ~p",
                       [Error, cloudi_x_uuid:uuid_to_string(ServiceId)]),
            Services
    end,
    pids_increase_loop(Count - 1, ProcessIndex + 1,
                       Service, ServiceId, NewServices).

pids_increase(Count, OldPids, ServiceId, Services) ->
    [Service | _] = lists:reverse(service_instance(OldPids,
                                                   ServiceId, Services)),
    #service{process_index = ProcessIndex} = Service,
    pids_increase_loop(Count, ProcessIndex + 1, Service, ServiceId, Services).

pids_decrease_loop(0, _) ->
    ok;
pids_decrease_loop(Count, [#service{pids = Pids} | ServiceL]) ->
    lists:foreach(fun(P) ->
        cloudi_rate_based_configuration:count_process_dynamic_terminate(P)
    end, Pids),
    pids_decrease_loop(Count + 1, ServiceL).

pids_decrease(Count, OldPids, ServiceId, Services) ->
    ServiceL = lists:reverse(service_instance(OldPids, ServiceId, Services)),
    pids_decrease_loop(Count, ServiceL),
    Services.

terminate_delay(_, 0) ->
    ?TERMINATE_DELAY_MIN;
terminate_delay(0, _) ->
    ?TERMINATE_DELAY_MIN;
terminate_delay(MaxT, MaxR) ->
    erlang:min(erlang:max(erlang:round((1000 * MaxT) / MaxR),
                          ?TERMINATE_DELAY_MIN),
               ?TERMINATE_DELAY_MAX).

