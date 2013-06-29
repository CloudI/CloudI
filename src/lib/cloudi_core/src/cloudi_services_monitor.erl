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
%%% Copyright (c) 2011-2013, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011-2013 Michael Truog
%%% @version 1.2.5 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_services_monitor).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).

%% external interface
-export([start_link/0,
         monitor/7,
         shutdown/2,
         restart/2,
         search/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("cloudi_logger.hrl").

-define(CATCH_EXIT(F),
        try F catch exit:{Reason, _} -> {error, Reason} end).

-record(state,
    {
        services = cloudi_x_key2value:new(dict) % {cloudi_x_uuid, pid} ->
                                                %     configuration
    }).

-record(service,
    {
        service_m,
        service_f,
        service_a,
        pids,
        monitor,
        restart_count = 0,
        restart_times = [],
        max_r, % from the supervisor behavior documentation:
        max_t  % If more than MaxR restarts occur within MaxT seconds,
               % the supervisor terminates all child processes...
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

monitor(M, F, A, MaxR, MaxT, ServiceId, Timeout)
    when is_atom(M), is_atom(F), is_list(A),
         is_integer(MaxR), MaxR >= 0, is_integer(MaxT), MaxT >= 0,
         is_binary(ServiceId), byte_size(ServiceId) == 16 ->
    ?CATCH_EXIT(gen_server:call(?MODULE,
                                {monitor, M, F, A, MaxR, MaxT, ServiceId},
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

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([]) ->
    {ok, #state{}}.

handle_call({monitor, M, F, A, MaxR, MaxT, ServiceId}, _,
            #state{services = Services} = State) ->
    case erlang:apply(M, F, A) of
        {ok, Pid} when is_pid(Pid) ->
            ?LOG_INFO("~p ~p -> ~p", [F, A, Pid]),
            NewServices =
                cloudi_x_key2value:store(ServiceId, Pid,
                    #service{service_m = M,
                             service_f = F,
                             service_a = A,
                             pids = [Pid],
                             monitor = erlang:monitor(process, Pid),
                             max_r = MaxR,
                             max_t = MaxT}, Services),
            {reply, ok, State#state{services = NewServices}};
        {ok, [Pid | _] = Pids} when is_pid(Pid) ->
            ?LOG_INFO("~p ~p -> ~p", [F, A, Pids]),
            NewServices = lists:foldl(fun(P, D) ->
                cloudi_x_key2value:store(ServiceId, P,
                    #service{service_m = M,
                             service_f = F,
                             service_a = A,
                             pids = Pids,
                             monitor = erlang:monitor(process, P),
                             max_r = MaxR,
                             max_t = MaxT}, D)
            end, Services, Pids),
            {reply, ok, State#state{services = NewServices}};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({shutdown, ServiceId}, _,
            #state{services = Services} = State) ->
    case cloudi_x_key2value:find1(ServiceId, Services) of
        {ok, {Pids, _}} ->
            NewServices = lists:foldl(fun(P, D) ->
                erlang:exit(P, kill),
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

handle_call(Request, _, State) ->
    ?LOG_WARN("Unknown call \"~p\"", [Request]),
    {stop, cloudi_string:format("Unknown call \"~p\"", [Request]),
     error, State}.

handle_cast(Request, State) ->
    ?LOG_WARN("Unknown cast \"~p\"", [Request]),
    {noreply, State}.

handle_info({'DOWN', _MonitorRef, 'process', Pid, shutdown},
            #state{services = Services} = State) ->
    case cloudi_x_key2value:find2(Pid, Services) of
        {ok, {[ServiceId], #service{service_m = M,
                                    service_f = F,
                                    service_a = A,
                                    pids = Pids}}} ->
            ?LOG_INFO("Service pid ~p shutdown~n ~p:~p~p", [Pid, M, F, A]),
            NewServices = lists:foldl(fun(P, D) ->
                erlang:exit(P, shutdown),
                cloudi_x_key2value:erase(ServiceId, P, D)
            end, Services, Pids),
            {noreply, State#state{services = NewServices}};
        error ->
            ?LOG_INFO("Service pid ~p does not exist for shutdown", [Pid]),
            {noreply, State}
    end;

handle_info({'DOWN', _MonitorRef, 'process', Pid, Info},
            #state{services = Services} = State) ->
    case cloudi_x_key2value:find2(Pid, Services) of
        {ok, {[ServiceId], #service{service_m = M,
                                    service_f = F,
                                    service_a = A} = Service}} ->
            ?LOG_WARN("Service pid ~p error: ~p~n ~p:~p~p~n",
                      [Pid, Info, M, F, A]),
            {noreply, restart(Service, Services, State, ServiceId, Pid)};
        error ->
            % Pids started together as threads for one OS process may
            % have died together.  The first death triggers the restart and
            % removes all other pids from the Services data structure.
            {noreply, State}
    end;

handle_info({restart_stage2, Service, ServiceId, OldPid},
            #state{services = Services} = State) ->
    {noreply, restart_stage2(Service, Services, State, ServiceId, OldPid)};

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
    NewServices = lists:foldl(fun(P, D) ->
        erlang:exit(P, kill),
        cloudi_x_key2value:erase(ServiceId, P, D)
    end, Services, Pids),
    restart_stage2(Service#service{pids = [],
                                   monitor = undefined},
                   NewServices, State, ServiceId, OldPid).

restart_stage2(#service{service_m = M,
                        service_f = F,
                        service_a = A,
                        restart_count = 0,
                        max_r = 0},
               Services, State, _, OldPid) ->
    % no restarts allowed
    ?LOG_WARN("max restarts (MaxR = 0) ~p~n ~p:~p~p", [OldPid, M, F, A]),
    State#state{services = Services};

restart_stage2(#service{service_m = M,
                        service_f = F,
                        service_a = A,
                        restart_count = 0,
                        restart_times = []} = Service,
               Services, State, ServiceId, OldPid) ->
    % first restart
    Now = erlang:now(),
    NewServices = case erlang:apply(M, F, A) of
        {ok, Pid} when is_pid(Pid) ->
            ?LOG_WARN("successful restart (R = 1)~n"
                      "                   (~p is now ~p)~n"
                      " ~p:~p~p", [OldPid, Pid, M, F, A]),
            Monitor = erlang:monitor(process, Pid),
            cloudi_x_key2value:store(ServiceId, Pid,
                Service#service{pids = [Pid],
                                monitor = Monitor,
                                restart_count = 1,
                                restart_times = [Now]}, Services);
        {ok, [Pid | _] = Pids} when is_pid(Pid) ->
            ?LOG_WARN("successful restart (R = 1)~n"
                      "                   (~p is now one of ~p)~n"
                      " ~p:~p~p", [OldPid, Pids, M, F, A]),
            lists:foldl(fun(P, D) ->
                Monitor = erlang:monitor(process, P),
                cloudi_x_key2value:store(ServiceId, P,
                    Service#service{pids = Pids,
                                    monitor = Monitor,
                                    restart_count = 1,
                                    restart_times = [Now]}, D)
            end, Services, Pids);
        {error, _} = Error ->
            ?LOG_ERROR("failed ~p restart~n ~p:~p~p", [Error, M, F, A]),
            self() ! {restart_stage2,
                      Service#service{restart_count = 1,
                                      restart_times = [Now]},
                      ServiceId,
                      OldPid},
            Services
    end,
    State#state{services = NewServices};
    
restart_stage2(#service{service_m = M,
                        service_f = F,
                        service_a = A,
                        restart_count = RestartCount,
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
                      " ~p:~p~p", [MaxR, MaxT, OldPid, M, F, A]),
            State#state{services = Services}
    end;

restart_stage2(#service{service_m = M,
                        service_f = F,
                        service_a = A,
                        restart_count = RestartCount,
                        restart_times = RestartTimes} = Service,
               Services, State, ServiceId, OldPid) ->
    % typical restart scenario
    Now = erlang:now(),
    R = RestartCount + 1,
    T = erlang:trunc(timer:now_diff(Now, lists:last(RestartTimes)) * 1.0e-6),
    NewServices = case erlang:apply(M, F, A) of
        {ok, Pid} when is_pid(Pid) ->
            ?LOG_WARN("successful restart (R = ~p, T = ~p elapsed seconds)~n"
                      "                   (~p is now ~p)~n"
                      " ~p:~p~p", [R, T, OldPid, Pid, M, F, A]),
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
                      " ~p:~p~p", [R, T, OldPid, Pids, M, F, A]),
            lists:foldl(fun(P, D) ->
                Monitor = erlang:monitor(process, P),
                cloudi_x_key2value:store(ServiceId, P,
                    Service#service{pids = Pids,
                                    monitor = Monitor,
                                    restart_count = R,
                                    restart_times = [Now | RestartTimes]}, D)
            end, Services, Pids);
        {error, _} = Error ->
            ?LOG_ERROR("failed ~p restart~n ~p:~p~p", [Error, M, F, A]),
            self() ! {restart_stage2,
                      Service#service{restart_count = R,
                                      restart_times = [Now | RestartTimes]},
                      ServiceId,
                      OldPid},
            Services
    end,
    State#state{services = NewServices}.

