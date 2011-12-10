%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
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
%%% Copyright (c) 2011, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011 Michael Truog
%%% @version 0.1.4 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_services).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).

%% external interface
-export([start_link/0,
         monitor/6,
         shutdown/1,
         restart/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("cloudi_logger.hrl").

-record(state,
    {
        services = key2value:new(dict) % {uuid, pid}  -> configuration
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

monitor(M, F, A, MaxR, MaxT, JobId)
    when is_atom(M), is_atom(F), is_list(A),
         is_integer(MaxR), MaxR >= 0, is_integer(MaxT), MaxT >= 0,
         is_binary(JobId), byte_size(JobId) == 16 ->
    gen_server:call(?MODULE, {monitor, M, F, A, MaxR, MaxT, JobId}).

shutdown(JobId)
    when is_binary(JobId), byte_size(JobId) == 16 ->
    gen_server:call(?MODULE, {shutdown, JobId}).

restart(JobId)
    when is_binary(JobId), byte_size(JobId) == 16 ->
    gen_server:call(?MODULE, {restart, JobId}).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([]) ->
    {ok, #state{}}.

handle_call({monitor, M, F, A, MaxR, MaxT, JobId}, _,
            #state{services = Services} = State) ->
    case erlang:apply(M, F, A) of
        {ok, Pid} when is_pid(Pid) ->
            NewServices =
                key2value:store(JobId, Pid,
                                #service{service_m = M,
                                         service_f = F,
                                         service_a = A,
                                         pids = [Pid],
                                         monitor = erlang:monitor(process, Pid),
                                         max_r = MaxR,
                                         max_t = MaxT}, Services),
            {reply, ok, State#state{services = NewServices}};
        {ok, [Pid | _] = Pids} when is_pid(Pid) ->
            NewServices = lists:foldl(fun(P, D) ->
                key2value:store(JobId, P,
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

handle_call({shutdown, JobId}, _,
            #state{services = Services} = State) ->
    case key2value:find1(JobId, Services) of
        {ok, {Pids, _}} ->
            NewServices = lists:foldl(fun(P, D) ->
                erlang:exit(P, kill),
                key2value:erase(JobId, P, D)
            end, Services, Pids),
            {reply, ok, State#state{services = NewServices}};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({restart, JobId}, _,
            #state{services = Services} = State) ->
    case key2value:find1(JobId, Services) of
        {ok, {Pids, _}} ->
            lists:foreach(fun(P) ->
                erlang:exit(P, restart)
            end, Pids),
            {reply, ok, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(Request, _, State) ->
    ?LOG_WARN("Unknown call \"~p\"", [Request]),
    {stop, string2:format("Unknown call \"~p\"", [Request]), error, State}.

handle_cast(Request, State) ->
    ?LOG_WARN("Unknown cast \"~p\"", [Request]),
    {noreply, State}.

handle_info({'DOWN', _MonitorRef, 'process', Pid, shutdown},
            #state{services = Services} = State) ->
    case key2value:find2(Pid, Services) of
        {ok, {[JobId], #service{service_m = M,
                                service_f = F,
                                service_a = A,
                                pids = Pids}}} ->
            ?LOG_INFO("Service pid ~p shutdown~n ~p:~p~p", [Pid, M, F, A]),
            NewServices = lists:foldl(fun(P, D) ->
                erlang:exit(P, shutdown),
                key2value:erase(JobId, P, D)
            end, Services, Pids),
            {noreply, State#state{services = NewServices}};
        error ->
            ?LOG_INFO("Service pid ~p does not exist for shutdown", [Pid]),
            {noreply, State}
    end;

handle_info({'DOWN', _MonitorRef, 'process', Pid, _Info},
            #state{services = Services} = State) ->
    case key2value:find2(Pid, Services) of
        {ok, {[JobId], Service}} ->
            {noreply, restart(Service, Services, State, JobId, Pid)};
        error ->
            % Pids started together as threads for one OS process may
            % have died together.  The first death triggers the restart and
            % removes all other pids from the Services data structure.
            {noreply, State}
    end;

handle_info({restart_stage2, Service, JobId, OldPid},
            #state{services = Services} = State) ->
    {noreply, restart_stage2(Service, Services, State, JobId, OldPid)};

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

restart(Service, Services, State, JobId, OldPid) ->
    restart_stage1(Service, Services, State, JobId, OldPid).

restart_stage1(#service{pids = Pids} = Service, Services,
               State, JobId, OldPid) ->
    NewServices = lists:foldl(fun(P, D) ->
        erlang:exit(P, kill),
        key2value:erase(JobId, P, D)
    end, Services, Pids),
    restart_stage2(Service#service{pids = [],
                                   monitor = undefined},
                   NewServices, State, JobId, OldPid).

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
               Services, State, JobId, OldPid) ->
    % first restart
    Now = erlang:now(),
    NewServices = case erlang:apply(M, F, A) of
        {ok, Pid} when is_pid(Pid) ->
            ?LOG_WARN("successful restart (R = 1)~n"
                      "                   (~p is now ~p)~n"
                      " ~p:~p~p", [OldPid, Pid, M, F, A]),
            Monitor = erlang:monitor(process, Pid),
            key2value:store(JobId, Pid,
                            Service#service{pids = [Pid],
                                            monitor = Monitor,
                                            restart_count = 1,
                                            restart_times = [Now]},
                            Services);
        {ok, [Pid | _] = Pids} when is_pid(Pid) ->
            ?LOG_WARN("successful restart (R = 1)~n"
                      "                   (~p is now one of ~p)~n"
                      " ~p:~p~p", [OldPid, Pids, M, F, A]),
            lists:foldl(fun(P, D) ->
                Monitor = erlang:monitor(process, P),
                key2value:store(JobId, P,
                                Service#service{pids = Pids,
                                                monitor = Monitor,
                                                restart_count = 1,
                                                restart_times = [Now]},
                                D)
            end, Services, Pids);
        {error, _} = Error ->
            ?LOG_ERROR("failed ~p restart~n ~p:~p~p", [Error, M, F, A]),
            self() ! {restart_stage2,
                      Service#service{restart_count = 1,
                                      restart_times = [Now]},
                      JobId,
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
               Services, State, JobId, OldPid)
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
                           Services, State, JobId, OldPid);
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
               Services, State, JobId, OldPid) ->
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
            key2value:store(JobId, Pid,
                            Service#service{pids = [Pid],
                                            monitor = Monitor,
                                            restart_count = R,
                                            restart_times =
                                                [Now | RestartTimes]},
                            Services);
        {ok, [Pid | _] = Pids} when is_pid(Pid) ->
            ?LOG_WARN("successful restart (R = ~p, T = ~p elapsed seconds)~n"
                      "                   (~p is now one of ~p)~n"
                      " ~p:~p~p", [R, T, OldPid, Pids, M, F, A]),
            lists:foldl(fun(P, D) ->
                Monitor = erlang:monitor(process, P),
                key2value:store(JobId, P,
                                Service#service{pids = Pids,
                                                monitor = Monitor,
                                                restart_count = R,
                                                restart_times =
                                                    [Now | RestartTimes]},
                                D)
            end, Services, Pids);
        {error, _} = Error ->
            ?LOG_ERROR("failed ~p restart~n ~p:~p~p", [Error, M, F, A]),
            self() ! {restart_stage2,
                      Service#service{restart_count = R,
                                      restart_times = [Now | RestartTimes]}},
            Services
    end,
    State#state{services = NewServices}.

