%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Internal Service==
%%% Erlang process which manages internal service requests and info messages
%%% for modules that implement the cloudi_service behavior.
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
%%% @version 1.2.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_services_internal).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).

%% external interface
-export([start_link/11]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("cloudi_configuration.hrl").
-include("cloudi_logger.hrl").
-include("cloudi_constants.hrl").

-record(state,
    {
        % common elements for cloudi_services_common.hrl
        dispatcher,                    % self()
        send_timeouts = dict:new(),    % tracking for send timeouts
        recv_timeouts = dict:new(),    % tracking for recv timeouts
        async_responses = dict:new(),  % tracking for async messages
        queue_requests = true,         % is the request pid busy?
        queued = cloudi_x_pqueue4:new(),        % queued incoming messages
        % unique state elements
        queued_info = queue:new(),     % queue process messages for service
        module,                        % service module
        service_state,                 % service state
        process_index,                 % 0-based index of the Erlang process
        prefix,                        % subscribe/unsubscribe name prefix
        timeout_async,                 % default timeout for send_async
        timeout_sync,                  % default timeout for send_sync
        duo_mode_pid,                  % dual mode pid
        request_pid = undefined,       % request pid
        info_pid = undefined,          % info pid
        uuid_generator,                % transaction id generator
        dest_refresh,                  % immediate_closest | lazy_closest |
                                       % immediate_furthest | lazy_furthest |
                                       % immediate_random | lazy_random |
                                       % immediate_local | lazy_local |
                                       % immediate_remote | lazy_remote |
                                       % immediate_newest | lazy_newest |
                                       % immediate_oldest | lazy_oldest ,
                                       % destination pid refresh
        cpg_data = cloudi_x_cpg_data:get_empty_groups(), % dest_refresh lazy
        dest_deny,                     % denied from sending to a destination
        dest_allow,                    % allowed to send to a destination
        options                        % #config_service_options{}
    }).

% used when duo_mode is true
-record(state_duo,
    {
        % common elements for cloudi_services_common.hrl
        duo_mode_pid,                  % self()
        unused_element1,               % (send_timeouts)
        recv_timeouts = dict:new(),    % tracking for recv timeouts
        unused_element2,               % (async_responses)
        queue_requests = true,         % is the request pid busy?
        queued = cloudi_x_pqueue4:new(),        % queued incoming messages
        % unique state elements
        queued_info = queue:new(),     % queue process messages for service
        dispatcher,                    % main dispatcher pid
        module,                        % service module
        service_state,                 % service state
        request_pid = undefined,       % request pid
        options                        % #config_service_options{}
    }).

-include("cloudi_services_common.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

start_link(ProcessIndex, Module, Args, Timeout, Prefix,
           TimeoutAsync, TimeoutSync,
           DestRefresh, DestDeny, DestAllow, ConfigOptions)
    when is_integer(ProcessIndex), is_atom(Module), is_list(Args),
         is_integer(Timeout), is_list(Prefix),
         is_integer(TimeoutAsync), is_integer(TimeoutSync),
         is_record(ConfigOptions, config_service_options) ->
    true = (DestRefresh =:= immediate_closest) orelse
           (DestRefresh =:= lazy_closest) orelse
           (DestRefresh =:= immediate_furthest) orelse
           (DestRefresh =:= lazy_furthest) orelse
           (DestRefresh =:= immediate_random) orelse
           (DestRefresh =:= lazy_random) orelse
           (DestRefresh =:= immediate_local) orelse
           (DestRefresh =:= lazy_local) orelse
           (DestRefresh =:= immediate_remote) orelse
           (DestRefresh =:= lazy_remote) orelse
           (DestRefresh =:= immediate_newest) orelse
           (DestRefresh =:= lazy_newest) orelse
           (DestRefresh =:= immediate_oldest) orelse
           (DestRefresh =:= lazy_oldest) orelse
           (DestRefresh =:= none),
    gen_server:start_link(?MODULE,
                          [ProcessIndex, Module, Args, Timeout, Prefix,
                           TimeoutAsync, TimeoutSync, DestRefresh,
                           DestDeny, DestAllow, ConfigOptions],
                          [{timeout, Timeout + ?TIMEOUT_DELTA}]).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([ProcessIndex, Module, Args, Timeout, Prefix, TimeoutAsync, TimeoutSync,
      DestRefresh, DestDeny, DestAllow, ConfigOptions]) ->
    Dispatcher = self(),
    Dispatcher ! {'cloudi_service_init', Args, Timeout},
    DuoModePid = if
        ConfigOptions#config_service_options.duo_mode =:= true ->
            proc_lib:spawn_opt(fun() ->
                duo_mode_loop_init(#state_duo{duo_mode_pid = self(),
                                              dispatcher = Dispatcher,
                                              module = Module,
                                              options = ConfigOptions})
            end, ConfigOptions#config_service_options.info_pid_options);
        true ->
            undefined
    end,
    cloudi_x_quickrand:seed(),
    destination_refresh_first(DestRefresh, ConfigOptions),
    {ok, #state{dispatcher = Dispatcher,
                module = Module,
                process_index = ProcessIndex,
                prefix = Prefix,
                timeout_async = TimeoutAsync,
                timeout_sync = TimeoutSync,
                duo_mode_pid = DuoModePid,
                uuid_generator = cloudi_x_uuid:new(Dispatcher),
                dest_refresh = DestRefresh,
                dest_deny = DestDeny,
                dest_allow = DestAllow,
                options = ConfigOptions}}.

handle_call(process_index, _, #state{process_index = ProcessIndex} = State) ->
    {reply, ProcessIndex, State};

handle_call(self, _, #state{dispatcher = Dispatcher,
                            duo_mode_pid = DuoModePid} = State) ->
    if
        is_pid(DuoModePid) ->
            {reply, DuoModePid, State};
        true ->
            {reply, Dispatcher, State}
    end;

handle_call({'subscribe', Pattern}, _,
            #state{dispatcher = Dispatcher,
                   prefix = Prefix,
                   duo_mode_pid = DuoModePid} = State) ->
    Receiver = if
        is_pid(DuoModePid) ->
            DuoModePid;
        true ->
            Dispatcher
    end,
    ok = cloudi_x_cpg:join(Prefix ++ Pattern, Receiver),
    {reply, ok, State};

handle_call({'unsubscribe', Pattern}, _,
            #state{dispatcher = Dispatcher,
                   prefix = Prefix,
                   duo_mode_pid = DuoModePid} = State) ->
    Receiver = if
        is_pid(DuoModePid) ->
            DuoModePid;
        true ->
            Dispatcher
    end,
    ok = cloudi_x_cpg:leave(Prefix ++ Pattern, Receiver),
    {reply, ok, State};

handle_call({'get_pid', Name}, Client,
            #state{timeout_sync = TimeoutSync} = State) ->
    handle_call({'get_pid', Name, TimeoutSync}, Client, State);

handle_call({'get_pid', Name, Timeout}, Client,
            #state{dest_deny = DestDeny,
                   dest_allow = DestAllow} = State) ->
    case destination_allowed(Name, DestDeny, DestAllow) of
        true ->
            handle_get_pid(Name, Timeout, Client, State);
        false ->
            {reply, {error, timeout}, State}
    end;

handle_call({'send_async', Name, RequestInfo, Request,
             undefined, Priority}, Client,
            #state{timeout_async = TimeoutAsync} = State) ->
    handle_call({'send_async', Name, RequestInfo, Request,
                 TimeoutAsync, Priority}, Client, State);

handle_call({'send_async', Name, RequestInfo, Request,
             Timeout, undefined}, Client,
            #state{options = ConfigOptions} = State) ->
    PriorityDefault = ConfigOptions#config_service_options.priority_default,
    handle_call({'send_async', Name, RequestInfo, Request,
                 Timeout, PriorityDefault}, Client, State);

handle_call({'send_async', Name, RequestInfo, Request,
             Timeout, Priority}, Client,
            #state{dest_deny = DestDeny,
                   dest_allow = DestAllow} = State) ->
    case destination_allowed(Name, DestDeny, DestAllow) of
        true ->
            handle_send_async(Name, RequestInfo, Request,
                              Timeout, Priority, Client, State);
        false ->
            {reply, {error, timeout}, State}
    end;

handle_call({'send_async', Name, RequestInfo, Request,
             undefined, Priority, PatternPid}, Client,
            #state{timeout_async = TimeoutAsync} = State) ->
    handle_call({'send_async', Name, RequestInfo, Request,
                 TimeoutAsync, Priority, PatternPid}, Client, State);

handle_call({'send_async', Name, RequestInfo, Request,
             Timeout, undefined, PatternPid}, Client,
            #state{options = ConfigOptions} = State) ->
    PriorityDefault = ConfigOptions#config_service_options.priority_default,
    handle_call({'send_async', Name, RequestInfo, Request,
                 Timeout, PriorityDefault, PatternPid}, Client, State);

handle_call({'send_async', Name, RequestInfo, Request,
             Timeout, Priority, {Pattern, Pid}}, _,
            State) ->
    handle_send_async_pid(Name, Pattern, RequestInfo, Request,
                          Timeout, Priority, Pid, State);

handle_call({'send_async_active', Name, RequestInfo, Request,
             undefined, Priority}, Client,
            #state{timeout_async = TimeoutAsync} = State) ->
    handle_call({'send_async_active', Name, RequestInfo, Request,
                 TimeoutAsync, Priority}, Client, State);

handle_call({'send_async_active', Name, RequestInfo, Request,
             Timeout, undefined}, Client,
            #state{options = ConfigOptions} = State) ->
    PriorityDefault = ConfigOptions#config_service_options.priority_default,
    handle_call({'send_async_active', Name, RequestInfo, Request,
                 Timeout, PriorityDefault}, Client, State);

handle_call({'send_async_active', Name, RequestInfo, Request,
             Timeout, Priority}, Client,
            #state{dest_deny = DestDeny,
                   dest_allow = DestAllow} = State) ->
    case destination_allowed(Name, DestDeny, DestAllow) of
        true ->
            handle_send_async_active(Name, RequestInfo, Request,
                                     Timeout, Priority, Client, State);
        false ->
            {reply, {error, timeout}, State}
    end;

handle_call({'send_async_active', Name, RequestInfo, Request,
             undefined, Priority, PatternPid}, Client,
            #state{timeout_async = TimeoutAsync} = State) ->
    handle_call({'send_async_active', Name, RequestInfo, Request,
                 TimeoutAsync, Priority, PatternPid}, Client, State);

handle_call({'send_async_active', Name, RequestInfo, Request,
             Timeout, undefined, PatternPid}, Client,
            #state{options = ConfigOptions} = State) ->
    PriorityDefault = ConfigOptions#config_service_options.priority_default,
    handle_call({'send_async_active', Name, RequestInfo, Request,
                 Timeout, PriorityDefault, PatternPid}, Client, State);

handle_call({'send_async_active', Name, RequestInfo, Request,
             Timeout, Priority, {Pattern, Pid}}, _,
            State) ->
    handle_send_async_active_pid(Name, Pattern, RequestInfo, Request,
                                 Timeout, Priority, Pid, State);

handle_call({'send_sync', Name, RequestInfo, Request,
             undefined, Priority}, Client,
            #state{timeout_sync = TimeoutSync} = State) ->
    handle_call({'send_sync', Name, RequestInfo, Request,
                 TimeoutSync, Priority}, Client, State);

handle_call({'send_sync', Name, RequestInfo, Request,
             Timeout, undefined}, Client,
            #state{options = ConfigOptions} = State) ->
    PriorityDefault = ConfigOptions#config_service_options.priority_default,
    handle_call({'send_sync', Name, RequestInfo, Request,
                 Timeout, PriorityDefault}, Client, State);

handle_call({'send_sync', Name, RequestInfo, Request,
             Timeout, Priority}, Client,
            #state{dest_deny = DestDeny,
                   dest_allow = DestAllow} = State) ->
    case destination_allowed(Name, DestDeny, DestAllow) of
        true ->
            handle_send_sync(Name, RequestInfo, Request,
                             Timeout, Priority, Client, State);
        false ->
            {reply, {error, timeout}, State}
    end;

handle_call({'send_sync', Name, RequestInfo, Request,
             undefined, Priority, PatternPid}, Client,
            #state{timeout_sync = TimeoutSync} = State) ->
    handle_call({'send_sync', Name, RequestInfo, Request,
                 TimeoutSync, Priority, PatternPid}, Client, State);

handle_call({'send_sync', Name, RequestInfo, Request,
             Timeout, undefined, PatternPid}, Client,
            #state{options = ConfigOptions} = State) ->
    PriorityDefault = ConfigOptions#config_service_options.priority_default,
    handle_call({'send_sync', Name, RequestInfo, Request,
                 Timeout, PriorityDefault, PatternPid}, Client, State);

handle_call({'send_sync', Name, RequestInfo, Request,
             Timeout, Priority, {Pattern, Pid}}, Client,
            State) ->
    handle_send_sync_pid(Name, Pattern, RequestInfo, Request,
                         Timeout, Priority, Pid, Client, State);

handle_call({'mcast_async', Name, RequestInfo, Request,
             undefined, Priority}, Client,
            #state{timeout_async = TimeoutAsync} = State) ->
    handle_call({'mcast_async', Name, RequestInfo, Request,
                 TimeoutAsync, Priority}, Client, State);

handle_call({'mcast_async', Name, RequestInfo, Request,
             Timeout, undefined}, Client,
            #state{options = ConfigOptions} = State) ->
    PriorityDefault = ConfigOptions#config_service_options.priority_default,
    handle_call({'mcast_async', Name, RequestInfo, Request,
                 Timeout, PriorityDefault}, Client, State);

handle_call({'mcast_async', Name, RequestInfo, Request,
             Timeout, Priority}, Client,
            #state{dest_deny = DestDeny,
                   dest_allow = DestAllow} = State) ->
    case destination_allowed(Name, DestDeny, DestAllow) of
        true ->
            handle_mcast_async(Name, RequestInfo, Request,
                               Timeout, Priority, Client, State);
        false ->
            {reply, {error, timeout}, State}
    end;

handle_call({'recv_async', TransId, Consume}, Client,
            #state{timeout_sync = TimeoutSync} = State) ->
    handle_call({'recv_async', TimeoutSync, TransId, Consume}, Client, State);

handle_call({'recv_async', Timeout, TransId, Consume}, Client,
            #state{dispatcher = Dispatcher,
                   async_responses = AsyncResponses} = State) ->
    if
        TransId == <<0:128>> ->
            case dict:to_list(AsyncResponses) of
                [] when Timeout >= ?RECV_ASYNC_INTERVAL ->
                    erlang:send_after(?RECV_ASYNC_INTERVAL, Dispatcher,
                                      {'cloudi_service_recv_async_retry',
                                       Timeout - ?RECV_ASYNC_INTERVAL,
                                       TransId, Consume, Client}),
                    {noreply, State};
                [] ->
                    {reply, {error, timeout}, State};
                L when Consume =:= true ->
                    TransIdPick = ?RECV_ASYNC_STRATEGY(L),
                    {ResponseInfo, Response} = dict:fetch(TransIdPick,
                                                          AsyncResponses),
                    {reply, {ok, ResponseInfo, Response, TransIdPick},
                     State#state{async_responses = dict:erase(TransIdPick,
                                                              AsyncResponses)}};
                L when Consume =:= false ->
                    TransIdPick = ?RECV_ASYNC_STRATEGY(L),
                    {ResponseInfo, Response} = dict:fetch(TransIdPick,
                                                          AsyncResponses),
                    {reply, {ok, ResponseInfo, Response, TransIdPick},
                     State}
            end;
        true ->
            case dict:find(TransId, AsyncResponses) of
                error when Timeout >= ?RECV_ASYNC_INTERVAL ->
                    erlang:send_after(?RECV_ASYNC_INTERVAL, Dispatcher,
                                      {'cloudi_service_recv_async_retry',
                                       Timeout - ?RECV_ASYNC_INTERVAL,
                                       TransId, Consume, Client}),
                    {noreply, State};
                error ->
                    {reply, {error, timeout}, State};
                {ok, {ResponseInfo, Response}} when Consume =:= true ->
                    {reply, {ok, ResponseInfo, Response, TransId},
                     State#state{async_responses = dict:erase(TransId,
                                                              AsyncResponses)}};
                {ok, {ResponseInfo, Response}} when Consume =:= false ->
                    {reply, {ok, ResponseInfo, Response, TransId},
                     State}
            end
    end;

handle_call(prefix, _, #state{prefix = Prefix} = State) ->
    {reply, Prefix, State};

handle_call(timeout_async, _, #state{timeout_async = TimeoutAsync} = State) ->
    {reply, TimeoutAsync, State};

handle_call(timeout_sync, _, #state{timeout_sync = TimeoutSync} = State) ->
    {reply, TimeoutSync, State};

handle_call(Request, _, State) ->
    ?LOG_WARN("Unknown call \"~p\"", [Request]),
    {stop, cloudi_string:format("Unknown call \"~p\"", [Request]),
     error, State}.

handle_cast(Request, State) ->
    ?LOG_WARN("Unknown cast \"~p\"", [Request]),
    {noreply, State}.

handle_info({'cloudi_service_init', Args, Timeout},
            #state{queue_requests = true,
                   module = Module,
                   prefix = Prefix,
                   duo_mode_pid = DuoModePid} = State) ->
    {ok, DispatcherProxy} = cloudi_services_internal_init:start_link(Timeout,
                                                                     State),
    Result = Module:cloudi_service_init(Args, Prefix, DispatcherProxy),
    NewState = cloudi_services_internal_init:stop_link(DispatcherProxy),
    case Result of
        {ok, ServiceState} ->
            erlang:process_flag(trap_exit, true),
            if
                is_pid(DuoModePid) ->
                    DuoModePid ! {'cloudi_service_init', Result},
                    {noreply, NewState};
                true ->
                    {noreply, process_queues(ServiceState, NewState)}
            end;
        {stop, Reason} ->
            {stop, Reason, NewState}
    end;

handle_info({'cloudi_service_request_success', RequestResponse,
             NewServiceState},
            #state{dispatcher = Dispatcher} = State) ->
    case RequestResponse of
        undefined ->
            ok;
        {'cloudi_service_return_async', _, _, _, _, _, _, Source} = T ->
            Source ! T;
        {'cloudi_service_return_sync', _, _, _, _, _, _, Source} = T ->
            Source ! T;
        {'cloudi_service_forward_async_retry', _, _, _, _, _, _, _} = T ->
            Dispatcher ! T;
        {'cloudi_service_forward_sync_retry', _, _, _, _, _, _, _} = T ->
            Dispatcher ! T
    end,
    {noreply, process_queues(NewServiceState, State)};

handle_info({'cloudi_service_info_success',
             NewServiceState}, State) ->
    {noreply, process_queues(NewServiceState, State)};

handle_info({'cloudi_service_request_failure',
             Type, Error, Stack, NewServiceState}, State) ->
    Reason = if
        Type =:= stop ->
            true = Stack =:= undefined,
            ?LOG_ERROR("request stop ~p", [Error]),
            Error;
        true ->
            ?LOG_ERROR("request ~p ~p~n~p", [Type, Error, Stack]),
            {Type, {Error, Stack}}
    end,
    {stop, Reason, State#state{service_state = NewServiceState}};

handle_info({'cloudi_service_info_failure',
             Reason, NewServiceState}, State) ->
    ?LOG_ERROR("info stop ~p", [Reason]),
    {stop, Reason, State#state{service_state = NewServiceState}};

handle_info({'EXIT', _, shutdown},
            #state{duo_mode_pid = DuoModePid} = State) ->
    % CloudI Service shutdown
    if
        is_pid(DuoModePid) ->
            erlang:exit(DuoModePid, shutdown);
        true ->
            ok
    end,
    {stop, shutdown, State};

handle_info({'EXIT', _, restart},
            #state{duo_mode_pid = DuoModePid} = State) ->
    % CloudI Service API requested a restart
    if
        is_pid(DuoModePid) ->
            erlang:exit(DuoModePid, restart);
        true ->
            ok
    end,
    {stop, restart, State};

handle_info({'EXIT', DuoModePid, Reason},
            #state{duo_mode_pid = DuoModePid} = State) ->
    ?LOG_ERROR("~p duo_mode exited: ~p", [DuoModePid, Reason]),
    {stop, Reason, State};

handle_info({'EXIT', RequestPid,
             {'cloudi_service_request_success', _RequestResponse,
              _NewServiceState} = Result},
            #state{request_pid = RequestPid} = State) ->
    handle_info(Result, State#state{request_pid = undefined});

handle_info({'EXIT', RequestPid,
             {'cloudi_service_request_failure',
              _Type, _Error, _Stack, _NewServiceState} = Result},
            #state{request_pid = RequestPid} = State) ->
    handle_info(Result, State#state{request_pid = undefined});

handle_info({'EXIT', RequestPid, Reason},
            #state{request_pid = RequestPid} = State) ->
    ?LOG_ERROR("~p request exited: ~p", [RequestPid, Reason]),
    {stop, Reason, State};

handle_info({'EXIT', InfoPid,
             {'cloudi_service_info_success',
              _NewServiceState} = Result},
            #state{info_pid = InfoPid} = State) ->
    handle_info(Result, State#state{info_pid = undefined});

handle_info({'EXIT', InfoPid,
             {'cloudi_service_info_failure',
              _Reason, _NewServiceState} = Result},
            #state{info_pid = InfoPid} = State) ->
    handle_info(Result, State#state{info_pid = undefined});

handle_info({'EXIT', InfoPid, Reason},
            #state{info_pid = InfoPid} = State) ->
    ?LOG_ERROR("~p info exited: ~p", [InfoPid, Reason]),
    {stop, Reason, State};

handle_info({'EXIT', Dispatcher, Reason},
            #state{dispatcher = Dispatcher} = State) ->
    ?LOG_ERROR("~p service exited: ~p", [Dispatcher, Reason]),
    {stop, Reason, State};

handle_info({cloudi_x_cpg_data, Groups},
            #state{dest_refresh = DestRefresh,
                   options = ConfigOptions} = State) ->
    destination_refresh_start(DestRefresh, ConfigOptions),
    {noreply, State#state{cpg_data = Groups}};

handle_info({'cloudi_service_get_pid_retry', Name, Timeout, Client}, State) ->
    handle_get_pid(Name, Timeout, Client, State);

handle_info({'cloudi_service_send_async_retry',
             Name, RequestInfo, Request, Timeout, Priority, Client}, State) ->
    handle_send_async(Name, RequestInfo, Request,
                      Timeout, Priority, Client, State);

handle_info({'cloudi_service_send_async_active_retry',
             Name, RequestInfo, Request, Timeout, Priority, Client}, State) ->
    handle_send_async_active(Name, RequestInfo, Request,
                             Timeout, Priority, Client, State);

handle_info({'cloudi_service_send_sync_retry',
             Name, RequestInfo, Request, Timeout, Priority, Client}, State) ->
    handle_send_sync(Name, RequestInfo, Request,
                     Timeout, Priority, Client, State);

handle_info({'cloudi_service_mcast_async_retry',
             Name, RequestInfo, Request, Timeout, Priority, Client}, State) ->
    handle_mcast_async(Name, RequestInfo, Request,
                       Timeout, Priority, Client, State);

handle_info({'cloudi_service_forward_async_retry',
             Name, RequestInfo, Request, Timeout, Priority, TransId, Source},
            #state{dispatcher = Dispatcher,
                   dest_refresh = DestRefresh,
                   cpg_data = Groups,
                   dest_deny = DestDeny,
                   dest_allow = DestAllow} = State) ->
    case destination_allowed(Name, DestDeny, DestAllow) of
        true ->
            case destination_get(DestRefresh, Name, Source, Groups) of
                {error, _} when Timeout >= ?FORWARD_ASYNC_INTERVAL ->
                    erlang:send_after(?FORWARD_ASYNC_INTERVAL, Dispatcher,
                                      {'cloudi_service_forward_async_retry',
                                       Name, RequestInfo, Request,
                                       Timeout - ?FORWARD_ASYNC_INTERVAL,
                                       Priority, TransId, Source}),
                    ok;
                {error, _} ->
                    ok;
                {ok, NextPattern, NextPid} when Timeout >= ?FORWARD_DELTA ->
                    NextPid ! {'cloudi_service_send_async', Name, NextPattern,
                               RequestInfo, Request,
                               Timeout - ?FORWARD_DELTA,
                               Priority, TransId, Source};
                _ ->
                    ok
            end;
        false ->
            ok
    end,
    {noreply, State};

handle_info({'cloudi_service_forward_sync_retry', Name, RequestInfo, Request,
             Timeout, Priority, TransId, Source},
            #state{dispatcher = Dispatcher,
                   dest_refresh = DestRefresh,
                   cpg_data = Groups,
                   dest_deny = DestDeny,
                   dest_allow = DestAllow} = State) ->
    case destination_allowed(Name, DestDeny, DestAllow) of
        true ->
            case destination_get(DestRefresh, Name, Source, Groups) of
                {error, _} when Timeout >= ?FORWARD_SYNC_INTERVAL ->
                    erlang:send_after(?FORWARD_SYNC_INTERVAL, Dispatcher,
                                      {'cloudi_service_forward_sync_retry',
                                       Name, RequestInfo, Request,
                                       Timeout - ?FORWARD_SYNC_INTERVAL,
                                       Priority, TransId, Source}),
                    ok;
                {error, _} ->
                    ok;
                {ok, NextPattern, NextPid} when Timeout >= ?FORWARD_DELTA ->
                    NextPid ! {'cloudi_service_send_sync', Name, NextPattern,
                               RequestInfo, Request,
                               Timeout - ?FORWARD_DELTA,
                               Priority, TransId, Source};
                _ ->
                    ok
            end;
        false ->
            ok
    end,
    {noreply, State};

handle_info({'cloudi_service_recv_async_retry',
             Timeout, TransId, Consume, Client},
            #state{dispatcher = Dispatcher,
                   async_responses = AsyncResponses} = State) ->
    if
        TransId == <<0:128>> ->
            case dict:to_list(AsyncResponses) of
                [] when Timeout >= ?RECV_ASYNC_INTERVAL ->
                    erlang:send_after(?RECV_ASYNC_INTERVAL, Dispatcher,
                                      {'cloudi_service_recv_async_retry',
                                       Timeout - ?RECV_ASYNC_INTERVAL,
                                       TransId, Consume, Client}),
                    {noreply, State};
                [] ->
                    gen_server:reply(Client, {error, timeout}),
                    {noreply, State};
                L when Consume =:= true ->
                    TransIdPick = ?RECV_ASYNC_STRATEGY(L),
                    {ResponseInfo, Response} = dict:fetch(TransIdPick,
                                                          AsyncResponses),
                    gen_server:reply(Client,
                                     {ok, ResponseInfo, Response, TransIdPick}),
                    {noreply, State#state{
                        async_responses = dict:erase(TransIdPick,
                                                     AsyncResponses)}};
                L when Consume =:= false ->
                    TransIdPick = ?RECV_ASYNC_STRATEGY(L),
                    {ResponseInfo, Response} = dict:fetch(TransIdPick,
                                                          AsyncResponses),
                    gen_server:reply(Client,
                                     {ok, ResponseInfo, Response, TransIdPick}),
                    {noreply, State}
            end;
        true ->
            case dict:find(TransId, AsyncResponses) of
                error when Timeout >= ?RECV_ASYNC_INTERVAL ->
                    erlang:send_after(?RECV_ASYNC_INTERVAL, Dispatcher,
                                      {'cloudi_service_recv_async_retry',
                                       Timeout - ?RECV_ASYNC_INTERVAL,
                                       TransId, Consume, Client}),
                    {noreply, State};
                error ->
                    gen_server:reply(Client, {error, timeout}),
                    {noreply, State};
                {ok, {ResponseInfo, Response}} when Consume =:= true ->
                    gen_server:reply(Client,
                                     {ok, ResponseInfo, Response, TransId}),
                    {noreply, State#state{
                        async_responses = dict:erase(TransId,
                                                     AsyncResponses)}};
                {ok, {ResponseInfo, Response}} when Consume =:= false ->
                    gen_server:reply(Client,
                                     {ok, ResponseInfo, Response, TransId}),
                    {noreply, State}
            end
    end;

handle_info({'cloudi_service_send_async',
             Name, Pattern, RequestInfo, Request,
             Timeout, Priority, TransId, Source},
            #state{dispatcher = Dispatcher,
                   queue_requests = false,
                   module = Module,
                   service_state = ServiceState,
                   request_pid = RequestPid,
                   options = ConfigOptions} = State) ->
    {noreply, State#state{
        queue_requests = true,
        request_pid = handle_module_request_loop_pid(RequestPid,
            {'cloudi_service_request_loop',
             'send_async', Name, Pattern,
             RequestInfo, Request,
             Timeout, Priority, TransId, Source,
             Module, Dispatcher, ConfigOptions,
             ServiceState}, ConfigOptions, Dispatcher)}};

handle_info({'cloudi_service_send_sync',
             Name, Pattern, RequestInfo, Request,
             Timeout, Priority, TransId, Source},
            #state{dispatcher = Dispatcher,
                   queue_requests = false,
                   module = Module,
                   service_state = ServiceState,
                   request_pid = RequestPid,
                   options = ConfigOptions} = State) ->
    {noreply, State#state{
        queue_requests = true,
        request_pid = handle_module_request_loop_pid(RequestPid,
            {'cloudi_service_request_loop',
             'send_sync', Name, Pattern,
             RequestInfo, Request,
             Timeout, Priority, TransId, Source,
             Module, Dispatcher, ConfigOptions,
             ServiceState}, ConfigOptions, Dispatcher)}};

handle_info({Type, _, _, _, _, 0, _, _, _},
            #state{queue_requests = true} = State)
    when Type =:= 'cloudi_service_send_async';
         Type =:= 'cloudi_service_send_sync' ->
    {noreply, State};

handle_info({Type, _, _, _, _, Timeout, Priority, TransId, _} = T,
            #state{queue_requests = true,
                   queued = Queue,
                   options = ConfigOptions} = State)
    when Type =:= 'cloudi_service_send_async';
         Type =:= 'cloudi_service_send_sync' ->
    QueueLimit = ConfigOptions#config_service_options.queue_limit,
    QueueLimitOk = if
        QueueLimit /= undefined ->
            cloudi_x_pqueue4:len(Queue) < QueueLimit;
        true ->
            true
    end,
    if
        QueueLimitOk ->
            {noreply, recv_timeout_start(Timeout, Priority, TransId, T, State)};
        true ->
            % message is discarded since too many messages have been queued
            {noreply, State}
    end;

handle_info({'cloudi_service_recv_timeout', Priority, TransId},
            #state{recv_timeouts = RecvTimeouts,
                   queued = Queue,
                   queue_requests = QueueRequests} = State) ->
    NewQueue = if
        QueueRequests =:= true ->
            cloudi_x_pqueue4:filter(fun({_, _, _, _, _, _, _, Id, _}) ->
                Id /= TransId
            end, Priority, Queue);
        true ->
            Queue
    end,
    {noreply, State#state{recv_timeouts = dict:erase(TransId, RecvTimeouts),
                          queued = NewQueue}};

handle_info({'cloudi_service_return_async',
             Name, Pattern, ResponseInfo, Response,
             OldTimeout, TransId, Source},
            #state{dispatcher = Dispatcher,
                   send_timeouts = SendTimeouts,
                   duo_mode_pid = DuoModePid,
                   options = ConfigOptions} = State) ->
    true = if
        is_pid(DuoModePid) ->
            Source =:= DuoModePid;
        true ->
            Source =:= Dispatcher
    end,
    case dict:find(TransId, SendTimeouts) of
        error ->
            % send_async timeout already occurred
            {noreply, State};
        {ok, {active, Tref}} when Response == <<>> ->
            if
                ConfigOptions
                #config_service_options.response_timeout_adjustment ->
                    erlang:cancel_timer(Tref);
                true ->
                    ok
            end,
            if
                is_pid(DuoModePid) ->
                    DuoModePid ! {'timeout_async_active', TransId};
                true ->
                    Dispatcher ! {'timeout_async_active', TransId}
            end,
            {noreply, send_timeout_end(TransId, State)};
        {ok, {active, Tref}} ->
            Timeout = if
                ConfigOptions
                #config_service_options.response_timeout_adjustment ->
                    case erlang:cancel_timer(Tref) of
                        false ->
                            0;
                        V ->
                            V
                    end;
                true ->
                    OldTimeout
            end,
            if
                is_pid(DuoModePid) ->
                    DuoModePid ! {'return_async_active', Name, Pattern,
                                  ResponseInfo, Response, Timeout, TransId};
                true ->
                    Dispatcher ! {'return_async_active', Name, Pattern,
                                  ResponseInfo, Response, Timeout, TransId}
            end,
            {noreply, send_timeout_end(TransId, State)};
        {ok, {passive, Tref}} when Response == <<>> ->
            if
                ConfigOptions
                #config_service_options.response_timeout_adjustment ->
                    erlang:cancel_timer(Tref);
                true ->
                    ok
            end,
            {noreply, send_timeout_end(TransId, State)};
        {ok, {passive, Tref}} ->
            Timeout = if
                ConfigOptions
                #config_service_options.response_timeout_adjustment ->
                    case erlang:cancel_timer(Tref) of
                        false ->
                            0;
                        V ->
                            V
                    end;
                true ->
                    OldTimeout
            end,
            {noreply, send_timeout_end(TransId,
                async_response_timeout_start(ResponseInfo, Response, Timeout,
                                             TransId, State))}
    end;

handle_info({'cloudi_service_return_sync',
             _, _, ResponseInfo, Response, _, TransId, Source},
            #state{dispatcher = Dispatcher,
                   send_timeouts = SendTimeouts,
                   duo_mode_pid = DuoModePid,
                   options = ConfigOptions} = State) ->
    true = if
        is_pid(DuoModePid) ->
            Source =:= DuoModePid;
        true ->
            Source =:= Dispatcher
    end,
    case dict:find(TransId, SendTimeouts) of
        error ->
            % send_async timeout already occurred
            {noreply, State};
        {ok, {Client, Tref}} ->
            if
                ConfigOptions
                #config_service_options.response_timeout_adjustment ->
                    erlang:cancel_timer(Tref);
                true ->
                    ok
            end,
            if
                Response == <<>> ->
                    gen_server:reply(Client, {error, timeout});
                ResponseInfo == <<>> ->
                    gen_server:reply(Client, {ok, Response});
                true ->
                    gen_server:reply(Client, {ok, ResponseInfo, Response})
            end,
            {noreply, send_timeout_end(TransId, State)}
    end;

handle_info({'cloudi_service_send_async_timeout', TransId},
            #state{dispatcher = Dispatcher,
                   send_timeouts = SendTimeouts,
                   duo_mode_pid = DuoModePid,
                   options = ConfigOptions} = State) ->
    case dict:find(TransId, SendTimeouts) of
        error ->
            if
                ConfigOptions
                #config_service_options.response_timeout_adjustment ->
                    % should never happen, timer should have been cancelled
                    % if the send_async already returned
                    ?LOG_WARN("send timeout not found (trans_id=~s)",
                              [cloudi_x_uuid:uuid_to_string(TransId)]);
                true ->
                    ok % cancel_timer avoided due to latency
            end,
            {noreply, State};
        {ok, {active, _}} ->
            if
                is_pid(DuoModePid) ->
                    DuoModePid ! {'timeout_async_active', TransId};
                true ->
                    Dispatcher ! {'timeout_async_active', TransId}
            end,
            {noreply, send_timeout_end(TransId, State)};
        {ok, _} ->
            {noreply, send_timeout_end(TransId, State)}
    end;

handle_info({'cloudi_service_send_sync_timeout', TransId},
            #state{send_timeouts = SendTimeouts,
                   options = ConfigOptions} = State) ->
    case dict:find(TransId, SendTimeouts) of
        error ->
            if
                ConfigOptions
                #config_service_options.response_timeout_adjustment ->
                    % should never happen, timer should have been cancelled
                    % if the send_sync already returned
                    ?LOG_WARN("send timeout not found (trans_id=~s)",
                              [cloudi_x_uuid:uuid_to_string(TransId)]);
                true ->
                    ok % cancel_timer avoided due to latency
            end,
            {noreply, State};
        {ok, {Client, _}} ->
            gen_server:reply(Client, {error, timeout}),
            {noreply, send_timeout_end(TransId, State)}
    end;

handle_info({'cloudi_service_recv_async_timeout', TransId},
            #state{async_responses = AsyncResponses} = State) ->
    {noreply,
     State#state{async_responses = dict:erase(TransId, AsyncResponses)}};

handle_info(Request,
            #state{queue_requests = true,
                   queued_info = QueueInfo} = State) ->
    {noreply, State#state{queued_info = queue:in(Request, QueueInfo)}};

handle_info(Request,
            #state{dispatcher = Dispatcher,
                   module = Module,
                   service_state = ServiceState,
                   info_pid = InfoPid,
                   duo_mode_pid = undefined,
                   options = ConfigOptions} = State) ->
    {noreply, State#state{
        queue_requests = true,
        info_pid = handle_module_info_loop_pid(InfoPid,
            {'cloudi_service_info_loop',
             Request, Module, Dispatcher, ServiceState},
            ConfigOptions, Dispatcher)}}.

terminate(Reason,
          #state{module = Module,
                 service_state = ServiceState,
                 duo_mode_pid = undefined}) ->
    Module:cloudi_service_terminate(Reason, ServiceState),
    ok;

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

handle_get_pid(Name, Timeout, Client,
               #state{dispatcher = Dispatcher,
                      dest_refresh = DestRefresh,
                      cpg_data = Groups,
                      duo_mode_pid = DuoModePid} = State) ->
    Receiver = if
        is_pid(DuoModePid) ->
            DuoModePid;
        true ->
            Dispatcher
    end,
    case destination_get(DestRefresh, Name, Receiver, Groups) of
        {error, _} when Timeout >= ?SEND_SYNC_INTERVAL ->
            erlang:send_after(?SEND_SYNC_INTERVAL, Dispatcher,
                              {'cloudi_service_get_pid_retry',
                               Name, Timeout - ?SEND_SYNC_INTERVAL, Client}),
            {noreply, State};
        {error, _} ->
            gen_server:reply(Client, {error, timeout}),
            {noreply, State};
        {ok, Pattern, Pid} ->
            gen_server:reply(Client, {ok, {Pattern, Pid}}),
            {noreply, State}
    end.

handle_send_async(Name, RequestInfo, Request,
                  Timeout, Priority, Client,
                  #state{dispatcher = Dispatcher,
                         uuid_generator = UUID,
                         dest_refresh = DestRefresh,
                         cpg_data = Groups,
                         duo_mode_pid = DuoModePid} = State) ->
    Receiver = if
        is_pid(DuoModePid) ->
            DuoModePid;
        true ->
            Dispatcher
    end,
    case destination_get(DestRefresh, Name, Receiver, Groups) of
        {error, _} when Timeout >= ?SEND_ASYNC_INTERVAL ->
            erlang:send_after(?SEND_ASYNC_INTERVAL, Dispatcher,
                              {'cloudi_service_send_async_retry',
                               Name, RequestInfo, Request,
                               Timeout - ?SEND_ASYNC_INTERVAL,
                               Priority, Client}),
            {noreply, State};
        {error, _} ->
            gen_server:reply(Client, {error, timeout}),
            {noreply, State};
        {ok, Pattern, Pid} ->
            TransId = cloudi_x_uuid:get_v1(UUID),
            Pid ! {'cloudi_service_send_async',
                   Name, Pattern, RequestInfo, Request,
                   Timeout, Priority, TransId, Receiver},
            gen_server:reply(Client, {ok, TransId}),
            {noreply, send_async_timeout_start(Timeout, TransId, State)}
    end.

handle_send_async_pid(Name, Pattern, RequestInfo, Request,
                      Timeout, Priority, Pid,
                      #state{dispatcher = Dispatcher,
                             uuid_generator = UUID,
                             duo_mode_pid = DuoModePid} = State) ->
    Receiver = if
        is_pid(DuoModePid) ->
            DuoModePid;
        true ->
            Dispatcher
    end,
    TransId = cloudi_x_uuid:get_v1(UUID),
    Pid ! {'cloudi_service_send_async',
           Name, Pattern, RequestInfo, Request,
           Timeout, Priority, TransId, Receiver},
    {reply, {ok, TransId}, send_async_timeout_start(Timeout, TransId, State)}.

handle_send_async_active(Name, RequestInfo, Request,
                         Timeout, Priority, Client,
                         #state{dispatcher = Dispatcher,
                                uuid_generator = UUID,
                                dest_refresh = DestRefresh,
                                cpg_data = Groups,
                                duo_mode_pid = DuoModePid} = State) ->
    Receiver = if
        is_pid(DuoModePid) ->
            DuoModePid;
        true ->
            Dispatcher
    end,
    case destination_get(DestRefresh, Name, Receiver, Groups) of
        {error, _} when Timeout >= ?SEND_ASYNC_INTERVAL ->
            erlang:send_after(?SEND_ASYNC_INTERVAL, Dispatcher,
                              {'cloudi_service_send_async_active_retry',
                               Name, RequestInfo, Request,
                               Timeout - ?SEND_ASYNC_INTERVAL,
                               Priority, Client}),
            {noreply, State};
        {error, _} ->
            gen_server:reply(Client, {error, timeout}),
            {noreply, State};
        {ok, Pattern, Pid} ->
            TransId = cloudi_x_uuid:get_v1(UUID),
            Pid ! {'cloudi_service_send_async',
                   Name, Pattern, RequestInfo, Request,
                   Timeout, Priority, TransId, Receiver},
            gen_server:reply(Client, {ok, TransId}),
            {noreply, send_async_active_timeout_start(Timeout, TransId, State)}
    end.

handle_send_async_active_pid(Name, Pattern, RequestInfo, Request,
                             Timeout, Priority, Pid,
                             #state{dispatcher = Dispatcher,
                                    uuid_generator = UUID,
                                    duo_mode_pid = DuoModePid} = State) ->
    Receiver = if
        is_pid(DuoModePid) ->
            DuoModePid;
        true ->
            Dispatcher
    end,
    TransId = cloudi_x_uuid:get_v1(UUID),
    Pid ! {'cloudi_service_send_async',
           Name, Pattern, RequestInfo, Request,
           Timeout, Priority, TransId, Receiver},
    {reply, {ok, TransId},
     send_async_active_timeout_start(Timeout, TransId, State)}.

handle_send_sync(Name, RequestInfo, Request,
                 Timeout, Priority, Client,
                 #state{dispatcher = Dispatcher,
                        uuid_generator = UUID,
                        dest_refresh = DestRefresh,
                        cpg_data = Groups,
                        duo_mode_pid = DuoModePid} = State) ->
    Receiver = if
        is_pid(DuoModePid) ->
            DuoModePid;
        true ->
            Dispatcher
    end,
    case destination_get(DestRefresh, Name, Receiver, Groups) of
        {error, _} when Timeout >= ?SEND_SYNC_INTERVAL ->
            erlang:send_after(?SEND_SYNC_INTERVAL, Dispatcher,
                              {'cloudi_service_send_sync_retry',
                               Name, RequestInfo, Request,
                               Timeout - ?SEND_SYNC_INTERVAL,
                               Priority, Client}),
            {noreply, State};
        {error, _} ->
            gen_server:reply(Client, {error, timeout}),
            {noreply, State};
        {ok, Pattern, Pid} ->
            TransId = cloudi_x_uuid:get_v1(UUID),
            Pid ! {'cloudi_service_send_sync',
                   Name, Pattern, RequestInfo, Request,
                   Timeout, Priority, TransId, Receiver},
            {noreply, send_sync_timeout_start(Timeout, TransId, Client, State)}
    end.

handle_send_sync_pid(Name, Pattern, RequestInfo, Request,
                     Timeout, Priority, Pid, Client,
                     #state{dispatcher = Dispatcher,
                            uuid_generator = UUID,
                            duo_mode_pid = DuoModePid} = State) ->
    Receiver = if
        is_pid(DuoModePid) ->
            DuoModePid;
        true ->
            Dispatcher
    end,
    TransId = cloudi_x_uuid:get_v1(UUID),
    Pid ! {'cloudi_service_send_sync',
           Name, Pattern, RequestInfo, Request,
           Timeout, Priority, TransId, Receiver},
    {noreply, send_sync_timeout_start(Timeout, TransId, Client, State)}.

handle_mcast_async(Name, RequestInfo, Request,
                   Timeout, Priority, Client,
                   #state{dispatcher = Dispatcher,
                          uuid_generator = UUID,
                          dest_refresh = DestRefresh,
                          cpg_data = Groups,
                          duo_mode_pid = DuoModePid} = State) ->
    Receiver = if
        is_pid(DuoModePid) ->
            DuoModePid;
        true ->
            Dispatcher
    end,
    case destination_all(DestRefresh, Name, Receiver, Groups) of
        {error, _} when Timeout >= ?MCAST_ASYNC_INTERVAL ->
            erlang:send_after(?MCAST_ASYNC_INTERVAL, Dispatcher,
                              {'cloudi_service_mcast_async_retry',
                               Name, RequestInfo, Request,
                               Timeout - ?MCAST_ASYNC_INTERVAL,
                               Priority, Client}),
            {noreply, State};
        {error, _} ->
            gen_server:reply(Client, {error, timeout}),
            {noreply, State};
        {ok, Pattern, PidList} ->
            TransIdList = lists:map(fun(Pid) ->
                TransId = cloudi_x_uuid:get_v1(UUID),
                Pid ! {'cloudi_service_send_async',
                       Name, Pattern, RequestInfo, Request,
                       Timeout, Priority, TransId, Receiver},
                TransId
            end, PidList),
            gen_server:reply(Client, {ok, TransIdList}),
            NewState = lists:foldl(fun(Id, S) ->
                send_async_timeout_start(Timeout, Id, S)
            end, State, TransIdList),
            {noreply, NewState}
    end.

handle_module_request('send_async', Name, Pattern, RequestInfo, Request,
                      Timeout, Priority, TransId, Source,
                      Module, Dispatcher, ConfigOptions, ServiceState) ->
    RequestTimeoutF = if
        ConfigOptions#config_service_options.request_timeout_adjustment ->
            RequestTimeStart = os:timestamp(),
            fun(T) ->
                erlang:max(0,
                           T - erlang:trunc(timer:now_diff(os:timestamp(),
                                                           RequestTimeStart) /
                                            1000.0))
            end;
        true ->
            fun(T) -> T end
    end,
    try Module:cloudi_service_handle_request('send_async',
                                             Name, Pattern,
                                             RequestInfo, Request,
                                             Timeout, Priority,
                                             TransId, Source,
                                             ServiceState,
                                             Dispatcher) of
        {reply, <<>>, NewServiceState} ->
            {'cloudi_service_request_success', undefined, NewServiceState};
        {reply, Response, NewServiceState} ->
            {'cloudi_service_request_success',
             {'cloudi_service_return_async', Name, Pattern,
              <<>>, Response,
              RequestTimeoutF(Timeout), TransId, Source},
             NewServiceState};
        {reply, ResponseInfo, Response, NewServiceState} ->
            {'cloudi_service_request_success',
             {'cloudi_service_return_async', Name, Pattern,
              ResponseInfo, Response,
              RequestTimeoutF(Timeout), TransId, Source},
             NewServiceState};
        {forward, _, _, _, NextTimeout, NextPriority, NewServiceState}
            when NextPriority < ?PRIORITY_HIGH;
                 NextPriority > ?PRIORITY_LOW;
                 NextTimeout < 0 ->
            {'cloudi_service_request_failure',
             exit, badarg, erlang:get_stacktrace(), NewServiceState};
        {forward, NextName, NextRequestInfo, NextRequest,
                  Timeout, NextPriority, NewServiceState} ->
            {'cloudi_service_request_success',
             {'cloudi_service_forward_async_retry', NextName,
              NextRequestInfo, NextRequest,
              RequestTimeoutF(Timeout), NextPriority, TransId, Source},
             NewServiceState};
        {forward, NextName, NextRequestInfo, NextRequest,
                  NextTimeout, NextPriority, NewServiceState} ->
            {'cloudi_service_request_success',
             {'cloudi_service_forward_async_retry', NextName,
              NextRequestInfo, NextRequest,
              NextTimeout, NextPriority, TransId, Source},
             NewServiceState};
        {forward, NextName, NextRequestInfo, NextRequest,
                  NewServiceState} ->
            {'cloudi_service_request_success',
             {'cloudi_service_forward_async_retry', NextName,
              NextRequestInfo, NextRequest,
              RequestTimeoutF(Timeout), Priority, TransId, Source},
             NewServiceState};
        {noreply, NewServiceState} ->
            {'cloudi_service_request_success', undefined, NewServiceState};
        {stop, Reason, NewServiceState} ->
            {'cloudi_service_request_failure',
             stop, Reason, undefined, NewServiceState}
    catch
        throw:{cloudi_service_return,
               {ReturnType, Name, Pattern,
                ResponseInfo, Response,
                Timeout, TransId, Source}}
            when ReturnType =:= 'cloudi_service_return_async' ->
            {'cloudi_service_request_success',
             {ReturnType, Name, Pattern,
              ResponseInfo, Response,
              RequestTimeoutF(Timeout), TransId, Source},
             ServiceState};
        throw:{cloudi_service_return,
               {ReturnType, Name, Pattern,
                ResponseInfo, Response,
                NextTimeout, TransId, Source}}
            when ReturnType =:= 'cloudi_service_return_async' ->
            {'cloudi_service_request_success',
             {ReturnType, Name, Pattern,
              ResponseInfo, Response,
              NextTimeout, TransId, Source},
             ServiceState};
        throw:{cloudi_service_forward,
               {ForwardType, NextName,
                NextRequestInfo, NextRequest,
                Timeout, NextPriority, TransId, Source}}
            when ForwardType =:= 'cloudi_service_forward_async_retry' ->
            {'cloudi_service_request_success',
             {ForwardType, NextName,
              NextRequestInfo, NextRequest,
              RequestTimeoutF(Timeout), NextPriority, TransId, Source},
             ServiceState};
        throw:{cloudi_service_forward,
               {ForwardType, NextName,
                NextRequestInfo, NextRequest,
                NextTimeout, NextPriority, TransId, Source}}
            when ForwardType =:= 'cloudi_service_forward_async_retry' ->
            {'cloudi_service_request_success',
             {ForwardType, NextName,
              NextRequestInfo, NextRequest,
              NextTimeout, NextPriority, TransId, Source},
             ServiceState};
        Type:Error ->
            {'cloudi_service_request_failure',
             Type, Error, erlang:get_stacktrace(), ServiceState}
    end;

handle_module_request('send_sync', Name, Pattern, RequestInfo, Request,
                      Timeout, Priority, TransId, Source,
                      Module, Dispatcher, ConfigOptions, ServiceState) ->
    RequestTimeoutF = if
        ConfigOptions#config_service_options.request_timeout_adjustment ->
            RequestTimeStart = os:timestamp(),
            fun(T) ->
                erlang:max(0,
                           T - erlang:trunc(timer:now_diff(os:timestamp(),
                                                           RequestTimeStart) /
                                            1000.0))
            end;
        true ->
            fun(T) -> T end
    end,
    try Module:cloudi_service_handle_request('send_sync',
                                             Name, Pattern,
                                             RequestInfo, Request,
                                             Timeout, Priority,
                                             TransId, Source,
                                             ServiceState,
                                             Dispatcher) of
        {reply, <<>>, NewServiceState} ->
            {'cloudi_service_request_success', undefined, NewServiceState};
        {reply, Response, NewServiceState} ->
            {'cloudi_service_request_success',
             {'cloudi_service_return_sync', Name, Pattern,
              <<>>, Response,
              RequestTimeoutF(Timeout), TransId, Source},
             NewServiceState};
        {reply, ResponseInfo, Response, NewServiceState} ->
            {'cloudi_service_request_success',
             {'cloudi_service_return_sync', Name, Pattern,
              ResponseInfo, Response,
              RequestTimeoutF(Timeout), TransId, Source},
             NewServiceState};
        {forward, _, _, _, NextTimeout, NextPriority, NewServiceState}
            when NextPriority < ?PRIORITY_HIGH;
                 NextPriority > ?PRIORITY_LOW;
                 NextTimeout < 0 ->
            {'cloudi_service_request_failure',
             exit, badarg, erlang:get_stacktrace(), NewServiceState};
        {forward, NextName, NextRequestInfo, NextRequest,
                  Timeout, NextPriority, NewServiceState} ->
            {'cloudi_service_request_success',
             {'cloudi_service_forward_sync_retry', NextName,
              NextRequestInfo, NextRequest,
              RequestTimeoutF(Timeout), NextPriority, TransId, Source},
             NewServiceState};
        {forward, NextName, NextRequestInfo, NextRequest,
                  NextTimeout, NextPriority, NewServiceState} ->
            {'cloudi_service_request_success',
             {'cloudi_service_forward_sync_retry', NextName,
              NextRequestInfo, NextRequest,
              NextTimeout, NextPriority, TransId, Source},
             NewServiceState};
        {forward, NextName, NextRequestInfo, NextRequest,
                  NewServiceState} ->
            {'cloudi_service_request_success',
             {'cloudi_service_forward_sync_retry', NextName,
              NextRequestInfo, NextRequest,
              RequestTimeoutF(Timeout), Priority, TransId, Source},
             NewServiceState};
        {noreply, NewServiceState} ->
            {'cloudi_service_request_success', undefined, NewServiceState};
        {stop, Reason, NewServiceState} ->
            {'cloudi_service_request_failure',
             stop, Reason, undefined, NewServiceState}
    catch
        throw:{cloudi_service_return,
               {ReturnType, Name, Pattern,
                ResponseInfo, Response,
                Timeout, TransId, Source}}
            when ReturnType =:= 'cloudi_service_return_sync' ->
            {'cloudi_service_request_success',
             {ReturnType, Name, Pattern,
              ResponseInfo, Response,
              RequestTimeoutF(Timeout), TransId, Source},
             ServiceState};
        throw:{cloudi_service_return,
               {ReturnType, Name, Pattern,
                ResponseInfo, Response,
                NextTimeout, TransId, Source}}
            when ReturnType =:= 'cloudi_service_return_sync' ->
            {'cloudi_service_request_success',
             {ReturnType, Name, Pattern,
              ResponseInfo, Response,
              NextTimeout, TransId, Source},
             ServiceState};
        throw:{cloudi_service_forward,
               {ForwardType, NextName,
                NextRequestInfo, NextRequest,
                Timeout, NextPriority, TransId, Source}}
            when ForwardType =:= 'cloudi_service_forward_sync_retry' ->
            {'cloudi_service_request_success',
             {ForwardType, NextName,
              NextRequestInfo, NextRequest,
              RequestTimeoutF(Timeout), NextPriority, TransId, Source},
             ServiceState};
        throw:{cloudi_service_forward,
               {ForwardType, NextName,
                NextRequestInfo, NextRequest,
                NextTimeout, NextPriority, TransId, Source}}
            when ForwardType =:= 'cloudi_service_forward_sync_retry' ->
            {'cloudi_service_request_success',
             {ForwardType, NextName,
              NextRequestInfo, NextRequest,
              NextTimeout, NextPriority, TransId, Source},
             ServiceState};
        Type:Error ->
            {'cloudi_service_request_failure',
             Type, Error, erlang:get_stacktrace(), ServiceState}
    end.

handle_module_info(Request, Module, Dispatcher, ServiceState) ->
    case Module:cloudi_service_handle_info(Request,
                                           ServiceState,
                                           Dispatcher) of
        {noreply, NewServiceState} ->
            {'cloudi_service_info_success',
             NewServiceState};
        {stop, Reason, NewServiceState} ->
            {'cloudi_service_info_failure',
             Reason, NewServiceState}
    end.

send_async_active_timeout_start(Timeout, TransId,
                                #state{dispatcher = Self,
                                       send_timeouts = SendTimeouts} = State)
    when is_integer(Timeout), is_binary(TransId) ->
    State#state{
        send_timeouts = dict:store(TransId, {active,
            erlang:send_after(Timeout, Self,
                              {'cloudi_service_send_async_timeout', TransId})},
            SendTimeouts)}.

process_queue(NewServiceState,
              #state{dispatcher = Dispatcher,
                     recv_timeouts = RecvTimeouts,
                     queue_requests = true,
                     queued = Queue,
                     module = Module,
                     request_pid = RequestPid,
                     options = ConfigOptions} = State) ->
    case cloudi_x_pqueue4:out(Queue) of
        {empty, NewQueue} ->
            State#state{queue_requests = false,
                        queued = NewQueue,
                        service_state = NewServiceState};
        {{value, {'cloudi_service_send_async', Name, Pattern,
                  RequestInfo, Request,
                  _, Priority, TransId, Pid}}, NewQueue} ->
            Timeout = case erlang:cancel_timer(dict:fetch(TransId,
                                                          RecvTimeouts)) of
                false ->
                    0;
                V ->
                    V
            end,
            State#state{
                recv_timeouts = dict:erase(TransId, RecvTimeouts),
                queued = NewQueue,
                request_pid = handle_module_request_loop_pid(RequestPid,
                    {'cloudi_service_request_loop',
                     'send_async', Name, Pattern,
                     RequestInfo, Request,
                     Timeout, Priority, TransId, Pid,
                     Module, Dispatcher, ConfigOptions,
                     NewServiceState}, ConfigOptions, Dispatcher)};
        {{value, {'cloudi_service_send_sync', Name, Pattern,
                  RequestInfo, Request,
                  _, Priority, TransId, Pid}}, NewQueue} ->
            Timeout = case erlang:cancel_timer(dict:fetch(TransId,
                                                          RecvTimeouts)) of
                false ->
                    0;
                V ->
                    V
            end,
            State#state{
                recv_timeouts = dict:erase(TransId, RecvTimeouts),
                queued = NewQueue,
                request_pid = handle_module_request_loop_pid(RequestPid,
                    {'cloudi_service_request_loop',
                     'send_sync', Name, Pattern,
                     RequestInfo, Request,
                     Timeout, Priority, TransId, Pid,
                     Module, Dispatcher, ConfigOptions,
                     NewServiceState}, ConfigOptions, Dispatcher)}
    end.

process_queue_info(NewServiceState,
                   #state{dispatcher = Dispatcher,
                          queue_requests = true,
                          queued_info = QueueInfo,
                          module = Module,
                          info_pid = InfoPid,
                          options = ConfigOptions} = State) ->
    case queue:out(QueueInfo) of
        {empty, NewQueueInfo} ->
            State#state{queue_requests = false,
                        queued_info = NewQueueInfo,
                        service_state = NewServiceState};
        {{value, Request}, NewQueueInfo} ->
            State#state{
                queued_info = NewQueueInfo,
                info_pid = handle_module_info_loop_pid(InfoPid,
                    {'cloudi_service_info_loop',
                     Request, Module, Dispatcher,
                     NewServiceState}, ConfigOptions, Dispatcher)}
    end.

process_queues(NewServiceState, State) ->
    % info messages should be processed before service requests
    NewState = process_queue_info(NewServiceState, State),
    if
        NewState#state.queue_requests =:= false ->
            process_queue(NewServiceState,
                          NewState#state{queue_requests = true});
        true ->
            NewState
    end.

handle_module_request_loop_pid(OldRequestPid, ModuleRequest,
                               ConfigOptions, ResultPid) ->
    if
        OldRequestPid =:= undefined ->
            Uses = ConfigOptions#config_service_options.request_pid_uses,
            erlang:spawn_opt(fun() ->
                handle_module_request_loop(Uses, ModuleRequest, ResultPid)
            end, ConfigOptions#config_service_options.request_pid_options);
        is_pid(OldRequestPid) ->
            OldRequestPid ! ModuleRequest,
            OldRequestPid
    end.

handle_module_request_loop(Uses, ResultPid) ->
    receive
        {'cloudi_service_request_loop',
         _Type, _Name, _Pattern,
         _RequestInfo, _Request,
         _Timeout, _Priority, _TransId, _Pid,
         _Module, _Dispatcher, _ConfigOptions,
         _NewServiceState} = ModuleRequest ->
            handle_module_request_loop(Uses, ModuleRequest, ResultPid)
    end.

handle_module_request_loop(Uses,
                           {'cloudi_service_request_loop',
                            Type, Name, Pattern,
                            RequestInfo, Request,
                            Timeout, Priority, TransId, Pid,
                            Module, Dispatcher, ConfigOptions,
                            NewServiceState}, ResultPid) ->
    Result = handle_module_request(Type, Name, Pattern,
                                   RequestInfo, Request,
                                   Timeout, Priority, TransId, Pid,
                                   Module, Dispatcher, ConfigOptions,
                                   NewServiceState),
    if
        Uses == 1 ->
            erlang:exit(Result);
        is_integer(Uses) ->
            ResultPid ! Result,
            handle_module_request_loop(Uses - 1, ResultPid);
        Uses =:= infinity ->
            ResultPid ! Result,
            handle_module_request_loop(Uses, ResultPid)
    end.

handle_module_info_loop_pid(OldInfoPid, ModuleInfo,
                            ConfigOptions, ResultPid) ->
    if
        OldInfoPid =:= undefined ->
            Uses = ConfigOptions#config_service_options.info_pid_uses,
            erlang:spawn_opt(fun() ->
                handle_module_info_loop(Uses, ModuleInfo, ResultPid)
            end, ConfigOptions#config_service_options.info_pid_options);
        is_pid(OldInfoPid) ->
            OldInfoPid ! ModuleInfo,
            OldInfoPid
    end.

handle_module_info_loop(Uses, ResultPid) ->
    receive
        {'cloudi_service_info_loop',
         _Request, _Module, _Dispatcher,
         _NewServiceState} = ModuleInfo ->
            handle_module_info_loop(Uses, ModuleInfo, ResultPid)
    end.

handle_module_info_loop(Uses,
                        {'cloudi_service_info_loop',
                         Request, Module, Dispatcher,
                         NewServiceState}, ResultPid) ->
    Result = handle_module_info(Request, Module, Dispatcher,
                                NewServiceState),
    if
        Uses == 1 ->
            erlang:exit(Result);
        is_integer(Uses) ->
            ResultPid ! Result,
            handle_module_info_loop(Uses - 1, ResultPid);
        Uses =:= infinity ->
            ResultPid ! Result,
            handle_module_info_loop(Uses, ResultPid)
    end.

% duo_mode specific logic

duo_mode_loop_init(#state_duo{dispatcher = Dispatcher} = State) ->
    receive
        {'cloudi_service_init', {ok, ServiceState}} ->
            erlang:process_flag(trap_exit, true),
            duo_mode_loop(duo_process_queues(ServiceState, State));
        Request ->
            % mimic a gen_server handle_info for code reuse
            case duo_handle_info(Request, State) of
                {stop, Reason, _} ->
                    % do not call Module:cloudi_service_terminate/2
                    % since init has not completed yet
                    erlang:exit(Dispatcher, Reason);
                {noreply, NewState} ->
                    duo_mode_loop_init(NewState)
            end
    end.

duo_mode_loop(#state_duo{dispatcher = Dispatcher} = State) ->
    receive
        Request ->
            % mimic a gen_server handle_info for code reuse
            case duo_handle_info(Request, State) of
                {stop, Reason, #state_duo{module = Module,
                                          service_state = ServiceState}} ->
                    Module:cloudi_service_terminate(Reason, ServiceState),
                    erlang:exit(Dispatcher, Reason);
                {noreply, NewState} ->
                    duo_mode_loop(NewState)
            end
    end.

duo_handle_info({'cloudi_service_return_async',
                 _, _, _, _, _, _, Source} = T,
                #state_duo{duo_mode_pid = DuoModePid,
                           dispatcher = Dispatcher} = State) ->
    true = Source =:= DuoModePid,
    Dispatcher ! T,
    {noreply, State};

duo_handle_info({'cloudi_service_return_sync',
                 _, _, _, _, _, _, Source} = T,
                #state_duo{duo_mode_pid = DuoModePid,
                           dispatcher = Dispatcher} = State) ->
    true = Source =:= DuoModePid,
    Dispatcher ! T,
    {noreply, State};

duo_handle_info({'cloudi_service_request_success', RequestResponse,
                 NewServiceState},
                #state_duo{dispatcher = Dispatcher} = State) ->
    case RequestResponse of
        undefined ->
            ok;
        {'cloudi_service_return_async', _, _, _, _, _, _, Source} = T ->
            Source ! T;
        {'cloudi_service_return_sync', _, _, _, _, _, _, Source} = T ->
            Source ! T;
        {'cloudi_service_forward_async_retry', _, _, _, _, _, _, _} = T ->
            Dispatcher ! T;
        {'cloudi_service_forward_sync_retry', _, _, _, _, _, _, _} = T ->
            Dispatcher ! T
    end,
    {noreply, duo_process_queues(NewServiceState, State)};

duo_handle_info({'cloudi_service_request_failure',
                 Type, Error, Stack, NewServiceState}, State) ->
    Reason = if
        Type =:= stop ->
            true = Stack =:= undefined,
            ?LOG_ERROR("duo_mode request stop ~p", [Error]),
            Error;
        true ->
            ?LOG_ERROR("duo_mode request ~p ~p~n~p", [Type, Error, Stack]),
            {Type, {Error, Stack}}
    end,
    {stop, Reason, State#state_duo{service_state = NewServiceState}};

duo_handle_info({'EXIT', RequestPid,
                 {'cloudi_service_request_success', _RequestResponse,
                  _NewServiceState} = Result},
                #state_duo{request_pid = RequestPid} = State) ->
    duo_handle_info(Result, State#state_duo{request_pid = undefined});

duo_handle_info({'EXIT', RequestPid,
                 {'cloudi_service_request_failure',
                  _Type, _Error, _Stack, _NewServiceState} = Result},
                #state_duo{request_pid = RequestPid} = State) ->
    duo_handle_info(Result, State#state_duo{request_pid = undefined});

duo_handle_info({'EXIT', RequestPid, Reason},
                #state_duo{request_pid = RequestPid} = State) ->
    ?LOG_ERROR("~p duo_mode request exited: ~p", [RequestPid, Reason]),
    {stop, Reason, State};

duo_handle_info({'EXIT', Dispatcher, shutdown},
                #state_duo{dispatcher = Dispatcher} = State) ->
    {stop, shutdown, State};

duo_handle_info({'EXIT', Dispatcher, restart},
                #state_duo{dispatcher = Dispatcher} = State) ->
    {stop, restart, State};

duo_handle_info({'EXIT', Dispatcher, Reason},
                #state_duo{dispatcher = Dispatcher} = State) ->
    ?LOG_ERROR("~p duo_mode dispatcher exited: ~p", [Dispatcher, Reason]),
    {stop, Reason, State};

duo_handle_info({'cloudi_service_send_async',
                 Name, Pattern, RequestInfo, Request,
                 Timeout, Priority, TransId, Pid},
                #state_duo{duo_mode_pid = DuoModePid,
                           queue_requests = false,
                           dispatcher = Dispatcher,
                           module = Module,
                           service_state = ServiceState,
                           request_pid = RequestPid,
                           options = ConfigOptions} = State) ->
    {noreply, State#state_duo{
        queue_requests = true,
        request_pid = handle_module_request_loop_pid(RequestPid,
            {'cloudi_service_request_loop',
             'send_async', Name, Pattern,
             RequestInfo, Request,
             Timeout, Priority, TransId, Pid,
             Module, Dispatcher, ConfigOptions,
             ServiceState}, ConfigOptions, DuoModePid)}};

duo_handle_info({'cloudi_service_send_sync',
                 Name, Pattern, RequestInfo, Request,
                 Timeout, Priority, TransId, Pid},
                #state_duo{duo_mode_pid = DuoModePid,
                           queue_requests = false,
                           dispatcher = Dispatcher,
                           module = Module,
                           service_state = ServiceState,
                           request_pid = RequestPid,
                           options = ConfigOptions} = State) ->
    {noreply, State#state_duo{
        queue_requests = true,
        request_pid = handle_module_request_loop_pid(RequestPid,
            {'cloudi_service_request_loop',
             'send_sync', Name, Pattern,
             RequestInfo, Request,
             Timeout, Priority, TransId, Pid,
             Module, Dispatcher, ConfigOptions,
             ServiceState}, ConfigOptions, DuoModePid)}};

duo_handle_info({Type, _, _, _, _, 0, _, _, _},
                #state_duo{queue_requests = true} = State)
    when Type =:= 'cloudi_service_send_async';
         Type =:= 'cloudi_service_send_sync' ->
    {noreply, State};

duo_handle_info({Type, _, _, _, _, Timeout, Priority, TransId, _} = T,
                #state_duo{queue_requests = true,
                           queued = Queue,
                           options = ConfigOptions} = State)
    when Type =:= 'cloudi_service_send_async';
         Type =:= 'cloudi_service_send_sync' ->
    QueueLimit = ConfigOptions#config_service_options.queue_limit,
    QueueLimitOk = if
        QueueLimit /= undefined ->
            cloudi_x_pqueue4:len(Queue) < QueueLimit;
        true ->
            true
    end,
    if
        QueueLimitOk ->
            {noreply,
             duo_recv_timeout_start(Timeout, Priority, TransId, T, State)};
        true ->
            % message is discarded since too many messages have been queued
            {noreply, State}
    end;

duo_handle_info({'cloudi_service_recv_timeout', Priority, TransId},
                #state_duo{recv_timeouts = RecvTimeouts,
                           queue_requests = QueueRequests,
                           queued = Queue} = State) ->
    NewQueue = if
        QueueRequests =:= true ->
            cloudi_x_pqueue4:filter(fun({_, _, _, _, _, _, _, Id, _}) ->
                Id /= TransId
            end, Priority, Queue);
        true ->
            Queue
    end,
    {noreply, State#state_duo{recv_timeouts = dict:erase(TransId, RecvTimeouts),
                              queued = NewQueue}};

duo_handle_info(Request,
                #state_duo{queue_requests = true,
                           queued_info = QueueInfo} = State) ->
    {noreply, State#state_duo{queued_info = queue:in(Request, QueueInfo)}};

duo_handle_info(Request,
                #state_duo{dispatcher = Dispatcher,
                           module = Module,
                           service_state = ServiceState} = State) ->
    case handle_module_info(Request, Module, Dispatcher, ServiceState) of
        {'cloudi_service_info_success', NewServiceState} ->
            {noreply, State#state_duo{service_state = NewServiceState}};
        {'cloudi_service_info_failure', Reason, NewServiceState} ->
            ?LOG_ERROR("duo_mode info stop ~p", [Reason]),
            {stop, Reason, State#state_duo{service_state = NewServiceState}}
    end.

duo_process_queue_info(NewServiceState,
                       #state_duo{queue_requests = true,
                                  queued_info = QueueInfo,
                                  dispatcher = Dispatcher,
                                  module = Module} = State) ->
    case queue:out(QueueInfo) of
        {empty, NewQueueInfo} ->
            State#state_duo{service_state = NewServiceState,
                            queue_requests = false,
                            queued_info = NewQueueInfo};
        {{value, Request}, NewQueueInfo} ->
            case handle_module_info(Request, Module, Dispatcher,
                                    NewServiceState) of
                {'cloudi_service_info_success', NextServiceState} ->
                    duo_process_queue_info(NextServiceState,
                        State#state_duo{queued_info = NewQueueInfo});
                {'cloudi_service_info_failure', Reason, NextServiceState} ->
                    ?LOG_ERROR("duo_mode info stop ~p", [Reason]),
                    {stop, Reason,
                     State#state_duo{service_state = NextServiceState,
                                     queued_info = NewQueueInfo}}
            end
    end.

duo_process_queue(NewServiceState,
                  #state_duo{duo_mode_pid = DuoModePid,
                             recv_timeouts = RecvTimeouts,
                             queue_requests = true,
                             queued = Queue,
                             dispatcher = Dispatcher,
                             module = Module,
                             request_pid = RequestPid,
                             options = ConfigOptions} = State) ->
    case cloudi_x_pqueue4:out(Queue) of
        {empty, NewQueue} ->
            State#state_duo{queue_requests = false,
                            queued = NewQueue,
                            service_state = NewServiceState};
        {{value, {'cloudi_service_send_async', Name, Pattern,
                  RequestInfo, Request,
                  _, Priority, TransId, Pid}}, NewQueue} ->
            Timeout = case erlang:cancel_timer(dict:fetch(TransId,
                                                          RecvTimeouts)) of
                false ->
                    0;
                V ->
                    V
            end,
            State#state_duo{
                recv_timeouts = dict:erase(TransId, RecvTimeouts),
                queued = NewQueue,
                request_pid = handle_module_request_loop_pid(RequestPid,
                    {'cloudi_service_request_loop',
                     'send_async', Name, Pattern,
                     RequestInfo, Request,
                     Timeout, Priority, TransId, Pid,
                     Module, Dispatcher, ConfigOptions,
                     NewServiceState}, ConfigOptions, DuoModePid)};
        {{value, {'cloudi_service_send_sync', Name, Pattern,
                  RequestInfo, Request,
                  _, Priority, TransId, Pid}}, NewQueue} ->
            Timeout = case erlang:cancel_timer(dict:fetch(TransId,
                                                          RecvTimeouts)) of
                false ->
                    0;
                V ->
                    V
            end,
            State#state_duo{
                recv_timeouts = dict:erase(TransId, RecvTimeouts),
                queued = NewQueue,
                request_pid = handle_module_request_loop_pid(RequestPid,
                    {'cloudi_service_request_loop',
                     'send_sync', Name, Pattern,
                     RequestInfo, Request,
                     Timeout, Priority, TransId, Pid,
                     Module, Dispatcher, ConfigOptions,
                     NewServiceState}, ConfigOptions, DuoModePid)}
    end.

duo_process_queues(NewServiceState, State) ->
    % info messages should be processed before service requests
    NewState = duo_process_queue_info(NewServiceState, State),
    if
        NewState#state_duo.queue_requests =:= false ->
            duo_process_queue(NewServiceState,
                              NewState#state_duo{queue_requests = true});
        true ->
            NewState
    end.

