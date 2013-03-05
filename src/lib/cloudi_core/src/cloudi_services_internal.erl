%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
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
%%% @version 1.2.0 {@date} {@time}
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
        dispatcher,                    % self()
        module,                        % service module
        service_state,                 % service state
        process_index,                 % 0-based index of the Erlang process
        prefix,                        % subscribe/unsubscribe name prefix
        timeout_async,                 % default timeout for send_async
        timeout_sync,                  % default timeout for send_sync
        init_timeout,                  % init timeout handler
        send_timeouts = dict:new(),    % tracking for send timeouts
        recv_timeouts = dict:new(),    % tracking for recv timeouts
        async_responses = dict:new(),  % tracking for async messages
        queue_requests = true,         % is the request pid busy?
        queued = pqueue4:new(),        % queued incoming messages
        queued_info = queue:new(),     % queue process messages for service
        init = undefined,              % init pid
        request = undefined,           % request pid
        info = undefined,              % info pid
        uuid_generator,                % transaction id generator
        dest_refresh,                  % immediate_closest | lazy_closest |
                                       % immediate_furthest | lazy_furthest |
                                       % immediate_random | lazy_random |
                                       % immediate_local | lazy_local |
                                       % immediate_remote | lazy_remote,
                                       % destination pid refresh
        cpg_data = cpg_data:get_empty_groups(), % dest_refresh lazy
        dest_deny,                     % denied from sending to a destination
        dest_allow,                    % allowed to send to a destination
        options                        % #config_service_options{}
    }).

-import(cloudi_services_common,
        [destination_allowed/3,
         destination_refresh_first/2,
         destination_refresh_start/2,
         destination_get/4,
         destination_all/4,
         send_async_timeout_start/4,
         send_sync_timeout_start/5,
         send_timeout_check/2,
         send_timeout_end/2,
         recv_timeout_start/7,
         async_response_timeout_start/6,
         async_response_timeout_end/2,
         ?RECV_ASYNC_STRATEGY/1]).

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
    true = (DestRefresh =:= immediate_closest) or
           (DestRefresh =:= lazy_closest) or
           (DestRefresh =:= immediate_furthest) or
           (DestRefresh =:= lazy_furthest) or
           (DestRefresh =:= immediate_random) or
           (DestRefresh =:= lazy_random) or
           (DestRefresh =:= immediate_local) or
           (DestRefresh =:= lazy_local) or
           (DestRefresh =:= immediate_remote) or
           (DestRefresh =:= lazy_remote) or
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
    InitPid = erlang:spawn_link(fun() ->
        Dispatcher ! {'cloudi_service_init',
                      Module:cloudi_service_init(Args, Prefix, Dispatcher)}
    end),
    InitTimeout = erlang:send_after(Timeout, Dispatcher,
                                    'cloudi_service_init_timeout'),
    quickrand:seed(),
    destination_refresh_first(DestRefresh, ConfigOptions),
    {ok, #state{dispatcher = Dispatcher,
                module = Module,
                process_index = ProcessIndex,
                prefix = Prefix,
                init_timeout = InitTimeout,
                timeout_async = TimeoutAsync,
                timeout_sync = TimeoutSync,
                init = InitPid,
                uuid_generator = uuid:new(Dispatcher),
                dest_refresh = DestRefresh,
                dest_deny = DestDeny,
                dest_allow = DestAllow,
                options = ConfigOptions}}.

handle_call(process_index, _, #state{process_index = ProcessIndex} = State) ->
    {reply, ProcessIndex, State};

handle_call({'subscribe', Pattern}, _,
            #state{dispatcher = Dispatcher,
                   prefix = Prefix} = State) ->
    ok = cpg:join(Prefix ++ Pattern, Dispatcher),
    {reply, ok, State};

handle_call({'unsubscribe', Pattern}, _,
            #state{dispatcher = Dispatcher,
                   prefix = Prefix} = State) ->
    ok = cpg:leave(Prefix ++ Pattern, Dispatcher),
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
                    NewAsyncResponses = dict:erase(TransIdPick, AsyncResponses),
                    {reply, {ok, ResponseInfo, Response, TransIdPick},
                     State#state{async_responses = NewAsyncResponses}};
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
                    NewAsyncResponses = dict:erase(TransId, AsyncResponses),
                    {reply, {ok, ResponseInfo, Response, TransId},
                     State#state{async_responses = NewAsyncResponses}};
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

handle_info({'cloudi_service_init', Result},
            #state{init = InitPid,
                   init_timeout = InitTimeout,
                   queue_requests = true} = State) ->
    case Result of
        {ok, ServiceState} ->
            erlang:cancel_timer(InitTimeout),
            erlang:unlink(InitPid),
            erlang:process_flag(trap_exit, true),
            {noreply, process_queues(ServiceState,
                                     State#state{init = undefined,
                                                 init_timeout = undefined})};
        {stop, Reason} ->
            {stop, Reason, State}
    end;

handle_info('cloudi_service_init_timeout', State) ->
    {stop, timeout, State};

handle_info({'EXIT', _, shutdown}, State) ->
    {stop, shutdown, State};

handle_info({'EXIT', _,
             {'cloudi_service_request_success',
              NewServiceState}}, State) ->
    {noreply, process_queues(NewServiceState, State)};

handle_info({'EXIT', _,
             {'cloudi_service_info_success',
              NewServiceState}}, State) ->
    {noreply, process_queues(NewServiceState, State)};

handle_info({'EXIT', _,
             {'cloudi_service_request_failure',
              Type, Error, Stack, NewServiceState}}, State) ->
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

handle_info({'EXIT', _,
             {'cloudi_service_info_failure',
              Reason, NewServiceState}}, State) ->
    ?LOG_ERROR("info stop ~p", [Reason]),
    {stop, Reason, State#state{service_state = NewServiceState}};

handle_info({'EXIT', _, restart}, State) ->
    % CloudI Service API requested a restart
    {stop, restart, State};

handle_info({'EXIT', Pid, Reason}, State) ->
    % a service request process died unexpectedly
    ?LOG_ERROR("~p exited: ~p", [Pid, Reason]),
    {stop, Reason, State};

handle_info({cpg_data, Groups},
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
             Name, RequestInfo, Request, Timeout, Priority, TransId, Pid},
            #state{dispatcher = Dispatcher,
                   dest_refresh = DestRefresh,
                   cpg_data = Groups,
                   dest_deny = DestDeny,
                   dest_allow = DestAllow} = State) ->
    case destination_allowed(Name, DestDeny, DestAllow) of
        true ->
            case destination_get(DestRefresh, Name, Pid, Groups) of
                {error, _} when Timeout >= ?FORWARD_ASYNC_INTERVAL ->
                    erlang:send_after(?FORWARD_ASYNC_INTERVAL, Dispatcher,
                                      {'cloudi_service_forward_async_retry',
                                       Name, RequestInfo, Request,
                                       Timeout - ?FORWARD_ASYNC_INTERVAL,
                                       Priority, TransId, Pid}),
                    ok;
                {error, _} ->
                    ok;
                {ok, NextPattern, NextPid} when Timeout >= ?FORWARD_DELTA ->
                    NextPid ! {'cloudi_service_send_async', Name, NextPattern,
                               RequestInfo, Request,
                               Timeout - ?FORWARD_DELTA,
                               Priority, TransId, Pid};
                _ ->
                    ok
            end;
        false ->
            ok
    end,
    {noreply, State};

handle_info({'cloudi_service_forward_sync_retry', Name, RequestInfo, Request,
             Timeout, Priority, TransId, Pid},
            #state{dispatcher = Dispatcher,
                   dest_refresh = DestRefresh,
                   cpg_data = Groups,
                   dest_deny = DestDeny,
                   dest_allow = DestAllow} = State) ->
    case destination_allowed(Name, DestDeny, DestAllow) of
        true ->
            case destination_get(DestRefresh, Name, Pid, Groups) of
                {error, _} when Timeout >= ?FORWARD_SYNC_INTERVAL ->
                    erlang:send_after(?FORWARD_SYNC_INTERVAL, Dispatcher,
                                      {'cloudi_service_forward_sync_retry',
                                       Name, RequestInfo, Request,
                                       Timeout - ?FORWARD_SYNC_INTERVAL,
                                       Priority, TransId, Pid}),
                    ok;
                {error, _} ->
                    ok;
                {ok, NextPattern, NextPid} when Timeout >= ?FORWARD_DELTA ->
                    NextPid ! {'cloudi_service_send_sync', Name, NextPattern,
                               RequestInfo, Request,
                               Timeout - ?FORWARD_DELTA,
                               Priority, TransId, Pid};
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
                    NewAsyncResponses = dict:erase(TransIdPick, AsyncResponses),
                    gen_server:reply(Client,
                                     {ok, ResponseInfo, Response, TransIdPick}),
                    {noreply, State#state{async_responses = NewAsyncResponses}};
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
                    NewAsyncResponses = dict:erase(TransId, AsyncResponses),
                    gen_server:reply(Client,
                                     {ok, ResponseInfo, Response, TransId}),
                    {noreply, State#state{async_responses = NewAsyncResponses}};
                {ok, {ResponseInfo, Response}} when Consume =:= false ->
                    gen_server:reply(Client,
                                     {ok, ResponseInfo, Response, TransId}),
                    {noreply, State}
            end
    end;

handle_info({'cloudi_service_send_async',
             Name, Pattern, RequestInfo, Request,
             Timeout, Priority, TransId, Pid},
            #state{dispatcher = Dispatcher,
                   module = Module,
                   service_state = ServiceState,
                   queue_requests = false,
                   options = ConfigOptions} = State) ->
    RequestPid = erlang:spawn_link(fun() ->
        handle_module_request('send_async', Name, Pattern,
                              RequestInfo, Request,
                              Timeout, Priority, TransId, Pid,
                              Module, Dispatcher, ConfigOptions,
                              ServiceState)
    end),
    {noreply, State#state{queue_requests = true,
                          request = RequestPid}};

handle_info({'cloudi_service_send_sync',
             Name, Pattern, RequestInfo, Request,
             Timeout, Priority, TransId, Pid},
            #state{dispatcher = Dispatcher,
                   module = Module,
                   service_state = ServiceState,
                   queue_requests = false,
                   options = ConfigOptions} = State) ->
    RequestPid = erlang:spawn_link(fun() ->
        handle_module_request('send_sync', Name, Pattern,
                              RequestInfo, Request,
                              Timeout, Priority, TransId, Pid,
                              Module, Dispatcher, ConfigOptions,
                              ServiceState)
    end),
    {noreply, State#state{queue_requests = true,
                          request = RequestPid}};

handle_info({Type, _, _, _, _, 0, _, _, _},
            #state{queue_requests = true} = State)
    when Type =:= 'cloudi_service_send_async';
         Type =:= 'cloudi_service_send_sync' ->
    {noreply, State};

handle_info({Type, _, _, _, _, Timeout, Priority, TransId, _} = T,
            #state{dispatcher = Dispatcher,
                   recv_timeouts = RecvTimeouts,
                   queue_requests = true,
                   queued = Queue,
                   options = ConfigOptions} = State)
    when Type =:= 'cloudi_service_send_async';
         Type =:= 'cloudi_service_send_sync' ->
    QueueLimit = ConfigOptions#config_service_options.queue_limit,
    QueueLimitOk = if
        QueueLimit /= undefined ->
            pqueue4:len(Queue) < QueueLimit;
        true ->
            true
    end,
    if
        QueueLimitOk ->
            {NewRecvTimeouts,
             NewQueue} = recv_timeout_start(Timeout, Priority, TransId, T,
                                            RecvTimeouts, Queue, Dispatcher),
            {noreply, State#state{recv_timeouts = NewRecvTimeouts,
                                  queued = NewQueue}};
        true ->
            % message is discarded since too many messages have been queued
            {noreply, State}
    end;

handle_info({'cloudi_service_recv_timeout', Priority, TransId},
            #state{recv_timeouts = RecvTimeouts,
                   queue_requests = QueueRequests,
                   queued = Queue} = State) ->
    NewQueue = if
        QueueRequests =:= true ->
            pqueue4:filter(fun({_, _, _, _, _, _, _, Id, _}) ->
                Id /= TransId
            end, Priority, Queue);
        true ->
            Queue
    end,
    {noreply, State#state{recv_timeouts = dict:erase(TransId, RecvTimeouts),
                          queued = NewQueue}};

handle_info({'cloudi_service_return_async',
             Name, Pattern, ResponseInfo, Response, _, TransId, Pid},
            #state{dispatcher = Dispatcher,
                   send_timeouts = SendTimeouts,
                   async_responses = AsyncResponses} = State) ->
    true = Pid =:= Dispatcher,
    case send_timeout_check(TransId, SendTimeouts) of
        error ->
            % send_async timeout already occurred
            {noreply, State};
        {ok, {active, Tref}} when Response == <<>> ->
            erlang:cancel_timer(Tref),
            NewSendTimeouts = send_timeout_end(TransId, SendTimeouts),
            Dispatcher ! {'timeout_async_active', TransId},
            {noreply, State#state{send_timeouts = NewSendTimeouts}};
        {ok, {active, Tref}} ->
            Timeout = case erlang:cancel_timer(Tref) of
                false ->
                    0;
                V ->
                    V
            end,
            NewSendTimeouts = send_timeout_end(TransId, SendTimeouts),
            Dispatcher ! {'return_async_active', Name, Pattern,
                          ResponseInfo, Response, Timeout, TransId},
            {noreply, State#state{send_timeouts = NewSendTimeouts}};
        {ok, {passive, Tref}} when Response == <<>> ->
            erlang:cancel_timer(Tref),
            NewSendTimeouts = send_timeout_end(TransId, SendTimeouts),
            {noreply, State#state{send_timeouts = NewSendTimeouts}};
        {ok, {passive, Tref}} ->
            Timeout = case erlang:cancel_timer(Tref) of
                false ->
                    0;
                V ->
                    V
            end,
            NewSendTimeouts = send_timeout_end(TransId, SendTimeouts),
            NewAsyncResponses = async_response_timeout_start(ResponseInfo,
                                                             Response,
                                                             Timeout,
                                                             TransId,
                                                             AsyncResponses,
                                                             Dispatcher),
            {noreply, State#state{send_timeouts = NewSendTimeouts,
                                  async_responses = NewAsyncResponses}}
    end;

handle_info({'cloudi_service_return_sync',
             _, _, ResponseInfo, Response, _, TransId, Pid},
            #state{dispatcher = Dispatcher,
                   send_timeouts = SendTimeouts} = State) ->
    true = Pid =:= Dispatcher,
    case send_timeout_check(TransId, SendTimeouts) of
        error ->
            % send_async timeout already occurred
            {noreply, State};
        {ok, {Client, Tref}} ->
            erlang:cancel_timer(Tref),
            NewSendTimeouts = send_timeout_end(TransId, SendTimeouts),
            if
                Response == <<>> ->
                    gen_server:reply(Client, {error, timeout});
                ResponseInfo == <<>> ->
                    gen_server:reply(Client, {ok, Response});
                true ->
                    gen_server:reply(Client, {ok, ResponseInfo, Response})
            end,
            {noreply, State#state{send_timeouts = NewSendTimeouts}}
    end;

handle_info({'cloudi_service_send_async_timeout', TransId},
            #state{dispatcher = Dispatcher,
                   send_timeouts = SendTimeouts} = State) ->
    case send_timeout_check(TransId, SendTimeouts) of
        error ->
            % should never happen, timer should have been cancelled
            % if the send_async already returned
            ?LOG_WARN("send timeout not found (trans_id=~s)",
                      [uuid:uuid_to_string(TransId)]),
            {noreply, State};
        {ok, {active, _}} ->
            Dispatcher ! {'timeout_async_active', TransId},
            NewSendTimeouts = send_timeout_end(TransId, SendTimeouts),
            {noreply, State#state{send_timeouts = NewSendTimeouts}};
        {ok, _} ->
            NewSendTimeouts = send_timeout_end(TransId, SendTimeouts),
            {noreply, State#state{send_timeouts = NewSendTimeouts}}
    end;

handle_info({'cloudi_service_send_sync_timeout', TransId},
            #state{send_timeouts = SendTimeouts} = State) ->
    case send_timeout_check(TransId, SendTimeouts) of
        error ->
            % should never happen, timer should have been cancelled
            % if the send_async already returned
            ?LOG_WARN("send timeout not found (trans_id=~s)",
                      [uuid:uuid_to_string(TransId)]),
            {noreply, State};
        {ok, {Client, _}} ->
            NewSendTimeouts = send_timeout_end(TransId, SendTimeouts),
            gen_server:reply(Client, {error, timeout}),
            {noreply, State#state{send_timeouts = NewSendTimeouts}}
    end;

handle_info({'cloudi_service_recv_async_timeout', TransId},
            #state{async_responses = AsyncResponses} = State) ->
    NewAsyncResponses = async_response_timeout_end(TransId, AsyncResponses),
    {noreply, State#state{async_responses = NewAsyncResponses}};

handle_info(Request,
            #state{queue_requests = true,
                   queued_info = QueueInfo} = State) ->
    {noreply, State#state{queued_info = queue:in(Request, QueueInfo)}};

handle_info(Request,
            #state{dispatcher = Dispatcher,
                   module = Module,
                   service_state = ServiceState} = State) ->
    InfoPid = erlang:spawn_link(fun() ->
        handle_module_info(Request, Module, Dispatcher, ServiceState)
    end),
    {noreply, State#state{queue_requests = true,
                          info = InfoPid}}.

terminate(Reason,
          #state{module = Module,
                 service_state = ServiceState}) ->
    Module:cloudi_service_terminate(Reason, ServiceState),
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

handle_get_pid(Name, Timeout, Client,
               #state{dispatcher = Dispatcher,
                      dest_refresh = DestRefresh,
                      cpg_data = Groups} = State) ->
    case destination_get(DestRefresh, Name, Dispatcher, Groups) of
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
                         send_timeouts = SendTimeouts} = State) ->
    case destination_get(DestRefresh, Name, Dispatcher, Groups) of
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
            TransId = uuid:get_v1(UUID),
            Pid ! {'cloudi_service_send_async',
                   Name, Pattern, RequestInfo, Request,
                   Timeout, Priority, TransId, Dispatcher},
            gen_server:reply(Client, {ok, TransId}),
            NewSendTimeouts = send_async_timeout_start(Timeout,
                                                       TransId,
                                                       SendTimeouts,
                                                       Dispatcher),
            {noreply, State#state{send_timeouts = NewSendTimeouts}}
    end.

handle_send_async_pid(Name, Pattern, RequestInfo, Request,
                      Timeout, Priority, Pid,
                      #state{dispatcher = Dispatcher,
                             uuid_generator = UUID,
                             send_timeouts = SendTimeouts} = State) ->
    TransId = uuid:get_v1(UUID),
    Pid ! {'cloudi_service_send_async',
           Name, Pattern, RequestInfo, Request,
           Timeout, Priority, TransId, Dispatcher},
    NewSendTimeouts = send_async_timeout_start(Timeout,
                                               TransId,
                                               SendTimeouts,
                                               Dispatcher),
    {reply, {ok, TransId}, State#state{send_timeouts = NewSendTimeouts}}.

handle_send_async_active(Name, RequestInfo, Request,
                         Timeout, Priority, Client,
                         #state{dispatcher = Dispatcher,
                                uuid_generator = UUID,
                                dest_refresh = DestRefresh,
                                cpg_data = Groups,
                                send_timeouts = SendTimeouts} = State) ->
    case destination_get(DestRefresh, Name, Dispatcher, Groups) of
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
            TransId = uuid:get_v1(UUID),
            Pid ! {'cloudi_service_send_async',
                   Name, Pattern, RequestInfo, Request,
                   Timeout, Priority, TransId, Dispatcher},
            gen_server:reply(Client, {ok, TransId}),
            NewSendTimeouts = send_async_active_timeout_start(Timeout,
                                                              TransId,
                                                              SendTimeouts),
            {noreply, State#state{send_timeouts = NewSendTimeouts}}
    end.

handle_send_async_active_pid(Name, Pattern, RequestInfo, Request,
                             Timeout, Priority, Pid,
                             #state{dispatcher = Dispatcher,
                                    uuid_generator = UUID,
                                    send_timeouts = SendTimeouts} = State) ->
    TransId = uuid:get_v1(UUID),
    Pid ! {'cloudi_service_send_async',
           Name, Pattern, RequestInfo, Request,
           Timeout, Priority, TransId, Dispatcher},
    NewSendTimeouts = send_async_active_timeout_start(Timeout,
                                                      TransId,
                                                      SendTimeouts),
    {reply, {ok, TransId}, State#state{send_timeouts = NewSendTimeouts}}.

handle_send_sync(Name, RequestInfo, Request,
                 Timeout, Priority, Client,
                 #state{dispatcher = Dispatcher,
                        uuid_generator = UUID,
                        dest_refresh = DestRefresh,
                        cpg_data = Groups,
                        send_timeouts = SendTimeouts} = State) ->
    case destination_get(DestRefresh, Name, Dispatcher, Groups) of
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
            TransId = uuid:get_v1(UUID),
            Pid ! {'cloudi_service_send_sync',
                   Name, Pattern, RequestInfo, Request,
                   Timeout, Priority, TransId, Dispatcher},
            NewSendTimeouts = send_sync_timeout_start(Timeout,
                                                      TransId,
                                                      Client,
                                                      SendTimeouts,
                                                      Dispatcher),
            {noreply, State#state{send_timeouts = NewSendTimeouts}}
    end.

handle_send_sync_pid(Name, Pattern, RequestInfo, Request,
                     Timeout, Priority, Pid, Client,
                     #state{dispatcher = Dispatcher,
                            uuid_generator = UUID,
                            send_timeouts = SendTimeouts} = State) ->
    TransId = uuid:get_v1(UUID),
    Pid ! {'cloudi_service_send_sync',
           Name, Pattern, RequestInfo, Request,
           Timeout, Priority, TransId, Dispatcher},
    NewSendTimeouts = send_sync_timeout_start(Timeout,
                                              TransId,
                                              Client,
                                              SendTimeouts,
                                              Dispatcher),
    {noreply, State#state{send_timeouts = NewSendTimeouts}}.

handle_mcast_async(Name, RequestInfo, Request,
                   Timeout, Priority, Client,
                   #state{dispatcher = Dispatcher,
                          uuid_generator = UUID,
                          dest_refresh = DestRefresh,
                          cpg_data = Groups,
                          send_timeouts = SendTimeouts} = State) ->
    case destination_all(DestRefresh, Name, Dispatcher, Groups) of
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
                TransId = uuid:get_v1(UUID),
                Pid ! {'cloudi_service_send_async',
                       Name, Pattern, RequestInfo, Request,
                       Timeout, Priority, TransId, Dispatcher},
                TransId
            end, PidList),
            gen_server:reply(Client, {ok, TransIdList}),
            NewSendTimeouts = lists:foldl(fun(Id, S) ->
                send_async_timeout_start(Timeout, Id, S, Dispatcher)
            end, SendTimeouts, TransIdList),
            {noreply, State#state{send_timeouts = NewSendTimeouts}}
    end.

handle_module_request('send_async', Name, Pattern, RequestInfo, Request,
                      Timeout, Priority, TransId, Pid,
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
    Result = try Module:cloudi_service_handle_request('send_async',
                                                      Name, Pattern,
                                                      RequestInfo, Request,
                                                      Timeout, Priority,
                                                      TransId, Pid,
                                                      ServiceState,
                                                      Dispatcher) of
        {reply, <<>>, NewServiceState} ->
            {'cloudi_service_request_success', NewServiceState};
        {reply, Response, NewServiceState} ->
            Pid ! {'cloudi_service_return_async', Name, Pattern,
                   <<>>, Response,
                   RequestTimeoutF(Timeout), TransId, Pid},
            {'cloudi_service_request_success', NewServiceState};
        {reply, ResponseInfo, Response, NewServiceState} ->
            Pid ! {'cloudi_service_return_async', Name, Pattern,
                   ResponseInfo, Response,
                   RequestTimeoutF(Timeout), TransId, Pid},
            {'cloudi_service_request_success', NewServiceState};
        {forward, _, _, _, NextTimeout, NextPriority, NewServiceState}
            when NextPriority < ?PRIORITY_HIGH;
                 NextPriority > ?PRIORITY_LOW;
                 NextTimeout < 0 ->
            {'cloudi_service_request_failure',
             exit, badarg, erlang:get_stacktrace(), NewServiceState};
        {forward, NextName, NextRequestInfo, NextRequest,
                  Timeout, NextPriority, NewServiceState} ->
            Dispatcher ! {'cloudi_service_forward_async_retry', NextName,
                          NextRequestInfo, NextRequest,
                          RequestTimeoutF(Timeout), NextPriority, TransId, Pid},
            {'cloudi_service_request_success', NewServiceState};
        {forward, NextName, NextRequestInfo, NextRequest,
                  NextTimeout, NextPriority, NewServiceState} ->
            Dispatcher ! {'cloudi_service_forward_async_retry', NextName,
                          NextRequestInfo, NextRequest,
                          NextTimeout, NextPriority, TransId, Pid},
            {'cloudi_service_request_success', NewServiceState};
        {forward, NextName, NextRequestInfo, NextRequest,
                  NewServiceState} ->
            Dispatcher ! {'cloudi_service_forward_async_retry', NextName,
                          NextRequestInfo, NextRequest,
                          RequestTimeoutF(Timeout), Priority, TransId, Pid},
            {'cloudi_service_request_success', NewServiceState};
        {noreply, NewServiceState} ->
            {'cloudi_service_request_success', NewServiceState};
        {stop, Reason, NewServiceState} ->
            {'cloudi_service_request_failure',
             stop, Reason, undefined, NewServiceState}
    catch
        throw:{cloudi_service_return,
               {ReturnType, Name, Pattern,
                ResponseInfo, Response,
                Timeout, TransId, Pid}}
            when ReturnType =:= 'cloudi_service_return_async' ->
            Pid ! {ReturnType, Name, Pattern,
                   ResponseInfo, Response,
                   RequestTimeoutF(Timeout), TransId, Pid},
            {'cloudi_service_request_success', ServiceState};
        throw:{cloudi_service_return,
               {ReturnType, Name, Pattern,
                ResponseInfo, Response,
                NextTimeout, TransId, Pid}}
            when ReturnType =:= 'cloudi_service_return_async' ->
            Pid ! {ReturnType, Name, Pattern,
                   ResponseInfo, Response,
                   NextTimeout, TransId, Pid},
            {'cloudi_service_request_success', ServiceState};
        throw:{cloudi_service_forward,
               {ForwardType, NextName,
                NextRequestInfo, NextRequest,
                Timeout, NextPriority, TransId, Pid}}
            when ForwardType =:= 'cloudi_service_forward_async_retry' ->
            Dispatcher ! {ForwardType, NextName,
                          NextRequestInfo, NextRequest,
                          RequestTimeoutF(Timeout), NextPriority, TransId, Pid},
            {'cloudi_service_request_success', ServiceState};
        throw:{cloudi_service_forward,
               {ForwardType, NextName,
                NextRequestInfo, NextRequest,
                NextTimeout, NextPriority, TransId, Pid}}
            when ForwardType =:= 'cloudi_service_forward_async_retry' ->
            Dispatcher ! {ForwardType, NextName,
                          NextRequestInfo, NextRequest,
                          NextTimeout, NextPriority, TransId, Pid},
            {'cloudi_service_request_success', ServiceState};
        Type:Error ->
            {'cloudi_service_request_failure',
             Type, Error, erlang:get_stacktrace(), ServiceState}
    end,
    erlang:exit(self(), Result);

handle_module_request('send_sync', Name, Pattern, RequestInfo, Request,
                      Timeout, Priority, TransId, Pid,
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
    Result = try Module:cloudi_service_handle_request('send_sync',
                                                      Name, Pattern,
                                                      RequestInfo, Request,
                                                      Timeout, Priority,
                                                      TransId, Pid,
                                                      ServiceState,
                                                      Dispatcher) of
        {reply, <<>>, NewServiceState} ->
            {'cloudi_service_request_success', NewServiceState};
        {reply, Response, NewServiceState} ->
            Pid ! {'cloudi_service_return_sync', Name, Pattern,
                   <<>>, Response,
                   RequestTimeoutF(Timeout), TransId, Pid},
            {'cloudi_service_request_success', NewServiceState};
        {reply, ResponseInfo, Response, NewServiceState} ->
            Pid ! {'cloudi_service_return_sync', Name, Pattern,
                   ResponseInfo, Response,
                   RequestTimeoutF(Timeout), TransId, Pid},
            {'cloudi_service_request_success', NewServiceState};
        {forward, _, _, _, NextTimeout, NextPriority, NewServiceState}
            when NextPriority < ?PRIORITY_HIGH;
                 NextPriority > ?PRIORITY_LOW;
                 NextTimeout < 0 ->
            {'cloudi_service_request_failure',
             exit, badarg, erlang:get_stacktrace(), NewServiceState};
        {forward, NextName, NextRequestInfo, NextRequest,
                  Timeout, NextPriority, NewServiceState} ->
            Dispatcher ! {'cloudi_service_forward_sync_retry', NextName,
                          NextRequestInfo, NextRequest,
                          RequestTimeoutF(Timeout), NextPriority, TransId, Pid},
            {'cloudi_service_request_success', NewServiceState};
        {forward, NextName, NextRequestInfo, NextRequest,
                  NextTimeout, NextPriority, NewServiceState} ->
            Dispatcher ! {'cloudi_service_forward_sync_retry', NextName,
                          NextRequestInfo, NextRequest,
                          NextTimeout, NextPriority, TransId, Pid},
            {'cloudi_service_request_success', NewServiceState};
        {forward, NextName, NextRequestInfo, NextRequest,
                  NewServiceState} ->
            Dispatcher ! {'cloudi_service_forward_sync_retry', NextName,
                          NextRequestInfo, NextRequest,
                          RequestTimeoutF(Timeout), Priority, TransId, Pid},
            {'cloudi_service_request_success', NewServiceState};
        {noreply, NewServiceState} ->
            {'cloudi_service_request_success', NewServiceState};
        {stop, Reason, NewServiceState} ->
            {'cloudi_service_request_failure',
             stop, Reason, undefined, NewServiceState}
    catch
        throw:{cloudi_service_return,
               {ReturnType, Name, Pattern,
                ResponseInfo, Response,
                Timeout, TransId, Pid}}
            when ReturnType =:= 'cloudi_service_return_sync' ->
            Pid ! {ReturnType, Name, Pattern,
                   ResponseInfo, Response,
                   RequestTimeoutF(Timeout), TransId, Pid},
            {'cloudi_service_request_success', ServiceState};
        throw:{cloudi_service_return,
               {ReturnType, Name, Pattern,
                ResponseInfo, Response,
                NextTimeout, TransId, Pid}}
            when ReturnType =:= 'cloudi_service_return_sync' ->
            Pid ! {ReturnType, Name, Pattern,
                   ResponseInfo, Response,
                   NextTimeout, TransId, Pid},
            {'cloudi_service_request_success', ServiceState};
        throw:{cloudi_service_forward,
               {ForwardType, NextName,
                NextRequestInfo, NextRequest,
                Timeout, NextPriority, TransId, Pid}}
            when ForwardType =:= 'cloudi_service_forward_sync_retry' ->
            Dispatcher ! {ForwardType, NextName,
                          NextRequestInfo, NextRequest,
                          RequestTimeoutF(Timeout), NextPriority, TransId, Pid},
            {'cloudi_service_request_success', ServiceState};
        throw:{cloudi_service_forward,
               {ForwardType, NextName,
                NextRequestInfo, NextRequest,
                NextTimeout, NextPriority, TransId, Pid}}
            when ForwardType =:= 'cloudi_service_forward_sync_retry' ->
            Dispatcher ! {ForwardType, NextName,
                          NextRequestInfo, NextRequest,
                          NextTimeout, NextPriority, TransId, Pid},
            {'cloudi_service_request_success', ServiceState};
        Type:Error ->
            {'cloudi_service_request_failure',
             Type, Error, erlang:get_stacktrace(), ServiceState}
    end,
    erlang:exit(self(), Result).

handle_module_info(Request, Module, Dispatcher, ServiceState) ->
    case Module:cloudi_service_handle_info(Request,
                                           ServiceState,
                                           Dispatcher) of
        {noreply, NewServiceState} ->
            erlang:exit(self(),
                        {'cloudi_service_info_success',
                         NewServiceState});
        {stop, Reason, NewServiceState} ->
            erlang:exit(self(),
                        {'cloudi_service_info_failure',
                         Reason, NewServiceState})
    end.

send_async_active_timeout_start(Timeout, TransId, SendTimeouts)
    when is_integer(Timeout), is_binary(TransId) ->
    Tref = erlang:send_after(Timeout, self(),
                             {'cloudi_service_send_async_timeout', TransId}),
    dict:store(TransId, {active, Tref}, SendTimeouts).

process_queue(NewServiceState,
              #state{dispatcher = Dispatcher,
                     module = Module,
                     recv_timeouts = RecvTimeouts,
                     queue_requests = true,
                     queued = Queue,
                     options = ConfigOptions} = State) ->
    case pqueue4:out(Queue) of
        {empty, NewQueue} ->
            State#state{service_state = NewServiceState,
                        queue_requests = false,
                        queued = NewQueue,
                        request = undefined,
                        info = undefined};
        {{value, {'cloudi_service_send_async', Name, Pattern,
                  RequestInfo, Request,
                  _, Priority, TransId, Pid}}, NewQueue} ->
            Tref = dict:fetch(TransId, RecvTimeouts),
            Timeout = case erlang:cancel_timer(Tref) of
                false ->
                    0;
                V ->
                    V
            end,
            RequestPid = erlang:spawn_link(fun() ->
                handle_module_request('send_async', Name, Pattern,
                                      RequestInfo, Request,
                                      Timeout, Priority, TransId, Pid,
                                      Module, Dispatcher, ConfigOptions,
                                      NewServiceState)
            end),
            State#state{recv_timeouts = dict:erase(TransId, RecvTimeouts),
                        queued = NewQueue,
                        request = RequestPid,
                        info = undefined};
        {{value, {'cloudi_service_send_sync', Name, Pattern,
                  RequestInfo, Request,
                  _, Priority, TransId, Pid}}, NewQueue} ->
            Tref = dict:fetch(TransId, RecvTimeouts),
            Timeout = case erlang:cancel_timer(Tref) of
                false ->
                    0;
                V ->
                    V
            end,
            RequestPid = erlang:spawn_link(fun() ->
                handle_module_request('send_sync', Name, Pattern,
                                      RequestInfo, Request,
                                      Timeout, Priority, TransId, Pid,
                                      Module, Dispatcher, ConfigOptions,
                                      NewServiceState)
            end),
            State#state{recv_timeouts = dict:erase(TransId, RecvTimeouts),
                        queued = NewQueue,
                        request = RequestPid,
                        info = undefined}
    end.

process_queue_info(NewServiceState,
                   #state{dispatcher = Dispatcher,
                          module = Module,
                          queue_requests = true,
                          queued_info = QueueInfo} = State) ->
    case queue:out(QueueInfo) of
        {empty, NewQueueInfo} ->
            State#state{service_state = NewServiceState,
                        queue_requests = false,
                        queued_info = NewQueueInfo,
                        request = undefined,
                        info = undefined};
        {{value, Request}, NewQueueInfo} ->
            InfoPid = erlang:spawn_link(fun() ->
                handle_module_info(Request, Module, Dispatcher,
                                   NewServiceState)
            end),
            State#state{queued_info = NewQueueInfo,
                        request = undefined,
                        info = InfoPid}
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
