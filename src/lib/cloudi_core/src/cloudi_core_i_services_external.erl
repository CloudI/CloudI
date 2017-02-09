%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI External Service==
%%% Erlang process which provides the connection to a thread in an
%%% external service.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2011-2017, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011-2017 Michael Truog
%%% @version 1.6.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_core_i_services_external).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_fsm).

%% external interface
-export([start_link/17,
         port/2,
         stdout/2,
         stderr/2,
         get_status/1,
         get_status/2]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4, format_status/2]).

%% FSM States
-export(['CONNECT'/2,
         'INIT_WAIT'/2,
         'INIT'/2,
         'HANDLE'/2]).

-include("cloudi_logger.hrl").
-include("cloudi_core_i_configuration.hrl").
-include("cloudi_core_i_constants.hrl").

% message type enumeration
-define(MESSAGE_INIT,                1).
-define(MESSAGE_SEND_ASYNC,          2).
-define(MESSAGE_SEND_SYNC,           3).
-define(MESSAGE_RECV_ASYNC,          4).
-define(MESSAGE_RETURN_ASYNC,        5).
-define(MESSAGE_RETURN_SYNC,         6).
-define(MESSAGE_RETURNS_ASYNC,       7).
-define(MESSAGE_KEEPALIVE,           8).
-define(MESSAGE_REINIT,              9).
-define(MESSAGE_SUBSCRIBE_COUNT,    10).
-define(MESSAGE_TERM,               11).

-record(state,
    {
        % state record fields common for cloudi_core_i_services_common.hrl:

        % ( 2) self() value cached
        dispatcher :: pid(),
        % ( 3) timeout enforcement for any outgoing service requests
        send_timeouts = ?MAP_NEW()
            :: maps_proxy(cloudi:trans_id(),
                          {passive, pid() | undefined, reference()}) |
               list({cloudi:trans_id(),
                     {passive, pid() | undefined, reference()}}),
        % ( 4) if a sent service request timeout is greater than the
        % service configuration option request_timeout_immediate_max,
        % monitor the destination process with the sent service request
        % transaction id
        send_timeout_monitors = ?MAP_NEW()
            :: maps_proxy(pid(), {reference(), list(cloudi:trans_id())}) |
               list({pid(), {reference(), list(cloudi:trans_id())}}),
        % ( 5) timeout enforcement for any incoming service requests
        recv_timeouts = ?MAP_NEW()
            :: maps_proxy(cloudi:trans_id(), reference()) |
               list({cloudi:trans_id(), reference()}),
        % ( 6) timeout enforcement for any responses to
        % asynchronous outgoing service requests
        async_responses = ?MAP_NEW()
            :: maps_proxy(cloudi:trans_id(), {binary(), binary()}) |
               list({cloudi:trans_id(), {binary(), binary()}}),
        % ( 7) pending update configuration
        update_plan = undefined
            :: undefined | #config_service_update{},
        % ( 8) is the external service OS process thread busy?
        queue_requests = true :: boolean(),
        % ( 9) queued incoming service requests
        queued = cloudi_x_pqueue4:new()
            :: cloudi_x_pqueue4:cloudi_x_pqueue4(
                   cloudi:message_service_request()) |
               list({cloudi:priority_value(), any()}),

        % state record fields unique to the external thread Erlang process:

        % (10) queued size in bytes
        queued_size = 0 :: non_neg_integer(),
        % (11) erlang:system_info(wordsize) cached
        queued_word_size :: pos_integer(),
        % (12) external thread connection protocol
        protocol = undefined :: undefined | tcp | udp | local,
        % (13) external thread connection port
        port = undefined :: undefined | non_neg_integer(),
        % (14) wait for cloudi_core_i_services_monitor to send initialize when
        % all of the service instance processes have been spawned
        initialize = false :: boolean(),
        % (15) udp incoming data port
        incoming_port = undefined :: undefined | inet:port_number(),
        % (16) tcp listener
        listener = undefined,
        % (17) tcp acceptor
        acceptor = undefined,
        % (18) local socket filesystem path
        socket_path = undefined :: undefined | string(),
        % (19) common socket options
        socket_options = undefined :: undefined | list(),
        % (20) data socket
        socket = undefined,
        % (21) service state for executing aspect functions
        % (as assigned from aspect function execution)
        service_state = undefined,
        % (22) pending aspects_request_after
        aspects_request_after_f = undefined,
        % (23) 0-based index of the process in all service instance processes
        process_index :: non_neg_integer(),
        % (24) current count of all Erlang processes for the service instance
        process_count :: pos_integer(),
        % (25) command line of OS execve
        command_line :: list(string()),
        % (26) subscribe/unsubscribe name prefix set in service configuration
        prefix :: cloudi:service_name_pattern(),
        % (27) pre-poll() timeout in the external service thread
        timeout_init
            :: cloudi_service_api:timeout_initialize_value_milliseconds(),
        % (28) default timeout for send_async set in service configuration
        timeout_async
            :: cloudi_service_api:timeout_send_async_value_milliseconds(),
        % (29) default timeout for send_sync set in service configuration
        timeout_sync
            :: cloudi_service_api:timeout_send_sync_value_milliseconds(),
        % (30) post-poll() timeout in the external service thread
        timeout_term
            :: cloudi_service_api:timeout_terminate_value_milliseconds(),
        % (31) OS process pid for SIGKILL
        os_pid = undefined :: undefined | pos_integer(),
        % (32) udp keepalive succeeded
        keepalive = undefined :: undefined | received,
        % (33) init timeout handler
        init_timer :: undefined | reference(),
        % (34) transaction id (UUIDv1) generator
        uuid_generator :: cloudi_x_uuid:state(),
        % (35) how service destination lookups occur for a service request send
        dest_refresh :: cloudi_service_api:dest_refresh(),
        % (36) cached cpg data for lazy destination refresh methods
        cpg_data
            :: undefined | cloudi_x_cpg_data:state() |
               list({cloudi:service_name_pattern(), any()}),
        % (37) old subscriptions to remove after an update's initialization
        subscribed = []
            :: list({cloudi:service_name_pattern(), pos_integer()}),
        % (38) ACL lookup for denied destinations
        dest_deny
            :: undefined | cloudi_x_trie:cloudi_x_trie() |
               list({cloudi:service_name_pattern(), any()}),
        % (39) ACL lookup for allowed destinations
        dest_allow
            :: undefined | cloudi_x_trie:cloudi_x_trie() |
               list({cloudi:service_name_pattern(), any()}),
        % (40) service configuration options
        options
            :: #config_service_options{} |
               cloudi_service_api:service_options_external()
    }).

-record(state_socket,
    {
        % write/read fields
        protocol :: tcp | udp | local,
        port :: non_neg_integer(),
        incoming_port = undefined :: undefined | inet:port_number(),
        listener = undefined,
        acceptor = undefined,
        socket_path = undefined :: undefined | string(),
        socket_options :: list(),
        socket = undefined,
        % read-only fields
        timeout_term = undefined
            :: undefined |
               cloudi_service_api:timeout_terminate_value_milliseconds(),
        os_pid = undefined :: undefined | pos_integer(),
        cgroup = undefined :: cloudi_service_api:cgroup_external()
    }).

-include("cloudi_core_i_services_common.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

start_link(Protocol, SocketPath,
           ThreadIndex, ProcessIndex, ProcessCount,
           CommandLine, BufferSize, Timeout, [PrefixC | _] = Prefix,
           TimeoutAsync, TimeoutSync, TimeoutTerm,
           DestRefresh, DestDeny, DestAllow,
           #config_service_options{
               scope = Scope,
               dispatcher_pid_options = PidOptions} = ConfigOptions, ID)
    when is_atom(Protocol), is_list(SocketPath), is_integer(ThreadIndex),
         is_integer(ProcessIndex), is_integer(ProcessCount),
         is_list(CommandLine),
         is_integer(BufferSize), is_integer(Timeout), is_integer(PrefixC),
         is_integer(TimeoutAsync), is_integer(TimeoutSync),
         is_integer(TimeoutTerm) ->
    true = (Protocol =:= tcp) orelse
           (Protocol =:= udp) orelse
           (Protocol =:= local),
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
    case cloudi_x_cpg:scope_exists(Scope) of
        ok ->
            gen_fsm:start_link(?MODULE,
                               [Protocol, SocketPath,
                                ThreadIndex, ProcessIndex, ProcessCount,
                                CommandLine, BufferSize, Timeout, Prefix,
                                TimeoutAsync, TimeoutSync, TimeoutTerm,
                                DestRefresh, DestDeny, DestAllow,
                                ConfigOptions, ID],
                               [{timeout, Timeout + ?TIMEOUT_DELTA},
                                {spawn_opt,
                                 spawn_opt_options_before(PidOptions)}]);
        {error, Reason} ->
            {error, {service_options_scope_invalid, Reason}}
    end.

port(Dispatcher, Timeout)
    when is_pid(Dispatcher), is_integer(Timeout) ->
    gen_fsm:sync_send_all_state_event(Dispatcher, port,
                                      Timeout + ?TIMEOUT_DELTA).

stdout(OSPid, Output) ->
    % uses a fake module name and a fake line number
    cloudi_core_i_logger_interface:info('STDOUT', OSPid,
                                        undefined, undefined,
                                        filter_stream(Output), undefined).

stderr(OSPid, Output) ->
    % uses a fake module name and a fake line number
    cloudi_core_i_logger_interface:error('STDERR', OSPid,
                                         undefined, undefined,
                                         filter_stream(Output), undefined).

get_status(Dispatcher) ->
    get_status(Dispatcher, 5000).

get_status(Dispatcher, Timeout) ->
    sys:get_status(Dispatcher, Timeout).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_fsm
%%%------------------------------------------------------------------------

init([Protocol, SocketPath,
      ThreadIndex, ProcessIndex, ProcessCount,
      CommandLine, BufferSize, Timeout, Prefix,
      TimeoutAsync, TimeoutSync, TimeoutTerm,
      DestRefresh, DestDeny, DestAllow,
      #config_service_options{
          dispatcher_pid_options = PidOptions} = ConfigOptions, ID])
    when Protocol =:= tcp;
         Protocol =:= udp;
         Protocol =:= local ->
    ok = spawn_opt_options_after(PidOptions),
    erlang:put(?SERVICE_ID_PDICT_KEY, ID),
    erlang:put(?SERVICE_FILE_PDICT_KEY, hd(CommandLine)),
    Dispatcher = self(),
    InitTimer = erlang:send_after(Timeout, Dispatcher,
                                  'cloudi_service_init_timeout'),
    case socket_open(Protocol, SocketPath, ThreadIndex, BufferSize) of
        {ok, StateSocket} ->
            cloudi_x_quickrand:seed(),
            WordSize = erlang:system_info(wordsize),
            NewConfigOptions =
                check_init_receive(check_init_send(ConfigOptions)),
            {ok, MacAddress} = application:get_env(cloudi_core, mac_address),
            {ok, TimestampType} = application:get_env(cloudi_core,
                                                      timestamp_type),
            UUID = cloudi_x_uuid:new(Dispatcher,
                                     [{timestamp_type, TimestampType},
                                      {mac_address, MacAddress}]),
            Groups = destination_refresh_groups(DestRefresh, undefined),
            #config_service_options{
                dest_refresh_start = Delay,
                scope = Scope} = NewConfigOptions,
            process_flag(trap_exit, true),
            destination_refresh(DestRefresh, Dispatcher, Delay, Scope),
            State = #state{dispatcher = Dispatcher,
                           queued_word_size = WordSize,
                           process_index = ProcessIndex,
                           process_count = ProcessCount,
                           command_line = CommandLine,
                           prefix = Prefix,
                           timeout_init = Timeout,
                           timeout_async = TimeoutAsync,
                           timeout_sync = TimeoutSync,
                           timeout_term = TimeoutTerm,
                           init_timer = InitTimer,
                           uuid_generator = UUID,
                           dest_refresh = DestRefresh,
                           cpg_data = Groups,
                           dest_deny = DestDeny,
                           dest_allow = DestAllow,
                           options = NewConfigOptions},
            {ok, 'CONNECT',
             socket_data_to_state(StateSocket, State)};
        {error, Reason} ->
            {stop, Reason}
    end.

% incoming messages (from the port socket)

'CONNECT'({'pid', OSPid}, State) ->
    {next_state, 'CONNECT', os_pid_set(OSPid, State)};

'CONNECT'('init', #state{initialize = Ready} = State) ->
    if
        Ready =:= true ->
            'INIT'('init', State);
        Ready =:= false ->
            {next_state, 'INIT_WAIT', State}
    end;

'CONNECT'(Request, State) ->
    {stop, {'CONNECT', undefined_message, Request}, State}.

'INIT_WAIT'(Request, State) ->
    {stop, {'INIT_WAIT', undefined_message, Request}, State}.

'INIT'('init',
       #state{dispatcher = Dispatcher,
              protocol = Protocol} = State) ->
    ok = os_init(State),
    if
        Protocol =:= udp ->
            ok = send('keepalive_out'(), State),
            erlang:send_after(?KEEPALIVE_UDP, Dispatcher, keepalive_udp);
        true ->
            ok
    end,
    {next_state, 'HANDLE', State};

'INIT'(Request, State) ->
    {stop, {'INIT', undefined_message, Request}, State}.

'HANDLE'('polling', #state{dispatcher = Dispatcher,
                           service_state = ServiceState,
                           command_line = CommandLine,
                           prefix = Prefix,
                           timeout_init = Timeout,
                           init_timer = InitTimer,
                           subscribed = Subscriptions,
                           options = #config_service_options{
                               scope = Scope,
                               aspects_init_after = Aspects}} = State) ->
    % initialization is now complete because the CloudI API poll function
    % has been called for the first time (i.e., by the service code)
    cancel_timer_async(InitTimer),
    case aspects_init(Aspects, CommandLine, Prefix, Timeout,
                      ServiceState) of
        {ok, NewServiceState} ->
            ok = cloudi_core_i_configurator:
                 service_initialized_process(Dispatcher),
            if
                Subscriptions == [] ->
                    ok;
                true ->
                    % if this is a new OS process after an update,
                    % the old subscriptions are removed so that only the
                    % subscriptions from the new initialization remain
                    ok = cloudi_x_cpg:leave_counts(Scope, Subscriptions,
                                                   Dispatcher, infinity)
            end,
            {next_state, 'HANDLE',
             process_queues(State#state{service_state = NewServiceState,
                                        init_timer = undefined,
                                        subscribed = []})};
        {stop, Reason, NewServiceState} ->
            {stop, Reason,
             State#state{service_state = NewServiceState,
                         init_timer = undefined}}
    end;

'HANDLE'({'subscribe', Pattern},
         #state{dispatcher = Dispatcher,
                prefix = Prefix,
                options = #config_service_options{
                    count_process_dynamic = CountProcessDynamic,
                    scope = Scope}} = State) ->
    true = is_list(Pattern) andalso is_integer(hd(Pattern)),
    case cloudi_core_i_rate_based_configuration:
         count_process_dynamic_terminated(CountProcessDynamic) of
        false ->
            ok = cloudi_x_cpg:join(Scope, Prefix ++ Pattern,
                                   Dispatcher, infinity);
        true ->
            ok
    end,
    {next_state, 'HANDLE', State};

'HANDLE'({'subscribe_count', Pattern},
         #state{dispatcher = Dispatcher,
                prefix = Prefix,
                options = #config_service_options{
                    scope = Scope}} = State) ->
    true = is_list(Pattern) andalso is_integer(hd(Pattern)),
    Count = cloudi_x_cpg:join_count(Scope, Prefix ++ Pattern,
                                    Dispatcher, infinity),
    ok = send('subscribe_count_out'(Count), State),
    {next_state, 'HANDLE', State};

'HANDLE'({'unsubscribe', Pattern},
         #state{dispatcher = Dispatcher,
                prefix = Prefix,
                options = #config_service_options{
                    count_process_dynamic = CountProcessDynamic,
                    scope = Scope}} = State) ->
    true = is_list(Pattern) andalso is_integer(hd(Pattern)),
    case cloudi_core_i_rate_based_configuration:
         count_process_dynamic_terminated(CountProcessDynamic) of
        false ->
            case cloudi_x_cpg:leave(Scope, Prefix ++ Pattern,
                                    Dispatcher, infinity) of
                ok ->
                    {next_state, 'HANDLE', State};
                error ->
                    {stop, {error, {unsubscribe_invalid, Pattern}}, State}
            end;
        true ->
            {next_state, 'HANDLE', State}
    end;

'HANDLE'({'send_async', Name, RequestInfo, Request, Timeout, Priority},
         #state{dest_deny = DestDeny,
                dest_allow = DestAllow} = State) ->
    true = is_list(Name) andalso is_integer(hd(Name)),
    true = is_integer(Timeout),
    true = (Timeout >= 0) andalso
           (Timeout =< ?TIMEOUT_MAX_ERLANG),
    true = is_integer(Priority),
    true = (Priority >= ?PRIORITY_HIGH) andalso
           (Priority =< ?PRIORITY_LOW),
    case destination_allowed(Name, DestDeny, DestAllow) of
        true ->
            handle_send_async(Name, RequestInfo, Request,
                              Timeout, Priority, 'HANDLE', State);
        false ->
            ok = send('return_async_out'(), State),
            {next_state, 'HANDLE', State}
    end;

'HANDLE'({'send_sync', Name, RequestInfo, Request, Timeout, Priority},
         #state{dest_deny = DestDeny,
                dest_allow = DestAllow} = State) ->
    true = is_list(Name) andalso is_integer(hd(Name)),
    true = is_integer(Timeout),
    true = (Timeout >= 0) andalso
           (Timeout =< ?TIMEOUT_MAX_ERLANG),
    true = is_integer(Priority),
    true = (Priority >= ?PRIORITY_HIGH) andalso
           (Priority =< ?PRIORITY_LOW),
    case destination_allowed(Name, DestDeny, DestAllow) of
        true ->
            handle_send_sync(Name, RequestInfo, Request,
                             Timeout, Priority, 'HANDLE', State);
        false ->
            ok = send('return_sync_out'(), State),
            {next_state, 'HANDLE', State}
    end;

'HANDLE'({'mcast_async', Name, RequestInfo, Request, Timeout, Priority},
         #state{dest_deny = DestDeny,
                dest_allow = DestAllow} = State) ->
    true = is_list(Name) andalso is_integer(hd(Name)),
    true = is_integer(Timeout),
    true = (Timeout >= 0) andalso
           (Timeout =< ?TIMEOUT_MAX_ERLANG),
    true = is_integer(Priority),
    true = (Priority >= ?PRIORITY_HIGH) andalso
           (Priority =< ?PRIORITY_LOW),
    case destination_allowed(Name, DestDeny, DestAllow) of
        true ->
            handle_mcast_async(Name, RequestInfo, Request,
                               Timeout, Priority, 'HANDLE', State);
        false ->
            ok = send('returns_async_out'(), State),
            {next_state, 'HANDLE', State}
    end;

'HANDLE'({'forward_async', Name, RequestInfo, Request,
          Timeout, Priority, TransId, Source},
         #state{dispatcher = Dispatcher,
                service_state = ServiceState,
                aspects_request_after_f = AspectsRequestAfterF,
                dest_refresh = DestRefresh,
                cpg_data = Groups,
                dest_deny = DestDeny,
                dest_allow = DestAllow,
                options = #config_service_options{
                    request_name_lookup = RequestNameLookup,
                    scope = Scope,
                    aspects_request_after = AspectsAfter}} = State) ->
    true = is_list(Name) andalso is_integer(hd(Name)),
    true = is_integer(Timeout),
    true = (Timeout >= 0) andalso
           (Timeout =< ?TIMEOUT_MAX_ERLANG),
    true = is_integer(Priority),
    true = (Priority >= ?PRIORITY_HIGH) andalso
           (Priority =< ?PRIORITY_LOW),
    <<_:48, 0:1, 0:1, 0:1, 1:1, _:12, 1:1, 0:1, _:62>> = TransId, % v1 UUID
    true = is_pid(Source),
    Result = {forward, Name, RequestInfo, Request, Timeout, Priority},
    try AspectsRequestAfterF(AspectsAfter, Timeout, Result, ServiceState) of
        {ok, NewTimeout, NewServiceState} ->
            case destination_allowed(Name, DestDeny, DestAllow) of
                true ->
                    case destination_get(DestRefresh, Scope, Name, Source,
                                         Groups, NewTimeout) of
                        {error, timeout} ->
                            ok;
                        {error, _}
                            when RequestNameLookup =:= async ->
                            ok;
                        {error, _}
                            when NewTimeout >= ?FORWARD_ASYNC_INTERVAL ->
                            Retry = {'cloudi_service_forward_async_retry',
                                     Name, RequestInfo, Request,
                                     NewTimeout - ?FORWARD_ASYNC_INTERVAL,
                                     Priority, TransId, Source},
                            erlang:send_after(?FORWARD_ASYNC_INTERVAL,
                                              Dispatcher, Retry),
                            ok;
                        {error, _} ->
                            ok;
                        {ok, NextPattern, NextPid}
                            when NewTimeout >= ?FORWARD_DELTA ->
                            NextPid ! {'cloudi_service_send_async',
                                       Name, NextPattern,
                                       RequestInfo, Request,
                                       NewTimeout - ?FORWARD_DELTA,
                                       Priority, TransId, Source};
                        _ ->
                            ok
                    end;
                false ->
                    ok
            end,
            {next_state, 'HANDLE',
             process_queues(State#state{service_state = NewServiceState,
                                        aspects_request_after_f = undefined})};
        {stop, Reason, NewServiceState} ->
            {stop, Reason,
             State#state{service_state = NewServiceState,
                         aspects_request_after_f = undefined}}
    catch
        ErrorType:Error ->
            Stack = erlang:get_stacktrace(),
            ?LOG_ERROR("request ~p ~p~n~p", [ErrorType, Error, Stack]),
            {stop, {ErrorType, {Error, Stack}},
             State#state{aspects_request_after_f = undefined}}
    end;

'HANDLE'({'forward_sync', Name, RequestInfo, Request,
          Timeout, Priority, TransId, Source},
         #state{dispatcher = Dispatcher,
                service_state = ServiceState,
                aspects_request_after_f = AspectsRequestAfterF,
                dest_refresh = DestRefresh,
                cpg_data = Groups,
                dest_deny = DestDeny,
                dest_allow = DestAllow,
                options = #config_service_options{
                    request_name_lookup = RequestNameLookup,
                    scope = Scope,
                    aspects_request_after = AspectsAfter}} = State) ->
    true = is_list(Name) andalso is_integer(hd(Name)),
    true = is_integer(Timeout),
    true = (Timeout >= 0) andalso
           (Timeout =< ?TIMEOUT_MAX_ERLANG),
    true = is_integer(Priority),
    true = (Priority >= ?PRIORITY_HIGH) andalso
           (Priority =< ?PRIORITY_LOW),
    <<_:48, 0:1, 0:1, 0:1, 1:1, _:12, 1:1, 0:1, _:62>> = TransId, % v1 UUID
    true = is_pid(Source),
    Result = {forward, Name, RequestInfo, Request, Timeout, Priority},
    try AspectsRequestAfterF(AspectsAfter, Timeout, Result, ServiceState) of
        {ok, NewTimeout, NewServiceState} ->
            case destination_allowed(Name, DestDeny, DestAllow) of
                true ->
                    case destination_get(DestRefresh, Scope, Name, Source,
                                         Groups, NewTimeout) of
                        {error, timeout} ->
                            ok;
                        {error, _}
                            when RequestNameLookup =:= async ->
                            ok;
                        {error, _}
                            when NewTimeout >= ?FORWARD_SYNC_INTERVAL ->
                            Retry = {'cloudi_service_forward_sync_retry',
                                     Name, RequestInfo, Request,
                                     NewTimeout - ?FORWARD_SYNC_INTERVAL,
                                     Priority, TransId, Source},
                            erlang:send_after(?FORWARD_SYNC_INTERVAL,
                                              Dispatcher, Retry),
                            ok;
                        {error, _} ->
                            ok;
                        {ok, NextPattern, NextPid}
                            when NewTimeout >= ?FORWARD_DELTA ->
                            NextPid ! {'cloudi_service_send_sync',
                                       Name, NextPattern,
                                       RequestInfo, Request,
                                       NewTimeout - ?FORWARD_DELTA,
                                       Priority, TransId, Source};
                        _ ->
                            ok
                    end;
                false ->
                    ok
            end,
            {next_state, 'HANDLE',
             process_queues(State#state{service_state = NewServiceState,
                                        aspects_request_after_f = undefined})};
        {stop, Reason, NewServiceState} ->
            {stop, Reason,
             State#state{service_state = NewServiceState,
                         aspects_request_after_f = undefined}}
    catch
        ErrorType:Error ->
            Stack = erlang:get_stacktrace(),
            ?LOG_ERROR("request ~p ~p~n~p", [ErrorType, Error, Stack]),
            {stop, {ErrorType, {Error, Stack}},
             State#state{aspects_request_after_f = undefined}}
    end;

'HANDLE'({ReturnType, Name, Pattern, ResponseInfo, Response,
          Timeout, TransId, Source},
         #state{service_state = ServiceState,
                aspects_request_after_f = AspectsRequestAfterF,
                options = #config_service_options{
                    response_timeout_immediate_max =
                        ResponseTimeoutImmediateMax,
                    aspects_request_after =
                        AspectsAfter}} = State)
    when ReturnType =:= 'return_async';
         ReturnType =:= 'return_sync' ->
    true = is_list(Name) andalso is_integer(hd(Name)),
    true = is_list(Pattern) andalso is_integer(hd(Pattern)),
    true = is_integer(Timeout),
    true = (Timeout >= 0) andalso
           (Timeout =< ?TIMEOUT_MAX_ERLANG),
    <<_:48, 0:1, 0:1, 0:1, 1:1, _:12, 1:1, 0:1, _:62>> = TransId, % v1 UUID
    true = is_pid(Source),
    Result = if
        ResponseInfo == <<>>, Response == <<>>,
        Timeout < ResponseTimeoutImmediateMax ->
            noreply;
        true ->
            {reply, ResponseInfo, Response}
    end,
    try AspectsRequestAfterF(AspectsAfter, Timeout, Result, ServiceState) of
        {ok, NewTimeout, NewServiceState} ->
            if
                Result =:= noreply ->
                    ok;
                ReturnType =:= 'return_async' ->
                    Source ! {'cloudi_service_return_async',
                              Name, Pattern, ResponseInfo, Response,
                              NewTimeout, TransId, Source};
                ReturnType =:= 'return_sync' ->
                    Source ! {'cloudi_service_return_sync',
                              Name, Pattern, ResponseInfo, Response,
                              NewTimeout, TransId, Source}
            end,
            {next_state, 'HANDLE',
             process_queues(State#state{service_state = NewServiceState,
                                        aspects_request_after_f = undefined})};
        {stop, Reason, NewServiceState} ->
            {stop, Reason,
             State#state{service_state = NewServiceState,
                         aspects_request_after_f = undefined}}
    catch
        ErrorType:Error ->
            Stack = erlang:get_stacktrace(),
            ?LOG_ERROR("request ~p ~p~n~p", [ErrorType, Error, Stack]),
            {stop, {ErrorType, {Error, Stack}},
             State#state{aspects_request_after_f = undefined}}
    end;

'HANDLE'({'recv_async', Timeout, TransId, Consume},
         #state{dispatcher = Dispatcher,
                async_responses = AsyncResponses} = State) ->
    true = is_integer(Timeout),
    true = (Timeout >= 0) andalso
           (Timeout =< ?TIMEOUT_MAX_ERLANG),
    true = is_boolean(Consume),
    case TransId of
        <<0:128>> ->
            case ?MAP_TO_LIST(AsyncResponses) of
                [] when Timeout >= ?RECV_ASYNC_INTERVAL ->
                    erlang:send_after(?RECV_ASYNC_INTERVAL, Dispatcher,
                                      {'cloudi_service_recv_async_retry',
                                       Timeout - ?RECV_ASYNC_INTERVAL,
                                       TransId, Consume}),
                    {next_state, 'HANDLE', State};
                [] ->
                    ok = send('recv_async_out'(timeout, TransId), State),
                    {next_state, 'HANDLE', State};
                L when Consume =:= true ->
                    TransIdPick = ?RECV_ASYNC_STRATEGY(L),
                    {ResponseInfo, Response} = ?MAP_FETCH(TransIdPick,
                                                          AsyncResponses),
                    ok = send('recv_async_out'(ResponseInfo, Response,
                                               TransIdPick),
                              State),
                    {next_state, 'HANDLE', State#state{
                        async_responses = ?MAP_ERASE(TransIdPick,
                                                     AsyncResponses)}};
                L when Consume =:= false ->
                    TransIdPick = ?RECV_ASYNC_STRATEGY(L),
                    {ResponseInfo, Response} = ?MAP_FETCH(TransIdPick,
                                                          AsyncResponses),
                    ok = send('recv_async_out'(ResponseInfo, Response,
                                               TransIdPick),
                              State),
                    {next_state, 'HANDLE', State}
            end;
        <<_:48, 0:1, 0:1, 0:1, 1:1, _:12, 1:1, 0:1, _:62>> -> % v1 UUID
            case ?MAP_FIND(TransId, AsyncResponses) of
                error when Timeout >= ?RECV_ASYNC_INTERVAL ->
                    erlang:send_after(?RECV_ASYNC_INTERVAL, Dispatcher,
                                      {'cloudi_service_recv_async_retry',
                                       Timeout - ?RECV_ASYNC_INTERVAL,
                                       TransId, Consume}),
                    {next_state, 'HANDLE', State};
                error ->
                    ok = send('recv_async_out'(timeout, TransId), State),
                    {next_state, 'HANDLE', State};
                {ok, {ResponseInfo, Response}} when Consume =:= true ->
                    ok = send('recv_async_out'(ResponseInfo, Response, TransId),
                              State),
                    {next_state, 'HANDLE', State#state{
                        async_responses = ?MAP_ERASE(TransId,
                                                     AsyncResponses)}};
                {ok, {ResponseInfo, Response}} when Consume =:= false ->
                    ok = send('recv_async_out'(ResponseInfo, Response, TransId),
                              State),
                    {next_state, 'HANDLE', State}
            end
    end;

'HANDLE'('keepalive', State) ->
    {next_state, 'HANDLE', State#state{keepalive = received}};

'HANDLE'(Request, State) ->
    {stop, {'HANDLE', undefined_message, Request}, State}.

handle_sync_event(port, _, StateName, #state{port = Port} = State) ->
    {reply, Port, StateName, State};

handle_sync_event(Event, _From, StateName, State) ->
    ?LOG_WARN("Unknown event \"~p\"", [Event]),
    {stop, {StateName, undefined_event, Event}, State}.

handle_event(Event, StateName, State) ->
    ?LOG_WARN("Unknown event \"~p\"", [Event]),
    {stop, {StateName, undefined_event, Event}, State}.

handle_info({'cloudi_service_send_async_retry', Name, RequestInfo, Request,
             Timeout, Priority}, StateName, State) ->
    handle_send_async(Name, RequestInfo, Request,
                      Timeout, Priority, StateName, State);

handle_info({'cloudi_service_send_sync_retry', Name, RequestInfo, Request,
             Timeout, Priority}, StateName, State) ->
    handle_send_sync(Name, RequestInfo, Request,
                     Timeout, Priority, StateName, State);

handle_info({'cloudi_service_mcast_async_retry', Name, RequestInfo, Request,
             Timeout, Priority}, StateName, State) ->
    handle_mcast_async(Name, RequestInfo, Request,
                       Timeout, Priority, StateName, State);

handle_info({'cloudi_service_forward_async_retry', Name, RequestInfo, Request,
             Timeout, Priority, TransId, Source}, StateName,
            #state{dispatcher = Dispatcher,
                   dest_refresh = DestRefresh,
                   cpg_data = Groups,
                   options = #config_service_options{
                       request_name_lookup = RequestNameLookup,
                       scope = Scope}} = State) ->
    case destination_get(DestRefresh, Scope, Name, Source, Groups, Timeout) of
        {error, timeout} ->
            ok;
        {error, _} when RequestNameLookup =:= async ->
            ok;
        {error, _} when Timeout >= ?FORWARD_ASYNC_INTERVAL ->
            erlang:send_after(?FORWARD_ASYNC_INTERVAL, Dispatcher,
                              {'cloudi_service_forward_async_retry',
                               Name, RequestInfo, Request,
                               Timeout - ?FORWARD_ASYNC_INTERVAL,
                               Priority, TransId, Source}),
            ok;
        {error, _} ->
            ok;
        {ok, Pattern, NextPid} when Timeout >= ?FORWARD_DELTA ->
            NextPid ! {'cloudi_service_send_async', Name, Pattern,
                       RequestInfo, Request,
                       Timeout - ?FORWARD_DELTA,
                       Priority, TransId, Source};
        _ ->
            ok
    end,
    {next_state, StateName, State};

handle_info({'cloudi_service_forward_sync_retry', Name, RequestInfo, Request,
             Timeout, Priority, TransId, Source}, StateName,
            #state{dispatcher = Dispatcher,
                   dest_refresh = DestRefresh,
                   cpg_data = Groups,
                   options = #config_service_options{
                       request_name_lookup = RequestNameLookup,
                       scope = Scope}} = State) ->
    case destination_get(DestRefresh, Scope, Name, Source, Groups, Timeout) of
        {error, timeout} ->
            ok;
        {error, _} when RequestNameLookup =:= async ->
            ok;
        {error, _} when Timeout >= ?FORWARD_SYNC_INTERVAL ->
            erlang:send_after(?FORWARD_SYNC_INTERVAL, Dispatcher,
                              {'cloudi_service_forward_sync_retry',
                               Name, RequestInfo, Request,
                               Timeout - ?FORWARD_SYNC_INTERVAL,
                               Priority, TransId, Source}),
            ok;
        {error, _} ->
            ok;
        {ok, Pattern, NextPid} when Timeout >= ?FORWARD_DELTA ->
            NextPid ! {'cloudi_service_send_sync', Name, Pattern,
                       RequestInfo, Request,
                       Timeout - ?FORWARD_DELTA,
                       Priority, TransId, Source};
        _ ->
            ok
    end,
    {next_state, StateName, State};

handle_info({'cloudi_service_recv_async_retry', Timeout, TransId, Consume},
            StateName, State) ->
    ?MODULE:StateName({'recv_async', Timeout, TransId, Consume}, State);

handle_info({SendType, Name, Pattern, RequestInfo, Request,
             Timeout, _, TransId, Source}, StateName,
            #state{options = #config_service_options{
                       response_timeout_immediate_max =
                           ResponseTimeoutImmediateMax}} = State)
    when SendType =:= 'cloudi_service_send_async' orelse
         SendType =:= 'cloudi_service_send_sync',
         is_binary(Request) =:= false orelse
         is_binary(RequestInfo) =:= false orelse
         Timeout =:= 0 ->
    if
        Timeout >= ResponseTimeoutImmediateMax ->
            if
                SendType =:= 'cloudi_service_send_async' ->
                    Source ! {'cloudi_service_return_async',
                              Name, Pattern, <<>>, <<>>,
                              Timeout, TransId, Source};
                SendType =:= 'cloudi_service_send_sync' ->
                    Source ! {'cloudi_service_return_sync',
                              Name, Pattern, <<>>, <<>>,
                              Timeout, TransId, Source}
            end;
        true ->
            ok
    end,
    {next_state, StateName, State};

handle_info({SendType, Name, Pattern, RequestInfo, Request,
             Timeout, Priority, TransId, Source}, StateName,
            #state{queue_requests = false,
                   service_state = ServiceState,
                   options = #config_service_options{
                       rate_request_max = RateRequest,
                       response_timeout_immediate_max =
                           ResponseTimeoutImmediateMax} = ConfigOptions
                   } = State)
    when SendType =:= 'cloudi_service_send_async' orelse
         SendType =:= 'cloudi_service_send_sync' ->
    {RateRequestOk, NewRateRequest} = if
        RateRequest =/= undefined ->
            cloudi_core_i_rate_based_configuration:
            rate_request_request(RateRequest);
        true ->
            {true, RateRequest}
    end,
    if
        RateRequestOk =:= true ->
            Type = if
                SendType =:= 'cloudi_service_send_async' ->
                    'send_async';
                SendType =:= 'cloudi_service_send_sync' ->
                    'send_sync'
            end,
            NewConfigOptions =
                check_incoming(true, ConfigOptions#config_service_options{
                                         rate_request_max = NewRateRequest}),
            #config_service_options{
                request_timeout_adjustment = RequestTimeoutAdjustment,
                aspects_request_before = AspectsBefore} = NewConfigOptions,
            try aspects_request_before(AspectsBefore, Type,
                                       Name, Pattern, RequestInfo, Request,
                                       Timeout, Priority, TransId, Source,
                                       ServiceState,
                                       RequestTimeoutAdjustment) of
                {ok, NextTimeout, NewServiceState} ->
                    if
                        SendType =:= 'cloudi_service_send_async' ->
                            ok = send('send_async_out'(Name, Pattern,
                                                       RequestInfo, Request,
                                                       NextTimeout, Priority,
                                                       TransId, Source),
                                      State);
                        SendType =:= 'cloudi_service_send_sync' ->
                            ok = send('send_sync_out'(Name, Pattern,
                                                      RequestInfo, Request,
                                                      NextTimeout, Priority,
                                                      TransId, Source),
                                      State)
                    end,
                    AspectsRequestAfterF = fun(AspectsAfter, NewTimeout,
                                               Result, S) ->
                        aspects_request_after(AspectsAfter, Type,
                                              Name, Pattern,
                                              RequestInfo, Request,
                                              NewTimeout, Priority,
                                              TransId, Source,
                                              Result, S,
                                              RequestTimeoutAdjustment)
                    end,
                    {next_state, StateName,
                     State#state{queue_requests = true,
                                 service_state = NewServiceState,
                                 aspects_request_after_f = AspectsRequestAfterF,
                                 options = NewConfigOptions}};
                {stop, Reason, NewServiceState} ->
                    {stop, Reason,
                     State#state{service_state = NewServiceState,
                                 options = NewConfigOptions}}
            catch
                ErrorType:Error ->
                    Stack = erlang:get_stacktrace(),
                    ?LOG_ERROR("request ~p ~p~n~p", [ErrorType, Error, Stack]),
                    {stop, {ErrorType, {Error, Stack}},
                     State#state{options = NewConfigOptions}}
            end;
        RateRequestOk =:= false ->
            if
                Timeout >= ResponseTimeoutImmediateMax ->
                    if
                        SendType =:= 'cloudi_service_send_async' ->
                            Source ! {'cloudi_service_return_async',
                                      Name, Pattern, <<>>, <<>>,
                                      Timeout, TransId, Source};
                        SendType =:= 'cloudi_service_send_sync' ->
                            Source ! {'cloudi_service_return_sync',
                                      Name, Pattern, <<>>, <<>>,
                                      Timeout, TransId, Source}
                    end;
                true ->
                    ok
            end,
            {next_state, StateName,
             State#state{options = ConfigOptions#config_service_options{
                             rate_request_max = NewRateRequest}}}
    end;

handle_info({SendType, Name, Pattern, _, _,
             Timeout, Priority, TransId, Source} = T, StateName,
            #state{queue_requests = true,
                   queued = Queue,
                   queued_size = QueuedSize,
                   queued_word_size = WordSize,
                   options = #config_service_options{
                       queue_limit = QueueLimit,
                       queue_size = QueueSize,
                       rate_request_max = RateRequest,
                       response_timeout_immediate_max =
                           ResponseTimeoutImmediateMax} = ConfigOptions
                   } = State)
    when SendType =:= 'cloudi_service_send_async';
         SendType =:= 'cloudi_service_send_sync' ->
    QueueLimitOk = if
        QueueLimit =/= undefined ->
            cloudi_x_pqueue4:len(Queue) < QueueLimit;
        true ->
            true
    end,
    {QueueSizeOk, Size} = if
        QueueSize =/= undefined ->
            QueueElementSize = cloudi_x_erlang_term:byte_size({0, T}, WordSize),
            {(QueuedSize + QueueElementSize) =< QueueSize, QueueElementSize};
        true ->
            {true, 0}
    end,
    {RateRequestOk, NewRateRequest} = if
        RateRequest =/= undefined ->
            cloudi_core_i_rate_based_configuration:
            rate_request_request(RateRequest);
        true ->
            {true, RateRequest}
    end,
    NewState = State#state{
        options = ConfigOptions#config_service_options{
            rate_request_max = NewRateRequest}},
    if
        QueueLimitOk, QueueSizeOk, RateRequestOk ->
            {next_state, StateName,
             recv_timeout_start(Timeout, Priority, TransId,
                                Size, T, NewState)};
        true ->
            if
                Timeout >= ResponseTimeoutImmediateMax ->
                    if
                        SendType =:= 'cloudi_service_send_async' ->
                            Source ! {'cloudi_service_return_async',
                                      Name, Pattern, <<>>, <<>>,
                                      Timeout, TransId, Source};
                        SendType =:= 'cloudi_service_send_sync' ->
                            Source ! {'cloudi_service_return_sync',
                                      Name, Pattern, <<>>, <<>>,
                                      Timeout, TransId, Source}
                    end;
                true ->
                    ok
            end,
            {next_state, StateName, NewState}
    end;

handle_info({'cloudi_service_recv_timeout', Priority, TransId, Size}, StateName,
            #state{recv_timeouts = RecvTimeouts,
                   queue_requests = QueueRequests,
                   queued = Queue,
                   queued_size = QueuedSize} = State) ->
    {NewQueue, NewQueuedSize} = if
        QueueRequests =:= true ->
            F = fun({_, {_, _, _, _, _, _, _, Id, _}}) -> Id == TransId end,
            {Removed,
             NextQueue} = cloudi_x_pqueue4:remove_unique(F, Priority, Queue),
            NextQueuedSize = if
                Removed =:= true ->
                    QueuedSize - Size;
                Removed =:= false ->
                    % false if a timer message was sent while cancelling
                    QueuedSize
            end,
            {NextQueue, NextQueuedSize};
        true ->
            {Queue, QueuedSize}
    end,
    {next_state, StateName,
     State#state{recv_timeouts = ?MAP_ERASE(TransId, RecvTimeouts),
                 queued = NewQueue,
                 queued_size = NewQueuedSize}};

handle_info({'cloudi_service_return_async', _Name, _Pattern,
             ResponseInfo, Response, OldTimeout, TransId, Source}, StateName,
            #state{dispatcher = Dispatcher,
                   send_timeouts = SendTimeouts,
                   options = #config_service_options{
                       request_timeout_immediate_max =
                           RequestTimeoutImmediateMax,
                       response_timeout_adjustment =
                           ResponseTimeoutAdjustment}} = State) ->
    true = Source =:= Dispatcher,
    case ?MAP_FIND(TransId, SendTimeouts) of
        error ->
            % send_async timeout already occurred
            {next_state, StateName, State};
        {ok, {passive, Pid, Tref}}
            when ResponseInfo == <<>>, Response == <<>> ->
            if
                ResponseTimeoutAdjustment;
                OldTimeout >= RequestTimeoutImmediateMax ->
                    cancel_timer_async(Tref);
                true ->
                    % avoid cancel_timer/1 latency
                    ok
            end,
            {next_state, StateName,
             send_timeout_end(TransId, Pid, State)};
        {ok, {passive, Pid, Tref}} ->
            Timeout = if
                ResponseTimeoutAdjustment;
                OldTimeout >= RequestTimeoutImmediateMax ->
                    case erlang:cancel_timer(Tref) of
                        false ->
                            0;
                        V ->
                            V
                    end;
                true ->
                    % avoid cancel_timer/1 latency
                    OldTimeout
            end,
            NewState = if
                is_binary(ResponseInfo) =:= false;
                is_binary(Response) =:= false ->
                    State;
                true ->
                    async_response_timeout_start(ResponseInfo, Response,
                                                 Timeout, TransId, State)
            end,
            {next_state, StateName,
             send_timeout_end(TransId, Pid, NewState)}
    end;

handle_info({'cloudi_service_return_sync', _Name, _Pattern,
             ResponseInfo, Response, OldTimeout, TransId, Source}, StateName,
            #state{dispatcher = Dispatcher,
                   send_timeouts = SendTimeouts,
                   options = #config_service_options{
                       request_timeout_immediate_max =
                           RequestTimeoutImmediateMax,
                       response_timeout_adjustment =
                           ResponseTimeoutAdjustment}} = State) ->
    true = Source =:= Dispatcher,
    case ?MAP_FIND(TransId, SendTimeouts) of
        error ->
            % send_sync timeout already occurred
            {next_state, StateName, State};
        {ok, {_, Pid, Tref}} ->
            if
                ResponseTimeoutAdjustment;
                OldTimeout >= RequestTimeoutImmediateMax ->
                    cancel_timer_async(Tref);
                true ->
                    % avoid cancel_timer/1 latency
                    ok
            end,
            if
                is_binary(ResponseInfo) =:= false;
                is_binary(Response) =:= false ->
                    ok = send('return_sync_out'(timeout, TransId),
                              State);
                true ->
                    ok = send('return_sync_out'(ResponseInfo, Response,
                                                TransId),
                              State)
            end,
            {next_state, StateName,
             send_timeout_end(TransId, Pid, State)}
    end;

handle_info({'cloudi_service_send_async_timeout', TransId}, StateName,
            #state{send_timeouts = SendTimeouts} = State) ->
    case ?MAP_FIND(TransId, SendTimeouts) of
        error ->
            {next_state, StateName, State};
        {ok, {_, Pid, _}} ->
            {next_state, StateName,
             send_timeout_end(TransId, Pid, State)}
    end;

handle_info({'cloudi_service_send_sync_timeout', TransId}, StateName,
            #state{send_timeouts = SendTimeouts} = State) ->
    case ?MAP_FIND(TransId, SendTimeouts) of
        error ->
            {next_state, StateName, State};
        {ok, {_, Pid, _}} ->
            ok = send('return_sync_out'(timeout, TransId), State),
            {next_state, StateName,
             send_timeout_end(TransId, Pid, State)}
    end;

handle_info({'cloudi_service_recv_async_timeout', TransId}, StateName,
            #state{async_responses = AsyncResponses} = State) ->
    {next_state, StateName,
     State#state{async_responses = ?MAP_ERASE(TransId, AsyncResponses)}};

handle_info({udp, Socket, _, IncomingPort, _} = UDP, StateName,
            #state{protocol = udp,
                   incoming_port = undefined,
                   socket = Socket} = State) ->
    handle_info(UDP, StateName, State#state{incoming_port = IncomingPort});

handle_info({udp, Socket, _, IncomingPort, Data}, StateName,
            #state{protocol = udp,
                   incoming_port = IncomingPort,
                   socket = Socket} = State) ->
    inet:setopts(Socket, [{active, once}]),
    try erlang:binary_to_term(Data, [safe]) of
        Term ->
            ?MODULE:StateName(Term, State)
    catch
        error:badarg ->
            ?LOG_ERROR("Protocol Error ~p", [Data]),
            {stop, {error, protocol}, State}
    end;

handle_info({tcp, Socket, Data}, StateName,
            #state{protocol = Protocol,
                   socket = Socket} = State)
    when Protocol =:= tcp; Protocol =:= local ->
    inet:setopts(Socket, [{active, once}]),
    try erlang:binary_to_term(Data, [safe]) of
        Term ->
            ?MODULE:StateName(Term, State)
    catch
        error:badarg ->
            ?LOG_ERROR("Protocol Error ~p", [Data]),
            {stop, {error, protocol}, State}
    end;

handle_info({cloudi_cpg_data, Groups}, StateName,
            #state{dispatcher = Dispatcher,
                   dest_refresh = DestRefresh,
                   options = #config_service_options{
                       dest_refresh_delay = Delay,
                       scope = Scope}} = State) ->
    destination_refresh(DestRefresh, Dispatcher, Delay, Scope),
    {next_state, StateName, State#state{cpg_data = Groups}};

handle_info(keepalive_udp, StateName,
            #state{dispatcher = Dispatcher,
                   keepalive = undefined,
                   socket = Socket} = State) ->
    Dispatcher ! {udp_closed, Socket},
    {next_state, StateName, State};

handle_info(keepalive_udp, StateName,
            #state{dispatcher = Dispatcher,
                   keepalive = received} = State) ->
    ok = send('keepalive_out'(), State),
    erlang:send_after(?KEEPALIVE_UDP, Dispatcher, keepalive_udp),
    {next_state, StateName, State#state{keepalive = undefined}};

handle_info({udp_closed, Socket}, _,
            #state{protocol = udp,
                   socket = Socket} = State) ->
    {stop, socket_closed, State};

handle_info({tcp_closed, Socket}, _,
            #state{protocol = Protocol,
                   socket = Socket} = State)
    when Protocol =:= tcp; Protocol =:= local ->
    {stop, socket_closed, State};

handle_info({tcp_error, Socket, Reason}, _,
            #state{protocol = Protocol,
                   socket = Socket} = State)
    when Protocol =:= tcp; Protocol =:= local ->
    {stop, Reason, State};

handle_info({inet_async, _, _, {ok, _}} = Accept, StateName, State) ->
    StateSocket = socket_data_from_state(State),
    NewStateSocket = socket_accept(Accept, StateSocket),
    {next_state, StateName,
     socket_data_to_state(NewStateSocket, State)};

handle_info({inet_async, Listener, Acceptor, Error}, StateName,
            #state{protocol = Protocol,
                   listener = Listener,
                   acceptor = Acceptor} = State)
    when Protocol =:= tcp; Protocol =:= local ->
    {stop, {StateName, inet_async, Error}, State};

handle_info(initialize, StateName, State) ->
    true = StateName /= 'HANDLE',
    NewState = State#state{initialize = true},
    if
        StateName =:= 'INIT_WAIT' ->
            'INIT'('init', NewState);
        true ->
            {next_state, StateName, NewState}
    end;

handle_info('cloudi_service_init_timeout', _, State) ->
    {stop, timeout, State};

handle_info('cloudi_count_process_dynamic_rate', StateName,
            #state{dispatcher = Dispatcher,
                   options = #config_service_options{
                       count_process_dynamic =
                           CountProcessDynamic} = ConfigOptions} = State) ->
    NewCountProcessDynamic = cloudi_core_i_rate_based_configuration:
                             count_process_dynamic_reinit(Dispatcher,
                                                          CountProcessDynamic),
    {next_state, StateName,
     State#state{options = ConfigOptions#config_service_options{
                     count_process_dynamic = NewCountProcessDynamic}}};

handle_info({'cloudi_count_process_dynamic_update', ProcessCount}, StateName,
            #state{timeout_async = TimeoutAsync,
                   timeout_sync = TimeoutSync,
                   options = #config_service_options{
                       priority_default = PriorityDefault,
                       request_timeout_adjustment = RequestTimeoutAdjustment}
                   } = State) ->
    ok = send('reinit_out'(ProcessCount, TimeoutAsync, TimeoutSync,
                           PriorityDefault, RequestTimeoutAdjustment), State),
    {next_state, StateName,
     State#state{process_count = ProcessCount}};

handle_info('cloudi_count_process_dynamic_terminate', StateName,
            #state{dispatcher = Dispatcher,
                   options = #config_service_options{
                       count_process_dynamic = CountProcessDynamic,
                       scope = Scope} = ConfigOptions} = State) ->
    cloudi_x_cpg:leave(Scope, Dispatcher, infinity),
    NewCountProcessDynamic =
        cloudi_core_i_rate_based_configuration:
        count_process_dynamic_terminate_set(Dispatcher, CountProcessDynamic),
    {next_state, StateName,
     State#state{options = ConfigOptions#config_service_options{
                     count_process_dynamic = NewCountProcessDynamic}}};

handle_info('cloudi_count_process_dynamic_terminate_check', StateName,
            #state{dispatcher = Dispatcher,
                   queue_requests = QueueRequests} = State) ->
    if
        QueueRequests =:= false ->
            {stop, {shutdown, cloudi_count_process_dynamic_terminate}, State};
        QueueRequests =:= true ->
            erlang:send_after(?COUNT_PROCESS_DYNAMIC_INTERVAL, Dispatcher,
                              'cloudi_count_process_dynamic_terminate_check'),
            {next_state, StateName, State}
    end;

handle_info('cloudi_count_process_dynamic_terminate_now', _, State) ->
    {stop, {shutdown, cloudi_count_process_dynamic_terminate}, State};

handle_info('cloudi_rate_request_max_rate', StateName,
            #state{options = #config_service_options{
                       rate_request_max =
                           RateRequest} = ConfigOptions} = State) ->
    NewRateRequest = cloudi_core_i_rate_based_configuration:
                     rate_request_reinit(RateRequest),
    {next_state, StateName,
     State#state{options = ConfigOptions#config_service_options{
                     rate_request_max = NewRateRequest}}};

handle_info({'EXIT', _, Reason}, _, State) ->
    {stop, Reason, State};

handle_info({'cloudi_service_update', UpdatePending, _}, StateName,
            #state{dispatcher = Dispatcher,
                   update_plan = undefined} = State)
    when StateName =/= 'HANDLE' ->
    UpdatePending ! {'cloudi_service_update', Dispatcher,
                     {error, invalid_state}},
    {next_state, StateName, State};

handle_info({'cloudi_service_update', UpdatePending, UpdatePlan}, StateName,
            #state{dispatcher = Dispatcher,
                   update_plan = undefined,
                   queue_requests = QueueRequests} = State) ->
    #config_service_update{sync = Sync} = UpdatePlan,
    NewUpdatePlan = if
        Sync =:= true, QueueRequests =:= true ->
            UpdatePlan#config_service_update{update_pending = UpdatePending,
                                             queue_requests = QueueRequests};
        true ->
            UpdatePending ! {'cloudi_service_update', Dispatcher},
            UpdatePlan#config_service_update{queue_requests = QueueRequests}
    end,
    {next_state, StateName,
     State#state{update_plan = NewUpdatePlan,
                 queue_requests = true}};

handle_info({'cloudi_service_update_now', UpdateNow, UpdateStart}, StateName,
            #state{update_plan = UpdatePlan} = State) ->
    #config_service_update{queue_requests = QueueRequests} = UpdatePlan,
    NewUpdatePlan = UpdatePlan#config_service_update{
                        update_now = UpdateNow,
                        update_start = UpdateStart},
    NewState = State#state{update_plan = NewUpdatePlan},
    if
        QueueRequests =:= true ->
            {next_state, StateName, NewState};
        QueueRequests =:= false ->
            {next_state, StateName, process_update(NewState)}
    end;

handle_info({'cloudi_service_update_state', CommandLine}, StateName, State) ->
    % state updates when #config_service_update{spawn_os_process = true}
    erlang:put(?SERVICE_FILE_PDICT_KEY, hd(CommandLine)),
    {next_state, StateName, State};

handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, StateName, State) ->
    {_, NewState} = send_timeout_dead(Pid, State),
    {next_state, StateName, NewState};

handle_info({ReplyRef, _}, StateName, State) when is_reference(ReplyRef) ->
    % gen_server:call/3 had a timeout exception that was caught but the
    % reply arrived later and must be discarded
    {next_state, StateName, State};

handle_info(Request, StateName, State) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {next_state, StateName, State}.

terminate(Reason, _,
          #state{dispatcher = Dispatcher,
                 timeout_term = TimeoutTerm,
                 service_state = ServiceState,
                 options = #config_service_options{
                     aspects_terminate_before = Aspects}} = State) ->
    _ = cloudi_core_i_services_monitor:terminate_kill(Dispatcher, Reason),
    {ok, _} = aspects_terminate(Aspects, Reason, TimeoutTerm, ServiceState),
    ok = socket_close(Reason, socket_data_from_state(State)),
    ok.

code_change(_, StateName, State, _) ->
    {ok, StateName, State}.

-ifdef(VERBOSE_STATE).
format_status(_Opt, [_PDict, State]) ->
    [{data, [{"StateData", State}]}].
-else.
format_status(_Opt,
              [_PDict,
               #state{send_timeouts = SendTimeouts,
                      send_timeout_monitors = SendTimeoutMonitors,
                      recv_timeouts = RecvTimeouts,
                      async_responses = AsyncResponses,
                      queued = Queue,
                      cpg_data = Groups,
                      dest_deny = DestDeny,
                      dest_allow = DestAllow,
                      options = ConfigOptions} = State]) ->
    NewGroups = case Groups of
        undefined ->
            undefined;
        {GroupsDictI, GroupsData} ->
            GroupsDictI:to_list(GroupsData)
    end,
    NewDestDeny = if
        DestDeny =:= undefined ->
            undefined;
        true ->
            cloudi_x_trie:to_list(DestDeny)
    end,
    NewDestAllow = if
        DestAllow =:= undefined ->
            undefined;
        true ->
            cloudi_x_trie:to_list(DestAllow)
    end,
    NewConfigOptions = cloudi_core_i_configuration:
                       services_format_options_internal(ConfigOptions),
    [{data,
      [{"StateData",
        State#state{send_timeouts = ?MAP_TO_LIST(SendTimeouts),
                    send_timeout_monitors = ?MAP_TO_LIST(SendTimeoutMonitors),
                    recv_timeouts = ?MAP_TO_LIST(RecvTimeouts),
                    async_responses = ?MAP_TO_LIST(AsyncResponses),
                    queued = cloudi_x_pqueue4:to_plist(Queue),
                    cpg_data = NewGroups,
                    dest_deny = NewDestDeny,
                    dest_allow = NewDestAllow,
                    options = NewConfigOptions}}]}].
-endif.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

filter_stream(Output0) ->
    % just consume the last newline character, if one exists
    case lists:reverse(Output0) of
        [10 | Output1] ->
            lists:reverse(Output1);
        _ ->
            Output0
    end.

os_pid_set(OSPid, State) ->
    % forked process has connected before CloudI API initialization
    % (only the thread_index == 0 Erlang process gets this message,
    %  since the OS process only needs to be killed once, if at all)
    ?LOG_INFO("OS pid ~w connected", [OSPid]),
    State#state{os_pid = OSPid}.

os_pid_kill(#state_socket{os_pid = OSPid,
                          cgroup = CGroup}) ->
    % if the OSPid exists at this point, it is probably stuck.
    % without this kill, the process could just stay around, while
    % being unresponsive and without its Erlang socket pids.
    if
        OSPid =:= undefined ->
            ok;
        true ->
            _ = os:cmd(cloudi_string:format("kill -9 ~w", [OSPid])),
            _ = cloudi_core_i_os_process:cgroup_unset(OSPid, CGroup),
            ok
    end,
    ok.

os_init(#state{initialize = true,
               process_index = ProcessIndex,
               process_count = ProcessCount,
               prefix = Prefix,
               timeout_init = TimeoutInit,
               timeout_async = TimeoutAsync,
               timeout_sync = TimeoutSync,
               timeout_term = TimeoutTerm,
               options = #config_service_options{
                   priority_default = PriorityDefault,
                   request_timeout_adjustment = RequestTimeoutAdjustment,
                   count_process_dynamic = CountProcessDynamic}} = State) ->
    CountProcessDynamicFormat =
        cloudi_core_i_rate_based_configuration:
        count_process_dynamic_format(CountProcessDynamic),
    {ProcessCountMax, ProcessCountMin} = if
        CountProcessDynamicFormat =:= false ->
            {ProcessCount, ProcessCount};
        true ->
            {_, Max} = lists:keyfind(count_max, 1, CountProcessDynamicFormat),
            {_, Min} = lists:keyfind(count_min, 1, CountProcessDynamicFormat),
            {Max, Min}
    end,
    % first message within the CloudI API received during
    % the object construction or init API function
    ok = send('init_out'(ProcessIndex, ProcessCount,
                         ProcessCountMax, ProcessCountMin, Prefix,
                         TimeoutInit, TimeoutAsync, TimeoutSync, TimeoutTerm,
                         PriorityDefault, RequestTimeoutAdjustment),
              State),
    ok.

handle_send_async(Name, RequestInfo, Request, Timeout, Priority, StateName,
                  #state{dispatcher = Dispatcher,
                         uuid_generator = UUID,
                         dest_refresh = DestRefresh,
                         cpg_data = Groups,
                         options = #config_service_options{
                             request_name_lookup = RequestNameLookup,
                             scope = Scope}} = State) ->
    case destination_get(DestRefresh, Scope, Name, Dispatcher,
                         Groups, Timeout) of
        {error, timeout} ->
            ok = send('return_async_out'(), State),
            {next_state, StateName, State};
        {error, _} when RequestNameLookup =:= async ->
            ok = send('return_async_out'(), State),
            {next_state, StateName, State};
        {error, _} when Timeout >= ?SEND_ASYNC_INTERVAL ->
            erlang:send_after(?SEND_ASYNC_INTERVAL, Dispatcher,
                              {'cloudi_service_send_async_retry',
                               Name, RequestInfo, Request,
                               Timeout - ?SEND_ASYNC_INTERVAL, Priority}),
            {next_state, StateName, State};
        {error, _} ->
            ok = send('return_async_out'(), State),
            {next_state, StateName, State};
        {ok, Pattern, Pid} ->
            {TransId, NewUUID} = cloudi_x_uuid:get_v1(UUID),
            Pid ! {'cloudi_service_send_async',
                   Name, Pattern, RequestInfo, Request,
                   Timeout, Priority, TransId, Dispatcher},
            ok = send('return_async_out'(TransId), State),
            {next_state, StateName,
             send_async_timeout_start(Timeout, TransId, Pid,
                                      State#state{uuid_generator = NewUUID})}
    end.

handle_send_sync(Name, RequestInfo, Request, Timeout, Priority, StateName,
                 #state{dispatcher = Dispatcher,
                        uuid_generator = UUID,
                        dest_refresh = DestRefresh,
                        cpg_data = Groups,
                        options = #config_service_options{
                            request_name_lookup = RequestNameLookup,
                            scope = Scope}} = State) ->
    case destination_get(DestRefresh, Scope, Name, Dispatcher,
                         Groups, Timeout) of
        {error, timeout} ->
            ok = send('return_sync_out'(), State),
            {next_state, StateName, State};
        {error, _} when RequestNameLookup =:= async ->
            ok = send('return_sync_out'(), State),
            {next_state, StateName, State};
        {error, _} when Timeout >= ?SEND_SYNC_INTERVAL ->
            erlang:send_after(?SEND_SYNC_INTERVAL, Dispatcher,
                              {'cloudi_service_send_sync_retry',
                               Name, RequestInfo, Request,
                               Timeout - ?SEND_SYNC_INTERVAL, Priority}),
            {next_state, StateName, State};
        {error, _} ->
            ok = send('return_sync_out'(), State),
            {next_state, StateName, State};
        {ok, Pattern, Pid} ->
            {TransId, NewUUID} = cloudi_x_uuid:get_v1(UUID),
            Pid ! {'cloudi_service_send_sync',
                   Name, Pattern, RequestInfo, Request,
                   Timeout, Priority, TransId, Dispatcher},
            {next_state, StateName,
             send_sync_timeout_start(Timeout, TransId, Pid, undefined,
                                     State#state{uuid_generator = NewUUID})}
    end.

handle_mcast_async_pids(_Name, _Pattern, _RequestInfo, _Request,
                        _Timeout, _Priority,
                        TransIdList, [],
                        State) ->
    ok = send('returns_async_out'(lists:reverse(TransIdList)), State),
    State;
handle_mcast_async_pids(Name, Pattern, RequestInfo, Request,
                        Timeout, Priority,
                        TransIdList, [Pid | PidList],
                        #state{dispatcher = Dispatcher,
                               uuid_generator = UUID} = State) ->
    {TransId, NewUUID} = cloudi_x_uuid:get_v1(UUID),
    Pid ! {'cloudi_service_send_async',
           Name, Pattern, RequestInfo, Request,
           Timeout, Priority, TransId, Dispatcher},
    handle_mcast_async_pids(Name, Pattern, RequestInfo, Request,
                            Timeout, Priority,
                            [TransId | TransIdList], PidList,
                            send_async_timeout_start(Timeout,
                                                     TransId,
                                                     Pid,
                                                     State#state{
                                                         uuid_generator =
                                                             NewUUID})).

handle_mcast_async(Name, RequestInfo, Request, Timeout, Priority, StateName,
                   #state{dispatcher = Dispatcher,
                          dest_refresh = DestRefresh,
                          cpg_data = Groups,
                          options = #config_service_options{
                              request_name_lookup = RequestNameLookup,
                              scope = Scope}} = State) ->
    case destination_all(DestRefresh, Scope, Name, Dispatcher,
                         Groups, Timeout) of
        {error, timeout} ->
            ok = send('returns_async_out'(), State),
            {next_state, StateName, State};
        {error, _} when RequestNameLookup =:= async ->
            ok = send('returns_async_out'(), State),
            {next_state, StateName, State};
        {error, _} when Timeout >= ?MCAST_ASYNC_INTERVAL ->
            erlang:send_after(?MCAST_ASYNC_INTERVAL, Dispatcher,
                              {'cloudi_service_mcast_async_retry',
                               Name, RequestInfo, Request,
                               Timeout - ?MCAST_ASYNC_INTERVAL, Priority}),
            {next_state, StateName, State};
        {error, _} ->
            ok = send('returns_async_out'(), State),
            {next_state, StateName, State};
        {ok, Pattern, PidList} ->
            {next_state, StateName,
             handle_mcast_async_pids(Name, Pattern, RequestInfo, Request,
                                     Timeout, Priority,
                                     [], PidList, State)}
    end.

'init_out'(ProcessIndex, ProcessCount,
           ProcessCountMax, ProcessCountMin, [PrefixC | _] = Prefix,
           TimeoutInit, TimeoutAsync, TimeoutSync, TimeoutTerm,
           PriorityDefault, RequestTimeoutAdjustment)
    when is_integer(ProcessIndex), is_integer(ProcessCount),
         is_integer(ProcessCountMax), is_integer(ProcessCountMin),
         is_integer(PrefixC), is_integer(TimeoutInit),
         is_integer(TimeoutAsync), is_integer(TimeoutSync),
         is_integer(TimeoutTerm), is_integer(PriorityDefault),
         PriorityDefault >= ?PRIORITY_HIGH, PriorityDefault =< ?PRIORITY_LOW,
         is_boolean(RequestTimeoutAdjustment) ->
    true = ProcessCount < 4294967296,
    true = ProcessCountMax < 4294967296,
    true = ProcessCountMin < 4294967296,
    PrefixBin = erlang:list_to_binary(Prefix),
    PrefixSize = erlang:byte_size(PrefixBin) + 1,
    true = PrefixSize < 4294967296,
    RequestTimeoutAdjustmentInt = if
        RequestTimeoutAdjustment ->
            1;
        true ->
            0
    end,
    <<?MESSAGE_INIT:32/unsigned-integer-native,
      ProcessIndex:32/unsigned-integer-native,
      ProcessCount:32/unsigned-integer-native,
      ProcessCountMax:32/unsigned-integer-native,
      ProcessCountMin:32/unsigned-integer-native,
      PrefixSize:32/unsigned-integer-native,
      PrefixBin/binary, 0:8,
      TimeoutInit:32/unsigned-integer-native,
      TimeoutAsync:32/unsigned-integer-native,
      TimeoutSync:32/unsigned-integer-native,
      TimeoutTerm:32/unsigned-integer-native,
      PriorityDefault:8/signed-integer-native,
      RequestTimeoutAdjustmentInt:8/unsigned-integer-native>>.

'reinit_out'(ProcessCount, TimeoutAsync, TimeoutSync,
             PriorityDefault, RequestTimeoutAdjustment)
    when is_integer(ProcessCount), is_integer(TimeoutAsync),
         is_integer(TimeoutSync), is_integer(PriorityDefault),
         PriorityDefault >= ?PRIORITY_HIGH, PriorityDefault =< ?PRIORITY_LOW,
         is_boolean(RequestTimeoutAdjustment) ->
    true = ProcessCount < 4294967296,
    RequestTimeoutAdjustmentInt = if
        RequestTimeoutAdjustment ->
            1;
        true ->
            0
    end,
    <<?MESSAGE_REINIT:32/unsigned-integer-native,
      ProcessCount:32/unsigned-integer-native,
      TimeoutAsync:32/unsigned-integer-native,
      TimeoutSync:32/unsigned-integer-native,
      PriorityDefault:8/signed-integer-native,
      RequestTimeoutAdjustmentInt:8/unsigned-integer-native>>.

'terminate_out'() ->
    <<?MESSAGE_TERM:32/unsigned-integer-native>>.

'keepalive_out'() ->
    <<?MESSAGE_KEEPALIVE:32/unsigned-integer-native>>.

'send_async_out'([NameC | _] = Name, [PatternC | _] = Pattern,
                 RequestInfo, Request, Timeout, Priority, TransId, Source)
    when is_integer(NameC), is_integer(PatternC),
         is_binary(RequestInfo), is_binary(Request),
         is_integer(Timeout), is_integer(Priority),
         is_binary(TransId), is_pid(Source) ->
    NameBin = erlang:list_to_binary(Name),
    NameSize = erlang:byte_size(NameBin) + 1,
    true = NameSize < 4294967296,
    PatternBin = erlang:list_to_binary(Pattern),
    PatternSize = erlang:byte_size(PatternBin) + 1,
    true = PatternSize < 4294967296,
    RequestInfoSize = erlang:byte_size(RequestInfo),
    true = RequestInfoSize < 4294967296,
    RequestSize = erlang:byte_size(Request),
    true = RequestSize < 4294967296,
    SourceBin = erlang:term_to_binary(Source),
    SourceSize = erlang:byte_size(SourceBin),
    true = SourceSize < 4294967296,
    <<?MESSAGE_SEND_ASYNC:32/unsigned-integer-native,
      NameSize:32/unsigned-integer-native,
      NameBin/binary, 0:8,
      PatternSize:32/unsigned-integer-native,
      PatternBin/binary, 0:8,
      RequestInfoSize:32/unsigned-integer-native,
      RequestInfo/binary, 0:8,
      RequestSize:32/unsigned-integer-native,
      Request/binary, 0:8,
      Timeout:32/unsigned-integer-native,
      Priority:8/signed-integer-native,
      TransId/binary,             % 128 bits
      SourceSize:32/unsigned-integer-native,
      SourceBin/binary>>.

'send_sync_out'([NameC | _] = Name, [PatternC | _] = Pattern,
                RequestInfo, Request, Timeout, Priority, TransId, Source)
    when is_integer(NameC), is_integer(PatternC),
         is_binary(RequestInfo), is_binary(Request),
         is_integer(Timeout), is_integer(Priority),
         is_binary(TransId), is_pid(Source) ->
    NameBin = erlang:list_to_binary(Name),
    NameSize = erlang:byte_size(NameBin) + 1,
    true = NameSize < 4294967296,
    PatternBin = erlang:list_to_binary(Pattern),
    PatternSize = erlang:byte_size(PatternBin) + 1,
    true = PatternSize < 4294967296,
    RequestInfoSize = erlang:byte_size(RequestInfo),
    true = RequestInfoSize < 4294967296,
    RequestSize = erlang:byte_size(Request),
    true = RequestSize < 4294967296,
    SourceBin = erlang:term_to_binary(Source),
    SourceSize = erlang:byte_size(SourceBin),
    true = SourceSize < 4294967296,
    <<?MESSAGE_SEND_SYNC:32/unsigned-integer-native,
      NameSize:32/unsigned-integer-native,
      NameBin/binary, 0:8,
      PatternSize:32/unsigned-integer-native,
      PatternBin/binary, 0:8,
      RequestInfoSize:32/unsigned-integer-native,
      RequestInfo/binary, 0:8,
      RequestSize:32/unsigned-integer-native,
      Request/binary, 0:8,
      Timeout:32/unsigned-integer-native,
      Priority:8/signed-integer-native,
      TransId/binary,             % 128 bits
      SourceSize:32/unsigned-integer-native,
      SourceBin/binary>>.

'return_async_out'() ->
    <<?MESSAGE_RETURN_ASYNC:32/unsigned-integer-native,
      0:128>>.                    % 128 bits

'return_async_out'(TransId)
    when is_binary(TransId) ->
    <<?MESSAGE_RETURN_ASYNC:32/unsigned-integer-native,
      TransId/binary>>.           % 128 bits

'return_sync_out'() ->
    <<?MESSAGE_RETURN_SYNC:32/unsigned-integer-native,
      0:32, 0:8,
      0:32, 0:8,
      0:128>>.                    % 128 bits

'return_sync_out'(timeout, TransId)
    when is_binary(TransId) ->
    <<?MESSAGE_RETURN_SYNC:32/unsigned-integer-native,
      0:32, 0:8,
      0:32, 0:8,
      TransId/binary>>.           % 128 bits

'return_sync_out'(ResponseInfo, Response, TransId)
    when is_binary(ResponseInfo), is_binary(Response), is_binary(TransId) ->
    ResponseInfoSize = erlang:byte_size(ResponseInfo),
    true = ResponseInfoSize < 4294967296,
    ResponseSize = erlang:byte_size(Response),
    true = ResponseSize < 4294967296,
    <<?MESSAGE_RETURN_SYNC:32/unsigned-integer-native,
      ResponseInfoSize:32/unsigned-integer-native,
      ResponseInfo/binary, 0:8,
      ResponseSize:32/unsigned-integer-native,
      Response/binary, 0:8,
      TransId/binary>>.           % 128 bits

'returns_async_out'() ->
    <<?MESSAGE_RETURNS_ASYNC:32/unsigned-integer-native,
      0:32>>.

'returns_async_out'(TransIdList)
    when is_list(TransIdList) ->
    TransIdListBin = erlang:list_to_binary(TransIdList),
    TransIdListCount = erlang:length(TransIdList),
    true = TransIdListCount < 4294967296,
    <<?MESSAGE_RETURNS_ASYNC:32/unsigned-integer-native,
      TransIdListCount:32/unsigned-integer-native,
      TransIdListBin/binary>>.    % 128 bits * count

'recv_async_out'(timeout, TransId)
    when is_binary(TransId) ->
    <<?MESSAGE_RECV_ASYNC:32/unsigned-integer-native,
      0:32, 0:8,
      0:32, 0:8,
      TransId/binary>>.           % 128 bits

'recv_async_out'(ResponseInfo, Response, TransId)
    when is_binary(ResponseInfo), is_binary(Response), is_binary(TransId) ->
    ResponseInfoSize = erlang:byte_size(ResponseInfo),
    true = ResponseInfoSize < 4294967296,
    ResponseSize = erlang:byte_size(Response),
    true = ResponseSize < 4294967296,
    <<?MESSAGE_RECV_ASYNC:32/unsigned-integer-native,
      ResponseInfoSize:32/unsigned-integer-native,
      ResponseInfo/binary, 0:8,
      ResponseSize:32/unsigned-integer-native,
      Response/binary, 0:8,
      TransId/binary>>.           % 128 bits

'subscribe_count_out'(Count)
    when is_integer(Count), Count >= 0, Count < 4294967296 ->
    <<?MESSAGE_SUBSCRIBE_COUNT:32/unsigned-integer-native,
      Count:32/unsigned-integer-native>>.

recv_timeout_start(Timeout, Priority, TransId, Size, T,
                   #state{dispatcher = Dispatcher,
                          recv_timeouts = RecvTimeouts,
                          queued = Queue,
                          queued_size = QueuedSize} = State)
    when is_integer(Timeout), is_integer(Priority), is_binary(TransId) ->
    State#state{
        recv_timeouts = ?MAP_STORE(TransId,
            erlang:send_after(Timeout, Dispatcher,
                {'cloudi_service_recv_timeout', Priority, TransId, Size}),
            RecvTimeouts),
        queued = cloudi_x_pqueue4:in({Size, T}, Priority, Queue),
        queued_size = QueuedSize + Size}.

process_queue(#state{dispatcher = Dispatcher,
                     recv_timeouts = RecvTimeouts,
                     queue_requests = true,
                     queued = Queue,
                     queued_size = QueuedSize,
                     service_state = ServiceState,
                     options = ConfigOptions} = State) ->
    case cloudi_x_pqueue4:out(Queue) of
        {empty, NewQueue} ->
            State#state{queue_requests = false,
                        queued = NewQueue};
        {{value,
          {Size,
           {'cloudi_service_send_async',
            Name, Pattern, RequestInfo, Request,
            OldTimeout, Priority, TransId, Source}}}, NewQueue} ->
            Type = 'send_async',
            NewConfigOptions = check_incoming(true, ConfigOptions),
            #config_service_options{
                request_timeout_adjustment =
                    RequestTimeoutAdjustment,
                aspects_request_before =
                    AspectsBefore} = NewConfigOptions,
            try aspects_request_before(AspectsBefore, Type,
                                       Name, Pattern, RequestInfo, Request,
                                       OldTimeout, Priority, TransId, Source,
                                       ServiceState, false) of
                {ok, _, NewServiceState} ->
                    RecvTimer = ?MAP_FETCH(TransId, RecvTimeouts),
                    Timeout = case erlang:cancel_timer(RecvTimer) of
                        false ->
                            0;
                        V ->    
                            V       
                    end,
                    ok = send('send_async_out'(Name, Pattern,
                                               RequestInfo, Request,
                                               Timeout, Priority, TransId,
                                               Source),
                              State),
                    AspectsRequestAfterF = fun(AspectsAfter, NewTimeout,
                                               Result, S) ->
                        aspects_request_after(AspectsAfter, Type,
                                              Name, Pattern,
                                              RequestInfo, Request,
                                              NewTimeout, Priority,
                                              TransId, Source,
                                              Result, S,
                                              RequestTimeoutAdjustment)
                    end,
                    State#state{recv_timeouts = ?MAP_ERASE(TransId,
                                                           RecvTimeouts),
                                queued = NewQueue,
                                queued_size = QueuedSize - Size,
                                service_state = NewServiceState,
                                aspects_request_after_f = AspectsRequestAfterF,
                                options = NewConfigOptions};
                {stop, Reason, NewServiceState} ->
                    Dispatcher ! {'EXIT', Dispatcher, Reason},
                    State#state{service_state = NewServiceState,
                                options = NewConfigOptions}
            catch
                ErrorType:Error ->
                    Stack = erlang:get_stacktrace(),
                    ?LOG_ERROR("request ~p ~p~n~p", [ErrorType, Error, Stack]),
                    Reason = {ErrorType, {Error, Stack}},
                    Dispatcher ! {'EXIT', Dispatcher, Reason},
                    State#state{options = NewConfigOptions}
            end;
        {{value,
          {Size,
           {'cloudi_service_send_sync',
            Name, Pattern, RequestInfo, Request,
            OldTimeout, Priority, TransId, Source}}}, NewQueue} ->
            Type = 'send_sync',
            NewConfigOptions = check_incoming(true, ConfigOptions),
            #config_service_options{
                request_timeout_adjustment =
                    RequestTimeoutAdjustment,
                aspects_request_before =
                    AspectsBefore} = NewConfigOptions,
            try aspects_request_before(AspectsBefore, Type,
                                       Name, Pattern, RequestInfo, Request,
                                       OldTimeout, Priority, TransId, Source,
                                       ServiceState, false) of
                {ok, _, NewServiceState} ->
                    RecvTimer = ?MAP_FETCH(TransId, RecvTimeouts),
                    Timeout = case erlang:cancel_timer(RecvTimer) of
                        false ->
                            0;
                        V ->    
                            V       
                    end,
                    ok = send('send_sync_out'(Name, Pattern,
                                              RequestInfo, Request,
                                              Timeout, Priority, TransId,
                                              Source),
                              State),
                    AspectsRequestAfterF = fun(AspectsAfter, NewTimeout,
                                               Result, S) ->
                        aspects_request_after(AspectsAfter, Type,
                                              Name, Pattern,
                                              RequestInfo, Request,
                                              NewTimeout, Priority,
                                              TransId, Source,
                                              Result, S,
                                              RequestTimeoutAdjustment)
                    end,
                    State#state{recv_timeouts = ?MAP_ERASE(TransId,
                                                           RecvTimeouts),
                                queued = NewQueue,
                                queued_size = QueuedSize - Size,
                                service_state = NewServiceState,
                                aspects_request_after_f = AspectsRequestAfterF,
                                options = NewConfigOptions};
                {stop, Reason, NewServiceState} ->
                    Dispatcher ! {'EXIT', Dispatcher, Reason},
                    State#state{service_state = NewServiceState,
                                options = NewConfigOptions}
            catch
                ErrorType:Error ->
                    Stack = erlang:get_stacktrace(),
                    ?LOG_ERROR("request ~p ~p~n~p", [ErrorType, Error, Stack]),
                    Reason = {ErrorType, {Error, Stack}},
                    Dispatcher ! {'EXIT', Dispatcher, Reason},
                    State#state{options = NewConfigOptions}
            end
    end.

process_update(#state{dispatcher = Dispatcher,
                      update_plan = UpdatePlan} = State) ->
    #config_service_update{update_now = UpdateNow,
                           spawn_os_process = SpawnOsProcess,
                           queue_requests = false} = UpdatePlan,
    {NewOsProcess, NewState} = case update(State, UpdatePlan) of
        {ok, undefined, NextState} ->
            false = SpawnOsProcess,
            % re-initialize the old OS process after the update success
            % when a new OS process is not created during the update
            #state{process_count = ProcessCount,
                   timeout_async = TimeoutAsync,
                   timeout_sync = TimeoutSync,
                   options = #config_service_options{
                       priority_default = PriorityDefault,
                       request_timeout_adjustment = RequestTimeoutAdjustment}
                   } = NextState,
            ok = send('reinit_out'(ProcessCount,
                                   TimeoutAsync, TimeoutSync, PriorityDefault,
                                   RequestTimeoutAdjustment), NextState),
            UpdateNow ! {'cloudi_service_update_now', Dispatcher, ok},
            {false, NextState};
        {ok, {error, _} = Error} ->
            UpdateNow ! {'cloudi_service_update_now', Dispatcher, Error},
            {false, State};
        {ok, #state_socket{port = Port} = StateSocket, NextState} ->
            true = SpawnOsProcess,
            UpdateNow ! {'cloudi_service_update_now', Dispatcher, {ok, Port}},
            receive
                {'cloudi_service_update_after', ok} ->
                    {true, update_after(StateSocket, NextState)};
                {'cloudi_service_update_after', error} ->
                    ok = socket_close(StateSocket),
                    erlang:exit(update_failed)
            end;
        {error, _} = Error ->
            true = SpawnOsProcess,
            UpdateNow ! {'cloudi_service_update_now', Dispatcher, Error},
            erlang:exit(update_failed)
    end,
    FinalState = NewState#state{update_plan = undefined},
    if
        NewOsProcess =:= true ->
            % wait to receive 'polling' to make sure initialization is complete
            % with the newly created OS process
            FinalState;
        NewOsProcess =:= false ->
            process_queues(FinalState)
    end.

process_queues(#state{dispatcher = Dispatcher,
                      update_plan = UpdatePlan} = State)
    when is_record(UpdatePlan, config_service_update) ->
    #config_service_update{update_pending = UpdatePending,
                           update_now = UpdateNow} = UpdatePlan,
    NewUpdatePlan = if
        is_pid(UpdatePending) ->
            UpdatePending ! {'cloudi_service_update', Dispatcher},
            UpdatePlan#config_service_update{update_pending = undefined,
                                             queue_requests = false};
        UpdatePending =:= undefined ->
            UpdatePlan#config_service_update{queue_requests = false}
    end,
    NewState = State#state{update_plan = NewUpdatePlan},
    if
        is_pid(UpdateNow) ->
            process_update(NewState);
        UpdateNow =:= undefined ->
            NewState
    end;
process_queues(State) ->
    process_queue(State).

send(Data, #state{protocol = Protocol,
                  incoming_port = IncomingPort,
                  socket = Socket}) when is_binary(Data) ->
    socket_send(Socket, IncomingPort, Data, Protocol);
send(Data, #state_socket{protocol = Protocol,
                         incoming_port = IncomingPort,
                         socket = Socket}) when is_binary(Data) ->
    socket_send(Socket, IncomingPort, Data, Protocol).

socket_send(Socket, _, Data, Protocol)
    when Protocol =:= tcp; Protocol =:= local ->
    gen_tcp:send(Socket, Data);
socket_send(Socket, IncomingPort, Data, Protocol)
    when Protocol =:= udp ->
    gen_udp:send(Socket, {127,0,0,1}, IncomingPort, Data).

socket_recv(#state_socket{protocol = udp,
                          socket = Socket,
                          incoming_port = IncomingPort} = StateSocket) ->
    receive
        {udp, Socket, _, IncomingPort, Data} when IncomingPort =:= undefined ->
            inet:setopts(Socket, [{active, once}]),
            {ok, Data, StateSocket#state_socket{incoming_port = IncomingPort}};
        {udp, Socket, _, IncomingPort, Data} ->
            inet:setopts(Socket, [{active, once}]),
            {ok, Data, StateSocket};
        {udp_closed, Socket} ->
            {error, socket_closed};
        'cloudi_service_init_timeout' ->
            {error, timeout}
    end;
socket_recv(#state_socket{protocol = Protocol,
                          listener = Listener,
                          acceptor = Acceptor,
                          socket = Socket} = StateSocket)
    when Protocol =:= tcp; Protocol =:= local ->
    receive
        {tcp, Socket, Data} ->
            inet:setopts(Socket, [{active, once}]),
            {ok, Data, StateSocket};
        {tcp_closed, Socket} ->
            {error, socket_closed};
        {tcp_error, Socket, Reason} ->
            {error, Reason};
        {inet_async, _, _, {ok, _}} = Accept ->
            socket_recv(socket_accept(Accept, StateSocket));
        {inet_async, Listener, Acceptor, _} ->
            {error, inet_async};
        'cloudi_service_init_timeout' ->
            {error, timeout}
    end.

socket_recv_term(StateSocket) ->
    case socket_recv(StateSocket) of
        {ok, Data, NewStateSocket} ->
            try erlang:binary_to_term(Data, [safe]) of
                Term ->
                    {ok, Term, NewStateSocket}
            catch
                error:badarg ->
                    {error, protocol}
            end;
        {error, _} = Error ->
            Error
    end.

socket_new(#state{protocol = Protocol,
                  port = Port,
                  socket_path = SocketPath,
                  socket_options = SocketOptions}) ->
    if
        Protocol =:= tcp ->
            socket_open_tcp(SocketOptions);
        Protocol =:= udp ->
            socket_open_udp(SocketOptions);
        Protocol =:= local ->
            socket_open_local(SocketOptions, Port, SocketPath)
    end.

socket_open(tcp, _, _, BufferSize) ->
    SocketOptions = [{recbuf, BufferSize}, {sndbuf, BufferSize},
                     {nodelay, true}, {delay_send, false}, {keepalive, false},
                     {send_timeout, 5000}, {send_timeout_close, true}],
    socket_open_tcp(SocketOptions);
socket_open(udp, _, _, BufferSize) ->
    SocketOptions = [{recbuf, BufferSize}, {sndbuf, BufferSize}],
    socket_open_udp(SocketOptions);
socket_open(local, SocketPath, ThreadIndex, BufferSize) ->
    SocketOptions = [{recbuf, BufferSize}, {sndbuf, BufferSize},
                     {nodelay, true}, {delay_send, false}, {keepalive, false},
                     {send_timeout, 5000}, {send_timeout_close, true}],
    ThreadSocketPath = SocketPath ++ erlang:integer_to_list(ThreadIndex),
    socket_open_local(SocketOptions, ThreadIndex, ThreadSocketPath).

socket_open_tcp(SocketOptions) ->
    try
        case gen_tcp:listen(0, [binary, inet, {ip, {127,0,0,1}},
                                {packet, 4}, {backlog, 1},
                                {active, false} | SocketOptions]) of
            {ok, Listener} ->
                {ok, Port} = inet:port(Listener),
                {ok, Acceptor} = prim_inet:async_accept(Listener, -1),
                {ok, #state_socket{protocol = tcp,
                                   port = Port,
                                   listener = Listener,
                                   acceptor = Acceptor,
                                   socket_options = SocketOptions}};
            {error, _} = Error ->
                Error
        end
    catch
        ErrorType:ErrorReason ->
            {error, {ErrorType, ErrorReason}}
    end.

socket_open_udp(SocketOptions) ->
    try
        case gen_udp:open(0, [binary, inet, {ip, {127,0,0,1}},
                              {active, once} | SocketOptions]) of
            {ok, Socket} ->
                {ok, Port} = inet:port(Socket),
                {ok, #state_socket{protocol = udp,
                                   port = Port,
                                   socket_options = SocketOptions,
                                   socket = Socket}};
            {error, _} = Error ->
                Error
        end
    catch
        ErrorType:ErrorReason ->
            {error, {ErrorType, ErrorReason}}
    end.

socket_open_local(SocketOptions, Port, SocketPath) ->
    try
        case gen_tcp:listen(0, [binary, local, {ifaddr, {local, SocketPath}},
                                {packet, 4}, {backlog, 1},
                                {active, false} | SocketOptions]) of
            {ok, Listener} ->
                {ok, Acceptor} = prim_inet:async_accept(Listener, -1),
                {ok, #state_socket{protocol = local,
                                   port = Port,
                                   listener = Listener,
                                   acceptor = Acceptor,
                                   socket_path = SocketPath,
                                   socket_options = SocketOptions}};
            {error, _} = Error ->
                Error
        end
    catch
        ErrorType:ErrorReason ->
            {error, {ErrorType, ErrorReason}}
    end.

socket_accept(Accept, #state_socket{protocol = tcp} = StateSocket) ->
    socket_accept_tcp(Accept, StateSocket);
socket_accept(Accept, #state_socket{protocol = local} = StateSocket) ->
    socket_accept_local(Accept, StateSocket).

socket_accept_tcp({inet_async, Listener, Acceptor, {ok, Socket}},
                  #state_socket{
                      protocol = tcp,
                      listener = Listener,
                      acceptor = Acceptor,
                      socket_options = SocketOptions} = StateSocket) ->
    true = inet_db:register_socket(Socket, inet_tcp),
    ok = inet:setopts(Socket, [{active, once} | SocketOptions]),
    catch gen_tcp:close(Listener),
    StateSocket#state_socket{listener = undefined,
                             acceptor = undefined,
                             socket = Socket}.

socket_accept_local({inet_async, Listener, Acceptor, {ok, Socket}},
                    #state_socket{
                        protocol = local,
                        listener = Listener,
                        acceptor = Acceptor,
                        socket_options = SocketOptions} = StateSocket) ->
    true = inet_db:register_socket(Socket, local_tcp),
    ok = inet:setopts(Socket, [{active, once} | SocketOptions]),
    catch gen_tcp:close(Listener),
    StateSocket#state_socket{listener = undefined,
                             acceptor = undefined,
                             socket = Socket}.

socket_close(socket_closed = Reason,
             #state_socket{socket = Socket} = StateSocket)
    when Socket =/= undefined ->
    socket_close(Reason, StateSocket#state_socket{socket = undefined});
socket_close(_Reason,
             #state_socket{protocol = Protocol,
                           socket = Socket,
                           timeout_term = TimeoutTerm} = StateSocket)
    when Protocol =:= tcp; Protocol =:= local ->
    if
        Socket =:= undefined ->
            ok;
        is_port(Socket) ->
            case send('terminate_out'(), StateSocket) of
                ok ->
                    receive
                        {tcp_closed, Socket} ->
                            ok;
                        {tcp_error, Socket, _} ->
                            ok
                    after
                        TimeoutTerm ->
                            ok
                    end;
                {error, _} ->
                    ok
            end
    end,
    ok = socket_close(StateSocket),
    ok;
socket_close(_Reason,
             #state_socket{protocol = udp,
                           socket = Socket,
                           timeout_term = TimeoutTerm} = StateSocket) ->
    if
        Socket =:= undefined ->
            ok;
        is_port(Socket) ->
            case send('terminate_out'(), StateSocket) of
                ok ->
                    TerminateSleep = erlang:min(?KEEPALIVE_UDP, TimeoutTerm),
                    receive after TerminateSleep -> ok end;
                {error, _} ->
                    ok
            end
    end,
    ok = socket_close(StateSocket),
    ok.

socket_close(#state_socket{protocol = Protocol,
                           listener = Listener,
                           socket_path = SocketPath,
                           socket = Socket} = StateSocket)
    when Protocol =:= tcp; Protocol =:= local ->
    if
        Socket =:= undefined ->
            ok;
        is_port(Socket) ->
            catch gen_tcp:close(Socket)
    end,
    catch gen_tcp:close(Listener),
    if
        Protocol =:= local ->
            catch file:delete(SocketPath);
        true ->
            ok
    end,
    ok = os_pid_kill(StateSocket),
    ok;
socket_close(#state_socket{protocol = udp,
                           socket = Socket} = StateSocket) ->
    if
        Socket =:= undefined ->
            ok;
        is_port(Socket) ->
            catch gen_udp:close(Socket)
    end,
    ok = os_pid_kill(StateSocket),
    ok.

socket_data_to_state(#state_socket{protocol = Protocol,
                                   port = Port,
                                   incoming_port = IncomingPort,
                                   listener = Listener,
                                   acceptor = Acceptor,
                                   socket_path = SocketPath,
                                   socket_options = SocketOptions,
                                   socket = Socket}, State) ->
    State#state{protocol = Protocol,
                port = Port,
                incoming_port = IncomingPort,
                listener = Listener,
                acceptor = Acceptor,
                socket_path = SocketPath,
                socket_options = SocketOptions,
                socket = Socket}.

socket_data_from_state(#state{protocol = Protocol,
                              port = Port,
                              incoming_port = IncomingPort,
                              listener = Listener,
                              acceptor = Acceptor,
                              socket_path = SocketPath,
                              socket_options = SocketOptions,
                              socket = Socket,
                              timeout_term = TimeoutTerm,
                              os_pid = OSPid,
                              options = #config_service_options{
                                  cgroup = CGroup}}) ->
    #state_socket{protocol = Protocol,
                  port = Port,
                  incoming_port = IncomingPort,
                  listener = Listener,
                  acceptor = Acceptor,
                  socket_path = SocketPath,
                  socket_options = SocketOptions,
                  socket = Socket,
                  timeout_term = TimeoutTerm,
                  os_pid = OSPid,
                  cgroup = CGroup}.

aspects_init([], _, _, _, ServiceState) ->
    {ok, ServiceState};
aspects_init([{M, F} | L], CommandLine, Prefix, Timeout, ServiceState) ->
    case M:F(CommandLine, Prefix, Timeout, ServiceState) of
        {ok, NewServiceState} ->
            aspects_init(L, CommandLine, Prefix, Timeout, NewServiceState);
        {stop, _, _} = Stop ->
            Stop
    end;
aspects_init([F | L], CommandLine, Prefix, Timeout, ServiceState) ->
    case F(CommandLine, Prefix, Timeout, ServiceState) of
        {ok, NewServiceState} ->
            aspects_init(L, CommandLine, Prefix, Timeout, NewServiceState);
        {stop, _, _} = Stop ->
            Stop
    end.

aspects_request_before([], _, _, _, _, _,
                       Timeout, _, _, _, ServiceState, _) ->
    {ok, Timeout, ServiceState};
aspects_request_before([_ | _] = L, Type, Name, Pattern, RequestInfo, Request,
                       Timeout, Priority, TransId, Source,
                       ServiceState, RequestTimeoutAdjustment) ->
    RequestTimeoutF = request_timeout_adjustment_f(RequestTimeoutAdjustment),
    aspects_request_before_f(L, Type,
                             Name, Pattern, RequestInfo, Request,
                             Timeout, Priority, TransId, Source,
                             ServiceState, RequestTimeoutF).

aspects_request_before_f([], _, _, _, _, _,
                         Timeout, _, _, _, ServiceState, RequestTimeoutF) ->
    {ok, RequestTimeoutF(Timeout), ServiceState};
aspects_request_before_f([{M, F} | L], Type,
                         Name, Pattern, RequestInfo, Request,
                         Timeout, Priority, TransId, Source,
                         ServiceState, RequestTimeoutF) ->
    case M:F(Type, Name, Pattern, RequestInfo, Request,
             Timeout, Priority, TransId, Source,
             ServiceState) of
        {ok, NewServiceState} ->
            aspects_request_before_f(L, Type,
                                     Name, Pattern, RequestInfo, Request,
                                     Timeout, Priority, TransId, Source,
                                     NewServiceState, RequestTimeoutF);
        {stop, _, _} = Stop ->
            Stop
    end;
aspects_request_before_f([F | L], Type,
                         Name, Pattern, RequestInfo, Request,
                         Timeout, Priority, TransId, Source,
                         ServiceState, RequestTimeoutF) ->
    case F(Type, Name, Pattern, RequestInfo, Request,
           Timeout, Priority, TransId, Source,
           ServiceState) of
        {ok, NewServiceState} ->
            aspects_request_before_f(L, Type,
                                     Name, Pattern, RequestInfo, Request,
                                     Timeout, Priority, TransId, Source,
                                     NewServiceState, RequestTimeoutF);
        {stop, _, _} = Stop ->
            Stop
    end.

aspects_request_after([], _, _, _, _, _,
                      Timeout, _, _, _, _, ServiceState, _) ->
    {ok, Timeout, ServiceState};
aspects_request_after([_ | _] = L, Type, Name, Pattern, RequestInfo, Request,
                      Timeout, Priority, TransId, Source,
                      Result, ServiceState, RequestTimeoutAdjustment) ->
    RequestTimeoutF = request_timeout_adjustment_f(RequestTimeoutAdjustment),
    aspects_request_after_f(L, Type,
                            Name, Pattern, RequestInfo, Request,
                            Timeout, Priority, TransId, Source,
                            Result, ServiceState, RequestTimeoutF).

aspects_request_after_f([], _, _, _, _, _,
                        Timeout, _, _, _, _, ServiceState, RequestTimeoutF) ->
    {ok, RequestTimeoutF(Timeout), ServiceState};
aspects_request_after_f([{M, F} | L], Type,
                        Name, Pattern, RequestInfo, Request,
                        Timeout, Priority, TransId, Source,
                        Result, ServiceState, RequestTimeoutF) ->
    case M:F(Type, Name, Pattern, RequestInfo, Request,
             Timeout, Priority, TransId, Source,
             Result, ServiceState) of
        {ok, NewServiceState} ->
            aspects_request_after_f(L, Type,
                                    Name, Pattern, RequestInfo, Request,
                                    Timeout, Priority, TransId, Source,
                                    Result, NewServiceState, RequestTimeoutF);
        {stop, _, _} = Stop ->
            Stop
    end;
aspects_request_after_f([F | L], Type,
                        Name, Pattern, RequestInfo, Request,
                        Timeout, Priority, TransId, Source,
                        Result, ServiceState, RequestTimeoutF) ->
    case F(Type, Name, Pattern, RequestInfo, Request,
           Timeout, Priority, TransId, Source,
           Result, ServiceState) of
        {ok, NewServiceState} ->
            aspects_request_after_f(L, Type,
                                    Name, Pattern, RequestInfo, Request,
                                    Timeout, Priority, TransId, Source,
                                    Result, NewServiceState, RequestTimeoutF);
        {stop, _, _} = Stop ->
            Stop
    end.

update(_, #config_service_update{type = Type})
    when Type =/= external ->
    {ok, {error, type}};
update(_, #config_service_update{update_start = false}) ->
    {ok, {error, update_start_failed}};
update(State, #config_service_update{spawn_os_process = false} = UpdatePlan) ->
    {ok, undefined, update_state(State, UpdatePlan)};
update(State, #config_service_update{spawn_os_process = true} = UpdatePlan) ->
    ok = socket_close(update, socket_data_from_state(State)),
    NewState = update_state(State, UpdatePlan),
    case socket_new(NewState) of
        {ok, StateSocket} ->
            #state{dispatcher = Dispatcher,
                   timeout_init = Timeout,
                   options = #config_service_options{
                       scope = Scope}} = NewState,
            InitTimer = erlang:send_after(Timeout, Dispatcher,
                                          'cloudi_service_init_timeout'),
            % all old subscriptions are stored but not removed until after
            % the new OS process is initialized
            % (if old service requests are queued for service name patterns
            %  that are no longer valid for the new OS process, the CloudI API
            %  implementation will make sure a null response is returned)
            Subscriptions = cloudi_x_cpg:which_groups_counts(Scope, Dispatcher,
                                                             Timeout),
            {ok, StateSocket,
             socket_data_to_state(StateSocket,
                                  NewState#state{os_pid = undefined,
                                                 init_timer = InitTimer,
                                                 subscribed = Subscriptions})};
        {error, _} = Error ->
            Error
    end.

update_state(#state{dispatcher = Dispatcher,
                    timeout_init = OldTimeoutInit,
                    timeout_async = OldTimeoutAsync,
                    timeout_sync = OldTimeoutSync,
                    dest_refresh = OldDestRefresh,
                    cpg_data = OldGroups,
                    dest_deny = OldDestDeny,
                    dest_allow = OldDestAllow,
                    options = OldConfigOptions} = State,
             #config_service_update{
                 dest_refresh = NewDestRefresh,
                 timeout_init = NewTimeoutInit,
                 timeout_async = NewTimeoutAsync,
                 timeout_sync = NewTimeoutSync,
                 dest_list_deny = NewDestListDeny,
                 dest_list_allow = NewDestListAllow,
                 options_keys = OptionsKeys,
                 options = NewConfigOptions}) ->
    DestRefresh = if
        NewDestRefresh =:= undefined ->
            OldDestRefresh;
        is_atom(NewDestRefresh) ->
            NewDestRefresh
    end,
    Groups = destination_refresh_groups(DestRefresh, OldGroups),
    TimeoutInit = if
        NewTimeoutInit =:= undefined ->
            OldTimeoutInit;
        is_integer(NewTimeoutInit) ->
            NewTimeoutInit
    end,
    TimeoutAsync = if
        NewTimeoutAsync =:= undefined ->
            OldTimeoutAsync;
        is_integer(NewTimeoutAsync) ->
            NewTimeoutAsync
    end,
    TimeoutSync = if
        NewTimeoutSync =:= undefined ->
            OldTimeoutSync;
        is_integer(NewTimeoutSync) ->
            NewTimeoutSync
    end,
    DestDeny = if
        NewDestListDeny =:= invalid ->
            OldDestDeny;
        NewDestListDeny =:= undefined ->
            undefined;
        is_list(NewDestListDeny) ->
            cloudi_x_trie:new(NewDestListDeny)
    end,
    DestAllow = if
        NewDestListAllow =:= invalid ->
            OldDestAllow;
        NewDestListAllow =:= undefined ->
            undefined;
        is_list(NewDestListAllow) ->
            cloudi_x_trie:new(NewDestListAllow)
    end,
    case lists:member(monkey_chaos, OptionsKeys) of
        true ->
            #config_service_options{
                monkey_chaos = OldMonkeyChaos} = OldConfigOptions,
            cloudi_core_i_runtime_testing:
            monkey_chaos_destroy(OldMonkeyChaos);
        false ->
            ok
    end,
    ConfigOptions0 = cloudi_core_i_configuration:
                     service_options_copy(OptionsKeys,
                                          OldConfigOptions,
                                          NewConfigOptions),
    ConfigOptionsN = case lists:member(rate_request_max, OptionsKeys) of
        true ->
            #config_service_options{
                rate_request_max = RateRequest} = ConfigOptions0,
            NewRateRequest = if
                RateRequest =/= undefined ->
                    cloudi_core_i_rate_based_configuration:
                    rate_request_init(RateRequest);
                true ->
                    RateRequest
            end,
            ConfigOptions0#config_service_options{
                rate_request_max = NewRateRequest};
        false ->
            ConfigOptions0
    end,
    if
        (OldDestRefresh =:= immediate_closest orelse
         OldDestRefresh =:= immediate_furthest orelse
         OldDestRefresh =:= immediate_random orelse
         OldDestRefresh =:= immediate_local orelse
         OldDestRefresh =:= immediate_remote orelse
         OldDestRefresh =:= immediate_newest orelse
         OldDestRefresh =:= immediate_oldest) andalso
        (NewDestRefresh =:= lazy_closest orelse
         NewDestRefresh =:= lazy_furthest orelse
         NewDestRefresh =:= lazy_random orelse
         NewDestRefresh =:= lazy_local orelse
         NewDestRefresh =:= lazy_remote orelse
         NewDestRefresh =:= lazy_newest orelse
         NewDestRefresh =:= lazy_oldest) ->
            #config_service_options{
                dest_refresh_delay = Delay,
                scope = Scope} = ConfigOptionsN,
            destination_refresh(DestRefresh, Dispatcher, Delay, Scope);
        true ->
            ok
    end,
    State#state{timeout_init = TimeoutInit,
                timeout_async = TimeoutAsync,
                timeout_sync = TimeoutSync,
                dest_refresh = DestRefresh,
                cpg_data = Groups,
                dest_deny = DestDeny,
                dest_allow = DestAllow,
                options = ConfigOptionsN}.

update_after(StateSocket, State) ->
    case socket_recv_term(StateSocket) of
        {ok, {'pid', OSPid}, NewStateSocket} ->
            NewState = socket_data_to_state(NewStateSocket, State),
            update_after(NewStateSocket, os_pid_set(OSPid, NewState));
        {ok, 'init', NewStateSocket} ->
            NewState = socket_data_to_state(NewStateSocket, State),
            ok = os_init(NewState),
            NewState;
        {error, Reason} ->
            ?LOG_ERROR("update_failed: ~p", [Reason]),
            erlang:exit(update_failed)
    end.

