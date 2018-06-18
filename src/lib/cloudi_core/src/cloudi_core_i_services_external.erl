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

-module(cloudi_core_i_services_external).
-author('mjtruog at protonmail dot com').

-behaviour(gen_statem).

%% external interface
-export([start_link/20,
         port/2,
         stdout/2,
         stderr/2,
         get_status/1,
         get_status/2]).

%% gen_statem callbacks
-export([callback_mode/0,
         init/1, handle_event/4,
         terminate/3, code_change/4, format_status/2]).

%% FSM States
-export(['CONNECT'/3,
         'INIT_WAIT'/3,
         'HANDLE'/3]).

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
        send_timeouts = #{}
            :: #{cloudi:trans_id() :=
                 {passive, pid() | undefined, reference()}} |
               list({cloudi:trans_id(),
                     {passive, pid() | undefined, reference()}}),
        % ( 4) if a sent service request timeout is greater than the
        % service configuration option request_timeout_immediate_max,
        % monitor the destination process with the sent service request
        % transaction id
        send_timeout_monitors = #{}
            :: #{pid() := {reference(), list(cloudi:trans_id())}} |
               list({pid(), {reference(), list(cloudi:trans_id())}}),
        % ( 5) timeout enforcement for any incoming service requests
        recv_timeouts = #{}
            :: #{cloudi:trans_id() := reference()} |
               list({cloudi:trans_id(), reference()}),
        % ( 6) timeout enforcement for any responses to
        % asynchronous outgoing service requests
        async_responses = #{}
            :: #{cloudi:trans_id() := {binary(), binary()}} |
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
        % (22) request data currently being processed by the OS process
        request_data = undefined
            :: undefined |
               {cloudi:message_service_request(),
                fun((cloudi:timeout_value_milliseconds()) ->
                    cloudi:timeout_value_milliseconds())},
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

start_link(Protocol, SocketPath, ThreadIndex, ProcessIndex, ProcessCount,
           TimeStart, TimeRestart, Restarts,
           CommandLine, BufferSize, Timeout, [PrefixC | _] = Prefix,
           TimeoutAsync, TimeoutSync, TimeoutTerm,
           DestRefresh, DestDeny, DestAllow,
           #config_service_options{
               scope = Scope,
               dispatcher_pid_options = PidOptions} = ConfigOptions, ID)
    when is_atom(Protocol), is_list(SocketPath), is_integer(ThreadIndex),
         is_integer(ProcessIndex), is_integer(ProcessCount),
         is_integer(TimeStart), is_integer(Restarts),
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
            gen_statem:start_link(?MODULE,
                                  [Protocol, SocketPath,
                                   ThreadIndex, ProcessIndex, ProcessCount,
                                   TimeStart, TimeRestart, Restarts,
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
    gen_statem:call(Dispatcher, port,
                    {dirty_timeout, Timeout + ?TIMEOUT_DELTA}).

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
%%% Callback functions from gen_statem
%%%------------------------------------------------------------------------

callback_mode() -> state_functions.

init([Protocol, SocketPath, ThreadIndex, ProcessIndex, ProcessCount,
      TimeStart, TimeRestart, Restarts,
      CommandLine, BufferSize, Timeout, Prefix,
      TimeoutAsync, TimeoutSync, TimeoutTerm,
      DestRefresh, DestDeny, DestAllow,
      #config_service_options{
          dispatcher_pid_options = PidOptions} = ConfigOptions, ID])
    when Protocol =:= tcp;
         Protocol =:= udp;
         Protocol =:= local ->
    ok = spawn_opt_options_after(PidOptions),
    Uptime = uptime(TimeStart, TimeRestart, Restarts),
    erlang:put(?SERVICE_ID_PDICT_KEY, ID),
    erlang:put(?SERVICE_UPTIME_PDICT_KEY, Uptime),
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
            erlang:process_flag(trap_exit, true),
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

handle_event(EventType, EventContent, StateName, State) ->
    Event = {EventType, EventContent},
    ?LOG_WARN("Unknown event \"~w\"", [Event]),
    {stop, {StateName, undefined_event, Event}, State}.

% incoming messages (from the port socket)

'CONNECT'({call, From}, port, #state{port = Port}) ->
    {keep_state_and_data, {reply, From, Port}};

'CONNECT'(connection, {'pid', OSPid}, State) ->
    {keep_state, os_pid_set(OSPid, State)};

'CONNECT'(connection, 'init', #state{initialize = Ready} = State) ->
    if
        Ready =:= true ->
            connection_init(State);
        Ready =:= false ->
            {next_state, 'INIT_WAIT', State}
    end;

'CONNECT'(info, {inet_async, _, _, {ok, _}} = Accept, State) ->
    StateSocket = socket_data_from_state(State),
    NewStateSocket = socket_accept(Accept, StateSocket),
    {keep_state,
     socket_data_to_state(NewStateSocket, State)};

'CONNECT'(info, {inet_async, Listener, Acceptor, Error},
          #state{protocol = Protocol,
                 listener = Listener,
                 acceptor = Acceptor})
    when Protocol =:= tcp; Protocol =:= local ->
    {stop, {?FUNCTION_NAME, inet_async, Error}};

'CONNECT'(info, cloudi_service_init_begin, State) ->
    {keep_state, State#state{initialize = true}};

'CONNECT'(info, EventContent, State) ->
    handle_info(EventContent, ?FUNCTION_NAME, State);

'CONNECT'(EventType, EventContent, _) ->
    Event = {EventType, EventContent},
    {stop, {?FUNCTION_NAME, undefined_message, Event}}.

'INIT_WAIT'({call, From}, port, #state{port = Port}) ->
    {keep_state_and_data, {reply, From, Port}};

'INIT_WAIT'(info, cloudi_service_init_begin, State) ->
    connection_init(State#state{initialize = true});

'INIT_WAIT'(info, EventContent, State) ->
    handle_info(EventContent, ?FUNCTION_NAME, State);

'INIT_WAIT'(EventType, EventContent, _) ->
    Event = {EventType, EventContent},
    {stop, {?FUNCTION_NAME, undefined_message, Event}}.

'HANDLE'(connection,
         'polling',
         #state{dispatcher = Dispatcher,
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
            ok = cloudi_core_i_services_monitor:
                 initialized_process(Dispatcher),
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
            {keep_state,
             process_queues(State#state{service_state = NewServiceState,
                                        init_timer = undefined,
                                        subscribed = []})};
        {stop, Reason, NewServiceState} ->
            {stop, Reason,
             State#state{service_state = NewServiceState,
                         init_timer = undefined}}
    end;

'HANDLE'(connection, Request,
         #state{init_timer = InitTimer} = State)
    when is_reference(InitTimer), is_tuple(Request),
         (element(1, Request) =:= 'send_sync' orelse
          element(1, Request) =:= 'recv_async') ->
    % service request responses are always handled after initialization
    % is successful though it is possible for the
    % service request response to be received before initialization
    % is complete (the response remains queued)
    {stop, {error, invalid_state}, State};

'HANDLE'(connection,
         {'subscribe', Suffix},
         #state{dispatcher = Dispatcher,
                prefix = Prefix,
                options = #config_service_options{
                    count_process_dynamic = CountProcessDynamic,
                    scope = Scope}}) ->
    true = is_list(Suffix),
    case cloudi_core_i_rate_based_configuration:
         count_process_dynamic_terminated(CountProcessDynamic) of
        false ->
            Pattern = Prefix ++ Suffix,
            _ = cloudi_x_trie:is_pattern(Pattern),
            ok = cloudi_x_cpg:join(Scope, Pattern,
                                   Dispatcher, infinity);
        true ->
            ok
    end,
    keep_state_and_data;

'HANDLE'(connection,
         {'subscribe_count', Suffix},
         #state{dispatcher = Dispatcher,
                prefix = Prefix,
                options = #config_service_options{
                    scope = Scope}} = State) ->
    true = is_list(Suffix),
    Pattern = Prefix ++ Suffix,
    _ = cloudi_x_trie:is_pattern(Pattern),
    Count = cloudi_x_cpg:join_count(Scope, Pattern,
                                    Dispatcher, infinity),
    ok = send('subscribe_count_out'(Count), State),
    keep_state_and_data;

'HANDLE'(connection,
         {'unsubscribe', Suffix},
         #state{dispatcher = Dispatcher,
                prefix = Prefix,
                options = #config_service_options{
                    count_process_dynamic = CountProcessDynamic,
                    scope = Scope}}) ->
    true = is_list(Suffix),
    case cloudi_core_i_rate_based_configuration:
         count_process_dynamic_terminated(CountProcessDynamic) of
        false ->
            Pattern = Prefix ++ Suffix,
            _ = cloudi_x_trie:is_pattern(Pattern),
            case cloudi_x_cpg:leave(Scope, Pattern,
                                    Dispatcher, infinity) of
                ok ->
                    keep_state_and_data;
                error ->
                    {stop, {error, {unsubscribe_invalid, Pattern}}}
            end;
        true ->
            keep_state_and_data
    end;

'HANDLE'(connection,
         {'send_async', Name, RequestInfo, Request, Timeout, Priority},
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
                              Timeout, Priority, State);
        false ->
            ok = send('return_async_out'(), State),
            keep_state_and_data
    end;

'HANDLE'(connection,
         {'send_sync', Name, RequestInfo, Request, Timeout, Priority},
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
                             Timeout, Priority, State);
        false ->
            ok = send('return_sync_out'(), State),
            keep_state_and_data
    end;

'HANDLE'(connection,
         {'mcast_async', Name, RequestInfo, Request, Timeout, Priority},
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
                               Timeout, Priority, State);
        false ->
            ok = send('returns_async_out'(), State),
            keep_state_and_data
    end;

'HANDLE'(connection,
         {'forward_async', NextName, NextRequestInfo, NextRequest,
          NextTimeout, NextPriority, TransId, Source},
         #state{dispatcher = Dispatcher,
                service_state = ServiceState,
                request_data = {{_, Name, Pattern, RequestInfo, Request,
                                 Timeout, Priority, TransId, Source},
                                RequestTimeoutF},
                dest_refresh = DestRefresh,
                cpg_data = Groups,
                dest_deny = DestDeny,
                dest_allow = DestAllow,
                options = #config_service_options{
                    request_name_lookup = RequestNameLookup,
                    response_timeout_immediate_max =
                        ResponseTimeoutImmediateMax,
                    scope = Scope,
                    aspects_request_after = AspectsAfter}} = State) ->
    true = is_list(NextName) andalso is_integer(hd(NextName)),
    true = is_integer(NextTimeout),
    true = (NextTimeout >= 0) andalso
           (NextTimeout =< ?TIMEOUT_MAX_ERLANG),
    true = is_integer(NextPriority),
    true = (NextPriority >= ?PRIORITY_HIGH) andalso
           (NextPriority =< ?PRIORITY_LOW),
    Type = send_async,
    Result = {forward, NextName,
              NextRequestInfo, NextRequest,
              NextTimeout, NextPriority},
    try aspects_request_after(AspectsAfter, Type,
                              Name, Pattern, RequestInfo, Request,
                              Timeout, Priority, TransId, Source,
                              Result, ServiceState) of
        {ok, NewServiceState} ->
            NewTimeout = if
                NextTimeout == Timeout ->
                    RequestTimeoutF(Timeout);
                true ->
                    NextTimeout
            end,
            case destination_allowed(NextName, DestDeny, DestAllow) of
                true ->
                    case destination_get(DestRefresh, Scope, NextName, Source,
                                         Groups, NewTimeout) of
                        {error, timeout} ->
                            ok;
                        {error, _}
                            when RequestNameLookup =:= async ->
                            if
                                NewTimeout >= ResponseTimeoutImmediateMax ->
                                    Source ! {'cloudi_service_return_async',
                                              Name, Pattern, <<>>, <<>>,
                                              NewTimeout, TransId, Source};
                                true ->
                                    ok
                            end,
                            ok;
                        {error, _}
                            when NewTimeout >= ?FORWARD_ASYNC_INTERVAL ->
                            Retry = {'cloudi_service_forward_async_retry',
                                     NextName, NextRequestInfo, NextRequest,
                                     NewTimeout - ?FORWARD_ASYNC_INTERVAL,
                                     NextPriority, TransId, Source},
                            erlang:send_after(?FORWARD_ASYNC_INTERVAL,
                                              Dispatcher, Retry),
                            ok;
                        {error, _} ->
                            ok;
                        {ok, NextPattern, NextPid}
                            when NewTimeout >= ?FORWARD_DELTA ->
                            NextPid ! {'cloudi_service_send_async',
                                       NextName, NextPattern,
                                       NextRequestInfo, NextRequest,
                                       NewTimeout - ?FORWARD_DELTA,
                                       NextPriority, TransId, Source};
                        _ ->
                            ok
                    end;
                false ->
                    ok
            end,
            {keep_state,
             process_queues(State#state{service_state = NewServiceState,
                                        request_data = undefined})};
        {stop, Reason, NewServiceState} ->
            {stop, Reason,
             State#state{service_state = NewServiceState,
                         request_data = undefined}}
    catch
        ?STACKTRACE(ErrorType, Error, ErrorStackTrace)
            ?LOG_ERROR("request ~p ~p~n~p",
                       [ErrorType, Error, ErrorStackTrace]),
            {stop, {ErrorType, {Error, ErrorStackTrace}},
             State#state{request_data = undefined}}
    end;

'HANDLE'(connection,
         {'forward_sync', NextName, NextRequestInfo, NextRequest,
          NextTimeout, NextPriority, TransId, Source},
         #state{dispatcher = Dispatcher,
                service_state = ServiceState,
                request_data = {{_, Name, Pattern, RequestInfo, Request,
                                 Timeout, Priority, TransId, Source},
                                RequestTimeoutF},
                dest_refresh = DestRefresh,
                cpg_data = Groups,
                dest_deny = DestDeny,
                dest_allow = DestAllow,
                options = #config_service_options{
                    request_name_lookup = RequestNameLookup,
                    response_timeout_immediate_max =
                        ResponseTimeoutImmediateMax,
                    scope = Scope,
                    aspects_request_after = AspectsAfter}} = State) ->
    true = is_list(NextName) andalso is_integer(hd(NextName)),
    true = is_integer(NextTimeout),
    true = (NextTimeout >= 0) andalso
           (NextTimeout =< ?TIMEOUT_MAX_ERLANG),
    true = is_integer(NextPriority),
    true = (NextPriority >= ?PRIORITY_HIGH) andalso
           (NextPriority =< ?PRIORITY_LOW),
    Type = send_sync,
    Result = {forward, NextName, NextRequestInfo, NextRequest,
              NextTimeout, NextPriority},
    try aspects_request_after(AspectsAfter, Type,
                              Name, Pattern, RequestInfo, Request,
                              Timeout, Priority, TransId, Source,
                              Result, ServiceState) of
        {ok, NewServiceState} ->
            NewTimeout = if
                NextTimeout == Timeout ->
                    RequestTimeoutF(Timeout);
                true ->
                    NextTimeout
            end,
            case destination_allowed(NextName, DestDeny, DestAllow) of
                true ->
                    case destination_get(DestRefresh, Scope, NextName, Source,
                                         Groups, NewTimeout) of
                        {error, timeout} ->
                            ok;
                        {error, _}
                            when RequestNameLookup =:= async ->
                            if
                                NewTimeout >= ResponseTimeoutImmediateMax ->
                                    Source ! {'cloudi_service_return_sync',
                                              Name, Pattern, <<>>, <<>>,
                                              NewTimeout, TransId, Source};
                                true ->
                                    ok
                            end,
                            ok;
                        {error, _}
                            when NewTimeout >= ?FORWARD_SYNC_INTERVAL ->
                            Retry = {'cloudi_service_forward_sync_retry',
                                     NextName, NextRequestInfo, NextRequest,
                                     NewTimeout - ?FORWARD_SYNC_INTERVAL,
                                     NextPriority, TransId, Source},
                            erlang:send_after(?FORWARD_SYNC_INTERVAL,
                                              Dispatcher, Retry),
                            ok;
                        {error, _} ->
                            ok;
                        {ok, NextPattern, NextPid}
                            when NewTimeout >= ?FORWARD_DELTA ->
                            NextPid ! {'cloudi_service_send_sync',
                                       NextName, NextPattern,
                                       NextRequestInfo, NextRequest,
                                       NewTimeout - ?FORWARD_DELTA,
                                       NextPriority, TransId, Source};
                        _ ->
                            ok
                    end;
                false ->
                    ok
            end,
            {keep_state,
             process_queues(State#state{service_state = NewServiceState,
                                        request_data = undefined})};
        {stop, Reason, NewServiceState} ->
            {stop, Reason,
             State#state{service_state = NewServiceState,
                         request_data = undefined}}
    catch
        ?STACKTRACE(ErrorType, Error, ErrorStackTrace)
            ?LOG_ERROR("request ~p ~p~n~p",
                       [ErrorType, Error, ErrorStackTrace]),
            {stop, {ErrorType, {Error, ErrorStackTrace}},
             State#state{request_data = undefined}}
    end;

'HANDLE'(connection,
         {ReturnType, Name, Pattern, ResponseInfo, Response,
          NextTimeout, TransId, Source},
         #state{service_state = ServiceState,
                request_data = {{_, Name, Pattern, RequestInfo, Request,
                                 Timeout, Priority, TransId, Source},
                                RequestTimeoutF},
                options = #config_service_options{
                    response_timeout_immediate_max =
                        ResponseTimeoutImmediateMax,
                    aspects_request_after =
                        AspectsAfter}} = State)
    when ReturnType =:= 'return_async';
         ReturnType =:= 'return_sync' ->
    true = is_integer(NextTimeout),
    true = (NextTimeout >= 0) andalso
           (NextTimeout =< ?TIMEOUT_MAX_ERLANG),
    Type = if
        ReturnType =:= 'return_async' ->
            send_async;
        ReturnType =:= 'return_sync' ->
            send_sync
    end,
    Result = if
        ResponseInfo == <<>>, Response == <<>>,
        NextTimeout < ResponseTimeoutImmediateMax ->
            noreply;
        true ->
            {reply, ResponseInfo, Response}
    end,
    try aspects_request_after(AspectsAfter, Type,
                              Name, Pattern, RequestInfo, Request,
                              Timeout, Priority, TransId, Source,
                              Result, ServiceState) of
        {ok, NewServiceState} ->
            NewTimeout = if
                NextTimeout == Timeout ->
                    RequestTimeoutF(Timeout);
                true ->
                    NextTimeout
            end,
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
            {keep_state,
             process_queues(State#state{service_state = NewServiceState,
                                        request_data = undefined})};
        {stop, Reason, NewServiceState} ->
            {stop, Reason,
             State#state{service_state = NewServiceState,
                         request_data = undefined}}
    catch
        ?STACKTRACE(ErrorType, Error, ErrorStackTrace)
            ?LOG_ERROR("request ~p ~p~n~p",
                       [ErrorType, Error, ErrorStackTrace]),
            {stop, {ErrorType, {Error, ErrorStackTrace}},
             State#state{request_data = undefined}}
    end;

'HANDLE'(connection,
         {'recv_async', Timeout, TransId, Consume},
         #state{dispatcher = Dispatcher,
                async_responses = AsyncResponses} = State) ->
    true = is_integer(Timeout),
    true = (Timeout >= 0) andalso
           (Timeout =< ?TIMEOUT_MAX_ERLANG),
    true = is_boolean(Consume),
    case TransId of
        <<0:128>> ->
            case maps:to_list(AsyncResponses) of
                [] when Timeout >= ?RECV_ASYNC_INTERVAL ->
                    erlang:send_after(?RECV_ASYNC_INTERVAL, Dispatcher,
                                      {'cloudi_service_recv_async_retry',
                                       Timeout - ?RECV_ASYNC_INTERVAL,
                                       TransId, Consume}),
                    keep_state_and_data;
                [] ->
                    ok = send('recv_async_out'(timeout, TransId), State),
                    keep_state_and_data;
                L when Consume =:= true ->
                    TransIdPick = ?RECV_ASYNC_STRATEGY(L),
                    {ResponseInfo, Response} = maps:get(TransIdPick,
                                                        AsyncResponses),
                    ok = send('recv_async_out'(ResponseInfo, Response,
                                               TransIdPick),
                              State),
                    {keep_state, State#state{
                        async_responses = maps:remove(TransIdPick,
                                                      AsyncResponses)}};
                L when Consume =:= false ->
                    TransIdPick = ?RECV_ASYNC_STRATEGY(L),
                    {ResponseInfo, Response} = maps:get(TransIdPick,
                                                        AsyncResponses),
                    ok = send('recv_async_out'(ResponseInfo, Response,
                                               TransIdPick),
                              State),
                    keep_state_and_data
            end;
        <<_:48, 0:1, 0:1, 0:1, 1:1, _:12, 1:1, 0:1, _:62>> -> % v1 UUID
            case maps:find(TransId, AsyncResponses) of
                error when Timeout >= ?RECV_ASYNC_INTERVAL ->
                    erlang:send_after(?RECV_ASYNC_INTERVAL, Dispatcher,
                                      {'cloudi_service_recv_async_retry',
                                       Timeout - ?RECV_ASYNC_INTERVAL,
                                       TransId, Consume}),
                    keep_state_and_data;
                error ->
                    ok = send('recv_async_out'(timeout, TransId), State),
                    keep_state_and_data;
                {ok, {ResponseInfo, Response}} when Consume =:= true ->
                    ok = send('recv_async_out'(ResponseInfo, Response, TransId),
                              State),
                    {keep_state, State#state{
                        async_responses = maps:remove(TransId,
                                                      AsyncResponses)}};
                {ok, {ResponseInfo, Response}} when Consume =:= false ->
                    ok = send('recv_async_out'(ResponseInfo, Response, TransId),
                              State),
                    keep_state_and_data
            end
    end;

'HANDLE'(connection, 'keepalive', State) ->
    {keep_state, State#state{keepalive = received}};

'HANDLE'(info,
         {'cloudi_service_send_async_retry', Name, RequestInfo, Request,
          Timeout, Priority}, State) ->
    handle_send_async(Name, RequestInfo, Request,
                      Timeout, Priority, State);

'HANDLE'(info,
         {'cloudi_service_send_sync_retry', Name, RequestInfo, Request,
          Timeout, Priority}, State) ->
    handle_send_sync(Name, RequestInfo, Request,
                     Timeout, Priority, State);

'HANDLE'(info,
         {'cloudi_service_mcast_async_retry', Name, RequestInfo, Request,
          Timeout, Priority}, State) ->
    handle_mcast_async(Name, RequestInfo, Request,
                       Timeout, Priority, State);

'HANDLE'(info,
         {'cloudi_service_forward_async_retry', Name, RequestInfo, Request,
          Timeout, Priority, TransId, Source},
         #state{dispatcher = Dispatcher,
                dest_refresh = DestRefresh,
                cpg_data = Groups,
                options = #config_service_options{
                    request_name_lookup = RequestNameLookup,
                    scope = Scope}}) ->
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
    keep_state_and_data;

'HANDLE'(info,
         {'cloudi_service_forward_sync_retry', Name, RequestInfo, Request,
          Timeout, Priority, TransId, Source},
         #state{dispatcher = Dispatcher,
                dest_refresh = DestRefresh,
                cpg_data = Groups,
                options = #config_service_options{
                    request_name_lookup = RequestNameLookup,
                    scope = Scope}}) ->
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
    keep_state_and_data;

'HANDLE'(info,
         {'cloudi_service_recv_async_retry', Timeout, TransId, Consume},
         State) ->
    'HANDLE'(connection, {'recv_async', Timeout, TransId, Consume}, State);

'HANDLE'(info,
         {SendType, Name, Pattern, RequestInfo, Request,
          Timeout, _, TransId, Source},
         #state{options = #config_service_options{
                    response_timeout_immediate_max =
                        ResponseTimeoutImmediateMax}})
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
    keep_state_and_data;

'HANDLE'(info,
         {SendType, Name, Pattern, RequestInfo, Request,
          Timeout, Priority, TransId, Source} = T,
         #state{queue_requests = false,
                service_state = ServiceState,
                options = #config_service_options{
                    rate_request_max = RateRequest,
                    response_timeout_immediate_max =
                        ResponseTimeoutImmediateMax} = ConfigOptions} = State)
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
            RequestTimeoutF =
                request_timeout_adjustment_f(RequestTimeoutAdjustment),
            try aspects_request_before(AspectsBefore, Type,
                                       Name, Pattern, RequestInfo, Request,
                                       Timeout, Priority, TransId, Source,
                                       ServiceState) of
                {ok, NewServiceState} ->
                    if
                        SendType =:= 'cloudi_service_send_async' ->
                            ok = send('send_async_out'(Name, Pattern,
                                                       RequestInfo, Request,
                                                       Timeout, Priority,
                                                       TransId, Source),
                                      State);
                        SendType =:= 'cloudi_service_send_sync' ->
                            ok = send('send_sync_out'(Name, Pattern,
                                                      RequestInfo, Request,
                                                      Timeout, Priority,
                                                      TransId, Source),
                                      State)
                    end,
                    {keep_state,
                     State#state{queue_requests = true,
                                 service_state = NewServiceState,
                                 request_data = {T, RequestTimeoutF},
                                 options = NewConfigOptions}};
                {stop, Reason, NewServiceState} ->
                    {stop, Reason,
                     State#state{service_state = NewServiceState,
                                 options = NewConfigOptions}}
            catch
                ?STACKTRACE(ErrorType, Error, ErrorStackTrace)
                    ?LOG_ERROR("request ~p ~p~n~p",
                               [ErrorType, Error, ErrorStackTrace]),
                    {stop, {ErrorType, {Error, ErrorStackTrace}},
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
            {keep_state,
             State#state{options = ConfigOptions#config_service_options{
                             rate_request_max = NewRateRequest}}}
    end;

'HANDLE'(info,
         {SendType, Name, Pattern, _, _,
          Timeout, Priority, TransId, Source} = T,
         #state{queue_requests = true,
                queued = Queue,
                queued_size = QueuedSize,
                queued_word_size = WordSize,
                options = #config_service_options{
                    queue_limit = QueueLimit,
                    queue_size = QueueSize,
                    rate_request_max = RateRequest,
                    response_timeout_immediate_max =
                        ResponseTimeoutImmediateMax} = ConfigOptions} = State)
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
            {keep_state,
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
            {keep_state, NewState}
    end;

'HANDLE'(info,
         {'cloudi_service_recv_timeout', Priority, TransId, Size},
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
    {keep_state,
     State#state{recv_timeouts = maps:remove(TransId, RecvTimeouts),
                 queued = NewQueue,
                 queued_size = NewQueuedSize}};

'HANDLE'(info,
         {'cloudi_service_return_async', _Name, _Pattern,
          ResponseInfo, Response, OldTimeout, TransId, Source},
         #state{dispatcher = Dispatcher,
                send_timeouts = SendTimeouts,
                options = #config_service_options{
                    request_timeout_immediate_max =
                        RequestTimeoutImmediateMax,
                    response_timeout_adjustment =
                        ResponseTimeoutAdjustment}} = State) ->
    true = Source =:= Dispatcher,
    case maps:find(TransId, SendTimeouts) of
        error ->
            % send_async timeout already occurred
            keep_state_and_data;
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
            {keep_state,
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
            {keep_state,
             send_timeout_end(TransId, Pid, NewState)}
    end;

'HANDLE'(info,
         {'cloudi_service_return_sync', _Name, _Pattern,
          ResponseInfo, Response, OldTimeout, TransId, Source},
         #state{dispatcher = Dispatcher,
                send_timeouts = SendTimeouts,
                options = #config_service_options{
                    request_timeout_immediate_max =
                        RequestTimeoutImmediateMax,
                    response_timeout_adjustment =
                        ResponseTimeoutAdjustment}} = State) ->
    true = Source =:= Dispatcher,
    case maps:find(TransId, SendTimeouts) of
        error ->
            % send_sync timeout already occurred
            keep_state_and_data;
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
            {keep_state,
             send_timeout_end(TransId, Pid, State)}
    end;

'HANDLE'(info, {'cloudi_service_send_async_timeout', TransId},
         #state{send_timeouts = SendTimeouts} = State) ->
    case maps:find(TransId, SendTimeouts) of
        error ->
            keep_state_and_data;
        {ok, {_, Pid, _}} ->
            {keep_state,
             send_timeout_end(TransId, Pid, State)}
    end;

'HANDLE'(info, {'cloudi_service_send_sync_timeout', TransId},
         #state{send_timeouts = SendTimeouts} = State) ->
    case maps:find(TransId, SendTimeouts) of
        error ->
            keep_state_and_data;
        {ok, {_, Pid, _}} ->
            ok = send('return_sync_out'(timeout, TransId), State),
            {keep_state,
             send_timeout_end(TransId, Pid, State)}
    end;

'HANDLE'(info, {'cloudi_service_recv_async_timeout', TransId},
         #state{async_responses = AsyncResponses} = State) ->
    {keep_state,
     State#state{async_responses = maps:remove(TransId, AsyncResponses)}};

'HANDLE'(info, keepalive_udp,
         #state{dispatcher = Dispatcher,
                keepalive = undefined,
                socket = Socket}) ->
    Dispatcher ! {udp_closed, Socket},
    keep_state_and_data;

'HANDLE'(info, keepalive_udp,
         #state{dispatcher = Dispatcher,
                keepalive = received} = State) ->
    ok = send('keepalive_out'(), State),
    erlang:send_after(?KEEPALIVE_UDP, Dispatcher, keepalive_udp),
    {keep_state, State#state{keepalive = undefined}};

'HANDLE'(info, 'cloudi_count_process_dynamic_rate',
         #state{dispatcher = Dispatcher,
                options = #config_service_options{
                    count_process_dynamic =
                        CountProcessDynamic} = ConfigOptions} = State) ->
    NewCountProcessDynamic = cloudi_core_i_rate_based_configuration:
                             count_process_dynamic_reinit(Dispatcher,
                                                          CountProcessDynamic),
    {keep_state,
     State#state{options = ConfigOptions#config_service_options{
                     count_process_dynamic = NewCountProcessDynamic}}};

'HANDLE'(info, {'cloudi_count_process_dynamic_update', ProcessCount},
         #state{timeout_async = TimeoutAsync,
                timeout_sync = TimeoutSync,
                options = #config_service_options{
                    priority_default = PriorityDefault}
                } = State) ->
    ok = send('reinit_out'(ProcessCount, TimeoutAsync, TimeoutSync,
                           PriorityDefault), State),
    {keep_state,
     State#state{process_count = ProcessCount}};

'HANDLE'(info, 'cloudi_count_process_dynamic_terminate',
         #state{dispatcher = Dispatcher,
                options = #config_service_options{
                    count_process_dynamic = CountProcessDynamic,
                    scope = Scope} = ConfigOptions} = State) ->
    cloudi_x_cpg:leave(Scope, Dispatcher, infinity),
    NewCountProcessDynamic =
        cloudi_core_i_rate_based_configuration:
        count_process_dynamic_terminate_set(Dispatcher, CountProcessDynamic),
    {keep_state,
     State#state{options = ConfigOptions#config_service_options{
                     count_process_dynamic = NewCountProcessDynamic}}};

'HANDLE'(info, 'cloudi_count_process_dynamic_terminate_check',
         #state{dispatcher = Dispatcher,
                queue_requests = QueueRequests}) ->
    if
        QueueRequests =:= false ->
            {stop, {shutdown, cloudi_count_process_dynamic_terminate}};
        QueueRequests =:= true ->
            erlang:send_after(?COUNT_PROCESS_DYNAMIC_INTERVAL, Dispatcher,
                              'cloudi_count_process_dynamic_terminate_check'),
            keep_state_and_data
    end;

'HANDLE'(info, 'cloudi_count_process_dynamic_terminate_now', _) ->
    {stop, {shutdown, cloudi_count_process_dynamic_terminate}};

'HANDLE'(info, 'cloudi_rate_request_max_rate',
         #state{options = #config_service_options{
                    rate_request_max = RateRequest} = ConfigOptions} = State) ->
    NewRateRequest = cloudi_core_i_rate_based_configuration:
                     rate_request_reinit(RateRequest),
    {keep_state,
     State#state{options = ConfigOptions#config_service_options{
                     rate_request_max = NewRateRequest}}};

'HANDLE'(info, {'cloudi_service_update', UpdatePending, UpdatePlan},
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
    {keep_state,
     State#state{update_plan = NewUpdatePlan,
                 queue_requests = true}};

'HANDLE'(info, {'cloudi_service_update_now', UpdateNow, UpdateStart},
         #state{update_plan = UpdatePlan} = State) ->
    #config_service_update{queue_requests = QueueRequests} = UpdatePlan,
    NewUpdatePlan = UpdatePlan#config_service_update{
                        update_now = UpdateNow,
                        update_start = UpdateStart},
    NewState = State#state{update_plan = NewUpdatePlan},
    if
        QueueRequests =:= true ->
            {keep_state, NewState};
        QueueRequests =:= false ->
            {keep_state, process_update(NewState)}
    end;

'HANDLE'(info, {'cloudi_service_update_state', CommandLine}, State) ->
    % state updates when #config_service_update{spawn_os_process = true}
    erlang:put(?SERVICE_FILE_PDICT_KEY, hd(CommandLine)),
    {keep_state, State#state{command_line = CommandLine}};

'HANDLE'(info, {'DOWN', _MonitorRef, process, Pid, _Info}, State) ->
    {_, NewState} = send_timeout_dead(Pid, State),
    {keep_state, NewState};

'HANDLE'(info, {ReplyRef, _}, _) when is_reference(ReplyRef) ->
    % gen_server:call/3 had a timeout exception that was caught but the
    % reply arrived later and must be discarded
    keep_state_and_data;

'HANDLE'(info, EventContent, State) ->
    handle_info(EventContent, ?FUNCTION_NAME, State);

'HANDLE'(EventType, EventContent, State) ->
    Event = {EventType, EventContent},
    {stop, {?FUNCTION_NAME, undefined_message, Event}, State}.

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
format_status(_Opt, [_PDict, _StateName, State]) ->
    [{data, [{"State", State}]}].
-else.
format_status(_Opt,
              [_PDict, _StateName,
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
                       services_format_options_external(ConfigOptions),
    [{data,
      [{"State",
        State#state{send_timeouts = maps:to_list(SendTimeouts),
                    send_timeout_monitors = maps:to_list(SendTimeoutMonitors),
                    recv_timeouts = maps:to_list(RecvTimeouts),
                    async_responses = maps:to_list(AsyncResponses),
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
                         PriorityDefault),
              State),
    ok.

connection_init(#state{dispatcher = Dispatcher,
                       protocol = Protocol} = State) ->
    ok = os_init(State),
    if
        Protocol =:= udp ->
            ok = send('keepalive_out'(), State),
            erlang:send_after(?KEEPALIVE_UDP, Dispatcher, keepalive_udp);
        true ->
            ok
    end,
    {next_state, 'HANDLE', State}.

handle_info({udp, Socket, _, IncomingPort, _} = UDP, StateName,
            #state{protocol = udp,
                   incoming_port = undefined,
                   socket = Socket} = State) ->
    handle_info(UDP, StateName,
                State#state{incoming_port = IncomingPort});

handle_info({udp, Socket, _, IncomingPort, Data}, StateName,
            #state{protocol = udp,
                   incoming_port = IncomingPort,
                   socket = Socket} = State) ->
    inet:setopts(Socket, [{active, once}]),
    try erlang:binary_to_term(Data, [safe]) of
        Request ->
            ?MODULE:StateName(connection, Request, State)
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
        Request ->
            ?MODULE:StateName(connection, Request, State)
    catch
        error:badarg ->
            ?LOG_ERROR("Protocol Error ~p", [Data]),
            {stop, {error, protocol}, State}
    end;

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

handle_info({cloudi_cpg_data, Groups}, _,
            #state{dispatcher = Dispatcher,
                   dest_refresh = DestRefresh,
                   options = #config_service_options{
                       dest_refresh_delay = Delay,
                       scope = Scope}} = State) ->
    destination_refresh(DestRefresh, Dispatcher, Delay, Scope),
    {keep_state, State#state{cpg_data = Groups}};

handle_info({'EXIT', _, Reason}, _, _) ->
    {stop, Reason};

handle_info('cloudi_service_init_timeout', _, _) ->
    {stop, timeout};

handle_info({'cloudi_service_update', UpdatePending, _}, _,
            #state{dispatcher = Dispatcher,
                   update_plan = undefined}) ->
    UpdatePending ! {'cloudi_service_update', Dispatcher,
                     {error, invalid_state}},
    keep_state_and_data;

handle_info(EventContent, StateName, State) ->
    Event = {info, EventContent},
    {stop, {StateName, undefined_message, Event}, State}.


handle_send_async(Name, RequestInfo, Request, Timeout, Priority,
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
            keep_state_and_data;
        {error, _} when RequestNameLookup =:= async ->
            ok = send('return_async_out'(), State),
            keep_state_and_data;
        {error, _} when Timeout >= ?SEND_ASYNC_INTERVAL ->
            erlang:send_after(?SEND_ASYNC_INTERVAL, Dispatcher,
                              {'cloudi_service_send_async_retry',
                               Name, RequestInfo, Request,
                               Timeout - ?SEND_ASYNC_INTERVAL, Priority}),
            keep_state_and_data;
        {error, _} ->
            ok = send('return_async_out'(), State),
            keep_state_and_data;
        {ok, Pattern, Pid} ->
            {TransId, NewUUID} = cloudi_x_uuid:get_v1(UUID),
            Pid ! {'cloudi_service_send_async',
                   Name, Pattern, RequestInfo, Request,
                   Timeout, Priority, TransId, Dispatcher},
            ok = send('return_async_out'(TransId), State),
            {keep_state,
             send_async_timeout_start(Timeout, TransId, Pid,
                                      State#state{uuid_generator = NewUUID})}
    end.

handle_send_sync(Name, RequestInfo, Request, Timeout, Priority,
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
            keep_state_and_data;
        {error, _} when RequestNameLookup =:= async ->
            ok = send('return_sync_out'(), State),
            keep_state_and_data;
        {error, _} when Timeout >= ?SEND_SYNC_INTERVAL ->
            erlang:send_after(?SEND_SYNC_INTERVAL, Dispatcher,
                              {'cloudi_service_send_sync_retry',
                               Name, RequestInfo, Request,
                               Timeout - ?SEND_SYNC_INTERVAL, Priority}),
            keep_state_and_data;
        {error, _} ->
            ok = send('return_sync_out'(), State),
            keep_state_and_data;
        {ok, Pattern, Pid} ->
            {TransId, NewUUID} = cloudi_x_uuid:get_v1(UUID),
            Pid ! {'cloudi_service_send_sync',
                   Name, Pattern, RequestInfo, Request,
                   Timeout, Priority, TransId, Dispatcher},
            {keep_state,
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
    NewState = State#state{uuid_generator = NewUUID},
    handle_mcast_async_pids(Name, Pattern, RequestInfo, Request,
                            Timeout, Priority,
                            [TransId | TransIdList], PidList,
                            send_async_timeout_start(Timeout,
                                                     TransId,
                                                     Pid,
                                                     NewState)).

handle_mcast_async(Name, RequestInfo, Request, Timeout, Priority,
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
            keep_state_and_data;
        {error, _} when RequestNameLookup =:= async ->
            ok = send('returns_async_out'(), State),
            keep_state_and_data;
        {error, _} when Timeout >= ?MCAST_ASYNC_INTERVAL ->
            erlang:send_after(?MCAST_ASYNC_INTERVAL, Dispatcher,
                              {'cloudi_service_mcast_async_retry',
                               Name, RequestInfo, Request,
                               Timeout - ?MCAST_ASYNC_INTERVAL, Priority}),
            keep_state_and_data;
        {error, _} ->
            ok = send('returns_async_out'(), State),
            keep_state_and_data;
        {ok, Pattern, PidList} ->
            {keep_state,
             handle_mcast_async_pids(Name, Pattern, RequestInfo, Request,
                                     Timeout, Priority,
                                     [], PidList, State)}
    end.

'init_out'(ProcessIndex, ProcessCount,
           ProcessCountMax, ProcessCountMin, [PrefixC | _] = Prefix,
           TimeoutInit, TimeoutAsync, TimeoutSync, TimeoutTerm,
           PriorityDefault)
    when is_integer(ProcessIndex), is_integer(ProcessCount),
         is_integer(ProcessCountMax), is_integer(ProcessCountMin),
         is_integer(PrefixC), is_integer(TimeoutInit),
         is_integer(TimeoutAsync), is_integer(TimeoutSync),
         is_integer(TimeoutTerm), is_integer(PriorityDefault),
         PriorityDefault >= ?PRIORITY_HIGH, PriorityDefault =< ?PRIORITY_LOW ->
    true = ProcessCount < 4294967296,
    true = ProcessCountMax < 4294967296,
    true = ProcessCountMin < 4294967296,
    PrefixBin = erlang:list_to_binary(Prefix),
    PrefixSize = erlang:byte_size(PrefixBin) + 1,
    true = PrefixSize < 4294967296,
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
      PriorityDefault:8/signed-integer-native>>.

'reinit_out'(ProcessCount, TimeoutAsync, TimeoutSync,
             PriorityDefault)
    when is_integer(ProcessCount), is_integer(TimeoutAsync),
         is_integer(TimeoutSync), is_integer(PriorityDefault),
         PriorityDefault >= ?PRIORITY_HIGH, PriorityDefault =< ?PRIORITY_LOW ->
    true = ProcessCount < 4294967296,
    <<?MESSAGE_REINIT:32/unsigned-integer-native,
      ProcessCount:32/unsigned-integer-native,
      TimeoutAsync:32/unsigned-integer-native,
      TimeoutSync:32/unsigned-integer-native,
      PriorityDefault:8/signed-integer-native>>.

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
        recv_timeouts = maps:put(TransId,
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
                                       ServiceState) of
                {ok, NewServiceState} ->
                    RecvTimer = maps:get(TransId, RecvTimeouts),
                    Timeout = case erlang:cancel_timer(RecvTimer) of
                        false ->
                            0;
                        V ->    
                            V       
                    end,
                    T = {'cloudi_service_send_async',
                         Name, Pattern, RequestInfo, Request,
                         Timeout, Priority, TransId, Source},
                    RequestTimeoutF =
                        request_timeout_adjustment_f(RequestTimeoutAdjustment),
                    ok = send('send_async_out'(Name, Pattern,
                                               RequestInfo, Request,
                                               Timeout, Priority, TransId,
                                               Source),
                              State),
                    State#state{recv_timeouts = maps:remove(TransId,
                                                            RecvTimeouts),
                                queued = NewQueue,
                                queued_size = QueuedSize - Size,
                                service_state = NewServiceState,
                                request_data = {T, RequestTimeoutF},
                                options = NewConfigOptions};
                {stop, Reason, NewServiceState} ->
                    Dispatcher ! {'EXIT', Dispatcher, Reason},
                    State#state{service_state = NewServiceState,
                                options = NewConfigOptions}
            catch
                ?STACKTRACE(ErrorType, Error, ErrorStackTrace)
                    ?LOG_ERROR("request ~p ~p~n~p",
                               [ErrorType, Error, ErrorStackTrace]),
                    Reason = {ErrorType, {Error, ErrorStackTrace}},
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
                                       ServiceState) of
                {ok, NewServiceState} ->
                    RecvTimer = maps:get(TransId, RecvTimeouts),
                    Timeout = case erlang:cancel_timer(RecvTimer) of
                        false ->
                            0;
                        V ->    
                            V       
                    end,
                    T = {'cloudi_service_send_sync',
                         Name, Pattern, RequestInfo, Request,
                         Timeout, Priority, TransId, Source},
                    RequestTimeoutF =
                        request_timeout_adjustment_f(RequestTimeoutAdjustment),
                    ok = send('send_sync_out'(Name, Pattern,
                                              RequestInfo, Request,
                                              Timeout, Priority, TransId,
                                              Source),
                              State),
                    State#state{recv_timeouts = maps:remove(TransId,
                                                            RecvTimeouts),
                                queued = NewQueue,
                                queued_size = QueuedSize - Size,
                                service_state = NewServiceState,
                                request_data = {T, RequestTimeoutF},
                                options = NewConfigOptions};
                {stop, Reason, NewServiceState} ->
                    Dispatcher ! {'EXIT', Dispatcher, Reason},
                    State#state{service_state = NewServiceState,
                                options = NewConfigOptions}
            catch
                ?STACKTRACE(ErrorType, Error, ErrorStackTrace)
                    ?LOG_ERROR("request ~p ~p~n~p",
                               [ErrorType, Error, ErrorStackTrace]),
                    Reason = {ErrorType, {Error, ErrorStackTrace}},
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
                       priority_default = PriorityDefault}
                   } = NextState,
            ok = send('reinit_out'(ProcessCount,
                                   TimeoutAsync, TimeoutSync,
                                   PriorityDefault), NextState),
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

aspects_request_before([], _, _, _, _, _, _, _, _, _, ServiceState) ->
    {ok, ServiceState};
aspects_request_before([{M, F} | L], Type, Name, Pattern, RequestInfo, Request,
                       Timeout, Priority, TransId, Source,
                       ServiceState) ->
    case M:F(Type, Name, Pattern, RequestInfo, Request,
             Timeout, Priority, TransId, Source,
             ServiceState) of
        {ok, NewServiceState} ->
            aspects_request_before(L, Type, Name, Pattern, RequestInfo, Request,
                                   Timeout, Priority, TransId, Source,
                                   NewServiceState);
        {stop, _, _} = Stop ->
            Stop
    end;
aspects_request_before([F | L], Type, Name, Pattern, RequestInfo, Request,
                       Timeout, Priority, TransId, Source,
                       ServiceState) ->
    case F(Type, Name, Pattern, RequestInfo, Request,
           Timeout, Priority, TransId, Source,
           ServiceState) of
        {ok, NewServiceState} ->
            aspects_request_before(L, Type, Name, Pattern, RequestInfo, Request,
                                   Timeout, Priority, TransId, Source,
                                   NewServiceState);
        {stop, _, _} = Stop ->
            Stop
    end.

aspects_request_after([], _, _, _, _, _, _, _, _, _, _, ServiceState) ->
    {ok, ServiceState};
aspects_request_after([{M, F} | L], Type, Name, Pattern, RequestInfo, Request,
                      Timeout, Priority, TransId, Source,
                      Result, ServiceState) ->
    case M:F(Type, Name, Pattern, RequestInfo, Request,
             Timeout, Priority, TransId, Source,
             Result, ServiceState) of
        {ok, NewServiceState} ->
            aspects_request_after(L, Type, Name, Pattern, RequestInfo, Request,
                                  Timeout, Priority, TransId, Source,
                                  Result, NewServiceState);
        {stop, _, _} = Stop ->
            Stop
    end;
aspects_request_after([F | L], Type, Name, Pattern, RequestInfo, Request,
                      Timeout, Priority, TransId, Source,
                      Result, ServiceState) ->
    case F(Type, Name, Pattern, RequestInfo, Request,
           Timeout, Priority, TransId, Source,
           Result, ServiceState) of
        {ok, NewServiceState} ->
            aspects_request_after(L, Type, Name, Pattern, RequestInfo, Request,
                                  Timeout, Priority, TransId, Source,
                                  Result, NewServiceState);
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

