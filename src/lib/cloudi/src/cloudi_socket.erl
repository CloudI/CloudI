%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Cloudi Socket==
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
%%% @version 0.1.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_socket).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_fsm).

%% external interface
-export([start_link/9, port/1]).

%% gen_fsm callbacks
-export([init/1, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

%% FSM States
-export(['CONNECT'/2,
         'HANDLE'/2]).

-include("cloudi_logger.hrl").
-include("cloudi_constants.hrl").

% message type enumeration
-define(MESSAGE_INIT,            1).
-define(MESSAGE_SEND_ASYNC,      2).
-define(MESSAGE_SEND_SYNC,       3).
-define(MESSAGE_RECV_ASYNC,      4).
-define(MESSAGE_RETURN_ASYNC,    5).
-define(MESSAGE_RETURN_SYNC,     6).

-record(state,
    {
        protocol,        % tcp or udp
        port,            % port number used
        incoming_port,   % udp incoming port
        listener,        % tcp listener
        acceptor,        % tcp acceptor
        socket,          % data socket
        prefix,          % subscribe/unsubscribe name prefix
        timeout_async,   % default timeout for send_async
        timeout_sync,    % default timeout for send_sync
        send_timeouts = dict:new(),    % tracking for timeouts
        async_responses = dict:new(),  % tracking for async messages
        uuid_generator,  % transaction id generator
        dest_refresh,    % immediate_closest |
                         % lazy_closest |
                         % immediate_random |
                         % lazy_random, destination pid refresh
        list_pg_data = list_pg_data:get_empty_groups(), % dest_refresh lazy
        dest_deny,       % is the socket denied from sending to a destination
        dest_allow       % is the socket allowed to send to a destination
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

start_link(Protocol, BufferSize, Timeout, Prefix,
           TimeoutAsync, TimeoutSync, DestRefresh, DestDeny, DestAllow)
    when is_integer(BufferSize), is_integer(Timeout), is_list(Prefix),
         is_integer(TimeoutAsync), is_integer(TimeoutSync) ->
    true = (Protocol == tcp) or (Protocol == udp),
    true = (DestRefresh == immediate_closest) or
           (DestRefresh == lazy_closest) or
           (DestRefresh == immediate_random) or
           (DestRefresh == lazy_random),
    gen_fsm:start_link(?MODULE, [Protocol, BufferSize, Timeout, Prefix,
                                 TimeoutAsync, TimeoutSync, DestRefresh,
                                 DestDeny, DestAllow], []).

port(Process) when is_pid(Process) ->
    gen_fsm:sync_send_all_state_event(Process, port).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_fsm
%%%------------------------------------------------------------------------

init([tcp, BufferSize, Timeout, Prefix,
      TimeoutAsync, TimeoutSync, DestRefresh, DestDeny, DestAllow]) ->
    process_flag(trap_exit, true),
    Opts = [binary, {ip, {127,0,0,1}},
            {recbuf, BufferSize}, {sndbuf, BufferSize},
            {packet, 0}, {nodelay, true}, {delay_send, false},
            {keepalive, false}, {backlog, 0},
            {active, false}],
    case gen_tcp:listen(0, Opts) of
        {ok, Listener} ->
            {ok, Port} = inet:port(Listener),
            {ok, Acceptor} = prim_inet:async_accept(Listener, -1),
            destination_refresh_start(DestRefresh),
            {ok, 'CONNECT', #state{protocol = tcp,
                                   port = Port,
                                   listener = Listener,
                                   acceptor = Acceptor,
                                   prefix = Prefix,
                                   timeout_async = TimeoutAsync,
                                   timeout_sync = TimeoutSync,
                                   uuid_generator = uuid:new(self()),
                                   dest_refresh = DestRefresh,
                                   dest_deny = DestDeny,
                                   dest_allow = DestAllow}, Timeout};
        {error, Reason} ->
            {stop, Reason}
    end;

init([udp, BufferSize, Timeout, Prefix,
      TimeoutAsync, TimeoutSync, DestRefresh, DestDeny, DestAllow]) ->
    process_flag(trap_exit, true),
    Opts = [binary, {ip, {127,0,0,1}},
            {recbuf, BufferSize}, {sndbuf, BufferSize},
            {active, once}],
    case gen_udp:open(0, Opts) of
        {ok, Socket} ->
            {ok, Port} = inet:port(Socket),
            destination_refresh_start(DestRefresh),
            {ok, 'CONNECT', #state{protocol = udp,
                                   port = Port,
                                   socket = Socket,
                                   prefix = Prefix,
                                   timeout_async = TimeoutAsync,
                                   timeout_sync = TimeoutSync,
                                   uuid_generator = uuid:new(self()),
                                   dest_refresh = DestRefresh,
                                   dest_deny = DestDeny,
                                   dest_allow = DestAllow}, Timeout};
        {error, Reason} ->
            {stop, Reason}
    end.

% incoming messages (from the port socket to other Erlang pids)

'CONNECT'(init, #state{prefix = Prefix,
                       timeout_async = TimeoutAsync,
                       timeout_sync = TimeoutSync} = StateData) ->
    send('init_out'(Prefix, TimeoutAsync, TimeoutSync), StateData),
    {next_state, 'HANDLE', StateData};

'CONNECT'(timeout, StateData) ->
    {stop, normal, StateData};

'CONNECT'(Request, StateData) ->
    {stop, {'CONNECT', undefined_message, Request}, StateData}.

'HANDLE'({'subscribe', Name},
         #state{prefix = Prefix} = StateData) ->
    list_pg:join(Prefix ++ Name, self()),
    {next_state, 'HANDLE', StateData};

'HANDLE'({'unsubscribe', Name},
         #state{prefix = Prefix} = StateData) ->
    list_pg:leave(Prefix ++ Name, self()),
    {next_state, 'HANDLE', StateData};

'HANDLE'({'send_async', Name, Request, Timeout},
         #state{uuid_generator = UUID,
                dest_refresh = DestRefresh,
                list_pg_data = Groups,
                dest_deny = DestDeny,
                dest_allow = DestAllow} = StateData) ->
    case destination_allowed(Name, DestDeny, DestAllow) of
        true ->
            Self = self(),
            case destination_get(DestRefresh, Name, Self, Groups) of
                {error, _} when Timeout >= ?SEND_ASYNC_INTERVAL ->
                    erlang:send_after(?SEND_ASYNC_INTERVAL, Self,
                                      {'send_async', Name, Request,
                                       Timeout - ?SEND_ASYNC_INTERVAL}),
                    {next_state, 'HANDLE', StateData};
                {error, _} ->
                    send('return_async_out'(), StateData),
                    {next_state, 'HANDLE', StateData};
                Pid ->
                    TransId = uuid:get_v1(UUID),
                    Pid ! {'send_async', Name, Request, Timeout, TransId, Self},
                    send('return_async_out'(TransId), StateData),
                    {next_state, 'HANDLE', send_async_timeout_start(Timeout,
                                                                    TransId,
                                                                    StateData)}
            end;
        false ->
            send('return_async_out'(), StateData),
            {next_state, 'HANDLE', StateData}
    end;

'HANDLE'({'send_sync', Name, Request, Timeout},
         #state{uuid_generator = UUID,
                dest_refresh = DestRefresh,
                list_pg_data = Groups,
                dest_deny = DestDeny,
                dest_allow = DestAllow} = StateData) ->
    case destination_allowed(Name, DestDeny, DestAllow) of
        true ->
            Self = self(),
            case destination_get(DestRefresh, Name, Self, Groups) of
                {error, _} when Timeout >= ?SEND_SYNC_INTERVAL ->
                    erlang:send_after(?SEND_SYNC_INTERVAL, Self,
                                      {'send_sync', Name, Request,
                                       Timeout - ?SEND_SYNC_INTERVAL}),
                    {next_state, 'HANDLE', StateData};
                {error, _} ->
                    send('return_sync_out'(), StateData),
                    {next_state, 'HANDLE', StateData};
                Pid ->
                    TransId = uuid:get_v1(UUID),
                    Pid ! {'send_sync', Name, Request, Timeout, TransId, Self},
                    {next_state, 'HANDLE', send_sync_timeout_start(Timeout,
                                                                   TransId,
                                                                   StateData)}
            end;
        false ->
            send('return_sync_out'(), StateData),
            {next_state, 'HANDLE', StateData}
    end;

'HANDLE'({'forward_async', Name, Request, Timeout, TransId, Pid},
         #state{dest_refresh = DestRefresh,
                list_pg_data = Groups,
                dest_deny = DestDeny,
                dest_allow = DestAllow} = StateData) ->
    case destination_allowed(Name, DestDeny, DestAllow) of
        true ->
            case destination_get(DestRefresh, Name, Pid, Groups) of
                {error, _} when Timeout >= ?FORWARD_ASYNC_INTERVAL ->
                    erlang:send_after(?FORWARD_ASYNC_INTERVAL, self(),
                                      {'forward_async', Name, Request,
                                       Timeout - ?FORWARD_ASYNC_INTERVAL,
                                       TransId, Pid}),
                    ok;
                {error, _} ->
                    ok;
                NextPid ->
                    NextPid ! {'send_async', Name, Request,
                               Timeout, TransId, Pid}
            end;
        false ->
            ok
    end,
    {next_state, 'HANDLE', StateData};

'HANDLE'({'forward_sync', Name, Request, Timeout, TransId, Pid},
         #state{dest_refresh = DestRefresh,
                list_pg_data = Groups,
                dest_deny = DestDeny,
                dest_allow = DestAllow} = StateData) ->
    case destination_allowed(Name, DestDeny, DestAllow) of
        true ->
            case destination_get(DestRefresh, Name, Pid, Groups) of
                {error, _} when Timeout >= ?FORWARD_SYNC_INTERVAL ->
                    erlang:send_after(?FORWARD_SYNC_INTERVAL, self(),
                                      {'forward_sync', Name, Request,
                                       Timeout - ?FORWARD_SYNC_INTERVAL,
                                       TransId, Pid}),
                    ok;
                {error, _} ->
                    ok;
                NextPid ->
                    NextPid ! {'send_sync', Name, Request,
                               Timeout, TransId, Pid}
            end;
        false ->
            ok
    end,
    {next_state, 'HANDLE', StateData};

'HANDLE'({'recv_async', Timeout, TransId},
         #state{async_responses = AsyncResponses} = StateData) ->
    if
        TransId == <<0:128>> ->
            case dict:to_list(AsyncResponses) of
                [] when Timeout >= ?RECV_ASYNC_INTERVAL ->
                    erlang:send_after(?RECV_ASYNC_INTERVAL, self(),
                                      {'recv_async',
                                       Timeout - ?RECV_ASYNC_INTERVAL,
                                       TransId}),
                    {next_state, 'HANDLE', StateData};
                [] ->
                    send('recv_async_out'(timeout, TransId), StateData),
                    {next_state, 'HANDLE', StateData};
                [{TransIdUsed, Response} | _] ->
                    NewAsyncResponses = dict:erase(TransIdUsed, AsyncResponses),
                    send('recv_async_out'(Response, TransIdUsed), StateData),
                    {next_state, 'HANDLE',
                     StateData#state{async_responses = NewAsyncResponses}}
            end;
        true ->
            case dict:find(TransId, AsyncResponses) of
                error when Timeout >= ?RECV_ASYNC_INTERVAL ->
                    erlang:send_after(?RECV_ASYNC_INTERVAL, self(),
                                      {'recv_async',
                                       Timeout - ?RECV_ASYNC_INTERVAL,
                                       TransId}),
                    {next_state, 'HANDLE', StateData};
                error ->
                    send('recv_async_out'(timeout, TransId), StateData),
                    {next_state, 'HANDLE', StateData};
                {ok, Response} ->
                    NewAsyncResponses = dict:erase(TransId, AsyncResponses),
                    send('recv_async_out'(Response, TransId), StateData),
                    {next_state, 'HANDLE',
                     StateData#state{async_responses = NewAsyncResponses}}
            end
    end;

'HANDLE'({'return_async', _Name, _Response, _Timeout, _TransId, Pid} = T,
         StateData) ->
    Pid ! T,
    {next_state, 'HANDLE', StateData};

'HANDLE'({'return_sync', _Name, _Response, _Timeout, _TransId, Pid} = T,
         StateData) ->
    Pid ! T,
    {next_state, 'HANDLE', StateData};

'HANDLE'(Request, StateData) ->
    {stop, {'HANDLE', undefined_message, Request}, StateData}.

handle_sync_event(port, _, StateName, #state{port = Port} = StateData) ->
    {reply, Port, StateName, StateData};

handle_sync_event(Event, _From, StateName, StateData) ->
    ?LOG_WARNING("Unknown event \"~p\"", [Event]),
    {stop, {StateName, undefined_event, Event}, StateData}.

handle_event(Event, StateName, StateData) ->
    ?LOG_WARNING("Unknown event \"~p\"", [Event]),
    {stop, {StateName, undefined_event, Event}, StateData}.

handle_info({udp, Socket, _, Port, Data}, StateName,
            #state{protocol = udp,
                   incoming_port = Port,
                   socket = Socket} = StateData) ->
    inet:setopts(Socket, [{active, once}]),
    try ?MODULE:StateName(erlang:binary_to_term(Data), StateData)
    catch
        error:badarg ->
            ?LOG_ERROR("Protocol Error ~p", [Data]),
            {stop, {error, protocol}, StateData}
    end;

handle_info({udp, Socket, _, Port, Data}, StateName,
            #state{protocol = udp,
                   socket = Socket} = StateData) ->
    inet:setopts(Socket, [{active, once}]),
    try ?MODULE:StateName(erlang:binary_to_term(Data),
                          StateData#state{incoming_port = Port})
    catch
        error:badarg ->
            ?LOG_ERROR("Protocol Error ~p", [Data]),
            {stop, {error, protocol}, StateData}
    end;

handle_info({udp_closed, Socket}, _,
            #state{protocol = udp,
                   socket = Socket} = StateData) ->
    {stop, normal, StateData};

handle_info({tcp, Socket, Data}, StateName,
            #state{protocol = tcp,
                   socket = Socket} = StateData) ->
    inet:setopts(Socket, [{active, once}]),
    try ?MODULE:StateName(erlang:binary_to_term(Data), StateData)
    catch
        error:badarg ->
            ?LOG_ERROR("Protocol Error ~p", [Data]),
            {stop, {error, protocol}, StateData}
    end;

handle_info({tcp_closed, Socket}, _,
            #state{protocol = tcp,
                   socket = Socket} = StateData) ->
    {stop, normal, StateData};

handle_info({inet_async, Listener, Acceptor, {ok, Socket}}, StateName,
            #state{protocol = tcp,
                   listener = Listener,
                   acceptor = Acceptor} = StateData) ->
    true = inet_db:register_socket(Socket, inet_tcp),
    CopyOpts = [recbuf, sndbuf, nodelay, keepalive, delay_send, priority, tos],
    {ok, Opts} = prim_inet:getopts(Listener, CopyOpts),
    ok = prim_inet:setopts(Socket, [{active, once} | Opts]),
    catch gen_tcp:close(Listener),
    {next_state, StateName, StateData#state{listener = undefined,
                                            acceptor = undefined,
                                            socket = Socket}};

handle_info({inet_async, Listener, Acceptor, Error}, StateName,
            #state{protocol = tcp,
                   listener = Listener,
                   acceptor = Acceptor} = StateData) ->
    {stop, {StateName, inet_async, Error}, StateData};

handle_info({list_pg_data, Groups}, StateName,
            #state{dest_refresh = DestRefresh} = StateData) ->
    destination_refresh_start(DestRefresh),
    {next_state, StateName, StateData#state{list_pg_data = Groups}};

handle_info({'send_async', _, _, _} = T, StateName, StateData) ->
    ?MODULE:StateName(T, StateData);

handle_info({'send_sync', _, _, _} = T, StateName, StateData) ->
    ?MODULE:StateName(T, StateData);

handle_info({'forward_async', _, _, _, _} = T, StateName, StateData) ->
    ?MODULE:StateName(T, StateData);

handle_info({'forward_sync', _, _, _, _} = T, StateName, StateData) ->
    ?MODULE:StateName(T, StateData);

handle_info({'recv_async', _, _} = T, StateName, StateData) ->
    ?MODULE:StateName(T, StateData);

% outgoing messages (from Erlang pids to the port socket)

handle_info({'send_async', Name, Request, Timeout, TransId, Pid},
            StateName, StateData) ->
    send('send_async_out'(Name, Request, Timeout, TransId, Pid), StateData),
    {next_state, StateName, StateData};

handle_info({'send_sync', Name, Request, Timeout, TransId, Pid},
            StateName, StateData) ->
    send('send_sync_out'(Name, Request, Timeout, TransId, Pid), StateData),
    {next_state, StateName, StateData};

handle_info({'return_async', _Name, Response, Timeout, TransId, Pid},
            StateName, StateData) ->
    true = Pid == self(),
    case send_timeout_check(TransId, StateData) of
        error ->
            % send_async timeout already occurred
            {next_state, StateName, StateData};
        {ok, Tref} when Response == <<>> ->
            erlang:cancel_timer(Tref),
            {next_state, StateName, send_timeout_end(TransId, StateData)};
        {ok, Tref} ->
            erlang:cancel_timer(Tref),
            {next_state, StateName,
             recv_async_timeout_start(Response, Timeout, TransId,
                                      send_timeout_end(TransId, StateData))}
    end;

handle_info({'return_sync', _Name, Response, _Timeout, TransId, Pid},
            StateName, StateData) ->
    true = Pid == self(),
    case send_timeout_check(TransId, StateData) of
        error ->
            % send_sync timeout already occurred
            {next_state, StateName, StateData};
        {ok, Tref} ->
            erlang:cancel_timer(Tref),
            send('return_sync_out'(Response, TransId), StateData),
            {next_state, StateName, send_timeout_end(TransId, StateData)}
    end;

handle_info({'send_async_timeout', TransId}, StateName, StateData) ->
    case send_timeout_check(TransId, StateData) of
        error ->
            % should never happen, timer should have been cancelled
            % if the send_async already returned
            %XXX
            {next_state, StateName, StateData};
        {ok, _} ->
            {next_state, StateName, send_timeout_end(TransId, StateData)}
    end;

handle_info({'send_sync_timeout', TransId}, StateName, StateData) ->
    case send_timeout_check(TransId, StateData) of
        error ->
            % should never happen, timer should have been cancelled
            % if the send_sync already returned
            %XXX
            {next_state, StateName, StateData};
        {ok, _} ->
            send('return_sync_out'(timeout, TransId), StateData),
            {next_state, StateName, send_timeout_end(TransId, StateData)}
    end;

handle_info({'recv_async_timeout', TransId}, StateName, StateData) ->
    {next_state, StateName, recv_async_timeout_end(TransId, StateData)};

handle_info(Request, StateName, StateData) ->
    ?LOG_WARNING("Unknown info \"~p\"", [Request]),
    {next_state, StateName, StateData}.

terminate(_, _, #state{protocol = tcp,
                       listener = Listener,
                       socket = Socket}) ->
    catch gen_tcp:close(Listener),
    catch gen_tcp:close(Socket),
    ok;

terminate(_, _, #state{protocol = udp,
                       socket = Socket}) ->
    catch gen_udp:close(Socket),
    ok.

code_change(_, StateName, StateData, _) ->
    {ok, StateName, StateData}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

'init_out'(Prefix, TimeoutAsync, TimeoutSync)
    when is_list(Prefix), is_integer(TimeoutAsync), is_integer(TimeoutSync) ->
    PrefixBin = erlang:list_to_binary(Prefix),
    PrefixSize = erlang:byte_size(PrefixBin) + 1,
    <<?MESSAGE_INIT:32/unsigned-integer-native,
      PrefixSize:32/unsigned-integer-native,
      PrefixBin/binary, 0,
      TimeoutAsync:32/unsigned-integer-native,
      TimeoutSync:32/unsigned-integer-native>>.

'send_async_out'(Name, Request, Timeout, TransId, Pid)
    when is_list(Name), is_binary(Request), is_integer(Timeout),
         is_binary(TransId), is_pid(Pid) ->
    NameBin = erlang:list_to_binary(Name),
    NameSize = erlang:byte_size(NameBin) + 1,
    RequestSize = erlang:byte_size(Request),
    PidBin = erlang:term_to_binary(Pid),
    PidSize = erlang:byte_size(PidBin),
    <<?MESSAGE_SEND_ASYNC:32/unsigned-integer-native,
      NameSize:32/unsigned-integer-native,
      NameBin/binary, 0,
      RequestSize:32/unsigned-integer-native,
      Request/binary,
      Timeout:32/unsigned-integer-native,
      TransId/binary,       % 128 bits
      PidSize:32/unsigned-integer-native,
      PidBin/binary>>.

'send_sync_out'(Name, Request, Timeout, TransId, Pid)
    when is_list(Name), is_binary(Request), is_integer(Timeout),
         is_binary(TransId), is_pid(Pid) ->
    NameBin = erlang:list_to_binary(Name),
    NameSize = erlang:byte_size(NameBin) + 1,
    RequestSize = erlang:byte_size(Request),
    PidBin = erlang:term_to_binary(Pid),
    PidSize = erlang:byte_size(PidBin),
    <<?MESSAGE_SEND_SYNC:32/unsigned-integer-native,
      NameSize:32/unsigned-integer-native,
      NameBin/binary, 0,
      RequestSize:32/unsigned-integer-native,
      Request/binary,
      Timeout:32/unsigned-integer-native,
      TransId/binary,       % 128 bits
      PidSize:32/unsigned-integer-native,
      PidBin/binary>>.

'return_async_out'() ->
    <<?MESSAGE_RETURN_ASYNC:32/unsigned-integer-native,
      0:128>>.              % 128 bits

'return_async_out'(TransId)
    when is_binary(TransId) ->
    <<?MESSAGE_RETURN_ASYNC:32/unsigned-integer-native,
      TransId/binary>>.     % 128 bits

'return_sync_out'() ->
    <<?MESSAGE_RETURN_SYNC:32/unsigned-integer-native,
      0:32,
      0:128>>.              % 128 bits

'return_sync_out'(timeout, TransId)
    when is_binary(TransId) ->
    <<?MESSAGE_RETURN_SYNC:32/unsigned-integer-native,
      0:32,
      TransId/binary>>;     % 128 bits

'return_sync_out'(Response, TransId)
    when is_binary(Response), is_binary(TransId) ->
    ResponseSize = erlang:byte_size(Response),
    <<?MESSAGE_RETURN_SYNC:32/unsigned-integer-native,
      ResponseSize:32/unsigned-integer-native,
      Response/binary,
      TransId/binary>>.     % 128 bits

'recv_async_out'(timeout, TransId)
    when is_binary(TransId) ->
    <<?MESSAGE_RECV_ASYNC:32/unsigned-integer-native,
      0:32,
      TransId/binary>>;     % 128 bits

'recv_async_out'(Response, TransId)
    when is_binary(Response), is_binary(TransId) ->
    ResponseSize = erlang:byte_size(Response),
    <<?MESSAGE_RECV_ASYNC:32/unsigned-integer-native,
      ResponseSize:32/unsigned-integer-native,
      Response/binary,
      TransId/binary>>.     % 128 bits

send(Data, #state{protocol = Protocol,
                  incoming_port = Port,
                  socket = Socket}) when is_binary(Data) ->
    if
        Protocol == tcp ->
            ok = gen_tcp:send(Socket, Data);
        Protocol == udp ->
            ok = gen_udp:send(Socket, {127,0,0,1}, Port, Data)
    end.

destination_allowed(_, undefined, undefined) ->
    true;

destination_allowed(Name, undefined, DestAllow) ->
    Prefix = string2:beforer($/, Name),
    trie:is_prefix(Prefix, DestAllow);

destination_allowed(Name, DestDeny, undefined) ->
    Prefix = string2:beforer($/, Name),
    not trie:is_prefix(Prefix, DestDeny);

destination_allowed(Name, DestDeny, DestAllow) ->
    Prefix = string2:beforer($/, Name),
    case trie:is_prefix(Prefix, DestDeny) of
        true ->
            false;
        false ->
            trie:is_prefix(Prefix, DestAllow)
    end.

destination_refresh_start(lazy_closest) ->
    list_pg_data:get_groups(?DEST_REFRESH_SLOW);

destination_refresh_start(lazy_random) ->
    list_pg_data:get_groups(?DEST_REFRESH_SLOW);

destination_refresh_start(immediate_closest) ->
    ok;

destination_refresh_start(immediate_random) ->
    ok.

destination_get(lazy_closest, Name, Pid, Groups)
    when is_list(Name) ->
    list_pg_data:get_closest_pid(Name, Pid, Groups);

destination_get(lazy_random, Name, Pid, Groups)
    when is_list(Name) ->
    list_pg_data:get_random_pid(Name, Pid, Groups);

destination_get(immediate_closest, Name, Pid, _)
    when is_list(Name) ->
    list_pg:get_closest_pid(Name, Pid);

destination_get(immediate_random, Name, Pid, _)
    when is_list(Name) ->
    list_pg:get_random_pid(Name, Pid).

send_async_timeout_start(Timeout, TransId,
                         #state{send_timeouts = Ids} = StateData)
    when is_integer(Timeout), is_binary(TransId) ->
    Tref = erlang:send_after(Timeout, self(), {'send_async_timeout', TransId}),
    StateData#state{send_timeouts = dict:store(TransId, Tref, Ids)}.

send_sync_timeout_start(Timeout, TransId,
                        #state{send_timeouts = Ids} = StateData)
    when is_integer(Timeout), is_binary(TransId) ->
    Tref = erlang:send_after(Timeout, self(), {'send_sync_timeout', TransId}),
    StateData#state{send_timeouts = dict:store(TransId, Tref, Ids)}.

send_timeout_check(TransId, #state{send_timeouts = Ids})
    when is_binary(TransId) ->
    dict:find(TransId, Ids).

send_timeout_end(TransId, #state{send_timeouts = Ids} = StateData)
    when is_binary(TransId) ->
    StateData#state{send_timeouts = dict:erase(TransId, Ids)}.

recv_async_timeout_start(Response, Timeout, TransId,
                         #state{async_responses = Ids} = StateData)
    when is_binary(Response), is_integer(Timeout), is_binary(TransId) ->
    erlang:send_after(Timeout, self(), {'recv_async_timeout', TransId}),
    StateData#state{async_responses = dict:store(TransId, Response, Ids)}.

recv_async_timeout_end(TransId,
                       #state{async_responses = Ids} = StateData)
    when is_binary(TransId) ->
    StateData#state{async_responses = dict:erase(TransId, Ids)}.

