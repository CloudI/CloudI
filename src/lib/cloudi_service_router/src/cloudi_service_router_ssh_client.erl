%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Router Service SSH Client==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2018 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2018 Michael Truog
%%% @version 1.7.4 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_router_ssh_client).
-author('mjtruog at protonmail dot com').

-behaviour(gen_server).

%% external interface
-export([forward/11,
         new/3]).

%% internal callbacks
-export([silently_accept_hosts/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include("cloudi_service_router_ssh.hrl").

-define(DEFAULT_HOST_NAME,              undefined).
-define(DEFAULT_USER,                   "${USER}").
-define(DEFAULT_PASSWORD,                      "").
-define(DEFAULT_DSA_PASSPHRASE,         undefined).
-define(DEFAULT_RSA_PASSPHRASE,         undefined).
-define(DEFAULT_ECDSA_PASSPHRASE,       undefined).

% XXX switch to the opaque types when ssh_connection functions get fixed
-type connection_handle() :: {ssh:ssh_connection_ref() | pid(),
                              ssh:ssh_channel_id() | pos_integer()}.

-record(ssh_client,
    {
        process :: pid()
    }).

-record(ssh_client_connection,
    {
        host_name :: string(),
        port :: pos_integer(),
        options :: list(),
        compression :: 0..9,
        handle = undefined :: undefined | connection_handle()
    }).

-type state() :: #ssh_client{}.
-type options() :: list({host_name, string()} |
                        {port, pos_integer()} |
                        {inet, inet | inet6 | undefined} |
                        {user_dir, string()} |
                        {system_dir, string()}).
-export_type([state/0,
              options/0]).

-define(TIMEOUT_DELTA, 100). % milliseconds
-define(TIMEOUT_SEND_MIN, 100). % milliseconds (>= FORWARD_DELTA)
-define(TIMEOUT_RECONNECT_MIN, 5000). % milliseconds

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-spec forward(Type :: cloudi_service:request_type(),
              Name :: cloudi_service:service_name(),
              Pattern :: cloudi_service:service_name_pattern(),
              NewName :: cloudi_service:service_name(),
              RequestInfo :: cloudi_service:request_info(),
              Request :: cloudi_service:request(),
              Timeout :: cloudi_service:timeout_value_milliseconds(),
              Priority :: cloudi_service:priority_value(),
              TransId :: cloudi_service:trans_id(),
              Source :: cloudi_service:source(),
              State :: state()) ->
    ok | timeout.

forward(Type, Name, Pattern, NewName, RequestInfo, Request,
        Timeout, Priority, TransId, Source,
        #ssh_client{process = Pid}) ->
    try gen_server:call(Pid,
                        {forward,
                         Type, Name, Pattern, NewName, RequestInfo, Request,
                         Timeout, Priority, TransId, Source},
                        Timeout + ?TIMEOUT_DELTA)
    catch
        exit:{_, _} ->
            timeout
    end.

-spec new(Options :: options(),
          EnvironmentLookup :: cloudi_environment:lookup(),
          SSH :: cloudi_service_router_ssh_server:state() | undefined) ->
    state() | undefined.

new(Options, EnvironmentLookup, SSH)
    when is_list(Options) ->
    Defaults = [
        {host_name,                     ?DEFAULT_HOST_NAME},
        {port,
         cloudi_service_router_ssh_server:config_port(SSH)},
        {inet,
         cloudi_service_router_ssh_server:config_inet(SSH)},
        {compression,
         cloudi_service_router_ssh_server:config_compression(SSH)},
        {user_dir,
         cloudi_service_router_ssh_server:config_user_dir(SSH)},
        {system_dir,
         cloudi_service_router_ssh_server:config_system_dir(SSH)},
        {user,                          ?DEFAULT_USER},
        {password,                      ?DEFAULT_PASSWORD},
        {dsa_passphrase,                ?DEFAULT_DSA_PASSPHRASE},
        {rsa_passphrase,                ?DEFAULT_RSA_PASSPHRASE},
        {ecdsa_passphrase,              ?DEFAULT_ECDSA_PASSPHRASE}],
    [HostNameRaw, Port, Inet, Compression, UserDirRaw, SystemDirRaw,
     UserRaw, PasswordRaw, DSAPassphraseRaw, RSAPassphraseRaw,
     ECDSAPassphraseRaw] = cloudi_proplists:take_values(Defaults, Options),
    true = is_list(HostNameRaw) andalso is_integer(hd(HostNameRaw)),
    true = is_integer(Port) andalso (Port > 0),
    true = is_integer(Compression) andalso
           (Compression >= 0) andalso (Compression =< 9),
    true = is_list(UserDirRaw) andalso is_integer(hd(UserDirRaw)),
    true = is_list(SystemDirRaw) andalso is_integer(hd(SystemDirRaw)),
    true = is_list(UserRaw) andalso is_integer(hd(UserRaw)),
    true = is_list(PasswordRaw),
    HostName = cloudi_environment:transform(HostNameRaw, EnvironmentLookup),
    UserDir = cloudi_environment:transform(UserDirRaw, EnvironmentLookup),
    SystemDir = cloudi_environment:transform(SystemDirRaw, EnvironmentLookup),
    User = cloudi_environment:transform(UserRaw, EnvironmentLookup),
    Password = cloudi_environment:transform(PasswordRaw, EnvironmentLookup),
    ClientOptions0 = if
        Inet =:= inet; Inet =:= inet6 ->
            [{inet, Inet}];
        Inet =:= undefined ->
            []
    end,
    ClientOptions1 = if
        DSAPassphraseRaw =:= undefined ->
            ClientOptions0;
        is_list(DSAPassphraseRaw) andalso
        is_integer(hd(DSAPassphraseRaw)) ->
            [{dsa_pass_phrase,
              cloudi_environment:
              transform(DSAPassphraseRaw, EnvironmentLookup)} |
             ClientOptions0]
    end,
    ClientOptions2 = if
        RSAPassphraseRaw =:= undefined ->
            ClientOptions1;
        is_list(RSAPassphraseRaw) andalso
        is_integer(hd(RSAPassphraseRaw)) ->
            [{rsa_pass_phrase,
              cloudi_environment:
              transform(RSAPassphraseRaw, EnvironmentLookup)} |
             ClientOptions1]
    end,
    ClientOptions3 = if
        ECDSAPassphraseRaw =:= undefined ->
            ClientOptions2;
        is_list(ECDSAPassphraseRaw) andalso
        is_integer(hd(ECDSAPassphraseRaw)) ->
            [{ecdsa_pass_phrase,
              cloudi_environment:
              transform(ECDSAPassphraseRaw, EnvironmentLookup)} |
             ClientOptions2]
    end,
    SilentlyAcceptHostsF = fun(PeerName, FingerPrint) ->
        ?MODULE:silently_accept_hosts(PeerName, FingerPrint, SystemDir)
    end,
    ClientOptionsN = [{user_interaction, false},
                      {user_dir, UserDir},
                      {silently_accept_hosts, SilentlyAcceptHostsF},
                      {user, User},
                      {password, Password} | ClientOptions3],

    % The ssh application and its dependencies are only started
    % if they are necessary for the cloudi_service_router processes
    ok = ssh:start(),

    {ok, Pid} = gen_server:start_link(?MODULE,
                                      [HostName, Port, ClientOptionsN,
                                       Compression], []),
    #ssh_client{process = Pid}.

%%%------------------------------------------------------------------------
%%% Callback functions used internally
%%%------------------------------------------------------------------------

-spec silently_accept_hosts(PeerName :: string(),
                            FingerPrint :: string(),
                            SystemDir :: string()) ->
    boolean().

silently_accept_hosts(_PeerName, FingerPrint, SystemDir) ->
    KeyFileName = filename:join(SystemDir, "ssh_host_rsa_key.pub"),
    case filelib:is_file(KeyFileName) of
        true ->
            {ok, KeyData} = file:read_file(KeyFileName),
            [{Key, _}] = public_key:ssh_decode(KeyData, public_key),
            public_key:ssh_hostkey_fingerprint(Key) == FingerPrint;
        false ->
            false
    end.

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([HostName, Port, ClientOptions, Compression]) ->
    {ok, #ssh_client_connection{host_name = HostName,
                                port = Port,
                                options = ClientOptions,
                                compression = Compression}}.

handle_call({forward,
             Type, Name, Pattern, NewName, RequestInfo, Request,
             Timeout, Priority, TransId, Source}, _, State) ->
    {Reply,
     NewState} = handle_forward(Type, Name, Pattern, NewName,
                                RequestInfo, Request,
                                Timeout, Priority, TransId, Source, State),
    {reply, Reply, NewState};
handle_call(Request, _, State) ->
    {stop, cloudi_string:format("Unknown call \"~w\"", [Request]), State}.

handle_cast(Request, State) ->
    {stop, cloudi_string:format("Unknown cast \"~w\"", [Request]), State}.

handle_info({ssh_cm, Connection,
             {closed, ChannelId}},
            #ssh_client_connection{handle = {Connection, ChannelId}} = State) ->
    {noreply, State#ssh_client_connection{handle = undefined}};
handle_info({ssh_cm, Connection,
             {data, ChannelId, 0, DataIn}},
            #ssh_client_connection{handle = {Connection, ChannelId}} = State) ->
    try erlang:binary_to_term(DataIn) of
        {ReturnType,
         _NewName, _NewPattern, _ResponseInfo, _Response,
         _NewTimeout, _TransId, Source} = DataInDecoded
        when ReturnType =:= 'cloudi_service_return_async' orelse
             ReturnType =:= 'cloudi_service_return_sync' ->
            Source ! DataInDecoded,
            ssh_connection:adjust_window(Connection, ChannelId,
                                         byte_size(DataIn)),
            {noreply, State}
    catch
        error:badarg ->
            ssh_connection:close(Connection, ChannelId),
            ?LOG_ERROR("received invalid data, connection closed", []),
            {noreply, State#ssh_client_connection{handle = undefined}}
    end;
handle_info({ssh_cm, Connection,
             {data, ChannelId, 1, DataIn}},
            #ssh_client_connection{handle = {Connection, ChannelId}} = State) ->
    ?LOG_ERROR("~s", [DataIn]),
    {noreply, State};
handle_info({ssh_cm, _, _}, State) ->
    {noreply, State};
handle_info(Request, State) ->
    {stop, cloudi_string:format("Unknown info \"~w\"", [Request]), State}.

terminate(_, #ssh_client_connection{handle = ConnectionHandle}) ->
    case ConnectionHandle of
        {Connection, ChannelId} ->
            ssh_connection:close(Connection, ChannelId);
        undefined ->
            ok
    end,
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

handle_forward(_, _, _, _, _, _, Timeout, _, _, _, State)
    when Timeout < ?TIMEOUT_SEND_MIN ->
    {timeout, State};
handle_forward(Type, Name, Pattern, NewName, RequestInfo, Request,
               Timeout, Priority, TransId, Source,
               #ssh_client_connection{
                   host_name = HostName,
                   port = Port,
                   options = Options,
                   handle = undefined} = State) ->
    ConnectTimeStart = cloudi_timestamp:milliseconds_monotonic(),
    case ssh:connect(HostName, Port, Options) of
        {ok, Connection} ->
            case ssh_connection:session_channel(Connection, infinity) of
                {ok, ChannelId} ->
                    Subsystem = ?SSH_SUBSYSTEM,
                    case ssh_connection:subsystem(Connection, ChannelId,
                                                  Subsystem, infinity) of
                        success ->
                            ConnectTime =
                                cloudi_timestamp:milliseconds_monotonic() - 
                                ConnectTimeStart,
                            NewTimeout = if
                                ConnectTime >= Timeout ->
                                    0;
                                true ->
                                    Timeout - ConnectTime
                            end,
                            ConnectionHandle = {Connection, ChannelId},
                            handle_forward(Type, Name, Pattern, NewName,
                                           RequestInfo, Request,
                                           NewTimeout, Priority,
                                           TransId, Source,
                                           State#ssh_client_connection{
                                               handle = ConnectionHandle});
                        failure ->
                            ?LOG_DEBUG("subsystem(~s) ~s:~w failure",
                                       [Subsystem, HostName, Port]),
                            ok = ssh_connection:close(Connection, ChannelId),
                            {timeout, State};
                        {error, Reason} ->
                            ?LOG_DEBUG("subsystem(~s) ~s:~w error: ~w",
                                       [Subsystem, HostName, Port, Reason]),
                            ok = ssh_connection:close(Connection, ChannelId),
                            {timeout, State}
                    end;
                {error, Reason} ->
                    ok = ssh:close(Connection),
                    ?LOG_DEBUG("channel ~s:~w error: ~w",
                               [HostName, Port, Reason]),
                    {timeout, State}
            end;
        {error, Reason} ->
            ?LOG_DEBUG("connect ~s:~w error: ~w",
                       [HostName, Port, Reason]),
            {timeout, State}
    end;
handle_forward(Type, Name, Pattern, NewName, RequestInfo, Request,
               Timeout, Priority, TransId, Source,
               #ssh_client_connection{
                   compression = Compression,
                   handle = {Connection, ChannelId}} = State) ->
    ForwardType = if
        Type =:= 'send_async' ->
            'cloudi_service_forward_async_retry';
        Type =:= 'send_sync' ->
            'cloudi_service_forward_sync_retry'
    end,
    DataOutDecoded = {ForwardType, Name, Pattern, NewName,
                      RequestInfo, Request,
                      Timeout, Priority, TransId, Source},
    DataOut = erlang:term_to_binary(DataOutDecoded,
                                    [{compressed, Compression}]),
    case ssh_connection:send(Connection, ChannelId,
                             DataOut, Timeout) of
        ok ->
            {ok, State};
        {error, timeout} ->
            NewState = if
                Timeout >= ?TIMEOUT_RECONNECT_MIN ->
                    ok = ssh_connection:close(Connection, ChannelId),
                    State#ssh_client_connection{handle = undefined};
                true ->
                    State
            end,
            {timeout, NewState};
        {error, closed} ->
            {timeout, State#ssh_client_connection{handle = undefined}}
    end.

