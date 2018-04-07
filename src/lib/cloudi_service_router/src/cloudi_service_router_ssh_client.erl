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

-define(DEFAULT_HOST_NAME,                    undefined).
-define(DEFAULT_PASSWORD,                            "").

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
-define(TIMEOUT_SEND_MIN, 1000). % milliseconds
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
        {user_dir,
         cloudi_service_router_ssh_server:config_user_dir(SSH)},
        {system_dir,
         cloudi_service_router_ssh_server:config_system_dir(SSH)},
        {password,                      ?DEFAULT_PASSWORD}],
    [HostNameRaw, Port, Inet, UserDirRaw, SystemDirRaw,
     PasswordRaw] = cloudi_proplists:take_values(Defaults, Options),
    true = is_list(HostNameRaw) andalso is_integer(hd(HostNameRaw)),
    true = is_integer(Port) andalso (Port > 0),
    true = is_list(UserDirRaw) andalso is_integer(hd(UserDirRaw)),
    true = is_list(SystemDirRaw) andalso is_integer(hd(SystemDirRaw)),
    true = is_list(PasswordRaw) andalso is_integer(hd(PasswordRaw)),
    HostName = cloudi_environment:transform(HostNameRaw, EnvironmentLookup),
    UserDir = cloudi_environment:transform(UserDirRaw, EnvironmentLookup),
    SystemDir = cloudi_environment:transform(SystemDirRaw, EnvironmentLookup),
    Password = cloudi_environment:transform(PasswordRaw, EnvironmentLookup),
    ClientOptions0 = if
        Inet =:= inet; Inet =:= inet6 ->
            [{inet, Inet}];
        Inet =:= undefined ->
            []
    end,
    SilentlyAcceptHostsF = fun(PeerName, FingerPrint) ->
        ?MODULE:silently_accept_hosts(PeerName, FingerPrint, SystemDir)
    end,
    ClientOptionsN = [{user_dir, UserDir},
                      {silently_accept_hosts, SilentlyAcceptHostsF},
                      {password, Password} | ClientOptions0],

    % The ssh application and its dependencies are only started
    % if they are necessary for the cloudi_service_router processes
    ok = ssh:start(),

    {ok, Pid} = gen_server:start_link(?MODULE,
                                      [HostName, Port, ClientOptionsN], []),
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
    {ok, KeyData} = file:read_file(KeyFileName),
    [{Key, _}] = public_key:ssh_decode(KeyData, public_key),
    public_key:ssh_hostkey_fingerprint(Key) == FingerPrint.

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([HostName, Port, ClientOptions]) ->
    {ok, #ssh_client_connection{host_name = HostName,
                                port = Port,
                                options = ClientOptions}}.

handle_call({forward,
             Type, Name, Pattern, NewName, RequestInfo, Request,
             Timeout, Priority, TransId, Source}, _, State) ->
    {Reply,
     NewState} = handle_forward(Type, Name, Pattern, NewName,
                                RequestInfo, Request,
                                Timeout, Priority, TransId, Source, State),
    {reply, Reply, NewState};
handle_call(Request, _, State) ->
    {stop, cloudi_string:format("Unknown call \"~p\"~n", [Request]), State}.

handle_cast(Request, State) ->
    {stop, cloudi_string:format("Unknown cast \"~p\"~n", [Request]), State}.

handle_info(Request, State) ->
    {stop, cloudi_string:format("Unknown info \"~p\"~n", [Request]), State}.

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
    DataOut = erlang:term_to_binary(DataOutDecoded),
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
            handle_forward(Type, Name, Pattern, NewName,
                           RequestInfo, Request,
                           Timeout, Priority, TransId, Source,
                           State#ssh_client_connection{handle = undefined})
    end;
handle_forward(Type, Name, Pattern, NewName, RequestInfo, Request,
               Timeout, Priority, TransId, Source,
               #ssh_client_connection{
                   host_name = HostName,
                   port = Port,
                   options = Options,
                   handle = undefined} = State) ->
    case ssh:connect(HostName, Port, Options) of
        {ok, Connection} ->
            case ssh_connection:session_channel(Connection, infinity) of
                {ok, ChannelId} ->
                    SubSystem = ?SSH_SUBSYSTEM,
                    case ssh_connection:subsystem(Connection, ChannelId,
                                                  SubSystem, infinity) of
                        success ->
                            ConnectionHandle = {Connection, ChannelId},
                            handle_forward(Type, Name, Pattern, NewName,
                                           RequestInfo, Request,
                                           Timeout, Priority, TransId, Source,
                                           State#ssh_client_connection{
                                               handle = ConnectionHandle});
                        failure ->
                            ?LOG_DEBUG("subsystem(~s) ~s:~w failure",
                                       [SubSystem, HostName, Port]),
                            ok = ssh_connection:close(Connection, ChannelId),
                            {timeout, State};
                        {error, Reason} ->
                            ?LOG_DEBUG("subsystem(~s) ~s:~w error: ~p",
                                       [SubSystem, HostName, Port, Reason]),
                            ok = ssh_connection:close(Connection, ChannelId),
                            {timeout, State}
                    end;
                {error, Reason} ->
                    ok = ssh:close(Connection),
                    ?LOG_DEBUG("channel ~s:~w error: ~p",
                               [HostName, Port, Reason]),
                    {timeout, State}
            end;
        {error, Reason} ->
            ?LOG_DEBUG("connect ~s:~w error: ~p",
                       [HostName, Port, Reason]),
            {timeout, State}
    end.

