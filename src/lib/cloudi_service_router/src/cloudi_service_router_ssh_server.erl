%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Router Service SSH Server==
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

-module(cloudi_service_router_ssh_server).
-author('mjtruog at protonmail dot com').

-behaviour(ssh_daemon_channel).

%% external interface
-export([config_compression/1,
         config_inet/1,
         config_port/1,
         config_system_dir/1,
         config_user_dir/1,
         destroy/1,
         new/3]).

%% ssh_daemon_channel callbacks
-export([init/1, handle_ssh_msg/2, handle_msg/2, terminate/2]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include("cloudi_service_router_ssh.hrl").

-define(DEFAULT_IP,                   {127,0,0,1}). % interface ip address
-define(DEFAULT_PORT,                        4368).
-define(DEFAULT_INET,                   undefined).
-define(DEFAULT_COMPRESSION,                    0). % zlib compression 0..9
-define(DEFAULT_USER_DIR,               undefined).
-define(DEFAULT_SYSTEM_DIR,             undefined).
-define(DEFAULT_USER_PASSWORDS,         undefined).
        % If passwords are used, after the public key check fails,
        % the password must have an entry in this list, for the appropriate
        % user.  Environment variables may be used for the username and the
        % password, so it is necessary to use "\\$" to represent a "$" symbol
        % if it is present in the password.
        %
        % Example:
        % [{"${USER}", "badpassword"}]

% XXX switch to the opaque types when ssh_connection functions get fixed
-type connection_handle() :: {ssh:ssh_connection_ref() | pid(),
                              ssh:ssh_channel_id() | pos_integer()}.

-record(ssh_server,
    {
        config_compression :: 0..9,
        config_inet :: inet | inet6 | undefined,
        config_port :: pos_integer(),
        config_system_dir :: string(),
        config_user_dir :: string(),
        process = undefined :: undefined | pid()
    }).

-record(ssh_server_connection,
    {
        dispatcher :: cloudi_service:dispatcher(),
        compression :: 0..9,
        handle = undefined :: undefined | connection_handle()
    }).

-type state() :: #ssh_server{}.
-type options() :: list({ip, inet:ip_address() | any | loopback} |
                        {port, pos_integer()} |
                        {inet, inet | inet6 | undefined} |
                        {user_dir, string()} |
                        {system_dir, string()}).
-export_type([state/0,
              options/0]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-spec config_compression(#ssh_server{} | undefined) ->
    0..9.

config_compression(undefined) ->
    ?DEFAULT_COMPRESSION;
config_compression(#ssh_server{config_compression = Compression}) ->
    Compression.

-spec config_inet(#ssh_server{} | undefined) ->
    inet | inet6 | undefined.

config_inet(undefined) ->
    ?DEFAULT_INET;
config_inet(#ssh_server{config_inet = Inet}) ->
    Inet.

-spec config_port(#ssh_server{} | undefined) ->
    pos_integer().

config_port(undefined) ->
    ?DEFAULT_PORT;
config_port(#ssh_server{config_port = Port}) ->
    Port.

-spec config_system_dir(#ssh_server{} | undefined) ->
    string() | undefined.

config_system_dir(undefined) ->
    ?DEFAULT_SYSTEM_DIR;
config_system_dir(#ssh_server{config_system_dir = SystemDir}) ->
    SystemDir.

-spec config_user_dir(#ssh_server{} | undefined) ->
    string() | undefined.

config_user_dir(undefined) ->
    ?DEFAULT_USER_DIR;
config_user_dir(#ssh_server{config_user_dir = UserDir}) ->
    UserDir.

-spec destroy(state() | undefined) ->
    ok.

destroy(undefined) ->
    ok;
destroy(#ssh_server{process = undefined}) ->
    ok;
destroy(#ssh_server{process = Pid}) ->
    ssh:stop_daemon(Pid),
    ok.

-spec new(Options :: options() | undefined,
          EnvironmentLookup :: cloudi_environment:lookup(),
          Dispatcher :: cloudi_service:dispatcher()) ->
    state() | undefined.

new(undefined, _, _) ->
    undefined;
new(Options, EnvironmentLookup, Dispatcher)
    when is_list(Options) ->
    Defaults = [
        {ip,                            ?DEFAULT_IP},
        {port,                          ?DEFAULT_PORT},
        {inet,                          ?DEFAULT_INET},
        {compression,                   ?DEFAULT_COMPRESSION},
        {user_dir,                      ?DEFAULT_USER_DIR},
        {system_dir,                    ?DEFAULT_SYSTEM_DIR},
        {user_passwords,                ?DEFAULT_USER_PASSWORDS}],
    [IP, Port, Inet, Compression, UserDirRaw, SystemDirRaw, UserPasswordsRaw
     ] = cloudi_proplists:take_values(Defaults, Options),
    true = is_tuple(IP) orelse (IP =:= any) orelse (IP =:= loopback),
    true = is_integer(Port) andalso (Port > 0),
    true = is_integer(Compression) andalso
           (Compression >= 0) andalso (Compression =< 9),
    true = is_list(UserDirRaw) andalso is_integer(hd(UserDirRaw)),
    true = is_list(SystemDirRaw) andalso is_integer(hd(SystemDirRaw)),
    UserDir = cloudi_environment:transform(UserDirRaw, EnvironmentLookup),
    SystemDir = cloudi_environment:transform(SystemDirRaw, EnvironmentLookup),

    State = #ssh_server{config_compression = Compression,
                        config_inet = Inet,
                        config_port = Port,
                        config_system_dir = SystemDirRaw,
                        config_user_dir = UserDirRaw},
    case cloudi_service:process_index(Dispatcher) of
        0 ->
            % The ssh application and its dependencies are only started
            % if they are necessary for the cloudi_service_router processes
            ok = ssh:start(),

            DaemonOptions0 = if
                Inet =:= inet; Inet =:= inet6 ->
                    [{inet, Inet}];
                Inet =:= undefined ->
                    []
            end,
            DaemonOptions1 = if
                UserPasswordsRaw =:= undefined ->
                    DaemonOptions0;
                is_list(UserPasswordsRaw) ->
                    [{user_passwords,
                      [{cloudi_environment:transform(UserRaw,
                                                     EnvironmentLookup),
                        cloudi_environment:transform(PasswordRaw,
                                                     EnvironmentLookup)}
                       || {UserRaw, PasswordRaw} <- UserPasswordsRaw]} |
                     DaemonOptions0]

            end,
            DaemonOptionsN = [{auth_methods, "publickey,password"},
                              {user_dir, UserDir},
                              {system_dir, SystemDir} | DaemonOptions1],
            SubsystemArgs = [cloudi_service:dispatcher(Dispatcher),
                             Compression],
            {ok, Pid} = ssh:daemon(IP, Port,
                                   [{subsystems,
                                     [{?SSH_SUBSYSTEM,
                                       {?MODULE, SubsystemArgs}}]} |
                                    DaemonOptionsN]),
            State#ssh_server{process = Pid};
        _ ->
            State
    end.

%%%------------------------------------------------------------------------
%%% Callback functions from ssh_daemon_channel
%%%------------------------------------------------------------------------

init([Dispatcher, Compression]) ->
    {ok, #ssh_server_connection{dispatcher = Dispatcher,
                                compression = Compression}}.

handle_ssh_msg({ssh_cm, Connection,
                {data, ChannelId, 0, DataIn}},
               #ssh_server_connection{
                   dispatcher = Dispatcher,
                   compression = Compression,
                   handle = {Connection, ChannelId}} = State) ->
    try erlang:binary_to_term(DataIn) of
        {ForwardType, Name, Pattern, NextName, RequestInfo, Request,
         Timeout, Priority, TransId, Source}
        when ForwardType =:= 'cloudi_service_forward_async_retry' orelse
             ForwardType =:= 'cloudi_service_forward_sync_retry' ->
            Self = self(),
            Dispatcher ! {ForwardType,
                          Name, Pattern, NextName, RequestInfo, Request,
                          Timeout, Priority, TransId, Self},
            DataOutDecoded = receive
                {'cloudi_service_return_async',
                 NewName, NewPattern, ResponseInfo, Response,
                 NewTimeout, TransId, Self} ->
                    {'cloudi_service_return_async',
                     NewName, NewPattern, ResponseInfo, Response,
                     NewTimeout, TransId, Source};
                {'cloudi_service_return_sync',
                 NewName, NewPattern, ResponseInfo, Response,
                 NewTimeout, TransId, Self} ->
                    {'cloudi_service_return_sync',
                     NewName, NewPattern, ResponseInfo, Response,
                     NewTimeout, TransId, Source}
            after
                Timeout ->
                    if
                        ForwardType =:= 'cloudi_service_forward_async_retry' ->
                            {'cloudi_service_return_async',
                             Name, Pattern, <<>>, <<>>, 0, TransId, Source};
                        ForwardType =:= 'cloudi_service_forward_sync_retry' ->
                            {'cloudi_service_return_sync',
                             Name, Pattern, <<>>, <<>>, 0, TransId, Source}
                    end
            end,
            DataOut = erlang:term_to_binary(DataOutDecoded,
                                            [{compressed, Compression}]),
            case ssh_connection:send(Connection, ChannelId, DataOut) of
                ok ->
                    {ok, State};
                {error, Reason} ->
                    ?LOG_DEBUG("send error: ~w", [Reason]),
                    {stop, ChannelId, State}
            end
    catch
        error:badarg ->
            ssh_connection:send_eof(Connection, ChannelId),
            {stop, ChannelId, State}
    end;
handle_ssh_msg({ssh_cm, Connection,
                {data, ChannelId, 1, DataIn}},
               #ssh_server_connection{
                   handle = {Connection, ChannelId}} = State) ->
    ?LOG_ERROR("~s", [DataIn]),
    {ok, State};
handle_ssh_msg({ssh_cm, Connection,
                {eof, ChannelId}},
               #ssh_server_connection{
                   handle = {Connection, ChannelId}} = State) ->
    {stop, ChannelId, State};
handle_ssh_msg({ssh_cm, _, {signal, _, _}}, State) ->
    % Ignore signals because of RFC 4254 section 6.9
    {ok, State};
handle_ssh_msg({ssh_cm, _, {exit_signal, ChannelId, _, _Error, _}}, State) ->
    {stop, ChannelId, State};
handle_ssh_msg({ssh_cm, _, {exit_status, ChannelId, _Status}}, State) ->
    {stop, ChannelId, State}.

handle_msg({ssh_channel_up, ChannelId, Connection}, State) ->
    {ok, State#ssh_server_connection{handle = {Connection, ChannelId}}};
handle_msg({ReturnType, _, _, _, _, _, _, _}, State)
    when ReturnType =:= 'cloudi_service_return_async' orelse
         ReturnType =:= 'cloudi_service_return_sync' ->
    % Ignore old timeouts that have already occurred
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

