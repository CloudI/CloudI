%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Spawn==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2011-2022 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2011-2022 Michael Truog
%%% @version 2.0.5 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_core_i_spawn).
-author('mjtruog at protonmail dot com').

%% external interface
-export([start_internal/18,
         start_external/21,
         status_internal/4,
         status_external/6,
         update_external/3,
         update_internal_f/9,
         update_external_f/12]).

-include("cloudi_logger.hrl").
-include("cloudi_core_i_configuration.hrl").
-include("cloudi_core_i_constants.hrl").

-define(PROCESS_START_INTERNAL,
        cloudi_core_i_services_internal_sup:process_start).
-define(PROCESS_START_EXTERNAL,
        cloudi_core_i_services_external_sup:process_start).

% environmental variables used by CloudI API initialization
-define(ENVIRONMENT_THREAD_COUNT,  "CLOUDI_API_INIT_THREAD_COUNT").
-define(ENVIRONMENT_PROTOCOL,      "CLOUDI_API_INIT_PROTOCOL").
-define(ENVIRONMENT_BUFFER_SIZE,   "CLOUDI_API_INIT_BUFFER_SIZE").

-ifdef(CLOUDI_CORE_STANDALONE).
-dialyzer({no_match,
           [start_external_spawn/20,
            update_external/3]}).
-endif.

-type error_reason_start_internal() ::
    {service_internal_module_not_loaded, any()}.
-type error_reason_start_external() ::
    {service_external_file_path_invalid_expanded |
     service_external_file_path_invalid_unicode |
     service_external_args_invalid_quotes |
     service_external_args_invalid_unicode |
     service_external_env_invalid_expanded |
     service_external_env_invalid_unicode, any()}.
-export_type([error_reason_start_internal/0,
              error_reason_start_external/0]).

% Service execution arguments that are constant across all
% service instance processes (used for each start/restart).
% Some argument values could be explicitly changed with an update.
-type arguments_execution_internal() ::
    nonempty_list(pid() | % GroupLeader
                  atom() | % Module
                  cloudi_service_api:args_internal() | % Args
                  cloudi_service_api:
                  timeout_initialize_milliseconds() | % Timeout
                  cloudi:service_name_pattern() | % Prefix
                  cloudi_service_api:
                  timeout_send_async_value_milliseconds() | % TimeoutAsync
                  cloudi_service_api:
                  timeout_send_sync_value_milliseconds() | % TimeoutSync
                  cloudi_service_api:
                  timeout_terminate_milliseconds() | % TimeoutTerm
                  cloudi_service_api:dest_refresh() | % DestRefresh
                  list(cloudi:service_name_pattern()) |
                  undefined | % DestListDeny
                  list(cloudi:service_name_pattern()) |
                  undefined | % DestListAllow
                  #config_service_options{} | % ConfigOptions
                  cloudi_service_api:service_id()). % ID
-type arguments_execution_external() ::
    nonempty_list(pos_integer() | % ThreadsPerProcess
                  cloudi_service_api:file_path() | % Filename
                  cloudi_service_api:args_external() | % Arguments
                  cloudi_service_api:env_external() | % Environment
                  'local' | 'tcp' | 'udp' | % Protocol
                  pos_integer() | % BufferSize
                  cloudi_service_api:
                  timeout_initialize_milliseconds() | % Timeout
                  cloudi:service_name_pattern() | % Prefix
                  cloudi_service_api:
                  timeout_send_async_value_milliseconds() | % TimeoutAsync
                  cloudi_service_api:
                  timeout_send_sync_value_milliseconds() | % TimeoutSync
                  cloudi_service_api:
                  timeout_terminate_milliseconds() | % TimeoutTerm
                  cloudi_service_api:dest_refresh() | % DestRefresh
                  list(cloudi:service_name_pattern()) |
                  undefined | % DestListDeny
                  list(cloudi:service_name_pattern()) |
                  undefined | % DestListAllow
                  #config_service_options{} | % ConfigOptions
                  cloudi_service_api:service_id()). % ID
-type arguments_execution() ::
    arguments_execution_internal() |
    arguments_execution_external().
-export_type([arguments_execution_internal/0,
              arguments_execution_external/0,
              arguments_execution/0]).

%%%------------------------------------------------------------------------
%%% External interface
%%%------------------------------------------------------------------------

-spec start_internal(ProcessIndex :: non_neg_integer(),
                     ProcessCount :: pos_integer(),
                     TimeStart :: cloudi_timestamp:native_monotonic(),
                     TimeRestart :: undefined |
                                    cloudi_timestamp:native_monotonic(),
                     Restarts :: non_neg_integer(),
                     GroupLeader :: pid(),
                     Module :: atom(),
                     Args :: list(),
                     Timeout ::
                         cloudi_service_api:timeout_initialize_milliseconds(),
                     Prefix :: cloudi:service_name_pattern(),
                     TimeoutAsync ::
                         cloudi_service_api:
                         timeout_send_async_value_milliseconds(),
                     TimeoutSync ::
                         cloudi_service_api:
                         timeout_send_sync_value_milliseconds(),
                     TimeoutTerm ::
                         cloudi_service_api:timeout_terminate_milliseconds(),
                     DestRefresh :: cloudi_service_api:dest_refresh(),
                     DestListDeny :: list(cloudi:service_name_pattern()) |
                                     undefined,
                     DestListAllow :: list(cloudi:service_name_pattern()) |
                                      undefined,
                     ConfigOptions :: #config_service_options{},
                     ID :: cloudi_service_api:service_id()) ->
    {ok, cloudi_service:source()} |
    {error, error_reason_start_internal() | any()}.

start_internal(ProcessIndex, ProcessCount, TimeStart, TimeRestart, Restarts,
               GroupLeader, Module, Args, Timeout, Prefix,
               TimeoutAsync, TimeoutSync, TimeoutTerm, DestRefresh,
               DestListDeny, DestListAllow, ConfigOptions, ID)
    when is_integer(ProcessIndex), is_integer(ProcessCount),
         is_integer(TimeStart), is_integer(Restarts),
         is_atom(Module), is_list(Args), is_integer(Timeout), is_list(Prefix),
         is_integer(TimeoutAsync), is_integer(TimeoutSync),
         is_integer(TimeoutTerm),
         is_record(ConfigOptions, config_service_options),
         is_binary(ID) ->
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
    DestDeny = if
        DestListDeny =:= undefined ->
            undefined;
        is_list(DestListDeny) ->
            cloudi_x_trie:new(DestListDeny)
    end,
    DestAllow = if
        DestListAllow =:= undefined ->
            undefined;
        is_list(DestListAllow) ->
            cloudi_x_trie:new(DestListAllow)
    end,
    case cloudi_x_reltool_util:is_module_loaded(Module, Timeout) of
        {ok, TimeoutNew} ->
            % Erlang application startup is asynchronous, so wait for the
            % module to be loaded or timeout
            ?PROCESS_START_INTERNAL(ProcessIndex, ProcessCount,
                                    TimeStart, TimeRestart, Restarts,
                                    GroupLeader, Module, Args,
                                    TimeoutNew, Prefix,
                                    TimeoutAsync, TimeoutSync, TimeoutTerm,
                                    DestRefresh, DestDeny, DestAllow,
                                    ConfigOptions, ID);
        {error, Reason} ->
            ?LOG_ERROR("loading ~p failed: ~tp", [Module, Reason]),
            {error, {service_internal_module_not_loaded, Module}}
    end.

-spec start_external(ProcessIndex :: non_neg_integer(),
                     ProcessCount :: pos_integer(),
                     TimeStart :: cloudi_timestamp:native_monotonic(),
                     TimeRestart :: undefined |
                                    cloudi_timestamp:native_monotonic(),
                     Restarts :: non_neg_integer(),
                     ThreadsPerProcess :: pos_integer(),
                     Filename :: cloudi_service_api:file_path(),
                     Arguments :: cloudi_service_api:args_external(),
                     Environment :: cloudi_service_api:env_external(),
                     Protocol :: 'local' | 'tcp' | 'udp',
                     BufferSize :: pos_integer(),
                     Timeout ::
                         cloudi_service_api:timeout_initialize_milliseconds(),
                     Prefix :: cloudi:service_name_pattern(),
                     TimeoutAsync ::
                         cloudi_service_api:
                         timeout_send_async_value_milliseconds(),
                     TimeoutSync ::
                         cloudi_service_api:
                         timeout_send_sync_value_milliseconds(),
                     TimeoutTerm ::
                         cloudi_service_api:timeout_terminate_milliseconds(),
                     DestRefresh :: cloudi_service_api:dest_refresh(),
                     DestListDeny :: list(cloudi:service_name_pattern()) |
                                     undefined,
                     DestListAllow :: list(cloudi:service_name_pattern()) |
                                      undefined,
                     ConfigOptions :: #config_service_options{},
                     ID :: cloudi_service_api:service_id()) ->
    {ok, nonempty_list(cloudi_service:source())} |
    {error, error_reason_start_external() | any()}.

start_external(ProcessIndex, ProcessCount, TimeStart, TimeRestart, Restarts,
               ThreadsPerProcess, Filename, Arguments, Environment,
               Protocol, BufferSize, Timeout, Prefix,
               TimeoutAsync, TimeoutSync, TimeoutTerm, DestRefresh,
               DestListDeny, DestListAllow, ConfigOptions, ID) ->
    case start_external_spawn_params(ProcessIndex, ProcessCount,
                                     ThreadsPerProcess,
                                     Filename, Arguments, Environment,
                                     Protocol, BufferSize, Timeout, Prefix,
                                     TimeoutAsync, TimeoutSync, TimeoutTerm,
                                     DestRefresh, ConfigOptions, ID) of
        {ok,
         SpawnProcess, SpawnProtocol, SocketPath,
         Rlimits, Owner, Nice, CGroup, Chroot, SyscallLock, Directory,
         CommandLine, FilenameNew, ArgumentsNew, EnvironmentLookup} ->
            {ok, DestDeny, DestAllow} =
                start_external_threads_params(DestListDeny, DestListAllow),
            ConfigOptionsNew = ConfigOptions#config_service_options{
                                   cgroup = CGroup},
            case start_external_threads(ThreadsPerProcess,
                                        ProcessIndex,
                                        ProcessCount,
                                        TimeStart,
                                        TimeRestart,
                                        Restarts,
                                        CommandLine,
                                        Protocol, SocketPath,
                                        BufferSize, Timeout,
                                        Prefix,
                                        TimeoutAsync,
                                        TimeoutSync,
                                        TimeoutTerm,
                                        DestRefresh,
                                        DestDeny, DestAllow,
                                        ConfigOptionsNew, ID) of
                {ok, Pids, Ports} ->
                    start_external_spawn(SpawnProcess,
                                         SpawnProtocol,
                                         SocketPath,
                                         Pids, Ports,
                                         Rlimits, Owner,
                                         Nice, CGroup, Chroot, SyscallLock,
                                         Directory,
                                         ThreadsPerProcess,
                                         CommandLine,
                                         FilenameNew,
                                         ArgumentsNew,
                                         Environment,
                                         EnvironmentLookup,
                                         Protocol, BufferSize);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

-spec status_internal(CountProcess :: pos_integer(),
                      PidsOrdered :: list(pid()),
                      arguments_execution_internal(),
                      Status :: nonempty_list()) ->
    cloudi_service_api:service_status_internal().

status_internal(CountProcess, PidsOrdered,
                [_GroupLeader,
                 Module, _Args, _TimeoutInit, Prefix,
                 _TimeoutAsync, _TimeoutSync, _TimeoutTerm,
                 _DestRefresh, _DestListDeny, _DestListAllow,
                 _ConfigOptions, _ID],
                Status) ->
    [{type, internal},
     {prefix, Prefix},
     {module, Module},
     {count_process, CountProcess},
     {pids_erlang, PidsOrdered} | Status].

-spec status_external(CountProcess :: pos_integer(),
                      CountThread :: pos_integer(),
                      OSPidsOrdered :: list(pos_integer()),
                      PidsOrdered :: list(pid()),
                      arguments_execution_external(),
                      Status :: nonempty_list()) ->
    cloudi_service_api:service_status_external().

status_external(CountProcess, CountThread, OSPidsOrdered, PidsOrdered,
                [_ThreadsPerProcess,
                 Filename, _Arguments, _Environment,
                 _Protocol, _BufferSize, _TimeoutInit, Prefix,
                 _TimeoutAsync, _TimeoutSync, _TimeoutTerm,
                 _DestRefresh, _DestListDeny, _DestListAllow,
                 _ConfigOptions, _ID],
                Status) ->
    [{type, external},
     {prefix, Prefix},
     {file_path, Filename},
     {count_process, CountProcess},
     {count_thread, CountThread},
     {pids_os, OSPidsOrdered},
     {pids_erlang, PidsOrdered} | Status].

-spec update_external(Pids :: nonempty_list(pid()),
                      Ports :: nonempty_list(pos_integer()),
                      arguments_execution_external()) ->
    ok |
    {error, error_reason_start_external() | any()}.

update_external(Pids, Ports,
                [ProcessIndex, ProcessCount, ThreadsPerProcess,
                 Filename, Arguments, Environment,
                 Protocol, BufferSize, Timeout, Prefix,
                 TimeoutAsync, TimeoutSync, TimeoutTerm, DestRefresh,
                 _DestListDeny, _DestListAllow, ConfigOptions, ID]) ->
    case start_external_spawn_params(ProcessIndex, ProcessCount,
                                     ThreadsPerProcess,
                                     Filename, Arguments, Environment,
                                     Protocol, BufferSize, Timeout, Prefix,
                                     TimeoutAsync, TimeoutSync, TimeoutTerm,
                                     DestRefresh, ConfigOptions, ID) of
        {ok,
         SpawnProcess, SpawnProtocol, SocketPath,
         Rlimits, Owner, Nice, CGroup, Chroot, SyscallLock, Directory,
         CommandLine, FilenameNew, ArgumentsNew, EnvironmentLookup} ->
            case start_external_spawn(SpawnProcess, SpawnProtocol, SocketPath,
                                      Pids, Ports,
                                      Rlimits, Owner,
                                      Nice, CGroup, Chroot, SyscallLock,
                                      Directory, ThreadsPerProcess,
                                      CommandLine, FilenameNew, ArgumentsNew,
                                      Environment, EnvironmentLookup,
                                      Protocol, BufferSize) of
                {ok, Pids} ->
                    _ = [Pid ! {'cloudi_service_update_state', CommandLine}
                         || Pid <- Pids],
                    ok;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

-spec update_internal_f(DestRefreshNew ::
                            cloudi_service_api:dest_refresh() | undefined,
                        TimeoutInitNew ::
                            cloudi_service_api:
                            timeout_initialize_milliseconds() | undefined,
                        TimeoutAsyncNew ::
                            cloudi_service_api:
                            timeout_send_async_value_milliseconds() | undefined,
                        TimeoutSyncNew ::
                            cloudi_service_api:
                            timeout_send_sync_value_milliseconds() | undefined,
                        DestListDenyNew ::
                            list(cloudi:service_name_pattern()) | undefined,
                        DestListAllowNew ::
                            list(cloudi:service_name_pattern()) | undefined,
                        OptionsKeys :: list(atom()),
                        ConfigOptionsNew :: #config_service_options{},
                        arguments_execution_internal()) ->
    arguments_execution_internal().

update_internal_f(DestRefreshNew, TimeoutInitNew,
                  TimeoutAsyncNew, TimeoutSyncNew,
                  DestListDenyNew, DestListAllowNew,
                  OptionsKeys, ConfigOptionsNew,
                  [GroupLeader,
                   Module, Args, TimeoutInitOld, Prefix,
                   TimeoutAsyncOld, TimeoutSyncOld, TimeoutTerm,
                   DestRefreshOld, DestListDenyOld, DestListAllowOld,
                   ConfigOptionsOld, ID]) ->
    [GroupLeader,
     Module, Args,
     if
        TimeoutInitNew =:= undefined ->
            TimeoutInitOld;
        is_integer(TimeoutInitNew) ->
            TimeoutInitNew
     end,
     Prefix,
     if
        TimeoutAsyncNew =:= undefined ->
            TimeoutAsyncOld;
        is_integer(TimeoutAsyncNew) ->
            TimeoutAsyncNew
     end,
     if
        TimeoutSyncNew =:= undefined ->
            TimeoutSyncOld;
        is_integer(TimeoutSyncNew) ->
            TimeoutSyncNew
     end,
     TimeoutTerm,
     if
        DestRefreshNew =:= undefined ->
            DestRefreshOld;
        is_atom(DestRefreshNew) ->
            DestRefreshNew
     end,
     if
        DestListDenyNew =:= invalid ->
            DestListDenyOld;
        DestListDenyNew =:= undefined; is_list(DestListDenyNew) ->
            DestListDenyNew
     end,
     if
        DestListAllowNew =:= invalid ->
            DestListAllowOld;
        DestListAllowNew =:= undefined; is_list(DestListAllowNew) ->
            DestListAllowNew
     end,
     cloudi_core_i_configuration:service_options_copy(OptionsKeys,
                                                      ConfigOptionsOld,
                                                      ConfigOptionsNew),
     ID].

-spec update_external_f(FilenameNew ::
                            cloudi_service_api:file_path() | undefined,
                        ArgumentsNew ::
                            cloudi_service_api:args_external() | undefined,
                        EnvironmentNew ::
                            cloudi_service_api:env_external() | undefined,
                        DestRefreshNew ::
                            cloudi_service_api:dest_refresh() | undefined,
                        TimeoutInitNew ::
                            cloudi_service_api:
                            timeout_initialize_milliseconds() | undefined,
                        TimeoutAsyncNew ::
                            cloudi_service_api:
                            timeout_send_async_value_milliseconds() | undefined,
                        TimeoutSyncNew ::
                            cloudi_service_api:
                            timeout_send_sync_value_milliseconds() | undefined,
                        DestListDenyNew ::
                            list(cloudi:service_name_pattern()) | undefined,
                        DestListAllowNew ::
                            list(cloudi:service_name_pattern()) | undefined,
                        OptionsKeys :: list(atom()),
                        ConfigOptionsNew :: #config_service_options{},
                        arguments_execution_external()) ->
    arguments_execution_external().

update_external_f(FilenameNew, ArgumentsNew, EnvironmentNew,
                  DestRefreshNew,
                  TimeoutInitNew, TimeoutAsyncNew, TimeoutSyncNew,
                  DestListDenyNew, DestListAllowNew,
                  OptionsKeys, ConfigOptionsNew,
                  [ThreadsPerProcess,
                   FilenameOld, ArgumentsOld, EnvironmentOld,
                   Protocol, BufferSize, TimeoutInitOld, Prefix,
                   TimeoutAsyncOld, TimeoutSyncOld, TimeoutTerm,
                   DestRefreshOld, DestListDenyOld, DestListAllowOld,
                   ConfigOptionsOld, ID]) ->
    [ThreadsPerProcess,
     if
        is_list(FilenameNew) ->
            FilenameNew;
        FilenameNew =:= undefined ->
            FilenameOld
     end,
     if
        is_list(ArgumentsNew) ->
            ArgumentsNew;
        ArgumentsNew =:= undefined ->
            ArgumentsOld
     end,
     if
        is_list(EnvironmentNew) ->
            EnvironmentNew;
        EnvironmentNew =:= undefined ->
            EnvironmentOld
     end,
     Protocol, BufferSize,
     if
        is_integer(TimeoutInitNew) ->
            TimeoutInitNew;
        TimeoutInitNew =:= undefined ->
            TimeoutInitOld
     end,
     Prefix,
     if
        is_integer(TimeoutAsyncNew) ->
            TimeoutAsyncNew;
        TimeoutAsyncNew =:= undefined ->
            TimeoutAsyncOld
     end,
     if
        is_integer(TimeoutSyncNew) ->
            TimeoutSyncNew;
        TimeoutSyncNew =:= undefined ->
            TimeoutSyncOld
     end,
     TimeoutTerm,
     if
        DestRefreshNew =:= undefined ->
            DestRefreshOld;
        is_atom(DestRefreshNew) ->
            DestRefreshNew
     end,
     if
        DestListDenyNew =:= invalid ->
            DestListDenyOld;
        DestListDenyNew =:= undefined; is_list(DestListDenyNew) ->
            DestListDenyNew
     end,
     if
        DestListAllowNew =:= invalid ->
            DestListAllowOld;
        DestListAllowNew =:= undefined; is_list(DestListAllowNew) ->
            DestListAllowNew
     end,
     cloudi_core_i_configuration:service_options_copy(OptionsKeys,
                                                      ConfigOptionsOld,
                                                      ConfigOptionsNew),
     ID].

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

rlimits(#config_service_options{limit = L}) ->
    cloudi_core_i_os_process:limit_format(L).

owner(#config_service_options{owner = L}, EnvironmentLookup) ->
    cloudi_core_i_os_process:owner_format(L, EnvironmentLookup).

nice(#config_service_options{nice = Nice}) ->
    Nice.

cgroup(#config_service_options{cgroup = L}, EnvironmentLookup) ->
    cloudi_core_i_os_process:cgroup_format(L, EnvironmentLookup).

chroot(#config_service_options{chroot = Chroot}, EnvironmentLookup) ->
    cloudi_core_i_os_process:chroot_format(Chroot, EnvironmentLookup).

syscall_lock(#config_service_options{syscall_lock = SyscallLock}) ->
    cloudi_core_i_os_process:syscall_lock_format(SyscallLock).

directory(#config_service_options{directory = Directory}, EnvironmentLookup) ->
    cloudi_core_i_os_process:directory_format(Directory, EnvironmentLookup).

create_socket_path(TemporaryDirectory, ID)
    when is_binary(ID) ->
    Path = filename:join([TemporaryDirectory,
                          "cloudi_socket_" ++
                          cloudi_x_uuid:uuid_to_string(ID, nodash) ++ "_"]),
    false = filelib:is_file(Path),
    Path.

start_external_spawn_params(ProcessIndex, ProcessCount, ThreadsPerProcess,
                            Filename, Arguments, Environment,
                            Protocol, BufferSize, Timeout, Prefix,
                            TimeoutAsync, TimeoutSync, TimeoutTerm, DestRefresh,
                            ConfigOptions, ID)
    when is_integer(ProcessIndex), is_integer(ProcessCount),
         is_integer(ThreadsPerProcess), ThreadsPerProcess > 0,
         is_list(Filename), is_list(Arguments), is_list(Environment),
         is_integer(BufferSize), is_integer(Timeout), is_list(Prefix),
         is_integer(TimeoutAsync), is_integer(TimeoutSync),
         is_integer(TimeoutTerm),
         is_record(ConfigOptions, config_service_options),
         is_binary(ID) ->
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
    TemporaryDirectory = case os:getenv("TMPDIR") of
        false ->
            "/tmp";
        L ->
            L
    end,
    SocketPath = create_socket_path(TemporaryDirectory, ID),
    EnvironmentLookup = cloudi_environment:lookup(),
    case start_external_spawn_params_parse(Filename, Arguments, ConfigOptions,
                                           EnvironmentLookup) of
        {ok, CommandLine, FilenameNew, ArgumentsNew, Chroot, Directory} ->
            case cloudi_x_supool:get(?OS_SPAWN_POOL) of
                SpawnProcess when is_pid(SpawnProcess) ->
                    SpawnProtocol = if
                        Protocol =:= tcp ->
                            $t; % inet
                        Protocol =:= udp ->
                            $u; % inet
                        Protocol =:= local ->
                            $l  % tcp local (unix domain socket)
                    end,
                    Rlimits = rlimits(ConfigOptions),
                    Owner = owner(ConfigOptions, EnvironmentLookup),
                    Nice = nice(ConfigOptions),
                    CGroup = cgroup(ConfigOptions, EnvironmentLookup),
                    SyscallLock = syscall_lock(ConfigOptions),
                    {ok,
                     SpawnProcess, SpawnProtocol, SocketPath,
                     Rlimits, Owner, Nice, CGroup, Chroot, SyscallLock,
                     Directory,
                     CommandLine, FilenameNew, ArgumentsNew,
                     EnvironmentLookup};
                undefined ->
                    {error, noproc}
            end;
        {error, _} = Error ->
            Error
    end.

start_external_threads_params(DestListDeny, DestListAllow) ->
    DestDeny = if
        DestListDeny =:= undefined ->
            undefined;
        is_list(DestListDeny) ->
            cloudi_x_trie:new(DestListDeny)
    end,
    DestAllow = if
        DestListAllow =:= undefined ->
            undefined;
        is_list(DestListAllow) ->
            cloudi_x_trie:new(DestListAllow)
    end,
    {ok, DestDeny, DestAllow}.

start_external_spawn_params_parse(Filename, Arguments, ConfigOptions,
                                  EnvironmentLookup) ->
    case filename_parse(Filename, EnvironmentLookup) of
        {ok, FilenameNew} ->
            case arguments_parse(Arguments, EnvironmentLookup) of
                {ok, ArgumentsNew, ArgumentsList} ->
                    CommandLine = [FilenameNew | ArgumentsList],
                    case chroot(ConfigOptions, EnvironmentLookup) of
                        {ok, Chroot} ->
                            case directory(ConfigOptions, EnvironmentLookup) of
                                {ok, Directory} ->
                                    {ok,
                                     CommandLine,
                                     FilenameNew,
                                     ArgumentsNew,
                                     Chroot,
                                     Directory};
                                {error, _} = Error ->
                                    Error
                            end;
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

start_external_thread(ThreadsPerProcess, Pids, Ports, ThreadsPerProcess,
                      _, _, _, _, _, _, _, _, _, _,
                      _, _, _, _, _, _, _, _, _) ->
    {ok, lists:reverse(Pids), lists:reverse(Ports)};

start_external_thread(I, Pids, Ports, ThreadsPerProcess,
                      ProcessIndex, ProcessCount,
                      TimeStart, TimeRestart, Restarts,
                      CommandLine, Protocol, SocketPath, BufferSize, Timeout,
                      Prefix, TimeoutAsync, TimeoutSync, TimeoutTerm,
                      DestRefresh, DestDeny, DestAllow,
                      ConfigOptions, ID) ->
    case ?PROCESS_START_EXTERNAL(Protocol, SocketPath,
                                 I + ThreadsPerProcess * ProcessIndex,
                                 ProcessIndex, ProcessCount,
                                 TimeStart, TimeRestart, Restarts,
                                 CommandLine, BufferSize,
                                 Timeout, Prefix,
                                 TimeoutAsync, TimeoutSync, TimeoutTerm,
                                 DestRefresh, DestDeny, DestAllow,
                                 ConfigOptions, ID) of
        {ok, Pid, Port} ->
            start_external_thread(I + 1, [Pid | Pids], [Port | Ports],
                                  ThreadsPerProcess,
                                  ProcessIndex, ProcessCount,
                                  TimeStart, TimeRestart, Restarts,
                                  CommandLine, Protocol, SocketPath,
                                  BufferSize, Timeout,
                                  Prefix, TimeoutAsync, TimeoutSync,
                                  TimeoutTerm,
                                  DestRefresh, DestDeny, DestAllow,
                                  ConfigOptions, ID);
        {error, _} = Error ->
            lists:foreach(fun(P) -> erlang:exit(P, kill) end, Pids),
            Error
    end.

start_external_threads(ThreadsPerProcess,
                       ProcessIndex, ProcessCount,
                       TimeStart, TimeRestart, Restarts,
                       CommandLine, Protocol, SocketPath, BufferSize, Timeout,
                       Prefix, TimeoutAsync, TimeoutSync, TimeoutTerm,
                       DestRefresh, DestDeny, DestAllow,
                       ConfigOptions, ID) ->
    start_external_thread(0, [], [], ThreadsPerProcess,
                          ProcessIndex, ProcessCount,
                          TimeStart, TimeRestart, Restarts,
                          CommandLine, Protocol, SocketPath,
                          BufferSize, Timeout,
                          Prefix, TimeoutAsync, TimeoutSync, TimeoutTerm,
                          DestRefresh, DestDeny, DestAllow,
                          ConfigOptions, ID).

start_external_spawn(SpawnProcess, SpawnProtocol, SocketPath,
                     Pids, Ports, Rlimits, Owner,
                     Nice, CGroup, Chroot, SyscallLock, Directory,
                     ThreadsPerProcess, CommandLine,
                     Filename, Arguments, Environment,
                     EnvironmentLookup, Protocol, BufferSize) ->
    case environment_parse(Environment, ThreadsPerProcess,
                           Protocol, BufferSize, EnvironmentLookup) of
        {ok, SpawnEnvironment} ->
            {UserI, UserStr, GroupI, GroupStr} = Owner,
            case cloudi_core_i_os_spawn:spawn(SpawnProcess,
                                              SpawnProtocol,
                                              string_terminate(SocketPath),
                                              Ports,
                                              Rlimits,
                                              UserI,
                                              string_terminate(UserStr),
                                              GroupI,
                                              string_terminate(GroupStr),
                                              Nice,
                                              string_terminate(Chroot),
                                              SyscallLock,
                                              string_terminate(Directory),
                                              string_terminate(Filename),
                                              string_terminate(Arguments),
                                              SpawnEnvironment) of
                {ok, OSPid} ->
                    ?LOG_INFO("OS pid ~p spawned ~p~n  ~tp",
                              [OSPid, Pids, CommandLine]),
                    case cloudi_core_i_os_process:cgroup_set(OSPid, CGroup) of
                        ok ->
                            {ok, Pids};
                        {error, _} = Error ->
                            Error
                    end;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

% update filename, including path
filename_parse(Filename, EnvironmentLookup) ->
    case cloudi_environment:transform(Filename, EnvironmentLookup) of
        [] ->
            {error, {service_external_file_path_invalid_expanded, Filename}};
        ExpandedFilename ->
            case unicode:characters_to_binary(ExpandedFilename, utf8) of
                {_, _, _} ->
                    {error,
                     {service_external_file_path_invalid_unicode,
                      ExpandedFilename}};
                Utf8Filename ->
                    {ok, erlang:binary_to_list(Utf8Filename)}
            end
    end.

% remove beginning whitespace and validate delimiters
% within the command-line arguments
arguments_parse([32 | Arguments], EnvironmentLookup) ->
    arguments_parse(Arguments, EnvironmentLookup);

arguments_parse(Arguments, EnvironmentLookup) ->
    ArgumentsNew = cloudi_environment:transform(Arguments, EnvironmentLookup),
    case arguments_parse([], [], [], none, ArgumentsNew) of
        {ok, _, _} = Success ->
            Success;
        Reason when is_atom(Reason) ->
            {error, {Reason, Arguments}}
    end.

arguments_parse(ArgumentsBinary, ArgumentsList, Argument, none, []) ->
    {ok,
     lists:reverse(ArgumentsBinary),
     lists:reverse([lists:reverse(Argument) | ArgumentsList])};

arguments_parse(_, _, _, _, []) ->
    service_external_args_invalid_quotes;

arguments_parse(ArgumentsBinary, ArgumentsList, Argument, none, [$' | T]) ->
    arguments_parse(ArgumentsBinary, ArgumentsList, Argument, $', T);

arguments_parse(ArgumentsBinary, ArgumentsList, Argument, none, [$" | T]) ->
    arguments_parse(ArgumentsBinary, ArgumentsList, Argument, $", T);

arguments_parse(ArgumentsBinary, ArgumentsList, Argument, none, [$` | T]) ->
    arguments_parse(ArgumentsBinary, ArgumentsList, Argument, $`, T);

arguments_parse(ArgumentsBinary, ArgumentsList, Argument,
                none, [32 | [32 | _] = T]) ->
    arguments_parse(ArgumentsBinary, ArgumentsList, Argument, none, T);

arguments_parse(ArgumentsBinary, ArgumentsList, Argument, none, [32 | T]) ->
    arguments_parse([0 | ArgumentsBinary],
                    [lists:reverse(Argument) | ArgumentsList], [], none, T);

arguments_parse(ArgumentsBinary, ArgumentsList, Argument, $', [$' | T]) ->
    arguments_parse(ArgumentsBinary, ArgumentsList, Argument, none, T);

arguments_parse(ArgumentsBinary, ArgumentsList, Argument, $", [$" | T]) ->
    arguments_parse(ArgumentsBinary, ArgumentsList, Argument, none, T);

arguments_parse(ArgumentsBinary, ArgumentsList, Argument, $`, [$` | T]) ->
    arguments_parse(ArgumentsBinary, ArgumentsList, Argument, none, T);

arguments_parse(ArgumentsBinary, ArgumentsList, Argument, Delim, [H | T]) ->
    case unicode:characters_to_binary([H], utf8) of
        {_, _, _} ->
            service_external_args_invalid_unicode;
        Utf8H ->
            ArgumentsBinaryNew = lists:reverse(erlang:binary_to_list(Utf8H),
                                               ArgumentsBinary),
            arguments_parse(ArgumentsBinaryNew, ArgumentsList,
                            [H | Argument], Delim, T)
    end.

% add CloudI API environmental variables and format into a single
% string that is easy to use in C/C++
environment_parse(Environment0, ThreadsPerProcess0,
                  Protocol0, BufferSize0, EnvironmentLookup0) ->
    ThreadsPerProcess1 = erlang:integer_to_list(ThreadsPerProcess0),
    Protocol1 = erlang:atom_to_list(Protocol0),
    BufferSize1 = erlang:integer_to_list(BufferSize0),
    Environment1 = lists:keystore(?ENVIRONMENT_THREAD_COUNT, 1, Environment0,
                                  {?ENVIRONMENT_THREAD_COUNT,
                                   ThreadsPerProcess1}),
    Environment2 = lists:keystore(?ENVIRONMENT_PROTOCOL, 1, Environment1,
                                  {?ENVIRONMENT_PROTOCOL,
                                   Protocol1}),
    Environment3 = lists:keystore(?ENVIRONMENT_BUFFER_SIZE, 1, Environment2,
                                  {?ENVIRONMENT_BUFFER_SIZE,
                                   BufferSize1}),
    EnvironmentLookup1 = cloudi_x_trie:store(?ENVIRONMENT_THREAD_COUNT,
                                             ThreadsPerProcess1,
                                             EnvironmentLookup0),
    EnvironmentLookup2 = cloudi_x_trie:store(?ENVIRONMENT_PROTOCOL,
                                             Protocol1,
                                             EnvironmentLookup1),
    EnvironmentLookup3 = cloudi_x_trie:store(?ENVIRONMENT_BUFFER_SIZE,
                                             BufferSize1,
                                             EnvironmentLookup2),
    environment_format(Environment3, EnvironmentLookup3).

environment_format_value([], _, K, _) ->
    {error, {service_external_env_invalid_expanded, K}};
environment_format_value(_, [], _, _) ->
    {ok, []};
environment_format_value([_ | _] = ExpandedK, [_ | _] = ExpandedV, K, V) ->
    case unicode:characters_to_binary(ExpandedK, utf8) of
        {_, _, _} ->
            {error, {service_external_env_invalid_unicode, K}};
        Utf8K ->
            case unicode:characters_to_binary(ExpandedV, utf8) of
                {_, _, _} ->
                    {error, {service_external_env_invalid_unicode, V}};
                Utf8V ->
                    {ok, [Utf8K, $=, Utf8V, 0]}
            end
    end.

environment_format(Environment, EnvironmentLookup) ->
    environment_format([], Environment, EnvironmentLookup).

environment_format(Output, [], _) ->
    {ok, erlang:iolist_to_binary(lists:reverse(Output))};

environment_format(Output, [{K, V} | Environment], EnvironmentLookup) ->
    ExpandedK = cloudi_environment:transform(K, EnvironmentLookup),
    ExpandedV = cloudi_environment:transform(V, EnvironmentLookup),
    case environment_format_value(ExpandedK, ExpandedV, K, V) of
        {ok, Pair} ->
            environment_format([Pair | Output], Environment, EnvironmentLookup);
        {error, _} = Error ->
            Error
    end.

% terminate the string for easy access within C/C++
string_terminate([]) ->
    <<0>>;
string_terminate([_ | _] = L) ->
    erlang:iolist_to_binary([L, 0]).

