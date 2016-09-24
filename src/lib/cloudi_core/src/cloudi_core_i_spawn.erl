%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Spawn==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2011-2016, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011-2016 Michael Truog
%%% @version 1.5.4 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_core_i_spawn).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([environment_lookup/0,
         environment_transform/1,
         environment_transform/2,
         start_internal/15,
         start_external/18,
         update_external/3,
         update_internal_f/9,
         update_external_f/12]).

-include("cloudi_logger.hrl").
-include("cloudi_core_i_configuration.hrl").
-include("cloudi_core_i_constants.hrl").

-define(CREATE_INTERNAL, cloudi_core_i_services_internal_sup:create_internal).
-define(CREATE_EXTERNAL, cloudi_core_i_services_external_sup:create_external).

% environmental variables used by CloudI API initialization
-define(ENVIRONMENT_THREAD_COUNT,  "CLOUDI_API_INIT_THREAD_COUNT").
-define(ENVIRONMENT_PROTOCOL,      "CLOUDI_API_INIT_PROTOCOL").
-define(ENVIRONMENT_BUFFER_SIZE,   "CLOUDI_API_INIT_BUFFER_SIZE").

-ifdef(CLOUDI_CORE_STANDALONE).
-ifdef(ERLANG_OTP_VERSION_16).
-else.
-ifdef(ERLANG_OTP_VERSION_17).
-else.
-dialyzer({no_match, start_external_spawn/15}).
-endif.
-endif.
-endif.

%%%------------------------------------------------------------------------
%%% External interface
%%%------------------------------------------------------------------------

% all environment variables currently in the Erlang VM shell
% are used as possible substitution values
environment_lookup() ->
    cloudi_x_trie:new(lists:map(fun(Entry) ->
        cloudi_string:splitl($=, Entry, input)
    end, os:getenv())).

environment_transform(String) ->
    EnvironmentLookup = environment_lookup(),
    environment_transform(String, EnvironmentLookup).

% update external service strings based on the Erlang VM shell
% environmental variables
environment_transform(String, EnvironmentLookup) ->
    environment_transform(String, [], undefined, EnvironmentLookup).

start_internal(ProcessIndex, ProcessCount, GroupLeader,
               Module, Args, Timeout, Prefix,
               TimeoutAsync, TimeoutSync, TimeoutTerm, DestRefresh,
               DestListDeny, DestListAllow, ConfigOptions, ID)
    when is_integer(ProcessIndex), is_integer(ProcessCount),
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
        {ok, NewTimeout} ->
            % Erlang application startup is asynchronous, so wait for the
            % module to be loaded or timeout
            ?CREATE_INTERNAL(ProcessIndex, ProcessCount, GroupLeader,
                             Module, Args, NewTimeout, Prefix,
                             TimeoutAsync, TimeoutSync, TimeoutTerm,
                             DestRefresh, DestDeny, DestAllow,
                             ConfigOptions, ID);
        {error, Reason} ->
            ?LOG_ERROR("loading ~p failed: ~p", [Module, Reason]),
            {error, {service_internal_module_not_loaded, Module}}
    end.

start_external(ProcessIndex, ProcessCount, ThreadsPerProcess,
               Filename, Arguments, Environment,
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
         Rlimits, Owner, Nice, CGroup, Directory,
         CommandLine, NewFilename, NewArguments, EnvironmentLookup} ->
            {ok, DestDeny, DestAllow} =
                start_external_threads_params(DestListDeny, DestListAllow),
            NewConfigOptions = ConfigOptions#config_service_options{
                                   cgroup = CGroup},
            case start_external_threads(ThreadsPerProcess,
                                        ProcessIndex,
                                        ProcessCount,
                                        CommandLine,
                                        Protocol, SocketPath,
                                        BufferSize, Timeout,
                                        Prefix,
                                        TimeoutAsync,
                                        TimeoutSync,
                                        TimeoutTerm,
                                        DestRefresh,
                                        DestDeny, DestAllow,
                                        NewConfigOptions, ID) of
                {ok, Pids, Ports} ->
                    start_external_spawn(SpawnProcess,
                                         SpawnProtocol,
                                         SocketPath,
                                         Pids, Ports,
                                         Rlimits, Owner,
                                         Nice, CGroup, Directory,
                                         ThreadsPerProcess,
                                         CommandLine,
                                         NewFilename,
                                         NewArguments,
                                         Environment,
                                         EnvironmentLookup,
                                         Protocol, BufferSize);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

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
         Rlimits, Owner, Nice, CGroup, Directory,
         CommandLine, NewFilename, NewArguments, EnvironmentLookup} ->
            case start_external_spawn(SpawnProcess, SpawnProtocol, SocketPath,
                                      Pids, Ports,
                                      Rlimits, Owner,
                                      Nice, CGroup, Directory,
                                      ThreadsPerProcess,
                                      CommandLine, NewFilename, NewArguments,
                                      Environment, EnvironmentLookup,
                                      Protocol, BufferSize) of
                {ok, Pids} ->
                    [Pid ! {'cloudi_service_update_state', CommandLine}
                     || Pid <- Pids],
                    ok;
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

update_internal_f(NewDestRefresh, NewTimeoutInit,
                  NewTimeoutAsync, NewTimeoutSync,
                  NewDestListDeny, NewDestListAllow,
                  OptionsKeys, NewConfigOptions,
                  [GroupLeader,
                   Module, Args, OldTimeoutInit, Prefix,
                   OldTimeoutAsync, OldTimeoutSync, TimeoutTerm,
                   OldDestRefresh, OldDestListDeny, OldDestListAllow,
                   OldConfigOptions, ID]) ->
    [GroupLeader,
     Module, Args,
     if
        NewTimeoutInit =:= undefined ->
            OldTimeoutInit;
        is_integer(NewTimeoutInit) ->
            NewTimeoutInit
     end,
     Prefix,
     if
        NewTimeoutAsync =:= undefined ->
            OldTimeoutAsync;
        is_integer(NewTimeoutAsync) ->
            NewTimeoutAsync
     end,
     if
        NewTimeoutSync =:= undefined ->
            OldTimeoutSync;
        is_integer(NewTimeoutSync) ->
            NewTimeoutSync
     end,
     TimeoutTerm,
     if
        NewDestRefresh =:= undefined ->
            OldDestRefresh;
        is_atom(NewDestRefresh) ->
            NewDestRefresh
     end,
     if
        NewDestListDeny =:= invalid ->
            OldDestListDeny;
        NewDestListDeny =:= undefined; is_list(NewDestListDeny) ->
            NewDestListDeny
     end,
     if
        NewDestListAllow =:= invalid ->
            OldDestListAllow;
        NewDestListAllow =:= undefined; is_list(NewDestListAllow) ->
            NewDestListAllow
     end,
     cloudi_core_i_configuration:service_options_copy(OptionsKeys,
                                                      OldConfigOptions,
                                                      NewConfigOptions),
     ID].

update_external_f(NewFilename, NewArguments, NewEnvironment,
                  NewDestRefresh,
                  NewTimeoutInit, NewTimeoutAsync, NewTimeoutSync,
                  NewDestListDeny, NewDestListAllow,
                  OptionsKeys, NewConfigOptions,
                  [ThreadsPerProcess,
                   OldFilename, OldArguments, OldEnvironment,
                   Protocol, BufferSize, OldTimeoutInit, Prefix,
                   OldTimeoutAsync, OldTimeoutSync, TimeoutTerm,
                   OldDestRefresh, OldDestListDeny, OldDestListAllow,
                   OldConfigOptions, ID]) ->
    [ThreadsPerProcess,
     if
        is_list(NewFilename) ->
            NewFilename;
        NewFilename =:= undefined ->
            OldFilename
     end,
     if
        is_list(NewArguments) ->
            NewArguments;
        NewArguments =:= undefined ->
            OldArguments
     end,
     if
        is_list(NewEnvironment) ->
            NewEnvironment;
        NewEnvironment =:= undefined ->
            OldEnvironment
     end,
     Protocol, BufferSize,
     if
        is_integer(NewTimeoutInit) ->
            NewTimeoutInit;
        NewTimeoutInit =:= undefined ->
            OldTimeoutInit
     end,
     Prefix,
     if
        is_integer(NewTimeoutAsync) ->
            NewTimeoutAsync;
        NewTimeoutAsync =:= undefined ->
            OldTimeoutAsync
     end,
     if
        is_integer(NewTimeoutSync) ->
            NewTimeoutSync;
        NewTimeoutSync =:= undefined ->
            OldTimeoutSync
     end,
     TimeoutTerm,
     if
        NewDestRefresh =:= undefined ->
            OldDestRefresh;
        is_atom(NewDestRefresh) ->
            NewDestRefresh
     end,
     if
        NewDestListDeny =:= invalid ->
            OldDestListDeny;
        NewDestListDeny =:= undefined; is_list(NewDestListDeny) ->
            NewDestListDeny
     end,
     if
        NewDestListAllow =:= invalid ->
            OldDestListAllow;
        NewDestListAllow =:= undefined; is_list(NewDestListAllow) ->
            NewDestListAllow
     end,
     cloudi_core_i_configuration:service_options_copy(OptionsKeys,
                                                      OldConfigOptions,
                                                      NewConfigOptions),
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
    EnvironmentLookup = environment_lookup(),
    case start_external_spawn_params_parse(Filename, Arguments, ConfigOptions,
                                           EnvironmentLookup) of
        {ok, CommandLine, NewFilename, NewArguments, Directory} ->
            case cloudi_x_supool:get(cloudi_core_i_os_spawn) of
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
                    {ok,
                     SpawnProcess, SpawnProtocol, SocketPath,
                     Rlimits, Owner, Nice, CGroup, Directory,
                     CommandLine, NewFilename, NewArguments,
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
        {ok, NewFilename} ->
            case arguments_parse(Arguments, EnvironmentLookup) of
                {ok, NewArguments} ->
                    CommandLine = [NewFilename |
                                   string:tokens(NewArguments, [0])],
                    case directory(ConfigOptions, EnvironmentLookup) of
                        {ok, Directory} ->
                            {ok,
                             CommandLine,
                             NewFilename,
                             NewArguments,
                             Directory};
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
                      _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _) ->
    {ok, lists:reverse(Pids), lists:reverse(Ports)};

start_external_thread(I, Pids, Ports, ThreadsPerProcess,
                      ProcessIndex, ProcessCount, CommandLine,
                      Protocol, SocketPath, BufferSize, Timeout,
                      Prefix, TimeoutAsync, TimeoutSync, TimeoutTerm,
                      DestRefresh, DestDeny, DestAllow,
                      ConfigOptions, ID) ->
    case ?CREATE_EXTERNAL(Protocol, SocketPath,
                          I + ThreadsPerProcess * ProcessIndex,
                          ProcessIndex, ProcessCount, CommandLine,
                          BufferSize, Timeout,
                          Prefix, TimeoutAsync, TimeoutSync, TimeoutTerm,
                          DestRefresh, DestDeny, DestAllow,
                          ConfigOptions, ID) of
        {ok, Pid, Port} ->
            start_external_thread(I + 1, [Pid | Pids], [Port | Ports],
                                  ThreadsPerProcess,
                                  ProcessIndex, ProcessCount, CommandLine,
                                  Protocol, SocketPath, BufferSize, Timeout,
                                  Prefix, TimeoutAsync, TimeoutSync,
                                  TimeoutTerm,
                                  DestRefresh, DestDeny, DestAllow,
                                  ConfigOptions, ID);
        {error, _} = Error ->
            lists:foreach(fun(P) -> erlang:exit(P, kill) end, Pids),
            Error
    end.

start_external_threads(ThreadsPerProcess,
                       ProcessIndex, ProcessCount, CommandLine,
                       Protocol, SocketPath, BufferSize, Timeout,
                       Prefix, TimeoutAsync, TimeoutSync, TimeoutTerm,
                       DestRefresh, DestDeny, DestAllow,
                       ConfigOptions, ID) ->
    start_external_thread(0, [], [], ThreadsPerProcess,
                          ProcessIndex, ProcessCount, CommandLine,
                          Protocol, SocketPath, BufferSize, Timeout,
                          Prefix, TimeoutAsync, TimeoutSync, TimeoutTerm,
                          DestRefresh, DestDeny, DestAllow,
                          ConfigOptions, ID).

start_external_spawn(SpawnProcess, SpawnProtocol, SocketPath,
                     Pids, Ports, Rlimits, Owner, Nice, CGroup, Directory,
                     ThreadsPerProcess, CommandLine,
                     Filename, Arguments, Environment,
                     EnvironmentLookup, Protocol, BufferSize) ->
    SpawnEnvironment = environment_parse(Environment, ThreadsPerProcess,
                                         Protocol, BufferSize,
                                         EnvironmentLookup),
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
                                      string_terminate(Directory),
                                      string_terminate(Filename),
                                      string_terminate(Arguments),
                                      SpawnEnvironment) of
        {ok, OSPid} ->
            ?LOG_INFO("OS pid ~p spawned ~p~n  ~p",
                      [OSPid, Pids, CommandLine]),
            case cloudi_core_i_os_process:cgroup_set(OSPid, CGroup) of
                ok ->
                    {ok, Pids};
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

% transform a string using a lookup containing environment variables
% (the loop doesn't have error conditions by design)
environment_transform([], Output, undefined, _) ->
    lists:reverse(Output);

environment_transform([$\\, $$ | String], Output,
                      undefined, EnvironmentLookup) ->
    environment_transform(String, [$$ | Output], undefined, EnvironmentLookup);

environment_transform([$$ | String], Output, undefined, EnvironmentLookup) ->
    case String of
        [${ | Rest] ->
            environment_transform(Rest, Output, [], EnvironmentLookup);
        _ ->
            environment_transform(String, Output, [], EnvironmentLookup)
    end;

environment_transform([C | String], Output, undefined, EnvironmentLookup) ->
    environment_transform(String, [C | Output], undefined, EnvironmentLookup);

environment_transform([$} | String], Output, Key, EnvironmentLookup) ->
    environment_transform_value(Key, String, Output, EnvironmentLookup);

environment_transform([C | String], Output, Key, EnvironmentLookup)
    when (C >= $A andalso C =< $Z); (C == $_);
         (C >= $a andalso C =< $z);
         (C >= $0 andalso C =< $9) ->
    % handles ASCII only
    environment_transform(String, Output, [C | Key], EnvironmentLookup);

environment_transform(String, Output, Key, EnvironmentLookup) ->
    environment_transform_value(Key, String, Output, EnvironmentLookup).

environment_transform_value([], String, Output, EnvironmentLookup) ->
    environment_transform(String, Output, undefined, EnvironmentLookup);

environment_transform_value(Key, String, Output, EnvironmentLookup) ->
    case cloudi_x_trie:find(lists:reverse(Key), EnvironmentLookup) of
        {ok, Value} ->
            environment_transform(String, lists:reverse(Value) ++ Output,
                                  undefined, EnvironmentLookup);
        error ->
            environment_transform(String, Output,
                                  undefined, EnvironmentLookup)
    end.

% update filename, including path
filename_parse(Filename, EnvironmentLookup) ->
    case environment_transform(Filename, EnvironmentLookup) of
        [] ->
            {error, {service_external_file_path_invalid_expanded, Filename}};
        NewFilename ->
            {ok, NewFilename}
    end.

% remove beginning whitespace and validate delimiters
% within the command-line arguments
arguments_parse([32 | Arguments], EnvironmentLookup) ->
    arguments_parse(Arguments, EnvironmentLookup);

arguments_parse(Arguments, EnvironmentLookup) ->
    NewArguments = environment_transform(Arguments, EnvironmentLookup),
    case arguments_parse([], none, NewArguments) of
        {ok, _} = Success ->
            Success;
        Reason when is_atom(Reason) ->
            {error, {Reason, Arguments}}
    end.

arguments_parse(Output, none, []) ->
    {ok, lists:reverse(Output)};

arguments_parse(_, _, []) ->
    service_external_args_malformed;

arguments_parse(Output, none, [$' | T]) ->
    arguments_parse(Output, $', T);

arguments_parse(Output, none, [$" | T]) ->
    arguments_parse(Output, $", T);

arguments_parse(Output, none, [$` | T]) ->
    arguments_parse(Output, $`, T);

arguments_parse(Output, none, [32 | [32 | _] = T]) ->
    arguments_parse(Output, none, T);

arguments_parse(Output, none, [32 | T]) ->
    arguments_parse([0 | Output], none, T);

arguments_parse(Output, $', [$' | T]) ->
    arguments_parse(Output, none, T);

arguments_parse(Output, $", [$" | T]) ->
    arguments_parse(Output, none, T);

arguments_parse(Output, $`, [$` | T]) ->
    arguments_parse(Output, none, T);

arguments_parse(Output, Delim, [H | T]) ->
    arguments_parse([H | Output], Delim, T).

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

environment_format_value([], _) ->
    [];
environment_format_value(_, []) ->
    [];
environment_format_value([_ | _] = K, [_ | _] = V) ->
    K ++ [$= | V] ++ [0].

environment_format(Environment, EnvironmentLookup) ->
    environment_format([], Environment, EnvironmentLookup).

environment_format(Output, [], _) ->
    Output;

environment_format(Output, [{K, V} | Environment], EnvironmentLookup) ->
    NewK = environment_transform(K, EnvironmentLookup),
    NewV = environment_transform(V, EnvironmentLookup),
    environment_format(Output ++ environment_format_value(NewK, NewV),
                       Environment, EnvironmentLookup).

% terminate the string for easy access within C/C++
string_terminate([]) ->
    [0];
string_terminate([_ | _] = L) ->
    L ++ [0].

