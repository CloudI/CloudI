%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Spawn==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2011-2014, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011-2014 Michael Truog
%%% @version 1.3.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_spawn).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([environment_lookup/0,
         environment_transform/1,
         environment_transform/2,
         start_internal/13,
         start_external/17]).

-include("cloudi_configuration.hrl").
-include("cloudi_logger.hrl").

-define(CREATE_INTERNAL, cloudi_services_internal_sup:create_internal).
-define(CREATE_EXTERNAL, cloudi_services_external_sup:create_external).

% environmental variables used by CloudI API initialization
-define(ENVIRONMENT_THREAD_COUNT,  "CLOUDI_API_INIT_THREAD_COUNT").
-define(ENVIRONMENT_PROTOCOL,      "CLOUDI_API_INIT_PROTOCOL").
-define(ENVIRONMENT_BUFFER_SIZE,   "CLOUDI_API_INIT_BUFFER_SIZE").

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

start_internal(ProcessIndex, ProcessCount,
               Module, Args, Timeout, Prefix,
               TimeoutAsync, TimeoutSync, DestRefresh,
               DestDenyList, DestAllowList, ConfigOptions, UUID)
    when is_integer(ProcessIndex), is_integer(ProcessCount),
         is_atom(Module), is_list(Args), is_integer(Timeout), is_list(Prefix),
         is_integer(TimeoutAsync), is_integer(TimeoutSync),
         is_record(ConfigOptions, config_service_options),
         is_binary(UUID) ->
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
        DestDenyList =:= undefined ->
            undefined;
        is_list(DestDenyList) ->
            cloudi_x_trie:new(DestDenyList)
    end,
    DestAllow = if
        DestAllowList =:= undefined ->
            undefined;
        is_list(DestAllowList) ->
            cloudi_x_trie:new(DestAllowList)
    end,
    case cloudi_x_reltool_util:is_module_loaded(Module, Timeout) of
        {ok, NewTimeout} ->
            % Erlang application startup is asynchronous, so wait for the
            % module to be loaded or timeout
            ?CREATE_INTERNAL(ProcessIndex, ProcessCount,
                             Module, Args, NewTimeout, Prefix,
                             TimeoutAsync, TimeoutSync, DestRefresh,
                             DestDeny, DestAllow, ConfigOptions);
        {error, Reason} ->
            ?LOG_ERROR("loading ~p failed: ~p", [Module, Reason]),
            {error, {service_internal_module_not_loaded, Module}}
    end.

start_external(ProcessIndex, ProcessCount, ThreadsPerProcess,
               Filename, Arguments, Environment,
               Protocol, BufferSize, Timeout, Prefix,
               TimeoutAsync, TimeoutSync, DestRefresh,
               DestDenyList, DestAllowList, ConfigOptions, UUID)
    when is_integer(ProcessIndex), is_integer(ProcessCount),
         is_integer(ThreadsPerProcess), ThreadsPerProcess > 0,
         is_list(Filename), is_list(Arguments), is_list(Environment),
         is_integer(BufferSize), is_integer(Timeout), is_list(Prefix),
         is_integer(TimeoutAsync), is_integer(TimeoutSync),
         is_record(ConfigOptions, config_service_options),
         is_binary(UUID) ->
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
    DestDeny = if
        DestDenyList =:= undefined ->
            undefined;
        is_list(DestDenyList) ->
            cloudi_x_trie:new(DestDenyList)
    end,
    DestAllow = if
        DestAllowList =:= undefined ->
            undefined;
        is_list(DestAllowList) ->
            cloudi_x_trie:new(DestAllowList)
    end,
    TemporaryDirectory = case os:getenv("TMPDIR") of
        false ->
            "/tmp";
        L ->
            L
    end,
    SocketPath = create_socket_path(TemporaryDirectory, UUID),
    case start_external_threads(ThreadsPerProcess, ProcessIndex, ProcessCount,
                                Protocol, SocketPath, BufferSize, Timeout,
                                Prefix, TimeoutAsync, TimeoutSync,
                                DestRefresh, DestDeny, DestAllow,
                                ConfigOptions) of
        {ok, Pids, Ports} ->
            case cloudi_pool:get(cloudi_os_spawn) of
                SpawnProcess when is_pid(SpawnProcess) ->
                    SpawnProtocol = if
                        Protocol =:= tcp ->
                            $t; % inet
                        Protocol =:= udp ->
                            $u; % inet
                        Protocol =:= local ->
                            $l  % tcp local (unix domain socket)
                    end,
                    start_external_spawn(SpawnProcess, SpawnProtocol,
                                         string_terminate(SocketPath),
                                         Pids, Ports, ThreadsPerProcess,
                                         Filename, Arguments, Environment,
                                         environment_lookup(),
                                         Protocol, BufferSize);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

create_socket_path(TemporaryDirectory, UUID)
    when is_binary(UUID) ->
    Path = filename:join([TemporaryDirectory,
                          "cloudi_socket_" ++
                          cloudi_x_uuid:uuid_to_string(UUID, nodash) ++ "_"]),
    false = filelib:is_file(Path),
    Path.

start_external_thread(0, Pids, Ports, _, _, _, _, _, _, _, _, _, _, _, _, _) ->
    {ok, lists:reverse(Pids), lists:reverse(Ports)};

start_external_thread(I, Pids, Ports,
                      ProcessIndex, ProcessCount,
                      Protocol, SocketPath, BufferSize, Timeout,
                      Prefix, TimeoutAsync, TimeoutSync,
                      DestRefresh, DestDeny, DestAllow,
                      ConfigOptions) ->
    case ?CREATE_EXTERNAL(Protocol, SocketPath,
                          I, ProcessIndex, ProcessCount,
                          BufferSize, Timeout,
                          Prefix, TimeoutAsync, TimeoutSync,
                          DestRefresh, DestDeny, DestAllow,
                          ConfigOptions) of
        {ok, Pid, Port} ->
            start_external_thread(I - 1, [Pid | Pids], [Port | Ports],
                                  ProcessIndex, ProcessCount,
                                  Protocol, SocketPath, BufferSize, Timeout,
                                  Prefix, TimeoutAsync, TimeoutSync,
                                  DestRefresh, DestDeny, DestAllow,
                                  ConfigOptions);
        {error, _} = Error ->
            lists:foreach(fun(P) -> erlang:exit(P, kill) end, Pids),
            Error
    end.

start_external_threads(ThreadsPerProcess, ProcessIndex, ProcessCount,
                       Protocol, SocketPath, BufferSize, Timeout,
                       Prefix, TimeoutAsync, TimeoutSync,
                       DestRefresh, DestDeny, DestAllow,
                       ConfigOptions) ->
    start_external_thread(ThreadsPerProcess, [], [],
                          ProcessIndex, ProcessCount,
                          Protocol, SocketPath, BufferSize, Timeout,
                          Prefix, TimeoutAsync, TimeoutSync,
                          DestRefresh, DestDeny, DestAllow,
                          ConfigOptions).

start_external_spawn(SpawnProcess, SpawnProtocol, SpawnSocketPath, Pids, Ports,
                     ThreadsPerProcess, Filename, Arguments, Environment,
                     EnvironmentLookup, Protocol, BufferSize) ->
    case filename_parse(Filename, EnvironmentLookup) of
        {ok, SpawnFilename} ->
            case arguments_parse(Arguments, EnvironmentLookup) of
                {ok, SpawnArguments} ->
                    SpawnEnvironment =  environment_parse(Environment,
                                                          ThreadsPerProcess,
                                                          Protocol, BufferSize,
                                                          EnvironmentLookup),
                    case cloudi_os_spawn:spawn(SpawnProcess,
                                               SpawnProtocol,
                                               SpawnSocketPath,
                                               Ports,
                                               SpawnFilename,
                                               SpawnArguments,
                                               SpawnEnvironment) of
                        {ok, OsPid} ->
                            ?LOG_INFO("OS pid ~p spawned ~p", [OsPid, Pids]),
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
            {ok, string_terminate(NewFilename)}
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
    {ok, lists:reverse([0 | Output])};

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
string_terminate([_ | _] = L) ->
    L ++ [0].

