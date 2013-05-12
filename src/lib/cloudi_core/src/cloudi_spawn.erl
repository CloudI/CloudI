%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Spawn==
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
%%% @version 1.2.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_spawn).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([start_internal/11,
         start_external/14]).

-include("cloudi_configuration.hrl").

-define(CREATE_INTERNAL, cloudi_services_internal_sup:create_internal).
-define(CREATE_EXTERNAL, cloudi_services_external_sup:create_external).

% environmental variables used by CloudI API initialization
-define(ENVIRONMENT_THREAD_COUNT,  "CLOUDI_API_INIT_THREAD_COUNT").
-define(ENVIRONMENT_PROTOCOL,      "CLOUDI_API_INIT_PROTOCOL").
-define(ENVIRONMENT_BUFFER_SIZE,   "CLOUDI_API_INIT_BUFFER_SIZE").

%%%------------------------------------------------------------------------
%%% External interface
%%%------------------------------------------------------------------------

start_internal(ProcessIndex, Module, Args, Timeout, Prefix,
               TimeoutAsync, TimeoutSync, DestRefresh,
               DestDenyList, DestAllowList, ConfigOptions)
    when is_integer(ProcessIndex), is_atom(Module), is_list(Args),
         is_integer(Timeout), is_list(Prefix),
         is_integer(TimeoutAsync), is_integer(TimeoutSync),
         is_record(ConfigOptions, config_service_options) ->
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
            trie:new(DestDenyList)
    end,
    DestAllow = if
        DestAllowList =:= undefined ->
            undefined;
        is_list(DestAllowList) ->
            trie:new(DestAllowList)
    end,
    case code:is_loaded(Module) of
        false ->
            {error, not_loaded};
        {file, _} ->
            ?CREATE_INTERNAL(ProcessIndex, Module, Args, Timeout,
                             Prefix, TimeoutAsync, TimeoutSync,
                             DestRefresh, DestDeny, DestAllow,
                             ConfigOptions)
    end.

start_external(ThreadsPerProcess,
               Filename, Arguments, Environment,
               Protocol, BufferSize, Timeout, Prefix,
               TimeoutAsync, TimeoutSync, DestRefresh,
               DestDenyList, DestAllowList, ConfigOptions)
    when is_integer(ThreadsPerProcess), ThreadsPerProcess > 0,
         is_list(Filename), is_list(Arguments), is_list(Environment),
         is_integer(BufferSize), is_integer(Timeout), is_list(Prefix),
         is_integer(TimeoutAsync), is_integer(TimeoutSync),
         is_record(ConfigOptions, config_service_options) ->
    true = (Protocol =:= tcp) orelse (Protocol =:= udp),
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
            trie:new(DestDenyList)
    end,
    DestAllow = if
        DestAllowList =:= undefined ->
            undefined;
        is_list(DestAllowList) ->
            trie:new(DestAllowList)
    end,
    {Pids, Ports} = cloudi_lists:itera2(fun(_, L1, L2, F) ->
        case ?CREATE_EXTERNAL(Protocol, BufferSize, Timeout,
                              Prefix, TimeoutAsync, TimeoutSync,
                              DestRefresh, DestDeny, DestAllow,
                              ConfigOptions) of
            {ok, Pid, Port} ->
                F([Pid | L1], [Port | L2]);
            {error, _} = Error ->
                lists:foreach(fun(P) -> erlang:exit(P, kill) end, L1),
                Error
        end
    end, [], [], lists:seq(1, ThreadsPerProcess)),
    NewEnvironment = environment_update(Environment,
                                        ThreadsPerProcess,
                                        Protocol,
                                        BufferSize),
    if
        Pids == error ->
            % an error occurred within ?CREATE_EXTERNAL
            {error, Ports};
        true ->
            SpawnProcess = cloudi_pool:get(cloudi_os_spawn),
            ProtocolChar = if
                Protocol =:= tcp ->
                    $t;
                Protocol =:= udp ->
                    $u
            end,
            case cloudi_os_spawn:spawn(SpawnProcess,
                                       ProtocolChar,
                                       Ports,
                                       string_terminate(Filename),
                                       arguments_parse(Arguments),
                                       environment_format(NewEnvironment)) of
                {ok, _} ->
                    {ok, Pids};
                {error, _} = Error ->
                    Error
            end
    end.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

string_terminate([_ | _] = L) ->
    L ++ [0].

arguments_parse([32 | Args]) ->
    arguments_parse(Args);

arguments_parse(Args) ->
    arguments_parse([], none, Args).

arguments_parse(Output, none, []) ->
    lists:reverse([0 | Output]);

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

environment_update(Environment0,
                   ThreadsPerProcess,
                   Protocol,
                   BufferSize) ->
    Environment1 = lists:keystore(?ENVIRONMENT_THREAD_COUNT, 1, Environment0,
                                  {?ENVIRONMENT_THREAD_COUNT,
                                   erlang:integer_to_list(ThreadsPerProcess)}),
    Environment2 = lists:keystore(?ENVIRONMENT_PROTOCOL, 1, Environment1,
                                  {?ENVIRONMENT_PROTOCOL,
                                   erlang:atom_to_list(Protocol)}),
    Environment3 = lists:keystore(?ENVIRONMENT_BUFFER_SIZE, 1, Environment2,
                                  {?ENVIRONMENT_BUFFER_SIZE,
                                   erlang:integer_to_list(BufferSize)}),
    Environment3.

environment_format(Environment) ->
    environment_format([], Environment).

environment_format(Output, []) ->
    Output;

environment_format(Output, [{K, V} | Environment]) ->
    environment_format(Output ++ K ++ [$=] ++ V ++ [0], Environment).

