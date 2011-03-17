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

-module(cloudi_spawn).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([start_internal/9,
         start_external/13]).

%%%------------------------------------------------------------------------
%%% External interface
%%%------------------------------------------------------------------------

start_internal(Module, Args, Timeout, Prefix,
               TimeoutAsync, TimeoutSync, DestRefresh,
               DestDenyList, DestAllowList)
    when is_atom(Module), is_list(Args), is_integer(Timeout), is_list(Prefix),
         is_integer(TimeoutAsync), is_integer(TimeoutSync) ->
    true = (DestRefresh == immediate_closest) or
           (DestRefresh == lazy_closest) or
           (DestRefresh == immediate_random) or
           (DestRefresh == lazy_random) or
           (DestRefresh == none),
    DestDeny = if
        DestDenyList == undefined ->
            undefined;
        is_list(DestDenyList) ->
            trie:new(DestDenyList)
    end,
    DestAllow = if
        DestAllowList == undefined ->
            undefined;
        is_list(DestAllowList) ->
            trie:new(DestAllowList)
    end,
    case code:is_loaded(Module) of
        false ->
            {error, not_loaded};
        {file, _} ->
            cloudi_job_sup:create_job(Module, Args, Timeout, Prefix,
                                      TimeoutAsync, TimeoutSync,
                                      DestRefresh, DestDeny, DestAllow)
    end.

start_external(ThreadsPerProcess,
               Filename, Arguments, Environment,
               Protocol, BufferSize, Timeout, Prefix,
               TimeoutAsync, TimeoutSync, DestRefresh,
               DestDenyList, DestAllowList)
    when is_integer(ThreadsPerProcess), ThreadsPerProcess > 0,
         is_list(Filename), is_list(Arguments), is_list(Environment),
         is_integer(BufferSize), is_integer(Timeout), is_list(Prefix),
         is_integer(TimeoutAsync), is_integer(TimeoutSync) ->
    true = (Protocol == tcp) or (Protocol == udp),
    true = (DestRefresh == immediate_closest) or
           (DestRefresh == lazy_closest) or
           (DestRefresh == immediate_random) or
           (DestRefresh == lazy_random) or
           (DestRefresh == none),
    DestDeny = if
        DestDenyList == undefined ->
            undefined;
        is_list(DestDenyList) ->
            trie:new(DestDenyList)
    end,
    DestAllow = if
        DestAllowList == undefined ->
            undefined;
        is_list(DestAllowList) ->
            trie:new(DestAllowList)
    end,
    {Pids, Ports} = lists2:itera2(fun(_, L1, L2, F) ->
        case cloudi_socket_sup:create_socket(Protocol, BufferSize, Timeout,
                                             Prefix, TimeoutAsync, TimeoutSync,
                                             DestRefresh,
                                             DestDeny, DestAllow) of
            {ok, Pid, Port} ->
                F([Pid | L1], [Port | L2]);
            {error, _} = Error ->
                lists:foreach(fun(P) -> erlang:exit(P, kill) end, L1),
                Error
        end
    end, [], [], lists:seq(1, ThreadsPerProcess)),
    if
        Pids == error ->
            % an error occurred in cloudi_socket_sup:create_socket
            {Pids, Ports};
        true ->
            SpawnProcess = pool2:get(cloudi_os_spawn),
            ProtocolChar = if Protocol == tcp -> $t; Protocol == udp -> $u end,
            case cloudi_os_spawn:spawn(SpawnProcess,
                                       ProtocolChar,
                                       Ports,
                                       terminate_string(Filename),
                                       parse_arguments(Arguments),
                                       format_environment(Environment)) of
                {ok, _} ->
                    {ok, Pids};
                {error, _} = Error ->
                    Error
            end
    end.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

terminate_string([_ | _] = L) ->
    L ++ [0].

parse_arguments([32 | Args]) ->
    parse_arguments(Args);

parse_arguments(Args) ->
    parse_arguments([], none, Args).

parse_arguments(Output, none, []) ->
    lists:reverse([0 | Output]);

parse_arguments(Output, none, [$' | T]) ->
    parse_arguments(Output, $', T);

parse_arguments(Output, none, [$" | T]) ->
    parse_arguments(Output, $", T);

parse_arguments(Output, none, [$` | T]) ->
    parse_arguments(Output, $`, T);

parse_arguments(Output, none, [32 | [32 | _] = T]) ->
    parse_arguments(Output, none, T);

parse_arguments(Output, none, [32 | T]) ->
    parse_arguments([0 | Output], none, T);

parse_arguments(Output, $', [$' | T]) ->
    parse_arguments(Output, none, T);

parse_arguments(Output, $", [$" | T]) ->
    parse_arguments(Output, none, T);

parse_arguments(Output, $`, [$` | T]) ->
    parse_arguments(Output, none, T);

parse_arguments(Output, Delim, [H | T]) ->
    parse_arguments([H | Output], Delim, T).

format_environment([]) ->
    [0];

format_environment(Environment) ->
    format_environment([], Environment).

format_environment(Output, []) ->
    Output;

format_environment(Output, [{K, V} | Environment]) ->
    format_environment(Output ++ K ++ [$=] ++ V ++ [0], Environment).

