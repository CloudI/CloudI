%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI OS Resource Limit Configuration==
%%% Maximum limits can be set after "setcap 'CAP_SYS_RESOURCE=+ep' executable"
%%% (on Linux).
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2015, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2015 Michael Truog
%%% @version 1.4.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_core_i_os_rlimit).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([limit_validate/1,
         limit_format/1]).

-include("cloudi_core_i_constants.hrl").
-ifdef(CLOUDI_CORE_STANDALONE).
-define(OS_RLIMIT_DEFAULTS, []).
-else.
-include("cloudi_core_i_os_rlimit.hrl").
-endif.

-spec limit_validate(cloudi_service_api:limit_external()) ->
    {ok, list({cloudi_service_api:limit_external_key(),
               cloudi_service_api:limit_external_value()})} |
    {error, {service_options_limit_invalid, any()}}.

limit_validate(system) ->
    L = application:get_env(cloudi_core, limit, []),
    if
        is_list(L) ->
            limit_validate(limit_validate_defaults(), L, []);
        true ->
            {error, {service_options_limit_invalid, system}}
    end;
limit_validate(L)
    when is_list(L) ->
    limit_validate(limit_validate_defaults(), L, []);
limit_validate(Invalid) ->
    {error, {service_options_limit_invalid, Invalid}}.

limit_validate([], [], Output) ->
    {ok, lists:reverse(Output)};
limit_validate([{Key, Default} | Defaults], L, Output) ->
    case lists:keytake(Key, 1, L) of
        {value, {Key, Value}, NewL} ->
            case limit_validate_value(Key, Value, Output) of
                {ok, NewOutput} ->
                    limit_validate(Defaults, NewL, NewOutput);
                {error, _} = Error ->
                    Error
            end;
        false ->
            case limit_validate_value(Key, Default, Output) of
                {ok, NewOutput} ->
                    limit_validate(Defaults, L, NewOutput);
                {error, _} = Error ->
                    Error
            end
    end.

limit_validate_defaults() ->
    ?OS_RLIMIT_DEFAULTS.

limit_validate_value(_, undefined, Output) ->
    {ok, Output};
limit_validate_value(Key, Current, Output)
    when (is_integer(Current) andalso (Current >= 0)) orelse
         (Current =:= infinity) ->
    {ok, [{Key, [{current, Current}]} | Output]};
limit_validate_value(Key, [_ | _] = Values, Output) ->
    Defaults = [
        {current, undefined},
        {maximum, undefined}],
    case cloudi_proplists:take_values(Defaults, Values) of
        [Current, _]
        when not ((Current =:= undefined) orelse
                  (Current =:= infinity) orelse
                  (is_integer(Current) andalso (Current >= 0))) ->
            {error, {service_options_limit_invalid, Key}};
        [_, Maximum]
        when not ((Maximum =:= undefined) orelse
                  (Maximum =:= infinity) orelse
                  (is_integer(Maximum) andalso (Maximum >= 0))) ->
            {error, {service_options_limit_invalid, Key}};
        [undefined, undefined] ->
            {ok, Output};
        [Current, undefined] ->
            {ok, [{Key, [{current, Current}]} | Output]};
        [undefined, Maximum] ->
            {ok, [{Key, [{maximum, Maximum}]} | Output]};
        [Current, Maximum]
        when is_integer(Current) andalso is_integer(Maximum) andalso
             (Current =< Maximum) ->
            {ok, [{Key, [{current, Current}, {maximum, Maximum}]} | Output]};
        [Current, Maximum]
        when (is_integer(Current) orelse (Current =:= infinity)) andalso
             (Maximum =:= infinity) ->
            {ok, [{Key, [{current, Current}, {maximum, Maximum}]} | Output]};
        [_, _ | _] ->
            {error, {service_options_limit_invalid, Key}}
    end;
limit_validate_value(Key, _, _) ->
    {error, {service_options_limit_invalid, Key}}.

% same defines used in the C++ source code

% limit type
-define(CLOUDI_RLIMIT_AS,               1).
-define(CLOUDI_RLIMIT_CORE,             2).
-define(CLOUDI_RLIMIT_CPU,              3).
-define(CLOUDI_RLIMIT_DATA,             4).
-define(CLOUDI_RLIMIT_FSIZE,            5).
-define(CLOUDI_RLIMIT_MEMLOCK,          6).
-define(CLOUDI_RLIMIT_MSGQUEUE,         7).
-define(CLOUDI_RLIMIT_NICE,             8).
-define(CLOUDI_RLIMIT_NOFILE,           9).
-define(CLOUDI_RLIMIT_NPROC,           10).
-define(CLOUDI_RLIMIT_RSS,             11).
-define(CLOUDI_RLIMIT_RTPRIO,          12).
-define(CLOUDI_RLIMIT_RTTIME,          13).
-define(CLOUDI_RLIMIT_SIGPENDING,      14).
-define(CLOUDI_RLIMIT_STACK,           15).
-define(CLOUDI_RLIMIT_VMEM,            16).

% limit combination
-define(CLOUDI_RLIMITS_CURRENT_ONLY,    1).
-define(CLOUDI_RLIMITS_MAXIMUM_ONLY,    2).
-define(CLOUDI_RLIMITS_CURRENT_MAXIMUM, 3).

% limit special values
-define(CLOUDI_RLIMITS_VALUE_INFINITY,  16#ffffffffffffffff).

-spec limit_format(L :: list({cloudi_service_api:limit_external_key(),
                              cloudi_service_api:limit_external_value()})) ->
    binary().

limit_format(L) ->
    erlang:iolist_to_binary(limit_format_list(L)).

limit_format_list([]) ->
    [];
limit_format_list([H | L]) ->
    [limit_format_list_type(H) | limit_format_list(L)].

limit_format_list_type({Type, Values}) ->
    TypeI = if
        Type =:= as ->
            ?CLOUDI_RLIMIT_AS;
        Type =:= core ->
            ?CLOUDI_RLIMIT_CORE;
        Type =:= cpu ->
            ?CLOUDI_RLIMIT_CPU;
        Type =:= data ->
            ?CLOUDI_RLIMIT_DATA;
        Type =:= fsize ->
            ?CLOUDI_RLIMIT_FSIZE;
        Type =:= memlock ->
            ?CLOUDI_RLIMIT_MEMLOCK;
        Type =:= msgqueue ->
            ?CLOUDI_RLIMIT_MSGQUEUE;
        Type =:= nice ->
            ?CLOUDI_RLIMIT_NICE;
        Type =:= nofile ->
            ?CLOUDI_RLIMIT_NOFILE;
        Type =:= nproc ->
            ?CLOUDI_RLIMIT_NPROC;
        Type =:= rss ->
            ?CLOUDI_RLIMIT_RSS;
        Type =:= rtprio ->
            ?CLOUDI_RLIMIT_RTPRIO;
        Type =:= rttime ->
            ?CLOUDI_RLIMIT_RTTIME;
        Type =:= sigpending ->
            ?CLOUDI_RLIMIT_SIGPENDING;
        Type =:= stack ->
            ?CLOUDI_RLIMIT_STACK;
        Type =:= vmem ->
            ?CLOUDI_RLIMIT_VMEM
    end,
    Defaults = [
        {current, undefined},
        {maximum, undefined}],
    case cloudi_proplists:take_values(Defaults, Values) of
        [Current, undefined] ->
            CurrentI = limit_format_list_value(Current),
            <<TypeI, ?CLOUDI_RLIMITS_CURRENT_ONLY,
              CurrentI:64/unsigned-integer-native>>;
        [undefined, Maximum] ->
            MaximumI = limit_format_list_value(Maximum),
            <<TypeI, ?CLOUDI_RLIMITS_MAXIMUM_ONLY,
              MaximumI:64/unsigned-integer-native>>;
        [Current, Maximum] ->
            CurrentI = limit_format_list_value(Current),
            MaximumI = limit_format_list_value(Maximum),
            <<TypeI, ?CLOUDI_RLIMITS_CURRENT_MAXIMUM,
              CurrentI:64/unsigned-integer-native,
              MaximumI:64/unsigned-integer-native>>
    end.

limit_format_list_value(infinity) ->
    ?CLOUDI_RLIMITS_VALUE_INFINITY;
limit_format_list_value(Value)
    when is_integer(Value), Value >= 0 ->
    Value.

