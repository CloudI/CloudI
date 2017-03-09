%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Quick Random Number Generation With Cached Data==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2017, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2017 Michael Truog
%%% @version 1.6.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(quickrand_cache).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([float/0,
         float/1,
         init/0,
         init/1,
         new/0,
         new/1,
         rand_bytes/1,
         rand_bytes/2,
         uniform/1,
         uniform/2]).

-record(quickrand_cache,
        {
            i :: non_neg_integer(),
            cache_size :: pos_integer(), % bytes
            cache :: binary()
        }).
-define(TUPLE_PDICT_KEY, quickrand_cache).

-type options() :: list({cache_size, Bytes :: pos_integer()}).
-type state() :: #quickrand_cache{}.
-export_type([options/0,
              state/0]).

-include("quickrand_internal.hrl").

%%-------------------------------------------------------------------------
%% @doc
%% ===Process dictionary cache version of quickrand:strong_float/0.===
%% @end
%%-------------------------------------------------------------------------

-spec float() ->
    float().

float() ->
    <<I:56/integer>> = rand_bytes(7),
    I / ?BITS56.

%%-------------------------------------------------------------------------
%% @doc
%% ===State cache version of quickrand:strong_float/0.===
%% @end
%%-------------------------------------------------------------------------

-spec float(State :: state()) ->
    {float(), state()}.

float(State) ->
    {<<I:56/integer>>, NewState} = rand_bytes(7, State),
    {I / ?BITS56, NewState}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Initialize cached process dictionary data.===
%% @end
%%-------------------------------------------------------------------------

-spec init() ->
    ok.

init() ->
    init([]).

%%-------------------------------------------------------------------------
%% @doc
%% ===Initialize cached process dictionary data with options.===
%% @end
%%-------------------------------------------------------------------------

-spec init(Options :: options()) ->
    ok.

init(Options) ->
    _ = erlang:put(?TUPLE_PDICT_KEY, state_to_tuple(new(Options))),
    ok.

%%-------------------------------------------------------------------------
%% @doc
%% ===Initialize cached state data.===
%% @end
%%-------------------------------------------------------------------------

-spec new() ->
    state().

new() ->
    new([]).

%%-------------------------------------------------------------------------
%% @doc
%% ===Initialize cached state data with options.===
%% @end
%%-------------------------------------------------------------------------

-spec new(Options :: options()) ->
    state().

new(Options) ->
    {CacheSize, []} = option(cache_size, Options),
    true = is_integer(CacheSize) andalso (CacheSize > 0),
    #quickrand_cache{i = 0,
                     cache_size = CacheSize,
                     cache = crypto:strong_rand_bytes(CacheSize)}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get random bytes using cached process dictionary data.===
%% @end
%%-------------------------------------------------------------------------

-spec rand_bytes(N :: pos_integer()) ->
    binary().

rand_bytes(N)
    when is_integer(N), N > 0 ->
    {I, CacheSize, Cache} = erlang:get(?TUPLE_PDICT_KEY),
    {Bytes, NewI, NewCache} = bytes_get(N, I, CacheSize, Cache),
    _ = erlang:put(?TUPLE_PDICT_KEY, {NewI, CacheSize, NewCache}),
    Bytes.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get random bytes using cached state data.===
%% @end
%%-------------------------------------------------------------------------

-spec rand_bytes(N :: pos_integer(),
                 State :: state()) ->
    {binary(), state()}.

rand_bytes(N, #quickrand_cache{i = I,
                               cache_size = CacheSize,
                               cache = Cache} = State)
    when is_integer(N), N > 0 ->
    {Bytes, NewI, NewCache} = bytes_get(N, I, CacheSize, Cache),
    {Bytes, State#quickrand_cache{i = NewI,
                                  cache = NewCache}}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Process dictionary cache version of quickrand:strong_uniform/1.===
%% @end
%%-------------------------------------------------------------------------

-spec uniform(N :: pos_integer()) ->
    pos_integer().

uniform(N) when is_integer(N), N < 1 ->
    erlang:exit(badarg);

uniform(1) ->
    1;

uniform(N) when is_integer(N), N > 1 ->
    Bytes = bytes(N),
    Bits = Bytes * 8,
    <<I:Bits/integer>> = rand_bytes(Bytes),
    (I rem N) + 1.

%%-------------------------------------------------------------------------
%% @doc
%% ===State cache version of quickrand:strong_uniform/1.===
%% @end
%%-------------------------------------------------------------------------

-spec uniform(N :: pos_integer(),
              State :: state()) ->
    {pos_integer(), state()}.

uniform(N, _) when is_integer(N), N < 1 ->
    erlang:exit(badarg);

uniform(1, State) ->
    {1, State};

uniform(N, State) when is_integer(N), N > 1 ->
    Bytes = bytes(N),
    Bits = Bytes * 8,
    {<<I:Bits/integer>>, NewState} = rand_bytes(Bytes, State),
    {(I rem N) + 1, NewState}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

bytes_get(N, I, CacheSize, Cache) ->
    BytesExist = CacheSize - I,
    BytesExistUsed = erlang:min(N, BytesExist),
    BytesOld = if
        BytesExistUsed == 0 ->
            <<>>;
        true ->
            binary:part(Cache, I, BytesExistUsed)
    end,
    if
        BytesExist < N ->
            BytesExtra = N - BytesExist,
            <<BytesNew:BytesExtra/binary-unit:8,
              NewCache:CacheSize/binary-unit:8>> =
                crypto:strong_rand_bytes(BytesExtra + CacheSize),
            {<<BytesOld/binary, BytesNew/binary>>, 0, NewCache};
        true ->
            {BytesOld, I + BytesExistUsed, Cache}
    end.

state_to_tuple(#quickrand_cache{i = I,
                                cache_size = CacheSize,
                                cache = Cache}) ->
    {I, CacheSize, Cache}.

option(Key, Options) ->
    case lists:keytake(Key, 1, Options) of
        false ->
            {ok, Value} = application:get_env(?APPLICATION, Key),
            {Value, Options};
        {value, {Key, Value}, NewOptions} ->
            {Value, NewOptions}
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

process_dictionary_basic_test() ->
    ok = init([{cache_size, 8}]),
    Random0 = rand_bytes(2),
    2 = byte_size(Random0),
    Random1 = rand_bytes(6),
    6 = byte_size(Random1),
    Random2 = rand_bytes(8),
    8 = byte_size(Random2),
    Random3 = rand_bytes(1024),
    1024 = byte_size(Random3),
    Random4 = rand_bytes(1023),
    1023 = byte_size(Random4),
    ok.

state_basic_test() ->
    State0 = new([{cache_size, 8}]),
    {Random0, State1} = rand_bytes(2, State0),
    2 = byte_size(Random0),
    {Random1, State2} = rand_bytes(6, State1),
    6 = byte_size(Random1),
    {Random2, State3} = rand_bytes(8, State2),
    8 = byte_size(Random2),
    {Random3, State4} = rand_bytes(1024, State3),
    1024 = byte_size(Random3),
    {Random4, _} = rand_bytes(1023, State4),
    1023 = byte_size(Random4),
    ok.

-endif.
