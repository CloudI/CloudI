%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Quick Random Number Generation With Cached Data==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2017 Michael Truog <mjtruog at gmail dot com>
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
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2017 Michael Truog
%%% @version 1.7.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(quickrand_cache).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([float/0,
         float/1,
         floatL/0,
         floatL/1,
         floatM/0,
         floatM/1,
         floatR/0,
         floatR/1,
         init/0,
         init/1,
         new/0,
         new/1,
         rand_bytes/1,
         rand_bytes/2,
         uniform/1,
         uniform/2,
         uniform_range/2,
         uniform_range/3]).

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
    <<I:53/unsigned-integer, Bit:1, _:2>> = rand_bytes(7),
    (I + Bit) * ?DBL_EPSILON_DIV2.

%%-------------------------------------------------------------------------
%% @doc
%% ===State cache version of quickrand:strong_float/0.===
%% @end
%%-------------------------------------------------------------------------

-spec float(State :: state()) ->
    {float(), state()}.

float(State) ->
    {<<I:53/unsigned-integer, Bit:1, _:2>>, NewState} = rand_bytes(7, State),
    {(I + Bit) * ?DBL_EPSILON_DIV2, NewState}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Process dictionary cache version of quickrand:strong_floatL/0.===
%% @end
%%-------------------------------------------------------------------------

-spec floatL() ->
    float().

floatL() ->
    <<I:53/unsigned-integer, _:3>> = rand_bytes(7),
    I * ?DBL_EPSILON_DIV2.

%%-------------------------------------------------------------------------
%% @doc
%% ===State cache version of quickrand:strong_floatL/0.===
%% @end
%%-------------------------------------------------------------------------

-spec floatL(State :: state()) ->
    {float(), state()}.

floatL(State) ->
    {<<I:53/unsigned-integer, _:3>>, NewState} = rand_bytes(7, State),
    {I * ?DBL_EPSILON_DIV2, NewState}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Process dictionary cache version of quickrand:strong_floatM/0.===
%% @end
%%-------------------------------------------------------------------------

-spec floatM() ->
    float().

floatM() ->
    <<I:53/unsigned-integer, _:3>> = rand_bytes(7),
    if
        I == 0 ->
            ?DBL_EPSILON_DIV2;
        true ->
            I * ?DBL_EPSILON_DIV2
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===State cache version of quickrand:strong_floatM/0.===
%% @end
%%-------------------------------------------------------------------------

-spec floatM(State :: state()) ->
    {float(), state()}.

floatM(State) ->
    {<<I:53/unsigned-integer, _:3>>, NewState} = rand_bytes(7, State),
    Value = if
        I == 0 ->
            ?DBL_EPSILON_DIV2;
        true ->
            I * ?DBL_EPSILON_DIV2
    end,
    {Value, NewState}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Process dictionary cache version of quickrand:strong_floatR/0.===
%% @end
%%-------------------------------------------------------------------------

-spec floatR() ->
    float().

floatR() ->
    <<I:53/unsigned-integer, _:3>> = rand_bytes(7),
    (I + 1) * ?DBL_EPSILON_DIV2.

%%-------------------------------------------------------------------------
%% @doc
%% ===State cache version of quickrand:strong_floatR/0.===
%% @end
%%-------------------------------------------------------------------------

-spec floatR(State :: state()) ->
    {float(), state()}.

floatR(State) ->
    {<<I:53/unsigned-integer, _:3>>, NewState} = rand_bytes(7, State),
    {(I + 1) * ?DBL_EPSILON_DIV2, NewState}.

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

%%-------------------------------------------------------------------------
%% @doc
%% ===Process dictionary cache version of quickrand:strong_uniform_range/2.===
%% @end
%%-------------------------------------------------------------------------

-spec uniform_range(Min :: non_neg_integer(),
                    Max :: non_neg_integer()) ->
    non_neg_integer().

uniform_range(Min, Min) ->
    Min;
uniform_range(Min, Max)
    when is_integer(Min), is_integer(Max), Min < Max ->
    uniform(1 + Max - Min) - 1 + Min.

%%-------------------------------------------------------------------------
%% @doc
%% ===State cache version of quickrand:strong_uniform_range/2.===
%% @end
%%-------------------------------------------------------------------------

-spec uniform_range(Min :: non_neg_integer(),
                    Max :: non_neg_integer(),
                    State :: state()) ->
    {non_neg_integer(), state()}.

uniform_range(Min, Min, State) ->
    {Min, State};
uniform_range(Min, Max, State)
    when is_integer(Min), is_integer(Max), Min < Max ->
    {Value, NewState} = uniform(1 + Max - Min, State),
    {Value - 1 + Min, NewState}.

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
