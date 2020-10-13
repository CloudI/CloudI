%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:

%% Modified version of random module
%% to use Wichmann-Hill algorithm published on 2006
%% which succeeds the old AS183 algorithm in 1982.

%% Copyright (c) 2010 Kenji Rikitake All rights reserved.
%% Copyright (c) 2012-2020 Michael Truog All rights reserved.

%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2011. All Rights Reserved.
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% 
%% %CopyrightEnd%
%%
-module(random_wh06_int).

%% Reasonable random number generator (period is 2.66e36):
%%  The method is attributed to B. A. Wichmann and I. D. Hill
%%  See "Generating good pseudo-random numbers",
%%  Computational Statistics & Data Analysis 51 (2006) 1614-1622.

%% Explanation:
%%  Example C code, using 64-bit integer arithmetic
%%  (from Richard O'Keefe)
%%
%%   a = (a * 11600LL) % 2147483579;
%%   b = (b * 47003LL) % 2147483543;
%%   c = (c * 23000LL) % 2147483423;
%%   d = (d * 33000LL) % 2147483123;
%%   w = a/2147483579.0 + b/2147483543.0
%%     + c/2147483423.0 + d/2147483123.0;
%%   if (w >= 2.0) w -= 2.0;
%%   if (w >= 1.0) w -= 1.0;
%%   return w;
%%
%%  To avoid floating-point precision problems,
%%  it is best to use Erlang's native bigint support:
%%
%%   B1 = (11600 * A1) rem 2147483579,
%%   B2 = (47003 * A2) rem 2147483543,
%%   B3 = (23000 * A3) rem 2147483423,
%%   B4 = (33000 * A4) rem 2147483123,
%%   put(random_wh06_seed, {B1, B2, B3, B4}),
%%   I = ((B1 * 9903516371291919229607132747) +
%%        (B2 * 9903516537312557910938853791) +
%%        (B3 * 9903517090714727049595319831) +
%%        (B4 * 9903518474220420479167438931))
%%       rem 21267638781707063560975648195455661513,
%%
%%  (21267638781707063560975648195455661513 ==
%%   2147483579 * 2147483543 * 2147483423 * 2147483123,
%%   so w * 21267638781707063560975648195455661513.0 == I,
%%   based on modular arithmetic)
%%
%%  The algorithm provides 123 bits of randomness:
%%  1> math:log(21267638781707063560975648195455661513) / math:log(2).
%%

-export([seed0/0, seed/0, seed/1, seed/4,
         uniform/0, uniform/1,
         uniform_s/1, uniform_s/2,
         next_sequence/1]).

-define(PRIME1, 2147483579).
-define(PRIME2, 2147483543).
-define(PRIME3, 2147483423).
-define(PRIME4, 2147483123).

-define(SEED_DICT, random_wh06_seed).

%%-----------------------------------------------------------------------
%% The type of the state

-type seed() :: {pos_integer(), pos_integer(), pos_integer(), pos_integer()}.

%%-----------------------------------------------------------------------

-spec seed0() -> {123456789, 345678901, 567890123, 789012345}.

seed0() ->
    {123456789, 345678901, 567890123, 789012345}.

%% seed()
%%  Seed random number generation with default values

-spec seed() -> seed().

seed() ->
    reseed(seed0()).

%% seed({A1, A2, A3, A4}) 
%%  Seed random number generation 

-spec seed(seed()) ->
    'undefined' | seed().

seed({A1, A2, A3, A4}) ->
    seed(A1, A2, A3, A4).

%% seed(A1, A2, A3, A4) 
%%  Seed random number generation 

-spec seed(pos_integer(), pos_integer(), pos_integer(), pos_integer()) ->
    'undefined' | seed().

seed(A1, A2, A3, A4)
    when is_integer(A1), A1 > 0,
         is_integer(A2), A2 > 0,
         is_integer(A3), A3 > 0,
         is_integer(A4), A4 > 0 ->
    put(?SEED_DICT,
        {(A1 rem (?PRIME1 - 1)) + 1,
         (A2 rem (?PRIME2 - 1)) + 1,
         (A3 rem (?PRIME3 - 1)) + 1,
         (A4 rem (?PRIME4 - 1)) + 1}).

reseed({A1, A2, A3, A4}) ->
    case seed(A1, A2, A3, A4) of
        undefined -> seed0();
        {_,_,_,_} = Tuple -> Tuple
    end.        

%% uniform()
%%  Returns a random integer between
%%  0 and 21267638781707063560975648195455661512.

-spec uniform() -> non_neg_integer().

uniform() ->
    {A1, A2, A3, A4} = case get(?SEED_DICT) of
                           undefined -> seed0();
                           Tuple -> Tuple
                       end,

    B1 = (11600 * A1) rem ?PRIME1,
    B2 = (47003 * A2) rem ?PRIME2,
    B3 = (23000 * A3) rem ?PRIME3,
    B4 = (33000 * A4) rem ?PRIME4,

    put(?SEED_DICT, {B1, B2, B3, B4}),

    I = ((B1 * 9903516371291919229607132747) +
         (B2 * 9903516537312557910938853791) +
         (B3 * 9903517090714727049595319831) +
         (B4 * 9903518474220420479167438931))
        rem 21267638781707063560975648195455661513,
    I.

%% uniform(N) -> I
%%  Given an integer N > 1, N =< 21267638781707063560975648195455661513,
%%  uniform(N) returns a random integer
%%  between 1 and N.

-spec uniform(pos_integer()) -> pos_integer().

uniform(N)
    when is_integer(N), N > 1, N =< 21267638781707063560975648195455661513 ->
    (uniform() rem N) + 1.

%%% Functional versions

%% uniform_s(State) -> {I, NewState}
%%  Returns a random integer I, between
%%  0 and 21267638781707063560975648195455661512 (inclusive).

-spec uniform_s(seed()) -> {non_neg_integer(), seed()}.

uniform_s({A1, A2, A3, A4})
    when is_integer(A1), A1 > 0,
         is_integer(A2), A2 > 0,
         is_integer(A3), A3 > 0,
         is_integer(A4), A4 > 0 ->
    B1 = (11600 * A1) rem ?PRIME1,
    B2 = (47003 * A2) rem ?PRIME2,
    B3 = (23000 * A3) rem ?PRIME3,
    B4 = (33000 * A4) rem ?PRIME4,

    I = ((B1 * 9903516371291919229607132747) +
         (B2 * 9903516537312557910938853791) +
         (B3 * 9903517090714727049595319831) +
         (B4 * 9903518474220420479167438931))
        rem 21267638781707063560975648195455661513,

    {I, {B1, B2, B3, B4}}.

%% uniform_s(N, State) -> {I, NewState}
%%  Given an integer N > 1, N =< 21267638781707063560975648195455661513,
%%  uniform(N) returns a random integer
%%  between 1 and N.

-spec uniform_s(pos_integer(), seed()) -> {pos_integer(), seed()}.

uniform_s(N, State0)
    when is_integer(N), N > 1, N =< 21267638781707063560975648195455661513 ->
    {I, State1} = uniform_s(State0),
    {(I rem N) + 1, State1}.

%% generating another seed for multiple sequences

-spec next_sequence(seed()) -> seed().

next_sequence({A1, A2, A3, A4})
    when is_integer(A1), A1 > 0,
         is_integer(A2), A2 > 0,
         is_integer(A3), A3 > 0,
         is_integer(A4), A4 > 0 ->
    B1 = (11600 * A1) rem ?PRIME1,
    B2 = (47003 * A2) rem ?PRIME2,
    B3 = (23000 * A3) rem ?PRIME3,
    B4 = (33000 * A4) rem ?PRIME4,
    {B1, B2, B3, B4}.

