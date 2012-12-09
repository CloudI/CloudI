%% Modified version of random module
%% to use Wichmann-Hill algorithm published on 2006
%% which succeeds the old AS183 algorithm in 1982.

%% This version uses big integers, as Michael Truog proposed.
%% reference git commit data URL:
%% https://github.com/okeuday/sfmt-erlang/commit/209f0c1300d454f4d2c16d0c952a7e748677e8d3

%% Copyright (c) 2010 Kenji Rikitake All rights reserved.
%% Copyright (c) 2012 Michael Truog All rights reserved.

%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
-module(random_wh06_int).

%% Reasonable random number generator.
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
%%  The algorithm provides 124 bits of randomness:
%%  1> (math:log(21267638781707063560975648195455661513) / math:log(2)) + 1.
%%

-export([seed/0, seed/1, seed/4,
         uniform/0, uniform/1,
         uniform_s/1, uniform_s/2, seed0/0,
         next_sequence/1]).

%%-----------------------------------------------------------------------
%% The type of the state

-type ran() :: {integer(), integer(), integer(), integer()}.

%%-----------------------------------------------------------------------

-spec seed0() -> ran().

seed0() ->
    {123456789, 345678901, 567890123, 789012345}.

%% seed()
%%  Seed random number generation with default values

-spec seed() -> ran().

seed() ->
    reseed(seed0()).

%% seed({A1, A2, A3, A4}) 
%%  Seed random number generation 

-spec seed({integer(), integer(), integer(), integer()}) -> 'undefined' | ran().

seed({A1, A2, A3, A4}) ->
    seed(A1, A2, A3, A4).

%% seed(A1, A2, A3, A4) 
%%  Seed random number generation 

-spec seed(integer(), integer(), integer(), integer()) -> 'undefined' | ran().

%% zero is prohibited for each seed element
%% (by Richard O'Keefe)
seed(A1, A2, A3, A4)
    when A1 /= 0, A2 /= 0, A3 /= 0, A4 /= 0,
         is_integer(A1), is_integer(A2), is_integer(A3), is_integer(A4) ->
    put(random_wh06_seed,
        {abs(A1) rem 2147483579,
         abs(A2) rem 2147483543,
         abs(A3) rem 2147483423,
         abs(A4) rem 2147483123}).

-spec reseed(ran()) -> ran().

reseed({A1, A2, A3, A4}) ->
    case seed(A1, A2, A3, A4) of
        undefined -> seed0();
        {_,_,_,_} = Tuple -> Tuple
    end.        

%% uniform()
%%  Returns a random integer between
%%  0 and 21267638781707063560975648195455661512.

-spec uniform() -> integer().

uniform() ->
    {A1, A2, A3, A4} = case get(random_wh06_seed) of
                           undefined -> seed0();
                           Tuple -> Tuple
                       end,

    B1 = (11600 * A1) rem 2147483579,
    B2 = (47003 * A2) rem 2147483543,
    B3 = (23000 * A3) rem 2147483423,
    B4 = (33000 * A4) rem 2147483123,

    put(random_wh06_seed, {B1, B2, B3, B4}),

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

%% uniform_s(State) -> {F, NewState}
%%  Returns a random integer F, between
%%  0 and 21267638781707063560975648195455661512 (inclusive).

-spec uniform_s(ran()) -> {integer(), ran()}.

uniform_s({A1, A2, A3, A4})
    when A1 /= 0, A2 /= 0, A3 /= 0, A4 /= 0,
         is_integer(A1), is_integer(A2), is_integer(A3), is_integer(A4) ->
    B1 = (11600 * A1) rem 2147483579,
    B2 = (47003 * A2) rem 2147483543,
    B3 = (23000 * A3) rem 2147483423,
    B4 = (33000 * A4) rem 2147483123,

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

-spec uniform_s(pos_integer(), ran()) -> {integer(), ran()}.

uniform_s(N, State0)
    when is_integer(N), N > 1, N =< 21267638781707063560975648195455661513 ->
    {I, State1} = uniform_s(State0),
    {(I rem N) + 1, State1}.

%% generating another seed for multiple sequences

-spec next_sequence(ran()) -> ran().

next_sequence({A1, A2, A3, A4})
    when A1 /= 0, A2 /= 0, A3 /= 0, A4 /= 0,
         is_integer(A1), is_integer(A2), is_integer(A3), is_integer(A4) ->
    B1 = (11600 * A1) rem 2147483579,
    B2 = (47003 * A2) rem 2147483543,
    B3 = (23000 * A3) rem 2147483423,
    B4 = (33000 * A4) rem 2147483123,
    {B1, B2, B3, B4}.

