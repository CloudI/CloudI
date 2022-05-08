%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Quick Random Number Generation==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2012-2022 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2012-2022 Michael Truog
%%% @version 2.0.5 {@date} {@time}
%%%------------------------------------------------------------------------

-module(quickrand).
-author('mjtruog at protonmail dot com').

%% external interface
-export([lcg35/1,
         mcg35/1,
         mwc128_64/1,
         mwc256_64/1,
         mwc256_128/1,
         seed/0,
         strong_float/0,
         strong_floatL/0,
         strong_floatM/0,
         strong_floatR/0,
         strong_uniform/1,
         strong_uniform_range/2,
         uniform/1,
         uniform_cache/1,
         uniform_cache/2]).

-ifdef(ERLANG_OTP_VERSION_16).
-else.
-ifdef(ERLANG_OTP_VERSION_17).
-else.
-define(ERLANG_OTP_VERSION_18_FEATURES, true).
-ifdef(ERLANG_OTP_VERSION_18).
-else.
-ifdef(ERLANG_OTP_VERSION_19).
-else.
-define(ERLANG_OTP_VERSION_20_FEATURES, true).
-ifdef(OTP_RELEASE). % Erlang/OTP >= 21.0
% able to use -if/-elif here
-if(?OTP_RELEASE >= 25).
-endif.
-endif.
-endif.
-endif.
-endif.
-endif.

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
-define(TIME_UNIT_MICROSECOND, microsecond).
-else.
-define(TIME_UNIT_MICROSECOND, micro_seconds).
-endif.

-define(LCG35_PDICT_KEY, quickrand_lcg35_seed).
-define(MCG35_PDICT_KEY, quickrand_mcg35_seed).
-define(MWC128_PDICT_KEY, quickrand_mwc128_seed).
-define(MWC256_PDICT_KEY, quickrand_mwc256_seed).
-define(BITMASK_35, 16#7ffffffff).
-define(BITMASK_64, 16#ffffffffffffffff).
-define(BITMASK_128, 16#ffffffffffffffffffffffffffffffff).

-include("quickrand_internal.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===35-bit classical Linear Congruential Generator.===
%%
%% Based on Erlang/OTP 25.0-rc3 rand:lcg35/1.  Provides fast low-quality
%% pseudo-random number generation without using Erlang bignums.
%% lcg35/1 is slightly faster than mcg35/1.
%%
%% X1 = (A * X0 + C) rem M
%% A = 15319397, C = 15366142135, M = 2^35
%%
%% C is an odd value close to M / sqrt(5).
%% The period of lcg35/1 is M (i.e., 2^35).
%% TestU01 SmallCrush failed (p-value statistics are outside [0.001..0.999])
%% 8 out of 15 tests (with seed == 1).
%%
%% L'Ecuyer, Pierre.  Tables of linear congruential generators of
%% different sizes and good lattice structure.
%% Mathematics of Computation, vol. 68, no. 225, pp. 249–260, 1999.
%% https://www.ams.org/journals/mcom/1999-68-225/S0025-5718-99-00996-5/
%% @end
%%-------------------------------------------------------------------------

-spec lcg35(N :: 1..(1 + ?BITMASK_35)) ->
    1..(1 + ?BITMASK_35).

lcg35(N) ->
    Seed1 = case erlang:get(?LCG35_PDICT_KEY) of
        undefined ->
            <<Seed0:35/unsigned-integer,
              _:5>> = crypto:strong_rand_bytes(5),
            Seed0 + 1;
        Seed0 when is_integer(Seed0) ->
            Seed0
    end,
    SeedN = (15319397 * (Seed1 band ?BITMASK_35) +
             15366142135) band ?BITMASK_35,
    _ = erlang:put(?LCG35_PDICT_KEY, SeedN),
    (SeedN rem N) + 1.

%%-------------------------------------------------------------------------
%% @doc
%% ===35-bit Multiplicative Congruential Generator.===
%% (i.e., Lehmer random number generator, Park-Miller random number generator)
%%
%% Based on Erlang/OTP 25.0-rc3 rand:mcg35/1.  Provides fast low-quality
%% pseudo-random number generation without using Erlang bignums.
%% mcg35/1 is slightly slower than lcg35/1.
%%
%% X1 = (A * X0) rem M
%% A = 185852, B = 35, D = 31, M = 2^B - D
%%
%% D makes M prime (M == 34359738337) so X0 is always coprime.
%% The period of mcg35/1 is M (i.e., 2^35 - 31).
%% TestU01 SmallCrush failed (p-value statistics are outside [0.001..0.999])
%% 3 out of 15 tests (with seed == 1).
%%
%% L'Ecuyer, Pierre.  Tables of linear congruential generators of
%% different sizes and good lattice structure.
%% Mathematics of Computation, vol. 68, no. 225, pp. 249–260, 1999.
%% https://www.ams.org/journals/mcom/1999-68-225/S0025-5718-99-00996-5/
%% @end
%%-------------------------------------------------------------------------

-spec mcg35(N :: 1..(1 + ?BITMASK_35)) ->
    1..(1 + ?BITMASK_35 - 31).

mcg35(N) ->
    Seed1 = case erlang:get(?MCG35_PDICT_KEY) of
        undefined ->
            <<Seed0:35/unsigned-integer,
              _:5>> = crypto:strong_rand_bytes(5),
            Seed0 + 1;
        Seed0 when is_integer(Seed0) ->
            Seed0
    end,
    Value0 = 185852 * (Seed1 band ?BITMASK_35),
    ValueN = (Value0 band ?BITMASK_35) + 31 * (Value0 bsr 35),
    SeedN = if
        ValueN =< ?BITMASK_35 - 31 ->
            ValueN;
        true ->
            % an optimization to avoid rem
            ValueN - (1 + ?BITMASK_35 - 31)
    end,
    _ = erlang:put(?MCG35_PDICT_KEY, SeedN),
    (SeedN rem N) + 1.

%%-------------------------------------------------------------------------
%% @doc
%% ===128-bit state 64-bit value Marsaglia multiply-with-carry generator.===
%%
%% T = A * X0 + C0
%% C1 = T bsr 64
%% X1 = T band 16#ffffffffffffffff
%% A = 16#ff3a275c007b8ee6, 0 < X0, 0 < C0 < A - 1
%%
%% Simulates a multiplicative LCG with prime modulus
%% M = 16#ff3a275c007b8ee5ffffffffffffffff .
%% The period is approximately 2^127.
%%
%% Vigna, Sebastiano.
%% https://prng.di.unimi.it/MWC128.c
%% https://prng.di.unimi.it/#quality
%% TestU01 BigCrush passed (p-value statistics are in [0.001..0.999])
%% when starting from 100 equispaced points of the state space.
%%
%% Marsaglia, George.  Xorshift RNGs.
%% Journal of Statistical Software, vol. 8, no. 14, pp. 1–6, 2003-07.
%% https://doi.org/10.18637/jss.v008.i14
%% @end
%%-------------------------------------------------------------------------

-spec mwc128_64(N :: 1..(1 + ?BITMASK_64)) ->
    1..(1 + ?BITMASK_64).

mwc128_64(N) ->
    {X1, C1} = case erlang:get(?MWC128_PDICT_KEY) of
        undefined ->
            <<X0:63/unsigned-integer,
              C0:63/unsigned-integer,
              _:2>> = crypto:strong_rand_bytes(16),
            {X0 + 1, C0 + 1};
        {X0, C0} = Seed when is_integer(X0), is_integer(C0) ->
            Seed
    end,
    T = 16#ff3a275c007b8ee6 * X1 + C1,
    CN = T bsr 64,
    XN = T band ?BITMASK_64,
    _ = erlang:put(?MWC128_PDICT_KEY, {XN, CN}),
    (XN rem N) + 1.

%%-------------------------------------------------------------------------
%% @doc
%% ===256-bit state 64-bit value Marsaglia multiply-with-carry generator.===
%%
%% T = A * X0 + C0
%% X1 = Y0
%% Y1 = Z0
%% C1 = T bsr 64
%% Z1 = T band 16#ffffffffffffffff
%% A = 16#ff377e26f82da74a, 0 < X0, 0 < Y0, 0 < Z0, 0 < C0 < A - 1
%%
%% Simulates a multiplicative LCG with prime modulus
%% M = 16#ff377e26f82da749ffffffffffffffffffffffffffffffffffffffffffffffff .
%% The period is approximately 2^255.
%%
%% Vigna, Sebastiano.
%% https://prng.di.unimi.it/MWC256.c
%% https://prng.di.unimi.it/#quality
%% TestU01 BigCrush passed (p-value statistics are in [0.001..0.999])
%% when starting from 100 equispaced points of the state space.
%%
%% Marsaglia, George.  Xorshift RNGs.
%% Journal of Statistical Software, vol. 8, no. 14, pp. 1–6, 2003-07.
%% https://doi.org/10.18637/jss.v008.i14
%% @end
%%-------------------------------------------------------------------------

-spec mwc256_64(N :: 1..(1 + ?BITMASK_64)) ->
    1..(1 + ?BITMASK_64).

mwc256_64(N) ->
    {X1, Y1, Z1, C1} = case erlang:get(?MWC256_PDICT_KEY) of
        undefined ->
            <<X0:63/unsigned-integer,
              Y0:63/unsigned-integer,
              Z0:63/unsigned-integer,
              C0:63/unsigned-integer,
              _:4>> = crypto:strong_rand_bytes(32),
            {X0 + 1, Y0 + 1, Z0 + 1, C0 + 1};
        {X0, Y0, Z0, C0} = Seed
            when is_integer(X0), is_integer(Y0),
                 is_integer(Z0), is_integer(C0) ->
            Seed
    end,
    T = 16#ff377e26f82da74a * X1 + C1,
    XN = Y1,
    YN = Z1,
    CN = T bsr 64,
    ZN = T band ?BITMASK_64,
    _ = erlang:put(?MWC256_PDICT_KEY, {XN, YN, ZN, CN}),
    (ZN rem N) + 1.

%%-------------------------------------------------------------------------
%% @doc
%% ===256-bit state 128-bit value Marsaglia multiply-with-carry generator.===
%% Two iterations of mwc256_64/1 to provide 128 bits without extra latency.
%% @end
%%-------------------------------------------------------------------------

-spec mwc256_128(N :: 1..(1 + ?BITMASK_128)) ->
    1..(1 + ?BITMASK_128).

mwc256_128(N) ->
    {X1, Y1, Z1, C1} = case erlang:get(?MWC256_PDICT_KEY) of
        undefined ->
            <<X0:63/unsigned-integer,
              Y0:63/unsigned-integer,
              Z0:63/unsigned-integer,
              C0:63/unsigned-integer,
              _:4>> = crypto:strong_rand_bytes(32),
            {X0 + 1, Y0 + 1, Z0 + 1, C0 + 1};
        {X0, Y0, Z0, C0} = Seed
            when is_integer(X0), is_integer(Y0),
                 is_integer(Z0), is_integer(C0) ->
            Seed
    end,
    A = 16#ff377e26f82da74a,
    T0 = A * X1 + C1,
    X2 = Y1,
    Y2 = Z1,
    C2 = T0 bsr 64,
    Z2 = T0 band ?BITMASK_64,
    T1 = A * X2 + C2,
    XN = Y2,
    YN = Z2,
    CN = T1 bsr 64,
    ZN = T1 band ?BITMASK_64,
    _ = erlang:put(?MWC256_PDICT_KEY, {XN, YN, ZN, CN}),
    (((Z2 bsl 64) bor ZN) rem N) + 1.

%%-------------------------------------------------------------------------
%% @doc
%% ===Seed random number generation.===
%% @end
%%-------------------------------------------------------------------------

-spec seed() ->
    'ok'.

seed() ->
    % to provide better seeding than erlang:now() or os:timestamp()
    <<I1:32/unsigned-integer,
      I2:32/unsigned-integer,
      I3:32/unsigned-integer,
      I4:32/unsigned-integer,
      I5:58/unsigned-integer,
      I6:58/unsigned-integer,
      I7:58/unsigned-integer,
      _:2>> = crypto:strong_rand_bytes(38),
    % only use positive integers for setting seed values
    IP1 = I1 + 1,
    IP2 = I2 + 1,
    IP3 = I3 + 1,
    IP4 = I4 + 1,
    IP5 = I5 + 1,
    IP6 = I6 + 1,
    IP7 = I7 + 1,
    _ = random_wh82:seed(IP1, IP2, IP3),
    _ = random_wh06_int:seed(IP1, IP2, IP3, IP4),
    1 = lcg35(1),
    1 = mcg35(1),
    1 = mwc128_64(1),
    1 = mwc256_64(1),
    ok = seed_rand(IP5, IP6, IP7),
    ok.

%%-------------------------------------------------------------------------
%% @doc
%% ===Return an Erlang double-precision random number with the range [0.0 .. 1.0].===
%% @end
%%-------------------------------------------------------------------------

-spec strong_float() ->
    float().

strong_float() ->
    % 53 bits maximum for double precision floating point representation
    % (need to use a maximum value of math:pow(2, 53) with extra bit,
    %  i.e. 1 + 16#1fffffffffffff)
    <<Bit:1, I:53/unsigned-integer, _:2>> = crypto:strong_rand_bytes(7),
    if
        Bit == 1, I == 0 ->
            1.0;
        true ->
            I * ?DBL_EPSILON_DIV2
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Return an Erlang double-precision random number with the range [0.0 .. 1.0).===
%% Left portion of the 0.0 to 1.0 range.
%% @end
%%-------------------------------------------------------------------------

-spec strong_floatL() ->
    float().

strong_floatL() ->
    <<I:53/unsigned-integer, _:3>> = crypto:strong_rand_bytes(7),
    I * ?DBL_EPSILON_DIV2.

%%-------------------------------------------------------------------------
%% @doc
%% ===Return an Erlang double-precision random number with the range (0.0 .. 1.0).===
%% Middle portion of the 0.0 to 1.0 range.
%% @end
%%-------------------------------------------------------------------------

-spec strong_floatM() ->
    float().

strong_floatM() ->
    <<I:53/unsigned-integer, _:3>> = crypto:strong_rand_bytes(7),
    if
        I == 0 ->
            % almost never executes this case, an additional function call
            % is necessary to have a uniform distribution
            strong_floatM();
        true ->
            I * ?DBL_EPSILON_DIV2
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Return an Erlang double-precision random number with the range (0.0 .. 1.0].===
%% Right portion of the 0.0 to 1.0 range.
%% @end
%%-------------------------------------------------------------------------

-spec strong_floatR() ->
    float().

strong_floatR() ->
    <<I:53/unsigned-integer, _:3>> = crypto:strong_rand_bytes(7),
    (I + 1) * ?DBL_EPSILON_DIV2.

%%-------------------------------------------------------------------------
%% @doc
%% ===Strong uniform random number generation.===
%% @end
%%-------------------------------------------------------------------------

-spec strong_uniform(N :: pos_integer()) ->
    pos_integer().

strong_uniform(N) when is_integer(N), N < 1 ->
    erlang:exit(badarg);

strong_uniform(1) ->
    1;

strong_uniform(N) when is_integer(N), N > 1 ->
    Bytes = bytes(N),
    Bits = Bytes * 8,
    <<I:Bits/unsigned-integer>> = crypto:strong_rand_bytes(Bytes),
    (I rem N) + 1.

%%-------------------------------------------------------------------------
%% @doc
%% ===Strong uniform random number generation in a range.===
%% @end
%%-------------------------------------------------------------------------

-spec strong_uniform_range(Min :: non_neg_integer(),
                           Max :: non_neg_integer()) ->
    non_neg_integer().

strong_uniform_range(Min, Min) ->
    Min;
strong_uniform_range(Min, Max)
    when is_integer(Min), is_integer(Max), Min < Max ->
    strong_uniform(1 + Max - Min) - 1 + Min.

%%-------------------------------------------------------------------------
%% @doc
%% ===Quick uniform random number generation.===
%% Not meant for cryptographic purposes.
%% @end
%%-------------------------------------------------------------------------

-spec uniform(N :: pos_integer()) ->
    pos_integer().

-ifdef(ERLANG_OTP_VERSION_18_FEATURES).

uniform(N) when is_integer(N), N < 1 ->
    erlang:exit(badarg);

uniform(1) ->
    1;

uniform(N) when is_integer(N), N =< ?BITMASK_35 ->
    % lcg35 for 35 bits, period 3.44e10
    lcg35(N);

uniform(N) when is_integer(N), N =< 16#03ffffffffffffff ->
    % assuming exsplus/exsp for 58 bits, period 8.31e34
    rand:uniform(N);

uniform(N) when is_integer(N), N =< ?BITMASK_128 ->
    % mwc256_128 for 128 bits, period 5.79e76
    mwc256_128(N);

uniform(N) when is_integer(N), N > ?BITMASK_128 ->
    strong_uniform(N).

-else.

uniform(N) when is_integer(N), N < 1 ->
    erlang:exit(badarg);

uniform(1) ->
    1;

uniform(N) when is_integer(N), N =< ?BITMASK_35 ->
    % lcg35 for 35 bits, period 3.44e10
    lcg35(N);

uniform(N) when is_integer(N), N =< 27817185604309 ->
    % 27817185604309 == 30269 * 30307 * 30323, period 2.78e13
    random_wh82:uniform(N);

uniform(N) when is_integer(N), N =< ?BITMASK_128 ->
    % mwc256_128 for 128 bits, period 5.79e76
    mwc256_128(N);

uniform(N) when is_integer(N), N > ?BITMASK_128 ->
    strong_uniform(N).

-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Quick uniform random number generation with cached data.===
%% Not meant for cryptographic purposes.
%% @end
%%-------------------------------------------------------------------------

-spec uniform_cache(N :: pos_integer()) ->
    pos_integer().

uniform_cache(N) when is_integer(N), N < 1 ->
    erlang:exit(badarg);

uniform_cache(1) ->
    1;

uniform_cache(N) when is_integer(N), N =< ?BITMASK_35 ->
    % lcg35 for 35 bits, period 3.44e10
    lcg35(N);

uniform_cache(N) when is_integer(N), N > ?BITMASK_35 ->
    quickrand_cache:uniform(N).

%%-------------------------------------------------------------------------
%% @doc
%% ===Quick uniform random number generation with cached data.===
%% Not meant for cryptographic purposes.
%% @end
%%-------------------------------------------------------------------------

-spec uniform_cache(N :: pos_integer(),
                    State :: quickrand_cache:state()) ->
    {pos_integer(), quickrand_cache:state()}.

uniform_cache(N, _) when is_integer(N), N < 1 ->
    erlang:exit(badarg);

uniform_cache(1, State) ->
    {1, State};

uniform_cache(N, State) when is_integer(N), N =< ?BITMASK_35 ->
    % lcg35 for 35 bits, period 3.44e10
    {lcg35(N), State};

uniform_cache(N, State) when is_integer(N), N > ?BITMASK_35 ->
    quickrand_cache:uniform(N, State).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
seed_rand(IP1, IP2, IP3) ->
    _ = rand:seed(exsp, {IP1, IP2, IP3}),
    ok.
-else.
-ifdef(ERLANG_OTP_VERSION_18_FEATURES).
seed_rand(IP1, IP2, IP3) ->
    _ = rand:seed(exsplus, {IP1, IP2, IP3}),
    ok.
-else.
seed_rand(_, _, _) ->
    ok.
-endif.
-endif.

