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
%%% Copyright (c) 2012-2023 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2012-2023 Michael Truog
%%% @version 2.0.7 {@date} {@time}
%%%------------------------------------------------------------------------

-module(quickrand).
-author('mjtruog at protonmail dot com').

%% external interface
-export([lcg35x_32/1,
         mwc59x_32/1,
         mwc256/1,
         mwc256_64/1,
         mwc256_128/1,
         seed/0,
         seed/1,
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

-define(ALGORITHMS,
        [lcg35x,
         mwc59x, mwc256,
         rand, random_wh06_int, random_wh82]).
-ifdef(ERLANG_OTP_VERSION_18_FEATURES).
-define(UNIFORM_FUNCTION_ALGORITHMS, [lcg35x, mwc59x, rand, mwc256]).
-else.
-define(UNIFORM_FUNCTION_ALGORITHMS, [lcg35x, mwc59x, mwc256]).
-endif.

-define(LCG35X_PDICT_KEY, quickrand_lcg35x_seed).
-define(MWC59X_PDICT_KEY, quickrand_mwc59x_seed).
-define(MWC256_PDICT_KEY, quickrand_mwc256_seed).

-type algorithms() ::
    lcg35x |
    mwc59x | mwc256 |
    rand | random_wh06_int | random_wh82.
-export_type([algorithms/0]).

-include("quickrand_constants.hrl").
-include("quickrand_internal.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===35-bit state 32-bit value Linear Congruential Generators xor.===
%% Both algorithms used for the variables LCG and MCG provide
%% fast low-quality pseudo-random number generation without using
%% Erlang bignums.
%% ```
%% LCG:
%%   35-bit classical Linear Congruential Generator
%%   based on Erlang/OTP 25.0-rc3 rand:lcg35/1.
%%
%%   X1 = (A * X0 + C) rem M
%%   A = 15319397, C = 15366142135, M = 2^35
%%
%%   C is an odd value close to M / sqrt(5).
%%   The period is M (i.e., 2^35).
%%
%% MCG:
%%   35-bit Multiplicative Congruential Generator
%%   (i.e., Lehmer random number generator,
%%    Park-Miller random number generator)
%%   based on Erlang/OTP 25.0-rc3 rand:mcg35/1.
%%
%%   X1 = (A * X0) rem M
%%   A = 185852, B = 35, D = 31, M = 2^B - D
%%
%%   D makes M prime (M == 34359738337) so X0 is always coprime.
%%   The period is M (i.e., 2^35 - 31).
%% '''
%% The LCG and MCG are combined with xor to produce a 32-bit random number.
%% TestU01 SmallCrush/Crush/BigCrush have been used to test the 32-bit result
%% (both with the bits forward and reversed)
%% and the p-value statistics are in [0.0000001..0.9999999]
%% (when starting from 100 equispaced points of the state space).
%% The wider bounds (i.e., wider than [0.001..0.999]) are due to the
%% shorter period.
%%
%% mwc59x_32/1 is slighly more efficient but provides slightly less randomness
%% (same p-value statistics bounds but the separate sums of
%%  (1e-8  .. 1e-4] and [1 - 1e-4 .. 1 - 1e-8) are less extreme
%%  for lcg35x_32/1, i.e., the mwc59x_32/1 (1e-8  .. 1e-4] sum is 25.5% smaller
%%  and the mwc59x_32/1 [1 - 1e-4 .. 1 - 1e-8) sum is 16.1% larger while
%%  mwc59x_32/1 provides roughly a 1.08x speedup with Erlang/OTP 25.0).
%%
%% Pierre L'Ecuyer, Richard Simard.
%% TestU01: A C Library for Empirical Testing of Random Number Generators.
%% ACM Transactions on Mathematical Software, vol. 33, iss. 4, article 22, 2007.
%% http://portal.acm.org/citation.cfm?doid=1268776.1268777
%% http://simul.iro.umontreal.ca/testu01/tu01.html
%%
%% (A is selected from)
%% L'Ecuyer, Pierre.  Tables of linear congruential generators of
%% different sizes and good lattice structure.
%% Mathematics of Computation, vol. 68, no. 225, pp. 249–260, 1999.
%% https://www.ams.org/journals/mcom/1999-68-225/S0025-5718-99-00996-5/
%% https://www.iro.umontreal.ca/~lecuyer/myftp/papers/latrules99Errata.pdf
%% @end
%%-------------------------------------------------------------------------

-spec lcg35x_32(N :: 1..(1 + ?BITMASK_32)) ->
    1..(1 + ?BITMASK_32).

lcg35x_32(N) ->
    {LCG1, MCG1} = case erlang:get(?LCG35X_PDICT_KEY) of
        undefined ->
            <<LCG0:34/unsigned-integer,
              MCG0:34/unsigned-integer,
              _:4>> = crypto:strong_rand_bytes(9),
            {LCG0 + 1, MCG0 + 1};
        {LCG0, MCG0} = Seed when is_integer(LCG0), is_integer(MCG0) ->
            Seed
    end,
    LCGN = (15319397 * (LCG1 band ?BITMASK_35) +
            15366142135) band ?BITMASK_35,
    MCG2 = 185852 * (MCG1 band ?BITMASK_35),
    MCG3 = (MCG2 band ?BITMASK_35) + 31 * (MCG2 bsr 35),
    MCGN = if
        MCG3 =< ?BITMASK_35 - 31 ->
            MCG3;
        true ->
            % an optimization to avoid rem
            MCG3 - (1 + ?BITMASK_35 - 31)
    end,
    _ = erlang:put(?LCG35X_PDICT_KEY, {LCGN, MCGN}),
    (((LCG1 bsr 4) bxor (MCG1 bsr 2)) rem N) + 1.

%%-------------------------------------------------------------------------
%% @doc
%% ===59-bit state 32-bit value Marsaglia multiply-with-carry generator xor.===
%% ```
%% T = A * X0 + C0
%% C1 = T bsr 32
%% X1 = T band 16#ffffffff
%% A = 16#7fa6502, 0 < X0, 0 < C0 < A - 1
%%
%% Simulates a multiplicative LCG with prime modulus
%% M = 16#7fa6501ffffffff (M = A * 2^32 - 1).
%% The period is approximately 2^58.
%% '''
%% X1 and C1 are combined with xor to produce a 32-bit random number.
%% TestU01 SmallCrush/Crush/BigCrush have been used to test the 32-bit result
%% (both with the bits forward and reversed)
%% and the p-value statistics are in [0.0000001..0.9999999]
%% (when starting from 100 equispaced points of the state space).
%% The wider bounds (i.e., wider than [0.001..0.999]) are due to the
%% shorter period.
%%
%% rand:mwc59/1 in Erlang/OTP 25.0 is similar.  However, usage of rand:mwc59/1
%% with rand:mwc59_value32/1 clearly fails the TestU01 Crush and BigCrush tests
%% (e.g., with X0 and C0 initially set to 1).  mwc59x_32/1 was created
%% to provide more statistically significant randomness than is possible when
%% using rand:mwc59/1 .
%%
%% Pierre L'Ecuyer, Richard Simard.
%% TestU01: A C Library for Empirical Testing of Random Number Generators.
%% ACM Transactions on Mathematical Software, vol. 33, iss. 4, article 22, 2007.
%% http://portal.acm.org/citation.cfm?doid=1268776.1268777
%% http://simul.iro.umontreal.ca/testu01/tu01.html
%%
%% Marsaglia, George.  Xorshift RNGs.
%% Journal of Statistical Software, vol. 8, no. 14, pp. 1–6, 2003-07.
%% https://doi.org/10.18637/jss.v008.i14
%% @end
%%-------------------------------------------------------------------------

-spec mwc59x_32(N :: 1..(1 + ?BITMASK_32)) ->
    1..(1 + ?BITMASK_32).

mwc59x_32(N) ->
    {X1, C1} = case erlang:get(?MWC59X_PDICT_KEY) of
        undefined ->
            <<X0:31/unsigned-integer,
              C0:26/unsigned-integer,
              _:7>> = crypto:strong_rand_bytes(8),
            {X0 + 1, C0 + 1};
        {X0, C0} = Seed
            when is_integer(X0), is_integer(C0) ->
            Seed
    end,
    T = (16#7fa6502 * X1 + C1) band ?BITMASK_59,
    CN = T bsr 32,
    XN = T band ?BITMASK_32,
    _ = erlang:put(?MWC59X_PDICT_KEY, {XN, CN}),
    ((XN bxor (CN bsl 3)) rem N) + 1.

%%-------------------------------------------------------------------------
%% @doc
%% ===256-bit state Marsaglia multiply-with-carry generator.===
%% ```
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
%% '''
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

-spec mwc256(N :: pos_integer()) ->
    pos_integer().

mwc256(N) when is_integer(N), N > 0 ->
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
    mwc256_next(N, 0, X1, Y1, Z1, C1, N).

%%-------------------------------------------------------------------------
%% @doc
%% ===256-bit state 64-bit value Marsaglia multiply-with-carry generator.===
%% mwc256/1 limited to a 64-bit return value for less latency.
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
%% mwc256/1 limited to a 128-bit return value for less latency.
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
%% ===Randomized seeding of random number generators.===
%% Backwards-compatible seeding of random number generators for this
%% module's uniform prefix functions and the external modules used
%% (rand, random_wh06_int and random_wh82).
%% Use seed/1 to seed specific random number generators.
%%
%% Instead of using this function, it is better to use a jump function
%% for obtaining non-overlapping sequences, if a jump function is available
%% and the number of Erlang processes used is limited
%% (to ensure concurrent usage of the same algorithm has no collisions).
%% @end
%%-------------------------------------------------------------------------

-spec seed() ->
    ok.

seed() ->
    seed([quickrand, rand, random_wh06_int, random_wh82]).

%%-------------------------------------------------------------------------
%% @doc
%% ===Randomized seeding of specific random number generators.===
%% Instead of using this function, it is better to use a jump function
%% for obtaining non-overlapping sequences, if a jump function is available
%% and the number of Erlang processes used is limited
%% (to ensure concurrent usage of the same algorithm has no collisions).
%% @end
%%-------------------------------------------------------------------------

-spec seed(nonempty_list(all | quickrand | algorithms())) ->
    ok.

seed([_ | _] = L) ->
    seed_algorithms(L).

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

strong_uniform(N) when is_integer(N) ->
    if
        N < 1 ->
            erlang:exit(badarg);
        N == 1 ->
            1;
        N > 1 ->
            Bytes = bytes(N),
            Bits = Bytes * 8,
            <<I:Bits/unsigned-integer>> = crypto:strong_rand_bytes(Bytes),
            (I rem N) + 1
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Strong uniform random number generation in a range.===
%% @end
%%-------------------------------------------------------------------------

-spec strong_uniform_range(Min :: integer(),
                           Max :: non_neg_integer()) ->
    integer().

strong_uniform_range(Min, Max)
    when is_integer(Min), is_integer(Max), Max >= 0 ->
    if
        Min == Max ->
            Min;
        Min < Max ->
            strong_uniform(1 + Max - Min) - 1 + Min
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Quick uniform random number generation.===
%% Not meant for cryptographic purposes.
%% @end
%%-------------------------------------------------------------------------

-spec uniform(N :: pos_integer()) ->
    pos_integer().

-ifdef(ERLANG_OTP_VERSION_18_FEATURES).

uniform(N) when is_integer(N) ->
    if
        N < 1 ->
            erlang:exit(badarg);
        N == 1 ->
            1;
        N =< ?BITMASK_32 ->
            % 32 bits, period 3.44e10
            lcg35x_32(N);
        N =< 16#3ffffffffffffff ->
            % assuming exsplus/exsp for 58 bits, period 8.31e34
            rand:uniform(N);
        N =< ?BITMASK_64 ->
            % 64 bits, period 5.79e76
            mwc256_64(N);
        N =< ?BITMASK_128 ->
            % 128 bits, period 5.79e76
            mwc256_128(N);
        N =< ?BITMASK_1024 ->
            % mwc256 for up to 1024 bits, period 5.79e76
            mwc256(N);
        true ->
            strong_uniform(N)
    end.

-else.

uniform(N) when is_integer(N) ->
    if
        N < 1 ->
            erlang:exit(badarg);
        N == 1 ->
            1;
        N =< ?BITMASK_32 ->
            % 32 bits, period 3.44e10
            lcg35x_32(N);
        N =< ?BITMASK_64 ->
            % 64 bits, period 5.79e76
            mwc256_64(N);
        N =< ?BITMASK_128 ->
            % 128 bits, period 5.79e76
            mwc256_128(N);
        N =< ?BITMASK_1024 ->
            % mwc256 for up to 1024 bits, period 5.79e76
            mwc256(N);
        true ->
            strong_uniform(N)
    end.

-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Quick uniform random number generation with cached data.===
%% Not meant for cryptographic purposes.
%% @end
%%-------------------------------------------------------------------------

-spec uniform_cache(N :: pos_integer()) ->
    pos_integer().

uniform_cache(N) when is_integer(N) ->
    if
        N < 1 ->
            erlang:exit(badarg);
        N == 1 ->
            1;
        N =< ?BITMASK_32 ->
            % 32 bits, period 3.44e10
            lcg35x_32(N);
        N > ?BITMASK_32 ->
            quickrand_cache:uniform(N)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Quick uniform random number generation with cached data.===
%% Not meant for cryptographic purposes.
%% @end
%%-------------------------------------------------------------------------

-spec uniform_cache(N :: pos_integer(),
                    State :: quickrand_cache:state()) ->
    {pos_integer(), quickrand_cache:state()}.

uniform_cache(N, State) when is_integer(N) ->
    if
        N < 1 ->
            erlang:exit(badarg);
        N == 1 ->
            {1, State};
        N =< ?BITMASK_32 ->
            % 32 bits, period 3.44e10
            {lcg35x_32(N), State};
        N > ?BITMASK_32 ->
            quickrand_cache:uniform(N, State)
    end.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

mwc256_next(0, I, X0, Y0, Z0, C0, N) ->
    _ = erlang:put(?MWC256_PDICT_KEY, {X0, Y0, Z0, C0}),
    (I rem N) + 1;
mwc256_next(B, I, X0, Y0, Z0, C0, N) ->
    T = 16#ff377e26f82da74a * X0 + C0,
    XN = Y0,
    YN = Z0,
    CN = T bsr 64,
    ZN = T band ?BITMASK_64,
    mwc256_next(B bsr 64, (I bsl 64) bor ZN, XN, YN, ZN, CN, N).

seed_algorithms([]) ->
    ok;
seed_algorithms([all]) ->
    seed_algorithms(?ALGORITHMS);
seed_algorithms([quickrand | L]) ->
    seed_algorithms(lists:usort(?UNIFORM_FUNCTION_ALGORITHMS ++ L));
seed_algorithms([lcg35x | L]) ->
    1 = lcg35x_32(1),
    seed_algorithms(L);
seed_algorithms([mwc59x | L]) ->
    1 = mwc59x_32(1),
    seed_algorithms(L);
seed_algorithms([mwc256 | L]) ->
    1 = mwc256_64(1),
    seed_algorithms(L);
seed_algorithms([rand | L]) ->
    <<I1:58/unsigned-integer,
      I2:58/unsigned-integer,
      I3:58/unsigned-integer,
      _:2>> = crypto:strong_rand_bytes(22),
    IP1 = I1 + 1,
    IP2 = I2 + 1,
    IP3 = I3 + 1,
    ok = seed_algorithms_rand(IP1, IP2, IP3),
    seed_algorithms(L);
seed_algorithms([random_wh06_int | L]) ->
    <<I1:32/unsigned-integer,
      I2:32/unsigned-integer,
      I3:32/unsigned-integer,
      I4:32/unsigned-integer>> = crypto:strong_rand_bytes(16),
    IP1 = I1 + 1,
    IP2 = I2 + 1,
    IP3 = I3 + 1,
    IP4 = I4 + 1,
    _ = random_wh06_int:seed(IP1, IP2, IP3, IP4),
    seed_algorithms(L);
seed_algorithms([random_wh82 | L]) ->
    <<I1:32/unsigned-integer,
      I2:32/unsigned-integer,
      I3:32/unsigned-integer>> = crypto:strong_rand_bytes(12),
    IP1 = I1 + 1,
    IP2 = I2 + 1,
    IP3 = I3 + 1,
    _ = random_wh82:seed(IP1, IP2, IP3),
    seed_algorithms(L).

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
seed_algorithms_rand(IP1, IP2, IP3) ->
    _ = rand:seed(exsp, {IP1, IP2, IP3}),
    ok.
-else.
-ifdef(ERLANG_OTP_VERSION_18_FEATURES).
seed_algorithms_rand(IP1, IP2, IP3) ->
    _ = rand:seed(exsplus, {IP1, IP2, IP3}),
    ok.
-else.
seed_algorithms_rand(_, _, _) ->
    ok.
-endif.
-endif.

