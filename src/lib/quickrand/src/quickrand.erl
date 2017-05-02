%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Quick Random Number Generation==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2012-2017, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2012-2017 Michael Truog
%%% @version 1.7.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(quickrand).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([seed/0,
         uniform/1,
         uniform_cache/1,
         uniform_cache/2,
         strong_uniform/1,
         strong_float/0]).

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
-endif.
-endif.
-endif.
-endif.

-include("quickrand_internal.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

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
      I4:32/unsigned-integer>> = crypto:strong_rand_bytes(16),
    % only use positive integers for setting seed values
    IP1 = I1 + 1,
    IP2 = I2 + 1,
    IP3 = I3 + 1,
    IP4 = I4 + 1,
    _ = random_wh82:seed(IP1, IP2, IP3),
    _ = random_wh06_int:seed(IP1, IP2, IP3, IP4),
    ok = seed_rand(IP1, IP2, IP3),
    ok.

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

uniform(N) when is_integer(N), N < 1000000 ->
    % os:timestamp/0 is currently the quickest source of uniform randomness
    {_, _, MicroSecs} = os:timestamp(),
    (MicroSecs rem N) + 1;

uniform(N) when is_integer(N), N =< 16#ffffffff ->
    (erlang:abs(erlang:monotonic_time() bxor
                erlang:unique_integer()) rem N) + 1;

uniform(N) when is_integer(N), N =< 16#03ffffffffffffff ->
    % assuming exsplus/exsp for 58 bits, period 8.31e34
    rand:uniform(N);

uniform(N) when is_integer(N), N =< 21267638781707063560975648195455661513 ->
    % 21267638781707063560975648195455661513 ==
    %   2147483579 * 2147483543 * 2147483423 * 2147483123, period 2.66e36
    random_wh06_int:uniform(N);

uniform(N) when is_integer(N), N > 21267638781707063560975648195455661513 ->
    strong_uniform(N).

-else.

uniform(N) when is_integer(N), N < 1 ->
    erlang:exit(badarg);

uniform(1) ->
    1;

uniform(N) when is_integer(N), N < 1000000 ->
    % os:timestamp/0 is currently the quickest source of uniform randomness
    {_, _, MicroSecs} = os:timestamp(),
    (MicroSecs rem N) + 1;

uniform(N) when is_integer(N), N =< 27817185604309 ->
    % 27817185604309 == 30269 * 30307 * 30323, period 2.78e13
    random_wh82:uniform(N);

uniform(N) when is_integer(N), N =< 21267638781707063560975648195455661513 ->
    % 21267638781707063560975648195455661513 ==
    %   2147483579 * 2147483543 * 2147483423 * 2147483123, period 2.66e36
    random_wh06_int:uniform(N);

uniform(N) when is_integer(N), N > 21267638781707063560975648195455661513 ->
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

-ifdef(ERLANG_OTP_VERSION_18_FEATURES).

uniform_cache(N) when is_integer(N), N < 1 ->
    erlang:exit(badarg);

uniform_cache(1) ->
    1;

uniform_cache(N) when is_integer(N), N < 1000000 ->
    % os:timestamp/0 is currently the quickest source of uniform randomness
    {_, _, MicroSecs} = os:timestamp(),
    (MicroSecs rem N) + 1;

uniform_cache(N) when is_integer(N), N =< 16#ffffffff ->
    (erlang:abs(erlang:monotonic_time() bxor
                erlang:unique_integer()) rem N) + 1;

uniform_cache(N) when is_integer(N), N > 16#ffffffff ->
    quickrand_cache:uniform(N).

-else.

uniform_cache(N) when is_integer(N), N < 1 ->
    erlang:exit(badarg);

uniform_cache(1) ->
    1;

uniform_cache(N) when is_integer(N), N < 1000000 ->
    % os:timestamp/0 is currently the quickest source of uniform randomness
    {_, _, MicroSecs} = os:timestamp(),
    (MicroSecs rem N) + 1;

uniform_cache(N) when is_integer(N), N >= 1000000 ->
    quickrand_cache:uniform(N).

-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Quick uniform random number generation with cached data.===
%% Not meant for cryptographic purposes.
%% @end
%%-------------------------------------------------------------------------

-spec uniform_cache(N :: pos_integer(),
                    State :: quickrand_cache:state()) ->
    {pos_integer(), quickrand_cache:state()}.

-ifdef(ERLANG_OTP_VERSION_18_FEATURES).

uniform_cache(N, _) when is_integer(N), N < 1 ->
    erlang:exit(badarg);

uniform_cache(1, State) ->
    {1, State};

uniform_cache(N, State) when is_integer(N), N < 1000000 ->
    % os:timestamp/0 is currently the quickest source of uniform randomness
    {_, _, MicroSecs} = os:timestamp(),
    {(MicroSecs rem N) + 1, State};

uniform_cache(N, State) when is_integer(N), N =< 16#ffffffff ->
    {(erlang:abs(erlang:monotonic_time() bxor
                 erlang:unique_integer()) rem N) + 1, State};

uniform_cache(N, State) when is_integer(N), N > 16#ffffffff ->
    quickrand_cache:uniform(N, State).

-else.

uniform_cache(N, _) when is_integer(N), N < 1 ->
    erlang:exit(badarg);

uniform_cache(1, State) ->
    {1, State};

uniform_cache(N, State) when is_integer(N), N < 1000000 ->
    % os:timestamp/0 is currently the quickest source of uniform randomness
    {_, _, MicroSecs} = os:timestamp(),
    {(MicroSecs rem N) + 1, State};

uniform_cache(N, State) when is_integer(N), N >= 1000000 ->
    quickrand_cache:uniform(N, State).

-endif.

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
    <<I:Bits/integer>> = crypto:strong_rand_bytes(Bytes),
    (I rem N) + 1.

%%-------------------------------------------------------------------------
%% @doc
%% ===Return an Erlang floating point random number (double-precision).===
%% return a floating point value between 0.0 and 1.0, inclusive
%% @end
%%-------------------------------------------------------------------------

-spec strong_float() ->
    float().

strong_float() ->
    % 53 bits maximum for double precision floating point representation
    % (need to use a maximum value of math:pow(2, 53) with extra bit,
    %  i.e. 16#1fffffffffffff + 1)
    <<I:53/unsigned-integer, Bit:1, _:2>> = crypto:strong_rand_bytes(7),
    (I + Bit) * ?DBL_EPSILON_DIV2.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
seed_rand(B1, B2, B3) ->
    _ = rand:seed(exsp, {B1, B2, B3}),
    ok.
-else.
-ifdef(ERLANG_OTP_VERSION_18_FEATURES).
seed_rand(B1, B2, B3) ->
    _ = rand:seed(exsplus, {B1, B2, B3}),
    ok.
-else.
seed_rand(_, _, _) ->
    ok.
-endif.
-endif.

