%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Math Functions==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2022-2023 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2022-2023 Michael Truog
%%% @version 2.0.7 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_math).
-author('mjtruog at protonmail dot com').

%% external interface
-export([erfcinv/1,
         erfinv/1]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Inverse Complementary Error Function.===
%% The inverse of math:erfc/1.
%% @end
%%-------------------------------------------------------------------------

-spec erfcinv(X :: float()) ->
    float().

erfcinv(X)
    when X > 0.0, X < 2.0 ->
    erfinv(1 - X).

%%-------------------------------------------------------------------------
%% @doc
%% ===Inverse Error Function.===
%% The inverse of math:erf/1.
%%
%% Giles, M.. Approximating the erfinv function.
%% GPU Computing Gems, Jade Edition, pp. 109-116. 2011.
%% https://people.maths.ox.ac.uk/gilesm/codes/erfinv/gems.pdf
%% https://stackoverflow.com/a/49743348
%% @end
%%-------------------------------------------------------------------------

-spec erfinv(X :: float()) ->
    float().

erfinv(X)
    when X == 0.0 ->
    0.0;
erfinv(X)
    when X > -1.0, X < 1.0 ->
    W = math:log(1 - X * X),
    PN = if
        abs(W) > 6.125 ->
            % maximum ulp error = 2.35793
            P0 = 3.03697567e-10,
            P1 = P0 * W + 2.93243101e-8,
            P2 = P1 * W + 1.22150334e-6,
            P3 = P2 * W + 2.84108955e-5,
            P4 = P3 * W + 3.93552968e-4,
            P5 = P4 * W + 3.02698812e-3,
            P6 = P5 * W + 4.83185798e-3,
            P7 = P6 * W - 2.64646143e-1,
            P7 * W + 8.40016484e-1;
        true ->
            % maximum ulp error = 2.35002
            P0 = 5.43877832e-9,
            P1 = P0 * W + 1.43285448e-7,
            P2 = P1 * W + 1.22774793e-6,
            P3 = P2 * W + 1.12963626e-7,
            P4 = P3 * W - 5.61530760e-5,
            P5 = P4 * W - 1.47697632e-4,
            P6 = P5 * W + 2.31468678e-3,
            P7 = P6 * W + 1.15392581e-2,
            P8 = P7 * W - 2.32015476e-1,
            P8 * W + 8.86226892e-1
    end,
    E0 = X * PN,
    % one iteration of Halley's method to refine estimate of inverse erf
    F0E = math:erf(E0) - X,
    F1E = (2.0 / math:pow(math:pi(), 0.5)) * math:exp(-(E0 * E0)),
    F2E = -2 * E0 * F1E,
    EN = E0 - (2 * F0E * F1E) / ((2 * F1E * F1E) - (F0E * F2E)),
    EN.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-include("cloudi_core_i_test.hrl").

module_test_() ->
    {timeout, ?TEST_TIMEOUT, [
        {"erfinv tests", ?_assertOk(t_erfinv())}
    ]}.

t_erfinv() ->
    true = erfinv(0.0) =:= 0.0,
    true = erfinv(0.1) =:= 0.08885599049425769,
    true = erfinv(0.2) =:= 0.1791434546212917,
    true = erfinv(0.3) =:= 0.2724627147267543,
    true = erfinv(0.4) =:= 0.37080715859355795,
    true = erfinv(0.5) =:= 0.47693627620446993,
    true = erfinv(0.6) =:= 0.5951160814499947,
    true = erfinv(0.7) =:= 0.7328690779592167,
    true = erfinv(0.8) =:= 0.9061938024368235,
    true = erfinv(0.9) =:= 1.1630871536766743,
    true = erfinv(0.99) =:= 1.8213863677184494,
    true = erfinv(0.999) =:= 2.326753765513533,
    true = erfinv(0.9999) =:= 2.751063905712038,
    true = erfinv(0.99999) =:= 3.1234132743422784,
    ok.

-endif.
