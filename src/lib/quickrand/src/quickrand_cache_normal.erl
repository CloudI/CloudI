%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Quick Normal Distribution Random Number Generation With Cached Data==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2017 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2017 Michael Truog
%%% @version 1.7.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(quickrand_cache_normal).
-author('mjtruog at protonmail dot com').

%% external interface
-export([box_muller/2,
         box_muller/3]).

-include("quickrand_math.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Process dictionary cache version of quickrand_normal:box_muller/2.===
%% @end
%%-------------------------------------------------------------------------

-spec box_muller(Mean :: number(),
                 StdDev :: number()) ->
    {Result1 :: float(), Result2 :: float()}.

box_muller(Mean, StdDev) ->
    X1 = quickrand_cache:floatR(),
    X2 = ?PI2 * quickrand_cache:floatR(),
    K = StdDev * math:sqrt(-2.0 * math:log(X1)),
    Result1 = Mean + K * math:cos(X2),
    Result2 = Mean + K * math:sin(X2),
    {Result1, Result2}.

%%-------------------------------------------------------------------------
%% @doc
%% ===State cache version of quickrand_normal:box_muller/2.===
%% @end
%%-------------------------------------------------------------------------

-spec box_muller(Mean :: number(),
                 StdDev :: number(),
                 State0 :: quickrand_cache:state()) ->
    {Result1 :: float(), Result2 :: float(), StateN :: quickrand_cache:state()}.

box_muller(Mean, StdDev, State0) ->
    {X1, State1} = quickrand_cache:floatR(State0),
    {R2, StateN} = quickrand_cache:floatR(State1),
    X2 = ?PI2 * R2,
    K = StdDev * math:sqrt(-2.0 * math:log(X1)),
    Result1 = Mean + K * math:cos(X2),
    Result2 = Mean + K * math:sin(X2),
    {Result1, Result2, StateN}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

