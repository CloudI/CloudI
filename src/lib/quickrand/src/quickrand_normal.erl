%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Quick Normal Distribution Random Number Generation==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2017-2022 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2017-2022 Michael Truog
%%% @version 2.0.5 {@date} {@time}
%%%------------------------------------------------------------------------

-module(quickrand_normal).
-author('mjtruog at protonmail dot com').

%% external interface
-export([box_muller/2]).

-include("quickrand_math.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Box-Muller transformation for generating Gaussian noise.===
%% Has numerical stability problems when X1 is very close to zero.
%% This is a serious problem for stochastic modeling that is generating
%% millions of numbers.  If the result is used with added noise
%% (e.g., creating a process sleep value) then it isn't a problem.
%% @end
%%-------------------------------------------------------------------------

-spec box_muller(Mean :: number(),
                 StdDev :: number()) ->
    {Result1 :: float(), Result2 :: float()}.

box_muller(Mean, StdDev) ->
    % use Box-Muller transformation to generate Gaussian noise
    %
    % George Edward Pelham Box, Mervin Edgar Muller.
    % A Note on the Generation of Random Normal Deviates.
    % The Annals of Mathematical Statistics,
    % vol. 29, no. 2, pp. 610–611, 1958.
    X1 = quickrand:strong_floatR(),
    X2 = ?PI2 * quickrand:strong_floatR(),
    K = StdDev * math:sqrt(-2.0 * math:log(X1)),
    Result1 = Mean + K * math:cos(X2),
    Result2 = Mean + K * math:sin(X2),
    {Result1, Result2}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

