%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% Fuctions Common to cloudi and cloudi_service
%%%
%%% MIT License
%%%
%%% Copyright (c) 2022 Michael Truog <mjtruog at protonmail dot com>
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
%%%------------------------------------------------------------------------

timeout_period_to_milliseconds(limit_min, Min, _) ->
    Min;
timeout_period_to_milliseconds(limit_max, _, Max) ->
    Max;
timeout_period_to_milliseconds({Multiplier, Unit}, Min, Max)
    when is_integer(Multiplier), Multiplier >= 1 ->
    Timeout = if
        Unit =:= seconds orelse Unit =:= second ->
            Multiplier * 1000;
        Unit =:= minutes orelse Unit =:= minute ->
            Multiplier * 60000;
        Unit =:= hours orelse Unit =:= hour ->
            Multiplier * 3600000;
        Unit =:= days orelse Unit =:= day ->
            Multiplier * 86400000
    end,
    true = Timeout >= Min andalso Timeout =< Max,
    Timeout;
timeout_period_to_milliseconds(Timeout, _, Max)
    when is_integer(Timeout), Timeout >= 0, Timeout =< Max ->
    Timeout.

