%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Concurrency==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2014-2023 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2014-2023 Michael Truog
%%% @version 2.0.6 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_concurrency).
-author('mjtruog at protonmail dot com').

%% external interface
-export([count/1,
         count/2]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===A count to be used for the number of service processes and threads used.===
%% An integer is an absolute count while a floating point number is a
%% multiplier on the number of logical processors available.
%% A floating point number is rounded (or floor-ed) to be close to the
%% logical processor count, to avoid extremes.
%% @end
%%-------------------------------------------------------------------------

-spec count(I :: pos_integer() | float()) ->
    pos_integer().

count(I)
    when is_integer(I), I > 0 ->
    I;
count(I)
    when is_float(I) ->
    count(I, erlang:system_info(schedulers)).

%%-------------------------------------------------------------------------
%% @doc
%% ===A count to be used for the number of service processes and threads used.===
%% An integer is an absolute count while a floating point number is a
%% multiplier on the number of logical processors available.
%% A floating point number is rounded (or floor-ed) to be close to the
%% logical processor count, to avoid extremes.
%% @end
%%-------------------------------------------------------------------------

-spec count(I :: pos_integer() | float(),
            Schedulers :: pos_integer()) ->
    pos_integer().

count(I, _)
    when is_integer(I), I > 0 ->
    I;
count(I, Schedulers)
    when is_float(I) ->
    true = Schedulers >= 1,
    if
        I > 1.0 ->
            floor(I * Schedulers);
        I > 0.0, I < 1.0 ->
            erlang:max(1, erlang:round(I * Schedulers));
        I == 1.0 ->
            Schedulers
    end.

