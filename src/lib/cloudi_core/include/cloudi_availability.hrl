%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
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

-define(NATIVE_TIME_IN_DAY,
        cloudi_timestamp:
        convert(86400000000000, nanosecond, native)).
-define(NATIVE_TIME_IN_WEEK,
        cloudi_timestamp:
        convert(86400000000000 * 7, nanosecond, native)).
-define(NATIVE_TIME_IN_MONTH,
        cloudi_timestamp:
        convert(ceil(86400000000000 * 30.4375), nanosecond, native)).
-define(NATIVE_TIME_IN_YEAR,
        cloudi_timestamp:
        convert(ceil(86400000000000 * 365.25), nanosecond, native)).
-define(AVAILABILITY_ZERO, "0 %").

