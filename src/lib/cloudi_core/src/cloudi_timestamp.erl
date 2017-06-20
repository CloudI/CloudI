%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Timestamp operations==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2015-2017 Michael Truog <mjtruog at gmail dot com>
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
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2015-2017 Michael Truog
%%% @version 1.7.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_timestamp).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([timestamp/0,
         seconds/0,
         seconds_os/0,
         milliseconds/0,
         milliseconds_os/0,
         microseconds/0,
         microseconds_os/0,
         seconds_filter/3,
         uptime/0,
         uptime/1]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Return an Erlang VM timestamp.===
%% Not guaranteed to be strictly monotonically increasing
%% (on Erlang >= 18.0).
%% @end
%%-------------------------------------------------------------------------

-spec timestamp() -> erlang:timestamp().

timestamp() ->
    erlang:timestamp().

%%-------------------------------------------------------------------------
%% @doc
%% ===Seconds since the UNIX epoch.===
%% (The UNIX epoch is 1970-01-01T00:00:00)
%% @end
%%-------------------------------------------------------------------------

-spec seconds() -> non_neg_integer().

seconds() ->
    erlang:system_time(seconds).

%%-------------------------------------------------------------------------
%% @doc
%% ===Seconds since an undefined point in time, from the hardware.===
%% @end
%%-------------------------------------------------------------------------

-spec seconds_os() -> non_neg_integer().

seconds_os() ->
    os:system_time(seconds).

%%-------------------------------------------------------------------------
%% @doc
%% ===Milliseconds since the UNIX epoch.===
%% (The UNIX epoch is 1970-01-01T00:00:00)
%% @end
%%-------------------------------------------------------------------------

-spec milliseconds() -> non_neg_integer().

milliseconds() ->
    erlang:system_time(milli_seconds).

%%-------------------------------------------------------------------------
%% @doc
%% ===Milliseconds since an undefined point in time, from the hardware.===
%% @end
%%-------------------------------------------------------------------------

-spec milliseconds_os() -> non_neg_integer().

milliseconds_os() ->
    os:system_time(milli_seconds).

%%-------------------------------------------------------------------------
%% @doc
%% ===Microseconds since the UNIX epoch.===
%% (The UNIX epoch is 1970-01-01T00:00:00)
%% @end
%%-------------------------------------------------------------------------

-spec microseconds() -> non_neg_integer().

microseconds() ->
    erlang:system_time(micro_seconds).

%%-------------------------------------------------------------------------
%% @doc
%% ===Microseconds since an undefined point in time, from the hardware.===
%% @end
%%-------------------------------------------------------------------------

-spec microseconds_os() -> non_neg_integer().

microseconds_os() ->
    os:system_time(micro_seconds).

%%-------------------------------------------------------------------------
%% @doc
%% ===Filter a list of seconds since the UNIX epoch..===
%% The list is not ordered.
%% @end
%%-------------------------------------------------------------------------

-spec seconds_filter(L :: list(non_neg_integer()),
                     SecondsNow :: non_neg_integer(),
                     MaxPeriod :: pos_integer()) ->
    {Count :: non_neg_integer(),
     NewL :: list(non_neg_integer())}.

seconds_filter(L, SecondsNow, MaxPeriod) ->
    seconds_filter(L, [], 0, SecondsNow, MaxPeriod).

seconds_filter([], Output, Count, _, _) ->
    {Count, Output};
seconds_filter([Seconds | L], Output, Count, SecondsNow, MaxPeriod) ->
    if
        (SecondsNow < Seconds) orelse
        ((SecondsNow - Seconds) > MaxPeriod) ->
            seconds_filter(L, Output, Count,
                           SecondsNow, MaxPeriod);
        true ->
            seconds_filter(L, [Seconds | Output], Count + 1,
                           SecondsNow, MaxPeriod)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===The uptime of the Erlang node in seconds.===
%% @end
%%-------------------------------------------------------------------------

-spec uptime() ->
    integer().

uptime() ->
    uptime(second).

%%-------------------------------------------------------------------------
%% @doc
%% ===The uptime of the Erlang node..===
%% @end
%%-------------------------------------------------------------------------

-spec uptime(TimeUnit :: second | millisecond | microsecond | nanosecond) ->
    integer().

uptime(TimeUnit)
    when TimeUnit =:= second; TimeUnit =:= millisecond;
         TimeUnit =:= microsecond; TimeUnit =:= nanosecond ->
    Value = erlang:monotonic_time() - erlang:system_info(start_time),
    erlang:convert_time_unit(Value, native, TimeUnit).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

