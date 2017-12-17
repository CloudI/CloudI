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
%%% @version 1.7.3 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_timestamp).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([convert/3,
         timestamp/0,
         native/0,
         native_monotonic/0,
         native_os/0,
         seconds/0,
         seconds_monotonic/0,
         seconds_os/0,
         milliseconds/0,
         milliseconds_monotonic/0,
         milliseconds_os/0,
         microseconds/0,
         microseconds_monotonic/0,
         microseconds_os/0,
         nanoseconds/0,
         nanoseconds_monotonic/0,
         nanoseconds_os/0,
         seconds_filter/3,
         uptime/0,
         uptime/1]).

-type time_unit() :: second | millisecond | microsecond | nanosecond |
                     native | perf_counter | pos_integer().
% assuming that no clocks are set to before the
% UNIX epoch (1970-01-01T00:00:00)
-type seconds_epoch() :: non_neg_integer().
-type milliseconds_epoch() :: non_neg_integer().
-type microseconds_epoch() :: non_neg_integer().
-type nanoseconds_epoch() :: non_neg_integer().
% monotonic time values may be negative
-type seconds_monotonic() :: integer().
-type milliseconds_monotonic() :: integer().
-type microseconds_monotonic() :: integer().
-type nanoseconds_monotonic() :: integer().
% a value coming from the hardware starting at some undefined point in time
-type seconds_hardware() :: non_neg_integer().
-type milliseconds_hardware() :: non_neg_integer().
-type microseconds_hardware() :: non_neg_integer().
-type nanoseconds_hardware() :: non_neg_integer().
-export_type([time_unit/0,
              seconds_epoch/0,
              milliseconds_epoch/0,
              microseconds_epoch/0,
              nanoseconds_epoch/0,
              seconds_monotonic/0,
              milliseconds_monotonic/0,
              microseconds_monotonic/0,
              nanoseconds_monotonic/0,
              seconds_hardware/0,
              milliseconds_hardware/0,
              microseconds_hardware/0,
              nanoseconds_hardware/0]).

-include("cloudi_core_i_constants.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Convert between time units.===
%% @end
%%-------------------------------------------------------------------------

-spec convert(Time :: integer(),
              FromUnit :: time_unit(),
              ToUnit :: time_unit()) ->
    integer().

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
convert(Time, FromUnit, ToUnit) ->
    erlang:convert_time_unit(Time, FromUnit, ToUnit).
-else.
convert(Time, FromUnit, ToUnit) ->
    FromUnitDeprecated = if
        FromUnit =:= second ->
            seconds;
        FromUnit =:= millisecond ->
            milli_seconds;
        FromUnit =:= microsecond ->
            micro_seconds;
        FromUnit =:= nanosecond ->
            nano_seconds;
        true ->
            FromUnit
    end,
    ToUnitDeprecated = if
        ToUnit =:= second ->
            seconds;
        ToUnit =:= millisecond ->
            milli_seconds;
        ToUnit =:= microsecond ->
            micro_seconds;
        ToUnit =:= nanosecond ->
            nano_seconds;
        true ->
            ToUnit
    end,
    erlang:convert_time_unit(Time, FromUnitDeprecated, ToUnitDeprecated).
-endif.

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
%% ===Native time units since the UNIX epoch.===
%% (The UNIX epoch is 1970-01-01T00:00:00)
%% @end
%%-------------------------------------------------------------------------

-spec native() -> integer().

native() ->
    erlang:system_time().

%%-------------------------------------------------------------------------
%% @doc
%% ===Native time units since an undefined point in time, from the Erlang VM.===
%% @end
%%-------------------------------------------------------------------------

-spec native_monotonic() -> integer().

native_monotonic() ->
    erlang:monotonic_time().

%%-------------------------------------------------------------------------
%% @doc
%% ===Native time units since an undefined point in time, from the hardware.===
%% @end
%%-------------------------------------------------------------------------

-spec native_os() -> integer().

native_os() ->
    os:system_time().

%%-------------------------------------------------------------------------
%% @doc
%% ===Seconds since the UNIX epoch.===
%% (The UNIX epoch is 1970-01-01T00:00:00)
%% @end
%%-------------------------------------------------------------------------

-spec seconds() -> seconds_epoch().

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
seconds() ->
    erlang:system_time(second).
-else.
seconds() ->
    erlang:system_time(seconds).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Seconds since an undefined point in time, from the Erlang VM.===
%% @end
%%-------------------------------------------------------------------------

-spec seconds_monotonic() -> seconds_monotonic().

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
seconds_monotonic() ->
    erlang:monotonic_time(second).
-else.
seconds_monotonic() ->
    erlang:monotonic_time(seconds).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Seconds since an undefined point in time, from the hardware.===
%% @end
%%-------------------------------------------------------------------------

-spec seconds_os() -> seconds_hardware().

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
seconds_os() ->
    os:system_time(second).
-else.
seconds_os() ->
    os:system_time(seconds).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Milliseconds since the UNIX epoch.===
%% (The UNIX epoch is 1970-01-01T00:00:00)
%% @end
%%-------------------------------------------------------------------------

-spec milliseconds() -> milliseconds_epoch().

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
milliseconds() ->
    erlang:system_time(millisecond).
-else.
milliseconds() ->
    erlang:system_time(milli_seconds).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Milliseconds since an undefined point in time, from the Erlang VM.===
%% @end
%%-------------------------------------------------------------------------

-spec milliseconds_monotonic() -> milliseconds_monotonic().

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
milliseconds_monotonic() ->
    erlang:monotonic_time(millisecond).
-else.
milliseconds_monotonic() ->
    erlang:monotonic_time(milli_seconds).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Milliseconds since an undefined point in time, from the hardware.===
%% @end
%%-------------------------------------------------------------------------

-spec milliseconds_os() -> milliseconds_hardware().

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
milliseconds_os() ->
    os:system_time(millisecond).
-else.
milliseconds_os() ->
    os:system_time(milli_seconds).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Microseconds since the UNIX epoch.===
%% (The UNIX epoch is 1970-01-01T00:00:00)
%% @end
%%-------------------------------------------------------------------------

-spec microseconds() -> microseconds_epoch().

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
microseconds() ->
    erlang:system_time(microsecond).
-else.
microseconds() ->
    erlang:system_time(micro_seconds).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Microseconds since an undefined point in time, from the Erlang VM.===
%% @end
%%-------------------------------------------------------------------------

-spec microseconds_monotonic() -> microseconds_monotonic().

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
microseconds_monotonic() ->
    erlang:monotonic_time(microsecond).
-else.
microseconds_monotonic() ->
    erlang:monotonic_time(micro_seconds).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Microseconds since an undefined point in time, from the hardware.===
%% @end
%%-------------------------------------------------------------------------

-spec microseconds_os() -> microseconds_hardware().

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
microseconds_os() ->
    os:system_time(microsecond).
-else.
microseconds_os() ->
    os:system_time(micro_seconds).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Nanoseconds since the UNIX epoch.===
%% (The UNIX epoch is 1970-01-01T00:00:00)
%% @end
%%-------------------------------------------------------------------------

-spec nanoseconds() -> nanoseconds_epoch().

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
nanoseconds() ->
    erlang:system_time(nanosecond).
-else.
nanoseconds() ->
    erlang:system_time(nano_seconds).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Nanoseconds since an undefined point in time, from the Erlang VM.===
%% @end
%%-------------------------------------------------------------------------

-spec nanoseconds_monotonic() -> nanoseconds_monotonic().

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
nanoseconds_monotonic() ->
    erlang:monotonic_time(nanosecond).
-else.
nanoseconds_monotonic() ->
    erlang:monotonic_time(nano_seconds).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Nanoseconds since an undefined point in time, from the hardware.===
%% @end
%%-------------------------------------------------------------------------

-spec nanoseconds_os() -> nanoseconds_hardware().

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
nanoseconds_os() ->
    os:system_time(nanosecond).
-else.
nanoseconds_os() ->
    os:system_time(nano_seconds).
-endif.

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
    non_neg_integer().

uptime() ->
    uptime(second).

%%-------------------------------------------------------------------------
%% @doc
%% ===The uptime of the Erlang node.===
%% @end
%%-------------------------------------------------------------------------

-spec uptime(TimeUnit :: second | millisecond | microsecond | nanosecond) ->
    non_neg_integer().

uptime(TimeUnit)
    when TimeUnit =:= second; TimeUnit =:= millisecond;
         TimeUnit =:= microsecond; TimeUnit =:= nanosecond ->
    Value = erlang:monotonic_time() - erlang:system_info(start_time),
    convert(Value, native, TimeUnit).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

