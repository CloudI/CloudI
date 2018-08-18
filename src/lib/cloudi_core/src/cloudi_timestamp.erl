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
%%% Copyright (c) 2015-2018 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2015-2018 Michael Truog
%%% @version 1.7.4 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_timestamp).
-author('mjtruog at protonmail dot com').

%% external interface
-export([convert/3,
         timestamp/0,
         native/0,
         native_monotonic/0,
         native_os/0,
         seconds/0,
         seconds_epoch_to_string/1,
         seconds_monotonic/0,
         seconds_os/0,
         seconds_to_string/1,
         seconds_to_string/2,
         milliseconds/0,
         milliseconds_epoch_to_string/1,
         milliseconds_monotonic/0,
         milliseconds_os/0,
         microseconds/0,
         microseconds_epoch_to_string/1,
         microseconds_monotonic/0,
         microseconds_os/0,
         nanoseconds/0,
         nanoseconds_monotonic/0,
         nanoseconds_os/0,
         nanoseconds_to_string/1,
         nanoseconds_to_string/2,
         seconds_filter/3,
         seconds_filter_monotonic/3,
         uptime/0,
         uptime/1,
         uptime_days/0,
         uptime_months/0,
         uptime_years/0]).

-type time_unit() :: second | millisecond | microsecond | nanosecond |
                     native | perf_counter | pos_integer().
% UNIX epoch (1970-01-01T00:00:00Z) offsets (POSIX time)
-type seconds_epoch() :: non_neg_integer().
-type milliseconds_epoch() :: non_neg_integer().
-type microseconds_epoch() :: non_neg_integer().
-type nanoseconds_epoch() :: non_neg_integer().
% monotonic time values may be negative
-type seconds_monotonic() :: integer().
-type milliseconds_monotonic() :: integer().
-type microseconds_monotonic() :: integer().
-type nanoseconds_monotonic() :: integer().
-type native_monotonic() :: integer().
-export_type([time_unit/0,
              seconds_epoch/0,
              milliseconds_epoch/0,
              microseconds_epoch/0,
              nanoseconds_epoch/0,
              seconds_monotonic/0,
              milliseconds_monotonic/0,
              microseconds_monotonic/0,
              nanoseconds_monotonic/0,
              native_monotonic/0]).

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
%% (The UNIX epoch is 1970-01-01T00:00:00Z)
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

-spec native_monotonic() -> native_monotonic().

native_monotonic() ->
    erlang:monotonic_time().

%%-------------------------------------------------------------------------
%% @doc
%% ===Native time units since the UNIX epoch, from the hardware.===
%% Always prefer the mative function instead of this function.
%% (The UNIX epoch is 1970-01-01T00:00:00Z)
%% @end
%%-------------------------------------------------------------------------

-spec native_os() -> integer().

native_os() ->
    os:system_time().

%%-------------------------------------------------------------------------
%% @doc
%% ===Seconds since the UNIX epoch.===
%% (The UNIX epoch is 1970-01-01T00:00:00Z)
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
%% ===Create an ISO8601 timestamp from seconds since the UNIX epoch.===
%% (The UNIX epoch is 1970-01-01T00:00:00Z)
%% @end
%%-------------------------------------------------------------------------

-spec seconds_epoch_to_string(Seconds :: seconds_epoch()) ->
    string().

seconds_epoch_to_string(Seconds) ->
    cloudi_core_i_logger:seconds_to_string(Seconds).

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
%% ===Seconds since the UNIX epoch, from the hardware.===
%% Always prefer the seconds function instead of this function.
%% (The UNIX epoch is 1970-01-01T00:00:00Z)
%% @end
%%-------------------------------------------------------------------------

-spec seconds_os() -> seconds_epoch().

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
seconds_os() ->
    os:system_time(second).
-else.
seconds_os() ->
    os:system_time(seconds).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Format a seconds duration as a minimal string with lower-precision integers.===
%% @end
%%-------------------------------------------------------------------------

-spec seconds_to_string(TotalSeconds :: non_neg_integer()) ->
    nonempty_string().

seconds_to_string(TotalSeconds)
    when is_integer(TotalSeconds), TotalSeconds >= 0 ->
    Seconds = TotalSeconds rem ?SECONDS_IN_HOUR,
    TotalHours = TotalSeconds div ?SECONDS_IN_HOUR,
    Hours = TotalHours rem ?HOURS_IN_DAY,
    TotalDays = TotalHours div ?HOURS_IN_DAY,
    if
        TotalDays > 0 ->
            lists:flatten([time_value_to_list(TotalDays, "day"), $ ,
                           time_value_to_list(Hours, "hour"), $ ,
                           time_value_to_list(Seconds, "second")]);
        Hours > 0 ->
            lists:flatten([time_value_to_list(Hours, "hour"), $ ,
                           time_value_to_list(Seconds, "second")]);
        true ->
            lists:flatten(time_value_to_list(Seconds, "second"))
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Format a seconds duration with options as a minimal string with lower-precision integers.===
%% @end
%%-------------------------------------------------------------------------

-spec seconds_to_string(TotalSeconds :: integer(),
                        Options :: signed) ->
    nonempty_string().

seconds_to_string(TotalSeconds, signed)
    when is_integer(TotalSeconds) ->
    {Sign, TotalSecondsPositive} = if
        TotalSeconds >= 0 ->
            {"+ ", TotalSeconds};
        true ->
            {"- ", -1 * TotalSeconds}
    end,
    Seconds = TotalSecondsPositive rem ?SECONDS_IN_HOUR,
    TotalHours = TotalSecondsPositive div ?SECONDS_IN_HOUR,
    Hours = TotalHours rem ?HOURS_IN_DAY,
    TotalDays = TotalHours div ?HOURS_IN_DAY,
    if
        TotalDays > 0 ->
            lists:flatten([Sign,
                           time_value_to_list(TotalDays, "day"), $ ,
                           time_value_to_list(Hours, "hour"), $ ,
                           time_value_to_list(Seconds, "second")]);
        Hours > 0 ->
            lists:flatten([Sign,
                           time_value_to_list(Hours, "hour"), $ ,
                           time_value_to_list(Seconds, "second")]);
        true ->
            lists:flatten([Sign,
                           time_value_to_list(Seconds, "second")])
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Milliseconds since the UNIX epoch.===
%% (The UNIX epoch is 1970-01-01T00:00:00Z)
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
%% ===Create an ISO8601 timestamp from milliseconds since the UNIX epoch.===
%% (The UNIX epoch is 1970-01-01T00:00:00Z)
%% @end
%%-------------------------------------------------------------------------

-spec milliseconds_epoch_to_string(MilliSeconds :: milliseconds_epoch()) ->
    string().

milliseconds_epoch_to_string(MilliSeconds) ->
    cloudi_core_i_logger:milliseconds_to_string(MilliSeconds).

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
%% ===Milliseconds since the UNIX epoch, from the hardware.===
%% Always prefer the milliseconds function instead of this function.
%% (The UNIX epoch is 1970-01-01T00:00:00Z)
%% @end
%%-------------------------------------------------------------------------

-spec milliseconds_os() -> milliseconds_epoch().

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
%% (The UNIX epoch is 1970-01-01T00:00:00Z)
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
%% ===Create an ISO8601 timestamp from microseconds since the UNIX epoch.===
%% (The UNIX epoch is 1970-01-01T00:00:00Z)
%% @end
%%-------------------------------------------------------------------------

-spec microseconds_epoch_to_string(MicroSeconds :: microseconds_epoch()) ->
    string().

microseconds_epoch_to_string(MicroSeconds) ->
    cloudi_core_i_logger:microseconds_to_string(MicroSeconds).

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
%% ===Microseconds since the UNIX epoch, from the hardware.===
%% Always prefer the microseconds function instead of this function.
%% (The UNIX epoch is 1970-01-01T00:00:00Z)
%% @end
%%-------------------------------------------------------------------------

-spec microseconds_os() -> microseconds_epoch().

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
%% (The UNIX epoch is 1970-01-01T00:00:00Z)
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
%% ===Nanoseconds since the UNIX epoch, from the hardware.===
%% Always prefer the nanoseconds function instead of this function.
%% (The UNIX epoch is 1970-01-01T00:00:00Z)
%% @end
%%-------------------------------------------------------------------------

-spec nanoseconds_os() -> nanoseconds_epoch().

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
nanoseconds_os() ->
    os:system_time(nanosecond).
-else.
nanoseconds_os() ->
    os:system_time(nano_seconds).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Format a nanoseconds duration as a minimal string with lower-precision integers.===
%% @end
%%-------------------------------------------------------------------------

-spec nanoseconds_to_string(TotalNanoSeconds :: non_neg_integer()) ->
    nonempty_string().

nanoseconds_to_string(TotalNanoSeconds)
    when is_integer(TotalNanoSeconds), TotalNanoSeconds >= 0 ->
    NanoSeconds = TotalNanoSeconds rem ?NANOSECONDS_IN_SECOND,
    TotalSeconds = TotalNanoSeconds div ?NANOSECONDS_IN_SECOND,
    Seconds = TotalSeconds rem ?SECONDS_IN_HOUR,
    TotalHours = TotalSeconds div ?SECONDS_IN_HOUR,
    Hours = TotalHours rem ?HOURS_IN_DAY,
    TotalDays = TotalHours div ?HOURS_IN_DAY,
    if
        TotalDays > 0 ->
            lists:flatten([time_value_to_list(TotalDays, "day"), $ ,
                           time_value_to_list(Hours, "hour"), $ ,
                           time_value_to_list(Seconds, "second"), $ ,
                           time_value_to_list(NanoSeconds, "nanosecond")]);
        Hours > 0 ->
            lists:flatten([time_value_to_list(Hours, "hour"), $ ,
                           time_value_to_list(Seconds, "second"), $ ,
                           time_value_to_list(NanoSeconds, "nanosecond")]);
        Seconds > 0 ->
            lists:flatten([time_value_to_list(Seconds, "second"), $ ,
                           time_value_to_list(NanoSeconds, "nanosecond")]);
        true ->
            lists:flatten(time_value_to_list(NanoSeconds, "nanosecond"))
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Format a nanoseconds duration with options as a minimal string with lower-precision integers.===
%% @end
%%-------------------------------------------------------------------------

-spec nanoseconds_to_string(TotalNanoSeconds :: integer(),
                            Options :: signed) ->
    nonempty_string().

nanoseconds_to_string(TotalNanoSeconds, signed)
    when is_integer(TotalNanoSeconds) ->
    {Sign, TotalNanoSecondsPositive} = if
        TotalNanoSeconds >= 0 ->
            {"+ ", TotalNanoSeconds};
        true ->
            {"- ", -1 * TotalNanoSeconds}
    end,
    NanoSeconds = TotalNanoSecondsPositive rem ?NANOSECONDS_IN_SECOND,
    TotalSeconds = TotalNanoSecondsPositive div ?NANOSECONDS_IN_SECOND,
    Seconds = TotalSeconds rem ?SECONDS_IN_HOUR,
    TotalHours = TotalSeconds div ?SECONDS_IN_HOUR,
    Hours = TotalHours rem ?HOURS_IN_DAY,
    TotalDays = TotalHours div ?HOURS_IN_DAY,
    if
        TotalDays > 0 ->
            lists:flatten([Sign,
                           time_value_to_list(TotalDays, "day"), $ ,
                           time_value_to_list(Hours, "hour"), $ ,
                           time_value_to_list(Seconds, "second"), $ ,
                           time_value_to_list(NanoSeconds, "nanosecond")]);
        Hours > 0 ->
            lists:flatten([Sign,
                           time_value_to_list(Hours, "hour"), $ ,
                           time_value_to_list(Seconds, "second"), $ ,
                           time_value_to_list(NanoSeconds, "nanosecond")]);
        Seconds > 0 ->
            lists:flatten([Sign,
                           time_value_to_list(Seconds, "second"), $ ,
                           time_value_to_list(NanoSeconds, "nanosecond")]);
        true ->
            lists:flatten([Sign,
                           time_value_to_list(NanoSeconds, "nanosecond")])
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Filter a list of seconds since the UNIX epoch.===
%% The list is not ordered.
%% @end
%%-------------------------------------------------------------------------

-spec seconds_filter(L :: list(seconds_epoch()),
                     SecondsNow :: seconds_epoch(),
                     MaxPeriod :: pos_integer()) ->
    {Count :: non_neg_integer(),
     NewL :: list(seconds_epoch())}.

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
%% ===Filter a list of seconds since an undefined point in time, from the Erlang VM.===
%% The list is not ordered.
%% @end
%%-------------------------------------------------------------------------

-spec seconds_filter_monotonic(L :: list(seconds_monotonic()),
                               SecondsNow :: seconds_monotonic(),
                               MaxPeriod :: pos_integer()) ->
    {Count :: non_neg_integer(),
     NewL :: list(seconds_monotonic())}.

seconds_filter_monotonic(L, SecondsNow, MaxPeriod) ->
    seconds_filter_monotonic(L, [], 0, SecondsNow, MaxPeriod).

seconds_filter_monotonic([], Output, Count, _, _) ->
    {Count, Output};
seconds_filter_monotonic([Seconds | L], Output, Count, SecondsNow, MaxPeriod) ->
    if
        (SecondsNow - Seconds) > MaxPeriod ->
            seconds_filter_monotonic(L, Output, Count,
                                     SecondsNow, MaxPeriod);
        true ->
            seconds_filter_monotonic(L, [Seconds | Output], Count + 1,
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
    Value = native_monotonic() - erlang:system_info(start_time),
    convert(Value, native, TimeUnit).

%%-------------------------------------------------------------------------
%% @doc
%% ===The uptime of the Erlang node in days.===
%% @end
%%-------------------------------------------------------------------------

-spec uptime_days() ->
    float().

uptime_days() ->
    uptime(second) / (?SECONDS_IN_HOUR * ?HOURS_IN_DAY).

%%-------------------------------------------------------------------------
%% @doc
%% ===The uptime of the Erlang node in months.===
%% @end
%%-------------------------------------------------------------------------

-spec uptime_months() ->
    float().

uptime_months() ->
    uptime_days() / ?DAYS_IN_MONTH.

%%-------------------------------------------------------------------------
%% @doc
%% ===The uptime of the Erlang node in years.===
%% @end
%%-------------------------------------------------------------------------

-spec uptime_years() ->
    float().

uptime_years() ->
    uptime_days() / ?DAYS_IN_YEAR.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

time_value_to_list(1, Label) ->
    ["1 ", Label];
time_value_to_list(I, Label) ->
    [erlang:integer_to_list(I), $ , Label, $s].

