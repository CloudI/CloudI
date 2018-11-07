%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Status Data==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2018 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2018 Michael Truog
%%% @version 1.7.5 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_core_i_status).
-author('mjtruog at protonmail dot com').

%% external interface
-export([durations_erase/2,
         durations_new/0,
         durations_state/2,
         durations_store/3,
         durations_sum/2,
         nanoseconds_to_availability_day/1,
         nanoseconds_to_availability_day/3,
         nanoseconds_to_availability_week/1,
         nanoseconds_to_availability_week/3,
         nanoseconds_to_availability_month/1,
         nanoseconds_to_availability_month/3,
         nanoseconds_to_availability_year/1,
         nanoseconds_to_availability_year/3,
         nanoseconds_to_string/2]).

-type nanoseconds() :: non_neg_integer().
-type duration() ::
    {T0 :: cloudi_timestamp:native_monotonic(),
     T1 :: cloudi_timestamp:native_monotonic()}.
-type durations_state() ::
    {DurationCount :: non_neg_integer(),
     DurationList :: list(duration())}.
-type durations(Key) ::
    #{Key := durations_state()}.
-export_type([durations/1]).

-include("cloudi_core_i_constants.hrl").

-spec durations_erase(Key :: any(),
                      DurationsLookup :: durations(any())) ->
    durations(any()).

durations_erase(Key, DurationsLookup) ->
    maps:remove(Key, DurationsLookup).

-spec durations_new() ->
    durations(any()).

durations_new() ->
    #{}.

-spec durations_state(Key :: any(),
                      DurationsLookup :: durations(any())) ->
    durations_state().

durations_state(Key, DurationsLookup) ->
    maps:get(Key, DurationsLookup, {0, []}).

-spec durations_store(KeyList :: list(),
                      Duration :: duration(),
                      DurationsLookup :: durations(any())) ->
    durations(any()).

durations_store(KeyList, {_, T1} = Duration, DurationsLookup) ->
    duration_store(KeyList, T1 - ?NATIVE_TIME_IN_YEAR,
                   Duration, DurationsLookup).

-spec durations_sum(durations_state(),
                    T :: cloudi_timestamp:native_monotonic()) ->
    {boolean(), nanoseconds()}.

durations_sum({DurationCount, DurationList}, T) ->
    durations_sum(DurationList, 0,
                  DurationCount == ?STATUS_DURATIONS_YEAR_MAX, T).

-spec nanoseconds_to_availability_day(NanoSecondsUptime :: nanoseconds()) ->
    string().

nanoseconds_to_availability_day(NanoSecondsUptime) ->
    availability_to_string(NanoSecondsUptime / ?NANOSECONDS_IN_DAY).

-spec nanoseconds_to_availability_day(NanoSecondsUptime :: nanoseconds(),
                                      Approximate :: boolean(),
                                      NanoSecondsDowntime :: nanoseconds()) ->
    string().

nanoseconds_to_availability_day(NanoSecondsUptime,
                                Approximate,
                                NanoSecondsDowntime) ->
    NanoSecondsDay = ?NANOSECONDS_IN_DAY,
    AvailabilityDay = (erlang:min(NanoSecondsUptime, NanoSecondsDay) -
        NanoSecondsDowntime) / NanoSecondsDay,
    availability_to_string(AvailabilityDay, Approximate).

-spec nanoseconds_to_availability_week(NanoSecondsUptime :: nanoseconds()) ->
    string().

nanoseconds_to_availability_week(NanoSecondsUptime) ->
    availability_to_string(NanoSecondsUptime / ?NANOSECONDS_IN_WEEK).

-spec nanoseconds_to_availability_week(NanoSecondsUptime :: nanoseconds(),
                                       Approximate :: boolean(),
                                       NanoSecondsDowntime :: nanoseconds()) ->
    string().

nanoseconds_to_availability_week(NanoSecondsUptime,
                                 Approximate,
                                 NanoSecondsDowntime) ->
    NanoSecondsWeek = ?NANOSECONDS_IN_WEEK,
    AvailabilityWeek = (erlang:min(NanoSecondsUptime, NanoSecondsWeek) -
        NanoSecondsDowntime) / NanoSecondsWeek,
    availability_to_string(AvailabilityWeek, Approximate).

-spec nanoseconds_to_availability_month(NanoSecondsUptime :: nanoseconds()) ->
    string().

nanoseconds_to_availability_month(NanoSecondsUptime) ->
    availability_to_string(NanoSecondsUptime / ?NANOSECONDS_IN_MONTH).

-spec nanoseconds_to_availability_month(NanoSecondsUptime :: nanoseconds(),
                                        Approximate :: boolean(),
                                        NanoSecondsDowntime :: nanoseconds()) ->
    string().

nanoseconds_to_availability_month(NanoSecondsUptime,
                                  Approximate,
                                  NanoSecondsDowntime) ->
    NanoSecondsMonth = ?NANOSECONDS_IN_MONTH,
    AvailabilityMonth = (erlang:min(NanoSecondsUptime, NanoSecondsMonth) -
        NanoSecondsDowntime) / NanoSecondsMonth,
    availability_to_string(AvailabilityMonth, Approximate).

-spec nanoseconds_to_availability_year(NanoSecondsUptime :: nanoseconds()) ->
    string().

nanoseconds_to_availability_year(NanoSecondsUptime) ->
    availability_to_string(NanoSecondsUptime / ?NANOSECONDS_IN_YEAR).

-spec nanoseconds_to_availability_year(NanoSecondsUptime :: nanoseconds(),
                                       Approximate :: boolean(),
                                       NanoSecondsDowntime :: nanoseconds()) ->
    string().

nanoseconds_to_availability_year(NanoSecondsUptime,
                                 Approximate,
                                 NanoSecondsDowntime) ->
    NanoSecondsYear = ?NANOSECONDS_IN_YEAR,
    AvailabilityYear = (erlang:min(NanoSecondsUptime, NanoSecondsYear) -
        NanoSecondsDowntime) / NanoSecondsYear,
    availability_to_string(AvailabilityYear, Approximate).

-spec nanoseconds_to_string(NanoSeconds :: nanoseconds(),
                            Approximate :: boolean()) ->
    string().

nanoseconds_to_string(NanoSeconds, true) ->
    [$>, $  | cloudi_timestamp:nanoseconds_to_string(NanoSeconds)];
nanoseconds_to_string(NanoSeconds, false) ->
    cloudi_timestamp:nanoseconds_to_string(NanoSeconds).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

duration_store([], _, _, DurationsLookup) ->
    DurationsLookup;
duration_store([Key | KeyList], T, Duration, DurationsLookup) ->
    {DurationCount,
     DurationList} = durations_state(Key, DurationsLookup),
    {DurationCountNew,
     DurationListNew} = duration_clear(lists:reverse(DurationList),
                                       DurationCount, T),
    DurationsStateNew = {DurationCountNew + 1, [Duration | DurationListNew]},
    duration_store(KeyList, T, Duration,
                   maps:put(Key, DurationsStateNew, DurationsLookup)).

duration_clear([] = DurationList, 0 = DurationCount, _) ->
    {DurationCount, DurationList};
duration_clear([{_, T1} | DurationList], DurationCount, T)
    when T1 < T ->
    duration_clear(DurationList, DurationCount - 1, T);
duration_clear([_ | DurationList], DurationCount, T)
    when DurationCount >= ?STATUS_DURATIONS_YEAR_MAX ->
    % duration list becomes approximate when it hits max size
    duration_clear(DurationList, DurationCount - 1, T);
duration_clear(DurationList, DurationCount, _) ->
    {DurationCount, lists:reverse(DurationList)}.

durations_sum([], NativeDowntime, Approximate, _) ->
    {Approximate,
     cloudi_timestamp:convert(NativeDowntime, native, nanosecond)};
durations_sum([{T0, T1} | DurationList], NativeDowntime, Approximate, T) ->
    if
        T0 >= T ->
            durations_sum(DurationList, NativeDowntime + (T1 - T0),
                          Approximate, T);
        T1 > T ->
            durations_sum(DurationList, NativeDowntime + (T1 - T),
                          Approximate, T);
        true ->
            % only older entries remain and their presence
            % implies the result is not approximate
            durations_sum([], NativeDowntime, false, T)
    end.

availability_to_string(Availability, true) ->
    case availability_to_string(Availability) of
        ?AVAILABILITY_ZERO = Zero ->
            Zero;
        AvailabilityStr ->
            [$<, $  | AvailabilityStr]
    end;
availability_to_string(Availability, false) ->
    availability_to_string(Availability).

% avoid erlang:float_to_list precision problems
% and keep the formatting efficient
availability_to_string(Availability)
    when Availability < 0.25 ->
    ?AVAILABILITY_ZERO;
availability_to_string(Availability)
    when Availability < 1 / 3 ->
    "25 %";
availability_to_string(Availability)
    when Availability < 0.5 ->
    "33.3 %";
availability_to_string(Availability)
    when Availability < 2 / 3 ->
    "50 %";
availability_to_string(Availability)
    when Availability < 0.75 ->
    "66.6 %";
availability_to_string(Availability)
    when Availability < 0.9 ->
    "75 %";
availability_to_string(Availability)
    when Availability < 0.99 ->
    if % 1 nine
        Availability < 0.91 ->
            "90 %";
        Availability < 0.92 ->
            "91 %";
        Availability < 0.93 ->
            "92 %";
        Availability < 0.94 ->
            "93 %";
        Availability < 0.95 ->
            "94 %";
        Availability < 0.96 ->
            "95 %";
        Availability < 0.97 ->
            "96 %";
        Availability < 0.98 ->
            "97 %";
        true ->
            "98 %"
    end;
availability_to_string(Availability)
    when Availability < 0.999 ->
    if % 2 nines
        Availability < 0.991 ->
            "99.0 %";
        Availability < 0.992 ->
            "99.1 %";
        Availability < 0.993 ->
            "99.2 %";
        Availability < 0.994 ->
            "99.3 %";
        Availability < 0.995 ->
            "99.4 %";
        Availability < 0.996 ->
            "99.5 %";
        Availability < 0.997 ->
            "99.6 %";
        Availability < 0.998 ->
            "99.7 %";
        true ->
            "99.8 %"
    end;
availability_to_string(Availability)
    when Availability < 0.9999 ->
    if % 3 nines
        Availability < 0.9991 ->
            "99.9 %";
        Availability < 0.9992 ->
            "99.91 %";
        Availability < 0.9993 ->
            "99.92 %";
        Availability < 0.9994 ->
            "99.93 %";
        Availability < 0.9995 ->
            "99.94 %";
        Availability < 0.9996 ->
            "99.95 %";
        Availability < 0.9997 ->
            "99.96 %";
        Availability < 0.9998 ->
            "99.97 %";
        true ->
            "99.98 %"
    end;
availability_to_string(Availability)
    when Availability < 0.99999 ->
    if % 4 nines
        Availability < 0.99991 ->
            "99.99 %";
        Availability < 0.99992 ->
            "99.991 %";
        Availability < 0.99993 ->
            "99.992 %";
        Availability < 0.99994 ->
            "99.993 %";
        Availability < 0.99995 ->
            "99.994 %";
        Availability < 0.99996 ->
            "99.995 %";
        Availability < 0.99997 ->
            "99.996 %";
        Availability < 0.99998 ->
            "99.997 %";
        true ->
            "99.998 %"
    end;
availability_to_string(Availability)
    when Availability < 0.999999 ->
    if % 5 nines
        Availability < 0.999991 ->
            "99.999 %";
        Availability < 0.999992 ->
            "99.9991 %";
        Availability < 0.999993 ->
            "99.9992 %";
        Availability < 0.999994 ->
            "99.9993 %";
        Availability < 0.999995 ->
            "99.9994 %";
        Availability < 0.999996 ->
            "99.9995 %";
        Availability < 0.999997 ->
            "99.9996 %";
        Availability < 0.999998 ->
            "99.9997 %";
        true ->
            "99.9998 %"
    end;
availability_to_string(Availability)
    when Availability < 0.9999999 ->
    if % 6 nines
        Availability < 0.9999991 ->
            "99.9999 %";
        Availability < 0.9999992 ->
            "99.99991 %";
        Availability < 0.9999993 ->
            "99.99992 %";
        Availability < 0.9999994 ->
            "99.99993 %";
        Availability < 0.9999995 ->
            "99.99994 %";
        Availability < 0.9999996 ->
            "99.99995 %";
        Availability < 0.9999997 ->
            "99.99996 %";
        Availability < 0.9999998 ->
            "99.99997 %";
        true ->
            "99.99998 %"
    end;
availability_to_string(Availability)
    when Availability < 0.99999999 ->
    if % 7 nines
        Availability < 0.99999991 ->
            "99.99999 %";
        Availability < 0.99999992 ->
            "99.999991 %";
        Availability < 0.99999993 ->
            "99.999992 %";
        Availability < 0.99999994 ->
            "99.999993 %";
        Availability < 0.99999995 ->
            "99.999994 %";
        Availability < 0.99999996 ->
            "99.999995 %";
        Availability < 0.99999997 ->
            "99.999996 %";
        Availability < 0.99999998 ->
            "99.999997 %";
        true ->
            "99.999998 %"
    end;
availability_to_string(Availability)
    when Availability < 0.999999999 ->
    if % 8 nines
        Availability < 0.999999991 ->
            "99.999999 %";
        Availability < 0.999999992 ->
            "99.9999991 %";
        Availability < 0.999999993 ->
            "99.9999992 %";
        Availability < 0.999999994 ->
            "99.9999993 %";
        Availability < 0.999999995 ->
            "99.9999994 %";
        Availability < 0.999999996 ->
            "99.9999995 %";
        Availability < 0.999999997 ->
            "99.9999996 %";
        Availability < 0.999999998 ->
            "99.9999997 %";
        true ->
            "99.9999998 %"
    end;
availability_to_string(Availability) ->
    if % 9 nines
        Availability < 1.0 ->
            "99.9999999 %";
        true ->
            "100 %"
    end.

