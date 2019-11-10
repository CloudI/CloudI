%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Cron Expression Functionality==
%%% The special character ? and the @reboot macro are not supported
%%% because they wouldn't work well with Erlang process restarts.
%%% The other Cron expression syntax is implemented,
%%% including the optional seconds and year values
%%% (info from https://en.wikipedia.org/wiki/Cron#CRON_expression):
%%% ```
%%% Field name     Required     Allowed values    Allowed special characters
%%% ----------     --------     --------------    --------------------------
%%% Seconds        No           0-59              * / , -
%%% Minutes        Yes          0-59              * / , -
%%% Hours          Yes          0-23              * / , -
%%% Day of month   Yes          1-31              * / , - L W
%%% Month          Yes          1-12 or JAN-DEC   * / , -
%%% Day of week    Yes          0-6 or SUN-SAT    * / , - L #
%%% Year           No           1970–9999         * / , -
%%% '''
%%%
%%% === Asterisk (*) ===
%%% An asterisk indicates that the cron expression matches for all values
%%% of the field.
%%%
%%% === Slash (/) ===
%%% Slashes can be combined with ranges to specify step values.
%%% For example, */5 in the minutes field indicates every 5 minutes.
%%%
%%% === Comma (,) ===
%%% Commas are used to separate items of a list. For example,
%%% using "MON,WED,FRI" in the day-of-week field means
%%% Mondays, Wednesdays and Fridays.
%%%
%%% === Hyphen (-) ===
%%% Hyphens define ranges. For example, 2000–2010 indicates every year
%%% between 2000 and 2010, inclusive.
%%%
%%% === L ===
%%% 'L' stands for "last". When used in the day-of-week field, it allows you
%%% to specify constructs such as "the last Friday" ("5L") of a given month.
%%% In the day-of-month field, it specifies the last day of the month.
%%%
%%% === W ===
%%% The 'W' character is allowed for the day-of-month field.
%%% This character is used to specify the weekday (Monday-Friday) nearest
%%% the given day.  As an example, if you were to specify "15W" as the value
%%% for the day-of-month field, the meaning is:
%%% "the nearest weekday to the 15th of the month".
%%% So, if the 15th is a Saturday, the trigger fires on Friday the 14th.
%%% If the 15th is a Sunday, the trigger fires on Monday the 16th.
%%% If the 15th is a Tuesday, then it fires on Tuesday the 15th.
%%% However, if you specify "1W" as the value for day-of-month, and
%%% the 1st is a Saturday, the trigger fires on Monday the 3rd,
%%% as it does not 'jump' over the boundary of a month's days.
%%%
%%% This implementation allows the W character to be used in a list:
%%% 1W,15W
%%%
%%% The W character can be combined with L as LW to mean
%%% "the last business day of the month".
%%%
%%% === Hash (#) ===
%%% '#' is allowed for the day-of-week field, and must be followed by
%%% a number between one and five.  It allows you to specify constructs such as
%%% "the second Friday" of a given month.  For example, entering "5#3" in the
%%% day-of-week field corresponds to the third Friday of every month.
%%%
%%% === Other Details ===
%%% ```
%%% * If only six fields are present,
%%%   a 0 second field is prepended
%%% * If only five fields are present,
%%%   a 0 second field is prepended and a wildcard year field is appended
%%% * The range for the day-of-week field is 0-7 instead of 0-6,
%%%   with 7 as Sunday (like 0) (BSD and ATT disagreed about this in the past)
%%% * The month names are case-insensitive
%%% * The day-of-week names are case-insensitive
%%% '''
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2019 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2019 Michael Truog
%%% @version 1.8.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_cron).
-author('mjtruog at protonmail dot com').

%% external interface
-export([expression/1,
         new/1,
         next_datetime/2]).

-type field_seconds() ::
    nonempty_list(0..59) |
    undefined.
-type field_minutes() ::
    nonempty_list(0..59) |
    undefined.
-type field_hours() ::
    nonempty_list(0..23) |
    undefined.
-type field_day_of_month() ::
    nonempty_list(1..31 |
                  last_day_of_month |
                  {weekday, last_day_of_month | 1..31}) |
    undefined.
-type field_month() ::
    nonempty_list(1..12) |
    undefined.
-type field_day_of_week() ::
    nonempty_list(0..6 |
                  {last_day_of_week, 0..6} |
                  {every, 1..5, 0..6}) |
    undefined.
-type field_year() ::
    nonempty_list(1970..9999) |
    undefined.
-type expression_strings() ::
    {nonempty_string(),
     nonempty_string(),
     nonempty_string(),
     nonempty_string(),
     nonempty_string(),
     nonempty_string(),
     nonempty_string()}.
-type expression() ::
    {field_seconds(),
     field_minutes(),
     field_hours(),
     field_day_of_month(),
     field_month(),
     field_day_of_week(),
     field_year()}.

-record(cloudi_cron,
    {
        expression_strings :: expression_strings(),
        expression :: expression()
    }).

-type state() :: #cloudi_cron{}.
-export_type([state/0]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Provide the cron expression string.===
%% @end
%%-------------------------------------------------------------------------

-spec expression(state()) ->
    nonempty_string().

expression(#cloudi_cron{expression_strings = ExpressionStrings}) ->
    cloudi_string:join(" ", erlang:tuple_to_list(ExpressionStrings)).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a parsed representation of a cron expression.===
%% @end
%%-------------------------------------------------------------------------

-spec new(Input :: nonempty_string()) ->
    state().

new([_ | _] = Input) ->
    SecondsDefault = "0",
    YearDefault = "*",
    [Seconds, Minutes, Hours, DayOfMonth, Month, DayOfWeek,
     Year] = case parse_expression_fields(Input) of
        [_, _, _, _, _] = Standard ->
            [SecondsDefault | Standard] ++ [YearDefault];
        [_, _, _, _, _, _] = ExpressionWithYear ->
            [SecondsDefault | ExpressionWithYear];
        [_, _, _, _, _, _, _] = Fields ->
            Fields;
        [Macro] ->
            if
                Macro == "@yearly";
                Macro == "@annually" ->
                    [SecondsDefault, "0", "0", "1", "1", "*", YearDefault];
                Macro == "@monthly" ->
                    [SecondsDefault, "0", "0", "1", "*", "*", YearDefault];
                Macro == "@weekly" ->
                    [SecondsDefault, "0", "0", "*", "*", "0", YearDefault];
                Macro == "@daily";
                Macro == "@midnight" ->
                    [SecondsDefault, "0", "0", "*", "*", "*", YearDefault];
                Macro == "@hourly" ->
                    [SecondsDefault, "0", "*", "*", "*", "*", YearDefault]
            end
    end,
    ExpressionStrings = {Seconds, Minutes, Hours,
                         DayOfMonth, Month, DayOfWeek, Year},
    Expression = {parse_seconds(Seconds),
                  parse_minutes(Minutes),
                  parse_hours(Hours),
                  parse_day_of_month(DayOfMonth),
                  parse_month(Month),
                  parse_day_of_week(DayOfWeek),
                  parse_year(Year)},
    #cloudi_cron{expression_strings = ExpressionStrings,
                 expression = Expression}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Determine the next datetime based on the cron expression.===
%% @end
%%-------------------------------------------------------------------------

-spec next_datetime(DateTime :: calendar:datetime(),
                    state()) ->
    calendar:datetime().

next_datetime({Date0, {_, _, _}} = DateTime0,
              #cloudi_cron{expression = Expression}) ->
    Equal0 = false,
    {Equal1,
     DateTime1} = next_datetime_seconds(Expression, Equal0, DateTime0),
    {Equal2,
     DateTime2} = next_datetime_minutes(Expression, Equal1, DateTime1),
    {Equal3,
     DateTime3} = next_datetime_hours(Expression, Equal2, DateTime2),
    {Equal4,
     DateTime4} = next_datetime_day_of_month(Expression, Equal3, DateTime3),
    {Equal5,
     DateTime5} = next_datetime_month(Expression, Equal4, DateTime4),
    {Equal6,
     DateTime6} = next_datetime_day_of_week(Expression, Equal5, DateTime5),
    DateTimeN = case next_datetime_year(Expression, Equal6, DateTime6) of
        {Date0, _} = DateTime7 ->
            DateTime7;
        {Date1, _} ->
            DateTime8 = {Date1, {0, 0, 0}},
            {true,
             DateTime9} = next_datetime_seconds(Expression, true, DateTime8),
            {true,
             DateTime10} = next_datetime_minutes(Expression, true, DateTime9),
            {true,
             DateTime11} = next_datetime_hours(Expression, true, DateTime10),
            DateTime11
    end,
    DateTimeN.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

parse_values("*", _, _, _, _) ->
    undefined;
parse_values(Input, Min, Max, Names, Allow) ->
    Values = [parse_value(Segment, Min, Max, Names, Allow)
              || Segment <- cloudi_string:split(",", Input)],
    lists:usort(lists:flatten(Values)).

parse_value(Segment, Min, Max, Names, Allow) ->
    case cloudi_string:splitr($/, Segment, input) of
        {"", Segment} ->
            case cloudi_string:splitl($-, Segment, input) of
                {Segment, ""} ->
                    parse_value_name(Allow, Segment, Min, Max, Names);
                {RangeMinStr, RangeMaxStr} ->
                    parse_value_range(RangeMinStr, RangeMaxStr, "1",
                                      Min, Max, Names)
            end;
        {"*", StepStr} ->
            Step = erlang:list_to_integer(StepStr),
            true = (Step > Min) andalso (Step < Max),
            lists:seq(Min, Max, Step);
        {RangeStr, StepStr} ->
            {RangeMinStr,
             RangeMaxStr} = cloudi_string:splitl($-, RangeStr, empty),
            parse_value_range(RangeMinStr, RangeMaxStr, StepStr,
                              Min, Max, Names)
    end.

parse_value_name_integer(Value, Min, Max, undefined) ->
    Integer = erlang:list_to_integer(Value),
    true = (Min =< Integer) andalso (Integer =< Max),
    Integer;
parse_value_name_integer(Value, Min, Max, Names) ->
    case cloudi_lists:index(cloudi_string:lowercase(Value), Names) of
        undefined ->
            Integer = erlang:list_to_integer(Value),
            true = (Min =< Integer) andalso (Integer =< Max),
            if
                Max =:= 7, Integer =:= 7 ->
                    % day_of_week == 7 becomes 0 for Sunday
                    0;
                true ->
                    Integer
            end;
        ValuePlus1 ->
            Integer = ValuePlus1 - 1 + Min,
            true = (Integer =< Max),
            Integer
    end.

parse_value_name([], Value, Min, Max, Names) ->
    parse_value_name_integer(Value, Min, Max, Names);
parse_value_name([every | Allow], Value, Min, Max, Names) ->
    case cloudi_string:splitl($#, Value, input) of
        {Value, ""} ->
            parse_value_name(Allow, Value, Min, Max, Names);
        {ValueAt, MultiplierStr} ->
            Multiplier = erlang:list_to_integer(MultiplierStr),
            true = (1 =< Multiplier) andalso (Multiplier =< 5),
            {every,
             Multiplier,
             parse_value_name_integer(ValueAt, Min, Max, Names)}
    end;
parse_value_name([last_day_of_month | Allow], Value, Min, Max, Names) ->
    if
        Value == "L" ->
            last_day_of_month;
        Value == "LW";
        Value == "WL" ->
            {weekday, last_day_of_month};
        true ->
            parse_value_name(Allow, Value, Min, Max, Names)
    end;
parse_value_name([SuffixName | Allow], Value, Min, Max, Names)
    when SuffixName =:= last_day_of_week; SuffixName =:= weekday ->
    SuffixChar = if
        SuffixName =:= last_day_of_week ->
            $L;
        SuffixName =:= weekday ->
            $W
    end,
    case lists:reverse(Value) of
        [SuffixChar | ValueReversed] ->
            PrefixStr = lists:reverse(ValueReversed),
            {SuffixName, parse_value_name_integer(PrefixStr, Min, Max, Names)};
        _ ->
            parse_value_name(Allow, Value, Min, Max, Names)
    end.

parse_value_range(RangeMinStr, RangeMaxStr, StepStr, Min, Max, Names) ->
    [_ | _] = RangeMinStr,
    [_ | _] = RangeMaxStr,
    RangeMin = parse_value_name_integer(RangeMinStr, Min, Max, Names),
    RangeMax = parse_value_name_integer(RangeMaxStr, Min, Max, Names),
    Step = erlang:list_to_integer(StepStr),
    true = is_integer(RangeMin),
    true = is_integer(RangeMax),
    true = (RangeMin < RangeMax),
    true = (Step >= 1) andalso (Step < Max),
    lists:seq(RangeMin, RangeMax, Step).

parse_seconds([_ | _] = Input) ->
    Min = 0,
    Max = 59,
    Names = undefined,
    Allow = [],
    parse_values(Input, Min, Max, Names, Allow).

parse_minutes([_ | _] = Input) ->
    Min = 0,
    Max = 59,
    Names = undefined,
    Allow = [],
    parse_values(Input, Min, Max, Names, Allow).

parse_hours([_ | _] = Input) ->
    Min = 0,
    Max = 23,
    Names = undefined,
    Allow = [],
    parse_values(Input, Min, Max, Names, Allow).

parse_day_of_month([_ | _] = Input) ->
    Min = 1,
    Max = 31,
    Names = undefined,
    Allow = [last_day_of_month, weekday],
    parse_values(Input, Min, Max, Names, Allow).

parse_month([_ | _] = Input) ->
    Min = 1,
    Max = 12,
    Names = ["jan","feb","mar","apr","may","jun",
             "jul","aug","sep","oct","nov","dec"],
    Allow = [],
    parse_values(Input, Min, Max, Names, Allow).

parse_day_of_week([_ | _] = Input) ->
    Min = 0,
    Max = 7,
    Names = ["sun","mon","tue","wed","thu","fri","sat"],
    Allow = [last_day_of_week, every],
    parse_values(Input, Min, Max, Names, Allow).

parse_year([_ | _] = Input) ->
    Min = 1970,
    Max = 9999,
    Names = undefined,
    Allow = [],
    parse_values(Input, Min, Max, Names, Allow).

parse_expression_fields(L) ->
    parse_expression_fields(parse_expression_spaces(L), [], []).

parse_expression_fields([], [], Fields) ->
    lists:reverse(Fields);
parse_expression_fields([], Field, Fields) ->
    lists:reverse([lists:reverse(Field) | Fields]);
parse_expression_fields([$  | T], Field, Fields) ->
    parse_expression_fields(parse_expression_spaces(T), [],
                            [lists:reverse(Field) | Fields]);
parse_expression_fields([H | T], Field, Fields) ->
    parse_expression_fields(T, [H | Field], Fields).

parse_expression_spaces([$  | T]) ->
    parse_expression_spaces(T);
parse_expression_spaces(L) ->
    L.

next_datetime_seconds({undefined, _, _, _, _, _, _}, Equal, DateTime) ->
    {Equal, DateTime};
next_datetime_seconds({FieldSeconds, _, _, _, _, _, _}, Equal,
                      {Date, {TimeHH, TimeMM, TimeSS}} = DateTime) ->
    DateTimeNew = case get_next_value(FieldSeconds, TimeSS, Equal, Date) of
        undefined ->
            [TimeSSNew | _] = FieldSeconds,
            SecondsIncr = 60 - TimeSS,
            {{DateYYNew, DateMMNew, DateDDNew},
             {TimeHHNew, TimeMMNew, _}} = datetime_add(DateTime, SecondsIncr),
            {{DateYYNew, DateMMNew, DateDDNew},
             {TimeHHNew, TimeMMNew, TimeSSNew}};
        TimeSSNew ->
            {Date,
             {TimeHH, TimeMM, TimeSSNew}}
    end,
    {true, DateTimeNew}.

next_datetime_minutes({_, undefined, _, _, _, _, _}, Equal, DateTime) ->
    {Equal, DateTime};
next_datetime_minutes({_, FieldMinutes, _, _, _, _, _}, Equal,
                      {Date, {TimeHH, TimeMM, TimeSS}} = DateTime) ->
    DateTimeNew = case get_next_value(FieldMinutes, TimeMM, Equal, Date) of
        undefined ->
            [TimeMMNew | _] = FieldMinutes,
            SecondsIncr = (60 - TimeMM) * 60,
            {{DateYYNew, DateMMNew, DateDDNew},
             {TimeHHNew, _, TimeSSNew}} = datetime_add(DateTime, SecondsIncr),
            {{DateYYNew, DateMMNew, DateDDNew},
             {TimeHHNew, TimeMMNew, TimeSSNew}};
        TimeMMNew ->
            {Date,
             {TimeHH, TimeMMNew, TimeSS}}
    end,
    {true, DateTimeNew}.

next_datetime_hours({_, _, undefined, _, _, _, _}, Equal, DateTime) ->
    {Equal, DateTime};
next_datetime_hours({_, _, FieldHours, _, _, _, _}, Equal,
                    {Date, {TimeHH, TimeMM, TimeSS}}) ->
    DateTimeNew = case get_next_value(FieldHours, TimeHH, Equal, Date) of
        undefined ->
            [TimeHHNew | _] = FieldHours,
            DateNew = date_add(Date, 1),
            {DateNew,
             {TimeHHNew, TimeMM, TimeSS}};
        TimeHHNew ->
            {Date,
             {TimeHHNew, TimeMM, TimeSS}}
    end,
    {true, DateTimeNew}.

next_datetime_day_of_month({_, _, _, undefined, _, _, _}, Equal, DateTime) ->
    {Equal, DateTime};
next_datetime_day_of_month({_, _, _,
                            FieldDayOfMonth, _, _, _} = Expression, Equal,
                           {{_, _, DateDD} = Date, Time} = DateTime) ->
    case get_next_value(FieldDayOfMonth, DateDD, Equal, Date) of
        DateDD ->
            {Equal, DateTime};
        _ ->
            next_datetime_day_of_month(Expression, true,
                                       {date_add(Date, 1), Time})
    end.

next_datetime_month({_, _, _, _, undefined, _, _}, Equal, DateTime) ->
    {Equal, DateTime};
next_datetime_month({_, _, _, _,
                     FieldMonth, _, _} = Expression, Equal,
                    {{_, DateMM, _} = Date, Time} = DateTime) ->
    case get_next_value(FieldMonth, DateMM, Equal, Date) of
        DateMM ->
            {Equal, DateTime};
        _ ->
            next_datetime_month(Expression, true,
                                {date_add(Date, 1), Time})
    end.

next_datetime_day_of_week({_, _, _, _, _, undefined, _}, Equal, DateTime) ->
    {Equal, DateTime};
next_datetime_day_of_week({_, _, _, _, _,
                           FieldDayOfWeek, _} = Expression, Equal,
                          {Date, Time} = DateTime) ->
    DayOfWeek = day_of_week(Date),
    case get_next_value(FieldDayOfWeek, DayOfWeek, Equal, Date) of
        DayOfWeek ->
            {Equal, DateTime};
        _ ->
            next_datetime_day_of_week(Expression, true,
                                      {date_add(Date, 1), Time})
    end.

next_datetime_year({_, _, _, _, _, _, undefined}, _, DateTime) ->
    DateTime;
next_datetime_year({_, _, _, _, _, _,
                    FieldYear} = Expression, Equal,
                   {{DateYY, _, _} = Date, Time} = DateTime) ->
    case get_next_value(FieldYear, DateYY, Equal, Date) of
        DateYY ->
            DateTime;
        _ ->
            next_datetime_year(Expression, true,
                               {date_add(Date, 1), Time})
    end.

get_next_value(L, I, Equal, Date) ->
    ExpandedL = lists:usort(get_next_value_expand(L, Date)),
    if
        Equal =:= true ->
            get_next_value_compare_greater_or_equal(ExpandedL, I);
        Equal =:= false ->
            get_next_value_compare_greater(ExpandedL, I)
    end.

get_next_value_expand([] = L, _) ->
    L;
get_next_value_expand([last_day_of_month | T], {DateYY, DateMM, _} = Date) ->
    Last = calendar:last_day_of_the_month(DateYY, DateMM),
    [Last | get_next_value_expand(T, Date)];
get_next_value_expand([{weekday, WeekDay} | T], {DateYY, DateMM, _} = Date) ->
    DateDD = if
        WeekDay =:= last_day_of_month ->
            calendar:last_day_of_the_month(DateYY, DateMM);
        is_integer(WeekDay) ->
            WeekDay
    end,
    [closest_weekday({DateYY, DateMM, DateDD}) |
     get_next_value_expand(T, Date)];
get_next_value_expand([{last_day_of_week, DayOfWeek} | T], Date) ->
    case last_day_of_week(Date, DayOfWeek) of
        true ->
            [DayOfWeek | get_next_value_expand(T, Date)];
        false ->
            get_next_value_expand(T, Date)
    end;
get_next_value_expand([{every, Multiplier, DayOfWeek} | T], Date) ->
    case every_day_of_week(Date, Multiplier, DayOfWeek) of
        true ->
            [DayOfWeek | get_next_value_expand(T, Date)];
        false ->
            get_next_value_expand(T, Date)
    end;
get_next_value_expand([H | T], Date)
    when is_integer(H) ->
    [H | get_next_value_expand(T, Date)].

get_next_value_compare_greater([], _) ->
    undefined;
get_next_value_compare_greater([H | _], I)
    when H > I ->
    H;
get_next_value_compare_greater([_ | T], I) ->
    get_next_value_compare_greater(T, I).

get_next_value_compare_greater_or_equal([], _) ->
    undefined;
get_next_value_compare_greater_or_equal([H | _], I)
    when H >= I ->
    H;
get_next_value_compare_greater_or_equal([_ | T], I) ->
    get_next_value_compare_greater_or_equal(T, I).

closest_weekday({DateYY, DateMM, DateDD0} = Date) ->
    case day_of_week(Date) of
        0 -> % Sunday
            case date_add(Date, 1) of
                {DateYY, DateMM, DateDD1} ->
                    DateDD1;
                _ ->
                    {DateYY, DateMM, DateDD2} = date_add(Date, -2),
                    DateDD2
            end;
        6 -> % Saturday
            case date_add(Date, -1) of
                {DateYY, DateMM, DateDD1} ->
                    DateDD1;
                _ ->
                    {DateYY, DateMM, DateDD2} = date_add(Date, 2),
                    DateDD2
            end;
        _ ->
            DateDD0
    end.

last_day_of_week_find(Date, DayOfWeek) ->
    case day_of_week(Date) of
        DayOfWeek ->
            Date;
        _ ->
            last_day_of_week_find(date_add(Date, -1), DayOfWeek)
    end.

last_day_of_week({DateYY, DateMM, _} = Date, DayOfWeek) ->
    DateDD = calendar:last_day_of_the_month(DateYY, DateMM),
    Date == last_day_of_week_find({DateYY, DateMM, DateDD}, DayOfWeek).

every_day_of_week_find(Date, I, Multiplier, DayOfWeek) ->
    case day_of_week(Date) of
        DayOfWeek when I == Multiplier ->
            Date;
        DayOfWeek ->
            every_day_of_week_find(date_add(Date, 7), I + 1,
                                   Multiplier, DayOfWeek);
        _ ->
            every_day_of_week_find(date_add(Date, 1), I,
                                   Multiplier, DayOfWeek)
    end.

every_day_of_week_find(Date, Multiplier, DayOfWeek) ->
    every_day_of_week_find(Date, 1, Multiplier, DayOfWeek).

every_day_of_week({DateYY, DateMM, _} = Date, Multiplier, DayOfWeek) ->
    Date == every_day_of_week_find({DateYY, DateMM, 1}, Multiplier, DayOfWeek).

datetime_add(DateTime, SecondsIncr) ->
    Seconds = calendar:datetime_to_gregorian_seconds(DateTime),
    calendar:gregorian_seconds_to_datetime(Seconds + SecondsIncr).

date_add(Date, DaysIncr) ->
    Days = calendar:date_to_gregorian_days(Date),
    calendar:gregorian_days_to_date(Days + DaysIncr).

day_of_week(Date) ->
    case calendar:day_of_the_week(Date) of
        7 -> % Sunday
            0;
        DayOfWeek ->
            DayOfWeek
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

parse_expression_test() ->
    ExpressionString0 = "  @midnight ",
    ExpressionStrings0 = {"0", "0", "0", "*", "*", "*", "*"},
    Expression0 = {[0], [0], [0], undefined, undefined, undefined, undefined},
    #cloudi_cron{expression_strings = ExpressionStrings0,
                 expression = Expression0} = new(ExpressionString0),
    ExpressionString1 = "  40 16   *   * mon-fri     ",
    ExpressionStrings1 = {"0", "40", "16", "*", "*", "mon-fri", "*"},
    Expression1 = {[0], [40], [16], undefined, undefined,
                   [1,2,3,4,5], undefined},
    #cloudi_cron{expression_strings = ExpressionStrings1,
                 expression = Expression1} = new(ExpressionString1),
    ExpressionString2 = "18,38,58 17           *   * sat",
    ExpressionStrings2 = {"0", "18,38,58", "17", "*", "*", "sat", "*"},
    Expression2 = {[0], [18,38,58], [17], undefined, undefined,
                   [6], undefined},
    #cloudi_cron{expression_strings = ExpressionStrings2,
                 expression = Expression2} = new(ExpressionString2),
    ExpressionString3 = " 0  3    *   * sun-mon,wed-sat",
    ExpressionStrings3 = {"0", "0", "3", "*", "*", "sun-mon,wed-sat", "*"},
    Expression3 = {[0], [0], [3], undefined, undefined,
                   [0,1,3,4,5,6], undefined},
    #cloudi_cron{expression_strings = ExpressionStrings3,
                 expression = Expression3} = new(ExpressionString3),
    ExpressionString4 = " 0  3    L   *   mon#2",
    ExpressionStrings4 = {"0", "0", "3", "L", "*", "mon#2", "*"},
    Expression4 = {[0], [0], [3], [last_day_of_month], undefined,
                   [{every, 2, 1}], undefined},
    #cloudi_cron{expression_strings = ExpressionStrings4,
                 expression = Expression4} = new(ExpressionString4),
    ExpressionString5 = " 0  3    LW   *   monL",
    ExpressionStrings5 = {"0", "0", "3", "LW", "*", "monL", "*"},
    Expression5 = {[0], [0], [3], [{weekday, last_day_of_month}], undefined,
                   [{last_day_of_week, 1}], undefined},
    #cloudi_cron{expression_strings = ExpressionStrings5,
                 expression = Expression5} = new(ExpressionString5),
    ExpressionString6 = " 0  3    15W   *   5L",
    ExpressionStrings6 = {"0", "0", "3", "15W", "*", "5L", "*"},
    Expression6 = {[0], [0], [3], [{weekday, 15}], undefined,
                   [{last_day_of_week, 5}], undefined},
    #cloudi_cron{expression_strings = ExpressionStrings6,
                 expression = Expression6} = new(ExpressionString6),
    ok.

next_datetime_test() ->
    Cron0 = new("18,38,58 17 * * mon-fri"),
    DateTime0 = {{2019, 10, 25}, {17, 17, 59}},
    DateTime1 = {{2019, 10, 25}, {17, 18, 0}},
    DateTime1 = next_datetime(DateTime0, Cron0),
    DateTime2 = {{2019, 10, 25}, {17, 38, 0}},
    DateTime2 = next_datetime(DateTime1, Cron0),
    DateTime3 = {{2019, 10, 25}, {17, 58, 0}},
    DateTime3 = next_datetime(DateTime2, Cron0),
    DateTime4 = {{2019, 10, 28}, {17, 18, 0}},
    DateTime4 = next_datetime(DateTime3, Cron0),
    Cron1 = new("0 17 LW * *"),
    DateTime5 = {{2019, 10, 30}, {0, 0, 0}},
    DateTime6 = {{2019, 10, 31}, {17, 0, 0}},
    DateTime6 = next_datetime(DateTime5, Cron1),
    Cron2 = new("0 17 * * 1#3"),
    DateTime7 = {{2019, 10, 1}, {0, 0, 0}},
    DateTime8 = {{2019, 10, 21}, {17, 0, 0}},
    DateTime8 = next_datetime(DateTime7, Cron2),
    Cron3 = new("0 17 13W * *"),
    DateTime9 = {{2019, 10, 14}, {17, 0, 0}},
    DateTime9 = next_datetime(DateTime7, Cron3),
    Cron4 = new("0 17 12W * *"),
    DateTime10 = {{2019, 10, 11}, {17, 0, 0}},
    DateTime10 = next_datetime(DateTime7, Cron4),
    Cron5 = new("0 17 * * 1L"),
    DateTime11 = {{2019, 10, 28}, {17, 0, 0}},
    DateTime11 = next_datetime(DateTime7, Cron5),
    DateTime12 = {{2019, 11, 3}, {12, 30, 53}},
    DateTime13 = {{2019, 11, 4}, {17, 18, 0}},
    DateTime13 = next_datetime(DateTime12, Cron0),
    ok.

-endif.

