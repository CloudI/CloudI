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
%%% Seconds        No           0-59              * / , - ~
%%% Minutes        Yes          0-59              * / , - ~
%%% Hours          Yes          0-23              * / , - ~
%%% Day of month   Yes          1-31              * / , - ~ L W
%%% Month          Yes          1-12 or JAN-DEC   * / , - ~
%%% Day of week    Yes          0-6 or SUN-SAT    * / , - ~ L #
%%% Year           No           1970–9999         * / , - ~
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
%%% between 2000 and 2010 inclusive.
%%%
%%% === Tilde (~) ===
%%% Tildes define random ranges. For example, 0~59 will result in a random
%%% value between 0 and 59 inclusive.
%%%
%%% A random range can be used with a step value. For example, 0~59/10 will
%%% use a random offset for the first sequence value between 0 and 9 inclusive.
%%% The field's min/max values may be used by excluding the random range
%%% values as ~ or ~/10.
%%%
%%% Random values are determined when the expression is parsed with the
%%% random value remaining constant afterwards.  The purpose of the randomness
%%% is to avoid any thundering herd problems between separate uses of
%%% similar cron expressions
%%% (https://en.wikipedia.org/wiki/Thundering_herd_problem).
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
%%% This implementation allows the W character to be used in a list.
%%% For example, "1W,15W" is valid in the day-of-month field.
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
%%% * Random range support (with ~) is based on OpenBSD cron
%%% '''
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2019-2023 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2019-2023 Michael Truog
%%% @version 2.0.6 {@date} {@time}
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
    true = Expression /= {undefined, undefined, undefined,
                          undefined, undefined, undefined, undefined},
    #cloudi_cron{expression_strings = ExpressionStrings,
                 expression = Expression}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Determine the next datetime based on the cron expression.===
%% @end
%%-------------------------------------------------------------------------

-spec next_datetime(DateTime :: calendar:datetime(),
                    state()) ->
    calendar:datetime() | undefined.

next_datetime({Date0, _} = DateTime0,
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
    case next_datetime_year(Expression, Equal6, DateTime6) of
        undefined ->
            undefined;
        {Date0, _} = DateTimeN ->
            DateTimeN;
        {DateN, _} ->
            DateTime7 = {DateN, {0, 0, 0}},
            {true,
             DateTime8} = next_datetime_seconds(Expression,
                                                true, DateTime7),
            {true,
             DateTime9} = next_datetime_minutes(Expression,
                                                true, DateTime8),
            {true,
             DateTime10} = next_datetime_hours(Expression,
                                               true, DateTime9),
            {true,
             DateTime11} = next_datetime_day_of_month(Expression,
                                                      true, DateTime10),
            {true,
             DateTime12} = next_datetime_month(Expression,
                                               true, DateTime11),
            {true,
             DateTimeN} = next_datetime_day_of_week(Expression,
                                                    true, DateTime12),
            DateTimeN
    end.

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
                    case cloudi_string:splitl($~, Segment, input) of
                        {Segment, ""} ->
                            parse_value_name(Allow, Segment, Min, Max, Names);
                        {RandomMinStr, RandomMaxStr} ->
                            parse_value_random(RandomMinStr, RandomMaxStr,
                                               undefined, Min, Max, Names)
                    end;
                {RangeMinStr, RangeMaxStr} ->
                    parse_value_range(RangeMinStr, RangeMaxStr,
                                      "1", Min, Max, Names)
            end;
        {[Match], StepStr}
            when Match =:= $* orelse Match =:= $~ ->
            Step = erlang:list_to_integer(StepStr),
            true = (Step > Min) andalso (Step < Max),
            MinNew = if
                Match =:= $* ->
                    Min;
                Match =:= $~ ->
                    random(Min, Min + Step - 1)
            end,
            lists:seq(MinNew, Max, Step);
        {RangeStr, StepStr} ->
            case cloudi_string:splitl($-, RangeStr, input) of
                {RangeStr, ""} ->
                    {RandomMinStr,
                     RandomMaxStr} = cloudi_string:splitl($~, RangeStr, empty),
                    parse_value_random(RandomMinStr, RandomMaxStr,
                                       StepStr, Min, Max, Names);
                {RangeMinStr, RangeMaxStr} ->
                    parse_value_range(RangeMinStr, RangeMaxStr,
                                      StepStr, Min, Max, Names)
            end
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
    true = is_integer(RangeMin),
    true = is_integer(RangeMax),
    true = (RangeMin < RangeMax),
    Step = erlang:list_to_integer(StepStr),
    true = (Step >= 1) andalso (Step < Max),
    lists:seq(RangeMin, RangeMax, Step).

parse_value_random([], [], undefined, Min, Max, _) ->
    random(Min, Max);
parse_value_random(RandomMinStr, RandomMaxStr, StepStr, Min, Max, Names) ->
    [_ | _] = RandomMinStr,
    [_ | _] = RandomMaxStr,
    RandomMin = parse_value_name_integer(RandomMinStr, Min, Max, Names),
    RandomMax = parse_value_name_integer(RandomMaxStr, Min, Max, Names),
    true = is_integer(RandomMin),
    true = is_integer(RandomMax),
    true = (RandomMin < RandomMax),
    if
        StepStr =:= undefined ->
            random(RandomMin, RandomMax);
        is_list(StepStr) ->
            Step = erlang:list_to_integer(StepStr),
            true = (Step > Min) andalso (Step < Max),
            RandomMinNew = random(RandomMin, RandomMin + Step - 1),
            lists:seq(RandomMinNew, RandomMax, Step)
    end.

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
                    FieldYear}, Equal,
                   {{DateYY, _, _} = Date, _} = DateTime) ->
    case get_next_value(FieldYear, DateYY, Equal, Date) of
        undefined ->
            undefined;
        DateYY ->
            DateTime;
        DateYYNew ->
            {{DateYYNew, 1, 1}, {0, 0, 0}}
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

random(Min, Max) ->
    cloudi_x_quickrand_cache:uniform_range(Min, Max).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-include("cloudi_service_cron_test.hrl").

module_test_() ->
    {timeout, ?TEST_TIMEOUT, [
        {"parse expression tests", ?_assertOk(t_parse_expression())},
        {"next_datetime tests", ?_assertOk(t_next_datetime())}
    ]}.

t_parse_expression() ->
    ok = cloudi_x_quickrand_cache:init([{cache_size, 65536}]),
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
    #cloudi_cron{expression_strings = {"0", "0~59/10", "23",
                                       "*", "*", "sun", "*"},
                 expression = {[0], Minutes0, [23],
                               undefined, undefined, [0], undefined}
                 } = new("0~59/10 23 * * sun"),
    [Minute0 | _] = Minutes0,
    true = is_integer(Minute0) andalso Minute0 >= 0 andalso Minute0 =< 9,
    true = length(Minutes0) == 6,
    #cloudi_cron{expression_strings = {"0", "~/10", "23", "*", "*", "sun", "*"},
                 expression = {[0], Minutes1, [23],
                               undefined, undefined, [0], undefined}
                 } = new("~/10 23 * * sun"),
    [Minute1 | _] = Minutes1,
    true = is_integer(Minute1) andalso Minute1 >= 0 andalso Minute1 =< 9,
    true = length(Minutes1) == 6,
    #cloudi_cron{expression_strings = {"0", "0~59", "23", "*", "*", "sun", "*"},
                 expression = {[0], [Minute2], [23],
                               undefined, undefined, [0], undefined}
                 } = new("0~59 23 * * sun"),
    true = is_integer(Minute2) andalso Minute2 >= 0 andalso Minute2 =< 59,
    #cloudi_cron{expression_strings = {"0", "~", "23", "*", "*", "sun", "*"},
                 expression = {[0], [Minute3], [23],
                               undefined, undefined, [0], undefined}
                 } = new("~ 23 * * sun"),
    true = is_integer(Minute3) andalso Minute3 >= 0 andalso Minute3 =< 59,
    ok = cloudi_x_quickrand_cache:destroy(),
    ok.

t_next_datetime() ->
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
    Cron6 = new("*/1 * * * * 1970"),
    DateTime14 = {{2020, 8, 17}, {17, 0, 0}},
    undefined = next_datetime(DateTime14, Cron6),
    Cron7 = new("18,38,58 17 * * mon-fri 2020"),
    DateTime15 = {{2020, 1, 1}, {17, 18, 0}},
    DateTime15 = next_datetime(DateTime0, Cron7),
    DateTime16 = {{2020, 1, 1}, {17, 38, 0}},
    DateTime16 = next_datetime(DateTime15, Cron7),
    ok.

-endif.

