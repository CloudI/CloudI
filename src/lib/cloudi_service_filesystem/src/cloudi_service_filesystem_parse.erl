%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Filesystem Parsing of HTTP data==
%%% @end
%%%
%%% MIT LICENSE
%%% 
%%% Copyright (c) 2011-2014, Loïc Hoguin <essen@ninenines.eu>
%%% 
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%% 
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%%
%%% @author Loïc Hoguin <essen@ninenines.eu>
%%% @copyright 2011-2014 Loïc Hoguin
%%% @version 2.0.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_filesystem_parse).
-author('essen [at] ninenines (dot) eu').

%% external interface
-export([datetime/1,
         range/1]).

-spec datetime(binary()) ->
    calendar:datetime() |
    {error, badarg}.

datetime(Binary) ->
    http_date(Binary).

-spec range(binary()) ->
    {binary(),
     list({non_neg_integer(),
           non_neg_integer() | infinity} |
          neg_integer())} |
    {error, badarg}.

range(Binary) ->
    token_ci(Binary, fun range/2).

%%%------------------------------------------------------------------------
%%% From cowboy1, in cowboy1_bstr.erl
%%%------------------------------------------------------------------------

-spec char_to_lower(byte()) -> byte().
char_to_lower($A) -> $a;
char_to_lower($B) -> $b;
char_to_lower($C) -> $c;
char_to_lower($D) -> $d;
char_to_lower($E) -> $e;
char_to_lower($F) -> $f;
char_to_lower($G) -> $g;
char_to_lower($H) -> $h;
char_to_lower($I) -> $i;
char_to_lower($J) -> $j;
char_to_lower($K) -> $k;
char_to_lower($L) -> $l;
char_to_lower($M) -> $m;
char_to_lower($N) -> $n;
char_to_lower($O) -> $o;
char_to_lower($P) -> $p;
char_to_lower($Q) -> $q;
char_to_lower($R) -> $r;
char_to_lower($S) -> $s;
char_to_lower($T) -> $t;
char_to_lower($U) -> $u;
char_to_lower($V) -> $v;
char_to_lower($W) -> $w;
char_to_lower($X) -> $x;
char_to_lower($Y) -> $y;
char_to_lower($Z) -> $z;
char_to_lower(Ch) -> Ch.

%%%------------------------------------------------------------------------
%%% From cowboy1, in cowboy1_http.erl
%%%------------------------------------------------------------------------

-type datetime_fun() ::
    fun((_, _) -> calendar:datetime() | {error, badarg}).

%% @doc Parse an HTTP date (RFC1123, RFC850 or asctime date).
%% @end
%%
%% While this may not be the most efficient date parsing we can do,
%% it should work fine for our purposes because all HTTP dates should
%% be sent as RFC1123 dates in HTTP/1.1.
-spec http_date(binary()) ->
    calendar:datetime() | {error, badarg}.
http_date(Data) ->
    case rfc1123_date(Data) of
        {error, badarg} ->
            case rfc850_date(Data) of
                {error, badarg} ->
                    case asctime_date(Data) of
                        {error, badarg} ->
                            {error, badarg};
                        HTTPDate ->
                            HTTPDate
                    end;
                HTTPDate ->
                    HTTPDate
            end;
        HTTPDate ->
            HTTPDate
    end.

%% @doc Parse an RFC1123 date.
-spec rfc1123_date(binary()) ->
    calendar:datetime() | {error, badarg}.
rfc1123_date(Data) ->
    wkday(Data,
        fun (<< ", ", Rest/binary >>, _WkDay) ->
                date1(Rest,
                    fun (<< " ", Rest2/binary >>, Date) ->
                            time(Rest2,
                                fun (<< " GMT", Rest3/binary >>, Time) ->
                                        http_date_ret(Rest3, {Date, Time});
                                    (_Any, _Time) ->
                                        {error, badarg}
                                end);
                        (_Any, _Date) ->
                            {error, badarg}
                    end);
            (_Any, _WkDay) ->
                {error, badarg}
        end).

%% @doc Parse an RFC850 date.
-spec rfc850_date(binary()) ->
    calendar:datetime() | {error, badarg}.
%% From the RFC:
%% HTTP/1.1 clients and caches SHOULD assume that an RFC-850 date
%% which appears to be more than 50 years in the future is in fact
%% in the past (this helps solve the "year 2000" problem).
rfc850_date(Data) ->
    weekday(Data,
        fun (<< ", ", Rest/binary >>, _WeekDay) ->
                date2(Rest,
                    fun (<< " ", Rest2/binary >>, Date) ->
                            time(Rest2,
                                fun (<< " GMT", Rest3/binary >>, Time) ->
                                        http_date_ret(Rest3, {Date, Time});
                                    (_Any, _Time) ->
                                        {error, badarg}
                                end);
                        (_Any, _Date) ->
                            {error, badarg}
                    end);
            (_Any, _WeekDay) ->
                {error, badarg}
        end).

%% @doc Parse an asctime date.
-spec asctime_date(binary()) ->
    calendar:datetime() | {error, badarg}.
asctime_date(Data) ->
    wkday(Data,
        fun (<< " ", Rest/binary >>, _WkDay) ->
                date3(Rest,
                    fun (<< " ", Rest2/binary >>, PartialDate) ->
                            time(Rest2,
                                fun (<< " ", Rest3/binary >>, Time) ->
                                        asctime_year(Rest3,
                                            PartialDate, Time);
                                    (_Any, _Time) ->
                                        {error, badarg}
                                end);
                        (_Any, _PartialDate) ->
                            {error, badarg}
                    end);
            (_Any, _WkDay) ->
                {error, badarg1}
        end).

-spec asctime_year(binary(), tuple(), tuple()) ->
    calendar:datetime() | {error, badarg}.
asctime_year(<< Y1, Y2, Y3, Y4, Rest/binary >>, {Month, Day}, Time)
        when Y1 >= $0, Y1 =< $9, Y2 >= $0, Y2 =< $9,
             Y3 >= $0, Y3 =< $9, Y4 >= $0, Y4 =< $9 ->
    Year = (Y1 - $0) * 1000 + (Y2 - $0) * 100 + (Y3 - $0) * 10 + (Y4 - $0),
    http_date_ret(Rest, {{Year, Month, Day}, Time}).

-spec http_date_ret(binary(), tuple()) ->
    calendar:datetime() | {error, badarg}.
http_date_ret(Data, DateTime = {Date, _Time}) ->
    whitespace(Data,
        fun (<<>>) ->
                case calendar:valid_date(Date) of
                    true -> DateTime;
                    false -> {error, badarg}
                end;
            (_Any) ->
                {error, badarg}
        end).

%% We never use it, pretty much just checks the wkday is right.
-spec wkday(binary(), fun()) -> any().
wkday(<< WkDay:3/binary, Rest/binary >>, Fun)
        when WkDay =:= <<"Mon">>; WkDay =:= <<"Tue">>; WkDay =:= <<"Wed">>;
             WkDay =:= <<"Thu">>; WkDay =:= <<"Fri">>; WkDay =:= <<"Sat">>;
             WkDay =:= <<"Sun">> ->
    Fun(Rest, WkDay);
wkday(_Any, _Fun) ->
    {error, badarg}.

%% We never use it, pretty much just checks the weekday is right.
-spec weekday(binary(), datetime_fun()) -> any().
weekday(<< "Monday", Rest/binary >>, Fun) ->
    Fun(Rest, <<"Monday">>);
weekday(<< "Tuesday", Rest/binary >>, Fun) ->
    Fun(Rest, <<"Tuesday">>);
weekday(<< "Wednesday", Rest/binary >>, Fun) ->
    Fun(Rest, <<"Wednesday">>);
weekday(<< "Thursday", Rest/binary >>, Fun) ->
    Fun(Rest, <<"Thursday">>);
weekday(<< "Friday", Rest/binary >>, Fun) ->
    Fun(Rest, <<"Friday">>);
weekday(<< "Saturday", Rest/binary >>, Fun) ->
    Fun(Rest, <<"Saturday">>);
weekday(<< "Sunday", Rest/binary >>, Fun) ->
    Fun(Rest, <<"Sunday">>);
weekday(_Any, _Fun) ->
    {error, badarg}.

-spec date1(binary(), datetime_fun()) -> calendar:datetime() | {error, badarg}.
date1(<< D1, D2, " ", M:3/binary, " ", Y1, Y2, Y3, Y4, Rest/binary >>, Fun)
        when D1 >= $0, D1 =< $9, D2 >= $0, D2 =< $9,
             Y1 >= $0, Y1 =< $9, Y2 >= $0, Y2 =< $9,
             Y3 >= $0, Y3 =< $9, Y4 >= $0, Y4 =< $9 ->
    case month(M) of
        {error, badarg} ->
            {error, badarg};
        Month ->
            Fun(Rest, {
                (Y1 - $0) * 1000 + (Y2 - $0) * 100 + (Y3 - $0) * 10 + (Y4 - $0),
                Month,
                (D1 - $0) * 10 + (D2 - $0)
            })
    end;
date1(_Data, _Fun) ->
    {error, badarg}.

-spec date2(binary(), datetime_fun()) -> calendar:datetime() | {error, badarg}.
date2(<< D1, D2, "-", M:3/binary, "-", Y1, Y2, Rest/binary >>, Fun)
        when D1 >= $0, D1 =< $9, D2 >= $0, D2 =< $9,
             Y1 >= $0, Y1 =< $9, Y2 >= $0, Y2 =< $9 ->
    case month(M) of
        {error, badarg} ->
            {error, badarg};
        Month ->
            Year = (Y1 - $0) * 10 + (Y2 - $0),
            Year2 = case Year > 50 of
                true -> Year + 1900;
                false -> Year + 2000
            end,
            Fun(Rest, {
                Year2,
                Month,
                (D1 - $0) * 10 + (D2 - $0)
            })
    end;
date2(_Data, _Fun) ->
    {error, badarg}.

-spec date3(binary(), datetime_fun()) -> any().
date3(<< M:3/binary, " ", D1, D2, Rest/binary >>, Fun)
        when (D1 >= $0 andalso D1 =< $3) orelse D1 =:= $\s,
             D2 >= $0, D2 =< $9 ->
    case month(M) of
        {error, badarg} ->
            {error, badarg};
        Month ->
            Day = case D1 of
                $\s -> D2 - $0;
                D1 -> (D1 - $0) * 10 + (D2 - $0)
            end,
            Fun(Rest, {Month, Day})
    end;
date3(_Data, _Fun) ->
    {error, badarg}.

-spec month(<< _:24 >>) -> 1..12 | {error, badarg}.
month(<<"Jan">>) -> 1;
month(<<"Feb">>) -> 2;
month(<<"Mar">>) -> 3;
month(<<"Apr">>) -> 4;
month(<<"May">>) -> 5;
month(<<"Jun">>) -> 6;
month(<<"Jul">>) -> 7;
month(<<"Aug">>) -> 8;
month(<<"Sep">>) -> 9;
month(<<"Oct">>) -> 10;
month(<<"Nov">>) -> 11;
month(<<"Dec">>) -> 12;
month(_Any) -> {error, badarg}.

-spec time(binary(), datetime_fun()) -> calendar:datetime() | {error, badarg}.
time(<< H1, H2, ":", M1, M2, ":", S1, S2, Rest/binary >>, Fun)
        when H1 >= $0, H1 =< $2, H2 >= $0, H2 =< $9,
             M1 >= $0, M1 =< $5, M2 >= $0, M2 =< $9,
             S1 >= $0, S1 =< $5, S2 >= $0, S2 =< $9 ->
    Hour = (H1 - $0) * 10 + (H2 - $0),
    case Hour < 24 of
        true ->
            Time = {
                Hour,
                (M1 - $0) * 10 + (M2 - $0),
                (S1 - $0) * 10 + (S2 - $0)
            },
            Fun(Rest, Time);
        false ->
            {error, badarg}
    end.

range(Data, Token) ->
    whitespace(Data,
        fun(<<"=", Rest/binary>>) ->
            case list(Rest, fun range_beginning/2) of
                {error, badarg} ->
                    {error, badarg};
                Ranges ->
                    {Token, Ranges}
            end;
           (_) ->
            {error, badarg}
        end).

range_beginning(Data, Fun) ->
    range_digits(Data, suffix,
        fun(D, RangeBeginning) ->
            range_ending(D, Fun, RangeBeginning)
        end).

range_ending(Data, Fun, RangeBeginning) ->
    whitespace(Data,
        fun(<<"-", R/binary>>) ->
            case RangeBeginning of
                suffix ->
                    range_digits(R,
                                 fun(D, RangeEnding) ->
                                     Fun(D, -RangeEnding)
                                 end);
                _ ->
                    range_digits(R, infinity,
                        fun(D, RangeEnding) ->
                            Fun(D, {RangeBeginning, RangeEnding})
                        end)
            end;
           (_) ->
            {error, badarg}
        end).

-spec range_digits(binary(), fun((_, _) -> any())) -> any().
range_digits(Data, Fun) ->
    whitespace(Data,
        fun(D) ->
            digits(D, Fun)
        end).

-spec range_digits(binary(), infinity | suffix, fun((_, _) -> any())) -> any().
range_digits(Data, Default, Fun) ->
    whitespace(Data,
        fun(<< C, Rest/binary >>) when C >= $0, C =< $9 ->
            digits(Rest, Fun, C - $0);
           (_) ->
            Fun(Data, Default)
        end).

-spec list(binary(), fun((_, _) -> any())) -> list() | {error, badarg}.
list(Data, Fun) ->
    case list(Data, Fun, []) of
        {error, badarg} -> {error, badarg};
        L -> lists:reverse(L)
    end.

-spec list(binary(), fun(), [binary()]) -> [any()] | {error, badarg}.
%% From the RFC:
%% <blockquote>Wherever this construct is used, null elements are allowed,
%% but do not contribute to the count of elements present.
%% That is, "(element), , (element) " is permitted, but counts
%% as only two elements. Therefore, where at least one element is required,
%% at least one non-null element MUST be present.</blockquote>
list(Data, Fun, Acc) ->
    whitespace(Data,
        fun (<<>>) -> Acc;
            (<< $,, Rest/binary >>) -> list(Rest, Fun, Acc);
            (Rest) -> Fun(Rest,
                fun (D, I) -> whitespace(D,
                        fun (<<>>) -> [I|Acc];
                            (<< $,, R/binary >>) -> list(R, Fun, [I|Acc]);
                            (_Any) -> {error, badarg}
                        end)
                end)
        end).

-spec digits(binary(), fun()) -> any().
digits(<< C, Rest/binary >>, Fun)
        when C >= $0, C =< $9 ->
    digits(Rest, Fun, C - $0);
digits(_Data, _Fun) ->
    {error, badarg}.

-spec digits(binary(), fun(), non_neg_integer()) -> any().
digits(<< C, Rest/binary >>, Fun, Acc)
        when C >= $0, C =< $9 ->
    digits(Rest, Fun, Acc * 10 + (C - $0));
digits(Data, Fun, Acc) ->
    Fun(Data, Acc).

%% Changes all characters to lowercase.
-spec token_ci(binary(), fun((_, _) -> any())) -> any().
token_ci(Data, Fun) ->
    token(Data, Fun, ci, <<>>).

%-spec token(binary(), fun()) -> any().
%token(Data, Fun) ->
%    token(Data, Fun, cs, <<>>).

-spec token(binary(), fun(), ci | cs, binary()) -> any().
token(<<>>, Fun, _Case, Acc) ->
    Fun(<<>>, Acc);
token(Data = << C, _Rest/binary >>, Fun, _Case, Acc)
        when C =:= $(; C =:= $); C =:= $<; C =:= $>; C =:= $@;
             C =:= $,; C =:= $;; C =:= $:; C =:= $\\; C =:= $";
             C =:= $/; C =:= $[; C =:= $]; C =:= $?; C =:= $=;
             C =:= ${; C =:= $}; C =:= $\s; C =:= $\t;
             C < 32; C =:= 127 ->
    Fun(Data, Acc);
token(<< C, Rest/binary >>, Fun, Case = ci, Acc) ->
    C2 = char_to_lower(C),
    token(Rest, Fun, Case, << Acc/binary, C2 >>);
token(<< C, Rest/binary >>, Fun, Case, Acc) ->
    token(Rest, Fun, Case, << Acc/binary, C >>).

%% @doc Skip whitespace.
-spec whitespace(binary(), fun()) -> any().
whitespace(<< C, Rest/binary >>, Fun)
        when C =:= $\s; C =:= $\t ->
    whitespace(Rest, Fun);
whitespace(Data, Fun) ->
    Fun(Data).

%% Tests.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-include("cloudi_service_filesystem_test.hrl").

module_test_() ->
    {timeout, ?TEST_TIMEOUT, [
        {"http date tests", t_http_date()},
        {"rfc1123 date tests", t_rfc1123_date()},
        {"rfc850 date tests", t_rfc850_date()},
        {"asctime date tests", t_asctime_date()},
        {"http range tests", t_http_range()}
    ]}.

t_http_date() ->
    %% {Tokens, Result}
    Tests = [
        {<<"Sun, 06 Nov 1994 08:49:37 GMT">>, {{1994, 11, 6}, {8, 49, 37}}},
        {<<"Sunday, 06-Nov-94 08:49:37 GMT">>, {{1994, 11, 6}, {8, 49, 37}}},
        {<<"Sun Nov  6 08:49:37 1994">>, {{1994, 11, 6}, {8, 49, 37}}}
    ],
    [{V, ?_assertEqual(R, http_date(V))} || {V, R} <- Tests].

t_rfc1123_date() ->
    %% {Tokens, Result}
    Tests = [
        {<<"Sun, 06 Nov 1994 08:49:37 GMT">>, {{1994, 11, 6}, {8, 49, 37}}}
    ],
    [{V, ?_assertEqual(R, rfc1123_date(V))} || {V, R} <- Tests].

t_rfc850_date() ->
    %% {Tokens, Result}
    Tests = [
        {<<"Sunday, 06-Nov-94 08:49:37 GMT">>, {{1994, 11, 6}, {8, 49, 37}}}
    ],
    [{V, ?_assertEqual(R, rfc850_date(V))} || {V, R} <- Tests].

t_asctime_date() ->
    %% {Tokens, Result}
    Tests = [
        {<<"Sun Nov  6 08:49:37 1994">>, {{1994, 11, 6}, {8, 49, 37}}}
    ],
    [{V, ?_assertEqual(R, asctime_date(V))} || {V, R} <- Tests].

t_http_range() ->
    Tests = [
        {<<"bytes=1-20">>,
            {<<"bytes">>, [{1, 20}]}},
        {<<"bytes=-100">>,
            {<<"bytes">>, [-100]}},
        {<<"bytes=1-">>,
            {<<"bytes">>, [{1, infinity}]}},
        {<<"bytes=1-20,30-40,50-">>,
            {<<"bytes">>, [{1, 20}, {30, 40}, {50, infinity}]}},
        {<<"bytes = 1 - 20 , 50 - , - 300 ">>,
            {<<"bytes">>, [{1, 20}, {50, infinity}, -300]}},
        {<<"bytes=1-20,-500,30-40">>,
            {<<"bytes">>, [{1, 20}, -500, {30, 40}]}},
        {<<"test=1-20,-500,30-40">>,
            {<<"test">>, [{1, 20}, -500, {30, 40}]}},
        {<<"bytes=-">>,
            {error, badarg}},
        {<<"bytes=-30,-">>,
            {error, badarg}}
    ],
    [?_assertEqual(R, range(V)) || {V, R} <- Tests].

-endif.

