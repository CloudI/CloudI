%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Filesystem Date/Time==
%%% @end
%%%
%%% MIT LICENSE
%%% 
%%% Copyright (c) 2011-2013, Loïc Hoguin <essen@ninenines.eu>
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
%%% @copyright 2011-2013 Loïc Hoguin
%%% @version 1.3.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_filesystem_datetime).
-author('essen [at] ninenines (dot) eu').

%% external interface
-export([parse/1]).

-spec parse(binary()) ->
    calendar:datetime() | {error, badarg}.

parse(Binary) ->
    http_date(Binary).

%%%------------------------------------------------------------------------
%%% From cowboy, in cowboy_http.erl
%%%------------------------------------------------------------------------

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
-spec weekday(binary(), fun()) -> any().
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

-spec date1(binary(), fun()) -> any().
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

-spec date2(binary(), fun()) -> any().
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

-spec date3(binary(), fun()) -> any().
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

-spec time(binary(), fun()) -> any().
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

http_date_test_() ->
    %% {Tokens, Result}
    Tests = [
        {<<"Sun, 06 Nov 1994 08:49:37 GMT">>, {{1994, 11, 6}, {8, 49, 37}}},
        {<<"Sunday, 06-Nov-94 08:49:37 GMT">>, {{1994, 11, 6}, {8, 49, 37}}},
        {<<"Sun Nov  6 08:49:37 1994">>, {{1994, 11, 6}, {8, 49, 37}}}
    ],
    [{V, fun() -> R = http_date(V) end} || {V, R} <- Tests].

rfc1123_date_test_() ->
    %% {Tokens, Result}
    Tests = [
        {<<"Sun, 06 Nov 1994 08:49:37 GMT">>, {{1994, 11, 6}, {8, 49, 37}}}
    ],
    [{V, fun() -> R = rfc1123_date(V) end} || {V, R} <- Tests].

rfc850_date_test_() ->
    %% {Tokens, Result}
    Tests = [
        {<<"Sunday, 06-Nov-94 08:49:37 GMT">>, {{1994, 11, 6}, {8, 49, 37}}}
    ],
    [{V, fun() -> R = rfc850_date(V) end} || {V, R} <- Tests].

asctime_date_test_() ->
    %% {Tokens, Result}
    Tests = [
        {<<"Sun Nov  6 08:49:37 1994">>, {{1994, 11, 6}, {8, 49, 37}}}
    ],
    [{V, fun() -> R = asctime_date(V) end} || {V, R} <- Tests].

-endif.

