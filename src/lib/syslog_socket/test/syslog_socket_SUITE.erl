%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%------------------------------------------------------------------------
%%% @doc
%%% ==syslog Socket Tests==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2016-2020 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2016-2020 Michael Truog
%%% @version 1.8.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(syslog_socket_SUITE).

%% CT callbacks
-export([all/0,
         groups/0,
         suite/0,
         init_per_suite/1,
         end_per_suite/1,
         group/1,
         init_per_group/2,
         end_per_group/2,
         init_per_testcase/2,
         end_per_testcase/2]).

%% test callbacks
-export([t_facility_1/1,
         t_severity_1/1,
         t_local_output_1/1]).

-include_lib("common_test/include/ct.hrl").

-ifdef(CLOUDI_TEST_TIMEOUT).
-define(TEST_TIMEOUT, ?CLOUDI_TEST_TIMEOUT). % seconds
-else.
-define(TEST_TIMEOUT, 10). % seconds
-endif.
% for features specific to Erlang/OTP version 20.x (and later versions)
-ifdef(ERLANG_OTP_VERSION_19).
-else.
-define(ERLANG_OTP_VERSION_20_FEATURES, true).
-endif.

%%%------------------------------------------------------------------------
%%% Callback functions from CT
%%%------------------------------------------------------------------------

all() ->
    [{group, basic_1}].

groups() ->
    [{basic_1, [],
      [t_facility_1,
       t_severity_1]}].

suite() ->
    [{ct_hooks, [cth_surefire]},
     {timetrap, {seconds, ?TEST_TIMEOUT}}].

init_per_suite(Config) ->
    {ok, _} = application:ensure_all_started(syslog_socket),
    Config.

end_per_suite(_Config) ->
    ok = application:stop(syslog_socket),
    ok.

group(_GroupName) ->
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, Config) ->
    Config.

%%%------------------------------------------------------------------------
%%% test cases
%%%------------------------------------------------------------------------

t_facility_1(_Config) ->
    0  = syslog_socket:facility(kernel),
    0  = syslog_socket:facility(kern),
    0  = syslog_socket:facility(0),
    4  = syslog_socket:facility(auth0),
    4  = syslog_socket:facility(auth),
    4  = syslog_socket:facility(security),
    6  = syslog_socket:facility(print),
    6  = syslog_socket:facility(lpr),
    9  = syslog_socket:facility(clock0),
    9  = syslog_socket:facility(cron),
    10 = syslog_socket:facility(auth1),
    10 = syslog_socket:facility(authpriv),
    13 = syslog_socket:facility(auth2),
    14 = syslog_socket:facility(auth3),
    15 = syslog_socket:facility(clock1),
    23 = syslog_socket:facility(local7),
    99 = syslog_socket:facility(99),
    false = syslog_socket:facility_valid(-1),
    true = syslog_socket:facility_valid(0),
    true = syslog_socket:facility_valid(99),
    true = syslog_socket:facility_valid(clock1),
    false = syslog_socket:facility_valid(invalid_facility),
    ok.

t_severity_1(_Config) ->
    0 = syslog_socket:severity(emergency),
    0 = syslog_socket:severity(emerg),
    0 = syslog_socket:severity(panic),
    0 = syslog_socket:severity(0),
    1 = syslog_socket:severity(alert),
    2 = syslog_socket:severity(critical),
    2 = syslog_socket:severity(crit),
    3 = syslog_socket:severity(error),
    3 = syslog_socket:severity(err),
    4 = syslog_socket:severity(warning),
    4 = syslog_socket:severity(warn),
    5 = syslog_socket:severity(notice),
    6 = syslog_socket:severity(informational),
    6 = syslog_socket:severity(info),
    7 = syslog_socket:severity(debug),
    7 = syslog_socket:severity(7),
    false = syslog_socket:severity_valid(-1),
    true = syslog_socket:severity_valid(0),
    true = syslog_socket:severity_valid(7),
    true = syslog_socket:severity_valid(debug),
    false = syslog_socket:severity_valid(trace),
    false = syslog_socket:severity_valid(8),
    ok.

t_local_output_1(_Config) ->
    AppName = "syslog_socket_SUITE",
    Options = [{protocol, rfc3164},
               {facility, user},
               {app_name, AppName}],
    MessageId = erlang:pid_to_list(self()),
    Message = "Automated Test Message",
    {ok, Pid} = syslog_socket:start_link(Options),
    ok = syslog_socket:send(Pid, notice, os:timestamp(),
                            MessageId, Message ++ " 1"),
    ok = syslog_socket:send(Pid, notice, os:timestamp(), Message ++ " 2"),
    ok = syslog_socket:send(Pid, notice, os:timestamp(), ""),
    ok = syslog_socket:stop_link(Pid, 5000),
    receive after 10 -> ok end,
    {ok, SyslogData} = file:read_file("/var/log/syslog"),
    SyslogDataLines = case lists:reverse(binary:split(SyslogData, <<"\n">>,
                                                      [global])) of
        [<<>> | SyslogDataLinesValue] ->
            SyslogDataLinesValue;
        SyslogDataLinesValue ->
            SyslogDataLinesValue
    end,
    [SyslogDataLine3,
     SyslogDataLine2,
     SyslogDataLine1 | _] = SyslogDataLines,
    SyslogDataLine1Str = erlang:binary_to_list(SyslogDataLine1),
    SyslogDataLine2Str = erlang:binary_to_list(SyslogDataLine2),
    SyslogDataLine3Str = erlang:binary_to_list(SyslogDataLine3),
    true = string_find(Message, SyslogDataLine1Str) /= nomatch,
    true = string_find(Message, SyslogDataLine2Str) /= nomatch,
    true = string_find(MessageId, SyslogDataLine1Str) /= nomatch,
    true = string_find(AppName, SyslogDataLine1Str) /= nomatch,
    true = string_find(AppName, SyslogDataLine2Str) /= nomatch,
    true = string_find(AppName, SyslogDataLine3Str) /= nomatch,
    ok.

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
string_find(SearchPattern, String) ->
    string:find(String, SearchPattern).
-else.
string_find(SearchPattern, String) ->
    StringList = if
        is_binary(String) ->
            erlang:binary_to_list(String);
        is_list(String) ->
            lists:flatten(String)
    end,
    SearchPatternList = if
        is_binary(SearchPattern) ->
            erlang:binary_to_list(SearchPattern);
        is_list(SearchPattern) ->
            lists:flatten(SearchPattern)
    end,
    case string:str(StringList, SearchPatternList) of
        0 ->
            nomatch;
        Index ->
            Result = lists:nthtail(Index - 1, StringList),
            if
                is_binary(String) ->
                    erlang:list_to_binary(Result);
                is_list(String) ->
                    Result
            end
    end.
-endif.
