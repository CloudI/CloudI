%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%------------------------------------------------------------------------
%%% @doc
%%% ==syslog Socket Tests==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2016, Michael Truog <mjtruog at gmail dot com>
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%% 
%%%     * Redistributions of source code must retain the above copyright
%%%       notice, this list of conditions and the following disclaimer.
%%%     * Redistributions in binary form must reproduce the above copyright
%%%       notice, this list of conditions and the following disclaimer in
%%%       the documentation and/or other materials provided with the
%%%       distribution.
%%%     * All advertising materials mentioning features or use of this
%%%       software must display the following acknowledgment:
%%%         This product includes software developed by Michael Truog
%%%     * The name of the author may not be used to endorse or promote
%%%       products derived from this software without specific prior
%%%       written permission
%%% 
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
%%% DAMAGE.
%%%
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2016 Michael Truog
%%% @version 1.5.5 {@date} {@time}
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

%%%------------------------------------------------------------------------
%%% Callback functions from CT
%%%------------------------------------------------------------------------

all() ->
    [{group, basic_1}].

groups() ->
    [{basic_1, [],
      [t_facility_1,
       t_severity_1,
       t_local_output_1]}].

suite() ->
    [{ct_hooks, [cth_surefire]},
     {timetrap, 10100}].

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
    {ok, SyslogData} = file:read_file("/var/l""og/sysl""og"),
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
    true = string:str(SyslogDataLine1Str, Message) > 0,
    true = string:str(SyslogDataLine2Str, Message) > 0,
    true = string:str(SyslogDataLine1Str, MessageId) > 0,
    true = string:str(SyslogDataLine1Str, AppName) > 0,
    true = string:str(SyslogDataLine2Str, AppName) > 0,
    true = string:str(SyslogDataLine3Str, AppName) > 0,
    ok.

