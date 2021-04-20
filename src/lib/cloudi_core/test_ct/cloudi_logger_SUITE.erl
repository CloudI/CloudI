%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Core Logger Tests==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2021 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2021 Michael Truog
%%% @version 2.0.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_logger_SUITE).

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
-export([t_cloudi_logger_basic_1/1,
         t_cloudi_logger_lager_1/1,
         t_cloudi_logger_hut_1/1]).

-include_lib("common_test/include/ct.hrl").
-include_lib("cloudi_core/include/cloudi_logger.hrl").
-compile([{parse_transform, cloudi_logger_lager}]).

-ifndef(CLOUDI_TEST_TIMEOUT).
-define(CLOUDI_TEST_TIMEOUT, 10). % seconds
-define(CLOUDI_CORE_STANDALONE, true).
-endif.
-define(LOG_MESSAGE1, "КатегорииРўСЋР»РїР°РЅС‹ВсетоварыОтзывыдалееНетотзывовИнформацияОтгрузкаивозвраттовараУведомлениеосекретностиУсловияиспользованияОбратнаясвязьКартасайтаДисконтныекарточкиПомощьДисконтныекарточкиОтписатсяотновостнойрассылкиТюлпаны").

%%%------------------------------------------------------------------------
%%% Callback functions from CT
%%%------------------------------------------------------------------------

all() ->
    [{group, cloudi_logger_1}].

groups() ->
    [{cloudi_logger_1, [sequence],
      [t_cloudi_logger_basic_1,
       t_cloudi_logger_lager_1,
       t_cloudi_logger_hut_1]}].

suite() ->
    [{ct_hooks, [cth_surefire]},
     {timetrap, {seconds, ?CLOUDI_TEST_TIMEOUT}}].

init_per_suite(Config) ->
    ok = cloudi_x_reltool_util:application_start(cloudi_x_hut,
                                                 [{use_log_level_gate, true},
                                                  {level, info}], infinity),
    ok = cloudi_x_reltool_util:application_start(cloudi_core, [], infinity),
    Config.

end_per_suite(_Config) ->
    ok = cloudi_x_reltool_util:application_stop(cloudi_core),
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

t_cloudi_logger_basic_1(_Config) ->
    ok = set_aspect_log_after(),
    ?LOG_INFO("~ts", [?LOG_MESSAGE1]),
    ?LOG_INFO(?LOG_MESSAGE1, []),
    ok = get_log_message(2, unicode:characters_to_binary(?LOG_MESSAGE1)),
    ?LOG_INFO(?LOG_MESSAGE1, undefined),
    ok = get_log_message(1, ?LOG_MESSAGE1),
    ok.

t_cloudi_logger_lager_1(_Config) ->
    ok = set_aspect_log_after(),
    cloudi_logger_lager:log(info, undefined, ?LOG_MESSAGE1),
    cloudi_logger_lager:log(info, undefined, ?LOG_MESSAGE1, []),
    lager:info(?LOG_MESSAGE1),
    lager:info(?LOG_MESSAGE1, []),
    lager:info([{}], ?LOG_MESSAGE1),
    lager:info([{}], ?LOG_MESSAGE1, []),
    ok = get_log_message(6, unicode:characters_to_binary(?LOG_MESSAGE1)),
    ok.

-ifdef(CLOUDI_CORE_STANDALONE).
t_cloudi_logger_hut_1(_Config) ->
    ok.
-else.
-include_lib("cloudi_x_hut/include/cloudi_x_hut.hrl").

t_cloudi_logger_hut_1(_Config) ->
    ok = set_aspect_log_after(),
    ?log(info, "~ts", [?LOG_MESSAGE1]),
    ?log(info, ?LOG_MESSAGE1, []),
    ok = get_log_message(2, unicode:characters_to_binary(?LOG_MESSAGE1)),
    ?slog(info, ?LOG_MESSAGE1),
    ok = get_log_message(1, ?LOG_MESSAGE1),
    ok.
-endif.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

set_aspect_log_after() ->
    MessagePid = self(),
    {ok, Logging} = cloudi_service_api:logging(infinity),
    AspectLogAfter = fun(_Level, _Timestamp, _Node, _Pid,
                         _Module, _Line, _Function, _Arity,
                         _MetaData, LogMessage) ->
        MessagePid ! LogMessage,
        ok
    end,
    LoggingNew = lists:keystore(aspects_log_after, 1, Logging,
                                {aspects_log_after, [AspectLogAfter]}),
    ok = cloudi_service_api:logging_set(LoggingNew, infinity).

get_log_message(0, _) ->
    ok;
get_log_message(Count, LogMessage) ->
    receive
        LogMessage ->
            get_log_message(Count - 1, LogMessage)
    after
        5000 ->
            receive
                Invalid ->
                    {error, Invalid}
            after
                0 ->
                    {error, undefined}
            end
    end.

