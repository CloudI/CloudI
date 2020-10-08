%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Runtime Testing==
%%% Routines for live-service testing during development.
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2013-2017 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2013-2017 Michael Truog
%%% @version 1.7.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_core_i_runtime_testing).
-author('mjtruog at protonmail dot com').

%% external interface
-export([monkey_latency_format/1,
         monkey_latency_validate/1,
         monkey_latency_init/1,
         monkey_latency_check/1,
         monkey_chaos_format/1,
         monkey_chaos_validate/1,
         monkey_chaos_init/1,
         monkey_chaos_check/1,
         monkey_chaos_destroy/1]).

-include("cloudi_logger.hrl").
-include("cloudi_core_i_constants.hrl").

-define(DAY_MILLISECONDS, (24 * 60 * 60 * 1000)).
-define(MONKEY_LATENCY_METHOD_DEFAULT, time_absolute).
-define(MONKEY_LATENCY_MEAN_DEFAULT, 5000). % milliseconds
-define(MONKEY_LATENCY_LOG, 5000). % milliseconds

-record(monkey_latency,
    {
        method = undefined :: undefined |
                              time_uniform | time_gaussian | time_absolute,
        value1 = undefined :: undefined | non_neg_integer(),  % milliseconds
        value2 = undefined :: undefined | pos_integer() | float(),
        result1 = undefined :: undefined | pos_integer(), % milliseconds
        pi2 = math:pi() * 2.0 :: float()
    }).

-record(monkey_chaos,
    {
        method = undefined :: undefined |
                              probability_request | probability_day,
        value1 = undefined :: undefined | float(),
        pid = undefined :: undefined | pid()
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-spec monkey_latency_format(#monkey_latency{} | system | false) ->
    list({atom(), any()}) | system | false.

monkey_latency_format(false) ->
    false;
monkey_latency_format(system) ->
    system;
monkey_latency_format(#monkey_latency{method = time_uniform,
                                      value1 = Min,
                                      value2 = Max}) ->
    true = (Min =< Max),
    [{time_uniform_min, ?LIMIT_FORMAT(Min, 0, ?TIMEOUT_MAX_ERLANG)},
     {time_uniform_max, ?LIMIT_FORMAT(Max, 1, ?TIMEOUT_MAX_ERLANG)}];
monkey_latency_format(#monkey_latency{method = time_gaussian,
                                      value1 = Mean,
                                      value2 = StdDev}) ->
    [{time_gaussian_mean, ?LIMIT_FORMAT(Mean, 0, ?TIMEOUT_MAX_ERLANG)},
     {time_gaussian_stddev, StdDev}];
monkey_latency_format(#monkey_latency{method = time_absolute,
                                      value1 = Time}) ->
    [{time_absolute, ?LIMIT_FORMAT(Time, 1, ?TIMEOUT_MAX_ERLANG)}].

-spec monkey_latency_validate(list({atom(), any()}) | system | false) ->
    {ok, #monkey_latency{} | system | false} |
    {error, {service_options_monkey_latency_invalid, any()}}.

monkey_latency_validate(false) ->
    {ok, false};
monkey_latency_validate(system) ->
    {ok, system};
monkey_latency_validate(Options) ->
    monkey_latency_validate(Options, #monkey_latency{}).

-spec monkey_latency_init(#monkey_latency{} | system) ->
    #monkey_latency{}.

monkey_latency_init(system) ->
    Options = application:get_env(cloudi_core, monkey_latency, false),
    true = (Options =/= system),
    {ok, MonkeyLatency} = monkey_latency_validate(Options),
    MonkeyLatency;
monkey_latency_init(#monkey_latency{} = MonkeyLatency) ->
    MonkeyLatency.

-spec monkey_latency_check(#monkey_latency{}) ->
    #monkey_latency{}.

monkey_latency_check(#monkey_latency{method = time_uniform,
                                     value1 = Min,
                                     value2 = Max} = MonkeyLatency) ->
    Result = random_range(Min, Max),
    sleep(Result),
    MonkeyLatency;
monkey_latency_check(#monkey_latency{method = time_gaussian,
                                     value1 = Mean,
                                     value2 = StdDev,
                                     result1 = undefined} = MonkeyLatency) ->
    {Latency1, Latency2} = cloudi_x_quickrand_normal:box_muller(Mean, StdDev),
    Result1 = erlang:max(erlang:round(Latency1), 1),
    Result2 = erlang:max(erlang:round(Latency2), 1),
    sleep(Result2),
    MonkeyLatency#monkey_latency{result1 = Result1};
monkey_latency_check(#monkey_latency{method = time_gaussian,
                                     result1 = Result1} = MonkeyLatency)
    when is_integer(Result1) ->
    sleep(Result1),
    MonkeyLatency#monkey_latency{result1 = undefined};
monkey_latency_check(#monkey_latency{method = time_absolute,
                                     value1 = Time} = MonkeyLatency) ->
    sleep(Time),
    MonkeyLatency.

-spec monkey_chaos_format(#monkey_chaos{} | system | false) ->
    list({atom(), any()}) | system | false.

monkey_chaos_format(false) ->
    false;
monkey_chaos_format(system) ->
    system;
monkey_chaos_format(#monkey_chaos{method = probability_request,
                                  value1 = Percent}) ->
    [{probability_request, Percent}];
monkey_chaos_format(#monkey_chaos{method = probability_day,
                                  value1 = Percent}) ->
    [{probability_day, Percent}].

-spec monkey_chaos_validate(list({atom(), any()}) | system | false) ->
    {ok, #monkey_chaos{} | system | false} |
    {error, {service_options_monkey_chaos_invalid, any()}}.

monkey_chaos_validate(false) ->
    {ok, false};
monkey_chaos_validate(system) ->
    {ok, system};
monkey_chaos_validate(Options) ->
    monkey_chaos_validate(Options, #monkey_chaos{}).

-spec monkey_chaos_init(#monkey_chaos{} | system) ->
    #monkey_chaos{}.

monkey_chaos_init(system) ->
    Options = application:get_env(cloudi_core, monkey_chaos, false),
    true = (Options =/= system),
    {ok, MonkeyChaos} = monkey_chaos_validate(Options),
    monkey_chaos_init(MonkeyChaos);
monkey_chaos_init(#monkey_chaos{method = probability_day} = MonkeyChaos) ->
    monkey_chaos_check(MonkeyChaos);
monkey_chaos_init(#monkey_chaos{} = MonkeyChaos) ->
    MonkeyChaos.

-spec monkey_chaos_check(#monkey_chaos{}) ->
    #monkey_chaos{}.

monkey_chaos_check(#monkey_chaos{method = probability_request,
                                 value1 = Percent} = MonkeyChaos) ->
    X = random(),
    if
        X =< Percent ->
            erlang:exit(monkey_chaos);
        true ->
            ok
    end,
    MonkeyChaos;
monkey_chaos_check(#monkey_chaos{method = probability_day,
                                 value1 = Percent,
                                 pid = undefined} = MonkeyChaos) ->
    Pid = erlang:spawn_link(fun() ->
        false = erlang:process_flag(trap_exit, true),
        monkey_chaos_pid_day(Percent)
    end),
    MonkeyChaos#monkey_chaos{pid = Pid};
monkey_chaos_check(#monkey_chaos{method = probability_day} = MonkeyChaos) ->
    MonkeyChaos.

-spec monkey_chaos_destroy(#monkey_chaos{} | false) ->
    ok.

monkey_chaos_destroy(false) ->
    ok;
monkey_chaos_destroy(#monkey_chaos{method = probability_request}) ->
    ok;
monkey_chaos_destroy(#monkey_chaos{method = probability_day,
                                   pid = Pid}) ->
    if
        is_pid(Pid) ->
            Pid ! 'monkey_chaos_destroy';
        Pid =:= undefined ->
            ok
    end,
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

monkey_latency_validate([],
                        #monkey_latency{method = Method,
                                        value1 = Value1,
                                        value2 = Value2} = MonkeyLatency) ->
    if
        (Method =:= time_uniform) andalso (Value1 == Value2) ->
            {error, {service_options_monkey_latency_invalid,
                     time_absolute}};
        Method =:= undefined ->
            {ok,
             MonkeyLatency#monkey_latency{
                method = ?MONKEY_LATENCY_METHOD_DEFAULT,
                value1 = ?MONKEY_LATENCY_MEAN_DEFAULT}};
        Method =/= undefined ->
            {ok, MonkeyLatency}
    end;
monkey_latency_validate([{time_uniform_min, Min} = Option | Options],
                        #monkey_latency{method = Method,
                                        value2 = Max} = MonkeyLatency)
    when ((Method =:= undefined) orelse (Method =:= time_uniform)),
         (is_integer(Min) andalso
          (Min >= 0) andalso (Min =< ?TIMEOUT_MAX_ERLANG)) orelse
         (Min =:= limit_min) orelse (Min =:= limit_max) ->
    NewMin = ?LIMIT_ASSIGN(Min, 0, ?TIMEOUT_MAX_ERLANG),
    NewMax = if
        Max =:= undefined ->
            NewMin;
        Max =/= undefined ->
            Max
    end,
    if
        NewMin =< NewMax ->
            monkey_latency_validate(Options,
                                    MonkeyLatency#monkey_latency{
                                        method = time_uniform,
                                        value1 = NewMin,
                                        value2 = NewMax});
        true ->
            {error, {service_options_monkey_latency_invalid, Option}}
    end;
monkey_latency_validate([{time_uniform_max, Max} = Option | Options],
                        #monkey_latency{method = Method,
                                        value1 = Min} = MonkeyLatency)
    when ((Method =:= undefined) orelse (Method =:= time_uniform)),
         (is_integer(Max) andalso
          (Max >= 1) andalso (Max =< ?TIMEOUT_MAX_ERLANG)) orelse
         (Max =:= limit_min) orelse (Max =:= limit_max) ->
    NewMax = ?LIMIT_ASSIGN(Max, 1, ?TIMEOUT_MAX_ERLANG),
    NewMin = if
        Min =:= undefined ->
            NewMax;
        Min =/= undefined ->
            Min
    end,
    if
        NewMin =< NewMax ->
            monkey_latency_validate(Options,
                                    MonkeyLatency#monkey_latency{
                                        method = time_uniform,
                                        value1 = NewMin,
                                        value2 = NewMax});
        true ->
            {error, {service_options_monkey_latency_invalid, Option}}
    end;
monkey_latency_validate([{time_gaussian_mean, Mean} | Options],
                        #monkey_latency{
                            method = Method,
                            value2 = StdDev} = MonkeyLatency)
    when ((Method =:= undefined) orelse (Method =:= time_gaussian)),
         (is_integer(Mean) andalso
          (Mean >= 0) andalso (Mean =< ?TIMEOUT_MAX_ERLANG)) orelse
         (Mean =:= limit_min) orelse (Mean =:= limit_max) ->
    NewMean = ?LIMIT_ASSIGN(Mean, 0, ?TIMEOUT_MAX_ERLANG),
    NewStdDev = if
        StdDev =:= undefined ->
            % most values are within 3-sigma,
            % so attempt to cover all values down to 0
            NewMean / 3.0;
        StdDev =/= undefined ->
            StdDev
    end,
    monkey_latency_validate(Options,
                            MonkeyLatency#monkey_latency{
                                method = time_gaussian,
                                value1 = NewMean,
                                value2 = NewStdDev});
monkey_latency_validate([{time_gaussian_stddev, StdDev} | Options],
                        #monkey_latency{method = Method,
                                        value1 = Mean} = MonkeyLatency)
    when ((Method =:= undefined) orelse (Method =:= time_gaussian)),
         is_number(StdDev) andalso (StdDev > 0.0) ->
    NewMean = if
        Mean =:= undefined ->
            ?MONKEY_LATENCY_MEAN_DEFAULT;
        Mean =/= undefined ->
            Mean
    end,
    monkey_latency_validate(Options,
                            MonkeyLatency#monkey_latency{
                                method = time_gaussian,
                                value1 = NewMean,
                                value2 = StdDev});
monkey_latency_validate([{time_absolute, Time} | Options],
                        #monkey_latency{method = Method} = MonkeyLatency)
    when ((Method =:= undefined) orelse (Method =:= time_absolute)),
         (is_integer(Time) andalso
          (Time >= 1) andalso (Time =< ?TIMEOUT_MAX_ERLANG)) orelse
         (Time =:= limit_min) orelse (Time =:= limit_max) ->
    NewTime = ?LIMIT_ASSIGN(Time, 1, ?TIMEOUT_MAX_ERLANG),
    monkey_latency_validate(Options,
                            MonkeyLatency#monkey_latency{
                                method = time_absolute,
                                value1 = NewTime});
monkey_latency_validate([Invalid | _Options],
                        _MonkeyLatency) ->
    {error, {service_options_monkey_latency_invalid, Invalid}}.

monkey_chaos_validate([],
                      #monkey_chaos{method = Method} = MonkeyChaos) ->
    if
        Method =:= undefined ->
            % die on the first service request or info message received
            {ok,
             MonkeyChaos#monkey_chaos{
                method = probability_request,
                value1 = 1.0}};
        Method =/= undefined ->
            {ok, MonkeyChaos}
    end;
monkey_chaos_validate([{probability_request, Percent} | Options],
                        #monkey_chaos{method = Method} = MonkeyChaos)
    when ((Method =:= undefined) orelse (Method =:= probability_request)),
         is_float(Percent), Percent > 0.0, Percent =< 1.0 ->
    monkey_chaos_validate(Options,
                          MonkeyChaos#monkey_chaos{
                              method = probability_request,
                              value1 = Percent});
monkey_chaos_validate([{probability_day, Percent} | Options],
                        #monkey_chaos{method = Method} = MonkeyChaos)
    when ((Method =:= undefined) orelse (Method =:= probability_day)),
         is_float(Percent), Percent > 0.0, Percent =< 1.0 ->
    monkey_chaos_validate(Options,
                          MonkeyChaos#monkey_chaos{
                              method = probability_day,
                              value1 = Percent});
monkey_chaos_validate([Invalid | _Options],
                      _MonkeyChaos) ->
    {error, {service_options_monkey_chaos_invalid, Invalid}}.

sleep(Time)
    when Time > ?TIMEOUT_MAX_ERLANG ->
    ?LOG_WARN("monkey_latency delay ~p ms", [Time]),
    sleep_loop(Time);
sleep(Time)
    when Time > ?MONKEY_LATENCY_LOG ->
    ?LOG_WARN("monkey_latency delay ~p ms", [Time]),
    receive after Time -> ok end;
sleep(Time) ->
    receive after Time -> ok end.

sleep_loop(Time)
    when Time > ?TIMEOUT_MAX_ERLANG ->
    receive after ?TIMEOUT_MAX_ERLANG -> ok end,
    sleep_loop(Time - ?TIMEOUT_MAX_ERLANG);
sleep_loop(Time) ->
    receive after Time -> ok end.

monkey_chaos_pid_day(Percent)
    when is_float(Percent) ->
    DieToday = random() =< Percent,
    Delay = if
        DieToday =:= true ->
            erlang:round(?DAY_MILLISECONDS * random());
        DieToday =:= false ->
            ?DAY_MILLISECONDS
    end,
    receive
        'monkey_chaos_destroy' ->
            ok;
        {'EXIT', _, _} ->
            ok % parent pid died
    after
        Delay ->
            if
                DieToday =:= true ->
                    erlang:exit(monkey_chaos);
                DieToday =:= false ->
                    monkey_chaos_pid_day(Percent)
            end
    end.

random_range(Min, Max) ->
    cloudi_x_quickrand:strong_uniform_range(Min, Max).

% return a floating point value between 0.0 and 1.0, inclusive
random() ->
    cloudi_x_quickrand:strong_float().

