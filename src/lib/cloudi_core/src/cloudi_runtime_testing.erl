%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Runtime Testing==
%%% Routines for live-service testing during development.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2013, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2013 Michael Truog
%%% @version 1.3.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_runtime_testing).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([monkey_latency_format/1,
         monkey_latency_validate/1,
         monkey_latency_init/1,
         monkey_latency_check/1,
         monkey_chaos_format/1,
         monkey_chaos_validate/1,
         monkey_chaos_init/1,
         monkey_chaos_check/1]).

-include("cloudi_logger.hrl").

-define(MONKEY_LATENCY_EXTREME, 5000).

-record(monkey_latency,
    {
        method :: time_uniform | time_gaussian | time_absolute,
        value1 :: pos_integer(),  % milliseconds
        value2 :: pos_integer() | float(),
        result1 :: pos_integer(), % milliseconds
        pi2 = math:pi() * 2.0 :: float()
    }).

-record(monkey_chaos,
    {
        method :: probability_request | probability_day,
        value1 :: float(),
        pid :: pid()
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
    [{time_uniform_min, Min},
     {time_uniform_max, Max}];
monkey_latency_format(#monkey_latency{method = time_gaussian,
                                      value1 = Mean,
                                      value2 = StdDev}) ->
    [{time_gaussian_mean, Mean},
     {time_gaussian_stddev, StdDev}];
monkey_latency_format(#monkey_latency{method = time_absolute,
                                      value1 = Time}) ->
    [{time_absolute, Time}].

-spec monkey_latency_validate(list({atom(), any()}) | system | false) ->
    {ok, #monkey_latency{} | system | false} |
    {error, any()}.

monkey_latency_validate(false) ->
    {ok, false};
monkey_latency_validate(system) ->
    {ok, system};
monkey_latency_validate(Options) ->
    monkey_latency_validate(Options, #monkey_latency{}).

-spec monkey_latency_init(#monkey_latency{} | system | false) ->
    #monkey_latency{} | false.

monkey_latency_init(false) ->
    false;
monkey_latency_init(system) ->
    Options = application:get_env(cloudi_core, monkey_latency, false),
    true = (Options =/= system),
    {ok, MonkeyLatency} = monkey_latency_validate(Options),
    MonkeyLatency;
monkey_latency_init(#monkey_latency{} = MonkeyLatency) ->
    MonkeyLatency.

-spec monkey_latency_check(#monkey_latency{} | false) ->
    #monkey_latency{} | false.

monkey_latency_check(false) ->
    false;
monkey_latency_check(#monkey_latency{method = time_uniform,
                                     value1 = Min,
                                     value2 = Max} = MonkeyLatency) ->
    Result = crypto:rand_uniform(Min, Max + 1),
    sleep(Result),
    MonkeyLatency;
monkey_latency_check(#monkey_latency{method = time_gaussian,
                                     value1 = Mean,
                                     value2 = StdDev,
                                     result1 = undefined,
                                     pi2 = PI2} = MonkeyLatency) ->
    % use Box-Muller transformation to generate Gaussian noise
    % (G. E. P. Box and Mervin E. Muller,
    %  A Note on the Generation of Random Normal Deviates,
    %  The Annals of Mathematical Statistics (1958),
    %  Vol. 29, No. 2 pp. 610–611)
    X1 = random(),
    X2 = PI2 * random(),
    K = StdDev * math:sqrt(-2.0 * math:log(X1)),
    Result1 = erlang:max(erlang:round(Mean + K * math:cos(X2)), 1),
    Result2 = erlang:max(erlang:round(Mean + K * math:sin(X2)), 1),
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
    {error, any()}.

monkey_chaos_validate(false) ->
    {ok, false};
monkey_chaos_validate(system) ->
    {ok, system};
monkey_chaos_validate(Options) ->
    monkey_chaos_validate(Options, #monkey_chaos{}).

-spec monkey_chaos_init(#monkey_chaos{} | system | false) ->
    #monkey_chaos{} | false.

monkey_chaos_init(false) ->
    false;
monkey_chaos_init(system) ->
    Options = application:get_env(cloudi_core, monkey_chaos, false),
    true = (Options =/= system),
    {ok, MonkeyChaos} = monkey_chaos_validate(Options),
    monkey_chaos_check(MonkeyChaos);
monkey_chaos_init(#monkey_chaos{} = MonkeyChaos) ->
    monkey_chaos_check(MonkeyChaos).

-spec monkey_chaos_check(#monkey_chaos{} | false) ->
    #monkey_chaos{} | false.

monkey_chaos_check(false) ->
    false;
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
        erlang:process_flag(trap_exit, true),
        monkey_chaos_pid_day(Percent)
    end),
    MonkeyChaos#monkey_chaos{pid = Pid};
monkey_chaos_check(#monkey_chaos{method = probability_day} = MonkeyChaos) ->
    MonkeyChaos.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

monkey_latency_validate([],
                        #monkey_latency{method = Method} = MonkeyLatency) ->
    if
        Method =:= undefined ->
            {ok,
             MonkeyLatency#monkey_latency{
                method = time_absolute,
                value1 = ?MONKEY_LATENCY_EXTREME}};
        Method =/= undefined ->
            {ok, MonkeyLatency}
    end;
monkey_latency_validate([{time_uniform_min, Min} | Options],
                        #monkey_latency{method = Method,
                                        value2 = Max} = MonkeyLatency)
    when ((Method =:= undefined) orelse (Method =:= time_uniform)),
         is_integer(Min), Min > 0 ->
    NewMax = if
        Max =:= undefined ->
            Min;
        Max =/= undefined ->
            Max
    end,
    monkey_latency_validate(Options,
                            MonkeyLatency#monkey_latency{
                                method = time_uniform,
                                value1 = Min,
                                value2 = erlang:max(NewMax, Min)});
monkey_latency_validate([{time_uniform_max, Max} | Options],
                        #monkey_latency{method = Method,
                                        value1 = Min} = MonkeyLatency)
    when ((Method =:= undefined) orelse (Method =:= time_uniform)),
         is_integer(Max), Max > 0 ->
    NewMin = if
        Min =:= undefined ->
            Max;
        Min =/= undefined ->
            Min
    end,
    monkey_latency_validate(Options,
                            MonkeyLatency#monkey_latency{
                                method = time_uniform,
                                value1 = erlang:min(NewMin, Max),
                                value2 = Max});
monkey_latency_validate([{time_gaussian_mean, Mean} | Options],
                        #monkey_latency{
                            method = Method,
                            value2 = StdDev} = MonkeyLatency)
    when ((Method =:= undefined) orelse (Method =:= time_gaussian)),
         is_integer(Mean), Mean > 0 ->
    NewStdDev = if
        StdDev =:= undefined ->
            % most values are within 3-sigma,
            % so attempt to cover all values down to 0
            Mean / 3.0;
        StdDev =/= undefined ->
            StdDev
    end,
    monkey_latency_validate(Options,
                            MonkeyLatency#monkey_latency{
                                method = time_gaussian,
                                value1 = Mean,
                                value2 = NewStdDev});
monkey_latency_validate([{time_gaussian_stddev, StdDev} | Options],
                        #monkey_latency{method = Method,
                                        value1 = Mean} = MonkeyLatency)
    when ((Method =:= undefined) orelse (Method =:= time_gaussian)),
         is_number(StdDev), StdDev > 0.0 ->
    NewMean = if
        Mean =:= undefined ->
            ?MONKEY_LATENCY_EXTREME;
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
         is_integer(Time), Time > 0 ->
    monkey_latency_validate(Options,
                            MonkeyLatency#monkey_latency{
                                method = time_absolute,
                                value1 = Time});
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
    when Time > ?MONKEY_LATENCY_EXTREME ->
    ?LOG_WARN("monkey_latency delay ~p ms", [Time]),
    receive after Time -> ok end;
sleep(Time) ->
    receive after Time -> ok end.

monkey_chaos_pid_day(Percent)
    when is_float(Percent) ->
    DayMilliseconds = 24 * 60 * 60 * 1000,
    X = random(),
    if
        X =< Percent ->
            % kill the service during a day, after?
            Delay = erlang:round(DayMilliseconds * random()),
            receive
                {'EXIT', _, _} ->
                    ok % parent pid died
            after
                Delay ->
                    erlang:exit(monkey_chaos)
            end;
        true ->
            receive
                {'EXIT', _, _} ->
                    ok % parent pid died
            after
                DayMilliseconds ->
                    monkey_chaos_pid_day(Percent)
            end
    end.

% return a floating point value between 0.0 and 1.0, inclusive
random() ->
    quickrand:strong_float().

