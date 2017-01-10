%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Rate-Based Configuration==
%%% Routines for service request and termination
%%% rate-based configuration adjustments.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2013-2017, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2013-2017 Michael Truog
%%% @version 1.5.5 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_core_i_rate_based_configuration).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([hibernate_format/1,
         hibernate_validate/1,
         hibernate_init/1,
         hibernate_reinit/1,
         hibernate_request/1,
         hibernate_check/1,
         restart_delay_format/1,
         restart_delay_validate/1,
         restart_delay_value/3,
         count_process_dynamic_format/1,
         count_process_dynamic_validate/2,
         count_process_dynamic_init/1,
         count_process_dynamic_reinit/2,
         count_process_dynamic_request/1,
         count_process_dynamic_update/2,
         count_process_dynamic_terminate/1,
         count_process_dynamic_terminate_set/2,
         count_process_dynamic_terminated/1,
         rate_request_format/1,
         rate_request_validate/1,
         rate_request_init/1,
         rate_request_reinit/1,
         rate_request_request/1]).

-include("cloudi_logger.hrl").
-include("cloudi_core_i_constants.hrl").

-define(HIBERNATE_METHOD_DEFAULT, rate_request).
-define(HIBERNATE_PERIOD_DEFAULT, 5). % seconds
-define(HIBERNATE_RATE_REQUEST_MIN_DEFAULT, 1). % req/sec
-define(RESTART_DELAY_METHOD_DEFAULT, exponential).
-define(RESTART_DELAY_EXPONENTIAL_TIME_MIN_DEFAULT, 1). % milliseconds
-define(RESTART_DELAY_EXPONENTIAL_TIME_MAX_DEFAULT, 500). % milliseconds
-define(RESTART_DELAY_LINEAR_TIME_MIN_DEFAULT, 0). % milliseconds
-define(RESTART_DELAY_LINEAR_TIME_SLOPE_DEFAULT, 50). % milliseconds
-define(RESTART_DELAY_LINEAR_TIME_MAX_DEFAULT, 250). % milliseconds
-define(COUNT_PROCESS_DYNAMIC_METHOD_DEFAULT, rate_request).
-define(COUNT_PROCESS_DYNAMIC_PERIOD_DEFAULT, 5). % seconds
-define(COUNT_PROCESS_DYNAMIC_RATE_REQUEST_MAX_DEFAULT, 1000). % req/sec
-define(COUNT_PROCESS_DYNAMIC_RATE_REQUEST_MIN_DEFAULT, 100). % req/sec
-define(COUNT_PROCESS_DYNAMIC_RATE_REQUEST_OFFSET_DEFAULT, 10). % req/sec
-define(COUNT_PROCESS_DYNAMIC_COUNT_MAX_DEFAULT, 4.0). % float%/integer_abs
-define(COUNT_PROCESS_DYNAMIC_COUNT_MIN_DEFAULT, 0.5). % float%/integer_abs
-define(RATE_REQUEST_PERIOD_DEFAULT, 5). % seconds
-define(RATE_REQUEST_MAX_DEFAULT, 1000). % req/sec

-record(hibernate,
    {
        method = undefined :: undefined | rate_request,
        period = undefined :: undefined | cloudi_service_api:period_seconds(),
        count = 0 :: non_neg_integer(),
        rate_min = undefined :: undefined | number(), % per seconds
        hibernate = false :: boolean()
    }).

-record(restart_delay,
    {
        method = undefined :: undefined | exponential | linear | absolute,
        time_min = undefined
            :: undefined |
               cloudi_service_api:restart_delay_value_milliseconds(),
        time_max = undefined
            :: undefined |
               cloudi_service_api:restart_delay_value_milliseconds(),
        time_slope = undefined
            :: undefined |
               cloudi_service_api:restart_delay_value_milliseconds()
    }).

-record(count_process_dynamic,
    {
        method = undefined :: undefined | rate_request,
        period = undefined :: undefined | cloudi_service_api:period_seconds(),
        count = 0 :: non_neg_integer(),
        rate_max = undefined :: undefined | number(), % per seconds
        rate_min = undefined :: undefined | number(), % per seconds
        count_process_max = undefined :: undefined | pos_integer(),
        count_process_min = undefined :: undefined | pos_integer(),
        terminate = false :: boolean()
    }).

-record(rate_request,
    {
        period = undefined :: undefined | cloudi_service_api:period_seconds(),
        count = 0 :: non_neg_integer(),
        rate_max = undefined :: undefined | number(), % per seconds
        blocking = false :: boolean()
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%% convert internal state to the configuration format

-spec hibernate_format(#hibernate{} | true | false) ->
    list({atom(), any()}) | true | false.

hibernate_format(true) ->
    true;
hibernate_format(false) ->
    false;
hibernate_format(#hibernate{method = rate_request,
                            period = Period,
                            rate_min = RateMin}) ->
    [{period, Period},
     {rate_request_min, RateMin}].

%% convert the configuration format to internal state

-spec hibernate_validate(list({atom(), any()}) | true | false) ->
    {ok, #hibernate{} | true | false} |
    {error, {service_options_hibernate_invalid, any()}}.

hibernate_validate(true) ->
    {ok, true};
hibernate_validate(false) ->
    {ok, false};
hibernate_validate(Options) ->
    hibernate_validate(Options, #hibernate{}).

%% called by init/1

-spec hibernate_init(State :: #hibernate{}) ->
    #hibernate{}.

hibernate_init(#hibernate{method = rate_request,
                          period = Period} = State) ->
    erlang:send_after(Period * 1000, self(),
                      'cloudi_hibernate_rate'),
    State#hibernate{count = 0}.

%% called by handle_info('cloudi_hibernate_rate', ...)

-spec hibernate_reinit(State :: #hibernate{} | boolean()) ->
    {boolean(), #hibernate{} | boolean()}.

hibernate_reinit(State)
    when is_boolean(State) ->
    {State, State};
hibernate_reinit(#hibernate{method = rate_request,
                            period = Period,
                            count = Count,
                            rate_min = RateMin,
                            hibernate = OldValue} = State) ->
    RateCurrent = Count / Period,
    erlang:send_after(Period * 1000, self(),
                      'cloudi_hibernate_rate'),
    Value = (RateCurrent < RateMin),
    if
        Value /= OldValue ->
            Direction = if
                Value =:= true ->
                    "below";
                Value =:= false ->
                    "above"
            end,
            ?LOG_TRACE("hibernate: ~5s at ~p requests/second "
                       "(~s ~p requests/second)",
                       [erlang:atom_to_list(Value),
                        erlang:round(RateCurrent * 10) / 10,
                        Direction, RateMin]);
        true ->
            ok
    end,
    {Value, State#hibernate{count = 0,
                            hibernate = Value}}.

%% called when a service request is handled

-spec hibernate_request(State :: #hibernate{}) ->
    #hibernate{}.

hibernate_request(#hibernate{method = rate_request,
                             count = Count} = State) ->
    State#hibernate{count = Count + 1}.

%% called to check if the service pid needs to hibernate

-spec hibernate_check(#hibernate{} | boolean()) ->
    boolean().

hibernate_check(Value)
    when is_boolean(Value) ->
    Value;
hibernate_check(#hibernate{method = rate_request,
                           hibernate = Value}) ->
    Value.

%% convert internal state to the configuration format

-spec restart_delay_format(#restart_delay{} | false) ->
    list({atom(), any()}) | false.

restart_delay_format(false) ->
    false;
restart_delay_format(#restart_delay{method = exponential,
                                    time_min = TimeMin,
                                    time_max = TimeMax}) ->
    [{time_exponential_min, ?LIMIT_FORMAT(TimeMin, 1, ?TIMEOUT_MAX_ERLANG)},
     {time_exponential_max, ?LIMIT_FORMAT(TimeMax, 1, ?TIMEOUT_MAX_ERLANG)}];
restart_delay_format(#restart_delay{method = linear,
                                    time_min = TimeMin,
                                    time_max = TimeMax,
                                    time_slope = TimeSlope}) ->
    [{time_linear_min, ?LIMIT_FORMAT(TimeMin, 0, ?TIMEOUT_MAX_ERLANG)},
     {time_linear_slope, TimeSlope},
     {time_linear_max, ?LIMIT_FORMAT(TimeMax, 1, ?TIMEOUT_MAX_ERLANG)}];
restart_delay_format(#restart_delay{method = absolute,
                                    time_min = TimeValue,
                                    time_max = TimeValue}) ->
    [{time_absolute, ?LIMIT_FORMAT(TimeValue, 1, ?TIMEOUT_MAX_ERLANG)}].

%% convert the configuration format to internal state

-spec restart_delay_validate(list({atom(), any()}) | false) ->
    {ok, #restart_delay{} | false} |
    {error, {service_options_restart_delay_invalid, any()}}.

restart_delay_validate(false) ->
    {ok, false};
restart_delay_validate(Options) ->
    restart_delay_validate(Options, #restart_delay{}).

%% provide the value result

-spec restart_delay_value(RestartTimes :: list(non_neg_integer()),
                          MaxT :: non_neg_integer(),
                          State :: #restart_delay{} | false) ->
    false |
    {NewRestartCount :: non_neg_integer(),
     NewRestartTimes :: list(non_neg_integer()),
     Value :: 0..?TIMEOUT_MAX_ERLANG}.

restart_delay_value(_, _, false) ->
    false;
restart_delay_value(RestartTimes, MaxT,
                    #restart_delay{} = State) ->
    SecondsNow = cloudi_timestamp:seconds(),
    {NewRestartCount,
     NewRestartTimes} = cloudi_timestamp:seconds_filter(RestartTimes,
                                                        SecondsNow, MaxT),
    Value = restart_delay_value_now(NewRestartCount, State),
    {NewRestartCount, NewRestartTimes, Value}.

%% convert internal state to the configuration format

-spec count_process_dynamic_format(#count_process_dynamic{} | false) ->
    list({atom(), any()}) | false.

count_process_dynamic_format(false) ->
    false;
count_process_dynamic_format(#count_process_dynamic{
                                 method = rate_request,
                                 period = Period,
                                 rate_max = RateMax,
                                 rate_min = RateMin,
                                 count_process_max = CountProcessMax,
                                 count_process_min = CountProcessMin}) ->
    [{period, Period},
     {rate_request_max, RateMax},
     {rate_request_min, RateMin},
     {count_max, CountProcessMax},
     {count_min, CountProcessMin}].

%% convert the configuration format to internal state

-spec count_process_dynamic_validate(Options :: list({atom(), any()}) | false,
                                     CountProcess :: number() | undefined) ->
    {ok, #count_process_dynamic{} | false} |
    {error, {service_options_count_process_dynamic_invalid, any()}}.

count_process_dynamic_validate(false, undefined) ->
    {ok, false};
count_process_dynamic_validate(false, _) ->
    {ok, false};
count_process_dynamic_validate(Options, CountProcess)
    when is_number(CountProcess) ->
    CountProcessInteger = cloudi_concurrency:count(CountProcess),
    count_process_dynamic_validate(Options,
                                   #count_process_dynamic{},
                                   CountProcessInteger).

%% called by init/1

-spec count_process_dynamic_init(State :: #count_process_dynamic{}) ->
    #count_process_dynamic{}.

count_process_dynamic_init(#count_process_dynamic{
                               method = rate_request,
                               period = Period} = State) ->
    erlang:send_after(Period * 1000, self(),
                      'cloudi_count_process_dynamic_rate'),
    State#count_process_dynamic{count = 0}.

%% called by handle_info('cloudi_count_process_dynamic_rate', ...)

-spec count_process_dynamic_reinit(Dispatcher :: pid(),
                                   State :: #count_process_dynamic{}) ->
    #count_process_dynamic{}.

count_process_dynamic_reinit(Dispatcher,
                             #count_process_dynamic{
                                 method = rate_request,
                                 period = Period,
                                 count = Count,
                                 rate_max = RateMax,
                                 rate_min = RateMin,
                                 count_process_max = CountProcessMax,
                                 count_process_min = CountProcessMin} = State)
    when is_pid(Dispatcher) ->
    RateCurrent = Count / Period,
    if
        RateCurrent > RateMax ->
            cloudi_core_i_services_monitor:
            increase(Dispatcher, Period, RateCurrent,
                     RateMax, CountProcessMax);
        RateCurrent < RateMin ->
            cloudi_core_i_services_monitor:
            decrease(Dispatcher, Period, RateCurrent,
                     RateMin, CountProcessMin);
        true ->
            ok
    end,
    erlang:send_after(Period * 1000, self(),
                      'cloudi_count_process_dynamic_rate'),
    State#count_process_dynamic{count = 0}.

%% called when a service request is handled

-spec count_process_dynamic_request(State :: #count_process_dynamic{}) ->
    #count_process_dynamic{}.

count_process_dynamic_request(#count_process_dynamic{
                                  method = rate_request,
                                  count = Count} = State) ->
    State#count_process_dynamic{count = Count + 1}.

%% update the service pid's count_process

-spec count_process_dynamic_update(Pid :: pid(),
                                   CountProcess :: pos_integer()) ->
    ok.

count_process_dynamic_update(Pid, CountProcess)
    when is_pid(Pid), is_integer(CountProcess) ->
    Pid ! {'cloudi_count_process_dynamic_update', CountProcess},
    ok.

%% get a service pid termination started

-spec count_process_dynamic_terminate(Pid :: pid()) ->
    ok.

count_process_dynamic_terminate(Pid)
    when is_pid(Pid) ->
    Pid ! 'cloudi_count_process_dynamic_terminate',
    ok.

%% set the service pid termination state

-spec count_process_dynamic_terminate_set(ReceiverPid :: pid(),
                                          State :: #count_process_dynamic{}) ->
    #count_process_dynamic{}.

count_process_dynamic_terminate_set(ReceiverPid,
                                    #count_process_dynamic{
                                        method = rate_request,
                                        period = Period} = State)
    when is_pid(ReceiverPid) ->
    ReceiverPid ! 'cloudi_count_process_dynamic_terminate_check',
    erlang:send_after(Period * 1000, ReceiverPid,
                      'cloudi_count_process_dynamic_terminate_now'),
    State#count_process_dynamic{terminate = true}.

%% check the service pid termination state

-spec count_process_dynamic_terminated(State :: #count_process_dynamic{} |
                                                false) ->
    boolean().

count_process_dynamic_terminated(false) ->
    false;
count_process_dynamic_terminated(#count_process_dynamic{
                                     method = rate_request,
                                     terminate = Value}) ->
    Value.

%% convert internal state to the configuration format

-spec rate_request_format(#rate_request{} | undefined) ->
    list({atom(), any()}) | undefined.

rate_request_format(undefined) ->
    undefined;
rate_request_format(#rate_request{period = Period,
                                  rate_max = RateMax}) ->
    [{period, Period},
     {value, RateMax}].

%% convert the configuration format to internal state

-spec rate_request_validate(list({atom(), any()}) | number() | undefined) ->
    {ok, #rate_request{} | undefined} |
    {error, {service_options_rate_request_max_invalid, any()}}.

rate_request_validate(undefined) ->
    {ok, undefined};
rate_request_validate(Value)
    when is_number(Value) ->
    rate_request_validate([{value, Value}], #rate_request{});
rate_request_validate(Options) ->
    rate_request_validate(Options, #rate_request{}).

%% called by init/1

-spec rate_request_init(State :: #rate_request{}) ->
    #rate_request{}.

rate_request_init(#rate_request{period = Period} = State) ->
    erlang:send_after(Period * 1000, self(),
                      'cloudi_rate_request_max_rate'),
    State#rate_request{count = 0,
                       blocking = false}.

%% called by handle_info('cloudi_rate_request_max_rate', ...)

-spec rate_request_reinit(State :: #rate_request{} | undefined) ->
    #rate_request{}.

rate_request_reinit(undefined) ->
    undefined;
rate_request_reinit(#rate_request{period = Period,
                                  count = Count,
                                  blocking = Blocking} = State) ->
    erlang:send_after(Period * 1000, self(),
                      'cloudi_rate_request_max_rate'),
    if
        Blocking =:= true ->
            RateCurrent = Count / Period,
            ?LOG_TRACE("rate_request_max: exceeded at ~p requests/second",
                       [erlang:round(RateCurrent * 10) / 10]);
        Blocking =:= false ->
            ok
    end,
    State#rate_request{count = 0,
                       blocking = false}.

%% called when a service request is handled

-spec rate_request_request(State :: #rate_request{}) ->
    {boolean(), #rate_request{}}.

rate_request_request(#rate_request{blocking = true} = State) ->
    {false, State};
rate_request_request(#rate_request{period = Period,
                                   count = Count,
                                   rate_max = RateMax} = State) ->
    NewCount = Count + 1,
    NewBlocking = (NewCount / Period) > RateMax,
    NewState = if
        NewBlocking =:= true ->
            State#rate_request{blocking = true};
        NewBlocking =:= false ->
            State#rate_request{count = NewCount}
    end,
    {not NewBlocking, NewState}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

hibernate_validate([],
                   #hibernate{method = Method,
                              period = Period,
                              rate_min = RateMin} = State) ->
    NewMethod = if
        Method =:= undefined ->
            ?HIBERNATE_METHOD_DEFAULT;
        Method =/= undefined ->
            Method
    end,
    NewPeriod = if
        Period =:= undefined ->
            ?HIBERNATE_PERIOD_DEFAULT;
        Period =/= undefined ->
            Period
    end,
    NewRateMin = if
        RateMin =:= undefined ->
            ?HIBERNATE_RATE_REQUEST_MIN_DEFAULT;
        RateMin =/= undefined ->
            RateMin
    end,
    {ok, State#hibernate{method = NewMethod,
                         period = NewPeriod,
                         rate_min = NewRateMin}};
hibernate_validate([{rate_request_min, RateMin} | Options],
                   #hibernate{method = Method} = State)
    when ((Method =:= undefined) orelse (Method =:= rate_request)),
         is_number(RateMin), RateMin > 0 ->
    hibernate_validate(Options,
                       State#hibernate{method = rate_request,
                                       rate_min = RateMin});
hibernate_validate([{period, Period} | Options],
                   #hibernate{} = State)
    when is_integer(Period), Period > 0,
         Period =< (?TIMEOUT_MAX_ERLANG div 1000) ->
    hibernate_validate(Options,
                       State#hibernate{period = Period});
hibernate_validate([Invalid | _Options],
                   _State) ->
    {error, {service_options_hibernate_invalid, Invalid}}.

restart_delay_validate([],
                       #restart_delay{method = Method,
                                      time_min = TimeMin,
                                      time_max = TimeMax,
                                      time_slope = TimeSlope} = State) ->
    NewMethod = if
        Method =:= undefined ->
            ?RESTART_DELAY_METHOD_DEFAULT;
        Method =/= undefined ->
            Method
    end,
    NewTimeMin = if
        TimeMin =:= undefined ->
            if
                NewMethod =:= exponential ->
                    ?RESTART_DELAY_EXPONENTIAL_TIME_MIN_DEFAULT;
                NewMethod =:= linear ->
                    ?RESTART_DELAY_LINEAR_TIME_MIN_DEFAULT
            end;
        TimeMin =/= undefined ->
            TimeMin
    end,
    NewTimeMax = if
        TimeMax =:= undefined ->
            if
                NewMethod =:= exponential ->
                    ?RESTART_DELAY_EXPONENTIAL_TIME_MAX_DEFAULT;
                NewMethod =:= linear ->
                    ?RESTART_DELAY_LINEAR_TIME_MAX_DEFAULT
            end;
        TimeMax =/= undefined ->
            TimeMax
    end,
    NewTimeSlope = if
        TimeSlope =:= undefined, NewMethod =:= linear ->
            ?RESTART_DELAY_LINEAR_TIME_SLOPE_DEFAULT;
        true ->
            TimeSlope
    end,
    if
        ((NewMethod =:= exponential) orelse (NewMethod =:= linear)) andalso
        (NewTimeMin == NewTimeMax) ->
            {error, {service_options_restart_delay_invalid,
                     time_absolute}};
        true ->
            {ok, State#restart_delay{method = NewMethod,
                                     time_min = NewTimeMin,
                                     time_max = NewTimeMax,
                                     time_slope = NewTimeSlope}}
    end;
restart_delay_validate([{time_exponential_min, TimeMin} = Option | Options],
                       #restart_delay{method = Method,
                                      time_max = TimeMax} = State)
    when ((Method =:= undefined) orelse (Method =:= exponential)),
         (is_integer(TimeMin) andalso
          (TimeMin >= 1) andalso (TimeMin =< ?TIMEOUT_MAX_ERLANG)) orelse
         (TimeMin =:= limit_min) orelse (TimeMin =:= limit_max) ->
    NewTimeMin = ?LIMIT_ASSIGN(TimeMin, 1, ?TIMEOUT_MAX_ERLANG),
    NewTimeMax = if
        TimeMax =:= undefined ->
            erlang:max(NewTimeMin, ?RESTART_DELAY_EXPONENTIAL_TIME_MAX_DEFAULT);
        TimeMax =/= undefined ->
            TimeMax
    end,
    if
        NewTimeMin =< NewTimeMax ->
            restart_delay_validate(Options,
                                   State#restart_delay{
                                       method = exponential,
                                       time_min = NewTimeMin,
                                       time_max = NewTimeMax});
        true ->
            {error, {service_options_restart_delay_invalid, Option}}
    end;
restart_delay_validate([{time_exponential_max, TimeMax} = Option | Options],
                       #restart_delay{method = Method,
                                      time_min = TimeMin} = State)
    when ((Method =:= undefined) orelse (Method =:= exponential)),
         (is_integer(TimeMax) andalso
          (TimeMax >= 1) andalso (TimeMax =< ?TIMEOUT_MAX_ERLANG)) orelse
         (TimeMax =:= limit_min) orelse (TimeMax =:= limit_max) ->
    NewTimeMax = ?LIMIT_ASSIGN(TimeMax, 1, ?TIMEOUT_MAX_ERLANG),
    NewTimeMin = if
        TimeMin =:= undefined ->
            erlang:min(NewTimeMax, ?RESTART_DELAY_EXPONENTIAL_TIME_MIN_DEFAULT);
        TimeMin =/= undefined ->
            TimeMin
    end,
    if
        NewTimeMin =< NewTimeMax ->
            restart_delay_validate(Options,
                                   State#restart_delay{
                                       method = exponential,
                                       time_max = NewTimeMax,
                                       time_min = NewTimeMin});
        true ->
            {error, {service_options_restart_delay_invalid, Option}}
    end;
restart_delay_validate([{time_linear_slope, TimeSlope} | Options],
                       #restart_delay{method = Method} = State)
    when ((Method =:= undefined) orelse (Method =:= linear)),
         is_integer(TimeSlope), TimeSlope >= 1 ->
    restart_delay_validate(Options,
                           State#restart_delay{
                               method = linear,
                               time_slope = TimeSlope});
restart_delay_validate([{time_linear_min, TimeMin} = Option | Options],
                       #restart_delay{method = Method,
                                      time_max = TimeMax} = State)
    when ((Method =:= undefined) orelse (Method =:= linear)),
         (is_integer(TimeMin) andalso
          (TimeMin >= 0) andalso (TimeMin =< ?TIMEOUT_MAX_ERLANG)) orelse
         (TimeMin =:= limit_min) orelse (TimeMin =:= limit_max) ->
    NewTimeMin = ?LIMIT_ASSIGN(TimeMin, 0, ?TIMEOUT_MAX_ERLANG),
    NewTimeMax = if
        TimeMax =:= undefined ->
            erlang:max(NewTimeMin, ?RESTART_DELAY_LINEAR_TIME_MAX_DEFAULT);
        TimeMax =/= undefined ->
            TimeMax
    end,
    if
        NewTimeMin =< NewTimeMax ->
            restart_delay_validate(Options,
                                   State#restart_delay{
                                       method = linear,
                                       time_min = NewTimeMin,
                                       time_max = NewTimeMax});
        true ->
            {error, {service_options_restart_delay_invalid, Option}}
    end;
restart_delay_validate([{time_linear_max, TimeMax} = Option | Options],
                       #restart_delay{method = Method,
                                      time_min = TimeMin} = State)
    when ((Method =:= undefined) orelse (Method =:= linear)),
         (is_integer(TimeMax) andalso
          (TimeMax >= 1) andalso (TimeMax =< ?TIMEOUT_MAX_ERLANG)) orelse
         (TimeMax =:= limit_min) orelse (TimeMax =:= limit_max) ->
    NewTimeMax = ?LIMIT_ASSIGN(TimeMax, 1, ?TIMEOUT_MAX_ERLANG),
    NewTimeMin = if
        TimeMin =:= undefined ->
            erlang:min(NewTimeMax, ?RESTART_DELAY_LINEAR_TIME_MIN_DEFAULT);
        TimeMin =/= undefined ->
            TimeMin
    end,
    if
        NewTimeMin =< NewTimeMax ->
            restart_delay_validate(Options,
                                   State#restart_delay{
                                       method = linear,
                                       time_max = NewTimeMax,
                                       time_min = NewTimeMin});
        true ->
            {error, {service_options_restart_delay_invalid, Option}}
    end;
restart_delay_validate([{time_absolute, TimeValue} | Options],
                       #restart_delay{method = Method} = State)
    when ((Method =:= undefined) orelse (Method =:= absolute)),
         (is_integer(TimeValue) andalso
          (TimeValue >= 1) andalso (TimeValue =< ?TIMEOUT_MAX_ERLANG)) orelse
         (TimeValue =:= limit_min) orelse (TimeValue =:= limit_max) ->
    NewTimeValue = ?LIMIT_ASSIGN(TimeValue, 1, ?TIMEOUT_MAX_ERLANG),
    restart_delay_validate(Options,
                           State#restart_delay{method = absolute,
                                               time_min = NewTimeValue,
                                               time_max = NewTimeValue});
restart_delay_validate([Invalid | _Options],
                       _State) ->
    {error, {service_options_restart_delay_invalid, Invalid}}.

restart_delay_value_now(RestartCount,
                        #restart_delay{method = exponential,
                                       time_min = TimeMin,
                                       time_max = TimeMax}) ->
    TimeValue = (1 bsl RestartCount) * TimeMin,
    if
        TimeValue > TimeMax ->
            TimeMax;
        true ->
            TimeValue
    end;
restart_delay_value_now(RestartCount,
                        #restart_delay{method = linear,
                                       time_min = TimeMin,
                                       time_max = TimeMax,
                                       time_slope = TimeSlope}) ->
    TimeValue = (TimeSlope * RestartCount) + TimeMin,
    if
        TimeValue > TimeMax ->
            TimeMax;
        true ->
            TimeValue
    end;
restart_delay_value_now(_,
                        #restart_delay{method = absolute,
                                       time_min = TimeValue,
                                       time_max = TimeValue}) ->
    TimeValue.

count_process_dynamic_validate([],
                               #count_process_dynamic{
                                   method = Method,
                                   period = Period,
                                   rate_max = RateMax,
                                   rate_min = RateMin,
                                   count_process_max = CountMax,
                                   count_process_min = CountMin} = State,
                               CountProcess) ->
    NewMethod = if
        Method =:= undefined ->
            ?COUNT_PROCESS_DYNAMIC_METHOD_DEFAULT;
        Method =/= undefined ->
            Method
    end,
    NewPeriod = if
        Period =:= undefined ->
            ?COUNT_PROCESS_DYNAMIC_PERIOD_DEFAULT;
        Period =/= undefined ->
            Period
    end,
    NewRateMax = if
        RateMax =:= undefined ->
            ?COUNT_PROCESS_DYNAMIC_RATE_REQUEST_MAX_DEFAULT;
        RateMax =/= undefined ->
            RateMax
    end,
    NewRateMin = if
        RateMin =:= undefined ->
            ?COUNT_PROCESS_DYNAMIC_RATE_REQUEST_MIN_DEFAULT;
        RateMin =/= undefined ->
            RateMin
    end,
    NewCountMax = if
        CountMax =:= undefined ->
            erlang:round(?COUNT_PROCESS_DYNAMIC_COUNT_MAX_DEFAULT *
                         CountProcess);
        CountMax =/= undefined ->
            CountMax
    end,
    NewCountMin = if
        CountMin =:= undefined ->
            erlang:max(1,
                       erlang:round(?COUNT_PROCESS_DYNAMIC_COUNT_MIN_DEFAULT *
                                    CountProcess));
        CountMin =/= undefined ->
            CountMin
    end,
    if
        (NewMethod =:= rate_request) andalso (NewRateMin == NewRateMax) ->
            {error, {service_options_count_process_dynamic_invalid,
                     rate_request_max}};
        (NewMethod =:= rate_request) andalso (NewCountMin == NewCountMax) ->
            {error, {service_options_count_process_dynamic_invalid,
                     count_max}};
        true ->
            {ok,
             State#count_process_dynamic{
                 method = NewMethod,
                 period = NewPeriod,
                 rate_max = NewRateMax,
                 rate_min = NewRateMin,
                 count_process_max = NewCountMax,
                 count_process_min = NewCountMin}}
    end;
count_process_dynamic_validate([{rate_request_max, RateMax} = Option | Options],
                               #count_process_dynamic{
                                   method = Method,
                                   rate_min = RateMin} = State,
                               CountProcess)
    when ((Method =:= undefined) orelse (Method =:= rate_request)),
         is_number(RateMax), RateMax > 0 ->
    NewRateMin = if
        RateMin =:= undefined ->
            RateMax - ?COUNT_PROCESS_DYNAMIC_RATE_REQUEST_OFFSET_DEFAULT;
        RateMin =/= undefined ->
            RateMin
    end,
    if
        NewRateMin =< RateMax ->
            count_process_dynamic_validate(Options,
                State#count_process_dynamic{
                    method = rate_request,
                    rate_max = RateMax,
                    rate_min = NewRateMin}, CountProcess);
        true ->
            {error, {service_options_count_process_dynamic_invalid, Option}}
    end;
count_process_dynamic_validate([{rate_request_min, RateMin} = Option | Options],
                               #count_process_dynamic{
                                   method = Method,
                                   rate_max = RateMax} = State,
                               CountProcess)
    when ((Method =:= undefined) orelse (Method =:= rate_request)),
         is_number(RateMin), RateMin > 0 ->
    NewRateMax = if
        RateMax =:= undefined ->
            RateMin + ?COUNT_PROCESS_DYNAMIC_RATE_REQUEST_OFFSET_DEFAULT;
        RateMax =/= undefined ->
            RateMax
    end,
    if
        RateMin =< NewRateMax ->
            count_process_dynamic_validate(Options,
                State#count_process_dynamic{
                    method = rate_request,
                    rate_max = NewRateMax,
                    rate_min = RateMin}, CountProcess);
        true ->
            {error, {service_options_count_process_dynamic_invalid, Option}}
    end;
count_process_dynamic_validate([{count_max, CountMax} = Option | Options],
                               #count_process_dynamic{
                                   count_process_min = CountMin} = State,
                               CountProcess)
    when (is_float(CountMax) andalso
          (CountMax >= 1.0));
         (is_integer(CountMax) andalso
          (CountMax >= CountProcess)) ->
    NewCountMax = if
        is_float(CountMax) ->
            erlang:round(CountMax * CountProcess);
        is_integer(CountMax) ->
            CountMax
    end,
    NewCountMin = if
        CountMin =:= undefined ->
            CountProcess;
        CountMin =/= undefined ->
            CountMin
    end,
    if
        NewCountMin =< NewCountMax ->
            count_process_dynamic_validate(Options,
                State#count_process_dynamic{
                    count_process_max = NewCountMax,
                    count_process_min = NewCountMin}, CountProcess);
        true ->
            {error, {service_options_count_process_dynamic_invalid, Option}}
    end;
count_process_dynamic_validate([{count_min, CountMin} = Option | Options],
                               #count_process_dynamic{
                                   count_process_max = CountMax} = State,
                               CountProcess)
    when (is_float(CountMin) andalso
          (CountMin > 0) andalso (CountMin =< 1.0));
         (is_integer(CountMin) andalso
          (CountMin > 0) andalso (CountMin =< CountProcess)) ->
    NewCountMin = if
        is_float(CountMin) ->
            erlang:max(1, erlang:round(CountMin * CountProcess));
        is_integer(CountMin) ->
            CountMin
    end,
    NewCountMax = if
        CountMax =:= undefined ->
            CountProcess;
        CountMax =/= undefined ->
            CountMax
    end,
    if
        NewCountMin =< NewCountMax ->
            count_process_dynamic_validate(Options,
                State#count_process_dynamic{
                    count_process_max = NewCountMax,
                    count_process_min = NewCountMin}, CountProcess);
        true ->
            {error, {service_options_count_process_dynamic_invalid, Option}}
    end;
count_process_dynamic_validate([{period, Period} | Options],
                               #count_process_dynamic{} = State,
                               CountProcess)
    when is_integer(Period), Period > 0,
         Period =< (?TIMEOUT_MAX_ERLANG div 1000) ->
    count_process_dynamic_validate(Options,
        State#count_process_dynamic{
            period = Period}, CountProcess);
count_process_dynamic_validate([Invalid | _Options],
                               _State, _CountProcess) ->
    {error, {service_options_count_process_dynamic_invalid, Invalid}}.

rate_request_validate([],
                      #rate_request{period = Period,
                                    rate_max = RateMax} = State) ->
    NewPeriod = if
        Period =:= undefined ->
            ?RATE_REQUEST_PERIOD_DEFAULT;
        Period =/= undefined ->
            Period
    end,
    NewRateMax = if
        RateMax =:= undefined ->
            ?RATE_REQUEST_MAX_DEFAULT;
        RateMax =/= undefined ->
            RateMax
    end,
    {ok, State#rate_request{period = NewPeriod,
                            rate_max = NewRateMax}};
rate_request_validate([{value, RateMax} | Options],
                      #rate_request{} = State)
    when is_number(RateMax), RateMax > 0 ->
    rate_request_validate(Options,
                          State#rate_request{rate_max = RateMax});
rate_request_validate([{period, Period} | Options],
                      #rate_request{} = State)
    when is_integer(Period), Period > 0,
         Period =< (?TIMEOUT_MAX_ERLANG div 1000) ->
    rate_request_validate(Options,
                          State#rate_request{period = Period});
rate_request_validate([Invalid | _Options],
                      _State) ->
    {error, {service_options_rate_request_max_invalid, Invalid}}.

