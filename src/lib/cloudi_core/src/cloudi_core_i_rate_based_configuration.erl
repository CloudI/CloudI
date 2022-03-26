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
%%% MIT License
%%%
%%% Copyright (c) 2013-2022 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2013-2022 Michael Truog
%%% @version 2.0.5 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_core_i_rate_based_configuration).
-author('mjtruog at protonmail dot com').

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

% macros used to simplify source code in this file

-define(RESTART_DELAY_EXPONENTIAL_TIME_MIN_MIN, 1).
-define(RESTART_DELAY_EXPONENTIAL_TIME_MIN_MAX, ?TIMEOUT_MAX_ERLANG).
-define(RESTART_DELAY_EXPONENTIAL_TIME_MAX_MIN, 1).
-define(RESTART_DELAY_EXPONENTIAL_TIME_MAX_MAX, ?TIMEOUT_MAX_ERLANG).
-define(RESTART_DELAY_LINEAR_TIME_MIN_MIN, 0).
-define(RESTART_DELAY_LINEAR_TIME_MIN_MAX, ?TIMEOUT_MAX_ERLANG).
-define(RESTART_DELAY_LINEAR_TIME_MAX_MIN, 1).
-define(RESTART_DELAY_LINEAR_TIME_MAX_MAX, ?TIMEOUT_MAX_ERLANG).
-define(RESTART_DELAY_ABSOLUTE_TIME_MIN, 1).
-define(RESTART_DELAY_ABSOLUTE_TIME_MAX, ?TIMEOUT_MAX_ERLANG).

-define(RESTART_DELAY_EXPONENTIAL_TIME_MIN_ASSIGN(TimeMin),
        ?LIMIT_ASSIGN_MILLISECONDS(TimeMin,
                                   ?RESTART_DELAY_EXPONENTIAL_TIME_MIN_MIN,
                                   ?RESTART_DELAY_EXPONENTIAL_TIME_MIN_MAX)).
-define(RESTART_DELAY_EXPONENTIAL_TIME_MIN_FORMAT(TimeMin),
        ?LIMIT_FORMAT_MILLISECONDS(TimeMin,
                                   ?RESTART_DELAY_EXPONENTIAL_TIME_MIN_MIN,
                                   ?RESTART_DELAY_EXPONENTIAL_TIME_MIN_MAX)).
-define(RESTART_DELAY_EXPONENTIAL_TIME_MIN_GUARD(TimeMin),
        ?LIMIT_GUARD_MILLISECONDS(TimeMin,
                                  ?RESTART_DELAY_EXPONENTIAL_TIME_MIN_MIN,
                                  ?RESTART_DELAY_EXPONENTIAL_TIME_MIN_MAX)).

-define(RESTART_DELAY_EXPONENTIAL_TIME_MAX_ASSIGN(TimeMax),
        ?LIMIT_ASSIGN_MILLISECONDS(TimeMax,
                                   ?RESTART_DELAY_EXPONENTIAL_TIME_MAX_MIN,
                                   ?RESTART_DELAY_EXPONENTIAL_TIME_MAX_MAX)).
-define(RESTART_DELAY_EXPONENTIAL_TIME_MAX_FORMAT(TimeMax),
        ?LIMIT_FORMAT_MILLISECONDS(TimeMax,
                                   ?RESTART_DELAY_EXPONENTIAL_TIME_MAX_MIN,
                                   ?RESTART_DELAY_EXPONENTIAL_TIME_MAX_MAX)).
-define(RESTART_DELAY_EXPONENTIAL_TIME_MAX_GUARD(TimeMax),
        ?LIMIT_GUARD_MILLISECONDS(TimeMax,
                                  ?RESTART_DELAY_EXPONENTIAL_TIME_MAX_MIN,
                                  ?RESTART_DELAY_EXPONENTIAL_TIME_MAX_MAX)).

-define(RESTART_DELAY_LINEAR_TIME_MIN_ASSIGN(TimeMin),
        ?LIMIT_ASSIGN_MILLISECONDS(TimeMin,
                                   ?RESTART_DELAY_LINEAR_TIME_MIN_MIN,
                                   ?RESTART_DELAY_LINEAR_TIME_MIN_MAX)).
-define(RESTART_DELAY_LINEAR_TIME_MIN_FORMAT(TimeMin),
        ?LIMIT_FORMAT_MILLISECONDS(TimeMin,
                                   ?RESTART_DELAY_LINEAR_TIME_MIN_MIN,
                                   ?RESTART_DELAY_LINEAR_TIME_MIN_MAX)).
-define(RESTART_DELAY_LINEAR_TIME_MIN_GUARD(TimeMin),
        ?LIMIT_GUARD_MILLISECONDS(TimeMin,
                                  ?RESTART_DELAY_LINEAR_TIME_MIN_MIN,
                                  ?RESTART_DELAY_LINEAR_TIME_MIN_MAX)).

-define(RESTART_DELAY_LINEAR_TIME_MAX_ASSIGN(TimeMax),
        ?LIMIT_ASSIGN_MILLISECONDS(TimeMax,
                                   ?RESTART_DELAY_LINEAR_TIME_MAX_MIN,
                                   ?RESTART_DELAY_LINEAR_TIME_MAX_MAX)).
-define(RESTART_DELAY_LINEAR_TIME_MAX_FORMAT(TimeMax),
        ?LIMIT_FORMAT_MILLISECONDS(TimeMax,
                                   ?RESTART_DELAY_LINEAR_TIME_MAX_MIN,
                                   ?RESTART_DELAY_LINEAR_TIME_MAX_MAX)).
-define(RESTART_DELAY_LINEAR_TIME_MAX_GUARD(TimeMax),
        ?LIMIT_GUARD_MILLISECONDS(TimeMax,
                                  ?RESTART_DELAY_LINEAR_TIME_MAX_MIN,
                                  ?RESTART_DELAY_LINEAR_TIME_MAX_MAX)).

-define(RESTART_DELAY_ABSOLUTE_TIME_ASSIGN(TimeValue),
        ?LIMIT_ASSIGN_MILLISECONDS(TimeValue,
                                   ?RESTART_DELAY_ABSOLUTE_TIME_MIN,
                                   ?RESTART_DELAY_ABSOLUTE_TIME_MAX)).
-define(RESTART_DELAY_ABSOLUTE_TIME_FORMAT(TimeValue),
        ?LIMIT_FORMAT_MILLISECONDS(TimeValue,
                                   ?RESTART_DELAY_ABSOLUTE_TIME_MIN,
                                   ?RESTART_DELAY_ABSOLUTE_TIME_MAX)).
-define(RESTART_DELAY_ABSOLUTE_TIME_GUARD(TimeValue),
        ?LIMIT_GUARD_MILLISECONDS(TimeValue,
                                  ?RESTART_DELAY_ABSOLUTE_TIME_MIN,
                                  ?RESTART_DELAY_ABSOLUTE_TIME_MAX)).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%% convert internal state to the configuration format

-spec hibernate_format(#hibernate{} | boolean()) ->
    cloudi_service_api:service_options_internal_hibernate_options() | boolean().

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

-spec hibernate_validate(list({atom(), any()}) | boolean()) ->
    {ok, #hibernate{} | boolean()} |
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
                            hibernate = ValueOld} = State) ->
    RateCurrent = Count / Period,
    erlang:send_after(Period * 1000, self(),
                      'cloudi_hibernate_rate'),
    Value = (RateCurrent < RateMin),
    if
        Value /= ValueOld ->
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
    cloudi_service_api:service_options_restart_delay_options() | false.

restart_delay_format(false) ->
    false;
restart_delay_format(#restart_delay{method = exponential,
                                    time_min = TimeMin,
                                    time_max = TimeMax}) ->
    [{time_exponential_min,
      ?RESTART_DELAY_EXPONENTIAL_TIME_MIN_FORMAT(TimeMin)},
     {time_exponential_max,
      ?RESTART_DELAY_EXPONENTIAL_TIME_MAX_FORMAT(TimeMax)}];
restart_delay_format(#restart_delay{method = linear,
                                    time_min = TimeMin,
                                    time_max = TimeMax,
                                    time_slope = TimeSlope}) ->
    [{time_linear_min, ?RESTART_DELAY_LINEAR_TIME_MIN_FORMAT(TimeMin)},
     {time_linear_slope, TimeSlope},
     {time_linear_max, ?RESTART_DELAY_LINEAR_TIME_MAX_FORMAT(TimeMax)}];
restart_delay_format(#restart_delay{method = absolute,
                                    time_min = TimeValue,
                                    time_max = TimeValue}) ->
    [{time_absolute, ?RESTART_DELAY_ABSOLUTE_TIME_FORMAT(TimeValue)}].

%% convert the configuration format to internal state

-spec restart_delay_validate(list({atom(), any()}) | false) ->
    {ok, #restart_delay{} | false} |
    {error, {service_options_restart_delay_invalid, any()}}.

restart_delay_validate(false) ->
    {ok, false};
restart_delay_validate(Options) ->
    restart_delay_validate(Options, #restart_delay{}).

%% provide the value result

-spec restart_delay_value(RestartTimes ::
                              list(cloudi_timestamp:seconds_monotonic()),
                          MaxT :: non_neg_integer(),
                          State :: #restart_delay{} | false) ->
    false |
    {RestartCountNew :: non_neg_integer(),
     RestartTimesNew :: list(cloudi_timestamp:seconds_monotonic()),
     Value :: 0..?TIMEOUT_MAX_ERLANG}.

restart_delay_value(_, _, false) ->
    false;
restart_delay_value(RestartTimes, MaxT,
                    #restart_delay{} = State) ->
    SecondsNow = cloudi_timestamp:seconds_monotonic(),
    {RestartCountNew,
     RestartTimesNew} = cloudi_timestamp:seconds_filter_monotonic(RestartTimes,
                                                                  SecondsNow,
                                                                  MaxT),
    Value = restart_delay_value_now(RestartCountNew, State),
    {RestartCountNew, RestartTimesNew, Value}.

%% convert internal state to the configuration format

-spec count_process_dynamic_format(#count_process_dynamic{} | false) ->
    cloudi_service_api:service_options_count_process_dynamic_options() | false.

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
            process_increase(Dispatcher, Period, RateCurrent,
                             RateMax, CountProcessMax);
        RateCurrent < RateMin ->
            cloudi_core_i_services_monitor:
            process_decrease(Dispatcher, Period, RateCurrent,
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
    cloudi_service_api:service_options_rate_request_max_options() | undefined.

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
    CountNew = Count + 1,
    BlockingNew = (CountNew / Period) > RateMax,
    StateNew = if
        BlockingNew =:= true ->
            State#rate_request{blocking = true};
        BlockingNew =:= false ->
            State#rate_request{count = CountNew}
    end,
    {not BlockingNew, StateNew}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

hibernate_validate([],
                   #hibernate{method = Method,
                              period = Period,
                              rate_min = RateMin} = State) ->
    MethodNew = if
        Method =:= undefined ->
            ?HIBERNATE_METHOD_DEFAULT;
        Method =/= undefined ->
            Method
    end,
    PeriodNew = if
        Period =:= undefined ->
            ?HIBERNATE_PERIOD_DEFAULT;
        Period =/= undefined ->
            Period
    end,
    RateMinNew = if
        RateMin =:= undefined ->
            ?HIBERNATE_RATE_REQUEST_MIN_DEFAULT;
        RateMin =/= undefined ->
            RateMin
    end,
    {ok, State#hibernate{method = MethodNew,
                         period = PeriodNew,
                         rate_min = RateMinNew}};
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
    MethodNew = if
        Method =:= undefined ->
            ?RESTART_DELAY_METHOD_DEFAULT;
        Method =/= undefined ->
            Method
    end,
    TimeMinNew = if
        TimeMin =:= undefined ->
            if
                MethodNew =:= exponential ->
                    ?RESTART_DELAY_EXPONENTIAL_TIME_MIN_DEFAULT;
                MethodNew =:= linear ->
                    ?RESTART_DELAY_LINEAR_TIME_MIN_DEFAULT
            end;
        TimeMin =/= undefined ->
            TimeMin
    end,
    TimeMaxNew = if
        TimeMax =:= undefined ->
            if
                MethodNew =:= exponential ->
                    ?RESTART_DELAY_EXPONENTIAL_TIME_MAX_DEFAULT;
                MethodNew =:= linear ->
                    ?RESTART_DELAY_LINEAR_TIME_MAX_DEFAULT
            end;
        TimeMax =/= undefined ->
            TimeMax
    end,
    TimeSlopeNew = if
        TimeSlope =:= undefined, MethodNew =:= linear ->
            ?RESTART_DELAY_LINEAR_TIME_SLOPE_DEFAULT;
        true ->
            TimeSlope
    end,
    if
        ((MethodNew =:= exponential) orelse (MethodNew =:= linear)) andalso
        (TimeMinNew == TimeMaxNew) ->
            {error, {service_options_restart_delay_invalid,
                     time_absolute}};
        true ->
            {ok, State#restart_delay{method = MethodNew,
                                     time_min = TimeMinNew,
                                     time_max = TimeMaxNew,
                                     time_slope = TimeSlopeNew}}
    end;
restart_delay_validate([{time_exponential_min, TimeMin} = Option | Options],
                       #restart_delay{method = Method,
                                      time_max = TimeMax} = State)
    when (Method =:= undefined) orelse (Method =:= exponential),
         ?RESTART_DELAY_EXPONENTIAL_TIME_MIN_GUARD(TimeMin) ->
    TimeMinNew = ?RESTART_DELAY_EXPONENTIAL_TIME_MIN_ASSIGN(TimeMin),
    TimeMaxNew = if
        TimeMax =:= undefined ->
            erlang:max(TimeMinNew, ?RESTART_DELAY_EXPONENTIAL_TIME_MAX_DEFAULT);
        TimeMax =/= undefined ->
            TimeMax
    end,
    if
        TimeMinNew =< TimeMaxNew ->
            restart_delay_validate(Options,
                                   State#restart_delay{
                                       method = exponential,
                                       time_min = TimeMinNew,
                                       time_max = TimeMaxNew});
        true ->
            {error, {service_options_restart_delay_invalid, Option}}
    end;
restart_delay_validate([{time_exponential_max, TimeMax} = Option | Options],
                       #restart_delay{method = Method,
                                      time_min = TimeMin} = State)
    when (Method =:= undefined) orelse (Method =:= exponential),
         ?RESTART_DELAY_EXPONENTIAL_TIME_MAX_GUARD(TimeMax) ->
    TimeMaxNew = ?RESTART_DELAY_EXPONENTIAL_TIME_MAX_ASSIGN(TimeMax),
    TimeMinNew = if
        TimeMin =:= undefined ->
            erlang:min(TimeMaxNew, ?RESTART_DELAY_EXPONENTIAL_TIME_MIN_DEFAULT);
        TimeMin =/= undefined ->
            TimeMin
    end,
    if
        TimeMinNew =< TimeMaxNew ->
            restart_delay_validate(Options,
                                   State#restart_delay{
                                       method = exponential,
                                       time_max = TimeMaxNew,
                                       time_min = TimeMinNew});
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
    when (Method =:= undefined) orelse (Method =:= linear),
         ?RESTART_DELAY_LINEAR_TIME_MIN_GUARD(TimeMin) ->
    TimeMinNew = ?RESTART_DELAY_LINEAR_TIME_MIN_ASSIGN(TimeMin),
    TimeMaxNew = if
        TimeMax =:= undefined ->
            erlang:max(TimeMinNew, ?RESTART_DELAY_LINEAR_TIME_MAX_DEFAULT);
        TimeMax =/= undefined ->
            TimeMax
    end,
    if
        TimeMinNew =< TimeMaxNew ->
            restart_delay_validate(Options,
                                   State#restart_delay{
                                       method = linear,
                                       time_min = TimeMinNew,
                                       time_max = TimeMaxNew});
        true ->
            {error, {service_options_restart_delay_invalid, Option}}
    end;
restart_delay_validate([{time_linear_max, TimeMax} = Option | Options],
                       #restart_delay{method = Method,
                                      time_min = TimeMin} = State)
    when (Method =:= undefined) orelse (Method =:= linear),
         ?RESTART_DELAY_LINEAR_TIME_MAX_GUARD(TimeMax) ->
    TimeMaxNew = ?RESTART_DELAY_LINEAR_TIME_MAX_ASSIGN(TimeMax),
    TimeMinNew = if
        TimeMin =:= undefined ->
            erlang:min(TimeMaxNew, ?RESTART_DELAY_LINEAR_TIME_MIN_DEFAULT);
        TimeMin =/= undefined ->
            TimeMin
    end,
    if
        TimeMinNew =< TimeMaxNew ->
            restart_delay_validate(Options,
                                   State#restart_delay{
                                       method = linear,
                                       time_max = TimeMaxNew,
                                       time_min = TimeMinNew});
        true ->
            {error, {service_options_restart_delay_invalid, Option}}
    end;
restart_delay_validate([{time_absolute, TimeValue} | Options],
                       #restart_delay{method = Method} = State)
    when (Method =:= undefined) orelse (Method =:= absolute),
         ?RESTART_DELAY_ABSOLUTE_TIME_GUARD(TimeValue) ->
    TimeValueNew = ?RESTART_DELAY_ABSOLUTE_TIME_ASSIGN(TimeValue),
    restart_delay_validate(Options,
                           State#restart_delay{method = absolute,
                                               time_min = TimeValueNew,
                                               time_max = TimeValueNew});
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
    MethodNew = if
        Method =:= undefined ->
            ?COUNT_PROCESS_DYNAMIC_METHOD_DEFAULT;
        Method =/= undefined ->
            Method
    end,
    PeriodNew = if
        Period =:= undefined ->
            ?COUNT_PROCESS_DYNAMIC_PERIOD_DEFAULT;
        Period =/= undefined ->
            Period
    end,
    RateMaxNew = if
        RateMax =:= undefined ->
            ?COUNT_PROCESS_DYNAMIC_RATE_REQUEST_MAX_DEFAULT;
        RateMax =/= undefined ->
            RateMax
    end,
    RateMinNew = if
        RateMin =:= undefined ->
            ?COUNT_PROCESS_DYNAMIC_RATE_REQUEST_MIN_DEFAULT;
        RateMin =/= undefined ->
            RateMin
    end,
    CountMaxNew = if
        CountMax =:= undefined ->
            erlang:round(?COUNT_PROCESS_DYNAMIC_COUNT_MAX_DEFAULT *
                         CountProcess);
        CountMax =/= undefined ->
            CountMax
    end,
    CountMinNew = if
        CountMin =:= undefined ->
            erlang:max(1,
                       erlang:round(?COUNT_PROCESS_DYNAMIC_COUNT_MIN_DEFAULT *
                                    CountProcess));
        CountMin =/= undefined ->
            CountMin
    end,
    if
        (MethodNew =:= rate_request) andalso (RateMinNew == RateMaxNew) ->
            {error, {service_options_count_process_dynamic_invalid,
                     rate_request_max}};
        (MethodNew =:= rate_request) andalso (CountMinNew == CountMaxNew) ->
            {error, {service_options_count_process_dynamic_invalid,
                     count_max}};
        true ->
            {ok,
             State#count_process_dynamic{
                 method = MethodNew,
                 period = PeriodNew,
                 rate_max = RateMaxNew,
                 rate_min = RateMinNew,
                 count_process_max = CountMaxNew,
                 count_process_min = CountMinNew}}
    end;
count_process_dynamic_validate([{rate_request_max, RateMax} = Option | Options],
                               #count_process_dynamic{
                                   method = Method,
                                   rate_min = RateMin} = State,
                               CountProcess)
    when ((Method =:= undefined) orelse (Method =:= rate_request)),
         is_number(RateMax), RateMax > 0 ->
    RateMinNew = if
        RateMin =:= undefined ->
            RateMax - ?COUNT_PROCESS_DYNAMIC_RATE_REQUEST_OFFSET_DEFAULT;
        RateMin =/= undefined ->
            RateMin
    end,
    if
        RateMinNew =< RateMax ->
            count_process_dynamic_validate(Options,
                State#count_process_dynamic{
                    method = rate_request,
                    rate_max = RateMax,
                    rate_min = RateMinNew}, CountProcess);
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
    RateMaxNew = if
        RateMax =:= undefined ->
            RateMin + ?COUNT_PROCESS_DYNAMIC_RATE_REQUEST_OFFSET_DEFAULT;
        RateMax =/= undefined ->
            RateMax
    end,
    if
        RateMin =< RateMaxNew ->
            count_process_dynamic_validate(Options,
                State#count_process_dynamic{
                    method = rate_request,
                    rate_max = RateMaxNew,
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
    CountMaxNew = if
        is_float(CountMax) ->
            erlang:round(CountMax * CountProcess);
        is_integer(CountMax) ->
            CountMax
    end,
    CountMinNew = if
        CountMin =:= undefined ->
            CountProcess;
        CountMin =/= undefined ->
            CountMin
    end,
    if
        CountMinNew =< CountMaxNew ->
            count_process_dynamic_validate(Options,
                State#count_process_dynamic{
                    count_process_max = CountMaxNew,
                    count_process_min = CountMinNew}, CountProcess);
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
    CountMinNew = if
        is_float(CountMin) ->
            erlang:max(1, erlang:round(CountMin * CountProcess));
        is_integer(CountMin) ->
            CountMin
    end,
    CountMaxNew = if
        CountMax =:= undefined ->
            CountProcess;
        CountMax =/= undefined ->
            CountMax
    end,
    if
        CountMinNew =< CountMaxNew ->
            count_process_dynamic_validate(Options,
                State#count_process_dynamic{
                    count_process_max = CountMaxNew,
                    count_process_min = CountMinNew}, CountProcess);
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
    PeriodNew = if
        Period =:= undefined ->
            ?RATE_REQUEST_PERIOD_DEFAULT;
        Period =/= undefined ->
            Period
    end,
    RateMaxNew = if
        RateMax =:= undefined ->
            ?RATE_REQUEST_MAX_DEFAULT;
        RateMax =/= undefined ->
            RateMax
    end,
    {ok, State#rate_request{period = PeriodNew,
                            rate_max = RateMaxNew}};
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

