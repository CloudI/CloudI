%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Rate-Based Configuration==
%%% Routines for service request rate-based configuration adjustments.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2013-2014, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2013-2014 Michael Truog
%%% @version 1.3.3 {@date} {@time}
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
         count_process_dynamic_format/1,
         count_process_dynamic_validate/2,
         count_process_dynamic_init/1,
         count_process_dynamic_reinit/2,
         count_process_dynamic_request/1,
         count_process_dynamic_update/2,
         count_process_dynamic_terminate/1,
         count_process_dynamic_terminate_set/2,
         count_process_dynamic_terminated/1]).

-include("cloudi_logger.hrl").
-include("cloudi_core_i_constants.hrl").

-define(HIBERNATE_PERIOD_DEFAULT, 5). % seconds
-define(HIBERNATE_RATE_REQUEST_MIN_DEFAULT, 1). % req/sec
-define(COUNT_PROCESS_DYNAMIC_PERIOD_DEFAULT, 5). % seconds
-define(COUNT_PROCESS_DYNAMIC_RATE_REQUEST_MAX_DEFAULT, 1000). % req/sec
-define(COUNT_PROCESS_DYNAMIC_RATE_REQUEST_MIN_DEFAULT, 100). % req/sec
-define(COUNT_PROCESS_DYNAMIC_RATE_REQUEST_OFFSET_DEFAULT, 10). % req/sec
-define(COUNT_PROCESS_DYNAMIC_COUNT_MAX_DEFAULT, 4.0). % float%/integer_abs
-define(COUNT_PROCESS_DYNAMIC_COUNT_MIN_DEFAULT, 0.5). % float%/integer_abs

-record(hibernate,
    {
        method :: rate_request,
        period :: cloudi_service_api:period_seconds(),
        count = 0 :: non_neg_integer(),
        rate_min :: number(), % per seconds
        hibernate = false :: boolean()
    }).

-record(count_process_dynamic,
    {
        method :: rate_request,
        period :: cloudi_service_api:period_seconds(),
        count = 0 :: non_neg_integer(),
        rate_max :: number(), % per seconds
        rate_min :: number(), % per seconds
        count_process_max :: pos_integer(),
        count_process_min :: pos_integer(),
        terminate = false :: boolean()
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

-spec hibernate_init(Hibernate :: #hibernate{}) ->
    #hibernate{}.

hibernate_init(#hibernate{method = rate_request,
                          period = Period} = Hibernate) ->
    erlang:send_after(Period * 1000, self(),
                      'cloudi_hibernate_rate'),
    Hibernate#hibernate{count = 0}.

%% called by handle_info('cloudi_hibernate_rate', ...)

-spec hibernate_reinit(Hibernate :: #hibernate{}) ->
    {boolean(), #hibernate{}}.

hibernate_reinit(#hibernate{method = rate_request,
                            period = Period,
                            count = Count,
                            rate_min = RateMin,
                            hibernate = OldValue} = Hibernate) ->
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
    {Value, Hibernate#hibernate{count = 0,
                                hibernate = Value}}.

%% called when a service request is handled

-spec hibernate_request(Hibernate :: #hibernate{}) ->
    #hibernate{}.

hibernate_request(#hibernate{method = rate_request,
                             count = Count} = Hibernate) ->
    Hibernate#hibernate{count = Count + 1}.

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
                                     CountProcess :: number()) ->
    {ok, #count_process_dynamic{} | false} |
    {error, {service_options_count_process_dynamic_invalid, any()}}.

count_process_dynamic_validate(false, _) ->
    {ok, false};
count_process_dynamic_validate(Options, CountProcess) ->
    CountProcessInteger = cloudi_core_i_configurator:concurrency(CountProcess),
    count_process_dynamic_validate(Options,
                                   #count_process_dynamic{},
                                   CountProcessInteger).

%% called by init/1

-spec count_process_dynamic_init(CountProcessDynamic ::
                                     #count_process_dynamic{}) ->
    #count_process_dynamic{}.

count_process_dynamic_init(#count_process_dynamic{
                               method = rate_request,
                               period = Period} = CountProcessDynamic) ->
    erlang:send_after(Period * 1000, self(),
                      'cloudi_count_process_dynamic_rate'),
    CountProcessDynamic#count_process_dynamic{count = 0}.

%% called by handle_info('cloudi_count_process_dynamic_rate', ...)

-spec count_process_dynamic_reinit(Dispatcher :: pid(),
                                   CountProcessDynamic ::
                                       #count_process_dynamic{}) ->
    #count_process_dynamic{}.

count_process_dynamic_reinit(Dispatcher,
                             #count_process_dynamic{
                                 method = rate_request,
                                 period = Period,
                                 count = Count,
                                 rate_max = RateMax,
                                 rate_min = RateMin,
                                 count_process_max = CountProcessMax,
                                 count_process_min = CountProcessMin} =
                                     CountProcessDynamic)
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
    CountProcessDynamic#count_process_dynamic{count = 0}.

%% called when a service request is handled

-spec count_process_dynamic_request(CountProcessDynamic ::
                                        #count_process_dynamic{}) ->
    #count_process_dynamic{}.

count_process_dynamic_request(#count_process_dynamic{
                                  method = rate_request,
                                  count = Count} = CountProcessDynamic) ->
    CountProcessDynamic#count_process_dynamic{count = Count + 1}.

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
                                          CountProcessDynamic ::
                                              #count_process_dynamic{}) ->
    #count_process_dynamic{}.

count_process_dynamic_terminate_set(ReceiverPid,
                                    #count_process_dynamic{
                                        method = rate_request,
                                        period = Period} =
                                            CountProcessDynamic)
    when is_pid(ReceiverPid) ->
    ReceiverPid ! 'cloudi_count_process_dynamic_terminate_check',
    erlang:send_after(Period * 1000, ReceiverPid,
                      'cloudi_count_process_dynamic_terminate_now'),
    CountProcessDynamic#count_process_dynamic{terminate = true}.

%% check the service pid termination state

-spec count_process_dynamic_terminated(CountProcessDynamic ::
                                           #count_process_dynamic{} | false) ->
    boolean().

count_process_dynamic_terminated(false) ->
    false;
count_process_dynamic_terminated(#count_process_dynamic{
                                     method = rate_request,
                                     terminate = Value}) ->
    Value.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

hibernate_validate([],
                   #hibernate{method = Method,
                              period = Period,
                              rate_min = RateMin} = Hibernate) ->
    NewMethod = if
        Method =:= undefined ->
            rate_request;
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
    {ok, Hibernate#hibernate{method = NewMethod,
                             period = NewPeriod,
                             rate_min = NewRateMin}};
hibernate_validate([{rate_request_min, RateMin} | Options],
                   #hibernate{method = Method} = Hibernate)
    when ((Method =:= undefined) orelse (Method =:= rate_request)),
         is_number(RateMin), RateMin > 0 ->
    hibernate_validate(Options,
                       Hibernate#hibernate{method = rate_request,
                                           rate_min = RateMin});
hibernate_validate([{period, Period} | Options],
                   #hibernate{} = Hibernate)
    when is_integer(Period), Period > 0,
         Period =< (?TIMEOUT_MAX_ERLANG div 1000) ->
    hibernate_validate(Options,
                       Hibernate#hibernate{period = Period});
hibernate_validate([Invalid | _Options],
                   _Hibernate) ->
    {error, {service_options_hibernate_invalid, Invalid}}.

count_process_dynamic_validate([],
                               #count_process_dynamic{
                                   method = Method,
                                   period = Period,
                                   rate_max = RateMax,
                                   rate_min = RateMin,
                                   count_process_max = CountMax,
                                   count_process_min = CountMin} =
                                       CountProcessDynamic,
                               CountProcess) ->
    NewMethod = if
        Method =:= undefined ->
            rate_request;
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
    {ok,
     CountProcessDynamic#count_process_dynamic{
         method = NewMethod,
         period = NewPeriod,
         rate_max = NewRateMax,
         rate_min = NewRateMin,
         count_process_max = NewCountMax,
         count_process_min = NewCountMin}};
count_process_dynamic_validate([{rate_request_max, RateMax} | Options],
                               #count_process_dynamic{
                                   method = Method,
                                   rate_min = RateMin} =
                                       CountProcessDynamic,
                               CountProcess)
    when ((Method =:= undefined) orelse (Method =:= rate_request)),
         is_number(RateMax), RateMax > 0 ->
    NewRateMin = if
        RateMin =:= undefined ->
            RateMax - ?COUNT_PROCESS_DYNAMIC_RATE_REQUEST_OFFSET_DEFAULT;
        RateMin =/= undefined ->
            RateMin
    end,
    count_process_dynamic_validate(Options,
        CountProcessDynamic#count_process_dynamic{
            method = rate_request,
            rate_max = RateMax,
            rate_min = erlang:min(NewRateMin, RateMax)}, CountProcess);
count_process_dynamic_validate([{rate_request_min, RateMin} | Options],
                               #count_process_dynamic{
                                   method = Method,
                                   rate_max = RateMax} =
                                       CountProcessDynamic,
                               CountProcess)
    when ((Method =:= undefined) orelse (Method =:= rate_request)),
         is_number(RateMin), RateMin > 0 ->
    NewRateMax = if
        RateMax =:= undefined ->
            RateMin + ?COUNT_PROCESS_DYNAMIC_RATE_REQUEST_OFFSET_DEFAULT;
        RateMax =/= undefined ->
            RateMax
    end,
    count_process_dynamic_validate(Options,
        CountProcessDynamic#count_process_dynamic{
            method = rate_request,
            rate_max = erlang:max(NewRateMax, RateMin),
            rate_min = RateMin}, CountProcess);
count_process_dynamic_validate([{count_max, CountMax} | Options],
                               #count_process_dynamic{
                                   count_process_min = CountMin} =
                                       CountProcessDynamic,
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
    count_process_dynamic_validate(Options,
        CountProcessDynamic#count_process_dynamic{
            count_process_max = NewCountMax,
            count_process_min = NewCountMin}, CountProcess);
count_process_dynamic_validate([{count_min, CountMin} | Options],
                               #count_process_dynamic{
                                   count_process_max = CountMax} =
                                       CountProcessDynamic,
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
    count_process_dynamic_validate(Options,
        CountProcessDynamic#count_process_dynamic{
            count_process_max = NewCountMax,
            count_process_min = NewCountMin}, CountProcess);
count_process_dynamic_validate([{period, Period} | Options],
                               #count_process_dynamic{} =
                                   CountProcessDynamic,
                               CountProcess)
    when is_integer(Period), Period > 0,
         Period =< (?TIMEOUT_MAX_ERLANG div 1000) ->
    count_process_dynamic_validate(Options,
        CountProcessDynamic#count_process_dynamic{
            period = Period}, CountProcess);
count_process_dynamic_validate([Invalid | _Options],
                               _CountProcessDynamic, _CountProcess) ->
    {error, {service_options_count_process_dynamic_invalid, Invalid}}.

