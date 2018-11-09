%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%% MIT License
%%%
%%% Copyright (c) 2014-2018 Michael Truog <mjtruog at protonmail dot com>
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Constants that should never be changed                                     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% for features specific to Erlang/OTP version 20.x (and later versions)
-ifdef(ERLANG_OTP_VERSION_19).
-else.
-define(ERLANG_OTP_VERSION_20_FEATURES, true).
-ifdef(ERLANG_OTP_VERSION_20).
-else.
-define(ERLANG_OTP_VERSION_21_FEATURES, true).
-endif.
-endif.

% Get the stacktrace in a way that is backwards compatible
-ifdef(ERLANG_OTP_VERSION_21_FEATURES).
-define(STACKTRACE(ErrorType, Error, ErrorStackTrace),
        ErrorType:Error:ErrorStackTrace ->).
-else.
-define(STACKTRACE(ErrorType, Error, ErrorStackTrace),
        ErrorType:Error ->
            ErrorStackTrace = erlang:get_stacktrace(),).
-endif.

% for using cloudi_core as an isolated Erlang application
% outside of the CloudI repository
% (only internal services are supported,
%  due to the extra compilation required for external services support)
%-define(CLOUDI_CORE_STANDALONE, true).

% used to calculate the timeout_terminate based on MaxT / MaxR
-define(TIMEOUT_TERMINATE_CALC0(MaxT),
        ((1000 * MaxT) - ?TIMEOUT_DELTA)).
-define(TIMEOUT_TERMINATE_CALC1(MaxR, MaxT),
        ((1000 * MaxT) div MaxR - ?TIMEOUT_DELTA)).

% termination timeout reduction to ensure enough time is available
% to send SIGKILL to the OS pid, if it is still running
% (the external service Erlang process needs to avoid getting
%  killed by the monitor to be able to send the SIGKILL)
-define(TIMEOUT_TERMINATE_EXTERNAL(TimeoutTerm),
        erlang:max(0, TimeoutTerm - 500)).

% cloudi_x_pqueue4 usage limited by the signed byte integer storage
-define(PRIORITY_HIGH, -128).
-define(PRIORITY_LOW, 127).

% if offsets to a priority value are used to create a new priority
% these macros are helpful for providing both a maximum and a minimum value
-define(PRIORITY_HIGHER_OFFSET, ?PRIORITY_HIGH * 2 + 1). % negative
-define(PRIORITY_LOWER_OFFSET,  ?PRIORITY_LOW  * 2 + 1). % positive

% process dictionary keys used by the cloudi_core source code
% (set in all service processes)
-define(SERVICE_ID_PDICT_KEY,      cloudi_service).
-define(SERVICE_UPTIME_PDICT_KEY,  cloudi_service_uptime).
-define(SERVICE_FILE_PDICT_KEY,    cloudi_service_file).
-define(LOGGER_FLOODING_PDICT_KEY, cloudi_logger). % all logging processes

% create the locally registered name for a cpg scope
% (in a way that does not cause conflict with custom cpg scopes)
-define(SCOPE_DEFAULT, cpg_default_scope).
-define(SCOPE_CUSTOM_PREFIX, "cloudi_x_cpg_x_").
-define(SCOPE_ASSIGN(Scope),
        if
            Scope =:= default ->
                % DEFAULT_SCOPE in cpg application
                ?SCOPE_DEFAULT;
            true ->
                erlang:list_to_atom(?SCOPE_CUSTOM_PREFIX ++
                                    erlang:atom_to_list(Scope))
        end).
-define(SCOPE_FORMAT(Name),
        if
            Name =:= ?SCOPE_DEFAULT ->
                default;
            true ->
                ?SCOPE_CUSTOM_PREFIX ++ L = erlang:atom_to_list(Name),
                erlang:list_to_atom(L)
        end).

% create the locally registered name for a cloudi_core_i_logger
% formatter output gen_event module
-define(LOGGING_FORMATTER_OUTPUT_CUSTOM_PREFIX,
        "cloudi_core_i_logger_output_sup_").
-define(LOGGING_FORMATTER_OUTPUT_ASSIGN(Output, Instance),
        if
            Output =:= undefined ->
                undefined;
            true ->
                erlang:list_to_atom(?LOGGING_FORMATTER_OUTPUT_CUSTOM_PREFIX ++
                                    erlang:atom_to_list(Output) ++ "_" ++
                                    erlang:integer_to_list(Instance))
        end).

% maximum timeout value for erlang:send_after/3 and gen_server:call
-define(TIMEOUT_MAX_ERLANG, 4294967295).
% maximum timeout value for a service request
% (limitation for internal service requests, external service requests
%  should have a maximum of TIMEOUT_MAX_ERLANG)
-define(TIMEOUT_MAX, ?TIMEOUT_MAX_ERLANG - ?TIMEOUT_DELTA).

% time-related constants
-define(DAYS_IN_YEAR, 365.25).
-define(DAYS_IN_MONTH, (?DAYS_IN_YEAR / 12)).
-define(DAYS_IN_WEEK, 7).
-define(HOURS_IN_DAY, 24).
-define(SECONDS_IN_HOUR, (60 * 60)).
-define(NANOSECONDS_IN_SECOND, 1000000000).
-define(NANOSECONDS_IN_HOUR,
        (?NANOSECONDS_IN_SECOND * ?SECONDS_IN_HOUR)).
-define(NANOSECONDS_IN_DAY,
        (?NANOSECONDS_IN_SECOND * ?SECONDS_IN_HOUR *
         ?HOURS_IN_DAY)).
-define(NANOSECONDS_IN_WEEK,
        (?NANOSECONDS_IN_SECOND * ?SECONDS_IN_HOUR *
         ?HOURS_IN_DAY * ?DAYS_IN_WEEK)).
-define(NANOSECONDS_IN_MONTH,
        (?NANOSECONDS_IN_SECOND * ?SECONDS_IN_HOUR *
         ?HOURS_IN_DAY * ?DAYS_IN_MONTH)).
-define(NANOSECONDS_IN_YEAR,
        (?NANOSECONDS_IN_SECOND * ?SECONDS_IN_HOUR *
         ?HOURS_IN_DAY * ?DAYS_IN_YEAR)).
-define(NATIVE_TIME_IN_DAY,
        cloudi_timestamp:
        convert(?NANOSECONDS_IN_DAY, nanosecond, native)).
-define(NATIVE_TIME_IN_WEEK,
        cloudi_timestamp:
        convert(?NANOSECONDS_IN_WEEK, nanosecond, native)).
-define(NATIVE_TIME_IN_MONTH,
        cloudi_timestamp:
        convert(cloudi_math:ceil(?NANOSECONDS_IN_MONTH), nanosecond, native)).
-define(NATIVE_TIME_IN_YEAR,
        cloudi_timestamp:
        convert(cloudi_math:ceil(?NANOSECONDS_IN_YEAR), nanosecond, native)).
-define(AVAILABILITY_ZERO, "0 %").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Safe to tune without causing major internal problems                       %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% recv_async null UUID strategy
-define(RECV_ASYNC_STRATEGY, recv_async_select_oldest).
%-define(RECV_ASYNC_STRATEGY, recv_async_select_random). % fastest

% have errors report the service Erlang state as-is without simplification
% (to aid with debugging, should not normally be necessary)
%-define(VERBOSE_STATE, true).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Reasonable constants that are unlikely to need modification.               %
% Possibly, in different environments, tuning may be beneficial, though      %
% it has not yet been necessary to modify these settings during testing.     %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% interval at which asynchronous messages are checked
-define(RECV_ASYNC_INTERVAL, 500). % milliseconds

% interval at which asynchronous messages are sent
-define(SEND_ASYNC_INTERVAL, 500). % milliseconds

% interval at which synchronous messages are sent
-define(SEND_SYNC_INTERVAL, 500). % milliseconds

% interval at which multicast asynchronous messages are sent
-define(MCAST_ASYNC_INTERVAL, 500). % milliseconds

% interval at which synchronous forwarded messages are sent
-define(FORWARD_SYNC_INTERVAL, 500). % milliseconds

% interval at which asynchronous forwarded messages are sent
-define(FORWARD_ASYNC_INTERVAL, 500). % milliseconds

% interval at which count_process_dynamic checks the service's incoming queue
% before terminating a service process when reducing the number of service
% processes due to an incoming service request rate lower than required
-define(COUNT_PROCESS_DYNAMIC_INTERVAL, 500). % milliseconds

% decrement the timeout of each successful forward, to prevent infinite messages
% (i.e., this is the timeout penalty a request takes when forwarding a request)
-define(FORWARD_DELTA, 100). % milliseconds

% blocking operations must decrement the timeout to make sure timeouts
% have time to unravel all synchronous calls
% (should be less than all INTERVAL constants)
-define(TIMEOUT_DELTA, 100). % milliseconds

% helper macros for handling limits
-define(LIMIT_ASSIGN(Value, Min, Max),
        if
            Value =:= limit_min ->
                Min;
            Value =:= limit_max ->
                Max;
            true ->
                Value
        end).
-define(LIMIT_FORMAT(Value, Min, Max),
        if
            Value =:= Min ->
                limit_min;
            Value =:= Max ->
                limit_max;
            true ->
                Value
        end).

% initialization timeout value limits
-define(TIMEOUT_INITIALIZE_MIN, ?TIMEOUT_DELTA + 1). % milliseconds
-define(TIMEOUT_INITIALIZE_MAX, ?TIMEOUT_MAX). % milliseconds
-define(TIMEOUT_INITIALIZE_ASSIGN(TimeoutInit),
        ?LIMIT_ASSIGN(TimeoutInit,
                      ?TIMEOUT_INITIALIZE_MIN,
                      ?TIMEOUT_INITIALIZE_MAX)).
-define(TIMEOUT_INITIALIZE_FORMAT(TimeoutInit),
        ?LIMIT_FORMAT(TimeoutInit,
                      ?TIMEOUT_INITIALIZE_MIN,
                      ?TIMEOUT_INITIALIZE_MAX)).

% asynchronous send timeout value limits
-define(TIMEOUT_SEND_ASYNC_MIN, ?SEND_ASYNC_INTERVAL - 1). % milliseconds
-define(TIMEOUT_SEND_ASYNC_MAX, ?TIMEOUT_MAX). % milliseconds
-define(TIMEOUT_SEND_ASYNC_ASSIGN(TimeoutSendAsync),
        ?LIMIT_ASSIGN(TimeoutSendAsync,
                      ?TIMEOUT_SEND_ASYNC_MIN,
                      ?TIMEOUT_SEND_ASYNC_MAX)).
-define(TIMEOUT_SEND_ASYNC_FORMAT(TimeoutSendAsync),
        ?LIMIT_FORMAT(TimeoutSendAsync,
                      ?TIMEOUT_SEND_ASYNC_MIN,
                      ?TIMEOUT_SEND_ASYNC_MAX)).

% synchronous send timeout value limits
-define(TIMEOUT_SEND_SYNC_MIN, ?SEND_SYNC_INTERVAL - 1). % milliseconds
-define(TIMEOUT_SEND_SYNC_MAX, ?TIMEOUT_MAX). % milliseconds
-define(TIMEOUT_SEND_SYNC_ASSIGN(TimeoutSendSync),
        ?LIMIT_ASSIGN(TimeoutSendSync,
                      ?TIMEOUT_SEND_SYNC_MIN,
                      ?TIMEOUT_SEND_SYNC_MAX)).
-define(TIMEOUT_SEND_SYNC_FORMAT(TimeoutSendSync),
        ?LIMIT_FORMAT(TimeoutSendSync,
                      ?TIMEOUT_SEND_SYNC_MIN,
                      ?TIMEOUT_SEND_SYNC_MAX)).

% termination timeout when MaxT == 0
% (if MaxR == 0, take MaxT as a terminate timeout value, i.e., as if MaxR == 1)
-define(TIMEOUT_TERMINATE_DEFAULT,  2000). % milliseconds
% absolute bounds for the terminate function execution time
% when a service stops or restarts
-define(TIMEOUT_TERMINATE_MIN,    10). % milliseconds
% fail-fast is somewhat arbitrary but failure occurs in 1 minute or less
-define(TIMEOUT_TERMINATE_MAX, 60000). % milliseconds

% interval to reload all internal services which have been configured to
% reload their modules automatically
-define(SERVICE_INTERNAL_RELOAD, 1000). % milliseconds

% when tracking the durations of downtime during the past year, limit the
% number of durations to this max to keep memory consumption low
% (if durations are discarded, the downtime result is considered approximate)
-define(STATUS_DURATIONS_YEAR_MAX, 366).

% maximum average time inbetween CloudI logger calls during the interval
% to trigger logger flooding prevention, so that logging messages are discarded
% since they are coming from source code that is misbehaving that has already
% logged enough (only affects the single Erlang process)
-define(LOGGER_FLOODING_DELTA, 10). % microseconds

% time interval to check logger flooding within
-define(LOGGER_FLOODING_INTERVAL_MAX, 10000). % milliseconds
-define(LOGGER_FLOODING_INTERVAL_MIN,     5). % milliseconds

% message queue size that causes the logger to use synchronous messaging
% to avoid excessive memory consumption and system death
% (i.e., when the logger is not being flooded quickly by an individual
%  process, but is simply overloaded by all processes)
-define(LOGGER_MSG_QUEUE_SYNC, 1000).

% message queue size that causes the logger to switch back to
% asynchronous messaging after using synchronous messaging
-define(LOGGER_MSG_QUEUE_ASYNC, (?LOGGER_MSG_QUEUE_SYNC - 250)).

% periodic connection checks to determine if the udp connection is still active
% must be a short time since this impacts MaxR and MaxT.  However, this time
% becomes a hard maximum (minus a delta for overhead) for a task time target
% used in a service (i.e., the maximum amount of time spent not responding
% to incoming API calls).
-define(KEEPALIVE_UDP, 5000). % milliseconds

