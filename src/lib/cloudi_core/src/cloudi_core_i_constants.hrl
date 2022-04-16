%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%% MIT License
%%%
%%% Copyright (c) 2014-2022 Michael Truog <mjtruog at protonmail dot com>
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

-ifdef(OTP_RELEASE). % Erlang/OTP >= 21.0
% able to use -if/-elif here
-if(?OTP_RELEASE >= 25).
-define(ERLANG_OTP_VERSION_25_FEATURES, true).
-endif.
-if(?OTP_RELEASE >= 24).
-define(ERLANG_OTP_VERSION_24_FEATURES, true).
-endif.
-if(?OTP_RELEASE < 22).
-error("Erlang/OTP version >= 22.0 is required!").
-endif.
-else.
-error("Erlang/OTP version >= 22.0 is required!").
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
-define(LOGGER_METADATA_PDICT_KEY, cloudi_logger_metadata).

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

% When distributed Erlang messages sent to remote nodes hit the
% distribution buffer busy limit (dist_buf_busy_limit) the
% sending Erlang process is suspended (process status becomes suspended).
% This behavior may change in the future (in the Erlang VM).
-define(SEND_REMOTE_MAY_SUSPEND, true).

% locally registered name for the supool cloudi_core_i_os_spawn processes
% OS_SPAWN_POOL is used for creating service OS processes and
%               collecting stdout/stderr stream data
-define(OS_SPAWN_POOL, cloudi_core_i_os_spawn_pool).

% locally registered name for the supool cloudi_core_i_os_command processes
% OS_COMMAND_POOL is used for separate functionality that must not block on
%                 managing service OS processes, like kill function calls
-define(OS_COMMAND_POOL, cloudi_core_i_os_command_pool).

% maximum timeout value for erlang:send_after/3, gen_server:call and
% internal/external service requests
-define(TIMEOUT_MAX_ERLANG, 4294967295).
% maximum timeout value for blocking on a response in Erlang source code
-define(TIMEOUT_MAX, ?TIMEOUT_MAX_ERLANG - ?TIMEOUT_DELTA).

% for gen_server:call/3 and similar functions that exit on timeout or noproc
-define(CATCH_EXIT(F),
        try F catch exit:{Reason, _} -> {error, Reason} end).

% time-related constants
-define(DAYS_IN_YEAR, 365.25).
-define(DAYS_IN_MONTH, (?DAYS_IN_YEAR / 12)).
-define(DAYS_IN_WEEK, 7).
-define(HOURS_IN_DAY, 24).
-define(SECONDS_IN_MINUTE, 60).
-define(SECONDS_IN_HOUR, (60 * ?SECONDS_IN_MINUTE)).
-define(MILLISECONDS_IN_SECOND, 1000).
-define(MILLISECONDS_IN_MINUTE,
        (?SECONDS_IN_MINUTE * ?MILLISECONDS_IN_SECOND)).
-define(MILLISECONDS_IN_HOUR,
        (?SECONDS_IN_HOUR * ?MILLISECONDS_IN_SECOND)).
-define(MILLISECONDS_IN_DAY,
        (?HOURS_IN_DAY * ?SECONDS_IN_HOUR * ?MILLISECONDS_IN_SECOND)).
-define(MICROSECONDS_IN_SECOND, 1000000).
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

% UUID v1 variant for service ids and transaction ids
-define(UUID_V1_VARIANT_DEFAULT, rfc4122).

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

% blocking on a response must have an outer timeout slightly larger than
% an inner timeout for all sychronous calls to reliably unravel
% (should not be used when the response data is critically necessary and
%  a race with a timer is possible, because timeouts are not accurate)
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
-define(LIMIT_ASSIGN_MILLISECONDS(Value, Min, Max),
        if
            Value =:= limit_min ->
                Min;
            Value =:= limit_max ->
                Max;
            is_tuple(Value) ->
                if
                    element(2, Value) =:= seconds orelse
                    element(2, Value) =:= second ->
                        element(1, Value) * ?MILLISECONDS_IN_SECOND;
                    element(2, Value) =:= minutes orelse
                    element(2, Value) =:= minute ->
                        element(1, Value) * ?MILLISECONDS_IN_MINUTE;
                    element(2, Value) =:= hours orelse
                    element(2, Value) =:= hour ->
                        element(1, Value) * ?MILLISECONDS_IN_HOUR;
                    element(2, Value) =:= days orelse
                    element(2, Value) =:= day ->
                        element(1, Value) * ?MILLISECONDS_IN_DAY
                end;
            is_integer(Value); Value =:= undefined ->
                Value
        end).
-define(LIMIT_FORMAT(Value, Min, Max),
        if
            Value == Min ->
                limit_min;
            Value == Max ->
                limit_max;
            true ->
                Value
        end).
-define(LIMIT_FORMAT_MILLISECONDS(Value, Min, Max),
        if
            Value == Min ->
                limit_min;
            Value == Max ->
                limit_max;
            Value == ?MILLISECONDS_IN_DAY ->
                {1, day};
            (Value div ?MILLISECONDS_IN_DAY) *
            ?MILLISECONDS_IN_DAY == Value ->
                {Value div ?MILLISECONDS_IN_DAY, days};
            Value == ?MILLISECONDS_IN_HOUR ->
                {1, hour};
            (Value div ?MILLISECONDS_IN_HOUR) *
            ?MILLISECONDS_IN_HOUR == Value ->
                {Value div ?MILLISECONDS_IN_HOUR, hours};
            Value == ?MILLISECONDS_IN_MINUTE ->
                {1, minute};
            (Value div ?MILLISECONDS_IN_MINUTE) *
            ?MILLISECONDS_IN_MINUTE == Value ->
                {Value div ?MILLISECONDS_IN_MINUTE, minutes};
            Value == ?MILLISECONDS_IN_SECOND ->
                {1, second};
            (Value div ?MILLISECONDS_IN_SECOND) *
            ?MILLISECONDS_IN_SECOND == Value ->
                {Value div ?MILLISECONDS_IN_SECOND, seconds};
            true ->
                Value
        end).
-define(LIMIT_GUARD_INTEGER(Value, Min, Max),
        ((is_integer(Value) andalso
          (Value >= Min) andalso (Value =< Max)) orelse
         (Value =:= limit_min) orelse
         (Value =:= limit_max))).
-define(LIMIT_GUARD_MILLISECONDS(Value, Min, Max),
        ((is_integer(Value) andalso
          (Value >= Min) andalso (Value =< Max)) orelse
         (is_tuple(Value) andalso
          (tuple_size(Value) == 2) andalso
          is_integer(element(1, Value)) andalso
          (Min =< ?MILLISECONDS_IN_SECOND) andalso % <-- current requirement
          (element(1, Value) >= 1) andalso
          ((((element(2, Value) =:= seconds) orelse
             (element(2, Value) =:= second)) andalso
            (element(1, Value) =< Max div ?MILLISECONDS_IN_SECOND)) orelse
           (((element(2, Value) =:= minutes) orelse
             (element(2, Value) =:= minute)) andalso
            (element(1, Value) =< Max div ?MILLISECONDS_IN_MINUTE)) orelse
           (((element(2, Value) =:= hours) orelse
             (element(2, Value) =:= hour)) andalso
            (element(1, Value) =< Max div ?MILLISECONDS_IN_HOUR)) orelse
           (((element(2, Value) =:= days) orelse
             (element(2, Value) =:= day)) andalso
            (element(1, Value) =< Max div ?MILLISECONDS_IN_DAY)))) orelse
         (Value =:= limit_min) orelse
         (Value =:= limit_max))).
-define(LIMIT_GUARD_MILLISECONDS_60000_MAX(Value, Min, Max),
        ((is_integer(Value) andalso
          (Value >= Min) andalso (Value =< Max)) orelse
         (is_tuple(Value) andalso
          (tuple_size(Value) == 2) andalso
          is_integer(element(1, Value)) andalso
          (Min =< ?MILLISECONDS_IN_SECOND) andalso
          (element(1, Value) >= 1) andalso
          ((((element(2, Value) =:= seconds) orelse
             (element(2, Value) =:= second)) andalso
            (element(1, Value) =< Max div ?MILLISECONDS_IN_SECOND)))) orelse
         (Value =:= limit_min) orelse
         (Value =:= limit_max))).

% The TIMEOUT_*_MIN values below are for initialization,
% the interface functions allow 0 as the min due to the service request
% timeout getting automatically decremented based on the elapsed time.
% So, that means the 'limit_min' atom represents the TIMEOUT_*_MIN constant
% below as the minimum limit for timeout initialization.

% initialization timeout value limits (milliseconds)
-define(TIMEOUT_INITIALIZE_MIN, ?TIMEOUT_DELTA + 1).
-define(TIMEOUT_INITIALIZE_MAX, ?TIMEOUT_MAX).

% asynchronous send timeout value limits (milliseconds)
-define(TIMEOUT_SEND_ASYNC_MIN, ?SEND_ASYNC_INTERVAL - 1).
-define(TIMEOUT_SEND_ASYNC_MAX, ?TIMEOUT_MAX_ERLANG).

% synchronous send timeout value limits (milliseconds)
-define(TIMEOUT_SEND_SYNC_MIN, ?SEND_SYNC_INTERVAL - 1).
-define(TIMEOUT_SEND_SYNC_MAX, ?TIMEOUT_MAX_ERLANG).

% Erlang CloudI API timeout value limits (milliseconds)
-define(TIMEOUT_GET_PID_MIN, ?TIMEOUT_SEND_SYNC_MIN).
-define(TIMEOUT_GET_PID_MAX, ?TIMEOUT_SEND_SYNC_MAX).
-define(TIMEOUT_GET_PIDS_MIN, ?TIMEOUT_SEND_SYNC_MIN).
-define(TIMEOUT_GET_PIDS_MAX, ?TIMEOUT_SEND_SYNC_MAX).
-define(TIMEOUT_MCAST_ASYNC_MIN, ?MCAST_ASYNC_INTERVAL - 1).
-define(TIMEOUT_MCAST_ASYNC_MAX, ?TIMEOUT_MAX_ERLANG).
-define(TIMEOUT_FORWARD_ASYNC_MIN, 0).
-define(TIMEOUT_FORWARD_ASYNC_MAX, ?TIMEOUT_MAX_ERLANG).
-define(TIMEOUT_FORWARD_SYNC_MIN, 0).
-define(TIMEOUT_FORWARD_SYNC_MAX, ?TIMEOUT_MAX_ERLANG).
-define(TIMEOUT_RETURN_ASYNC_MIN, 0).
-define(TIMEOUT_RETURN_ASYNC_MAX, ?TIMEOUT_MAX_ERLANG).
-define(TIMEOUT_RETURN_SYNC_MIN, 0).
-define(TIMEOUT_RETURN_SYNC_MAX, ?TIMEOUT_MAX_ERLANG).
-define(TIMEOUT_RECV_ASYNC_MIN, ?RECV_ASYNC_INTERVAL - 1).
-define(TIMEOUT_RECV_ASYNC_MAX, ?TIMEOUT_MAX_ERLANG).
-define(TIMEOUT_RECV_ASYNCS_MIN, ?RECV_ASYNC_INTERVAL - 1).
-define(TIMEOUT_RECV_ASYNCS_MAX, ?TIMEOUT_MAX_ERLANG).

% service configuration options limits (milliseconds)
-define(DEST_REFRESH_START_MIN, 0).
-define(DEST_REFRESH_START_MAX, ?TIMEOUT_MAX_ERLANG).
-define(DEST_REFRESH_DELAY_MIN, ?TIMEOUT_DELTA + 1).
-define(DEST_REFRESH_DELAY_MAX, ?TIMEOUT_MAX_ERLANG).
-define(REQUEST_TIMEOUT_IMMEDIATE_MAX_MIN, 0).
-define(REQUEST_TIMEOUT_IMMEDIATE_MAX_MAX, ?TIMEOUT_MAX_ERLANG).
-define(RESPONSE_TIMEOUT_IMMEDIATE_MAX_MIN, 0).
-define(RESPONSE_TIMEOUT_IMMEDIATE_MAX_MAX, ?TIMEOUT_MAX_ERLANG).

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
-define(LOGGER_FLOODING_DELTA_LOCAL,        10). % microseconds
% harsher constraint for logging remotely to avoid negatively impacting
% the remote logging process and prevent it from changing its process mode
-define(LOGGER_FLOODING_DELTA_REMOTE,      100). % microseconds

% time interval to check logger flooding within (millisecond granularity)
-define(LOGGER_FLOODING_INTERVAL_MAX, 10000000). % microseconds
-define(LOGGER_FLOODING_INTERVAL_MIN,     5000). % microseconds

% log messages to process before switching back to sync mode
-define(LOGGER_MODE_OVERLOAD_OFFSET, 250).

% logger options limits (milliseconds)
-define(LOGGER_FILE_SYNC_MIN, 0).
-define(LOGGER_FILE_SYNC_MAX, 60000).

% periodic connection checks to determine if the udp connection is still active
% must be a short time since this impacts MaxR and MaxT.  However, this time
% becomes a hard maximum (minus a delta for overhead) for a task time target
% used in a service (i.e., the maximum amount of time spent not responding
% to incoming API calls).
-define(KEEPALIVE_UDP, 5000). % milliseconds

% common limit macro usage:

-define(TIMEOUT_SEND_ASYNC_ASSIGN(TimeoutSendAsync),
        ?LIMIT_ASSIGN_MILLISECONDS(TimeoutSendAsync,
                                   ?TIMEOUT_SEND_ASYNC_MIN,
                                   ?TIMEOUT_SEND_ASYNC_MAX)).
-define(TIMEOUT_SEND_ASYNC_FORMAT(TimeoutSendAsync),
        ?LIMIT_FORMAT_MILLISECONDS(TimeoutSendAsync,
                                   ?TIMEOUT_SEND_ASYNC_MIN,
                                   ?TIMEOUT_SEND_ASYNC_MAX)).
-define(TIMEOUT_SEND_ASYNC_GUARD(TimeoutSendAsync),
        ?LIMIT_GUARD_MILLISECONDS(TimeoutSendAsync,
                                  ?TIMEOUT_SEND_ASYNC_MIN,
                                  ?TIMEOUT_SEND_ASYNC_MAX)).

-define(TIMEOUT_SEND_SYNC_ASSIGN(TimeoutSendSync),
        ?LIMIT_ASSIGN_MILLISECONDS(TimeoutSendSync,
                                   ?TIMEOUT_SEND_SYNC_MIN,
                                   ?TIMEOUT_SEND_SYNC_MAX)).
-define(TIMEOUT_SEND_SYNC_FORMAT(TimeoutSendSync),
        ?LIMIT_FORMAT_MILLISECONDS(TimeoutSendSync,
                                   ?TIMEOUT_SEND_SYNC_MIN,
                                   ?TIMEOUT_SEND_SYNC_MAX)).
-define(TIMEOUT_SEND_SYNC_GUARD(TimeoutSendSync),
        ?LIMIT_GUARD_MILLISECONDS(TimeoutSendSync,
                                  ?TIMEOUT_SEND_SYNC_MIN,
                                  ?TIMEOUT_SEND_SYNC_MAX)).

-define(DEST_REFRESH_START_ASSIGN(DestRefreshStart),
        ?LIMIT_ASSIGN_MILLISECONDS(DestRefreshStart,
                                   ?DEST_REFRESH_START_MIN,
                                   ?DEST_REFRESH_START_MAX)).
-define(DEST_REFRESH_START_FORMAT(DestRefreshStart),
        ?LIMIT_FORMAT_MILLISECONDS(DestRefreshStart,
                                   ?DEST_REFRESH_START_MIN,
                                   ?DEST_REFRESH_START_MAX)).
-define(DEST_REFRESH_START_GUARD(DestRefreshStart),
        ?LIMIT_GUARD_MILLISECONDS(DestRefreshStart,
                                  ?DEST_REFRESH_START_MIN,
                                  ?DEST_REFRESH_START_MAX)).

-define(DEST_REFRESH_DELAY_ASSIGN(DestRefreshDelay),
        ?LIMIT_ASSIGN_MILLISECONDS(DestRefreshDelay,
                                   ?DEST_REFRESH_DELAY_MIN,
                                   ?DEST_REFRESH_DELAY_MAX)).
-define(DEST_REFRESH_DELAY_FORMAT(DestRefreshDelay),
        ?LIMIT_FORMAT_MILLISECONDS(DestRefreshDelay,
                                   ?DEST_REFRESH_DELAY_MIN,
                                   ?DEST_REFRESH_DELAY_MAX)).
-define(DEST_REFRESH_DELAY_GUARD(DestRefreshDelay),
        ?LIMIT_GUARD_MILLISECONDS(DestRefreshDelay,
                                  ?DEST_REFRESH_DELAY_MIN,
                                  ?DEST_REFRESH_DELAY_MAX)).

