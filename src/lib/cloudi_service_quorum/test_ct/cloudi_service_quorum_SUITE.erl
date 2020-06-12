%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
-module(cloudi_service_quorum_SUITE).
-behaviour(cloudi_service).

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/3]).

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
-export([t_quorum_timeout_byzantine/1,
         t_quorum_timeout_integer/1,
         t_quorum_timeout_float/1,
         t_quorum_crash_byzantine/1,
         t_quorum_crash_integer/1,
         t_quorum_crash_float/1]).

-include_lib("proper/include/proper.hrl").
-include_lib("common_test/include/ct.hrl").
-include_lib("cloudi_core/include/cloudi_logger.hrl").

%-define(NUMTESTS, 10000).
-define(NUMTESTS, 1).
-ifndef(CLOUDI_LONG_TEST_TIMEOUT).
-define(CLOUDI_LONG_TEST_TIMEOUT, 60). % minutes
-endif.

% for features specific to Erlang/OTP version 21.x (and later versions)
-ifdef(OTP_RELEASE).
-define(ERLANG_OTP_VERSION_21_FEATURES, true).
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

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(_Args, _Prefix, _Timeout, Dispatcher) ->
    cloudi_service:subscribe(Dispatcher, "proper"),
    {ok, undefined}.

cloudi_service_handle_request(_Type, _Name, _Pattern, RequestInfo, Request,
                              _Timeout, _Priority, _TransId, _Pid,
                              State, _Dispatcher) ->
    {reply, RequestInfo, Request, State}.

cloudi_service_handle_info(Request, State, _Dispatcher) ->
    {stop, cloudi_string:format("Unknown info \"~w\"", [Request]), State}.

cloudi_service_terminate(_Reason, _Timeout, _State) ->
    ok.

%%%------------------------------------------------------------------------
%%% Callback functions from CT
%%%------------------------------------------------------------------------

all() ->
    test_condition([{group, quorum_timeout},
                    {group, quorum_crash}],
                   ?CLOUDI_LONG_TEST_TIMEOUT).

groups() ->
    [{quorum_timeout, [],
      [t_quorum_timeout_byzantine,
       t_quorum_timeout_integer,
       t_quorum_timeout_float]},
     {quorum_crash, [],
      [t_quorum_crash_byzantine,
       t_quorum_crash_integer,
       t_quorum_crash_float]}].

suite() ->
    [{enable_builtin_hooks, false},
     {timetrap, {minutes, ?CLOUDI_LONG_TEST_TIMEOUT}}].

init_per_suite(Config) ->
    ok = cloudi_x_reltool_util:
         application_start(sasl, [{sasl_error_logger, false}], infinity),
    ok = cloudi_x_reltool_util:application_start(cloudi_core, [], infinity),
    [{numtests, ?NUMTESTS} | Config].

end_per_suite(_Config) ->
    ok = cloudi_x_reltool_util:application_stop(cloudi_core),
    ok.

group(_GroupName) ->
    [].

init_per_group(_GroupName, Config) ->
    Config.

end_per_group(_GroupName, Config) ->
    Config.

init_per_testcase(TestCase, Config) ->
    error_logger:info_msg("~p init~n", [TestCase]),
    {ok, Services} = cloudi_service_api:services(infinity),
    lists:foreach(fun({ServiceId, _}) ->
        cloudi_service_api:services_remove([ServiceId], infinity)
    end, Services),
    error_logger:tty(false),
    Config.

end_per_testcase(TestCase, Config) ->
    error_logger:tty(true),
    error_logger:info_msg("~p end~n", [TestCase]),
    Config.

%%%------------------------------------------------------------------------
%%% test cases
%%%------------------------------------------------------------------------

t_quorum_timeout_byzantine(Config) ->
    true = proper:quickcheck(prop_quorum_timeout(byzantine, Config),
                             [{numtests, ?config(numtests, Config)},
                              long_result]),
    ok.

t_quorum_timeout_integer(Config) ->
    true = proper:quickcheck(prop_quorum_timeout(integer(1, 1000), Config),
                             [{numtests, ?config(numtests, Config)},
                              long_result]),
    ok.

t_quorum_timeout_float(Config) ->
    true = proper:quickcheck(prop_quorum_timeout(float(1.0e-100, 1.0), Config),
                             [{numtests, ?config(numtests, Config)},
                              long_result]),
    ok.

prop_quorum_timeout(QuorumTypeProper, _Config) ->
    ?FORALL({QuorumType, UseResponseInfo,
             CountProcess, Monkey, RequestInfo, Request},
            {QuorumTypeProper, use_response_info(),
             count_process(), monkey(), request_info(), request()},
            try
                validate_quorum_timeout(QuorumType, UseResponseInfo,
                                        CountProcess, Monkey,
                                        RequestInfo, Request)
            catch
                ?STACKTRACE(ErrorType, Error, ErrorStackTrace)
                    error_msg("validate_quorum_timeout failed: ~p ~p~n~p~n",
                              [ErrorType, Error, ErrorStackTrace]),
                    false
            end).

validate_quorum_timeout(QuorumType, UseResponseInfo,
                        CountProcess, Monkey, RequestInfo, Request) ->
    Context0 = cloudi:new(),
    Timeout = 15000,
    CountTimeouts = count_process_errors(CountProcess, Monkey),
    CountSuccesses = CountProcess - CountTimeouts,
    ServiceIds0 = services_add(CountTimeouts,
        {internal,
            "/test/",
            ?MODULE,
            [],
            immediate_closest,
            5000, 5000, 5000, undefined, undefined, 1, 0, 0,
            [{automatic_loading, false},
             {monkey_latency,
              [{time_absolute, Timeout * 2}]}]}),
    ServiceIds1 = services_add(CountSuccesses, ServiceIds0,
        {internal,
            "/test/",
            ?MODULE,
            [],
            immediate_closest,
            5000, 5000, 5000, undefined, undefined, 1, 0, 0,
            [{automatic_loading, false}]}),
    {ok, [E]} = cloudi_service_api:services_add([
        {internal,
            "/quorum",
            cloudi_service_quorum,
            [{quorum, QuorumType},
             {use_response_info, UseResponseInfo}],
            immediate_closest,
            5000, 5000, 5000, undefined, undefined, 1, 0, 0, []}],
        infinity),
    ServiceIdsN = [E | ServiceIds1],
    {Result, _} = cloudi:send_sync(Context0, "/quorum/test/proper",
                                   RequestInfo, Request,
                                   Timeout, undefined),
    [cloudi_service_api:services_remove([ServiceId], infinity) ||
     ServiceId <- ServiceIdsN],
    ProperResult = result_expected(QuorumType, CountProcess, Monkey,
                                   RequestInfo, Request),
    if
        Result /= ProperResult ->
            error_msg("quorum_timeout (~p, ~p, ~p)~n ~p /= ~p~n",
                      [QuorumType, CountProcess, Monkey,
                       Result, ProperResult]),
            false;
        true ->
            true
    end.

t_quorum_crash_byzantine(Config) ->
    true = proper:quickcheck(prop_quorum_crash(byzantine, Config),
                             [{numtests, ?config(numtests, Config)},
                              long_result]),
    ok.

t_quorum_crash_integer(Config) ->
    true = proper:quickcheck(prop_quorum_crash(integer(1, 1000), Config),
                             [{numtests, ?config(numtests, Config)},
                              long_result]),
    ok.

t_quorum_crash_float(Config) ->
    true = proper:quickcheck(prop_quorum_crash(float(1.0e-100, 1.0), Config),
                             [{numtests, ?config(numtests, Config)},
                              long_result]),
    ok.

prop_quorum_crash(QuorumTypeProper, _Config) ->
    ?FORALL({QuorumType, UseResponseInfo,
             CountProcess, Monkey, RequestInfo, Request},
            {QuorumTypeProper, use_response_info(),
             count_process(), monkey(), request_info(), request()},
            try
                validate_quorum_crash(QuorumType, UseResponseInfo,
                                      CountProcess, Monkey,
                                      RequestInfo, Request)
            catch
                ?STACKTRACE(ErrorType, Error, ErrorStackTrace)
                    error_msg("validate_quorum_crash failed: ~p ~p~n~p~n",
                              [ErrorType, Error, ErrorStackTrace]),
                    false
            end).

validate_quorum_crash(QuorumType, UseResponseInfo,
                      CountProcess, Monkey, RequestInfo, Request) ->

    Context0 = cloudi:new(),
    Timeout = 15000,
    CountCrashes = count_process_errors(CountProcess, Monkey),
    CountSuccesses = CountProcess - CountCrashes,
    ServiceIds0 = services_add(CountCrashes,
        {internal,
            "/test/",
            ?MODULE,
            [],
            immediate_closest,
            5000, 5000, 5000, undefined, undefined, 1, 0, 0,
            [{automatic_loading, false},
             {monkey_chaos,
              [{probability_request, 1.0}]}]}),
    ServiceIds1 = services_add(CountSuccesses, ServiceIds0,
        {internal,
            "/test/",
            ?MODULE,
            [],
            immediate_closest,
            5000, 5000, 5000, undefined, undefined, 1, 0, 0,
            [{automatic_loading, false}]}),
    {ok, [E]} = cloudi_service_api:services_add([
        {internal,
            "/quorum",
            cloudi_service_quorum,
            [{quorum, QuorumType},
             {use_response_info, UseResponseInfo}],
            immediate_closest,
            5000, 5000, 5000, undefined, undefined, 1, 0, 0, []}],
        infinity),
    ServiceIdsN = [E | ServiceIds1],
    {Result, _} = cloudi:send_sync(Context0, "/quorum/test/proper",
                                   RequestInfo, Request,
                                   Timeout, undefined),
    [cloudi_service_api:services_remove([ServiceId], infinity) ||
     ServiceId <- ServiceIdsN],
    ProperResult = result_expected(QuorumType, CountProcess, Monkey,
                                   RequestInfo, Request),
    if
        Result /= ProperResult ->
            error_msg("quorum_crash (~p, ~p, ~p)~n ~p /= ~p~n",
                      [QuorumType, CountProcess, Monkey,
                       Result, ProperResult]),
            false;
        true ->
            true
    end.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

test_condition(_, 0) ->
    {skip, long_tests_disabled};
test_condition(L, LongTestTimeout)
    when LongTestTimeout > 0 ->
    L.

services_add(I, Configuration) ->
    services_add(I, [], Configuration).

services_add(0, L, _) ->
    L;
services_add(I, L, Configuration) ->
    {ok, [E]} = cloudi_service_api:services_add([Configuration], infinity),
    services_add(I - 1, [E | L], Configuration).

result_expected(byzantine, CountProcess, Monkey, RequestInfo, Request) ->
    QuorumProcesses = CountProcess - cloudi_math:floor((CountProcess - 1) / 3),
    case (QuorumProcesses =< count_process_successes(CountProcess, Monkey)) of
        true ->
            result_success(RequestInfo, Request);
        false ->
            result_error()
    end;
result_expected(Quorum, CountProcess, Monkey, RequestInfo, Request)
    when is_integer(Quorum) ->
    QuorumProcesses = Quorum,
    case (QuorumProcesses =< count_process_successes(CountProcess, Monkey)) of
        true ->
            result_success(RequestInfo, Request);
        false ->
            result_error()
    end;
result_expected(Quorum, CountProcess, Monkey, RequestInfo, Request)
    when is_float(Quorum) ->
    QuorumProcesses = erlang:min(CountProcess,
                                 cloudi_math:ceil(Quorum * CountProcess)),
    case (QuorumProcesses =< count_process_successes(CountProcess, Monkey)) of
        true ->
            result_success(RequestInfo, Request);
        false ->
            result_error()
    end.

count_process_successes(CountProcess, Monkey) ->
    CountProcess - count_process_errors(CountProcess, Monkey).

count_process_errors(CountProcess, Monkey) ->
    erlang:min(erlang:round(CountProcess * Monkey), CountProcess).

result_success(RequestInfo, Request) ->
    {ok, RequestInfo, Request}.

result_error() ->
    {error, timeout}.

use_response_info() ->
    boolean().

count_process() ->
    integer(1000, 1100). % number of receiving service instances

monkey() ->
    float(1.0e-100, 1.0). % percentage of service requests that will fail

request() ->
    binary(1).

request_info() ->
    binary(1).

error_msg(Format, Args) ->
    error_logger:tty(true),
    error_logger:error_msg(Format, Args),
    error_logger:tty(false),
    ok.

