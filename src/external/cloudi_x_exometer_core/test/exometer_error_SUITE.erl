-module(exometer_error_SUITE).

%% -------------------------------------------------------------------
%%
%% Copyright (c) 2014 Basho Technologies, Inc.  All Rights Reserved.
%%
%%   This Source Code Form is subject to the terms of the Mozilla Public
%%   License, v. 2.0. If a copy of the MPL was not distributed with this
%%   file, You can obtain one at http://mozilla.org/MPL/2.0/.
%%
%% -------------------------------------------------------------------
%% common_test exports
-export(
   [
    all/0, groups/0, suite/0,
    init_per_suite/1, end_per_suite/1,
    init_per_testcase/2, end_per_testcase/2
   ]).

%% test case exports
-export(
   [
    test_failing_probe/1,
    test_escalation_1/1,
    test_escalation_2/1
   ]).

-import(exometer_test_util, [majority/2]).

-include_lib("common_test/include/ct.hrl").

%%%===================================================================
%%% common_test API
%%%===================================================================

all() ->
    [
     {group, test_probes}
    ].

groups() ->
    [
     {test_probes, [shuffle],
      [
       test_failing_probe,
       test_escalation_1,
       test_escalation_2
      ]}
    ].

suite() ->
    [].

init_per_suite(Config) ->
    _ = application:stop(exometer_core),
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_Case, Config) ->
    {ok, Started} = exometer_test_util:ensure_all_started(exometer_core),
    ct:log("Started: ~p~n", [[{T, catch ets:tab2list(T)}
                              || T <- exometer_util:tables()]]),
    [{started_apps, Started}|Config].

end_per_testcase(_Case, Config) ->
    ct:log("end_per_testcase(Config = ~p)~n", [Config]),
    stop_started_apps(Config),
    ok.

stop_started_apps(Config) ->
    [stop_app(A) || A <- lists:reverse(?config(started_apps, Config))],
    ok.

stop_app(App) ->
    case application:stop(App) of
        ok -> ok;
        {error, {not_started,_}} -> ok
    end.

%%%===================================================================
%%% Test Cases
%%%===================================================================
test_failing_probe(Config) ->
    M = [?MODULE, ?LINE],
    majority(fun test_failing_probe_/1, [{metric_name, M}|Config]).

test_failing_probe_({cleanup, Config}) ->
    M = ?config(metric_name, Config),
    exometer:delete(M);
test_failing_probe_(Config) ->
    M = ?config(metric_name, Config),
    ok = exometer:new(M, histogram, []),
    true = killed_probe_restarts(M),
    ok.

test_escalation_1(Config) ->
    M = [?MODULE, ?LINE],
    majority(fun test_escalation_1_/1, [{metric_name, M}|Config]).

test_escalation_1_({cleanup, Config}) ->
    exometer:delete(?config(metric_name, Config));
test_escalation_1_(Config) ->
    M = ?config(metric_name, Config),
    Levels = [{{3,5000}, restart},
              {'_', disable}],
    ok = exometer:new(M, histogram, [{restart, Levels}]),
    true = killed_probe_restarts(M),
    true = killed_probe_restarts(M),
    true = killed_probe_restarts(M),
    true = killed_probe_disabled(M),
    ok.

test_escalation_2(Config) ->
    M = [?MODULE, ?LINE],
    majority(fun test_escalation_2_/1, [{metric_name, M}|Config]).

test_escalation_2_({cleanup, Config}) ->
    exometer:delete(?config(metric_name, Config));
test_escalation_2_(Config) ->
    M = ?config(metric_name, Config),
    Levels = [{{3,5000}, restart},
              {'_', delete}],
    ok = exometer:new(M, histogram, [{restart, Levels}]),
    true = killed_probe_restarts(M),
    true = killed_probe_restarts(M),
    true = killed_probe_restarts(M),
    true = killed_probe_deleted(M),
    ok.

killed_probe_restarts(M) ->
    Pid = exometer:info(M, ref),
    ct:log("Pid = ~p~n", [Pid]),
    exit(Pid, kill),
    await_death(Pid),
    NewPid = exometer:info(M, ref),
    ct:log("NewPid = ~p~n", [NewPid]),
    enabled = exometer:info(M, status),
    true = Pid =/= NewPid.

killed_probe_disabled(M) ->
    Pid = exometer:info(M, ref),
    ct:log("Pid = ~p~n", [Pid]),
    exit(Pid, kill),
    await_death(Pid),
    undefined = exometer:info(M, ref),
    ct:log("Ref = undefined~n", []),
    disabled = exometer:info(M, status),
    true.

killed_probe_deleted(M) ->
    Pid = exometer:info(M, ref),
    ct:log("Pid = ~p~n", [Pid]),
    exit(Pid, kill),
    await_death(Pid),
    ct:log("Ets = ~p~n", [[{T,ets:tab2list(T)} ||
                              T <- exometer_util:tables()]]),
    {error, not_found} = exometer:get_value(M),
    ct:log("~p deleted~n", [M]),
    true.

await_death(Pid) ->
    Ref = erlang:send_after(1000, self(), zombie),
    await_death(Pid, Ref),
    %% Now ping exometer_admin twice to give it time to handle the DOWN msg
    sys:get_status(exometer_admin),
    sys:get_status(exometer_admin),
    ok.

await_death(Pid, Ref) ->
    case erlang:read_timer(Ref) of
        false ->
            error({process_not_dead, Pid});
        _ ->
            case erlang:is_process_alive(Pid) of
                true ->
                    erlang:bump_reductions(500),
                    await_death(Pid, Ref);
                false ->
                    erlang:cancel_timer(Ref),
                    sys:get_status(exometer_admin),
                    sys:get_status(exometer_admin),
                    ok
            end
    end.
