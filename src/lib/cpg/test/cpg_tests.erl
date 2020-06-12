%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CPG Tests==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2013-2020 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2013-2020 Michael Truog
%%% @version 2.0.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cpg_tests).

-author('mjtruog at protonmail dot com').

-include_lib("eunit/include/eunit.hrl").

-ifndef(_assertOk).
-define(_assertOk(Expr), ?_assertEqual(ok, Expr)).
-endif.

-ifdef(CLOUDI_TEST_TIMEOUT).
-define(TEST_TIMEOUT, ?CLOUDI_TEST_TIMEOUT). % seconds
-else.
-define(TEST_TIMEOUT, 10). % seconds
-endif.

module_test_() ->
    {timeout, ?TEST_TIMEOUT, [
        {"cpg start", ?_assertOk(reltool_util:application_start(cpg))},
        {"via test 1", ?_assertOk(t_via_1())},
        {"via test 2", ?_assertOk(t_via_2())},
        {"via test 3", ?_assertOk(t_via_3())},
        {"supervisor_cpg via test", ?_assertOk(t_supervisor_via())},
        {"pid age test 1", ?_assertOk(t_pid_age_1())},
        {"pid age test 2", ?_assertOk(t_pid_age_2())},
        {"callbacks test", ?_assertOk(t_callbacks())},
        {"pid counts test", ?_assertOk(t_pid_counts())},
        {"cpg stop", ?_assertOk(reltool_util:application_stop(cpg))}
    ]}.

t_via_1() ->
    {ok, Pid} = cpg_test_server:start_link("message"),
    % OTP behaviors require that the process group have only a single process
    {error, {already_started, Pid}} = cpg_test_server:start_link("message"),
    ok = cpg_test_server:put("message", "Hello World!"),
    "Hello World!" = cpg_test_server:get("message"),
    erlang:unlink(Pid),
    ok = kill_pid(Pid),
    ok.

t_via_2() ->
    {ok, Pid} = cpg_test_server:start_link("error"),
    MonitorRef = erlang:monitor(process, Pid),
    erlang:unlink(Pid),
    error = gen_server:call({via, cpg, "error"}, undefined_call),
    receive
        {'DOWN', MonitorRef, process, Pid, _} ->
            ok
    end,
    false = is_process_alive(Pid),
    ok.

t_via_3() ->
    ViaName = {"local group", 4},
    {ok, Pid1} = cpg_test_server:start_link(ViaName),
    {ok, Pid2} = cpg_test_server:start_link(ViaName),
    {ok, Pid3} = cpg_test_server:start_link(ViaName),
    {ok, Pid4} = cpg_test_server:start_link(ViaName),
    Pids = [Pid1, Pid2, Pid3, Pid4],
    I1 = index(cpg_test_server:pid(ViaName), Pids),
    true = is_integer(I1),
    I2 = index(cpg_test_server:pid(ViaName), Pids),
    true = is_integer(I2),
    I3 = index(cpg_test_server:pid(ViaName), Pids),
    true = is_integer(I3),
    I4 = index(cpg_test_server:pid(ViaName), Pids),
    true = is_integer(I4),
    I5 = index(cpg_test_server:pid(ViaName), Pids),
    true = is_integer(I5),
    I6 = index(cpg_test_server:pid(ViaName), Pids),
    true = is_integer(I6),
    I7 = index(cpg_test_server:pid(ViaName), Pids),
    true = is_integer(I7),
    I8 = index(cpg_test_server:pid(ViaName), Pids),
    true = is_integer(I8),
    true = ((I1 /= I2) orelse
            (I2 /= I3) orelse
            (I3 /= I4) orelse
            (I4 /= I5) orelse
            (I5 /= I6) orelse
            (I6 /= I7) orelse
            (I7 /= I8)),
    erlang:unlink(Pid1),
    erlang:unlink(Pid2),
    erlang:unlink(Pid3),
    erlang:unlink(Pid4),
    ok = kill_pids([Pid1, Pid2, Pid3, Pid4]),
    ok.

t_supervisor_via() ->
    SupViaName = {local, "supervisor group"},
    MaxR = 5,
    MaxT = 60,
    ChildSpecs = [],
    {ok, SupPid} = supervisor_cpg:start_link(SupViaName,
                                             MaxR, MaxT, ChildSpecs),
    erlang:unlink(SupPid),
    ChildViaName1 = {local, "child group1"},
    ChildSpecEntry1 = {cpg_test_server1,
                       {cpg_test_server, start_link, [ChildViaName1]},
                       permanent, 5000, worker, [cpg_test_server]},
    {ok, ChildPid1a} = supervisor_cpg:start_child(SupViaName,
                                                  ChildSpecEntry1),
    ChildViaName2 = {local, "child group2"},
    ChildSpecEntry2 = {cpg_test_server2,
                       {cpg_test_server, start_link, [ChildViaName2]},
                       permanent, 5000, worker, [cpg_test_server]},
    {ok, ChildPid2} = supervisor_cpg:start_remote_child(SupViaName,
                                                        ChildSpecEntry2),
    ChildViaName3 = {local, "child group3"},
    ChildSpecEntry3 = {cpg_test_server3,
                       {cpg_test_server, start_link, [ChildViaName3]},
                       permanent, 5000, worker, [cpg_test_server]},
    NomadMaxR = 3,
    NomadMaxT = 60,
    {ok, ChildPid3} = supervisor_cpg:start_nomad_child(SupViaName,
                                                       NomadMaxR,
                                                       NomadMaxT,
                                                       ChildSpecEntry3),
    [{cpg_test_server3, ChildPid3, worker, [cpg_test_server]},
     {cpg_test_server2, ChildPid2, worker, [cpg_test_server]},
     {cpg_test_server1, ChildPid1a, worker, [cpg_test_server]}] =
        supervisor_cpg:which_children(SupViaName),
    ok = supervisor_cpg:terminate_child(SupViaName,
                                        cpg_test_server1),
    ok = supervisor_cpg:terminate_child(SupViaName,
                                        cpg_test_server1),
    {ok, ChildPid1b} = supervisor_cpg:restart_child(SupViaName,
                                                     cpg_test_server1),
    {cpg_test_server1, ChildPid1b, _, _} =
        lists:keyfind(cpg_test_server1, 1,
                      supervisor_cpg:which_children(SupViaName)),
    ok = kill_pid(ChildPid1b),
    timer:sleep(500),
    {cpg_test_server1, ChildPid1c, _, _} =
        lists:keyfind(cpg_test_server1, 1,
                      supervisor_cpg:which_children(SupViaName)),
    true = is_pid(ChildPid1c),
    ok = kill_pid(ChildPid1c),
    timer:sleep(500),
    {cpg_test_server1, ChildPid1d, _, _} =
        lists:keyfind(cpg_test_server1, 1,
                      supervisor_cpg:which_children(SupViaName)),
    true = is_pid(ChildPid1d),
    ok = supervisor_cpg:terminate_child(SupViaName,
                                        cpg_test_server1),
    ok = supervisor_cpg:delete_child(SupViaName,
                                     cpg_test_server1),
    {error, not_found} = supervisor_cpg:terminate_child(SupViaName,
                                                        cpg_test_server1),
    {error, not_found} = supervisor_cpg:delete_child(SupViaName,
                                                     cpg_test_server1),
    ok = kill_pid(ChildPid3),
    timer:sleep(500),
    [{active, 1},
     {specs, 1},
     {supervisors, 0},
     {workers, 1}] = lists:sort(supervisor_cpg:count_children(SupViaName)),
    ok = kill_pid(SupPid),
    ok.

t_pid_age_1() ->
    Pid1 = erlang:spawn(fun busy_pid/0),
    Pid2 = erlang:spawn(fun busy_pid/0),
    Pid3 = erlang:spawn(fun busy_pid/0),
    ok = cpg:join("GroupA", Pid1),
    ok = cpg:join("GroupA", Pid2),
    ok = cpg:join("GroupA", Pid3),
    ok = cpg:join("GroupA", Pid1),
    ok = cpg:join("GroupA", Pid2),
    {ok, "GroupA", Pid2} = cpg:get_newest_pid("GroupA"),
    {ok, "GroupA", Pid2} = cpg:get_local_newest_pid("GroupA"),
    {ok, "GroupA", Pid1} = cpg:get_newest_pid("GroupA", Pid2),
    {ok, "GroupA", Pid1} = cpg:get_local_newest_pid("GroupA", Pid2),
    {ok, "GroupA", Pid1} = cpg:get_oldest_pid("GroupA"),
    {ok, "GroupA", Pid1} = cpg:get_local_oldest_pid("GroupA"),
    {ok, "GroupA", Pid2} = cpg:get_oldest_pid("GroupA", Pid1),
    {ok, "GroupA", Pid2} = cpg:get_local_oldest_pid("GroupA", Pid1),
    History0 = [Pid2, Pid1, Pid3, Pid2, Pid1],
    {ok, "GroupA", History0} = cpg:get_members("GroupA"),
    {ok, "GroupA", History0} = cpg:get_local_members("GroupA"),
    {error, {no_such_group, "GroupA"}} = cpg:get_remote_members("GroupA"),
    ok = cpg:leave("GroupA", Pid1),
    History1 = [Pid2, Pid1, Pid3, Pid2],
    {ok, "GroupA", History1} = cpg:get_members("GroupA"),
    {ok, "GroupA", History1} = cpg:get_local_members("GroupA"),
    ok = kill_pid(Pid1),
    History2 = [Pid2, Pid3, Pid2],
    {ok, "GroupA", History2} = cpg:get_members("GroupA"),
    {ok, "GroupA", History2} = cpg:get_local_members("GroupA"),
    {ok, "GroupA", Pid3} = cpg:get_oldest_pid("GroupA", Pid2),
    {ok, "GroupA", Pid3} = cpg:get_local_oldest_pid("GroupA", Pid2),
    {ok, "GroupA", Pid3} = cpg:get_newest_pid("GroupA", Pid2),
    {ok, "GroupA", Pid3} = cpg:get_local_newest_pid("GroupA", Pid2),
    {ok, "GroupA", Pid2} = cpg:get_oldest_pid("GroupA"),
    {ok, "GroupA", Pid2} = cpg:get_local_oldest_pid("GroupA"),
    {ok, "GroupA", Pid2} = cpg:get_newest_pid("GroupA"),
    {ok, "GroupA", Pid2} = cpg:get_local_newest_pid("GroupA"),
    ok = kill_pids([Pid2, Pid3]),
    ok.

t_pid_age_2() ->
    Pid1 = erlang:spawn(fun busy_pid/0),
    Pid2 = erlang:spawn(fun busy_pid/0),
    Pid3 = erlang:spawn(fun busy_pid/0),
    0 = cpg:join_count("GroupA", Pid1),
    ok = cpg:join("GroupA", Pid1),
    1 = cpg:join_count("GroupA", Pid1),
    0 = cpg:join_count("GroupA", Pid2),
    ok = cpg:join("GroupA", Pid2),
    1 = cpg:join_count("GroupA", Pid2),
    0 = cpg:join_count("GroupA", Pid3),
    ok = cpg:join("GroupA", Pid3),
    1 = cpg:join_count("GroupA", Pid3),
    ok = cpg:join("GroupA", Pid1),
    2 = cpg:join_count("GroupA", Pid1),
    ok = cpg:join("GroupA", Pid2),
    2 = cpg:join_count("GroupA", Pid2),
    History0 = [Pid2, Pid1, Pid3, Pid2, Pid1],
    {ok, "GroupA", History0} = cpg:get_members("GroupA"),
    {ok, "GroupA", History0} = cpg:get_local_members("GroupA"),
    ok = cpg:leave("GroupA", Pid1),
    1 = cpg:join_count("GroupA", Pid1),
    History1 = [Pid2, Pid1, Pid3, Pid2],
    {ok, "GroupA", History1} = cpg:get_members("GroupA"),
    {ok, "GroupA", History1} = cpg:get_local_members("GroupA"),
    ok = cpg:leave("GroupA", Pid1),
    0 = cpg:join_count("GroupA", Pid1),
    History2 = [Pid2, Pid3, Pid2],
    {ok, "GroupA", History2} = cpg:get_members("GroupA"),
    {ok, "GroupA", History2} = cpg:get_local_members("GroupA"),
    {ok, "GroupA", Pid3} = cpg:get_oldest_pid("GroupA", Pid2),
    {ok, "GroupA", Pid3} = cpg:get_newest_pid("GroupA", Pid2),
    {ok, "GroupA", Pid2} = cpg:get_oldest_pid("GroupA"),
    {ok, "GroupA", Pid2} = cpg:get_newest_pid("GroupA"),
    ok = kill_pids([Pid1, Pid2, Pid3]),
    ok.

t_callbacks() ->
    F = fun(F1, L) ->
        receive
            {put, E} ->
                F1(F1, [E | L]);
            {get, Pid} ->
                Pid ! lists:reverse(L),
                F1(F1, [])
        end
    end,
    Pid = erlang:spawn(fun() -> F(F, []) end),
    Callback1 = fun(GroupName1, Pid1, Reason1) ->
        Pid ! {put, {callback1_join, GroupName1, Pid1, Reason1}}
    end,
    Callback2 = fun(GroupName2, Pid2, Reason2) ->
        Pid ! {put, {callback2_join, GroupName2, Pid2, Reason2}}
    end,
    Callback3 = fun(GroupName3, Pid3, Reason3) ->
        Pid ! {put, {callback3_join, GroupName3, Pid3, Reason3}}
    end,
    Callback4 = fun(GroupName4, Pid4, Reason4) ->
        Pid ! {put, {callback4_leave, GroupName4, Pid4, Reason4}}
    end,
    Callback5 = fun(GroupName5, Pid5, Reason5) ->
        Pid ! {put, {callback5_leave, GroupName5, Pid5, Reason5}}
    end,
    Callback6 = fun(GroupName6, Pid6, Reason6) ->
        Pid ! {put, {callback6_leave, GroupName6, Pid6, Reason6}}
    end,
    ok = cpg:add_join_callback("GroupA", Callback1),
    ok = cpg:add_join_callback("GroupB", Callback2),
    ok = cpg:add_join_callback("GroupC", Callback3),
    ok = cpg:add_leave_callback("GroupA", Callback4),
    ok = cpg:add_leave_callback("GroupB", Callback5),
    ok = cpg:add_leave_callback("GroupC", Callback6),
    GroupPid1 = erlang:spawn(fun busy_pid/0),
    GroupPid2 = erlang:spawn(fun busy_pid/0),
    GroupPid3 = erlang:spawn(fun busy_pid/0),
    ok = cpg:join("GroupA", GroupPid1),
    ok = cpg:join("GroupB", GroupPid2),
    ok = cpg:join("GroupA", GroupPid2),
    ok = cpg:join("GroupC", GroupPid3),
    ok = cpg:join("GroupA", GroupPid3),
    ok = cpg:join("GroupB", GroupPid3),
    ok = kill_pid(GroupPid2),
    timer:sleep(100),
    Pid ! {get, self()},
    Sequence1 = receive
        GroupSequence1 ->
            GroupSequence1
    end,
    ok = kill_pid(GroupPid1),
    timer:sleep(100),
    Pid ! {get, self()},
    Sequence2 = receive
        GroupSequence2 ->
            GroupSequence2
    end,
    ok = kill_pid(GroupPid3),
    timer:sleep(100),
    Pid ! {get, self()},
    Sequence3 = receive
        GroupSequence3 ->
            GroupSequence3
    end,
    [{callback1_join, "GroupA", GroupPid1, join_local},
     {callback2_join, "GroupB", GroupPid2, join_local},
     {callback1_join, "GroupA", GroupPid2, join_local},
     {callback3_join, "GroupC", GroupPid3, join_local},
     {callback1_join, "GroupA", GroupPid3, join_local},
     {callback2_join, "GroupB", GroupPid3, join_local},
     {callback4_leave, "GroupA", GroupPid2, {exit, killed}},
     {callback5_leave, "GroupB", GroupPid2, {exit, killed}}] = Sequence1,
    [{callback4_leave, "GroupA", GroupPid1, {exit, killed}}] = Sequence2,
    % lists:foldl on lists:umerge list of groups means that
    % leave callbacks fire in GroupName order (upon a pid death)
    % (although the pid deaths are not delivered in-order,
    %  which is why order is imposed in the code above)
    [{callback4_leave, "GroupA", GroupPid3, {exit, killed}},
     {callback5_leave, "GroupB", GroupPid3, {exit, killed}},
     {callback6_leave, "GroupC", GroupPid3, {exit, killed}}] = Sequence3,
    ok = kill_pid(Pid),
    ok = cpg:remove_join_callback("GroupA", Callback1),
    ok = cpg:remove_join_callback("GroupB", Callback2),
    ok = cpg:remove_join_callback("GroupC", Callback3),
    ok = cpg:remove_leave_callback("GroupA", Callback4),
    ok = cpg:remove_leave_callback("GroupB", Callback5),
    ok = cpg:remove_leave_callback("GroupC", Callback6),
    ok.

t_pid_counts() ->
    Pid1 = erlang:spawn(fun busy_pid/0),
    Pid2 = erlang:spawn(fun busy_pid/0),
    Pid3 = erlang:spawn(fun busy_pid/0),
    ok = cpg:join("GroupA", Pid1),
    ok = cpg:join("GroupA", Pid1),
    ok = cpg:join("GroupA", Pid1),
    ok = cpg:join("GroupB", Pid1),
    ok = cpg:join("GroupB", Pid1),
    ok = cpg:join("GroupC", Pid1),
    ok = cpg:join("GroupB", Pid2),
    ok = cpg:join("GroupB", Pid2),
    ok = cpg:join("GroupB", Pid3),
    ok = cpg:join("GroupC", Pid3),
    [{"GroupA", 3},
     {"GroupB", 2},
     {"GroupC", 1}] = Pid1Counts = cpg:which_groups_counts(Pid1),
    [{"GroupB", 2}] = Pid2Counts = cpg:which_groups_counts(Pid2),
    [{"GroupB", 1},
     {"GroupC", 1}] = Pid3Counts = cpg:which_groups_counts(Pid3),
    ok = cpg:leave_counts(Pid1Counts, Pid1),
    ok = cpg:leave_counts(Pid2Counts, Pid2),
    ok = cpg:leave_counts(Pid3Counts, Pid3),
    [] = cpg:which_groups_counts(Pid1),
    [] = cpg:which_groups_counts(Pid2),
    [] = cpg:which_groups_counts(Pid2),
    [] = cpg:which_groups(Pid1),
    [] = cpg:which_groups(Pid2),
    [] = cpg:which_groups(Pid2),
    ok = kill_pids([Pid1, Pid2, Pid3]),
    ok.

busy_pid() ->
    timer:sleep(1000),
    busy_pid().

kill_pid(Pid) when is_pid(Pid) ->
    MonitorRef = erlang:monitor(process, Pid),
    erlang:exit(Pid, kill),
    receive
        {'DOWN', MonitorRef, process, Pid, killed} ->
            ok
    end.

kill_pids(Pids) when is_list(Pids) ->
    MonitorPids = lists:map(fun(Pid) ->
        MonitorRef = erlang:monitor(process, Pid),
        erlang:exit(Pid, kill),
        {MonitorRef, Pid}
    end, Pids),
    lists:foreach(fun({MonitorRef, Pid}) ->
        receive
            {'DOWN', MonitorRef, process, Pid, killed} ->
                ok
        end
    end, MonitorPids).

index(Item, L)
    when is_list(L) ->
    index(Item, L, 1).
index(_, [], _) ->
    not_found;
index(Item, [Item | _], I) ->
    I;
index(Item, [_ | T], I) ->
    index(Item, T, I + 1).

