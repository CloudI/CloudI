%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CPG Tests==
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
%%% @version 1.2.5 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cpg_test).

-author('mjtruog [at] gmail (dot) com').

-include_lib("eunit/include/eunit.hrl").

cpg_start_test() ->
    ok = reltool_util:application_start(cpg),
    ok = application:start(sasl).

via1_test() ->
    {ok, Pid} = cpg_test_server:start_link("message"),
    % OTP behaviors require that the process group have only a single process
    {error, {already_started, Pid}} = cpg_test_server:start_link("message"),
    ok = cpg_test_server:put("message", "Hello World!"),
    "Hello World!" = cpg_test_server:get("message"),
    erlang:unlink(Pid),
    erlang:exit(Pid, kill),
    ok.

via2_test() ->
    {ok, Pid} = cpg_test_server:start_link("error"),
    erlang:unlink(Pid),
    error = gen_server:call({via, cpg, "error"}, undefined_call),
    false = is_process_alive(Pid),
    ok.

via3_test() ->
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
    true = (I1 /= I2 orelse I1 /= I3 orelse I1 /= I4 orelse I1 /= I5),
    erlang:unlink(Pid1),
    erlang:exit(Pid1, kill),
    erlang:unlink(Pid2),
    erlang:exit(Pid2, kill),
    erlang:unlink(Pid3),
    erlang:exit(Pid3, kill),
    erlang:unlink(Pid4),
    erlang:exit(Pid4, kill),
    ok.

supervisor_via_test() ->
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
    ChildPid1bRef = erlang:monitor(process, ChildPid1b),
    erlang:exit(ChildPid1b, kill),
    receive
        {'DOWN', ChildPid1bRef, process, ChildPid1b, killed} ->
            timer:sleep(500)
    end,
    {cpg_test_server1, ChildPid1c, _, _} =
        lists:keyfind(cpg_test_server1, 1,
                      supervisor_cpg:which_children(SupViaName)),
    true = is_pid(ChildPid1c),
    ChildPid1cRef = erlang:monitor(process, ChildPid1c),
    erlang:exit(ChildPid1c, kill),
    receive
        {'DOWN', ChildPid1cRef, process, ChildPid1c, killed} ->
            timer:sleep(500)
    end,
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
    ChildPid3Ref = erlang:monitor(process, ChildPid3),
    erlang:exit(ChildPid3, kill),
    receive
        {'DOWN', ChildPid3Ref, process, ChildPid3, killed} ->
            timer:sleep(500)
    end,
    [{active, 1},
     {specs, 1},
     {supervisors, 0},
     {workers, 1}] = lists:sort(supervisor_cpg:count_children(SupViaName)),
    erlang:exit(SupPid, kill),
    ok.

pid_age_test() ->
    Pid1 = erlang:spawn(fun busy_pid/0),
    Pid2 = erlang:spawn(fun busy_pid/0),
    Pid3 = erlang:spawn(fun busy_pid/0),
    ok = cpg:join("GroupA", Pid1),
    ok = cpg:join("GroupA", Pid2),
    ok = cpg:join("GroupA", Pid3),
    ok = cpg:join("GroupA", Pid1),
    ok = cpg:join("GroupA", Pid2),
    {ok, "GroupA", Pid2} = cpg:get_newest_pid("GroupA"),
    {ok, "GroupA", Pid1} = cpg:get_newest_pid("GroupA", Pid2),
    {ok, "GroupA", Pid1} = cpg:get_oldest_pid("GroupA"),
    {ok, "GroupA", Pid2} = cpg:get_oldest_pid("GroupA", Pid1),
    Pid1Ref = erlang:monitor(process, Pid1),
    erlang:exit(Pid1, kill),
    receive
        {'DOWN', Pid1Ref, process, Pid1, killed} ->
            timer:sleep(500)
    end,
    {ok, "GroupA", Pid3} = cpg:get_oldest_pid("GroupA", Pid2),
    {ok, "GroupA", Pid3} = cpg:get_newest_pid("GroupA", Pid2),
    erlang:exit(Pid2, kill),
    erlang:exit(Pid3, kill),
    ok.

callbacks_test() ->
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
    Callback1 = fun(GroupName1, Pid1) ->
        Pid ! {put, {callback1_join, GroupName1, Pid1}}
    end,
    Callback2 = fun(GroupName2, Pid2) ->
        Pid ! {put, {callback2_join, GroupName2, Pid2}}
    end,
    Callback3 = fun(GroupName3, Pid3) ->
        Pid ! {put, {callback3_join, GroupName3, Pid3}}
    end,
    Callback4 = fun(GroupName4, Pid4) ->
        Pid ! {put, {callback4_leave, GroupName4, Pid4}}
    end,
    Callback5 = fun(GroupName5, Pid5) ->
        Pid ! {put, {callback5_leave, GroupName5, Pid5}}
    end,
    Callback6 = fun(GroupName6, Pid6) ->
        Pid ! {put, {callback6_leave, GroupName6, Pid6}}
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
    erlang:exit(GroupPid2, kill),
    receive after 100 -> ok end,
    Pid ! {get, self()},
    Sequence1 = receive
        GroupSequence1 ->
            GroupSequence1
    end,
    erlang:exit(GroupPid1, kill),
    receive after 100 -> ok end,
    Pid ! {get, self()},
    Sequence2 = receive
        GroupSequence2 ->
            GroupSequence2
    end,
    erlang:exit(GroupPid3, kill),
    receive after 100 -> ok end,
    Pid ! {get, self()},
    Sequence3 = receive
        GroupSequence3 ->
            GroupSequence3
    end,
    [{callback1_join, "GroupA", GroupPid1},
     {callback2_join, "GroupB", GroupPid2},
     {callback1_join, "GroupA", GroupPid2},
     {callback3_join, "GroupC", GroupPid3},
     {callback1_join, "GroupA", GroupPid3},
     {callback2_join, "GroupB", GroupPid3},
     {callback4_leave, "GroupA", GroupPid2},
     {callback5_leave, "GroupB", GroupPid2}] = Sequence1,
    [{callback4_leave, "GroupA", GroupPid1}] = Sequence2,
    % lists:foldl on lists:umerge list of groups means that
    % leave callbacks fire in GroupName order (upon a pid death)
    % (although the pid deaths are not delivered in-order,
    %  which is why order is imposed in the code above)
    [{callback4_leave, "GroupA", GroupPid3},
     {callback5_leave, "GroupB", GroupPid3},
     {callback6_leave, "GroupC", GroupPid3}] = Sequence3,
    erlang:exit(Pid, kill),
    ok = cpg:remove_join_callback("GroupA", Callback1),
    ok = cpg:remove_join_callback("GroupB", Callback2),
    ok = cpg:remove_join_callback("GroupC", Callback3),
    ok = cpg:remove_leave_callback("GroupA", Callback4),
    ok = cpg:remove_leave_callback("GroupB", Callback5),
    ok = cpg:remove_leave_callback("GroupC", Callback6),
    ok.

cpg_stop_test() ->
    ok = reltool_util:application_stop(cpg).

busy_pid() ->
    timer:sleep(1000),
    busy_pid().

index(Item, L)
    when is_list(L) ->
    index(Item, L, 1).
index(_, [], _) ->
    not_found;
index(Item, [Item | _], I) ->
    I;
index(Item, [_ | T], I) ->
    index(Item, T, I + 1).

