%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CPG Callback Handling.==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2014, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2014 Michael Truog
%%% @version 1.3.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cpg_callbacks).
-author('mjtruog [at] gmail (dot) com').

-export([stop_link/2,
         add_join/3,
         add_leave/3,
         remove_join/3,
         remove_leave/3,
         notify_join/4,
         notify_join/5,
         notify_leave/4,
         notify_leave/5]).

-record(state,
    {
        f_join,
        f_leave
    }).

-include("cpg_logging.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

stop_link(undefined, _) ->
    undefined;
stop_link(Pid, Reason)
    when is_pid(Pid) ->
    erlang:unlink(Pid),
    erlang:exit(Pid, Reason),
    undefined.

add_join(undefined, GroupName, F) ->
    add_join(start_link(), GroupName, F);
add_join(Pid, GroupName, F)
    when (is_function(F, 2) orelse is_function(F, 3)) ->
    Pid ! {add_join, GroupName, F},
    Pid.

add_leave(undefined, GroupName, F) ->
    add_leave(start_link(), GroupName, F);
add_leave(Pid, GroupName, F)
    when (is_function(F, 2) orelse is_function(F, 3)) ->
    Pid ! {add_leave, GroupName, F},
    Pid.

remove_join(undefined, _, _) ->
    undefined;
remove_join(Pid, GroupName, F)
    when (is_function(F, 2) orelse is_function(F, 3)) ->
    Pid ! {remove_join, GroupName, F},
    Pid.

remove_leave(undefined, _, _) ->
    undefined;
remove_leave(Pid, GroupName, F)
    when (is_function(F, 2) orelse is_function(F, 3)) ->
    Pid ! {remove_leave, GroupName, F},
    Pid.

notify_join(undefined, _, _, _) ->
    ok;
notify_join(Pid, GroupName, GroupPid, Reason)
    when is_pid(Pid), is_pid(GroupPid) ->
    Pid ! {notify_join, GroupName, GroupPid, Reason},
    ok.

notify_join(undefined, _, _, _, _) ->
    ok;
notify_join(Pid, _, _, _, 0)
    when is_pid(Pid) ->
    ok;
notify_join(Pid, GroupName, GroupPid, Reason, I)
    when is_pid(Pid), is_pid(GroupPid) ->
    Pid ! {notify_join, GroupName, GroupPid, Reason},
    notify_join(Pid, GroupName, GroupPid, Reason, I - 1).

notify_leave(undefined, _, _, _) ->
    ok;
notify_leave(Pid, GroupName, GroupPid, Reason)
    when is_pid(Pid), is_pid(GroupPid) ->
    Pid ! {notify_leave, GroupName, GroupPid, Reason},
    ok.

notify_leave(undefined, _, _, _, _) ->
    ok;
notify_leave(Pid, _, _, _, 0)
    when is_pid(Pid) ->
    ok;
notify_leave(Pid, GroupName, GroupPid, Reason, I)
    when is_pid(Pid), is_pid(GroupPid) ->
    Pid ! {notify_leave, GroupName, GroupPid, Reason},
    notify_leave(Pid, GroupName, GroupPid, Reason, I - 1).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

start_link() ->
    Fs = cpg_data:get_empty_groups(),
    erlang:spawn_link(fun() ->
        loop(#state{f_join = Fs,
                    f_leave = Fs})
    end).

loop(#state{f_join = FJoin,
            f_leave = FLeave} = State) ->
    receive
        {add_join, GroupName, F} ->
            loop(State#state{f_join = add(GroupName, F, FJoin)});
        {add_leave, GroupName, F} ->
            loop(State#state{f_leave = add(GroupName, F, FLeave)});
        {remove_join, GroupName, F} ->
            loop(State#state{f_join = remove(GroupName, F, FJoin)});
        {remove_leave, GroupName, F} ->
            loop(State#state{f_leave = remove(GroupName, F, FLeave)});
        {notify_join, GroupName, GroupPid, Reason} ->
            notify(GroupName, GroupPid, Reason, FJoin),
            loop(State);
        {notify_leave, GroupName, GroupPid, Reason} ->
            notify(GroupName, GroupPid, Reason, FLeave),
            loop(State)
    end.

add(GroupName, F, {DictI, FsData}) ->
    {DictI, DictI:update(GroupName, fun(V) ->
                             [F | V]
                         end, [F], FsData)}.

remove(GroupName, F, {DictI, FsData}) ->
    {DictI, DictI:update(GroupName, fun(V) ->
                             lists:delete(F, V)
                         end, [], FsData)}.

notify_f(F, GroupName, GroupPid, _)
    when is_function(F, 2) ->
    F(GroupName, GroupPid);
notify_f(F, GroupName, GroupPid, Reason)
    when is_function(F, 3) ->
    F(GroupName, GroupPid, Reason).

notify(GroupName, GroupPid, Reason, {DictI, FsData}) ->
    case DictI:find(GroupName, FsData) of
        {ok, L} ->
            lists:foreach(fun(F) ->
                try notify_f(F, GroupName, GroupPid, Reason)
                catch
                    Type:Error ->
                        ?LOG_ERROR("callback ~p: ~p~n~p",
                                   [Type, Error, erlang:get_stacktrace()])
                end
            end, L);
        error ->
            ok
    end.

