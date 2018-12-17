%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CPG Callback Handling.==
%%% @end
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
%%% @author Michael Truog <mjtruog at protonmail dot com>
%%% @copyright 2014-2018 Michael Truog
%%% @version 1.7.5 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cpg_callbacks).
-author('mjtruog at protonmail dot com').

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

% for features specific to Erlang/OTP version 21.x (and later versions)
-ifdef(ERLANG_OTP_VERSION_19).
-else.
-ifdef(ERLANG_OTP_VERSION_20).
-else.
-ifdef(OTP_RELEASE).
-define(ERLANG_OTP_VERSION_21_FEATURES, true).
-else.
-error("Erlang/OTP version invalid").
-endif.
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
                    ?STACKTRACE(ErrorType, Error, ErrorStackTrace)
                        ?LOG_ERROR("callback failed ~p ~p~n~p",
                                   [ErrorType, Error, ErrorStackTrace])
                end
            end, L);
        error ->
            ok
    end.

