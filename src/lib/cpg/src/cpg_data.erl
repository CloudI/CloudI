%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CPG Groups Handling.==
%%% Method of using cpg instead of pg2.  The resulting process group
%%% handling is more scalable and more efficient.  However, usage is limited
%%% to string (list of integers) group names (unless the GROUP_STORAGE macro
%%% is changed and pattern matching is disabled).  The groups state is
%%% obtained from the cpg process for a specific scope and is then used with
%%% the functions provided here, so that contention for the cpg process
%%% can be avoided.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2011-2013, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011-2013 Michael Truog
%%% @version 1.2.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cpg_data).
-author('mjtruog [at] gmail (dot) com').

-export([get_groups/0,
         get_groups/1,
         get_groups/2,
         get_empty_groups/0,
         get_members/2,
         get_members/3,
         get_local_members/2,
         get_local_members/3,
         get_remote_members/2,
         get_remote_members/3,
         which_groups/1,
         get_closest_pid/2,
         get_closest_pid/3,
         get_furthest_pid/2,
         get_furthest_pid/3,
         get_random_pid/2,
         get_random_pid/3,
         get_local_pid/2,
         get_local_pid/3,
         get_remote_pid/2,
         get_remote_pid/3]).

-include("cpg_data.hrl").
-include("cpg_constants.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the group storage.===
%% This provides the internal representation of process groups so that
%% requests will not be blocked by the single process managing the scope
%% of the process groups.
%% @end
%%-------------------------------------------------------------------------

get_groups() ->
    gen_server:call(?DEFAULT_SCOPE, cpg_data).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the group storage for a particular scope or after a period of time.===
%% This provides the internal representation of process groups so that
%% requests will not be blocked by the single process managing the scope
%% of the process groups.
%% @end
%%-------------------------------------------------------------------------

get_groups(Scope) when is_atom(Scope) ->
    gen_server:call(Scope, cpg_data);

% send the groups as {cpg_data, Groups} after Time milliseconds to self()
get_groups(Time) when is_integer(Time) ->
    erlang:send_after(Time, ?DEFAULT_SCOPE, {cpg_data, self()}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the group storage for a particular scope after a period of time.===
%% This provides the internal representation of process groups so that
%% requests will not be blocked by the single process managing the scope
%% of the process groups.
%% @end
%%-------------------------------------------------------------------------

get_groups(Scope, Time) when is_atom(Scope), is_integer(Time) ->
    erlang:send_after(Time, Scope, {cpg_data, self()}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get empty group storage.===
%% @end
%%-------------------------------------------------------------------------

get_empty_groups() ->
    ?GROUP_STORAGE:new().

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the members of a specific group.===
%% @end
%%-------------------------------------------------------------------------

get_members(GroupName, Groups) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, Pattern, #cpg_data{local_count = 0,
                                remote_count = 0}} ->
            {ok, Pattern, []};
        {ok, Pattern, #cpg_data{local = Local,
                                remote = Remote}} ->
            {ok, Pattern, lists:foldl(fun(#cpg_data_pid{pid = Pid}, L) ->
                [Pid | L]
            end, [], Remote ++ Local)}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the members of a specific group while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

get_members(GroupName, Exclude, Groups)
    when is_pid(Exclude) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{local_count = 0,
                          remote_count = 0}} ->
            {error, {'no_process', GroupName}};
        {ok, Pattern, #cpg_data{local = Local,
                                remote = Remote}} ->
            Members = lists:foldl(fun(#cpg_data_pid{pid = Pid}, L) ->
                if
                    Pid =/= Exclude ->
                        [Pid | L];
                    true ->
                        L
                end
            end, [], Remote ++ Local),
            if
                Members == [] ->
                    {error, {'no_process', GroupName}};
                true ->
                    {ok, Pattern, Members}
            end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get only the local members of a specific group.===
%% @end
%%-------------------------------------------------------------------------

get_local_members(GroupName, Groups) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{local_count = 0}} ->
            {error, {'no_process', GroupName}};
        {ok, Pattern, #cpg_data{local = Local}} ->
            {ok, Pattern, lists:foldl(fun(#cpg_data_pid{pid = Pid}, L) ->
                [Pid | L]
            end, [], Local)}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get only the local members of a specific group while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

get_local_members(GroupName, Groups, Exclude)
    when is_pid(Exclude) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{local_count = 0}} ->
            {error, {'no_process', GroupName}};
        {ok, Pattern, #cpg_data{local = Local}} ->
            Members = lists:foldl(fun(#cpg_data_pid{pid = Pid}, L) ->
                if
                    Pid =/= Exclude ->
                        [Pid | L];
                    true ->
                        L
                end
            end, [], Local),
            if
                Members == [] ->
                    {error, {'no_process', GroupName}};
                true ->
                    {ok, Pattern, Members}
            end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get only the remote members of a specific group.===
%% @end
%%-------------------------------------------------------------------------

get_remote_members(GroupName, Groups) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{remote_count = 0}} ->
            {error, {'no_process', GroupName}};
        {ok, Pattern, #cpg_data{remote = Remote}} ->
            {ok, Pattern, lists:foldl(fun(#cpg_data_pid{pid = Pid}, L) ->
                [Pid | L]
            end, [], Remote)}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get only the remote members of a specific group while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

get_remote_members(GroupName, Groups, Exclude)
    when is_pid(Exclude) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{remote_count = 0}} ->
            {error, {'no_process', GroupName}};
        {ok, Pattern, #cpg_data{remote = Remote}} ->
            Members = lists:foldl(fun(#cpg_data_pid{pid = Pid}, L) ->
                if
                    Pid =/= Exclude ->
                        [Pid | L];
                    true ->
                        L
                end
            end, [], Remote),
            if
                Members == [] ->
                    {error, {'no_process', GroupName}};
                true ->
                    {ok, Pattern, Members}
            end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get all the groups currently defined.===
%% @end
%%-------------------------------------------------------------------------

which_groups(Groups) ->
    ?GROUP_STORAGE:fetch_keys(Groups).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a group member, with local pids given priority.===
%% @end
%%-------------------------------------------------------------------------

get_closest_pid(GroupName, Groups) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{local_count = 0,
                          remote_count = 0}} ->
            {error, {'no_process', GroupName}};
        {ok, Pattern, #cpg_data{local_count = 0,
                                remote_count = RemoteCount,
                                remote = Remote}} ->
            pick(RemoteCount, Remote, Pattern);
        {ok, Pattern, #cpg_data{local_count = LocalCount,
                                local = Local}} ->
            pick(LocalCount, Local, Pattern)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a group member, with local pids given priority while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

get_closest_pid(GroupName, Exclude, Groups)
    when is_pid(Exclude) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{local_count = 0,
                          remote_count = 0}} ->
            {error, {'no_process', GroupName}};
        {ok, Pattern, #cpg_data{local_count = 0,
                                remote_count = RemoteCount,
                                remote = Remote}} ->
            pick(RemoteCount, Remote,
                 Exclude, GroupName, Pattern);
        {ok, Pattern, #cpg_data{local_count = LocalCount,
                                local = Local,
                                remote_count = RemoteCount,
                                remote = Remote}} ->
            pick(LocalCount, Local, RemoteCount, Remote,
                 Exclude, GroupName, Pattern)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a group member, with remote pids given priority.===
%% @end
%%-------------------------------------------------------------------------

get_furthest_pid(GroupName, Groups) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{remote_count = 0,
                          local_count = 0}} ->
            {error, {'no_process', GroupName}};
        {ok, Pattern, #cpg_data{remote_count = 0,
                                local_count = LocalCount,
                                local = Local}} ->
            pick(LocalCount, Local, Pattern);
        {ok, Pattern, #cpg_data{remote_count = RemoteCount,
                                remote = Remote}} ->
            pick(RemoteCount, Remote, Pattern)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a group member, with remote pids given priority while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

get_furthest_pid(GroupName, Exclude, Groups)
    when is_pid(Exclude) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{remote_count = 0,
                          local_count = 0}} ->
            {error, {'no_process', GroupName}};
        {ok, Pattern, #cpg_data{remote_count = 0,
                                local_count = LocalCount,
                                local = Local}} ->
            pick(LocalCount, Local, Exclude, GroupName, Pattern);
        {ok, Pattern, #cpg_data{remote_count = RemoteCount,
                                remote = Remote,
                                local_count = LocalCount,
                                local = Local}} ->
            pick(RemoteCount, Remote, LocalCount, Local,
                 Exclude, GroupName, Pattern)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a group member.===
%% @end
%%-------------------------------------------------------------------------

get_random_pid(GroupName, Groups) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{local_count = 0,
                          remote_count = 0}} ->
            {error, {'no_process', GroupName}};
        {ok, Pattern, #cpg_data{local_count = LocalCount,
                                local = Local,
                                remote_count = RemoteCount,
                                remote = Remote}} ->
            pick(LocalCount + RemoteCount, Local ++ Remote, Pattern)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a group member while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

get_random_pid(GroupName, Exclude, Groups)
    when is_pid(Exclude) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{local_count = 0,
                          remote_count = 0}} ->
            {error, {'no_process', GroupName}};
        {ok, Pattern, #cpg_data{local_count = LocalCount,
                                local = Local,
                                remote_count = RemoteCount,
                                remote = Remote}} ->
            pick(LocalCount + RemoteCount, Local ++ Remote,
                 Exclude, GroupName, Pattern)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a local group member.===
%% @end
%%-------------------------------------------------------------------------

get_local_pid(GroupName, Groups) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{local_count = 0}} ->
            {error, {'no_process', GroupName}};
        {ok, Pattern, #cpg_data{local_count = LocalCount,
                                local = Local}} ->
            pick(LocalCount, Local, Pattern)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a local group member while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

get_local_pid(GroupName, Exclude, Groups)
    when is_pid(Exclude) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{local_count = 0}} ->
            {error, {'no_process', GroupName}};
        {ok, Pattern, #cpg_data{local_count = LocalCount,
                                local = Local}} ->
            pick(LocalCount, Local, Exclude, GroupName, Pattern)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a remote group member.===
%% @end
%%-------------------------------------------------------------------------

get_remote_pid(GroupName, Groups) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{remote_count = 0}} ->
            {error, {'no_process', GroupName}};
        {ok, Pattern, #cpg_data{remote_count = RemoteCount,
                                remote = Remote}} ->
            pick(RemoteCount, Remote, Pattern)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a remote group member while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

get_remote_pid(GroupName, Exclude, Groups)
    when is_pid(Exclude) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{remote_count = 0}} ->
            {error, {'no_process', GroupName}};
        {ok, Pattern, #cpg_data{remote_count = RemoteCount,
                                remote = Remote}} ->
            pick(RemoteCount, Remote, Exclude, GroupName, Pattern)
    end.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

% should names be matched with "*" interpreted as a wildcard within the
% trie holding the groups of processes

-ifdef(GROUP_NAME_PATTERN_MATCHING).
% matching with patterns
group_find(GroupName, Groups) ->
    try ?GROUP_STORAGE:find_match(GroupName, Groups) catch
        exit:badarg ->
            error
    end.
-else.
% matching without patterns
group_find(GroupName, Groups) ->
    case ?GROUP_STORAGE:find(GroupName, Groups) of
        {ok, Value} ->
            {ok, GroupName, Value};
        error ->
            error
    end.
-endif.

pick(N, L, Pattern) ->
    #cpg_data_pid{pid = Pid} = lists:nth(random(N), L),
    {ok, Pattern, Pid}.

pick_i(I, I, _, [], [], _, GroupName, _) ->
    {error, {'no_process', GroupName}};

pick_i(I, I, Length, Filtered, [], _, _, Pattern) ->
    {ok, Pattern, lists:nth((I rem Length) + 1, Filtered)};

pick_i(I, I, Length, Filtered,
       [#cpg_data_pid{pid = Exclude} | L],
       Exclude, GroupName, Pattern) ->
    pick_i(I, I, Length, Filtered, L, Exclude, GroupName, Pattern);

pick_i(I, I, _, _, [#cpg_data_pid{pid = Pid} | _], _, _, Pattern) ->
    {ok, Pattern, Pid};

pick_i(I, Random, Length, Filtered,
       [#cpg_data_pid{pid = Exclude} | L],
       Exclude, GroupName, Pattern) ->
    pick_i(I + 1, Random, Length, Filtered, L, Exclude, GroupName, Pattern);

pick_i(I, Random, Length, Filtered,
       [#cpg_data_pid{pid = Pid} | L],
       Exclude, GroupName, Pattern) ->
    pick_i(I + 1, Random, Length + 1, [Pid | Filtered],
           L, Exclude, GroupName, Pattern).

pick(0, [], _, GroupName, _) ->
    {error, {'no_process', GroupName}};

pick(N, L, Exclude, GroupName, Pattern) ->
    pick_i(1, random(N), 0, [], L, Exclude, GroupName, Pattern).

pick(0, [], N2, L2, Exclude, GroupName, Pattern) ->
    pick(N2, L2, Exclude, GroupName, Pattern);

pick(N1, L1, N2, L2, Exclude, GroupName, Pattern) ->
    case pick(N1, L1, Exclude, GroupName, Pattern) of
        {error, _} ->
            pick(N2, L2, Exclude, GroupName, Pattern);
        {ok, _, _} = Success ->
            Success
    end.

random(N) ->
    quickrand:uniform(N).

