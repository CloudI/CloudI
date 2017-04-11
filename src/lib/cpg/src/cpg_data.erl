%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CPG Groups Handling.==
%%% Method of using cpg instead of pg2.  The resulting process group
%%% handling is more scalable and more efficient.  The groups state is
%%% obtained from the cpg process for a specific scope and is then used with
%%% the functions provided here, so that contention for the cpg process
%%% can be avoided.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2011-2017, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011-2017 Michael Truog
%%% @version 1.7.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cpg_data).
-author('mjtruog [at] gmail (dot) com').

-export([get_groups/0,
         get_groups/1,
         get_groups/2,
         get_groups/3,
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
         get_remote_pid/3,
         get_oldest_pid/2,
         get_oldest_pid/3,
         get_local_oldest_pid/2,
         get_local_oldest_pid/3,
         get_remote_oldest_pid/2,
         get_remote_oldest_pid/3,
         get_newest_pid/2,
         get_newest_pid/3,
         get_local_newest_pid/2,
         get_local_newest_pid/3,
         get_remote_newest_pid/2,
         get_remote_newest_pid/3]).

-include("cpg_data.hrl").
-include("cpg_constants.hrl").

% GroupsData == GroupName -> #cpg_data{} lookup, using the DictI module
-type state() :: {DictI :: module(), GroupsData :: any()}.
-export_type([state/0]).

-type get_members_return() ::
    {ok, cpg:name(), list(pid())} |
    {error, {no_such_group, cpg:name()}}.
-type get_pid_error_reason() ::
    {no_process, cpg:name()} |
    {no_such_group, cpg:name()}.
-export_type([get_members_return/0,
              get_pid_error_reason/0]).

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

-spec get_groups() ->
    state().

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

-spec get_groups(atom() | non_neg_integer()) ->
    state() | reference().

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

-spec get_groups(Scope :: atom(),
                 Time :: non_neg_integer()) ->
    reference().

get_groups(Scope, Time) when is_atom(Scope), is_integer(Time) ->
    erlang:send_after(Time, Scope, {cpg_data, self()}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the group storage for a particular scope and destination after a period of time.===
%% This provides the internal representation of process groups so that
%% requests will not be blocked by the single process managing the scope
%% of the process groups.
%% @end
%%-------------------------------------------------------------------------

-spec get_groups(Scope :: atom(),
                 Destination :: pid() | atom(),
                 Time :: non_neg_integer()) ->
    reference().

get_groups(Scope, Destination, Time)
    when is_atom(Scope), (is_pid(Destination) orelse is_atom(Destination)),
         is_integer(Time) ->
    erlang:send_after(Time, Scope, {cpg_data, Destination}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get empty group storage.===
%% @end
%%-------------------------------------------------------------------------

-spec get_empty_groups() ->
    state().

get_empty_groups() ->
    DictI = cpg_app:group_storage(),
    {DictI, DictI:new()}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the members of a specific group.===
%% All members are ordered from newest to oldest, based on the group
%% membership surviving netsplits (join order, not pid creation time).
%% @end
%%-------------------------------------------------------------------------

-spec get_members(GroupName :: cpg:name(),
                  Groups :: state()) ->
    get_members_return().

get_members(GroupName, Groups) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{history = []}} ->
            % to keep return values consistent with pg2
            {error, {'no_such_group', GroupName}};
        {ok, Pattern, #cpg_data{history = History}} ->
            {ok, Pattern, History}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the members of a specific group while excluding a specific pid.===
%% All members are ordered from newest to oldest, based on the group
%% membership surviving netsplits (join order, not pid creation time).
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_members(GroupName :: cpg:name(),
                  Exclude :: pid(),
                  Groups :: state()) ->
    get_members_return().

get_members(GroupName, Exclude, Groups)
    when is_pid(Exclude) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{history = []}} ->
            % to keep return values consistent with pg2
            {error, {'no_such_group', GroupName}};
        {ok, Pattern, #cpg_data{history = History}} ->
            Members = [Pid || Pid <- History,
                       Pid =/= Exclude],
            if
                Members == [] ->
                    % to keep return values consistent with pg2
                    {error, {'no_such_group', GroupName}};
                true ->
                    {ok, Pattern, Members}
            end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get only the local members of a specific group.===
%% All members are ordered from newest to oldest, based on the 
%% join order, not pid creation time.
%% @end
%%-------------------------------------------------------------------------

-spec get_local_members(GroupName :: cpg:name(),
                        Groups :: state()) ->
    get_members_return().

get_local_members(GroupName, Groups) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{local_count = 0}} ->
            % to keep return values consistent with pg2
            {error, {'no_such_group', GroupName}};
        {ok, Pattern, #cpg_data{local = Local}} ->
            {ok, Pattern, Local}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get only the local members of a specific group while excluding a specific pid.===
%% All members are ordered from newest to oldest, based on the 
%% join order, not pid creation time.
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_local_members(GroupName :: cpg:name(),
                        Exclude :: pid(),
                        Groups :: state()) ->
    get_members_return().

get_local_members(GroupName, Exclude, Groups)
    when is_pid(Exclude) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{local_count = 0}} ->
            % to keep return values consistent with pg2
            {error, {'no_such_group', GroupName}};
        {ok, Pattern, #cpg_data{local = Local}} ->
            Members = [Pid || Pid <- Local, Pid =/= Exclude],
            if
                Members == [] ->
                    % to keep return values consistent with pg2
                    {error, {'no_such_group', GroupName}};
                true ->
                    {ok, Pattern, Members}
            end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get only the remote members of a specific group.===
%% All members are ordered from newest to oldest, based on the group
%% membership surviving netsplits (join order, not pid creation time).
%% @end
%%-------------------------------------------------------------------------

-spec get_remote_members(GroupName :: cpg:name(),
                         Groups :: state()) ->
    get_members_return().

get_remote_members(GroupName, Groups) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{remote_count = 0}} ->
            % to keep return values consistent with pg2
            {error, {'no_such_group', GroupName}};
        {ok, Pattern, #cpg_data{remote = Remote}} ->
            {ok, Pattern, Remote}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get only the remote members of a specific group while excluding a specific pid.===
%% All members are ordered from newest to oldest, based on the group
%% membership surviving netsplits (join order, not pid creation time).
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_remote_members(GroupName :: cpg:name(),
                         Exclude :: pid(),
                         Groups :: state()) ->
    get_members_return().

get_remote_members(GroupName, Exclude, Groups)
    when is_pid(Exclude) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{remote_count = 0}} ->
            % to keep return values consistent with pg2
            {error, {'no_such_group', GroupName}};
        {ok, Pattern, #cpg_data{remote = Remote}} ->
            Members = [Pid || Pid <- Remote, Pid =/= Exclude],
            if
                Members == [] ->
                    % to keep return values consistent with pg2
                    {error, {'no_such_group', GroupName}};
                true ->
                    {ok, Pattern, Members}
            end
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get all the groups currently defined.===
%% @end
%%-------------------------------------------------------------------------

-spec which_groups(state()) ->
    list(cpg:name()).

which_groups({DictI, GroupsData}) ->
    DictI:fetch_keys(GroupsData).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a group member, with local pids given priority.===
%% @end
%%-------------------------------------------------------------------------

-spec get_closest_pid(GroupName :: cpg:name(),
                      Groups :: state()) ->
    {ok, cpg:name(), pid()} |
    {error, get_pid_error_reason()}.

get_closest_pid(GroupName, Groups) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{history = []}} ->
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

-spec get_closest_pid(GroupName :: cpg:name(),
                      Exclude :: pid(),
                      Groups :: state()) ->
    {ok, cpg:name(), pid()} |
    {error, get_pid_error_reason()}.

get_closest_pid(GroupName, Exclude, Groups)
    when is_pid(Exclude) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{history = []}} ->
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

-spec get_furthest_pid(GroupName :: cpg:name(),
                       Groups :: state()) ->
    {ok, cpg:name(), pid()} |
    {error, get_pid_error_reason()}.

get_furthest_pid(GroupName, Groups) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{history = []}} ->
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

-spec get_furthest_pid(GroupName :: cpg:name(),
                       Exclude :: pid(),
                       Groups :: state()) ->
    {ok, cpg:name(), pid()} |
    {error, get_pid_error_reason()}.

get_furthest_pid(GroupName, Exclude, Groups)
    when is_pid(Exclude) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{history = []}} ->
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

-spec get_random_pid(GroupName :: cpg:name(),
                     Groups :: state()) ->
    {ok, cpg:name(), pid()} |
    {error, get_pid_error_reason()}.

get_random_pid(GroupName, Groups) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{history = []}} ->
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

-spec get_random_pid(GroupName :: cpg:name(),
                     Exclude :: pid(),
                     Groups :: state()) ->
    {ok, cpg:name(), pid()} |
    {error, get_pid_error_reason()}.

get_random_pid(GroupName, Exclude, Groups)
    when is_pid(Exclude) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{history = []}} ->
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

-spec get_local_pid(GroupName :: cpg:name(),
                    Groups :: state()) ->
    {ok, cpg:name(), pid()} |
    {error, get_pid_error_reason()}.

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

-spec get_local_pid(GroupName :: cpg:name(),
                    Exclude :: pid(),
                    Groups :: state()) ->
    {ok, cpg:name(), pid()} |
    {error, get_pid_error_reason()}.

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

-spec get_remote_pid(GroupName :: cpg:name(),
                     Groups :: state()) ->
    {ok, cpg:name(), pid()} |
    {error, get_pid_error_reason()}.

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

-spec get_remote_pid(GroupName :: cpg:name(),
                     Exclude :: pid(),
                     Groups :: state()) ->
    {ok, cpg:name(), pid()} |
    {error, get_pid_error_reason()}.

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

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the oldest group member.===
%% @end
%%-------------------------------------------------------------------------

-spec get_oldest_pid(GroupName :: cpg:name(),
                     Groups :: state()) ->
    {ok, cpg:name(), pid()} |
    {error, get_pid_error_reason()}.

get_oldest_pid(GroupName, Groups) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{history = []}} ->
            {error, {'no_process', GroupName}};
        {ok, Pattern, #cpg_data{history = History}} ->
            history_oldest(History, Pattern)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the oldest group member while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_oldest_pid(GroupName :: cpg:name(),
                     Exclude :: pid(),
                     Groups :: state()) ->
    {ok, cpg:name(), pid()} |
    {error, get_pid_error_reason()}.

get_oldest_pid(GroupName, Exclude, Groups)
    when is_pid(Exclude) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{history = []}} ->
            {error, {'no_process', GroupName}};
        {ok, Pattern, #cpg_data{history = History}} ->
            history_oldest(History, Exclude, GroupName, Pattern)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the oldest local group member.===
%% @end
%%-------------------------------------------------------------------------

-spec get_local_oldest_pid(GroupName :: cpg:name(),
                           Groups :: state()) ->
    {ok, cpg:name(), pid()} |
    {error, get_pid_error_reason()}.

get_local_oldest_pid(GroupName, Groups) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{local_count = 0}} ->
            {error, {'no_process', GroupName}};
        {ok, Pattern, #cpg_data{history = History}} ->
            history_local_oldest(History, GroupName, Pattern)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the oldest local group member while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_local_oldest_pid(GroupName :: cpg:name(),
                           Exclude :: pid(),
                           Groups :: state()) ->
    {ok, cpg:name(), pid()} |
    {error, get_pid_error_reason()}.

get_local_oldest_pid(GroupName, Exclude, Groups)
    when is_pid(Exclude) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{local_count = 0}} ->
            {error, {'no_process', GroupName}};
        {ok, Pattern, #cpg_data{history = History}} ->
            history_local_oldest(History, Exclude, GroupName, Pattern)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the oldest remote group member.===
%% @end
%%-------------------------------------------------------------------------

-spec get_remote_oldest_pid(GroupName :: cpg:name(),
                            Groups :: state()) ->
    {ok, cpg:name(), pid()} |
    {error, get_pid_error_reason()}.

get_remote_oldest_pid(GroupName, Groups) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{remote_count = 0}} ->
            {error, {'no_process', GroupName}};
        {ok, Pattern, #cpg_data{history = History}} ->
            history_remote_oldest(History, GroupName, Pattern)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the oldest remote group member while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_remote_oldest_pid(GroupName :: cpg:name(),
                            Exclude :: pid(),
                            Groups :: state()) ->
    {ok, cpg:name(), pid()} |
    {error, get_pid_error_reason()}.

get_remote_oldest_pid(GroupName, Exclude, Groups)
    when is_pid(Exclude) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{remote_count = 0}} ->
            {error, {'no_process', GroupName}};
        {ok, Pattern, #cpg_data{history = History}} ->
            history_remote_oldest(History, Exclude, GroupName, Pattern)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the newest group member.===
%% @end
%%-------------------------------------------------------------------------

-spec get_newest_pid(GroupName :: cpg:name(),
                     Groups :: state()) ->
    {ok, cpg:name(), pid()} |
    {error, get_pid_error_reason()}.

get_newest_pid(GroupName, Groups) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{history = []}} ->
            {error, {'no_process', GroupName}};
        {ok, Pattern, #cpg_data{history = History}} ->
            history_newest(History, Pattern)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the newest group member while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_newest_pid(GroupName :: cpg:name(),
                     Exclude :: pid(),
                     Groups :: state()) ->
    {ok, cpg:name(), pid()} |
    {error, get_pid_error_reason()}.

get_newest_pid(GroupName, Exclude, Groups)
    when is_pid(Exclude) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{history = []}} ->
            {error, {'no_process', GroupName}};
        {ok, Pattern, #cpg_data{history = History}} ->
            history_newest(History, Exclude, GroupName, Pattern)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the newest local group member.===
%% @end
%%-------------------------------------------------------------------------

-spec get_local_newest_pid(GroupName :: cpg:name(),
                           Groups :: state()) ->
    {ok, cpg:name(), pid()} |
    {error, get_pid_error_reason()}.

get_local_newest_pid(GroupName, Groups) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{local_count = 0}} ->
            {error, {'no_process', GroupName}};
        {ok, Pattern, #cpg_data{history = History}} ->
            history_local_newest(History, GroupName, Pattern)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the newest local group member while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_local_newest_pid(GroupName :: cpg:name(),
                           Exclude :: pid(),
                           Groups :: state()) ->
    {ok, cpg:name(), pid()} |
    {error, get_pid_error_reason()}.

get_local_newest_pid(GroupName, Exclude, Groups)
    when is_pid(Exclude) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{local_count = 0}} ->
            {error, {'no_process', GroupName}};
        {ok, Pattern, #cpg_data{history = History}} ->
            history_local_newest(History, Exclude, GroupName, Pattern)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the newest remote group member.===
%% @end
%%-------------------------------------------------------------------------

-spec get_remote_newest_pid(GroupName :: cpg:name(),
                            Groups :: state()) ->
    {ok, cpg:name(), pid()} |
    {error, get_pid_error_reason()}.

get_remote_newest_pid(GroupName, Groups) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{remote_count = 0}} ->
            {error, {'no_process', GroupName}};
        {ok, Pattern, #cpg_data{history = History}} ->
            history_remote_newest(History, GroupName, Pattern)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the newest remote group member while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_remote_newest_pid(GroupName :: cpg:name(),
                            Exclude :: pid(),
                            Groups :: state()) ->
    {ok, cpg:name(), pid()} |
    {error, get_pid_error_reason()}.

get_remote_newest_pid(GroupName, Exclude, Groups)
    when is_pid(Exclude) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #cpg_data{remote_count = 0}} ->
            {error, {'no_process', GroupName}};
        {ok, Pattern, #cpg_data{history = History}} ->
            history_remote_newest(History, Exclude, GroupName, Pattern)
    end.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

% should names be matched with "*" interpreted as a wildcard within the
% trie holding the groups of processes

% matching with patterns
group_find(GroupName, {trie, GroupsData}) ->
    try trie:find_match(GroupName, GroupsData) catch
        exit:badarg ->
            error
    end;
% matching without patterns
group_find(GroupName, {DictI, GroupsData}) ->
    case DictI:find(GroupName, GroupsData) of
        {ok, Value} ->
            {ok, GroupName, Value};
        error ->
            error
    end.

pick(1, [Pid], Pattern) ->
    {ok, Pattern, Pid};

pick(N, L, Pattern) ->
    Pid = lists:nth(random(N), L),
    {ok, Pattern, Pid}.

pick_i(I, I, _, [], [], _, GroupName, _) ->
    {error, {'no_process', GroupName}};

pick_i(I, I, 1, [Pid], [], _, _, Pattern) ->
    {ok, Pattern, Pid};

pick_i(I, I, Length, Filtered, [], _, _, Pattern) ->
    {ok, Pattern, lists:nth((I rem Length) + 1, Filtered)};

pick_i(I, I, Length, Filtered,
       [Exclude | L], Exclude, GroupName, Pattern) ->
    pick_i(I, I, Length, Filtered, L, Exclude, GroupName, Pattern);

pick_i(I, I, _, _, [Pid | _], _, _, Pattern) ->
    {ok, Pattern, Pid};

pick_i(I, Random, Length, Filtered,
       [Exclude | L], Exclude, GroupName, Pattern) ->
    pick_i(I + 1, Random, Length, Filtered, L, Exclude, GroupName, Pattern);

pick_i(I, Random, Length, Filtered,
       [Pid | L], Exclude, GroupName, Pattern) ->
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

history_oldest([_ | _] = History, Pattern) ->
    [Pid | _] = lists:reverse(History),
    {ok, Pattern, Pid}.

history_oldest_pid([], _, GroupName, _) ->
    {error, {'no_process', GroupName}};
history_oldest_pid([Exclude | History], Exclude, GroupName, Pattern) ->
    history_oldest_pid(History, Exclude, GroupName, Pattern);
history_oldest_pid([Pid | _], _, _, Pattern) ->
    {ok, Pattern, Pid}.
history_oldest([_ | _] = History, Exclude, GroupName, Pattern) ->
    history_oldest_pid(lists:reverse(History), Exclude, GroupName, Pattern).

history_local_oldest_pid([], _, GroupName, _) ->
    {error, {'no_process', GroupName}};
history_local_oldest_pid([Pid | _], Node, _, Pattern)
    when node(Pid) =:= Node ->
    {ok, Pattern, Pid};
history_local_oldest_pid([_ | History], Node, GroupName, Pattern) ->
    history_local_oldest_pid(History, Node, GroupName, Pattern).
history_local_oldest([_ | _] = History, GroupName, Pattern) ->
    history_local_oldest_pid(lists:reverse(History), node(),
                             GroupName, Pattern).

history_local_oldest_pid([], _, _, GroupName, _) ->
    {error, {'no_process', GroupName}};
history_local_oldest_pid([Exclude | History], Exclude, Node,
                         GroupName, Pattern) ->
    history_local_oldest_pid(History, Exclude, Node, GroupName, Pattern);
history_local_oldest_pid([Pid | _], _, Node, _, Pattern)
    when node(Pid) =:= Node ->
    {ok, Pattern, Pid};
history_local_oldest_pid([_ | History], Exclude, Node, GroupName, Pattern) ->
    history_local_oldest_pid(History, Exclude, Node, GroupName, Pattern).
history_local_oldest([_ | _] = History, Exclude, GroupName, Pattern) ->
    history_local_oldest_pid(lists:reverse(History), Exclude, node(),
                             GroupName, Pattern).

history_remote_oldest_pid([], _, GroupName, _) ->
    {error, {'no_process', GroupName}};
history_remote_oldest_pid([Pid | _], Node, _, Pattern)
    when node(Pid) =/= Node ->
    {ok, Pattern, Pid};
history_remote_oldest_pid([_ | History], Node, GroupName, Pattern) ->
    history_remote_oldest_pid(History, Node, GroupName, Pattern).
history_remote_oldest([_ | _] = History, GroupName, Pattern) ->
    history_remote_oldest_pid(lists:reverse(History), node(),
                              GroupName, Pattern).

history_remote_oldest_pid([], _, _, GroupName, _) ->
    {error, {'no_process', GroupName}};
history_remote_oldest_pid([Exclude | History], Exclude, Node,
                          GroupName, Pattern) ->
    history_remote_oldest_pid(History, Exclude, Node, GroupName, Pattern);
history_remote_oldest_pid([Pid | _], _, Node, _, Pattern)
    when node(Pid) =/= Node ->
    {ok, Pattern, Pid};
history_remote_oldest_pid([_ | History], Exclude, Node, GroupName, Pattern) ->
    history_remote_oldest_pid(History, Exclude, Node, GroupName, Pattern).
history_remote_oldest([_ | _] = History, Exclude, GroupName, Pattern) ->
    history_remote_oldest_pid(lists:reverse(History), Exclude, node(),
                              GroupName, Pattern).

history_newest([Pid | _], Pattern) ->
    {ok, Pattern, Pid}.

history_newest_pid([], _, GroupName, _) ->
    {error, {'no_process', GroupName}};
history_newest_pid([Exclude | History], Exclude, GroupName, Pattern) ->
    history_newest_pid(History, Exclude, GroupName, Pattern);
history_newest_pid([Pid | _], _, _, Pattern) ->
    {ok, Pattern, Pid}.
history_newest([_ | _] = History, Exclude, GroupName, Pattern) ->
    history_newest_pid(History, Exclude, GroupName, Pattern).

history_local_newest_pid([], _, GroupName, _) ->
    {error, {'no_process', GroupName}};
history_local_newest_pid([Pid | _], Node, _, Pattern)
    when node(Pid) =:= Node ->
    {ok, Pattern, Pid};
history_local_newest_pid([_ | History], Node, GroupName, Pattern) ->
    history_local_newest_pid(History, Node, GroupName, Pattern).
history_local_newest([_ | _] = History, GroupName, Pattern) ->
    history_local_newest_pid(History, node(), GroupName, Pattern).

history_local_newest_pid([], _, _, GroupName, _) ->
    {error, {'no_process', GroupName}};
history_local_newest_pid([Exclude | History], Exclude, Node,
                         GroupName, Pattern) ->
    history_local_newest_pid(History, Exclude, Node, GroupName, Pattern);
history_local_newest_pid([Pid | _], _, Node, _, Pattern)
    when node(Pid) =:= Node ->
    {ok, Pattern, Pid};
history_local_newest_pid([_ | History], Exclude, Node, GroupName, Pattern) ->
    history_local_newest_pid(History, Exclude, Node, GroupName, Pattern).
history_local_newest([_ | _] = History, Exclude, GroupName, Pattern) ->
    history_local_newest_pid(History, Exclude, node(), GroupName, Pattern).

history_remote_newest_pid([], _, GroupName, _) ->
    {error, {'no_process', GroupName}};
history_remote_newest_pid([Pid | _], Node, _, Pattern)
    when node(Pid) =/= Node ->
    {ok, Pattern, Pid};
history_remote_newest_pid([_ | History], Node, GroupName, Pattern) ->
    history_remote_newest_pid(History, Node, GroupName, Pattern).
history_remote_newest([_ | _] = History, GroupName, Pattern) ->
    history_remote_newest_pid(History, node(), GroupName, Pattern).

history_remote_newest_pid([], _, _, GroupName, _) ->
    {error, {'no_process', GroupName}};
history_remote_newest_pid([Exclude | History], Exclude, Node,
                          GroupName, Pattern) ->
    history_remote_newest_pid(History, Exclude, Node, GroupName, Pattern);
history_remote_newest_pid([Pid | _], _, Node, _, Pattern)
    when node(Pid) =/= Node ->
    {ok, Pattern, Pid};
history_remote_newest_pid([_ | History], Exclude, Node, GroupName, Pattern) ->
    history_remote_newest_pid(History, Exclude, Node, GroupName, Pattern).
history_remote_newest([_ | _] = History, Exclude, GroupName, Pattern) ->
    history_remote_newest_pid(History, Exclude, node(), GroupName, Pattern).

-compile({inline, [{random,1}]}).

random(N) ->
    quickrand:uniform(N).

