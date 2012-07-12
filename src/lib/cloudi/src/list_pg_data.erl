%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==list_pg Groups Handling.==
%%% Method of using list_pg instead of pg2.  The resulting process group
%%% handling is more scalable and more efficient.  However, usage is limited
%%% to string (list of integers) group names.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2011-2012, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011-2012 Michael Truog
%%% @version 1.0.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(list_pg_data).
-author('mjtruog [at] gmail (dot) com').

-export([get_groups/0,
         get_groups/1,
         get_groups/2,
         get_empty_groups/0,
         get_members/2,
         get_members/3,
         get_local_members/2,
         which_groups/1,
         get_closest_pid/2,
         get_closest_pid/3,
         get_random_pid/2,
         get_random_pid/3]).

-include("list_pg_data.hrl").
-include("list_pg_constants.hrl").

get_groups() ->
    gen_server:call(list_pg, list_pg_data).

get_groups(Scope) when is_atom(Scope) ->
    gen_server:call(Scope, list_pg_data);

% send the groups as {list_pg_data, Groups} after Time milliseconds to self()
get_groups(Time) when is_integer(Time) ->
    erlang:send_after(Time, list_pg, {list_pg_data, self()}).

get_groups(Scope, Time) when is_atom(Scope), is_integer(Time) ->
    erlang:send_after(Time, Scope, {list_pg_data, self()}).

get_empty_groups() ->
    trie:new().

get_members(GroupName, Groups) when is_list(GroupName) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, Pattern, #list_pg_data{local_count = 0,
                                    remote_count = 0}} ->
            {ok, Pattern, []};
        {ok, Pattern, #list_pg_data{local = Local,
                                    remote = Remote}} ->
            {ok, Pattern, lists:foldl(fun(#list_pg_data_pid{pid = Pid}, L) ->
                [Pid | L]
            end, [], Remote ++ Local)}
    end.

get_members(GroupName, Exclude, Groups)
    when is_list(GroupName), is_pid(Exclude) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #list_pg_data{local_count = 0,
                              remote_count = 0}} ->
            {error, {'no_process', GroupName}};
        {ok, Pattern, #list_pg_data{local = Local,
                                    remote = Remote}} ->
            Members = lists:foldl(fun(#list_pg_data_pid{pid = Pid}, L) ->
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

get_local_members(GroupName, Groups) when is_list(GroupName) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, Pattern, #list_pg_data{local = Local}} ->
            {ok, Pattern, lists:foldl(fun(#list_pg_data_pid{pid = Pid}, L) ->
                [Pid | L]
            end, [], Local)}
    end.

which_groups(Groups) ->
    trie:fetch_keys(Groups).

get_closest_pid(GroupName, Groups) when is_list(GroupName) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #list_pg_data{local_count = 0,
                              remote_count = 0}} ->
            {error, {'no_process', GroupName}};
        {ok, Pattern, #list_pg_data{local_count = 0,
                                    remote_count = RemoteCount,
                                    remote = Remote}} ->
            pick(RemoteCount, Remote, Pattern);
        {ok, Pattern, #list_pg_data{local_count = LocalCount,
                                    local = Local}} ->
            pick(LocalCount, Local, Pattern)
    end.

get_closest_pid(GroupName, Exclude, Groups)
    when is_list(GroupName), is_pid(Exclude) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #list_pg_data{local_count = 0,
                              remote_count = 0}} ->
            {error, {'no_process', GroupName}};
        {ok, Pattern, #list_pg_data{local_count = 0,
                                    remote_count = RemoteCount,
                                    remote = Remote}} ->
            pick(RemoteCount, Remote, Exclude, GroupName, Pattern);
        {ok, Pattern, #list_pg_data{local_count = LocalCount,
                                    local = Local,
                                    remote_count = RemoteCount,
                                    remote = Remote}} ->
            pick(LocalCount, Local, RemoteCount, Remote,
                 Exclude, GroupName, Pattern)
    end.

get_random_pid(GroupName, Groups) when is_list(GroupName) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #list_pg_data{local_count = 0,
                              remote_count = 0}} ->
            {error, {'no_process', GroupName}};
        {ok, Pattern, #list_pg_data{local_count = LocalCount,
                                    local = Local,
                                    remote_count = RemoteCount,
                                    remote = Remote}} ->
            pick(LocalCount + RemoteCount, Local ++ Remote, Pattern)
    end.

get_random_pid(GroupName, Exclude, Groups)
    when is_list(GroupName), is_pid(Exclude) ->
    case group_find(GroupName, Groups) of
        error ->
            {error, {'no_such_group', GroupName}};
        {ok, _, #list_pg_data{local_count = 0,
                              remote_count = 0}} ->
            {error, {'no_process', GroupName}};
        {ok, Pattern, #list_pg_data{local_count = LocalCount,
                                    local = Local,
                                    remote_count = RemoteCount,
                                    remote = Remote}} ->
            pick(LocalCount + RemoteCount, Local ++ Remote,
                 Exclude, GroupName, Pattern)
    end.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

% should names be matched with "*" interpreted as a wildcard within the
% trie holding the groups of processes

-ifdef(GROUP_NAME_PATTERN_MATCHING).
% matching with patterns
group_find(GroupName, Groups) ->
    try trie:find_match(GroupName, Groups) catch
        exit:badarg ->
            error
    end.
-else.
% matching without patterns
group_find(GroupName, Groups) ->
    case trie:find(GroupName, Groups) of
        {ok, Value} ->
            {ok, GroupName, Value};
        error ->
            error
    end.
-endif.

pick(N, L, Pattern) ->
    #list_pg_data_pid{pid = Pid} = lists:nth(random(N), L),
    {ok, Pattern, Pid}.

pick_i_exclude(_, _, [], [], _, GroupName, _) ->
    {error, {'no_process', GroupName}};

pick_i_exclude(I, Count, Filtered, [], _, _, Pattern) ->
    {ok, Pattern, lists:nth((I rem Count) + 1, Filtered)};

pick_i_exclude(I, Count, Filtered,
               [#list_pg_data_pid{pid = Exclude} | L],
               Exclude, GroupName, Pattern) ->
    pick_i_exclude(I, Count, Filtered, L, Exclude, GroupName, Pattern);
    
pick_i_exclude(I, Count, Filtered,
               [#list_pg_data_pid{pid = Pid} | L],
               Exclude, GroupName, Pattern) ->
    pick_i_exclude(I, Count + 1, [Pid | Filtered], L,
                   Exclude, GroupName, Pattern).

pick_i(I, I, Filtered,
       [#list_pg_data_pid{pid = Exclude} | L],
       Exclude, GroupName, Pattern) ->
    pick_i_exclude(I, erlang:length(Filtered), Filtered, L,
                   Exclude, GroupName, Pattern);

pick_i(I, I, _, [#list_pg_data_pid{pid = Pid} | _], _, _, Pattern) ->
    {ok, Pattern, Pid};

pick_i(I, Random, Filtered,
       [#list_pg_data_pid{pid = Exclude} | L],
       Exclude, GroupName, Pattern) ->
    pick_i(I + 1, Random, Filtered, L, Exclude, GroupName, Pattern);

pick_i(I, Random, Filtered,
       [#list_pg_data_pid{pid = Pid} | L],
       Exclude, GroupName, Pattern) ->
    pick_i(I + 1, Random, [Pid | Filtered], L, Exclude, GroupName, Pattern).

pick(0, [], _, GroupName, _) ->
    {error, {'no_process', GroupName}};

pick(N, L, Exclude, GroupName, Pattern) ->
    pick_i(1, random(N), [], L, Exclude, GroupName, Pattern).

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
    random:uniform(N).

