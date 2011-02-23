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
%%% Copyright (c) 2011, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011 Michael Truog
%%% @version 0.1.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(list_pg_data).
-author('mjtruog [at] gmail (dot) com').

-export([get_groups/0,
         get_groups/1,
         get_empty_groups/0,
         get_members/2,
         get_local_members/2,
         which_groups/1,
         get_closest_pid/2,
         get_closest_pid/3,
         get_random_pid/2,
         get_random_pid/3]).

get_groups() ->
    gen_server:call(list_pg, list_pg_data).

% send the groups as {list_pg_data, Groups} after Time milliseconds to self()
get_groups(Time) when is_integer(Time) ->
    erlang:send_after(Time, list_pg, {list_pg_data, self()}).

get_empty_groups() ->
    trie:new().

get_members(Name, Groups) when is_list(Name) ->
    case trie:find(Name, Groups) of
        error ->
            {error, {'no_such_group', Name}};
        {ok, []} ->
            [];
        {ok, Members} ->
            lists:foldl(fun({Pid, _}, T) ->
                            [Pid | T]
                        end, [], Members)
    end.

get_local_members(Name, Groups) when is_list(Name) ->
    case trie:find(Name, Groups) of
        error ->
            {error, {'no_such_group', Name}};
        {ok, []} ->
            [];
        {ok, Members} ->
            lists:foldl(fun({Pid, _}, T) ->
                            if
                                node(Pid) =:= node() ->
                                    [Pid | T];
                                true ->
                                    T
                            end
                        end, [], Members)
    end.

which_groups(Groups) ->
    trie:fetch_keys(Groups).

get_closest_pid(Name, Groups) when is_list(Name) ->
    case trie:find(Name, Groups) of
        error ->
            {error, {'no_such_group', Name}};
        {ok, []} ->
            {error, {'no_process', Name}};
        {ok, [{Pid, _}]} ->
            Pid;
        {ok, Members} ->
            case lists:splitwith(fun({Pid, _}) ->
                                     node(Pid) =:= node()
                                 end, Members) of
                {[], [{Pid, _}]} ->
                    Pid;
                {[], Remote} ->
                    {_, _, X} = erlang:now(),
                    {Pid, _} = lists:nth((X rem length(Remote)) + 1, Remote),
                    Pid;
                {[{Pid, _}], _} ->
                    Pid;
                {Local, _} ->
                    {_, _, X} = erlang:now(),
                    {Pid, _} = lists:nth((X rem length(Local)) + 1, Local),
                    Pid
            end
    end.

get_closest_pid(Name, Exclude, Groups) when is_list(Name), is_pid(Exclude) ->
    case trie:find(Name, Groups) of
        error ->
            {error, {'no_such_group', Name}};
        {ok, []} ->
            {error, {'no_process', Name}};
        {ok, [{Exclude, _}]} ->
            {error, {'no_process', Name}};
        {ok, [{Pid, _}]} ->
            Pid;
        {ok, Members} ->
            case lists:splitwith(fun({Pid, _}) ->
                                     node(Pid) =:= node()
                                 end, Members) of
                {[], [{Exclude, _}]} ->
                    {error, {'no_process', Name}};
                {[], [{Pid, _}]} ->
                    Pid;
                {[], L} ->
                    Remote = lists:filter(fun({P, _}) ->
                                              P =/= Exclude
                                          end, L),
                    if
                        Remote == [] ->
                            {error, {'no_process', Name}};
                        true ->
                            {_, _, X} = erlang:now(),
                            {Pid, _} = lists:nth((X rem length(Remote)) + 1,
                                                 Remote),
                            Pid
                    end;
                {[{Exclude, _}], Remote} ->
                    {_, _, X} = erlang:now(),
                    {Pid, _} = lists:nth((X rem length(Remote)) + 1, Remote),
                    Pid;
                {[{Pid, _}], _} ->
                    Pid;
                {L, Remote} ->
                    Local = lists:filter(fun({P, _}) ->
                                             P =/= Exclude
                                         end, L),
                    {_, _, X} = erlang:now(),
                    if
                        Local == [] ->
                            {Pid, _} = lists:nth((X rem length(Remote)) + 1,
                                                 Remote),
                            Pid;
                        true ->
                            {Pid, _} = lists:nth((X rem length(Local)) + 1,
                                                 Local),
                            Pid
                    end
            end
    end.

get_random_pid(Name, Groups) when is_list(Name) ->
    case trie:find(Name, Groups) of
        error ->
            {error, {'no_such_group', Name}};
        {ok, []} ->
            {error, {'no_process', Name}};
        {ok, [{Pid, _}]} ->
            Pid;
        {ok, Members} ->
            {_, _, X} = erlang:now(),
            {Pid, _} = lists:nth((X rem length(Members)) + 1, Members),
            Pid
    end.

get_random_pid(Name, Exclude, Groups) when is_list(Name), is_pid(Exclude) ->
    case trie:find(Name, Groups) of
        error ->
            {error, {'no_such_group', Name}};
        {ok, []} ->
            {error, {'no_process', Name}};
        {ok, [{Exclude, _}]} ->
            {error, {'no_process', Name}};
        {ok, [{Pid, _}]} ->
            Pid;
        {ok, L} ->
            Members = lists:filter(fun({P, _}) ->
                                       P =/= Exclude
                                   end, L),
            if
                Members == [] ->
                    {error, {'no_process', Name}};
                true ->
                    {_, _, X} = erlang:now(),
                    {Pid, _} = lists:nth((X rem length(Members)) + 1, Members),
                    Pid
            end
    end.

