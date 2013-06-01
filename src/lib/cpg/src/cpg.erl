%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Process Groups (CPG)==
%%% Based on the pg2 module in the Erlang OTP kernel application
%%% (lib/kernel-x.x.x/src/pg2.erl).
%%% cpg relies on distributed Erlang for node communication, which means
%%% a fully connected network topology is created.  With distributed Erlang
%%% Erlang pids either exist on the local node or a remote node
%%% (which shares a connection with the local node,
%%%  so only 1 node hop is necessary in the worst case).
%%% @end
%%% The pg2 module copyright is below:
%%%
%%% Copyright (c) 2011-2013 Michael Truog. All Rights Reserved.
%%%
%%% %CopyrightBegin%
%%%
%%% Copyright Ericsson AB 1997-2010. All Rights Reserved.
%%%
%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved online at http://www.erlang.org/.
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%
%%% %CopyrightEnd%
%%%
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2011-2013 Michael Truog
%%% @version 1.2.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cpg).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).

%% external interface
-export([start_link/0,
         start_link/1,
         create/1,
         create/2,
         delete/1,
         delete/2,
         join/1,
         join/2,
         join/3,
         leave/1,
         leave/2,
         leave/3,
         whereis_name/1,
         register_name/2,
         unregister_name/1,
         send/2,
         get_members/1,
         get_members/2,
         get_members/3,
         get_local_members/1,
         get_local_members/2,
         get_local_members/3,
         get_remote_members/1,
         get_remote_members/2,
         get_remote_members/3,
         which_groups/0,
         which_groups/1,
         get_closest_pid/1,
         get_closest_pid/2,
         get_closest_pid/3,
         get_furthest_pid/1,
         get_furthest_pid/2,
         get_furthest_pid/3,
         get_random_pid/1,
         get_random_pid/2,
         get_random_pid/3,
         get_local_pid/1,
         get_local_pid/2,
         get_local_pid/3,
         get_remote_pid/1,
         get_remote_pid/2,
         get_remote_pid/3,
         get_oldest_pid/1,
         get_oldest_pid/2,
         get_oldest_pid/3,
         get_local_oldest_pid/1,
         get_local_oldest_pid/2,
         get_local_oldest_pid/3,
         get_remote_oldest_pid/1,
         get_remote_oldest_pid/2,
         get_remote_oldest_pid/3,
         get_newest_pid/1,
         get_newest_pid/2,
         get_newest_pid/3,
         get_local_newest_pid/1,
         get_local_newest_pid/2,
         get_local_newest_pid/3,
         get_remote_newest_pid/1,
         get_remote_newest_pid/2,
         get_remote_newest_pid/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         code_change/3, terminate/2]).

-include("cpg_constants.hrl").
-include("cpg_data.hrl").
-include("cpg_logging.hrl").

-record(state,
    {
        scope = undefined, % locally registered process name
        groups = cpg_data:get_empty_groups(), % string() -> #cpg_data{}
        pids = dict:new()                     % pid() -> list(string())
    }).

-type scope() :: atom().
-type name() :: any(). % GROUP_STORAGE macro controls this
-type via_name() :: {'global', scope(), name(), 'random'} |
                    {'global', scope(), name(), 'oldest'} |
                    {'global', scope(), name(), pos_integer()} |
                    {'local', scope(), name(), 'random'} |
                    {'local', scope(), name(), 'oldest'} |
                    {'local', scope(), name(), pos_integer()} |
                    {'global', scope(), name()} |
                    {'local', scope(), name()} |
                    {'global', name(), pos_integer()} |
                    {'local', name(), pos_integer()} |
                    {'global', name()} |
                    {'local', name()} |
                    {scope(), name()} |
                    {name(), pos_integer()} |
                    name(). % for OTP behaviors
-export_type([scope/0, name/0, via_name/0]).

-compile({nowarn_unused_function,
          [{fake_put, 2}]}).

-ifdef(CPG_ETS_CACHE).
-define(CPG_ETS_CACHE_PUT(G), cpg_ets:put(Scope, G)).
-else.
-define(CPG_ETS_CACHE_PUT(G), fake_put(Scope, G)).
-endif.

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Start process groups storage for the default scope.===
%% @end
%%-------------------------------------------------------------------------

-spec start_link() -> {'ok', pid()} | {'error', term()}.

start_link() ->
    start_link(?DEFAULT_SCOPE).

%%-------------------------------------------------------------------------
%% @doc
%% ===Start process groups storage for a specific scope.===
%% @end
%%-------------------------------------------------------------------------

-spec start_link(atom()) -> {'ok', pid()} | {'error', term()}.

start_link(Scope) when is_atom(Scope) ->
    true = (Scope /= local andalso
            Scope /= global),
    gen_server:start_link({local, Scope}, ?MODULE, [Scope], []).

-ifdef(GROUP_NAME_WITH_LOCAL_PIDS_ONLY).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a group explicitly no-op.===
%% @end
%%-------------------------------------------------------------------------

-spec create(name()) -> 'ok'.

create(_) ->
    ok.

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a group explicitly in a specific scope no-op.===
%% @end
%%-------------------------------------------------------------------------

-spec create(scope(), name()) -> 'ok'.

create(Scope, _)
    when is_atom(Scope) ->
    ok.

%%-------------------------------------------------------------------------
%% @doc
%% ===Delete a group explicitly no-op.===
%% @end
%%-------------------------------------------------------------------------

-spec delete(name()) -> 'ok'.

delete(_) ->
    ok.

%%-------------------------------------------------------------------------
%% @doc
%% ===Delete a group explicitly in a specific scope no-op.===
%% @end
%%-------------------------------------------------------------------------

-spec delete(scope(), name()) -> 'ok'.

delete(Scope, _)
    when is_atom(Scope) ->
    ok.

%%-------------------------------------------------------------------------
%% @doc
%% ===Join a specific group with self() as a local pid.===
%% A group is automatically created if it does not already exist.
%% @end
%%-------------------------------------------------------------------------

-spec join(name()) -> 'ok' | 'error'.

join(GroupName) ->
    group_name_validate(GroupName),
    case gen_server:multi_call(?DEFAULT_SCOPE, {join, GroupName, self()}) of
        {[_ | _] = Replies, _} ->
            check_multi_call_replies(Replies);
        _ ->
            error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Join a specific group with the specified local pid or a specific group within a specific scope with self() as a local pid.===
%% The pid must be a local pid to justify not using a distributed transaction
%% since the cpg gen_server process acts like mutex lock, enforcing consistent
%% local state for all local pid process groups.  A group is automatically
%% created if it does not already exist.
%% @end
%%-------------------------------------------------------------------------

-spec join(name() | scope(), pid() | name()) -> 'ok' | 'error'.

join(GroupName, Pid)
    when is_pid(Pid), node(Pid) =:= node() ->
    group_name_validate(GroupName),
    case gen_server:multi_call(?DEFAULT_SCOPE, {join, GroupName, Pid}) of
        {[_ | _] = Replies, _} ->
            check_multi_call_replies(Replies);
        _ ->
            error
    end;

join(Scope, GroupName)
    when is_atom(Scope) ->
    group_name_validate(GroupName),
    case gen_server:multi_call(Scope, {join, GroupName, self()}) of
        {[_ | _] = Replies, _} ->
            check_multi_call_replies(Replies);
        _ ->
            error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Join a specific group within a specific scope with a local pid.===
%% The pid must be a local pid to justify not using a distributed transaction
%% since the cpg gen_server process acts like mutex lock, enforcing consistent
%% local state for all local pid process groups.  A group is automatically
%% created if it does not already exist.
%% @end
%%-------------------------------------------------------------------------

-spec join(scope(), name(), pid()) -> 'ok' | 'error'.

join(Scope, GroupName, Pid)
    when is_atom(Scope), is_pid(Pid),
         node(Pid) =:= node() ->
    group_name_validate(GroupName),
    case gen_server:multi_call(Scope, {join, GroupName, Pid}) of
        {[_ | _] = Replies, _} ->
            check_multi_call_replies(Replies);
        _ ->
            error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Leave a specific group with a local pid.===
%% The group is automatically removed if it becomes empty.
%% @end
%%-------------------------------------------------------------------------

-spec leave(name()) -> 'ok' | 'error'.

leave(GroupName) ->
    group_name_validate(GroupName),
    case gen_server:multi_call(?DEFAULT_SCOPE, {leave, GroupName, self()}) of
        {[_ | _] = Replies, _} ->
            check_multi_call_replies(Replies);
        _ ->
            error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Leave a specific group with the specified local pid or a specific group within a specific scope with self() as a local pid.===
%% The pid must be a local pid to justify not using a distributed transaction
%% since the cpg gen_server process acts like mutex lock, enforcing consistent
%% local state for all local pid process groups.  The group will automatically
%% be removed if it becomes empty.
%% @end
%%-------------------------------------------------------------------------

-spec leave(name() | scope(), pid() | name()) -> 'ok' | 'error'.

leave(GroupName, Pid)
    when is_pid(Pid), node(Pid) =:= node() ->
    group_name_validate(GroupName),
    case gen_server:multi_call(?DEFAULT_SCOPE, {leave, GroupName, Pid}) of
        {[_ | _] = Replies, _} ->
            check_multi_call_replies(Replies);
        _ ->
            error
    end;

leave(Scope, GroupName)
    when is_atom(Scope) ->
    group_name_validate(GroupName),
    case gen_server:multi_call(Scope, {leave, GroupName, self()}) of
        {[_ | _] = Replies, _} ->
            check_multi_call_replies(Replies);
        _ ->
            error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Leave a specific group within a specific scope with a local pid.===
%% The pid must be a local pid to justify not using a distributed transaction
%% since the cpg gen_server process acts like mutex lock, enforcing consistent
%% local state for all local pid process groups.  The group will automatically
%% be removed if it becomes empty.
%% @end
%%-------------------------------------------------------------------------

-spec leave(scope(), name(), pid()) -> 'ok' | 'error'.

leave(Scope, GroupName, Pid)
    when is_atom(Scope), is_pid(Pid),
         node(Pid) =:= node() ->
    group_name_validate(GroupName),
    case gen_server:multi_call(Scope, {leave, GroupName, Pid}) of
        {[_ | _] = Replies, _} ->
            check_multi_call_replies(Replies);
        _ ->
            error
    end.

-else. % GROUP_NAME_WITH_LOCAL_PIDS_ONLY not defined

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a group explicitly.===
%% The calling pid does not need to be a local pid because the function uses a
%% distributed transaction to enforce global consistency.
%% @end
%%-------------------------------------------------------------------------

-spec create(name()) -> 'ok' | 'error'.

create(GroupName) ->
    group_name_validate(GroupName),
    case global:trans({{?DEFAULT_SCOPE, GroupName}, self()},
                      fun() ->
                          gen_server:multi_call(?DEFAULT_SCOPE,
                                                {create, GroupName})
                      end) of
        {[_ | _] = Replies, _} ->
            check_multi_call_replies(Replies);
        _ ->
            error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a group explicitly in a specific scope.===
%% The calling pid does not need to be a local pid because the function uses a
%% distributed transaction to enforce global consistency.
%% @end
%%-------------------------------------------------------------------------

-spec create(scope(), name()) -> 'ok' | 'error'.

create(Scope, GroupName)
    when is_atom(Scope) ->
    group_name_validate(GroupName),
    case global:trans({{Scope, GroupName}, self()},
                      fun() ->
                          gen_server:multi_call(Scope,
                                                {create, GroupName})
                      end) of
        {[_ | _] = Replies, _} ->
            check_multi_call_replies(Replies);
        _ ->
            error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Delete a group explicitly.===
%% The calling pid does not need to be a local pid because the function uses a
%% distributed transaction to enforce global consistency.
%% @end
%%-------------------------------------------------------------------------

-spec delete(name()) -> 'ok' | 'error'.

delete(GroupName) ->
    group_name_validate(GroupName),
    case global:trans({{?DEFAULT_SCOPE, GroupName}, self()},
                      fun() ->
                          gen_server:multi_call(?DEFAULT_SCOPE,
                                                {delete, GroupName})
                      end) of
        {[_ | _] = Replies, _} ->
            check_multi_call_replies(Replies);
        _ ->
            error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Delete a group explicitly in a specific scope.===
%% The calling pid does not need to be a local pid because the function uses a
%% distributed transaction to enforce global consistency.
%% @end
%%-------------------------------------------------------------------------

-spec delete(scope(), name()) -> 'ok' | 'error'.

delete(Scope, GroupName)
    when is_atom(Scope) ->
    group_name_validate(GroupName),
    case global:trans({{Scope, GroupName}, self()},
                      fun() ->
                          gen_server:multi_call(Scope,
                                                {delete, GroupName})
                      end) of
        {[_ | _] = Replies, _} ->
            check_multi_call_replies(Replies);
        _ ->
            error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Join a specific group with self().===
%% @end
%%-------------------------------------------------------------------------

-spec join(name()) -> 'ok' | 'error'.

join(GroupName) ->
    group_name_validate(GroupName),
    Self = self(),
    case global:trans({{?DEFAULT_SCOPE, GroupName}, Self},
                      fun() ->
                          gen_server:multi_call(?DEFAULT_SCOPE,
                                                {join, GroupName, Self})
                      end) of
        {[_ | _] = Replies, _} ->
            check_multi_call_replies(Replies);
        _ ->
            error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Join a specific group.===
%% The pid does not need to be a local pid because the function uses a
%% distributed transaction to enforce global consistency.
%% @end
%%-------------------------------------------------------------------------

-spec join(name(), pid()) -> 'ok' | 'error'.

join(GroupName, Pid)
    when is_pid(Pid) ->
    group_name_validate(GroupName),
    case global:trans({{?DEFAULT_SCOPE, GroupName}, self()},
                      fun() ->
                          gen_server:multi_call(?DEFAULT_SCOPE,
                                                {join, GroupName, Pid})
                      end) of
        {[_ | _] = Replies, _} ->
            check_multi_call_replies(Replies);
        _ ->
            error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Join a specific group in a specific scope.===
%% The pid does not need to be a local pid because the function uses a
%% distributed transaction to enforce global consistency.
%% @end
%%-------------------------------------------------------------------------

-spec join(scope(), name(), pid()) -> 'ok' | 'error'.

join(Scope, GroupName, Pid)
    when is_atom(Scope), is_pid(Pid) ->
    group_name_validate(GroupName),
    case global:trans({{Scope, GroupName}, self()},
                      fun() ->
                          gen_server:multi_call(Scope,
                                                {join, GroupName, Pid})
                      end) of
        {[_ | _] = Replies, _} ->
            check_multi_call_replies(Replies);
        _ ->
            error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Leave a specific group.===
%% @end
%%-------------------------------------------------------------------------

-spec leave(name()) -> 'ok' | 'error'.

leave(GroupName) ->
    group_name_validate(GroupName),
    Self = self(),
    case global:trans({{?DEFAULT_SCOPE, GroupName}, Self},
                      fun() ->
                          gen_server:multi_call(?DEFAULT_SCOPE,
                                                {leave, GroupName, Self})
                      end) of
        {[_ | _] = Replies, _} ->
            check_multi_call_replies(Replies);
        _ ->
            error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Leave a specific group.===
%% The pid does not need to be a local pid because the function uses a
%% distributed transaction to enforce global consistency.
%% @end
%%-------------------------------------------------------------------------

-spec leave(name(), pid()) -> 'ok' | 'error'.

leave(GroupName, Pid)
    when is_pid(Pid) ->
    group_name_validate(GroupName),
    case global:trans({{?DEFAULT_SCOPE, GroupName}, self()},
                      fun() ->
                          gen_server:multi_call(?DEFAULT_SCOPE,
                                                {leave, GroupName, Pid})
                      end) of
        {[_ | _] = Replies, _} ->
            check_multi_call_replies(Replies);
        _ ->
            error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Leave a specific group in a specific scope.===
%% The pid does not need to be a local pid because the function uses a
%% distributed transaction to enforce global consistency.
%% @end
%%-------------------------------------------------------------------------

-spec leave(scope(), name(), pid()) -> 'ok' | 'error'.

leave(Scope, GroupName, Pid)
    when is_atom(Scope), is_pid(Pid) ->
    group_name_validate(GroupName),
    case global:trans({{Scope, GroupName}, self()},
                      fun() ->
                          gen_server:multi_call(Scope,
                                                {leave, GroupName, Pid})
                      end) of
        {[_ | _] = Replies, _} ->
            check_multi_call_replies(Replies);
        _ ->
            error
    end.

-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Function to provide via process registration functionality.===
%% Use within an OTP behavior by specifying {via, cpg, via_name()} for the
%% process registration (instead of {local, atom()} or {global, atom()})
%% @end
%%-------------------------------------------------------------------------

-spec whereis_name(via_name()) -> pid() | 'undefined'.

whereis_name({global, Scope, GroupName, random})
    when is_atom(Scope) ->
    case get_random_pid(Scope, GroupName) of
        {ok, _, Pid} ->
            Pid;
        {error, _} ->
            undefined
    end;

whereis_name({global, Scope, GroupName, oldest})
    when is_atom(Scope) ->
    case get_oldest_pid(Scope, GroupName) of
        {ok, _, Pid} ->
            Pid;
        {error, _} ->
            undefined
    end;

whereis_name({global, Scope, GroupName, Instances})
    when is_atom(Scope), is_integer(Instances), Instances > 0 ->
    case get_members(Scope, GroupName) of
        {ok, _, Pids} ->
            Count = erlang:length(Pids),
            if
                Count < Instances ->
                    undefined;
                true ->
                    whereis_name_random(Count, Pids)
            end;
        {error, _} ->
            undefined
    end;

whereis_name({local, Scope, GroupName, random})
    when is_atom(Scope) ->
    case get_local_pid(Scope, GroupName) of
        {ok, _, Pid} ->
            Pid;
        {error, _} ->
            undefined
    end;

whereis_name({local, Scope, GroupName, oldest})
    when is_atom(Scope) ->
    case get_local_oldest_pid(Scope, GroupName) of
        {ok, _, Pid} ->
            Pid;
        {error, _} ->
            undefined
    end;

whereis_name({local, Scope, GroupName, Instances})
    when is_atom(Scope), is_integer(Instances), Instances > 0 ->
    case get_local_members(Scope, GroupName) of
        {ok, _, Pids} ->
            Count = erlang:length(Pids),
            if
                Count < Instances ->
                    undefined;
                true ->
                    whereis_name_random(Count, Pids)
            end;
        {error, _} ->
            undefined
    end;

whereis_name({global, Scope, GroupName})
    when is_atom(Scope) ->
    case get_oldest_pid(Scope, GroupName) of
        {ok, _, Pid} ->
            Pid;
        {error, _} ->
            undefined
    end;

whereis_name({local, Scope, GroupName})
    when is_atom(Scope) ->
    case get_local_pid(Scope, GroupName) of
        {ok, _, Pid} ->
            Pid;
        {error, _} ->
            undefined
    end;

whereis_name({global, GroupName, Instances})
    when is_integer(Instances), Instances > 0 ->
    case get_members(GroupName) of
        {ok, _, Pids} ->
            Count = erlang:length(Pids),
            if
                Count < Instances ->
                    undefined;
                true ->
                    whereis_name_random(Count, Pids)
            end;
        {error, _} ->
            undefined
    end;

whereis_name({local, GroupName, Instances})
    when is_integer(Instances), Instances > 0 ->
    case get_local_members(GroupName) of
        {ok, _, Pids} ->
            Count = erlang:length(Pids),
            if
                Count < Instances ->
                    undefined;
                true ->
                    whereis_name_random(Count, Pids)
            end;
        {error, _} ->
            undefined
    end;

whereis_name({Scope, GroupName, Instances})
    when is_atom(Scope), is_integer(Instances), Instances > 0 ->
    case get_local_members(Scope, GroupName) of
        {ok, _, Pids} ->
            Count = erlang:length(Pids),
            if
                Count < Instances ->
                    undefined;
                true ->
                    whereis_name_random(Count, Pids)
            end;
        {error, _} ->
            undefined
    end;

whereis_name({global, GroupName}) ->
    case get_oldest_pid(?DEFAULT_SCOPE, GroupName) of
        {ok, _, Pid} ->
            Pid;
        {error, _} ->
            undefined
    end;

whereis_name({local, GroupName}) ->
    case get_local_pid(?DEFAULT_SCOPE, GroupName) of
        {ok, _, Pid} ->
            Pid;
        {error, _} ->
            undefined
    end;

whereis_name({Scope, GroupName})
    when is_atom(Scope) ->
    % default is local
    case get_local_pid(Scope, GroupName) of
        {ok, _, Pid} ->
            Pid;
        {error, _} ->
            undefined
    end;

whereis_name({GroupName, Instances})
    when is_integer(Instances), Instances > 0 ->
    case get_local_members(GroupName) of
        {ok, _, Pids} ->
            Count = erlang:length(Pids),
            if
                Count < Instances ->
                    undefined;
                true ->
                    whereis_name_random(Count, Pids)
            end;
        {error, _} ->
            undefined
    end;

whereis_name(GroupName) ->
    % default is local
    case get_local_pid(?DEFAULT_SCOPE, GroupName) of
        {ok, _, Pid} ->
            Pid;
        {error, _} ->
            undefined
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Function to provide via process registration functionality.===
%% Use within an OTP behavior by specifying {via, cpg, via_name()} for the
%% process registration (instead of {local, atom()} or {global, atom()})
%% @end
%%-------------------------------------------------------------------------

-spec register_name(via_name(), pid()) -> 'yes' | 'no'.

register_name({RegistrationType, Scope, GroupName, Lookup}, Pid)
    when (RegistrationType =:= global orelse
          RegistrationType =:= local), is_atom(Scope),
         (Lookup =:= random orelse Lookup =:= oldest) ->
    case join(Scope, GroupName, Pid) of
        ok ->
            yes;
        error ->
            no
    end;

register_name({RegistrationType, Scope, GroupName, Instances}, Pid)
    when (RegistrationType =:= global orelse
          RegistrationType =:= local), is_atom(Scope),
         is_integer(Instances), Instances > 0 ->
    case join(Scope, GroupName, Pid) of
        ok ->
            yes;
        error ->
            no
    end;

register_name({RegistrationType, Scope, GroupName}, Pid)
    when (RegistrationType =:= global orelse
          RegistrationType =:= local), is_atom(Scope) ->
    case join(Scope, GroupName, Pid) of
        ok ->
            yes;
        error ->
            no
    end;

register_name({RegistrationType, GroupName, Instances}, Pid)
    when (RegistrationType =:= global orelse
          RegistrationType =:= local),
         is_integer(Instances), Instances > 0 ->
    case join(?DEFAULT_SCOPE, GroupName, Pid) of
        ok ->
            yes;
        error ->
            no
    end;

register_name({RegistrationType, GroupName}, Pid)
    when (RegistrationType =:= global orelse
          RegistrationType =:= local) ->
    case join(?DEFAULT_SCOPE, GroupName, Pid) of
        ok ->
            yes;
        error ->
            no
    end;

register_name({Scope, GroupName}, Pid)
    when is_atom(Scope) ->
    case join(Scope, GroupName, Pid) of
        ok ->
            yes;
        error ->
            no
    end;

register_name({GroupName, Instances}, Pid)
    when is_integer(Instances), Instances > 0 ->
    case join(?DEFAULT_SCOPE, GroupName, Pid) of
        ok ->
            yes;
        error ->
            no
    end;

register_name(GroupName, Pid) ->
    case join(?DEFAULT_SCOPE, GroupName, Pid) of
        ok ->
            yes;
        error ->
            no
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Function to provide via process registration functionality.===
%% Use within an OTP behavior by specifying {via, cpg, via_name()} for the
%% process registration (instead of {local, atom()} or {global, atom()})
%% @end
%%-------------------------------------------------------------------------

-spec unregister_name(via_name()) -> 'ok' | 'error'.

unregister_name({RegistrationType, Scope, GroupName, Lookup})
    when (RegistrationType =:= global orelse
          RegistrationType =:= local), is_atom(Scope),
         (Lookup =:= random orelse Lookup =:= oldest) ->
    leave(Scope, GroupName, self());

unregister_name({RegistrationType, Scope, GroupName, Instances})
    when (RegistrationType =:= global orelse
          RegistrationType =:= local), is_atom(Scope),
         is_integer(Instances), Instances > 0 ->
    leave(Scope, GroupName, self());

unregister_name({RegistrationType, Scope, GroupName})
    when (RegistrationType =:= global orelse
          RegistrationType =:= local), is_atom(Scope) ->
    leave(Scope, GroupName, self());

unregister_name({RegistrationType, GroupName, Instances})
    when (RegistrationType =:= global orelse
          RegistrationType =:= local),
         is_integer(Instances), Instances > 0 ->
    leave(?DEFAULT_SCOPE, GroupName, self());

unregister_name({RegistrationType, GroupName})
    when (RegistrationType =:= global orelse
          RegistrationType =:= local) ->
    leave(?DEFAULT_SCOPE, GroupName, self());

unregister_name({Scope, GroupName})
    when is_atom(Scope) ->
    leave(Scope, GroupName, self());

unregister_name({GroupName, Instances})
    when is_integer(Instances), Instances > 0 ->
    leave(?DEFAULT_SCOPE, GroupName, self());

unregister_name(GroupName) ->
    leave(?DEFAULT_SCOPE, GroupName, self()).

%%-------------------------------------------------------------------------
%% @doc
%% ===Function to provide via process registration functionality.===
%% Use within an OTP behavior by specifying {via, cpg, via_name()} for the
%% process registration (instead of {local, atom()} or {global, atom()})
%% @end
%%-------------------------------------------------------------------------

-spec send(via_name(), any()) -> pid().

send(ViaName, Msg) ->
    case whereis_name(ViaName) of
        undefined ->
            erlang:exit({badarg, {ViaName, Msg}});
        Pid when is_pid(Pid) ->
            Pid ! Msg,
            Pid
    end.

-type get_members_ret() ::
    {ok, name(), list(pid())} | {'error', {'no_such_group', name()}}.

-type gcp_error_reason() ::
    {'no_process', name()} | {'no_such_group', name()}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the members of a specific group.===
%% @end
%%-------------------------------------------------------------------------

-spec get_members(name()) -> get_members_ret().
   
-ifdef(CPG_ETS_CACHE).
get_members(GroupName) ->
    cpg_data:get_members(GroupName,
                         cpg_ets:get(?DEFAULT_SCOPE)).
-else.
get_members(GroupName) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_members, GroupName}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the members of a specific group while excluding a specific pid or within a specific scope.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_members(name() | scope(), pid() | name()) -> get_members_ret().

-ifdef(CPG_ETS_CACHE).
get_members(GroupName, Exclude)
    when is_pid(Exclude) ->
    cpg_data:get_members(GroupName, Exclude,
                         cpg_ets:get(?DEFAULT_SCOPE));

get_members(Scope, GroupName)
    when is_atom(Scope) ->
    cpg_data:get_members(GroupName,
                         cpg_ets:get(Scope)).
-else.
get_members(GroupName, Exclude)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_members, GroupName, Exclude});

get_members(Scope, GroupName)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_members, GroupName}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the members of a specific group within a specific scope while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_members(scope(), name(), pid()) -> get_members_ret().

-ifdef(CPG_ETS_CACHE).
get_members(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    cpg_data:get_members(GroupName, Exclude,
                         cpg_ets:get(Scope)).
-else.
get_members(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_members, GroupName, Exclude}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get only the local members of a specific group.===
%% @end
%%-------------------------------------------------------------------------

-spec get_local_members(name()) -> get_members_ret().

-ifdef(CPG_ETS_CACHE).
get_local_members(GroupName) ->
    cpg_data:get_local_members(GroupName,
                               cpg_ets:get(?DEFAULT_SCOPE)).
-else.
get_local_members(GroupName) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_local_members, GroupName}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get only the local members of a specific group while excluding a specific pid or within a specific scope.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_local_members(name() | scope(), pid() | name()) -> get_members_ret().

-ifdef(CPG_ETS_CACHE).
get_local_members(GroupName, Exclude)
    when is_pid(Exclude) ->
    cpg_data:get_local_members(GroupName, Exclude,
                               cpg_ets:get(?DEFAULT_SCOPE));

get_local_members(Scope, GroupName)
    when is_atom(Scope) ->
    cpg_data:get_local_members(GroupName,
                               cpg_ets:get(Scope)).
-else.
get_local_members(GroupName, Exclude)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_local_members, GroupName, Exclude});

get_local_members(Scope, GroupName)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_local_members, GroupName}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get only the local members of a specific group within a specific scope while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_local_members(scope(), name(), pid()) -> get_members_ret().

-ifdef(CPG_ETS_CACHE).
get_local_members(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    cpg_data:get_local_members(GroupName, Exclude,
                               cpg_ets:get(Scope)).
-else.
get_local_members(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_local_members, GroupName, Exclude}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get only the remote members of a specific group.===
%% @end
%%-------------------------------------------------------------------------

-spec get_remote_members(name()) -> get_members_ret().

-ifdef(CPG_ETS_CACHE).
get_remote_members(GroupName) ->
    cpg_data:get_remote_members(GroupName,
                                cpg_ets:get(?DEFAULT_SCOPE)).
-else.
get_remote_members(GroupName) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_remote_members, GroupName}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get only the remote members of a specific group while excluding a specific pid or within a specific scope.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_remote_members(name() | scope(), pid() | name()) -> get_members_ret().

-ifdef(CPG_ETS_CACHE).
get_remote_members(GroupName, Exclude)
    when is_pid(Exclude) ->
    cpg_data:get_remote_members(GroupName, Exclude,
                                cpg_ets:get(?DEFAULT_SCOPE));

get_remote_members(Scope, GroupName)
    when is_atom(Scope) ->
    cpg_data:get_remote_members(GroupName,
                                cpg_ets:get(Scope)).
-else.
get_remote_members(GroupName, Exclude)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_remote_members, GroupName, Exclude});

get_remote_members(Scope, GroupName)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_remote_members, GroupName}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get only the remote members of a specific group within a specific scope while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_remote_members(scope(), name(), pid()) -> get_members_ret().

-ifdef(CPG_ETS_CACHE).
get_remote_members(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    cpg_data:get_remote_members(GroupName, Exclude,
                                cpg_ets:get(Scope)).
-else.
get_remote_members(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_remote_members, GroupName, Exclude}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get all the groups currently defined.===
%% @end
%%-------------------------------------------------------------------------

-spec which_groups() -> [name()].

-ifdef(CPG_ETS_CACHE).
which_groups() ->
    cpg_data:which_groups(cpg_ets:get(?DEFAULT_SCOPE)).
-else.
which_groups() ->
    gen_server:call(?DEFAULT_SCOPE,
                    which_groups).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get all the groups currently defined within a specific scope.===
%% @end
%%-------------------------------------------------------------------------

-spec which_groups(scope()) -> [name()].

-ifdef(CPG_ETS_CACHE).
which_groups(Scope)
    when is_atom(Scope) ->
    cpg_data:which_groups(cpg_ets:get(Scope)).
-else.
which_groups(Scope)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    which_groups).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a group member, with local pids given priority.===
%% @end
%%-------------------------------------------------------------------------

-spec get_closest_pid(name()) ->
    {ok, name(), pid()} | {'error', gcp_error_reason()}.

-ifdef(CPG_ETS_CACHE).
get_closest_pid(GroupName) ->
    cpg_data:get_closest_pid(GroupName,
                             cpg_ets:get(?DEFAULT_SCOPE)).
-else.
get_closest_pid(GroupName) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_closest_pid, GroupName}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a group member, with local pids given priority while excluding a specific pid or within a specific scope.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_closest_pid(name() | scope(), pid() | name()) ->
    {ok, name(), pid()} | {'error', gcp_error_reason()}.

-ifdef(CPG_ETS_CACHE).
get_closest_pid(GroupName, Exclude)
    when is_pid(Exclude) ->
    cpg_data:get_closest_pid(GroupName, Exclude,
                             cpg_ets:get(?DEFAULT_SCOPE));

get_closest_pid(Scope, GroupName)
    when is_atom(Scope) ->
    cpg_data:get_closest_pid(GroupName,
                             cpg_ets:get(Scope)).
-else.
get_closest_pid(GroupName, Exclude)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_closest_pid, GroupName, Exclude});

get_closest_pid(Scope, GroupName)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_closest_pid, GroupName}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a group member within a specific scope, with local pids given priority while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_closest_pid(scope(), name(), pid()) ->
    {ok, name(), pid()} | {'error', gcp_error_reason()}.

-ifdef(CPG_ETS_CACHE).
get_closest_pid(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    cpg_data:get_closest_pid(GroupName, Exclude,
                             cpg_ets:get(Scope)).
-else.
get_closest_pid(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_closest_pid, GroupName, Exclude}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a group member, with remote pids given priority.===
%% @end
%%-------------------------------------------------------------------------

-spec get_furthest_pid(name()) ->
    {ok, name(), pid()} | {'error', gcp_error_reason()}.

-ifdef(CPG_ETS_CACHE).
get_furthest_pid(GroupName) ->
    cpg_data:get_furthest_pid(GroupName,
                              cpg_ets:get(?DEFAULT_SCOPE)).
-else.
get_furthest_pid(GroupName) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_furthest_pid, GroupName}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a group member, with remote pids given priority while excluding a specific pid or within a specific scope.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_furthest_pid(name() | scope(), pid() | name()) ->
    {ok, name(), pid()} | {'error', gcp_error_reason()}.

-ifdef(CPG_ETS_CACHE).
get_furthest_pid(GroupName, Exclude)
    when is_pid(Exclude) ->
    cpg_data:get_furthest_pid(GroupName, Exclude,
                              cpg_ets:get(?DEFAULT_SCOPE));

get_furthest_pid(Scope, GroupName)
    when is_atom(Scope) ->
    cpg_data:get_furthest_pid(GroupName,
                              cpg_ets:get(Scope)).
-else.
get_furthest_pid(GroupName, Exclude)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_furthest_pid, GroupName, Exclude});

get_furthest_pid(Scope, GroupName)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_furthest_pid, GroupName}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a group member within a specific scope, with remote pids given priority while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_furthest_pid(scope(), name(), pid()) ->
    {ok, name(), pid()} | {'error', gcp_error_reason()}.

-ifdef(CPG_ETS_CACHE).
get_furthest_pid(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    cpg_data:get_furthest_pid(GroupName, Exclude,
                              cpg_ets:get(Scope)).
-else.
get_furthest_pid(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_furthest_pid, GroupName, Exclude}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a group member.===
%% @end
%%-------------------------------------------------------------------------

-spec get_random_pid(name()) ->
    {ok, name(), pid()} | {'error', gcp_error_reason()}.

-ifdef(CPG_ETS_CACHE).
get_random_pid(GroupName) ->
    cpg_data:get_random_pid(GroupName,
                            cpg_ets:get(?DEFAULT_SCOPE)).
-else.
get_random_pid(GroupName) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_random_pid, GroupName}).
-endif.

-spec get_random_pid(name() | scope(), pid() | name()) ->
    {ok, name(), pid()} | {'error', gcp_error_reason()}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a group member while excluding a specific pid or within a specific scope.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-ifdef(CPG_ETS_CACHE).
get_random_pid(GroupName, Exclude)
    when is_pid(Exclude) ->
    cpg_data:get_random_pid(GroupName, Exclude,
                            cpg_ets:get(?DEFAULT_SCOPE));

get_random_pid(Scope, GroupName)
    when is_atom(Scope) ->
    cpg_data:get_random_pid(GroupName,
                            cpg_ets:get(Scope)).
-else.
get_random_pid(GroupName, Exclude)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_random_pid, GroupName, Exclude});

get_random_pid(Scope, GroupName)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_random_pid, GroupName}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a group member within a specific scope while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_random_pid(scope(), name(), pid()) ->
    {ok, name(), pid()} | {'error', gcp_error_reason()}.

-ifdef(CPG_ETS_CACHE).
get_random_pid(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    cpg_data:get_random_pid(GroupName, Exclude,
                            cpg_ets:get(Scope)).
-else.
get_random_pid(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_random_pid, GroupName, Exclude}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a local group member.===
%% @end
%%-------------------------------------------------------------------------

-spec get_local_pid(name()) ->
    {ok, name(), pid()} | {'error', gcp_error_reason()}.

-ifdef(CPG_ETS_CACHE).
get_local_pid(GroupName) ->
    cpg_data:get_local_pid(GroupName,
                           cpg_ets:get(?DEFAULT_SCOPE)).
-else.
get_local_pid(GroupName) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_local_pid, GroupName}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a local group member while excluding a specific pid or within a specific scope.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_local_pid(name() | scope(), pid() | name()) ->
    {ok, name(), pid()} | {'error', gcp_error_reason()}.

-ifdef(CPG_ETS_CACHE).
get_local_pid(GroupName, Exclude)
    when is_pid(Exclude) ->
    cpg_data:get_local_pid(GroupName, Exclude,
                           cpg_ets:get(?DEFAULT_SCOPE));

get_local_pid(Scope, GroupName)
    when is_atom(Scope) ->
    cpg_data:get_local_pid(GroupName,
                           cpg_ets:get(Scope)).
-else.
get_local_pid(GroupName, Exclude)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_local_pid, GroupName, Exclude});

get_local_pid(Scope, GroupName)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_local_pid, GroupName}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a local group member within a specific scope, while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_local_pid(scope(), name(), pid()) ->
    {ok, name(), pid()} | {'error', gcp_error_reason()}.

-ifdef(CPG_ETS_CACHE).
get_local_pid(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    cpg_data:get_local_pid(GroupName, Exclude,
                           cpg_ets:get(Scope)).
-else.
get_local_pid(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_local_pid, GroupName, Exclude}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a remote group member.===
%% @end
%%-------------------------------------------------------------------------

-spec get_remote_pid(name()) ->
    {ok, name(), pid()} | {'error', gcp_error_reason()}.

-ifdef(CPG_ETS_CACHE).
get_remote_pid(GroupName) ->
    cpg_data:get_remote_pid(GroupName,
                            cpg_ets:get(?DEFAULT_SCOPE)).
-else.
get_remote_pid(GroupName) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_remote_pid, GroupName}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a remote group member while excluding a specific pid or within a specific scope.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_remote_pid(name() | scope(), pid() | name()) ->
    {ok, name(), pid()} | {'error', gcp_error_reason()}.

-ifdef(CPG_ETS_CACHE).
get_remote_pid(GroupName, Exclude)
    when is_pid(Exclude) ->
    cpg_data:get_remote_pid(GroupName, Exclude,
                            cpg_ets:get(?DEFAULT_SCOPE));

get_remote_pid(Scope, GroupName)
    when is_atom(Scope) ->
    cpg_data:get_remote_pid(GroupName,
                            cpg_ets:get(Scope)).
-else.
get_remote_pid(GroupName, Exclude)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_remote_pid, GroupName, Exclude});

get_remote_pid(Scope, GroupName)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_remote_pid, GroupName}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a remote group member within a specific scope, while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_remote_pid(scope(), name(), pid()) ->
    {ok, name(), pid()} | {'error', gcp_error_reason()}.

-ifdef(CPG_ETS_CACHE).
get_remote_pid(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    cpg_data:get_remote_pid(GroupName, Exclude,
                            cpg_ets:get(Scope)).
-else.
get_remote_pid(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_remote_pid, GroupName, Exclude}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the oldest group member.===
%% @end
%%-------------------------------------------------------------------------

-spec get_oldest_pid(name()) ->
    {ok, name(), pid()} | {'error', gcp_error_reason()}.

-ifdef(CPG_ETS_CACHE).
get_oldest_pid(GroupName) ->
    cpg_data:get_oldest_pid(GroupName,
                            cpg_ets:get(?DEFAULT_SCOPE)).
-else.
get_oldest_pid(GroupName) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_oldest_pid, GroupName}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the oldest group member while excluding a specific pid or within a specific scope.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_oldest_pid(name() | scope(), pid() | name()) ->
    {ok, name(), pid()} | {'error', gcp_error_reason()}.

-ifdef(CPG_ETS_CACHE).
get_oldest_pid(GroupName, Exclude)
    when is_pid(Exclude) ->
    cpg_data:get_oldest_pid(GroupName, Exclude,
                            cpg_ets:get(?DEFAULT_SCOPE));

get_oldest_pid(Scope, GroupName)
    when is_atom(Scope) ->
    cpg_data:get_oldest_pid(GroupName,
                            cpg_ets:get(Scope)).
-else.
get_oldest_pid(GroupName, Exclude)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_oldest_pid, GroupName, Exclude});

get_oldest_pid(Scope, GroupName)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_oldest_pid, GroupName}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the oldest group member within a specific scope, while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_oldest_pid(scope(), name(), pid()) ->
    {ok, name(), pid()} | {'error', gcp_error_reason()}.

-ifdef(CPG_ETS_CACHE).
get_oldest_pid(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    cpg_data:get_oldest_pid(GroupName, Exclude,
                            cpg_ets:get(Scope)).
-else.
get_oldest_pid(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_oldest_pid, GroupName, Exclude}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the oldest local group member.===
%% @end
%%-------------------------------------------------------------------------

-spec get_local_oldest_pid(name()) ->
    {ok, name(), pid()} | {'error', gcp_error_reason()}.

-ifdef(CPG_ETS_CACHE).
get_local_oldest_pid(GroupName) ->
    cpg_data:get_local_oldest_pid(GroupName,
                                  cpg_ets:get(?DEFAULT_SCOPE)).
-else.
get_local_oldest_pid(GroupName) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_local_oldest_pid, GroupName}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the oldest local group member while excluding a specific pid or within a specific scope.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_local_oldest_pid(name() | scope(), pid() | name()) ->
    {ok, name(), pid()} | {'error', gcp_error_reason()}.

-ifdef(CPG_ETS_CACHE).
get_local_oldest_pid(GroupName, Exclude)
    when is_pid(Exclude) ->
    cpg_data:get_local_oldest_pid(GroupName, Exclude,
                                  cpg_ets:get(?DEFAULT_SCOPE));

get_local_oldest_pid(Scope, GroupName)
    when is_atom(Scope) ->
    cpg_data:get_local_oldest_pid(GroupName,
                                  cpg_ets:get(Scope)).
-else.
get_local_oldest_pid(GroupName, Exclude)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_local_oldest_pid, GroupName, Exclude});

get_local_oldest_pid(Scope, GroupName)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_local_oldest_pid, GroupName}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the oldest local group member within a specific scope, while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_local_oldest_pid(scope(), name(), pid()) ->
    {ok, name(), pid()} | {'error', gcp_error_reason()}.

-ifdef(CPG_ETS_CACHE).
get_local_oldest_pid(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    cpg_data:get_local_oldest_pid(GroupName, Exclude,
                                  cpg_ets:get(Scope)).
-else.
get_local_oldest_pid(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_local_oldest_pid, GroupName, Exclude}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the oldest remote group member.===
%% @end
%%-------------------------------------------------------------------------

-spec get_remote_oldest_pid(name()) ->
    {ok, name(), pid()} | {'error', gcp_error_reason()}.

-ifdef(CPG_ETS_CACHE).
get_remote_oldest_pid(GroupName) ->
    cpg_data:get_remote_oldest_pid(GroupName,
                                   cpg_ets:get(?DEFAULT_SCOPE)).
-else.
get_remote_oldest_pid(GroupName) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_remote_oldest_pid, GroupName}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the oldest remote group member while excluding a specific pid or within a specific scope.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_remote_oldest_pid(name() | scope(), pid() | name()) ->
    {ok, name(), pid()} | {'error', gcp_error_reason()}.

-ifdef(CPG_ETS_CACHE).
get_remote_oldest_pid(GroupName, Exclude)
    when is_pid(Exclude) ->
    cpg_data:get_remote_oldest_pid(GroupName, Exclude,
                                   cpg_ets:get(?DEFAULT_SCOPE));

get_remote_oldest_pid(Scope, GroupName)
    when is_atom(Scope) ->
    cpg_data:get_remote_oldest_pid(GroupName,
                                   cpg_ets:get(Scope)).
-else.
get_remote_oldest_pid(GroupName, Exclude)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_remote_oldest_pid, GroupName, Exclude});

get_remote_oldest_pid(Scope, GroupName)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_remote_oldest_pid, GroupName}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the oldest remote group member within a specific scope, while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_remote_oldest_pid(scope(), name(), pid()) ->
    {ok, name(), pid()} | {'error', gcp_error_reason()}.

-ifdef(CPG_ETS_CACHE).
get_remote_oldest_pid(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    cpg_data:get_remote_oldest_pid(GroupName, Exclude,
                                   cpg_ets:get(Scope)).
-else.
get_remote_oldest_pid(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_remote_oldest_pid, GroupName, Exclude}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the newest group member.===
%% @end
%%-------------------------------------------------------------------------

-spec get_newest_pid(name()) ->
    {ok, name(), pid()} | {'error', gcp_error_reason()}.

-ifdef(CPG_ETS_CACHE).
get_newest_pid(GroupName) ->
    cpg_data:get_newest_pid(GroupName,
                            cpg_ets:get(?DEFAULT_SCOPE)).
-else.
get_newest_pid(GroupName) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_newest_pid, GroupName}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the newest group member while excluding a specific pid or within a specific scope.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_newest_pid(name() | scope(), pid() | name()) ->
    {ok, name(), pid()} | {'error', gcp_error_reason()}.

-ifdef(CPG_ETS_CACHE).
get_newest_pid(GroupName, Exclude)
    when is_pid(Exclude) ->
    cpg_data:get_newest_pid(GroupName, Exclude,
                            cpg_ets:get(?DEFAULT_SCOPE));

get_newest_pid(Scope, GroupName)
    when is_atom(Scope) ->
    cpg_data:get_newest_pid(GroupName,
                            cpg_ets:get(Scope)).
-else.
get_newest_pid(GroupName, Exclude)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_newest_pid, GroupName, Exclude});

get_newest_pid(Scope, GroupName)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_newest_pid, GroupName}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the newest group member within a specific scope, while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_newest_pid(scope(), name(), pid()) ->
    {ok, name(), pid()} | {'error', gcp_error_reason()}.

-ifdef(CPG_ETS_CACHE).
get_newest_pid(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    cpg_data:get_newest_pid(GroupName, Exclude,
                            cpg_ets:get(Scope)).
-else.
get_newest_pid(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope, {get_newest_pid, GroupName, Exclude}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the newest local group member.===
%% @end
%%-------------------------------------------------------------------------

-spec get_local_newest_pid(name()) ->
    {ok, name(), pid()} | {'error', gcp_error_reason()}.

-ifdef(CPG_ETS_CACHE).
get_local_newest_pid(GroupName) ->
    cpg_data:get_local_newest_pid(GroupName,
                                  cpg_ets:get(?DEFAULT_SCOPE)).
-else.
get_local_newest_pid(GroupName) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_local_newest_pid, GroupName}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the newest local group member while excluding a specific pid or within a specific scope.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_local_newest_pid(name() | scope(), pid() | name()) ->
    {ok, name(), pid()} | {'error', gcp_error_reason()}.

-ifdef(CPG_ETS_CACHE).
get_local_newest_pid(GroupName, Exclude)
    when is_pid(Exclude) ->
    cpg_data:get_local_newest_pid(GroupName, Exclude,
                                  cpg_ets:get(?DEFAULT_SCOPE));

get_local_newest_pid(Scope, GroupName)
    when is_atom(Scope) ->
    cpg_data:get_local_newest_pid(GroupName,
                                  cpg_ets:get(Scope)).
-else.
get_local_newest_pid(GroupName, Exclude)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_local_newest_pid, GroupName, Exclude});

get_local_newest_pid(Scope, GroupName)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_local_newest_pid, GroupName}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the newest local group member within a specific scope, while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_local_newest_pid(scope(), name(), pid()) ->
    {ok, name(), pid()} | {'error', gcp_error_reason()}.

-ifdef(CPG_ETS_CACHE).
get_local_newest_pid(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    cpg_data:get_local_newest_pid(GroupName, Exclude,
                                  cpg_ets:get(Scope)).
-else.
get_local_newest_pid(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_local_newest_pid, GroupName, Exclude}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the newest remote group member.===
%% @end
%%-------------------------------------------------------------------------

-spec get_remote_newest_pid(name()) ->
    {ok, name(), pid()} | {'error', gcp_error_reason()}.

-ifdef(CPG_ETS_CACHE).
get_remote_newest_pid(GroupName) ->
    cpg_data:get_remote_newest_pid(GroupName,
                                   cpg_ets:get(?DEFAULT_SCOPE)).
-else.
get_remote_newest_pid(GroupName) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_remote_newest_pid, GroupName}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the newest remote group member while excluding a specific pid or within a specific scope.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_remote_newest_pid(name() | scope(), pid() | name()) ->
    {ok, name(), pid()} | {'error', gcp_error_reason()}.

-ifdef(CPG_ETS_CACHE).
get_remote_newest_pid(GroupName, Exclude)
    when is_pid(Exclude) ->
    cpg_data:get_remote_newest_pid(GroupName, Exclude,
                                   cpg_ets:get(?DEFAULT_SCOPE));

get_remote_newest_pid(Scope, GroupName)
    when is_atom(Scope) ->
    cpg_data:get_remote_newest_pid(GroupName,
                                   cpg_ets:get(Scope)).
-else.
get_remote_newest_pid(GroupName, Exclude)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_remote_newest_pid, GroupName, Exclude});

get_remote_newest_pid(Scope, GroupName)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_remote_newest_pid, GroupName}).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the newest remote group member within a specific scope, while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_remote_newest_pid(scope(), name(), pid()) ->
    {ok, name(), pid()} | {'error', gcp_error_reason()}.

-ifdef(CPG_ETS_CACHE).
get_remote_newest_pid(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    cpg_data:get_remote_newest_pid(GroupName, Exclude,
                                   cpg_ets:get(Scope)).
-else.
get_remote_newest_pid(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_remote_newest_pid, GroupName, Exclude}).
-endif.

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%% @private
%% @doc
%% @end

-spec init([scope()]) -> {'ok', #state{}}.

init([Scope]) ->
    Ns = nodes(),
    net_kernel:monitor_nodes(true),
    lists:foreach(fun(N) ->
                          {Scope, N} ! {new, node()}
                          % data is not persistent in ets, so trust the
                          % State coming from other nodes if this server
                          % has restarted and wants previous state
                          %self() ! {nodeup, N} % pg2 does this
                  end, Ns),
    quickrand:seed(),
    {ok, #state{scope = Scope,
                groups = ?CPG_ETS_CACHE_PUT(cpg_data:get_empty_groups())}}.

%% @private
%% @doc
%% @end

-type call() :: {'create', name()}
              | {'delete', name()}
              | {'join', name(), pid()}
              | {'leave', name(), pid()}.

-spec handle_call(call(), _, #state{}) -> {'reply', 'ok', #state{}}.

handle_call({create, GroupName}, _, State) ->
    {reply, ok, create_group(GroupName, State)};

handle_call({delete, GroupName}, _, State) ->
    {reply, ok, delete_group(GroupName, State)};

handle_call({join, GroupName, Pid}, _, State) ->
    {reply, ok, join_group(GroupName, Pid, State)};

handle_call({leave, GroupName, Pid}, _,
            #state{pids = Pids} = State) ->
    Found = case dict:find(Pid, Pids) of
        error ->
            false;
        {ok, GroupNameList} ->
            lists:member(GroupName, GroupNameList)
    end,
    if
        Found ->
            {reply, ok, leave_group(GroupName, Pid, State)};
        true ->
            {reply, error, State}
    end;

handle_call(cpg_data, _,
            #state{groups = Groups} = State) ->
    {reply, Groups, State};

handle_call({get_members, GroupName}, _,
            #state{groups = Groups} = State) ->
    {reply, cpg_data:get_members(GroupName, Groups), State};

handle_call({get_members, GroupName, Exclude}, _,
            #state{groups = Groups} = State) ->
    {reply, cpg_data:get_members(GroupName, Exclude, Groups), State};

handle_call({get_local_members, GroupName}, _,
            #state{groups = Groups} = State) ->
    {reply, cpg_data:get_local_members(GroupName, Groups), State};

handle_call({get_local_members, GroupName, Exclude}, _,
            #state{groups = Groups} = State) ->
    {reply, cpg_data:get_local_members(GroupName, Exclude, Groups), State};

handle_call({get_remote_members, GroupName}, _,
            #state{groups = Groups} = State) ->
    {reply, cpg_data:get_remote_members(GroupName, Groups), State};

handle_call({get_remote_members, GroupName, Exclude}, _,
            #state{groups = Groups} = State) ->
    {reply, cpg_data:get_remote_members(GroupName, Exclude, Groups), State};

handle_call(which_groups, _,
            #state{groups = Groups} = State) ->
    {reply, cpg_data:which_groups(Groups), State};

handle_call({get_closest_pid, GroupName}, _,
            #state{groups = Groups} = State) ->
    {reply, cpg_data:get_closest_pid(GroupName, Groups), State};

handle_call({get_closest_pid, GroupName, Exclude}, _,
            #state{groups = Groups} = State) ->
    {reply, cpg_data:get_closest_pid(GroupName, Exclude, Groups), State};

handle_call({get_furthest_pid, GroupName}, _,
            #state{groups = Groups} = State) ->
    {reply, cpg_data:get_furthest_pid(GroupName, Groups), State};

handle_call({get_furthest_pid, GroupName, Exclude}, _,
            #state{groups = Groups} = State) ->
    {reply, cpg_data:get_furthest_pid(GroupName, Exclude, Groups), State};

handle_call({get_random_pid, GroupName}, _,
            #state{groups = Groups} = State) ->
    {reply, cpg_data:get_random_pid(GroupName, Groups), State};

handle_call({get_random_pid, GroupName, Exclude}, _,
            #state{groups = Groups} = State) ->
    {reply, cpg_data:get_random_pid(GroupName, Exclude, Groups), State};

handle_call({get_local_pid, GroupName}, _,
            #state{groups = Groups} = State) ->
    {reply, cpg_data:get_local_pid(GroupName, Groups), State};

handle_call({get_local_pid, GroupName, Exclude}, _,
            #state{groups = Groups} = State) ->
    {reply, cpg_data:get_local_pid(GroupName, Exclude, Groups), State};

handle_call({get_remote_pid, GroupName}, _,
            #state{groups = Groups} = State) ->
    {reply, cpg_data:get_remote_pid(GroupName, Groups), State};

handle_call({get_remote_pid, GroupName, Exclude}, _,
            #state{groups = Groups} = State) ->
    {reply, cpg_data:get_remote_pid(GroupName, Exclude, Groups), State};

handle_call({get_oldest_pid, GroupName}, _,
            #state{groups = Groups} = State) ->
    {reply, cpg_data:get_oldest_pid(GroupName, Groups), State};

handle_call({get_oldest_pid, GroupName, Exclude}, _,
            #state{groups = Groups} = State) ->
    {reply, cpg_data:get_oldest_pid(GroupName, Exclude, Groups), State};

handle_call({get_local_oldest_pid, GroupName}, _,
            #state{groups = Groups} = State) ->
    {reply, cpg_data:get_local_oldest_pid(GroupName, Groups), State};

handle_call({get_local_oldest_pid, GroupName, Exclude}, _,
            #state{groups = Groups} = State) ->
    {reply, cpg_data:get_local_oldest_pid(GroupName, Exclude, Groups), State};

handle_call({get_remote_oldest_pid, GroupName}, _,
            #state{groups = Groups} = State) ->
    {reply, cpg_data:get_remote_oldest_pid(GroupName, Groups), State};

handle_call({get_remote_oldest_pid, GroupName, Exclude}, _,
            #state{groups = Groups} = State) ->
    {reply, cpg_data:get_remote_oldest_pid(GroupName, Exclude, Groups), State};

handle_call({get_newest_pid, GroupName}, _,
            #state{groups = Groups} = State) ->
    {reply, cpg_data:get_newest_pid(GroupName, Groups), State};

handle_call({get_newest_pid, GroupName, Exclude}, _,
            #state{groups = Groups} = State) ->
    {reply, cpg_data:get_newest_pid(GroupName, Exclude, Groups), State};

handle_call({get_local_newest_pid, GroupName}, _,
            #state{groups = Groups} = State) ->
    {reply, cpg_data:get_local_newest_pid(GroupName, Groups), State};

handle_call({get_local_newest_pid, GroupName, Exclude}, _,
            #state{groups = Groups} = State) ->
    {reply, cpg_data:get_local_newest_pid(GroupName, Exclude, Groups), State};

handle_call({get_remote_newest_pid, GroupName}, _,
            #state{groups = Groups} = State) ->
    {reply, cpg_data:get_remote_newest_pid(GroupName, Groups), State};

handle_call({get_remote_newest_pid, GroupName, Exclude}, _,
            #state{groups = Groups} = State) ->
    {reply, cpg_data:get_remote_newest_pid(GroupName, Exclude, Groups), State};

handle_call(Request, _, State) ->
    ?LOG_WARN("Unknown call \"~p\"", [Request]),
    {stop, lists:flatten(io_lib:format("Unknown call \"~p\"", [Request])),
     error, State}.

%% @private
%% @doc
%% @end

-type cast() :: {'exchange', node(), #state{}}.

-spec handle_cast(cast(), #state{}) -> {'noreply', #state{}}.

handle_cast({exchange, Node, ExternalState}, State) ->
    ?LOG_INFO("received state from ~p", [Node]),
    {noreply, store(ExternalState, State)};

handle_cast(_, State) ->
    {noreply, State}.

%% @private
%% @doc
%% @end

-spec handle_info(tuple(), #state{}) -> {'noreply', #state{}}.

handle_info({'DOWN', _MonitorRef, process, Pid, _Info}, State) ->
    {noreply, member_died(Pid, State)};

handle_info({nodeup, Node},
            #state{scope = Scope} = State) ->
    gen_server:cast({Scope, Node}, {exchange, node(), State}),
    {noreply, State};

handle_info({new, Node},
            #state{scope = Scope} = State) ->
    gen_server:cast({Scope, Node}, {exchange, node(), State}),
    {noreply, State};

handle_info({cpg_data, From},
            #state{groups = Groups} = State) ->
    From ! {cpg_data, Groups},
    {noreply, State};

handle_info(_, State) ->
    {noreply, State}.

%% @private
%% @doc
%% @end

-spec terminate(term(), #state{}) -> 'ok'.

terminate(_, _) ->
    ok.

%% @private
%% @doc
%% @end

code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

create_group(GroupName, #state{scope = Scope,
                               groups = Groups} = State) ->
    NewGroups = ?GROUP_STORAGE:update(GroupName,
        fun(OldValue) -> OldValue end, #cpg_data{}, Groups),
    State#state{groups = ?CPG_ETS_CACHE_PUT(NewGroups)}.

delete_group(GroupName, #state{scope = Scope,
                               groups = Groups,
                               pids = Pids} = State) ->
    case ?GROUP_STORAGE:find(GroupName, Groups) of
        error ->
            State;
        {ok, #cpg_data{local_count = 0,
                       remote_count = 0}} ->
            NewGroups = ?GROUP_STORAGE:erase(GroupName, Groups),
            State#state{groups = ?CPG_ETS_CACHE_PUT(NewGroups)};
        {ok, #cpg_data{local = Local,
                       remote = Remote}} ->
            NewPids = lists:foldl(fun(#cpg_data_pid{pid = Pid,
                                                    monitor = Ref}, P) ->
                true = erlang:demonitor(Ref, [flush]),
                dict:update(Pid,
                            fun(OldValue) ->
                                lists:delete(GroupName, OldValue)
                            end, P)
            end, Pids, Local ++ Remote),
            NewGroups = ?GROUP_STORAGE:erase(GroupName, Groups),
            State#state{groups = ?CPG_ETS_CACHE_PUT(NewGroups),
                        pids = NewPids}
    end.

join_group(GroupName, Pid, #state{scope = Scope,
                                  groups = Groups,
                                  pids = Pids} = State) ->
    Entry = #cpg_data_pid{pid = Pid,
                          monitor = erlang:monitor(process, Pid)},
    NewGroups = if
        node() =:= node(Pid) ->
            ?GROUP_STORAGE:update(GroupName,
                fun(#cpg_data{local_count = LocalI,
                              local = Local,
                              history = History} = OldValue) ->
                    OldValue#cpg_data{local_count = LocalI + 1,
                                      local = [Entry | Local],
                                      history = [Pid | History]}
                end,
                #cpg_data{local_count = 1,
                          local = [Entry],
                          history = [Pid]},
                Groups);
        true ->
            ?GROUP_STORAGE:update(GroupName,
                fun(#cpg_data{remote_count = RemoteI,
                              remote = Remote,
                              history = History} = OldValue) ->
                    OldValue#cpg_data{remote_count = RemoteI + 1,
                                      remote = [Entry | Remote],
                                      history = [Pid | History]}
                end,
                #cpg_data{remote_count = 1,
                          remote = [Entry],
                          history = [Pid]},
                Groups)
    end,
    GroupNameList = [GroupName],
    NewPids = dict:update(Pid,
                          fun(OldValue) ->
                              lists:umerge(OldValue, GroupNameList)
                          end,
                          GroupNameList, Pids),
    State#state{groups = ?CPG_ETS_CACHE_PUT(NewGroups),
                pids = NewPids}.

leave_group(GroupName, Pid, #state{scope = Scope,
                                   groups = Groups,
                                   pids = Pids} = State) ->
    Fpartition = fun(#cpg_data_pid{pid = P, monitor = Ref}) ->
        if 
            P == Pid ->
                true = erlang:demonitor(Ref, [flush]),
                true;
            true ->
                false
        end
    end,
    NewGroups = if
        node() =:= node(Pid) ->
            ?GROUP_STORAGE:update(GroupName,
                fun(#cpg_data{local_count = LocalI,
                              local = Local,
                              history = History} = OldValue) ->
                    {OldLocal,
                     NewLocal} = lists:partition(Fpartition, Local),
                    OldValue#cpg_data{local_count = LocalI -
                                      erlang:length(OldLocal),
                                      local = NewLocal,
                                      history = delete_all(Pid, History)}
                end, Groups);
        true ->
            ?GROUP_STORAGE:update(GroupName,
                fun(#cpg_data{remote_count = RemoteI,
                              remote = Remote,
                              history = History} = OldValue) ->
                    {OldRemote,
                     NewRemote} = lists:partition(Fpartition, Remote),
                    OldValue#cpg_data{remote_count = RemoteI -
                                      erlang:length(OldRemote),
                                      remote = NewRemote,
                                      history = delete_all(Pid, History)}
                end, Groups)
    end,
    NewPids = dict:update(Pid,
                          fun(OldValue) ->
                              lists:delete(GroupName, OldValue)
                          end,
                          Pids),
    State#state{groups = ?CPG_ETS_CACHE_PUT(NewGroups),
                pids = NewPids}.

store_conflict_add_entries(0, Entries, _) ->
    Entries;
store_conflict_add_entries(I, Entries, Pid) ->
    Ref = erlang:monitor(process, Pid),
    store_conflict_add_entries(I - 1,
                               [#cpg_data_pid{pid = Pid,
                                              monitor = Ref} |
                                Entries], Pid).

store_conflict_add_history(0, History, _) ->
    History;
store_conflict_add_history(I, History, Pid) ->
    store_conflict_add_history(I - 1, [Pid | History], Pid).

store_conflict_remove_entries_monitors([]) ->
    ok;
store_conflict_remove_entries_monitors([#cpg_data_pid{monitor = M} |
                                        OldEntries]) ->
    true = erlang:demonitor(M, [flush]),
    store_conflict_remove_entries_monitors(OldEntries).

store_conflict_remove_entries(I, Entries) ->
    {Remove, NewEntries} = lists:split(I * -1, Entries),
    store_conflict_remove_entries_monitors(Remove),
    NewEntries.

store_conflict_remove_history(0, History, _) ->
    History;
store_conflict_remove_history(I, History, Pid) ->
    store_conflict_remove_history(I + 1, lists:delete(Pid, History), Pid).

store_conflict_f([], V2, _) ->
    V2;
store_conflict_f([Pid | V1AllPids],
                 #cpg_data{local_count = LocalI,
                           local = Local,
                           remote_count = RemoteI,
                           remote = Remote,
                           history = History} = V2, V1All) ->
    % for each external Pid, check the internal Pids within the same group
    Fpartition = fun(#cpg_data_pid{pid = P}) ->
        if 
            P == Pid ->
                true;
            true ->
                false
        end
    end,
    if
        node() =:= node(Pid) ->
            % make sure there are equal counts of a local pid
            % based on the external group pids
            {V1Pids, _} = lists:partition(Fpartition, V1All),
            {V2Pids, LocalRest} = lists:partition(Fpartition, Local),
            I = erlang:length(V1Pids) - erlang:length(V2Pids),
            if
                I > 0 ->
                    % add
                    NewLocal = store_conflict_add_entries(I, Local, Pid),
                    NewHistory = store_conflict_add_history(I, History, Pid),
                    store_conflict_f(V1AllPids,
                                     V2#cpg_data{local_count = LocalI + I,
                                                 local = NewLocal,
                                                 history = NewHistory},
                                     V1All);
                I < 0 ->
                    % remove
                    NewV2Pids = store_conflict_remove_entries(I, V2Pids),
                    NewHistory = store_conflict_remove_history(I, History, Pid),
                    store_conflict_f(V1AllPids,
                                     V2#cpg_data{local_count = LocalI + I,
                                                 local = NewV2Pids ++
                                                         LocalRest,
                                                 history = NewHistory},
                                     V1All);
                true ->
                    store_conflict_f(V1AllPids, V2, V1All)
            end;
        true ->
            % make sure there are equal counts of a remote pid
            % based on the external group pids
            {V1Pids, _} = lists:partition(Fpartition, V1All),
            {V2Pids, RemoteRest} = lists:partition(Fpartition, Remote),
            I = erlang:length(V1Pids) - erlang:length(V2Pids),
            if
                I > 0 ->
                    % add
                    NewRemote = store_conflict_add_entries(I, Remote, Pid),
                    NewHistory = store_conflict_add_history(I, History, Pid),
                    store_conflict_f(V1AllPids,
                                     V2#cpg_data{remote_count = RemoteI + I,
                                                 remote = NewRemote,
                                                 history = NewHistory},
                                     V1All);
                I < 0 ->
                    % remove
                    NewV2Pids = store_conflict_remove_entries(I, V2Pids),
                    NewHistory = store_conflict_remove_history(I, History, Pid),
                    store_conflict_f(V1AllPids,
                                     V2#cpg_data{remote_count = RemoteI + I,
                                                 remote = NewV2Pids ++
                                                          RemoteRest,
                                                 history = NewHistory},
                                     V1All);
                true ->
                    store_conflict_f(V1AllPids, V2, V1All)
            end
    end.

store_conflict(_,
               #cpg_data{local = V1Local,
                         remote = V1Remote}, V2) ->
    % V1 is external
    % V2 is internal
    V1All = V1Local ++ V1Remote,
    store_conflict_f(lists:usort(lists:map(fun(#cpg_data_pid{pid = Pid}) ->
                         Pid
                     end, V1All)),
                     V2, V1All).

store_new_group([], V2) ->
    V2;
store_new_group([#cpg_data_pid{pid = Pid} = E | OldEntries],
                 #cpg_data{local_count = LocalI,
                           local = Local,
                           remote_count = RemoteI,
                           remote = Remote} = V2) ->
    NewE = E#cpg_data_pid{monitor = erlang:monitor(process, Pid)},
    if
        node() =:= node(Pid) ->
            store_new_group(OldEntries,
                            V2#cpg_data{local_count = LocalI + 1,
                                        local = [NewE | Local]});
        true ->
            store_new_group(OldEntries,
                            V2#cpg_data{remote_count = RemoteI + 1,
                                        remote = [NewE | Remote]})
    end.

store(#state{groups = ExternalGroups,
             pids = ExternalPids},
      #state{scope = Scope,
             groups = Groups,
             pids = Pids} = State) ->
    % V1 is external
    % V2 is internal
    NewGroups = ?GROUP_STORAGE:fold(fun(GroupName,
        #cpg_data{local = V1Local,
                  remote = V1Remote,
                  history = V1History} = V1, T) ->
        case ?GROUP_STORAGE:is_key(GroupName, T) of
            true ->
                % merge the external group in
                ?GROUP_STORAGE:update(GroupName,
                    fun(V2) ->
                        store_conflict(GroupName, V1, V2)
                    end, T);
            false ->
                % create the new external group as an internal group
                ?GROUP_STORAGE:store(GroupName,
                    store_new_group(V1Local ++ V1Remote,
                                    #cpg_data{history = V1History}), T)
        end
    end, Groups, ExternalGroups),
    NewPids = dict:merge(fun(_, V1, V2) ->
                             lists:umerge(V2, V1)
                         end, ExternalPids, Pids),
    State#state{groups = ?CPG_ETS_CACHE_PUT(NewGroups),
                pids = NewPids}.

member_died(Pid, #state{pids = Pids} = State) ->
    case dict:find(Pid, Pids) of
        error ->
            % monitor message latency
            State;
        {ok, GroupNames} ->
            lists:foldl(fun(GroupName, S) ->
                leave_group(GroupName, Pid, S)
            end, State, GroupNames)
    end.

-ifdef(GROUP_NAME_PATTERN_MATCHING).
group_name_validate(Name) ->
    trie:is_pattern(Name),
    ok.
-else.
group_name_validate(_) ->
    ok.
-endif.

check_multi_call_replies([]) ->
    ok;
check_multi_call_replies([{_, ok} | Replies]) ->
    check_multi_call_replies(Replies);
check_multi_call_replies([{_, Result} | _]) ->
    Result.

whereis_name_random(1, [Pid]) ->
    Pid;
whereis_name_random(N, L) ->
    lists:nth(random(N), L).

delete_all(Elem, List) when is_list(List) ->
    delete_all(Elem, [], List).
delete_all(Elem, L, [Elem | T]) ->
    delete_all(Elem, L, T);
delete_all(Elem, L, [H | T]) ->
    delete_all(Elem, [H | L], T);
delete_all(_, L, []) ->
    lists:reverse(L).

-compile({inline, [{random,1}]}).

random(N) ->
    quickrand:uniform(N).

fake_put(Scope, G)
    when is_atom(Scope) ->
    G.

