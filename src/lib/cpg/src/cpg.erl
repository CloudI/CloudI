%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
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
%%% @version 1.3.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cpg).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).

%% external interface
-export([start_link/0,
         start_link/1,
         scope_exists/1,
         create/1,
         create/2,
         create/3,
         delete/1,
         delete/2,
         delete/3,
         join/1,
         join/2,
         join/3,
         join/4,
         leave/0,
         leave/1,
         leave/2,
         leave/3,
         leave/4,
         whereis_name/1,
         register_name/2,
         unregister_name/1,
         send/2,
         get_members/1,
         get_members/2,
         get_members/3,
         get_members/4,
         get_local_members/1,
         get_local_members/2,
         get_local_members/3,
         get_local_members/4,
         get_remote_members/1,
         get_remote_members/2,
         get_remote_members/3,
         get_remote_members/4,
         which_groups/0,
         which_groups/1,
         which_groups/2,
         which_groups/3,
         get_closest_pid/1,
         get_closest_pid/2,
         get_closest_pid/3,
         get_closest_pid/4,
         get_furthest_pid/1,
         get_furthest_pid/2,
         get_furthest_pid/3,
         get_furthest_pid/4,
         get_random_pid/1,
         get_random_pid/2,
         get_random_pid/3,
         get_random_pid/4,
         get_local_pid/1,
         get_local_pid/2,
         get_local_pid/3,
         get_local_pid/4,
         get_remote_pid/1,
         get_remote_pid/2,
         get_remote_pid/3,
         get_remote_pid/4,
         get_oldest_pid/1,
         get_oldest_pid/2,
         get_oldest_pid/3,
         get_oldest_pid/4,
         get_local_oldest_pid/1,
         get_local_oldest_pid/2,
         get_local_oldest_pid/3,
         get_local_oldest_pid/4,
         get_remote_oldest_pid/1,
         get_remote_oldest_pid/2,
         get_remote_oldest_pid/3,
         get_remote_oldest_pid/4,
         get_newest_pid/1,
         get_newest_pid/2,
         get_newest_pid/3,
         get_newest_pid/4,
         get_local_newest_pid/1,
         get_local_newest_pid/2,
         get_local_newest_pid/3,
         get_local_newest_pid/4,
         get_remote_newest_pid/1,
         get_remote_newest_pid/2,
         get_remote_newest_pid/3,
         get_remote_newest_pid/4]).

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
-type via_name() :: {global, scope(), name(), random} |
                    {global, scope(), name(), oldest} |
                    {global, scope(), name(), pos_integer()} |
                    {local, scope(), name(), random} |
                    {local, scope(), name(), oldest} |
                    {local, scope(), name(), pos_integer()} |
                    {global, scope(), name()} |
                    {local, scope(), name()} |
                    {global, name(), pos_integer()} |
                    {local, name(), pos_integer()} |
                    {global, name()} |
                    {local, name()} |
                    {scope(), name()} |
                    {name(), pos_integer()} |
                    name(). % for OTP behaviors
-export_type([scope/0, name/0, via_name/0]).

-compile({nowarn_unused_function,
          [{check_multi_call_replies, 1}]}).
-compile({inline, [{join_impl, 4},
                   {leave_impl, 4}]}).

-define(DEFAULT_TIMEOUT, 5000). % from gen_server:call/2

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Start process groups storage for the default scope.===
%% @end
%%-------------------------------------------------------------------------

-spec start_link() ->
    {ok, pid()} |
    {error, term()}.

start_link() ->
    start_link(?DEFAULT_SCOPE).

%%-------------------------------------------------------------------------
%% @doc
%% ===Start process groups storage for a specific scope.===
%% @end
%%-------------------------------------------------------------------------

-spec start_link(atom()) ->
    {ok, pid()} |
    {error, term()}.

start_link(Scope) when is_atom(Scope) ->
    true = (Scope /= local andalso
            Scope /= global),
    gen_server:start_link({local, Scope}, ?MODULE, [Scope], []).

%%-------------------------------------------------------------------------
%% @doc
%% ===Confirm a scope exists.===
%% @end
%%-------------------------------------------------------------------------

-spec scope_exists(Scope :: atom()) ->
    ok |
    {error, term()}.

scope_exists(Scope) ->
    case cpg_sup:start_scope(Scope) of
        ok ->
            ok;
        {error, {already_started, Scope}} ->
            ok;
        {error, _} = Error ->
            Error
    end.

-ifdef(GROUP_NAME_WITH_LOCAL_PIDS_ONLY).
-define(MEMBERSHIP_CHECK(F, TRUE, FALSE), case F of ok -> TRUE end).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a group explicitly no-op.===
%% @end
%%-------------------------------------------------------------------------

-spec create(name()) ->
    ok.

create(_) ->
    ok.

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a group explicitly in a specific scope no-op.===
%% @end
%%-------------------------------------------------------------------------

-spec create(scope(),
             name()) ->
    ok.

create(_, _) ->
    ok.

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a group explicitly in a specific scope no-op.===
%% @end
%%-------------------------------------------------------------------------

-spec create(scope(),
             name(),
             pos_integer() | infinity) ->
    ok.

create(_, _, _) ->
    ok.

%%-------------------------------------------------------------------------
%% @doc
%% ===Delete a group explicitly no-op.===
%% @end
%%-------------------------------------------------------------------------

-spec delete(name()) ->
    ok.

delete(_) ->
    ok.

%%-------------------------------------------------------------------------
%% @doc
%% ===Delete a group explicitly in a specific scope no-op.===
%% @end
%%-------------------------------------------------------------------------

-spec delete(scope(),
             name()) ->
    ok.

delete(_, _) ->
    ok.

%%-------------------------------------------------------------------------
%% @doc
%% ===Delete a group explicitly in a specific scope no-op.===
%% @end
%%-------------------------------------------------------------------------

-spec delete(scope(),
             name(),
             pos_integer() | infinity) ->
    ok.

delete(_, _, _) ->
    ok.

%%-------------------------------------------------------------------------
%% @doc
%% ===Join a specific group with self() as a local pid.===
%% A group is automatically created if it does not already exist.
%% @end
%%-------------------------------------------------------------------------

-spec join(name()) ->
    ok.

join(GroupName) ->
    join_impl(?DEFAULT_SCOPE, GroupName, self(), ?DEFAULT_TIMEOUT).

%%-------------------------------------------------------------------------
%% @doc
%% ===Join a specific group with the specified local pid or a specific group within a specific scope with self() as a local pid.===
%% The pid must be a local pid to justify not using a distributed transaction
%% since the cpg gen_server process acts like mutex lock, enforcing consistent
%% local state for all local pid process groups.  A group is automatically
%% created if it does not already exist.
%% @end
%%-------------------------------------------------------------------------

-spec join(name() | scope(),
           pid() | name()) ->
    ok.

join(GroupName, Pid)
    when is_pid(Pid) ->
    join_impl(?DEFAULT_SCOPE, GroupName, Pid, ?DEFAULT_TIMEOUT);

join(Scope, GroupName)
    when is_atom(Scope) ->
    join_impl(Scope, GroupName, self(), ?DEFAULT_TIMEOUT).

%%-------------------------------------------------------------------------
%% @doc
%% ===Join a specific group within a specific scope with a local pid.===
%% The pid must be a local pid to justify not using a distributed transaction
%% since the cpg gen_server process acts like mutex lock, enforcing consistent
%% local state for all local pid process groups.  A group is automatically
%% created if it does not already exist.
%% @end
%%-------------------------------------------------------------------------

-spec join(scope() | name(),
           name() | pid(),
           pid() | pos_integer() | infinity) ->
    ok.

join(Scope, GroupName, Pid)
    when is_atom(Scope), is_pid(Pid) ->
    join_impl(Scope, GroupName, Pid, ?DEFAULT_TIMEOUT);

join(GroupName, Pid, Timeout)
    when is_pid(Pid) ->
    join_impl(?DEFAULT_SCOPE, GroupName, Pid, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Join a specific group within a specific scope with a local pid.===
%% The pid must be a local pid to justify not using a distributed transaction
%% since the cpg gen_server process acts like mutex lock, enforcing consistent
%% local state for all local pid process groups.  A group is automatically
%% created if it does not already exist.
%% @end
%%-------------------------------------------------------------------------

-spec join(scope(),
           name(),
           pid(),
           pos_integer() | infinity) ->
    ok.

join(Scope, GroupName, Pid, Timeout)
    when is_atom(Scope), is_pid(Pid) ->
    join_impl(Scope, GroupName, Pid, Timeout).

join_impl(Scope, GroupName, Pid, Timeout)
    when node(Pid) =:= node() ->
    group_name_validate(GroupName),
    Request = {join, GroupName, Pid},
    ok = gen_server:call(Scope, Request, Timeout),
    gen_server:abcast(nodes(), Scope, Request),
    ok.

%%-------------------------------------------------------------------------
%% @doc
%% ===Leave all groups.===
%% @end
%%-------------------------------------------------------------------------

-spec leave() ->
    ok | error.

leave() ->
    leave(?DEFAULT_SCOPE, self(), ?DEFAULT_TIMEOUT).

%%-------------------------------------------------------------------------
%% @doc
%% ===Leave a specific group or all groups with a local pid.===
%% The group is automatically removed if it becomes empty.
%% @end
%%-------------------------------------------------------------------------

-spec leave(pid() | name()) ->
    ok | error.

leave(Pid)
    when is_pid(Pid) ->
    leave(?DEFAULT_SCOPE, Pid, ?DEFAULT_TIMEOUT);

leave(GroupName) ->
    leave(?DEFAULT_SCOPE, GroupName, self(), ?DEFAULT_TIMEOUT).

%%-------------------------------------------------------------------------
%% @doc
%% ===Leave a specific group or all groups with the specified local pid or a specific group within a specific scope with self() as a local pid.===
%% The pid must be a local pid to justify not using a distributed transaction
%% since the cpg gen_server process acts like mutex lock, enforcing consistent
%% local state for all local pid process groups.  The group will automatically
%% be removed if it becomes empty.
%% @end
%%-------------------------------------------------------------------------

-spec leave(name() | scope() | pid(),
            pid() | name() | pos_integer() | infinity) ->
    ok | error.

leave(Scope, Pid)
    when is_atom(Scope), is_pid(Pid) ->
    leave_impl(Scope, Pid, ?DEFAULT_TIMEOUT);

leave(Pid, Timeout)
    when is_pid(Pid) ->
    leave_impl(?DEFAULT_SCOPE, Pid, Timeout);

leave(GroupName, Pid)
    when is_pid(Pid) ->
    leave_impl(?DEFAULT_SCOPE, GroupName, Pid, ?DEFAULT_TIMEOUT);

leave(Scope, GroupName)
    when is_atom(Scope) ->
    leave_impl(Scope, GroupName, self(), ?DEFAULT_TIMEOUT).

%%-------------------------------------------------------------------------
%% @doc
%% ===Leave a specific group or all groups within a specific scope with a local pid.===
%% The pid must be a local pid to justify not using a distributed transaction
%% since the cpg gen_server process acts like mutex lock, enforcing consistent
%% local state for all local pid process groups.  The group will automatically
%% be removed if it becomes empty.
%% @end
%%-------------------------------------------------------------------------

-spec leave(scope() | name(),
            name() | pid(),
            pid() | pos_integer() | infinity) ->
    ok | error.

leave(Scope, Pid, Timeout)
    when is_atom(Scope), is_pid(Pid) ->
    leave_impl(Scope, Pid, Timeout);

leave(Scope, GroupName, Pid)
    when is_atom(Scope), is_pid(Pid) ->
    leave_impl(Scope, GroupName, Pid, ?DEFAULT_TIMEOUT);

leave(GroupName, Pid, Timeout)
    when is_pid(Pid) ->
    leave_impl(?DEFAULT_SCOPE, GroupName, Pid, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Leave a specific group or all groups within a specific scope with a local pid.===
%% The pid must be a local pid to justify not using a distributed transaction
%% since the cpg gen_server process acts like mutex lock, enforcing consistent
%% local state for all local pid process groups.  The group will automatically
%% be removed if it becomes empty.
%% @end
%%-------------------------------------------------------------------------

-spec leave(scope(),
            name(),
            pid(),
            pos_integer() | infinity) ->
    ok | error.

leave(Scope, GroupName, Pid, Timeout)
    when is_atom(Scope), is_pid(Pid) ->
    leave_impl(Scope, GroupName, Pid, Timeout).

leave_impl(Scope, Pid, Timeout)
    when node(Pid) =:= node() ->
    Request = {leave, Pid},
    case gen_server:call(Scope, Request, Timeout) of
        ok ->
            gen_server:abcast(nodes(), Scope, Request),
            ok;
        error ->
            error
    end.

leave_impl(Scope, GroupName, Pid, Timeout)
    when node(Pid) =:= node() ->
    group_name_validate(GroupName),
    Request = {leave, GroupName, Pid},
    case gen_server:call(Scope, Request, Timeout) of
        ok ->
            gen_server:abcast(nodes(), Scope, Request),
            ok;
        error ->
            error
    end.

-else. % GROUP_NAME_WITH_LOCAL_PIDS_ONLY not defined
-define(MEMBERSHIP_CHECK(F, TRUE, FALSE), case F of ok -> TRUE;
                                                    error -> FALSE end).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a group explicitly.===
%% The calling pid does not need to be a local pid because the function uses a
%% distributed transaction to enforce global consistency.
%% @end
%%-------------------------------------------------------------------------

-spec create(name()) ->
    ok | error.

create(GroupName) ->
    create(?DEFAULT_SCOPE, GroupName, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a group explicitly in a specific scope.===
%% The calling pid does not need to be a local pid because the function uses a
%% distributed transaction to enforce global consistency.
%% @end
%%-------------------------------------------------------------------------

-spec create(scope(),
             name()) ->
    ok | error.

create(Scope, GroupName)
    when is_atom(Scope) ->
    create(Scope, GroupName, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a group explicitly in a specific scope.===
%% The calling pid does not need to be a local pid because the function uses a
%% distributed transaction to enforce global consistency.
%% @end
%%-------------------------------------------------------------------------

-spec create(scope(),
             name(),
             pos_integer() | infinity) ->
    ok | error.

create(Scope, GroupName, Timeout)
    when is_atom(Scope) ->
    group_name_validate(GroupName),
    case global:trans({{Scope, GroupName}, self()},
                      fun() ->
                          gen_server:multi_call(Scope,
                                                {create, GroupName},
                                                Timeout)
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

-spec delete(name()) ->
    ok | error.

delete(GroupName) ->
    delete(?DEFAULT_SCOPE, GroupName, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Delete a group explicitly in a specific scope.===
%% The calling pid does not need to be a local pid because the function uses a
%% distributed transaction to enforce global consistency.
%% @end
%%-------------------------------------------------------------------------

-spec delete(scope(),
             name()) ->
    ok | error.

delete(Scope, GroupName)
    when is_atom(Scope) ->
    delete(Scope, GroupName, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Delete a group explicitly in a specific scope.===
%% The calling pid does not need to be a local pid because the function uses a
%% distributed transaction to enforce global consistency.
%% @end
%%-------------------------------------------------------------------------

-spec delete(scope(),
             name(),
             pos_integer() | infinity) ->
    ok | error.

delete(Scope, GroupName, Timeout)
    when is_atom(Scope) ->
    group_name_validate(GroupName),
    case global:trans({{Scope, GroupName}, self()},
                      fun() ->
                          gen_server:multi_call(Scope,
                                                {delete, GroupName},
                                                Timeout)
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

-spec join(name()) ->
    ok | error.

join(GroupName) ->
    join_impl(?DEFAULT_SCOPE, GroupName, self(), infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Join a specific group.===
%% The pid does not need to be a local pid because the function uses a
%% distributed transaction to enforce global consistency.
%% @end
%%-------------------------------------------------------------------------

-spec join(name(),
           pid()) ->
    ok | error.

join(GroupName, Pid)
    when is_pid(Pid) ->
    join_impl(?DEFAULT_SCOPE, GroupName, Pid, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Join a specific group in a specific scope.===
%% The pid does not need to be a local pid because the function uses a
%% distributed transaction to enforce global consistency.
%% @end
%%-------------------------------------------------------------------------

-spec join(scope() | name(),
           name() | pid(),
           pid() | pos_integer() | infinity) ->
    ok | error.

join(Scope, GroupName, Pid)
    when is_atom(Scope), is_pid(Pid) ->
    join_impl(Scope, GroupName, Pid, infinity);

join(GroupName, Pid, Timeout)
    when is_pid(Pid) ->
    join_impl(?DEFAULT_SCOPE, GroupName, Pid, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Join a specific group in a specific scope.===
%% The pid does not need to be a local pid because the function uses a
%% distributed transaction to enforce global consistency.
%% @end
%%-------------------------------------------------------------------------

-spec join(scope(),
           name(),
           pid(),
           pos_integer() | infinity) ->
    ok | error.

join(Scope, GroupName, Pid, Timeout)
    when is_atom(Scope), is_pid(Pid) ->
    join_impl(Scope, GroupName, Pid, Timeout).

join_impl(Scope, GroupName, Pid, Timeout) ->
    group_name_validate(GroupName),
    case global:trans({{Scope, GroupName}, self()},
                      fun() ->
                          gen_server:multi_call(Scope,
                                                {join, GroupName, Pid},
                                                Timeout)
                      end) of
        {[_ | _] = Replies, _} ->
            check_multi_call_replies(Replies);
        _ ->
            error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Leave a specific group or all groups.===
%% @end
%%-------------------------------------------------------------------------

-spec leave(pid() | name()) ->
    ok | error.

leave(Pid)
    when is_pid(Pid) ->
    leave_impl(?DEFAULT_SCOPE, self(), infinity);

leave(GroupName) ->
    leave_impl(?DEFAULT_SCOPE, GroupName, self(), infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Leave a specific group or all groups.===
%% The pid does not need to be a local pid because the function uses a
%% distributed transaction to enforce global consistency.
%% @end
%%-------------------------------------------------------------------------

-spec leave(name() | pid(),
            pid() | pos_integer() | infinity) ->
    ok | error.

leave(Pid, Timeout)
    when is_pid(Pid) ->
    leave_impl(?DEFAULT_SCOPE, Pid, Timeout);

leave(GroupName, Pid)
    when is_pid(Pid) ->
    leave_impl(?DEFAULT_SCOPE, GroupName, Pid, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Leave a specific group in a specific scope or all groups in a specific scope.===
%% The pid does not need to be a local pid because the function uses a
%% distributed transaction to enforce global consistency.
%% @end
%%-------------------------------------------------------------------------

-spec leave(scope() | name(),
            name() | pid(),
            pid() | pos_integer() | infinity) ->
    ok | error.

leave(Scope, Pid, Timeout)
    when is_atom(Scope), is_pid(Pid) ->
    leave_impl(Scope, Pid, Timeout);

leave(Scope, GroupName, Pid)
    when is_atom(Scope), is_pid(Pid) ->
    leave_impl(Scope, GroupName, Pid, infinity);

leave(GroupName, Pid, Timeout)
    when is_pid(Pid) ->
    leave_impl(?DEFAULT_SCOPE, GroupName, Pid, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Leave a specific group in a specific scope.===
%% The pid does not need to be a local pid because the function uses a
%% distributed transaction to enforce global consistency.
%% @end
%%-------------------------------------------------------------------------

-spec leave(scope(),
            name(),
            pid(),
            pos_integer() | infinity) ->
    ok | error.

leave(Scope, GroupName, Pid, Timeout)
    when is_atom(Scope), is_pid(Pid) ->
    leave_impl(Scope, GroupName, Pid, Timeout).

leave_impl(Scope, Pid, Timeout) ->
    case global:trans({Scope, self()},
                      fun() ->
                          gen_server:multi_call(Scope,
                                                {leave, Pid},
                                                Timeout)
                      end) of
        {[_ | _] = Replies, _} ->
            check_multi_call_replies(Replies);
        _ ->
            error
    end.

leave_impl(Scope, GroupName, Pid, Timeout) ->
    group_name_validate(GroupName),
    case global:trans({{Scope, GroupName}, self()},
                      fun() ->
                          gen_server:multi_call(Scope,
                                                {leave, GroupName, Pid},
                                                Timeout)
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

-spec whereis_name(via_name()) ->
    pid() | undefined.

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

-spec register_name(via_name(),
                    pid()) ->
    yes | no.

register_name({RegistrationType, Scope, GroupName, Lookup}, Pid)
    when (RegistrationType =:= global orelse
          RegistrationType =:= local), is_atom(Scope),
         (Lookup =:= random orelse Lookup =:= oldest) ->
    ?MEMBERSHIP_CHECK(join(Scope, GroupName, Pid), yes, no);

register_name({RegistrationType, Scope, GroupName, Instances}, Pid)
    when (RegistrationType =:= global orelse
          RegistrationType =:= local), is_atom(Scope),
         is_integer(Instances), Instances > 0 ->
    ?MEMBERSHIP_CHECK(join(Scope, GroupName, Pid), yes, no);

register_name({RegistrationType, Scope, GroupName}, Pid)
    when (RegistrationType =:= global orelse
          RegistrationType =:= local), is_atom(Scope) ->
    ?MEMBERSHIP_CHECK(join(Scope, GroupName, Pid), yes, no);

register_name({RegistrationType, GroupName, Instances}, Pid)
    when (RegistrationType =:= global orelse
          RegistrationType =:= local),
         is_integer(Instances), Instances > 0 ->
    ?MEMBERSHIP_CHECK(join(?DEFAULT_SCOPE, GroupName, Pid), yes, no);

register_name({RegistrationType, GroupName}, Pid)
    when (RegistrationType =:= global orelse
          RegistrationType =:= local) ->
    ?MEMBERSHIP_CHECK(join(?DEFAULT_SCOPE, GroupName, Pid), yes, no);

register_name({Scope, GroupName}, Pid)
    when is_atom(Scope) ->
    ?MEMBERSHIP_CHECK(join(Scope, GroupName, Pid), yes, no);

register_name({GroupName, Instances}, Pid)
    when is_integer(Instances), Instances > 0 ->
    ?MEMBERSHIP_CHECK(join(?DEFAULT_SCOPE, GroupName, Pid), yes, no);

register_name(GroupName, Pid) ->
    ?MEMBERSHIP_CHECK(join(?DEFAULT_SCOPE, GroupName, Pid), yes, no).

%%-------------------------------------------------------------------------
%% @doc
%% ===Function to provide via process registration functionality.===
%% Use within an OTP behavior by specifying {via, cpg, via_name()} for the
%% process registration (instead of {local, atom()} or {global, atom()})
%% @end
%%-------------------------------------------------------------------------

-spec unregister_name(via_name()) ->
    ok | error.

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

-spec send(via_name(),
           any()) ->
    pid().

send(ViaName, Msg) ->
    case whereis_name(ViaName) of
        undefined ->
            erlang:exit({badarg, {ViaName, Msg}});
        Pid when is_pid(Pid) ->
            Pid ! Msg,
            Pid
    end.

-type get_members_ret() ::
    {ok, name(), list(pid())} |
    {error, {no_such_group, name()}}.

-type gcp_error_reason() ::
    {no_process, name()} |
    {no_such_group, name()}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the members of a specific group.===
%% @end
%%-------------------------------------------------------------------------

-spec get_members(name()) ->
    get_members_ret().

get_members(GroupName) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_members, GroupName}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the members of a specific group while excluding a specific pid or within a specific scope.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_members(name() | scope(),
                  pid() | name() | pos_integer() | infinity) ->
    get_members_ret().

get_members(GroupName, Exclude)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_members, GroupName, Exclude});

get_members(Scope, GroupName)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_members, GroupName});

get_members(GroupName, Timeout) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_members, GroupName},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the members of a specific group within a specific scope while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_members(scope() | name(),
                  name() | pid(),
                  pid() | pos_integer() | infinity) ->
    get_members_ret().

get_members(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_members, GroupName, Exclude});

get_members(GroupName, Exclude, Timeout)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_members, GroupName, Exclude},
                    Timeout);

get_members(Scope, GroupName, Timeout)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_members, GroupName},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the members of a specific group within a specific scope while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_members(scope(),
                  name(),
                  pid(),
                  pos_integer() | infinity) ->
    get_members_ret().

get_members(Scope, GroupName, Exclude, Timeout)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_members, GroupName, Exclude},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get only the local members of a specific group.===
%% @end
%%-------------------------------------------------------------------------

-spec get_local_members(name()) ->
    get_members_ret().

get_local_members(GroupName) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_local_members, GroupName}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get only the local members of a specific group while excluding a specific pid or within a specific scope.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_local_members(name() | scope(),
                        pid() | name() | pos_integer() | infinity) ->
    get_members_ret().

get_local_members(GroupName, Exclude)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_local_members, GroupName, Exclude});

get_local_members(Scope, GroupName)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_local_members, GroupName});

get_local_members(GroupName, Timeout) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_local_members, GroupName},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get only the local members of a specific group within a specific scope while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_local_members(scope() | name(),
                        name() | pid(),
                        pid() | pos_integer() | infinity) ->
    get_members_ret().

get_local_members(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_local_members, GroupName, Exclude});

get_local_members(GroupName, Exclude, Timeout)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_local_members, GroupName, Exclude},
                    Timeout);

get_local_members(Scope, GroupName, Timeout)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_local_members, GroupName},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get only the local members of a specific group within a specific scope while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_local_members(scope(),
                        name(),
                        pid(),
                        pos_integer() | infinity) ->
    get_members_ret().

get_local_members(Scope, GroupName, Exclude, Timeout)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_local_members, GroupName, Exclude},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get only the remote members of a specific group.===
%% @end
%%-------------------------------------------------------------------------

-spec get_remote_members(name()) ->
    get_members_ret().

get_remote_members(GroupName) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_remote_members, GroupName}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get only the remote members of a specific group while excluding a specific pid or within a specific scope.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_remote_members(name() | scope(),
                         pid() | name() | pos_integer() | infinity) ->
    get_members_ret().

get_remote_members(GroupName, Exclude)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_remote_members, GroupName, Exclude});

get_remote_members(Scope, GroupName)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_remote_members, GroupName});

get_remote_members(GroupName, Timeout) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_remote_members, GroupName},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get only the remote members of a specific group within a specific scope while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_remote_members(scope() | name(),
                         name() | pid(),
                         pid() | pos_integer() | infinity) ->
    get_members_ret().

get_remote_members(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_remote_members, GroupName, Exclude});

get_remote_members(GroupName, Exclude, Timeout)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_remote_members, GroupName, Exclude},
                    Timeout);

get_remote_members(Scope, GroupName, Timeout)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_remote_members, GroupName},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get only the remote members of a specific group within a specific scope while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_remote_members(scope(),
                         name(),
                         pid(),
                         pos_integer() | infinity) ->
    get_members_ret().

get_remote_members(Scope, GroupName, Exclude, Timeout)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_remote_members, GroupName, Exclude},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get all the groups currently defined.===
%% @end
%%-------------------------------------------------------------------------

-spec which_groups() ->
    [name()].

which_groups() ->
    gen_server:call(?DEFAULT_SCOPE,
                    which_groups).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get all the groups currently defined within a specific scope.===
%% @end
%%-------------------------------------------------------------------------

-spec which_groups(scope() | pid() | pos_integer() | infinity) ->
    [name()].

which_groups(Scope)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    which_groups);

which_groups(Pid)
    when is_pid(Pid) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {which_groups, Pid});

which_groups(Timeout) ->
    gen_server:call(?DEFAULT_SCOPE,
                    which_groups,
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get all the groups currently defined within a specific scope.===
%% @end
%%-------------------------------------------------------------------------

-spec which_groups(scope() | pid(),
                   pid() | pos_integer() | infinity) ->
    [name()].

which_groups(Scope, Pid)
    when is_atom(Scope), is_pid(Pid) ->
    gen_server:call(Scope,
                    {which_groups, Pid});

which_groups(Scope, Timeout)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    which_groups,
                    Timeout);

which_groups(Pid, Timeout)
    when is_pid(Pid) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {which_groups, Pid},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get all the groups currently defined within a specific scope.===
%% @end
%%-------------------------------------------------------------------------

-spec which_groups(scope(),
                   pid(),
                   pos_integer() | infinity) ->
    [name()].

which_groups(Scope, Pid, Timeout)
    when is_atom(Scope), is_pid(Pid) ->
    gen_server:call(Scope,
                    {which_groups, Pid},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a group member, with local pids given priority.===
%% @end
%%-------------------------------------------------------------------------

-spec get_closest_pid(name()) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_closest_pid(GroupName) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_closest_pid, GroupName}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a group member, with local pids given priority while excluding a specific pid or within a specific scope.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_closest_pid(name() | scope(),
                      pid() | name() | pos_integer() | infinity) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_closest_pid(GroupName, Exclude)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_closest_pid, GroupName, Exclude});

get_closest_pid(Scope, GroupName)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_closest_pid, GroupName});

get_closest_pid(GroupName, Timeout) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_closest_pid, GroupName},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a group member within a specific scope, with local pids given priority while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_closest_pid(scope() | name(),
                      name() | pid(),
                      pid() | pos_integer() | infinity) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_closest_pid(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_closest_pid, GroupName, Exclude});

get_closest_pid(GroupName, Exclude, Timeout)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_closest_pid, GroupName, Exclude},
                    Timeout);

get_closest_pid(Scope, GroupName, Timeout)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_closest_pid, GroupName},
                    Timeout).


%%-------------------------------------------------------------------------
%% @doc
%% ===Get a group member within a specific scope, with local pids given priority while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_closest_pid(scope(),
                      name(),
                      pid(),
                      pos_integer() | infinity) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_closest_pid(Scope, GroupName, Exclude, Timeout)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_closest_pid, GroupName, Exclude},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a group member, with remote pids given priority.===
%% @end
%%-------------------------------------------------------------------------

-spec get_furthest_pid(name()) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_furthest_pid(GroupName) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_furthest_pid, GroupName}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a group member, with remote pids given priority while excluding a specific pid or within a specific scope.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_furthest_pid(name() | scope(),
                       pid() | name() | pos_integer() | infinity) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_furthest_pid(GroupName, Exclude)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_furthest_pid, GroupName, Exclude});

get_furthest_pid(Scope, GroupName)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_furthest_pid, GroupName});

get_furthest_pid(GroupName, Timeout) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_furthest_pid, GroupName},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a group member within a specific scope, with remote pids given priority while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_furthest_pid(scope() | name(),
                       name() | pid(),
                       pid() | pos_integer() | infinity) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_furthest_pid(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_furthest_pid, GroupName, Exclude});

get_furthest_pid(GroupName, Exclude, Timeout)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_furthest_pid, GroupName, Exclude},
                    Timeout);

get_furthest_pid(Scope, GroupName, Timeout)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_furthest_pid, GroupName},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a group member within a specific scope, with remote pids given priority while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_furthest_pid(scope(),
                       name(),
                       pid(),
                       pos_integer() | infinity) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_furthest_pid(Scope, GroupName, Exclude, Timeout)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_furthest_pid, GroupName, Exclude},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a group member.===
%% @end
%%-------------------------------------------------------------------------

-spec get_random_pid(name()) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_random_pid(GroupName) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_random_pid, GroupName}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a group member while excluding a specific pid or within a specific scope.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_random_pid(name() | scope(),
                     pid() | name() | pos_integer() | infinity) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_random_pid(GroupName, Exclude)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_random_pid, GroupName, Exclude});

get_random_pid(Scope, GroupName)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_random_pid, GroupName});

get_random_pid(GroupName, Timeout) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_random_pid, GroupName},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a group member within a specific scope while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_random_pid(scope() | name(),
                     name() | pid(),
                     pid() | pos_integer() | infinity) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_random_pid(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_random_pid, GroupName, Exclude});

get_random_pid(GroupName, Exclude, Timeout)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_random_pid, GroupName, Exclude},
                    Timeout);

get_random_pid(Scope, GroupName, Timeout)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_random_pid, GroupName},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a group member within a specific scope while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_random_pid(scope(),
                     name(),
                     pid(),
                     pos_integer() | infinity) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_random_pid(Scope, GroupName, Exclude, Timeout)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_random_pid, GroupName, Exclude},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a local group member.===
%% @end
%%-------------------------------------------------------------------------

-spec get_local_pid(name()) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_local_pid(GroupName) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_local_pid, GroupName}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a local group member while excluding a specific pid or within a specific scope.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_local_pid(name() | scope(),
                    pid() | name() | pos_integer() | infinity) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_local_pid(GroupName, Exclude)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_local_pid, GroupName, Exclude});

get_local_pid(Scope, GroupName)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_local_pid, GroupName});

get_local_pid(GroupName, Timeout) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_local_pid, GroupName},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a local group member within a specific scope, while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_local_pid(scope() | name(),
                    name() | pid(),
                    pid() | pos_integer() | infinity) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_local_pid(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_local_pid, GroupName, Exclude});

get_local_pid(GroupName, Exclude, Timeout)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_local_pid, GroupName, Exclude},
                    Timeout);

get_local_pid(Scope, GroupName, Timeout)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_local_pid, GroupName},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a local group member within a specific scope, while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_local_pid(scope(),
                    name(),
                    pid(),
                    pos_integer() | infinity) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_local_pid(Scope, GroupName, Exclude, Timeout)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_local_pid, GroupName, Exclude},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a remote group member.===
%% @end
%%-------------------------------------------------------------------------

-spec get_remote_pid(name()) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_remote_pid(GroupName) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_remote_pid, GroupName}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a remote group member while excluding a specific pid or within a specific scope.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_remote_pid(name() | scope(),
                     pid() | name() | pos_integer() | infinity) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_remote_pid(GroupName, Exclude)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_remote_pid, GroupName, Exclude});

get_remote_pid(Scope, GroupName)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_remote_pid, GroupName});

get_remote_pid(GroupName, Timeout) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_remote_pid, GroupName},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a remote group member within a specific scope, while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_remote_pid(scope() | name(),
                     name() | pid(),
                     pid() | pos_integer() | infinity) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_remote_pid(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_remote_pid, GroupName, Exclude});

get_remote_pid(GroupName, Exclude, Timeout)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_remote_pid, GroupName, Exclude},
                    Timeout);

get_remote_pid(Scope, GroupName, Timeout)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_remote_pid, GroupName},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a remote group member within a specific scope, while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_remote_pid(scope(),
                     name(),
                     pid(),
                     pos_integer() | infinity) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_remote_pid(Scope, GroupName, Exclude, Timeout)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_remote_pid, GroupName, Exclude},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the oldest group member.===
%% @end
%%-------------------------------------------------------------------------

-spec get_oldest_pid(name()) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_oldest_pid(GroupName) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_oldest_pid, GroupName}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the oldest group member while excluding a specific pid or within a specific scope.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_oldest_pid(name() | scope(),
                     pid() | name() | pos_integer() | infinity) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_oldest_pid(GroupName, Exclude)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_oldest_pid, GroupName, Exclude});

get_oldest_pid(Scope, GroupName)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_oldest_pid, GroupName});

get_oldest_pid(GroupName, Timeout) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_oldest_pid, GroupName},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the oldest group member within a specific scope, while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_oldest_pid(scope() | name(),
                     name() | pid(),
                     pid() | pos_integer() | infinity) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_oldest_pid(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_oldest_pid, GroupName, Exclude});

get_oldest_pid(GroupName, Exclude, Timeout)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_oldest_pid, GroupName, Exclude},
                    Timeout);

get_oldest_pid(Scope, GroupName, Timeout)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_oldest_pid, GroupName},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the oldest group member within a specific scope, while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_oldest_pid(scope(),
                     name(),
                     pid(),
                     pos_integer() | infinity) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_oldest_pid(Scope, GroupName, Exclude, Timeout)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_oldest_pid, GroupName, Exclude},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the oldest local group member.===
%% @end
%%-------------------------------------------------------------------------

-spec get_local_oldest_pid(name()) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_local_oldest_pid(GroupName) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_local_oldest_pid, GroupName}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the oldest local group member while excluding a specific pid or within a specific scope.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_local_oldest_pid(name() | scope(),
                           pid() | name() | pos_integer() | infinity) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_local_oldest_pid(GroupName, Exclude)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_local_oldest_pid, GroupName, Exclude});

get_local_oldest_pid(Scope, GroupName)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_local_oldest_pid, GroupName});

get_local_oldest_pid(GroupName, Timeout) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_local_oldest_pid, GroupName},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the oldest local group member within a specific scope, while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_local_oldest_pid(scope() | name(),
                           name() | pid(),
                           pid() | pos_integer() | infinity) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_local_oldest_pid(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_local_oldest_pid, GroupName, Exclude});

get_local_oldest_pid(GroupName, Exclude, Timeout)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_local_oldest_pid, GroupName, Exclude},
                    Timeout);

get_local_oldest_pid(Scope, GroupName, Timeout)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_local_oldest_pid, GroupName},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the oldest local group member within a specific scope, while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_local_oldest_pid(scope(),
                           name(),
                           pid(),
                           pos_integer() | infinity) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_local_oldest_pid(Scope, GroupName, Exclude, Timeout)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_local_oldest_pid, GroupName, Exclude},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the oldest remote group member.===
%% @end
%%-------------------------------------------------------------------------

-spec get_remote_oldest_pid(name()) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_remote_oldest_pid(GroupName) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_remote_oldest_pid, GroupName}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the oldest remote group member while excluding a specific pid or within a specific scope.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_remote_oldest_pid(name() | scope(),
                            pid() | name() | pos_integer() | infinity) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_remote_oldest_pid(GroupName, Exclude)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_remote_oldest_pid, GroupName, Exclude});

get_remote_oldest_pid(Scope, GroupName)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_remote_oldest_pid, GroupName});

get_remote_oldest_pid(GroupName, Timeout) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_remote_oldest_pid, GroupName},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the oldest remote group member within a specific scope, while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_remote_oldest_pid(scope() | name(),
                            name() | pid(),
                            pid() | pos_integer() | infinity) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_remote_oldest_pid(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_remote_oldest_pid, GroupName, Exclude});

get_remote_oldest_pid(GroupName, Exclude, Timeout)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_remote_oldest_pid, GroupName, Exclude},
                    Timeout);

get_remote_oldest_pid(Scope, GroupName, Timeout)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_remote_oldest_pid, GroupName},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the oldest remote group member within a specific scope, while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_remote_oldest_pid(scope(),
                            name(),
                            pid(),
                            pos_integer() | infinity) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_remote_oldest_pid(Scope, GroupName, Exclude, Timeout)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_remote_oldest_pid, GroupName, Exclude},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the newest group member.===
%% @end
%%-------------------------------------------------------------------------

-spec get_newest_pid(name()) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_newest_pid(GroupName) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_newest_pid, GroupName}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the newest group member while excluding a specific pid or within a specific scope.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_newest_pid(name() | scope(),
                     pid() | name() | pos_integer() | infinity) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_newest_pid(GroupName, Exclude)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_newest_pid, GroupName, Exclude});

get_newest_pid(Scope, GroupName)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_newest_pid, GroupName});

get_newest_pid(GroupName, Timeout) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_newest_pid, GroupName},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the newest group member within a specific scope, while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_newest_pid(scope() | name(),
                     name() | pid(),
                     pid() | pos_integer() | infinity) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_newest_pid(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_newest_pid, GroupName, Exclude});

get_newest_pid(GroupName, Exclude, Timeout)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_newest_pid, GroupName, Exclude},
                    Timeout);

get_newest_pid(Scope, GroupName, Timeout)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_newest_pid, GroupName},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the newest group member within a specific scope, while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_newest_pid(scope(),
                     name(),
                     pid(),
                     pos_integer() | infinity) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_newest_pid(Scope, GroupName, Exclude, Timeout)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_newest_pid, GroupName, Exclude},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the newest local group member.===
%% @end
%%-------------------------------------------------------------------------

-spec get_local_newest_pid(name()) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_local_newest_pid(GroupName) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_local_newest_pid, GroupName}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the newest local group member while excluding a specific pid or within a specific scope.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_local_newest_pid(name() | scope(),
                           pid() | name() | pos_integer() | infinity) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_local_newest_pid(GroupName, Exclude)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_local_newest_pid, GroupName, Exclude});

get_local_newest_pid(Scope, GroupName)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_local_newest_pid, GroupName});

get_local_newest_pid(GroupName, Timeout) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_local_newest_pid, GroupName},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the newest local group member within a specific scope, while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_local_newest_pid(scope() | name(),
                           name() | pid(),
                           pid() | pos_integer() | infinity) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_local_newest_pid(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_local_newest_pid, GroupName, Exclude});

get_local_newest_pid(GroupName, Exclude, Timeout)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_local_newest_pid, GroupName, Exclude},
                    Timeout);

get_local_newest_pid(Scope, GroupName, Timeout)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_local_newest_pid, GroupName},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the newest local group member within a specific scope, while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_local_newest_pid(scope(),
                           name(),
                           pid(),
                           pos_integer() | infinity) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_local_newest_pid(Scope, GroupName, Exclude, Timeout)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_local_newest_pid, GroupName, Exclude},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the newest remote group member.===
%% @end
%%-------------------------------------------------------------------------

-spec get_remote_newest_pid(name()) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_remote_newest_pid(GroupName) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_remote_newest_pid, GroupName}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the newest remote group member while excluding a specific pid or within a specific scope.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_remote_newest_pid(name() | scope(),
                            pid() | name() | pos_integer() | infinity) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_remote_newest_pid(GroupName, Exclude)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_remote_newest_pid, GroupName, Exclude});

get_remote_newest_pid(Scope, GroupName)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_remote_newest_pid, GroupName});

get_remote_newest_pid(GroupName, Timeout) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_remote_newest_pid, GroupName},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the newest remote group member within a specific scope, while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_remote_newest_pid(scope() | name(),
                            name() | pid(),
                            pid() | pos_integer() | infinity) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_remote_newest_pid(Scope, GroupName, Exclude)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_remote_newest_pid, GroupName, Exclude});

get_remote_newest_pid(GroupName, Exclude, Timeout)
    when is_pid(Exclude) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_remote_newest_pid, GroupName, Exclude},
                    Timeout);

get_remote_newest_pid(Scope, GroupName, Timeout)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {get_remote_newest_pid, GroupName},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the newest remote group member within a specific scope, while excluding a specific pid.===
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_remote_newest_pid(scope(),
                            name(),
                            pid(),
                            pos_integer() | infinity) ->
    {ok, name(), pid()} |
    {error, gcp_error_reason()}.

get_remote_newest_pid(Scope, GroupName, Exclude, Timeout)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_remote_newest_pid, GroupName, Exclude},
                    Timeout).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%% @private
%% @doc
%% @end

-spec init([scope()]) -> {ok, #state{}}.

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
                groups = cpg_data:get_empty_groups()}}.

%% @private
%% @doc
%% @end

-type call() :: {create, name()}
              | {delete, name()}
              | {join, name(), pid()}
              | {leave, name(), pid()}.

-spec handle_call(call(), _, #state{}) -> {reply, ok, #state{}}.

handle_call({create, GroupName}, _, State) ->
    {reply, ok, create_group(GroupName, State)};

handle_call({delete, GroupName}, _, State) ->
    {reply, ok, delete_group(GroupName, State)};

handle_call({join, GroupName, Pid}, _, State) ->
    {reply, ok, join_group(GroupName, Pid, State)};

handle_call({leave, Pid}, _,
            #state{pids = Pids} = State) ->
    case dict:find(Pid, Pids) of
        {ok, GroupNameList} ->
            NewState = lists:foldl(fun(GroupName, S) ->
                leave_group(GroupName, Pid, S)
            end, State, GroupNameList),
            {reply, ok, NewState};
        error ->
            {reply, error, State}
    end;

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

handle_call({which_groups, Pid}, _,
            #state{pids = Pids} = State) ->
    case dict:find(Pid, Pids) of
        {ok, L} ->
            {reply, L, State};
        error ->
            {reply, [], State}
    end;

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

-type cast() :: {exchange, node(), #state{}}.

-spec handle_cast(cast(), #state{}) -> {noreply, #state{}}.

handle_cast({exchange, Node, ExternalState}, State) ->
    ?LOG_INFO("received state from ~p", [Node]),
    {noreply, store(ExternalState, State)};

handle_cast({join, GroupName, Pid}, State) ->
    {noreply, join_group(GroupName, Pid, State)};

handle_cast({leave, GroupName, Pid},
            #state{pids = Pids} = State) ->
    Found = case dict:find(Pid, Pids) of
        error ->
            false;
        {ok, GroupNameList} ->
            lists:member(GroupName, GroupNameList)
    end,
    if
        Found ->
            {noreply, leave_group(GroupName, Pid, State)};
        true ->
            {noreply, State}
    end;

handle_cast(_, State) ->
    {noreply, State}.

%% @private
%% @doc
%% @end

-spec handle_info(tuple(), #state{}) -> {noreply, #state{}}.

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
    From ! {cloudi_cpg_data, Groups},
    {noreply, State};

handle_info(_, State) ->
    {noreply, State}.

%% @private
%% @doc
%% @end

-spec terminate(term(), #state{}) -> ok.

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

create_group(GroupName, #state{groups = Groups} = State) ->
    NewGroups = ?GROUP_STORAGE:update(GroupName,
        fun(OldValue) -> OldValue end, #cpg_data{}, Groups),
    State#state{groups = NewGroups}.

delete_group(GroupName, #state{groups = Groups,
                               pids = Pids} = State) ->
    case ?GROUP_STORAGE:find(GroupName, Groups) of
        error ->
            State;
        {ok, #cpg_data{local_count = 0,
                       remote_count = 0}} ->
            NewGroups = ?GROUP_STORAGE:erase(GroupName, Groups),
            State#state{groups = NewGroups};
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
            State#state{groups = NewGroups,
                        pids = NewPids}
    end.

join_group(GroupName, Pid, #state{groups = Groups,
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
    State#state{groups = NewGroups,
                pids = NewPids}.

leave_group(GroupName, Pid, #state{groups = Groups,
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
    NextGroups = if
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
    NewGroups = case ?GROUP_STORAGE:find(GroupName, NextGroups) of
        error ->
            NextGroups;
        {ok, #cpg_data{local_count = 0,
                       remote_count = 0}} ->
            % necessary so that pattern matching entries are not shadowed
            % by empty entries that provide exact matches
            ?GROUP_STORAGE:erase(GroupName, NextGroups);
        {ok, #cpg_data{}} ->
            NextGroups
    end,
    State#state{groups = NewGroups,
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
      #state{groups = Groups,
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
    State#state{groups = NewGroups,
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

