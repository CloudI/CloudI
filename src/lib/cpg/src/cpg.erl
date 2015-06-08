%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
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
%%% Copyright (c) 2011-2014 Michael Truog. All Rights Reserved.
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
%%% @copyright 2011-2014 Michael Truog
%%% @version 1.3.3 {@date} {@time}
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
         join_count/1,
         join_count/2,
         join_count/3,
         join_count/4,
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
         get_remote_newest_pid/4,
         reset/1,
         add_join_callback/2,
         add_join_callback/3,
         add_leave_callback/2,
         add_leave_callback/3,
         remove_join_callback/2,
         remove_join_callback/3,
         remove_leave_callback/2,
         remove_leave_callback/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         code_change/3, terminate/2]).

-include("cpg_constants.hrl").
-include("cpg_data.hrl").
-include("cpg_logging.hrl").

-type scope() :: atom().
-type name() :: any().
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
-type reason_join() :: join_local |
                       join_remote |
                       {exit, any()}.
-type reason_leave() :: leave_local |
                        leave_remote |
                        {exit, any()}.
-type callback_join() :: fun((any(), pid()) -> any()) |
                         fun((any(), pid(), reason_join()) -> any()).
-type callback_leave() :: fun((any(), pid()) -> any()) |
                          fun((any(), pid(), reason_leave()) -> any()).
-type callback() :: callback_join() |
                    callback_leave().
-export_type([scope/0,
              name/0,
              via_name/0,
              callback_join/0,
              callback_leave/0,
              callback/0]).

-record(state,
    {
        scope :: scope(),  % locally registered process name
        groups,            % GroupName -> #cpg_data{}
        pids = dict:new(), % pid() -> list(GroupName)
        callbacks = undefined :: undefined | pid(),
        listen :: visible | all
    }).

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
    case global:trans({{Scope, GroupName}, self()},
                      fun() ->
                          gen_server:multi_call([node() | nodes()],
                                                Scope,
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
    case global:trans({{Scope, GroupName}, self()},
                      fun() ->
                          gen_server:multi_call([node() | nodes()],
                                                Scope,
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
    case global:trans({{Scope, GroupName}, self()},
                      fun() ->
                          gen_server:multi_call([node() | nodes()],
                                                Scope,
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
%% ===Leave all groups.===
%% @end
%%-------------------------------------------------------------------------

-spec leave() ->
    ok | error.

leave() ->
    leave_impl(?DEFAULT_SCOPE, self(), infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Leave a specific group or all groups.===
%% @end
%%-------------------------------------------------------------------------

-spec leave(pid() | name()) ->
    ok | error.

leave(Pid)
    when is_pid(Pid) ->
    leave_impl(?DEFAULT_SCOPE, Pid, infinity);

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
                          gen_server:multi_call([node() | nodes()],
                                                Scope,
                                                {leave, Pid},
                                                Timeout)
                      end) of
        {[_ | _] = Replies, _} ->
            check_multi_call_replies(Replies);
        _ ->
            error
    end.

leave_impl(Scope, GroupName, Pid, Timeout) ->
    case global:trans({{Scope, GroupName}, self()},
                      fun() ->
                          gen_server:multi_call([node() | nodes()],
                                                Scope,
                                                {leave, GroupName, Pid},
                                                Timeout)
                      end) of
        {[_ | _] = Replies, _} ->
            check_multi_call_replies(Replies);
        _ ->
            error
    end.

check_multi_call_replies([]) ->
    ok;
check_multi_call_replies([{_, ok} | Replies]) ->
    check_multi_call_replies(Replies);
check_multi_call_replies([{_, Result} | _]) ->
    Result.

-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Provide a count of the previous joins of a specific group with self().===
%% @end
%%-------------------------------------------------------------------------

-spec join_count(name()) ->
    non_neg_integer().

join_count(GroupName) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {join_count, GroupName, self()}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Provide a count of the previous joins of a specific group with a specific pid.===
%% @end
%%-------------------------------------------------------------------------

-spec join_count(name() | scope(),
                 pid() | name()) ->
    non_neg_integer().

join_count(GroupName, Pid)
    when is_pid(Pid) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {join_count, GroupName, Pid});

join_count(Scope, GroupName)
    when is_atom(Scope) ->
    gen_server:call(Scope,
                    {join_count, GroupName, self()}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Provide a count of the previous joins of a specific group with a specific pid.===
%% @end
%%-------------------------------------------------------------------------

-spec join_count(scope() | name(),
                 name() | pid(),
                 pid() | pos_integer() | infinity) ->
    non_neg_integer().

join_count(Scope, GroupName, Pid)
    when is_atom(Scope), is_pid(Pid) ->
    gen_server:call(Scope,
                    {join_count, GroupName, self()});

join_count(GroupName, Pid, Timeout)
    when is_pid(Pid) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {join_count, GroupName, Pid}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Provide a count of the previous joins of a specific group with a specific pid.===
%% @end
%%-------------------------------------------------------------------------

-spec join_count(scope(),
                 name(),
                 pid(),
                 pos_integer() | infinity) ->
    non_neg_integer().

join_count(Scope, GroupName, Pid, Timeout)
    when is_atom(Scope), is_pid(Pid) ->
    gen_server:call(Scope,
                    {join_count, GroupName, Pid}, Timeout).

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

%%-------------------------------------------------------------------------
%% @doc
%% ===Reset any internal scope state.===
%% Updates cpg application node_type monitoring
%% @end
%%-------------------------------------------------------------------------

-spec reset(scope()) ->
    ok.

reset(Scope)
    when is_atom(Scope) ->
    gen_server:cast(Scope, reset).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add a join callback.===
%% @end
%%-------------------------------------------------------------------------

-spec add_join_callback(name(),
                        callback_join()) ->
    ok.

add_join_callback(GroupName, F)
    when (is_function(F, 2) orelse is_function(F, 3)) ->
    gen_server:cast(?DEFAULT_SCOPE, {add_join_callback, GroupName, F}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add a join callback.===
%% @end
%%-------------------------------------------------------------------------

-spec add_join_callback(scope(),
                        name(),
                        callback_join()) ->
    ok.

add_join_callback(Scope, GroupName, F)
    when is_atom(Scope), (is_function(F, 2) orelse is_function(F, 3)) ->
    gen_server:cast(Scope, {add_join_callback, GroupName, F}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add a leave callback.===
%% @end
%%-------------------------------------------------------------------------

-spec add_leave_callback(name(),
                         callback_leave()) ->
    ok.

add_leave_callback(GroupName, F)
    when (is_function(F, 2) orelse is_function(F, 3)) ->
    gen_server:cast(?DEFAULT_SCOPE, {add_leave_callback, GroupName, F}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add a leave callback.===
%% @end
%%-------------------------------------------------------------------------

-spec add_leave_callback(scope(),
                         name(),
                         callback_leave()) ->
    ok.

add_leave_callback(Scope, GroupName, F)
    when is_atom(Scope), (is_function(F, 2) orelse is_function(F, 3)) ->
    gen_server:cast(Scope, {add_leave_callback, GroupName, F}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove a join callback.===
%% @end
%%-------------------------------------------------------------------------

-spec remove_join_callback(name(),
                           callback_join()) ->
    ok.

remove_join_callback(GroupName, F)
    when (is_function(F, 2) orelse is_function(F, 3)) ->
    gen_server:cast(?DEFAULT_SCOPE, {remove_join_callback, GroupName, F}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove a join callback.===
%% @end
%%-------------------------------------------------------------------------

-spec remove_join_callback(scope(),
                           name(),
                           callback_join()) ->
    ok.

remove_join_callback(Scope, GroupName, F)
    when is_atom(Scope), (is_function(F, 2) orelse is_function(F, 3)) ->
    gen_server:cast(Scope, {remove_join_callback, GroupName, F}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove a leave callback.===
%% @end
%%-------------------------------------------------------------------------

-spec remove_leave_callback(name(),
                            callback_leave()) ->
    ok.

remove_leave_callback(GroupName, F)
    when (is_function(F, 2) orelse is_function(F, 3)) ->
    gen_server:cast(?DEFAULT_SCOPE, {remove_leave_callback, GroupName, F}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove a leave callback.===
%% @end
%%-------------------------------------------------------------------------

-spec remove_leave_callback(scope(),
                            name(),
                            callback_leave()) ->
    ok.

remove_leave_callback(Scope, GroupName, F)
    when is_atom(Scope), (is_function(F, 2) orelse is_function(F, 3)) ->
    gen_server:cast(Scope, {remove_leave_callback, GroupName, F}).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

%% @private
%% @doc
%% @end

init([Scope]) ->
    Listen = cpg_app:listen_type(),
    monitor_nodes(true, Listen),
    Ns = if
        Listen =:= visible ->
            nodes();
        Listen =:= all ->
            nodes(connected)
    end,
    lists:foreach(fun(N) ->
                          {Scope, N} ! {new, node()}
                          % data is not persistent in ets, so trust the
                          % State coming from other nodes if this server
                          % has restarted and wants previous state
                          %self() ! {nodeup, N} % pg2 does this
                  end, Ns),
    quickrand:seed(),
    {ok, #state{scope = Scope,
                groups = cpg_data:get_empty_groups(),
                listen = Listen}}.

%% @private
%% @doc
%% @end

handle_call({create, GroupName}, _, State) ->
    {reply, ok, create_group(GroupName, State)};

handle_call({delete, GroupName}, _, State) ->
    {reply, ok, delete_group(GroupName, State)};

handle_call({join, GroupName, Pid} = Request, _, State) ->
    abcast_hidden_nodes(Request, State),
    {reply, ok, join_group(GroupName, Pid, join_local, State)};

handle_call({leave, Pid} = Request, _,
            #state{pids = Pids} = State) ->
    case dict:find(Pid, Pids) of
        {ok, GroupNameList} ->
            abcast_hidden_nodes(Request, State),
            NewState = lists:foldl(fun(GroupName, S) ->
                leave_group_completely(GroupName, Pid, leave_local, S)
            end, State, GroupNameList),
            {reply, ok, NewState};
        error ->
            {reply, error, State}
    end;

handle_call({leave, GroupName, Pid} = Request, _,
            #state{pids = Pids} = State) ->
    Found = case dict:find(Pid, Pids) of
        error ->
            false;
        {ok, GroupNameList} ->
            lists:member(GroupName, GroupNameList)
    end,
    if
        Found ->
            abcast_hidden_nodes(Request, State),
            {reply, ok, leave_group(GroupName, Pid, leave_local, State)};
        true ->
            {reply, error, State}
    end;

handle_call({join_count, GroupName, Pid}, _,
            #state{groups = {DictI, GroupsData}} = State) ->
    Count = case DictI:find(GroupName, GroupsData) of
        error ->
            0;
        {ok, #cpg_data{local_count = 0,
                       remote_count = 0}} ->
            0;
        {ok, #cpg_data{history = History}} ->
            count(Pid, History)
    end,
    {reply, Count, State};

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
    {stop, lists:flatten(io_lib:format("Unknown call \"~p\"", [Request])),
     error, State}.

%% @private
%% @doc
%% @end

handle_cast({exchange, Node, ExternalState},
            #state{scope = Scope} = State) ->
    ?LOG_INFO("scope ~p received state from ~p", [Scope, Node]),
    {noreply, store(ExternalState, State)};

handle_cast({join, GroupName, Pid}, State) ->
    {noreply, join_group(GroupName, Pid, join_remote, State)};

handle_cast({leave, Pid},
            #state{pids = Pids} = State) ->
    case dict:find(Pid, Pids) of
        {ok, GroupNameList} ->
            NewState = lists:foldl(fun(GroupName, S) ->
                leave_group_completely(GroupName, Pid, leave_remote, S)
            end, State, GroupNameList),
            {noreply, NewState};
        error ->
            {noreply, State}
    end;

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
            {noreply, leave_group(GroupName, Pid, leave_remote, State)};
        true ->
            {noreply, State}
    end;

handle_cast(reset,
            #state{listen = OldListen} = State) ->
    Listen = cpg_app:listen_type(),
    if
        Listen /= OldListen ->
            monitor_nodes(false, OldListen),
            monitor_nodes(true, Listen);
        true ->
            ok
    end,
    {noreply, State#state{listen = Listen}};

handle_cast({add_join_callback, GroupName, F},
            #state{callbacks = Callbacks} = State) ->
    NewCallbacks = cpg_callbacks:add_join(Callbacks, GroupName, F),
    {noreply, State#state{callbacks = NewCallbacks}};

handle_cast({add_leave_callback, GroupName, F},
            #state{callbacks = Callbacks} = State) ->
    NewCallbacks = cpg_callbacks:add_leave(Callbacks, GroupName, F),
    {noreply, State#state{callbacks = NewCallbacks}};

handle_cast({remove_join_callback, GroupName, F},
            #state{callbacks = Callbacks} = State) ->
    NewCallbacks = cpg_callbacks:remove_join(Callbacks, GroupName, F),
    {noreply, State#state{callbacks = NewCallbacks}};

handle_cast({remove_leave_callback, GroupName, F},
            #state{callbacks = Callbacks} = State) ->
    NewCallbacks = cpg_callbacks:remove_leave(Callbacks, GroupName, F),
    {noreply, State#state{callbacks = NewCallbacks}};

handle_cast(Request, State) ->
    {stop, lists:flatten(io_lib:format("Unknown cast \"~p\"", [Request])),
     State}.

%% @private
%% @doc
%% @end

handle_info({'DOWN', _MonitorRef, process, Pid, Info}, State) ->
    {noreply, member_died(Pid, {exit, Info}, State)};

handle_info({nodeup, Node, _},
            #state{scope = Scope} = State) ->
    gen_server:cast({Scope, Node}, {exchange, node(), State}),
    {noreply, State};

handle_info({nodedown, _, _}, State) ->
    % rely on pid monitors for internal group changes
    {noreply, State};

handle_info({new, Node},
            #state{scope = Scope} = State) ->
    gen_server:cast({Scope, Node}, {exchange, node(), State}),
    {noreply, State};

handle_info({cpg_data, From},
            #state{groups = Groups} = State) ->
    From ! {cloudi_cpg_data, Groups},
    {noreply, State};

handle_info(Request, State) ->
    {stop, lists:flatten(io_lib:format("Unknown info \"~p\"", [Request])),
     State}.

%% @private
%% @doc
%% @end

terminate(Reason,
          #state{callbacks = Callbacks}) ->
    cpg_callbacks:stop_link(Callbacks, Reason),
    ok.

%% @private
%% @doc
%% @end

code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

monitor_nodes(Flag, Listen) ->
    net_kernel:monitor_nodes(Flag, [{node_type, Listen}]).

abcast_hidden_nodes(_, #state{listen = visible}) ->
    ok;
abcast_hidden_nodes(Request, #state{scope = Scope,
                                    listen = all}) ->
    case nodes(hidden) of
        [] ->
            ok;
        [_ | _] = HiddenNodes ->
            gen_server:abcast(HiddenNodes, Scope, Request)
    end.

create_group(GroupName,
             #state{groups = {DictI, GroupsData}} = State) ->
    NewGroupsData = DictI:update(GroupName,
        fun(OldValue) -> OldValue end, #cpg_data{}, GroupsData),
    State#state{groups = {DictI, NewGroupsData}}.

delete_group(GroupName,
             #state{groups = {DictI, GroupsData},
                    pids = Pids,
                    callbacks = Callbacks} = State) ->
    case DictI:find(GroupName, GroupsData) of
        error ->
            State;
        {ok, #cpg_data{local_count = 0,
                       remote_count = 0}} ->
            NewGroupsData = DictI:erase(GroupName, GroupsData),
            State#state{groups = {DictI, NewGroupsData}};
        {ok, #cpg_data{local = Local,
                       remote = Remote}} ->
            NewPids = lists:foldl(fun(#cpg_data_pid{pid = Pid,
                                                    monitor = Ref}, P) ->
                true = erlang:demonitor(Ref, [flush]),
                cpg_callbacks:notify_leave(Callbacks, GroupName, Pid,
                                           leave_local),
                dict:update(Pid,
                            fun(OldValue) ->
                                lists:delete(GroupName, OldValue)
                            end, P)
            end, Pids, Local ++ Remote),
            NewGroupsData = DictI:erase(GroupName, GroupsData),
            State#state{groups = {DictI, NewGroupsData},
                        pids = NewPids}
    end.

join_group(GroupName, Pid, Reason,
           #state{groups = {DictI, GroupsData},
                  pids = Pids,
                  callbacks = Callbacks} = State) ->
    Entry = #cpg_data_pid{pid = Pid,
                          monitor = erlang:monitor(process, Pid)},
    NewGroupsData = if
        node() =:= node(Pid) ->
            DictI:update(GroupName,
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
                GroupsData);
        true ->
            DictI:update(GroupName,
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
                GroupsData)
    end,
    GroupNameList = [GroupName],
    NewPids = dict:update(Pid,
                          fun(OldValue) ->
                              lists:umerge(OldValue, GroupNameList)
                          end,
                          GroupNameList, Pids),
    cpg_callbacks:notify_join(Callbacks, GroupName, Pid, Reason),
    State#state{groups = {DictI, NewGroupsData},
                pids = NewPids}.

leave_group(GroupName, Pid, Reason,
            #state{groups = {DictI, GroupsData},
                   pids = Pids,
                   callbacks = Callbacks} = State) ->
    Fselect = fun(#cpg_data_pid{pid = P, monitor = Ref}) ->
        if 
            P == Pid ->
                true = erlang:demonitor(Ref, [flush]),
                true;
            true ->
                false
        end
    end,
    NextGroupsData = if
        node() =:= node(Pid) ->
            DictI:update(GroupName,
                fun(#cpg_data{local_count = LocalI,
                              local = Local,
                              history = History} = OldValue) ->
                    {OldLocalEntry,
                     NewLocal} = select(Fselect, Local),
                    I = if
                        OldLocalEntry =:= undefined ->
                            0;
                        true ->
                            1
                    end,
                    cpg_callbacks:notify_leave(Callbacks,
                                               GroupName, Pid, Reason, I),
                    NewHistory = lists:reverse(lists:delete(Pid,
                        lists:reverse(History))),
                    OldValue#cpg_data{local_count = LocalI - I,
                                      local = NewLocal,
                                      history = NewHistory}
                end, GroupsData);
        true ->
            DictI:update(GroupName,
                fun(#cpg_data{remote_count = RemoteI,
                              remote = Remote,
                              history = History} = OldValue) ->
                    {OldRemoteEntry,
                     NewRemote} = select(Fselect, Remote),
                    I = if
                        OldRemoteEntry =:= undefined ->
                            0;
                        true ->
                            1
                    end,
                    cpg_callbacks:notify_leave(Callbacks,
                                               GroupName, Pid, Reason, I),
                    NewHistory = lists:reverse(lists:delete(Pid,
                        lists:reverse(History))),
                    OldValue#cpg_data{remote_count = RemoteI - I,
                                      remote = NewRemote,
                                      history = NewHistory}
                end, GroupsData)
    end,
    {NewGroupsData, NewPids} = case DictI:find(GroupName, NextGroupsData) of
        error ->
            {NextGroupsData, Pids};
        {ok, #cpg_data{local_count = 0,
                       remote_count = 0}} ->
            {DictI:erase(GroupName, NextGroupsData),
             dict:update(Pid,
                         fun(OldValue) ->
                             lists:delete(GroupName, OldValue)
                         end,
                         Pids)};
        {ok, #cpg_data{}} ->
            {NextGroupsData, Pids}
    end,
    State#state{groups = {DictI, NewGroupsData},
                pids = NewPids}.

leave_group_completely(GroupName, Pid, Reason,
                       #state{groups = {DictI, GroupsData},
                              pids = Pids,
                              callbacks = Callbacks} = State) ->
    Fpartition = fun(#cpg_data_pid{pid = P, monitor = Ref}) ->
        if 
            P == Pid ->
                true = erlang:demonitor(Ref, [flush]),
                true;
            true ->
                false
        end
    end,
    NextGroupsData = if
        node() =:= node(Pid) ->
            DictI:update(GroupName,
                fun(#cpg_data{local_count = LocalI,
                              local = Local,
                              history = History} = OldValue) ->
                    {OldLocal,
                     NewLocal} = lists:partition(Fpartition, Local),
                    I = erlang:length(OldLocal),
                    cpg_callbacks:notify_leave(Callbacks,
                                               GroupName, Pid, Reason, I),
                    OldValue#cpg_data{local_count = LocalI - I,
                                      local = NewLocal,
                                      history = delete_all(Pid, History)}
                end, GroupsData);
        true ->
            DictI:update(GroupName,
                fun(#cpg_data{remote_count = RemoteI,
                              remote = Remote,
                              history = History} = OldValue) ->
                    {OldRemote,
                     NewRemote} = lists:partition(Fpartition, Remote),
                    I = erlang:length(OldRemote),
                    cpg_callbacks:notify_leave(Callbacks,
                                               GroupName, Pid, Reason, I),
                    OldValue#cpg_data{remote_count = RemoteI - I,
                                      remote = NewRemote,
                                      history = delete_all(Pid, History)}
                end, GroupsData)
    end,
    NewPids = dict:update(Pid,
                          fun(OldValue) ->
                              lists:delete(GroupName, OldValue)
                          end,
                          Pids),
    NewGroupsData = case DictI:find(GroupName, NextGroupsData) of
        error ->
            NextGroupsData;
        {ok, #cpg_data{local_count = 0,
                       remote_count = 0}} ->
            % necessary so that pattern matching entries are not shadowed
            % by empty entries that provide exact matches
            DictI:erase(GroupName, NextGroupsData);
        {ok, #cpg_data{}} ->
            NextGroupsData
    end,
    State#state{groups = {DictI, NewGroupsData},
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

store_conflict_f([], V2, _, _, _) ->
    V2;
store_conflict_f([Pid | V1AllPids],
                 #cpg_data{local_count = LocalI,
                           local = Local,
                           remote_count = RemoteI,
                           remote = Remote,
                           history = History} = V2,
                 V1All, GroupName,
                 #state{callbacks = Callbacks} = State) ->
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
                    cpg_callbacks:notify_join(Callbacks, GroupName, Pid,
                                              join_remote, I),
                    NewLocal = store_conflict_add_entries(I, Local, Pid),
                    NewHistory = store_conflict_add_history(I, History, Pid),
                    store_conflict_f(V1AllPids,
                                     V2#cpg_data{local_count = LocalI + I,
                                                 local = NewLocal,
                                                 history = NewHistory},
                                     V1All, GroupName, State);
                I < 0 ->
                    % remove
                    cpg_callbacks:notify_leave(Callbacks, GroupName, Pid,
                                               leave_remote, I * -1),
                    NewV2Pids = store_conflict_remove_entries(I, V2Pids),
                    NewHistory = store_conflict_remove_history(I, History, Pid),
                    store_conflict_f(V1AllPids,
                                     V2#cpg_data{local_count = LocalI + I,
                                                 local = NewV2Pids ++
                                                         LocalRest,
                                                 history = NewHistory},
                                     V1All, GroupName, State);
                true ->
                    store_conflict_f(V1AllPids, V2, V1All, GroupName, State)
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
                    cpg_callbacks:notify_join(Callbacks, GroupName, Pid,
                                              join_remote, I),
                    NewRemote = store_conflict_add_entries(I, Remote, Pid),
                    NewHistory = store_conflict_add_history(I, History, Pid),
                    store_conflict_f(V1AllPids,
                                     V2#cpg_data{remote_count = RemoteI + I,
                                                 remote = NewRemote,
                                                 history = NewHistory},
                                     V1All, GroupName, State);
                I < 0 ->
                    % remove
                    cpg_callbacks:notify_leave(Callbacks, GroupName, Pid,
                                               leave_remote, I * -1),
                    NewV2Pids = store_conflict_remove_entries(I, V2Pids),
                    NewHistory = store_conflict_remove_history(I, History, Pid),
                    store_conflict_f(V1AllPids,
                                     V2#cpg_data{remote_count = RemoteI + I,
                                                 remote = NewV2Pids ++
                                                          RemoteRest,
                                                 history = NewHistory},
                                     V1All, GroupName, State);
                true ->
                    store_conflict_f(V1AllPids, V2, V1All, GroupName, State)
            end
    end.

store_conflict(GroupName,
               #cpg_data{local = V1Local,
                         remote = V1Remote}, V2, State) ->
    % V1 is external
    % V2 is internal
    V1All = V1Local ++ V1Remote,
    store_conflict_f(lists:usort(lists:map(fun(#cpg_data_pid{pid = Pid}) ->
                         Pid
                     end, V1All)),
                     V2, V1All, GroupName, State).

store_new_group([], V2, _, _) ->
    V2;
store_new_group([#cpg_data_pid{pid = Pid} = E | OldEntries],
                 #cpg_data{local_count = LocalI,
                           local = Local,
                           remote_count = RemoteI,
                           remote = Remote} = V2,
                GroupName,
                #state{callbacks = Callbacks} = State) ->
    NewE = E#cpg_data_pid{monitor = erlang:monitor(process, Pid)},
    cpg_callbacks:notify_join(Callbacks, GroupName, Pid, join_remote),
    if
        node() =:= node(Pid) ->
            store_new_group(OldEntries,
                            V2#cpg_data{local_count = LocalI + 1,
                                        local = [NewE | Local]},
                            GroupName, State);
        true ->
            store_new_group(OldEntries,
                            V2#cpg_data{remote_count = RemoteI + 1,
                                        remote = [NewE | Remote]},
                            GroupName, State)
    end.

store(#state{groups = {DictI, ExternalGroupsData},
             pids = ExternalPids},
      #state{groups = {DictI, GroupsData},
             pids = Pids} = State) ->
    % V1 is external
    % V2 is internal
    NewGroupsData = DictI:fold(fun(GroupName,
                                   #cpg_data{local = V1Local,
                                             remote = V1Remote,
                                             history = V1History} = V1, T) ->
        case DictI:is_key(GroupName, T) of
            true ->
                % merge the external group in
                DictI:update(GroupName,
                             fun(V2) ->
                                 store_conflict(GroupName, V1, V2, State)
                             end, T);
            false ->
                % create the new external group as an internal group
                DictI:store(GroupName,
                            store_new_group(V1Local ++ V1Remote,
                                            #cpg_data{history = V1History},
                                            GroupName, State), T)
        end
    end, GroupsData, ExternalGroupsData),
    NewPids = dict:merge(fun(_, V1, V2) ->
                             lists:umerge(V2, V1)
                         end, ExternalPids, Pids),
    State#state{groups = {DictI, NewGroupsData},
                pids = NewPids}.

member_died(Pid, Reason, #state{pids = Pids} = State) ->
    case dict:find(Pid, Pids) of
        error ->
            % monitor message latency
            State;
        {ok, GroupNames} ->
            #state{pids = NewPids} = NewState = lists:foldl(fun(GroupName, S) ->
                leave_group_completely(GroupName, Pid, Reason, S)
            end, State, GroupNames),
            NewState#state{pids = dict:erase(Pid, NewPids)}
    end.

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

count(Elem, List) when is_list(List) ->
    count(List, 0, Elem).
count([], I, _) ->
    I;
count([Elem | T], I, Elem) ->
    count(T, I + 1, Elem);
count([_ | T], I, Elem) ->
    count(T, I, Elem).

select(F, List) when is_function(F, 1), is_list(List) ->
    select(List, [], F).
select([], Output, _) ->
    {undefined, lists:reverse(Output)};
select([H | T], Output, F) ->
    case F(H) of
        true ->
            {H, lists:reverse(Output) ++ T};
        false ->
            select(T, [H | Output], F)
    end.

-compile({inline, [{random,1}]}).

random(N) ->
    quickrand:uniform(N).

