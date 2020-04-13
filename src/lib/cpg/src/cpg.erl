%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Process Groups (CPG)==
%%% cpg relies on distributed Erlang for node communication, which means
%%% a fully connected network topology is created.  With Distributed Erlang,
%%% Erlang pids either exist on the local node or a remote node
%%% (which shares a connection with the local node,
%%%  so only 1 node hop is necessary in the worst case).
%%% @end
%%%
%%% Copyright (c) 2011-2020 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2011-2020 Michael Truog
%%% @version 1.8.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cpg).
-author('mjtruog at protonmail dot com').

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
         join_counts/1,
         join_counts/2,
         join_counts/3,
         join_counts/4,
         leave/0,
         leave/1,
         leave/2,
         leave/3,
         leave/4,
         leave_counts/1,
         leave_counts/2,
         leave_counts/3,
         leave_counts/4,
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
         which_groups_counts/1,
         which_groups_counts/2,
         which_groups_counts/3,
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
         reset_all/1,
         listen_nodes/2,
         visible_nodes/1,
         hidden_nodes/1,
         ignore_node/2,
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
-type listen() :: visible |
                  all.
-type reason_join() :: join_local |
                       join_remote.
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
              listen/0,
              callback_join/0,
              callback_leave/0,
              callback/0]).

-record(state_monitor,
    {
        monitor :: reference() | cpg_node_monitor:process(),
        names :: list(name())
    }).

-record(state,
    {
        node_name :: nonempty_string(),
        scope :: scope(), % locally registered process name
        groups :: cpg_data:state(), % GroupName -> #cpg_data{}
        monitors = #{} :: #{pid() := #state_monitor{}},
        node_monitors = #{} :: #{node() := cpg_node_monitor:process()},
        callbacks = undefined :: undefined | pid(),
        listen :: visible | all
    }).

-compile({inline,
          [{join_impl, 4},
           {join_counts_impl, 4},
           {leave_impl, 4},
           {leave_counts_impl, 4}]}).

-define(NODETOOL_SUFFIX, "_script_process").
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

%%-------------------------------------------------------------------------
%% @doc
%% ===Join specific groups a specific number of times.===
%% @end
%%-------------------------------------------------------------------------

-spec join_counts(list({name(), pos_integer()})) ->
    ok | error.

join_counts(Counts)
    when is_list(Counts) ->
    join_counts_impl(?DEFAULT_SCOPE, Counts, self(), ?DEFAULT_TIMEOUT).

%%-------------------------------------------------------------------------
%% @doc
%% ===Join specific groups a specific number of times.===
%% @end
%%-------------------------------------------------------------------------

-spec join_counts(list({name(), pos_integer()}) | scope(),
                  pid() | list({name(), pos_integer()})) ->
    ok | error.

join_counts(Counts, Pid)
    when is_list(Counts), is_pid(Pid) ->
    join_counts_impl(?DEFAULT_SCOPE, Counts, Pid, ?DEFAULT_TIMEOUT);

join_counts(Scope, Counts)
    when is_atom(Scope), is_list(Counts) ->
    join_counts_impl(Scope, Counts, self(), ?DEFAULT_TIMEOUT).

%%-------------------------------------------------------------------------
%% @doc
%% ===Join specific groups a specific number of times.===
%% @end
%%-------------------------------------------------------------------------

-spec join_counts(scope() | list({name(), pos_integer()}),
                  list({name(), pos_integer()}) | pid(),
                  pid() | pos_integer() | infinity) ->
    ok | error.

join_counts(Scope, Counts, Pid)
    when is_atom(Scope), is_list(Counts), is_pid(Pid) ->
    join_counts_impl(Scope, Counts, Pid, ?DEFAULT_TIMEOUT);

join_counts(Counts, Pid, Timeout)
    when is_list(Counts), is_pid(Pid) ->
    join_counts_impl(?DEFAULT_SCOPE, Counts, Pid, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Join specific groups a specific number of times.===
%% @end
%%-------------------------------------------------------------------------

-spec join_counts(scope(),
                  list({name(), pos_integer()}),
                  pid(),
                  pos_integer() | infinity) ->
    ok | error.

join_counts(Scope, Counts, Pid, Timeout)
    when is_atom(Scope), is_list(Counts), is_pid(Pid) ->
    join_counts_impl(Scope, Counts, Pid, Timeout).

join_impl(Scope, GroupName, Pid, Timeout)
    when node(Pid) =:= node() ->
    Request = {join, GroupName, Pid},
    {ok, VisibleNodes} = gen_server:call(Scope, Request, Timeout),
    abcast = gen_server:abcast(VisibleNodes, Scope, Request),
    ok.

join_counts_impl(Scope, Counts, Pid, Timeout)
    when node(Pid) =:= node() ->
    if
        Counts == [] ->
            ok;
        true ->
            Request = {join_counts, Counts, Pid},
            case gen_server:call(Scope, Request, Timeout) of
                {ok, VisibleNodes} ->
                    abcast = gen_server:abcast(VisibleNodes, Scope, Request),
                    ok;
                error ->
                    error
            end
    end.

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

%%-------------------------------------------------------------------------
%% @doc
%% ===Leave specific groups a specific number of times.===
%% @end
%%-------------------------------------------------------------------------

-spec leave_counts(list({name(), pos_integer()})) ->
    ok | error.

leave_counts(Counts)
    when is_list(Counts) ->
    leave_counts_impl(?DEFAULT_SCOPE, Counts, self(), ?DEFAULT_TIMEOUT).

%%-------------------------------------------------------------------------
%% @doc
%% ===Leave specific groups a specific number of times.===
%% @end
%%-------------------------------------------------------------------------

-spec leave_counts(list({name(), pos_integer()}) | scope(),
                   pid() | list({name(), pos_integer()})) ->
    ok | error.

leave_counts(Counts, Pid)
    when is_list(Counts), is_pid(Pid) ->
    leave_counts_impl(?DEFAULT_SCOPE, Counts, Pid, ?DEFAULT_TIMEOUT);

leave_counts(Scope, Counts)
    when is_atom(Scope), is_list(Counts) ->
    leave_counts_impl(Scope, Counts, self(), ?DEFAULT_TIMEOUT).

%%-------------------------------------------------------------------------
%% @doc
%% ===Leave specific groups a specific number of times.===
%% @end
%%-------------------------------------------------------------------------

-spec leave_counts(scope() | list({name(), pos_integer()}),
                   list({name(), pos_integer()}) | pid(),
                   pid() | pos_integer() | infinity) ->
    ok | error.

leave_counts(Scope, Counts, Pid)
    when is_atom(Scope), is_list(Counts), is_pid(Pid) ->
    leave_counts_impl(Scope, Counts, Pid, ?DEFAULT_TIMEOUT);

leave_counts(Counts, Pid, Timeout)
    when is_list(Counts), is_pid(Pid) ->
    leave_counts_impl(?DEFAULT_SCOPE, Counts, Pid, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Leave specific groups a specific number of times.===
%% @end
%%-------------------------------------------------------------------------

-spec leave_counts(scope(),
                   list({name(), pos_integer()}),
                   pid(),
                   pos_integer() | infinity) ->
    ok | error.

leave_counts(Scope, Counts, Pid, Timeout)
    when is_atom(Scope), is_list(Counts), is_pid(Pid) ->
    leave_counts_impl(Scope, Counts, Pid, Timeout).

leave_impl(Scope, Pid, Timeout)
    when node(Pid) =:= node() ->
    Request = {leave, Pid},
    case gen_server:call(Scope, Request, Timeout) of
        {ok, VisibleNodes} ->
            abcast = gen_server:abcast(VisibleNodes, Scope, Request),
            ok;
        error ->
            error
    end.

leave_impl(Scope, GroupName, Pid, Timeout)
    when node(Pid) =:= node() ->
    Request = {leave, GroupName, Pid},
    case gen_server:call(Scope, Request, Timeout) of
        {ok, VisibleNodes} ->
            abcast = gen_server:abcast(VisibleNodes, Scope, Request),
            ok;
        error ->
            error
    end.

leave_counts_impl(Scope, Counts, Pid, Timeout)
    when node(Pid) =:= node() ->
    if
        Counts == [] ->
            ok;
        true ->
            Request = {leave_counts, Counts, Pid},
            case gen_server:call(Scope, Request, Timeout) of
                {ok, VisibleNodes} ->
                    abcast = gen_server:abcast(VisibleNodes, Scope, Request),
                    ok;
                error ->
                    error
            end
    end.

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
                    {join_count, GroupName, Pid});

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
    yes.

register_name({RegistrationType, Scope, GroupName, Lookup}, Pid)
    when (RegistrationType =:= global orelse
          RegistrationType =:= local), is_atom(Scope),
         (Lookup =:= random orelse Lookup =:= oldest) ->
    ok = join(Scope, GroupName, Pid),
    yes;

register_name({RegistrationType, Scope, GroupName, Instances}, Pid)
    when (RegistrationType =:= global orelse
          RegistrationType =:= local), is_atom(Scope),
         is_integer(Instances), Instances > 0 ->
    ok = join(Scope, GroupName, Pid),
    yes;

register_name({RegistrationType, Scope, GroupName}, Pid)
    when (RegistrationType =:= global orelse
          RegistrationType =:= local), is_atom(Scope) ->
    ok = join(Scope, GroupName, Pid),
    yes;

register_name({RegistrationType, GroupName, Instances}, Pid)
    when (RegistrationType =:= global orelse
          RegistrationType =:= local),
         is_integer(Instances), Instances > 0 ->
    ok = join(?DEFAULT_SCOPE, GroupName, Pid),
    yes;

register_name({RegistrationType, GroupName}, Pid)
    when (RegistrationType =:= global orelse
          RegistrationType =:= local) ->
    ok = join(?DEFAULT_SCOPE, GroupName, Pid),
    yes;

register_name({Scope, GroupName}, Pid)
    when is_atom(Scope) ->
    ok = join(Scope, GroupName, Pid),
    yes;

register_name({GroupName, Instances}, Pid)
    when is_integer(Instances), Instances > 0 ->
    ok = join(?DEFAULT_SCOPE, GroupName, Pid),
    yes;

register_name(GroupName, Pid) ->
    ok = join(?DEFAULT_SCOPE, GroupName, Pid),
    yes.

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

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the members of a specific group.===
%% All members are ordered from newest to oldest, based on the group
%% membership surviving netsplits (join order, not pid creation time).
%% @end
%%-------------------------------------------------------------------------

-spec get_members(name()) ->
    cpg_data:get_members_return().

get_members(GroupName) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_members, GroupName}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the members of a specific group while excluding a specific pid or within a specific scope.===
%% All members are ordered from newest to oldest, based on the group
%% membership surviving netsplits (join order, not pid creation time).
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_members(name() | scope(),
                  pid() | name() | pos_integer() | infinity) ->
    cpg_data:get_members_return().

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
%% All members are ordered from newest to oldest, based on the group
%% membership surviving netsplits (join order, not pid creation time).
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_members(scope() | name(),
                  name() | pid(),
                  pid() | pos_integer() | infinity) ->
    cpg_data:get_members_return().

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
%% All members are ordered from newest to oldest, based on the group
%% membership surviving netsplits (join order, not pid creation time).
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_members(scope(),
                  name(),
                  pid(),
                  pos_integer() | infinity) ->
    cpg_data:get_members_return().

get_members(Scope, GroupName, Exclude, Timeout)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_members, GroupName, Exclude},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get only the local members of a specific group.===
%% All members are ordered from newest to oldest, based on the 
%% join order, not pid creation time.
%% @end
%%-------------------------------------------------------------------------

-spec get_local_members(name()) ->
    cpg_data:get_members_return().

get_local_members(GroupName) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_local_members, GroupName}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get only the local members of a specific group while excluding a specific pid or within a specific scope.===
%% All members are ordered from newest to oldest, based on the 
%% join order, not pid creation time.
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_local_members(name() | scope(),
                        pid() | name() | pos_integer() | infinity) ->
    cpg_data:get_members_return().

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
%% All members are ordered from newest to oldest, based on the 
%% join order, not pid creation time.
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_local_members(scope() | name(),
                        name() | pid(),
                        pid() | pos_integer() | infinity) ->
    cpg_data:get_members_return().

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
%% All members are ordered from newest to oldest, based on the 
%% join order, not pid creation time.
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_local_members(scope(),
                        name(),
                        pid(),
                        pos_integer() | infinity) ->
    cpg_data:get_members_return().

get_local_members(Scope, GroupName, Exclude, Timeout)
    when is_atom(Scope), is_pid(Exclude) ->
    gen_server:call(Scope,
                    {get_local_members, GroupName, Exclude},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get only the remote members of a specific group.===
%% All members are ordered from newest to oldest, based on the group
%% membership surviving netsplits (join order, not pid creation time).
%% @end
%%-------------------------------------------------------------------------

-spec get_remote_members(name()) ->
    cpg_data:get_members_return().

get_remote_members(GroupName) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {get_remote_members, GroupName}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get only the remote members of a specific group while excluding a specific pid or within a specific scope.===
%% All members are ordered from newest to oldest, based on the group
%% membership surviving netsplits (join order, not pid creation time).
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_remote_members(name() | scope(),
                         pid() | name() | pos_integer() | infinity) ->
    cpg_data:get_members_return().

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
%% All members are ordered from newest to oldest, based on the group
%% membership surviving netsplits (join order, not pid creation time).
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_remote_members(scope() | name(),
                         name() | pid(),
                         pid() | pos_integer() | infinity) ->
    cpg_data:get_members_return().

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
%% All members are ordered from newest to oldest, based on the group
%% membership surviving netsplits (join order, not pid creation time).
%% Usually the self() pid is excluded with this function call.
%% @end
%%-------------------------------------------------------------------------

-spec get_remote_members(scope(),
                         name(),
                         pid(),
                         pos_integer() | infinity) ->
    cpg_data:get_members_return().

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
    list(name()).

which_groups() ->
    gen_server:call(?DEFAULT_SCOPE,
                    which_groups).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get all the groups currently defined within a specific scope.===
%% @end
%%-------------------------------------------------------------------------

-spec which_groups(scope() | pid() | pos_integer() | infinity) ->
    list(name()).

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
    list(name()).

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
    list(name()).

which_groups(Scope, Pid, Timeout)
    when is_atom(Scope), is_pid(Pid) ->
    gen_server:call(Scope,
                    {which_groups, Pid},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the join_count of each group a process has joined.===
%% @end
%%-------------------------------------------------------------------------

-spec which_groups_counts(pid()) ->
    list({name(), pos_integer()}).

which_groups_counts(Pid)
    when is_pid(Pid) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {which_groups_counts, Pid}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the join_count of each group a process has joined.===
%% @end
%%-------------------------------------------------------------------------

-spec which_groups_counts(scope() | pid(),
                          pid() | pos_integer() | infinity) ->
    list({name(), pos_integer()}).

which_groups_counts(Scope, Pid)
    when is_atom(Scope), is_pid(Pid) ->
    gen_server:call(Scope,
                    {which_groups_counts, Pid});

which_groups_counts(Pid, Timeout)
    when is_pid(Pid) ->
    gen_server:call(?DEFAULT_SCOPE,
                    {which_groups_counts, Pid},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the join_count of each group a process has joined.===
%% @end
%%-------------------------------------------------------------------------

-spec which_groups_counts(scope(),
                          pid(),
                          pos_integer() | infinity) ->
    list({name(), pos_integer()}).

which_groups_counts(Scope, Pid, Timeout)
    when is_atom(Scope), is_pid(Pid) ->
    gen_server:call(Scope,
                    {which_groups_counts, Pid},
                    Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a group member, with local pids given priority.===
%% @end
%%-------------------------------------------------------------------------

-spec get_closest_pid(name()) ->
    {ok, name(), pid()} |
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    {error, cpg_data:get_pid_error_reason()}.

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
    reset_all([Scope]).

%%-------------------------------------------------------------------------
%% @doc
%% ===Reset internal scopes state.===
%% Updates cpg application node_type monitoring
%% @end
%%-------------------------------------------------------------------------

-spec reset_all(list(scope())) ->
    ok.

reset_all(Scopes)
    when is_list(Scopes) ->
    ok = reset_all_send(Scopes, self()),
    reset_all_recv(Scopes).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the monitored listen nodes.===
%% @end
%%-------------------------------------------------------------------------

-spec listen_nodes(listen(),
                   nonempty_string()) ->
    list(node()).

listen_nodes(Listen, [_ | _] = NodeNameLocal) ->
    Nodes = if
        Listen =:= visible ->
            erlang:nodes(visible);
        Listen =:= all ->
            erlang:nodes(connected)
    end,
    monitored_nodes(Nodes, NodeNameLocal).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the visible nodes.===
%% @end
%%-------------------------------------------------------------------------

-spec visible_nodes(nonempty_string()) ->
    list(node()).

visible_nodes([_ | _] = NodeNameLocal) ->
    monitored_nodes(erlang:nodes(visible), NodeNameLocal).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the hidden nodes.===
%% @end
%%-------------------------------------------------------------------------

-spec hidden_nodes(nonempty_string()) ->
    list(node()).

hidden_nodes([_ | _] = NodeNameLocal) ->
    monitored_nodes(erlang:nodes(hidden), NodeNameLocal).

%%-------------------------------------------------------------------------
%% @doc
%% ===Check if a node is ignored.===
%% Nodes that connect periodically for running operations commands are
%% ignored by this function.
%% @end
%%-------------------------------------------------------------------------

-spec ignore_node(node(),
                  nonempty_string()) ->
    boolean().

ignore_node(Node, [_ | _] = NodeNameLocal) ->
    {NodeName, _} = node_split(Node),
    lists:prefix(NodeNameLocal ++ ?NODETOOL_SUFFIX, NodeName).

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
    {[_ | _] = NodeNameLocal, _} = node_split(node()),
    Listen = cpg_app:listen_type(),
    ok = monitor_nodes(true, Listen),
    ok = gather_groups(listen_nodes(Listen, NodeNameLocal), Scope),
    ok = quickrand:seed(),
    {ok, #state{node_name = NodeNameLocal,
                scope = Scope,
                groups = cpg_data:get_empty_groups(),
                listen = Listen}}.

%% @private
%% @doc
%% @end

handle_call({join, GroupName, Pid} = Request, _,
            #state{node_name = NodeNameLocal,
                   scope = Scope,
                   listen = Listen} = State) ->
    ok = abcast_hidden_nodes(Request, Listen, Scope, NodeNameLocal),
    VisibleNodes = visible_nodes(NodeNameLocal),
    {reply, {ok, VisibleNodes},
     join_group_local(1, GroupName, Pid, State)};

handle_call({join_counts, Counts, Pid} = Request, _,
            #state{node_name = NodeNameLocal,
                   scope = Scope,
                   listen = Listen} = State) ->
    Valid = lists:all(fun({_, Count}) ->
        is_integer(Count) andalso (Count > 0)
    end, Counts),
    if
        Valid =:= true ->
            ok = abcast_hidden_nodes(Request, Listen, Scope, NodeNameLocal),
            VisibleNodes = visible_nodes(NodeNameLocal),
            {reply, {ok, VisibleNodes},
             join_groups_local(Counts, Pid, State)};
        Valid =:= false ->
            {reply, error, State}
    end;

handle_call({leave, Pid} = Request, _,
            #state{node_name = NodeNameLocal,
                   scope = Scope,
                   listen = Listen,
                   monitors = Monitors} = State) ->
    case maps:take(Pid, Monitors) of
        {#state_monitor{monitor = MonitorRef,
                        names = GroupNameList}, MonitorsNew} ->
            true = is_reference(MonitorRef),
            true = erlang:demonitor(MonitorRef, [flush]),
            ok = abcast_hidden_nodes(Request, Listen, Scope, NodeNameLocal),
            VisibleNodes = visible_nodes(NodeNameLocal),
            {reply, {ok, VisibleNodes},
             leave_all_local(GroupNameList, Pid, leave_local,
                             State#state{monitors = MonitorsNew})};
        error ->
            {reply, error, State}
    end;

handle_call({leave, GroupName, Pid} = Request, _,
            #state{node_name = NodeNameLocal,
                   scope = Scope,
                   listen = Listen,
                   monitors = Monitors} = State) ->
    case maps:find(Pid, Monitors) of
        {ok, #state_monitor{monitor = MonitorRef,
                            names = GroupNameList}} ->
            true = is_reference(MonitorRef),
            case lists:member(GroupName, GroupNameList) of
                true ->
                    ok = abcast_hidden_nodes(Request, Listen,
                                             Scope, NodeNameLocal),
                    VisibleNodes = visible_nodes(NodeNameLocal),
                    {reply, {ok, VisibleNodes},
                     leave_group_local(1, GroupName, Pid, State)};
                false ->
                    {reply, error, State}
            end;
        error ->
            {reply, error, State}
    end;

handle_call({leave_counts, Counts, Pid} = Request, _,
            #state{node_name = NodeNameLocal,
                   scope = Scope,
                   listen = Listen,
                   monitors = Monitors} = State) ->
    case maps:find(Pid, Monitors) of
        {ok, #state_monitor{monitor = MonitorRef,
                            names = GroupNameList}} ->
            true = is_reference(MonitorRef),
            Valid = lists:all(fun({GroupName, Count}) ->
                lists:member(GroupName, GroupNameList) andalso
                is_integer(Count) andalso (Count > 0)
            end, Counts),
            if
                Valid =:= true ->
                    ok = abcast_hidden_nodes(Request, Listen,
                                             Scope, NodeNameLocal),
                    VisibleNodes = visible_nodes(NodeNameLocal),
                    {reply, {ok, VisibleNodes},
                     leave_groups_local(Counts, Pid, State)};
                Valid =:= false ->
                    {reply, error, State}
            end;
        error ->
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
            #state{monitors = Monitors} = State) ->
    case maps:find(Pid, Monitors) of
        {ok, #state_monitor{names = GroupNameList}} ->
            {reply, GroupNameList, State};
        error ->
            {reply, [], State}
    end;

handle_call({which_groups_counts, Pid}, _,
            #state{groups = {DictI, GroupsData},
                   monitors = Monitors} = State) ->
    case maps:find(Pid, Monitors) of
        {ok, #state_monitor{names = GroupNameList}} ->
            CountsN = lists:foldr(fun(GroupName, Counts0) ->
                case DictI:find(GroupName, GroupsData) of
                    error ->
                        Counts0;
                    {ok, #cpg_data{local_count = 0,
                                   remote_count = 0}} ->
                        Counts0;
                    {ok, #cpg_data{history = History}} ->
                        case count(Pid, History) of
                            0 ->
                                Counts0;
                            Count ->
                                [{GroupName, Count} | Counts0]
                        end
                end
            end, [], GroupNameList),
            {reply, CountsN, State};
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
    {stop, lists:flatten(io_lib:format("Unknown call \"~w\"", [Request])),
     error, State}.

%% @private
%% @doc
%% @end

handle_cast({exchange, Node, HistoryL},
            #state{node_name = NodeNameLocal,
                   scope = Scope,
                   listen = Listen} = State) ->
    {MonitoredNode,
     ListenNodes} = monitored_node(Node, Listen, NodeNameLocal),
    StateNew = if
        MonitoredNode =:= true ->
            ?LOG_INFO("scope ~p received state from ~p", [Scope, Node]),
            merge(HistoryL, ListenNodes, State);
        MonitoredNode =:= false ->
            State
    end,
    {noreply, StateNew};

handle_cast({join, GroupName, Pid}, State) ->
    {noreply, join_group_remote(1, GroupName, Pid, State)};

handle_cast({join_counts, Counts, Pid}, State) ->
    {noreply, join_groups_remote(Counts, Pid, State)};

handle_cast({leave, Pid},
            #state{monitors = Monitors} = State) ->
    case maps:take(Pid, Monitors) of
        {#state_monitor{monitor = MonitorProcess,
                        names = GroupNameList}, MonitorsNew} ->
            true = is_pid(MonitorProcess),
            ok = cpg_node_monitor:remove(MonitorProcess, Pid),
            {noreply,
             leave_all_remote(GroupNameList, Pid, leave_remote,
                              State#state{monitors = MonitorsNew})};
        error ->
            {noreply, State}
    end;

handle_cast({leave, GroupName, Pid},
            #state{monitors = Monitors} = State) ->
    case maps:find(Pid, Monitors) of
        {ok, #state_monitor{monitor = MonitorProcess,
                            names = GroupNameList}} ->
            true = is_pid(MonitorProcess),
            case lists:member(GroupName, GroupNameList) of
                true ->
                    {noreply, leave_group_remote(1, GroupName, Pid, State)};
                false ->
                    {noreply, State}
            end;
        error ->
            {noreply, State}
    end;

handle_cast({leave_counts, Counts, Pid},
            #state{monitors = Monitors} = State) ->
    case maps:find(Pid, Monitors) of
        {ok, #state_monitor{monitor = MonitorProcess,
                            names = GroupNameList}} ->
            true = is_pid(MonitorProcess),
            Valid = lists:all(fun({GroupName, _}) ->
                lists:member(GroupName, GroupNameList)
            end, Counts),
            if
                Valid =:= true ->
                    {noreply, leave_groups_remote(Counts, Pid, State)};
                Valid =:= false ->
                    {noreply, State}
            end;
        error ->
            {noreply, State}
    end;

handle_cast({add_join_callback, GroupName, F},
            #state{callbacks = Callbacks} = State) ->
    CallbacksNew = cpg_callbacks:add_join(Callbacks, GroupName, F),
    {noreply, State#state{callbacks = CallbacksNew}};

handle_cast({add_leave_callback, GroupName, F},
            #state{callbacks = Callbacks} = State) ->
    CallbacksNew = cpg_callbacks:add_leave(Callbacks, GroupName, F),
    {noreply, State#state{callbacks = CallbacksNew}};

handle_cast({remove_join_callback, GroupName, F},
            #state{callbacks = Callbacks} = State) ->
    CallbacksNew = cpg_callbacks:remove_join(Callbacks, GroupName, F),
    {noreply, State#state{callbacks = CallbacksNew}};

handle_cast({remove_leave_callback, GroupName, F},
            #state{callbacks = Callbacks} = State) ->
    CallbacksNew = cpg_callbacks:remove_leave(Callbacks, GroupName, F),
    {noreply, State#state{callbacks = CallbacksNew}};

handle_cast(Request, State) ->
    {stop, lists:flatten(io_lib:format("Unknown cast \"~w\"", [Request])),
     State}.

%% @private
%% @doc
%% @end

handle_info({'DOWN', _MonitorRef, process, Pid, Info}, State) ->
    {noreply, member_died_local(Pid, {exit, Info}, State)};

handle_info({'DOWNS', PidReasons}, State) ->
    {noreply, members_died_remote(PidReasons, State)};

handle_info({nodeup, Node, _InfoList},
            #state{node_name = NodeNameLocal} = State) ->
    Ignore = ignore_node(Node, NodeNameLocal),
    if
        Ignore =:= true ->
            ok;
        Ignore =:= false ->
            ok = merge_start(Node, State)
    end,
    {noreply, State};

handle_info({nodedown, Node, InfoList},
            #state{node_monitors = NodeMonitors} = State) ->
    case maps:find(Node, NodeMonitors) of
        {ok, Process} ->
            NodeDownReason = {_, _} = lists:keyfind(nodedown_reason, 1,
                                                    InfoList),
            {Pids, PidReasonsL} = cpg_node_monitor:died(Process),
            NodeMonitorsNew = maps:remove(Node, NodeMonitors),
            {noreply,
             node_died(Pids, {exit, NodeDownReason}, PidReasonsL,
                       State#state{node_monitors = NodeMonitorsNew})};
        error ->
            {noreply, State}
    end;

handle_info({new, Node}, State) ->
    ok = merge_start(Node, State),
    {noreply, State};

handle_info({cpg_data, From},
            #state{groups = Groups} = State) ->
    From ! {cloudi_cpg_data, Groups},
    {noreply, State};

handle_info({reset, From},
            #state{node_name = NodeNameLocal,
                   scope = Scope,
                   listen = ListenOld} = State) ->
    ListenNew = cpg_app:listen_type(),
    ok = listen_reset(ListenNew, ListenOld, Scope, NodeNameLocal),
    From ! {cloudi_cpg_reset, Scope},
    {noreply, State#state{listen = ListenNew}};

handle_info(Request, State) ->
    {stop, lists:flatten(io_lib:format("Unknown info \"~w\"", [Request])),
     State}.

%% @private
%% @doc
%% @end

terminate(Reason,
          #state{node_monitors = NodeMonitors,
                 callbacks = Callbacks}) ->
    maps:fold(fun(_, Process, _) ->
        ok = cpg_node_monitor:stop_link(Process)
    end, ok, NodeMonitors),
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
    net_kernel:monitor_nodes(Flag, [{node_type, Listen}, nodedown_reason]).

monitored_nodes([] = L, _) ->
    L;
monitored_nodes([Node | Nodes], NodeNameLocal) ->
    Ignore = ignore_node(Node, NodeNameLocal),
    if
        Ignore =:= true ->
            monitored_nodes(Nodes, NodeNameLocal);
        Ignore =:= false ->
            [Node | monitored_nodes(Nodes, NodeNameLocal)]
    end.

monitored_node(Node, visible, NodeNameLocal) ->
    ListenNodes = listen_nodes(visible, NodeNameLocal),
    Visible = lists:member(Node, ListenNodes),
    if
        Visible =:= true ->
            ok;
        Visible =:= false ->
            All = lists:member(Node, listen_nodes(all, NodeNameLocal)),
            if
                All =:= true ->
                    ?LOG_ERROR("listen should be 'all' for ~p monitoring",
                               [Node]);
                All =:= false ->
                    ok
            end
    end,
    {Visible, ListenNodes};
monitored_node(Node, all, NodeNameLocal) ->
    ListenNodes = listen_nodes(all, NodeNameLocal),
    {lists:member(Node, ListenNodes), ListenNodes}.

gather_groups([], _, _) ->
    ok;
gather_groups([RemoteNode | RemoteNodes], Node, Scope) ->
    % request data to merge for current groups state
    {Scope, RemoteNode} ! {new, Node},
    gather_groups(RemoteNodes, Node, Scope).

gather_groups(RemoteNodes, Scope) ->
    gather_groups(RemoteNodes, node(), Scope).

abcast_hidden_nodes(_, visible, _, _) ->
    ok;
abcast_hidden_nodes(Request, all, Scope, NodeNameLocal) ->
    case hidden_nodes(NodeNameLocal) of
        [] ->
            ok;
        [_ | _] = HiddenNodes ->
            abcast = gen_server:abcast(HiddenNodes, Scope, Request),
            ok
    end.

reset_all_send([], _) ->
    ok;
reset_all_send([Scope | Scopes], Pid) ->
    Scope ! {reset, Pid},
    reset_all_send(Scopes, Pid).

reset_all_recv([]) ->
    ok;
reset_all_recv([Scope | Scopes]) ->
    receive
        {cloudi_cpg_reset, Scope} ->
            ok
    end,
    reset_all_recv(Scopes).

listen_reset(Listen, Listen, _, _) ->
    ok;
listen_reset(ListenNew, ListenOld, Scope, NodeNameLocal) ->
    ok = monitor_nodes(true, ListenNew),
    HiddenNodesBefore = hidden_nodes(NodeNameLocal),
    ok = monitor_nodes(false, ListenOld),
    if
        ListenNew =:= all ->
            visible = ListenOld,
            ok = listen_reset_all(HiddenNodesBefore, Scope);
        ListenNew =:= visible ->
            all = ListenOld,
            HiddenNodesAfter = lists:usort(hidden_nodes(NodeNameLocal) ++
                                           HiddenNodesBefore),
            ok = listen_reset_visible(HiddenNodesAfter, Scope)
    end,
    ok.

listen_reset_all(HiddenNodes, Scope) ->
    gather_groups(HiddenNodes, Scope).

listen_reset_visible([], _, _) ->
    ok;
listen_reset_visible([HiddenNode | HiddenNodes], HiddenNodeInfo, Scope) ->
    Scope ! {nodedown, HiddenNode, HiddenNodeInfo},
    listen_reset_visible(HiddenNodes, HiddenNodeInfo, Scope).

listen_reset_visible(HiddenNodes, Scope) ->
    HiddenNodeInfo = [{nodedown_reason, cpg_reset}, {node_type, hidden}],
    listen_reset_visible(HiddenNodes, HiddenNodeInfo, Scope).

join_groups_local([], _, State) ->
    State;
join_groups_local([{GroupName, Count} | Counts], Pid, State) ->
    join_groups_local(Counts, Pid,
                      join_group_local(Count, GroupName, Pid, State)).

join_group_local(Count, GroupName, Pid,
                 #state{groups = {DictI, GroupsDataOld},
                        monitors = MonitorsOld,
                        callbacks = Callbacks} = State) ->
    Reason = join_local,
    cpg_callbacks:notify_join(Callbacks, GroupName, Pid, Reason),
    GroupData = case DictI:find(GroupName, GroupsDataOld) of
        {ok, #cpg_data{local_count = LocalI,
                       local = Local,
                       history = History} = GroupDataOld} ->
            GroupDataOld#cpg_data{local_count = LocalI + Count,
                                  local = prepend(Pid, Count, Local),
                                  history = prepend(Pid, Count, History)};
        error ->
            PidList = lists:duplicate(Count, Pid),
            #cpg_data{local_count = Count,
                      local = PidList,
                      history = PidList}
    end,
    GroupsData = DictI:store(GroupName, GroupData, GroupsDataOld),
    State#state{groups = {DictI, GroupsData},
                monitors = monitor_local(Pid, GroupName, MonitorsOld)}.

join_groups_remote([], _, State) ->
    State;
join_groups_remote([{GroupName, Count} | Counts], Pid, State) ->
    join_groups_remote(Counts, Pid,
                       join_group_remote(Count, GroupName, Pid, State)).

join_group_remote(Count, GroupName, Pid,
                  #state{groups = {DictI, GroupsDataOld},
                         monitors = MonitorsOld,
                         node_monitors = NodeMonitorsOld,
                         callbacks = Callbacks} = State) ->
    Reason = join_remote,
    cpg_callbacks:notify_join(Callbacks, GroupName, Pid, Reason),
    GroupData = case DictI:find(GroupName, GroupsDataOld) of
        {ok, #cpg_data{remote_count = RemoteI,
                       remote = Remote,
                       history = History} = GroupDataOld} ->
            GroupDataOld#cpg_data{remote_count = RemoteI + Count,
                                  remote = prepend(Pid, Count, Remote),
                                  history = prepend(Pid, Count, History)};
        error ->
            PidList = lists:duplicate(Count, Pid),
            #cpg_data{remote_count = Count,
                      remote = PidList,
                      history = PidList}
    end,
    GroupsData = DictI:store(GroupName, GroupData, GroupsDataOld),
    {Monitors,
     NodeMonitors} = monitor_remote(Pid, node(Pid), GroupName,
                                    MonitorsOld, NodeMonitorsOld),
    State#state{groups = {DictI, GroupsData},
                monitors = Monitors,
                node_monitors = NodeMonitors}.

leave_groups_local([], _, State) ->
    State;
leave_groups_local([{GroupName, Count} | Counts], Pid, State) ->
    leave_groups_local(Counts, Pid,
                       leave_group_local(Count, GroupName, Pid, State)).

leave_group_local(Count, GroupName, Pid,
                  #state{groups = {DictI, GroupsDataOld},
                         monitors = MonitorsOld,
                         callbacks = Callbacks} = State) ->
    Reason = leave_local,
    GroupDataOld = DictI:fetch(GroupName, GroupsDataOld),
    #cpg_data{local_count = LocalI,
              local = LocalOld,
              history = HistoryOld} = GroupDataOld,
    {I, Local} = leave_group_pid(Count, LocalOld, Pid),
    History = leave_group_pid_count(I, HistoryOld, Pid),
    cpg_callbacks:notify_leave(Callbacks, GroupName, Pid, Reason, I),
    {Member,
     GroupsData} = if
        History == [] ->
            {false,
             DictI:erase(GroupName, GroupsDataOld)};
        true ->
            {lists:member(Pid, Local),
             DictI:store(GroupName,
                         GroupDataOld#cpg_data{local_count = LocalI - I,
                                               local = Local,
                                               history = History},
                         GroupsDataOld)}
    end,
    Monitors = if
        Member =:= true ->
            MonitorsOld;
        Member =:= false ->
            case maps:get(Pid, MonitorsOld) of
                #state_monitor{monitor = MonitorRef,
                               names = [GroupName]} ->
                    true = is_reference(MonitorRef),
                    true = erlang:demonitor(MonitorRef, [flush]),
                    maps:remove(Pid, MonitorsOld);
                #state_monitor{names = GroupNameListOld} = StateMonitorOld ->
                    GroupNameList = lists:delete(GroupName, GroupNameListOld),
                    maps:put(Pid,
                             StateMonitorOld#state_monitor{
                                 names = GroupNameList},
                             MonitorsOld)
            end
    end,
    State#state{groups = {DictI, GroupsData},
                monitors = Monitors}.

leave_groups_remote([], _, State) ->
    State;
leave_groups_remote([{GroupName, Count} | Counts], Pid, State) ->
    leave_groups_remote(Counts, Pid,
                        leave_group_remote(Count, GroupName, Pid, State)).

leave_group_remote(Count, GroupName, Pid,
                   #state{groups = {DictI, GroupsDataOld},
                          monitors = MonitorsOld,
                          callbacks = Callbacks} = State) ->
    Reason = leave_remote,
    GroupDataOld = DictI:fetch(GroupName, GroupsDataOld),
    #cpg_data{remote_count = RemoteI,
              remote = RemoteOld,
              history = HistoryOld} = GroupDataOld,
    {I, Remote} = leave_group_pid(Count, RemoteOld, Pid),
    History = leave_group_pid_count(I, HistoryOld, Pid),
    cpg_callbacks:notify_leave(Callbacks, GroupName, Pid, Reason, I),
    {Member,
     GroupsData} = if
        History == [] ->
            {false,
             DictI:erase(GroupName, GroupsDataOld)};
        true ->
            {lists:member(Pid, Remote),
             DictI:store(GroupName,
                         GroupDataOld#cpg_data{remote_count = RemoteI - I,
                                               remote = Remote,
                                               history = History},
                         GroupsDataOld)}
    end,
    Monitors = if
        Member =:= true ->
            MonitorsOld;
        Member =:= false ->
            case maps:get(Pid, MonitorsOld) of
                #state_monitor{monitor = MonitorProcess,
                               names = [GroupName]} ->
                    true = is_pid(MonitorProcess),
                    ok = cpg_node_monitor:remove(MonitorProcess, Pid),
                    maps:remove(Pid, MonitorsOld);
                #state_monitor{names = GroupNameListOld} = StateMonitorOld ->
                    GroupNameList = lists:delete(GroupName, GroupNameListOld),
                    maps:put(Pid,
                             StateMonitorOld#state_monitor{
                                 names = GroupNameList},
                             MonitorsOld)
            end
    end,
    State#state{groups = {DictI, GroupsData},
                monitors = Monitors}.

leave_group_pid(0, Pids, I, Output, _) ->
    {I, lists:reverse(Pids, Output)};
leave_group_pid(_, [], I, Output, _) ->
    {I, Output};
leave_group_pid(Count, [Pid | Pids], I, Output, Pid) ->
    leave_group_pid(Count - 1, Pids, I + 1, Output, Pid);
leave_group_pid(Count, [P | Pids], I, Output, Pid) ->
    leave_group_pid(Count, Pids, I, [P | Output], Pid).

leave_group_pid(Count, Pids, Pid) ->
    leave_group_pid(Count, lists:reverse(Pids), 0, [], Pid).

leave_group_pid_count(0, Pids, Output, _) ->
    lists:reverse(Pids, Output);
leave_group_pid_count(Count, [Pid | Pids], Output, Pid) ->
    leave_group_pid_count(Count - 1, Pids, Output, Pid);
leave_group_pid_count(Count, [P | Pids], Output, Pid) ->
    leave_group_pid_count(Count, Pids, [P | Output], Pid).

leave_group_pid_count(Count, Pids, Pid) ->
    leave_group_pid_count(Count, lists:reverse(Pids), [], Pid).

leave_all_local([], _, _, State) ->
    State;
leave_all_local([GroupName | GroupNameList], Pid, Reason,
                #state{groups = {DictI, GroupsDataOld},
                       callbacks = Callbacks} = State) ->
    GroupDataOld = DictI:fetch(GroupName, GroupsDataOld),
    #cpg_data{local_count = LocalI,
              local = LocalOld,
              history = HistoryOld} = GroupDataOld,
    {I, Local} = leave_all_pid(LocalOld, Pid),
    cpg_callbacks:notify_leave(Callbacks, GroupName, Pid, Reason, I),
    History = [P || P <- HistoryOld, P /= Pid],
    GroupsData = if
        History == [] ->
            DictI:erase(GroupName, GroupsDataOld);
        true ->
            DictI:store(GroupName,
                        GroupDataOld#cpg_data{local_count = LocalI - I,
                                              local = Local,
                                              history = History},
                        GroupsDataOld)
    end,
    leave_all_local(GroupNameList, Pid, Reason,
                    State#state{groups = {DictI, GroupsData}}).

leave_all_remote([], _, _, State) ->
    State;
leave_all_remote([GroupName | GroupNameList], Pid, Reason,
                 #state{groups = {DictI, GroupsDataOld},
                        callbacks = Callbacks} = State) ->
    GroupDataOld = DictI:fetch(GroupName, GroupsDataOld),
    #cpg_data{remote_count = RemoteI,
              remote = RemoteOld,
              history = HistoryOld} = GroupDataOld,
    {I, Remote} = leave_all_pid(RemoteOld, Pid),
    cpg_callbacks:notify_leave(Callbacks, GroupName, Pid, Reason, I),
    History = [P || P <- HistoryOld, P /= Pid],
    GroupsData = if
        History == [] ->
            DictI:erase(GroupName, GroupsDataOld);
        true ->
            DictI:store(GroupName,
                        GroupDataOld#cpg_data{remote_count = RemoteI - I,
                                              remote = Remote,
                                              history = History},
                        GroupsDataOld)
    end,
    leave_all_remote(GroupNameList, Pid, Reason,
                     State#state{groups = {DictI, GroupsData}}).

leave_all_pid([], I, Output, _) ->
    {I, lists:reverse(Output)};
leave_all_pid([Pid | Pids], I, Output, Pid) ->
    leave_all_pid(Pids, I + 1, Output, Pid);
leave_all_pid([P | Pids], I, Output, Pid) ->
    leave_all_pid(Pids, I, [P | Output], Pid).

leave_all_pid(Pids, Pid) ->
    leave_all_pid(Pids, 0, [], Pid).

merge_pid_conflict([], GroupData, _, GroupName,
                   DictI, GroupsData, Monitors0, NodeMonitors0,
                   _, _, _) ->
    {DictI:store(GroupName, GroupData, GroupsData),
     Monitors0,
     NodeMonitors0};
merge_pid_conflict([Pid | Pids_X], GroupData, History_X, GroupName,
                   DictI, GroupsData, Monitors, NodeMonitors,
                   Callbacks, Node, NodesConnected) ->
    PidNode = node(Pid),
    if
        PidNode =:= Node ->
            merge_pid_conflict_local(Pid, Pids_X,
                                     GroupData, History_X, GroupName,
                                     DictI, GroupsData, Monitors, NodeMonitors,
                                     Callbacks, Node, NodesConnected);
        true ->
            case sets:is_element(PidNode, NodesConnected) of
                true ->
                    merge_pid_conflict_remote(Pid, PidNode, Pids_X,
                                              GroupData, History_X, GroupName,
                                              DictI, GroupsData,
                                              Monitors, NodeMonitors,
                                              Callbacks, Node, NodesConnected);
                false ->
                    merge_pid_conflict(Pids_X,
                                       GroupData, History_X, GroupName,
                                       DictI, GroupsData,
                                       Monitors, NodeMonitors,
                                       Callbacks, Node, NodesConnected)
            end
    end.

merge_pid_conflict_local(Pid, Pids_X,
                         #cpg_data{local_count = LocalI,
                                   local = Local,
                                   history = History} = GroupData,
                         History_X, GroupName,
                         DictI, GroupsData, Monitors, NodeMonitors,
                         Callbacks, Node, NodesConnected) ->
    % local pid counts must be equal
    I = count(Pid, History_X) - count(Pid, Local),
    if
        I > 0 ->
            % add
            cpg_callbacks:notify_join(Callbacks, GroupName, Pid,
                                      join_local, I),
            LocalNew = merge_pid_conflict_add(I, Local, Pid),
            HistoryNew = merge_pid_conflict_add(I, History, Pid),
            MonitorsNew = monitor_local(Pid, GroupName, Monitors),
            merge_pid_conflict(Pids_X,
                               GroupData#cpg_data{
                                   local_count = LocalI + I,
                                   local = LocalNew,
                                   history = HistoryNew},
                               History_X, GroupName,
                               DictI, GroupsData, MonitorsNew, NodeMonitors,
                               Callbacks, Node, NodesConnected);
        I < 0 ->
            % remove
            cpg_callbacks:notify_leave(Callbacks, GroupName, Pid,
                                       leave_local, I * -1),
            LocalNew = merge_pid_conflict_remove(I, Local, Pid),
            HistoryNew = merge_pid_conflict_remove(I, History, Pid),
            merge_pid_conflict(Pids_X,
                               GroupData#cpg_data{
                                   local_count = LocalI + I,
                                   local = LocalNew,
                                   history = HistoryNew},
                               History_X, GroupName,
                               DictI, GroupsData, Monitors, NodeMonitors,
                               Callbacks, Node, NodesConnected);
        true ->
            merge_pid_conflict(Pids_X, GroupData, History_X, GroupName,
                               DictI, GroupsData, Monitors, NodeMonitors,
                               Callbacks, Node, NodesConnected)
    end.

merge_pid_conflict_remote(Pid, PidNode, Pids_X,
                          #cpg_data{remote_count = RemoteI,
                                    remote = Remote,
                                    history = History} = GroupData,
                          History_X, GroupName,
                          DictI, GroupsData, Monitors, NodeMonitors,
                          Callbacks, Node, NodesConnected) ->
    % remote pid counts must be equal
    I = count(Pid, History_X) - count(Pid, Remote),
    if
        I > 0 ->
            % add
            cpg_callbacks:notify_join(Callbacks, GroupName, Pid,
                                      join_remote, I),
            RemoteNew = merge_pid_conflict_add(I, Remote, Pid),
            HistoryNew = merge_pid_conflict_add(I, History, Pid),
            {MonitorsNew,
             NodeMonitorsNew} = monitor_remote(Pid, PidNode, GroupName,
                                               Monitors, NodeMonitors),
            merge_pid_conflict(Pids_X,
                               GroupData#cpg_data{
                                   remote_count = RemoteI + I,
                                   remote = RemoteNew,
                                   history = HistoryNew},
                               History_X, GroupName,
                               DictI, GroupsData, MonitorsNew, NodeMonitorsNew,
                               Callbacks, Node, NodesConnected);
        I < 0 ->
            % remove
            cpg_callbacks:notify_leave(Callbacks, GroupName, Pid,
                                       leave_remote, I * -1),
            RemoteNew = merge_pid_conflict_remove(I, Remote, Pid),
            HistoryNew = merge_pid_conflict_remove(I, History, Pid),
            merge_pid_conflict(Pids_X,
                               GroupData#cpg_data{
                                   remote_count = RemoteI + I,
                                   remote = RemoteNew,
                                   history = HistoryNew},
                               History_X, GroupName,
                               DictI, GroupsData, Monitors, NodeMonitors,
                               Callbacks, Node, NodesConnected);
        true ->
            merge_pid_conflict(Pids_X, GroupData, History_X, GroupName,
                               DictI, GroupsData, Monitors, NodeMonitors,
                               Callbacks, Node, NodesConnected)
    end.

merge_pid_conflict_add(0, Pids, _) ->
    Pids;
merge_pid_conflict_add(I, Pids, Pid) ->
    merge_pid_conflict_add(I - 1, [Pid | Pids], Pid).

merge_pid_conflict_remove(0, Pids, _) ->
    Pids;
merge_pid_conflict_remove(I, [Pid | Pids], Pid) ->
    merge_pid_conflict_remove(I + 1, Pids, Pid);
merge_pid_conflict_remove(I, [P | Pids], Pid) ->
    [P | merge_pid_conflict_remove(I, Pids, Pid)].

merge_pid_new([], _,
              #cpg_data{history = History} = GroupData, GroupName,
              DictI, GroupsData, Monitors, NodeMonitors, _, _, _) ->
    GroupsDataNew = if
        History == [] ->
            GroupsData;
        true ->
            DictI:store(GroupName, GroupData, GroupsData)
    end,
    {GroupsDataNew, Monitors, NodeMonitors};
merge_pid_new([Pid | Pids_X], PidsMonitored,
              #cpg_data{history = History} = GroupData, GroupName,
              DictI, GroupsData, Monitors, NodeMonitors,
              Callbacks, Node, NodesConnected) ->
    PidNode = node(Pid),
    if
        PidNode =:= Node ->
            merge_pid_new_local(Pid, Pids_X, PidsMonitored,
                                GroupData, GroupName,
                                DictI, GroupsData, Monitors, NodeMonitors,
                                Callbacks, Node, NodesConnected);
        true ->
            case sets:is_element(PidNode, NodesConnected) of
                true ->
                    merge_pid_new_remote(Pid, PidNode, Pids_X, PidsMonitored,
                                         GroupData, GroupName,
                                         DictI, GroupsData,
                                         Monitors, NodeMonitors,
                                         Callbacks, Node, NodesConnected);
                false ->
                    HistoryNew = delete_all(Pid, History),
                    merge_pid_new(delete_all(Pid, Pids_X), PidsMonitored,
                                  GroupData#cpg_data{history = HistoryNew},
                                  GroupName,
                                  DictI, GroupsData,
                                  Monitors, NodeMonitors,
                                  Callbacks, Node, NodesConnected)
            end
    end.

merge_pid_new_local(Pid, Pids_X, PidsMonitored,
                    #cpg_data{% history already set from remote data
                              local_count = LocalI,
                              local = Local} = GroupData, GroupName,
                    DictI, GroupsData, Monitors, NodeMonitors,
                    Callbacks, Node, NodesConnected) ->
    % (could get here if cpg was restarted on the local node)
    cpg_callbacks:notify_join(Callbacks, GroupName, Pid, join_local),
    {PidsMonitoredNew,
     MonitorsNew} = case sets:is_element(Pid, PidsMonitored) of
        true ->
            {PidsMonitored, Monitors};
        false ->
            {sets:add_element(Pid, PidsMonitored),
             monitor_local(Pid, GroupName, Monitors)}
    end,
    merge_pid_new(Pids_X, PidsMonitoredNew,
                  GroupData#cpg_data{
                      local_count = LocalI + 1,
                      local = [Pid | Local]}, GroupName,
                  DictI, GroupsData, MonitorsNew, NodeMonitors,
                  Callbacks, Node, NodesConnected).

merge_pid_new_remote(Pid, PidNode, Pids_X, PidsMonitored,
                     #cpg_data{% history already set from remote data
                               remote_count = RemoteI,
                               remote = Remote} = GroupData, GroupName,
                     DictI, GroupsData, Monitors, NodeMonitors,
                     Callbacks, Node, NodesConnected) ->
    cpg_callbacks:notify_join(Callbacks, GroupName, Pid, join_remote),
    {PidsMonitoredNew,
     {MonitorsNew,
      NodeMonitorsNew}} = case sets:is_element(Pid, PidsMonitored) of
        true ->
            {PidsMonitored, {Monitors, NodeMonitors}};
        false ->
            {sets:add_element(Pid, PidsMonitored),
             monitor_remote(Pid, PidNode, GroupName,
                            Monitors, NodeMonitors)}
    end,
    merge_pid_new(Pids_X, PidsMonitoredNew,
                  GroupData#cpg_data{
                      remote_count = RemoteI + 1,
                      remote = [Pid | Remote]}, GroupName,
                  DictI, GroupsData, MonitorsNew, NodeMonitorsNew,
                  Callbacks, Node, NodesConnected).

merge_pids([], _, GroupsData, Monitors, NodeMonitors, _, _, _) ->
    {GroupsData,
     Monitors,
     NodeMonitors};
merge_pids([{GroupName, History_X} | HistoryL_X],
           DictI, GroupsData, Monitors, NodeMonitors,
           Callbacks, Node, NodesConnected) ->
    {GroupsDataNew,
     MonitorsNew,
     NodeMonitorsNew} = case DictI:find(GroupName, GroupsData) of
        {ok, GroupData} ->
            % merge the external group in
            merge_pid_conflict(lists:usort(History_X),
                               GroupData, History_X, GroupName,
                               DictI, GroupsData, Monitors, NodeMonitors,
                               Callbacks, Node, NodesConnected);
        error ->
            % create the new external group as an internal group
            merge_pid_new(lists:reverse(History_X), sets:new(),
                          #cpg_data{history = History_X}, GroupName,
                          DictI, GroupsData, Monitors, NodeMonitors,
                          Callbacks, Node, NodesConnected)
    end,
    merge_pids(HistoryL_X,
               DictI, GroupsDataNew, MonitorsNew, NodeMonitorsNew,
               Callbacks, Node, NodesConnected).

merge(HistoryL_X, ListenNodes,
      #state{groups = {DictI, GroupsData},
             monitors = Monitors,
             node_monitors = NodeMonitors,
             callbacks = Callbacks} = State) ->
    {GroupsDataNew,
     MonitorsNew,
     NodeMonitorsNew} = merge_pids(HistoryL_X,
                                   DictI, GroupsData, Monitors, NodeMonitors,
                                   Callbacks, node(),
                                   sets:from_list(ListenNodes)),
    State#state{groups = {DictI, GroupsDataNew},
                monitors = MonitorsNew,
                node_monitors = NodeMonitorsNew}.

merge_start(Node,
            #state{scope = Scope,
                   groups = {DictI, GroupsData}}) ->
    HistoryL = DictI:fold(fun(GroupName, #cpg_data{history = History}, L) ->
        [{GroupName, History} | L]
    end, [], GroupsData),
    if
        HistoryL == [] ->
            ok;
        true ->
            gen_server:cast({Scope, Node}, {exchange, node(), HistoryL})
    end.

member_died_local(Pid, Reason,
                  #state{monitors = Monitors} = State) ->
    case maps:take(Pid, Monitors) of
        {#state_monitor{names = GroupNameList}, MonitorsNew} ->
            leave_all_local(GroupNameList, Pid, Reason,
                            State#state{monitors = MonitorsNew});
        error ->
            State
    end.

members_died_remote([], State) ->
    State;
members_died_remote([{Pid, Reason} | PidReasons],
                    #state{monitors = Monitors} = State) ->
    case maps:take(Pid, Monitors) of
        {#state_monitor{names = GroupNameList}, MonitorsNew} ->
            StateNew = State#state{monitors = MonitorsNew},
            members_died_remote(PidReasons,
                                leave_all_remote(GroupNameList,
                                                 Pid, Reason, StateNew));
        error ->
            members_died_remote(PidReasons, State)
    end.

node_died([], _, PidReasonsL, State) ->
    node_died_related(PidReasonsL, State);
node_died([Pid | Pids], Reason, PidReasonsL,
          #state{monitors = Monitors} = State) ->
    case maps:take(Pid, Monitors) of
        {#state_monitor{names = GroupNameList}, MonitorsNew} ->
            StateNew = State#state{monitors = MonitorsNew},
            node_died(Pids, Reason, PidReasonsL,
                      leave_all_remote(GroupNameList,
                                       Pid, Reason, StateNew));
        error ->
            node_died(Pids, Reason, PidReasonsL, State)
    end.

node_died_related([], State) ->
    State;
node_died_related([PidReasons | PidReasonsL], State) ->
    node_died_related(PidReasonsL, members_died_remote(PidReasons, State)).

monitor_local(Pid, GroupName, MonitorsOld) ->
    case maps:find(Pid, MonitorsOld) of
        {ok, #state_monitor{names = GroupNameListOld} = StateMonitorOld} ->
            GroupNameList = lists:umerge(GroupNameListOld, [GroupName]),
            maps:put(Pid,
                     StateMonitorOld#state_monitor{names = GroupNameList},
                     MonitorsOld);
        error ->
            MonitorRef = erlang:monitor(process, Pid),
            maps:put(Pid,
                     #state_monitor{monitor = MonitorRef,
                                    names = [GroupName]},
                     MonitorsOld)
    end.

monitor_remote(Pid, PidNode, GroupName, MonitorsOld, NodeMonitorsOld) ->
    case maps:find(Pid, MonitorsOld) of
        {ok, #state_monitor{names = GroupNameListOld} = StateMonitorOld} ->
            GroupNameList = lists:umerge(GroupNameListOld, [GroupName]),
            {maps:put(Pid,
                      StateMonitorOld#state_monitor{names = GroupNameList},
                      MonitorsOld),
             NodeMonitorsOld};
        error ->
            {MonitorProcess,
             NodeMonitorsNext} = case maps:find(PidNode, NodeMonitorsOld) of
                {ok, MonitorProcessOld} ->
                    ok = cpg_node_monitor:add(MonitorProcessOld, Pid),
                    {MonitorProcessOld, NodeMonitorsOld};
                error ->
                    {ok, MonitorProcessNew} = cpg_node_monitor:start_link(Pid),
                    {MonitorProcessNew,
                     maps:put(PidNode, MonitorProcessNew, NodeMonitorsOld)}
            end,
            {maps:put(Pid,
                      #state_monitor{monitor = MonitorProcess,
                                     names = [GroupName]},
                      MonitorsOld),
             NodeMonitorsNext}
    end.

node_split(Node) when is_atom(Node) ->
    node_split(erlang:atom_to_list(Node), []).

node_split([], Name) ->
    {lists:reverse(Name), []};
node_split([$@ | NodeStr], Name) ->
    {lists:reverse(Name), NodeStr};
node_split([C | NodeStr], Name) ->
    node_split(NodeStr, [C | Name]).

whereis_name_random(1, [Pid]) ->
    Pid;
whereis_name_random(N, L) ->
    lists:nth(random(N), L).

count(Elem, List) ->
    count(List, 0, Elem).
count([], I, _) ->
    I;
count([Elem | T], I, Elem) ->
    count(T, I + 1, Elem);
count([_ | T], I, Elem) ->
    count(T, I, Elem).

prepend(_, 0, List) ->
    List;
prepend(Elem, Count, List) ->
    prepend(Elem, Count - 1, [Elem | List]).

delete_all(_, []) ->
    [];
delete_all(Elem, [Elem | T]) ->
    delete_all(Elem, T);
delete_all(Elem, [H | T]) ->
    [H | delete_all(Elem, T)].

-compile({inline, [{random,1}]}).

random(N) ->
    quickrand:uniform(N).

