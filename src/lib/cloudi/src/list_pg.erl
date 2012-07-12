%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%% Derived from the pg2 module in the OTP kernel application
%% (lib/kernel-x.x.x/src/pg2.erl)
%% the pg2 module copyright is below:
%%
%% Copyright (c) 2011-2012 Michael Truog. All Rights Reserved.
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 1997-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
-module(list_pg).

-behaviour(gen_server).

-include("list_pg_constants.hrl").

-ifdef(GROUP_NAME_WITH_LOCAL_PIDS_ONLY).
% does not require global locking
-export([join/1,
         join/2,
         join/3,
         leave/1,
         leave/2,
         leave/3]).
-else.
% requires global locking
-export([create/1,
         create/2,
         delete/1,
         delete/2,
         join/2,
         join/3,
         leave/2,
         leave/3]).
-endif.
-export([start_link/0,
         start_link/1,
         get_members/1,
         get_members/2,
         get_members/3,
         get_local_members/1,
         get_local_members/2,
         which_groups/0,
         which_groups/1,
         get_closest_pid/1,
         get_closest_pid/2,
         get_closest_pid/3,
         get_random_pid/1,
         get_random_pid/2,
         get_random_pid/3]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         code_change/3, terminate/2]).

-include("list_pg_data.hrl").
-include("cloudi_logger.hrl").

-record(state,
    {
        scope = undefined, % locally registered process name
        groups = list_pg_data:get_empty_groups(), % string() -> #list_pg_data{}
        pids = dict:new()                         % pid() -> list(string())
    }).

%%% monitors are used instead of links.

%%%
%%% Exported functions
%%%

-spec start_link() -> {'ok', pid()} | {'error', term()}.

start_link() ->
    start_link(?MODULE). % list_pg is the default Scope

-spec start_link(atom()) -> {'ok', pid()} | {'error', term()}.

start_link(Scope) when is_atom(Scope) ->
    gen_server:start_link({local, Scope}, ?MODULE, [Scope], []).

-type scope() :: atom().
-type name() :: string().

-ifdef(GROUP_NAME_WITH_LOCAL_PIDS_ONLY).

-spec join(name()) -> 'ok'.

join(GroupName)
    when is_list(GroupName) ->
    group_name_validate_new(GroupName),
    gen_server:multi_call(?MODULE, {join, GroupName, self()}),
    ok.

-spec join(name() | scope(), pid() | name()) -> 'ok'.

join(GroupName, Pid)
    when is_list(GroupName), is_pid(Pid), node(Pid) =:= node() ->
    group_name_validate_new(GroupName),
    gen_server:multi_call(?MODULE, {join, GroupName, Pid}),
    ok;

join(Scope, GroupName)
    when is_atom(Scope), is_list(GroupName) ->
    group_name_validate_new(GroupName),
    gen_server:multi_call(Scope, {join, GroupName, self()}),
    ok.

-spec join(scope(), name(), pid()) -> 'ok'.

join(Scope, GroupName, Pid)
    when is_atom(Scope), is_list(GroupName), is_pid(Pid),
         node(Pid) =:= node() ->
    group_name_validate_new(GroupName),
    gen_server:multi_call(Scope, {join, GroupName, Pid}),
    ok.

-spec leave(name()) -> 'ok'.

leave(GroupName)
    when is_list(GroupName) ->
    group_name_validate_new(GroupName),
    gen_server:multi_call(?MODULE, {leave, GroupName, self()}),
    ok.

-spec leave(name() | scope(), pid() | name()) -> 'ok'.

leave(GroupName, Pid)
    when is_list(GroupName), is_pid(Pid), node(Pid) =:= node() ->
    group_name_validate_new(GroupName),
    gen_server:multi_call(?MODULE, {leave, GroupName, Pid}),
    ok;

leave(Scope, GroupName)
    when is_atom(Scope), is_list(GroupName) ->
    group_name_validate_new(GroupName),
    gen_server:multi_call(Scope, {leave, GroupName, self()}),
    ok.

-spec leave(scope(), name(), pid()) -> 'ok'.

leave(Scope, GroupName, Pid)
    when is_atom(Scope), is_list(GroupName), is_pid(Pid),
         node(Pid) =:= node() ->
    group_name_validate_new(GroupName),
    gen_server:multi_call(Scope, {leave, GroupName, Pid}),
    ok.

-else. % GROUP_NAME_WITH_LOCAL_PIDS_ONLY not defined

-spec create(name()) -> 'ok'.

create(GroupName)
    when is_list(GroupName) ->
    group_name_validate_new(GroupName),
    global:trans({{?MODULE, GroupName}, self()},
                 fun() ->
                     gen_server:multi_call(?MODULE, {create, GroupName})
                 end),
    ok.

-spec create(scope(), name()) -> 'ok'.

create(Scope, GroupName)
    when is_atom(Scope), is_list(GroupName) ->
    group_name_validate_new(GroupName),
    global:trans({{Scope, GroupName}, self()},
                 fun() ->
                     gen_server:multi_call(Scope, {create, GroupName})
                 end),
    ok.

-spec delete(name()) -> 'ok'.

delete(GroupName)
    when is_list(GroupName) ->
    group_name_validate_new(GroupName),
    global:trans({{?MODULE, GroupName}, self()},
                 fun() ->
                     gen_server:multi_call(?MODULE, {delete, GroupName})
                 end),
    ok.

-spec delete(scope(), name()) -> 'ok'.

delete(Scope, GroupName)
    when is_atom(Scope), is_list(GroupName) ->
    group_name_validate_new(GroupName),
    global:trans({{Scope, GroupName}, self()},
                 fun() ->
                     gen_server:multi_call(Scope, {delete, GroupName})
                 end),
    ok.

-spec join(name(), pid()) -> 'ok'.

join(GroupName, Pid)
    when is_list(GroupName), is_pid(Pid) ->
    group_name_validate_new(GroupName),
    global:trans({{?MODULE, GroupName}, self()},
                 fun() ->
                     gen_server:multi_call(?MODULE, {join, GroupName, Pid})
                 end),
    ok.

-spec join(scope(), name(), pid()) -> 'ok'.

join(Scope, GroupName, Pid)
    when is_atom(Scope), is_list(GroupName), is_pid(Pid) ->
    group_name_validate_new(GroupName),
    global:trans({{Scope, GroupName}, self()},
                 fun() ->
                     gen_server:multi_call(Scope, {join, GroupName, Pid})
                 end),
    ok.

-spec leave(name(), pid()) -> 'ok'.

leave(GroupName, Pid)
    when is_list(GroupName), is_pid(Pid) ->
    group_name_validate_new(GroupName),
    global:trans({{?MODULE, GroupName}, self()},
                 fun() ->
                     gen_server:multi_call(?MODULE, {leave, GroupName, Pid})
                 end),
    ok.

-spec leave(scope(), name(), pid()) -> 'ok'.

leave(Scope, GroupName, Pid)
    when is_atom(Scope), is_list(GroupName), is_pid(Pid) ->
    group_name_validate_new(GroupName),
    global:trans({{Scope, GroupName}, self()},
                 fun() ->
                     gen_server:multi_call(Scope, {leave, GroupName, Pid})
                 end),
    ok.

-endif.

-type get_members_ret() :: list(pid()) | {'error', {'no_such_group', name()}}.

-type gcp_error_reason() :: {'no_process', name()} | {'no_such_group', name()}.

-spec get_members(name()) -> get_members_ret().
   
get_members(GroupName)
    when is_list(GroupName) ->
    gen_server:call(?MODULE, {get_members, GroupName}).

-spec get_members(name() | scope(), pid() | name()) -> get_members_ret().
   
get_members(GroupName, Exclude)
    when is_list(GroupName), is_pid(Exclude) ->
    gen_server:call(?MODULE, {get_members, GroupName, Exclude});

get_members(Scope, GroupName)
    when is_atom(Scope), is_list(GroupName) ->
    gen_server:call(Scope, {get_members, GroupName}).

-spec get_members(scope(), name(), pid()) -> get_members_ret().

get_members(Scope, GroupName, Exclude)
    when is_atom(Scope), is_list(GroupName), is_pid(Exclude) ->
    gen_server:call(Scope, {get_members, GroupName, Exclude}).

-spec get_local_members(name()) -> get_members_ret().

get_local_members(GroupName)
    when is_list(GroupName) ->
    gen_server:call(?MODULE, {get_local_members, GroupName}).

-spec get_local_members(scope(), name()) -> get_members_ret().

get_local_members(Scope, GroupName)
    when is_atom(Scope), is_list(GroupName) ->
    gen_server:call(Scope, {get_local_members, GroupName}).

-spec which_groups() -> [name()].

which_groups() ->
    gen_server:call(?MODULE, which_groups).

-spec which_groups(scope()) -> [name()].

which_groups(Scope)
    when is_atom(Scope) ->
    gen_server:call(Scope, which_groups).

-spec get_closest_pid(name()) -> pid() | {'error', gcp_error_reason()}.

get_closest_pid(GroupName)
    when is_list(GroupName) ->
    gen_server:call(?MODULE, {get_closest_pid, GroupName}).

-spec get_closest_pid(name() | scope(), pid() | name()) ->
    pid() | {'error', gcp_error_reason()}.

get_closest_pid(GroupName, Exclude)
    when is_list(GroupName), is_pid(Exclude) ->
    gen_server:call(?MODULE, {get_closest_pid, GroupName, Exclude});

get_closest_pid(Scope, GroupName)
    when is_atom(Scope), is_list(GroupName) ->
    gen_server:call(Scope, {get_closest_pid, GroupName}).

-spec get_closest_pid(scope(), name(), pid()) ->
    pid() | {'error', gcp_error_reason()}.

get_closest_pid(Scope, GroupName, Exclude)
    when is_atom(Scope), is_list(GroupName), is_pid(Exclude) ->
    gen_server:call(Scope, {get_closest_pid, GroupName, Exclude}).

-spec get_random_pid(name()) -> pid() | {'error', gcp_error_reason()}.

get_random_pid(GroupName)
    when is_list(GroupName) ->
    gen_server:call(?MODULE, {get_random_pid, GroupName}).

-spec get_random_pid(name() | scope(), pid() | name()) ->
    pid() | {'error', gcp_error_reason()}.

get_random_pid(GroupName, Exclude)
    when is_list(GroupName), is_pid(Exclude) ->
    gen_server:call(?MODULE, {get_random_pid, GroupName, Exclude});

get_random_pid(Scope, GroupName)
    when is_atom(Scope), is_list(GroupName) ->
    gen_server:call(Scope, {get_random_pid, GroupName}).

-spec get_random_pid(scope(), name(), pid()) ->
    pid() | {'error', gcp_error_reason()}.

get_random_pid(Scope, GroupName, Exclude)
    when is_atom(Scope), is_list(GroupName), is_pid(Exclude) ->
    gen_server:call(Scope, {get_random_pid, GroupName, Exclude}).

%%%
%%% Callback functions from gen_server
%%%

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
    cloudi_random:seed(),
    {ok, #state{scope = Scope}}.

-type call() :: {'create', name()}
              | {'delete', name()}
              | {'join', name(), pid()}
              | {'leave', name(), pid()}.

-spec handle_call(call(), _, #state{}) -> 
        {'reply', 'ok', #state{}}.

handle_call({create, GroupName}, _, State) ->
    {reply, ok, create_group(GroupName, State)};

handle_call({delete, GroupName}, _, State) ->
    {reply, ok, delete_group(GroupName, State)};

handle_call({join, GroupName, Pid}, _, State) ->
    {reply, ok, join_group(GroupName, Pid, State)};

handle_call({leave, GroupName, Pid}, _, State) ->
    {reply, ok, leave_group(GroupName, Pid, State)};

handle_call(list_pg_data, _,
            #state{groups = Groups} = State) ->
    {reply, Groups, State};

handle_call({get_members, GroupName}, _,
            #state{groups = Groups} = State) ->
    {reply, list_pg_data:get_members(GroupName, Groups), State};

handle_call({get_members, GroupName, Exclude}, _,
            #state{groups = Groups} = State) ->
    {reply, list_pg_data:get_members(GroupName, Exclude, Groups), State};

handle_call({get_local_members, GroupName}, _,
            #state{groups = Groups} = State) ->
    {reply, list_pg_data:get_local_members(GroupName, Groups), State};

handle_call(which_groups, _,
            #state{groups = Groups} = State) ->
    {reply, list_pg_data:which_groups(Groups), State};

handle_call({get_closest_pid, GroupName}, _,
            #state{groups = Groups} = State) ->
    {reply, list_pg_data:get_closest_pid(GroupName, Groups), State};

handle_call({get_closest_pid, GroupName, Exclude}, _,
            #state{groups = Groups} = State) ->
    {reply, list_pg_data:get_closest_pid(GroupName, Exclude, Groups), State};

handle_call({get_random_pid, GroupName}, _,
            #state{groups = Groups} = State) ->
    {reply, list_pg_data:get_random_pid(GroupName, Groups), State};

handle_call({get_random_pid, GroupName, Exclude}, _,
            #state{groups = Groups} = State) ->
    {reply, list_pg_data:get_random_pid(GroupName, Exclude, Groups), State};

handle_call(Request, _, State) ->
    ?LOG_WARN("Unknown call \"~p\"", [Request]),
    {stop, cloudi_string:format("Unknown call \"~p\"", [Request]),
     error, State}.

-type cast() :: {'exchange', node(), #state{}}.

-spec handle_cast(cast(), #state{}) -> {'noreply', #state{}}.

handle_cast({exchange, Node, ExternalState}, State) ->
    ?LOG_INFO("received state from ~p", [Node]),
    {noreply, store(ExternalState, State)};

handle_cast(_, State) ->
    {noreply, State}.

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

handle_info({list_pg_data, From},
            #state{groups = Groups} = State) ->
    From ! {list_pg_data, Groups},
    {noreply, State};

handle_info(_, State) ->
    {noreply, State}.

-spec terminate(term(), #state{}) -> 'ok'.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%
%%% Local functions
%%%

create_group(GroupName, #state{groups = Groups} = State) ->
    NewGroups = trie:update(GroupName, fun(OldValue) -> OldValue end,
                            #list_pg_data{}, Groups),
    State#state{groups = NewGroups}.

delete_group(GroupName, #state{groups = Groups,
                               pids = Pids} = State) ->
    case trie:find(GroupName, Groups) of
        error ->
            State;
        {ok, #list_pg_data{local_count = 0,
                           remote_count = 0}} ->
            State#state{groups = trie:erase(GroupName, Groups)};
        {ok, #list_pg_data{local = Local,
                           remote = Remote}} ->
            NewPids = lists:foldl(fun(#list_pg_data_pid{pid = Pid,
                                                        monitor = Ref}, P) ->
                true = erlang:demonitor(Ref, [flush]),
                dict:update(Pid,
                            fun(OldValue) ->
                                lists:delete(GroupName, OldValue)
                            end, P)
            end, Pids, Local ++ Remote),
            State#state{groups = trie:erase(GroupName, Groups),
                        pids = NewPids}
    end.

join_group(GroupName, Pid, #state{groups = Groups,
                                  pids = Pids} = State) ->
    Entry = #list_pg_data_pid{pid = Pid,
                              monitor = erlang:monitor(process, Pid)},
    NewGroups = if
        node() =:= node(Pid) ->
            trie:update(GroupName,
                        fun(#list_pg_data{local_count = LocalI,
                                          local = Local} = OldValue) ->
                            OldValue#list_pg_data{local_count = LocalI + 1,
                                                  local = [Entry | Local]}
                        end,
                        #list_pg_data{local_count = 1,
                                      local = [Entry]},
                        Groups);
        true ->
            trie:update(GroupName,
                        fun(#list_pg_data{remote_count = RemoteI,
                                          remote = Remote} = OldValue) ->
                            OldValue#list_pg_data{remote_count = RemoteI + 1,
                                                  remote = [Entry | Remote]}
                        end,
                        #list_pg_data{remote_count = 1,
                                      remote = [Entry]},
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
    Fpartition = fun(#list_pg_data_pid{pid = P, monitor = Ref}) ->
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
            trie:update(GroupName,
                        fun(#list_pg_data{local_count = LocalI,
                                          local = Local} = OldValue) ->
                            {OldLocal,
                             NewLocal} = lists:partition(Fpartition, Local),
                            OldValue#list_pg_data{local_count = LocalI -
                                                  erlang:length(OldLocal),
                                                  local = NewLocal}
                        end, Groups);
        true ->
            trie:update(GroupName,
                        fun(#list_pg_data{remote_count = RemoteI,
                                          remote = Remote} = OldValue) ->
                            {OldRemote,
                             NewRemote} = lists:partition(Fpartition, Remote),
                            OldValue#list_pg_data{remote_count = RemoteI -
                                                  erlang:length(OldRemote),
                                                  remote = NewRemote}
                        end, Groups)
    end,
    NewPids = dict:update(Pid,
                          fun(OldValue) ->
                              lists:delete(GroupName, OldValue)
                          end,
                          Pids),
    State#state{groups = NewGroups,
                pids = NewPids}.

store_conflict_add(0, Entries, _) ->
    Entries;
store_conflict_add(I, Entries, Pid) ->
    Ref = erlang:monitor(process, Pid),
    store_conflict_add(I - 1,
                       [#list_pg_data_pid{pid = Pid,
                                          monitor = Ref} | Entries], Pid).

store_conflict_remove_monitors([]) ->
    ok;
store_conflict_remove_monitors([#list_pg_data_pid{monitor = M} | OldEntries]) ->
    true = erlang:demonitor(M, [flush]),
    store_conflict_remove_monitors(OldEntries).

store_conflict_remove(I, Entries) ->
    {Remove, NewEntries} = lists:split(I, Entries),
    store_conflict_remove_monitors(Remove),
    NewEntries.

store_conflict_f([], V2, _) ->
    V2;
store_conflict_f([Pid | V1AllPids],
                 #list_pg_data{local_count = LocalI,
                               local = Local,
                               remote_count = RemoteI,
                               remote = Remote} = V2, V1All) ->
    % for each external Pid, check the internal Pids within the same group
    Fpartition = fun(#list_pg_data_pid{pid = P}) ->
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
                    NewLocal = store_conflict_add(I, Local, Pid),
                    store_conflict_f(V1AllPids,
                                     V2#list_pg_data{local_count = LocalI + I,
                                                     local = NewLocal},
                                     V1All);
                I < 0 ->
                    % remove
                    NewV2Pids = store_conflict_remove(I * -1, V2Pids),
                    store_conflict_f(V1AllPids,
                                     V2#list_pg_data{local_count = LocalI + I,
                                                     local = NewV2Pids ++
                                                             LocalRest},
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
                    NewRemote = store_conflict_add(I, Remote, Pid),
                    store_conflict_f(V1AllPids,
                                     V2#list_pg_data{remote_count = RemoteI + I,
                                                     remote = NewRemote},
                                     V1All);
                I < 0 ->
                    % remove
                    NewV2Pids = store_conflict_remove(I * -1, V2Pids),
                    store_conflict_f(V1AllPids,
                                     V2#list_pg_data{remote_count = RemoteI + I,
                                                     remote = NewV2Pids ++
                                                              RemoteRest},
                                     V1All);
                true ->
                    store_conflict_f(V1AllPids, V2, V1All)
            end
    end.

store_conflict(_,
               #list_pg_data{local = V1Local,
                             remote = V1Remote}, V2) ->
    % V1 is external
    % V2 is internal
    V1All = V1Local ++ V1Remote,
    store_conflict_f(lists:usort(lists:map(fun(#list_pg_data_pid{pid = Pid}) ->
                         Pid
                     end, V1All)),
                     V2, V1All).

store_new_group(OldEntries) ->
    store_new_group(OldEntries, #list_pg_data{}).

store_new_group([], V2) ->
    V2;
store_new_group([#list_pg_data_pid{pid = Pid} = E | OldEntries],
                 #list_pg_data{local_count = LocalI,
                               local = Local,
                               remote_count = RemoteI,
                               remote = Remote} = V2) ->
    NewE = E#list_pg_data_pid{monitor = erlang:monitor(process, Pid)},
    if
        node() =:= node(Pid) ->
            store_new_group(OldEntries,
                            V2#list_pg_data{local_count = LocalI + 1,
                                            local = [NewE | Local]});
        true ->
            store_new_group(OldEntries,
                            V2#list_pg_data{remote_count = RemoteI + 1,
                                            remote = [NewE | Remote]})
    end.

store(#state{groups = ExternalGroups,
             pids = ExternalPids},
      #state{groups = Groups,
             pids = Pids} = State) ->
    % V1 is external
    % V2 is internal
    NewGroups = trie:fold(fun(GroupName,
                              #list_pg_data{local = V1Local,
                                            remote = V1Remote} = V1, T) ->
        case trie:is_key(GroupName, T) of
            true ->
                % merge the external group in
                trie:update(GroupName,
                            fun(V2) ->
                                store_conflict(GroupName, V1, V2)
                            end, T);
            false ->
                % create the new external group as an internal group
                trie:store(GroupName, store_new_group(V1Local ++ V1Remote), T)
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
            % if a pid is added to a group multiple times,
            % a monitor is created for each instance
            % (so later monitor messages will fail the lookup here)
            State;
        {ok, GroupNames} ->
            lists:foldl(fun(GroupName, S) ->
                leave_group(GroupName, Pid, S)
            end, State, GroupNames)
    end.

-ifdef(GROUP_NAME_PATTERN_MATCHING).
% pattern matching occurs with a "*" character, but "**" is forbidden,
% so, this makes sure all group names are valid, before creating a new group
group_name_validate_new([]) ->
    ok;
group_name_validate_new([$*, $* | _]) ->
    erlang:exit(badarg);
group_name_validate_new([_ | L]) ->
    group_name_validate_new(L).
-else.
group_name_validate_new(_) ->
    ok.
-endif.
