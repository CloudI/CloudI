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

-export([start_link/0,
         create/1,
         delete/1,
         join/2,
         leave/2,
         get_members/1,
         get_members/2,
         get_local_members/1,
         which_groups/0,
         get_closest_pid/1,
         get_closest_pid/2,
         get_random_pid/1,
         get_random_pid/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         code_change/3, terminate/2]).

-include("list_pg_data.hrl").
-include("cloudi_logger.hrl").
-include("cloudi_constants.hrl").

-record(state,
    {
        groups = list_pg_data:get_empty_groups(), % string() -> #list_pg_data{}
        pids = dict:new()                         % pid() -> list(string())
    }).

%%% monitors are used instead of links.

%%%
%%% Exported functions
%%%

-spec start_link() -> {'ok', pid()} | {'error', term()}.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-type name() :: string().

-spec create(name()) -> 'ok'.

create(Name) when is_list(Name) ->
    group_name_validate_new(Name),
    global:trans({{?MODULE, Name}, self()},
                 fun() ->
                     gen_server:multi_call(?MODULE, {create, Name})
                 end),
    ok.

-spec delete(name()) -> 'ok'.

delete(Name) when is_list(Name) ->
    group_name_validate_new(Name),
    global:trans({{?MODULE, Name}, self()},
                 fun() ->
                     gen_server:multi_call(?MODULE, {delete, Name})
                 end),
    ok.

-spec join(name(), pid()) -> 'ok'.

join(Name, Pid) when is_list(Name), is_pid(Pid) ->
    group_name_validate_new(Name),
    global:trans({{?MODULE, Name}, self()},
                 fun() ->
                     gen_server:multi_call(?MODULE, {join, Name, Pid})
                 end),
    ok.

-spec leave(name(), pid()) -> 'ok'.

leave(Name, Pid) when is_list(Name), is_pid(Pid) ->
    group_name_validate_new(Name),
    global:trans({{?MODULE, Name}, self()},
                 fun() ->
                     gen_server:multi_call(?MODULE, {leave, Name, Pid})
                 end),
    ok.

-type get_members_ret() :: [pid()] | {'error', {'no_such_group', name()}}.

-spec get_members(name()) -> get_members_ret().
   
get_members(Name) when is_list(Name) ->
    gen_server:call(?MODULE, {get_members, Name}).

-spec get_members(name(), pid()) -> list(pid()) | {'error', gcp_error_reason()}.
   
get_members(Name, Exclude) when is_list(Name), is_pid(Exclude) ->
    gen_server:call(?MODULE, {get_members, Name, Exclude}).

-spec get_local_members(name()) -> get_members_ret().

get_local_members(Name) when is_list(Name) ->
    gen_server:call(?MODULE, {get_local_members, Name}).

-spec which_groups() -> [name()].

which_groups() ->
    gen_server:call(?MODULE, which_groups).

-type gcp_error_reason() :: {'no_process', name()} | {'no_such_group', name()}.

-spec get_closest_pid(name()) -> pid() | {'error', gcp_error_reason()}.

get_closest_pid(Name) when is_list(Name) ->
    gen_server:call(?MODULE, {get_closest_pid, Name}).

-spec get_closest_pid(name(), pid()) -> pid() | {'error', gcp_error_reason()}.

get_closest_pid(Name, Exclude) when is_list(Name), is_pid(Exclude) ->
    gen_server:call(?MODULE, {get_closest_pid, Name, Exclude}).

-spec get_random_pid(name()) -> pid() | {'error', gcp_error_reason()}.

get_random_pid(Name) when is_list(Name) ->
    gen_server:call(?MODULE, {get_random_pid, Name}).

-spec get_random_pid(name(), pid()) -> pid() | {'error', gcp_error_reason()}.

get_random_pid(Name, Exclude) when is_list(Name), is_pid(Exclude) ->
    gen_server:call(?MODULE, {get_random_pid, Name, Exclude}).

%%%
%%% Callback functions from gen_server
%%%

-spec init([]) -> {'ok', #state{}}.

init([]) ->
    Ns = nodes(),
    net_kernel:monitor_nodes(true),
    lists:foreach(fun(N) ->
                          {?MODULE, N} ! {new, node()}
                          % data is not persistent in ets, so trust the
                          % State coming from other nodes if this server
                          % has restarted and wants previous state
                          %self() ! {nodeup, N} % pg2 does this
                  end, Ns),
    cloudi_random:seed(),
    {ok, #state{}}.

-type call() :: {'create', name()}
              | {'delete', name()}
              | {'join', name(), pid()}
              | {'leave', name(), pid()}.

-spec handle_call(call(), _, #state{}) -> 
        {'reply', 'ok', #state{}}.

handle_call({create, Name}, _, State) ->
    {reply, ok, create_group(Name, State)};

handle_call({delete, Name}, _, State) ->
    {reply, ok, delete_group(Name, State)};

handle_call({join, Name, Pid}, _, State) ->
    {reply, ok, join_group(Name, Pid, State)};

handle_call({leave, Name, Pid}, _, State) ->
    {reply, ok, leave_group(Name, Pid, State)};

handle_call(list_pg_data, _, #state{groups = Groups} = State) ->
    {reply, Groups, State};

handle_call({get_members, Name}, _, #state{groups = Groups} = State) ->
    {reply, list_pg_data:get_members(Name, Groups), State};

handle_call({get_members, Name, Exclude}, _, #state{groups = Groups} = State) ->
    {reply, list_pg_data:get_members(Name, Exclude, Groups), State};

handle_call({get_local_members, Name}, _, #state{groups = Groups} = State) ->
    {reply, list_pg_data:get_local_members(Name, Groups), State};

handle_call(which_groups, _, #state{groups = Groups} = State) ->
    {reply, list_pg_data:which_groups(Groups), State};

handle_call({get_closest_pid, Name}, _, #state{groups = Groups} = State) ->
    {reply, list_pg_data:get_closest_pid(Name, Groups), State};

handle_call({get_closest_pid, Name, Exclude}, _,
            #state{groups = Groups} = State) ->
    {reply, list_pg_data:get_closest_pid(Name, Exclude, Groups), State};

handle_call({get_random_pid, Name}, _, #state{groups = Groups} = State) ->
    {reply, list_pg_data:get_random_pid(Name, Groups), State};

handle_call({get_random_pid, Name, Exclude}, _,
            #state{groups = Groups} = State) ->
    {reply, list_pg_data:get_random_pid(Name, Exclude, Groups), State};

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

handle_info({nodeup, Node}, State) ->
    gen_server:cast({?MODULE, Node}, {exchange, node(), State}),
    {noreply, State};

handle_info({new, Node}, State) ->
    gen_server:cast({?MODULE, Node}, {exchange, node(), State}),
    {noreply, State};

handle_info({list_pg_data, From}, #state{groups = Groups} = State) ->
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

create_group(Name, #state{groups = Groups} = State) ->
    NewGroups = trie:update(Name, fun(OldValue) -> OldValue end,
                            #list_pg_data{}, Groups),
    State#state{groups = NewGroups}.

delete_group(Name, #state{groups = Groups,
                          pids = Pids} = State) ->
    case trie:find(Name, Groups) of
        error ->
            State;
        {ok, #list_pg_data{local_count = 0,
                           remote_count = 0}} ->
            State#state{groups = trie:erase(Name, Groups)};
        {ok, #list_pg_data{local = Local,
                           remote = Remote}} ->
            NewPids = lists:foldl(fun(#list_pg_data_pid{pid = Pid,
                                                        monitor = Ref}, P) ->
                true = erlang:demonitor(Ref, [flush]),
                dict:update(Pid,
                            fun(OldValue) ->
                                lists:delete(Name, OldValue)
                            end, P)
            end, Pids, Local ++ Remote),
            State#state{groups = trie:erase(Name, Groups),
                        pids = NewPids}
    end.

join_group(Name, Pid, #state{groups = Groups,
                             pids = Pids} = State) ->
    Entry = #list_pg_data_pid{pid = Pid,
                              monitor = erlang:monitor(process, Pid)},
    NewGroups = if
        node() =:= node(Pid) ->
            trie:update(Name,
                        fun(#list_pg_data{local_count = LocalI,
                                          local = Local} = OldValue) ->
                            OldValue#list_pg_data{local_count = LocalI + 1,
                                                  local = [Entry | Local]}
                        end,
                        #list_pg_data{local_count = 1,
                                      local = [Entry]},
                        Groups);
        true ->
            trie:update(Name,
                        fun(#list_pg_data{remote_count = RemoteI,
                                          remote = Remote} = OldValue) ->
                            OldValue#list_pg_data{remote_count = RemoteI + 1,
                                                  remote = [Entry | Remote]}
                        end,
                        #list_pg_data{remote_count = 1,
                                      remote = [Entry]},
                        Groups)
    end,
    NameList = [Name],
    NewPids = dict:update(Pid,
                          fun(OldValue) ->
                              lists:umerge(OldValue, NameList)
                          end,
                          NameList, Pids),
    State#state{groups = NewGroups,
                pids = NewPids}.

leave_group(Name, Pid, #state{groups = Groups,
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
            trie:update(Name,
                        fun(#list_pg_data{local_count = LocalI,
                                          local = Local} = OldValue) ->
                            {OldLocal,
                             NewLocal} = lists:partition(Fpartition, Local),
                            OldValue#list_pg_data{local_count = LocalI -
                                                  erlang:length(OldLocal),
                                                  local = NewLocal}
                        end, Groups);
        true ->
            trie:update(Name,
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
                              lists:delete(Name, OldValue)
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
    NewGroups = trie:fold(fun(Name, #list_pg_data{local = V1Local,
                                                  remote = V1Remote} = V1, T) ->
        case trie:is_key(Name, T) of
            true ->
                % merge the external group in
                trie:update(Name,
                            fun(V2) ->
                                store_conflict(Name, V1, V2)
                            end, T);
            false ->
                % create the new external group as an internal group
                trie:store(Name, store_new_group(V1Local ++ V1Remote), T)
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
        {ok, Names} ->
            lists:foldl(fun(Name, S) ->
                leave_group(Name, Pid, S)
            end, State, Names)
    end.

-ifdef(SERVICE_NAME_PATTERN_MATCHING).
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
