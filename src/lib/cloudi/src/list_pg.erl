%% Derived from the pg2 module in the OTP stdlib application
%% (lib/kernel-2.14.2/src/pg2.erl)
%% the pg2 module copyright is below:
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
         get_local_members/1,
         which_groups/0,
         get_closest_pid/1,
         get_closest_pid/2,
         get_random_pid/1,
         get_random_pid/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         code_change/3, terminate/2]).

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
    global:trans({{?MODULE, Name}, self()},
                 fun() ->
                     gen_server:multi_call(?MODULE, {create, Name})
                 end),
    ok.

-spec delete(name()) -> 'ok'.

delete(Name) when is_list(Name) ->
    global:trans({{?MODULE, Name}, self()},
                 fun() ->
                     gen_server:multi_call(?MODULE, {delete, Name})
                 end),
    ok.

-spec join(name(), pid()) -> 'ok'.

join(Name, Pid) when is_list(Name), is_pid(Pid) ->
    global:trans({{?MODULE, Name}, self()},
                 fun() ->
                     gen_server:multi_call(?MODULE, {join, Name, Pid})
                 end),
    ok.

-spec leave(name(), pid()) -> 'ok'.

leave(Name, Pid) when is_list(Name), is_pid(Pid) ->
    global:trans({{?MODULE, Name}, self()},
                 fun() ->
                     gen_server:multi_call(?MODULE, {leave, Name, Pid})
                 end),
    ok.

-type get_members_ret() :: [pid()] | {'error', {'no_such_group', name()}}.

-spec get_members(name()) -> get_members_ret().

   
get_members(Name) when is_list(Name) ->
    gen_server:call(?MODULE, {get_members, Name}).

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

-record(state, {groups = trie:new(),
                pids = dict:new()}).

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

handle_call(Request, From, S) ->
    error_logger:warning_msg("The ~p server received an "
                             "unexpected message:\n"
                             "handle_call(~p, ~p, _)\n", 
                             [?MODULE, Request, From]),
    {noreply, S}.

-type cast() :: {'exchange', node(), #state{}}.

-spec handle_cast(cast(), #state{}) -> {'noreply', #state{}}.

handle_cast({exchange, _Node, ExternalState}, State) ->
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
    NewGroups = trie:update(Name, fun(OldValue) -> OldValue end, [], Groups),
    State#state{groups = NewGroups}.

delete_group(Name, #state{groups = Groups,
                          pids = Pids} = State) ->
    case trie:find(Name, Groups) of
        error ->
            State;
        {ok, []} ->
            State#state{groups = trie:erase(Name, Groups)};
        {ok, Members} ->
            NewPids = lists:foldl(fun({Pid, Ref}, P) ->
                true = erlang:demonitor(Ref, [flush]),
                dict:update(Pid,
                            fun(OldValue) ->
                                lists:delete(Name, OldValue)
                            end, P)
            end, Pids, Members),
            State#state{groups = trie:erase(Name, Groups),
                        pids = NewPids}
    end.

join_group(Name, Pid, #state{groups = Groups,
                             pids = Pids} = State) ->
    Entry = {Pid, erlang:monitor(process, Pid)},
    Fgroups = if
        node() =:= node(Pid) ->
            fun(OldValue) -> [Entry | OldValue] end;
        true ->
            fun(OldValue) -> [OldValue] ++ [Entry] end
    end,
    NameList = [Name],
    Fpids = fun(OldValue) ->
        lists:umerge(OldValue, NameList)
    end,
    State#state{groups = trie:update(Name, Fgroups, [Entry], Groups),
                pids = dict:update(Pid, Fpids, NameList, Pids)}.

leave_group(Name, Pid, #state{groups = Groups,
                              pids = Pids} = State) ->
    Fgroups = fun(OldValue) ->
        F = fun({P, Ref}) ->
            if 
                P == Pid ->
                    true = erlang:demonitor(Ref, [flush]),
                    true;
                true ->
                    false
            end
        end,
        {_, Value} = lists:partition(F, OldValue),
        Value
    end,
    Fpids = fun(OldValue) ->
        lists:delete(Name, OldValue)
    end,
    State#state{groups = trie:update(Name, Fgroups, Groups),
                pids = dict:update(Pid, Fpids, Pids)}.

store(#state{groups = ExternalGroups,
             pids = ExternalPids},
      #state{groups = Groups,
             pids = Pids} = State) ->
    Fgroups = fun(_, V1, V2) ->
        lists:foldl(fun({Pid, _}, V) ->
            case lists:keymember(Pid, 1, V) of
                false when node() =:= node(Pid) ->
                    [{Pid, erlang:monitor(process, Pid)} | V];
                false ->
                    V ++ [{Pid, erlang:monitor(process, Pid)}];
                true ->
                    V
            end
        end, V2, V1)
    end,
    NewGroups = trie:fold(fun(Key, V1, T) ->
        case trie:is_key(Key, T) of
            true ->
                trie:update(Key, fun(V2) -> Fgroups(Key, V1, V2) end, T);
            false ->
                NewV1 = lists:foldl(fun({Pid, _}, V) ->
                    if
                        node() =:= node(Pid) ->
                            [{Pid, erlang:monitor(process, Pid)} | V];
                        true ->
                            V ++ [{Pid, erlang:monitor(process, Pid)}]
                    end
                end, [], V1),
                trie:store(Key, NewV1, T)
        end
    end, Groups, ExternalGroups),
    Fpids = fun(_, V1, V2) -> lists:umerge(V2, V1) end,
    State#state{groups = NewGroups,
                pids = dict:merge(Fpids, ExternalPids, Pids)}.


member_died(Pid, #state{pids = Pids} = State) ->
    case dict:find(Pid, Pids) of
        error ->
            State;
        {ok, Names} ->
            lists:foldl(fun(Name, S) ->
                leave_group(Name, Pid, S)
            end, State, Names)
    end.

