%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Local Variable Pool==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2015-2017 Michael Truog <mjtruog at gmail dot com>
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
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2015-2017 Michael Truog
%%% @version 1.7.3 {@date} {@time}
%%%------------------------------------------------------------------------

-module(varpool).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([new/1,
         destroy/1,
         update/2,
         get/2,
         get/3]).

-record(group,
    {
        count_hash :: pos_integer(),    % dimension 1
        count_random :: pos_integer(),  % dimension 2
        count_total :: pos_integer(),
        hash :: fun((any(), pos_integer()) -> non_neg_integer()),
        random :: fun((pos_integer()) -> non_neg_integer()),
        processes :: array:array(pid())
    }).

-record(varpool,
    {
        owner :: pid(),
        supervisor :: pid(),
        max_r :: non_neg_integer(),
        max_t :: pos_integer(),
        groups :: dict:dict(any(), #group{}),
        processes = dict:new() :: dict:dict(pid(), {any(), non_neg_integer()}),
        monitors = [] :: list(reference())
    }).

-ifdef(ERLANG_OTP_VERSION_16).
-else.
-ifdef(ERLANG_OTP_VERSION_17).
-else.
-define(ERLANG_OTP_VERSION_18_FEATURES, true).
-endif.
-endif.
-ifdef(ERLANG_OTP_VERSION_18_FEATURES).
-define(PSEUDO_RANDOM(N),
        % assuming exsplus/exsp for 58 bits, period 8.31e34
        rand:uniform(N)).
-else.
-define(PSEUDO_RANDOM(N),
        % period 2.78e13
        random:uniform(N)).
-endif.

-define(DEFAULT_MAX_R,                              5). % max restart count
-define(DEFAULT_MAX_T,                            300). % max time in seconds
-define(DEFAULT_HASH,             fun erlang:phash2/2).
-define(DEFAULT_RANDOM,
        fun(Count) ->
            ?PSEUDO_RANDOM(Count) - 1
        end).

-type group() ::
    {Group :: any(),
     {M :: module(), F :: atom(), A :: list()},
     Options :: list({shutdown, pos_integer()} |
                     {count_hash, pos_integer()} |
                     {count_random, pos_integer()} |
                     {hash,
                      {module(), atom()} |
                      fun((any(), pos_integer()) -> non_neg_integer())} |
                     {random,
                      {module(), atom()} |
                      fun((pos_integer()) -> non_neg_integer())})}.
-type options() ::
    nonempty_list({max_r, non_neg_integer()} |
                  {max_t, pos_integer()} |
                  {groups, nonempty_list(group())}).
-export_type([options/0]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-spec new(Options :: options()) ->
    #varpool{}.

new([_ | _] = Options) ->
    Defaults = [
        {max_r,            ?DEFAULT_MAX_R},
        {max_t,            ?DEFAULT_MAX_T},
        {groups,                       []}],
    [MaxR, MaxT, Groups] = take_values(Defaults, Options),
    true = is_integer(MaxR) andalso (MaxR >= 0),
    true = is_integer(MaxT) andalso (MaxT >= 1),
    true = is_list(Groups) andalso (length(Groups) > 0),
    ShutdownDefault = if
        MaxR >= 1 ->
            ((MaxT * 1000) div MaxR) - 100;
        MaxR == 0 ->
            (MaxT * 1000) - 100
    end,
    {GroupsData, GroupsSupervisor} = groups(Groups, ShutdownDefault),
    Owner = self(),
    {ok, Supervisor} = varpool_sup:start_link(Owner, MaxR, MaxT,
                                              GroupsSupervisor),
    #varpool{owner = Owner,
             supervisor = Supervisor,
             max_r = MaxR,
             max_t = MaxT,
             groups = GroupsData}.

-spec destroy(#varpool{}) ->
    ok.

destroy(#varpool{owner = Owner,
                 supervisor = Supervisor,
                 monitors = Monitors}) ->
    true = Owner =:= self(),
    [erlang:demonitor(MonitorRef, [flush]) || MonitorRef <- Monitors],
    ok = varpool_sup:stop_link(Supervisor),
    ok.

-spec update(any(), #varpool{}) ->
    {updated, #varpool{}} |
    {ignored, #varpool{}}.

update({'UP', Supervisor, process, Child, {Group, I} = Info},
       #varpool{owner = Owner,
                supervisor = Supervisor,
                groups = Groups,
                processes = Processes,
                monitors = Monitors} = VarPool) ->
    true = Owner =:= self(),
    NewProcesses = dict:store(Child, Info, Processes),
    GroupState = dict:fetch(Group, Groups),
    #group{processes = GroupProcesses} = GroupState,
    NewGroupProcesses = array:set(I, Child, GroupProcesses),
    NewGroupState = GroupState#group{processes = NewGroupProcesses},
    NewGroups = dict:store(Group, NewGroupState, Groups),
    NewMonitors = [erlang:monitor(process, Child) | Monitors],
    {updated, VarPool#varpool{groups = NewGroups,
                              processes = NewProcesses,
                              monitors = NewMonitors}};
update({'DOWN', MonitorRef, process, Child, _Info},
       #varpool{owner = Owner,
                groups = Groups,
                processes = Processes,
                monitors = Monitors} = VarPool) ->
    true = Owner =:= self(),
    case dict:find(Child, Processes) of
        error ->
            {ignored, VarPool};
        {ok, {Group, I}} ->
            NewProcesses = dict:erase(Child, Processes),
            GroupState = dict:fetch(Group, Groups),
            #group{processes = GroupProcesses} = GroupState,
            NewGroupProcesses = array:set(I, undefined, GroupProcesses),
            NewGroupState = GroupState#group{processes = NewGroupProcesses},
            NewGroups = dict:store(Group, NewGroupState, Groups),
            NewMonitors = lists:delete(MonitorRef, Monitors),
            {updated, VarPool#varpool{groups = NewGroups,
                                      processes = NewProcesses,
                                      monitors = NewMonitors}}
    end;
update(_,
       #varpool{owner = Owner} = VarPool) ->
    true = Owner =:= self(),
    {ignored, VarPool}.

-spec get(Group :: any(), #varpool{}) ->
    pid() | undefined.

get(Group, #varpool{groups = Groups}) ->
    #group{count_hash = 1,
           count_random = CountRandom,
           random = Random,
           processes = GroupProcesses} = dict:fetch(Group, Groups),
    IndexHash = 0,
    IndexRandom = if
        CountRandom > 1 ->
            Random(CountRandom);
        CountRandom =:= 1 ->
            0
    end,
    I = IndexHash * CountRandom + IndexRandom,
    Child = array:get(I, GroupProcesses),
    if
        Child =:= undefined ->
            get_process(I,
                        IndexHash * CountRandom,
                        (IndexHash + 1) * CountRandom - 1, GroupProcesses);
        is_pid(Child) ->
            Child
    end.

-spec get(Group :: any(), Key :: any(), #varpool{}) ->
    pid() | undefined.

get(Group, Key, #varpool{groups = Groups}) ->
    #group{count_hash = CountHash,
           count_random = CountRandom,
           hash = Hash,
           random = Random,
           processes = GroupProcesses} = dict:fetch(Group, Groups),
    IndexHash = if
        CountHash > 1 ->
            Hash(Key, CountHash);
        CountHash =:= 1 ->
            0
    end,
    IndexRandom = if
        CountRandom > 1 ->
            Random(CountRandom);
        CountRandom =:= 1 ->
            0
    end,
    I = IndexHash * CountRandom + IndexRandom,
    Child = array:get(I, GroupProcesses),
    if
        Child =:= undefined ->
            get_process(I,
                        IndexHash * CountRandom,
                        (IndexHash + 1) * CountRandom - 1, GroupProcesses);
        is_pid(Child) ->
            Child
    end.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

groups([], GroupsData, GroupsSupervisor, _) ->
    {GroupsData, lists:reverse(GroupsSupervisor)};
groups([{Group, {M, F, A}, Options} | Groups],
       GroupsData, GroupsSupervisor, ShutdownDefault)
    when is_atom(M), is_atom(F), is_list(A), is_list(Options) ->
    Defaults = [
        {shutdown,        ShutdownDefault},
        {count_hash,                    1},
        {count_random,                  1},
        {hash,              ?DEFAULT_HASH},
        {random,          ?DEFAULT_RANDOM}],
    [Shutdown, CountHash, CountRandom,
     Hash0, Random0] = take_values(Defaults, Options),
    true = is_integer(Shutdown) andalso
           (Shutdown >= 1) andalso (Shutdown =< ShutdownDefault),
    true = is_integer(CountHash) andalso (CountHash >= 1),
    true = is_integer(CountRandom) andalso (CountRandom >= 1),
    Hash1 = case Hash0 of
        {HashModule, HashFunction}
            when is_atom(HashModule), is_atom(HashFunction) ->
            true = erlang:function_exported(HashModule, HashFunction, 2),
            fun(HashArg1, HashArg2) ->
                HashModule:HashFunction(HashArg1, HashArg2)
            end;
        _ when is_function(Hash0, 2) ->
            Hash0
    end,
    Random1 = case Random0 of
        {RandomModule, RandomFunction}
            when is_atom(RandomModule), is_atom(RandomFunction) ->
            true = erlang:function_exported(RandomModule, RandomFunction, 1),
            fun(RandomArg1) ->
                RandomModule:RandomFunction(RandomArg1)
            end;
        _ when is_function(Random0, 1) ->
            Random0
    end,
    CountTotal = CountHash * CountRandom,
    GroupProcesses = array:new([{size, CountTotal}]),
    NewGroupsData = dict:store(Group,
                               #group{count_hash = CountHash,
                                      count_random = CountRandom,
                                      count_total = CountTotal,
                                      hash = Hash1,
                                      random = Random1,
                                      processes = GroupProcesses},
                               GroupsData),
    NewGroupsSupervisor = [{Group, CountTotal, M, F, A, Shutdown} |
                           GroupsSupervisor],
    groups(Groups, NewGroupsData, NewGroupsSupervisor, ShutdownDefault).

groups(Groups, ShutdownDefault) ->
    groups(Groups, dict:new(), [], ShutdownDefault).

get_process(Istop, Istop, _, _, _) ->
    undefined;
get_process(I, Istop, Istart, Iend, GroupProcesses) ->
    Child = array:get(I, GroupProcesses),
    if
        Child =:= undefined ->
            if
                I == Iend ->
                    get_process(Istart, Istop, Istart, Iend, GroupProcesses);
                true ->
                    get_process(I + 1, Istop, Istart, Iend, GroupProcesses)
            end;
        is_pid(Child) ->
            Child
    end.

get_process(Iend, Istart, Iend, GroupProcesses) ->
    get_process(Istart, Iend, Istart, Iend, GroupProcesses);
get_process(I, Istart, Iend, GroupProcesses) ->
    get_process(I + 1, I, Istart, Iend, GroupProcesses).

take_values(DefaultList, List)
    when is_list(DefaultList), is_list(List) ->
    take_values([], DefaultList, List).

take_values(Result, [], List) ->
    lists:reverse(Result) ++ List;

take_values(Result, [{Key, Default} | DefaultList], List)
    when is_atom(Key) ->
    case lists:keytake(Key, 1, List) of
        false ->
            take_values([Default | Result], DefaultList, List);
        {value, {Key, Value}, RemainingList} ->
            take_values([Value | Result], DefaultList, RemainingList)
    end.

