%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%% @doc EC2 Erlang node discovery.
%% @end

-module(nodefinder_ec2).

-behaviour(gen_server).

%% external interface
-export([start_link/5,
         discover/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-type group() :: string().
-type tag() :: {list(string()) | string(), list(string()) | string()} |
               list(string()) |
               string().
-export_type([group/0,
              tag/0]).

-include_lib("erlcloud/include/erlcloud.hrl").
-include_lib("erlcloud/include/erlcloud_ec2.hrl").

-record(state,
    {
        ec2_config,
        ec2_instances,
        ec2_tagged_instances,
        groups :: list(group()),
        tags :: list(tag()),
        nodes :: list(node()),
        connect :: visible | hidden
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

% finds nodes with security group id OR tags (union of both sets)

-spec start_link(AccessKeyID :: string(),
                 SecretAccessKey :: string(),
                 EC2Host :: string(),
                 Groups :: list(group()),
                 Tags :: list(tag())) ->
    {ok, pid()} |
    ignore |
    {error, any()}.

start_link(AccessKeyID, SecretAccessKey, EC2Host, Groups, Tags) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE,
                          [AccessKeyID, SecretAccessKey, EC2Host,
                           Groups, Tags], []).

-spec discover(Timeout :: pos_integer()) ->
    ok |
    {error, discover_failed | timeout | noproc | any()}.

discover(Timeout) ->
    try gen_server:call(?MODULE, discover, Timeout)
    catch
        exit:{Reason, _} ->
            {error, Reason}
    end.

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([AccessKeyID, SecretAccessKey, EC2Host, Groups, Tags]) ->
    Config = erlcloud_ec2:new(AccessKeyID, SecretAccessKey, EC2Host),
    Connect = nodefinder_app:connect_type(),
    case do_discover(#state{ec2_config = Config,
                            groups = Groups,
                            tags = Tags,
                            nodes = [],
                            connect = Connect}) of
        {ok, #state{}} = Success ->
            Success;
        {error, _} = Error ->
            {stop, Error}
    end.

handle_call(discover, _From, State) ->
    case do_discover(State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {error, _} = Error ->
            {stop, Error, {error, discover_failed}, State}
    end;
handle_call(Request, _From, State) ->
    {stop, lists:flatten(io_lib:format("Unknown call \"~p\"", [Request])),
     error, State}.

handle_cast(Request, State) ->
    {stop, lists:flatten(io_lib:format("Unknown cast \"~p\"", [Request])),
     State}.

handle_info(Request, State) ->
    {stop, lists:flatten(io_lib:format("Unknown info \"~p\"", [Request])),
     State}.

terminate(_Reason, #state{}) ->
    ok.

code_change(_OldVsn, State, _Extra) -> 
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

node_names(Hosts, #state{} = State) ->
    Name = string:sub_word(erlang:atom_to_list(node()), 1, $@),
    Nodes = lists:foldl(fun(Host, L) ->
        lists:umerge(L, [erlang:list_to_atom(Name ++ [$@ | Host])])
    end, [], Hosts),
    {ok, Nodes, State}.

ec2_instances_get(#state{groups = [],
                         tags = []} = State) ->
    {ok, false, State};
ec2_instances_get(#state{ec2_config = Config,
                         ec2_instances = OldResult} = State) ->
    case erlcloud_ec2:describe_instances(Config) of
        {ok, OldResult} ->
            {ok, false, State};
        {ok, NewResult} ->
            {ok, true, State#state{ec2_instances = NewResult}};
        {error, _} = Error ->
            Error
    end.

ec2_tagged_instances_get_entries([], Results, _) ->
    {ok, Results};
ec2_tagged_instances_get_entries([Entry | Tags], Results, Config) ->
    Filters = case Entry of
        {Keys, Values} ->
            KeyCheck = case Keys of
                [[I0 | _] | _] when is_integer(I0) ->
                    Keys;
                [I0 | _] when is_integer(I0) ->
                    [Keys]
            end,
            ValueCheck = case Values of
                [[I1 | _] | _] when is_integer(I1) ->
                    Values;
                [I1 | _] when is_integer(I1) ->
                    [Values]
            end,
            [{key, KeyCheck}, {value, ValueCheck}];
        [[I | _] | _] when is_integer(I) ->
            [{key, Entry}];
        [I | _] when is_integer(I) ->
            [{key, [Entry]}]
    end,
    case erlcloud_ec2:describe_tags([{resource_type, ["instance"]} |
                                     Filters], Config) of
        {ok, EntryResults} ->
            NewResults = lists:foldl(fun(#ec2_tag{resource_id = Id}, L) ->
                lists:umerge(L, [Id])
            end, Results, EntryResults),
            ec2_tagged_instances_get_entries(Tags, NewResults, Config);
        {error, _} = Error ->
            Error
    end.

ec2_tagged_instances_get(#state{tags = []} = State) ->
    {ok, false, State};
ec2_tagged_instances_get(#state{ec2_config = Config,
                                ec2_tagged_instances = OldResult,
                                tags = Tags} = State) ->
    case ec2_tagged_instances_get_entries(Tags, [], Config) of
        {ok, OldResult} ->
            {ok, false, State};
        {ok, NewResult} ->
            {ok, true, State#state{ec2_tagged_instances = NewResult}};
        {error, _} = Error ->
            Error
    end.

update_from_instance(Instance, Hosts) ->
    {_, Host} = lists:keyfind(private_dns_name, 1, Instance),
    lists:umerge(Hosts, [Host]).

update_from_instances_set([], Hosts) ->
    Hosts;
update_from_instances_set([Instance | InstancesSet], Hosts) ->
    update_from_instances_set(InstancesSet,
                              update_from_instance(Instance, Hosts)).

update_from_instances_set([], Hosts, _) ->
    Hosts;
update_from_instances_set([Instance | InstancesSet], Hosts, F) ->
    NextHosts = case F(Instance) of
        true ->
            update_from_instance(Instance, Hosts);
        false ->
            Hosts
    end,
    update_from_instances_set(InstancesSet, NextHosts, F).

update_from_groups(#state{ec2_instances = Instances,
                          groups = Groups} = State) ->
    HostsFound = lists:foldl(fun(Reservation, Hosts) ->
        {_, GroupSet} = lists:keyfind(group_set, 1, Reservation),
        Found = lists:any(fun(Group) ->
            lists:member(Group, GroupSet)
        end, Groups),
        if
            Found =:= true ->
                {_, InstancesSet} = lists:keyfind(instances_set, 1,
                                                  Reservation),
                update_from_instances_set(InstancesSet, Hosts);
            Found =:= false ->
                Hosts
        end
    end, [], Instances),
    node_names(HostsFound, State).

update_from_tags(#state{ec2_instances = Instances,
                        ec2_tagged_instances = TaggedInstances} = State) ->
    HostsFound = lists:foldl(fun(InstanceId, Hosts) ->
        lists:foldl(fun(Reservation, NextHosts) ->
            {_, InstancesSet} = lists:keyfind(instances_set, 1, Reservation),
            Check = fun(Instance) ->
                case lists:keyfind(instance_id, 1, Instance) of
                    {_, InstanceId} ->
                        true;
                    {_, _} ->
                        false
                end
            end,
            update_from_instances_set(InstancesSet, NextHosts, Check)
        end, Hosts, Instances)
    end, [], TaggedInstances),
    node_names(HostsFound, State).

updates_gather(true, true, State) ->
    case update_from_groups(State) of
        {ok, Nodes0, NextState} ->
            case update_from_tags(NextState) of
                {ok, Nodes1, NewState} ->
                    {ok, lists:umerge(Nodes0, Nodes1), NewState}
            end
    end;
updates_gather(true, false,
               #state{nodes = OldNodes} = State) ->
    case update_from_groups(State) of
        {ok, Nodes, NewState} ->
            {ok, lists:umerge(Nodes, OldNodes), NewState}
    end;
updates_gather(false, true,
               #state{nodes = OldNodes} = State) ->
    case update_from_tags(State) of
        {ok, Nodes, NewState} ->
            {ok, lists:umerge(Nodes, OldNodes), NewState}
    end;
updates_gather(false, false,
               #state{nodes = OldNodes} = State) ->
    {ok, OldNodes, State}.

update_nodes(Nodes,
             #state{connect = Connect} = State) ->
    ConnectNodes = lists:subtract(Nodes, nodes()),
    pforeach(fun(Node) ->
        % avoid the possibly long synchronous call here
        connect_node(Connect, Node)
    end, ConnectNodes),
    State#state{nodes = Nodes}.

update(UpdateGroups, UpdateTags, State) ->
    case updates_gather(UpdateGroups, UpdateTags, State) of
        {ok, Nodes, NewState} ->
            {ok, update_nodes(Nodes, NewState)}
    end.

do_discover(#state{} = State) ->
    case ec2_instances_get(State) of
        {ok, UpdatedGroups, NextState} ->
            case ec2_tagged_instances_get(NextState) of
                {ok, UpdatedTags, NewState} ->
                    update(UpdatedGroups, UpdatedTags, NewState);
                {error, _} = Error ->
                    Error
            end;
        {error, _} = Error ->
            Error
    end.

connect_node(visible, Node) ->
    net_kernel:connect_node(Node);
connect_node(hidden, Node) ->
    net_kernel:hidden_connect_node(Node).

pforeach(_, []) ->
    ok;
pforeach(F, L) ->
    [erlang:spawn_link(fun() -> F(E) end) || E <- L],
    ok.
