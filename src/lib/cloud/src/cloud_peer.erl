%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

-module(cloud_peer).

%% Derived from the slave module in the OTP stdlib application
%% (lib/erlang/lib/stdlib-1.16.2/src/slave.erl)
%% the slave module copyright is below:
%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2009. All Rights Reserved.
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

%% external interface
-export([start/2, start/3, start/4, start/5]).

%% external callbacks
-export([peer_start/1, wait_for_peer/8]).

-include("cloud_logger.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% Start an Erlang VM instance on the specified host with the
%% specified node name.  If the Erlang VM instance already exists,
%% then create a connection to it.
%% @end
%%-------------------------------------------------------------------------
start(Host, Name) ->
    start(Host, Name, "").

%%-------------------------------------------------------------------------
%% @doc
%% Start an Erlang VM instance on the specified host with the
%% specified node name and additional Erlang VM command line arguments.
%% If the Erlang VM instance already exists, then create a connection to it.
%% @end
%%-------------------------------------------------------------------------
start(Host, Name, Args) ->
    start(Host, Name, Args, 32000).

start(Host, Name, Args, Timeout) ->
    Prog = atom_to_list(lib:progname()),
    start(Host, Name, Args, Prog, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% Start an Erlang VM instance on the specified host with the
%% specified node name, additional Erlang VM command line arguments,
%% the Erlang VM command and a timeout.  If the Erlang VM instance
%% already exists, then create a connection to it.
%% @end
%%-------------------------------------------------------------------------
start(Host, Name, Args, Prog, Timeout) ->
    LongNames = net_kernel:longnames(),
    NodeHost = get_node_host_component(LongNames, Host),
    Node = list_to_atom(lists:concat([Name, "@", NodeHost])),
    case net_adm:ping(Node) of
        pong ->
            {error, {already_running, Node}};
        pang ->
            Child = spawn(?MODULE, wait_for_peer,
                [self(), Node, LongNames, Prog, Host, Name, Args, Timeout]),
            receive
                {Child, Result} -> Result
            end
    end.

%%%------------------------------------------------------------------------
%%% External callbacks
%%%------------------------------------------------------------------------

%% notify the waiting process that the Erlang VM has started
peer_start([Master, Waiter]) ->
    {Waiter, Master} ! {self(), peer_started}.

%% wait for the Erlang VM to start
wait_for_peer(Parent, Node, LongNames, Prog, Host, Name, Args, Timeout) ->
    Waiter = register_unique_name(0),
    case erl_command_line_args(Waiter, LongNames, Prog, Host, Name, Args) of
        {ok, Cmd} ->
            open_port({spawn, Cmd}, [stream]),
            receive
                {PeerPid, peer_started} ->
                    unregister(Waiter),
                    PeerNode = node(PeerPid),
                    case cloud_logger:load_interface_module(PeerNode) of
                        ok ->
                            ok;
                        {error, Reason} ->
                            ?LOG_ERROR("unable to load remote logger on ~p: ~p",
                                       [PeerNode, Reason])
                    end,
                    Parent ! {self(), {ok, PeerNode}}
            after
                Timeout ->
                    unregister(Waiter),
                    case net_adm:ping(Node) of
                        pong ->
                            % zombie nodes must die
                            spawn(Node, fun() -> erlang:halt() end),
                            ok;
                        _ ->
                            ok
                    end,
                    Parent ! {self(), {error, timeout}}
            end;
        Other ->
            Parent ! {self(), Other}
    end.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

register_unique_name(Number) ->
    Name = erlang:list_to_atom("cloud_peer_waiter" ++
        erlang:integer_to_list(Number)),
    case catch register(Name, self()) of
        true ->
            Name;
        {'EXIT', {badarg, _}} ->
            register_unique_name(Number + 1)
    end.

erl_command_line_args(Waiter, LongNames, Prog, Host, Name, Args) ->
    NodeFlag = if LongNames -> "-name "; true -> "-sname " end,
    BasicCmd = lists:concat([Prog, " -detached -noinput ",
                             NodeFlag, Name, "@", Host,
                             " -s cloud_peer peer_start ", node(), " ", Waiter,
                             " ", Args]),
    case string_extensions:after_character($@, atom_to_list(node())) of
        Host ->
            {ok, BasicCmd};
        _ ->
            case ssh() of
                {ok, Ssh} ->
                    {ok, lists:concat([Ssh, " ", Host, " ", BasicCmd])};
                Other ->
                    Other
            end
    end.

ssh() ->
    Ssh = case init:get_argument(rsh) of
        {ok, [[Prog]]} -> Prog;
        _ -> "ssh"
    end,
    case os:find_executable(Ssh) of
        false -> {error, no_ssh};
        Path -> {ok, Path}
    end.

get_node_host_component(ignored, _) ->
    exit(not_alive);
get_node_host_component(true, Host) ->
    {ok, FullHostname} = net_adm:dns_hostname(Host),
    FullHostname;
get_node_host_component(false, Host) when is_list(Host) ->
    string_extensions:before_character($., Host);
get_node_host_component(false, Host) when is_atom(Host) ->
    string_extensions:before_character($., erlang:atom_to_list(Host)).

