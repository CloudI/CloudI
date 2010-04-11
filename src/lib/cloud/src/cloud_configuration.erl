%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Cloudi Configuration==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2009, Michael Truog <mjtruog at gmail dot com>
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%% 
%%%     * Redistributions of source code must retain the above copyright
%%%       notice, this list of conditions and the following disclaimer.
%%%     * Redistributions in binary form must reproduce the above copyright
%%%       notice, this list of conditions and the following disclaimer in
%%%       the documentation and/or other materials provided with the
%%%       distribution.
%%%     * All advertising materials mentioning features or use of this
%%%       software must display the following acknowledgment:
%%%         This product includes software developed by Michael Truog
%%%     * The name of the author may not be used to endorse or promote
%%%       products derived from this software without specific prior
%%%       written permission
%%% 
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
%%% DAMAGE.
%%%
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2009 Michael Truog
%%% @version 0.0.7 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloud_configuration).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([open/0, open/1,
         parse_job/1, parse_data/1, parse_machine/2,
         save/1, save/2]).

-include("cloud_configuration.hrl").

-define(CONFIGURATION_FILE_NAME, "cloud.conf").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Parse the Cloudi configuration file.===
%% ===The configuration file contains the following terms:===
%% ====The cookie to use for distributed communication:====
%%   `{cookie, "cookie value"}'
%%
%% ====The JSON RPC server settings:====
%%   `{json_rpc, Port}'
%%
%% ====The logging settings for all nodes:====
%%   `{logging, "path/to/log/file", LogLevel}'
%%
%%   LogLevel is either one of the atoms:
%%
%%   `undefined, critical, error, warning, info, debug'
%%
%%   or LogLevel is an integer:
%%
%%   `0,         1,        2,     3,       4,    5'
%%
%% ====The jobs that Cloudi will run:====
%%   `{jobs, [{"cloud_job_uniquename.tag", [Argument1, Argument2, ...], Tasks, UseThreads}]}'
%%
%%   The "cloud_job_uniquename" string is both the name of a C dynamic library
%%   and an Erlang module (that is locally registered) that coordinates work
%%   for the C code.  "cloud_job_uniquename.tag" is called the work title
%%   and defines the type of work in other parts of Cloudi.
%%   The job prefix of "cloud_job_" is used as a namespace for the
%%   Erlang module and the "uniquename" describes the work.  The ".tag"
%%   may be ommitted but must be unique if the same "uniquename" is instanced
%%   more than once in the same cloud.  The ".tag" allows separate instances
%%   of the work, possibly with different Arguments.
%%   The Arguments are provided to the Erlang module's start_link function.
%%   The Tasks can be either the atom 'all' to allocate all machine
%%   resources to the work, or it can be an integer.
%%   The UseThreads can be either the atoms 'no_threads" or 'threads', or
%%   it can be an integer which determines how many threads per process.
%%
%% ====The data that Cloudi will use:====
%%   `{data, [{"cloud_data_uniquename", [{Parameter0, ParameterValue0}]}]}'
%%
%%   The "cloud_data_uniquename" string is the name of an Erlang module
%%   (that is locally registered) that handles all data of this type.
%%   If multiple databases are used by this data module, they become a suffix
%%   like: "cloud_data_uniquename.databasename", for the data title.  The
%%   data title is used in other parts of Cloudi to describe the destination
%%   of data.  The data module prefix of "cloud_data_" is used as a namespace
%%   for the Erlang module and the "uniquename" describes the data storage.
%%   All the parameters supplied to the data module are specific to the
%%   data module.  The cloud_data_repository_sup module does expect
%%   'database' as a
%%   parameter when creating data titles and starting the data modules but
%%   it is not required (so a data title could be the data module name if
%%   no database distinction is necessary).
%%
%% ====The machines that Cloudi will run on:====
%%   `{machines, [{"node1@hostname1", [{LowPort, HighPort}], Processes, Threads}]}'
%%
%%   The first entry in the machines list is the machine which Cloudi is
%%   started on (all later entries can occur in any order).  The node name
%%   must include a valid hostname (that can be resolved with
%%   DNS or a hosts file).  LowPort and HighPort defines a range of ports
%%   that must be usable on the machine for cnode worker traffic
%%   (the exchange of work assignments and results).  The ports can also be
%%   be specified as a list of numbers, instead of a list of tuples
%%   (or integer ports can be mixed with tuple port ranges).
%%   The number of ports specified must be greater than or equal to
%%   Processes, which is the number of processes to be started on the
%%   machine.  The Threads parameter specifies the number of threads
%%   that can be used on the machine for Cloudi work allocation.
%%
%% @end
%%-------------------------------------------------------------------------

-spec open() -> #config{}.

open() ->
    {ok, Terms} = file:consult(?CONFIGURATION_FILE_NAME),
    new(Terms, #config{}).

-spec open(Path :: string()) -> #config{}.

open(Path) when is_list(Path) ->
    {ok, Terms} = file:consult(Path),
    new(Terms, #config{}).

%%-------------------------------------------------------------------------
%% @doc
%% ===Parse a string containing the configuration for a single job, e.g.:===
%%
%% `"{\"cloud_job_tests\", [Argument1, Argument2, ...], all, threads}"'
%% @end
%%-------------------------------------------------------------------------

-spec parse_job(L :: string()) -> #config_work{}.

parse_job(L) when is_list(L) ->
    [Value] = new_jobs([string_extensions:list_to_term(L)]),
    Value.

%%-------------------------------------------------------------------------
%% @doc
%% ===Parse a string containing the configuration for a single data module, e.g.:===
%%
%% `"{\"cloud_data_pgsql\", [Option1, Option2, ...]}"'
%% @end
%%-------------------------------------------------------------------------

-spec parse_data(L :: string()) -> #config_data{}.

parse_data(L) when is_list(L) ->
    [Value] = new_data([string_extensions:list_to_term(L)]),
    Value.

%%-------------------------------------------------------------------------
%% @doc
%% ===Parse a string containing the configuration for a single machine (node), e.g.:===
%%
%% `"{\"cloud@machine\",   [{3501, 3508}], 1, 4}"'
%% @end
%%-------------------------------------------------------------------------

-spec parse_machine(L :: string(), LongNames :: bool()) -> #config_machine{}.

parse_machine(L, LongNames) when is_list(L), is_boolean(LongNames) ->
    [Value] = new_machines([string_extensions:list_to_term(L)], LongNames),
    Value.

%%-------------------------------------------------------------------------
%% @doc
%% ===Save the Cloudi configuration file.===
%% @end
%%-------------------------------------------------------------------------

-spec save(Config :: #config{}) -> 'ok'.

save(Config) when is_record(Config, config) ->
    save(?CONFIGURATION_FILE_NAME, Config).

%%-------------------------------------------------------------------------
%% @doc
%% ===Save the Cloudi configuration file at a specific path.===
%% @end
%%-------------------------------------------------------------------------

-spec save(Path :: string(), Config :: #config{}) -> 'ok'.

save(Path, Config) when is_list(Path), is_record(Config, config) ->
    TemporaryPath = Path ++ ".tmp",
    {ok, FD} = file:open(TemporaryPath, [write]),
    io:format(FD, "~p.~n", [{cookie, atom_to_list(Config#config.cookie)}]),
    io:format(FD, "~p.~n", [{logging,
        (Config#config.logging)#config_logging.filename,
        (Config#config.logging)#config_logging.loglevel}]),
    io:format(FD, "~p.~n", [{json_rpc,
        Config#config.json_rpc_port}]),
    io:format(FD, "~p.~n", [{jobs, 
        lists:foldr(fun(Work, L) -> [{
            Work#config_work.work_title,
            Work#config_work.work_arguments,
            Work#config_work.concurrent_tasks,
            (if
                Work#config_work.use_threads ->
                    threads;
                true ->
                    no_threads
            end)
            } | L] end, [], Config#config.jobs
        )
    }]),
    io:format(FD, "~p.~n", [{data, 
        lists:foldr(fun(Data, L) -> [{
            Data#config_data.data_module,
            Data#config_data.arguments
            } | L] end, [], Config#config.data
        )
    }]),
    io:format(FD, "~p.~n", [{machines, 
        lists:foldr(fun(Machine, L) -> [{
            atom_to_list(Machine#config_machine.node_name),
            Machine#config_machine.ports,
            Machine#config_machine.processes,
            Machine#config_machine.threads
            } | L] end, [], Config#config.machines
        )
    }]),
    ok = file:close(FD),
    % make the backup file unique
    {{Year, Month, Day}, {Hour, Minute, Second}} = 
        erlang:universaltime_to_localtime(erlang:universaltime()),
    {_, _, MicroSecs} = now(),
    BackupSuffix = string_extensions:format(
        ".~4..0w~2..0w~2..0w~2..0w~2..0w~2..0w~6..0w",
        [Year, Month, Day, Hour, Minute, Second, MicroSecs]
    ),
    ok = file:rename(Path, Path ++ BackupSuffix),
    ok = file:rename(TemporaryPath, Path),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

%% define the structure of the configuration file for parsing
new([], Config) when is_record(Config, config) ->
    % finalize the configuration

    % make sure the local host is the first machine entry
    {ok, HostName} = inet:gethostname(),
    Result = if
        Config#config.node_longnames ->
            lists_extensions:keypttake(
                {undefined, HostName, undefined},
                #config_machine.node_name_parsed,
                2#010, Config#config.machines);
        true ->
            lists_extensions:keypttake(
                {undefined, HostName},
                #config_machine.node_name_parsed,
                2#01, Config#config.machines)
    end,
    % just modifying the cookie will not prevent collisions
    % of the same node name in the same epmd.  however, to make
    % the system redundant, separate epmds are used for each instance.
    case Result of
        false ->
            throw("invalid configuration");
        {value, Position, Value, Remaining} ->
            Config#config{
                % modify the cookie with the index so that replication
                % in separate Erlang VMs provides failover
                cookie = list_to_atom(
                    Config#config.cookie ++ integer_to_list(Position)),
                machines = [Value] ++ Remaining}
    end;
new([{cookie, Value} | Terms], Config)
    when is_list(Value), is_record(Config, config) ->
    new(Terms, Config#config{cookie = Value});
new([{logging, FileName, Level} | Terms], Config)
    when is_list(FileName), is_record(Config, config) ->
    new(Terms, Config#config{logging = #config_logging{
        filename = FileName, loglevel = Level}});
new([{json_rpc, Port} | Terms], Config)
    when is_integer(Port), Port > 0, is_record(Config, config) ->
    new(Terms, Config#config{json_rpc_port = Port});
new([{jobs, []} | Terms], Config) ->
    new(Terms, Config);
new([{jobs, [H | _] = Value} | Terms], Config)
    when is_tuple(H), is_record(Config, config) ->
    new(Terms, Config#config{jobs = new_jobs(Value)});
new([{data, []} | Terms], Config) ->
    new(Terms, Config);
new([{data, [H | _] = Value} | Terms], Config)
    when is_tuple(H), is_record(Config, config) ->
    new(Terms, Config#config{data = new_data(Value)});
new([{machines, [H | _] = Value} | Terms], Config)
    when is_tuple(H), is_record(Config, config) ->
    {LongNames, _} = parse_node_name(erlang:element(1, H)),
    new(Terms, Config#config{node_longnames = LongNames,
                             machines = new_machines(Value, LongNames)}).

%% parse configuration file jobs term tuple value list into a record
new_jobs(Terms) when is_list(Terms) ->
    new_jobs(Terms, []).
new_jobs([], Jobs) when is_list(Jobs) ->
    Jobs;
new_jobs([{WorkTitle, Arguments, Tasks, UseThreads} | Terms], Jobs)
    when is_list(WorkTitle), is_list(Arguments),
         ((Tasks =:= all) or is_integer(Tasks)),
         ((UseThreads =:= no_threads) or (UseThreads =:= threads) or
          is_integer(UseThreads)), is_list(Jobs) ->
    Threads = case UseThreads of
        no_threads -> false;
        threads -> true;
        I when is_integer(I) -> I
    end,
    new_jobs(Terms, Jobs ++ 
             [#config_work{
                work_title = WorkTitle,
                work_arguments = Arguments,
                concurrent_tasks = Tasks,
                use_threads = Threads
              }]).

%% parse configuration file data term tuple value list into records that
%% define data sources and repositories
new_data(Terms) when is_list(Terms) ->
    new_data(Terms, []).
new_data([], Databases) when is_list(Databases) ->
    Databases;
new_data([{DataModule, Arguments} | Terms], Databases)
    when is_list(DataModule), is_list(Arguments), is_list(Databases) ->
    new_data(Terms, Databases ++
             [#config_data{
                data_module = DataModule,
                arguments = Arguments
              }]).

%% parse configuration file machines term tuple value list into a record
%%
%% make sure that all names are either longnames or shortnames since
%% you can not mix the naming conventions with connected nodes.
new_machines(Terms, LongNames) when is_list(Terms) ->
    new_machines(Terms, LongNames, []).
new_machines([], _, Machines) when is_list(Machines) ->
    Machines;
new_machines([{NodeName, PortRanges, Processes, Threads} | Terms],
             LongNames, Machines)
    when is_list(NodeName), is_list(PortRanges),
         is_integer(Processes), is_integer(Threads), is_list(Machines) ->   
    {LongNames, ParsedNodeName} = parse_node_name(NodeName),
    PortsList = parse_port_ranges(PortRanges),
    true = (length(PortsList) >= Processes),
    new_machines(Terms, LongNames, Machines ++
                 [#config_machine{
                    node_name = list_to_atom(NodeName),
                    node_name_parsed = ParsedNodeName,
                    ports = PortsList,
                    processes = Processes,
                    threads = Threads
                 }]).

%% parse a valid short node name or long node name as a list into a tuple
parse_node_name(Name) when is_list(Name) ->
    parse_node_name([], [], [], 0, Name);
parse_node_name(Name) when is_atom(Name) ->
    parse_node_name([], [], [], 0, atom_to_list(Name)).
parse_node_name(Node, HostName, DomainName, 0, [$@ | Name])
    when is_list(Node), is_list(HostName), is_list(DomainName), is_list(Name) ->
    parse_node_name(Node, HostName, DomainName, 1, Name);
parse_node_name(Node, HostName, DomainName, 1, [$. | Name])
    when is_list(Node), is_list(HostName), is_list(DomainName), is_list(Name) ->
    parse_node_name(Node, HostName, DomainName, 2, Name);
parse_node_name(Node, HostName, DomainName, 0, [C | Name])
    when is_list(Node), is_list(HostName), is_list(DomainName), is_list(Name) ->
    parse_node_name(Node ++ [C], HostName, DomainName, 0, Name);
parse_node_name(Node, HostName, DomainName, 1, [C | Name])
    when is_list(Node), is_list(HostName), is_list(DomainName), is_list(Name) ->
    parse_node_name(Node, HostName ++ [C], DomainName, 1, Name);
parse_node_name(Node, HostName, DomainName, 2, [C | Name])
    when is_list(Node), is_list(HostName), is_list(DomainName), is_list(Name) ->
    parse_node_name(Node, HostName, DomainName ++ [C], 2, Name);
parse_node_name(Node, HostName, _, 1, [])
    when is_list(Node), is_list(HostName) ->
    {false, {Node, HostName}};
parse_node_name(Node, HostName, DomainName, 2, [])
    when is_list(Node), is_list(HostName), is_list(DomainName) ->
    {true, {Node, HostName, DomainName}}.

%% parse a list of port ranges specified by tuples that represent inclusive
%% ranges of ports, and expand into a list of port numbers
parse_port_ranges(Ranges) when is_list(Ranges) ->
    parse_port_ranges([], 0, 0, Ranges).
parse_port_ranges(Ports, Pmax, Pmax, [])
    when is_list(Ports), is_integer(Pmax) -> Ports;
parse_port_ranges(Ports, Pmax, Pmax, [{Pstart, Pend} | Ranges])
    when is_list(Ports), is_integer(Pmax),
         is_integer(Pstart), is_integer(Pend), is_list(Ranges) ->
    parse_port_ranges(Ports ++ [Pstart], Pstart + 1, Pend + 1, Ranges);
parse_port_ranges(Ports, Pmax, Pmax, [{P} | Ranges])
    when is_list(Ports), is_integer(Pmax), is_integer(P), is_list(Ranges) ->
    parse_port_ranges(Ports ++ [P], P, P, Ranges);
parse_port_ranges(Ports, Pmax, Pmax, [P | Ranges])
    when is_list(Ports), is_integer(Pmax), is_integer(P), is_list(Ranges) ->
    parse_port_ranges(Ports ++ [P], P, P, Ranges);
parse_port_ranges(Ports, P, Pmax, Ranges)
    when is_list(Ports), is_integer(Pmax), is_integer(P), is_list(Ranges) ->
    parse_port_ranges(Ports ++ [P], P + 1, Pmax, Ranges).

