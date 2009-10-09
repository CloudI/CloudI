%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Cloudi Data Repository Supervisor==
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
%%% @version 0.0.5 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloud_data_repository_sup).
-author('mjtruog [at] gmail (dot) com').

-behaviour(supervisor).

%% external interface
-export([start_link/1,
         add_data/1, remove_data/1,
         exists/1]).

%% supervisor callbacks
-export([init/1]).

-include("cloud_configuration.hrl").
-include("cloud_logger.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Start a supervisor to monitor all data repositories specified in the configuration.===
%% A data module process for every database mentioned in
%% the configuration will be started under the supervisor.
%% @end
%%-------------------------------------------------------------------------

-spec start_link(Config :: #config{}) -> {'ok', pid()} | {'error', any()}.

start_link(Config) when is_record(Config, config) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Config]).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add a data repository to the cloud.===
%% @end
%%-------------------------------------------------------------------------

-spec add_data(L :: string()) -> 'ok' | {'error', any()}.

add_data(L) when is_list(L) ->
    try cloud_configuration:parse_data(L) of
        C when is_record(C, config_data) ->
            lists_extensions:iter(fun(ChildSpec, Iter) ->
                case supervisor:start_child(?MODULE, ChildSpec) of
                    {ok, _} ->
                        ?LOG_DEBUG("added data ~p",
                            [erlang:element(1, ChildSpec)]),
                        Iter();
                    {ok, _, _} ->
                        ?LOG_DEBUG("added data ~p",
                            [erlang:element(1, ChildSpec)]),
                        Iter();
                    {error, _} = Error ->
                        Error
                end
            end, ok, build_child_specifications([], [C]))
    catch
        _:Reason ->
            {error, Reason}
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove a data repository from the cloud.===
%% @end
%%-------------------------------------------------------------------------

-spec remove_data(DataTitle :: string() | atom()) -> 'ok' | {'error', any()}.

remove_data(DataTitle) when is_list(DataTitle) ->
    try erlang:list_to_existing_atom(DataTitle) of
        A ->
            remove_data(A)
    catch
        error:badarg ->
            {error, "invalid data title"}
    end;

remove_data(DataTitle) when is_atom(DataTitle) ->
    case supervisor:terminate_child(?MODULE, DataTitle) of
        ok ->
            Result = supervisor:delete_child(?MODULE, DataTitle),
            if
                Result == ok ->
                    ?LOG_DEBUG("removed data ~p", [DataTitle]);
                true ->
                    ok
            end,
            Result;
        {error, _} = Error ->
            Error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Check if a data repository (database) exists.===
%% Check the data title, "DATA_MODULE.DATABASE_NAME",
%% when supplied as an atom.  The data title should also have
%% been used as a locally registered name for the
%% the data module handling the database.
%% @end
%%-------------------------------------------------------------------------

-spec exists(DataTitle :: atom()) -> bool().

exists(DataTitle) when is_atom(DataTitle) ->
    try gen_server:call(?MODULE, which_children, 5000) of
        [] -> false;
        L when is_list(L) -> lists:keymember(DataTitle, 1, L)
    catch
        _:_ -> false
    end.

%%%------------------------------------------------------------------------
%%% Callback functions from supervisor
%%%------------------------------------------------------------------------

init([Config]) when is_record(Config, config) ->
    MaxRestarts = 5,
    MaxTime = 60, % seconds (1 minute)
    {ok, {{one_for_one, MaxRestarts, MaxTime}, 
          build_child_specifications(Config)}}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

%% create the child specification for the data repositories
build_child_specifications(#config{data = DataRepositories}) ->
    build_child_specifications([], DataRepositories).
build_child_specifications(Specification, []) when is_list(Specification) ->
    Specification;
build_child_specifications(Specification, [
    #config_data{data_module = DataModuleString,
                 arguments = Arguments} | DataRepositories])
    when is_list(Specification), is_list(DataModuleString),
         is_list(Arguments) ->
    Restart = permanent, % always restarted
    Shutdown = 2000, % milliseconds (2 seconds)
    Type = worker,
    DataModule = erlang:list_to_atom(DataModuleString),
    c:l(DataModule),
    {DatabaseArguments, NonDatabaseArguments} = lists:partition(fun(A) ->
        erlang:element(1, A) == database
    end, Arguments),
    % should be the only call of list_to_atom/1 for the DataTitle, other
    % code uses list_to_existing_atom/1
    if
        erlang:length(DatabaseArguments) == 0 ->
            DataTitle = DataModule,
            build_child_specifications(
                Specification ++ [
                    {DataTitle,
                     {DataModule, start_link,
                      [DataTitle, Arguments]},
                     Restart, Shutdown, Type, [DataTitle]}
                ], DataRepositories);
        true ->
            build_child_specifications(
                lists:foldl(fun(DArg, S) ->
                    % should be the only call of list_to_atom/1 for the
                    % DataTitle, other code uses list_to_existing_atom/1
                    DataTitle = erlang:list_to_atom(
                        DataModuleString ++ "." ++ erlang:element(2, DArg)),
                    S ++ [
                        {DataTitle,
                         {DataModule, start_link,
                          [DataTitle, NonDatabaseArguments ++ [DArg]]},
                         Restart, Shutdown, Type, [DataTitle]}
                    ]
                end, Specification, DatabaseArguments), DataRepositories)
    end.

