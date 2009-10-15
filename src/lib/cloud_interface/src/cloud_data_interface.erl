%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Cloudi Data Repository Interface Behavior==
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

-module(cloud_data_interface).
-author('mjtruog [at] gmail (dot) com').

%% behavior callbacks
-export([behaviour_info/1]).

%% external interface
-export([do_queries_group/5]).

%% behavior external interface
-export([stop/1, do_queries/2]).

-include("cloud_logger.hrl").
-include("cloud_types.hrl").

%%%------------------------------------------------------------------------
%%% Callback functions from behavior
%%%------------------------------------------------------------------------

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), byte()}].

behaviour_info(callbacks) ->
    [
        {start_link, 2},
        {handle_stop, 1},
        {handle_do_queries, 2}
    ];
behaviour_info(_) ->
    undefined.

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Group queries based on the DataTitle===
%% Do queries that are grouped based on the DataTitle to be processed
%% (only queries that contain the specified DataTitle can be processed
%%  with the Module:Function/2 call that is specified
%%  (with arguments [Processing, State] where Processing is the
%%  list of queries and State is anything specific to the function)
%% all the processed queries are removed from the QueryList
%% so that it may be processed in other data modules, if necessary.
%% @end
%%-------------------------------------------------------------------------

-spec do_queries_group(QueryList :: data_list(),
                       Module :: atom(),
                       Function :: atom(),
                       State :: any(),
                       DataTitle :: atom()) ->
    {'ok', data_list()} |
    {'error', data_list()}.

do_queries_group(QueryList, Module, Function, State, DataTitle)
    when is_list(QueryList), is_atom(Module),
         is_atom(Function), is_atom(DataTitle) ->
    do_queries_group([], QueryList, QueryList, 
                     Module, Function, State, DataTitle).

%%-------------------------------------------------------------------------
%% @doc
%% ===Stop the data module.===
%% @end
%%-------------------------------------------------------------------------

-spec stop(DataTitle :: atom()) -> 'ok' | 'error'.

stop(DataTitle) when is_atom(DataTitle) ->
    call_data(DataTitle, 'handle_stop', [DataTitle],
              error).

%%-------------------------------------------------------------------------
%% @doc
%% ===Do queries in the data module.===
%% Not all the queries provided will be handled by the data module.
%% @end
%%-------------------------------------------------------------------------

-spec do_queries(DataTitle :: atom(),
                 QueryList :: data_list()) ->
    {'ok', data_list()} |
    {'error', data_list()}.

do_queries(DataTitle, QueryList)
    when is_atom(DataTitle), is_list(QueryList) ->
    call_data(DataTitle, 'handle_do_queries', [DataTitle, QueryList],
              {error, QueryList}).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

call_data(DataTitle, Function, Arguments, Error)
    when is_atom(DataTitle), is_atom(Function), is_list(Arguments) ->
    DataModuleString = 
        string_extensions:before_character($., erlang:atom_to_list(DataTitle)),
    try erlang:list_to_existing_atom(DataModuleString) of
        DataModule ->
            erlang:apply(DataModule, Function, Arguments)
    catch
        error:badarg ->
            ?LOG_ERROR("invalid data title \"~p\"", [DataTitle]),
            Error
    end.

%% remove queries that were processed from the main QueryList
get_all_remaining_queries(RemainingQueryList,
                          Remaining, Remaining,
                          QueryList, _)
    when is_list(RemainingQueryList), is_list(Remaining), is_list(QueryList) ->
    % all the other processing queries failed
    % (first failure aborts the process of executing remaining queries)
    RemainingQueryList ++ QueryList;
get_all_remaining_queries(RemainingQueryList,
                          [Query | ProcessingRemaining] = Processing,
                          Remaining, [QueryEntry | QueryList], DataTitle)
    when is_list(RemainingQueryList), is_list(Processing),
         is_list(Remaining), is_list(QueryList), is_atom(DataTitle) ->
    case lists_extensions:checked_delete({DataTitle, Query}, QueryEntry) of
        false ->
            get_all_remaining_queries(RemainingQueryList ++ [QueryEntry],
                                      Processing, Remaining,
                                      QueryList, DataTitle);
        [] ->
            get_all_remaining_queries(RemainingQueryList,
                                      ProcessingRemaining, Remaining,
                                      QueryList, DataTitle);
        NewQueryEntry ->
            get_all_remaining_queries(RemainingQueryList ++ [NewQueryEntry],
                                      ProcessingRemaining, Remaining,
                                      QueryList, DataTitle)
    end.

%% do_queries_group/5 implementation
do_queries_group([], [], QueryList, _, _, _, _) when is_list(QueryList) ->
    {ok, QueryList}; % no queries to handle in this module
do_queries_group(Processing, [], QueryList,
                 Module, Function, State, DataTitle)
    when is_list(Processing), is_list(QueryList), is_atom(Module),
         is_atom(Function), is_atom(DataTitle) ->
    case erlang:apply(Module, Function, [Processing, State]) of
        [] ->
            {ok, get_all_remaining_queries(
                [], Processing, [], QueryList, DataTitle)};
        Remaining when is_list(Remaining) ->
            {error, get_all_remaining_queries(
                [], Processing, Remaining, QueryList, DataTitle)}
    end;
do_queries_group(Processing, [QueryEntry | OtherQueries],
                 QueryList, Module, Function, State, DataTitle)
    when is_list(Processing), is_list(OtherQueries),
         is_list(QueryList), is_atom(Module),
         is_atom(Function), is_atom(DataTitle) ->
    case lists:keyfind(DataTitle, 1, QueryEntry) of
        false ->
            do_queries_group(Processing, OtherQueries,
                             QueryList, Module, Function, State, DataTitle);
        {DataTitle, Query} ->
            do_queries_group(Processing ++ [Query], OtherQueries,
                             QueryList, Module, Function, State, DataTitle)
    end.

