%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Router Service==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2014, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2014 Michael Truog
%%% @version 1.3.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_router).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_service).

%% external interface

%% cloudi_service callbacks
-export([cloudi_service_init/3,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/2]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include_lib("cloudi_core/include/cloudi_service.hrl").

-define(DEFAULT_ADD_PREFIX,                  true). % to destinations
-define(DEFAULT_MODE,                 round_robin).
-define(DEFAULT_PARAMETERS_ALLOWED,          true).
-define(DEFAULT_PARAMETERS_STRICT_MATCHING,  true).

-record(destination,
    {
        mode = ?DEFAULT_MODE :: random | round_robin,
        service_names = [] :: list(string()),
        index = 1 :: pos_integer(),
        parameters_allowed = ?DEFAULT_PARAMETERS_ALLOWED
            :: boolean(),
        parameters_strict_matching = ?DEFAULT_PARAMETERS_STRICT_MATCHING
            :: boolean(),
        parameters_selected = [] :: list(pos_integer()),
        length = 1 :: pos_integer()
    }).

-record(state,
    {
        destinations % pattern -> #destination{}
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, Prefix, Dispatcher) ->
    Defaults = [
        {destinations,               []},
        {add_prefix,                 ?DEFAULT_ADD_PREFIX}],
    [DestinationsL, AddPrefix] = cloudi_proplists:take_values(Defaults, Args),
    true = is_list(DestinationsL) andalso
           (erlang:length(DestinationsL) > 0),
    true = is_boolean(AddPrefix),
    ConfigDefaults = [
        {mode,                       ?DEFAULT_MODE},
        {parameters_allowed,         ?DEFAULT_PARAMETERS_ALLOWED},
        {parameters_strict_matching, ?DEFAULT_PARAMETERS_STRICT_MATCHING},
        {parameters_selected,        []},
        {service_names,              []}],
    Destinations = lists:foldl(fun({PatternSuffix, L}, D) ->
        cloudi_service:subscribe(Dispatcher, PatternSuffix),
        case L of
            [I | _] when is_integer(I) ->
                Names = if
                    AddPrefix =:= true ->
                        [Prefix ++ L];
                    AddPrefix =:= false ->
                        [L]
                end,
                cloudi_x_trie:store(Prefix ++ PatternSuffix,
                                    #destination{service_names = Names}, D);
            [_ | _] ->
                [Mode,
                 ParametersAllowed,
                 ParametersStrictMatching,
                 ParametersSelected,
                 Names0] = cloudi_proplists:take_values(ConfigDefaults, L),
                true = is_atom(Mode) andalso
                       ((Mode =:= random) orelse
                        (Mode =:= round_robin)),
                true = is_boolean(ParametersAllowed),
                true = is_boolean(ParametersStrictMatching),
                true = is_list(ParametersSelected),
                true = ((ParametersSelected == []) orelse
                        ((ParametersSelected /= []) andalso
                         (ParametersAllowed =:= true))),
                true = lists:all(fun(I) -> is_integer(I) andalso I > 0 end,
                                 ParametersSelected),
                true = is_list(Names0),
                Length = erlang:length(Names0),
                true = (Length > 0),
                Names1 = if
                    AddPrefix =:= true ->
                        [Prefix ++ Suffix || Suffix <- Names0];
                    AddPrefix =:= false ->
                        Names0
                end,
                true = ((ParametersAllowed =:= true) orelse
                        ((ParametersAllowed =:= false) andalso
                         (lists:any(fun cloudi_x_trie:is_pattern/1,
                                    Names1) =:= false))),
                Destination = #destination{mode = Mode,
                                           parameters_allowed =
                                               ParametersAllowed,
                                           parameters_strict_matching =
                                               ParametersStrictMatching,
                                           parameters_selected =
                                               ParametersSelected,
                                           service_names = Names1,
                                           length = Length},
                cloudi_x_trie:store(Prefix ++ PatternSuffix, Destination, D)
        end
    end, cloudi_x_trie:new(), DestinationsL),
    {ok, #state{destinations = Destinations}}.

cloudi_service_handle_request(_Type, Name, Pattern, RequestInfo, Request,
                              Timeout, Priority, _TransId, _Pid,
                              #state{destinations = Destinations} = State,
                              _Dispatcher) ->
    case cloudi_x_trie:find(Pattern, Destinations) of
        {ok, #destination{} = Destination} ->
            {NextName, NewDestination} = destination_pick(Destination),
            Parameters = cloudi_service:service_name_parse(Name, Pattern),
            case name_parameters(NextName, Parameters, NewDestination) of
                {ok, NewName} ->
                    NewDestinations = cloudi_x_trie:store(Pattern,
                                                          NewDestination,
                                                          Destinations),
                    {forward, NewName, RequestInfo, Request,
                     Timeout, Priority,
                     State#state{destinations = NewDestinations}};
                {error, Reason} ->
                    ?LOG_ERROR("(~p -> ~p) error: ~p",
                               [Name, NextName, Reason]),
                    {noreply, State}
            end;
        error ->
            {noreply, State}
    end.

cloudi_service_handle_info(Request, State, _) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(_, #state{}) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

destination_pick(#destination{service_names = [Name]} = Destination) ->
    {Name, Destination};
destination_pick(#destination{mode = round_robin,
                              service_names = Names,
                              index = Index,
                              length = Length} = Destination) ->
    NewIndex = if
        Index == Length ->
            1;
        true ->
            Index + 1
    end,
    Name = lists:nth(Index, Names),
    {Name, Destination#destination{index = NewIndex}};
destination_pick(#destination{mode = random,
                              service_names = Names,
                              length = Length} = Destination) ->
    Index = cloudi_x_quickrand:uniform(Length),
    Name = lists:nth(Index, Names),
    {Name, Destination#destination{index = Index}}.

name_parameters_strip([], NewName) ->
    {ok, lists:reverse(NewName)};
name_parameters_strip([$* | Name], NewName) ->
    name_parameters_strip(Name, NewName);
name_parameters_strip([C | Name], NewName) ->
    name_parameters_strip(Name, [C | NewName]).

name_parameters_insert([], NewName, [], _) ->
    {ok, lists:reverse(NewName)};
name_parameters_insert([], NewName, [_ | _], ParametersStrictMatching) ->
    if
        ParametersStrictMatching =:= true ->
            {error, parameters_ignored};
        true ->
            {ok, lists:reverse(NewName)}
    end;
name_parameters_insert([$* | Name], NewName,
                       [], ParametersStrictMatching) ->
    if
        ParametersStrictMatching =:= true ->
            {error, parameter_missing};
        true ->
            name_parameters_strip(Name, NewName)
    end;
name_parameters_insert([$* | Name], NewName,
                       [Parameter | Parameters], ParametersStrictMatching) ->
    name_parameters_insert(Name, lists:reverse(Parameter) ++ NewName,
                           Parameters, ParametersStrictMatching);
name_parameters_insert([C | Name], NewName,
                       Parameters, ParametersStrictMatching) ->
    name_parameters_insert(Name, [C | NewName],
                           Parameters, ParametersStrictMatching).

name_parameters_insert(Name, Parameters,
                       ParametersStrictMatching) ->
    name_parameters_insert(Name, [], Parameters,
                           ParametersStrictMatching).

name_parameters_select([], NewName, _,
                       ParametersSelected, ParametersStrictMatching) ->
    if
        ParametersStrictMatching =:= true, ParametersSelected /= [] ->
            {error, {parameters_selected_ignored, ParametersSelected}};
        true ->
            {ok, lists:reverse(NewName)}
    end;
name_parameters_select([$* | Name], NewName, _,
                       [], ParametersStrictMatching) ->
    if
        ParametersStrictMatching =:= true ->
            {error, parameters_selected_empty};
        true ->
            name_parameters_strip(Name, NewName)
    end;
name_parameters_select([$* | Name], NewName, Parameters,
                       [I | ParametersSelected], ParametersStrictMatching) ->
    try lists:nth(I, Parameters) of
        Parameter ->
            name_parameters_select(Name, lists:reverse(Parameter) ++ NewName,
                                   Parameters, ParametersSelected,
                                   ParametersStrictMatching)
    catch
        error:_ ->
            if
                ParametersStrictMatching =:= true ->
                    {error, {parameters_selected_missing, I}};
                true ->
                    name_parameters_strip(Name, NewName)
            end
    end;
name_parameters_select([C | Name], NewName, Parameters,
                       ParametersSelected, ParametersStrictMatching) ->
    name_parameters_select(Name, [C | NewName], Parameters,
                           ParametersSelected, ParametersStrictMatching).

name_parameters_select(Name, Parameters,
                       ParametersSelected, ParametersStrictMatching) ->
    name_parameters_select(Name, [], Parameters,
                           ParametersSelected, ParametersStrictMatching).

name_parameters(Name, [], #destination{}) ->
    {ok, Name};
name_parameters(_, [_ | _],
                #destination{parameters_allowed = false}) ->
    {error, parameters_not_allowed};
name_parameters(Name, Parameters,
                #destination{parameters_strict_matching =
                                 ParametersStrictMatching,
                             parameters_selected =
                                 ParametersSelected}) ->
    if
        ParametersSelected == [] ->
            name_parameters_insert(Name, Parameters,
                                   ParametersStrictMatching);
        true ->
            name_parameters_select(Name, Parameters, ParametersSelected,
                                   ParametersStrictMatching)
    end.

