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
%%% @version 1.3.1 {@date} {@time}
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

-record(destination,
    {
        mode = round_robin :: random | round_robin,
        service_names = [] :: list(string()),
        index = 1 :: pos_integer(),
        length :: pos_integer()
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
        {destinations,             []}],
    [DestinationsL] = cloudi_proplists:take_values(Defaults, Args),
    true = is_list(DestinationsL) andalso
           (erlang:length(DestinationsL) > 0),
    ConfigDefaults = [
        {mode,                     round_robin},
        {service_names,            []}],
    Destinations = lists:foldl(fun({PatternSuffix, L}, D) ->
        cloudi_service:subscribe(Dispatcher, PatternSuffix),
        case L of
            [I | _] when is_integer(I) ->
                cloudi_x_trie:store(Prefix ++ PatternSuffix,
                                    #destination{service_names = [L]}, D);
            [_ | _] ->
                [Mode,
                 Names] = cloudi_proplists:take_values(ConfigDefaults, L),
                true = is_atom(Mode) andalso
                       ((Mode =:= random) orelse
                        (Mode =:= round_robin)),
                true = is_list(Names) andalso
                       (erlang:length(Names) > 0),
                true = lists:all(fun(Name) ->
                    not cloudi_x_trie:is_pattern(Name)
                end, Names),
                Length = erlang:length(Names),
                cloudi_x_trie:store(Prefix ++ PatternSuffix,
                                    #destination{mode = Mode,
                                                 service_names = Names,
                                                 length = Length}, D)
        end
    end, cloudi_x_trie:new(), DestinationsL),
    {ok, #state{destinations = Destinations}}.

cloudi_service_handle_request(_Type, _Name, Pattern, RequestInfo, Request,
                              Timeout, Priority, _TransId, _Pid,
                              #state{destinations = Destinations} = State,
                              _Dispatcher) ->
    case cloudi_x_trie:find(Pattern, Destinations) of
        {ok, #destination{} = Destination} ->
            {NextName, NextDestination} = destination_pick(Destination),
            {forward, NextName, RequestInfo, Request, Timeout, Priority,
             State#state{destinations = cloudi_x_trie:store(Pattern,
                                                            NextDestination,
                                                            Destinations)}};
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

