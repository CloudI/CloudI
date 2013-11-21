%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Service for the messaging Test (sequence3)==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2012-2013, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2012-2013 Michael Truog
%%% @version 1.3.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_messaging_sequence3).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_service).

%% cloudi_service callbacks
-export([cloudi_service_init/3,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/2]).

-include("cloudi_logger.hrl").

-record(state, {
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------


%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(_Args, _Prefix, Dispatcher) ->
    cloudi_service:subscribe(Dispatcher, "f1"),
    cloudi_service:subscribe(Dispatcher, "f2"),
    cloudi_service:subscribe(Dispatcher, "g1"),
    cloudi_service:subscribe(Dispatcher, "sequence3"),
    {ok, #state{}}.

cloudi_service_handle_request(_Type, _Name, Pattern, _RequestInfo, Request,
                              _Timeout, _Priority, _TransId, _Pid,
                              #state{} = State,
                              Dispatcher) ->
    Prefix = cloudi_service:prefix(Dispatcher),
    Suffix = string:substr(Pattern, erlang:length(Prefix) + 1),
    case Suffix of
        "sequence3" ->
            ?LOG_INFO(" messaging sequence3 start erlang", []),
            sequence3(Dispatcher, Prefix),
            ?LOG_INFO(" messaging sequence3 end erlang", []),
            cloudi_service:send_async(Dispatcher,
                                      Prefix ++ "sequence4", "start"),
            {reply, "end", State};
        "f1" ->
            RequestI = erlang:list_to_integer(Request),
            if
                RequestI == 4 ->
                    {reply, "done", State};
                true ->
                    RequestNew = RequestI + 2, % two steps forward
                    {forward, Prefix ++ "f2", <<>>,
                     cloudi_string:term_to_list(RequestNew), State}
            end;
        "f2" ->
            RequestI = erlang:list_to_integer(Request),
            RequestNew = RequestI - 1, % one step back
            {forward, Prefix ++ "f1", <<>>,
             cloudi_string:term_to_list(RequestNew), State};
        "g1" ->
            {reply, Request ++ "suffix", State}
    end.

cloudi_service_handle_info(Request, State, _) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(_, #state{}) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

sequence3(Dispatcher, Prefix) ->
    {ok, Test1Id} = cloudi_service:send_async(Dispatcher, Prefix ++ "f1", "0"),
    {ok, <<>>, Test1Check, Test1Id} =
        cloudi_service:recv_async(Dispatcher, Test1Id),
    true = Test1Check == "done",
    {ok, Test2Check} = cloudi_service:send_sync(Dispatcher, Prefix ++ "g1",
                                            "prefix_"),
    true = Test2Check == "prefix_suffix",
    ok.
