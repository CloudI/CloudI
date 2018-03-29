%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Service for the messaging Test (sequence3)==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2012-2017 Michael Truog <mjtruog at protonmail dot com>
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
%%% @author Michael Truog <mjtruog at protonmail dot com>
%%% @copyright 2012-2017 Michael Truog
%%% @version 1.7.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_test_messaging_sequence3).
-author('mjtruog at protonmail dot com').

-behaviour(cloudi_service).

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_request/11,
         cloudi_service_terminate/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-record(state, {
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(_Args, _Prefix, _Timeout, Dispatcher) ->
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
            ?LOG_INFO("messaging sequence3 start erlang", []),
            sequence3(Dispatcher, Prefix),
            ?LOG_INFO("messaging sequence3 end erlang", []),
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

cloudi_service_terminate(_Reason, _Timeout, #state{}) ->
    ?LOG_INFO("terminate messaging 3 erlang", []),
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
