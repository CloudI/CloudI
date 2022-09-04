%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Null Response CloudI Service==
%%% Simple /dev/null equivalent as a CloudI service.
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2017-2021 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2017-2021 Michael Truog
%%% @version 2.0.3 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_null).
-author('mjtruog at protonmail dot com').

-behaviour(cloudi_service).

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_request/11,
         cloudi_service_terminate/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-define(DEFAULT_DEBUG,                      false). % log output for debugging
-define(DEFAULT_DEBUG_LEVEL,                trace).
-define(DEFAULT_DEBUG_CONTENTS,             false).

-record(state,
    {
        debug_level :: off | trace | debug | info | warn | error | fatal,
        debug_contents :: boolean()
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, _Prefix, _Timeout, Dispatcher) ->
    Defaults = [
        {debug,                    ?DEFAULT_DEBUG},
        {debug_level,              ?DEFAULT_DEBUG_LEVEL},
        {debug_contents,           ?DEFAULT_DEBUG_CONTENTS}],
    [Debug, DebugLevel,
     DebugContents] = cloudi_proplists:take_values(Defaults, Args),
    true = is_boolean(Debug),
    true = ((DebugLevel =:= trace) orelse
            (DebugLevel =:= debug) orelse
            (DebugLevel =:= info) orelse
            (DebugLevel =:= warn) orelse
            (DebugLevel =:= error) orelse
            (DebugLevel =:= fatal)),
    true = is_boolean(DebugContents),
    cloudi_service:subscribe(Dispatcher, ""),
    DebugLogLevel = if
        Debug =:= false ->
            off;
        Debug =:= true ->
            DebugLevel
    end,
    {ok, #state{debug_level = DebugLogLevel,
                debug_contents = DebugContents}}.

cloudi_service_handle_request(_RequestType, Name, _Pattern,
                              RequestInfo, Request,
                              _Timeout, _Priority, _TransId, _Source,
                              #state{debug_level = DebugLogLevel,
                                     debug_contents = DebugContents} = State,
                              _Dispatcher) ->
    if
        DebugContents =:= true ->
            ?LOG(DebugLogLevel,
                 "\"~ts\" consumed~n"
                 "(~tp,~n"
                 " ~tp)",
                 [cloudi_service_name:utf8(Name), RequestInfo, Request]);
        DebugContents =:= false ->
            ?LOG(DebugLogLevel,
                 "\"~ts\" consumed",
                 [cloudi_service_name:utf8(Name)])
    end,
    {reply, <<>>, State}.

cloudi_service_terminate(_Reason, _Timeout, _State) ->
    ok.

