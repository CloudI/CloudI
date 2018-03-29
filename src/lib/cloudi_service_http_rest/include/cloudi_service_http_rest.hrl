%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%%
%%% MIT License
%%%
%%% Copyright (c) 2015-2017 Michael Truog <mjtruog at protonmail dot com>
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
%%%------------------------------------------------------------------------

-define(SPEC_REST_INITIALIZE(Function),
-spec Function(Args :: list(),
               Timeout :: cloudi_service_api:
                          timeout_initialize_value_milliseconds(),
               Dispatcher :: cloudi:dispatcher()) ->
    {ok, State :: any()} |
    {stop, Reason :: any()} |
    {stop, Reason :: any(), State :: any()}).
-define(SPEC_REST_HANDLER_11(Function),
-spec Function(Method :: cloudi_service_http_rest:method(),
               Path :: cloudi:service_name_pattern(),
               Parameters :: list(string()),
               Format :: atom(),
               RequestInfo :: any(),
               Request :: any(),
               Timeout :: cloudi:timeout_value_milliseconds(),
               Priority :: cloudi:priority_value(),
               TransId :: cloudi:trans_id(),
               State :: any(),
               Dispatcher :: cloudi:dispatcher()) ->
    {reply, Response :: any(), NewState :: any()} |
    {reply, ResponseInfo :: any(), Response :: any(), NewState :: any()} |
    {forward, NextName :: cloudi:service_name(),
     NextRequestInfo :: any(), NextRequest :: any(), NewState :: any()} |
    {forward, NextName :: cloudi:service_name(),
     NextRequestInfo :: any(), NextRequest :: any(),
     NextTimeout :: cloudi:timeout_value_milliseconds(),
     NextPriority :: cloudi:priority_value(), NewState :: any()} |
    {noreply, NewState :: any()} |
    {stop, Reason :: any(), NewState :: any()}).
-define(SPEC_REST_INFO(Function),
-spec Function(Request :: any(),
               State :: any(),
               Dispatcher :: cloudi:dispatcher()) ->
    {noreply, NewState :: any()} |
    {stop, Reason :: any(), NewState :: any()}).
-define(SPEC_REST_TERMINATE(Function),
-spec Function(Reason :: any(),
               Timeout :: cloudi_service_api:
                          timeout_terminate_value_milliseconds(),
               State :: any()) ->
    ok).

