%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2015-2017, Michael Truog <mjtruog at gmail dot com>
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

