%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2013-2014, Michael Truog <mjtruog at gmail dot com>
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

-behaviour(service).
-behaviour(cloudi_service).

%% cloudi_service callbacks
-export([cloudi_service_init/3,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/2]).

-include_lib("cloudi_core/include/cloudi_service.hrl").
-include_lib("cloudi_core/include/cloudi_service_api.hrl").
-include_lib("service/include/service_req.hrl").

-spec cloudi_service_init(Args :: list(),
                          Prefix :: cloudi_service:service_name_pattern(),
                          Dispatcher :: cloudi_service:dispatcher()) ->
    {'ok', State :: any()} |
    {'stop', Reason :: any()} |
    {'stop', Reason :: any(), State :: any()}.

cloudi_service_init(Args, Prefix, Dispatcher) ->
    service_init(Args, Prefix, Dispatcher).

-spec cloudi_service_handle_request(Type ::
                                        cloudi_service:request_type(),
                                    Name ::
                                        cloudi_service:service_name(),
                                    Pattern ::
                                        cloudi_service:service_name_pattern(),
                                    RequestInfo ::
                                        cloudi_service:request_info(),
                                    Request ::
                                        cloudi_service:request(),
                                    Timeout ::
                                        cloudi_service:timeout_milliseconds(),
                                    Priority ::
                                        cloudi_service:priority(),
                                    TransId ::
                                        cloudi_service:trans_id(),
                                    Pid ::
                                        cloudi_service:source(),
                                    State :: any(),
                                    Dispatcher ::
                                        cloudi_service:dispatcher()) ->
    {'reply', Response :: cloudi_service:response(), NewState :: any()} |
    {'reply', ResponseInfo :: cloudi_service:response_info(),
     Response :: cloudi_service:response(), NewState :: any()} |
    {'forward', NextName :: cloudi_service:service_name(),
     NextRequestInfo :: cloudi_service:request_info(),
     NextRequest :: cloudi_service:request(), NewState :: any()} |
    {'forward', NextName :: cloudi_service:service_name(),
     NextRequestInfo :: cloudi_service:request_info(),
     NextRequest :: cloudi_service:request(),
     NextTimeout :: cloudi_service:timeout_milliseconds(),
     NextPriority :: cloudi_service:priority(), NewState :: any()} |
    {'noreply', NewState :: any()} |
    {'stop', Reason :: any(), NewState :: any()}.

cloudi_service_handle_request(Type, Name, Pattern, RequestInfo, Request,
                              Timeout, Priority, TransId, Pid, State,
                              Dispatcher) ->
    case service_request(#service_req{type = Type,
                                      name = Name,
                                      pattern = Pattern,
                                      request_info = RequestInfo,
                                      request = Request,
                                      timeout = Timeout,
                                      priority = Priority,
                                      trans_id = TransId,
                                      pid = Pid}, State, Dispatcher) of
        {reply, _, _} = Reply ->
            Reply;
        {reply, _, _, _} = Reply ->
            Reply;
        {forward,
         #service_req{name = NextName,
                      request_info = NextRequestInfo,
                      request = NextRequest,
                      timeout = NextTimeout,
                      priority = NextPriority}, NewState} ->
            {forward, NextName, NextRequestInfo, NextRequest,
             NextTimeout, NextPriority, NewState};
        {noreply, _} = NoReply ->
            NoReply;
        {stop, _, _} = Stop ->
            Stop
    end.

-spec cloudi_service_handle_info(Request :: any(),
                                 State :: any(),
                                 Dispatcher :: cloudi_service:dispatcher()) ->
    {'noreply', NewState :: any()} |
    {'stop', Reason :: any(), NewState :: any()}.

cloudi_service_handle_info(Request, State, Dispatcher) ->
    service_info(Request, State, Dispatcher).

-spec cloudi_service_terminate(Reason :: any(),
                               State :: any()) ->
    'ok'.

cloudi_service_terminate(Reason, State) ->
    service_terminate(Reason, State).

