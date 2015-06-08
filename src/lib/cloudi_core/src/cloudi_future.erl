%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Futures==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2015, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2015 Michael Truog
%%% @version 1.5.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_future).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([new/3,
         new/4,
         new/5,
         new/6,
         new/7]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-type value() :: fun((cloudi:agent()) ->
                     {{ok, cloudi:response_info(),
                           cloudi:response()} |
                      {ok, cloudi:response()} |
                      {error, cloudi:error_reason_sync()}, cloudi:agent()}).
-export_type([value/0]).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a future.===
%% @end
%%-------------------------------------------------------------------------

-spec new(Agent :: cloudi:agent(),
          Name :: cloudi:service_name(),
          Request :: cloudi:request()) ->
    {value(), cloudi:agent()}.

new(Agent, Name, Request) ->
    new(Agent, Name, <<>>, Request,
        undefined, undefined, undefined).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a future.===
%% @end
%%-------------------------------------------------------------------------

-spec new(Agent :: cloudi:agent(),
          Name :: cloudi:service_name(),
          Request :: cloudi:request(),
          Timeout :: cloudi:timeout_milliseconds()) ->
    {value(), cloudi:agent()}.

new(Agent, Name, Request, Timeout) ->
    new(Agent, Name, <<>>, Request,
        Timeout, undefined, undefined).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a future.===
%% @end
%%-------------------------------------------------------------------------

-spec new(Agent :: cloudi:agent(),
          Name :: cloudi:service_name(),
          Request :: cloudi:request(),
          Timeout :: cloudi:timeout_milliseconds(),
          PatternPid :: cloudi:pattern_pid()) ->
    {value(), cloudi:agent()}.

new(Agent, Name, Request, Timeout, PatternPid) ->
    new(Agent, Name, <<>>, Request,
        Timeout, undefined, PatternPid).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a future.===
%% @end
%%-------------------------------------------------------------------------

-spec new(Agent :: cloudi:agent(),
          Name :: cloudi:service_name(),
          RequestInfo :: cloudi:request_info(),
          Request :: cloudi:request(),
          Timeout :: cloudi:timeout_milliseconds(),
          Priority :: cloudi:priority()) ->
    {value(), cloudi:agent()}.

new(Agent, Name, RequestInfo, Request, Timeout, Priority) ->
    new(Agent, Name, RequestInfo, Request,
        Timeout, Priority, undefined).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a future.===
%% @end
%%-------------------------------------------------------------------------

-spec new(Agent :: cloudi:agent(),
          Name :: cloudi:service_name(),
          RequestInfo :: cloudi:request_info(),
          Request :: cloudi:request(),
          Timeout :: cloudi:timeout_milliseconds(),
          Priority :: cloudi:priority(),
          PatternPid :: cloudi:pattern_pid() | undefined) ->
    {value(), cloudi:agent()}.

new(Agent, Name, RequestInfo, Request, Timeout, Priority, PatternPid) ->
    case cloudi:send_async(Agent, Name, RequestInfo, Request,
                           Timeout, Priority, PatternPid) of
        {{ok, TransId}, NewAgent} ->
            {fun(AgentArg) ->
                case cloudi:recv_async(AgentArg, Timeout, TransId) of
                    {{ok, <<>>, Response, TransId}, NewAgentArg} ->
                        {{ok, Response}, NewAgentArg};
                    {{ok, ResponseInfo, Response, TransId}, NewAgentArg} ->
                        {{ok, ResponseInfo, Response}, NewAgentArg};
                    {{error, _}, _} = Error ->
                        Error
                end
             end, NewAgent};
        {{error, _} = Error, NewAgent} ->
            {fun(AgentArg) ->
                {Error, AgentArg}
             end, NewAgent}
    end.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

