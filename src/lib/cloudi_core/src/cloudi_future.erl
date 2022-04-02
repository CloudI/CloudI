%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Futures==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2015-2022 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2015-2022 Michael Truog
%%% @version 2.0.5 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_future).
-author('mjtruog at protonmail dot com').

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
                      {error, cloudi:error_reason()}, cloudi:agent()}).
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
          Timeout :: cloudi:timeout_period()) ->
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
          Timeout :: cloudi:timeout_period(),
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
          Timeout :: cloudi:timeout_period(),
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
          Timeout :: cloudi:timeout_period(),
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

