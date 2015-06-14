%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Service Futures==
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

-module(cloudi_service_future).
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

-type value() :: fun(() ->
                     {ok, cloudi_service:response_info(),
                          cloudi_service:response()} |
                     {ok, cloudi_service:response()} |
                     {error, cloudi_service:error_reason_sync()}).
-export_type([value/0]).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a future.===
%% @end
%%-------------------------------------------------------------------------

-spec new(Dispatcher :: cloudi_service:dispatcher(),
          Name :: cloudi_service:service_name(),
          Request :: cloudi_service:request()) ->
    value().

new(Dispatcher, Name, Request) ->
    new(Dispatcher, Name, <<>>, Request,
        undefined, undefined, undefined).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a future.===
%% @end
%%-------------------------------------------------------------------------

-spec new(Dispatcher :: cloudi_service:dispatcher(),
          Name :: cloudi_service:service_name(),
          Request :: cloudi_service:request(),
          Timeout :: cloudi_service:timeout_milliseconds()) ->
    value().

new(Dispatcher, Name, Request, Timeout) ->
    new(Dispatcher, Name, <<>>, Request,
        Timeout, undefined, undefined).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a future.===
%% @end
%%-------------------------------------------------------------------------

-spec new(Dispatcher :: cloudi_service:dispatcher(),
          Name :: cloudi_service:service_name(),
          Request :: cloudi_service:request(),
          Timeout :: cloudi_service:timeout_milliseconds(),
          PatternPid :: cloudi_service:pattern_pid()) ->
    value().

new(Dispatcher, Name, Request, Timeout, PatternPid) ->
    new(Dispatcher, Name, <<>>, Request,
        Timeout, undefined, PatternPid).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a future.===
%% @end
%%-------------------------------------------------------------------------

-spec new(Dispatcher :: cloudi_service:dispatcher(),
          Name :: cloudi_service:service_name(),
          RequestInfo :: cloudi_service:request_info(),
          Request :: cloudi_service:request(),
          Timeout :: cloudi_service:timeout_milliseconds(),
          Priority :: cloudi_service:priority()) ->
    value().

new(Dispatcher, Name, RequestInfo, Request, Timeout, Priority) ->
    new(Dispatcher, Name, RequestInfo, Request,
        Timeout, Priority, undefined).

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a future.===
%% @end
%%-------------------------------------------------------------------------

-spec new(Dispatcher :: cloudi_service:dispatcher(),
          Name :: cloudi_service:service_name(),
          RequestInfo :: cloudi_service:request_info(),
          Request :: cloudi_service:request(),
          Timeout :: cloudi_service:timeout_milliseconds(),
          Priority :: cloudi_service:priority(),
          PatternPid :: cloudi_service:pattern_pid() | undefined) ->
    value().

new(Dispatcher, Name, RequestInfo, Request, Timeout, Priority, PatternPid) ->
    case cloudi_service:send_async(Dispatcher, Name, RequestInfo, Request,
                                   Timeout, Priority, PatternPid) of
        {ok, TransId} ->
            fun() ->
                case cloudi_service:recv_async(Dispatcher, Timeout, TransId) of
                    {ok, <<>>, Response, TransId} ->
                        {ok, Response};
                    {ok, ResponseInfo, Response, TransId} ->
                        {ok, ResponseInfo, Response};
                    {error, _} = Error ->
                        Error
                end
            end;
        {error, _} = Error ->
            fun() ->
                Error
            end
    end.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

