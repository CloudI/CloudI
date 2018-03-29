%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Service Futures==
%%% @end
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
%%% @author Michael Truog <mjtruog at protonmail dot com>
%%% @copyright 2015-2017 Michael Truog
%%% @version 1.7.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_future).
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

