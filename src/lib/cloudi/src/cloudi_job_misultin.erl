%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Cloudi Misultin Integration==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2011, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011 Michael Truog
%%% @version 0.1.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_job_misultin).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_job).

%% external interface

%% cloudi_job callbacks
-export([cloudi_job_init/2,
         cloudi_job_handle_request/8,
         cloudi_job_handle_info/3,
         cloudi_job_terminate/2]).

-include("cloudi_logger.hrl").

-define(DEFAULT_PORT,                   8080).
-define(DEFAULT_BACKLOG,                 128).
-define(DEFAULT_RECV_TIMEOUT,      30 * 1000). % milliseconds
-define(DEFAULT_SSL,                   false).
-define(DEFAULT_COMPRESS,              false).
-define(DEFAULT_WS_AUTOEXIT,            true).

-record(state,
    {
        process
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_job
%%%------------------------------------------------------------------------

cloudi_job_init(Args, Dispatcher) ->
    Defaults = [
        {port,            ?DEFAULT_PORT},
        {backlog,         ?DEFAULT_BACKLOG},
        {recv_timeout,    ?DEFAULT_RECV_TIMEOUT},
        {ssl,             ?DEFAULT_SSL},
        {compress,        ?DEFAULT_COMPRESS},
        {ws_autoexit,     ?DEFAULT_WS_AUTOEXIT}],
    [Port, Backlog, RecvTimeout, SSL, Compress, WsAutoExit, []] =
        proplists2:take_values(Defaults, Args),
    Loop = fun(HttpRequest) ->
        handle_http(HttpRequest, Dispatcher)
    end,
    case misultin:start_link([{port, Port},
                              {backlog, Backlog},
                              {recv_timeout, RecvTimeout},
                              {ssl, SSL},
                              {compress, Compress},
                              {ws_autoexit, WsAutoExit},
                              {loop, Loop}]) of
        {ok, Process} ->
            {ok, #state{process = Process}};
        {error, Reason} ->
            {stop, Reason}
    end.

cloudi_job_handle_request(_Type, _Name, _Request, _Timeout, _TransId, _Pid,
                          State, _Dispatcher) ->
    {reply, <<>>, State}.

cloudi_job_handle_info(Request, State, _) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_job_terminate(_, #state{process = Process}) ->
    misultin:stop(Process),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

content_type(Headers) ->
    case misultin_utility:get_key_value('Content-Type', Headers) of
        undefined ->
            "";
        ContentType ->
            case string2:beforel($;, ContentType) of
                [] ->
                    ContentType;
                L ->
                    L
            end
    end.

handle_http(HttpRequest, Dispatcher) ->
    Name = HttpRequest:get(str_uri),
    Request = case HttpRequest:get(method) of
        'GET' ->
            erlang:list_to_binary(HttpRequest:get(args));
        'POST' ->
            % do not pass type information along with the request!
            % make sure to encourage good design that provides
            % one type per name (path),
            % though multiple names may lead to the same callback
            % (i.e., the name can be checked in the callback if different
            %  types must be handled in the same area of code)
            Type = content_type(HttpRequest:get(headers)),
            if
                Type == "application/zip" ->
                    zlib:unzip(HttpRequest:get(body));
                true ->
                    HttpRequest:get(body)
            end
    end,
    case cloudi_job:send_sync(Dispatcher, Name, Request) of
        {ok, Response} ->
            HttpRequest:raw_headers_respond(Response);
        {error, timeout} ->
            HttpRequest:respond(504);
        {error, Reason} ->
            ?LOG_ERROR("Request Failed: ~p", [Reason]),
            HttpRequest:respond(500)
    end.

