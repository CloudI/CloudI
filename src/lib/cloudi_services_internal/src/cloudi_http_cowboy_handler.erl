%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Cowboy CloudI HTTP Handler==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2012, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2012 Michael Truog
%%% @version 1.1.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_http_cowboy_handler).
-author('mjtruog [at] gmail (dot) com').

%-behaviour(cowboy_http_handler).

%% external interface

%% cowboy_http_handler callbacks
-export([init/3,
         handle/2,
         terminate/2]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include("cloudi_http_cowboy_handler.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cowboy_http_handler
%%%------------------------------------------------------------------------

init({tcp, http}, Req, Opts) ->
    State = #cowboy_state{} = Opts,
    {ok, Req, State}.

handle(Req0, #cowboy_state{job = Job,
                           output_type = OutputType,
                           use_host_prefix = UseHostPrefix,
                           use_method_suffix = UseMethodSuffix} = State) ->
    RequestStartMicroSec = uuid:get_v1_time(os),
    {Method, Req1} = cowboy_http_req:method(Req0),
    {HeadersIncoming, Req2} = cowboy_http_req:headers(Req1),
    {PathRaw, Req3} = cowboy_http_req:raw_path(Req2),
    {HostRaw, Req4} = cowboy_http_req:raw_host(Req3),
    {QsVals, Req5} = cowboy_http_req:qs_vals(Req4),
    {ok, Body, ReqN} = cowboy_http_req:body(Req5),
    NameIncoming = if
        UseHostPrefix =:= false; HostRaw =:= undefined ->
            erlang:binary_to_list(PathRaw);
        true ->
            erlang:binary_to_list(<<HostRaw/binary, PathRaw/binary>>)
    end,
    NameOutgoing = if
        UseMethodSuffix =:= false ->
            NameIncoming;
        Method =:= 'GET' ->
            NameIncoming ++ "/get";
        Method =:= 'POST' ->
            NameIncoming ++ "/post";
        Method =:= 'PUT' ->
            NameIncoming ++ "/put";
        Method =:= 'DELETE' ->
            NameIncoming ++ "/delete";
        Method =:= 'HEAD' ->
            NameIncoming ++ "/head";
        Method =:= 'TRACE' ->
            NameIncoming ++ "/trace";
        Method =:= 'OPTIONS' ->
            NameIncoming ++ "/options"%;
        %Method =:= 'CONNECT' ->
        %    NameIncoming ++ "/connect"
    end,
    RequestBinary = if
        Method =:= 'GET' ->
            if
                QsVals =:= [] ->
                    <<>>;
                true ->
                    erlang:iolist_to_binary(lists:foldr(fun({K, V}, L) ->
                        if
                            V =:= true ->
                                [K, 0, <<"true">>, 0 | L];
                            V =:= false ->
                                [K, 0, <<"false">>, 0 | L];
                            true ->
                                [K, 0, V, 0 | L]
                        end
                    end, [], QsVals))
            end;
        Method =:= 'POST'; Method =:= 'PUT' ->
            % do not pass type information along with the request!
            % make sure to encourage good design that provides
            % one type per name (path)
            case header_content_type(HeadersIncoming) of
                <<"application/zip">> ->
                    zlib:unzip(Body);
                _ ->
                    Body
            end
    end,
    Request = if
        OutputType =:= list ->
            erlang:binary_to_list(RequestBinary);
        OutputType =:= internal; OutputType =:= external;
        OutputType =:= binary ->
            RequestBinary
    end,
    RequestInfo = if
        OutputType =:= internal; OutputType =:= list ->
            HeadersIncoming;
        OutputType =:= external; OutputType =:= binary ->
            headers_external(HeadersIncoming)
    end,
    Job ! {http_request,
           #request_state{name_incoming = NameIncoming,
                          name_outgoing = NameOutgoing,
                          request_info = RequestInfo,
                          request = Request,
                          request_pid = self()}},
    receive
        {ok, HttpCode, HeadersOutgoing, ResponseBinary} ->
            ?LOG_TRACE("~w ~s ~s (to ~s) ~p ms",
                       [HttpCode, Method, NameIncoming, NameOutgoing,
                        (uuid:get_v1_time(os) -
                         RequestStartMicroSec) / 1000.0]),
            {ok, Req} = cowboy_http_req:reply(HttpCode,
                                              HeadersOutgoing,
                                              ResponseBinary,
                                              ReqN),
            {ok, Req, State};
        {error, HttpCode, Reason} ->
            ?LOG_WARN("~w ~s ~s ~p ms: ~p",
                      [HttpCode, Method, NameIncoming,
                       (uuid:get_v1_time(os) -
                        RequestStartMicroSec) / 1000.0, Reason]),
            {ok, Req} = cowboy_http_req:reply(HttpCode,
                                              ReqN),
            {ok, Req, State}
    end.

terminate(_Req, _State) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

header_content_type(Headers) ->
    case lists:keyfind('Content-Type', 1, Headers) of
        false ->
            <<>>;
        {'Content-Type', Value} ->
            hd(binary:split(Value, <<",">>))
    end.

% format for external jobs, http headers passed as key-value pairs
headers_external(L) ->
    erlang:iolist_to_binary(lists:reverse(headers_external([], L))).

headers_external(Result, []) ->
    Result;
headers_external(Result, [{K, V} | L]) when is_atom(K) ->
    headers_external([[erlang:atom_to_binary(K, utf8), 0, V, 0] | Result], L);
headers_external(Result, [{K, V} | L]) when is_binary(K) ->
    headers_external([[K, 0, V, 0] | Result], L).

