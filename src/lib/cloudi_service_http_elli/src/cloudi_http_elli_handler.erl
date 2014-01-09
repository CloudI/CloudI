%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Elli CloudI HTTP Handler==
%%% @end
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
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2013-2014 Michael Truog
%%% @version 1.3.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_http_elli_handler).
-author('mjtruog [at] gmail (dot) com').

%-behaviour(elli_handler).

%% external interface

%% elli_http_handler callbacks
-export([handle/2,
         handle_event/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include("cloudi_http_elli_handler.hrl").
-include_lib("elli/include/cloudi_x_elli.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from elli_handler
%%%------------------------------------------------------------------------

%% Reply with a normal response. 'ok' can be used instead of '200'
%%     to signal success.
handle(Req, #elli_state{dispatcher = Dispatcher,
                        output_type = OutputType,
                        default_content_type = DefaultContentType,
                        use_host_prefix = UseHostPrefix,
                        use_method_suffix = UseMethodSuffix,
                        content_type_lookup = ContentTypeLookup}) ->
    RequestStartMicroSec = ?LOG_WARN_APPLY(fun request_time_start/0, []),
    Method = cloudi_x_elli_request:method(Req),
    HeadersIncoming = cloudi_x_elli_request:headers(Req),
    [PathRaw | _] = binary:split(cloudi_x_elli_request:raw_path(Req),
                                 [<<"?">>]),
    HostRaw = case lists:keyfind(<<"Host">>, 1, HeadersIncoming) of
        {<<"Host">>, H} ->
            H;
        false ->
            undefined
    end,
    QsVals = cloudi_x_elli_request:query_str(Req),
    Body = cloudi_x_elli_request:body(Req),
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
                QsVals =:= <<>> ->
                    <<>>;
                true ->
                    erlang:iolist_to_binary(lists:foldr(fun({K, V}, L) ->
                        if
                            V =:= true ->
                                [K, 0, <<"true">>, 0 | L];
                            V =:= false ->
                                [K, 0, <<"false">>, 0 | L];
                            is_binary(V) ->
                                [K, 0, V, 0 | L]
                        end
                    end, [],
                    cloudi_x_cow_qs:parse_qs(QsVals)))
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
            end;
        true ->
            <<>>
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
            headers_external_incoming(HeadersIncoming)
    end,
    Context = cloudi:new([{groups_static, true} |
                          cloudi_service:context_options(Dispatcher)]),
    case cloudi:send_sync(Context, NameOutgoing, RequestInfo, Request,
                          undefined, undefined) of
        {ok, ResponseInfo, Response} ->
            HeadersOutgoing = if
                OutputType =:= internal; OutputType =:= list ->
                    ResponseInfo;
                OutputType =:= external; OutputType =:= binary ->
                    headers_external_outgoing(ResponseInfo)
            end,
            {HttpCode, _, _} = Result =
                return_response(NameIncoming, HeadersOutgoing, Response,
                                OutputType, DefaultContentType,
                                ContentTypeLookup),
            ?LOG_TRACE_APPLY(fun request_time_end_success/5,
                             [HttpCode, Method, NameIncoming, NameOutgoing,
                              RequestStartMicroSec]),
            Result;
        {ok, Response} ->
            {HttpCode, _, _} = Result =
                return_response(NameIncoming, [], Response,
                                OutputType, DefaultContentType,
                                ContentTypeLookup),
            ?LOG_TRACE_APPLY(fun request_time_end_success/5,
                             [HttpCode, Method, NameIncoming, NameOutgoing,
                              RequestStartMicroSec]),
            Result;
        {error, timeout} ->
            HttpCode = 504,
            ?LOG_WARN_APPLY(fun request_time_end_error/5,
                            [HttpCode, Method, NameIncoming,
                             RequestStartMicroSec, timeout]),
            {HttpCode, [], <<>>};
        {error, Reason} ->
            HttpCode = 500,
            ?LOG_WARN_APPLY(fun request_time_end_error/5,
                            [HttpCode, Method, NameIncoming,
                             RequestStartMicroSec, Reason]),
            {HttpCode, [], <<>>}
    end.

%% Handle request events, like request completed, exception
%%     thrown, client timeout, etc. Must return 'ok'.
handle_event(request_throw, Error, _Config) ->
    ?LOG_ERROR("request_throw: ~p", [Error]),
    ok;
handle_event(request_error, Error, _Config) ->
    ?LOG_ERROR("request_error: ~p", [Error]),
    ok;
handle_event(request_exit, Error, _Config) ->
    ?LOG_ERROR("request_exit: ~p", [Error]),
    ok;
handle_event(_Event, _Args, _Config) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

header_content_type(Headers) ->
    case lists:keyfind(<<"Content-Type">>, 1, Headers) of
        false ->
            <<>>;
        {<<"Content-Type">>, Value} ->
            hd(binary:split(Value, <<",">>))
    end.

% format for external services, http headers passed as key-value pairs
headers_external_incoming(L) ->
    erlang:iolist_to_binary(lists:reverse(headers_external_incoming([], L))).

headers_external_incoming(Result, []) ->
    Result;
headers_external_incoming(Result, [{K, V} | L]) when is_binary(K) ->
    headers_external_incoming([[K, 0, V, 0] | Result], L).

headers_external_outgoing(<<>>) ->
    [];
headers_external_outgoing(ResponseInfo) ->
    Options = case binary:last(ResponseInfo) of
        0 ->
            [global, {scope, {0, erlang:byte_size(ResponseInfo) - 1}}];
        _ ->
            [global]
    end,
    headers_external_outgoing([], binary:split(ResponseInfo, <<0>>, Options)).

headers_external_outgoing(Result, []) ->
    Result;
headers_external_outgoing(Result, [K, V | L]) ->
    headers_external_outgoing([{K, V} | Result], L).

request_time_start() ->
    cloudi_x_uuid:get_v1_time(os).

request_time_end_success(HttpCode, Method, NameIncoming, NameOutgoing,
                         RequestStartMicroSec) ->
    ?LOG_TRACE("~w ~s ~s (to ~s) ~p ms",
               [HttpCode, Method, NameIncoming, NameOutgoing,
                (cloudi_x_uuid:get_v1_time(os) -
                 RequestStartMicroSec) / 1000.0]).

request_time_end_error(HttpCode, Method, NameIncoming,
                       RequestStartMicroSec, Reason) ->
    ?LOG_WARN("~w ~s ~s ~p ms: ~p",
              [HttpCode, Method, NameIncoming,
               (cloudi_x_uuid:get_v1_time(os) -
                RequestStartMicroSec) / 1000.0, Reason]).

return_response(NameIncoming, HeadersOutgoing, Response,
                OutputType, DefaultContentType,
                ContentTypeLookup) ->
    ResponseBinary = if
        OutputType =:= list, is_list(Response) ->
            erlang:list_to_binary(Response);
        OutputType =:= internal; OutputType =:= external;
        OutputType =:= binary; is_binary(Response) ->
            Response
    end,
    FileName = cloudi_string:afterr($/, NameIncoming, input),
    ResponseHeadersOutgoing = if
        HeadersOutgoing =/= [] ->
            HeadersOutgoing;
        DefaultContentType =/= undefined ->
            [{<<"content-type">>, DefaultContentType}];
        true ->
            Extension = filename:extension(FileName),
            if
                Extension == [] ->
                    [{<<"content-type">>, <<"text/html">>}];
                true ->
                    case cloudi_x_trie:find(Extension, ContentTypeLookup) of
                        error ->
                            [{<<"content-disposition">>,
                              erlang:list_to_binary("attachment; filename=\"" ++
                                                    NameIncoming ++ "\"")},
                             {<<"content-type">>,
                              <<"application/octet-stream">>}];
                        {ok, {request, ContentType}} ->
                            [{<<"content-type">>, ContentType}];
                        {ok, {attachment, ContentType}} ->
                            [{<<"content-disposition">>,
                              erlang:list_to_binary("attachment; filename=\"" ++
                                                    NameIncoming ++ "\"")},
                             {<<"content-type">>, ContentType}]
                    end
            end
    end,
    HttpCode = 200,
    {HttpCode,
     ResponseHeadersOutgoing,
     ResponseBinary}.

