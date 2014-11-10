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
%%% @version 1.4.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_http_elli_handler).
-author('mjtruog [at] gmail (dot) com').

%-behaviour(elli_handler).

%% external interface

%% elli_http_handler callbacks
-export([handle/2,
         handle_event/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include_lib("cloudi_core/include/cloudi_service_children.hrl").
-include("cloudi_http_elli_handler.hrl").
-include_lib("cloudi_x_elli/include/cloudi_x_elli.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from elli_handler
%%%------------------------------------------------------------------------

%% Reply with a normal response. 'ok' can be used instead of '200'
%%     to signal success.
handle(Req, #elli_state{dispatcher = Dispatcher,
                        context = Context,
                        output_type = OutputType,
                        default_content_type = DefaultContentType,
                        use_host_prefix = UseHostPrefix,
                        use_method_suffix = UseMethodSuffix}) ->
    RequestStartMicroSec = ?LOG_WARN_APPLY(fun request_time_start/0, []),
    Method = cloudi_x_elli_request:method(Req),
    HeadersIncoming0 = cloudi_x_elli_request:headers(Req),
    [PathRaw | _] = binary:split(cloudi_x_elli_request:raw_path(Req),
                                 [<<"?">>]),
    HostRaw = case lists:keyfind(<<"Host">>, 1, HeadersIncoming0) of
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
    HeadersIncomingN = [{<<"url-path">>, PathRaw} |
                        HeadersIncoming0],
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
            NameIncoming ++ "/options";
        is_binary(Method) ->
            NameIncoming ++
            [$/ | string:to_lower(erlang:binary_to_list(Method))]
    end,
    RequestBinary = if
        Method =:= 'GET' ->
            if
                (OutputType =:= external) orelse
                (OutputType =:= binary) orelse (OutputType =:= list) ->
                    get_query_string_external(cloudi_x_cow_qs:parse_qs(QsVals));
                OutputType =:= internal ->
                    cloudi_x_cow_qs:parse_qs(QsVals)
            end;
        Method =:= 'POST'; Method =:= 'PUT' ->
            % do not pass type information along with the request!
            % make sure to encourage good design that provides
            % one type per name (path)
            case header_content_type(HeadersIncomingN) of
                <<"application/zip">> ->
                    zlib:unzip(Body);
                _ ->
                    Body
            end;
        true ->
            <<>>
    end,
    RequestInfo = if
        (OutputType =:= external) orelse (OutputType =:= binary) ->
            headers_external_incoming(HeadersIncomingN);
        (OutputType =:= internal) orelse (OutputType =:= list) ->
            HeadersIncomingN
    end,
    Request = if
        (OutputType =:= external) orelse (OutputType =:= internal) orelse
        (OutputType =:= binary) ->
            RequestBinary;
        OutputType =:= list ->
            erlang:binary_to_list(RequestBinary)
    end,
    case send_sync_minimal(Dispatcher, Context,
                           NameOutgoing, RequestInfo, Request, self()) of
        {ok, ResponseInfo, Response} ->
            HeadersOutgoing = headers_external_outgoing(ResponseInfo),
            {HttpCode, _, _} = Result =
                return_response(NameIncoming, HeadersOutgoing, Response,
                                OutputType, DefaultContentType),
            ?LOG_TRACE_APPLY(fun request_time_end_success/5,
                             [HttpCode, Method, NameIncoming, NameOutgoing,
                              RequestStartMicroSec]),
            Result;
        {error, timeout} ->
            HttpCode = 504,
            ?LOG_WARN_APPLY(fun request_time_end_error/5,
                            [HttpCode, Method, NameIncoming,
                             RequestStartMicroSec, timeout]),
            {HttpCode, [], <<>>}
    end.

%% elli_startup is sent when Elli is starting up. If you are
%% implementing a middleware, you can use it to spawn processes,
%% create ETS tables or start supervised processes in a supervisor
%% tree.
handle_event(elli_startup, [], _) -> ok;
%% request_complete fires *after* Elli has sent the response to the
%% client. Timings contains timestamps of events like when the
%% connection was accepted, when request parsing finished, when the
%% user callback returns, etc. This allows you to collect performance
%% statistics for monitoring your app.
handle_event(request_complete, [_Request,
                                _ResponseCode, _ResponseHeaders, _ResponseBody,
                                _Timings], _) -> ok;
%% request_throw, request_error and request_exit events are sent if
%% the user callback code throws an exception, has an error or
%% exits. After triggering this event, a generated response is sent to
%% the user.
handle_event(request_throw, [_Request, _Exception, _Stacktrace] = Error, _) ->
    ?LOG_ERROR("request_throw: ~p", [Error]),
    ok;
handle_event(request_error, [_Request, _Exception, _Stacktrace] = Error, _) ->
    ?LOG_ERROR("request_error: ~p", [Error]),
    ok;
handle_event(request_exit, [_Request, _Exception, _Stacktrace] = Error, _) ->
    ?LOG_ERROR("request_exit: ~p", [Error]),
    ok;
%% invalid_return is sent if the user callback code returns a term not
%% understood by elli, see elli_http:execute_callback/1.
%% After triggering this event, a generated response is sent to the user.
handle_event(invalid_return, [_Request, _ReturnValue], _) -> ok;
%% chunk_complete fires when a chunked response is completely
%% sent. It's identical to the request_complete event, except instead
%% of the response body you get the atom "client" or "server"
%% depending on who closed the connection.
handle_event(chunk_complete, [_Request,
                              _ResponseCode, _ResponseHeaders, _ClosingEnd,
                              _Timings], _) -> ok;
%% request_closed is sent if the client closes the connection when
%% Elli is waiting for the next request on a keep alive connection.
handle_event(request_closed, [], _) -> ok;
%% request_timeout is sent if the client times out when
%% Elli is waiting for the request.
handle_event(request_timeout, [], _) -> ok;
%% request_parse_error fires if the request is invalid and cannot be
%% parsed by erlang:decode_packet/3 or it contains a path Elli cannot
%% parse or does not support.
handle_event(request_parse_error, [_], _) -> ok;
%% client_closed can be sent from multiple parts of the request
%% handling. It's sent when the client closes the connection or if for
%% any reason the socket is closed unexpectedly. The "Where" atom
%% tells you in which part of the request processing the closed socket
%% was detected: receiving_headers, receiving_body, before_response
handle_event(client_closed, [_Where], _) -> ok;
%% client_timeout can as with client_closed be sent from multiple
%% parts of the request handling. If Elli tries to receive data from
%% the client socket and does not receive anything within a timeout,
%% this event fires and the socket is closed.
handle_event(client_timeout, [_Where], _) -> ok;
%% bad_request is sent when Elli detects a request is not well
%% formatted or does not conform to the configured limits. Currently
%% the Reason variable can be any of the following: {too_many_headers,
%% Headers}, {body_size, ContentLength}
handle_event(bad_request, [_Reason], _) -> ok;
%% file_error is sent when the user wants to return a file as a
%% response, but for some reason it cannot be opened.
handle_event(file_error, [_ErrorReason], _) -> ok.

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
    erlang:iolist_to_binary(headers_external_incoming_text(L)).

headers_external_incoming_text([]) ->
    [];
headers_external_incoming_text([{K, V} | L]) when is_binary(K) ->
    [[K, 0, V, 0] | headers_external_incoming_text(L)].

headers_external_outgoing(<<>>) ->
    [];
headers_external_outgoing([] = ResponseInfo) ->
    ResponseInfo;
headers_external_outgoing([{_, _} | _] = ResponseInfo) ->
    ResponseInfo;
headers_external_outgoing(ResponseInfo)
    when is_binary(ResponseInfo) ->
    headers_external_outgoing_text(binary:split(ResponseInfo, <<0>>, [global])).

headers_external_outgoing_text([<<>>]) ->
    [];
headers_external_outgoing_text([K, V | L]) ->
    [{K, V} | headers_external_outgoing_text(L)].

get_query_string_external([]) ->
    <<>>;
get_query_string_external(QsVals) ->
    erlang:iolist_to_binary(get_query_string_external_text(QsVals)).

get_query_string_external_text([]) ->
    [];
get_query_string_external_text([{K, V} | L]) ->
    if
        V =:= true ->
            [[K, 0, <<"true">>, 0] | get_query_string_external_text(L)];
        is_binary(V) ->
            [[K, 0, V, 0] | get_query_string_external_text(L)]
    end.

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
                OutputType, DefaultContentType) ->
    ResponseBinary = if
        (((OutputType =:= external) orelse
          (OutputType =:= internal)) andalso
         (is_binary(Response) orelse is_list(Response))) orelse
        ((OutputType =:= binary) andalso
         is_binary(Response)) ->
            Response;
        (OutputType =:= list) andalso is_list(Response) ->
            erlang:list_to_binary(Response)
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
                    case cloudi_response_info:
                         lookup_content_type(binary, Extension) of
                        error ->
                            [{<<"content-disposition">>,
                              erlang:iolist_to_binary([
                                  "attachment; filename=\"",
                                  filename:basename(NameIncoming), "\""])},
                             {<<"content-type">>,
                              <<"application/octet-stream">>}];
                        {ok, {request, ContentType}} ->
                            [{<<"content-type">>, ContentType}];
                        {ok, {attachment, ContentType}} ->
                            [{<<"content-disposition">>,
                              erlang:iolist_to_binary([
                                  "attachment; filename=\"",
                                  filename:basename(NameIncoming), "\""])},
                             {<<"content-type">>, ContentType}]
                    end
            end
    end,
    HttpCode = 200,
    {HttpCode,
     ResponseHeadersOutgoing,
     ResponseBinary}.

