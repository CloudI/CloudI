%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Elli CloudI HTTP Handler==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2013-2015, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2013-2015 Michael Truog
%%% @version 1.5.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_http_elli_handler).
-author('mjtruog [at] gmail (dot) com').

%-behaviour(cloudi_x_elli_handler).

%% external interface

%% cloudi_x_elli_handler callbacks
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

handle(Req,
       #elli_state{output_type = OutputType,
                   content_type_forced = ContentTypeForced,
                   content_types_accepted = ContentTypesAccepted,
                   set_x_forwarded_for = SetXForwardedFor,
                   status_code_timeout = StatusCodeTimeout,
                   use_host_prefix = UseHostPrefix,
                   use_client_ip_prefix = UseClientIpPrefix,
                   use_method_suffix = UseMethodSuffix} = State) ->
    RequestStartMicroSec = ?LOG_WARN_APPLY(fun request_time_start/0, []),
    HeadersIncoming0 = headers_to_lower(cloudi_x_elli_request:headers(Req)),
    Method = cloudi_x_elli_request:method(Req),
    QsVals = cloudi_x_elli_request:query_str(Req),
    [PathRaw | _] = binary:split(cloudi_x_elli_request:raw_path(Req),
                                 [<<"?">>]),
    {ClientIpAddr, ClientPort} = Client = peer(Req),
    NameIncoming = service_name_incoming(UseClientIpPrefix,
                                         UseHostPrefix,
                                         PathRaw,
                                         Client,
                                         HeadersIncoming0),
    RequestAccepted = if
        ContentTypesAccepted =:= undefined ->
            true;
        true ->
            header_accept_check(HeadersIncoming0, ContentTypesAccepted)
    end,
    if
        RequestAccepted =:= false ->
            HttpCode = 406,
            ?LOG_WARN_APPLY(fun request_time_end_error/5,
                            [HttpCode, Method, NameIncoming,
                             RequestStartMicroSec, not_acceptable]),
            {HttpCode, [], <<>>};
        RequestAccepted =:= true ->
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
            PeerShort = erlang:list_to_binary(inet_parse:ntoa(ClientIpAddr)),
            PeerLong = cloudi_ip_address:to_binary(ClientIpAddr),
            PeerPort = erlang:integer_to_binary(ClientPort),
            HeadersIncoming1 = [{<<"peer">>, PeerShort},
                                {<<"peer-port">>, PeerPort},
                                {<<"source-address">>, PeerLong},
                                {<<"source-port">>, PeerPort},
                                {<<"url-path">>, PathRaw} |
                                HeadersIncoming0],
            HeadersIncomingN = if
                SetXForwardedFor =:= true ->
                    case lists:keyfind(<<"x-forwarded-for">>, 1,
                                       HeadersIncoming0) of
                        false ->
                            [{<<"x-forwarded-for">>, PeerShort} |
                             HeadersIncoming1];
                        _ ->
                            HeadersIncoming1

                    end;
                SetXForwardedFor =:= false ->
                    HeadersIncoming1
            end,
            Body = if
                Method =:= 'GET' ->
                    ParsedQsVals = cloudi_x_cow_qs:parse_qs(QsVals),
                    if
                        (OutputType =:= external) orelse
                        (OutputType =:= binary) orelse (OutputType =:= list) ->
                            get_query_string_external(ParsedQsVals);
                        OutputType =:= internal ->
                            ParsedQsVals
                    end;
                Method =:= 'POST'; Method =:= 'PUT' ->
                    % do not pass type information along with the request!
                    % make sure to encourage good design that provides
                    % one type per name (path)
                    case header_content_type(HeadersIncomingN) of
                        <<"application/zip">> ->
                            'application_zip';
                        _ ->
                            'normal'
                    end;
                (Method =:= 'DELETE') orelse
                (Method =:= 'HEAD') orelse
                (Method =:= 'OPTIONS') orelse
                (Method =:= 'TRACE') orelse
                (Method == <<"CONNECT">>) ->
                    <<>>;
                true ->
                    'normal'
            end,
            case handle_request(NameOutgoing, HeadersIncomingN,
                                Body, Req, State) of
                {elli_response, HeadersOutgoing, Response} ->
                    {HttpCode, _,
                     _} = Result = handle_response(NameIncoming,
                                                   HeadersOutgoing,
                                                   Response, Req, OutputType,
                                                   ContentTypeForced),
                    ?LOG_TRACE_APPLY(fun request_time_end_success/5,
                                     [HttpCode, Method,
                                      NameIncoming, NameOutgoing,
                                      RequestStartMicroSec]),
                    Result;
                {elli_error, timeout} ->
                    HttpCode = StatusCodeTimeout,
                    Result = if
                        HttpCode =:= 405 ->
                            % currently not providing a list of valid methods
                            % (a different HTTP status code is a better
                            %  choice, since this service name may not exist)
                            HeadersOutgoing = [{<<"allow">>, <<"">>}],
                            {HttpCode, HeadersOutgoing, <<>>};
                        true ->
                            {HttpCode, [], <<>>}
                    end,
                    ?LOG_WARN_APPLY(fun request_time_end_error/5,
                                    [HttpCode, Method, NameIncoming,
                                     RequestStartMicroSec, timeout]),
                    Result
            end
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

headers_to_lower([] = L) ->
    L;
headers_to_lower([{K, V} | Headers]) ->
    [{to_lower(K), V} | headers_to_lower(Headers)].

peer(#req{socket = {ssl, Socket}}) ->
    case ssl:peername(Socket) of
        {ok, Client} ->
            Client;
        {error, _} ->
            erlang:exit(closed)
    end;
peer(#req{socket = {plain, Socket}}) ->
    case inet:peername(Socket) of
        {ok, Client} ->
            Client;
        {error, _} ->
            erlang:exit(closed)
    end.

header_accept_check(Headers, ContentTypesAccepted) ->
    case lists:keyfind(<<"accept">>, 1, Headers) of
        false ->
            true;
        {<<"accept">>, Value} ->
            case binary:match(Value, ContentTypesAccepted) of
                nomatch ->
                    false;
                _ ->
                    true
            end
    end.

header_content_type(Headers) ->
    case lists:keyfind(<<"content-type">>, 1, Headers) of
        false ->
            <<>>;
        {<<"content-type">>, Value} ->
            hd(binary:split(Value, <<",">>))
    end.

% format for external services, http headers passed as key-value pairs
headers_external_incoming(L) ->
    erlang:iolist_to_binary(headers_external_incoming_text(L)).

headers_external_incoming_text([] = L) ->
    L;
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

get_query_string_external_text([] = L) ->
    L;
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

handle_request(Name, Headers, 'normal', Req, State) ->
    Body = cloudi_x_elli_request:body(Req),
    handle_request(Name, Headers, Body, Req, State);
handle_request(Name, Headers, 'application_zip', Req, State) ->
    Body = cloudi_x_elli_request:body(Req),
    handle_request(Name, Headers, zlib:unzip(Body), Req, State);
handle_request(Name, Headers, Body, _Req,
               #elli_state{
                   dispatcher = Dispatcher,
                   context = Context,
                   output_type = OutputType}) ->
    RequestInfo = if
        (OutputType =:= external) orelse (OutputType =:= binary) ->
            headers_external_incoming(Headers);
        (OutputType =:= internal) orelse (OutputType =:= list) ->
            Headers
    end,
    Request = if
        (OutputType =:= external) orelse (OutputType =:= internal) orelse
        (OutputType =:= binary) ->
            Body;
        (OutputType =:= list) ->
            erlang:binary_to_list(Body)
    end,
    case send_sync_minimal(Dispatcher, Context,
                           Name, RequestInfo, Request, self()) of
        {{ok, ResponseInfo, Response}, _} ->
            HeadersOutgoing = headers_external_outgoing(ResponseInfo),
            {elli_response, HeadersOutgoing, Response};
        {{error, timeout}, _} ->
            {elli_error, timeout}
    end.

handle_response(NameIncoming, HeadersOutgoing0, Response,
                _Req, OutputType, ContentTypeForced) ->
    ResponseBinary = if
        (((OutputType =:= external) orelse
          (OutputType =:= internal)) andalso
         (is_binary(Response) orelse is_list(Response))) orelse
        ((OutputType =:= binary) andalso
         is_binary(Response)) ->
            Response;
        ((OutputType =:= list) andalso
         is_list(Response)) ->
            erlang:list_to_binary(Response)
    end,
    {HttpCode, HeadersOutgoingN} = case lists:keytake(<<"status">>, 1,
                                                      HeadersOutgoing0) of
        false ->
            {200, HeadersOutgoing0};
        {value, {_, Status}, HeadersOutgoing1}
            when is_binary(Status) ->
            {erlang:binary_to_integer(hd(binary:split(Status, <<" ">>))),
             HeadersOutgoing1}
    end,
    ResponseHeadersOutgoing = if
        HeadersOutgoingN =/= [] ->
            HeadersOutgoingN;
        ContentTypeForced =/= undefined ->
            [{<<"content-type">>, ContentTypeForced}];
        true ->
            Extension = filename:extension(NameIncoming),
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

service_name_incoming(UseClientIpPrefix, UseHostPrefix, PathRaw, Client,
                      HeadersIncoming)
    when UseClientIpPrefix =:= true, UseHostPrefix =:= true ->
    HostRaw = case lists:keyfind(<<"host">>, 1, HeadersIncoming) of
        {<<"host">>, H} ->
            H;
        false ->
            undefined
    end,
    service_name_incoming_merge(Client, HostRaw, PathRaw);
service_name_incoming(UseClientIpPrefix, UseHostPrefix, PathRaw, Client,
                      _HeadersIncoming)
    when UseClientIpPrefix =:= true, UseHostPrefix =:= false ->
    service_name_incoming_merge(Client, undefined, PathRaw);
service_name_incoming(UseClientIpPrefix, UseHostPrefix, PathRaw, _Client,
                      HeadersIncoming)
    when UseClientIpPrefix =:= false, UseHostPrefix =:= true ->
    HostRaw = case lists:keyfind(<<"host">>, 1, HeadersIncoming) of
        {<<"host">>, H} ->
            H;
        false ->
            undefined
    end,
    service_name_incoming_merge(undefined, HostRaw, PathRaw);
service_name_incoming(UseClientIpPrefix, UseHostPrefix, PathRaw, _Client,
                      _HeadersIncoming)
    when UseClientIpPrefix =:= false, UseHostPrefix =:= false ->
    service_name_incoming_merge(undefined, undefined, PathRaw).

service_name_incoming_merge(undefined, undefined, PathRaw) ->
    erlang:binary_to_list(PathRaw);
service_name_incoming_merge(undefined, HostRaw, PathRaw) ->
    erlang:binary_to_list(<<HostRaw/binary, PathRaw/binary>>);
service_name_incoming_merge({ClientIpAddr, _ClientPort}, undefined, PathRaw) ->
    cloudi_ip_address:to_string(ClientIpAddr) ++
    erlang:binary_to_list(PathRaw);
service_name_incoming_merge({ClientIpAddr, _ClientPort}, HostRaw, PathRaw) ->
    cloudi_ip_address:to_string(ClientIpAddr) ++
    erlang:binary_to_list(<<$/, HostRaw/binary, PathRaw/binary>>).

to_lower(Input) when is_binary(Input) ->
    to_lower(Input, <<>>).

to_lower(<<>>, Output) ->
    Output;
to_lower(<<C, Rest/binary>>, Output) ->
    NewC = if
        C == $A -> $a;
        C == $B -> $b;
        C == $C -> $c;
        C == $D -> $d;
        C == $E -> $e;
        C == $F -> $f;
        C == $G -> $g;
        C == $H -> $h;
        C == $I -> $i;
        C == $J -> $j;
        C == $K -> $k;
        C == $L -> $l;
        C == $M -> $m;
        C == $N -> $n;
        C == $O -> $o;
        C == $P -> $p;
        C == $Q -> $q;
        C == $R -> $r;
        C == $S -> $s;
        C == $T -> $t;
        C == $U -> $u;
        C == $V -> $v;
        C == $W -> $w;
        C == $X -> $x;
        C == $Y -> $y;
        C == $Z -> $z;
        true -> C
    end,
    to_lower(Rest, <<Output/binary, NewC>>).

