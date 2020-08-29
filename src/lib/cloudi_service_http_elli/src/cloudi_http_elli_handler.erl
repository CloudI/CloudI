%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Elli CloudI HTTP Handler==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2013-2020 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2013-2020 Michael Truog
%%% @version 2.0.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_http_elli_handler).
-author('mjtruog at protonmail dot com').

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
                   content_security_policy = ContentSecurityPolicy,
                   content_security_policy_report = ContentSecurityPolicyReport,
                   set_x_forwarded_for = SetXForwardedFor,
                   set_x_xss_protection = SetXXSSProtection,
                   set_x_content_type_options = SetXContentTypeOptions,
                   status_code_timeout = StatusCodeTimeout,
                   query_get_format = QueryGetFormat,
                   use_host_prefix = UseHostPrefix,
                   use_client_ip_prefix = UseClientIpPrefix,
                   use_x_method_override = UseXMethodOverride,
                   use_method_suffix = UseMethodSuffix} = State) ->
    RequestStartMicroSec = ?LOG_WARN_APPLY(fun request_time_start/0, []),
    MethodHTTP = cloudi_x_elli_request:method(Req),
    HeadersIncoming0 = headers_to_lower(cloudi_x_elli_request:headers(Req)),
    Method = if
        UseXMethodOverride =:= true ->
            case lists:keyfind(<<"x-http-method-override">>, 1,
                               HeadersIncoming0) of
                {_, <<"GET">>} ->
                    'GET';
                {_, <<"POST">>} ->
                    'POST';
                {_, <<"PUT">>} ->
                    'PUT';
                {_, <<"DELETE">>} ->
                    'DELETE';
                {_, <<"HEAD">>} ->
                    'HEAD';
                {_, <<"TRACE">>} ->
                    'TRACE';
                {_, <<"OPTIONS">>} ->
                    'OPTIONS';
                {_, MethodOverride} ->
                    MethodOverride;
                false ->
                    MethodHTTP
            end;
        UseXMethodOverride =:= false ->
            MethodHTTP
    end,
    [PathRaw | QSRawL] = binary:split(cloudi_x_elli_request:raw_path(Req),
                                      [<<"?">>]),
    QS = if
        MethodHTTP =:= 'GET' ->
            QSRaw = case QSRawL of
                [] ->
                    <<>>;
                [QSRawValue] ->
                    QSRawValue
            end,
            if
                QueryGetFormat =:= text_pairs ->
                    QSVals = cloudi_x_cow1_qs:parse_qs(QSRaw),
                    if
                        (OutputType =:= external) orelse
                        (OutputType =:= binary) orelse (OutputType =:= list) ->
                            get_query_string_external(QSVals);
                        OutputType =:= internal ->
                            % cloudi_key_value format
                            QSVals
                    end;
                QueryGetFormat =:= raw ->
                    QSRaw
            end;
        true ->
            % query strings only handled for GET methods
            undefined
    end,
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
            ?LOG_WARN_APPLY(fun request_time_end_error/6,
                            [HttpCode, MethodHTTP,
                             NameIncoming, undefined,
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
                    NameIncoming ++ [$/ |
                        cloudi_string:lowercase(erlang:binary_to_list(Method))]
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
                MethodHTTP =:= 'GET' ->
                    % only the query string is provided as the
                    % body of a GET request passed within the Request parameter
                    % of a CloudI service request, which prevents misuse of GET
                    QS;
                (MethodHTTP =:= 'HEAD') orelse
                (MethodHTTP =:= 'OPTIONS') orelse
                (MethodHTTP =:= 'TRACE') orelse
                (MethodHTTP == <<"CONNECT">>) ->
                    <<>>;
                true ->
                    % POST, PUT, DELETE or anything else
                    case header_content_type(HeadersIncomingN) of
                        <<"application/zip">> ->
                            'application_zip';
                        _ ->
                            'normal'
                    end
            end,
            case handle_request(NameOutgoing, HeadersIncomingN,
                                Body, Req, State) of
                {elli_response, HeadersOutgoing, Response} ->
                    {HttpCode, _,
                     _} = Result = handle_response(NameIncoming,
                                                   HeadersOutgoing,
                                                   Response, Req, OutputType,
                                                   ContentTypeForced,
                                                   SetXContentTypeOptions,
                                                   SetXXSSProtection,
                                                   ContentSecurityPolicy,
                                                   ContentSecurityPolicyReport),
                    ?LOG_TRACE_APPLY(fun request_time_end_success/5,
                                     [HttpCode, MethodHTTP,
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
                    ?LOG_WARN_APPLY(fun request_time_end_error/6,
                                    [HttpCode, MethodHTTP,
                                     NameIncoming, NameOutgoing,
                                     RequestStartMicroSec, timeout]),
                    Result
            end
    end.

%% elli_startup is sent when Elli is starting up. If you are
%% implementing a middleware, you can use it to spawn processes,
%% create ETS tables or start supervised processes in a supervisor
%% tree.
handle_event(elli_startup, [], _) -> ok;
%% elli_reconfigure is an event after elli:set_callback/3 is called
handle_event(elli_reconfigure, [], _) -> ok;
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
    cloudi_request_info:key_value_new(L, text_pairs).

headers_external_outgoing(<<>>) ->
    [];
headers_external_outgoing([] = ResponseInfo) ->
    ResponseInfo;
headers_external_outgoing([{_, _} | _] = ResponseInfo) ->
    ResponseInfo;
headers_external_outgoing(ResponseInfo)
    when is_binary(ResponseInfo) ->
    cloudi_response_info:key_value_parse(ResponseInfo, list).

header_set_if_not(Key, Value, Headers) ->
    case lists:keyfind(Key, 1, Headers) of
        {_, _} ->
            Headers;
        false ->
            [{Key, Value} | Headers]
    end.

get_query_string_external(QsVals) ->
    cloudi_request_info:key_value_new(QsVals, text_pairs).

request_time_start() ->
    cloudi_timestamp:microseconds_monotonic().

request_time_end_success(HttpCode, Method, NameIncoming, NameOutgoing,
                         RequestStartMicroSec) ->
    ?LOG_TRACE("~w ~s ~s (to ~s) ~p ms",
               [HttpCode, Method, NameIncoming, NameOutgoing,
                (cloudi_timestamp:microseconds_monotonic() -
                 RequestStartMicroSec) / 1000.0]).

request_time_end_error(HttpCode, Method, NameIncoming, NameOutgoing,
                       RequestStartMicroSec, Reason) ->
    RequestTime = (cloudi_timestamp:microseconds_monotonic() -
                   RequestStartMicroSec) / 1000.0,
    if
        NameOutgoing =:= undefined ->
            ?LOG_WARN("~w ~s ~s ~p ms: ~p",
                      [HttpCode, Method, NameIncoming,
                       RequestTime, Reason]);
        true ->
            ?LOG_WARN("~w ~s ~s (to ~s) ~p ms: ~p",
                      [HttpCode, Method, NameIncoming, NameOutgoing,
                       RequestTime, Reason])
    end.

handle_request(Name, Headers, 'normal', Req, State) ->
    Body = cloudi_x_elli_request:body(Req),
    handle_request(Name, Headers, Body, Req, State);
handle_request(Name, Headers, 'application_zip', Req, State) ->
    Body = cloudi_x_elli_request:body(Req),
    handle_request(Name, Headers, zlib:unzip(Body), Req, State);
handle_request(Name, Headers, Body, _Req,
               #elli_state{
                   dispatcher = Dispatcher,
                   timeout_sync = Timeout,
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
    case send_sync_minimal(Dispatcher, Name, RequestInfo, Request,
                           Timeout, self()) of
        {ok, ResponseInfo, Response} ->
            HeadersOutgoing = headers_external_outgoing(ResponseInfo),
            {elli_response, HeadersOutgoing, Response};
        {error, timeout} ->
            {elli_error, timeout}
    end.

handle_response(NameIncoming, HeadersOutgoing0, Response,
                _Req, OutputType, ContentTypeForced,
                SetXContentTypeOptions, SetXXSSProtection,
                ContentSecurityPolicy, ContentSecurityPolicyReport) ->
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
    {HttpCode, HeadersOutgoing2} = case lists:keytake(<<"status">>, 1,
                                                      HeadersOutgoing0) of
        false ->
            {200, HeadersOutgoing0};
        {value, {_, Status}, HeadersOutgoing1}
            when is_binary(Status) ->
            {erlang:binary_to_integer(hd(binary:split(Status, <<" ">>))),
             HeadersOutgoing1}
    end,
    HeadersOutgoing3 = if
        HeadersOutgoing2 =/= [] ->
            HeadersOutgoing2;
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
    {ContentTypeHTML,
     ContentTypeSet} = case lists:keyfind(<<"content-type">>, 1,
                                          HeadersOutgoing3) of
        {_,  <<"text/html", _/binary>>} ->
            {true, true};
        {_,  <<_/binary>>} ->
            {false, true};
        false ->
            {false, false}
    end,
    HeadersOutgoing4 = if
        ContentTypeSet =:= true ->
            if
                SetXContentTypeOptions =:= true ->
                    header_set_if_not(<<"x-content-type-options">>,
                                      <<"nosniff">>,
                                      HeadersOutgoing3);
                SetXContentTypeOptions =:= false ->
                    HeadersOutgoing3
            end;
        ContentTypeSet =:= false ->
            HeadersOutgoing3
    end,
    HeadersOutgoingN = if
        ContentTypeHTML =:= true ->
            HeadersOutgoing5 = if
                SetXXSSProtection =:= true ->
                    header_set_if_not(<<"x-xss-protection">>,
                                      <<"0">>,
                                      HeadersOutgoing4);
                SetXXSSProtection =:= false ->
                    HeadersOutgoing4
            end,
            HeadersOutgoing6 = if
                is_binary(ContentSecurityPolicyReport) ->
                    header_set_if_not(<<"content-security-policy-report-only">>,
                                      ContentSecurityPolicyReport,
                                      HeadersOutgoing5);
                ContentSecurityPolicyReport =:= undefined ->
                    HeadersOutgoing5
            end,
            if
                is_binary(ContentSecurityPolicy) ->
                    header_set_if_not(<<"content-security-policy">>,
                                      ContentSecurityPolicy,
                                      HeadersOutgoing6);
                ContentSecurityPolicy =:= undefined ->
                    HeadersOutgoing6
            end;
        ContentTypeHTML =:= false ->
            HeadersOutgoing4
    end,
    ResponseHeadersOutgoing = HeadersOutgoingN,
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

