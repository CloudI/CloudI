%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI HTTP Integration==
%%% Uses the cloudi_x_cowboy Erlang HTTP Server.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2012-2015, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2012-2015 Michael Truog
%%% @version 1.5.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_http_cowboy).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_service).

%% external interface
-export([close/1]).

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include_lib("cloudi_core/include/cloudi_service_children.hrl").
-include("cloudi_http_cowboy_handler.hrl").

-define(DEFAULT_IP,                         {127,0,0,1}). % interface ip address
-define(DEFAULT_PORT,                              8080).
-define(DEFAULT_BACKLOG,                            128).
-define(DEFAULT_NODELAY,                           true).
-define(DEFAULT_RECV_TIMEOUT,                      5000). % milliseconds
-define(DEFAULT_BODY_TIMEOUT,                     15000). % milliseconds
-define(DEFAULT_BODY_LENGTH_READ,               1000000).
-define(DEFAULT_BODY_LENGTH_CHUNK,              8000000).
-define(DEFAULT_MULTIPART_HEADER_TIMEOUT,          5000). % milliseconds
-define(DEFAULT_MULTIPART_HEADER_LENGTH_READ,     64000).
-define(DEFAULT_MULTIPART_HEADER_LENGTH_CHUNK,    64000).
-define(DEFAULT_MULTIPART_BODY_TIMEOUT,           15000). % milliseconds
-define(DEFAULT_MULTIPART_BODY_LENGTH_READ,     1000000).
-define(DEFAULT_MULTIPART_BODY_LENGTH_CHUNK,    8000000).
-define(DEFAULT_MULTIPART_DESTINATION_LOCK,        true).
-define(DEFAULT_WEBSOCKET_TIMEOUT,             infinity). % milliseconds
-define(DEFAULT_WEBSOCKET_OUTPUT,             undefined).
-define(DEFAULT_WEBSOCKET_CONNECT_ASYNC,      undefined).
-define(DEFAULT_WEBSOCKET_CONNECT_SYNC,       undefined).
-define(DEFAULT_WEBSOCKET_DISCONNECT_ASYNC,   undefined).
-define(DEFAULT_WEBSOCKET_DISCONNECT_SYNC,    undefined).
-define(DEFAULT_WEBSOCKET_PING,               undefined). % milliseconds
-define(DEFAULT_WEBSOCKET_PROTOCOL,           undefined). % see below:
        % To avoid blocking on bidirectional communication requiring an
        % outgoing service request response without a websocket data identifier,
        % provide a function that provides the protocol's data identifier to be
        % used as a one-to-one mapping with the service request transaction id.
        % The incoming case does not need to produce an identifier, but is
        % provided for completeness (all websocket data uses the function).
        % The outgoing case needs to always return a iodata to be sent.
        % (incoming/outgoing shows whether it is coming into or out-of CloudI)
        % fun((incoming | outgoing, any()) ->
        %     {ID :: any(), any()} |
        %     {incoming, Request :: any()})
        % can be provided as {Module, FunctionName}
        % e.g.:
        % fun
        % (incoming, WebSocketBinary) ->
        %     case protocol_rpc:decode(WebSocketBinary) of
        %         #protocol_request{} = IncomingRequest ->
        %             {incoming, IncomingRequest};
        %         #protocol_response{id = ID} = IncomingResponse ->
        %             {ID, IncomingResponse};
        %         #protocol_error{id = ID} = IncomingResponse ->
        %             {ID, IncomingResponse}
        %     end;
        % (outgoing, #protocol_request{id = ID} = OutgoingRequest) ->
        %     {ID, protocol_rpc:encode(OutgoingRequest)};
        % (outgoing, #protocol_response{id = ID} = OutgoingResponse) ->
        %     {ID, protocol_rpc:encode(OutgoingResponse)};
        % (outgoing, #protocol_error{id = ID} = OutgoingResponse) ->
        %     {ID, protocol_rpc:encode(OutgoingResponse)}
        % end.
-define(DEFAULT_WEBSOCKET_NAME_UNIQUE,            false). % see below:
        % set to true if the websocket name (for outgoing service requests)
        % should only ever be used by a single websocket connection.
        % When set to true, the newest connection will kill any older
        % connection(s) that share the same service name.
-define(DEFAULT_WEBSOCKET_SUBSCRIPTIONS,             []). % see below:
        % Provide configuration similar to cloudi_service_router:
        % list({PatternSuffix :: string(),
        %       string() |
        %       list({parameters_allowed, boolean()} |
        %            {parameters_strict_matching, boolean()} |
        %            {parameters_selected, list(pos_integer())} |
        %            {service_name, PatternTemplate :: string()})})
        % Use this to create a mapping from the websocket connection URL path
        % and a separate service name used as a separate websocket
        % subscription for receiving service requests to forward through the
        % websocket connection.  If more than one subscription needs to occur,
        % just list the same PatternSuffix for each subscription.
-define(DEFAULT_SSL,                              false).
-define(DEFAULT_COMPRESS,                         false).
-define(DEFAULT_MAX_CONNECTIONS,                   4096).
-define(DEFAULT_MAX_EMPTY_LINES,                      5).
-define(DEFAULT_MAX_HEADER_NAME_LENGTH,              64).
-define(DEFAULT_MAX_HEADER_VALUE_LENGTH,           4096).
-define(DEFAULT_MAX_HEADERS,                        100).
-define(DEFAULT_MAX_KEEPALIVE,                      100). % requests in session
-define(DEFAULT_MAX_REQUEST_LINE_LENGTH,           4096).
-define(DEFAULT_OUTPUT,                        external).
-define(DEFAULT_CONTENT_TYPE,                 undefined). % force a content type
-define(DEFAULT_CONTENT_TYPES_ACCEPTED,       undefined). % see below:
        % Provide a list of content types strings
        % (list of integers or binaries) which must match the
        % HTTP request "Accept" header value
-define(DEFAULT_SET_X_FORWARDED_FOR,              false). % if it is missing
-define(DEFAULT_STATUS_CODE_TIMEOUT,                504). % "Gateway Timeout"
-define(DEFAULT_QUERY_GET_FORMAT,                   raw). % see below:
        % If set to 'text_pairs' any GET query string is parsed and
        % encoded based on the format defined in the cloudi_request_info
        % module (to create a single binary accessible in external services)
        % if the output is not internal, if the output is internal the
        % parsed query string is provided in an Erlang data structure used
        % by the cloudi_key_value module.
        % Due to making testing simpler without requiring extra dependencies,
        % the 'text_pairs' format is used when performing basic loadtests
        % of CloudI with the http_req integration test.
        % The default 'raw' setting will not parse GET query string data.
        % GET query string data is always provided in the service request
        % Request data (so using GET request body data will be ignored,
        % due to being bad practice).
-define(DEFAULT_USE_WEBSOCKETS,                   false).
-define(DEFAULT_USE_SPDY,                         false).
-define(DEFAULT_USE_HOST_PREFIX,                  false). % for virtual hosts
-define(DEFAULT_USE_CLIENT_IP_PREFIX,             false).
-define(DEFAULT_USE_X_METHOD_OVERRIDE,            false).
-define(DEFAULT_USE_METHOD_SUFFIX,                 true). % see below:
        % Always append a suffix on the service name used to send the
        % HTTP request as a CloudI service request, to utilize the service
        % name routing when handling separate HTTP request methods.
        % For example, a GET HTTP request method would cause "/get" to be
        % added to the service name (the URL path) that is used when sending
        % the service request.

-record(state,
    {
        listener,
        service
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Close a cowboy websocket connection.===
%% Use the Pid from cloudi_service_handle_request/11.  Otherwise, you can
%% use either the get_pid function or get_pids function in the cloudi module
%% (or the cloudi_service module) to find a match for the connection URL.
%% @end
%%-------------------------------------------------------------------------

-spec close(pid() | {string(), pid()}) ->
    ok.

close(Pid)
    when is_pid(Pid) ->
    Pid ! {cowboy_error, shutdown},
    ok;
close({Pattern, Pid})
    when is_list(Pattern), is_pid(Pid) ->
    Pid ! {cowboy_error, shutdown},
    ok.

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, Prefix, _Timeout, Dispatcher) ->
    Defaults = [
        {ip,                            ?DEFAULT_IP},
        {port,                          ?DEFAULT_PORT},
        {backlog,                       ?DEFAULT_BACKLOG},
        {nodelay,                       ?DEFAULT_NODELAY},
        {recv_timeout,                  ?DEFAULT_RECV_TIMEOUT},
        {body_timeout,                  ?DEFAULT_BODY_TIMEOUT},
        {body_length_read,              ?DEFAULT_BODY_LENGTH_READ},
        {body_length_chunk,             ?DEFAULT_BODY_LENGTH_CHUNK},
        {multipart_header_timeout,      ?DEFAULT_MULTIPART_HEADER_TIMEOUT},
        {multipart_header_length_read,  ?DEFAULT_MULTIPART_HEADER_LENGTH_READ},
        {multipart_header_length_chunk, ?DEFAULT_MULTIPART_HEADER_LENGTH_CHUNK},
        {multipart_body_timeout,        ?DEFAULT_MULTIPART_BODY_TIMEOUT},
        {multipart_body_length_read,    ?DEFAULT_MULTIPART_BODY_LENGTH_READ},
        {multipart_body_length_chunk,   ?DEFAULT_MULTIPART_BODY_LENGTH_CHUNK},
        {multipart_destination_lock,    ?DEFAULT_MULTIPART_DESTINATION_LOCK},
        {websocket_timeout,             ?DEFAULT_WEBSOCKET_TIMEOUT},
        {websocket_output,              ?DEFAULT_WEBSOCKET_OUTPUT},
        {websocket_connect,             ?DEFAULT_WEBSOCKET_CONNECT_ASYNC},
        {websocket_disconnect,          ?DEFAULT_WEBSOCKET_DISCONNECT_ASYNC},
        {websocket_connect_async,       ?DEFAULT_WEBSOCKET_CONNECT_ASYNC},
        {websocket_connect_sync,        ?DEFAULT_WEBSOCKET_CONNECT_SYNC},
        {websocket_disconnect_async,    ?DEFAULT_WEBSOCKET_DISCONNECT_ASYNC},
        {websocket_disconnect_sync,     ?DEFAULT_WEBSOCKET_DISCONNECT_SYNC},
        {websocket_ping,                ?DEFAULT_WEBSOCKET_PING},
        {websocket_protocol,            ?DEFAULT_WEBSOCKET_PROTOCOL},
        {websocket_name_unique,         ?DEFAULT_WEBSOCKET_NAME_UNIQUE},
        {websocket_subscriptions,       ?DEFAULT_WEBSOCKET_SUBSCRIPTIONS},
        {ssl,                           ?DEFAULT_SSL},
        {compress,                      ?DEFAULT_COMPRESS},
        {max_connections,               ?DEFAULT_MAX_CONNECTIONS},
        {max_empty_lines,               ?DEFAULT_MAX_EMPTY_LINES},
        {max_header_name_length,        ?DEFAULT_MAX_HEADER_NAME_LENGTH},
        {max_header_value_length,       ?DEFAULT_MAX_HEADER_VALUE_LENGTH},
        {max_headers,                   ?DEFAULT_MAX_HEADERS},
        {max_keepalive,                 ?DEFAULT_MAX_KEEPALIVE},
        {max_request_line_length,       ?DEFAULT_MAX_REQUEST_LINE_LENGTH},
        {output,                        ?DEFAULT_OUTPUT},
        {content_type,                  ?DEFAULT_CONTENT_TYPE},
        {content_types_accepted,        ?DEFAULT_CONTENT_TYPES_ACCEPTED},
        {set_x_forwarded_for,           ?DEFAULT_SET_X_FORWARDED_FOR},
        {status_code_timeout,           ?DEFAULT_STATUS_CODE_TIMEOUT},
        {query_get_format,              ?DEFAULT_QUERY_GET_FORMAT},
        {use_websockets,                ?DEFAULT_USE_WEBSOCKETS},
        {use_spdy,                      ?DEFAULT_USE_SPDY},
        {use_host_prefix,               ?DEFAULT_USE_HOST_PREFIX},
        {use_client_ip_prefix,          ?DEFAULT_USE_CLIENT_IP_PREFIX},
        {use_x_method_override,         ?DEFAULT_USE_X_METHOD_OVERRIDE},
        {use_method_suffix,             ?DEFAULT_USE_METHOD_SUFFIX}],
    [Interface, Port, Backlog, NoDelay, RecvTimeout,
     BodyTimeout, BodyLengthRead, BodyLengthChunk, MultipartHeaderTimeout,
     MultipartHeaderLengthRead, MultipartHeaderLengthChunk,
     MultipartBodyTimeout, MultipartBodyLengthRead, MultipartBodyLengthChunk,
     MultipartDestinationLock, WebSocketTimeout, WebSocketOutputType0,
     WebSocketConnect0, WebSocketDisconnect0,
     WebSocketConnectAsync0, WebSocketConnectSync,
     WebSocketDisconnectAsync0, WebSocketDisconnectSync,
     WebSocketPing, WebSocketProtocol0, WebSocketNameUnique,
     WebSocketSubscriptions0, SSL, Compress,
     MaxConnections, MaxEmptyLines, MaxHeaderNameLength, MaxHeaderValueLength,
     MaxHeaders, MaxKeepAlive, MaxRequestLineLength,
     OutputType, ContentTypeForced0, ContentTypesAccepted0, SetXForwardedFor,
     StatusCodeTimeout, QueryGetFormat, UseWebSockets, UseSpdy,
     UseHostPrefix, UseClientIpPrefix, UseXMethodOverride, UseMethodSuffix] =
        cloudi_proplists:take_values(Defaults, Args),
    1 = cloudi_service:process_count_max(Dispatcher),
    true = is_integer(Port),
    true = is_integer(Backlog),
    true = is_boolean(NoDelay),
    true = is_integer(RecvTimeout) andalso (RecvTimeout > 0),
    true = is_integer(BodyTimeout) andalso (BodyTimeout > 0),
    true = is_integer(BodyLengthRead) andalso (BodyLengthRead > 0),
    true = is_integer(BodyLengthChunk) andalso (BodyLengthChunk > 0),
    true = is_integer(MultipartHeaderTimeout) andalso
           (MultipartHeaderTimeout > 0),
    true = is_integer(MultipartHeaderLengthRead) andalso
           (MultipartHeaderLengthRead > 0),
    true = is_integer(MultipartHeaderLengthChunk) andalso
           (MultipartHeaderLengthChunk > 0),
    true = is_integer(MultipartBodyTimeout) andalso
           (MultipartBodyTimeout > 0),
    true = is_integer(MultipartBodyLengthRead) andalso
           (MultipartBodyLengthRead > 0),
    true = is_integer(MultipartBodyLengthChunk) andalso
           (MultipartBodyLengthChunk > 0),
    true = is_boolean(MultipartDestinationLock),
    true = (WebSocketTimeout =:= infinity) orelse
           (is_integer(WebSocketTimeout) andalso (WebSocketTimeout > 0)),
    WebSocketOutputType1 = if
        WebSocketOutputType0 =:= undefined ->
            if
                OutputType =:= external; OutputType =:= internal;
                OutputType =:= binary ->
                    binary;
                OutputType =:= list ->
                    text
            end;
        WebSocketOutputType0 =:= binary ->
            binary;
        WebSocketOutputType0 =:= text ->
            text
    end,
    WebSocketConnectAsync1 = if
        WebSocketConnectAsync0 =:= undefined ->
            true = (WebSocketConnect0 =:= undefined) orelse
                   (is_list(WebSocketConnect0) andalso
                    is_integer(hd(WebSocketConnect0))),
            WebSocketConnect0;
        is_list(WebSocketConnectAsync0),
        is_integer(hd(WebSocketConnectAsync0)) ->
            WebSocketConnectAsync0
    end,
    true = (WebSocketConnectSync =:= undefined) orelse
           (is_list(WebSocketConnectSync) andalso
            is_integer(hd(WebSocketConnectSync))),
    WebSocketConnect1 = if
        WebSocketConnectAsync1 =/= undefined,
        WebSocketConnectSync =:= undefined ->
            {async, WebSocketConnectAsync1};
        WebSocketConnectAsync1 =:= undefined,
        WebSocketConnectSync =/= undefined ->
            {sync, WebSocketConnectSync};
        WebSocketConnectAsync1 =:= undefined,
        WebSocketConnectSync =:= undefined ->
            undefined
    end,
    WebSocketDisconnectAsync1 = if
        WebSocketDisconnectAsync0 =:= undefined ->
            true = (WebSocketDisconnect0 =:= undefined) orelse
                   (is_list(WebSocketDisconnect0) andalso
                    is_integer(hd(WebSocketDisconnect0))),
            WebSocketDisconnect0;
        is_list(WebSocketDisconnectAsync0),
        is_integer(hd(WebSocketDisconnectAsync0)) ->
            WebSocketDisconnectAsync0
    end,
    true = (WebSocketDisconnectSync =:= undefined) orelse
           (is_list(WebSocketDisconnectSync) andalso
            is_integer(hd(WebSocketDisconnectSync))),
    WebSocketDisconnect1 = if
        WebSocketDisconnectAsync1 =/= undefined,
        WebSocketDisconnectSync =:= undefined ->
            {async, WebSocketDisconnectAsync1};
        WebSocketDisconnectAsync1 =:= undefined,
        WebSocketDisconnectSync =/= undefined ->
            {sync, WebSocketDisconnectSync};
        WebSocketDisconnectAsync1 =:= undefined,
        WebSocketDisconnectSync =:= undefined ->
            undefined
    end,
    true = (WebSocketPing =:= undefined) orelse
           (is_integer(WebSocketPing) andalso (WebSocketPing > 0)),
    WebSocketProtocol1 = cloudi_args_type:
                         function_optional(WebSocketProtocol0, 2),
    true = is_boolean(WebSocketNameUnique),
    WebSocketSubscriptions1 = if
        WebSocketSubscriptions0 == [] ->
            undefined;
        is_list(WebSocketSubscriptions0) ->
            websocket_subscriptions_lookup(WebSocketSubscriptions0, Prefix)
    end,
    true = is_boolean(Compress),
    true = is_integer(MaxConnections),
    true = is_integer(MaxEmptyLines),
    true = is_integer(MaxHeaderNameLength),
    true = is_integer(MaxHeaderValueLength),
    true = is_integer(MaxHeaders),
    true = is_integer(MaxKeepAlive),
    true = is_integer(MaxRequestLineLength),
    true = (OutputType =:= external) orelse (OutputType =:= internal) orelse
           (OutputType =:= list) orelse (OutputType =:= binary),
    ContentTypeForced1 = if
        ContentTypeForced0 =:= undefined ->
            undefined;
        is_list(ContentTypeForced0) ->
            erlang:list_to_binary(ContentTypeForced0);
        is_binary(ContentTypeForced0) ->
            ContentTypeForced0
    end,
    ContentTypesAccepted1 = if
        ContentTypesAccepted0 =:= undefined ->
            undefined;
        is_list(ContentTypesAccepted0) ->
            content_types_accepted_pattern(ContentTypesAccepted0)
    end,
    true = is_boolean(SetXForwardedFor),
    true = is_integer(StatusCodeTimeout) andalso
           (StatusCodeTimeout > 100) andalso
           (StatusCodeTimeout =< 599),
    true = (QueryGetFormat =:= raw) orelse
           (QueryGetFormat =:= text_pairs),
    true = (is_boolean(UseWebSockets) orelse
            (UseWebSockets =:= exclusively)),
    true = is_boolean(UseSpdy),
    true = is_boolean(UseHostPrefix),
    true = is_boolean(UseClientIpPrefix),
    true = ((UseXMethodOverride =:= true) andalso
            (UseMethodSuffix =:= true)) orelse
           (UseXMethodOverride =:= false),
    true = is_boolean(UseMethodSuffix),
    false = lists:member($*, Prefix),
    {_, Scope} = lists:keyfind(groups_scope, 1,
                               cloudi_service:context_options(Dispatcher)),
    Dispatch = cloudi_x_cowboy_router:compile([
        %% {Host, list({Path, Handler, Opts})}
        {'_', [{'_', cloudi_http_cowboy_handler,
                #cowboy_state{
                    dispatcher = cloudi_service:dispatcher(Dispatcher),
                    context = create_context(Dispatcher),
                    scope = Scope,
                    prefix = Prefix,
                    timeout_body = BodyTimeout,
                    timeout_part_header = MultipartHeaderTimeout,
                    timeout_part_body = MultipartBodyTimeout,
                    timeout_websocket = WebSocketTimeout,
                    length_body_read = BodyLengthRead,
                    length_body_chunk = BodyLengthChunk,
                    length_part_header_read = MultipartHeaderLengthRead,
                    length_part_header_chunk = MultipartHeaderLengthChunk,
                    length_part_body_read = MultipartBodyLengthRead,
                    length_part_body_chunk = MultipartBodyLengthChunk,
                    parts_destination_lock = MultipartDestinationLock,
                    output_type = OutputType,
                    content_type_forced = ContentTypeForced1,
                    content_types_accepted = ContentTypesAccepted1,
                    set_x_forwarded_for = SetXForwardedFor,
                    status_code_timeout = StatusCodeTimeout,
                    query_get_format = QueryGetFormat,
                    websocket_output_type = WebSocketOutputType1,
                    websocket_connect = WebSocketConnect1,
                    websocket_disconnect = WebSocketDisconnect1,
                    websocket_ping = WebSocketPing,
                    websocket_protocol = WebSocketProtocol1,
                    websocket_name_unique = WebSocketNameUnique,
                    websocket_subscriptions = WebSocketSubscriptions1,
                    use_websockets = UseWebSockets,
                    use_host_prefix = UseHostPrefix,
                    use_client_ip_prefix = UseClientIpPrefix,
                    use_x_method_override = UseXMethodOverride,
                    use_method_suffix = UseMethodSuffix}}]}
    ]),
    Service = cloudi_service:self(Dispatcher),
    StartFunction = if
        UseSpdy =:= true ->
            start_spdy;
        is_list(SSL) ->
            start_https;
        SSL =:= false ->
            start_http
    end,
    {ok, ListenerPid} = if
        is_list(SSL) ->
            {value,
             {certfile, CertFile}, SSLOpts} = lists:keytake(certfile, 1, SSL),
            [] = cloudi_proplists:delete_all([cacertfile,
                                              ciphers,
                                              keyfile,
                                              password,
                                              verify], SSLOpts),
            Environment = cloudi_environment:lookup(),
            NewSSLOpts = environment_transform_ssl_options(SSLOpts,
                                                           Environment),
            cloudi_x_cowboy:StartFunction(
                Service, % Ref
                100, % Number of acceptor processes
                [{ip, Interface},
                 {port, Port},
                 {backlog, Backlog},
                 {nodelay, NoDelay},
                 {max_connections, MaxConnections},
                 {certfile, CertFile}] ++
                NewSSLOpts, % Transport options
                [{env, [{dispatch, Dispatch}]},
                 {compress, Compress},
                 {max_empty_lines, MaxEmptyLines},
                 {max_header_name_length, MaxHeaderNameLength},
                 {max_header_value_length, MaxHeaderValueLength},
                 {max_headers, MaxHeaders},
                 {max_keepalive, MaxKeepAlive},
                 {max_request_line_length, MaxRequestLineLength},
                 {timeout, RecvTimeout}] % Protocol options
            );
        SSL =:= false ->
            cloudi_x_cowboy:StartFunction(
                Service, % Ref
                100, % Number of acceptor processes
                [{ip, Interface},
                 {port, Port},
                 {backlog, Backlog},
                 {nodelay, NoDelay},
                 {max_connections, MaxConnections}], % Transport options
                [{env, [{dispatch, Dispatch}]},
                 {compress, Compress},
                 {max_empty_lines, MaxEmptyLines},
                 {max_header_name_length, MaxHeaderNameLength},
                 {max_header_value_length, MaxHeaderValueLength},
                 {max_headers, MaxHeaders},
                 {max_keepalive, MaxKeepAlive},
                 {max_request_line_length, MaxRequestLineLength},
                 {timeout, RecvTimeout}] % Protocol options
            )
    end,
    {ok, #state{listener = ListenerPid,
                service = Service}}.

cloudi_service_handle_request(_Type, _Name, _Pattern, _RequestInfo, _Request,
                              _Timeout, _Priority, _TransId, _Pid,
                              State, _Dispatcher) ->
    {reply, <<>>, State}.

cloudi_service_handle_info(Request, State, _Dispatcher) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(_Reason, _Timeout,
                         #state{service = Service}) ->
    try cloudi_x_cowboy:stop_listener(Service)
    catch
        exit:{noproc, _} ->
            ?LOG_WARN("ranch noproc", [])
    end,
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

websocket_subscriptions_lookup([], Lookup, _) ->
    Lookup;
websocket_subscriptions_lookup([{PatternSuffix, L} |
                                WebSocketSubscriptions], Lookup, Prefix) ->
    ConfigDefaults = [
        {parameters_allowed,         true},
        {parameters_strict_matching, true},
        {parameters_selected,        []},
        {service_name,               undefined}],
    case L of
        [I | _] when is_integer(I) ->
            Name = Prefix ++ L,
            F = fun(Parameters) ->
                cloudi_service_name:new(Name, Parameters)
            end,
            NewLookup = cloudi_x_trie:update(Prefix ++ PatternSuffix,
                                             fun(Functions) ->
                                                 [F | Functions]
                                             end, [F], Lookup),
            websocket_subscriptions_lookup(WebSocketSubscriptions,
                                           NewLookup, Prefix);
        [_ | _] ->
            [ParametersAllowed,
             ParametersStrictMatching,
             ParametersSelected,
             Suffix] = cloudi_proplists:take_values(ConfigDefaults, L),
            true = is_boolean(ParametersAllowed),
            true = is_boolean(ParametersStrictMatching),
            true = is_list(ParametersSelected),
            true = ((ParametersSelected == []) orelse
                    ((ParametersSelected /= []) andalso
                     (ParametersAllowed =:= true))),
            true = lists:all(fun(I) -> is_integer(I) andalso I > 0 end,
                             ParametersSelected),
            true = (is_list(Suffix) andalso
                    is_integer(hd(Suffix))),
            Name = Prefix ++ Suffix,
            true = ((ParametersAllowed =:= true) orelse
                    ((ParametersAllowed =:= false) andalso
                     (cloudi_x_trie:is_pattern(Name) =:= false))),
            F = if
                ParametersAllowed =:= true ->
                    fun(Parameters) ->
                        cloudi_service:
                        service_name_new(Name,
                                         Parameters,
                                         ParametersSelected,
                                         ParametersStrictMatching)
                    end;
                ParametersAllowed =:= false ->
                    fun(Parameters) ->
                        if
                            Parameters == [] ->
                                {ok, Name};
                            true ->
                                {error, parameters_ignored}
                        end
                    end
            end,
            NewLookup = cloudi_x_trie:update(Prefix ++ PatternSuffix,
                                             fun(Functions) ->
                                                 [F | Functions]
                                             end, [F], Lookup),
            websocket_subscriptions_lookup(WebSocketSubscriptions,
                                           NewLookup, Prefix)
    end.

websocket_subscriptions_lookup(WebSocketSubscriptions, Prefix) ->
    websocket_subscriptions_lookup(WebSocketSubscriptions,
                                   cloudi_x_trie:new(), Prefix).

environment_transform_ssl_options(SSLOpts, Environment) ->
    environment_transform_ssl_options(SSLOpts, [], Environment).

environment_transform_ssl_options([], Output, _) ->
    lists:reverse(Output);
environment_transform_ssl_options([{K, FilePath} | SSLOpts],
                                  Output, Environment)
    when K =:= certfile; K =:= cacertfile; K =:= keyfile ->
    NewFilePath = cloudi_environment:transform(FilePath, Environment),
    environment_transform_ssl_options(SSLOpts, [{K, NewFilePath} | Output],
                                      Environment);
environment_transform_ssl_options([E | SSLOpts], Output, Environment) ->
    environment_transform_ssl_options(SSLOpts, [E | Output], Environment).

content_types_accepted_pattern(ContentTypesAccepted) ->
    content_types_accepted_pattern(ContentTypesAccepted, [<<"*/*">>]).

content_types_accepted_pattern([], L) ->
    binary:compile_pattern(L);
content_types_accepted_pattern([H | ContentTypesAccepted], L) ->
    ContentType = if
        is_list(H), is_integer(hd(H)) ->
            erlang:list_to_binary(H);
        is_binary(H) ->
            H
    end,
    NewL = case binary:split(ContentType, <<"/">>) of
        [<<"*">>, <<"*">>] ->
            lists:umerge(L, [ContentType]);
        [_, <<"*">>] ->
            lists:umerge(L, [ContentType]);
        [Type, _] ->
            lists:umerge(L, [<<Type/binary,<<"/*">>/binary>>, ContentType])
    end,
    content_types_accepted_pattern(ContentTypesAccepted, NewL).

