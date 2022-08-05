%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI HTTP Integration==
%%% Uses the cloudi_x_cowboy Erlang HTTP Server.
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2012-2022 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2012-2022 Michael Truog
%%% @version 2.0.5 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_http_cowboy).
-author('mjtruog at protonmail dot com').

-behaviour(cloudi_service).

%% external interface
-export([close/1]).

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include_lib("cloudi_core/include/cloudi_service_children.hrl").
-include("cloudi_http_cowboy_handler.hrl").

-define(DEFAULT_IP,                         {127,0,0,1}). % interface ip address
-define(DEFAULT_PORT,                              8080).
-define(DEFAULT_REUSE_PORT,                   undefined).
        % If undefined, count_process > 1 will automatically attempt to
        % use SO_REUSEPORT or SO_REUSEPORT_LB (depending on the OS support).
        % If true or false, the socket option addition will be
        % separate from the count_process value.
-define(DEFAULT_BACKLOG,                            128).
-define(DEFAULT_NODELAY,                           true).
-define(DEFAULT_RECV_TIMEOUT,                     60000). % milliseconds
-define(DEFAULT_BODY_TIMEOUT,                     15000). % milliseconds
-define(DEFAULT_BODY_LENGTH_READ,               8000000).
-define(DEFAULT_MULTIPART_HEADER_TIMEOUT,          5000). % milliseconds
-define(DEFAULT_MULTIPART_HEADER_LENGTH_READ,     64000).
-define(DEFAULT_MULTIPART_BODY_TIMEOUT,           15000). % milliseconds
-define(DEFAULT_MULTIPART_BODY_LENGTH_READ,     8000000).
-define(DEFAULT_MULTIPART_DESTINATION_LOCK,        true).
-define(DEFAULT_WEBSOCKET_TIMEOUT,             infinity). % milliseconds
-define(DEFAULT_WEBSOCKET_OUTPUT,                binary).
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
-define(DEFAULT_CONTENT_TYPE,                 undefined).
        % Provide the exact value to set if no
        % response headers were provided.
        % If this value is not set and no response headers
        % were provided, the value is guessed based on the
        % file extension.
-define(DEFAULT_CONTENT_TYPES_ACCEPTED,       undefined). % see below:
        % Provide a list of content types strings
        % (list of integers or binaries) which must match the
        % HTTP request "Accept" header value
-define(DEFAULT_CONTENT_SECURITY_POLICY,      undefined).
        % Provide the exact value to set
        % if it was not already provided and the content-type is text/html
        % (e.g., "default-src 'self'").
-define(DEFAULT_CONTENT_SECURITY_POLICY_REPORT_ONLY,
                                              undefined).
        % Provide the exact value to set
        % if it was not already provided and the content-type is text/html
-define(DEFAULT_SET_X_FORWARDED_FOR,              false). % if it is missing
-define(DEFAULT_SET_X_XSS_PROTECTION,             false).
        % If true, then use "0" for the value
        % if it was not already provided and the content-type is text/html.
-define(DEFAULT_SET_X_CONTENT_TYPE_OPTIONS,       false).
        % If true, then use "nosniff" for the value
        % if it was not already provided (when "content-type" is set).
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
-define(DEFAULT_UPDATE_DELAY,                         5). % see below:
        % The number of seconds before applying service configuration
        % updates (e.g., changes with cloudi_service_api:services_update/2)
        % for the values timeout_async and timeout_sync.

-record(state,
    {
        listener :: pid(),
        service :: pid(),
        handler_state :: #cowboy_state{}
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
        {reuse_port,                    ?DEFAULT_REUSE_PORT},
        {backlog,                       ?DEFAULT_BACKLOG},
        {nodelay,                       ?DEFAULT_NODELAY},
        {recv_timeout,                  ?DEFAULT_RECV_TIMEOUT},
        {body_timeout,                  ?DEFAULT_BODY_TIMEOUT},
        {body_length_read,              ?DEFAULT_BODY_LENGTH_READ},
        {multipart_header_timeout,      ?DEFAULT_MULTIPART_HEADER_TIMEOUT},
        {multipart_header_length_read,  ?DEFAULT_MULTIPART_HEADER_LENGTH_READ},
        {multipart_body_timeout,        ?DEFAULT_MULTIPART_BODY_TIMEOUT},
        {multipart_body_length_read,    ?DEFAULT_MULTIPART_BODY_LENGTH_READ},
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
        {content_security_policy,       ?DEFAULT_CONTENT_SECURITY_POLICY},
        {content_security_policy_report_only,
         ?DEFAULT_CONTENT_SECURITY_POLICY_REPORT_ONLY},
        {set_x_forwarded_for,           ?DEFAULT_SET_X_FORWARDED_FOR},
        {set_x_xss_protection,          ?DEFAULT_SET_X_XSS_PROTECTION},
        {set_x_content_type_options,    ?DEFAULT_SET_X_CONTENT_TYPE_OPTIONS},
        {status_code_timeout,           ?DEFAULT_STATUS_CODE_TIMEOUT},
        {query_get_format,              ?DEFAULT_QUERY_GET_FORMAT},
        {use_websockets,                ?DEFAULT_USE_WEBSOCKETS},
        {use_host_prefix,               ?DEFAULT_USE_HOST_PREFIX},
        {use_client_ip_prefix,          ?DEFAULT_USE_CLIENT_IP_PREFIX},
        {use_x_method_override,         ?DEFAULT_USE_X_METHOD_OVERRIDE},
        {use_method_suffix,             ?DEFAULT_USE_METHOD_SUFFIX},
        {update_delay,                  ?DEFAULT_UPDATE_DELAY}],
    [Interface, Port, ReusePort0, Backlog, NoDelay, RecvTimeout,
     BodyTimeout, BodyLengthRead, MultipartHeaderTimeout,
     MultipartHeaderLengthRead,
     MultipartBodyTimeout, MultipartBodyLengthRead,
     MultipartDestinationLock, WebSocketTimeout, WebSocketOutputType,
     WebSocketConnect0, WebSocketDisconnect0,
     WebSocketConnectAsync0, WebSocketConnectSync,
     WebSocketDisconnectAsync0, WebSocketDisconnectSync,
     WebSocketPing, WebSocketProtocol0, WebSocketNameUnique,
     WebSocketSubscriptions0, SSL, Compress,
     MaxConnections, MaxEmptyLines, MaxHeaderNameLength, MaxHeaderValueLength,
     MaxHeaders, MaxKeepAlive, MaxRequestLineLength, OutputType,
     ContentTypeForced0, ContentTypesAccepted0, ContentSecurityPolicy0,
     ContentSecurityPolicyReport0,
     SetXForwardedFor, SetXXSSProtection, SetXContentTypeOptions,
     StatusCodeTimeout, QueryGetFormat, UseWebSockets,
     UseHostPrefix, UseClientIpPrefix, UseXMethodOverride, UseMethodSuffix,
     UpdateDelaySeconds] =
        cloudi_proplists:take_values(Defaults, Args),
    OSType = os:type(),
    CountProcess = cloudi_service:process_count(Dispatcher),
    CountProcess = cloudi_service:process_count_max(Dispatcher),
    CountProcess = cloudi_service:process_count_min(Dispatcher),
    true = is_integer(Port),
    ReusePortN = if
        ReusePort0 =:= undefined ->
            CountProcess > 1;
        is_boolean(ReusePort0) ->
            ReusePort0
    end,
    ReusePortOptL = if
        ReusePortN =:= true ->
            ReusePortOpt = socket_option_reuseport(OSType),
            {raw, _, _, _} = ReusePortOpt,
            [ReusePortOpt,
             {reuseaddr, true}];
        ReusePortN =:= false ->
            []
    end,
    true = is_integer(Backlog),
    true = is_boolean(NoDelay),
    true = is_integer(RecvTimeout) andalso (RecvTimeout > 0),
    true = is_integer(BodyTimeout) andalso (BodyTimeout > 0),
    true = is_integer(BodyLengthRead) andalso (BodyLengthRead > 0),
    true = is_integer(MultipartHeaderTimeout) andalso
           (MultipartHeaderTimeout > 0),
    true = is_integer(MultipartHeaderLengthRead) andalso
           (MultipartHeaderLengthRead > 0),
    true = is_integer(MultipartBodyTimeout) andalso
           (MultipartBodyTimeout > 0),
    true = is_integer(MultipartBodyLengthRead) andalso
           (MultipartBodyLengthRead > 0),
    true = is_boolean(MultipartDestinationLock),
    true = (WebSocketTimeout =:= infinity) orelse
           (is_integer(WebSocketTimeout) andalso (WebSocketTimeout > 0)),
    true = (WebSocketOutputType =:= binary) orelse
           (WebSocketOutputType =:= text),
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
    WebSocketConnectN = if
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
    WebSocketDisconnectN = if
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
    WebSocketProtocolN = cloudi_args_type:
                         function_optional(WebSocketProtocol0, 2),
    true = is_boolean(WebSocketNameUnique),
    WebSocketSubscriptionsN = if
        WebSocketSubscriptions0 == [] ->
            undefined;
        is_list(WebSocketSubscriptions0) ->
            websocket_subscriptions_lookup(WebSocketSubscriptions0, Prefix)
    end,
    CompressThreshold = if
        Compress =:= true ->
            300; % bytes
        Compress =:= false ->
            18446744073709551615
    end,
    true = is_integer(MaxConnections),
    true = is_integer(MaxEmptyLines),
    true = is_integer(MaxHeaderNameLength),
    true = is_integer(MaxHeaderValueLength),
    true = is_integer(MaxHeaders),
    true = is_integer(MaxKeepAlive),
    true = is_integer(MaxRequestLineLength),
    true = (OutputType =:= external) orelse (OutputType =:= internal) orelse
           (OutputType =:= list) orelse (OutputType =:= binary),
    ContentTypeForcedN = if
        ContentTypeForced0 =:= undefined ->
            undefined;
        is_list(ContentTypeForced0),
        is_integer(hd(ContentTypeForced0)) ->
            erlang:list_to_binary(ContentTypeForced0);
        is_binary(ContentTypeForced0),
        ContentTypeForced0 /= <<>> ->
            ContentTypeForced0
    end,
    ContentTypesAcceptedN = if
        ContentTypesAccepted0 =:= undefined ->
            undefined;
        is_list(ContentTypesAccepted0) ->
            content_types_accepted_pattern(ContentTypesAccepted0)
    end,
    ContentSecurityPolicyN = if
        ContentSecurityPolicy0 =:= undefined ->
            undefined;
        is_list(ContentSecurityPolicy0),
        is_integer(hd(ContentSecurityPolicy0)) ->
            erlang:list_to_binary(ContentSecurityPolicy0);
        is_binary(ContentSecurityPolicy0),
        ContentSecurityPolicy0 /= <<>> ->
            ContentSecurityPolicy0
    end,
    ContentSecurityPolicyReportN = if
        ContentSecurityPolicyReport0 =:= undefined ->
            undefined;
        is_list(ContentSecurityPolicyReport0),
        is_integer(hd(ContentSecurityPolicyReport0)) ->
            erlang:list_to_binary(ContentSecurityPolicyReport0);
        is_binary(ContentSecurityPolicyReport0),
        ContentSecurityPolicyReport0 /= <<>> ->
            ContentSecurityPolicyReport0
    end,
    true = is_boolean(SetXForwardedFor),
    true = is_boolean(SetXXSSProtection),
    true = is_boolean(SetXContentTypeOptions),
    true = is_integer(StatusCodeTimeout) andalso
           (StatusCodeTimeout > 100) andalso
           (StatusCodeTimeout =< 599),
    true = (QueryGetFormat =:= raw) orelse
           (QueryGetFormat =:= text_pairs),
    true = (is_boolean(UseWebSockets) orelse
            (UseWebSockets =:= exclusively)),
    true = is_boolean(UseHostPrefix),
    true = is_boolean(UseClientIpPrefix),
    true = ((UseXMethodOverride =:= true) andalso
            (UseMethodSuffix =:= true)) orelse
           (UseXMethodOverride =:= false),
    true = is_boolean(UseMethodSuffix),
    true = is_integer(UpdateDelaySeconds) andalso
           (UpdateDelaySeconds > 0) andalso (UpdateDelaySeconds =< 4294967),
    false = cloudi_service_name:pattern(Prefix),
    {_, Scope} = lists:keyfind(groups_scope, 1,
                               cloudi_service:context_options(Dispatcher)),
    HandlerState = #cowboy_state{
        dispatcher = cloudi_service:dispatcher(Dispatcher),
        timeout_async = cloudi_service:timeout_async(Dispatcher),
        timeout_sync = cloudi_service:timeout_sync(Dispatcher),
        scope = Scope,
        prefix = Prefix,
        timeout_body = BodyTimeout,
        timeout_part_header = MultipartHeaderTimeout,
        timeout_part_body = MultipartBodyTimeout,
        timeout_websocket = WebSocketTimeout,
        length_body_read = BodyLengthRead,
        length_part_header_read = MultipartHeaderLengthRead,
        length_part_body_read = MultipartBodyLengthRead,
        parts_destination_lock = MultipartDestinationLock,
        output_type = OutputType,
        content_type_forced = ContentTypeForcedN,
        content_types_accepted = ContentTypesAcceptedN,
        content_security_policy = ContentSecurityPolicyN,
        content_security_policy_report = ContentSecurityPolicyReportN,
        set_x_forwarded_for = SetXForwardedFor,
        set_x_xss_protection = SetXXSSProtection,
        set_x_content_type_options = SetXContentTypeOptions,
        status_code_timeout = StatusCodeTimeout,
        query_get_format = QueryGetFormat,
        websocket_output_type = WebSocketOutputType,
        websocket_connect = WebSocketConnectN,
        websocket_disconnect = WebSocketDisconnectN,
        websocket_ping = WebSocketPing,
        websocket_protocol = WebSocketProtocolN,
        websocket_name_unique = WebSocketNameUnique,
        websocket_subscriptions = WebSocketSubscriptionsN,
        use_websockets = UseWebSockets,
        use_host_prefix = UseHostPrefix,
        use_client_ip_prefix = UseClientIpPrefix,
        use_x_method_override = UseXMethodOverride,
        use_method_suffix = UseMethodSuffix},
    Service = cloudi_service:self(Dispatcher),
    erlang:send_after(UpdateDelaySeconds * 1000, Service,
                      {update, UpdateDelaySeconds}),
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
            SSLOptsNew = environment_transform_ssl_options(SSLOpts,
                                                           Environment),
            cloudi_x_cowboy:start_tls(
                Service, % Ref
                #{
                    % Transport options
                    socket_opts => ReusePortOptL ++ [
                        {ip, Interface},
                        {port, Port},
                        {backlog, Backlog},
                        {nodelay, NoDelay},
                        {certfile, CertFile} |
                        SSLOptsNew
                    ],
                    max_connections => MaxConnections,
                    connection_type => supervisor,
                    num_acceptors => 10
                },
                #{
                    % Protocol options
                    env => #{dispatch => cowboy_dispatch(HandlerState)},
                    compress_threshold =>  CompressThreshold,
                    max_empty_lines => MaxEmptyLines,
                    max_header_name_length => MaxHeaderNameLength,
                    max_header_value_length => MaxHeaderValueLength,
                    max_headers => MaxHeaders,
                    max_keepalive => MaxKeepAlive,
                    max_request_line_length => MaxRequestLineLength,
                    idle_timeout => RecvTimeout
                }
            );
        SSL =:= false ->
            cloudi_x_cowboy:start_clear(
                Service, % Ref
                #{
                    % Transport options
                    socket_opts => ReusePortOptL ++ [
                        {ip, Interface},
                        {port, Port},
                        {backlog, Backlog},
                        {nodelay, NoDelay}
                    ],
                    max_connections => MaxConnections,
                    connection_type => supervisor,
                    num_acceptors => 10
                },
                #{
                    % Protocol options
                    env => #{dispatch => cowboy_dispatch(HandlerState)},
                    compress_threshold =>  CompressThreshold,
                    max_empty_lines => MaxEmptyLines,
                    max_header_name_length => MaxHeaderNameLength,
                    max_header_value_length => MaxHeaderValueLength,
                    max_headers => MaxHeaders,
                    max_keepalive => MaxKeepAlive,
                    max_request_line_length => MaxRequestLineLength,
                    idle_timeout => RecvTimeout
                }
            )
    end,
    {ok, #state{listener = ListenerPid,
                service = Service,
                handler_state = HandlerState}}.

cloudi_service_handle_info({update, UpdateDelaySeconds},
                           #state{service = Service,
                                  handler_state = HandlerState} = State,
                           Dispatcher) ->
    % The timeout_async and timeout_sync service configuration values
    % need to be updated within the cowboy_state record used for new
    % connection processes, after an update has occurred using the
    % CloudI Service API function services_update
    #cowboy_state{timeout_async = TimeoutAsync,
                  timeout_sync = TimeoutSync} = HandlerState,
    ContextOptions = cloudi_service:context_options(Dispatcher),
    {_, TimeoutAsyncCurrent} = lists:keyfind(timeout_async, 1, ContextOptions),
    {_, TimeoutSyncCurrent} = lists:keyfind(timeout_sync, 1, ContextOptions),
    HandlerStateNew = if
        TimeoutAsync == TimeoutAsyncCurrent,
        TimeoutSync == TimeoutSyncCurrent ->
            HandlerState;
        true ->
            HandlerStateNext = HandlerState#cowboy_state{
                timeout_async = TimeoutAsyncCurrent,
                timeout_sync = TimeoutSyncCurrent},
            ok = cloudi_x_cowboy:set_env(Service,
                                         dispatch,
                                         cowboy_dispatch(HandlerStateNext)),
            HandlerStateNext
    end,
    erlang:send_after(UpdateDelaySeconds * 1000, Service,
                      {update, UpdateDelaySeconds}),
    {noreply, State#state{handler_state = HandlerStateNew}};
cloudi_service_handle_info(Request, State, _Dispatcher) ->
    {stop, cloudi_string:format("Unknown info \"~w\"", [Request]), State}.

cloudi_service_terminate(_Reason, _Timeout, undefined) ->
    ok;
cloudi_service_terminate(_Reason, _Timeout,
                         #state{service = Service}) ->
    _ = (catch cloudi_x_cowboy:stop_listener(Service)),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

cowboy_dispatch(HandlerState) ->
    cloudi_x_cowboy_router:compile([
        %% {Host, list({Path, Handler, Opts})}
        {'_', [{'_', cloudi_http_cowboy_handler, HandlerState}]}
    ]).

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
            LookupNew = cloudi_x_trie:update(Prefix ++ PatternSuffix,
                                             fun(Functions) ->
                                                 [F | Functions]
                                             end, [F], Lookup),
            websocket_subscriptions_lookup(WebSocketSubscriptions,
                                           LookupNew, Prefix);
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
                     (cloudi_service_name:pattern(Name) =:= false))),
            F = if
                ParametersAllowed =:= true ->
                    fun(Parameters) ->
                        cloudi_service_name:
                        new(Name,
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
            LookupNew = cloudi_x_trie:update(Prefix ++ PatternSuffix,
                                             fun(Functions) ->
                                                 [F | Functions]
                                             end, [F], Lookup),
            websocket_subscriptions_lookup(WebSocketSubscriptions,
                                           LookupNew, Prefix)
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
    FilePathNew = cloudi_environment:transform(FilePath, Environment),
    environment_transform_ssl_options(SSLOpts, [{K, FilePathNew} | Output],
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
    LNew = case binary:split(ContentType, <<"/">>) of
        [<<"*">>, <<"*">>] ->
            lists:umerge(L, [ContentType]);
        [_, <<"*">>] ->
            lists:umerge(L, [ContentType]);
        [Type, _] ->
            lists:umerge(L, [<<Type/binary,<<"/*">>/binary>>, ContentType])
    end,
    content_types_accepted_pattern(ContentTypesAccepted, LNew).

socket_option_reuseport({unix, linux}) ->
    % SO_REUSEPORT requires Linux 3.9 or higher
    {raw, 1, 15, <<1:32/unsigned-integer-native>>};
socket_option_reuseport({unix, freebsd}) ->
    % SO_REUSEPORT_LB requires FreeBSD 12.0 or higher
    {raw, 16#ffff, 16#00010000, <<1:32/unsigned-integer-native>>};
socket_option_reuseport(_) ->
    undefined.
