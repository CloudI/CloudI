%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI HTTP Integration==
%%% Uses the cloudi_x_cowboy Erlang HTTP Server.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2012-2013, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2012-2013 Michael Truog
%%% @version 1.3.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_http_cowboy).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_service).

%% external interface
-export([close/1]).

%% cloudi_service callbacks
-export([cloudi_service_init/3,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/2]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").
-include("cloudi_http_cowboy_handler.hrl").

-define(DEFAULT_INTERFACE,            {127,0,0,1}). % ip address
-define(DEFAULT_PORT,                        8080).
-define(DEFAULT_BACKLOG,                      128).
-define(DEFAULT_NODELAY,                     true).
-define(DEFAULT_RECV_TIMEOUT,           30 * 1000). % milliseconds
-define(DEFAULT_WEBSOCKET_TIMEOUT,       infinity). % milliseconds
-define(DEFAULT_SSL,                        false).
-define(DEFAULT_COMPRESS,                   false).
-define(DEFAULT_MAX_CONNECTIONS,             4096).
-define(DEFAULT_MAX_EMPTY_LINES,                5).
-define(DEFAULT_MAX_HEADER_NAME_LENGTH,        64).
-define(DEFAULT_MAX_HEADER_VALUE_LENGTH,     4096).
-define(DEFAULT_MAX_HEADERS,                  100).
-define(DEFAULT_MAX_KEEPALIVE,                100). % requests in session
-define(DEFAULT_MAX_REQUEST_LINE_LENGTH,     4096).
-define(DEFAULT_OUTPUT,                  external).
-define(DEFAULT_CONTENT_TYPE,           undefined). % force a content type
-define(DEFAULT_CONTENT_TYPES_ACCEPTED, undefined).
-define(DEFAULT_STATUS_CODE_TIMEOUT,          504). % "Gateway Timeout"
-define(DEFAULT_SET_X_FORWARDED_FOR,        false). % if it is missing
-define(DEFAULT_USE_WEBSOCKETS,             false).
-define(DEFAULT_USE_HOST_PREFIX,            false). % for virtual hosts
-define(DEFAULT_USE_CLIENT_IP_PREFIX,       false).
-define(DEFAULT_USE_METHOD_SUFFIX,           true). % get/post/etc. name suffix


-record(state,
    {
        listener,
        service,
        requests = dict:new()
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------


%%-------------------------------------------------------------------------
%% @doc
%% ===Close a cowboy handler socket pid which represents a live connection.===
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

cloudi_service_init(Args, Prefix, Dispatcher) ->
    Defaults = [
        {ip,                       ?DEFAULT_INTERFACE},
        {port,                     ?DEFAULT_PORT},
        {backlog,                  ?DEFAULT_BACKLOG},
        {nodelay,                  ?DEFAULT_NODELAY},
        {recv_timeout,             ?DEFAULT_RECV_TIMEOUT},
        {websocket_timeout,        ?DEFAULT_WEBSOCKET_TIMEOUT},
        {ssl,                      ?DEFAULT_SSL},
        {compress,                 ?DEFAULT_COMPRESS},
        {max_connections,          ?DEFAULT_MAX_CONNECTIONS},
        {max_empty_lines,          ?DEFAULT_MAX_EMPTY_LINES},
        {max_header_name_length,   ?DEFAULT_MAX_HEADER_NAME_LENGTH},
        {max_header_value_length,  ?DEFAULT_MAX_HEADER_VALUE_LENGTH},
        {max_headers,              ?DEFAULT_MAX_HEADERS},
        {max_keepalive,            ?DEFAULT_MAX_KEEPALIVE},
        {max_request_line_length,  ?DEFAULT_MAX_REQUEST_LINE_LENGTH},
        {output,                   ?DEFAULT_OUTPUT},
        {content_type,             ?DEFAULT_CONTENT_TYPE},
        {content_types_accepted,   ?DEFAULT_CONTENT_TYPES_ACCEPTED},
        {set_x_forwarded_for,      ?DEFAULT_SET_X_FORWARDED_FOR},
        {status_code_timeout,      ?DEFAULT_STATUS_CODE_TIMEOUT},
        {use_websockets,           ?DEFAULT_USE_WEBSOCKETS},
        {use_host_prefix,          ?DEFAULT_USE_HOST_PREFIX},
        {use_client_ip_prefix,     ?DEFAULT_USE_CLIENT_IP_PREFIX},
        {use_method_suffix,        ?DEFAULT_USE_METHOD_SUFFIX}],
    [Interface, Port, Backlog, NoDelay, RecvTimeout, WebsocketTimeout,
     SSL, Compress, MaxConnections,
     MaxEmptyLines, MaxHeaderNameLength, MaxHeaderValueLength,
     MaxHeaders, MaxKeepAlive, MaxRequestLineLength,
     OutputType, ContentTypeForced0, ContentTypesAccepted0, SetXForwardedFor,
     StatusCodeTimeout, UseWebSockets, UseHostPrefix, UseClientIpPrefix,
     UseMethodSuffix] =
        cloudi_proplists:take_values(Defaults, Args),
    true = is_integer(Port),
    true = is_integer(Backlog),
    true = is_boolean(NoDelay),
    true = is_integer(RecvTimeout) andalso (RecvTimeout > 0),
    true = (WebsocketTimeout =:= infinity) orelse
           (is_integer(WebsocketTimeout) andalso (WebsocketTimeout > 0)),
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
    true = is_integer(StatusCodeTimeout),
    true = is_boolean(UseWebSockets),
    true = is_boolean(UseHostPrefix),
    true = is_boolean(UseClientIpPrefix),
    true = is_boolean(UseMethodSuffix),
    Service = cloudi_service:self(Dispatcher),
    TimeoutAsync = cloudi_service:timeout_async(Dispatcher),
    Dispatch = cloudi_x_cowboy_router:compile([
        %% {Host, list({Path, Handler, Opts})}
        {'_', [{'_', cloudi_http_cowboy_handler,
                #cowboy_state{service = Service,
                              timeout_async = TimeoutAsync,
                              prefix = Prefix,
                              timeout_websocket = WebsocketTimeout,
                              output_type = OutputType,
                              content_type_forced = ContentTypeForced1,
                              content_types_accepted = ContentTypesAccepted1,
                              set_x_forwarded_for = SetXForwardedFor,
                              status_code_timeout = StatusCodeTimeout,
                              use_websockets = UseWebSockets,
                              use_host_prefix = UseHostPrefix,
                              use_client_ip_prefix = UseClientIpPrefix,
                              use_method_suffix = UseMethodSuffix,
                              content_type_lookup = content_type_lookup()}}]}
    ]),
    {ok, ListenerPid} = if
        is_list(SSL) ->
            {value,
             {certfile, CertFile}, SSLOpts} = lists:keytake(certfile, 1, SSL),
            [] = cloudi_proplists:delete_all([cacertfile,
                                              ciphers,
                                              keyfile,
                                              password,
                                              verify], SSLOpts),
            cloudi_x_cowboy:start_https(
                Service, % Ref
                100, % Number of acceptor processes
                [{ip, Interface},
                 {port, Port},
                 {backlog, Backlog},
                 {nodelay, NoDelay},
                 {max_connections, MaxConnections},
                 {certfile, CertFile}] ++
                SSLOpts, % Transport options
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
            cloudi_x_cowboy:start_http(
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

cloudi_service_handle_info({cowboy_request, HandlerPid, NameOutgoing,
                            RequestInfo, Request},
                           #state{requests = Requests} = State, Dispatcher) ->
    case cloudi_service:send_async_active(Dispatcher, NameOutgoing,
                                          RequestInfo, Request,
                                          undefined, undefined) of
        {ok, TransId} ->
            {noreply, State#state{requests = dict:store(TransId, HandlerPid,
                                                        Requests)}};
        {error, Reason} ->
            HandlerPid ! {cowboy_error, Reason},
            {noreply, State}
    end;

cloudi_service_handle_info({'return_async_active', _Name, _Pattern,
                            ResponseInfo, Response, _Timeout, TransId},
                           #state{requests = Requests} = State, _) ->
    HandlerPid = dict:fetch(TransId, Requests),
    HandlerPid ! {cowboy_response, ResponseInfo, Response},
    {noreply, State#state{requests = dict:erase(TransId, Requests)}};

cloudi_service_handle_info({'timeout_async_active', TransId},
                           #state{requests = Requests} = State, _) ->
    HandlerPid = dict:fetch(TransId, Requests),
    HandlerPid ! {cowboy_error, timeout},
    {noreply, State#state{requests = dict:erase(TransId, Requests)}};

cloudi_service_handle_info(Request, State, _) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(_, #state{service = Service,
                                   requests = Requests}) ->
    cloudi_x_cowboy:stop_listener(Service),
    dict:map(fun(_, HandlerPid) ->
        HandlerPid ! {cowboy_error, timeout}
    end, Requests),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

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

% static content type detection
content_type_lookup() ->
    cloudi_x_trie:new([
        {".txt",     {request, <<"text/plain">>}},
        {".json",    {request, <<"application/json">>}},
        {".xml",     {request, <<"text/xml">>}},
        {".csv",     {request, <<"text/csv">>}},
        {".htm",     {request, <<"text/html">>}},
        {".html",    {request, <<"text/html">>}},
        {".exe",     {attachment, <<"application/octet-stream">>}},
        {".pdf",     {attachment, <<"application/pdf">>}},
        {".rtf",     {attachment, <<"application/rtf">>}},
        {".ppt",     {attachment, <<"application/vnd.ms-powerpoint">>}},
        {".tgz",     {attachment, <<"application/x-compressed">>}},
        {".tar",     {attachment, <<"application/x-tar">>}},
        {".zip",     {attachment, <<"application/zip">>}},
        {".mp3",     {attachment, <<"audio/mpeg">>}},
        {".wav",     {attachment, <<"audio/x-wav">>}},
        {".bmp",     {attachment, <<"image/bmp">>}},
        {".ram",     {attachment, <<"audio/x-pn-realaudio">>}},
        {".gif",     {attachment, <<"image/gif">>}},
        {".jpe",     {attachment, <<"image/jpeg">>}},
        {".jpeg",    {attachment, <<"image/jpeg">>}},
        {".jpg",     {attachment, <<"image/jpeg">>}},
        {".tif",     {attachment, <<"image/tiff">>}},
        {".tiff",    {attachment, <<"image/tiff">>}},
        {".mp2",     {attachment, <<"video/mpeg">>}},
        {".mpa",     {attachment, <<"video/mpeg">>}},
        {".mpe",     {attachment, <<"video/mpeg">>}},
        {".mpeg",    {attachment, <<"video/mpeg">>}},
        {".mpg",     {attachment, <<"video/mpeg">>}},
        {".mov",     {attachment, <<"video/quicktime">>}},
        {".avi",     {attachment, <<"video/x-msvideo">>}},
        {".evy",     {attachment, <<"application/envoy">>}},
        {".fif",     {attachment, <<"application/fractals">>}},
        {".spl",     {attachment, <<"application/futuresplash">>}},
        {".hta",     {attachment, <<"application/hta">>}},
        {".acx",     {attachment, <<"application/internet-property-stream">>}},
        {".hqx",     {attachment, <<"application/mac-binhex40">>}},
        {".dot",     {attachment, <<"application/msword">>}},
        {".bin",     {attachment, <<"application/octet-stream">>}},
        {".class",   {attachment, <<"application/octet-stream">>}},
        {".dms",     {attachment, <<"application/octet-stream">>}},
        {".lha",     {attachment, <<"application/octet-stream">>}},
        {".lzh",     {attachment, <<"application/octet-stream">>}},
        {".oda",     {attachment, <<"application/oda">>}},
        {".axs",     {attachment, <<"application/olescript">>}},
        {".prf",     {attachment, <<"application/pics-rules">>}},
        {".p10",     {attachment, <<"application/pkcs10">>}},
        {".crl",     {attachment, <<"application/pkix-crl">>}},
        {".ai",      {attachment, <<"application/postscript">>}},
        {".eps",     {attachment, <<"application/postscript">>}},
        {".ps",      {attachment, <<"application/postscript">>}},
        {".setpay",  {attachment, <<"application/set-payment-initiation">>}},
        {".setreg",  {attachment, <<"application/set-registration-initiation">>}},
        {".xla",     {attachment, <<"application/vnd.ms-excel">>}},
        {".xlc",     {attachment, <<"application/vnd.ms-excel">>}},
        {".xlm",     {attachment, <<"application/vnd.ms-excel">>}},
        {".xls",     {attachment, <<"application/vnd.ms-excel">>}},
        {".xlt",     {attachment, <<"application/vnd.ms-excel">>}},
        {".xlw",     {attachment, <<"application/vnd.ms-excel">>}},
        {".msg",     {attachment, <<"application/vnd.ms-outlook">>}},
        {".sst",     {attachment, <<"application/vnd.ms-pkicertstore">>}},
        {".cat",     {attachment, <<"application/vnd.ms-pkiseccat">>}},
        {".stl",     {attachment, <<"application/vnd.ms-pkistl">>}},
        {".pot",     {attachment, <<"application/vnd.ms-powerpoint">>}},
        {".pps",     {attachment, <<"application/vnd.ms-powerpoint">>}},
        {".mpp",     {attachment, <<"application/vnd.ms-project">>}},
        {".wcm",     {attachment, <<"application/vnd.ms-works">>}},
        {".wdb",     {attachment, <<"application/vnd.ms-works">>}},
        {".wks",     {attachment, <<"application/vnd.ms-works">>}},
        {".wps",     {attachment, <<"application/vnd.ms-works">>}},
        {".hlp",     {attachment, <<"application/winhlp">>}},
        {".bcpio",   {attachment, <<"application/x-bcpio">>}},
        {".cdf",     {attachment, <<"application/x-cdf">>}},
        {".z",       {attachment, <<"application/x-compress">>}},
        {".cpio",    {attachment, <<"application/x-cpio">>}},
        {".csh",     {attachment, <<"application/x-csh">>}},
        {".dcr",     {attachment, <<"application/x-director">>}},
        {".dir",     {attachment, <<"application/x-director">>}},
        {".dxr",     {attachment, <<"application/x-director">>}},
        {".dvi",     {attachment, <<"application/x-dvi">>}},
        {".gtar",    {attachment, <<"application/x-gtar">>}},
        {".gz",      {attachment, <<"application/x-gzip">>}},
        {".hdf",     {attachment, <<"application/x-hdf">>}},
        {".ins",     {attachment, <<"application/x-internet-signup">>}},
        {".isp",     {attachment, <<"application/x-internet-signup">>}},
        {".iii",     {attachment, <<"application/x-iphone">>}},
        {".js",      {attachment, <<"application/x-javascript">>}},
        {".latex",   {attachment, <<"application/x-latex">>}},
        {".mdb",     {attachment, <<"application/x-msaccess">>}},
        {".crd",     {attachment, <<"application/x-mscardfile">>}},
        {".clp",     {attachment, <<"application/x-msclip">>}},
        {".dll",     {attachment, <<"application/x-msdownload">>}},
        {".m13",     {attachment, <<"application/x-msmediaview">>}},
        {".m14",     {attachment, <<"application/x-msmediaview">>}},
        {".mvb",     {attachment, <<"application/x-msmediaview">>}},
        {".wmf",     {attachment, <<"application/x-msmetafile">>}},
        {".mny",     {attachment, <<"application/x-msmoney">>}},
        {".pub",     {attachment, <<"application/x-mspublisher">>}},
        {".scd",     {attachment, <<"application/x-msschedule">>}},
        {".trm",     {attachment, <<"application/x-msterminal">>}},
        {".wri",     {attachment, <<"application/x-mswrite">>}},
        {".nc",      {attachment, <<"application/x-netcdf">>}},
        {".pma",     {attachment, <<"application/x-perfmon">>}},
        {".pmc",     {attachment, <<"application/x-perfmon">>}},
        {".pml",     {attachment, <<"application/x-perfmon">>}},
        {".pmr",     {attachment, <<"application/x-perfmon">>}},
        {".pmw",     {attachment, <<"application/x-perfmon">>}},
        {".p12",     {attachment, <<"application/x-pkcs12">>}},
        {".pfx",     {attachment, <<"application/x-pkcs12">>}},
        {".p7b",     {attachment, <<"application/x-pkcs7-certificates">>}},
        {".spc",     {attachment, <<"application/x-pkcs7-certificates">>}},
        {".p7r",     {attachment, <<"application/x-pkcs7-certreqresp">>}},
        {".p7c",     {attachment, <<"application/x-pkcs7-mime">>}},
        {".p7m",     {attachment, <<"application/x-pkcs7-mime">>}},
        {".p7s",     {attachment, <<"application/x-pkcs7-signature">>}},
        {".sh",      {attachment, <<"application/x-sh">>}},
        {".shar",    {attachment, <<"application/x-shar">>}},
        {".swf",     {attachment, <<"application/x-shockwave-flash">>}},
        {".sit",     {attachment, <<"application/x-stuffit">>}},
        {".sv4cpio", {attachment, <<"application/x-sv4cpio">>}},
        {".sv4crc",  {attachment, <<"application/x-sv4crc">>}},
        {".tcl",     {attachment, <<"application/x-tcl">>}},
        {".tex",     {attachment, <<"application/x-tex">>}},
        {".texi",    {attachment, <<"application/x-texinfo">>}},
        {".texinfo", {attachment, <<"application/x-texinfo">>}},
        {".roff",    {attachment, <<"application/x-troff">>}},
        {".t",       {attachment, <<"application/x-troff">>}},
        {".tr",      {attachment, <<"application/x-troff">>}},
        {".man",     {attachment, <<"application/x-troff-man">>}},
        {".me",      {attachment, <<"application/x-troff-me">>}},
        {".ms",      {attachment, <<"application/x-troff-ms">>}},
        {".ustar",   {attachment, <<"application/x-ustar">>}},
        {".src",     {attachment, <<"application/x-wais-source">>}},
        {".cer",     {attachment, <<"application/x-x509-ca-cert">>}},
        {".crt",     {attachment, <<"application/x-x509-ca-cert">>}},
        {".der",     {attachment, <<"application/x-x509-ca-cert">>}},
        {".pko",     {attachment, <<"application/ynd.ms-pkipko">>}},
        {".au",      {attachment, <<"audio/basic">>}},
        {".snd",     {attachment, <<"audio/basic">>}},
        {".mid",     {attachment, <<"audio/mid">>}},
        {".rmi",     {attachment, <<"audio/mid">>}},
        {".aif",     {attachment, <<"audio/x-aiff">>}},
        {".aifc",    {attachment, <<"audio/x-aiff">>}},
        {".aiff",    {attachment, <<"audio/x-aiff">>}},
        {".m3u",     {attachment, <<"audio/x-mpegurl">>}},
        {".ra",      {attachment, <<"audio/x-pn-realaudio">>}},
        {".cod",     {attachment, <<"image/cis-cod">>}},
        {".ief",     {attachment, <<"image/ief">>}},
        {".jfif",    {attachment, <<"image/pipeg">>}},
        {".svg",     {attachment, <<"image/svg+xml">>}},
        {".ras",     {attachment, <<"image/x-cmu-raster">>}},
        {".cmx",     {attachment, <<"image/x-cmx">>}},
        {".ico",     {attachment, <<"image/x-icon">>}},
        {".pnm",     {attachment, <<"image/x-portable-anymap">>}},
        {".pbm",     {attachment, <<"image/x-portable-bitmap">>}},
        {".pgm",     {attachment, <<"image/x-portable-graymap">>}},
        {".ppm",     {attachment, <<"image/x-portable-pixmap">>}},
        {".rgb",     {attachment, <<"image/x-rgb">>}},
        {".xbm",     {attachment, <<"image/x-xbitmap">>}},
        {".xpm",     {attachment, <<"image/x-xpixmap">>}},
        {".xwd",     {attachment, <<"image/x-xwindowdump">>}},
        {".mht",     {attachment, <<"message/rfc822">>}},
        {".mhtml",   {attachment, <<"message/rfc822">>}},
        {".nws",     {attachment, <<"message/rfc822">>}},
        {".css",     {attachment, <<"text/css">>}},
        {".323",     {attachment, <<"text/h323">>}},
        {".stm",     {attachment, <<"text/html">>}},
        {".uls",     {attachment, <<"text/iuls">>}},
        {".bas",     {attachment, <<"text/plain">>}},
        {".c",       {attachment, <<"text/plain">>}},
        {".h",       {attachment, <<"text/plain">>}},
        {".rtx",     {attachment, <<"text/richtext">>}},
        {".sct",     {attachment, <<"text/scriptlet">>}},
        {".tsv",     {attachment, <<"text/tab-separated-values">>}},
        {".htt",     {attachment, <<"text/webviewhtml">>}},
        {".htc",     {attachment, <<"text/x-component">>}},
        {".etx",     {attachment, <<"text/x-setext">>}},
        {".vcf",     {attachment, <<"text/x-vcard">>}},
        {".mpv2",    {attachment, <<"video/mpeg">>}},
        {".qt",      {attachment, <<"video/quicktime">>}},
        {".lsf",     {attachment, <<"video/x-la-asf">>}},
        {".lsx",     {attachment, <<"video/x-la-asf">>}},
        {".asf",     {attachment, <<"video/x-ms-asf">>}},
        {".asr",     {attachment, <<"video/x-ms-asf">>}},
        {".asx",     {attachment, <<"video/x-ms-asf">>}},
        {".movie",   {attachment, <<"video/x-sgi-movie">>}},
        {".flr",     {attachment, <<"x-world/x-vrml">>}},
        {".vrml",    {attachment, <<"x-world/x-vrml">>}},
        {".wrl",     {attachment, <<"x-world/x-vrml">>}},
        {".wrz",     {attachment, <<"x-world/x-vrml">>}},
        {".xaf",     {attachment, <<"x-world/x-vrml">>}},
        {".xof",     {attachment, <<"x-world/x-vrml">>}}
        ]).

