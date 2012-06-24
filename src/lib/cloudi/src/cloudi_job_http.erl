%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI HTTP Integration==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2011-2012, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011-2012 Michael Truog
%%% @version 0.2.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_job_http).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_job).

%% external interface

%% cloudi_job callbacks
-export([cloudi_job_init/3,
         cloudi_job_handle_request/11,
         cloudi_job_handle_info/3,
         cloudi_job_terminate/2]).

-include("cloudi_logger.hrl").
-include("cloudi_constants.hrl").

-define(DEFAULT_INTERFACE,       {127,0,0,1}). % ip address
-define(DEFAULT_PORT,                   8080).
-define(DEFAULT_BACKLOG,                 128).
-define(DEFAULT_RECV_TIMEOUT,      30 * 1000). % milliseconds
-define(DEFAULT_SSL,                   false).
-define(DEFAULT_COMPRESS,              false).
-define(DEFAULT_WS_AUTOEXIT,            true).
-define(DEFAULT_MAX_CONNECTIONS,        4096).
-define(DEFAULT_OUTPUT,               binary).
-define(DEFAULT_CONTENT_TYPE,      undefined). % force a content type
-define(DEFAULT_USE_HOST_PREFIX,       false). % for virtual hosts
-define(DEFAULT_USE_METHOD_SUFFIX,      true). % get/post/etc. as a name suffix

-record(state,
    {
        process,         % http server process
        output_type,
        default_content_type,
        content_type_lookup = content_type_lookup(),
        requests = dict:new()
    }).

-record(request_data,
    {
        name_incoming,
        name_outgoing,
        request_info,
        request,
        request_pid
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_job
%%%------------------------------------------------------------------------

cloudi_job_init(Args, _Prefix, Dispatcher) ->
    Defaults = [
        {ip,                     ?DEFAULT_INTERFACE},
        {port,                   ?DEFAULT_PORT},
        {backlog,                ?DEFAULT_BACKLOG},
        {recv_timeout,           ?DEFAULT_RECV_TIMEOUT},
        {ssl,                    ?DEFAULT_SSL},
        {compress,               ?DEFAULT_COMPRESS},
        {ws_autoexit,            ?DEFAULT_WS_AUTOEXIT},
        {max_connections,        ?DEFAULT_MAX_CONNECTIONS},
        {output,                 ?DEFAULT_OUTPUT},
        {content_type,           ?DEFAULT_CONTENT_TYPE},
        {use_host_prefix,        ?DEFAULT_USE_HOST_PREFIX},
        {use_method_suffix,      ?DEFAULT_USE_METHOD_SUFFIX}],
    [Interface, Port, Backlog, RecvTimeout, SSL, Compress, WsAutoExit,
     MaxConnections,
     OutputType, DefaultContentType, UseHostPrefix, UseMethodSuffix] =
        cloudi_proplists:take_values(Defaults, Args),
    Job = cloudi_job:self(Dispatcher),
    Loop = fun(HttpRequest) ->
        http_request(OutputType, UseHostPrefix, UseMethodSuffix,
                     HttpRequest, Job)
    end,
    case misultin:start_link([{ip, Interface},
                              {port, Port},
                              {backlog, Backlog},
                              {recv_timeout, RecvTimeout},
                              {ssl, SSL},
                              {compress, Compress},
                              {ws_autoexit, WsAutoExit},
                              {max_connections, MaxConnections},
                              {loop, Loop},
                              {name, false}]) of
        {ok, Process} ->
            {ok, #state{process = Process,
                        output_type = OutputType,
                        default_content_type = DefaultContentType}};
        {error, Reason} ->
            {stop, Reason}
    end.

cloudi_job_handle_request(_Type, _Name, _Pattern, _RequestInfo, _Request,
                          _Timeout, _Priority, _TransId, _Pid,
                          State, _Dispatcher) ->
    {reply, <<>>, State}.

http_request(OutputType, UseHostPrefix, UseMethodSuffix, HttpRequest, Job) ->
    RequestStartMicroSec = uuid:get_v1_time(os),
    Method = HttpRequest:get(method),
    HeadersIncoming = HttpRequest:get(headers),
    NameIncoming = if
        UseHostPrefix =:= false ->
            HttpRequest:get(uri_unquoted);
        true ->
            case header_host(HeadersIncoming) of
                undefined ->
                    HttpRequest:get(uri_unquoted);
                Host ->
                    Host ++ HttpRequest:get(uri_unquoted)
            end
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
            NameIncoming ++ "/options";
        Method =:= 'CONNECT' ->
            NameIncoming ++ "/connect"
        % more cases here than necessary probably
    end,
    RequestBinary = if
        Method =:= 'GET' ->
            erlang:iolist_to_binary(lists:foldr(fun({K, V}, L) ->
                [erlang:list_to_binary(K), 0,
                 erlang:list_to_binary(V), 0 | L]
            end, [], HttpRequest:parse_qs()));
        Method =:= 'POST'; Method =:= 'PUT' ->
            % do not pass type information along with the request!
            % make sure to encourage good design that provides
            % one type per name (path)
            case header_content_type(HeadersIncoming) of
                "application/zip" ->
                    zlib:unzip(HttpRequest:get(body));
                _ ->
                    HttpRequest:get(body)
            end
    end,
    Request = if
        OutputType =:= list ->
            erlang:binary_to_list(RequestBinary);
        OutputType =:= binary ->
            RequestBinary
    end,
    RequestInfo = if
        OutputType =:= list ->
            % list data will only be handled by erlang jobs, so there is no
            % need to use a special format for the headers data
            HeadersIncoming;
        OutputType =:= binary ->
            headers_external(HeadersIncoming)
    end,
    Job ! {http_request,
           #request_data{name_incoming = NameIncoming,
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
            HttpRequest:ok(HeadersOutgoing, ResponseBinary);
        {error, HttpCode, Reason} ->
            ?LOG_WARN("~w ~s ~s ~p ms: ~p",
                      [HttpCode, Method, NameIncoming,
                       (uuid:get_v1_time(os) -
                        RequestStartMicroSec) / 1000.0, Reason]),
            HttpRequest:respond(HttpCode)
    end.

cloudi_job_handle_info({'http_request',
                        #request_data{name_outgoing = NameOutgoing,
                                      request_info = RequestInfo,
                                      request = Request,
                                      request_pid = RequestPid} = RequestData},
                       #state{requests = Requests} = State,
                       Dispatcher) ->
    case cloudi_job:send_async_active(Dispatcher, NameOutgoing,
                                      RequestInfo, Request,
                                      undefined, undefined) of
        {ok, TransId} ->
            NewRequestData = RequestData#request_data{request_info = undefined,
                                                      request = undefined},
            {noreply, State#state{requests = dict:store(TransId, NewRequestData,
                                                        Requests)}};
        {error, timeout} ->
            RequestPid ! {error, 504, timeout},
            {noreply, State};
        {error, Reason} ->
            RequestPid ! {error, 500, Reason},
            {noreply, State}
    end;

cloudi_job_handle_info({'return_async_active', _Name, _Pattern,
                        _ResponseInfo, Response,
                        _Timeout, TransId},
                       #state{output_type = OutputType,
                              default_content_type = DefaultContentType,
                              content_type_lookup = ContentTypeLookup,
                              requests = Requests} = State,
                       _Dispatcher) ->
    #request_data{name_incoming = NameIncoming,
                  request_pid = RequestPid} = dict:fetch(TransId, Requests),
    ResponseBinary = if
        OutputType =:= list ->
            erlang:list_to_binary(Response);
        OutputType =:= binary ->
            Response
    end,
    FileName = cloudi_string:afterr($/, NameIncoming, input),
    HeadersOutgoing = if
        is_list(DefaultContentType) ->
            [{'Content-Type', DefaultContentType}];
        true ->
            Extension = filename:extension(FileName),
            if
                Extension == [] ->
                    [{'Content-Type', "text/html"}];
                true ->
                    case trie:find(Extension, ContentTypeLookup) of
                        error ->
                            [{'Content-Disposition',
                              "attachment; filename=" ++ NameIncoming},
                             {'Content-Type',
                              "application/octet-stream"}];
                        {ok, {request, ContentType}} ->
                            [{'Content-Type', ContentType}];
                        {ok, {attachment, ContentType}} ->
                            [{'Content-Disposition',
                              "attachment; filename=" ++ NameIncoming},
                             {'Content-Type', ContentType}]
                    end
            end
    end,
    RequestPid ! {ok, 200, HeadersOutgoing, ResponseBinary},
    {noreply, State#state{requests = dict:erase(TransId, Requests)}};

cloudi_job_handle_info({'timeout_async_active', TransId},
                       #state{requests = Requests} = State,
                       _Dispatcher) ->
    #request_data{request_pid = RequestPid} = dict:fetch(TransId, Requests),
    RequestPid ! {error, 504, timeout},
    {noreply, State#state{requests = dict:erase(TransId, Requests)}};

cloudi_job_handle_info(Request, State, _) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_job_terminate(_, #state{process = Process}) ->
    misultin:stop(Process),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

header_host(Headers) ->
    case misultin_utility:get_key_value('Host', Headers) of
        undefined ->
            undefined;
        Host ->
            case cloudi_string:beforel($:, Host, input) of
                "" ->
                    undefined;
                HostName ->
                    HostName
            end
    end.

header_content_type(Headers) ->
    case misultin_utility:get_key_value('Content-Type', Headers) of
        undefined ->
            "";
        ContentType ->
            cloudi_string:beforel($;, ContentType, input)
    end.

% format for external jobs, http headers passed as key-value pairs
headers_external(L) ->
    erlang:iolist_to_binary(lists:reverse(headers_external([], L))).

headers_external(Result, []) ->
    Result;
headers_external(Result, [{K, V} | L]) when is_atom(K) ->
    headers_external([[erlang:list_to_binary(erlang:atom_to_list(K)), 0,
                       erlang:list_to_binary(V), 0] | Result], L);
headers_external(Result, [{K, V} | L]) when is_list(K) ->
    headers_external([[erlang:list_to_binary(K), 0,
                       erlang:list_to_binary(V), 0] | Result], L).

% static content type lookup (based on misultin_utility)
content_type_lookup() ->
    trie:new([
        {".txt",     {request, "text/plain"}},
        {".json",    {request, "application/json"}},
        {".xml",     {request, "text/xml"}},
        {".csv",     {request, "text/csv"}},
        {".htm",     {request, "text/html"}},
        {".html",    {request, "text/html"}},
        {".exe",     {attachment, "application/octet-stream"}},
        {".pdf",     {attachment, "application/pdf"}},
        {".rtf",     {attachment, "application/rtf"}},
        {".ppt",     {attachment, "application/vnd.ms-powerpoint"}},
        {".tgz",     {attachment, "application/x-compressed"}},
        {".tar",     {attachment, "application/x-tar"}},
        {".zip",     {attachment, "application/zip"}},
        {".mp3",     {attachment, "audio/mpeg"}},
        {".wav",     {attachment, "audio/x-wav"}},
        {".bmp",     {attachment, "image/bmp"}},
        {".ram",     {attachment, "audio/x-pn-realaudio"}},
        {".gif",     {attachment, "image/gif"}},
        {".jpe",     {attachment, "image/jpeg"}},
        {".jpeg",    {attachment, "image/jpeg"}},
        {".jpg",     {attachment, "image/jpeg"}},
        {".tif",     {attachment, "image/tiff"}},
        {".tiff",    {attachment, "image/tiff"}},
        {".mp2",     {attachment, "video/mpeg"}},
        {".mpa",     {attachment, "video/mpeg"}},
        {".mpe",     {attachment, "video/mpeg"}},
        {".mpeg",    {attachment, "video/mpeg"}},
        {".mpg",     {attachment, "video/mpeg"}},
        {".mov",     {attachment, "video/quicktime"}},
        {".avi",     {attachment, "video/x-msvideo"}},
        {".evy",     {attachment, "application/envoy"}},
        {".fif",     {attachment, "application/fractals"}},
        {".spl",     {attachment, "application/futuresplash"}},
        {".hta",     {attachment, "application/hta"}},
        {".acx",     {attachment, "application/internet-property-stream"}},
        {".hqx",     {attachment, "application/mac-binhex40"}},
        {".dot",     {attachment, "application/msword"}},
        {".bin",     {attachment, "application/octet-stream"}},
        {".class",   {attachment, "application/octet-stream"}},
        {".dms",     {attachment, "application/octet-stream"}},
        {".lha",     {attachment, "application/octet-stream"}},
        {".lzh",     {attachment, "application/octet-stream"}},
        {".oda",     {attachment, "application/oda"}},
        {".axs",     {attachment, "application/olescript"}},
        {".prf",     {attachment, "application/pics-rules"}},
        {".p10",     {attachment, "application/pkcs10"}},
        {".crl",     {attachment, "application/pkix-crl"}},
        {".ai",      {attachment, "application/postscript"}},
        {".eps",     {attachment, "application/postscript"}},
        {".ps",      {attachment, "application/postscript"}},
        {".setpay",  {attachment, "application/set-payment-initiation"}},
        {".setreg",  {attachment, "application/set-registration-initiation"}},
        {".xla",     {attachment, "application/vnd.ms-excel"}},
        {".xlc",     {attachment, "application/vnd.ms-excel"}},
        {".xlm",     {attachment, "application/vnd.ms-excel"}},
        {".xls",     {attachment, "application/vnd.ms-excel"}},
        {".xlt",     {attachment, "application/vnd.ms-excel"}},
        {".xlw",     {attachment, "application/vnd.ms-excel"}},
        {".msg",     {attachment, "application/vnd.ms-outlook"}},
        {".sst",     {attachment, "application/vnd.ms-pkicertstore"}},
        {".cat",     {attachment, "application/vnd.ms-pkiseccat"}},
        {".stl",     {attachment, "application/vnd.ms-pkistl"}},
        {".pot",     {attachment, "application/vnd.ms-powerpoint"}},
        {".pps",     {attachment, "application/vnd.ms-powerpoint"}},
        {".mpp",     {attachment, "application/vnd.ms-project"}},
        {".wcm",     {attachment, "application/vnd.ms-works"}},
        {".wdb",     {attachment, "application/vnd.ms-works"}},
        {".wks",     {attachment, "application/vnd.ms-works"}},
        {".wps",     {attachment, "application/vnd.ms-works"}},
        {".hlp",     {attachment, "application/winhlp"}},
        {".bcpio",   {attachment, "application/x-bcpio"}},
        {".cdf",     {attachment, "application/x-cdf"}},
        {".z",       {attachment, "application/x-compress"}},
        {".cpio",    {attachment, "application/x-cpio"}},
        {".csh",     {attachment, "application/x-csh"}},
        {".dcr",     {attachment, "application/x-director"}},
        {".dir",     {attachment, "application/x-director"}},
        {".dxr",     {attachment, "application/x-director"}},
        {".dvi",     {attachment, "application/x-dvi"}},
        {".gtar",    {attachment, "application/x-gtar"}},
        {".gz",      {attachment, "application/x-gzip"}},
        {".hdf",     {attachment, "application/x-hdf"}},
        {".ins",     {attachment, "application/x-internet-signup"}},
        {".isp",     {attachment, "application/x-internet-signup"}},
        {".iii",     {attachment, "application/x-iphone"}},
        {".js",      {attachment, "application/x-javascript"}},
        {".latex",   {attachment, "application/x-latex"}},
        {".mdb",     {attachment, "application/x-msaccess"}},
        {".crd",     {attachment, "application/x-mscardfile"}},
        {".clp",     {attachment, "application/x-msclip"}},
        {".dll",     {attachment, "application/x-msdownload"}},
        {".m13",     {attachment, "application/x-msmediaview"}},
        {".m14",     {attachment, "application/x-msmediaview"}},
        {".mvb",     {attachment, "application/x-msmediaview"}},
        {".wmf",     {attachment, "application/x-msmetafile"}},
        {".mny",     {attachment, "application/x-msmoney"}},
        {".pub",     {attachment, "application/x-mspublisher"}},
        {".scd",     {attachment, "application/x-msschedule"}},
        {".trm",     {attachment, "application/x-msterminal"}},
        {".wri",     {attachment, "application/x-mswrite"}},
        {".nc",      {attachment, "application/x-netcdf"}},
        {".pma",     {attachment, "application/x-perfmon"}},
        {".pmc",     {attachment, "application/x-perfmon"}},
        {".pml",     {attachment, "application/x-perfmon"}},
        {".pmr",     {attachment, "application/x-perfmon"}},
        {".pmw",     {attachment, "application/x-perfmon"}},
        {".p12",     {attachment, "application/x-pkcs12"}},
        {".pfx",     {attachment, "application/x-pkcs12"}},
        {".p7b",     {attachment, "application/x-pkcs7-certificates"}},
        {".spc",     {attachment, "application/x-pkcs7-certificates"}},
        {".p7r",     {attachment, "application/x-pkcs7-certreqresp"}},
        {".p7c",     {attachment, "application/x-pkcs7-mime"}},
        {".p7m",     {attachment, "application/x-pkcs7-mime"}},
        {".p7s",     {attachment, "application/x-pkcs7-signature"}},
        {".sh",      {attachment, "application/x-sh"}},
        {".shar",    {attachment, "application/x-shar"}},
        {".swf",     {attachment, "application/x-shockwave-flash"}},
        {".sit",     {attachment, "application/x-stuffit"}},
        {".sv4cpio", {attachment, "application/x-sv4cpio"}},
        {".sv4crc",  {attachment, "application/x-sv4crc"}},
        {".tcl",     {attachment, "application/x-tcl"}},
        {".tex",     {attachment, "application/x-tex"}},
        {".texi",    {attachment, "application/x-texinfo"}},
        {".texinfo", {attachment, "application/x-texinfo"}},
        {".roff",    {attachment, "application/x-troff"}},
        {".t",       {attachment, "application/x-troff"}},
        {".tr",      {attachment, "application/x-troff"}},
        {".man",     {attachment, "application/x-troff-man"}},
        {".me",      {attachment, "application/x-troff-me"}},
        {".ms",      {attachment, "application/x-troff-ms"}},
        {".ustar",   {attachment, "application/x-ustar"}},
        {".src",     {attachment, "application/x-wais-source"}},
        {".cer",     {attachment, "application/x-x509-ca-cert"}},
        {".crt",     {attachment, "application/x-x509-ca-cert"}},
        {".der",     {attachment, "application/x-x509-ca-cert"}},
        {".pko",     {attachment, "application/ynd.ms-pkipko"}},
        {".au",      {attachment, "audio/basic"}},
        {".snd",     {attachment, "audio/basic"}},
        {".mid",     {attachment, "audio/mid"}},
        {".rmi",     {attachment, "audio/mid"}},
        {".aif",     {attachment, "audio/x-aiff"}},
        {".aifc",    {attachment, "audio/x-aiff"}},
        {".aiff",    {attachment, "audio/x-aiff"}},
        {".m3u",     {attachment, "audio/x-mpegurl"}},
        {".ra",      {attachment, "audio/x-pn-realaudio"}},
        {".cod",     {attachment, "image/cis-cod"}},
        {".ief",     {attachment, "image/ief"}},
        {".jfif",    {attachment, "image/pipeg"}},
        {".svg",     {attachment, "image/svg+xml"}},
        {".ras",     {attachment, "image/x-cmu-raster"}},
        {".cmx",     {attachment, "image/x-cmx"}},
        {".ico",     {attachment, "image/x-icon"}},
        {".pnm",     {attachment, "image/x-portable-anymap"}},
        {".pbm",     {attachment, "image/x-portable-bitmap"}},
        {".pgm",     {attachment, "image/x-portable-graymap"}},
        {".ppm",     {attachment, "image/x-portable-pixmap"}},
        {".rgb",     {attachment, "image/x-rgb"}},
        {".xbm",     {attachment, "image/x-xbitmap"}},
        {".xpm",     {attachment, "image/x-xpixmap"}},
        {".xwd",     {attachment, "image/x-xwindowdump"}},
        {".mht",     {attachment, "message/rfc822"}},
        {".mhtml",   {attachment, "message/rfc822"}},
        {".nws",     {attachment, "message/rfc822"}},
        {".css",     {attachment, "text/css"}},
        {".323",     {attachment, "text/h323"}},
        {".stm",     {attachment, "text/html"}},
        {".uls",     {attachment, "text/iuls"}},
        {".bas",     {attachment, "text/plain"}},
        {".c",       {attachment, "text/plain"}},
        {".h",       {attachment, "text/plain"}},
        {".rtx",     {attachment, "text/richtext"}},
        {".sct",     {attachment, "text/scriptlet"}},
        {".tsv",     {attachment, "text/tab-separated-values"}},
        {".htt",     {attachment, "text/webviewhtml"}},
        {".htc",     {attachment, "text/x-component"}},
        {".etx",     {attachment, "text/x-setext"}},
        {".vcf",     {attachment, "text/x-vcard"}},
        {".mpv2",    {attachment, "video/mpeg"}},
        {".qt",      {attachment, "video/quicktime"}},
        {".lsf",     {attachment, "video/x-la-asf"}},
        {".lsx",     {attachment, "video/x-la-asf"}},
        {".asf",     {attachment, "video/x-ms-asf"}},
        {".asr",     {attachment, "video/x-ms-asf"}},
        {".asx",     {attachment, "video/x-ms-asf"}},
        {".movie",   {attachment, "video/x-sgi-movie"}},
        {".flr",     {attachment, "x-world/x-vrml"}},
        {".vrml",    {attachment, "x-world/x-vrml"}},
        {".wrl",     {attachment, "x-world/x-vrml"}},
        {".wrz",     {attachment, "x-world/x-vrml"}},
        {".xaf",     {attachment, "x-world/x-vrml"}},
        {".xof",     {attachment, "x-world/x-vrml"}}
        ]).

