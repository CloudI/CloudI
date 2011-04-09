%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Misultin Integration==
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
%%% @version 0.1.4 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_job_misultin).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_job).

%% external interface

%% cloudi_job callbacks
-export([cloudi_job_init/3,
         cloudi_job_handle_request/8,
         cloudi_job_handle_info/3,
         cloudi_job_terminate/2]).

-include("cloudi_logger.hrl").

-define(DEFAULT_INTERFACE,       {127,0,0,1}). % ip address
-define(DEFAULT_PORT,                   8080).
-define(DEFAULT_BACKLOG,                 128).
-define(DEFAULT_RECV_TIMEOUT,      30 * 1000). % milliseconds
-define(DEFAULT_SSL,                   false).
-define(DEFAULT_COMPRESS,              false).
-define(DEFAULT_WS_AUTOEXIT,            true).
-define(DEFAULT_OUTPUT,               binary).
-define(DEFAULT_CONTENT_TYPE,      undefined). % force a content type

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

cloudi_job_init(Args, _Prefix, Dispatcher) ->
    Defaults = [
        {ip,              ?DEFAULT_INTERFACE},
        {port,            ?DEFAULT_PORT},
        {backlog,         ?DEFAULT_BACKLOG},
        {recv_timeout,    ?DEFAULT_RECV_TIMEOUT},
        {ssl,             ?DEFAULT_SSL},
        {compress,        ?DEFAULT_COMPRESS},
        {ws_autoexit,     ?DEFAULT_WS_AUTOEXIT},
        {output,          ?DEFAULT_OUTPUT},
        {content_type,    ?DEFAULT_CONTENT_TYPE}],
    [Interface, Port, Backlog, RecvTimeout, SSL, Compress, WsAutoExit,
     OutputType, DefaultContentType] =
        proplists2:take_values(Defaults, Args),
    Loop = fun(HttpRequest) ->
        handle_http(HttpRequest, OutputType, DefaultContentType, Dispatcher)
    end,
    case misultin:start_link([{ip, Interface},
                              {port, Port},
                              {backlog, Backlog},
                              {recv_timeout, RecvTimeout},
                              {ssl, SSL},
                              {compress, Compress},
                              {ws_autoexit, WsAutoExit},
                              {loop, Loop},
                              {name, false}]) of
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

handle_http(HttpRequest, OutputType, DefaultContentType, Dispatcher) ->
    Name = HttpRequest:get(str_uri),
    RequestBinary = case HttpRequest:get(method) of
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
    Request = if
        OutputType =:= list ->
            erlang:binary_to_list(RequestBinary);
        OutputType =:= binary ->
            RequestBinary
    end,
    case cloudi_job:send_sync(Dispatcher, Name, Request) of
        {ok, Response} ->
            ResponseBinary = if
                OutputType =:= list ->
                    string2:term_to_binary(Response);
                OutputType =:= binary ->
                    Response
            end,
            FileName = string2:afterr($/, Name),
            Headers = if
                is_list(DefaultContentType) ->
                    [{'Content-Type', DefaultContentType}];
                FileName == [] ->
                    [{'Content-Type', "text/html"}];
                true ->
                    Extension = filename:extension(FileName),
                    if
                        Extension == [] ->
                            [{'Content-Type', "text/plain"}];
                        true ->
                            case get_content_type(Extension) of
                                "text/html" ->
                                    [{'Content-Type', "text/html"}];
                                "text/plain" ->
                                    [{'Content-Type', "text/plain"}];
                                ContentType ->
                                    [{'Content-Disposition',
                                      "attachment; filename=" ++ Name},
                                     {'Content-Type', ContentType}]
                            end
                    end
            end,
            HttpRequest:ok(Headers, ResponseBinary);
        {error, timeout} ->
            HttpRequest:respond(504);
        {error, Reason} ->
            ?LOG_ERROR("Request Failed: ~p", [Reason]),
            HttpRequest:respond(500)
    end.

% get content type (taken from misultin_utility)
get_content_type(Extension) ->
    case Extension of
        % most common first
        [] -> "text/plain";
        ".json" -> "application/json";
        ".doc" -> "application/msword";
        ".exe" -> "application/octet-stream";
        ".pdf" -> "application/pdf";
        ".rtf" -> "application/rtf";
        ".ppt" -> "application/vnd.ms-powerpoint";
        ".tgz" -> "application/x-compressed";
        ".tar" -> "application/x-tar";
        ".zip" -> "application/zip";
        ".mp3" -> "audio/mpeg";
        ".wav" -> "audio/x-wav";
        ".bmp" -> "image/bmp";
        ".ram" -> "audio/x-pn-realaudio";
        ".gif" -> "image/gif";
        ".jpe" -> "image/jpeg";
        ".jpeg" -> "image/jpeg";
        ".jpg" -> "image/jpeg";
        ".tif" -> "image/tiff";
        ".tiff" -> "image/tiff";
        ".htm" -> "text/html";
        ".html" -> "text/html";
        ".txt" -> "text/plain";
        ".mp2" -> "video/mpeg";
        ".mpa" -> "video/mpeg";
        ".mpe" -> "video/mpeg";
        ".mpeg" -> "video/mpeg";
        ".mpg" -> "video/mpeg";
        ".mov" -> "video/quicktime";
        ".avi" -> "video/x-msvideo";
        % less common last
        ".evy" -> "application/envoy";
        ".fif" -> "application/fractals";
        ".spl" -> "application/futuresplash";
        ".hta" -> "application/hta";
        ".acx" -> "application/internet-property-stream";
        ".hqx" -> "application/mac-binhex40";
        ".dot" -> "application/msword";
        ".bin" -> "application/octet-stream";
        ".class" -> "application/octet-stream";
        ".dms" -> "application/octet-stream";
        ".lha" -> "application/octet-stream";
        ".lzh" -> "application/octet-stream";
        ".oda" -> "application/oda";
        ".axs" -> "application/olescript";
        ".prf" -> "application/pics-rules";
        ".p10" -> "application/pkcs10";
        ".crl" -> "application/pkix-crl";
        ".ai" -> "application/postscript";
        ".eps" -> "application/postscript";
        ".ps" -> "application/postscript";
        ".setpay" -> "application/set-payment-initiation";
        ".setreg" -> "application/set-registration-initiation";
        ".xla" -> "application/vnd.ms-excel";
        ".xlc" -> "application/vnd.ms-excel";
        ".xlm" -> "application/vnd.ms-excel";
        ".xls" -> "application/vnd.ms-excel";
        ".xlt" -> "application/vnd.ms-excel";
        ".xlw" -> "application/vnd.ms-excel";
        ".msg" -> "application/vnd.ms-outlook";
        ".sst" -> "application/vnd.ms-pkicertstore";
        ".cat" -> "application/vnd.ms-pkiseccat";
        ".stl" -> "application/vnd.ms-pkistl";
        ".pot" -> "application/vnd.ms-powerpoint";
        ".pps" -> "application/vnd.ms-powerpoint";
        ".mpp" -> "application/vnd.ms-project";
        ".wcm" -> "application/vnd.ms-works";
        ".wdb" -> "application/vnd.ms-works";
        ".wks" -> "application/vnd.ms-works";
        ".wps" -> "application/vnd.ms-works";
        ".hlp" -> "application/winhlp";
        ".bcpio" -> "application/x-bcpio";
        ".cdf" -> "application/x-cdf";
        ".z" -> "application/x-compress";
        ".cpio" -> "application/x-cpio";
        ".csh" -> "application/x-csh";
        ".dcr" -> "application/x-director";
        ".dir" -> "application/x-director";
        ".dxr" -> "application/x-director";
        ".dvi" -> "application/x-dvi";
        ".gtar" -> "application/x-gtar";
        ".gz" -> "application/x-gzip";
        ".hdf" -> "application/x-hdf";
        ".ins" -> "application/x-internet-signup";
        ".isp" -> "application/x-internet-signup";
        ".iii" -> "application/x-iphone";
        ".js" -> "application/x-javascript";
        ".latex" -> "application/x-latex";
        ".mdb" -> "application/x-msaccess";
        ".crd" -> "application/x-mscardfile";
        ".clp" -> "application/x-msclip";
        ".dll" -> "application/x-msdownload";
        ".m13" -> "application/x-msmediaview";
        ".m14" -> "application/x-msmediaview";
        ".mvb" -> "application/x-msmediaview";
        ".wmf" -> "application/x-msmetafile";
        ".mny" -> "application/x-msmoney";
        ".pub" -> "application/x-mspublisher";
        ".scd" -> "application/x-msschedule";
        ".trm" -> "application/x-msterminal";
        ".wri" -> "application/x-mswrite";
        ".nc" -> "application/x-netcdf";
        ".pma" -> "application/x-perfmon";
        ".pmc" -> "application/x-perfmon";
        ".pml" -> "application/x-perfmon";
        ".pmr" -> "application/x-perfmon";
        ".pmw" -> "application/x-perfmon";
        ".p12" -> "application/x-pkcs12";
        ".pfx" -> "application/x-pkcs12";
        ".p7b" -> "application/x-pkcs7-certificates";
        ".spc" -> "application/x-pkcs7-certificates";
        ".p7r" -> "application/x-pkcs7-certreqresp";
        ".p7c" -> "application/x-pkcs7-mime";
        ".p7m" -> "application/x-pkcs7-mime";
        ".p7s" -> "application/x-pkcs7-signature";
        ".sh" -> "application/x-sh";
        ".shar" -> "application/x-shar";
        ".swf" -> "application/x-shockwave-flash";
        ".sit" -> "application/x-stuffit";
        ".sv4cpio" -> "application/x-sv4cpio";
        ".sv4crc" -> "application/x-sv4crc";
        ".tcl" -> "application/x-tcl";
        ".tex" -> "application/x-tex";
        ".texi" -> "application/x-texinfo";
        ".texinfo" -> "application/x-texinfo";
        ".roff" -> "application/x-troff";
        ".t" -> "application/x-troff";
        ".tr" -> "application/x-troff";
        ".man" -> "application/x-troff-man";
        ".me" -> "application/x-troff-me";
        ".ms" -> "application/x-troff-ms";
        ".ustar" -> "application/x-ustar";
        ".src" -> "application/x-wais-source";
        ".cer" -> "application/x-x509-ca-cert";
        ".crt" -> "application/x-x509-ca-cert";
        ".der" -> "application/x-x509-ca-cert";
        ".pko" -> "application/ynd.ms-pkipko";
        ".au" -> "audio/basic";
        ".snd" -> "audio/basic";
        ".mid" -> "audio/mid";
        ".rmi" -> "audio/mid";
        ".aif" -> "audio/x-aiff";
        ".aifc" -> "audio/x-aiff";
        ".aiff" -> "audio/x-aiff";
        ".m3u" -> "audio/x-mpegurl";
        ".ra" -> "audio/x-pn-realaudio";
        ".cod" -> "image/cis-cod";
        ".ief" -> "image/ief";
        ".jfif" -> "image/pipeg";
        ".svg" -> "image/svg+xml";
        ".ras" -> "image/x-cmu-raster";
        ".cmx" -> "image/x-cmx";
        ".ico" -> "image/x-icon";
        ".pnm" -> "image/x-portable-anymap";
        ".pbm" -> "image/x-portable-bitmap";
        ".pgm" -> "image/x-portable-graymap";
        ".ppm" -> "image/x-portable-pixmap";
        ".rgb" -> "image/x-rgb";
        ".xbm" -> "image/x-xbitmap";
        ".xpm" -> "image/x-xpixmap";
        ".xwd" -> "image/x-xwindowdump";
        ".mht" -> "message/rfc822";
        ".mhtml" -> "message/rfc822";
        ".nws" -> "message/rfc822";
        ".css" -> "text/css";
        ".323" -> "text/h323";
        ".stm" -> "text/html";
        ".uls" -> "text/iuls";
        ".bas" -> "text/plain";
        ".c" -> "text/plain";
        ".h" -> "text/plain";
        ".rtx" -> "text/richtext";
        ".sct" -> "text/scriptlet";
        ".tsv" -> "text/tab-separated-values";
        ".htt" -> "text/webviewhtml";
        ".htc" -> "text/x-component";
        ".etx" -> "text/x-setext";
        ".vcf" -> "text/x-vcard";
        ".mpv2" -> "video/mpeg";
        ".qt" -> "video/quicktime";
        ".lsf" -> "video/x-la-asf";
        ".lsx" -> "video/x-la-asf";
        ".asf" -> "video/x-ms-asf";
        ".asr" -> "video/x-ms-asf";
        ".asx" -> "video/x-ms-asf";
        ".movie" -> "video/x-sgi-movie";
        ".flr" -> "x-world/x-vrml";
        ".vrml" -> "x-world/x-vrml";
        ".wrl" -> "x-world/x-vrml";
        ".wrz" -> "x-world/x-vrml";
        ".xaf" -> "x-world/x-vrml";
        ".xof" -> "x-world/x-vrml";
        _ -> "application/octet-stream"
    end.

