%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI ResponseInfo Creation==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2014, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2014 Michael Truog
%%% @version 1.3.3 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_response_info).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([lookup_content_type/0,
         lookup_content_type/1]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Provide a lookup with common content types.===
%% @end
%%-------------------------------------------------------------------------

-spec lookup_content_type() ->
    cloudi_x_trie:cloudi_x_trie().

lookup_content_type() ->
    lookup_content_type(binary).

%%-------------------------------------------------------------------------
%% @doc
%% ===Provide a lookup with common content types while setting string type of the content type value.===
%% @end
%%-------------------------------------------------------------------------

-spec lookup_content_type(binary | list) ->
    cloudi_x_trie:cloudi_x_trie().

lookup_content_type(binary) ->
    cloudi_x_trie:new(lookup_content_type_data());
lookup_content_type(list) ->
    cloudi_x_trie:new([{E, {T, erlang:binary_to_list(V)}}
                       || {E, {T, V}} <- lookup_content_type_data()]).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

lookup_content_type_data() ->
    [{".txt",    {request, <<"text/plain">>}},
     {".json",   {request, <<"application/json">>}},
     {".xml",    {request, <<"text/xml">>}},
     {".csv",    {request, <<"text/csv">>}},
     {".htm",    {request, <<"text/html">>}},
     {".html",   {request, <<"text/html">>}},
     {".erl",    {request, <<"application/x-erlang">>}},
     {".exe",    {attachment, <<"application/octet-stream">>}},
     {".pdf",    {attachment, <<"application/pdf">>}},
     {".rtf",    {attachment, <<"application/rtf">>}},
     {".ppt",    {attachment, <<"application/vnd.ms-powerpoint">>}},
     {".tgz",    {attachment, <<"application/x-compressed">>}},
     {".tar",    {attachment, <<"application/x-tar">>}},
     {".zip",    {attachment, <<"application/zip">>}},
     {".mp3",    {attachment, <<"audio/mpeg">>}},
     {".wav",    {attachment, <<"audio/x-wav">>}},
     {".bmp",    {attachment, <<"image/bmp">>}},
     {".ram",    {attachment, <<"audio/x-pn-realaudio">>}},
     {".gif",    {attachment, <<"image/gif">>}},
     {".jpe",    {attachment, <<"image/jpeg">>}},
     {".jpeg",   {attachment, <<"image/jpeg">>}},
     {".jpg",    {attachment, <<"image/jpeg">>}},
     {".tif",    {attachment, <<"image/tiff">>}},
     {".tiff",   {attachment, <<"image/tiff">>}},
     {".mp2",    {attachment, <<"video/mpeg">>}},
     {".mpa",    {attachment, <<"video/mpeg">>}},
     {".mpe",    {attachment, <<"video/mpeg">>}},
     {".mpeg",   {attachment, <<"video/mpeg">>}},
     {".mpg",    {attachment, <<"video/mpeg">>}},
     {".mov",    {attachment, <<"video/quicktime">>}},
     {".avi",    {attachment, <<"video/x-msvideo">>}},
     {".evy",    {attachment, <<"application/envoy">>}},
     {".fif",    {attachment, <<"application/fractals">>}},
     {".spl",    {attachment, <<"application/futuresplash">>}},
     {".hta",    {attachment, <<"application/hta">>}},
     {".acx",    {attachment, <<"application/internet-property-stream">>}},
     {".hqx",    {attachment, <<"application/mac-binhex40">>}},
     {".dot",    {attachment, <<"application/msword">>}},
     {".bin",    {attachment, <<"application/octet-stream">>}},
     {".class",  {attachment, <<"application/octet-stream">>}},
     {".dms",    {attachment, <<"application/octet-stream">>}},
     {".lha",    {attachment, <<"application/octet-stream">>}},
     {".lzh",    {attachment, <<"application/octet-stream">>}},
     {".oda",    {attachment, <<"application/oda">>}},
     {".axs",    {attachment, <<"application/olescript">>}},
     {".prf",    {attachment, <<"application/pics-rules">>}},
     {".p10",    {attachment, <<"application/pkcs10">>}},
     {".crl",    {attachment, <<"application/pkix-crl">>}},
     {".ai",     {attachment, <<"application/postscript">>}},
     {".eps",    {attachment, <<"application/postscript">>}},
     {".ps",     {attachment, <<"application/postscript">>}},
     {".setpay", {attachment, <<"application/set-payment-initiation">>}},
     {".setreg", {attachment, <<"application/set-registration-initiation">>}},
     {".xla",    {attachment, <<"application/vnd.ms-excel">>}},
     {".xlc",    {attachment, <<"application/vnd.ms-excel">>}},
     {".xlm",    {attachment, <<"application/vnd.ms-excel">>}},
     {".xls",    {attachment, <<"application/vnd.ms-excel">>}},
     {".xlt",    {attachment, <<"application/vnd.ms-excel">>}},
     {".xlw",    {attachment, <<"application/vnd.ms-excel">>}},
     {".msg",    {attachment, <<"application/vnd.ms-outlook">>}},
     {".sst",    {attachment, <<"application/vnd.ms-pkicertstore">>}},
     {".cat",    {attachment, <<"application/vnd.ms-pkiseccat">>}},
     {".stl",    {attachment, <<"application/vnd.ms-pkistl">>}},
     {".pot",    {attachment, <<"application/vnd.ms-powerpoint">>}},
     {".pps",    {attachment, <<"application/vnd.ms-powerpoint">>}},
     {".mpp",    {attachment, <<"application/vnd.ms-project">>}},
     {".wcm",    {attachment, <<"application/vnd.ms-works">>}},
     {".wdb",    {attachment, <<"application/vnd.ms-works">>}},
     {".wks",    {attachment, <<"application/vnd.ms-works">>}},
     {".wps",    {attachment, <<"application/vnd.ms-works">>}},
     {".hlp",    {attachment, <<"application/winhlp">>}},
     {".bcpio",  {attachment, <<"application/x-bcpio">>}},
     {".cdf",    {attachment, <<"application/x-cdf">>}},
     {".z",      {attachment, <<"application/x-compress">>}},
     {".cpio",   {attachment, <<"application/x-cpio">>}},
     {".csh",    {attachment, <<"application/x-csh">>}},
     {".dcr",    {attachment, <<"application/x-director">>}},
     {".dir",    {attachment, <<"application/x-director">>}},
     {".dxr",    {attachment, <<"application/x-director">>}},
     {".dvi",    {attachment, <<"application/x-dvi">>}},
     {".gtar",   {attachment, <<"application/x-gtar">>}},
     {".gz",     {attachment, <<"application/x-gzip">>}},
     {".hdf",    {attachment, <<"application/x-hdf">>}},
     {".ins",    {attachment, <<"application/x-internet-signup">>}},
     {".isp",    {attachment, <<"application/x-internet-signup">>}},
     {".iii",    {attachment, <<"application/x-iphone">>}},
     {".js",     {attachment, <<"application/x-javascript">>}},
     {".latex",  {attachment, <<"application/x-latex">>}},
     {".mdb",    {attachment, <<"application/x-msaccess">>}},
     {".crd",    {attachment, <<"application/x-mscardfile">>}},
     {".clp",    {attachment, <<"application/x-msclip">>}},
     {".dll",    {attachment, <<"application/x-msdownload">>}},
     {".m13",    {attachment, <<"application/x-msmediaview">>}},
     {".m14",    {attachment, <<"application/x-msmediaview">>}},
     {".mvb",    {attachment, <<"application/x-msmediaview">>}},
     {".wmf",    {attachment, <<"application/x-msmetafile">>}},
     {".mny",    {attachment, <<"application/x-msmoney">>}},
     {".pub",    {attachment, <<"application/x-mspublisher">>}},
     {".scd",    {attachment, <<"application/x-msschedule">>}},
     {".trm",    {attachment, <<"application/x-msterminal">>}},
     {".wri",    {attachment, <<"application/x-mswrite">>}},
     {".nc",     {attachment, <<"application/x-netcdf">>}},
     {".pma",    {attachment, <<"application/x-perfmon">>}},
     {".pmc",    {attachment, <<"application/x-perfmon">>}},
     {".pml",    {attachment, <<"application/x-perfmon">>}},
     {".pmr",    {attachment, <<"application/x-perfmon">>}},
     {".pmw",    {attachment, <<"application/x-perfmon">>}},
     {".p12",    {attachment, <<"application/x-pkcs12">>}},
     {".pfx",    {attachment, <<"application/x-pkcs12">>}},
     {".p7b",    {attachment, <<"application/x-pkcs7-certificates">>}},
     {".spc",    {attachment, <<"application/x-pkcs7-certificates">>}},
     {".p7r",    {attachment, <<"application/x-pkcs7-certreqresp">>}},
     {".p7c",    {attachment, <<"application/x-pkcs7-mime">>}},
     {".p7m",    {attachment, <<"application/x-pkcs7-mime">>}},
     {".p7s",    {attachment, <<"application/x-pkcs7-signature">>}},
     {".sh",     {attachment, <<"application/x-sh">>}},
     {".shar",   {attachment, <<"application/x-shar">>}},
     {".swf",    {attachment, <<"application/x-shockwave-flash">>}},
     {".sit",    {attachment, <<"application/x-stuffit">>}},
     {".sv4cpio",{attachment, <<"application/x-sv4cpio">>}},
     {".sv4crc", {attachment, <<"application/x-sv4crc">>}},
     {".tcl",    {attachment, <<"application/x-tcl">>}},
     {".tex",    {attachment, <<"application/x-tex">>}},
     {".texi",   {attachment, <<"application/x-texinfo">>}},
     {".texinfo",{attachment, <<"application/x-texinfo">>}},
     {".roff",   {attachment, <<"application/x-troff">>}},
     {".t",      {attachment, <<"application/x-troff">>}},
     {".tr",     {attachment, <<"application/x-troff">>}},
     {".man",    {attachment, <<"application/x-troff-man">>}},
     {".me",     {attachment, <<"application/x-troff-me">>}},
     {".ms",     {attachment, <<"application/x-troff-ms">>}},
     {".ustar",  {attachment, <<"application/x-ustar">>}},
     {".src",    {attachment, <<"application/x-wais-source">>}},
     {".cer",    {attachment, <<"application/x-x509-ca-cert">>}},
     {".crt",    {attachment, <<"application/x-x509-ca-cert">>}},
     {".der",    {attachment, <<"application/x-x509-ca-cert">>}},
     {".pko",    {attachment, <<"application/ynd.ms-pkipko">>}},
     {".au",     {attachment, <<"audio/basic">>}},
     {".snd",    {attachment, <<"audio/basic">>}},
     {".mid",    {attachment, <<"audio/mid">>}},
     {".rmi",    {attachment, <<"audio/mid">>}},
     {".aif",    {attachment, <<"audio/x-aiff">>}},
     {".aifc",   {attachment, <<"audio/x-aiff">>}},
     {".aiff",   {attachment, <<"audio/x-aiff">>}},
     {".m3u",    {attachment, <<"audio/x-mpegurl">>}},
     {".ra",     {attachment, <<"audio/x-pn-realaudio">>}},
     {".cod",    {attachment, <<"image/cis-cod">>}},
     {".ief",    {attachment, <<"image/ief">>}},
     {".jfif",   {attachment, <<"image/pipeg">>}},
     {".svg",    {attachment, <<"image/svg+xml">>}},
     {".ras",    {attachment, <<"image/x-cmu-raster">>}},
     {".cmx",    {attachment, <<"image/x-cmx">>}},
     {".ico",    {attachment, <<"image/x-icon">>}},
     {".pnm",    {attachment, <<"image/x-portable-anymap">>}},
     {".pbm",    {attachment, <<"image/x-portable-bitmap">>}},
     {".pgm",    {attachment, <<"image/x-portable-graymap">>}},
     {".ppm",    {attachment, <<"image/x-portable-pixmap">>}},
     {".rgb",    {attachment, <<"image/x-rgb">>}},
     {".xbm",    {attachment, <<"image/x-xbitmap">>}},
     {".xpm",    {attachment, <<"image/x-xpixmap">>}},
     {".xwd",    {attachment, <<"image/x-xwindowdump">>}},
     {".mht",    {attachment, <<"message/rfc822">>}},
     {".mhtml",  {attachment, <<"message/rfc822">>}},
     {".nws",    {attachment, <<"message/rfc822">>}},
     {".css",    {attachment, <<"text/css">>}},
     {".323",    {attachment, <<"text/h323">>}},
     {".stm",    {attachment, <<"text/html">>}},
     {".uls",    {attachment, <<"text/iuls">>}},
     {".bas",    {attachment, <<"text/plain">>}},
     {".c",      {attachment, <<"text/plain">>}},
     {".h",      {attachment, <<"text/plain">>}},
     {".rtx",    {attachment, <<"text/richtext">>}},
     {".sct",    {attachment, <<"text/scriptlet">>}},
     {".tsv",    {attachment, <<"text/tab-separated-values">>}},
     {".htt",    {attachment, <<"text/webviewhtml">>}},
     {".htc",    {attachment, <<"text/x-component">>}},
     {".etx",    {attachment, <<"text/x-setext">>}},
     {".vcf",    {attachment, <<"text/x-vcard">>}},
     {".mpv2",   {attachment, <<"video/mpeg">>}},
     {".qt",     {attachment, <<"video/quicktime">>}},
     {".lsf",    {attachment, <<"video/x-la-asf">>}},
     {".lsx",    {attachment, <<"video/x-la-asf">>}},
     {".asf",    {attachment, <<"video/x-ms-asf">>}},
     {".asr",    {attachment, <<"video/x-ms-asf">>}},
     {".asx",    {attachment, <<"video/x-ms-asf">>}},
     {".movie",  {attachment, <<"video/x-sgi-movie">>}},
     {".flr",    {attachment, <<"x-world/x-vrml">>}},
     {".vrml",   {attachment, <<"x-world/x-vrml">>}},
     {".wrl",    {attachment, <<"x-world/x-vrml">>}},
     {".wrz",    {attachment, <<"x-world/x-vrml">>}},
     {".xaf",    {attachment, <<"x-world/x-vrml">>}},
     {".xof",    {attachment, <<"x-world/x-vrml">>}}
     ].

