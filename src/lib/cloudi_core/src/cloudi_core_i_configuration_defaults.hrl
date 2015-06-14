%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
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
%%%------------------------------------------------------------------------

% defaults common to cloudi module usage and cloudi_service module usage
% (set manually in the cloudi_service_api.hrl records)
-define(DEFAULT_SERVICE_PREFIX,                   "/").
-define(DEFAULT_DEST_REFRESH,       immediate_closest).
-define(DEFAULT_TIMEOUT_INIT,                    5000). % milliseconds
-define(DEFAULT_TIMEOUT_ASYNC,                   5000). % milliseconds
-define(DEFAULT_TIMEOUT_SYNC,                    5000). % milliseconds
-define(DEFAULT_MAX_R,                              5). % max restart count
-define(DEFAULT_MAX_T,                            300). % max time in seconds
-define(DEFAULT_DEST_REFRESH_START,               500). % milliseconds
-define(DEFAULT_DEST_REFRESH_DELAY,            300000). % milliseconds
-define(DEFAULT_REQUEST_NAME_LOOKUP,             sync).
-define(DEFAULT_PRIORITY,                           0).
-define(DEFAULT_SCOPE,                        default).

% CloudI system defaults
-define(DEFAULT_NODE_RECONNECT_START, 300). % seconds
-define(DEFAULT_NODE_RECONNECT_DELAY, 60). % seconds

