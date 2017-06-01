%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%%
%%% MIT License
%%%
%%% Copyright (c) 2013-2017 Michael Truog <mjtruog at gmail dot com>
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
-define(DEFAULT_NODE_RECONNECT_START,             300). % seconds
-define(DEFAULT_NODE_RECONNECT_DELAY,              60). % seconds
% (values based on the Linux localhost MTU)
-define(DEFAULT_BUFFER_SIZE_TCP,                65536). % bytes
-define(DEFAULT_BUFFER_SIZE_UDP,                65536). % bytes
-define(DEFAULT_BUFFER_SIZE_LOCAL,              65536). % bytes

