%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%%
%%% MIT License
%%%
%%% Copyright (c) 2009-2020 Michael Truog <mjtruog at protonmail dot com>
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

% logging levels explained:
%
%   FATAL: indicates the system has failed and can not continue
%   ERROR: indicates a subsystem has failed but the failure is not fatal
%   WARN:  indicates an unexpected occurance was found in a subsystem
%   INFO:  indicates a subsystem has changed state
%   DEBUG: reports subsystem data that should be useful for debugging
%   TRACE: reports subsystem data that is only for tracing execution

% Logging output will log asynchronously until the logger's
% message queue becomes too large, then use synchronous logging
% while the message queue remains large

-define(LOG_FATAL(Format, Args),
    cloudi_core_i_logger_interface:fatal(?MODULE, ?LINE,
                                         ?FUNCTION_NAME, ?FUNCTION_ARITY,
                                         Format, Args)).

-define(LOG_ERROR(Format, Args),
    cloudi_core_i_logger_interface:error(?MODULE, ?LINE,
                                         ?FUNCTION_NAME, ?FUNCTION_ARITY,
                                         Format, Args)).

-define(LOG_WARN(Format, Args),
    cloudi_core_i_logger_interface:warn(?MODULE, ?LINE,
                                        ?FUNCTION_NAME, ?FUNCTION_ARITY,
                                        Format, Args)).

-define(LOG_INFO(Format, Args),
    cloudi_core_i_logger_interface:info(?MODULE, ?LINE,
                                        ?FUNCTION_NAME, ?FUNCTION_ARITY,
                                        Format, Args)).

-define(LOG_DEBUG(Format, Args),
    cloudi_core_i_logger_interface:debug(?MODULE, ?LINE,
                                         ?FUNCTION_NAME, ?FUNCTION_ARITY,
                                         Format, Args)).

-define(LOG_TRACE(Format, Args),
    cloudi_core_i_logger_interface:trace(?MODULE, ?LINE,
                                         ?FUNCTION_NAME, ?FUNCTION_ARITY,
                                         Format, Args)).

-define(LOG(Level, Format, Args),
    cloudi_core_i_logger_interface:log(?MODULE, ?LINE,
                                       ?FUNCTION_NAME, ?FUNCTION_ARITY,
                                       Level, Format, Args)).

% Force the logging to be done synchronously
% (use if you need to be sure the data is written to the log,
%  assuming no filesystem errors occur when writing to the log file)

-define(LOG_FATAL_SYNC(Format, Args),
    cloudi_core_i_logger_interface:fatal_sync(?MODULE, ?LINE,
                                              ?FUNCTION_NAME, ?FUNCTION_ARITY,
                                              Format, Args)).

-define(LOG_ERROR_SYNC(Format, Args),
    cloudi_core_i_logger_interface:error_sync(?MODULE, ?LINE,
                                              ?FUNCTION_NAME, ?FUNCTION_ARITY,
                                              Format, Args)).

-define(LOG_WARN_SYNC(Format, Args),
    cloudi_core_i_logger_interface:warn_sync(?MODULE, ?LINE,
                                             ?FUNCTION_NAME, ?FUNCTION_ARITY,
                                             Format, Args)).

-define(LOG_INFO_SYNC(Format, Args),
    cloudi_core_i_logger_interface:info_sync(?MODULE, ?LINE,
                                             ?FUNCTION_NAME, ?FUNCTION_ARITY,
                                             Format, Args)).

-define(LOG_DEBUG_SYNC(Format, Args),
    cloudi_core_i_logger_interface:debug_sync(?MODULE, ?LINE,
                                              ?FUNCTION_NAME, ?FUNCTION_ARITY,
                                              Format, Args)).

-define(LOG_TRACE_SYNC(Format, Args),
    cloudi_core_i_logger_interface:trace_sync(?MODULE, ?LINE,
                                              ?FUNCTION_NAME, ?FUNCTION_ARITY,
                                              Format, Args)).

-define(LOG_SYNC(Level, Format, Args),
    cloudi_core_i_logger_interface:log_sync(?MODULE, ?LINE,
                                            ?FUNCTION_NAME, ?FUNCTION_ARITY,
                                            Level, Format, Args)).

% Apply an anonymous function if allowed by the current logging level setting

-define(LOG_FATAL_APPLY(F, A),
    cloudi_core_i_logger_interface:fatal_apply(F, A)).

-define(LOG_ERROR_APPLY(F, A),
    cloudi_core_i_logger_interface:error_apply(F, A)).

-define(LOG_WARN_APPLY(F, A),
    cloudi_core_i_logger_interface:warn_apply(F, A)).

-define(LOG_INFO_APPLY(F, A),
    cloudi_core_i_logger_interface:info_apply(F, A)).

-define(LOG_DEBUG_APPLY(F, A),
    cloudi_core_i_logger_interface:debug_apply(F, A)).

-define(LOG_TRACE_APPLY(F, A),
    cloudi_core_i_logger_interface:trace_apply(F, A)).

-define(LOG_APPLY(Level, F, A),
    cloudi_core_i_logger_interface:log_apply(Level, F, A)).

% Apply a module function if allowed by the current logging level setting

-define(LOG_FATAL_APPLY(M, F, A),
    cloudi_core_i_logger_interface:fatal_apply(M, F, A)).

-define(LOG_ERROR_APPLY(M, F, A),
    cloudi_core_i_logger_interface:error_apply(M, F, A)).

-define(LOG_WARN_APPLY(M, F, A),
    cloudi_core_i_logger_interface:warn_apply(M, F, A)).

-define(LOG_INFO_APPLY(M, F, A),
    cloudi_core_i_logger_interface:info_apply(M, F, A)).

-define(LOG_DEBUG_APPLY(M, F, A),
    cloudi_core_i_logger_interface:debug_apply(M, F, A)).

-define(LOG_TRACE_APPLY(M, F, A),
    cloudi_core_i_logger_interface:trace_apply(M, F, A)).

-define(LOG_APPLY(Level, M, F, A),
    cloudi_core_i_logger_interface:log_apply(Level, M, F, A)).

% Get/Set lager-compatible logging metadata

-define(LOG_METADATA_GET(),
    cloudi_core_i_logger:metadata_get()).

-define(LOG_METADATA_SET(L),
    cloudi_core_i_logger:metadata_set(L)).

