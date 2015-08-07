%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2009-2015, Michael Truog <mjtruog at gmail dot com>
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

% Typical logging output which will log asynchronously until the logger's
% message queue becomes too large, switching to synchronous logging
% while the message queue remains large

-define(LOG_FATAL(Format, Args),
    cloudi_core_i_logger_interface:fatal(?MODULE, ?LINE, Format, Args)).

-define(LOG_ERROR(Format, Args),
    cloudi_core_i_logger_interface:error(?MODULE, ?LINE, Format, Args)).

-define(LOG_WARN(Format, Args),
    cloudi_core_i_logger_interface:warn(?MODULE, ?LINE, Format, Args)).

-define(LOG_INFO(Format, Args),
    cloudi_core_i_logger_interface:info(?MODULE, ?LINE, Format, Args)).

-define(LOG_DEBUG(Format, Args),
    cloudi_core_i_logger_interface:debug(?MODULE, ?LINE, Format, Args)).

-define(LOG_TRACE(Format, Args),
    cloudi_core_i_logger_interface:trace(?MODULE, ?LINE, Format, Args)).

% Force the logging to be done synchronously to the local log only
% (if you are concerned about losing a logging message when the logging
%  is done asynchronously while the logger's message queue is somewhat large,
%  or if you want to make sure the logger's message queue is flushed,
%  during a rapid shutdown or crash, use these macros where necessary...
%  they are already used for service restart/stop events with the info
%  logging level, so it is unlikely it would be necessary to use the macros in
%  custom source code, if the info logging level is enabled)

-define(LOG_FATAL_SYNC(Format, Args),
    cloudi_core_i_logger_interface:fatal_sync(?MODULE, ?LINE, Format, Args)).

-define(LOG_ERROR_SYNC(Format, Args),
    cloudi_core_i_logger_interface:error_sync(?MODULE, ?LINE, Format, Args)).

-define(LOG_WARN_SYNC(Format, Args),
    cloudi_core_i_logger_interface:warn_sync(?MODULE, ?LINE, Format, Args)).

-define(LOG_INFO_SYNC(Format, Args),
    cloudi_core_i_logger_interface:info_sync(?MODULE, ?LINE, Format, Args)).

-define(LOG_DEBUG_SYNC(Format, Args),
    cloudi_core_i_logger_interface:debug_sync(?MODULE, ?LINE, Format, Args)).

-define(LOG_TRACE_SYNC(Format, Args),
    cloudi_core_i_logger_interface:trace_sync(?MODULE, ?LINE, Format, Args)).

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

% Get/Set lager-compatible logging metadata

-define(LOG_METADATA_GET(),
    cloudi_core_i_logger:metadata_get()).

-define(LOG_METADATA_SET(L),
    cloudi_core_i_logger:metadata_set(L)).

% Convenience macros
% (due to not using any parse transforms, the current function is unavailable,
%  but can be retrieved by the macros below with a small amount of runtime 
%  latency)

-ifndef(FUNCTION). % atom
-define(FUNCTION,
    erlang:element(2,
        erlang:element(2,
            erlang:process_info(self(), current_function)))).
-endif.
-ifndef(FUNCTION_ARITY). % string (list of characters)
-define(FUNCTION_ARITY,
    lists:concat(erlang:tl(lists:merge(
        [["/", E] || E <- erlang:tl(erlang:tuple_to_list(erlang:element(2,
            erlang:process_info(self(), current_function))))])))).
-endif.

