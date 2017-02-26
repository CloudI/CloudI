%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Logger Hut Integration==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2017, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2017 Michael Truog
%%% @version 1.6.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_logger_hut).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([log/4]).

%%-------------------------------------------------------------------------
%% @doc
%% ===Hut callback for CloudI logging.===
%% @end
%%-------------------------------------------------------------------------

-spec log(Level :: critical | alert | emergency |
                   error | warning | notice | info | debug,
          Fmt :: string(),
          Args :: list(),
          _Opts :: list()) ->
    ok.

log(Level, Fmt, Args, _Opts) ->
    % consistent with
    % cloudi_core_i_logger:lager_severity_output/1,
    % cloudi_core_i_logger:lager_severity_input/1 and
    % cloudi_core_i_configuration:logging_formatter_level/1
    LogLevel = if
        Level =:= critical; Level =:= alert; Level =:= emergency ->
            fatal;
        Level =:= error ->
            error;
        Level =:= warning; Level =:= notice ->
            warn;
        Level =:= info ->
            info;
        Level =:= debug ->
            debug;
        true ->
            undefined
    end,
    if
        LogLevel =:= undefined ->
            cloudi_core_i_logger_interface:error('HUT(invalid_level)', 0,
                                                 undefined, undefined,
                                                 Fmt, Args);
        true ->
            cloudi_core_i_logger_interface:LogLevel('HUT', 0,
                                                    undefined, undefined,
                                                    Fmt, Args)
    end.

