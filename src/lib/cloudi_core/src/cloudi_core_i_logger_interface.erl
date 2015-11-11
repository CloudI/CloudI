%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Fake CloudI Logger Interface==
%%% These functions get replaced dynamically.
%%% This module just helps allow code analysis.
%%% @end
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
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2011-2015 Michael Truog
%%% @version 1.5.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_core_i_logger_interface).
-author('mjtruog [at] gmail (dot) com').
-export([fatal/6, error/6, warn/6, info/6, debug/6, trace/6,
         fatal_sync/6, error_sync/6, warn_sync/6,
         info_sync/6, debug_sync/6, trace_sync/6,
         fatal_apply/2, error_apply/2, warn_apply/2,
         info_apply/2, debug_apply/2, trace_apply/2,
         fatal_apply/3, error_apply/3, warn_apply/3,
         info_apply/3, debug_apply/3, trace_apply/3]).
fatal(Module, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:fatal(async, cloudi_core_i_logger,
                               Module, Line, Function, Arity,
                               Format, Arguments).
error(Module, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:error(async, cloudi_core_i_logger,
                               Module, Line, Function, Arity,
                               Format, Arguments).
warn(Module, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:warn(async, cloudi_core_i_logger,
                              Module, Line, Function, Arity,
                              Format, Arguments).
info(Module, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:info(async, cloudi_core_i_logger,
                              Module, Line, Function, Arity,
                              Format, Arguments).
debug(Module, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:debug(async, cloudi_core_i_logger,
                               Module, Line, Function, Arity,
                               Format, Arguments).
trace(Module, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:trace(async, cloudi_core_i_logger,
                               Module, Line, Function, Arity,
                               Format, Arguments).
fatal_sync(Module, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:fatal(sync, cloudi_core_i_logger,
                               Module, Line, Function, Arity,
                               Format, Arguments).
error_sync(Module, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:error(sync, cloudi_core_i_logger,
                               Module, Line, Function, Arity,
                               Format, Arguments).
warn_sync(Module, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:warn(sync, cloudi_core_i_logger,
                              Module, Line, Function, Arity,
                              Format, Arguments).
info_sync(Module, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:info(sync, cloudi_core_i_logger,
                              Module, Line, Function, Arity,
                              Format, Arguments).
debug_sync(Module, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:debug(sync, cloudi_core_i_logger,
                               Module, Line, Function, Arity,
                               Format, Arguments).
trace_sync(Module, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:trace(sync, cloudi_core_i_logger,
                               Module, Line, Function, Arity,
                               Format, Arguments).
fatal_apply(F, A) ->
    erlang:apply(F, A).
error_apply(F, A) ->
    erlang:apply(F, A).
warn_apply(F, A) ->
    erlang:apply(F, A).
info_apply(F, A) ->
    erlang:apply(F, A).
debug_apply(F, A) ->
    erlang:apply(F, A).
trace_apply(F, A) ->
    erlang:apply(F, A).
fatal_apply(M, F, A) ->
    erlang:apply(M, F, A).
error_apply(M, F, A) ->
    erlang:apply(M, F, A).
warn_apply(M, F, A) ->
    erlang:apply(M, F, A).
info_apply(M, F, A) ->
    erlang:apply(M, F, A).
debug_apply(M, F, A) ->
    erlang:apply(M, F, A).
trace_apply(M, F, A) ->
    erlang:apply(M, F, A).

