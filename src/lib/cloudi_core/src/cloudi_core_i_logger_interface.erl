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
%%% MIT License
%%%
%%% Copyright (c) 2011-2017 Michael Truog <mjtruog at gmail dot com>
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
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2011-2017 Michael Truog
%%% @version 1.7.1 {@date} {@time}
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

