%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%%
%%% MIT License
%%%
%%% Copyright (c) 2020-2021 Michael Truog <mjtruog at protonmail dot com>
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

-define(INTERFACE_MODULE_HEADER, "\
-module(cloudi_core_i_logger_interface).
-author('mjtruog at protonmail dot com').
-export([fatal/6, error/6, warn/6, info/6, debug/6, trace/6, log/7,
         fatal_sync/6, error_sync/6, warn_sync/6,
         info_sync/6, debug_sync/6, trace_sync/6, log_sync/7,
         fatal_apply/2, error_apply/2, warn_apply/2,
         info_apply/2, debug_apply/2, trace_apply/2, log_apply/3,
         fatal_apply/3, error_apply/3, warn_apply/3,
         info_apply/3, debug_apply/3, trace_apply/3, log_apply/4]).").

% Level =:= off
-define(INTERFACE_MODULE_CODE_LEVEL_OFF, "
fatal(_, _, _, _, _, _) -> ok.
error(_, _, _, _, _, _) -> ok.
warn(_, _, _, _, _, _) -> ok.
info(_, _, _, _, _, _) -> ok.
debug(_, _, _, _, _, _) -> ok.
trace(_, _, _, _, _, _) -> ok.
log(_, _, _, _, Level, _, _)
    when Level =:= fatal; Level =:= error; Level =:= warn;
         Level =:= info; Level =:= debug; Level =:= trace;
         Level =:= off -> ok.").
-define(INTERFACE_MODULE_ARGS_LEVEL_OFF,
        []).
-define(INTERFACE_MODULE_CODE_LEVEL_SYNC_OFF, "
fatal_sync(_, _, _, _, _, _) -> ok.
error_sync(_, _, _, _, _, _) -> ok.
warn_sync(_, _, _, _, _, _) -> ok.
info_sync(_, _, _, _, _, _) -> ok.
debug_sync(_, _, _, _, _, _) -> ok.
trace_sync(_, _, _, _, _, _) -> ok.
log_sync(_, _, _, _, Level, _, _)
    when Level =:= fatal; Level =:= error; Level =:= warn;
         Level =:= info; Level =:= debug; Level =:= trace;
         Level =:= off -> ok.").
-define(INTERFACE_MODULE_ARGS_LEVEL_SYNC_OFF,
        []).
-define(INTERFACE_MODULE_CODE_APPLY2_OFF, "
fatal_apply(_, _) -> undefined.
error_apply(_, _) -> undefined.
warn_apply(_, _) -> undefined.
info_apply(_, _) -> undefined.
debug_apply(_, _) -> undefined.
trace_apply(_, _) -> undefined.
log_apply(Level, _, _)
    when Level =:= fatal; Level =:= error; Level =:= warn;
         Level =:= info; Level =:= debug; Level =:= trace;
         Level =:= off -> undefined.").
-define(INTERFACE_MODULE_CODE_APPLY3_OFF, "
fatal_apply(_, _, _) -> undefined.
error_apply(_, _, _) -> undefined.
warn_apply(_, _, _) -> undefined.
info_apply(_, _, _) -> undefined.
debug_apply(_, _, _) -> undefined.
trace_apply(_, _, _) -> undefined.
log_apply(Level, _, _, _)
    when Level =:= fatal; Level =:= error; Level =:= warn;
         Level =:= info; Level =:= debug; Level =:= trace;
         Level =:= off -> undefined.").

% Level =:= fatal
-define(INTERFACE_MODULE_CODE_LEVEL_FATAL, "
fatal(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:fatal(~w, ~w, FileName, Line, Function, Arity,
                               Format, Arguments).
error(_, _, _, _, _, _) -> ok.
warn(_, _, _, _, _, _) -> ok.
info(_, _, _, _, _, _) -> ok.
debug(_, _, _, _, _, _) -> ok.
trace(_, _, _, _, _, _) -> ok.
log(FileName, Line, Function, Arity, Level, Format, Arguments)
    when Level =:= fatal ->
    cloudi_core_i_logger:Level(~w, ~w, FileName, Line, Function, Arity,
                               Format, Arguments);
log(_, _, _, _, Level, _, _)
    when Level =:= error; Level =:= warn;
         Level =:= info; Level =:= debug; Level =:= trace;
         Level =:= off -> ok.").
-define(INTERFACE_MODULE_ARGS_LEVEL_FATAL(ModeInterface, Process),
        [ModeInterface, Process, ModeInterface, Process]).
-define(INTERFACE_MODULE_CODE_LEVEL_SYNC_FATAL, "
fatal_sync(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:fatal(sync, ~w, FileName, Line, Function, Arity,
                               Format, Arguments).
error_sync(_, _, _, _, _, _) -> ok.
warn_sync(_, _, _, _, _, _) -> ok.
info_sync(_, _, _, _, _, _) -> ok.
debug_sync(_, _, _, _, _, _) -> ok.
trace_sync(_, _, _, _, _, _) -> ok.
log_sync(FileName, Line, Function, Arity, Level, Format, Arguments)
    when Level =:= fatal ->
    cloudi_core_i_logger:Level(sync, ~w, FileName, Line, Function, Arity,
                               Format, Arguments);
log_sync(_, _, _, _, Level, _, _)
    when Level =:= error; Level =:= warn;
         Level =:= info; Level =:= debug; Level =:= trace;
         Level =:= off -> ok.").
-define(INTERFACE_MODULE_ARGS_LEVEL_SYNC_FATAL(Process),
        [Process, Process]).
-define(INTERFACE_MODULE_CODE_APPLY2_FATAL, "
fatal_apply(F, A) ->
    erlang:apply(F, A).
error_apply(_, _) -> undefined.
warn_apply(_, _) -> undefined.
info_apply(_, _) -> undefined.
debug_apply(_, _) -> undefined.
trace_apply(_, _) -> undefined.
log_apply(Level, F, A)
    when Level =:= fatal ->
    erlang:apply(F, A);
log_apply(Level, _, _)
    when Level =:= error; Level =:= warn;
         Level =:= info; Level =:= debug; Level =:= trace;
         Level =:= off -> undefined.").
-define(INTERFACE_MODULE_CODE_APPLY3_FATAL, "
fatal_apply(M, F, A) ->
    erlang:apply(M, F, A).
error_apply(_, _, _) -> undefined.
warn_apply(_, _, _) -> undefined.
info_apply(_, _, _) -> undefined.
debug_apply(_, _, _) -> undefined.
trace_apply(_, _, _) -> undefined.
log_apply(Level, M, F, A)
    when Level =:= fatal ->
    erlang:apply(M, F, A);
log_apply(Level, _, _, _)
    when Level =:= error; Level =:= warn;
         Level =:= info; Level =:= debug; Level =:= trace;
         Level =:= off -> undefined.").

% Level =:= error
-define(INTERFACE_MODULE_CODE_LEVEL_ERROR, "
fatal(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:fatal(~w, ~w, FileName, Line, Function, Arity,
                               Format, Arguments).
error(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:error(~w, ~w, FileName, Line, Function, Arity,
                               Format, Arguments).
warn(_, _, _, _, _, _) -> ok.
info(_, _, _, _, _, _) -> ok.
debug(_, _, _, _, _, _) -> ok.
trace(_, _, _, _, _, _) -> ok.
log(FileName, Line, Function, Arity, Level, Format, Arguments)
    when Level =:= fatal; Level =:= error ->
    cloudi_core_i_logger:Level(~w, ~w, FileName, Line, Function, Arity,
                               Format, Arguments);
log(_, _, _, _, Level, _, _)
    when Level =:= warn;
         Level =:= info; Level =:= debug; Level =:= trace;
         Level =:= off -> ok.").
-define(INTERFACE_MODULE_ARGS_LEVEL_ERROR(ModeInterface, Process),
        [ModeInterface, Process, ModeInterface, Process,
         ModeInterface, Process]).
-define(INTERFACE_MODULE_CODE_LEVEL_SYNC_ERROR, "
fatal_sync(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:fatal(sync, ~w, FileName, Line, Function, Arity,
                               Format, Arguments).
error_sync(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:error(sync, ~w, FileName, Line, Function, Arity,
                               Format, Arguments).
warn_sync(_, _, _, _, _, _) -> ok.
info_sync(_, _, _, _, _, _) -> ok.
debug_sync(_, _, _, _, _, _) -> ok.
trace_sync(_, _, _, _, _, _) -> ok.
log_sync(FileName, Line, Function, Arity, Level, Format, Arguments)
    when Level =:= fatal; Level =:= error ->
    cloudi_core_i_logger:Level(sync, ~w, FileName, Line, Function, Arity,
                               Format, Arguments);
log_sync(_, _, _, _, Level, _, _)
    when Level =:= warn;
         Level =:= info; Level =:= debug; Level =:= trace;
         Level =:= off -> ok.").
-define(INTERFACE_MODULE_ARGS_LEVEL_SYNC_ERROR(Process),
        [Process, Process, Process]).
-define(INTERFACE_MODULE_CODE_APPLY2_ERROR, "
fatal_apply(F, A) ->
    erlang:apply(F, A).
error_apply(F, A) ->
    erlang:apply(F, A).
warn_apply(_, _) -> undefined.
info_apply(_, _) -> undefined.
debug_apply(_, _) -> undefined.
trace_apply(_, _) -> undefined.
log_apply(Level, F, A)
    when Level =:= fatal; Level =:= error ->
    erlang:apply(F, A);
log_apply(Level, _, _)
    when Level =:= warn;
         Level =:= info; Level =:= debug; Level =:= trace;
         Level =:= off -> undefined.").
-define(INTERFACE_MODULE_CODE_APPLY3_ERROR, "
fatal_apply(M, F, A) ->
    erlang:apply(M, F, A).
error_apply(M, F, A) ->
    erlang:apply(M, F, A).
warn_apply(_, _, _) -> undefined.
info_apply(_, _, _) -> undefined.
debug_apply(_, _, _) -> undefined.
trace_apply(_, _, _) -> undefined.
log_apply(Level, M, F, A)
    when Level =:= fatal; Level =:= error ->
    erlang:apply(M, F, A);
log_apply(Level, _, _, _)
    when Level =:= warn;
         Level =:= info; Level =:= debug; Level =:= trace;
         Level =:= off -> undefined.").

% Level =:= warn
-define(INTERFACE_MODULE_CODE_LEVEL_WARN, "
fatal(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:fatal(~w, ~w, FileName, Line, Function, Arity,
                               Format, Arguments).
error(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:error(~w, ~w, FileName, Line, Function, Arity,
                               Format, Arguments).
warn(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:warn(~w, ~w, FileName, Line, Function, Arity,
                              Format, Arguments).
info(_, _, _, _, _, _) -> ok.
debug(_, _, _, _, _, _) -> ok.
trace(_, _, _, _, _, _) -> ok.
log(FileName, Line, Function, Arity, Level, Format, Arguments)
    when Level =:= fatal; Level =:= error; Level =:= warn ->
    cloudi_core_i_logger:Level(~w, ~w, FileName, Line, Function, Arity,
                               Format, Arguments);
log(_, _, _, _, Level, _, _)
    when Level =:= info; Level =:= debug; Level =:= trace;
         Level =:= off -> ok.").
-define(INTERFACE_MODULE_ARGS_LEVEL_WARN(ModeInterface, Process),
        [ModeInterface, Process, ModeInterface, Process,
         ModeInterface, Process, ModeInterface, Process]).
-define(INTERFACE_MODULE_CODE_LEVEL_SYNC_WARN, "
fatal_sync(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:fatal(sync, ~w, FileName, Line, Function, Arity,
                               Format, Arguments).
error_sync(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:error(sync, ~w, FileName, Line, Function, Arity,
                               Format, Arguments).
warn_sync(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:warn(sync, ~w, FileName, Line, Function, Arity,
                              Format, Arguments).
info_sync(_, _, _, _, _, _) -> ok.
debug_sync(_, _, _, _, _, _) -> ok.
trace_sync(_, _, _, _, _, _) -> ok.
log_sync(FileName, Line, Function, Arity, Level, Format, Arguments)
    when Level =:= fatal; Level =:= error; Level =:= warn ->
    cloudi_core_i_logger:Level(sync, ~w, FileName, Line, Function, Arity,
                               Format, Arguments);
log_sync(_, _, _, _, Level, _, _)
    when Level =:= info; Level =:= debug; Level =:= trace;
         Level =:= off -> ok.").
-define(INTERFACE_MODULE_ARGS_LEVEL_SYNC_WARN(Process),
        [Process, Process, Process, Process]).
-define(INTERFACE_MODULE_CODE_APPLY2_WARN, "
fatal_apply(F, A) ->
    erlang:apply(F, A).
error_apply(F, A) ->
    erlang:apply(F, A).
warn_apply(F, A) ->
    erlang:apply(F, A).
info_apply(_, _) -> undefined.
debug_apply(_, _) -> undefined.
trace_apply(_, _) -> undefined.
log_apply(Level, F, A)
    when Level =:= fatal; Level =:= error; Level =:= warn ->
    erlang:apply(F, A);
log_apply(Level, _, _)
    when Level =:= info; Level =:= debug; Level =:= trace;
         Level =:= off -> undefined.").
-define(INTERFACE_MODULE_CODE_APPLY3_WARN, "
fatal_apply(M, F, A) ->
    erlang:apply(M, F, A).
error_apply(M, F, A) ->
    erlang:apply(M, F, A).
warn_apply(M, F, A) ->
    erlang:apply(M, F, A).
info_apply(_, _, _) -> undefined.
debug_apply(_, _, _) -> undefined.
trace_apply(_, _, _) -> undefined.
log_apply(Level, M, F, A)
    when Level =:= fatal; Level =:= error; Level =:= warn ->
    erlang:apply(M, F, A);
log_apply(Level, _, _, _)
    when Level =:= info; Level =:= debug; Level =:= trace;
         Level =:= off -> undefined.").

% Level =:= info
-define(INTERFACE_MODULE_CODE_LEVEL_INFO, "
fatal(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:fatal(~w, ~w, FileName, Line, Function, Arity,
                               Format, Arguments).
error(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:error(~w, ~w, FileName, Line, Function, Arity,
                               Format, Arguments).
warn(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:warn(~w, ~w, FileName, Line, Function, Arity,
                              Format, Arguments).
info(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:info(~w, ~w, FileName, Line, Function, Arity,
                              Format, Arguments).
debug(_, _, _, _, _, _) -> ok.
trace(_, _, _, _, _, _) -> ok.
log(FileName, Line, Function, Arity, Level, Format, Arguments)
    when Level =:= fatal; Level =:= error; Level =:= warn;
         Level =:= info ->
    cloudi_core_i_logger:Level(~w, ~w, FileName, Line, Function, Arity,
                               Format, Arguments);
log(_, _, _, _, Level, _, _)
    when Level =:= debug; Level =:= trace;
         Level =:= off -> ok.").
-define(INTERFACE_MODULE_ARGS_LEVEL_INFO(ModeInterface, Process),
        [ModeInterface, Process, ModeInterface, Process,
         ModeInterface, Process, ModeInterface, Process,
         ModeInterface, Process]).
-define(INTERFACE_MODULE_CODE_LEVEL_SYNC_INFO, "
fatal_sync(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:fatal(sync, ~w, FileName, Line, Function, Arity,
                               Format, Arguments).
error_sync(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:error(sync, ~w, FileName, Line, Function, Arity,
                               Format, Arguments).
warn_sync(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:warn(sync, ~w, FileName, Line, Function, Arity,
                              Format, Arguments).
info_sync(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:info(sync, ~w, FileName, Line, Function, Arity,
                              Format, Arguments).
debug_sync(_, _, _, _, _, _) -> ok.
trace_sync(_, _, _, _, _, _) -> ok.
log_sync(FileName, Line, Function, Arity, Level, Format, Arguments)
    when Level =:= fatal; Level =:= error; Level =:= warn;
         Level =:= info ->
    cloudi_core_i_logger:Level(sync, ~w, FileName, Line, Function, Arity,
                               Format, Arguments);
log_sync(_, _, _, _, Level, _, _)
    when Level =:= debug; Level =:= trace;
         Level =:= off -> ok.").
-define(INTERFACE_MODULE_ARGS_LEVEL_SYNC_INFO(Process),
        [Process, Process, Process, Process, Process]).
-define(INTERFACE_MODULE_CODE_APPLY2_INFO, "
fatal_apply(F, A) ->
    erlang:apply(F, A).
error_apply(F, A) ->
    erlang:apply(F, A).
warn_apply(F, A) ->
    erlang:apply(F, A).
info_apply(F, A) ->
    erlang:apply(F, A).
debug_apply(_, _) -> undefined.
trace_apply(_, _) -> undefined.
log_apply(Level, F, A)
    when Level =:= fatal; Level =:= error; Level =:= warn;
         Level =:= info ->
    erlang:apply(F, A);
log_apply(Level, _, _)
    when Level =:= debug; Level =:= trace;
         Level =:= off -> undefined.").
-define(INTERFACE_MODULE_CODE_APPLY3_INFO, "
fatal_apply(M, F, A) ->
    erlang:apply(M, F, A).
error_apply(M, F, A) ->
    erlang:apply(M, F, A).
warn_apply(M, F, A) ->
    erlang:apply(M, F, A).
info_apply(M, F, A) ->
    erlang:apply(M, F, A).
debug_apply(_, _, _) -> undefined.
trace_apply(_, _, _) -> undefined.
log_apply(Level, M, F, A)
    when Level =:= fatal; Level =:= error; Level =:= warn;
         Level =:= info ->
    erlang:apply(M, F, A);
log_apply(Level, _, _, _)
    when Level =:= debug; Level =:= trace;
         Level =:= off -> undefined.").

% Level =:= debug
-define(INTERFACE_MODULE_CODE_LEVEL_DEBUG, "
fatal(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:fatal(~w, ~w, FileName, Line, Function, Arity,
                               Format, Arguments).
error(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:error(~w, ~w, FileName, Line, Function, Arity,
                               Format, Arguments).
warn(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:warn(~w, ~w, FileName, Line, Function, Arity,
                              Format, Arguments).
info(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:info(~w, ~w, FileName, Line, Function, Arity,
                              Format, Arguments).
debug(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:debug(~w, ~w, FileName, Line, Function, Arity,
                               Format, Arguments).
trace(_, _, _, _, _, _) -> ok.
log(FileName, Line, Function, Arity, Level, Format, Arguments)
    when Level =:= fatal; Level =:= error; Level =:= warn;
         Level =:= info; Level =:= debug ->
    cloudi_core_i_logger:Level(~w, ~w, FileName, Line, Function, Arity,
                               Format, Arguments);
log(_, _, _, _, Level, _, _)
    when Level =:= trace;
         Level =:= off -> ok.").
-define(INTERFACE_MODULE_ARGS_LEVEL_DEBUG(ModeInterface, Process),
        [ModeInterface, Process, ModeInterface, Process,
         ModeInterface, Process, ModeInterface, Process,
         ModeInterface, Process, ModeInterface, Process]).
-define(INTERFACE_MODULE_CODE_LEVEL_SYNC_DEBUG, "
fatal_sync(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:fatal(sync, ~w, FileName, Line, Function, Arity,
                               Format, Arguments).
error_sync(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:error(sync, ~w, FileName, Line, Function, Arity,
                               Format, Arguments).
warn_sync(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:warn(sync, ~w, FileName, Line, Function, Arity,
                              Format, Arguments).
info_sync(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:info(sync, ~w, FileName, Line, Function, Arity,
                              Format, Arguments).
debug_sync(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:debug(sync, ~w, FileName, Line, Function, Arity,
                               Format, Arguments).
trace_sync(_, _, _, _, _, _) -> ok.
log_sync(FileName, Line, Function, Arity, Level, Format, Arguments)
    when Level =:= fatal; Level =:= error; Level =:= warn;
         Level =:= info; Level =:= debug ->
    cloudi_core_i_logger:Level(sync, ~w, FileName, Line, Function, Arity,
                               Format, Arguments);
log_sync(_, _, _, _, Level, _, _)
    when Level =:= trace;
         Level =:= off -> ok.").
-define(INTERFACE_MODULE_ARGS_LEVEL_SYNC_DEBUG(Process),
        [Process, Process, Process, Process, Process,
         Process]).
-define(INTERFACE_MODULE_CODE_APPLY2_DEBUG, "
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
trace_apply(_, _) -> undefined.
log_apply(Level, F, A)
    when Level =:= fatal; Level =:= error; Level =:= warn;
         Level =:= info; Level =:= debug ->
    erlang:apply(F, A);
log_apply(Level, _, _)
    when Level =:= trace;
         Level =:= off -> undefined.").
-define(INTERFACE_MODULE_CODE_APPLY3_DEBUG, "
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
trace_apply(_, _, _) -> undefined.
log_apply(Level, M, F, A)
    when Level =:= fatal; Level =:= error; Level =:= warn;
         Level =:= info; Level =:= debug ->
    erlang:apply(M, F, A);
log_apply(Level, _, _, _)
    when Level =:= trace;
         Level =:= off -> undefined.").

% Level =:= trace
-define(INTERFACE_MODULE_CODE_LEVEL_TRACE, "
fatal(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:fatal(~w, ~w, FileName, Line, Function, Arity,
                               Format, Arguments).
error(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:error(~w, ~w, FileName, Line, Function, Arity,
                               Format, Arguments).
warn(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:warn(~w, ~w, FileName, Line, Function, Arity,
                              Format, Arguments).
info(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:info(~w, ~w, FileName, Line, Function, Arity,
                              Format, Arguments).
debug(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:debug(~w, ~w, FileName, Line, Function, Arity,
                               Format, Arguments).
trace(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:trace(~w, ~w, FileName, Line, Function, Arity,
                               Format, Arguments).
log(FileName, Line, Function, Arity, Level, Format, Arguments)
    when Level =:= fatal; Level =:= error; Level =:= warn;
         Level =:= info; Level =:= debug; Level =:= trace ->
    cloudi_core_i_logger:Level(~w, ~w, FileName, Line, Function, Arity,
                               Format, Arguments);
log(_, _, _, _, Level, _, _)
    when Level =:= off -> ok.").
-define(INTERFACE_MODULE_ARGS_LEVEL_TRACE(ModeInterface, Process),
        [ModeInterface, Process, ModeInterface, Process,
         ModeInterface, Process, ModeInterface, Process,
         ModeInterface, Process, ModeInterface, Process,
         ModeInterface, Process]).
-define(INTERFACE_MODULE_CODE_LEVEL_SYNC_TRACE, "
fatal_sync(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:fatal(sync, ~w, FileName, Line, Function, Arity,
                               Format, Arguments).
error_sync(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:error(sync, ~w, FileName, Line, Function, Arity,
                               Format, Arguments).
warn_sync(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:warn(sync, ~w, FileName, Line, Function, Arity,
                              Format, Arguments).
info_sync(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:info(sync, ~w, FileName, Line, Function, Arity,
                              Format, Arguments).
debug_sync(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:debug(sync, ~w, FileName, Line, Function, Arity,
                               Format, Arguments).
trace_sync(FileName, Line, Function, Arity, Format, Arguments) ->
    cloudi_core_i_logger:trace(sync, ~w, FileName, Line, Function, Arity,
                               Format, Arguments).
log_sync(FileName, Line, Function, Arity, Level, Format, Arguments)
    when Level =:= fatal; Level =:= error; Level =:= warn;
         Level =:= info; Level =:= debug; Level =:= trace ->
    cloudi_core_i_logger:Level(sync, ~w, FileName, Line, Function, Arity,
                               Format, Arguments);
log_sync(_, _, _, _, Level, _, _)
    when Level =:= off -> ok.").
-define(INTERFACE_MODULE_ARGS_LEVEL_SYNC_TRACE(Process),
        [Process, Process, Process, Process, Process,
         Process, Process]).
-define(INTERFACE_MODULE_CODE_APPLY2_TRACE, "
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
log_apply(Level, F, A)
    when Level =:= fatal; Level =:= error; Level =:= warn;
         Level =:= info; Level =:= debug; Level =:= trace ->
    erlang:apply(F, A);
log_apply(Level, _, _)
    when Level =:= off -> undefined.").
-define(INTERFACE_MODULE_CODE_APPLY3_TRACE, "
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
log_apply(Level, M, F, A)
    when Level =:= fatal; Level =:= error; Level =:= warn;
         Level =:= info; Level =:= debug; Level =:= trace ->
    erlang:apply(M, F, A);
log_apply(Level, _, _, _)
    when Level =:= off -> undefined.").

% Mode =:= async | sync
-define(INTERFACE_MODULE_NORMAL_CODE(Level),
    if
        Level =:= off ->
            ?INTERFACE_MODULE_HEADER
            ?INTERFACE_MODULE_CODE_LEVEL_OFF
            ?INTERFACE_MODULE_CODE_LEVEL_SYNC_OFF
            ?INTERFACE_MODULE_CODE_APPLY2_OFF
            ?INTERFACE_MODULE_CODE_APPLY3_OFF;
        Level =:= fatal ->
            ?INTERFACE_MODULE_HEADER
            ?INTERFACE_MODULE_CODE_LEVEL_FATAL
            ?INTERFACE_MODULE_CODE_LEVEL_SYNC_FATAL
            ?INTERFACE_MODULE_CODE_APPLY2_FATAL
            ?INTERFACE_MODULE_CODE_APPLY3_FATAL;
        Level =:= error ->
            ?INTERFACE_MODULE_HEADER
            ?INTERFACE_MODULE_CODE_LEVEL_ERROR
            ?INTERFACE_MODULE_CODE_LEVEL_SYNC_ERROR
            ?INTERFACE_MODULE_CODE_APPLY2_ERROR
            ?INTERFACE_MODULE_CODE_APPLY3_ERROR;
        Level =:= warn ->
            ?INTERFACE_MODULE_HEADER
            ?INTERFACE_MODULE_CODE_LEVEL_WARN
            ?INTERFACE_MODULE_CODE_LEVEL_SYNC_WARN
            ?INTERFACE_MODULE_CODE_APPLY2_WARN
            ?INTERFACE_MODULE_CODE_APPLY3_WARN;
        Level =:= info ->
            ?INTERFACE_MODULE_HEADER
            ?INTERFACE_MODULE_CODE_LEVEL_INFO
            ?INTERFACE_MODULE_CODE_LEVEL_SYNC_INFO
            ?INTERFACE_MODULE_CODE_APPLY2_INFO
            ?INTERFACE_MODULE_CODE_APPLY3_INFO;
        Level =:= debug ->
            ?INTERFACE_MODULE_HEADER
            ?INTERFACE_MODULE_CODE_LEVEL_DEBUG
            ?INTERFACE_MODULE_CODE_LEVEL_SYNC_DEBUG
            ?INTERFACE_MODULE_CODE_APPLY2_DEBUG
            ?INTERFACE_MODULE_CODE_APPLY3_DEBUG;
        Level =:= trace ->
            ?INTERFACE_MODULE_HEADER
            ?INTERFACE_MODULE_CODE_LEVEL_TRACE
            ?INTERFACE_MODULE_CODE_LEVEL_SYNC_TRACE
            ?INTERFACE_MODULE_CODE_APPLY2_TRACE
            ?INTERFACE_MODULE_CODE_APPLY3_TRACE
    end).
-define(INTERFACE_MODULE_NORMAL_ARGS(Level, ModeInterface, Destination),
    if
        Level =:= off ->
            ?INTERFACE_MODULE_ARGS_LEVEL_OFF ++
            ?INTERFACE_MODULE_ARGS_LEVEL_SYNC_OFF;
        Level =:= fatal ->
            ?INTERFACE_MODULE_ARGS_LEVEL_FATAL(ModeInterface, Destination) ++
            ?INTERFACE_MODULE_ARGS_LEVEL_SYNC_FATAL(Destination);
        Level =:= error ->
            ?INTERFACE_MODULE_ARGS_LEVEL_ERROR(ModeInterface, Destination) ++
            ?INTERFACE_MODULE_ARGS_LEVEL_SYNC_ERROR(Destination);
        Level =:= warn ->
            ?INTERFACE_MODULE_ARGS_LEVEL_WARN(ModeInterface, Destination) ++
            ?INTERFACE_MODULE_ARGS_LEVEL_SYNC_WARN(Destination);
        Level =:= info ->
            ?INTERFACE_MODULE_ARGS_LEVEL_INFO(ModeInterface, Destination) ++
            ?INTERFACE_MODULE_ARGS_LEVEL_SYNC_INFO(Destination);
        Level =:= debug ->
            ?INTERFACE_MODULE_ARGS_LEVEL_DEBUG(ModeInterface, Destination) ++
            ?INTERFACE_MODULE_ARGS_LEVEL_SYNC_DEBUG(Destination);
        Level =:= trace ->
            ?INTERFACE_MODULE_ARGS_LEVEL_TRACE(ModeInterface, Destination) ++
            ?INTERFACE_MODULE_ARGS_LEVEL_SYNC_TRACE(Destination)
    end).

% Mode =:= overload
% (anything not sync is turned off except for error, apply is left as-is)
-define(INTERFACE_MODULE_OVERLOAD_CODE(Level),
    if
        Level =:= off ->
            ?INTERFACE_MODULE_HEADER
            ?INTERFACE_MODULE_CODE_LEVEL_OFF
            ?INTERFACE_MODULE_CODE_LEVEL_SYNC_OFF
            ?INTERFACE_MODULE_CODE_APPLY2_OFF
            ?INTERFACE_MODULE_CODE_APPLY3_OFF;
        Level =:= fatal ->
            ?INTERFACE_MODULE_HEADER
            ?INTERFACE_MODULE_CODE_LEVEL_FATAL
            ?INTERFACE_MODULE_CODE_LEVEL_SYNC_FATAL
            ?INTERFACE_MODULE_CODE_APPLY2_FATAL
            ?INTERFACE_MODULE_CODE_APPLY3_FATAL;
        Level =:= error ->
            ?INTERFACE_MODULE_HEADER
            ?INTERFACE_MODULE_CODE_LEVEL_ERROR
            ?INTERFACE_MODULE_CODE_LEVEL_SYNC_ERROR
            ?INTERFACE_MODULE_CODE_APPLY2_ERROR
            ?INTERFACE_MODULE_CODE_APPLY3_ERROR;
        Level =:= warn ->
            ?INTERFACE_MODULE_HEADER
            ?INTERFACE_MODULE_CODE_LEVEL_ERROR
            ?INTERFACE_MODULE_CODE_LEVEL_SYNC_WARN
            ?INTERFACE_MODULE_CODE_APPLY2_WARN
            ?INTERFACE_MODULE_CODE_APPLY3_WARN;
        Level =:= info ->
            ?INTERFACE_MODULE_HEADER
            ?INTERFACE_MODULE_CODE_LEVEL_ERROR
            ?INTERFACE_MODULE_CODE_LEVEL_SYNC_INFO
            ?INTERFACE_MODULE_CODE_APPLY2_INFO
            ?INTERFACE_MODULE_CODE_APPLY3_INFO;
        Level =:= debug ->
            ?INTERFACE_MODULE_HEADER
            ?INTERFACE_MODULE_CODE_LEVEL_ERROR
            ?INTERFACE_MODULE_CODE_LEVEL_SYNC_DEBUG
            ?INTERFACE_MODULE_CODE_APPLY2_DEBUG
            ?INTERFACE_MODULE_CODE_APPLY3_DEBUG;
        Level =:= trace ->
            ?INTERFACE_MODULE_HEADER
            ?INTERFACE_MODULE_CODE_LEVEL_ERROR
            ?INTERFACE_MODULE_CODE_LEVEL_SYNC_TRACE
            ?INTERFACE_MODULE_CODE_APPLY2_TRACE
            ?INTERFACE_MODULE_CODE_APPLY3_TRACE
    end).
-define(INTERFACE_MODULE_OVERLOAD_ARGS(Level, Destination),
    if
        Level =:= off ->
            ?INTERFACE_MODULE_ARGS_LEVEL_OFF ++
            ?INTERFACE_MODULE_ARGS_LEVEL_SYNC_OFF;
        Level =:= fatal ->
            ?INTERFACE_MODULE_ARGS_LEVEL_FATAL(sync, Destination) ++
            ?INTERFACE_MODULE_ARGS_LEVEL_SYNC_FATAL(Destination);
        Level =:= error ->
            ?INTERFACE_MODULE_ARGS_LEVEL_ERROR(sync, Destination) ++
            ?INTERFACE_MODULE_ARGS_LEVEL_SYNC_ERROR(Destination);
        Level =:= warn ->
            ?INTERFACE_MODULE_ARGS_LEVEL_ERROR(sync, Destination) ++
            ?INTERFACE_MODULE_ARGS_LEVEL_SYNC_WARN(Destination);
        Level =:= info ->
            ?INTERFACE_MODULE_ARGS_LEVEL_ERROR(sync, Destination) ++
            ?INTERFACE_MODULE_ARGS_LEVEL_SYNC_INFO(Destination);
        Level =:= debug ->
            ?INTERFACE_MODULE_ARGS_LEVEL_ERROR(sync, Destination) ++
            ?INTERFACE_MODULE_ARGS_LEVEL_SYNC_DEBUG(Destination);
        Level =:= trace ->
            ?INTERFACE_MODULE_ARGS_LEVEL_ERROR(sync, Destination) ++
            ?INTERFACE_MODULE_ARGS_LEVEL_SYNC_TRACE(Destination)
    end).
