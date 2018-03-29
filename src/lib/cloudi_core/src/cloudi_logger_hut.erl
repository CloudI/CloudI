%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Logger Hut Integration==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2017 Michael Truog <mjtruog at protonmail dot com>
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
%%% @author Michael Truog <mjtruog at protonmail dot com>
%%% @copyright 2017 Michael Truog
%%% @version 1.7.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_logger_hut).
-author('mjtruog at protonmail dot com').

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
          Opts0 :: list({module, atom()} |
                        {line, pos_integer()} |
                        {function_name, atom()} |
                        {function_arity, arity()})) ->
    ok.

log(Level, Fmt, Args, Opts0) ->
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
    {Module, Line, Opts3} = case lists:keytake(module, 1, Opts0) of
        false ->
            {'HUT', 0, Opts0};
        {value, {module, ModuleValue}, Opts1} ->
            case lists:keytake(line, 1, Opts1) of
                false ->
                    {ModuleValue, 0, Opts1};
                {value, {line, LineValue}, Opts2} ->
                    {ModuleValue, LineValue, Opts2}
            end
    end,
    {FunctionName, FunctionArity,
     _} = case lists:keytake(function_name, 1, Opts3) of
        false ->
            {undefined, undefined, Opts3};
        {value, {function_name, FunctionNameValue}, Opts4} ->
            case lists:keytake(function_arity, 1, Opts4) of
                false ->
                    {FunctionNameValue, undefined, Opts4};
                {value, {function_arity, FunctionArityValue}, Opts5} ->
                    {FunctionNameValue, FunctionArityValue, Opts5}
            end
    end,
    if
        LogLevel =:= undefined ->
            cloudi_core_i_logger_interface:error('HUT(invalid_level)', 0,
                                                 FunctionName,
                                                 FunctionArity,
                                                 Fmt, Args);
        true ->
            cloudi_core_i_logger_interface:LogLevel(Module, Line,
                                                    FunctionName,
                                                    FunctionArity,
                                                    Fmt, Args)
    end.

