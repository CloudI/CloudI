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
%%% Copyright (c) 2017-2021 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2017-2021 Michael Truog
%%% @version 2.0.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_logger_hut).
-author('mjtruog at protonmail dot com').

%% external interface
-export([log/4,
         slog/3]).

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
    log_output(log_level(Level), Module, Line,
               FunctionName, FunctionArity, Fmt, Args).

%%-------------------------------------------------------------------------
%% @doc
%% ===Hut structured report callback for CloudI logging.===
%% @end
%%-------------------------------------------------------------------------

-spec slog(Level :: critical | alert | emergency |
                    error | warning | notice | info | debug,
           Data :: string() | #{} | list({atom(), any()}),
           Meta0 :: #{atom() := any()}) ->
    ok.

slog(Level, Data, Meta0) ->
    {Module,
     FunctionName,
     FunctionArity,
     Meta2} = case maps:take(mfa, Meta0) of
        {{ModuleValue, FunctionNameValue, FunctionArityValue}, Meta1} ->
            {ModuleValue, FunctionNameValue, FunctionArityValue, Meta1};
        error ->
            {'HUT', undefined, undefined, Meta0}
    end,
    {Line,
     Meta4} = case maps:take(line, Meta2) of
        {LineValue, Meta3} ->
            {LineValue, Meta3};
        error ->
            {0, Meta2}
    end,
    Meta5 = maps:remove(file, Meta4),
    {Format,
     MetaN} = case Data of
        [{_, _} | _] = Report ->
            {"", maps:merge(Meta5, maps:from_list(Report))};
        Report when is_map(Report) ->
            {"", maps:merge(Meta5, Report)};
        FormatValue when is_list(FormatValue) ->
            {FormatValue, Meta5}
    end,
    ok = cloudi_core_i_logger:metadata_set(MetaN),
    log_output(log_level(Level), Module, Line,
               FunctionName, FunctionArity, Format, undefined).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

% log level consistent with
% cloudi_core_i_logger:lager_severity_output/1,
% cloudi_core_i_logger:lager_severity_input/1 and
% cloudi_core_i_configuration:logging_formatter_level/1
log_level(critical) -> fatal;
log_level(alert) -> fatal;
log_level(emergency) -> fatal;
log_level(error) -> error;
log_level(warning) -> warn;
log_level(notice) -> warn;
log_level(info) -> info;
log_level(debug) -> debug;
log_level(_) -> undefined.

log_output(undefined, _Module, _Line,
           FunctionName, FunctionArity, Format, Args) ->
    cloudi_core_i_logger_interface:error('HUT(invalid_level)', 0,
                                         FunctionName,
                                         FunctionArity,
                                         Format, Args);
log_output(LogLevel, Module, Line,
           FunctionName, FunctionArity, Format, Args) ->
    cloudi_core_i_logger_interface:LogLevel(Module, Line,
                                            FunctionName,
                                            FunctionArity,
                                            Format, Args).

