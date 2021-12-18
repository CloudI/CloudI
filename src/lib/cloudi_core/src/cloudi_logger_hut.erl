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
%%% @version 2.0.5 {@date} {@time}
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
                        {file, nonempty_string()} |
                        {line, pos_integer()} |
                        {function_name, atom()} |
                        {function_arity, arity()})) ->
    ok.

log(Level, Fmt, Args, Opts0) ->
    {FileName, Line, Opts3} = case lists:keytake(file, 1, Opts0) of
        false ->
            {"HUT", 0, Opts0};
        {value, {file, FileNameValue}, Opts1} ->
            case lists:keytake(line, 1, Opts1) of
                false ->
                    {FileNameValue, 0, Opts1};
                {value, {line, LineValue}, Opts2} ->
                    {FileNameValue, LineValue, Opts2}
            end
    end,
    {FunctionName, FunctionArity,
     OptsN} = case lists:keytake(function_name, 1, Opts3) of
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
    ok = cloudi_core_i_logger:metadata_set(maps:from_list(OptsN)),
    log_output(log_level(Level), FileName, Line,
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
    {FileName, Line, Meta3} = case maps:take(file, Meta0) of
        error ->
            {"HUT", 0, Meta0};
        {FileNameValue, Meta1} ->
            case maps:take(line, Meta1) of
                error ->
                    {FileNameValue, 0, Meta1};
                {LineValue, Meta2} ->
                    {FileNameValue, LineValue, Meta2}
            end
    end,
    {FunctionName, FunctionArity, MetaN} = case maps:take(mfa, Meta3) of
        error ->
            {undefined, undefined, Meta3};
        {{_, FunctionNameValue, FunctionArityValue}, Meta4} ->
            {FunctionNameValue, FunctionArityValue, Meta4}
    end,
    Format = case Data of
        [{_, _} | _] = Report ->
            cloudi_string:format("~tp", [Report]);
        Report when is_map(Report) ->
            cloudi_string:format("~tp", [Report]);
        FormatValue when is_list(FormatValue) ->
            FormatValue
    end,
    ok = cloudi_core_i_logger:metadata_set(MetaN),
    log_output(log_level(Level), FileName, Line,
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

log_output(undefined, _FileName, _Line,
           FunctionName, FunctionArity, Format, Args) ->
    cloudi_core_i_logger_interface:error("HUT(invalid_level)", 0,
                                         FunctionName,
                                         FunctionArity,
                                         Format, Args);
log_output(LogLevel, FileName, Line,
           FunctionName, FunctionArity, Format, Args) ->
    cloudi_core_i_logger_interface:LogLevel(FileName, Line,
                                            FunctionName,
                                            FunctionArity,
                                            Format, Args).

