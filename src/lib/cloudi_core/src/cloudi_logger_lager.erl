%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Logger Lager Integration==
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

-module(cloudi_logger_lager).
-author('mjtruog at protonmail dot com').

%% external interface
-export([log/3,
         log/4,
         md/0,
         md/1,
         parse_transform/2]).

-record(state,
        {
            file_name = undefined :: nonempty_string() | undefined,
            function_name = undefined :: atom() | undefined,
            function_arity = undefined :: arity() | undefined
        }).

% erl_parse tree nodes represented as records
-record('clause',
        {
            anno :: erl_anno:anno(),
            pattern :: list(any()),
            guard :: list(list(any())),
            body :: list(erl_parse:abstract_expr())
        }).
-record('function',
        {
            anno :: erl_anno:anno(),
            function_name :: atom(),
            function_arity :: arity(),
            clauses :: list(#'clause'{})
        }).
-record('remote',
        {
            anno :: erl_anno:anno(),
            module :: erl_parse:abstract_expr(),
            function_name :: erl_parse:abstract_expr()
        }).
-record('call',
        {
            anno :: erl_anno:anno(),
            function :: erl_parse:abstract_expr() | #'remote'{},
            args :: list(erl_parse:abstract_expr())
        }).

%%-------------------------------------------------------------------------
%% @doc
%% ===Log without the parse transform and arguments.===
%% @end
%%-------------------------------------------------------------------------

-spec log(Level :: critical | alert | emergency |
                   error | warning | notice | info | debug,
          Unknown :: any(),
          Format :: string()) ->
    ok.

log(Level, Unknown, Format) ->
    log(Level, Unknown, Format, []).

%%-------------------------------------------------------------------------
%% @doc
%% ===Log without the parse transform.===
%% @end
%%-------------------------------------------------------------------------

-spec log(Level :: critical | alert | emergency |
                   error | warning | notice | info | debug,
          any(),
          Format :: string(),
          Args :: list()) ->
    ok.

log(Level, MetaData, Format, Args) when is_list(MetaData) ->
    ok = cloudi_core_i_logger:metadata_set(MetaData),
    log_output(log_level(Level), Format, Args);
log(Level, _, Format, Args) ->
    log_output(log_level(Level), Format, Args).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get lager metadata.===
%% @end
%%-------------------------------------------------------------------------

-spec md() ->
    list({atom(), any()}).

md() ->
    case cloudi_core_i_logger:metadata_get() of
        L when is_list(L) ->
            L;
        _ ->
            []
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Set lager metadata.===
%% @end
%%-------------------------------------------------------------------------

-spec md(list({atom(), any()})) ->
    ok.

md(MetaData) when is_list(MetaData) ->
    cloudi_core_i_logger:metadata_set(MetaData).

%%-------------------------------------------------------------------------
%% @doc
%% ===Lager parse transform replacement for using CloudI logging.===
%% use {parse_transform, cloudi_logger_lager} as a compile option
%% instead of {parse_transform, lager_transform}
%% (e.g., -compile([{parse_transform, cloudi_logger_lager}]).)
%% @end
%%-------------------------------------------------------------------------

-spec parse_transform(Forms :: list(erl_parse:abstract_form() |
                                    erl_parse:form_info()),
                      CompileOptions :: list(compile:option())) ->
    list(erl_parse:abstract_form() | erl_parse:form_info()).

parse_transform(Forms, _CompileOptions) ->
    forms_process(Forms, [], #state{}).

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

log_output(undefined, Format, Args) ->
    cloudi_core_i_logger_interface:error("LAGER(invalid_level)", 0,
                                         undefined, undefined,
                                         Format, Args);
log_output(LogLevel, Format, Args) ->
    cloudi_core_i_logger_interface:LogLevel("LAGER", 0,
                                            undefined, undefined,
                                            Format, Args).

forms_process([{eof, _} = EOF], L, _) ->
    lists:reverse([EOF | L]);
forms_process([{attribute, _, file, {FileName, _}} = Attribute | Forms],
              L, State) ->
    forms_process(Forms, [Attribute | L], State#state{file_name = FileName});
forms_process([#'function'{function_name = FunctionName,
                           function_arity = FunctionArity,
                           clauses = Clauses} = Form | Forms], L, State) ->
    NewClauses = clauses_process(Clauses, [],
                                 State#state{function_name = FunctionName,
                                             function_arity = FunctionArity}),
    forms_process(Forms, [Form#'function'{clauses = NewClauses} | L], State);
forms_process([H | Forms], L, State) ->
    forms_process(Forms, [H | L], State).

clauses_process([], L, _) ->
    lists:reverse(L);
clauses_process([#'clause'{body = Body} = Clause | Clauses], L, State) ->
    NewBody = body_process(Body, [], State),
    clauses_process(Clauses, [Clause#'clause'{body = NewBody} | L], State).

lager_args_process({atom, Anno, none}) ->
    {nil, Anno};
lager_args_process({nil, _} = Args) ->
    Args;
lager_args_process({cons, _, _, _} = Args) ->
    Args.

body_process([], L, _) ->
    lists:reverse(L);
body_process([#'call'{anno = Anno,
                      function = #'remote'{module = {atom, _, lager},
                                           function_name = {atom, _, Level}},
                      args = Args} = Call |
              Statements], L,
             #state{file_name = FileName,
                    function_name = FunctionName,
                    function_arity = FunctionArity} = State) ->
    case log_level(Level) of
        undefined ->
            body_process(Statements, [Call | L], State);
        LogLevel ->
            NewArgsSuffix = case Args of
                [{string, LagerFormatAnno, _} = LagerFormat] ->
                    [LagerFormat, {nil, LagerFormatAnno}];
                [{string, _, _} = LagerFormat, LagerArgs] ->
                    [LagerFormat, lager_args_process(LagerArgs)];
                [_Attrs, {string, LagerFormatAnno, _} = LagerFormat] ->
                    [LagerFormat, {nil, LagerFormatAnno}];
                [_Attrs, {string, _, _} = LagerFormat, LagerArgs] ->
                    [LagerFormat, lager_args_process(LagerArgs)]
            end,
            NewArgs = [{string, Anno, FileName},
                       {integer, Anno, erl_anno:line(Anno)},
                       {atom, Anno, FunctionName},
                       {integer, Anno, FunctionArity} | NewArgsSuffix],
            NewCall = Call#'call'{
                function = #'remote'{
                    anno = Anno,
                    module = {atom, Anno, cloudi_core_i_logger_interface},
                    function_name = {atom, Anno, LogLevel}},
                args = NewArgs},
            body_process(Statements, [NewCall | L], State)
    end;
body_process([Statement | Statements], L, State) ->
    body_process(Statements, [Statement | L], State).

