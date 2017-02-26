%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Logger Lager Integration==
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

-module(cloudi_logger_lager).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([log/3,
         log/4,
         md/0,
         md/1,
         parse_transform/2]).

-record(state,
        {
            module = undefined :: module() | undefined,
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
    log(Level, undefined, Format, Args);
log(Level, _, Format, Args) ->
    case log_level(Level) of
        undefined ->
            cloudi_core_i_logger_interface:error('LAGER(invalid_level)', 0,
                                                 undefined, undefined,
                                                 Format, Args);
        LogLevel ->
            cloudi_core_i_logger_interface:LogLevel('LAGER', 0,
                                                    undefined, undefined,
                                                    Format, Args)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get lager metadata.===
%% @end
%%-------------------------------------------------------------------------

-spec md() ->
    list({atom(), any()}).

md() ->
    cloudi_core_i_logger:metadata_get().

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

log_level(Level) ->
    % consistent with
    % cloudi_core_i_logger:lager_severity_output/1,
    % cloudi_core_i_logger:lager_severity_input/1 and
    % cloudi_core_i_configuration:logging_formatter_level/1
    if
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
    end.

forms_process([{eof, _} = EOF], L, _) ->
    lists:reverse([EOF | L]);
forms_process([{attribute, _, module, {Module, _}} = Attribute | Forms],
              L, State) ->
    % still handle parameterized modules
    forms_process(Forms, [Attribute | L], State#state{module = Module});
forms_process([{attribute, _, module, Module} = Attribute | Forms],
              L, State) ->
    forms_process(Forms, [Attribute | L], State#state{module = Module});
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
             #state{module = Module,
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
            NewArgs = [{atom, Anno, Module},
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

