%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Logger Formatter Output Initialization==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2014-2017 Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2014-2017 Michael Truog
%%% @version 1.7.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_core_i_logger_output).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).

%% external interface
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("cloudi_logger.hrl").
-include("cloudi_core_i_configuration.hrl").

-record(state,
    {
        config :: #config_logging_formatter{}
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% @end
%%-------------------------------------------------------------------------

-spec start_link(FormatterConfig :: #config_logging_formatter{}) ->
    {'ok', pid()} |
    {'error', any()}.

start_link(FormatterConfig)
    when is_record(FormatterConfig, config_logging_formatter) ->
    gen_server:start_link(?MODULE, [FormatterConfig], []).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([#config_logging_formatter{
          output = Output,
          output_name = OutputName,
          output_args = OutputArgs,
          formatter = Formatter,
          formatter_config = Config} = FormatterConfig]) ->
    case gen_event:start_link({local, OutputName}) of
        {ok, _} ->
            ok;
        {error, {already_started, _}} ->
            ok
    end,
    Args = if
        Formatter =:= undefined ->
            OutputArgs;
        true ->
            [{formatter, Formatter},
             {formatter_config, Config} | OutputArgs]
    end,
    case gen_event:add_sup_handler(OutputName, Output, Args) of
        ok ->
            erlang:process_flag(trap_exit, true),
            {ok, #state{config = FormatterConfig}};
        {'EXIT', Reason} ->
            {stop, {error, Reason}};
        {error, _} = Error ->
            {stop, Error};
        Reason ->
            {stop, {error, Reason}}
    end.

handle_call(Request, _, State) ->
    ?LOG_WARN("Unknown call \"~p\"", [Request]),
    {stop, cloudi_string:format("Unknown call \"~p\"", [Request]),
     error, State}.

handle_cast(Request, State) ->
    ?LOG_WARN("Unknown cast \"~p\"", [Request]),
    {noreply, State}.

handle_info({'gen_event_EXIT', _, Reason}, State) ->
    {stop, Reason, State};

handle_info({'EXIT', _, Reason}, State) ->
    {stop, Reason, State};

handle_info(Request, State) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

terminate(Reason,
          #state{config = #config_logging_formatter{
                              output = Output,
                              output_name = OutputName}}) ->
    _ = gen_event:delete_handler(OutputName, Output, Reason),
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

