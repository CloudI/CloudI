%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Logger Formatter Output Initialization==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2014, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2014 Michael Truog
%%% @version 1.3.3 {@date} {@time}
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

