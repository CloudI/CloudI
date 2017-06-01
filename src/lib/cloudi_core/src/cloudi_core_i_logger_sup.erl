%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Logger Supervisor of Formatters==
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

-module(cloudi_core_i_logger_sup).
-author('mjtruog [at] gmail (dot) com').

-behaviour(supervisor).

%% external interface
-export([start_link/1,
         reconfigure/1]).

%% supervisor callbacks
-export([init/1]).

-include("cloudi_core_i_configuration.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% @end
%%-------------------------------------------------------------------------

-spec start_link(Config :: #config{}) ->
    {'ok', pid()} |
    {'error', any()}.

start_link(#config{logging = LoggingConfig}) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [LoggingConfig]).

%%-------------------------------------------------------------------------
%% @doc
%% @end
%%-------------------------------------------------------------------------

-spec reconfigure(FormattersConfig ::
                      #config_logging_formatters{} | undefined) ->
    'ok'.

reconfigure(FormattersConfig)
    when FormattersConfig =:= undefined;
         is_record(FormattersConfig, config_logging_formatters) ->
    Children = children_filter_dead(supervisor:which_children(?MODULE)),
    NewChildSpecs = child_specification(formatters_output(FormattersConfig)),
    {Start, Stop} = children_filter_alive(Children, NewChildSpecs),
    ok = children_start(Start),
    ok = children_stop(Stop),
    ok.

%%%------------------------------------------------------------------------
%%% Callback functions from supervisor
%%%------------------------------------------------------------------------

init([#config_logging{formatters = FormattersConfig}]) ->
    MaxRestarts = 0,
    MaxTime = 1,
    {ok, {{one_for_one, MaxRestarts, MaxTime}, 
          child_specification(formatters_output(FormattersConfig))}}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

formatters_output_list(#config_logging_formatters{default = Default,
                                                  lookup = Lookup}) ->
    L0 = cloudi_x_keys1value:to_list(Lookup),
    LN = if
        Default =:= undefined ->
            L0;
        true ->
            [{any, Default} | L0]
    end,
    lists:filter(fun({_, #config_logging_formatter{output = Output}}) ->
        Output =/= undefined
    end, LN).

formatters_output(undefined) ->
    [];
formatters_output(FormattersConfig) ->
    formatters_output_list(FormattersConfig).

child_specification(FormatterOutput) ->
    child_specification(FormatterOutput, []).

child_specification([], ChildSpec) ->
    ChildSpec;
child_specification([{_, #config_logging_formatter{
                             output_name = OutputName} = FormatterConfig} | L],
                    ChildSpec) ->
    child_specification(L,
                        [{OutputName,
                          {cloudi_core_i_logger_output_sup, start_link,
                           [FormatterConfig]},
                          temporary, infinity, supervisor,
                          [cloudi_core_i_logger_output_sup]} | ChildSpec]).

children_filter_dead(L) ->
    children_filter_dead(L, []).

children_filter_dead([], Result) ->
    lists:reverse(Result);
children_filter_dead([{OutputName, undefined, _, _} | L], Result) ->
    % temporary processes, so this should never happen
    supervisor:delete_child(?MODULE, OutputName),
    children_filter_dead(L, Result);
children_filter_dead([{_, _, _, _} = Entry | L], Result) ->
    children_filter_dead(L, [Entry | Result]).

children_filter_alive(L, NewChildSpecs) ->
    children_filter_alive(L, NewChildSpecs, []).

children_filter_alive([], ChildrenToStart, ChildrenToStop) ->
    {ChildrenToStart, ChildrenToStop};
children_filter_alive([{OutputName, _, _, _} = Entry | L],
                      ChildrenToStart0, ChildrenToStop) ->
    case lists:keytake(OutputName, 1, ChildrenToStart0) of
        {value, ChildSpec, ChildrenToStartN}
            when is_tuple(ChildSpec) ->
            children_filter_alive(L, ChildrenToStartN,
                                  ChildrenToStop);
        false ->
            children_filter_alive(L, ChildrenToStart0,
                                  [Entry | ChildrenToStop])
    end.

children_start([]) ->
    ok;
children_start([ChildSpec | ChildSpecs]) ->
    case supervisor:start_child(?MODULE, ChildSpec) of
        {ok, _} ->
            ok;
        {ok, _, _} ->
            ok;
        {error, _} ->
            % errors will be logged when the output module is used
            ok
    end,
    children_start(ChildSpecs).

children_stop([]) ->
    ok;
children_stop([{OutputName, _, _, _} | L]) ->
    ok = supervisor:terminate_child(?MODULE, OutputName),
    children_stop(L).
