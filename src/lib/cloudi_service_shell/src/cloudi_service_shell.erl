%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Shell Service==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2019 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2019 Michael Truog
%%% @version 1.8.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_shell).
-author('mjtruog at protonmail dot com').

-behaviour(cloudi_service).

%% external interface
-export([exec/3,
         exec/4]).

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_request/11,
         cloudi_service_terminate/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-define(DEFAULT_EXECUTABLE,             "/bin/sh").
-define(DEFAULT_DIRECTORY,                    "/").

-record(state,
    {
        executable :: nonempty_string(),
        directory :: nonempty_string()
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-type agent() :: cloudi:agent().
-type service_name() :: cloudi:service_name().
-type timeout_milliseconds() :: cloudi:timeout_milliseconds().
-type module_response(Result) ::
    {{ok, Result}, NewAgent :: agent()} |
    {{error, cloudi:error_reason_sync()}, NewAgent :: agent()}.

-spec exec(Agent :: agent(),
           Prefix :: service_name(),
           Command :: nonempty_string() | binary()) ->
    module_response(binary()).

exec(Agent, Prefix, Command) ->
    cloudi:send_sync(Agent, Prefix, Command).

-spec exec(Agent :: agent(),
           Prefix :: service_name(),
           Command :: nonempty_string() | binary(),
           Timeout :: timeout_milliseconds()) ->
    module_response(binary()).

exec(Agent, Prefix, Command, Timeout) ->
    cloudi:send_sync(Agent, Prefix, Command, Timeout).

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, _Prefix, _Timeout, Dispatcher) ->
    Defaults = [
        {executable,                   ?DEFAULT_EXECUTABLE},
        {directory,                    ?DEFAULT_DIRECTORY}],
    [Executable, Directory] = cloudi_proplists:take_values(Defaults, Args),
    [_ | _] = Executable,
    true = filelib:is_regular(Executable),
    [_ | _] = Directory,
    true = filelib:is_dir(Directory),
    cloudi_service:subscribe(Dispatcher, ""),
    {ok, #state{executable = Executable,
                directory = Directory}}.

cloudi_service_handle_request(_RequestType, _Name, _Pattern,
                              _RequestInfo, Request,
                              _Timeout, _Priority, _TransId, _Pid,
                              State, _Dispatcher) ->
    {Status, Output} = request(Request, State),
    log_output(Status, Output, Request),
    {reply, erlang:integer_to_binary(Status), State}.

cloudi_service_terminate(_Reason, _Timeout, _State) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

request(Exec, #state{executable = Executable,
                     directory = Directory}) ->
    Shell = erlang:open_port({spawn_executable, Executable},
                             [{args, ["-"]}, {cd, Directory},
                              stream, binary, stderr_to_stdout, exit_status]),
    true = erlang:port_command(Shell, ["exec ", Exec, "\n"]),
    request_output(Shell, []).

request_output(Shell, Output) ->
    receive
        {Shell, {data, Data}} ->
            request_output(Shell, [Data | Output]);
        {Shell, {exit_status, Status}} ->
            {Status, lists:reverse(Output)}
    end.

log_output(Status, Output, Request) ->
    Level = if
        Status == 0 ->
            trace;
        true ->
            error
    end,
    if
        Output == [] ->
            ?LOG(Level, "~s = ~w",
                 [Request, Status]);
        true ->
            ?LOG(Level, "~s = ~w (stdout/stderr below)~n~s",
                 [Request, Status, erlang:iolist_to_binary(Output)])
    end.
