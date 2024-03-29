%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Application==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2009-2023 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2009-2023 Michael Truog
%%% @version 2.0.7 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_core_i_app).
-author('mjtruog at protonmail dot com').

-behaviour(application).

%% external interface
-export([test/0]).

%% application callbacks
-export([start/2,
         start_phase/3,
         stop/1]).

-include("cloudi_core_i_configuration.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Is cloudi_core being ran during a test with eunit or ct?===
%% @end
%%-------------------------------------------------------------------------

-spec test() ->
    boolean().

-ifdef(TEST).
test() ->
    true.
-else.
test() ->
    (init:get_argument(test) /= error).
-endif.

%%%------------------------------------------------------------------------
%%% Callback functions from application
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Start the CloudI application.===
%% @end
%%-------------------------------------------------------------------------

-spec start(StartType :: normal | {takeover, node()} | {failover, node()},
            StartArgs :: any()) ->
    {ok, Pid :: pid()} |
    {error, Reason :: any()}.

start(_, _) ->
    ok = application:set_env(cloudi_core, mac_address,
                             cloudi_x_uuid:mac_address()),
    ok = cloudi_x_quickrand:seed([cloudi_x_quickrand]),
    PathOrData = case application:get_env(cloudi_core, configuration) of
        {ok, C} ->
            C;
        undefined ->
            % default configuration
            % (it is better to not use this, to have everything fail-fast)
            []
    end,
    case cloudi_core_i_configuration:load(PathOrData) of
        {ok, Config} ->
            cloudi_core_i_sup:start_link(Config);
        {error, _} = Error ->
            Error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Start phase of the CloudI application.===
%% The init phase is after cloudi_core has started.
%% @end
%%-------------------------------------------------------------------------

-spec start_phase(Phase :: atom(),
                  StartType :: normal | {takeover, node()} | {failover, node()},
                  PhaseArgs :: any()) ->
    ok |
    {error, Reason :: any()}.

start_phase(init, _, _) ->
    cloudi_core_i_configurator:cloudi_core_started();
start_phase(Phase, _, PhaseArgs) ->
    {error, {application_start_phase_invalid, Phase, PhaseArgs}}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Stop the CloudI application.===
%% @end
%%-------------------------------------------------------------------------

-spec stop(State :: any()) ->
    'ok'.

stop(_) ->
    ok.

