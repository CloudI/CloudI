%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==OS Process Spawn==
%%% Used interaction with the cloudi_os_spawn process.
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2011-2020 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2011-2020 Michael Truog
%%% @version 2.0.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_core_i_os_spawn).
-author('mjtruog at protonmail dot com').

-behavior(gen_server).

%% gen_server interface
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, 
         handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-include("cloudi_core_i_constants.hrl").
-ifdef(CLOUDI_CORE_STANDALONE).
-export([spawn/16]).
-define(ERL_PORT_NAME, undefined).
-define(ERL_PORT_MODULE, cloudi_core_i_os_port).
spawn(_SpawnProcess, _SpawnProtocol, _SpawnSocketPath, _Ports, _SpawnRlimits,
      _SpawnUserI, _SpawnUserStr, _SpawnGroupI, _SpawnGroupStr,
      _SpawnNice, _SpawnChroot, _SpawnSyscallLock, _SpawnDirectory,
      _SpawnFilename, _SpawnArguments, _SpawnEnvironment) ->
    {error, badarg}.
-else.
-include("cloudi_core_i_os_spawn.hrl").
-endif.

%%%------------------------------------------------------------------------
%%% Interface functions from gen_server
%%%------------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([]) ->
    ?ERL_PORT_MODULE:init(?ERL_PORT_NAME, false,
                          fun cloudi_core_i_services_external:stdout/2,
                          fun cloudi_core_i_services_external:stderr/2).

handle_call(Request, Client, State) ->
    ?ERL_PORT_MODULE:handle_call(Request, Client, State).

handle_cast(Request, State) ->
    ?ERL_PORT_MODULE:handle_cast(Request, State).

handle_info(Request, State) ->
    ?ERL_PORT_MODULE:handle_info(Request, State).

terminate(Reason, State) ->
    ?ERL_PORT_MODULE:terminate(Reason, State).

code_change(OldVsn, State, Extra) ->
    ?ERL_PORT_MODULE:code_change(OldVsn, State, Extra).

