%% -*- mode: erlang; indent-tabs-mode: nil; -*-
%%=============================================================================
%% Copyright 2014-2016 Ulf Wiger
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%=============================================================================
-module(setup_app).
-behaviour(application).

-export([start/2,
         start_phase/3,
         stop/1]).

start(_Type, _Args) ->
    setup_sup:start_link().

start_phase(run_setup, _Type, []) ->
    _ = setup_srv:run_setup(),
    ok.

stop(_) ->
    ok.
