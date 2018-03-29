%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CPG Application==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2012-2017 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2012-2017 Michael Truog
%%% @version 1.7.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cpg_app).
-author('mjtruog at protonmail dot com').

-behaviour(application).

%% external interface
-export([listen_type/0,
         group_storage/0]).

%% application callbacks
-export([start/2,
         stop/1]).

-include("cpg_constants.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Determine how CPG will monitor node connections.===
%% @end
%%-------------------------------------------------------------------------

listen_type() ->
    case application:get_env(cpg, node_type) of
        {ok, V} when (V =:= visible) orelse (V =:= all) ->
            V;
        undefined ->
            visible
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Provide the CPG group_storage configuration.===
%% Must be a module that provides a dict interface
%% (an interface based on the dict module).
%% @end
%%-------------------------------------------------------------------------

group_storage() ->
    case application:get_env(cpg, group_storage) of
        {ok, V} when is_atom(V) ->
            V;
        undefined ->
            trie
    end.

%%%------------------------------------------------------------------------
%%% Callback functions from application
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Start the CPG application.===
%% @end
%%-------------------------------------------------------------------------

start(_, _) ->
    {ok, ScopeList} = application:get_env(scope),
    case cpg_sup:start_link(ScopeList) of
        {ok, _} = Success ->
            Success;
        {error, _} = Error ->
            Error
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Stop the CPG application.===
%% @end
%%-------------------------------------------------------------------------

stop(_) ->
    ok.

