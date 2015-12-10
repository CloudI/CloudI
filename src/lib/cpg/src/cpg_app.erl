%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CPG Application==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2012-2015, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2012-2015 Michael Truog
%%% @version 1.5.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cpg_app).
-author('mjtruog [at] gmail (dot) com').

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

