%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Enforce immediate garbage collection on a function==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2009-2011, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2009-2011 Michael Truog
%%% @version 0.1.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(immediate_gc).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([sync_fun/2, sync_fun/3,
         async_fun/2, async_fun/3]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Make a synchronous call to a function that will be garbage collected when the function returns.===
%% @end
%%-------------------------------------------------------------------------

-spec sync_fun(M :: atom(), F :: atom(), A :: list()) -> any().

sync_fun(M, F, A) when is_atom(M), is_atom(F), is_list(A) ->
    Parent = self(),
    Child = erlang:spawn_opt(fun() ->
        Parent ! {self(), erlang:apply(M, F, A)},
        erlang:garbage_collect()
    end, [link, {fullsweep_after, 0}]),
    receive
        {Child, Result} -> Result
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Make a synchronous call to an anonymous function that will be garbage collected when the function returns.===
%% @end
%%-------------------------------------------------------------------------

-spec sync_fun(F :: fun(), A :: list()) -> any().

sync_fun(F, A) when is_function(F), is_list(A) ->
    Parent = self(),
    Child = erlang:spawn_opt(fun() ->
        Parent ! {self(), erlang:apply(F, A)},
        erlang:garbage_collect()
    end, [link, {fullsweep_after, 0}]),
    receive
        {Child, Result} -> Result
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Make an asynchronous call to a function that will be garbage collected when the function returns.===
%% @end
%%-------------------------------------------------------------------------

-spec async_fun(M :: atom(), F :: atom(), A :: list()) -> pid().

async_fun(M, F, A) when is_atom(M), is_atom(F), is_list(A) ->
    erlang:spawn_opt(fun() ->
        erlang:apply(M, F, A),
        erlang:garbage_collect()
    end, [link, {fullsweep_after, 0}]).

%%-------------------------------------------------------------------------
%% @doc
%% ===Make an asynchronous call to an anonymous function that will be garbage collected when the function returns.===
%% @end
%%-------------------------------------------------------------------------

-spec async_fun(F :: fun(), A :: list()) -> pid().

async_fun(F, A) when is_function(F), is_list(A) ->
    erlang:spawn_opt(fun() ->
        erlang:apply(F, A),
        erlang:garbage_collect()
    end, [link, {fullsweep_after, 0}]).

