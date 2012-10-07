%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Tuple operations==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2009-2012, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2009-2012 Michael Truog
%%% @version 0.2.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_tuple).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([move/4,
         match/3]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Move a tuple into a larger tuple.===
%% Make a new tuple with arity N and default D, then
%% move tuple T into the new tuple at index I.
%% @end
%%-------------------------------------------------------------------------

-spec move(I :: pos_integer(),
           N :: pos_integer(),
           T :: tuple(),
           D :: any()) -> tuple().

move(I, N, T, D)
    when is_integer(I), is_integer(N), is_tuple(T),
         (N - I + 1) >= tuple_size(T) ->
    move_i(I, 1, I + tuple_size(T), erlang:make_tuple(N, D), T).

move_i(N1, _, N1, T1, _) ->
    T1;

move_i(I1, I0, N1, T1, T0) ->
    move_i(I1 + 1, I0 + 1, N1,
           erlang:setelement(I1, T1, erlang:element(I0, T0)), T0).


%%-------------------------------------------------------------------------
%% @doc
%% ===Match tuples based on a bitmask.===
%% @end
%%-------------------------------------------------------------------------

-spec match({any()} | {any(), any()} | {any(), any(), any()},
            {any()} | {any(), any()} | {any(), any(), any()},
            pos_integer()) -> boolean().

match({E0}, {E0}, 2#1) ->
    true;
match({ _, E1}, { _, E1}, 2#01) ->
    true;
match({ _,  _, E2}, { _,  _, E2}, 2#001) ->
    true;
match({E0,  _}, {E0,  _}, 2#10) ->
    true;
match({ _, E1,  _}, { _, E1,  _}, 2#010) ->
    true;
match({E0, E1}, {E0, E1}, 2#11) ->
    true;
match({ _, E1, E2}, { _, E1, E2}, 2#011) ->
    true;
match({E0,  _,  _}, {E0,  _,  _}, 2#100) ->
    true;
match({E0,  _, E2}, {E0,  _, E2}, 2#101) ->
    true;
match({E0, E1,  _}, {E0, E1,  _}, 2#110) ->
    true;
match({E0, E1, E2}, {E0, E1, E2}, 2#111) ->
    true;
match({ _,  _,  _}, { _,  _,  _},     _) ->
    false;
match({ _,  _}, { _,  _},    _) ->
    false;
match({ _}, { _},   _) ->
    false.

