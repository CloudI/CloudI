%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==List Queue.==
%%% A queue implemented as a single list that is reversed a minimal
%%% number of times.  Usage patterns that rarely alternate between the
%%% getting/putting should experience more efficiency.  A subset of the
%%% queue module interface functions are implemented.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2011, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011 Michael Truog
%%% @version 0.1.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(lqueue).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([dropwhile/2,
         filter/2,
         from_list/1,
         in/2,
         in_r/2,
         is_empty/1,
         isnt_empty/1,
         len/1,
         new/0,
         out/1,
         out_r/1,
         reverse/1,
         to_list/1]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

dropwhile(F, {forward, L}) ->
    {forward, lists:dropwhile(F, L)};
dropwhile(F, {backward, [_] = L}) ->
    {backward, lists:dropwhile(F, L)};
dropwhile(F, {backward, L}) ->
    {forward, lists:dropwhile(F, lists:reverse(L))}.

filter(F, {Direction, L}) ->
    {Direction, lists:filter(F, L)}.

from_list(L) ->
    {forward, L}.

in(Item, {forward, [OldItem]}) ->
    {forward, [OldItem, Item]};
in(Item, {forward, L}) ->
    {backward, [Item | lists:reverse(L)]};
in(Item, {backward, L}) ->
    {backward, [Item | L]}.

in_r(Item, {backward, [OldItem]}) ->
    {backward, [OldItem, Item]};
in_r(Item, {backward, L}) ->
    {forward, [Item | lists:reverse(L)]};
in_r(Item, {forward, L}) ->
    {forward, [Item | L]}.

is_empty({_, []}) ->
    true;
is_empty({_, _}) ->
    false.

isnt_empty({_, []}) ->
    false;
isnt_empty({_, _}) ->
    true.

len({_, []}) ->
    0;
len({_, L}) ->
    erlang:length(L).

new() ->
    {forward, []}.

out({_, []} = State) ->
    {empty, State};
out({forward, [Item | L]}) ->
    {{value, Item}, {forward, L}};
out({backward, [Item]}) ->
    {{value, Item}, {backward, []}};
out({backward, L}) ->
    [Item | NewL] = lists:reverse(L),
    {{value, Item}, {forward, NewL}}.

out_r({_, []} = State) ->
    {empty, State};
out_r({backward, [Item | L]}) ->
    {{value, Item}, {backward, L}};
out_r({forward, [Item]}) ->
    {{value, Item}, {forward, []}};
out_r({forward, L}) ->
    [Item | NewL] = lists:reverse(L),
    {{value, Item}, {backward, NewL}}.

reverse({Direction, L}) ->
    {Direction, lists:reverse(L)}.

to_list({_, []}) ->
    [];
to_list({forward, L}) ->
    L;
to_list({backward, L}) ->
    lists:reverse(L).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

