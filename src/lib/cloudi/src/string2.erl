%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==String extensions==
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

-module(string2).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([afterl/2, beforel/2, splitl/2,
         afterr/2, beforer/2,
         binary_to_term/1,
         list_to_term/1, term_to_list/1,
         format/2]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the string that occurs after a character, otherwise return an empty string, when traversing left to right.===
%% @end
%%-------------------------------------------------------------------------

-spec afterl(Char :: pos_integer(), string()) -> string().

afterl(_, []) ->
    [];
afterl(Char, [Char | Rest]) when is_integer(Char) ->
    Rest;
afterl(Char, [_ | Rest]) when is_integer(Char) ->
    afterl(Char, Rest).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the string that occurs after a character, otherwise return an empty string, when traversing right to left.===
%% @end
%%-------------------------------------------------------------------------

-spec afterr(Char :: pos_integer(), string()) -> string().

afterr(Char, Input) when is_integer(Char), is_list(Input) ->
    afterr([], Char, Input).
afterr(L, _, []) ->
    L;
afterr(_, Char, [Char | Rest]) ->
    afterr(Rest, Char, Rest);
afterr(L, Char, [_ | Rest]) ->
    afterr(L, Char, Rest).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the string that occurs before a character, otherwise return an empty string, when traversing left to right.===
%% @end
%%-------------------------------------------------------------------------

-spec beforel(Char :: pos_integer(), string()) -> string().

beforel(Char, Input) when is_integer(Char), is_list(Input) ->
    beforel([], Char, Input).
beforel(_, _, []) ->
    [];
beforel(Before, Char, [Char | _]) when is_integer(Char) ->
    Before;
beforel(Before, Char, [H | Input]) when is_integer(Char) ->
    beforel(Before ++ [H], Char, Input).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the string that occurs before a character, otherwise return an empty string, when traversing right to left.===
%% @end
%%-------------------------------------------------------------------------

-spec beforer(Char :: pos_integer(), string()) -> string().

beforer(Char, Input) when is_integer(Char), is_list(Input) ->
    beforer([], [], Char, Input).
beforer(Before, _, _, []) ->
    Before;
beforer(Before, L, Char, [Char | Input]) ->
    beforer(Before ++ L, [Char], Char, Input);
beforer(Before, L, Char, [H | Input]) ->
    beforer(Before, L ++ [H], Char, Input).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the two strings split at the first occurrence of the character, when traversing left to right.===
%% @end
%%-------------------------------------------------------------------------

-spec splitl(Char :: pos_integer(), string()) ->
    {string(), string()}.

splitl(Char, Input) when is_integer(Char), is_list(Input) ->
    splitl([], Char, Input).
splitl(_, _, []) ->
    {[], []};
splitl(Before, Char, [Char | Input]) when is_integer(Char) ->
    {Before, Input};
splitl(Before, Char, [H | Input]) when is_integer(Char) ->
    splitl(Before ++ [H], Char, Input).

%%-------------------------------------------------------------------------
%% @doc
%% ===Convert a binary string to an Erlang term.===
%% @end
%%-------------------------------------------------------------------------

-spec binary_to_term(B :: binary()) -> any().

binary_to_term(B) when is_binary(B) ->
    list_to_term(erlang:binary_to_list(B)).

%%-------------------------------------------------------------------------
%% @doc
%% ===Convert a string to an Erlang term.===
%% @end
%%-------------------------------------------------------------------------

-spec list_to_term(L :: string()) -> any().

list_to_term(L) when is_list(L) ->
    {ok, S, _} = erl_scan:string(L ++ "."),
    case erl_parse:parse_term(S) of
        {ok, Term} ->
            Term;
        {error, Reason} ->
            throw(Reason)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Convert an Erlang term to a string.===
%% @end
%%-------------------------------------------------------------------------

-spec term_to_list(T :: any()) -> string().

term_to_list(T) ->
    format("~w", [T]).

%%-------------------------------------------------------------------------
%% @doc
%% ===Format a string based on the arguments.===
%% @end
%%-------------------------------------------------------------------------

-spec format(L :: string(), A :: list()) -> string().

format(L, A) when is_list(L), is_list(A) ->
    lists:flatten(io_lib:format(L, A)).

