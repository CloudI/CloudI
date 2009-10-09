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
%%% Copyright (c) 2009, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2009 Michael Truog
%%% @version 0.0.5 {@date} {@time}
%%%------------------------------------------------------------------------

-module(string_extensions).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([after_character/2, before_character/2,
         split_on_character/2,
         list_to_term/1, term_to_list/1,
         format/2]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the string that occurs after a character, otherwise return an empty string.===
%% @end
%%-------------------------------------------------------------------------

-spec after_character(Char :: pos_integer(), string()) -> string().

after_character(_, []) ->
    [];
after_character(Char, [Char | Rest]) when is_integer(Char) ->
    Rest;
after_character(Char, [_ | Rest]) when is_integer(Char) ->
    after_character(Char, Rest).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the string that occurs before a character, otherwise return an empty string.===
%% @end
%%-------------------------------------------------------------------------

-spec before_character(Char :: pos_integer(), string()) -> string().

before_character(Char, Input) when is_integer(Char), is_list(Input) ->
    before_character([], Char, Input).
before_character(_, _, []) ->
    [];
before_character(Before, Char, [Char | _]) when is_integer(Char) ->
    Before;
before_character(Before, Char, [H | Input]) when is_integer(Char) ->
    before_character(Before ++ [H], Char, Input).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the two strings split at the first occurrence of the character.===
%% @end
%%-------------------------------------------------------------------------

-spec split_on_character(Char :: pos_integer(), string()) ->
    {string(), string()}.

split_on_character(Char, Input) when is_integer(Char), is_list(Input) ->
    split_on_character([], Char, Input).
split_on_character(_, _, []) ->
    {[], []};
split_on_character(Before, Char, [Char | Input]) when is_integer(Char) ->
    {Before, Input};
split_on_character(Before, Char, [H | Input]) when is_integer(Char) ->
    split_on_character(Before ++ [H], Char, Input).

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

