%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==String manipulation functions==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2009-2014, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2009-2014 Michael Truog
%%% @version 1.3.3 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_string).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([afterl/2,
         afterl/3,
         afterr/2,
         afterr/3,
         beforel/2,
         beforel/3,
         beforer/2,
         beforer/3,
         splitl/2,
         splitl/3,
         splitr/2,
         splitr/3,
         binary_to_term/1,
         list_to_term/1,
         term_to_binary/1,
         term_to_list/1,
         format/2,
         format_to_list/2,
         format_to_binary/2,
         compare_constant/2,
         compare_constant_list/2,
         compare_constant_binary/2]).

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
%% ===Return the string that occurs after a character, otherwise return based on the failure atom, when traversing left to right.===
%% @end
%%-------------------------------------------------------------------------

-spec afterl(Char :: pos_integer(),
             Input :: string(), 'empty' | 'input') -> string().

afterl(Char, Input, empty) when is_integer(Char), is_list(Input) ->
    afterl(Char, Input);
afterl(Char, Input, input) when is_integer(Char), is_list(Input) ->
    afterl_input(Char, Input, Input).
afterl_input(_, [], Input) ->
    Input;
afterl_input(Char, [Char | Rest], _) ->
    Rest;
afterl_input(Char, [_ | Rest], Input) ->
    afterl_input(Char, Rest, Input).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the string that occurs after a character, otherwise return an empty string, when traversing right to left.===
%% @end
%%-------------------------------------------------------------------------

-spec afterr(Char :: pos_integer(), string()) -> string().

afterr(Char, Input) when is_integer(Char), is_list(Input) ->
    afterr_empty([], Char, Input).
afterr_empty(L, _, []) ->
    L;
afterr_empty(_, Char, [Char | Rest]) ->
    afterr_empty(Rest, Char, Rest);
afterr_empty(L, Char, [_ | Rest]) ->
    afterr_empty(L, Char, Rest).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the string that occurs after a character, otherwise return based on the failure atom, when traversing right to left.===
%% @end
%%-------------------------------------------------------------------------

-spec afterr(Char :: pos_integer(), string(), 'empty' | 'input') -> string().

afterr(Char, Input, empty) when is_integer(Char), is_list(Input) ->
    afterr_empty([], Char, Input);
afterr(Char, Input, input) when is_integer(Char), is_list(Input) ->
    afterr_input([], Char, Input, Input).
afterr_input([], _, [], Input) ->
    Input;
afterr_input(L, _, [], _) ->
    L;
afterr_input(_, Char, [Char | Rest], Input) ->
    afterr_input(Rest, Char, Rest, Input);
afterr_input(L, Char, [_ | Rest], Input) ->
    afterr_input(L, Char, Rest, Input).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the string that occurs before a character, otherwise return an empty string, when traversing left to right.===
%% @end
%%-------------------------------------------------------------------------

-spec beforel(Char :: pos_integer(), string()) -> string().

beforel(Char, Input) when is_integer(Char), is_list(Input) ->
    beforel_empty([], Char, Input).
beforel_empty(_, _, []) ->
    [];
beforel_empty(Before, Char, [Char | _]) ->
    lists:reverse(Before);
beforel_empty(Before, Char, [H | Input]) ->
    beforel_empty([H | Before], Char, Input).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the string that occurs before a character, otherwise return based on the failure atom, when traversing left to right.===
%% @end
%%-------------------------------------------------------------------------

-spec beforel(Char :: pos_integer(), string(), 'empty' | 'input') -> string().

beforel(Char, Input, empty) when is_integer(Char), is_list(Input) ->
    beforel_empty([], Char, Input);
beforel(Char, Input, input) when is_integer(Char), is_list(Input) ->
    beforel_input([], Char, Input, Input).
beforel_input(_, _, [], Input) ->
    Input;
beforel_input(Before, Char, [Char | _], _) ->
    lists:reverse(Before);
beforel_input(Before, Char, [H | Rest], Input) ->
    beforel_input([H | Before], Char, Rest, Input).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the string that occurs before a character, otherwise return an empty string, when traversing right to left.===
%% @end
%%-------------------------------------------------------------------------

-spec beforer(Char :: pos_integer(), string()) -> string().

beforer(Char, Input) when is_integer(Char), is_list(Input) ->
    beforer_empty([], [], Char, Input).
beforer_empty(Before, _, _, []) ->
    Before;
beforer_empty(Before, L, Char, [Char | Rest]) ->
    beforer_empty(Before ++ lists:reverse(L), [Char], Char, Rest);
beforer_empty(Before, L, Char, [H | Rest]) ->
    beforer_empty(Before, [H | L], Char, Rest).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the string that occurs before a character, otherwise return based on the failure atom, when traversing right to left.===
%% @end
%%-------------------------------------------------------------------------

-spec beforer(Char :: pos_integer(), string(), 'empty' | 'input') -> string().

beforer(Char, Input, empty) when is_integer(Char), is_list(Input) ->
    beforer_empty([], [], Char, Input);
beforer(Char, Input, input) when is_integer(Char), is_list(Input) ->
    beforer_input([], [], Char, Input, Input).
beforer_input([], _, _, [], Input) ->
    Input;
beforer_input(Before, _, _, [], _) ->
    Before;
beforer_input(Before, L, Char, [Char | Rest], Input) ->
    beforer_input(Before ++ lists:reverse(L), [Char], Char, Rest, Input);
beforer_input(Before, L, Char, [H | Rest], Input) ->
    beforer_input(Before, [H | L], Char, Rest, Input).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the two strings split at the first occurrence of the character, otherwise return an empty string, when traversing left to right.===
%% @end
%%-------------------------------------------------------------------------

-spec splitl(Char :: pos_integer(), string()) -> {string(), string()}.

splitl(Char, Input) when is_integer(Char), is_list(Input) ->
    splitl_empty([], Char, Input).
splitl_empty(_, _, []) ->
    {[], []};
splitl_empty(Before, Char, [Char | Rest]) ->
    {lists:reverse(Before), Rest};
splitl_empty(Before, Char, [H | Rest]) ->
    splitl_empty([H | Before], Char, Rest).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the two strings split at the first occurrence of the character, otherwise return based on the failure atom, when traversing left to right.===
%% @end
%%-------------------------------------------------------------------------

-spec splitl(Char :: pos_integer(),
             string(), 'empty' | 'input') -> {string(), string()}.

splitl(Char, Input, empty) when is_integer(Char), is_list(Input) ->
    splitl_empty([], Char, Input);
splitl(Char, Input, input) when is_integer(Char), is_list(Input) ->
    splitl_input([], Char, Input, Input).
splitl_input(_, _, [], Input) ->
    {Input, []};
splitl_input(Before, Char, [Char | Rest], _) ->
    {lists:reverse(Before), Rest};
splitl_input(Before, Char, [H | Rest], Input) ->
    splitl_input([H | Before], Char, Rest, Input).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the two strings split at the first occurrence of the character, otherwise return an empty string, when traversing right to left.===
%% @end
%%-------------------------------------------------------------------------

-spec splitr(Char :: pos_integer(), string()) -> {string(), string()}.

splitr(Char, Input) when is_integer(Char), is_list(Input) ->
    splitr_empty([], [], Char, Input).
splitr_empty([Char | L1], [], Char, []) ->
    {lists:reverse(L1), []};
splitr_empty(_, [], _, []) ->
    {[], []};
splitr_empty(L1, L2, Char, []) ->
    [Char | NewL1] = lists:foldl(fun(_, [_ | L]) -> L end, L1, L2),
    {lists:reverse(NewL1), L2};
splitr_empty(L1, _, Char, [Char | Rest]) ->
    splitr_empty([Char | L1], Rest, Char, Rest);
splitr_empty(L1, L2, Char, [C | Rest]) ->
    splitr_empty([C | L1], L2, Char, Rest).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the two strings split at the first occurrence of the character, otherwise return based on the failure atom, when traversing right to left.===
%% @end
%%-------------------------------------------------------------------------

-spec splitr(Char :: pos_integer(), string(),
             'empty' | 'input') -> {string(), string()}.

splitr(Char, Input, empty) when is_integer(Char), is_list(Input) ->
    splitr_empty([], [], Char, Input);
splitr(Char, Input, input) when is_integer(Char), is_list(Input) ->
    splitr_input([], [], Char, Input, Input).
splitr_input([Char | L1], [], Char, [], _) ->
    {lists:reverse(L1), []};
splitr_input(_, [], _, [], Input) ->
    {[], Input};
splitr_input(L1, L2, Char, [], _) ->
    [Char | NewL1] = lists:foldl(fun(_, [_ | L]) -> L end, L1, L2),
    {lists:reverse(NewL1), L2};
splitr_input(L1, _, Char, [Char | Rest], Input) ->
    splitr_input([Char | L1], Rest, Char, Rest, Input);
splitr_input(L1, L2, Char, [C | Rest], Input) ->
    splitr_input([C | L1], L2, Char, Rest, Input).

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
%% ===Convert an Erlang term to a binary string.===
%% @end
%%-------------------------------------------------------------------------

term_to_binary(T) ->
    erlang:iolist_to_binary(io_lib:format("~p", [T])).

%%-------------------------------------------------------------------------
%% @doc
%% ===Convert an Erlang term to a string.===
%% @end
%%-------------------------------------------------------------------------

-spec term_to_list(T :: any()) -> string().

term_to_list(T) ->
    format("~p", [T]).

%%-------------------------------------------------------------------------
%% @doc
%% ===Format a string based on the arguments.===
%% @end
%%-------------------------------------------------------------------------

-spec format(L :: string(), A :: list()) -> string().

format(L, A) when is_list(L), is_list(A) ->
    format_to_list(L, A).

%%-------------------------------------------------------------------------
%% @doc
%% ===Format a string based on the arguments, stored as a list.===
%% @end
%%-------------------------------------------------------------------------

-spec format_to_list(L :: string(), A :: list()) -> string().
-compile({inline, [{format_to_list, 2}]}).

format_to_list(L, A) when is_list(L), is_list(A) ->
    lists:flatten(io_lib:format(L, A)).

%%-------------------------------------------------------------------------
%% @doc
%% ===Format a string based on the arguments, stored as a binary.===
%% @end
%%-------------------------------------------------------------------------

-spec format_to_binary(L :: string(), A :: list()) -> binary().

format_to_binary(L, A) when is_list(L), is_list(A) ->
    erlang:iolist_to_binary(io_lib:format(L, A)).

%%-------------------------------------------------------------------------
%% @doc
%% ===Time insensitive compare to avoid a timing leak.===
%% Use for password or other authentication comparisons.
%% Execution time is based on the length of Test.
%% @end
%%-------------------------------------------------------------------------

-spec compare_constant(Test :: string(),
                       Correct :: nonempty_string()) ->
    boolean().

compare_constant(Test, [_ | _] = Correct) when is_list(Test) ->
    compare_constant(Test, Correct, 0) =:= 0.

compare_constant([], [], Bits) ->
    Bits;
compare_constant([], [_ | _], _) ->
    1;
compare_constant([C | Test], [] = Correct, Bits) ->
    compare_constant(Test, Correct, Bits bor (C bxor -1));
compare_constant([C1 | Test], [C2 | Correct], Bits) ->
    compare_constant(Test, Correct, Bits bor (C1 bxor C2)).

%%-------------------------------------------------------------------------
%% @doc
%% ===Time insensitive compare to avoid a timing leak with strings as lists.===
%% Use for password or other authentication comparisons.
%% Execution time is based on the length of Test.
%% @end
%%-------------------------------------------------------------------------

-spec compare_constant_list(Test :: string(),
                            Correct :: nonempty_string()) ->
    boolean().

compare_constant_list(Test, Correct) ->
    compare_constant(Test, Correct).

%%-------------------------------------------------------------------------
%% @doc
%% ===Time insensitive compare to avoid a timing leak with strings as binaries.===
%% Use for password or other authentication comparisons.
%% Execution time is based on the length of Test.
%% @end
%%-------------------------------------------------------------------------

-spec compare_constant_binary(Test :: binary(),
                              Correct :: binary()) ->
    boolean().

compare_constant_binary(Test, Correct)
    when is_binary(Test), is_binary(Correct) ->
    compare_constant(erlang:binary_to_list(Test),
                     erlang:binary_to_list(Correct)).


