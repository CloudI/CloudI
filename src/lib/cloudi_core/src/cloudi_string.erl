%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==String manipulation functions==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2009-2018 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2009-2018 Michael Truog
%%% @version 1.7.4 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_string).
-author('mjtruog at protonmail dot com').

%% external interface
-export([afterl/2,
         afterl/3,
         afterr/2,
         afterr/3,
         beforel/2,
         beforel/3,
         beforer/2,
         beforer/3,
         binary_to_term/1,
         compare_constant/2,
         compare_constant_list/2,
         compare_constant_binary/2,
         findl/2,
         findr/2,
         format/2,
         format_to_binary/2,
         format_to_list/2,
         join/2,
         list_to_term/1,
         lowercase/1,
         split/2,
         splitl/2,
         splitl/3,
         splitr/2,
         splitr/3,
         term_to_binary/1,
         term_to_binary_compact/1,
         term_to_list/1,
         term_to_list_compact/1,
         titlecase/1,
         trim/1,
         trim/2,
         triml/1,
         triml/2,
         trimr/1,
         trimr/2,
         uppercase/1]).

% based on unicode_util:whitespace/0
-define(WHITESPACE, [13,9,10,11,12,13,32,133,8206,8207,8232,8233]).
% keep output in a single line when using ~p (printable) formatting
-ifdef(ERLANG_OTP_VERSION_21_FEATURES).
-define(COMPACT_LIMIT, "0").
-else.
-define(COMPACT_LIMIT, "1000000000000").
-endif.

-include("cloudi_core_i_constants.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the string that occurs after a character, otherwise return an empty string, when traversing left to right.===
%% @end
%%-------------------------------------------------------------------------

-spec afterl(Char :: non_neg_integer(),
             Input :: string()) ->
    string().

afterl(_, []) ->
    [];
afterl(Char, [Char | Rest]) ->
    Rest;
afterl(Char, [_ | Rest]) ->
    afterl(Char, Rest).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the string that occurs after a character, otherwise return based on the failure atom, when traversing left to right.===
%% @end
%%-------------------------------------------------------------------------

-spec afterl(Char :: non_neg_integer(),
             Input :: string(),
             empty | input) ->
    string().

afterl(Char, Input, empty) ->
    afterl(Char, Input);
afterl(Char, Input, input) ->
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

-spec afterr(Char :: non_neg_integer(),
             Input :: string()) ->
    string().

afterr(Char, Input) ->
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

-spec afterr(Char :: non_neg_integer(),
             Input :: string(),
             empty | input) ->
    string().

afterr(Char, Input, empty) ->
    afterr_empty([], Char, Input);
afterr(Char, Input, input) ->
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

-spec beforel(Char :: non_neg_integer(),
              Input :: string()) ->
    string().

beforel(Char, Input) ->
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

-spec beforel(Char :: non_neg_integer(),
              Input :: string(),
              empty | input) ->
    string().

beforel(Char, Input, empty) ->
    beforel_empty([], Char, Input);
beforel(Char, Input, input) ->
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

-spec beforer(Char :: non_neg_integer(),
              Input :: string()) ->
    string().

beforer(Char, Input) ->
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

-spec beforer(Char :: non_neg_integer(),
              Input :: string(),
              empty | input) ->
    string().

beforer(Char, Input, empty) ->
    beforer_empty([], [], Char, Input);
beforer(Char, Input, input) ->
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
%% ===Convert a binary string to an Erlang term.===
%% @end
%%-------------------------------------------------------------------------

-spec binary_to_term(B :: binary()) ->
    any().

binary_to_term(B) ->
    list_to_term(erlang:binary_to_list(B)).

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

compare_constant(Test, [_ | _] = Correct) ->
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

compare_constant_binary(Test, Correct) ->
    compare_constant(erlang:binary_to_list(Test),
                     erlang:binary_to_list(Correct)).

%%-------------------------------------------------------------------------
%% @doc
%% ===Find the beginning of a substring in a string from the left.===
%% @end
%%-------------------------------------------------------------------------

-spec findl(SearchPattern :: string() | binary(),
            String :: string() | binary()) ->
    string() | binary() | false.

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
findl(SearchPattern, String) ->
    case string:find(String, SearchPattern, leading) of
        nomatch ->
            false;
        Result ->
            Result
    end.
-else.
findl(SearchPattern, String)
    when is_list(SearchPattern) ->
    StringList = if
        is_binary(String) ->
            erlang:binary_to_list(String);
        is_list(String) ->
            lists:flatten(String)
    end,
    SearchPatternList = if
        is_binary(SearchPattern) ->
            erlang:binary_to_list(SearchPattern);
        is_list(SearchPattern) ->
            lists:flatten(SearchPattern)
    end,
    case string:str(StringList, SearchPatternList) of
        0 ->
            false;
        Index ->
            Result = lists:nthtail(Index - 1, StringList),
            if
                is_binary(String) ->
                    erlang:list_to_binary(Result);
                is_list(String) ->
                    Result
            end
    end.
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Find the beginning of a substring in a string from the right.===
%% @end
%%-------------------------------------------------------------------------

-spec findr(SearchPattern :: string() | binary(),
            String :: string() | binary()) ->
    string() | binary() | false.

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
findr(SearchPattern, String) ->
    case string:find(String, SearchPattern, trailing) of
        nomatch ->
            false;
        Result ->
            Result
    end.
-else.
findr(SearchPattern, String) ->
    StringList = if
        is_binary(String) ->
            erlang:binary_to_list(String);
        is_list(String) ->
            lists:flatten(String)
    end,
    SearchPatternList = if
        is_binary(SearchPattern) ->
            erlang:binary_to_list(SearchPattern);
        is_list(SearchPattern) ->
            lists:flatten(SearchPattern)
    end,
    case string:rstr(StringList, SearchPatternList) of
        0 ->
            false;
        Index ->
            Result = lists:nthtail(Index - 1, StringList),
            if
                is_binary(String) ->
                    erlang:list_to_binary(Result);
                is_list(String) ->
                    Result
            end
    end.
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Format a string based on the arguments.===
%% @end
%%-------------------------------------------------------------------------

-spec format(L :: string(),
             A :: list()) ->
    string().

-compile({inline, [{format, 2}]}).

format(L, A) ->
    format_to_list(L, A).

%%-------------------------------------------------------------------------
%% @doc
%% ===Format a string based on the arguments, stored as a binary.===
%% Output is a utf8 encoded binary.
%% @end
%%-------------------------------------------------------------------------

-spec format_to_binary(L :: string(),
                       A :: list()) ->
    binary().

format_to_binary(L, A) when is_list(L), is_list(A) ->
    unicode:characters_to_binary(io_lib:format(L, A)).

%%-------------------------------------------------------------------------
%% @doc
%% ===Format a string based on the arguments, stored as a list.===
%% Output may include unicode characters with a numerical value greater
%% than 255 (preventing the output from being used directly with
%% erlang:iolist_to_binary/1)..
%% @end
%%-------------------------------------------------------------------------

-spec format_to_list(L :: string(),
                     A :: list()) ->
    string().

-compile({inline, [{format_to_list, 2}]}).

format_to_list(L, A) when is_list(L), is_list(A) ->
    lists:flatten(io_lib:format(L, A)).

%%-------------------------------------------------------------------------
%% @doc
%% ===Join a list of strings with a string.===
%% @end
%%-------------------------------------------------------------------------

-spec join(Delimiters :: string() | binary(),
           L:: list(string() | binary())) ->
    string() | binary().

join(Delimiters, L) when is_binary(Delimiters) ->
    [[_ | H] | T] = [[Delimiters, S] || S <- L],
    unicode:characters_to_binary([H | T]);
join(Delimiters, L) when is_list(Delimiters) ->
    join_list(L, [], Delimiters).

join_list([], [], _) ->
    [];
join_list([H], Output, _) ->
    lists:reverse(Output, H);
join_list([H | T], Output, Delimiters) ->
    join_list(T, join_list(Delimiters, join_list(H, Output)), Delimiters).

join_list([], Output) ->
    Output;
join_list([H | T], Output) ->
    join_list(T, [H | Output]).

%%-------------------------------------------------------------------------
%% @doc
%% ===Convert a string to an Erlang term.===
%% @end
%%-------------------------------------------------------------------------

-spec list_to_term(L :: string()) ->
    any().

list_to_term(L) ->
    {ok, S, _} = erl_scan:string(L ++ "."),
    case erl_parse:parse_term(S) of
        {ok, Term} ->
            Term;
        {error, Reason} ->
            throw(Reason)
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the string in lowercase.===
%% @end
%%-------------------------------------------------------------------------

-spec lowercase(String :: string() | binary()) ->
    string() | binary().

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
lowercase(String) ->
    string:lowercase(String).
-else.
lowercase(String)
    when is_list(String) ->
    string:to_lower(String);
lowercase(String)
    when is_binary(String) ->
    erlang:list_to_binary(string:to_lower(erlang:binary_to_list(String))).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Split the string at all occurrences of the search pattern.===
%% @end
%%-------------------------------------------------------------------------

-spec split(SearchPattern :: string() | binary() | list(string() | binary()),
            String :: string() | binary()) ->
    list(string() | binary()).

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
split(SearchPattern, String) ->
    string:split(String, SearchPattern, all).
-else.
split(SearchPattern, String)
    when is_list(String) ->
    [erlang:binary_to_list(S)
     || S <- split(SearchPattern, erlang:list_to_binary(String))];
split(SearchPattern, String)
    when is_binary(String) ->
    Pattern = if
        is_binary(SearchPattern) ->
            [SearchPattern];
        is_integer(hd(SearchPattern)) ->
            [erlang:list_to_binary(SearchPattern)];
        is_list(SearchPattern) ->
            [unicode:characters_to_binary(S) || S <- SearchPattern]
    end,
    binary:split(String, Pattern, [global]).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the two strings split at the first occurrence of the character, otherwise return an empty string, when traversing left to right.===
%% @end
%%-------------------------------------------------------------------------

-spec splitl(Char :: non_neg_integer(),
             Input :: string()) ->
    {string(), string()}.

splitl(Char, Input) ->
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

-spec splitl(Char :: non_neg_integer(),
             Input :: string(),
             empty | input) ->
    {string(), string()}.

splitl(Char, Input, empty) ->
    splitl_empty([], Char, Input);
splitl(Char, Input, input) ->
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

-spec splitr(Char :: non_neg_integer(),
             Input :: string()) ->
    {string(), string()}.

splitr(Char, Input) ->
    splitr_empty([], [], Char, Input).
splitr_empty([Char | L1], [], Char, []) ->
    {lists:reverse(L1), []};
splitr_empty(_, [], _, []) ->
    {[], []};
splitr_empty(L1, L2, Char, []) ->
    {splitr_prefix(L1, L2, Char), L2};
splitr_empty(L1, _, Char, [Char | Rest]) ->
    splitr_empty([Char | L1], Rest, Char, Rest);
splitr_empty(L1, L2, Char, [C | Rest]) ->
    splitr_empty([C | L1], L2, Char, Rest).
splitr_prefix([Char | L1], [], Char) ->
    lists:reverse(L1);
splitr_prefix([_ | L1], [_ | L2], Char) ->
    splitr_prefix(L1, L2, Char).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the two strings split at the first occurrence of the character, otherwise return based on the failure atom, when traversing right to left.===
%% @end
%%-------------------------------------------------------------------------

-spec splitr(Char :: non_neg_integer(),
             Input :: string(),
             empty | input) ->
    {string(), string()}.

splitr(Char, Input, empty) ->
    splitr_empty([], [], Char, Input);
splitr(Char, Input, input) ->
    splitr_input([], [], Char, Input, Input).
splitr_input([Char | L1], [], Char, [], _) ->
    {lists:reverse(L1), []};
splitr_input(_, [], _, [], Input) ->
    {[], Input};
splitr_input(L1, L2, Char, [], _) ->
    {splitr_prefix(L1, L2, Char), L2};
splitr_input(L1, _, Char, [Char | Rest], Input) ->
    splitr_input([Char | L1], Rest, Char, Rest, Input);
splitr_input(L1, L2, Char, [C | Rest], Input) ->
    splitr_input([C | L1], L2, Char, Rest, Input).

%%-------------------------------------------------------------------------
%% @doc
%% ===Convert an Erlang term to a binary string.===
%% Output is a utf8 encoded binary.
%% @end
%%-------------------------------------------------------------------------

-spec term_to_binary(T :: any()) ->
    binary().

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
term_to_binary(T) ->
    unicode:characters_to_binary(io_lib:format("~tw", [T])).
-else.
term_to_binary(T) ->
    unicode:characters_to_binary(io_lib:format("~w", [T])).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Convert an Erlang term to a compact binary string.===
%% Output is a utf8 encoded binary.
%% @end
%%-------------------------------------------------------------------------

-spec term_to_binary_compact(T :: any()) ->
    binary().

term_to_binary_compact(T) ->
    unicode:characters_to_binary(io_lib:format("~" ?COMPACT_LIMIT "p", [T])).

%%-------------------------------------------------------------------------
%% @doc
%% ===Convert an Erlang term to a string.===
%% @end
%%-------------------------------------------------------------------------

-spec term_to_list(T :: any()) ->
    string().

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
term_to_list(T) ->
    format("~tw", [T]).
-else.
term_to_list(T) ->
    format("~w", [T]).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Convert an Erlang term to a compact string.===
%% @end
%%-------------------------------------------------------------------------

-spec term_to_list_compact(T :: any()) ->
    string().

term_to_list_compact(T) ->
    format("~" ?COMPACT_LIMIT "p", [T]).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the string in titlecase.===
%% @end
%%-------------------------------------------------------------------------

-spec titlecase(String :: string() | binary()) ->
    string() | binary().

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
titlecase(String) ->
    string:titlecase(String).
-else.
titlecase(String) ->
    % functionality was not present in any form
    String.
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Trim the edges of the string.===
%% @end
%%-------------------------------------------------------------------------

-spec trim(String :: string() | binary()) ->
    string() | binary().

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
trim(String) ->
    string:trim(String).
-else.
trim(String) ->
    trim(?WHITESPACE, String).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Trim the edges of the string.===
%% @end
%%-------------------------------------------------------------------------

-spec trim(Characters :: string() | list(string()),
           String :: string() | binary()) ->
    string() | binary().

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
trim(Characters, String) ->
    string:trim(String, both, Characters).
-else.
trim(Characters, String)
    when is_list(String) ->
    trim_list(lists:flatten(Characters), String);
trim(Characters, String)
    when is_binary(String) ->
    erlang:list_to_binary(trim_list(lists:flatten(Characters),
                                    erlang:binary_to_list(String))).

trim_list([], String) ->
    String;
trim_list([H | T], String) ->
    trim_list(T, string:strip(String, both, H)).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Trim the edges of the string from the left.===
%% @end
%%-------------------------------------------------------------------------

-spec triml(String :: string() | binary()) ->
    string() | binary().

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
triml(String) ->
    string:trim(String, leading).
-else.
triml(String) ->
    triml(?WHITESPACE, String).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Trim the edges of the string from the left.===
%% @end
%%-------------------------------------------------------------------------

-spec triml(Characters :: string() | list(string()),
            String :: string() | binary()) ->
    string() | binary().

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
triml(Characters, String) ->
    string:trim(String, leading, Characters).
-else.
triml(Characters, String)
    when is_list(String) ->
    triml_list(lists:flatten(Characters), String);
triml(Characters, String)
    when is_binary(String) ->
    erlang:list_to_binary(triml_list(lists:flatten(Characters),
                                     erlang:binary_to_list(String))).

triml_list([], String) ->
    String;
triml_list([H | T], String) ->
    triml_list(T, string:strip(String, left, H)).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Trim the edges of the string from the right.===
%% @end
%%-------------------------------------------------------------------------

-spec trimr(String :: string() | binary()) ->
    string() | binary().

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
trimr(String) ->
    string:trim(String, trailing).
-else.
trimr(String) ->
    trimr(?WHITESPACE, String).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Trim the edges of the string from the right.===
%% @end
%%-------------------------------------------------------------------------

-spec trimr(Characters :: string() | list(string()),
            String :: string() | binary()) ->
    string() | binary().

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
trimr(Characters, String) ->
    string:trim(String, trailing, Characters).
-else.
trimr(Characters, String)
    when is_list(String) ->
    trimr_list(lists:flatten(Characters), String);
trimr(Characters, String)
    when is_binary(String) ->
    erlang:list_to_binary(trimr_list(lists:flatten(Characters),
                                     erlang:binary_to_list(String))).

trimr_list([], String) ->
    String;
trimr_list([H | T], String) ->
    trimr_list(T, string:strip(String, right, H)).
-endif.

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the string in uppercase.===
%% @end
%%-------------------------------------------------------------------------

-spec uppercase(String :: string() | binary()) ->
    string() | binary().

-ifdef(ERLANG_OTP_VERSION_20_FEATURES).
uppercase(String) ->
    string:uppercase(String).
-else.
uppercase(String)
    when is_list(String) ->
    string:to_upper(String);
uppercase(String)
    when is_binary(String) ->
    erlang:list_to_binary(string:to_upper(erlang:binary_to_list(String))).
-endif.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

afterl_test() ->
    "this-is-all-input" = afterl($/, "this-is-all-input", input),
    "" = afterl($/, "this-is-all-input", empty),
    "" = afterl($/, "this-is-all-input"),
    "is-all-input" = afterl($-, "this-is-all-input", input),
    "is-all-input" = afterl($-, "this-is-all-input", empty),
    "is-all-input" = afterl($-, "this-is-all-input"),
    ok.

afterr_test() ->
    "this-is-all-input" = afterr($/, "this-is-all-input", input),
    "" = afterr($/, "this-is-all-input", empty),
    "" = afterr($/, "this-is-all-input"),
    "input" = afterr($-, "this-is-all-input", input),
    "input" = afterr($-, "this-is-all-input", empty),
    "input" = afterr($-, "this-is-all-input"),
    ok.

beforel_test() ->
    "this-is-all-input" = beforel($/, "this-is-all-input", input),
    "" = beforel($/, "this-is-all-input", empty),
    "" = beforel($/, "this-is-all-input"),
    "this" = beforel($-, "this-is-all-input", input),
    "this" = beforel($-, "this-is-all-input", empty),
    "this" = beforel($-, "this-is-all-input"),
    ok.

beforer_test() ->
    "this-is-all-input" = beforer($/, "this-is-all-input", input),
    "" = beforer($/, "this-is-all-input", empty),
    "" = beforer($/, "this-is-all-input"),
    "this-is-all" = beforer($-, "this-is-all-input", input),
    "this-is-all" = beforer($-, "this-is-all-input", empty),
    "this-is-all" = beforer($-, "this-is-all-input"),
    ok.

findl_test() ->
    "..cd..ef" = findl(".", "ab..cd..ef"),
    <<"..cd..ef">> = findl(".", <<"ab..cd..ef">>),
    <<"..cd..ef">> = findl("..", <<"ab..cd..ef">>),
    "..cd..ef" = findl("..", "ab..cd..ef"),
    false = findl("x", <<"ab..cd..ef">>),
    false = findl("x", "ab..cd..ef"),
    ok.

findr_test() ->
    ".ef" = findr(".", "ab..cd..ef"),
    <<".ef">> = findr(".", <<"ab..cd..ef">>),
    <<"..ef">> = findr("..", <<"ab..cd..ef">>),
    "..ef" = findr("..", "ab..cd..ef"),
    false = findr("x", <<"ab..cd..ef">>),
    false = findr("x", "ab..cd..ef"),
    ok.

join_test() ->
    "ab..bc..cd" = join("..", ["ab","bc","cd"]),
    <<"ab..bc..cd">> = join(<<"..">>, [<<"ab">>,<<"bc">>,<<"cd">>]),
    <<"ab..bc....cd">> = join(<<"..">>, [<<"ab">>,<<"bc">>,<<>>,<<"cd">>]),
    ok.

split_test() ->
    ["ab","bc","cd"] = split("..", "ab..bc..cd"),
    [<<"ab">>,<<"bc">>,<<"cd">>] = split(<<"..">>, <<"ab..bc..cd">>),
    [<<"ab">>,<<"bc">>,<<>>,<<"cd">>] = split(<<"..">>, <<"ab..bc....cd">>),
    ok.

splitl_test() ->
    {"this-is-all-input", ""} = splitl($/, "this-is-all-input", input),
    {"", ""} = splitl($/, "this-is-all-input", empty),
    {"", ""} = splitl($/, "this-is-all-input"),
    {"this", "is-all-input"} = splitl($-, "this-is-all-input", input),
    {"this", "is-all-input"} = splitl($-, "this-is-all-input", empty),
    {"this", "is-all-input"} = splitl($-, "this-is-all-input"),
    ok.

splitr_test() ->
    {"", "this-is-all-input"} = splitr($/, "this-is-all-input", input),
    {"", ""} = splitr($/, "this-is-all-input", empty),
    {"", ""} = splitr($/, "this-is-all-input"),
    {"this-is-all", "input"} = splitr($-, "this-is-all-input", input),
    {"this-is-all", "input"} = splitr($-, "this-is-all-input", empty),
    {"this-is-all", "input"} = splitr($-, "this-is-all-input"),
    ok.

trim_test() ->
    <<"Hello">> = trim(<<"\t  Hello  \n">>),
    "Hello" = trim("\t  Hello  \n"),
    <<".Hello.">> = trim(<<".Hello.\n">>),
    ".Hello." = trim(".Hello.\n"),
    ok.

triml_test() ->
    <<"Hello  \n">> = triml(<<"\t  Hello  \n">>),
    "Hello  \n" = triml("\t  Hello  \n"),
    <<"Hello.\n">> = triml("\n.", <<".Hello.\n">>),
    "Hello.\n" = triml("\n.", ".Hello.\n"),
    ok.

trimr_test() ->
    <<"\t  Hello">> = trimr(<<"\t  Hello  \n">>),
    "\t  Hello" = trimr("\t  Hello  \n"),
    <<".Hello">> = trimr("\n.", <<".Hello.\n">>),
    ".Hello" = trimr("\n.", ".Hello.\n"),
    ok.

-endif.
