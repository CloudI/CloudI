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
%%% Copyright (c) 2009-2022 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2009-2022 Michael Truog
%%% @version 2.0.5 {@date} {@time}
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
-define(COMPACT_LIMIT, "0").

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

compare_constant(Test, Correct) ->
    compare_constant_list(Test, Correct).

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

compare_constant_list(_, []) ->
    erlang:exit(badarg);
compare_constant_list(Test, [_ | _] = Correct) ->
    compare_constant_list(Test, Correct,
                          [-1 || _ <- Test], [], 0) =:= 0.

compare_constant_list([], [], _, _, Bits) ->
    Bits;
compare_constant_list([], [_ | _], _, _, _) ->
    -1;
compare_constant_list([C1 | Test], [] = Correct,
                      [C2 | Busy], BusyEmpty, Bits) ->
    compare_constant_list(Test, Correct,
                          Busy, BusyEmpty, Bits bor (C1 bxor C2));
compare_constant_list([C1 | Test], [C2 | Correct],
                      Busy, [] = BusyEmpty, Bits) ->
    compare_constant_list(Test, Correct,
                          Busy, BusyEmpty, Bits bor (C1 bxor C2)).

%%-------------------------------------------------------------------------
%% @doc
%% ===Time insensitive compare to avoid a timing leak with strings as binaries.===
%% Use for password or other authentication comparisons.
%% Execution time is based on the length of Test.
%% @end
%%-------------------------------------------------------------------------

-spec compare_constant_binary(Test :: <<_:_*8>>,
                              Correct :: <<_:8, _:_*8>>) ->
    boolean().

compare_constant_binary(_, <<>>) ->
    erlang:exit(badarg);
compare_constant_binary(Test, Correct) ->
    compare_constant_binary(Test, Correct,
                            <<<<255>> || <<_>> <= Test>>, <<>>, 0) =:= 0.

compare_constant_binary(<<>>, <<>>, _, _, Bits) ->
    Bits;
compare_constant_binary(<<>>, <<_:8, _/binary>>, _, _, _) ->
    -1;
compare_constant_binary(<<C1:8, Test/binary>>, <<>> = Correct,
                        <<C2:8/signed, Busy/binary>>, BusyEmpty, Bits) ->
    compare_constant_binary(Test, Correct,
                            Busy, BusyEmpty, Bits bor (C1 bxor C2));
compare_constant_binary(<<C1:8, Test/binary>>, <<C2:8, Correct/binary>>,
                        Busy, <<>> = BusyEmpty, Bits) ->
    compare_constant_binary(Test, Correct,
                            Busy, BusyEmpty, Bits bor (C1 bxor C2)).

%%-------------------------------------------------------------------------
%% @doc
%% ===Find the beginning of a substring in a string from the left.===
%% @end
%%-------------------------------------------------------------------------

-spec findl(SearchPattern :: string() | binary(),
            String :: string() | binary()) ->
    string() | binary() | false.

findl(SearchPattern, String) ->
    case string:find(String, SearchPattern, leading) of
        nomatch ->
            false;
        Result ->
            Result
    end.

%%-------------------------------------------------------------------------
%% @doc
%% ===Find the beginning of a substring in a string from the right.===
%% @end
%%-------------------------------------------------------------------------

-spec findr(SearchPattern :: string() | binary(),
            String :: string() | binary()) ->
    string() | binary() | false.

findr(SearchPattern, String) ->
    case string:find(String, SearchPattern, trailing) of
        nomatch ->
            false;
        Result ->
            Result
    end.

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

lowercase(String) ->
    string:lowercase(String).

%%-------------------------------------------------------------------------
%% @doc
%% ===Split the string at all occurrences of the search pattern.===
%% @end
%%-------------------------------------------------------------------------

-spec split(SearchPattern :: string() | binary() | list(string() | binary()),
            String :: string() | binary()) ->
    list(string() | binary()).

split(SearchPattern, String) ->
    string:split(String, SearchPattern, all).

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

term_to_binary(T) ->
    unicode:characters_to_binary(io_lib:format("~tw", [T])).

%%-------------------------------------------------------------------------
%% @doc
%% ===Convert an Erlang term to a compact binary string.===
%% Output is a utf8 encoded binary.
%% @end
%%-------------------------------------------------------------------------

-spec term_to_binary_compact(T :: any()) ->
    binary().

term_to_binary_compact(T) ->
    unicode:characters_to_binary(io_lib:format("~" ?COMPACT_LIMIT "tp", [T])).

%%-------------------------------------------------------------------------
%% @doc
%% ===Convert an Erlang term to a string.===
%% @end
%%-------------------------------------------------------------------------

-spec term_to_list(T :: any()) ->
    string().

term_to_list(T) ->
    format("~tw", [T]).

%%-------------------------------------------------------------------------
%% @doc
%% ===Convert an Erlang term to a compact string.===
%% @end
%%-------------------------------------------------------------------------

-spec term_to_list_compact(T :: any()) ->
    string().

term_to_list_compact(T) ->
    format("~" ?COMPACT_LIMIT "tp", [T]).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the string in titlecase.===
%% @end
%%-------------------------------------------------------------------------

-spec titlecase(String :: string() | binary()) ->
    string() | binary().

titlecase(String) ->
    string:titlecase(String).

%%-------------------------------------------------------------------------
%% @doc
%% ===Trim the edges of the string.===
%% @end
%%-------------------------------------------------------------------------

-spec trim(String :: string() | binary()) ->
    string() | binary().

trim(String) ->
    string:trim(String).

%%-------------------------------------------------------------------------
%% @doc
%% ===Trim the edges of the string.===
%% @end
%%-------------------------------------------------------------------------

-spec trim(Characters :: string() | list(string()),
           String :: string() | binary()) ->
    string() | binary().

trim(Characters, String) ->
    string:trim(String, both, Characters).

%%-------------------------------------------------------------------------
%% @doc
%% ===Trim the edges of the string from the left.===
%% @end
%%-------------------------------------------------------------------------

-spec triml(String :: string() | binary()) ->
    string() | binary().

triml(String) ->
    string:trim(String, leading).

%%-------------------------------------------------------------------------
%% @doc
%% ===Trim the edges of the string from the left.===
%% @end
%%-------------------------------------------------------------------------

-spec triml(Characters :: string() | list(string()),
            String :: string() | binary()) ->
    string() | binary().

triml(Characters, String) ->
    string:trim(String, leading, Characters).

%%-------------------------------------------------------------------------
%% @doc
%% ===Trim the edges of the string from the right.===
%% @end
%%-------------------------------------------------------------------------

-spec trimr(String :: string() | binary()) ->
    string() | binary().

trimr(String) ->
    string:trim(String, trailing).

%%-------------------------------------------------------------------------
%% @doc
%% ===Trim the edges of the string from the right.===
%% @end
%%-------------------------------------------------------------------------

-spec trimr(Characters :: string() | list(string()),
            String :: string() | binary()) ->
    string() | binary().

trimr(Characters, String) ->
    string:trim(String, trailing, Characters).

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the string in uppercase.===
%% @end
%%-------------------------------------------------------------------------

-spec uppercase(String :: string() | binary()) ->
    string() | binary().

uppercase(String) ->
    string:uppercase(String).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-include("cloudi_core_i_test.hrl").

module_test_() ->
    {timeout, ?TEST_TIMEOUT, [
        {"afterl tests", ?_assertOk(t_afterl())},
        {"afterr tests", ?_assertOk(t_afterr())},
        {"beforel tests", ?_assertOk(t_beforel())},
        {"beforer tests", ?_assertOk(t_beforer())},
        {"compare_constant_binary", ?_assertOk(t_compare_constant_binary())},
        {"compare_constant_list", ?_assertOk(t_compare_constant_list())},
        {"findl tests", ?_assertOk(t_findl())},
        {"findr tests", ?_assertOk(t_findr())},
        {"join tests", ?_assertOk(t_join())},
        {"split tests", ?_assertOk(t_split())},
        {"splitl tests", ?_assertOk(t_splitl())},
        {"splitr tests", ?_assertOk(t_splitr())},
        {"trim tests", ?_assertOk(t_trim())},
        {"triml tests", ?_assertOk(t_triml())},
        {"trimr tests", ?_assertOk(t_trimr())}
    ]}.

t_afterl() ->
    "this-is-all-input" = afterl($/, "this-is-all-input", input),
    "" = afterl($/, "this-is-all-input", empty),
    "" = afterl($/, "this-is-all-input"),
    "is-all-input" = afterl($-, "this-is-all-input", input),
    "is-all-input" = afterl($-, "this-is-all-input", empty),
    "is-all-input" = afterl($-, "this-is-all-input"),
    ok.

t_afterr() ->
    "this-is-all-input" = afterr($/, "this-is-all-input", input),
    "" = afterr($/, "this-is-all-input", empty),
    "" = afterr($/, "this-is-all-input"),
    "input" = afterr($-, "this-is-all-input", input),
    "input" = afterr($-, "this-is-all-input", empty),
    "input" = afterr($-, "this-is-all-input"),
    ok.

t_beforel() ->
    "this-is-all-input" = beforel($/, "this-is-all-input", input),
    "" = beforel($/, "this-is-all-input", empty),
    "" = beforel($/, "this-is-all-input"),
    "this" = beforel($-, "this-is-all-input", input),
    "this" = beforel($-, "this-is-all-input", empty),
    "this" = beforel($-, "this-is-all-input"),
    ok.

t_beforer() ->
    "this-is-all-input" = beforer($/, "this-is-all-input", input),
    "" = beforer($/, "this-is-all-input", empty),
    "" = beforer($/, "this-is-all-input"),
    "this-is-all" = beforer($-, "this-is-all-input", input),
    "this-is-all" = beforer($-, "this-is-all-input", empty),
    "this-is-all" = beforer($-, "this-is-all-input"),
    ok.

t_compare_constant_binary() ->
    true = compare_constant_binary(<<"abc">>, <<"abc">>),
    false = compare_constant_binary(<<"abc">>, <<"abd">>),
    false = compare_constant_binary(<<"abc">>, <<"abcd">>),
    false = compare_constant_binary(<<"abcde">>, <<"abcd">>),
    true = compare_constant_binary(<<"abcd">>, <<"abcd">>),
    ok.

t_compare_constant_list() ->
    true = compare_constant_list("abc", "abc"),
    false = compare_constant_list("abc", "abd"),
    false = compare_constant_list("abc", "abcd"),
    false = compare_constant_list("abcde", "abcd"),
    true = compare_constant_list("abcd", "abcd"),
    ok.

t_findl() ->
    "..cd..ef" = findl(".", "ab..cd..ef"),
    <<"..cd..ef">> = findl(".", <<"ab..cd..ef">>),
    <<"..cd..ef">> = findl("..", <<"ab..cd..ef">>),
    "..cd..ef" = findl("..", "ab..cd..ef"),
    false = findl("x", <<"ab..cd..ef">>),
    false = findl("x", "ab..cd..ef"),
    ok.

t_findr() ->
    ".ef" = findr(".", "ab..cd..ef"),
    <<".ef">> = findr(".", <<"ab..cd..ef">>),
    <<"..ef">> = findr("..", <<"ab..cd..ef">>),
    "..ef" = findr("..", "ab..cd..ef"),
    false = findr("x", <<"ab..cd..ef">>),
    false = findr("x", "ab..cd..ef"),
    ok.

t_join() ->
    "ab..bc..cd" = join("..", ["ab","bc","cd"]),
    <<"ab..bc..cd">> = join(<<"..">>, [<<"ab">>,<<"bc">>,<<"cd">>]),
    <<"ab..bc....cd">> = join(<<"..">>, [<<"ab">>,<<"bc">>,<<>>,<<"cd">>]),
    ok.

t_split() ->
    ["ab","bc","cd"] = split("..", "ab..bc..cd"),
    [<<"ab">>,<<"bc">>,<<"cd">>] = split(<<"..">>, <<"ab..bc..cd">>),
    [<<"ab">>,<<"bc">>,<<>>,<<"cd">>] = split(<<"..">>, <<"ab..bc....cd">>),
    ok.

t_splitl() ->
    {"this-is-all-input", ""} = splitl($/, "this-is-all-input", input),
    {"", ""} = splitl($/, "this-is-all-input", empty),
    {"", ""} = splitl($/, "this-is-all-input"),
    {"this", "is-all-input"} = splitl($-, "this-is-all-input", input),
    {"this", "is-all-input"} = splitl($-, "this-is-all-input", empty),
    {"this", "is-all-input"} = splitl($-, "this-is-all-input"),
    ok.

t_splitr() ->
    {"", "this-is-all-input"} = splitr($/, "this-is-all-input", input),
    {"", ""} = splitr($/, "this-is-all-input", empty),
    {"", ""} = splitr($/, "this-is-all-input"),
    {"this-is-all", "input"} = splitr($-, "this-is-all-input", input),
    {"this-is-all", "input"} = splitr($-, "this-is-all-input", empty),
    {"this-is-all", "input"} = splitr($-, "this-is-all-input"),
    ok.

t_trim() ->
    <<"Hello">> = trim(<<"\t  Hello  \n">>),
    "Hello" = trim("\t  Hello  \n"),
    <<".Hello.">> = trim(<<".Hello.\n">>),
    ".Hello." = trim(".Hello.\n"),
    ok.

t_triml() ->
    <<"Hello  \n">> = triml(<<"\t  Hello  \n">>),
    "Hello  \n" = triml("\t  Hello  \n"),
    <<"Hello.\n">> = triml("\n.", <<".Hello.\n">>),
    "Hello.\n" = triml("\n.", ".Hello.\n"),
    ok.

t_trimr() ->
    <<"\t  Hello">> = trimr(<<"\t  Hello  \n">>),
    "\t  Hello" = trimr("\t  Hello  \n"),
    <<".Hello">> = trimr("\n.", <<".Hello.\n">>),
    ".Hello" = trimr("\n.", ".Hello.\n"),
    ok.

-endif.
