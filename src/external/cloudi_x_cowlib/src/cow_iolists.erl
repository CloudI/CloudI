%% Copyright (c) 2017-2018, Loïc Hoguin <essen@ninenines.eu>
%%
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%%
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

-module(cow_iolists).

-export([split/2]).

-ifdef(TEST).
-include_lib("proper/include/proper.hrl").
-endif.

-spec split(non_neg_integer(), iodata()) -> {iodata(), iodata()}.
split(N, Iolist) ->
	case split(N, Iolist, []) of
		{ok, Before, After} ->
			{Before, After};
		{more, _, Before} ->
			{lists:reverse(Before), <<>>}
	end.

split(0, Rest, Acc) ->
	{ok, lists:reverse(Acc), Rest};
split(N, [], Acc) ->
	{more, N, Acc};
split(N, Binary, Acc) when byte_size(Binary) =< N ->
	{more, N - byte_size(Binary), [Binary|Acc]};
split(N, Binary, Acc) when is_binary(Binary) ->
	<< Before:N/binary, After/bits >> = Binary,
	{ok, lists:reverse([Before|Acc]), After};
split(N, [Binary|Tail], Acc) when byte_size(Binary) =< N ->
	split(N - byte_size(Binary), Tail, [Binary|Acc]);
split(N, [Binary|Tail], Acc) when is_binary(Binary) ->
	<< Before:N/binary, After/bits >> = Binary,
	{ok, lists:reverse([Before|Acc]), [After|Tail]};
split(N, [Char|Tail], Acc) when is_integer(Char) ->
	split(N - 1, Tail, [Char|Acc]);
split(N, [List|Tail], Acc0) ->
	case split(N, List, Acc0) of
		{ok, Before, After} ->
			{ok, Before, [After|Tail]};
		{more, More, Acc} ->
			split(More, Tail, Acc)
	end.

-ifdef(TEST).

split_test_() ->
	Tests = [
		{10, "Hello world!", "Hello worl", "d!"},
		{10, <<"Hello world!">>, "Hello worl", "d!"},
		{10, ["He", [<<"llo">>], $\s, [["world"], <<"!">>]], "Hello worl", "d!"},
		{10, ["Hello "|<<"world!">>], "Hello worl", "d!"},
		{10, "Hello!", "Hello!", ""},
		{10, <<"Hello!">>, "Hello!", ""},
		{10, ["He", [<<"ll">>], $o, [["!"]]], "Hello!", ""},
		{10, ["Hel"|<<"lo!">>], "Hello!", ""},
		{10, [[<<>>|<<>>], [], <<"Hello world!">>], "Hello worl", "d!"},
		{10, [[<<"He">>|<<"llo">>], [$\s], <<"world!">>], "Hello worl", "d!"},
		{10, [[[]|<<"He">>], [[]|<<"llo wor">>]|<<"ld!">>], "Hello worl", "d!"}
	],
	[{iolist_to_binary(V), fun() ->
		{B, A} = split(N, V),
		true = iolist_to_binary(RB) =:= iolist_to_binary(B),
		true = iolist_to_binary(RA) =:= iolist_to_binary(A)
	end} || {N, V, RB, RA} <- Tests].

prop_split_test() ->
	?FORALL({N, Input},
		{non_neg_integer(), iolist()},
		begin
			Size = iolist_size(Input),
			{Before, After} = split(N, Input),
			if
				N >= Size ->
					((iolist_size(After) =:= 0)
						andalso iolist_to_binary(Before) =:= iolist_to_binary(Input));
				true ->
					<<ExpectBefore:N/binary, ExpectAfter/bits>> = iolist_to_binary(Input),
					(ExpectBefore =:= iolist_to_binary(Before))
						andalso (ExpectAfter =:= iolist_to_binary(After))
			end
		end).

-endif.
