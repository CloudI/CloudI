%% Copyright (c) 2019, Loïc Hoguin <essen@ninenines.eu>
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

%% The mapping between Erlang and structured headers types is as follow:
%%
%% List: list()
%% Dictionary: map()
%% Bare item: one bare_item() that can be of type:
%% Integer: integer()
%% Float: float()
%% String: {string, binary()}
%% Token: {token, binary()}
%% Byte sequence: {binary, binary()}
%% Boolean: boolean()
%% And finally:
%% Type with Parameters: {with_params, Type, Parameters}
%% Parameters: [{binary(), bare_item()}]

-module(cow_http_struct_hd).

-export([parse_dictionary/1]).
-export([parse_item/1]).
-export([parse_list/1]).
-export([dictionary/1]).
-export([item/1]).
-export([list/1]).

-include("cow_parse.hrl").

-type sh_list() :: [sh_item() | sh_inner_list()].
-type sh_inner_list() :: sh_with_params([sh_item()]).
-type sh_params() :: #{binary() => sh_bare_item() | undefined}.
-type sh_dictionary() :: {#{binary() => sh_item() | sh_inner_list()}, [binary()]}.
-type sh_item() :: sh_with_params(sh_bare_item()).
-type sh_bare_item() :: integer() | float() | boolean()
	| {string | token | binary, binary()}.
-type sh_with_params(Type) :: {with_params, Type, sh_params()}.

-define(IS_LC_ALPHA(C),
	(C =:= $a) or (C =:= $b) or (C =:= $c) or (C =:= $d) or (C =:= $e) or
	(C =:= $f) or (C =:= $g) or (C =:= $h) or (C =:= $i) or (C =:= $j) or
	(C =:= $k) or (C =:= $l) or (C =:= $m) or (C =:= $n) or (C =:= $o) or
	(C =:= $p) or (C =:= $q) or (C =:= $r) or (C =:= $s) or (C =:= $t) or
	(C =:= $u) or (C =:= $v) or (C =:= $w) or (C =:= $x) or (C =:= $y) or
	(C =:= $z)
).

%% Parsing.

-spec parse_dictionary(binary()) -> sh_dictionary().
parse_dictionary(<<>>) ->
	{#{}, []};
parse_dictionary(<<C,R/bits>>) when ?IS_LC_ALPHA(C) ->
	{Dict, Order, <<>>} = parse_dict_key(R, #{}, [], <<C>>),
	{Dict, Order}.

parse_dict_key(<<$=,$(,R0/bits>>, Acc, Order, K) ->
	false = maps:is_key(K, Acc),
	{Item, R} = parse_inner_list(R0, []),
	parse_dict_before_sep(R, Acc#{K => Item}, [K|Order]);
parse_dict_key(<<$=,R0/bits>>, Acc, Order, K) ->
	false = maps:is_key(K, Acc),
	{Item, R} = parse_item1(R0),
	parse_dict_before_sep(R, Acc#{K => Item}, [K|Order]);
parse_dict_key(<<C,R/bits>>, Acc, Order, K)
		when ?IS_LC_ALPHA(C) or ?IS_DIGIT(C)
			or (C =:= $_) or (C =:= $-) or (C =:= $*) ->
	parse_dict_key(R, Acc, Order, <<K/binary,C>>).

parse_dict_before_sep(<<C,R/bits>>, Acc, Order) when ?IS_WS(C) ->
	parse_dict_before_sep(R, Acc, Order);
parse_dict_before_sep(<<C,R/bits>>, Acc, Order) when C =:= $, ->
	parse_dict_before_member(R, Acc, Order);
parse_dict_before_sep(<<>>, Acc, Order) ->
	{Acc, lists:reverse(Order), <<>>}.

parse_dict_before_member(<<C,R/bits>>, Acc, Order) when ?IS_WS(C) ->
	parse_dict_before_member(R, Acc, Order);
parse_dict_before_member(<<C,R/bits>>, Acc, Order) when ?IS_LC_ALPHA(C) ->
	parse_dict_key(R, Acc, Order, <<C>>).

-spec parse_item(binary()) -> sh_item().
parse_item(Bin) ->
	{Item, <<>>} = parse_item1(Bin),
	Item.

parse_item1(Bin) ->
	case parse_bare_item(Bin) of
		{Item, <<$;,R/bits>>} ->
			{Params, Rest} = parse_before_param(R, #{}),
			{{with_params, Item, Params}, Rest};
		{Item, Rest} ->
			{{with_params, Item, #{}}, Rest}
	end.

-spec parse_list(binary()) -> sh_list().
parse_list(<<>>) ->
	[];
parse_list(Bin) ->
	parse_list_before_member(Bin, []).

parse_list_member(<<$(,R0/bits>>, Acc) ->
	{Item, R} = parse_inner_list(R0, []),
	parse_list_before_sep(R, [Item|Acc]);
parse_list_member(R0, Acc) ->
	{Item, R} = parse_item1(R0),
	parse_list_before_sep(R, [Item|Acc]).

parse_list_before_sep(<<C,R/bits>>, Acc) when ?IS_WS(C) ->
	parse_list_before_sep(R, Acc);
parse_list_before_sep(<<$,,R/bits>>, Acc) ->
	parse_list_before_member(R, Acc);
parse_list_before_sep(<<>>, Acc) ->
	lists:reverse(Acc).

parse_list_before_member(<<C,R/bits>>, Acc) when ?IS_WS(C) ->
	parse_list_before_member(R, Acc);
parse_list_before_member(R, Acc) ->
	parse_list_member(R, Acc).

%% Internal.

parse_inner_list(<<C,R/bits>>, Acc) when ?IS_WS(C) ->
	parse_inner_list(R, Acc);
parse_inner_list(<<$),$;,R0/bits>>, Acc) ->
	{Params, R} = parse_before_param(R0, #{}),
	{{with_params, lists:reverse(Acc), Params}, R};
parse_inner_list(<<$),R/bits>>, Acc) ->
	{{with_params, lists:reverse(Acc), #{}}, R};
parse_inner_list(R0, Acc) ->
	{Item, R = <<C,_/bits>>} = parse_item1(R0),
	true = (C =:= $\s) orelse (C =:= $)),
	parse_inner_list(R, [Item|Acc]).

parse_before_param(<<C,R/bits>>, Acc) when ?IS_WS(C) ->
	parse_before_param(R, Acc);
parse_before_param(<<C,R/bits>>, Acc) when ?IS_LC_ALPHA(C) ->
	parse_param(R, Acc, <<C>>).

parse_param(<<$;,R/bits>>, Acc, K) ->
	parse_before_param(R, Acc#{K => undefined});
parse_param(<<$=,R0/bits>>, Acc, K) ->
	case parse_bare_item(R0) of
		{Item, <<$;,R/bits>>} ->
			false = maps:is_key(K, Acc),
			parse_before_param(R, Acc#{K => Item});
		{Item, R} ->
			false = maps:is_key(K, Acc),
			{Acc#{K => Item}, R}
	end;
parse_param(<<C,R/bits>>, Acc, K)
		when ?IS_LC_ALPHA(C) or ?IS_DIGIT(C)
			or (C =:= $_) or (C =:= $-) or (C =:= $*) ->
	parse_param(R, Acc, <<K/binary,C>>);
parse_param(R, Acc, K) ->
	false = maps:is_key(K, Acc),
	{Acc#{K => undefined}, R}.

%% Integer or float.
parse_bare_item(<<$-,R/bits>>) -> parse_number(R, 0, <<$->>);
parse_bare_item(<<C,R/bits>>) when ?IS_DIGIT(C) -> parse_number(R, 1, <<C>>);
%% String.
parse_bare_item(<<$",R/bits>>) -> parse_string(R, <<>>);
%% Token.
parse_bare_item(<<C,R/bits>>) when ?IS_ALPHA(C) -> parse_token(R, <<C>>);
%% Byte sequence.
parse_bare_item(<<$*,R/bits>>) -> parse_binary(R, <<>>);
%% Boolean.
parse_bare_item(<<"?0",R/bits>>) -> {false, R};
parse_bare_item(<<"?1",R/bits>>) -> {true, R}.

parse_number(<<C,R/bits>>, L, Acc) when ?IS_DIGIT(C) ->
	parse_number(R, L+1, <<Acc/binary,C>>);
parse_number(<<C,R/bits>>, L, Acc) when C =:= $. ->
	parse_float(R, L, 0, <<Acc/binary,C>>);
parse_number(R, L, Acc) when L =< 15 ->
	{binary_to_integer(Acc), R}.

parse_float(<<C,R/bits>>, L1, L2, Acc) when ?IS_DIGIT(C) ->
	parse_float(R, L1, L2+1, <<Acc/binary,C>>);
parse_float(R, L1, L2, Acc) when
		L1 =< 9, L2 =< 6;
		L1 =< 10, L2 =< 5;
		L1 =< 11, L2 =< 4;
		L1 =< 12, L2 =< 3;
		L1 =< 13, L2 =< 2;
		L1 =< 14, L2 =< 1 ->
	{binary_to_float(Acc), R}.

parse_string(<<$\\,$",R/bits>>, Acc) ->
	parse_string(R, <<Acc/binary,$">>);
parse_string(<<$\\,$\\,R/bits>>, Acc) ->
	parse_string(R, <<Acc/binary,$\\>>);
parse_string(<<$",R/bits>>, Acc) ->
	{{string, Acc}, R};
parse_string(<<C,R/bits>>, Acc) when
		C >= 16#20, C =< 16#21;
		C >= 16#23, C =< 16#5b;
		C >= 16#5d, C =< 16#7e ->
	parse_string(R, <<Acc/binary,C>>).

parse_token(<<C,R/bits>>, Acc) when ?IS_TOKEN(C) or (C =:= $:) or (C =:= $/) ->
	parse_token(R, <<Acc/binary,C>>);
parse_token(R, Acc) ->
	{{token, Acc}, R}.

parse_binary(<<$*,R/bits>>, Acc) ->
	{{binary, base64:decode(Acc)}, R};
parse_binary(<<C,R/bits>>, Acc) when ?IS_ALPHANUM(C) or (C =:= $+) or (C =:= $/) or (C =:= $=) ->
	parse_binary(R, <<Acc/binary,C>>).

-ifdef(TEST).
parse_struct_hd_test_() ->
	Files = filelib:wildcard("deps/structured-header-tests/*.json"),
	lists:flatten([begin
		{ok, JSON} = file:read_file(File),
		Tests = jsx:decode(JSON, [return_maps]),
		[
			{iolist_to_binary(io_lib:format("~s: ~s", [filename:basename(File), Name])), fun() ->
				%% The implementation is strict. We fail whenever we can.
				CanFail = maps:get(<<"can_fail">>, Test, false),
				MustFail = maps:get(<<"must_fail">>, Test, false),
				Expected = case MustFail of
					true -> undefined;
					false -> expected_to_term(maps:get(<<"expected">>, Test))
				end,
				Raw = raw_to_binary(Raw0),
				case HeaderType of
					<<"dictionary">> when MustFail; CanFail ->
						{'EXIT', _} = (catch parse_dictionary(Raw));
					%% The test "binary.json: non-zero pad bits" does not fail
					%% due to our reliance on Erlang/OTP's base64 module.
					<<"item">> when CanFail ->
						case (catch parse_item(Raw)) of
							{'EXIT', _} -> ok;
							Expected -> ok
						end;
					<<"item">> when MustFail ->
						{'EXIT', _} = (catch parse_item(Raw));
					<<"list">> when MustFail; CanFail ->
						{'EXIT', _} = (catch parse_list(Raw));
					<<"dictionary">> ->
						{Expected, _Order} = (catch parse_dictionary(Raw));
					<<"item">> ->
						Expected = (catch parse_item(Raw));
					<<"list">> ->
						Expected = (catch parse_list(Raw))
				end
			end}
		|| Test=#{
			<<"name">> := Name,
			<<"header_type">> := HeaderType,
			<<"raw">> := Raw0
		} <- Tests]
	end || File <- Files]).

%% Item.
expected_to_term(E=[_, Params]) when is_map(Params) ->
	e2t(E);
%% Outer list.
expected_to_term(Expected) when is_list(Expected) ->
	[e2t(E) || E <- Expected];
expected_to_term(Expected) ->
	e2t(Expected).

%% Dictionary.
e2t(Dict) when is_map(Dict) ->
	maps:map(fun(_, V) -> e2t(V) end, Dict);
%% Inner list.
e2t([List, Params]) when is_list(List) ->
	{with_params, [e2t(E) || E <- List],
		maps:map(fun(_, P) -> e2tb(P) end, Params)};
%% Item.
e2t([Bare, Params]) ->
	{with_params, e2tb(Bare),
		maps:map(fun(_, P) -> e2tb(P) end, Params)}.

%% Bare item.
e2tb(#{<<"__type">> := <<"token">>, <<"value">> := V}) ->
	{token, V};
e2tb(#{<<"__type">> := <<"binary">>, <<"value">> := V}) ->
	{binary, base32:decode(V)};
e2tb(V) when is_binary(V) ->
	{string, V};
e2tb(null) ->
	undefined;
e2tb(V) ->
	V.

%% The Cowlib parsers currently do not support resuming parsing
%% in the case of multiple headers. To make tests work we modify
%% the raw value the same way Cowboy does when encountering
%% multiple headers: by adding a comma and space in between.
%%
%% Similarly, the Cowlib parsers expect the leading and trailing
%% whitespace to be removed before calling the parser.
raw_to_binary(RawList) ->
	trim_ws(iolist_to_binary(lists:join(<<", ">>, RawList))).

trim_ws(<<C,R/bits>>) when ?IS_WS(C) -> trim_ws(R);
trim_ws(R) -> trim_ws_end(R, byte_size(R) - 1).

trim_ws_end(_, -1) ->
	<<>>;
trim_ws_end(Value, N) ->
	case binary:at(Value, N) of
		$\s -> trim_ws_end(Value, N - 1);
		$\t -> trim_ws_end(Value, N - 1);
		_ ->
			S = N + 1,
			<< Value2:S/binary, _/bits >> = Value,
			Value2
	end.
-endif.

%% Building.

-spec dictionary(#{binary() => sh_item() | sh_inner_list()}
		| [{binary(), sh_item() | sh_inner_list()}])
	-> iolist().
%% @todo Also accept this? dictionary({Map, Order}) ->
dictionary(Map) when is_map(Map) ->
	dictionary(maps:to_list(Map));
dictionary(KVList) when is_list(KVList) ->
	lists:join(<<", ">>, [
		[Key, $=, item_or_inner_list(Value)]
	|| {Key, Value} <- KVList]).

-spec item(sh_item()) -> iolist().
item({with_params, BareItem, Params}) ->
	[bare_item(BareItem), params(Params)].

-spec list(sh_list()) -> iolist().
list(List) ->
	lists:join(<<", ">>, [item_or_inner_list(Value) || Value <- List]).

item_or_inner_list(Value={with_params, List, _}) when is_list(List) ->
	inner_list(Value);
item_or_inner_list(Value) ->
	item(Value).

inner_list({with_params, List, Params}) ->
	[$(, lists:join($\s, [item(Value) || Value <- List]), $), params(Params)].

bare_item({string, String}) ->
	[$", escape_string(String, <<>>), $"];
bare_item({token, Token}) ->
	Token;
bare_item({binary, Binary}) ->
	[$*, base64:encode(Binary), $*];
bare_item(Integer) when is_integer(Integer) ->
	integer_to_binary(Integer);
%% In order to properly reproduce the float as a string we
%% must first determine how many decimals we want in the
%% fractional component, otherwise rounding errors may occur.
bare_item(Float) when is_float(Float) ->
	Decimals = case trunc(Float) of
		I when I >= 10000000000000 -> 1;
		I when I >= 1000000000000 -> 2;
		I when I >= 100000000000 -> 3;
		I when I >= 10000000000 -> 4;
		I when I >= 1000000000 -> 5;
		_ -> 6
	end,
	float_to_binary(Float, [{decimals, Decimals}, compact]);
bare_item(true) ->
	<<"?1">>;
bare_item(false) ->
	<<"?0">>.

escape_string(<<>>, Acc) -> Acc;
escape_string(<<$\\,R/bits>>, Acc) -> escape_string(R, <<Acc/binary,$\\,$\\>>);
escape_string(<<$",R/bits>>, Acc) -> escape_string(R, <<Acc/binary,$\\,$">>);
escape_string(<<C,R/bits>>, Acc) -> escape_string(R, <<Acc/binary,C>>).

params(Params) ->
	maps:fold(fun
		(Key, undefined, Acc) ->
			[[$;, Key]|Acc];
		(Key, Value, Acc) ->
			[[$;, Key, $=, bare_item(Value)]|Acc]
	end, [], Params).

-ifdef(TEST).
struct_hd_identity_test_() ->
	Files = filelib:wildcard("deps/structured-header-tests/*.json"),
	lists:flatten([begin
		{ok, JSON} = file:read_file(File),
		Tests = jsx:decode(JSON, [return_maps]),
		[
			{iolist_to_binary(io_lib:format("~s: ~s", [filename:basename(File), Name])), fun() ->
				Expected = expected_to_term(Expected0),
				case HeaderType of
					<<"dictionary">> ->
						{Expected, _Order} = parse_dictionary(iolist_to_binary(dictionary(Expected)));
					<<"item">> ->
						Expected = parse_item(iolist_to_binary(item(Expected)));
					<<"list">> ->
						Expected = parse_list(iolist_to_binary(list(Expected)))
				end
			end}
		|| #{
			<<"name">> := Name,
			<<"header_type">> := HeaderType,
			%% We only run tests that must not fail.
			<<"expected">> := Expected0
		} <- Tests]
	end || File <- Files]).
-endif.
