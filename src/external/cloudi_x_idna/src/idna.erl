-module(idna).

-export([to_ascii/1,
         utf8_to_ascii/1,
         from_ascii/1]).

-define(ACE_PREFIX, "xn--").

to_ascii(Domain) ->
    to_ascii(string:tokens(lowercase(Domain), "."), []).

utf8_to_ascii(Domain) ->
    to_ascii(idna_ucs:from_utf8(Domain)).

-spec from_ascii(nonempty_string()) -> nonempty_string().
from_ascii(Domain) ->
    from_ascii(string:tokens(Domain, "."), []).

%% Helper functions
%%
to_ascii([], Acc) ->
    lists:reverse(Acc);
to_ascii([Label|Labels], []) ->
    to_ascii(Labels, lists:reverse(label_to_ascii(Label)));
to_ascii([Label|Labels], Acc) ->
    to_ascii(Labels, lists:reverse(label_to_ascii(Label), [$.|Acc])).

label_to_ascii(Label) ->
    case lists:all(fun(C) -> idna_ucs:is_ascii(C) end, Label) of
        true ->
            Label;
        false ->
            ?ACE_PREFIX ++ punycode:encode(characters_to_nfkc_list(Label))
    end.

from_ascii([], Acc) ->
    lists:reverse(Acc);
from_ascii([Label|Labels], []) ->
    from_ascii(Labels, lists:reverse(label_from_ascii(Label)));
from_ascii([Label|Labels], Acc) ->
    from_ascii(Labels, lists:reverse(label_from_ascii(Label), [$.|Acc])).

label_from_ascii(?ACE_PREFIX ++ Label) ->
    punycode:decode(Label);
label_from_ascii(Label) ->
    Label.


%% Lowercase all chars in Str
-spec lowercase(String::unicode:chardata()) -> unicode:chardata().
lowercase(CD) when is_list(CD) ->
  lowercase_list(CD);
lowercase(CD) when is_binary(CD) ->
  lowercase_bin(CD,<<>>).

lowercase_list(CPs0) ->
  case unicode_util_compat:lowercase(CPs0) of
    [Char|CPs] -> append(Char,lowercase_list(CPs));
    [] -> []
  end.

lowercase_bin(CPs0, Acc) ->
  case unicode_util_compat:lowercase(CPs0) of
    [Char|CPs] when is_integer(Char) ->
      lowercase_bin(CPs, <<Acc/binary, Char/utf8>>);
    [Chars|CPs] ->
      lowercase_bin(CPs, <<Acc/binary,
                           << <<CP/utf8>> || CP <- Chars>>/binary >>);
    [] -> Acc
  end.

append(Char, <<>>) when is_integer(Char) -> [Char];
append(Char, <<>>) when is_list(Char) -> Char;
append(Char, Bin) when is_binary(Bin) -> [Char,Bin];
append(Char, Str) when is_integer(Char) -> [Char|Str];
append(GC, Str) when is_list(GC) -> GC ++ Str.


characters_to_nfkc_list(CD) ->
    case unicode_util_compat:nfkc(CD) of
        [CPs|Str] when is_list(CPs) -> CPs ++ characters_to_nfkc_list(Str);
        [CP|Str] -> [CP|characters_to_nfkc_list(Str)];
        [] -> []
    end.
