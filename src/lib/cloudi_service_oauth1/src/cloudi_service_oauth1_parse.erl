%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI OAuth v1.0 Parsing==
%%% based on https://github.com/ninenines/cowboy
%%% @end
%%%
%%% MIT LICENSE
%%% 
%%% Copyright (c) 2011-2013, Loïc Hoguin <essen@ninenines.eu>
%%% 
%%% Permission to use, copy, modify, and/or distribute this software for any
%%% purpose with or without fee is hereby granted, provided that the above
%%% copyright notice and this permission notice appear in all copies.
%%% 
%%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%%%
%%% @author Loïc Hoguin <essen@ninenines.eu>
%%% @copyright 2011-2013 Loïc Hoguin
%%% @version 2.0.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_oauth1_parse).
-author('essen [at] ninenines (dot) eu').

%% external interface
-export([authorization/1]).

-spec authorization(binary()) ->
    list({string(), string()}) | {error, badarg}.

authorization(Binary) ->
    nonempty_list(Binary, fun authorization_tokens_param/2).

%%%------------------------------------------------------------------------
%%% From cowboy1, in cowboy1_http.erl
%%%------------------------------------------------------------------------

-spec authorization_tokens_param(binary(), fun()) -> any().
authorization_tokens_param(Data, Fun) ->
    whitespace(Data,
        fun (Rest) ->
            token(Rest,
                fun (_Rest2, <<>>) -> {error, badarg};
                    (<< $=, Rest2/binary >>, Attr) ->
                    word(Rest2,
                         fun (Rest3, Value) ->
                             NewValue = if
                                 Attr == <<"oauth_signature">>;
                                 Attr == <<"oauth_callback">>;
                                 Attr == <<"oauth_nonce">>;
                                 Attr == <<"oauth_consumer_key">>;
                                 Attr == <<"oauth_verifier">> ->
                                    cloudi_x_cow1_qs:urldecode(Value);
                                 true ->
                                    Value
                             end,
                             Fun(Rest3,
                                 {erlang:binary_to_list(Attr),
                                  erlang:binary_to_list(NewValue)})
                         end);
                    (Rest2, Attr) ->
                         Fun(Rest2, Attr)
                end)
        end).

%% @doc Parse either a token or a quoted string.
-spec word(binary(), fun((_, _) -> any())) -> any().
word(Data = << $", _/binary >>, Fun) ->
    quoted_string(Data, Fun);
word(Data, Fun) ->
    token(Data,
        fun (_Rest, <<>>) -> {error, badarg};
            (Rest, Token) -> Fun(Rest, Token)
        end).

%% @doc Parse a quoted string.
-spec quoted_string(<<_:8, _:_*8>>, fun((_, _) -> any())) -> any().
quoted_string(<< $", Rest/binary >>, Fun) ->
    quoted_string(Rest, Fun, <<>>).

-spec quoted_string(binary(), fun(), binary()) -> any().
quoted_string(<<>>, _Fun, _Acc) ->
    {error, badarg};
quoted_string(<< $", Rest/binary >>, Fun, Acc) ->
    Fun(Rest, Acc);
quoted_string(<< $\\, C, Rest/binary >>, Fun, Acc) ->
    quoted_string(Rest, Fun, << Acc/binary, C >>);
quoted_string(<< C, Rest/binary >>, Fun, Acc) ->
    quoted_string(Rest, Fun, << Acc/binary, C >>).

%% @doc Parse a token.
-spec token(binary(), fun((_, _) -> any())) -> any().
token(Data, Fun) ->
    token(Data, Fun, cs, <<>>).

-spec token(binary(), fun(), ci | cs, binary()) -> any().
token(<<>>, Fun, _Case, Acc) ->
    Fun(<<>>, Acc);
token(Data = << C, _Rest/binary >>, Fun, _Case, Acc)
        when C =:= $(; C =:= $); C =:= $<; C =:= $>; C =:= $@;
             C =:= $,; C =:= $;; C =:= $:; C =:= $\\; C =:= $";
             C =:= $/; C =:= $[; C =:= $]; C =:= $?; C =:= $=;
             C =:= ${; C =:= $}; C =:= $\s; C =:= $\t;
             C < 32; C =:= 127 ->
    Fun(Data, Acc);
%token(<< C, Rest/binary >>, Fun, Case = ci, Acc) ->
%    C2 = char_to_lower(C),
%    token(Rest, Fun, Case, << Acc/binary, C2 >>);
token(<< C, Rest/binary >>, Fun, Case, Acc) ->
    token(Rest, Fun, Case, << Acc/binary, C >>).

%% @doc Parse a non-empty list of the given type.
-spec nonempty_list(binary(), fun((_, _) -> any())) ->
    [any(), ...] | {error, badarg}.
nonempty_list(Data, Fun) ->
    case list(Data, Fun, []) of
        {error, badarg} -> {error, badarg};
        [] -> {error, badarg};
        L -> lists:reverse(L)
    end.

-spec list(binary(), fun(), [binary()]) -> [any()] | {error, badarg}.
%% From the RFC:
%% <blockquote>Wherever this construct is used, null elements are allowed,
%% but do not contribute to the count of elements present.
%% That is, "(element), , (element) " is permitted, but counts
%% as only two elements. Therefore, where at least one element is required,
%% at least one non-null element MUST be present.</blockquote>
list(Data, Fun, Acc) ->
    whitespace(Data,
        fun (<<>>) -> Acc;
            (<< $,, Rest/binary >>) -> list(Rest, Fun, Acc);
            (Rest) -> Fun(Rest,
                fun (D, I) -> whitespace(D,
                        fun (<<>>) -> [I|Acc];
                            (<< $,, R/binary >>) -> list(R, Fun, [I|Acc]);
                            (_Any) -> {error, badarg}
                        end)
                end)
        end).

%% @doc Skip whitespace.
-spec whitespace(binary(), fun()) -> any().
whitespace(<< C, Rest/binary >>, Fun)
        when C =:= $\s; C =:= $\t ->
    whitespace(Rest, Fun);
whitespace(Data, Fun) ->
    Fun(Data).

%% Convert [A-Z] characters to lowercase.
%%
%% We gain noticeable speed by matching each value directly.
%-spec char_to_lower(char()) -> char().
%char_to_lower($A) -> $a;
%char_to_lower($B) -> $b;
%char_to_lower($C) -> $c;
%char_to_lower($D) -> $d;
%char_to_lower($E) -> $e;
%char_to_lower($F) -> $f;
%char_to_lower($G) -> $g;
%char_to_lower($H) -> $h;
%char_to_lower($I) -> $i;
%char_to_lower($J) -> $j;
%char_to_lower($K) -> $k;
%char_to_lower($L) -> $l;
%char_to_lower($M) -> $m;
%char_to_lower($N) -> $n;
%char_to_lower($O) -> $o;
%char_to_lower($P) -> $p;
%char_to_lower($Q) -> $q;
%char_to_lower($R) -> $r;
%char_to_lower($S) -> $s;
%char_to_lower($T) -> $t;
%char_to_lower($U) -> $u;
%char_to_lower($V) -> $v;
%char_to_lower($W) -> $w;
%char_to_lower($X) -> $x;
%char_to_lower($Y) -> $y;
%char_to_lower($Z) -> $z;
%char_to_lower(Ch) -> Ch.

%% Tests.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-include("cloudi_service_oauth1_test.hrl").

module_test_() ->
    {timeout, ?TEST_TIMEOUT, [
        {"oauth tests", t_oauth()}
    ]}.

t_oauth() ->
    %% {Authorization, Result}
    Tests = [
        % from http://tools.ietf.org/html/rfc5849#section-1.2
        {<<"realm=\"Photos\", "
           "oauth_consumer_key=\"dpf43f3p2l4k3l03\", "
           "oauth_signature_method=\"HMAC-SHA1\", "
           "oauth_timestamp=\"137131200\", "
           "oauth_nonce=\"wIjqoS\", "
           "oauth_callback=\"http%3A%2F%2Fprinter.example.com%2Fready\", "
           "oauth_signature=\"74KNZJeDHnMBp0EMJ9ZHt%2FXKycU%3D\"">>,
         [{"realm", "Photos"},
          {"oauth_consumer_key", "dpf43f3p2l4k3l03"},
          {"oauth_signature_method", "HMAC-SHA1"},
          {"oauth_timestamp", "137131200"},
          {"oauth_nonce", "wIjqoS"},
          {"oauth_callback", "http://printer.example.com/ready"},
          {"oauth_signature", "74KNZJeDHnMBp0EMJ9ZHt/XKycU="}]}
    ],
    [{A, ?_assertEqual(R, authorization(A))} || {A, R} <- Tests].

-endif.

