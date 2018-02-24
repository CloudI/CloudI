%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% 
%%% Copyright (C) 2010 Brian Buchanan. All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%% 1. Redistributions of source code must retain the above copyright
%%%    notice, this list of conditions and the following disclaimer.
%%% 2. Redistributions in binary form must reproduce the above copyright
%%%    notice, this list of conditions and the following disclaimer in the
%%%    documentation and/or other materials provided with the distribution.
%%% 
%%% THIS SOFTWARE IS PROVIDED BY AUTHOR AND CONTRIBUTORS "AS IS" AND
%%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED.  IN NO EVENT SHALL AUTHOR OR CONTRIBUTORS BE LIABLE
%%% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
%%% OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
%%% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
%%% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
%%% SUCH DAMAGE.
%%%
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% From erlcloud, in erlcloud_http.erl
%%%------------------------------------------------------------------------

-module(nodefinder_ec2_api_http).
-export([make_query_string/1, value_to_string/1, url_encode/1]).

%% encode an empty value query string differently based on the
%% argument provided, this is based on the fact that S3 and SQS
%% sign url differently, S3 requires that empty arguments have no
%% '=' (/?acl) while SQS requires it (/?QueuePrefix=)
%% default behaviour is adding '='
make_query_string(Params) ->
  make_query_string(Params, empty_assignment).

make_query_string(Params, EmptyQueryOpt) ->
    lists:join($&,
               [encode_query_term(Key, Value, EmptyQueryOpt) ||
                {Key, Value} <- Params, Value =/= none, Value =/= undefined]).

%encode_query_term(Key, [], no_assignment) ->
%  [Key];
encode_query_term(Key, [], empty_assignment) ->
  [Key, "="];
encode_query_term(Key, Value, _) ->
  [Key, "=", url_encode(value_to_string(Value))].

value_to_string(Integer) when is_integer(Integer) -> integer_to_list(Integer);
value_to_string(Atom) when is_atom(Atom) -> atom_to_list(Atom);
value_to_string(Binary) when is_binary(Binary) -> Binary;
value_to_string(String) when is_list(String) -> unicode:characters_to_binary(String).

url_encode(Binary) when is_binary(Binary) ->
    url_encode(unicode:characters_to_list(Binary));
url_encode(String) ->
    url_encode(String, []).
url_encode([], Accum) ->
    lists:reverse(Accum);
url_encode([Char|String], Accum)
  when Char >= $A, Char =< $Z;
       Char >= $a, Char =< $z;
       Char >= $0, Char =< $9;
       Char =:= $-; Char =:= $_;
       Char =:= $.; Char =:= $~ ->
    url_encode(String, [Char|Accum]);
url_encode([Char|String], Accum) ->
    url_encode(String, utf8_encode_char(Char) ++ Accum).

utf8_encode_char(Char) when Char > 16#7FFF, Char =< 16#7FFFF ->
    encode_char(Char band 16#3F + 16#80)
      ++ encode_char((16#3F band (Char bsr 6)) + 16#80)
      ++ encode_char((16#3F band (Char bsr 12)) + 16#80)
      ++ encode_char((Char bsr 18) + 16#F0);
utf8_encode_char(Char) when Char > 16#7FF, Char =< 16#7FFF ->
    encode_char(Char band 16#3F + 16#80)
      ++ encode_char((16#3F band (Char bsr 6)) + 16#80)
      ++ encode_char((Char bsr 12) + 16#E0);
utf8_encode_char(Char) when Char > 16#7F, Char =< 16#7FF ->
    encode_char(Char band 16#3F + 16#80)
      ++ encode_char((Char bsr 6) + 16#C0);
utf8_encode_char(Char) when Char =< 16#7F ->
  encode_char(Char).

encode_char(Char) ->
  [hex_char(Char rem 16), hex_char(Char div 16), $%].

hex_char(C) when C < 10 -> $0 + C;
hex_char(C) when C < 16 -> $A + C - 10.
