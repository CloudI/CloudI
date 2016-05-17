#!/usr/bin/env escript
%% -*- erlang-acc -*-

-export([main/1]).

-include_lib("eunit/include/eunit.hrl").

main(_) ->
  code:add_patha("../ebin/"),
  String = "[cat1]\nkey=value\n",
%%  String = "[cat\t1]",
  go(String),
  ok.

go(String) ->
  case eini:lex(String) of
    {ok, Tokens} ->
      io:format("~p~n", [Tokens]),
      case eini:parse_tokens(Tokens) of
        {ok, Res} ->
          io:format("~p~n", [Res]);
        {error, {Line, Reason}} ->
          io:format("Parse error at line ~B: ~s~n", [Line, Reason])
      end;
    {error, {Line, Reason}} ->
      io:format("Lex error at line ~B: ~s~n", [Line, Reason])
  end.
