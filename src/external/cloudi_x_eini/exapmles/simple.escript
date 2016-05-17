#!/usr/bin/env escript
%% -*- erlang-acc -*-

-export([main/1]).

-include_lib("eunit/include/eunit.hrl").

main(_) ->
  code:add_patha("../ebin/"),
  go(""),
  go("\n"),
  go("\n  \n"),
  go("[title]"),
  go("[title]\nkey=value"),
  go("[title]\nkey=value [value]"),
  go("[title]\nkey=value ;value "),
  go("[title]\nkey=value ;value [value]"),
  go("[title]\nkey=value ;value [value]\n"
     "[second]  \nkey2=value2"),
  ok.

go(String) ->
  {ok, Tokens} = eini:lex(String),
  ?debugVal(Tokens),
  case eini:parse_tokens(Tokens) of
    {ok, Res} ->
      io:format("~p~n", [Res]);
    {error, {Line, Reason}} ->
      io:format("Error at line ~B: ~s~n", [Line, Reason])
  end,
  ok.
