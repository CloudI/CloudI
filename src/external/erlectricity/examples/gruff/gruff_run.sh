#!/usr/bin/env escript

-export([main/1]).

main(_Any) ->
  Data = [
    {apples, [10, 2, 3, 4, 4, 3]},
    {oranges, [4, 8, 7, 9, 8, 9]},
    {watermelons, [2, 3, 1, 5, 6, 8]},
    {peaches, [9, 9, 10, 8, 7, 9]}
  ],
  gruff:start(),
  Result = gruff:plot(
    <<"My Charts">>,
    <<"/Users/scott/Library/Fonts/Arial">>,
    Data,
    [{0, <<"2003">>}, {2, <<"2004">>}, {4, <<"2005">>}]
  ),
  file:write_file("out.png", Result).