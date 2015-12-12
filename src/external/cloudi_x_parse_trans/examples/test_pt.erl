-module(test_pt).

-export([parse_transform/2]).

parse_transform(Forms, _Options) ->
    io:fwrite("Forms = ~p~n", [Forms]),
    Forms.

