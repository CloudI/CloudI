-module(lc).
-export([f/1]).

f(_X) ->
    [fun(_) ->
	     erlang:timestamp()
     end || {_Y1,_Y2} <- [{1,a},{2,b}]].
