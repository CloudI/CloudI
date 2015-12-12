-module(lc).
-export([f/1]).

f(X) ->
    [fun(_) ->
	     erlang:now()
     end || {Y1,Y2} <- [{1,a},{2,b}]].
