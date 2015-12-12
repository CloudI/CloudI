-module(t).
-export([f/1]).

f(X) ->
    (fun(X1) ->
	     X1
     end)(X).
