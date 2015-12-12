-module(ex_pmod).

-compile({parse_transform, pmod}).

-pmod_vars(['A', 'B']).

-pmod_funs([a/1,
	    b/2]).

a(X) ->
    X.

b(X,Y) ->
    {X,Y,A,B}.
