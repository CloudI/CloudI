-module(t_ex).

-record(r, {a,b}).

-export(['sel-r'/1]).

-spec 'sel-r'(a | b) ->
		     {fun((#r{}) -> any()), fun((any(), #r{}) -> #r{})}.
'sel-r'(a) ->
    {fun(#r{a = A}) ->
	     A
     end,
     fun(X, #r{} = R) ->
	     R#r{a = X}
     end}.
