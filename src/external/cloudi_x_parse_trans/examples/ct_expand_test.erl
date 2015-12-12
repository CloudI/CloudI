-module(ct_expand_test).

-export([f/0]).

-compile({parse_transform, ct_expand}).
-pt_pp_src(true).

f() ->
    ct_expand:term(
      [{a, 1},
       {b, ct_expand:term(
             [{ba, 1},
              {bb, ct_expand:term(2)}])}]).

%% expand a term which calls a local function - even one which uses a fun reference.
g() ->
    ct_expand:term(zip([1,2], [a,b])).

h() ->
    ct_expand:term(wrap(my_fun())).

i() ->
    ct_expand:term(gb_trees:insert(a_fun, my_fun2(), gb_trees:empty())).

zip([H1|T1], [H2|T2]) ->
    F = my_fun2(),
    [{F(H1),F(H2)} | zip(T1, T2)];
zip([], []) ->
    [].

wrap(X) ->
    {X}.

my_fun() ->
    fun() -> foo end.

my_fun2() ->
    fun wrap/1.
