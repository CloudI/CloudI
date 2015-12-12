-module(ex_gproc_send_xform).
-export([parse_transform/2]).


parse_transform(Forms, _Options) ->
    parse_trans:light_transform(fun do_transform/1, Forms).

do_transform({'op', L, '!', Lhs, Rhs}) ->
     [NewLhs] = parse_trans:light_transform(fun do_transform/1, [Lhs]),
     [NewRhs] = parse_trans:light_transform(fun do_transform/1, [Rhs]),
    {call, L, {remote, L, {atom, L, gproc}, {atom, L, send}},
     [NewLhs, NewRhs]};
do_transform(_) ->
    continue.

