-module(prop_backoff).
-include_lib("proper/include/proper.hrl").

%% Increment operations are always returning bigger
%% and bigger values, assuming positive integers
prop_increment_increases() ->
    ?FORALL(X, pos_integer(),
        backoff:increment(X) > X).

%% increments should never go higher than the max
%% value allowed.
prop_increment_ceiled_increases() ->
    ?FORALL({X,Y}, backoff_range(),
        ?WHENFAIL(io:format("~p~n",[{X,Y,backoff:increment(X,Y)}]),
            backoff:increment(X,Y) =< Y
            andalso
            (backoff:increment(X,Y) > X
             orelse
             (backoff:increment(X,Y) =:= X andalso X =:= Y))
     )).

%% increments from an init value always go higher when unbound
prop_fail_increases() ->
    ?FORALL(S0, backoff_infinity(),
        begin
            X = backoff:get(S0),
            {Y, S1} = backoff:fail(S0),
            Y = backoff:get(S1),
            {Z, S2} = backoff:fail(S1),
            Z = backoff:get(S2),
            Y > X andalso Z > Y
        end).

%% increments with a max value keep growing until a fixed point
%% when failing
prop_fail_bounded_increase() ->
    ?FORALL(S0, backoff(),
        begin
            List = until_fixed_point(S0),
            [{X,_},{X,S1}|Rest] = lists:reverse(List),
            try
                lists:foldl(fun({N,_},Prev) when N > Prev -> N end,
                            0,
                            lists:reverse([{X,S1}|Rest])),
                true
            catch
                _:_ -> false
            end
        end).

%% Failing multiple times then succeeding brings the value back
%% to its initial one.
prop_succeed_reset() ->
    ?FORALL(S0, backoff(),
        begin
            X = backoff:get(S0),
            [{_,S1}|_] = lists:reverse(until_fixed_point(S0)),
            {Y, _} = backoff:succeed(S1),
            Y =:= X
        end).

%% Backoffs started with the right arguments can be used to
%% start timers.
prop_fire_message() ->
    ?FORALL({S,Term}, backoff_with_timer(50),
        begin
            Ref = backoff:fire(S),
            receive
                {timeout, Ref, Term} -> true
            after 100 ->
                false
            end
        end).

%% Helpers
until_fixed_point(S) -> until_fixed_point(S, backoff:get(S)).

until_fixed_point(S0, Prev) ->
    {Next, S1} = backoff:fail(S0),
    if Next =:= Prev -> [{Next,S1}];
       Next > Prev -> [{Next,S1} | until_fixed_point(S1, Next)]
    end.


%% Generators
backoff() ->
    ?LET({Start,Max}, backoff_range(), backoff:init(Start,Max)).

backoff_infinity() ->
    ?LET(Start, pos_integer(), backoff:init(Start,infinity)).

backoff_with_timer(N) ->
    ?LET({{Start,Max},T}, {bound_backoff_range(N),term()},
        {backoff:init(Start,Max,self(),T),T}).

backoff_range() ->
    ?SUCHTHAT({X,Y}, {pos_integer(), pos_integer()},
              X < Y).

bound_backoff_range(N) ->
    ?SUCHTHAT({X,Y}, {pos_integer(), pos_integer()},
              X < Y andalso Y < N).
