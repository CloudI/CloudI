%% Copyright (c) 2008 Robert Virding. All rights reserved.
%%
%% Redistribution and use in source and binary forms, with or without
%% modification, are permitted provided that the following conditions
%% are met:
%%
%% 1. Redistributions of source code must retain the above copyright
%%    notice, this list of conditions and the following disclaimer.
%% 2. Redistributions in binary form must reproduce the above copyright
%%    notice, this list of conditions and the following disclaimer in the
%%    documentation and/or other materials provided with the distribution.
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
%% FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
%% COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
%% INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
%% LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
%% CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
%% ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
%% POSSIBILITY OF SUCH DAMAGE.

-module(rbsets).

%% Standard interface.
-export([new/0,is_set/1,size/1,to_list/1,from_list/1]).
-export([is_element/2,add_element/2,del_element/2]).
-export([union/2,union/1,intersection/2,intersection/1]).
-export([subtract/2,is_subset/2]).
-export([fold/3,filter/2]).

%% Extended interface.
-export([all/2,any/2,foreach/2,partition/2,iter/3,itera/3]).

%% Deprecated interface.

-export([new_set/0,set_to_list/1,list_to_set/1,subset/2]).
-deprecated([{new_set,0},{set_to_list,1},{list_to_set,1},{subset,2}]).

-ifdef(DEBUG).
-export([check/1,erase_check/2,t/1,r1/0,r2/0]).
-endif.

-include("rbsets.hrl").

%% The algorithms here are taken directly from Okasaki and Rbset in
%% ML/Scheme. The interface is compatible with the standard dict
%% interface.
%%
%% The following structures are used to build the the RB-set:
%%
%% {r,Left,Element,Right}
%% {b,Left,Element,Right}
%% empty
%%
%% It is interesting to note that expanding out the first argument of
%% l/rbalance, the colour, in store etc. is actually slower than not
%% doing it. Measured.

%% new() -> Set.

-spec new() -> rbsets().

new() -> empty.

%% is_set(Set) -> bool().
%%  Return 'true' if Set is a set of elements, else 'false'.

-spec is_set(rbsets()) -> bool().

is_set({r,Left,_,Right}) ->
    is_set(Left) andalso is_set(Right);
is_set({b,Left,_,Right}) ->
    is_set(Left) andalso is_set(Right);
is_set(empty) -> true.

%% size(Set) -> int().

-spec size(S :: rbsets()) -> non_neg_integer().

size(S) -> size1(S).

size1({r,Left,_,Right}) ->
    size1(Left) + 1 + size1(Right);
size1({b,Left,_,Right}) ->
    size1(Left) + 1 + size1(Right);
size1(empty) -> 0.

%% to_list(Set) -> [Element].

-spec to_list(T :: rbsets()) -> list().

to_list(T) -> to_list(T, []).

to_list(empty, List) -> List;
to_list({_,A,X,B}, List) ->
    to_list(A, [X|to_list(B, List)]).

%% from_list([Element]) -> Set.

-spec from_list(L :: list()) -> rbsets().

from_list(L) ->
    lists:foldl(fun (E, S) -> add_element(E, S) end, new(), L).

%% is_element(Element, Set) -> true | false.

-spec is_element(any(), rbsets()) -> bool().

is_element(_, empty) -> false;
is_element(X, {_,A,Y,_}) when X < Y ->
    is_element(X, A);
is_element(X, {_,_,Y,B}) when X > Y ->
    is_element(X, B);
is_element(_, {_,_,_,_}) -> true.

%% add_element(Element, Set) -> Set.

-spec add_element(E :: any(), T :: rbsets()) -> rbsets().

add_element(E, T) ->
    {_,L,E1,R} = add_element1(E, T),
    {b,L,E1,R}.					%setelement(1, b, T1).

add_element1(X, empty) -> {r,empty,X,empty};
add_element1(X, {C,A,Y,B}) when X < Y ->
    lbalance(C, add_element1(X, A), Y, B);
add_element1(X, {C,A,Y,B}) when X > Y ->
    rbalance(C, A, Y, add_element1(X, B));
add_element1(_, {_,_,_,_}=T) -> T.

%% Expanding out l/rbalance is slower!
%% add_element1(X, empty) -> {r,empty,X,empty};
%% add_element1(X, {r,Left,Y,Right}) ->
%%     if X < Y -> {r,add_element1(X, Left),Y,Right};
%%        X > Y -> {r,Left,Y,add_element1(X, Right)};
%%        true -> {r,Left,X,Right}
%%     end;
%% add_element1(X, {b,Left,Y,Right}) ->
%%     if X < Y ->
%% 	    lbalance(add_element1(X, Left), Y, Right);
%%        X > Y ->
%% 	    rbalance(Left, Y, add_element1(X, Right));
%%        true -> {b,Left,X,Right}
%%     end.

%% lbalance(Colour, Left, Key, Val, Right).
%% rbalance(Colour, Left, Key, Val, Right).
%% Balance a tree afer (possibly) adding a node to the left/right.

lbalance(b, {r,{r,A,X,B},Y,C}, Z, D) ->
    {r,{b,A,X,B},Y,{b,C,Z,D}};
lbalance(b, {r,A,X,{r,B,Y,C}}, Z, D) ->
    {r,{b,A,X,B},Y,{b,C,Z,D}};
lbalance(C, A, X, B) -> {C,A,X,B}.

rbalance(b, A, X, {r,{r,B,Y,C},Z,D}) ->
    {r,{b,A,X,B},Y,{b,C,Z,D}};
rbalance(b, A, X, {r,B,Y,{r,C,Z,D}}) ->
    {r,{b,A,X,B},Y,{b,C,Z,D}};
rbalance(C, A, X, B) -> {C,A,X,B}.

%% del_element(Element, Set) -> Set.

-spec del_element(K :: any(), T :: rbsets()) -> rbsets().

del_element(K, T) ->
    {T1,_} = del_aux(K, T),
    T1.

%% del_aux(Key, Node) -> {Node,Decreased}.

del_aux(_, empty) -> {empty,false};
del_aux(K, {b,A,X,B}) ->
    if K < X ->
	    {A1,Dec} = del_aux(K, A),
	    if  Dec -> unbalright(b, A1, X, B);
		true -> {{b,A1,X,B},false}
	    end;
       K > X ->
	    {B1,Dec} = del_aux(K, B),
	    if  Dec -> unballeft(b, A, X, B1);
		true -> {{b,A,X,B1},false}
	    end;
       true ->
	    case B of
		empty -> blackify(A);
		_ ->
		    {B1,M,Dec} = del_min(B),
		    if  Dec -> unballeft(b, A, M, B1);
			true -> {{b,A,M,B1},false}
		    end
	    end
    end;
del_aux(K, {r,A,X,B}) ->
    if K < X ->
	    {A1,Dec} = del_aux(K, A),
	    if  Dec -> unbalright(r, A1, X, B);
		true -> {{r,A1,X,B},false}
	    end;
       K > X ->
	    {B1,Dec} = del_aux(K, B),
	    if  Dec -> unballeft(r, A, X, B1);
		true -> {{r,A,X,B1},false}
	    end;
       true ->
	    case B of
		empty -> {A,false};
		_ ->
		    {B1,M,Dec} = del_min(B),
		    if  Dec -> unballeft(r, A, M, B1);
			true -> {{r,A,M,B1},false}
		    end
	    end
    end.

%% del_min(Node) -> {Node,{NodeKey,NodeVal},Decreased}.

del_min({b,empty,X,empty}) ->
    {empty,X,true};
del_min({b,empty,X,{r,A,Y,B}}) ->
    {{b,A,Y,B},X,false};
del_min({b,empty,_,{b,_,_,_}}) -> exit(boom);
del_min({r,empty,X,A}) ->
    {A,X,false};
%% Rec from left
del_min({b,A,X,B}) ->
    {A1,Min,Dec} = del_min(A),
    if Dec ->
	    {T,Dec1} = unbalright(b, A1, X, B),
	    {T,Min,Dec1};
       true -> {{b,A1,X,B},Min,false}
    end;
del_min({r,A,X,B}) ->
    {A1,Min,Dec} = del_min(A),
    if Dec ->
	    {T,Dec1} = unbalright(r, A1, X, B),
	    {T,Min,Dec1};
       true -> {{r,A1,X,B},Min,false}
    end.

blackify({r,A,E,B}) -> {{b,A,E,B},false};
blackify(Node) -> {Node,true}.

unballeft(r, {b,A,X,B}, Y, C) ->
    {lbalance(b, {r,A,X,B}, Y, C),false};
unballeft(b, {b,A,X,B}, Y, C) ->
    {lbalance(b, {r,A,X,B}, Y, C),true};
unballeft(b, {r,A,X,{b,B,Y,C}}, Z, D) ->
    {{b,A,X,lbalance(b, {r,B,Y,C}, Z, D)},false}.

unbalright(r, A, X, {b,B,Y,C}) ->
    {rbalance(b, A, X, {r,B,Y,C}),false};
unbalright(b, A, X, {b,B,Y,C}) ->
    {rbalance(b, A, X, {r,B,Y,C}),true};
unbalright(b, A, X, {r,{b,B,Y,C},Z,D}) ->
    {{b,rbalance(b, A, X, {r,B,Y,C}), Z, D},false}.

%% union(Set1, Set2) -> Set.
%%  Return the union of Set1 and Set2.

-spec union(S1 :: rbsets(), S2 :: rbsets()) -> rbsets().

union(S1, S2) ->
    fold(fun (E, S) -> add_element(E, S) end, S1, S2).

%% union([Set]) -> Set.
%%  Return the union of the list of sets.

union([S1,S2|Ss]) ->
    union([union(S1, S2)|Ss]);
union([S]) -> S;
union([]) -> new().

%% intersection(Set1, Set2) -> Set.
%%  Return the intersection of Set1 and Set2.

-spec intersection(S1 :: rbsets(), S2 :: rbsets()) -> rbsets().

intersection(S1, S2) ->
    filter(fun (E) -> is_element(E, S2) end, S1).

%% intersection([Set]) -> Set.
%%  Return the intersection of the list of sets.

-spec intersection(list(rbsets())) -> rbsets().

intersection([S1,S2|Ss]) ->
    intersection([intersection(S1, S2)|Ss]);
intersection([S]) -> S.

%% subtract(Set1, Set2) -> Set.
%%  Return all and only the elements of Set1 which are not also in
%%  Set2.

-spec subtract(S1 :: rbsets(), S2 :: rbsets()) -> rbsets().

subtract(S1, S2) ->
    filter(fun (E) -> not is_element(E, S2) end, S1).

%% is_subset(Set1, Set2) -> bool().
%%  Return 'true' when every element of Set1 is also a member of
%%  Set2, else 'false'.

-spec is_subset(S1 :: rbsets(), S2 :: rbsets()) -> bool().

is_subset(S1, S2) ->
    all(fun (E) -> is_element(E, S2) end, S1).

%% fold(Fun, Acc, Set) -> Acc.

-spec fold(fun((any(), any()) -> any()), Acc :: any(), rbsets()) -> any().

fold(_, Acc, empty) -> Acc;
fold(F, Acc, {_,A,E,B}) ->
    fold(F, F(E, fold(F, Acc, B)), A).

%% filter(Pred, Set) -> Set.
%%  Filter Set with Pred.

-spec filter(P :: fun((any()) -> bool()), T :: rbsets()) -> rbsets().

filter(P, T) -> filter(P, T, new()).

filter(_, empty, New) -> New;
filter(P, {_,A,X,B}, New0) ->
    New1 = filter(P, A, New0),
    New2 = case P(X) of
	       true -> add_element(X, New1);
	       false -> New1
    end,
    filter(P, B, New2).

%% all(Pred, Set) -> bool().
%%  Return 'true' when Pred(Elem) is true for all elements, else 'false'.

-spec all(Pred :: fun((any()) -> bool()), rbsets()) -> bool().

all(Pred, empty) when is_function(Pred, 2) -> true;
all(Pred, {_,A,X,B}) ->
    all(Pred(X), A, B, Pred).
all(false, _, _, _) ->
    false;
all(true, empty, B, F) ->
    all(true, B, F);
all(true, {_,A,X,B1}, B0, F) ->
    all(all(F(X), A, B1, F), B0, F).
all(false, _, _) ->
    false;
all(true, empty, _) ->
    true;
all(true, {_,A,X,B}, F) ->
    all(F(X), A, B, F).

%% any(Pred, Set) -> bool().
%%  Return 'true' when Pred(Elem) is true for any element, else 'false'.

-spec any(Pred :: fun((any()) -> bool()), rbsets()) -> bool().

any(Pred, empty) when is_function(Pred, 2) -> false;
any(Pred, {_,A,X,B}) ->
    any(Pred(X), A, B, Pred).
any(true, _, _, _) ->
    true;
any(false, empty, B, F) ->
    any(false, B, F);
any(false, {_,A,X,B1}, B0, F) ->
    any(any(F(X), A, B1, F), B0, F).
any(true, _, _) ->
    true;
any(false, empty, _) ->
    false;
any(false, {_,A,X,B}, F) ->
    any(F(X), A, B, F).

%% foreach(Fun, Set) -> ok.
%%  Apply Fun to each element in Set.

-spec foreach(fun((any()) -> any()), rbsets()) -> 'ok'.

foreach(_, empty) -> ok;
foreach(F, {_,A,X,B}) ->
    foreach(F, A),
    F(X),
    foreach(F, B).

%% partition(Pred, Set) -> {Set1,Set2}.
%%  Partition Set so Set1 contains all elements for which Pred(E) is true.

-spec partition(P :: fun((any()) -> any()),
                S :: rbsets()) -> {rbsets(), rbsets()}.

partition(P, S) -> partition(P, S, new(), new()).

partition(_, empty, T, F) -> {T,F};
partition(P, {_,A,X,B}, T, F) ->
    {T1,F1} = partition(P, A, T, F),
    case P(X) of
	true -> partition(P, B, add_element(X, T1), F1);
	false -> partition(P, B, T1, add_element(X, F1))
    end.

%% iter(Fun, Default, Set) -> any().

-spec iter(F :: fun((any(), fun(() -> any())) -> any()),
           D :: any(),
           rbsets()) -> any().

iter(_, D, empty) ->
    D;
iter(F, D, {_,empty,X,empty}) ->
    F(X, fun() -> D end);
iter(F, D, {_,A,X,empty}) ->
    F(X, fun() -> iter(F, D, A) end);
iter(F, D, {_,empty,X,B}) ->
    F(X, fun() -> iter(F, D, B) end);
iter(F, D, {_,A,X,B}) ->
    F(X, fun() ->
        iter(F, D, fun() -> iter(F, D, B) end, A)
    end).
iter(F, _, I, {_,empty,X,empty}) ->
    F(X, I);
iter(F, D, I, {_,empty,X,B}) ->
    F(X, fun() -> iter(F, D, I, B) end);
iter(F, D, I, {_,A,X,empty}) ->
    F(X, fun() -> iter(F, D, I, A) end);
iter(F, D, I, {_,A,X,B}) ->
    F(X, fun() ->
        iter(F, D, fun() -> iter(F, D, I, B) end, A)
    end).

%% itera(Fun, Acc, Set) -> any().

-spec itera(F :: fun((any(), any(), fun((any()) -> any())) -> any()),
            Acc :: any(),
            rbsets()) -> any().

itera(_, Acc, empty) ->
    Acc;
itera(F, Acc, {_,empty,X,empty}) ->
    F(X, Acc, fun(V) -> V end);
itera(F, Acc, {_,A,X,empty}) ->
    F(X, Acc, fun(V) -> itera(F, V, A) end);
itera(F, Acc, {_,empty,X,B}) ->
    F(X, Acc, fun(V) -> itera(F, V, B) end);
itera(F, Acc, {_,A,X,B}) ->
    F(X, Acc, fun(V1) ->
        itera(F, V1, fun(V2) -> itera(F, V2, B) end, A)
    end).
itera(F, Acc, I, {_,empty,X,empty}) ->
    F(X, Acc, I);
itera(F, Acc, I, {_,empty,X,B}) ->
    F(X, Acc, fun(V) -> itera(F, V, I, B) end);
itera(F, Acc, I, {_,A,X,empty}) ->
    F(X, Acc, fun(V) -> itera(F, V, I, A) end);
itera(F, Acc, I, {_,A,X,B}) ->
    F(X, Acc, fun(V1) ->
        itera(F, V1, fun(V2) -> itera(F, V2, I, B) end, A)
    end).

%% fold(fun (X, {T,F}) ->
%% 	     case P(X) of
%% 		 true -> {add_element(X, T),F};
%% 		 false -> {T,add_element(X, F)}
%% 	     end
%%      end, {new(),new()}, S).

%% Deprecated interface.

new_set() -> new().

set_to_list(S) -> to_list(S).

list_to_set(L) -> from_list(L).

subset(S1, S2) -> is_subset(S1, S2).

-ifdef(DEBUG).
%% Test functions.

erase_check(K, T) ->
    T1 = erase(K, T),
    check(T1),
    T1.

check(T) -> check(T, r).

check(empty, _) -> 1;
check({r,A,X,B}, b) ->			       %Must have black parent
    case {check(A, r),check(B, r)} of
	{D,D}-> D;
	{Dl,Dr} -> exit({depth,{r,Dl,X,Dr}})
    end;
check({r,_,X,_}, r) ->			       %Must have black parent
    exit({parent,{r,'-',X,'-'}});
check({b,A,X,B}, _) ->
    case {check(A, b),check(B,b)} of
	{D,D}-> D+1;				%Increase depth
	{Dl,Dr} -> exit({depth,{b,Dl,X,Dr}})
    end.

t(Ks) -> t(Ks, new()).

t([K|Ks], D0) ->
    D1 = store(K, K, D0),
    t(Ks, D1);
t([], D) -> D.

%% Known error cases which have been fixed.

r1() ->
    {{b,{b,empty,37,empty},
       38,
       {b,{r,empty,39,empty},40,empty}},
     39,
     {b,{r,empty,37,empty},38,{b,empty,40,empty}}}.

r2() ->
    {{b,{r,{b,empty,43,empty},
	   46,
	   {b,empty,48,empty}},
	50,
	{b,empty,53,empty}},
     53,
     {b,{b,empty,43,empty},
	46,
	{r,{b,empty,48,empty},50,empty}}}.
-endif.
