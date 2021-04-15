%%% -*- coding: utf-8 -*-
%%% -*- erlang-indent-level: 2 -*-
%%% -------------------------------------------------------------------
%%% Copyright 2010-2020 Manolis Papadakis <manopapad@gmail.com>,
%%%                     Eirini Arvaniti <eirinibob@gmail.com>
%%%                 and Kostis Sagonas <kostis@cs.ntua.gr>
%%%
%%% This file is part of PropEr.
%%%
%%% PropEr is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% PropEr is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with PropEr.  If not, see <http://www.gnu.org/licenses/>.

%%% @copyright 2010-2020 Manolis Papadakis, Eirini Arvaniti and Kostis Sagonas
%%% @version {@version}
%%% @author Eirini Arvaniti

-module(error_statem).
-export([command/1, initial_state/0, next_state/3,
	 precondition/2, postcondition/3, foo/1, bar/0]).

-include_lib("proper/include/proper.hrl").

-record(state, {step = 0 :: non_neg_integer()}).

initial_state() ->
    #state{}.

command(_S) ->
    oneof([{call,?MODULE,foo,[integer()]},
	   {call,?MODULE,bar,[]}]).

precondition(_, _) ->
    true.

next_state(#state{step = Step}, _, _) ->
    #state{step = Step+1}.

postcondition(_, _, _) ->
    true.

foo(I) ->
    case I > 10 of
	false -> ok;
	true  -> throw(badarg)
    end.

bar() -> 42.

prop_simple() ->
    ?FORALL(Cmds, commands(?MODULE),
	    begin
		{_H,_S,Res} = run_commands(?MODULE, Cmds),
		equals(Res, ok)
	    end).
