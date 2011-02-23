%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Increment N-d indexes with a single index==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2010-2011, Michael Truog <mjtruog at gmail dot com>
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%% 
%%%     * Redistributions of source code must retain the above copyright
%%%       notice, this list of conditions and the following disclaimer.
%%%     * Redistributions in binary form must reproduce the above copyright
%%%       notice, this list of conditions and the following disclaimer in
%%%       the documentation and/or other materials provided with the
%%%       distribution.
%%%     * All advertising materials mentioning features or use of this
%%%       software must display the following acknowledgment:
%%%         This product includes software developed by Michael Truog
%%%     * The name of the author may not be used to endorse or promote
%%%       products derived from this software without specific prior
%%%       written permission
%%% 
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
%%% DAMAGE.
%%%
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2010-2011 Michael Truog
%%% @version 0.1.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(nd_index).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([new/2,
         new/3,
         set/2,
         count/1,
         clear/1,
         empty/1,
         value/1,
         min/1,
         max/1,
         increment/2,
         increment_seq/2]).

%% store state as tuples with the
%% least significant on the left and most significant on the right
-record(nd_index_state,
    {
        min,
        max,
        value
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a new N dimensional index, set to Min.===
%% @end
%%-------------------------------------------------------------------------

-spec new(Min :: tuple(),
          Max :: tuple()) -> #nd_index_state{}.

new(Min, Max) when tuple_size(Min) == tuple_size(Max), Min < Max ->
    #nd_index_state{min = Min, max = Max, value = Min}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Create a new N dimensional index, set to Value.===
%% @end
%%-------------------------------------------------------------------------

-spec new(Value :: tuple(),
          Min :: tuple(),
          Max :: tuple()) -> #nd_index_state{}.

new(Value, Min, Max)
    when tuple_size(Min) == tuple_size(Max), Min < Max,
         tuple_size(Min) == tuple_size(Value), Min =< Value, Value =< Max ->
    #nd_index_state{min = Min, max = Max, value = Value}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Set the current N dimensional index value.===
%% @end
%%-------------------------------------------------------------------------

-spec set(Value :: tuple(),
          State :: #nd_index_state{}) -> #nd_index_state{}.

set(Value, #nd_index_state{min = Min, max = Max} = State)
    when tuple_size(Min) == tuple_size(Value), Min =< Value, Value =< Max ->
    State#nd_index_state{value = Value}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Return a count of how many iterations remain.===
%% @end
%%-------------------------------------------------------------------------

-spec count(#nd_index_state{}) -> number().

count(#nd_index_state{max = Max, value = Value}) ->
    lists:foldr(fun(Index, Count) ->
        (erlang:element(Index, Max) - erlang:element(Index, Value) + 1) * Count
    end, 1, lists:seq(1, tuple_size(Value))) - 1.

%%-------------------------------------------------------------------------
%% @doc
%% ===Reset the current N dimensional index value to min.===
%% @end
%%-------------------------------------------------------------------------

-spec clear(State :: #nd_index_state{}) -> #nd_index_state{}.

clear(#nd_index_state{min = Min} = State) ->
    State#nd_index_state{value = Min}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Determine if no iterations remain.===
%% @end
%%-------------------------------------------------------------------------

-spec empty(#nd_index_state{}) -> bool().

empty(#nd_index_state{max = Max, value = Max}) ->
    true;

empty(_) ->
    false.

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the current N dimensional index value.===
%% @end
%%-------------------------------------------------------------------------

-spec value(#nd_index_state{}) -> tuple().

value(#nd_index_state{value = Value}) ->
    Value.

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the minimum N dimensional index value.===
%% @end
%%-------------------------------------------------------------------------

-spec min(#nd_index_state{}) -> tuple().

min(#nd_index_state{min = Min}) ->
    Min.

%%-------------------------------------------------------------------------
%% @doc
%% ===Return the maximum N dimensional index value.===
%% @end
%%-------------------------------------------------------------------------

-spec max(#nd_index_state{}) -> tuple().

max(#nd_index_state{max = Max}) ->
    Max.

%%-------------------------------------------------------------------------
%% @doc
%% ===Increment the N dimensional index value by a single count and return the new state.===
%% @end
%%-------------------------------------------------------------------------

-spec increment(Count :: non_neg_integer(),
                #nd_index_state{}) ->
    #nd_index_state{} |
    'false'.

increment(Count, #nd_index_state{min = Min, max = Max, value = Value})
    when is_integer(Count) ->
    increment(Count, 1, Value, Min, Max).
    
%%-------------------------------------------------------------------------
%% @doc
%% ===Increment the N dimensional index value by a single count and return the sequence result with the new state.===
%% all sequences returned are inclusive without overlap
%% @end
%%-------------------------------------------------------------------------

-spec increment_seq(Count :: non_neg_integer(),
                    #nd_index_state{}) ->
    {{tuple(), tuple()}, #nd_index_state{}} |
    'false'.

increment_seq(Count, #nd_index_state{min = Min, max = Max, value = Value0}) ->
    case increment(Count, 1, Value0, Min, Max) of
        false ->
            {{Value0, Max}, #nd_index_state{min = Min, max = Max, value = Max}};
        #nd_index_state{value = Max} = State ->
            {{Value0, Max}, State};
        #nd_index_state{value = Value1} ->
            case increment(1, 1, Value1, Min, Max) of
                false ->
                    % should have been caught above
                    false;
                State when is_record(State, nd_index_state) ->
                    {{Value0, Value1}, State}
            end
    end.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

increment(0, _, Value, Min, Max) ->
    #nd_index_state{min = Min, max = Max, value = Value};

increment(Count0, Index, Value, Min, Max) when Index =< tuple_size(Value) ->
    Min0 = erlang:element(Index, Min),
    Value0 = erlang:element(Index, Value),
    Span = erlang:element(Index, Max) - Min0 + 1,
    Value1 = ((Count0 + Value0 - Min0) rem Span) + Min0,
    Count1 = (Count0 + Value0 - Min0) div Span,
    increment(Count1, Index + 1,
              erlang:setelement(Index, Value, Value1), Min, Max);

increment(_, _, _, _, _) ->
    false.

