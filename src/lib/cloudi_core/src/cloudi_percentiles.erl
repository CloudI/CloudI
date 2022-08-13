%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Percentiles==
%%% Calculate percentiles with all samples stored using definition 8
%%% (proposed as a standard sample quantile definition by Hyndman and Fan).
%%%
%%% The 50% percentile is the median.
%%%
%%% Rob J. Hyndman, Yanan Fan.  Sample Quantiles in Statistical Packages.
%%% American Statistician, vol. 50, no. 4, pp. 361–365,
%%% American Statistical Association, 1996-11.
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2022 Michael Truog <mjtruog at protonmail dot com>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a
%%% copy of this software and associated documentation files (the "Software"),
%%% to deal in the Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%% and/or sell copies of the Software, and to permit persons to whom the
%%% Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%%% DEALINGS IN THE SOFTWARE.
%%%
%%% @author Michael Truog <mjtruog at protonmail dot com>
%%% @copyright 2022 Michael Truog
%%% @version 2.0.5 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_percentiles).
-author('mjtruog at protonmail dot com').

%% external interface
-export([add/2,
         calculate/2,
         count/1,
         merge/2,
         new/0]).

-include("cloudi_core_i_constants.hrl").

-record(percentiles,
    {
        n = 0 :: non_neg_integer(),
        samples = [] :: list(float())
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-type state() :: #percentiles{}.
-export_type([state/0]).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add a sample for computing percentiles.===
%% @end
%%-------------------------------------------------------------------------

-spec add(X :: number(),
          State :: state()) ->
    state().

add(X, #percentiles{n = N,
                    samples = Samples} = State)
    when is_number(X) ->
    State#percentiles{n = N + 1,
                      samples = lists:merge(Samples, [float(X)])}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Calculate percentiles from the samples previously added.===
%% Percentiles must be provided in ascending order and be in the
%% range (0.0 .. 1.0).
%% @end
%%-------------------------------------------------------------------------

-spec calculate(Percentiles :: nonempty_list(float()),
                State :: state()) ->
    nonempty_list(float()) | undefined.

calculate(_,
          #percentiles{n = 0}) ->
    undefined;
calculate(Percentiles,
          #percentiles{n = N,
                       samples = Samples}) ->
    interpolate(Samples, indexes(Percentiles, N)).

%%-------------------------------------------------------------------------
%% @doc
%% ===Count of samples previously added.===
%% @end
%%-------------------------------------------------------------------------

-spec count(State :: state()) ->
    non_neg_integer().

count(#percentiles{n = N}) ->
    N.

%%-------------------------------------------------------------------------
%% @doc
%% ===Merge percentiles state.===
%% @end
%%-------------------------------------------------------------------------

-spec merge(StateA :: state(),
            StateB :: state()) ->
    state().

merge(#percentiles{} = StateA,
      #percentiles{n = 0}) ->
    StateA;
merge(#percentiles{n = 0},
      #percentiles{} = StateB) ->
    StateB;
merge(#percentiles{n = NA,
                   samples = SamplesA},
      #percentiles{n = NB,
                   samples = SamplesB}) ->
    #percentiles{n = NA + NB,
                 samples = lists:merge(SamplesA, SamplesB)}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Create percentiles state.===
%% @end
%%-------------------------------------------------------------------------

-spec new() ->
    state().

new() ->
    #percentiles{}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

indexes([Percentile0 | Percentiles], N)
    when is_float(Percentile0),
         Percentile0 > 0.0 andalso Percentile0 < 1.0 ->
    IndexFloat0 = Percentile0 * (N + 1 / 3) + (1 / 3),
    IndexJ0 = trunc(IndexFloat0),
    IndexG0 = IndexFloat0 - IndexJ0,
    case Percentiles of
        [] ->
            [{IndexJ0, IndexG0}];
        [Percentile1 | _]
            when Percentile0 < Percentile1 ->
            [{IndexJ0, IndexG0} | indexes(Percentiles, N)]
    end.

interpolate([Sample0 | _] = Samples, [{IndexJ, IndexG} | Indexes]) ->
    interpolate(Sample0, Samples, 0, IndexJ, IndexG, Indexes, []).

interpolate(Sample0, [Sample1 | _] = Samples,
            IndexJ, IndexJ, IndexG, Indexes, Results) ->
    Result0 = (1 - IndexG) * Sample0 + IndexG * Sample1,
    ResultsNew = [Result0 | Results],
    case Indexes of
        [] ->
            lists:reverse(ResultsNew);
        [{IndexJNew, IndexGNew} | IndexesNew] ->
            if
                IndexJ =:= IndexJNew ->
                    interpolate(Sample0, Samples,
                                IndexJ, IndexJ, IndexGNew, IndexesNew,
                                ResultsNew);
                IndexJ < IndexJNew ->
                    interpolate_incr(Samples,
                                     IndexJ, IndexJNew, IndexGNew, IndexesNew,
                                     ResultsNew)
            end
    end;
interpolate(_, Samples, I, IndexJ, IndexG, Indexes, Results) ->
    interpolate_incr(Samples, I, IndexJ, IndexG, Indexes, Results).

interpolate_incr([Sample0 | SamplesNew] = Samples,
                 I, IndexJ, IndexG, Indexes, Results) ->
    if
        SamplesNew == [] ->
            interpolate(Sample0, Samples,
                        I + 1, IndexJ, IndexG, Indexes, Results);
        true ->
            interpolate(Sample0, SamplesNew,
                        I + 1, IndexJ, IndexG, Indexes, Results)
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

-include("cloudi_core_i_test.hrl").

module_test_() ->
    {timeout, ?TEST_TIMEOUT, [
        {"basic tests", ?_assertOk(t_basic())}
    ]}.

t_basic() ->
    undefined = calculate([0.9], new()),
    % data from
    % NIST/SEMATECH e-Handbook of Statistical Methods,
    % https://www.itl.nist.gov/div898/handbook/prc/section2/prc262.htm
    % , 2022-08-12.
    StateA0 = new(),
    Minimum = 95.0610,
    Maximum = 95.1990,
    StateA1 = add(95.1772, StateA0),
    StateA2 = add(95.1567, StateA1),
    StateA3 = add(95.1937, StateA2),
    StateA4 = add(95.1959, StateA3),
    StateA5 = add(95.1442, StateA4),
    StateA6 = add(Minimum, StateA5),
    StateA7 = add(95.1591, StateA6),
    StateA8 = add(95.1195, StateA7),
    StateA9 = add(95.1065, StateA8),
    StateA10 = add(95.0925, StateA9),
    StateA11 = add(Maximum, StateA10),
    StateA12 = add(95.1682, StateA11),
    {'EXIT', {function_clause, _}} = catch calculate([], StateA12),
    [Minimum,
     Minimum,
     Minimum,
     Minimum,
     Percentile90,
     Maximum,
     Maximum,
     Maximum,
     Maximum] = calculate([0.00001,
                           0.0001,
                           0.001,
                           0.01,
                           0.9,
                           0.99,
                           0.999,
                           0.9999,
                           0.99999], StateA12),
    95.19724333333332 = Percentile90,
    [Percentile90] = calculate([0.9], StateA12),
    ok.

-endif.
