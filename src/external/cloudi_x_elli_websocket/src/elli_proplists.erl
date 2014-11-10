%%
%% Case insensitive value retrieval from a proplist
%%

-module(elli_proplists).

-export([get_value_ci/2, 
    get_all_values_ci/2]).

%%
get_value_ci(_Key, []) ->
        undefined;
get_value_ci(Key, [{K, Value}|Rest]) ->
        case elli_bstr:is_equal_ci(Key, K) of
                true ->
                        Value;
                false ->
                        get_value_ci(Key, Rest)
        end.


%%
get_all_values_ci(Key, Proplist) ->
    get_all_values_ci1(Key, Proplist, []).


get_all_values_ci1(_Key, [], Acc) ->
        lists:reverse(Acc);
get_all_values_ci1(Key, [{K, Value}|Rest], Acc) ->
        case elli_bstr:is_equal_ci(Key, K) of
                true ->
                        get_all_values_ci1(Key, Rest, [Value|Acc]);
                false ->
                        get_all_values_ci1(Key, Rest, Acc)
        end.
