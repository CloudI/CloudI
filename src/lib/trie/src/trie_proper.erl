%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:

-module(trie_proper).
-ifdef(TEST).
-behaviour(proper_statem).
-include_lib("proper/include/proper.hrl").

%% external interface
-export([qc_run/1, correct/1]).

%% proper_statem callbacks
-export([command/1, initial_state/0, next_state/3,
         postcondition/3, precondition/2]).

-record(state,
    {
        dict
    }).
-define(SERVER, proper_srv).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

qc_opts() ->
    [{numtests, 10000}].
     
qc_run(M) when is_atom(M) ->
    proper:quickcheck(trie_proper:correct(M), qc_opts()).

%%%------------------------------------------------------------------------
%%% Callback functions from proper_statem
%%%------------------------------------------------------------------------

initial_state() ->
    #state{dict = dict:new()}.

% dict functions not tested:
% fetch/2 from_list/1 map/2 update/3 update_counter/3
command(#state{}) ->
    oneof([{call, ?SERVER, append, [key(), value()]},
           {call, ?SERVER, append_list, [key(), [value(), value()]]},
           {call, ?SERVER, erase, [key()]},
           {call, ?SERVER, fetch_keys, []},
           {call, ?SERVER, filter, [fun(K, _) -> K > key() end]},
           {call, ?SERVER, find, [key()]},
           {call, ?SERVER, fold, [fun(_, V, S) -> lists:sum(V) + S end, 0]},
           {call, ?SERVER, is_key, [key()]},
           {call, ?SERVER, map, [fun(_, V) ->
                                     lists:map(fun(I) -> I + 1 end, V)
                                 end]},
           {call, ?SERVER, size, []},
           {call, ?SERVER, store, [key(), [value()]]},
           {call, ?SERVER, update, [key(),
                                    fun(V) ->
                                        lists:map(fun(I) -> I + 1 end, V)
                                    end,
                                    [value()]]}]).

next_state(#state{dict = D} = S, _V, {call, _, append, [Key, Value]}) ->
    S#state{dict = dict:append(Key, Value, D)};
next_state(#state{dict = D} = S, _V, {call, _, append_list, [Key, L]}) ->
    S#state{dict = dict:append_list(Key, L, D)};
next_state(#state{dict = D} = S, _V, {call, _, erase, [Key]}) ->
    S#state{dict = dict:erase(Key, D)};
next_state(S, _V, {call, _, fetch_keys, _}) ->
    S;
next_state(S, _V, {call, _, filter, [_F]}) ->
    S;
next_state(S, _V, {call, _, find, [_Key]}) ->
    S;
next_state(S, _V, {call, _, fold, [_F, _Acc]}) ->
    S;
next_state(S, _V, {call, _, is_key, [_Key]}) ->
    S;
next_state(#state{dict = D} = S, _V, {call, _, map, [F]}) ->
    S#state{dict = dict:map(F, D)};
next_state(S, _V, {call, _, size, _}) ->
    S;
next_state(#state{dict = D} = S, _V, {call, _, store, [Key, L]}) ->
    S#state{dict = dict:store(Key, L, D)};
next_state(#state{dict = D} = S, _V, {call, _, update, [Key, F, L]}) ->
    S#state{dict = dict:update(Key, F, L, D)}.

precondition(_S, _Call) ->
    true. % No limitation on the things we can call at all.

postcondition(#state{dict = D}, {call, _, filter, [F]}, R) ->
    R == lists:keysort(1, dict:to_list(dict:filter(F, D)));
postcondition(#state{dict = D}, {call, _, fetch_keys, []}, R) ->
    R == lists:sort(dict:fetch_keys(D));
postcondition(#state{dict = D}, {call, _, find, [Key]}, R) ->
    R == dict:find(Key, D);
postcondition(#state{dict = D}, {call, _, fold, [F, Acc]}, R) ->
    R == dict:fold(F, Acc, D);
postcondition(#state{dict = D}, {call, _, is_key, [Key]}, R) ->
    R == dict:is_key(Key, D);
postcondition(#state{dict = D}, {call, _, size, []}, R) ->
    R == dict:size(D);
postcondition(#state{dict = D}, {call, _, _F, _A}, R) ->
    R == lists:keysort(1, dict:to_list(D));
postcondition(_, _, _) ->
    false.

correct(M) ->
    ?FORALL(Cmds, commands(?MODULE),
            ?TRAPEXIT(
                begin
                    ?SERVER:start_link(M),
                    {History,State,Result} = run_commands(?MODULE, Cmds),
                    ?SERVER:stop(),
                    ?WHENFAIL(io:format("History: ~w\nState: ~w\nResult: ~w\n",
                                        [History, State, Result]),
                              aggregate(command_names(Cmds), Result =:= ok))
                end)).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

% random 8 character string
key() ->
    fixed_list([integer(48, 126),
                integer(48, 126),
                integer(48, 126),
                integer(48, 126),
                integer(48, 126),
                integer(48, 126),
                integer(48, 126),
                integer(48, 126)]).

value() ->
    integer().

-endif.
