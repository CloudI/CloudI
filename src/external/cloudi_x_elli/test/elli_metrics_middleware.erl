-module(elli_metrics_middleware).
-export([init/2, preprocess/2, handle/2, postprocess/3, handle_event/3]).
-behaviour(elli_handler).


%%
%% ELLI
%%

init(_Req, _Args) ->
    ignore.

preprocess(Req, _Args) ->
    Req.

handle(_Req, _Args) ->
    ignore.

postprocess(_Req, Res, _Args) ->
    Res.


%%
%% ELLI EVENT CALLBACKS
%%

handle_event(request_complete, [_Req,_C,_Hs,_B, {Timings, Sizes}], _) ->
    ets:insert(elli_stat_table, {timings, Timings}),
    ets:insert(elli_stat_table, {sizes, Sizes});
handle_event(chunk_complete, [_Req,_C,_Hs,_B, {Timings, Sizes}], _) ->
    ets:insert(elli_stat_table, {timings, Timings}),
    ets:insert(elli_stat_table, {sizes, Sizes});
handle_event(_Event, _Data, _Args) ->
    ok.
