%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% Test the dict API functions on an ordered collection.
%%% @end
%%%
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2012 Michael Truog
%%%------------------------------------------------------------------------

-module(proper_srv).

-behaviour(gen_server).

%% external interface
-export([start_link/1,
         stop/0]).

%% data structure API
-export([append/2,
         append_list/2,
         erase/1,
         fetch_keys/0,
         filter/1,
         find/1,
         fold/2,
         is_key/1,
         map/1,
         size/0,
         store/2,
         update/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,
    {
        module,
        data_structure
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

start_link(Module) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Module], []).

stop() ->
    call(stop, undefined).

%%%------------------------------------------------------------------------
%%% Data structure API
%%%------------------------------------------------------------------------

append(Key, Value) ->
    call(append, [Key, Value]).

append_list(Key, L) ->
    call(append_list, [Key, L]).

erase(Key) ->
    call(erase, [Key]).

fetch_keys() ->
    call(fetch_keys, []).

filter(F) ->
    call(filter, [F]).

find(Key) ->
    call(find, [Key]).

fold(F, Acc) ->
    call(fold, [F, Acc]).

is_key(Key) ->
    call(is_key, [Key]).

map(F) ->
    call(map, [F]).

size() ->
    call(size, []).

store(Key, L) ->
    call(store, [Key, L]).

update(Key, F, L) ->
    call(update, [Key, F, L]).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([Module]) ->
    {ok, #state{module = Module,
                data_structure = Module:new()}}.

handle_call({stop, undefined}, _F, S) ->
    {stop, normal, ok, S};
handle_call({F, A}, _,
            #state{data_structure = D,
                   module = M} = S) when F =:= filter ->
    Result = erlang:apply(M, F, A ++ [D]),
    {reply, M:to_list(Result), S};
handle_call({F, A}, _,
            #state{data_structure = D,
                   module = M} = S) when F =:= fetch_keys;
                                         F =:= find;
                                         F =:= fold;
                                         F =:= is_key;
                                         F =:= size ->
    Result = erlang:apply(M, F, A ++ [D]),
    {reply, Result, S};
handle_call({F, A}, _,
            #state{data_structure = D,
                   module = M} = S) ->
    Result = M:to_list(D),
    NewD = erlang:apply(M, F, A ++ [D]),
    {reply, Result, S#state{data_structure = NewD}};
handle_call(_, _, State) ->
    {stop, invalid_call, error, State}.

handle_cast(_Msg, State) ->
    {stop, invalid_cast, State}.

handle_info(_Info, State) ->
    {stop, invalid_info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

call(Command, Args) ->
    gen_server:call(?MODULE, {Command, Args}, infinity).

