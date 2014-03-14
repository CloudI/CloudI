%%%-------------------------------------------------------------------
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright (C) 2013 Mahesh Paolini-Subramanya
%%% @doc type definitions and records.
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(erlang_cassandra_poolboy_worker).
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').

-behaviour(gen_server).
%-behaviour(poolboy_worker).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {worker :: pid()}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Args) ->
    process_flag(trap_exit, true),
    {ok, Worker} = erlang_cassandra:start_link(Args),
    {ok, #state{worker=Worker}}.

handle_call({F, A1, A2, A3, A4, A5, A6, A7, A8}, _From, State=#state{worker=Worker}) ->
    Reply = erlang_cassandra:F(Worker, A1, A2, A3, A4, A5, A6, A7, A8),
    {reply, Reply, State};
handle_call({F, A1, A2, A3, A4, A5, A6, A7}, _From, State=#state{worker=Worker}) ->
    Reply = erlang_cassandra:F(Worker, A1, A2, A3, A4, A5, A6, A7),
    {reply, Reply, State};
handle_call({F, A1, A2, A3, A4, A5, A6}, _From, State=#state{worker=Worker}) ->
    Reply = erlang_cassandra:F(Worker, A1, A2, A3, A4, A5, A6),
    {reply, Reply, State};
handle_call({F, A1, A2, A3, A4, A5}, _From, State=#state{worker=Worker}) ->
    Reply = erlang_cassandra:F(Worker, A1, A2, A3, A4, A5),
    {reply, Reply, State};
handle_call({F, A1, A2, A3, A4}, _From, State=#state{worker=Worker}) ->
    Reply = erlang_cassandra:F(Worker, A1, A2, A3, A4),
    {reply, Reply, State};
handle_call({F, A1, A2, A3}, _From, State=#state{worker=Worker}) ->
    Reply = erlang_cassandra:F(Worker, A1, A2, A3),
    {reply, Reply, State};
handle_call({F, A1, A2}, _From, State=#state{worker=Worker}) ->
    Reply = erlang_cassandra:F(Worker, A1, A2),
    {reply, Reply, State};
handle_call({F, A1}, _From, State=#state{worker=Worker}) ->
    Reply = erlang_cassandra:F(Worker, A1),
    {reply, Reply, State};
handle_call({F}, _From, State=#state{worker=Worker}) ->
    Reply = erlang_cassandra:F(Worker),
    {reply, Reply, State}.

handle_cast(_Msg, State) ->
    {stop, unhandled_cast, State}.

handle_info({'EXIT', _, shutdown}, State) ->
    {noreply, State};

handle_info(_Info, State) ->
    {stop, unhandled_info, State}.

terminate(_Reason, #state{worker=Worker}) ->
    ok = erlang_cassandra:stop(Worker),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
