%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved via the world wide web at http://www.erlang.org/.
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%-------------------------------------------------------------------
%%% File:      medici_native_conn.erl
%%% @author    Jim McCoy <mccoy@mad-scientist.com>
%%% @copyright Copyright (c) 2009, Jim McCoy.  All Rights Reserved.
%%%
%%% @doc
%%% The medici_native conn module handles a single principe connection to the 
%%% Tyrant remote database.  If is a simple gen_server that will dispatch requests
%%% to the remote database and exit if its connection to the remote database
%%% closes so that its supervisor can start another connection handler.  This
%%% module differs from the medici_conn module in that it will tranform returned
%%% data from binaries into Erlang terms before sending the reply to the caller.
%%% @end
-module(medici_native_conn).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("medici.hrl").

-record(state, {socket, mod, endian, controller = undefined}).

%% @spec start_link(SupervisorPid, Options) -> {ok,Pid} | ignore | {error,Error}
%% @private Starts the connection handler
start_link(SupervisorPid, Options) ->
    gen_server:start_link(?MODULE, [SupervisorPid, Options], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%% @spec init(Args) -> {ok, State} | {stop, Reason}
%% @private 
%% Initiates the server. Makes sure the remote database is in either
%% hash or b-tree mode.
%% @end
init([SupervisorPid, Options]) ->
    {ok, Sock} = principe:connect(Options),
    Timeout = case proplists:get_value(timeout, Options, 5000) of
        infinity ->
            5000;
        Value ->
            Value
    end,
    case get_db_type(Sock, Timeout) of
	{ok, _Endian, table} ->
	    {error, bad_tyrant_mode_for_native_storage};
	{ok, _Endian, fixed} ->
	    {error, bad_tyrant_mode_for_native_storage};
	{ok, Endian, _} ->
	    gen_server:cast(self(), {client_start, SupervisorPid}),
	    process_flag(trap_exit, true),
	    {ok, #state{socket=Sock, mod=principe, endian=Endian}};
	{error, _} ->
	    {stop, connect_failure}
    end.

%% @spec handle_call(Request, From, State) -> {stop, Reason, State}
%% @private 
%% Handle call messages. Since none are expected (all calls should come in
%% as casts) a call message will result in termination of the server.
%% @end
handle_call(Request, _From, State) ->
    ?DEBUG_LOG("Unknown call ~p~n", [Request]),
    {stop, {unknown_call, Request}, State}.

%% @spec handle_cast(Msg, State) -> {noreply, State} | {stop, Reason, State}
%% @private
%% Handle cast messages to forward to the remote database. This section 
%% differs from the regular medici_conn module in that calls which return 
%% data are given their own functions so that the data can be converted 
%% back to erlang terms prior to being sent back to the requestor.
%% @end
handle_cast({client_start, SupervisorPid}, State) ->
    case medici_sup:get_controller(SupervisorPid) of
        {ok, ControllerPid} ->
            ControllerPid ! {client_start, self()},
            {noreply, State#state{controller=ControllerPid}};
        {error, _} = Error ->
            {stop, Error, State}
    end;
handle_cast(stop, State) ->
    {stop, asked_to_stop, State};
handle_cast({From, tune, Timeout}, State) ->
    %% DB tuning request will come in via this channel, but is not just passed
    %% through to principe/tyrant.  Handle it here.
    Result = tune_db(State, Timeout),
    gen_server:reply(From, Result),
    {noreply, State};
handle_cast({From, iternext, Timeout}=Request, State) ->
    Module = State#state.mod,
    Result = Module:iternext(State#state.socket, Timeout),
    case Result of
	{error, conn_closed} ->
	    State#state.controller ! {retry, self(), Result, Request},
	    {stop, connection_error, State};
	{error, conn_error} ->
	    State#state.controller ! {retry, self(), Result, Request},
	    {stop, connection_error, State};
	{error, Reason} ->
	    gen_server:reply(From, {error, Reason}),
	    {noreply, State};
	_ ->
	    gen_server:reply(From, binary_to_term(Result)),
	    {noreply, State}
    end;
handle_cast({From, get, Key, Timeout}=Request, State) ->
    Module = State#state.mod,
    Result = Module:get(State#state.socket, Key, Timeout),
    case Result of
	{error, conn_closed} ->
	    State#state.controller ! {retry, self(), Result, Request},
	    {stop, connection_error, State};
	{error, conn_error} ->
	    State#state.controller ! {retry, self(), Result, Request},
	    {stop, connection_error, State};
	_ ->
	    gen_server:reply(From, binary_to_term(Result)),
	    {noreply, State}
    end;
handle_cast({From, mget, KeyList, Timeout}=Request, State) ->
    Module = State#state.mod,
    Result = Module:mget(State#state.socket, KeyList, Timeout),
    case Result of
	{error, conn_closed} ->
	    State#state.controller ! {retry, self(), Result, Request},
	    {stop, connection_error, State};
	{error, conn_error} ->
	    State#state.controller ! {retry, self(), Result, Request},
	    {stop, connection_error, State};
	_ ->
	    ResultList = [{binary_to_term(K), binary_to_term(V)} || {K, V} <- Result],
	    gen_server:reply(From, ResultList),
	    {noreply, State}
    end;
handle_cast({From, CallFunc}=Request, State) when is_atom(CallFunc) ->
    Module = State#state.mod,
    Result = Module:CallFunc(State#state.socket),
    case Result of
	{error, conn_closed} ->
	    State#state.controller ! {retry, self(), Result, Request},
	    {stop, connection_error, State};
	{error, conn_error} ->
	    State#state.controller ! {retry, self(), Result, Request},
	    {stop, connection_error, State};
	_ ->
	    gen_server:reply(From, Result),
	    {noreply, State}
    end;
handle_cast({From, CallFunc, Arg1}=Request, State) when is_atom(CallFunc) ->
    Module = State#state.mod,
    Result = Module:CallFunc(State#state.socket, Arg1),
    case Result of
	{error, conn_closed} ->
	    State#state.controller ! {retry, self(), Result, Request},
	    {stop, connection_error, State};
	{error, conn_error} ->
	    State#state.controller ! {retry, self(), Result, Request},
	    {stop, connection_error, State};
	_ ->
	    gen_server:reply(From, Result),
	    {noreply, State}
    end;
handle_cast({_From, putnr, Key, Value, Timeout}, State) ->
    Module = State#state.mod,
    Module:putnr(State#state.socket, Key, Value, Timeout),
    {noreply, State};
handle_cast({From, CallFunc, Arg1, Arg2}=Request, State) when is_atom(CallFunc) ->
    Module = State#state.mod,
    Result = Module:CallFunc(State#state.socket, Arg1, Arg2),
    case Result of
	{error, conn_closed} ->
	    State#state.controller ! {retry, self(), Result, Request},
	    {stop, connection_error, State};
	{error, conn_error} ->
	    State#state.controller ! {retry, self(), Result, Request},
	    {stop, connection_error, State};
	_ ->
	    gen_server:reply(From, Result),
	    {noreply, State}
    end;
handle_cast({From, CallFunc, Arg1, Arg2, Arg3}=Request, State) when is_atom(CallFunc) ->
    Module = State#state.mod,
    Result = Module:CallFunc(State#state.socket, Arg1, Arg2, Arg3),
    case Result of
	{error, conn_closed} ->
	    State#state.controller ! {retry, self(), Result, Request},
	    {stop, connection_error, State};
	{error, conn_error} ->
	    State#state.controller ! {retry, self(), Result, Request},
	    {stop, connection_error, State};
	_ ->
	    gen_server:reply(From, Result),
	    {noreply, State}
    end.


%% @spec handle_info(Info, State) -> {noreply, State}
%% @private Handle all non call/cast messages (none are expected).
handle_info(_Info, State) ->
    ?DEBUG_LOG("An unknown info message was received: ~w~n", [_Info]),
    %%% XXX: does this handle tcp connection closed events?
    {noreply, State}.

%% @spec terminate(Reason, State) -> void()
%% @private 
%% Server termination.  Will sync remote database, close connection, and
%% notify controller that it is shutting down.
%% @end
terminate(_Reason, State) ->
    Module = State#state.mod,
    Module:sync(State#state.socket, 1000),
    gen_tcp:close(State#state.socket),
    State#state.controller ! {client_end, self()},
    ok.

%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @private Convert process state when code is changed
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

%% @spec get_db_type(Socket::port(),
%%                   Timeout::integer()) -> {error, Reason::term()} |
%%                                          {ok, endian(), db_type()}
%% @type endian() = little | big
%% @type db_type() = hash | tree | fixed | table
%% @private: Query the remote end of the socket to get the remote database type
get_db_type(Socket, Timeout)
    when is_port(Socket), is_integer(Timeout) ->
    StatInfo = principe:stat(Socket, Timeout),
    case StatInfo of
	{error, Reason} ->
	    {error, Reason};
	StatList ->
	    Endian = case proplists:get_value(bigend, StatList) of
		"0" ->
		    little;
		_ ->
		    big
	    end,
	    Type = case proplists:get_value(type, StatList) of
		"on-memory hash" -> 
		    hash;
		"table" -> 
		    table;
		"on-memory tree" -> 
		    tree;
		"B+ tree" -> 
		    tree;
		"hash" ->
		    hash;
		"fixed-length" ->
		    fixed;
		_ -> 
		    ?DEBUG_LOG("~p:get_db_type returned ~p~n", [?MODULE, proplists:get_value(type, StatList)]),
		    error
	    end,
	    case Type of
		error ->
		    {error, unknown_db_type};
		_ ->
		    {ok, Endian, Type}
	    end	    
    end.

tune_db(State, Timeout) ->
    StatInfo = principe:stat(State#state.socket, Timeout),
    case StatInfo of
	{error, Reason} ->
	    ?DEBUG_LOG("Error getting db type for tuning: ~p", [Reason]),
	    {error, Reason};
	StatList ->
	    case proplists:get_value(type, StatList) of
		"on-memory hash" -> 
		    Records = list_to_integer(proplists:get_value(rnum, StatList)),
		    BnumInt = Records * 4,
		    TuningParam = "bnum=" ++ integer_to_list(BnumInt),
		    principe:optimize(State#state.socket, TuningParam, Timeout);
		"hash" ->
		    Records = list_to_integer(proplists:get_value(rnum, StatList)),
		    BnumInt = Records * 4,
		    TuningParam = "bnum=" ++ integer_to_list(BnumInt),
		    principe:optimize(State#state.socket, TuningParam, Timeout);
		_Other -> 
		    ?DEBUG_LOG("Can't tune a db of type ~p yet", [_Other]),
		    {error, db_type_unsupported_for_tuning}
	    end
    end.
