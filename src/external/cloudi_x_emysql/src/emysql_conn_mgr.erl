%% -*- mode: erlang;erlang-indent-level: 8;indent-tabs-mode:t -*-
%% Copyright (c) 2009-2012
%% Bill Warnecke <bill@rupture.com>
%% Jacob Vorreuter <jacob.vorreuter@gmail.com>
%% Henning Diedrich <hd2010@eonblast.com>
%% Eonblast Corporation <http://www.eonblast.com>
%%
%% Permission is  hereby  granted,  free of charge,  to any person
%% obtaining  a copy of this software and associated documentation
%% files (the "Software"),to deal in the Software without restric-
%% tion,  including  without  limitation the rights to use,  copy,
%% modify, merge,  publish,  distribute,  sublicense,  and/or sell
%% copies  of the  Software,  and to  permit  persons to  whom the
%% Software  is  furnished  to do  so,  subject  to the  following
%% conditions:
%%
%% The above  copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF  MERCHANTABILITY,  FITNESS  FOR  A  PARTICULAR  PURPOSE  AND
%% NONINFRINGEMENT. IN  NO  EVENT  SHALL  THE AUTHORS OR COPYRIGHT
%% HOLDERS  BE  LIABLE FOR  ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT,  TORT  OR OTHERWISE,  ARISING
%% FROM,  OUT OF OR IN CONNECTION WITH THE SOFTWARE  OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.

-module(emysql_conn_mgr).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3]).

-export([pools/0, add_pool/1, has_pool/1,remove_pool/1,
        add_connections/2, remove_connections/2,
        lock_connection/1, wait_for_connection/1, wait_for_connection/2,
        pass_connection/1, replace_connection_as_locked/2, replace_connection_as_available/2,
        find_pool/2, give_manager_control/1]).

-include("emysql.hrl").

-record(state, {pools, lockers = dict:new()}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

pools() ->
    gen_server:call(?MODULE, pools, infinity).

add_pool(Pool) ->
    do_gen_call({add_pool, Pool}).

has_pool(Pool) ->
    do_gen_call({has_pool, Pool}).

remove_pool(PoolId) ->
    do_gen_call({remove_pool, PoolId}).

add_connections(PoolId, Conns) when is_list(Conns) ->
    do_gen_call({add_connections, PoolId, Conns}).

remove_connections(PoolId, Num) when is_integer(Num) ->
    do_gen_call({remove_connections, PoolId, Num}).

lock_connection(PoolId)->
	do_gen_call({lock_connection, PoolId, false, self()}).

wait_for_connection(PoolId)->
	wait_for_connection(PoolId, lock_timeout()).

wait_for_connection(PoolId ,Timeout)->
    %% try to lock a connection. if no connections are available then
    %% wait to be notified of the next available connection
    %-% io:format("~p waits for connection to pool ~p~n", [self(), PoolId]),
    case do_gen_call({lock_connection, PoolId, true, self()}) of
        unavailable ->
            %-% io:format("~p is queued~n", [self()]),
            receive
                {connection, Connection} -> Connection
            after Timeout ->
                do_gen_call({abort_wait, PoolId}),
                receive
                    {connection, Connection} -> Connection
                after
                    0 -> exit(connection_lock_timeout)
                end
            end;
        Connection ->
            %-% io:format("~p gets connection~n", [self()]),
            Connection
    end.

pass_connection(Connection) ->
    do_gen_call({{replace_connection, available}, Connection, Connection}).

replace_connection_as_available(OldConn, NewConn) ->
    do_gen_call({{replace_connection, available}, OldConn, NewConn}).

replace_connection_as_locked(OldConn, NewConn) ->
    do_gen_call({{replace_connection, locked}, OldConn, NewConn}).

give_manager_control(Socket) ->
	case whereis(?MODULE) of
		undefined -> {error ,failed_to_find_conn_mgr};
		MgrPid -> gen_tcp:controlling_process(Socket, MgrPid)
	end.

%% the stateful loop functions of the gen_server never
%% want to call exit/1 because it would crash the gen_server.
%% instead we want to return error tuples and then throw
%% the error once outside of the gen_server process
do_gen_call(Msg) ->
    case gen_server:call(?MODULE, Msg, infinity) of
        {error, Reason} ->
            exit(Reason);
        Result ->
            Result
    end.

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    Pools = lists:map(
        fun (Pool1) ->
                case emysql_conn:open_connections(Pool1) of
                    {ok, Pool2} -> Pool2;
                    {error, Reason} -> throw(Reason)
                end
        end,
        initialize_pools()
    ),
    {ok, #state{pools=Pools}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(pools, _From, State) ->
    {reply, State#state.pools, State};

handle_call({has_pool, PoolID}, _From, State) ->
    case find_pool(PoolID, State#state.pools) of
        {_, _} ->
            {reply, true, State};
        undefined ->
            {reply, false, State}
    end;

handle_call({add_pool, Pool}, _From, State) ->
    case find_pool(Pool#pool.pool_id, State#state.pools) of
        {_, _} ->
            {reply, {error, pool_already_exists}, State};
        undefined ->
            {reply, ok, State#state{pools = [Pool|State#state.pools]}}
    end;

handle_call({remove_pool, PoolId}, _From, State) ->
    case find_pool(PoolId, State#state.pools) of
        {Pool, OtherPools} ->
            {reply, Pool, State#state{pools=OtherPools}};
        undefined ->
            {reply, {error, pool_not_found}, State}
    end;

handle_call({add_connections, PoolId, Conns}, _From, State) ->
    case find_pool(PoolId, State#state.pools) of
        {Pool, OtherPools} ->
            Pool1 = Pool#pool{available = queue:join(queue:from_list(Conns), Pool#pool.available)},
	    {NewPool, NewMonitorData} = serve_waiting_pids(Pool1),
	    NewLockers = lists:foldl(fun({MonitorRef, ConnInfo}, Dict) ->
						     dict:store(MonitorRef, ConnInfo, Dict)
				     end, State#state.lockers, NewMonitorData),
            State1 = State#state{pools = [NewPool|OtherPools],
				 lockers = NewLockers},
            {reply, ok, State1};
        undefined ->
            {reply, {error, pool_not_found}, State}
    end;

handle_call({remove_connections, PoolId, Num}, _From, State) ->
    case find_pool(PoolId, State#state.pools) of
        {Pool, OtherPools} ->
            case Num > queue:len(Pool#pool.available) of
                true ->
                    State1 = State#state{pools = [Pool#pool{available = queue:new()}]},
                    {reply, queue:to_list(Pool#pool.available), State1};
                false ->
                    {Conns, OtherConns} = queue:split(Num, Pool#pool.available),
                    State1 = State#state{pools = [Pool#pool{available = OtherConns}|OtherPools]},
                    {reply, queue:to_list(Conns), State1}
            end;
        undefined ->
            {reply, {error, pool_not_found}, State}
    end;

handle_call({lock_connection, PoolId, Wait, Who}, {From, _Mref}, State) ->
    case find_pool(PoolId, State#state.pools) of
        {Pool, OtherPools} ->
            case lock_next_connection(Pool, Who) of
                {ok, Connection, PoolNow, {MonitorRef, Data}} ->
		    Lockers = State#state.lockers,
                    {reply, Connection, State#state{pools=[PoolNow|OtherPools],
						    lockers = dict:store(MonitorRef, Data, Lockers)}};
                unavailable when Wait =:= true ->
                    %% place the calling pid at the end of the waiting queue of its pool
                    PoolNow = Pool#pool{waiting = queue:in(From, Pool#pool.waiting)},
                    {reply, unavailable, State#state{pools=[PoolNow|OtherPools]}};
                unavailable when Wait =:= false ->
                    {reply, unavailable, State}
            end;
        undefined ->
            {reply, {error, pool_not_found}, State}
    end;

handle_call({abort_wait, PoolId}, {From, _Mref}, State) ->
    case find_pool(PoolId, State#state.pools) of
        {Pool, OtherPools} ->
            %% Remove From from the wait queue
            QueueNow = queue:filter(
                     fun(Pid) -> Pid =/= From end,
                     Pool#pool.waiting),
            PoolNow = Pool#pool{ waiting = QueueNow },
            %% See if the length changed to know if From was removed.
            OldLen = queue:len(Pool#pool.waiting),
            NewLen = queue:len(QueueNow),
            if
                OldLen =:= NewLen ->
                    Reply = not_waiting;
                true ->
                    Reply = ok
            end,
            {reply, Reply, State#state{pools=[PoolNow|OtherPools]}};
        undefined ->
            {reply, {error, pool_not_found}, State}
    end;


handle_call({{replace_connection, Kind}, OldConn, NewConn}, _From, State) ->
    %% if an error occurs while doing work over a connection then
    %% the connection must be closed and a new one created in its
    %% place. The calling process is responsible for creating the
    %% new connection, closing the old one and replacing it in state.
    %% This function expects a new, available connection to be
    %% passed in to serve as the replacement for the old one.
    %% But i.e. if the sql server is down, it can be fed a dead
    %% old connection as new connection, to preserve the pool size.

    case find_pool(OldConn#emysql_connection.pool_id, State#state.pools) of
	    {#pool{available = Available, locked = Locked} = Pool, Pools} ->
		    OldRef = OldConn#emysql_connection.monitor_ref,
                    Stripped = gb_trees:delete_any(OldConn#emysql_connection.id, Locked),
		    {NewPool, NewMonitors} =
			    case Kind of
				    available ->
					    serve_waiting_pids(
					      Pool#pool { locked = Stripped,
							  available = queue:in(
									NewConn#emysql_connection { locked_at = undefined,
												    monitor_ref = undefined},
									Available) });
				    locked ->
					    {Pool#pool { locked = gb_trees:enter(NewConn#emysql_connection.id,
										 NewConn#emysql_connection{monitor_ref = OldRef},
										 Stripped) },
					     []} % There are no new monitors set up
			    end,
		    Lockers = State#state.lockers,
		    NewLockers = case Kind of
					 available ->
						 %% Remove the current monitor as the connection has been freed
						 %% Add the monitors for pids that has been take from the waiting queue
						 erlang:demonitor(OldRef, [flush]),
						 lists:foldl(fun({MonitorRef, ConnInfo}, Dict) ->
									     dict:store(MonitorRef, ConnInfo, Dict)
							     end, dict:erase(OldRef, Lockers), NewMonitors);
					 locked ->
						 %% We need to keep the monitor here
						 dict:store(OldRef, {NewConn#emysql_connection.pool_id, NewConn#emysql_connection.id}, Lockers)
				 end,
		    {reply, ok, State#state{pools = [NewPool|Pools],
					    lockers = NewLockers}};
	   undefined ->
		    {reply, {error, pool_not_found}, State}
    end;

handle_call(_, _From, State) -> {reply, {error, invalid_call}, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info({'DOWN', MonitorRef, _, _, _}, State) ->
	case dict:find(MonitorRef, State#state.lockers) of
		{ok, {PoolId, ConnId}} ->
			case find_pool(PoolId, State#state.pools) of
				{Pool, _} ->
					case gb_trees:lookup(ConnId, Pool#pool.locked) of
						{value, Conn} -> async_reset_conn(State#state.pools, Conn);
						_             -> ok
					end;
				_ ->
					ok
			end;
		_ ->
			ok
	end,
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

async_reset_conn(Pools, Conn) ->
	spawn(fun() ->
			      %% This interacts with the conn mgr so needs to be spawned
			      %% TODO: refactor
			      emysql_conn:reset_connection(Pools, Conn, pass)
	      end).
%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------
initialize_pools() ->
    %% if the emysql application values are not present in the config
    %% file we will initialize and empty set of pools. Otherwise, the
    %% values defined in the config are used to initialize the state.
    [
        #pool{
            pool_id = PoolId,
            size = proplists:get_value(size, Props, 1),
            user = proplists:get_value(user, Props),
            password = proplists:get_value(password, Props),
            host = proplists:get_value(host, Props),
            port = proplists:get_value(port, Props),
            database = proplists:get_value(database, Props),
            encoding = proplists:get_value(encoding, Props),
            start_cmds = proplists:get_value(start_cmds, Props, [])
        } || {PoolId, Props} <- emysql_app:pools()
    ].

find_pool(PoolId, Pools) ->
    find_pool(PoolId, Pools, []).

find_pool(_, [], _) -> undefined;

find_pool(PoolId, [#pool{pool_id = PoolId} = Pool|Tail], OtherPools) ->
    {Pool, lists:append(OtherPools, Tail)};

find_pool(PoolId, [Pool|Tail], OtherPools) ->
    find_pool(PoolId, Tail, [Pool|OtherPools]).

lock_next_connection(Pool, Who) ->
	case lock_next_connection(Pool#pool.available, Pool#pool.locked, Who) of
		{ok, Connection, OtherAvailable, NewLocked, MonitorTuple} ->
			{ok ,Connection ,Pool#pool{available=OtherAvailable, locked=NewLocked}, MonitorTuple};
		unavailable ->
			unavailable
	end.

lock_next_connection(Available ,Locked, Who) ->
	case queue:out(Available) of
		{{value, Conn}, OtherAvailable} ->
			MonitorRef = erlang:monitor(process, Who),
            NewConn = connection_locked_at(Conn, MonitorRef),
			MonitorTuple = {MonitorRef,
					{NewConn#emysql_connection.pool_id, NewConn#emysql_connection.id}},
			NewLocked = gb_trees:enter(NewConn#emysql_connection.id, NewConn, Locked),
			{ok, NewConn, OtherAvailable, NewLocked, MonitorTuple};
        {empty, _} ->
            unavailable
    end.

connection_locked_at(Conn, MonitorRef) ->
	Conn#emysql_connection{locked_at=lists:nth(2, tuple_to_list(now())),
			       monitor_ref = MonitorRef}.

serve_waiting_pids(Pool) ->
    {Waiting, Available, Locked, NewRefs} = serve_waiting_pids(Pool#pool.waiting, Pool#pool.available, Pool#pool.locked, []),
    {Pool#pool{waiting=Waiting, available=Available, locked=Locked}, NewRefs}.

serve_waiting_pids(Waiting, Available, Locked, MonitorRefs) ->
    case queue:is_empty(Waiting) of
        false ->
		    Who = queue:get(Waiting),
			case lock_next_connection(Available, Locked, Who) of
				{ok, Connection, OtherAvailable, NewLocked, NewRef} ->
                    {{value, Pid}, OtherWaiting} = queue:out(Waiting),
                    case erlang:is_process_alive(Pid) of
                        true ->
                            erlang:send(Pid, {connection, Connection}),
                            serve_waiting_pids(OtherWaiting, OtherAvailable, NewLocked, [NewRef | MonitorRefs]);
                        _    ->
                            serve_waiting_pids(OtherWaiting, Available, Locked, MonitorRefs)
                    end;
				unavailable ->
                    {Waiting, Available, Locked, MonitorRefs}
            end;
        true ->
            {Waiting, Available, Locked, MonitorRefs}
    end.

lock_timeout() ->
    emysql_app:lock_timeout().
