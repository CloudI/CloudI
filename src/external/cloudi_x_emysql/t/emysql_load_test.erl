%% erlc -o ebin t/emysql_load_test.erl -W0
%% erl -pa ebin -boot start_sasl -name emysql_load_test@`hostname` -config priv/load-test -eval 'emysql_load_test:start_link()'
%% erl -pa ebin -boot start_sasl -name emysql_load_test@`hostname` -config priv/load-test -eval 'emysql_tracer:trace_module(emysql_load_test, fun emysql_load_test:start_link/0)'
%% emysql_tracer:trace_module(emysql_load_test, fun emysql_load_test:start_link/0).
%% emysql_tracer:trace_module(emysql_load_test, fun emysql_load_test:start_link/0, [call,procs,send,'receive']).
%% emysql_load_test:add_pool().
%% emysql_load_test:select_all().
%% emysql_load_test:increment_pool_size().
%% emysql_load_test:increment_pool_size().
%% [emysql_load_test:select_all() || _ <- lists:seq(1,10)].
%% emysql_tracer:trace_module(emysql_load_test, fun emysql_load_test:select_all/0).
%% emysql_tracer:trace_module(emysql_load_test, fun emysql_load_test:select_all/0).
%% emysql:execute(test1, <<"SELECT * FROM `user_games`">>). 
-module(emysql_load_test).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([terminate/2, code_change/3]).

-compile(export_all).

-include_lib("emysql/include/emysql.hrl").

-record(state, {}).

start_link() ->
	application:start(crypto),
	application:start(emysql),
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
	
add_pool() ->
	gen_server:call(?MODULE, add_pool, infinity).
	
remove_pool() ->
	gen_server:call(?MODULE, remove_pool, infinity).
	
increment_pool_size() ->
	gen_server:call(?MODULE, increment_pool_size, infinity).
	
decrement_pool_size() ->
	gen_server:call(?MODULE, decrement_pool_size, infinity).

select_all() ->
	gen_server:call(?MODULE, select_all, infinity).
	
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
	apply(random, seed, tuple_to_list(now())),
	{ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------
handle_call(add_pool, _From, State) ->
	NewPoolId = 
		case lists:reverse(lists:usort([PoolId || #pool{pool_id=PoolId} <- emysql_conn_mgr:pools()])) of
			[] ->
				test1;
			[Last|_] ->
				"test" ++ Rest = atom_to_list(Last),
				list_to_atom(lists:concat(["test", list_to_integer(Rest)+1]))
		end,
	{ok, User} = application:get_env(emysql, user),
	{ok, Password} = application:get_env(emysql, password),
	{ok, Host} = application:get_env(emysql, host),
	{ok, Port} = application:get_env(emysql, port),
	{ok, Database} = application:get_env(emysql, database),
	{ok, Encoding} = application:get_env(emysql, encoding),
	Res = emysql:add_pool(NewPoolId, 1, User, Password, Host, Port, Database, Encoding),
	{reply, Res, State};
	
handle_call(remove_pool, _From, State) ->
	case emysql_conn_mgr:pools() of
		[] ->
			{reply, ok, State};
		[Pool|_] ->
			{reply, emysql:remove_pool(Pool#pool.pool_id), State}
	end;	
	
handle_call(increment_pool_size, _From, State) ->
	case emysql_conn_mgr:pools() of
		[] ->
			{reply, ok, State};
		[Pool|_] ->
			{reply, emysql:increment_pool_size(Pool#pool.pool_id, 1), State}
	end;

handle_call(decrement_pool_size, _From, State) ->
	case emysql_conn_mgr:pools() of
		[] ->
			{reply, ok, State};
		[Pool|_] ->
			{reply, emysql:decrement_pool_size(Pool#pool.pool_id, 1), State}
	end;
	
handle_call(select_all, _From, State) ->
	Pool = get_pool(),
	I = random:uniform(length(tables())),
	spawn(fun() ->
		{TableName, _} = lists:nth(I, tables()),
		case (catch emysql:execute(Pool#pool.pool_id, <<"SELECT * FROM `", (list_to_binary(TableName))/binary, "`">>)) of
			Result when is_record(Result, result_packet) ->
				ok;
			{'EXIT',connection_lock_timeout} ->
				ok;
			{'EXIT',mysql_timeout} ->
				ok;
			Err ->
				io:format("~p error: ~p~n", [TableName, Err])
		end
	end),
	{reply, ok, State};
		
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
handle_info(_Info, State) ->
    {noreply, State}.

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
get_pool() ->
	case emysql_conn_mgr:pools() of
		[] -> undefined;
		[Pool|_] -> Pool
	end.

tables() ->
	[{"applications",[]},
	 {"asset_tokens",[]},
	 {"assets",[]},
	 {"auth_tokens",[]},
	 {"avatars",[]},
	 {"blog_posts",[]},
	 {"box_art",[]},
	 {"challenge_issuance_watches",[]},
	 {"challenge_issuances",[]},
	 {"challenge_participations",[]},
	 {"challenge_ratings",[]},
	 {"challenges",[]},
	 {"challenges_events",[]},
	 {"client_igo_stats",[]},
	 {"client_session_stats",[]},
	 {"comment_photos",[]},
	 {"comments",[]},
	 {"conversions",[]},
	 {"critic_reviews",[]},
	 {"datasource_games",[]},
	 {"datasources",[]},
	 {"event_media",[]},
	 {"event_medium_photos",[]},
	 {"events",[]},
	 {"facebook_applications",[]},
	 {"facebook_identities",[]},
	 {"facebook_invitations",[]},
	 {"feedbacks",[]},
	 {"feeds",[]},
	 {"flags",[]},
	 {"game_aliases",[]},
	 {"game_client_data",[]},
	 {"game_objective_types",[]},
	 {"game_objectives",[]},
	 {"game_sessions",[]},
	 {"games",[]},
	 {"identities",[]},
	 {"identity_group_definitions",[]},
	 {"identity_group_memberships",[]},
	 {"identity_groups",[]},
	 {"identity_properties",[]},
	 {"identity_providers",[]},
	 {"image_sizes",[]},
	 {"images",[]},
	 {"invitations",[]},
	 {"items",[]},
	 {"lobby_dataset_stats",[]},
	 {"lobby_datasets",[]},
	 {"lobby_game_servers",[]},
	 {"messages",[]},
	 {"metagame_event_types",[]},
	 {"metagame_events",[]},
	 {"plasma_game_stats",[]},
	 {"platforms",[]},
	 {"privacy_lists",[]},
	 {"promotions",[]},
	 {"rostergroups",[]},
	 {"rupture_invitations",[]},
	 {"schema_migrations",[]},
	 {"server_settings",[]},
	 {"sessions",[]},
	 {"socialapplication_identity_associations",[]},
	 {"socialapplication_networks",[]},
	 {"subscriptions",[]},
	 {"test_groups",[]},
	 {"top_games",[]},
	 {"top_users",[]},
	 {"user_games",[]},
	 {"user_invitations",[]},
	 {"user_presence",[]},
	 {"users",[]}].