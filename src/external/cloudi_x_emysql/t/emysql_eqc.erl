%% erlc -o ebin t/*.erl -W0
%% erl -pa ebin -sasl sasl_error_logger false -name emysql_eqc@`hostname`
%% eqc:quickcheck(emysql_eqc:prop_emysql_eqc()).
%% eqc_gen:sample(eqc_statem:commands(rpcore_user_eqc)).
-module(emysql_eqc).
-behaviour(eqc_statem).
-export([command/1, initial_state/0, next_state/3, precondition/2, postcondition/3]).
-compile(export_all).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eqc/include/eqc_statem.hrl").
-include_lib("emysql/include/emysql.hrl").

-record(state, {pool_id, num_conns, tables, insert_statements}).
-record(column_def, {'Field', 'Type', 'Null', 'Key', 'Default', 'Extra'}).

-define(POOLID, test1).
-define(MAX_CONNECTIONS, 40).

initial_state() -> #state{pool_id=?POOLID, num_conns=1, tables=[], insert_statements=[]}.

command(State) ->
    oneof(
		[{call, ?MODULE, increment_pool_size, [State#state.pool_id, 1]}] ++
		[{call, ?MODULE, decrement_pool_size, [State#state.pool_id, 1]}] ++
		[{call, ?MODULE, show_tables, [State#state.pool_id]}] ++
		[{call, ?MODULE, create_table, [State#state.pool_id, table_name(), ?SUCHTHAT(Y, list(column()), length(Y) > 0)]}] ++
		[{call, ?MODULE, drop_table, [State#state.pool_id, oneof(State#state.tables)]} || length(State#state.tables) > 0] ++
		[{call, ?MODULE, prepare_insert, [State#state.pool_id, oneof(State#state.tables)]} || length(State#state.tables) > 0] ++
		[{call, ?MODULE, call_insert, [State#state.pool_id, ?LET({TN,CD}, oneof(State#state.tables), {TN, column_data(CD)})]} || length(State#state.tables) > 0] ++
		[{call, ?MODULE, timeout, [State#state.pool_id]}] ++
		[]
    ).

%% #############################################################################
%% #############################################################################
%% #############################################################################

precondition(S, {call, ?MODULE, increment_pool_size, [_PoolId, Num]}) ->
	S#state.num_conns + Num < ?MAX_CONNECTIONS;

precondition(S, {call, ?MODULE, decrement_pool_size, [_PoolId, Num]}) ->
	S#state.num_conns - Num >= 1;

precondition(_S, {call, ?MODULE, create_table, [_PoolId, _Name, Columns]}) ->	
	length(lists:usort([string:to_lower(K) || {K,_} <- Columns])) == length(Columns);

precondition(S, {call, ?MODULE, drop_table, [_PoolId, {Name, _}]}) ->	
	proplists:is_defined(Name, S#state.tables);

precondition(S, {call, ?MODULE, call_insert, [_PoolId, {Name, _}]}) ->
	proplists:is_defined(Name, S#state.tables) andalso lists:member(Name, S#state.insert_statements);
	
precondition(_, _) -> true.

%% #############################################################################
%% #############################################################################
%% #############################################################################

next_state(S, _V, {call, ?MODULE, increment_pool_size, [_PoolId, Num]}) ->
    S#state{num_conns = (S#state.num_conns + Num)};

next_state(S, _V, {call, ?MODULE, decrement_pool_size, [_PoolId, Num]}) ->
    S#state{num_conns = (S#state.num_conns - Num)};

next_state(S, _V, {call, ?MODULE, create_table, [_PoolId, Name, Columns]}) ->
	case proplists:is_defined(Name, S#state.tables) of
		false ->
			S#state{tables = [{Name, Columns} | S#state.tables]};
		true ->
			S
	end;
	
next_state(S, _V, {call, ?MODULE, drop_table, [_PoolId, {Name, _}]}) ->
	S#state{tables = proplists:delete(Name, S#state.tables)};

next_state(S, _V, {call, ?MODULE, prepare_insert, [_PoolId, {TableName, _}]}) ->
	S#state{insert_statements = [TableName|S#state.insert_statements]};
		
next_state(S, _, _) -> S.

%% #############################################################################
%% #############################################################################
%% #############################################################################

postcondition(_S, {call, ?MODULE, increment_pool_size, [_PoolId, _Num]}, Result) ->
    Result == ok;

postcondition(_S, {call, ?MODULE, decrement_pool_size, [_PoolId, _Num]}, Result) ->
    Result == ok;

postcondition(_S, {call, ?MODULE, show_tables, [_PoolId]}, Result) ->
	is_record(Result, result_packet);
	
postcondition(_S, {call, ?MODULE, create_table, [_PoolId, _Name, _Columns]}, Result) ->
	case Result of
		Err when is_record(Err, error_packet) ->
			case Err:code() of
				1050 -> %% table already exists
					true;
				_ -> 
					false
			end;
		_ ->
			true == is_record(Result, ok_packet)
    end;

postcondition(_S, {call, ?MODULE, drop_table, [_PoolId, {_Name, _}]}, Result) ->
	case Result of
		OK when is_record(OK, ok_packet) ->
			true;
		Err when is_record(Err, error_packet) ->
			case Err:code() of
				1051 -> %% unknown table
					true;
				_ ->
					false
			end
	end;

postcondition(_S, {call, ?MODULE, prepare_insert, [_PoolId, {_Name, _}]}, Result) ->
	case Result of
		ok ->
			true;
		Err when is_record(Err, error_packet) ->
			case Err:code() of
				1146 ->
					true;
				_ ->
					false
			end
	end;

postcondition(_S, {call, ?MODULE, call_insert, [_PoolId, {_Name, _}]}, Result) ->
	is_record(Result, ok_packet) andalso 1 == Result:affected_rows();
				
postcondition(_S, {call, ?MODULE, timeout, [_PoolId]}, Result) ->
	Result == {'EXIT', mysql_timeout};
				
postcondition(_, _, _) -> true.

%% #############################################################################
%% #############################################################################
%% #############################################################################

prop_emysql_eqc() ->
    ?FORALL(Cmds, commands(?MODULE),
		begin
			init(),
			{H,S,Res} = run_commands(?MODULE,Cmds),
			cleanup(),
			?WHENFAIL(io:format("FAIL ~p\n~p\n~p\n",[H, S, Res]), Res==ok)
		end).

increment_pool_size(PoolId, Num) ->
	emysql:increment_pool_size(PoolId, Num).

decrement_pool_size(PoolId, Num) ->
	emysql:decrement_pool_size(PoolId, Num).
	
column() ->
	{column_name(), column_type()}.
	
column_name() ->
	?SUCHTHAT(X, list(alpha()), length(X) > 0).
	%?SUCHTHAT(X, [alpha()] ++ list(safe_char()), length(X) > 0 andalso hd(lists:reverse(X)) =/= 32).

column_data(Columns) ->
	[data_for_type(Type) || {_Name, Type} <- Columns].

data_for_type("DECIMAL") -> real();
data_for_type("TINYINT") -> int();
data_for_type("LONG") -> int();
data_for_type("FLOAT") -> real();
data_for_type("DOUBLE") -> real();
data_for_type("TIMESTAMP") -> "null";
data_for_type("INT") -> int();
data_for_type("DATE") -> "null";
data_for_type("TIME") -> "null";
data_for_type("DATETIME") -> "null";
data_for_type("YEAR") -> choose(0, 3000);
data_for_type("VARCHAR(255)") -> list(char());
data_for_type("BIT") -> oneof([1,0,true,false]);
data_for_type("BLOB") -> list(char()).

table_name() ->
	?SUCHTHAT(X, list(alpha()), length(X) > 0).
	%?SUCHTHAT(X, [alpha()] ++ list(safe_char()), length(X) > 0 andalso hd(lists:reverse(X)) =/= 32).
	
alpha() ->
	oneof([
		%%choose(65,90),
		choose(97,122)
	]).
	
safe_char() ->
	oneof([
		choose(32,95),
		choose(97,126)
	]).

column_type() ->
	elements([
		"DECIMAL",
		"TINYINT",
		"LONG",
		"FLOAT",
		"DOUBLE",
		"TIMESTAMP",
		"INT",
		"DATE",
		"TIME",
		"DATETIME",
		"YEAR",
		"VARCHAR(255)",
		"BIT",
		"BLOB"
	]).
		
show_tables(PoolId) ->
	(catch emysql:execute(PoolId, "SHOW TABLES")).
	
create_table(PoolId, TableName, Columns) ->
	ColumnDefs = ["`" ++ CName ++ "` " ++ CType || {CName, CType} <- Columns],
	Query = "CREATE TABLE IF NOT EXISTS `" ++ TableName ++ "` ( " ++ string:join(ColumnDefs, ", ") ++ ")",
	(catch emysql:execute(PoolId, Query)).
			
drop_table(PoolId, {TableName, _}) ->
	(catch emysql:execute(PoolId, "DROP TABLE `" ++ TableName ++ "`")).
		
prepare_insert(PoolId, {TableName, _}) ->
	case (catch emysql:execute(PoolId, "DESC `" ++ TableName ++ "`")) of
		Result when is_record(Result, result_packet) ->
			ColDefs = Result:as_record(column_def, record_info(fields, column_def)),
			Stmt = "INSERT INTO `" ++ TableName ++ "` ( " ++ string:join(["`" ++ binary_to_list(Col) ++ "`" || {_,Col,_,_,_,_,_} <- ColDefs], ", ") ++ " ) VALUES ( " ++ string:join(["?" || _ <- ColDefs], ", ") ++ " )",
			(catch emysql:prepare(list_to_atom(TableName), Stmt));
		Err ->
			Err
	end.
	
call_insert(PoolId, {TableName, Args}) ->
	(catch emysql:execute(PoolId, list_to_atom(TableName), Args)).
		
timeout(PoolId) ->
	(catch emysql:execute(PoolId, "SELECT SLEEP(8)", 10)).
		
init() ->
	error_logger:tty(false),
    ok = load_app(crypto),
	ok = application:start(crypto),
	ok = load_app(emysql),
	ok = application:set_env(emysql, pools, [
		{?POOLID, [
			{size, 1},
			{user, "test"},
			{password, "test"},
			{host, "localhost"},
			{port, 3306},
			{database, "testdatabase"},
			{encoding, 'utf8'}
		]}
	]),
	ok = application:start(emysql),
	[emysql:execute(?POOLID, "DROP TABLE " ++ Table) || [Table] <- (emysql:execute(?POOLID, "SHOW TABLES")):rows()],
    ok.

cleanup() ->
    ok = application:stop(crypto),
	ok = application:stop(emysql),
    ok = application:unload(crypto),
	ok = application:unload(emysql),
    ok.

load_app(Name) ->
	case application:load(Name) of
		{error, {already_loaded, Name}} ->
			application:stop(Name),
			application:unload(Name),
			application:load(Name);
		ok ->
			ok
	end.