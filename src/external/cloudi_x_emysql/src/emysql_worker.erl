-module(emysql_worker).
-export([behaviour_info/1]).

-export([start/1, execute/2]).
-export([start_worker/2, worker_loop/2]).

behaviour_info(callbacks) -> [{init, 1}, {process, 1}];
behaviour_info(_) -> undefined.

%% @spec start(Module) -> Result
%%       Module = atom()
%%       Result = {ok, pid()} | {error, term()}
start(Module) ->
    proc_lib:start_link(emysql_worker, start_worker, [self(), Module]).

%% @spec execute(Pid, Message) -> Result
%%       Pid = pid()
%%       Message = term()
%%       Result = term()
execute(To, Message) ->
    {ok, Response} = gen:call(To, '$emysql_worker', Message),
    Response.

%% @hidden
%% @private
start_worker(Parent, Module) ->
    try Module:init(Module) of
        {ok, Prepares, Pool} ->
            ok = init_prepares(Prepares, Pool),
            proc_lib:init_ack(Parent, {ok, self()}),
            error_logger:info_report([?MODULE, {start_worker, Module}, init_ok]),
            emysql_worker:worker_loop(Module, Pool);
        Response ->
            error_logger:error_report([?MODULE, {start_worker, init_nok}, Response]),
            proc_lib:init_ack(Parent, {error, Response})
    catch
        Ma:Mi ->
            error_logger:error_report([?MODULE, {start_worker, init_nok}, {Ma, Mi}]),
            proc_lib:init_ack(Parent, {Ma, Mi})
    end.

%% @hidden
%% @private
worker_loop(Module, Pool) ->
    receive
        {'$emysql_worker', {From, Mref}, Message} ->
            Results = (catch process_message(Module, Pool, Message)),
            gen:reply({From, Mref}, Results);
        Message ->
            error_logger:warning_report([emysql_worker, Module, {unknown_message, Message}])
    end,
    emysql_worker:worker_loop(Module, Pool).

%% @hidden
%% @private
init_prepares([], _) -> ok;
init_prepares([{StmtName, Statement} | Prepares], Pool) ->
    emysql:prepare(StmtName, Statement),
    init_prepares(Prepares, Pool).

%% @hidden
%% @private
process_message(Module, Pool, {Key}) ->
    process_message(Module, Pool, {Key, []});
process_message(Module, Pool, {Key, Args}) ->
    case (catch Module:process({Key, Args})) of
        {Statement} ->
            emysql:execute(Pool, Statement, Args);
        {Statement, NewArgs} ->
            emysql:execute(Pool, Statement, NewArgs);
        {Statement, NewArgs, ApplyFunction} ->
            Results = emysql:execute(Pool, Statement, NewArgs),
            case ApplyFunction of
                {Field, RecordInfo} ->
                    Results:as_record(Field, RecordInfo);
                {Field, RecordInfo, Fun} ->
                    Results:as_record(Field, RecordInfo, Fun)
            end;
        Info ->
            error_logger:warning_report([?MODULE, ?LINE, Module, {unknown_process_directive, Info}])
    end.
