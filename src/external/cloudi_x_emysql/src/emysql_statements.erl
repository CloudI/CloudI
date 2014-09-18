%% Copyright (c) 2009
%% Bill Warnecke <bill@rupture.com>
%% Jacob Vorreuter <jacob.vorreuter@gmail.com>
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
-module(emysql_statements).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2]).
-export([handle_info/2, terminate/2, code_change/3]).

-export([all/0, fetch/1, add/2, version/2, prepare/3, remove/1]).

-include("emysql.hrl").

-record(state, {statements=gb_trees:empty(), prepared=gb_trees:empty()}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

all() ->
    State = gen_server:call(?MODULE, all, infinity),
    [{statements, [StmtName || {StmtName, _} <- gb_trees:to_list(State#state.statements)]},
        {prepared, gb_trees:to_list(State#state.prepared)}].

fetch(StmtName) ->
    gen_server:call(?MODULE, {fetch, StmtName}, infinity).

add(StmtName, Statement) ->
    gen_server:call(?MODULE, {add, StmtName, Statement}, infinity).

version(ConnId, StmtName) ->
    gen_server:call(?MODULE, {version, ConnId, StmtName}, infinity).

prepare(ConnId, StmtName, Version) ->
    gen_server:call(?MODULE, {prepare, ConnId, StmtName, Version}, infinity).

remove(ConnId) ->
    gen_server:call(?MODULE, {remove, ConnId}, infinity).
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
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%
%% GB trees, see [http://www.erlang.org/doc/man/gb_trees.html]
%%--------------------------------------------------------------------
handle_call(all, _From, State) ->
    {reply, State, State};

handle_call({fetch, StmtName}, _From, State) ->
    {reply, lookup(StmtName, State#state.statements), State};

handle_call({add, StmtName, Statement}, _From, State) ->
    State1 =
        case lookup(StmtName, State#state.statements) of
            undefined ->
                State#state{
                    statements = gb_trees:enter(StmtName, {1, Statement}, State#state.statements)
                };
            {_, Statement} ->
                State;
            {Version, _} ->
                State#state{
                    statements = gb_trees:enter(StmtName, {Version+1, Statement}, State#state.statements)
                }
        end,
    {reply, ok, State1};

handle_call({version, ConnId, StmtName}, _From, State) ->
    Version =
        case lookup(ConnId, State#state.prepared) of
            undefined -> undefined;
            Versions -> lookup(StmtName, Versions)
        end,
    {reply, Version, State};

handle_call({prepare, ConnId, StmtName, Version}, _From, State) ->
    Versions =
        case lookup(ConnId, State#state.prepared) of
            undefined ->
                gb_trees:enter(StmtName, Version, gb_trees:empty());
            Versions1 ->
                gb_trees:enter(StmtName, Version, Versions1)
        end,
    Prepared = gb_trees:enter(ConnId, Versions, State#state.prepared),
    {reply, ok, State#state{prepared=Prepared}};

handle_call({remove, ConnId}, _From, State) ->
    StmtNames =
        case lookup(ConnId, State#state.prepared) of
            undefined -> [];
            Versions -> gb_trees:keys(Versions)
        end,
    Prepared = gb_trees:delete_any(ConnId, State#state.prepared),
    {reply, StmtNames, State#state{prepared=Prepared}};

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
lookup(Key, Tree) ->
    case gb_trees:lookup(Key, Tree) of
        {value, Val} -> Val;
        none -> undefined
    end.
