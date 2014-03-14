%%%-------------------------------------------------------------------
%%% File    : medici_conn_sup.erl
%%% Author  : Jim McCoy <>
%%% Description : 
%%%
%%% Created :  6 May 2009 by Jim McCoy <>
%%%-------------------------------------------------------------------
-module(medici_conn_sup).

-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

-include("medici.hrl").

%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link(SupervisorPid, Options) -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link(SupervisorPid, Options) ->
    supervisor:start_link(?MODULE, [SupervisorPid, Options]).

%%====================================================================
%% Supervisor callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Func: init(Args) -> {ok,  {SupFlags,  [ChildSpec]}} |
%%                     ignore                          |
%%                     {error, Reason}
%% Description: Whenever a supervisor is started using 
%% supervisor:start_link/[2,3], this function is called by the new process 
%% to find out about restart strategy, maximum restart frequency and child 
%% specifications.
%%--------------------------------------------------------------------
init([SupervisorPid, Options]) ->
    ClientCount = proplists:get_value(num_connections, Options, ?NUM_CLIENTS),
    ChildList = case proplists:get_bool(native, Options) of
    	false ->
    	    [{ChildNum, 
    			  {medici_conn, start_link,
                   [SupervisorPid, Options]},
    			  permanent,
    			  2000,
    			  worker,
    			  [medici_conn]} || ChildNum <-
                   lists:seq(1, ClientCount)];
    	true ->
    	    [{ChildNum, 
    			  {medici_native_conn, start_link,
                   [SupervisorPid, Options]},
    			  permanent,
    			  2000,
    			  worker,
    			  [medici_native_conn]} || ChildNum <-
                   lists:seq(1, ClientCount)]
    end,
    {ok,{{one_for_one,ClientCount*2,5}, ChildList}}.

%%====================================================================
%% Internal functions
%%====================================================================
