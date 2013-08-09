%%%-------------------------------------------------------------------
%%% File    : medici_sup.erl
%%% Author  : Jim McCoy <>
%%% Description : 
%%%
%%% Created :  6 May 2009 by Jim McCoy <>
%%%-------------------------------------------------------------------
-module(medici_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).
-export([get_controller/1]).

%% Supervisor callbacks
-export([init/1]).


%%====================================================================
%% API functions
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the supervisor
%%--------------------------------------------------------------------
start_link(Options) ->
    supervisor:start_link(?MODULE, Options).

%%--------------------------------------------------------------------
%% Obtain the controller pid
%%--------------------------------------------------------------------
get_controller(SupervisorRef) ->
    case lists:keyfind(controller, 1,
                       supervisor:which_children(SupervisorRef)) of
        false ->
            {error, "controller does not exist"};
        {controller, undefined, _, _} ->
            {error, "controller was not started"};
        {controller, ControllerPid, _, _} ->
            {ok, ControllerPid}
    end.

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
init(Options) ->
    MediciController = case proplists:get_bool(native, Options) of
    false ->
        {controller,
         {medici_controller, start_link, [Options]},
         permanent,
         2000,
         worker,
         [medici_controller]};
    true ->
        {controller,
         {medici_native_controller, start_link, [Options]},
         permanent,
         2000,
         worker,
         [medici_native_controller]}
    end,
    MediciConnSupervisor = {connection_supervisor,
                            {medici_conn_sup, start_link, [self(), Options]},
                            permanent, 
                            infinity, 
                            supervisor, 
                            [medici_conn_sup]},
    case proplists:get_value(run_server, Options) of
    undefined ->
        {ok,{{one_for_all,1,10}, [MediciController, 
                                  MediciConnSupervisor]}};
    _ ->
        MediciPortSupervisor = {port_supervisor,
                                {medici_port_sup, start_link, [Options]},
                                permanent,
                                infinity,
                                supervisor,
                                [medici_port_sup]},
        {ok,{{one_for_all,1,10}, [MediciController, 
                                  MediciConnSupervisor,
                                  MediciPortSupervisor]}}
    end.

%%====================================================================
%% Internal functions
%%====================================================================
