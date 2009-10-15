%%%-------------------------------------------------------------------
%%% File    : medici.erl
%%% Author  : Jim McCoy <>
%%% Description : 
%%%
%%% Created :  7 May 2009 by Jim McCoy <>
%%%-------------------------------------------------------------------
-module(medici_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% Application callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start(Type, StartArgs) -> {ok, Pid} |
%%                                     {ok, Pid, State} |
%%                                     {error, Reason}
%% Description: This function is called whenever an application 
%% is started using application:start/1,2, and should start the processes
%% of the application. If the application is structured according to the
%% OTP design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%--------------------------------------------------------------------
start(_Type, StartArgs) ->
    {ok, AppEnvOptions} = application:get_env(medici, options),
    CombinedOptions = [StartArgs | AppEnvOptions],
    %% Merge into a single set of options, favoring those passed in
    %% to start/2 over the app env.
    Options = [{K, proplists:get_value(K, CombinedOptions)} ||
               K <- proplists:get_keys(CombinedOptions)],
    medici_sup:start_link(Options).

%%--------------------------------------------------------------------
%% Function: stop(State) -> void()
%% Description: This function is called whenever an application
%% has stopped. It is intended to be the opposite of Module:start/2 and
%% should do any necessary cleaning up. The return value is ignored. 
%%--------------------------------------------------------------------
stop(_State) ->
    ok.
