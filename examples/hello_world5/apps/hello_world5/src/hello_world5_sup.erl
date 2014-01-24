%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
-module(hello_world5_sup).

-behaviour(supervisor).

%% external interface
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Start the Hello World 5 application supervisor.===
%% @end
%%-------------------------------------------------------------------------

-spec start_link() ->
    {'ok', pid()} |
    {'error', any()}.

start_link() ->
    supervisor:start_link(?MODULE, []).

%%%------------------------------------------------------------------------
%%% Callback functions from supervisor
%%%------------------------------------------------------------------------

init([]) ->
    MaxRestarts = 5,
    MaxTime = 60, % seconds (1 minute)
    {ok,
     {{one_for_all, MaxRestarts, MaxTime},
      []}}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

