%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%% @doc nodefinder Application.
%% @end

-module(nodefinder_app).

-behaviour (application).

%% external interface
-export([connect_type/0]).

%% application callbacks
-export([start/2,
         stop/1]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

connect_type() ->
    case application:get_env(nodefinder, node_type) of
        {ok, V} when (V =:= visible) orelse (V =:= hidden) ->
            V;
        undefined ->
            visible
    end.

%%%------------------------------------------------------------------------
%%% Callback functions from application
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Start the nodefinder application.===
%% @end
%%-------------------------------------------------------------------------

-spec start(StartType :: normal | {takeover, node()} | {failover, node()},
            StartArgs :: any()) ->
    {ok, Pid :: pid()} |
    {ok, Pid :: pid(), State :: any()} |
    {error, Reason :: any()}.

start(_, _) ->
    nodefinder_sup:start_link().

%%-------------------------------------------------------------------------
%% @doc
%% ===Stop the nodefinder application.===
%% @end
%%-------------------------------------------------------------------------

-spec stop(State :: any()) ->
    'ok'.

stop(_) ->
    ok.

