%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:

-module(nodefinder_sup).

-behaviour(supervisor).

%% external interface
-export([start_link/0,
         start_child/2,
         stop_child/1]).

%% supervisor callback
-export([init/1]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-spec start_link() ->
    {ok, pid()} |
    ignore |
    {error, any()}.

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

-spec start_child(Name :: module(),
                  Args :: list()) ->
    {ok, pid()} | 
    {error, any()}.

start_child(Name, Args)
    when is_atom(Name), is_list(Args) ->
    % only one instance of the Name module, at a time
    ChildSpec = {Name, {Name, start_link, Args},
                 transient, 1000, worker, [Name]},
    case supervisor:start_child(?MODULE, ChildSpec) of
        {ok, _} = Success ->
            Success;
        {ok, Pid, _} ->
            {ok, Pid};
        {error, _} = Error ->
            Error
    end.

-spec stop_child(Name :: module()) ->
    ok |
    {error, restarting | not_running | not_found | simple_one_for_one}.

stop_child(Name)
    when is_atom(Name) ->
    case lists:keyfind(Name, 1, supervisor:which_children(?MODULE)) of
        {_, Child, _, _} when is_pid(Child) ->
            case supervisor:terminate_child(?MODULE, Name) of
                ok ->
                    supervisor:delete_child(?MODULE, Name);
                {error, _} = Error ->
                    Error
            end;
        {_, restarting, _, _} ->
            {error, restarting};
        {_, undefined, _, _} ->
            {error, not_running};
        false ->
            {error, not_found}
    end.

%%%------------------------------------------------------------------------
%%% Callback functions from supervisor
%%%------------------------------------------------------------------------

init([]) ->
    MaxRestarts = 3,
    MaxTime = 10, % seconds
    {ok, {{one_for_one, MaxRestarts, MaxTime}, []}}.

