%%%-------------------------------------------------------------------
%%% @author Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>
%%% @copyright (C) 2013 Mahesh Paolini-Subramanya
%%% @doc Root Supervisor for erlasticsearch
%%% @end
%%%
%%% This source file is subject to the New BSD License. You should have received
%%% a copy of the New BSD license with this software. If not, it can be
%%% retrieved from: http://www.opensource.org/licenses/bsd-license.php
%%%-------------------------------------------------------------------
-module(erlasticsearch_client_sup).
-author('Mahesh Paolini-Subramanya <mahesh@dieswaytoofast.com>').

-behaviour(supervisor).

-include("erlasticsearch.hrl").

%% API
-export([start_link/0]).
-export([start_client/1, start_client/2]).
-export([stop_client/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% @doc Helper macro for declaring children of supervisor
-define(WORKER(Restart, Module, Args), {Module, {Module, start_link, Args}, Restart, 5000, worker, [Module]}).
-define(SUPERVISOR(Restart, Module, Args), {Module, {Module, start_link, Args}, Restart, 5000, supervisor, [Module]}).

-type startlink_err() :: {'already_started', pid()} | 'shutdown' | term().
-type startlink_ret() :: {'ok', pid()} | 'ignore' | {'error', startlink_err()}.

-spec start_link() -> startlink_ret().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @equiv start_client(ClientName, []).
-spec start_client(client_name()) -> supervisor:startchild_ret().
start_client(ClientName) when is_binary(ClientName) ->
    start_client(ClientName, []).

-spec start_client(client_name(), params()) -> supervisor:startchild_ret().
start_client(ClientName, Options) when is_binary(ClientName),
                                       is_list(Options) ->
    supervisor:start_child(?SERVER, [ClientName, Options]).


-spec stop_client(client_name()) -> ok | error().
stop_client(ClientName) ->
    ChildName = erlasticsearch:registered_client_name(ClientName),
    supervisor:terminate_child(?SERVER, whereis(ChildName)).

-spec init(Args :: term()) -> {ok, {{RestartStrategy :: supervisor:strategy(), MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
                                    [ChildSpec :: supervisor:child_spec()]}}.
init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 3,
    MaxSecondsBetweenRestarts = 60,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    {ok, {SupFlags, [?WORKER(permanent, erlasticsearch, [])]}}.
