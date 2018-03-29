%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Pool Supervisor==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2011-2017 Michael Truog <mjtruog at protonmail dot com>
%%%
%%% Permission is hereby granted, free of charge, to any person obtaining a
%%% copy of this software and associated documentation files (the "Software"),
%%% to deal in the Software without restriction, including without limitation
%%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%% and/or sell copies of the Software, and to permit persons to whom the
%%% Software is furnished to do so, subject to the following conditions:
%%%
%%% The above copyright notice and this permission notice shall be included in
%%% all copies or substantial portions of the Software.
%%%
%%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%%% DEALINGS IN THE SOFTWARE.
%%%
%%% @author Michael Truog <mjtruog at protonmail dot com>
%%% @copyright 2011-2017 Michael Truog
%%% @version 1.7.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(supool_sup).
-author('mjtruog at protonmail dot com').

-behaviour(supervisor).

%% external interface
-export([start_link/4,
         start_children/2,
         which_children/1]).

%% supervisor callbacks
-export([init/1]).

-define(DEFAULT_MAX_R,                              5). % max restart count
-define(DEFAULT_MAX_T,                            300). % max time in seconds

% anticipated minimum delay for the supervisor behaviour to
% restart a child process, used to check the current pool state
-define(RESTART_DELAY,                            100). % milliseconds

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-spec start_link(Name :: atom(),
                 Count :: pos_integer(),
                 ChildSpec :: supool:child_spec(),
                 Options :: supool:options()) ->
    {ok, pid()} |
    {error, any()}.

start_link(Name, Count,
           {_, StartFunc, Restart, Shutdown, Type, Modules}, Options)
    when is_atom(Name), is_integer(Count), Count > 0, is_list(Options) ->
    ChildSpecs = lists:foldl(fun(Id, L) ->
        [{Id, StartFunc, Restart, Shutdown, Type, Modules} | L]
    end, [], lists:seq(Count, 1, -1)),
    Result = supervisor:start_link(?MODULE, [Name, Options]),
    case Result of
        {ok, _} ->
            PoolWorker = erlang:whereis(Name),
            true = is_pid(PoolWorker),
            % needs to be the first message to the pool worker process to
            % make sure pool processes exist before they are requested
            PoolWorker ! {start, ChildSpecs},
            Result;
        _ ->
            Result
    end.

-spec start_children(Supervisor :: pid(),
                     ChildSpecs :: nonempty_list(supool:child_spec())) ->
    {ok, list(pid())} |
    {error, any()}.

start_children(Supervisor, ChildSpecs)
    when is_pid(Supervisor), is_list(ChildSpecs) ->
    start_child(ChildSpecs, [], Supervisor).

-spec which_children(Supervisor :: pid()) ->
    list(pid()).

which_children(Supervisor)
    when is_pid(Supervisor) ->
    which_child(supervisor:which_children(Supervisor), [], Supervisor).

%%%------------------------------------------------------------------------
%%% Callback functions from supervisor
%%%------------------------------------------------------------------------

init([Name, Options]) ->
    Defaults = [
        {max_r,            ?DEFAULT_MAX_R},
        {max_t,            ?DEFAULT_MAX_T}],
    [MaxR, MaxT] = take_values(Defaults, Options),
    true = is_integer(MaxR) andalso (MaxR >= 0),
    true = is_integer(MaxT) andalso (MaxT >= 1),
    {ok, {{one_for_one, MaxR, MaxT}, 
          [{supool,
            {supool, pool_worker_start_link, [Name, self()]},
            permanent, brutal_kill, worker, [supool]}]}}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

start_child([], Pids, _) ->
    {ok, lists:reverse(Pids)};
start_child([ChildSpec | ChildSpecs], Pids, Supervisor) ->
    case supervisor:start_child(Supervisor, ChildSpec) of
        {ok, Pid} ->
            start_child(ChildSpecs, [Pid | Pids], Supervisor);
        {ok, Pid, _} ->
            start_child(ChildSpecs, [Pid | Pids], Supervisor);
        {error, _} = Error ->
            Error
    end.

which_child([], L, _) ->
    L;
which_child([{supool, _, _, _} | Children], L, Supervisor) ->
    which_child(Children, L, Supervisor);
which_child([{_, undefined, _, _} | Children], L, Supervisor) ->
    which_child(Children, L, Supervisor);
which_child([{_, restarting, _, _} | _], _, Supervisor) ->
    receive after ?RESTART_DELAY -> ok end,
    which_children(Supervisor);
which_child([{_, Pid, _, _} | Children], L, Supervisor)
    when is_pid(Pid) ->
    which_child(Children, [Pid | L], Supervisor).

take_values(DefaultList, List)
    when is_list(DefaultList), is_list(List) ->
    take_values([], DefaultList, List).
take_values(Result, [], List) ->
    lists:reverse(Result) ++ List;
take_values(Result, [{Key, Default} | DefaultList], List) ->
    case lists:keytake(Key, 1, List) of
        false ->
            take_values([Default | Result], DefaultList, List);
        {value, {Key, Value}, RemainingList} ->
            take_values([Value | Result], DefaultList, RemainingList)
    end.

