%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Pool Supervisor==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2011-2015, Michael Truog <mjtruog at gmail dot com>
%%% All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions are met:
%%% 
%%%     * Redistributions of source code must retain the above copyright
%%%       notice, this list of conditions and the following disclaimer.
%%%     * Redistributions in binary form must reproduce the above copyright
%%%       notice, this list of conditions and the following disclaimer in
%%%       the documentation and/or other materials provided with the
%%%       distribution.
%%%     * All advertising materials mentioning features or use of this
%%%       software must display the following acknowledgment:
%%%         This product includes software developed by Michael Truog
%%%     * The name of the author may not be used to endorse or promote
%%%       products derived from this software without specific prior
%%%       written permission
%%% 
%%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
%%% CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
%%% INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
%%% OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
%%% DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
%%% CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
%%% BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
%%% SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
%%% INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
%%% WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
%%% NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
%%% DAMAGE.
%%%
%%% @author Michael Truog <mjtruog [at] gmail (dot) com>
%%% @copyright 2011-2015 Michael Truog
%%% @version 1.5.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(supool_sup).
-author('mjtruog [at] gmail (dot) com').

-behaviour(supervisor).

%% external interface
-export([start_link/4,
         start_children/2,
         which_children/1]).

%% supervisor callbacks
-export([init/1]).

-define(DEFAULT_MAX_R,                              5). % max restart count
-define(DEFAULT_MAX_T,                            300). % max time in seconds

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
    which_child(supervisor:which_children(Supervisor), []).

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

which_child([], L) ->
    L;
which_child([{supool, _, _, _} | Children], L) ->
    which_child(Children, L);
which_child([{_, undefined, _, _} | Children], L) ->
    which_child(Children, L);
which_child([{_, Pid, _, _} | Children], L) ->
    which_child(Children, [Pid | L]).

take_values(DefaultList, List)
    when is_list(DefaultList), is_list(List) ->
    take_values([], DefaultList, List).

take_values(Result, [], List)
    when is_list(Result), is_list(List) ->
    lists:reverse(Result) ++ List;

take_values(Result, [{Key, Default} | DefaultList], List)
    when is_list(Result), is_atom(Key), is_list(List) ->
    case lists:keytake(Key, 1, List) of
        false ->
            take_values([Default | Result], DefaultList, List);
        {value, {Key, Value}, RemainingList} ->
            take_values([Value | Result], DefaultList, RemainingList)
    end.
