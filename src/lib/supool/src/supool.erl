%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Supervisor Pool==
%%% Simple supervisor process pool with round-robin.
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2011-2021 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2011-2021 Michael Truog
%%% @version 2.0.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(supool).
-author('mjtruog at protonmail dot com').

-behaviour(gen_server).

%% external interface
-export([start_link/3,
         start_link/4,
         get/1]).

%% internal interface
-export([pool_worker_start_link/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state,
    {
        supervisor :: pid(),
        index = 1 :: pos_integer(),
        count = undefined :: pos_integer() | undefined,
        pool = undefined :: tuple() | undefined
    }).

-type options() ::
    list({max_r, non_neg_integer()} |
         {max_t, pos_integer()}).
-type child_spec() ::
    {Id :: any(),
     StartFunc :: {module(), atom(), list()},
     Restart :: permanent | transient | temporary,
     Shutdown :: brutal_kill | pos_integer(),
     Type :: worker | supervisor,
     Modules :: [module()] | dynamic}.
-export_type([options/0,
              child_spec/0]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Start the pool supervisor.===
%% @end
%%-------------------------------------------------------------------------

-spec start_link(Name :: atom(),
                 Count :: pos_integer(),
                 ChildSpec :: child_spec()) ->
    {ok, pid()} |
    {error, any()}.

start_link(Name, Count, ChildSpec) ->
    supool_sup:start_link(Name, Count, ChildSpec, []).

%%-------------------------------------------------------------------------
%% @doc
%% ===Start the pool supervisor with restart options.===
%% @end
%%-------------------------------------------------------------------------

-spec start_link(Name :: atom(),
                 Count :: pos_integer(),
                 ChildSpec :: child_spec(),
                 Options :: options()) ->
    {ok, pid()} |
    {error, any()}.

start_link(Name, Count, ChildSpec, Options) ->
    supool_sup:start_link(Name, Count, ChildSpec, Options).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a pool process.===
%% @end
%%-------------------------------------------------------------------------

-spec get(Name :: atom()) ->
    pid() | undefined.

get(Name)
    when is_atom(Name) ->
    try gen_server:call(Name, get, infinity)
    catch
        exit:{noproc, _} ->
            undefined
    end.

%%%------------------------------------------------------------------------
%%% Internal interface functions
%%%------------------------------------------------------------------------

-spec pool_worker_start_link(Name :: atom(),
                             Supervisor :: pid()) ->
    {ok, pid()} |
    {error, any()}.

pool_worker_start_link(Name, Supervisor)
    when is_atom(Name), is_pid(Supervisor) ->
    gen_server:start_link({local, Name}, ?MODULE, [Supervisor], []).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([Supervisor]) ->
    self() ! restart,
    {ok, #state{supervisor = Supervisor}}.

handle_call(get, _, #state{index = Index,
                           count = Count,
                           pool = Pool} = State) ->
    {Result, IndexNew} = pool_pid(Index, Count, Pool),
    {reply, Result, State#state{index = IndexNew}};
handle_call(Request, _, State) ->
    {stop, lists:flatten(io_lib:format("Unknown call \"~w\"", [Request])),
     error, State}.

handle_cast(Request, State) ->
    {stop, lists:flatten(io_lib:format("Unknown cast \"~w\"", [Request])),
     State}.

handle_info({start, ChildSpecs}, #state{supervisor = Supervisor} = State) ->
    case supool_sup:start_children(Supervisor, ChildSpecs) of
        {ok, []} ->
            {stop, {error, noproc}, State};
        {ok, Pids} ->
            {noreply, State#state{count = length(Pids),
                                  pool = erlang:list_to_tuple(Pids)}};
        {error, _} = Error ->
            {stop, Error, State}
    end;
handle_info(restart, #state{supervisor = Supervisor} = State) ->
    Pids = supool_sup:which_children(Supervisor),
    case length(Pids) of
        0 ->
            % pool worker started for the first time
            {noreply, State};
        Count ->
            % pool worker restarted
            {noreply,
             State#state{count = Count,
                         pool = erlang:list_to_tuple(Pids)}}
    end;
handle_info(update, #state{supervisor = Supervisor} = State) ->
    Pids = supool_sup:which_children(Supervisor),
    case length(Pids) of
        0 ->
            {stop, {error, noproc}, State};
        Count ->
            {noreply,
             State#state{count = Count,
                         pool = erlang:list_to_tuple(Pids)}}
    end;
handle_info(Request, State) ->
    {stop, lists:flatten(io_lib:format("Unknown info \"~w\"", [Request])),
     State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

pool_pid(I, Count, Pool) ->
    Pid = element(I, Pool),
    case erlang:is_process_alive(Pid) of
        true ->
            {Pid, increment(I, Count)};
        false ->
            self() ! update,
            pool_pid(increment(I, Count), I, Count, Pool)
    end.

pool_pid(I, I, _, _) ->
    {undefined, I};
pool_pid(I, IndexStart, Count, Pool) ->
    Pid = element(I, Pool),
    case erlang:is_process_alive(Pid) of
        true ->
            {Pid, increment(I, Count)};
        false ->
            pool_pid(increment(I, Count), IndexStart, Count, Pool)
    end.

increment(Count, Count) ->
    1;
increment(I, _) ->
    I + 1.

