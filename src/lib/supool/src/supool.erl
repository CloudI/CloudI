%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Supervisor Pool==
%%% Simple supervisor process pool with round-robin.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2011-2017, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011-2017 Michael Truog
%%% @version 1.5.5 {@date} {@time}
%%%------------------------------------------------------------------------

-module(supool).
-author('mjtruog [at] gmail (dot) com').

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

-include("supool_logging.hrl").

-record(state,
    {
        supervisor :: pid(),
        index = 1 :: pos_integer(),
        pool = undefined :: tuple() | undefined,
        count = undefined :: pos_integer() | undefined
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

handle_call(get, _, #state{supervisor = Supervisor,
                           index = I,
                           pool = Pool,
                           count = Count} = State) ->
    Pid = erlang:element(I, Pool),
    case erlang:is_process_alive(Pid) of
        true ->
            {reply, Pid, State#state{index = increment(I, Count)}};
        false ->
            NewPids = supool_sup:which_children(Supervisor),
            case erlang:length(NewPids) of
                0 ->
                    {stop, {error, noproc}, undefined, State};
                NewCount ->
                    NewPool = erlang:list_to_tuple(NewPids),
                    NewI = if I > NewCount -> 1; true -> I end,
                    {reply, erlang:element(NewI, NewPool),
                     State#state{index = increment(NewI, NewCount),
                                 pool = NewPool,
                                 count = NewCount}}
            end
    end;

handle_call(Request, _, State) ->
    ?LOG_WARN("Unknown call \"~p\"", [Request]),
    {stop, lists:flatten(io_lib:format("Unknown call \"~p\"", [Request])),
     error, State}.

handle_cast(Request, State) ->
    ?LOG_WARN("Unknown cast \"~p\"", [Request]),
    {noreply, State}.

handle_info({start, ChildSpecs}, #state{supervisor = Supervisor} = State) ->
    case supool_sup:start_children(Supervisor, ChildSpecs) of
        {ok, []} ->
            {stop, {error, noproc}, State};
        {ok, Pids} ->
            {noreply, State#state{pool = erlang:list_to_tuple(Pids),
                                  count = erlang:length(Pids)}};
        {error, _} = Error ->
            {stop, Error, State}
    end;

handle_info(restart, #state{supervisor = Supervisor} = State) ->
    Pids = supool_sup:which_children(Supervisor),
    case erlang:length(Pids) of
        0 ->
            % pool worker started for the first time
            {noreply, State};
        Count ->
            % pool worker restarted
            {noreply,
             State#state{pool = erlang:list_to_tuple(Pids),
                         count = Count}}
    end;

handle_info(Request, State) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

increment(Count, Count) ->
    1;
increment(I, _) ->
    I + 1.

