%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Pool==
%%% Simple process pool with round-robin.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2011-2012, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011-2012 Michael Truog
%%% @version 0.2.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_pool).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).

%% external interface
-export([start_link/3,
         get/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("cloudi_logger.hrl").

-record(state,
    {
        pool,
        count,
        supervisor
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

start_link(Name, ChildSpecs, Supervisor)
    when is_atom(Name), is_list(ChildSpecs), is_pid(Supervisor) ->
    gen_server:start_link({local, Name}, ?MODULE, [ChildSpecs, Supervisor], []).

get(Name)
    when is_atom(Name) ->
    gen_server:call(Name, get).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([ChildSpecs, Supervisor]) ->
    self() ! {start, ChildSpecs},
    {ok, #state{supervisor = Supervisor}}.


handle_call(get, _, #state{pool = Pool,
                           count = Count} = State) ->
    I = erlang:get(current),
    erlang:put(current, if I == Count -> 1; true -> I + 1 end),
    Pid = erlang:element(I, Pool),
    case erlang:is_process_alive(Pid) of
        true ->
            {reply, Pid, State};
        false ->
            {NewI, NewState} = update(I, State),
            if
                NewState#state.count == 0 ->
                    {stop, {error, noproc}, {error, noproc}, NewState};
                true ->
                    {reply, erlang:element(NewI, NewState#state.pool), NewState}
            end
    end;

handle_call(Request, _, State) ->
    ?LOG_WARN("Unknown call \"~p\"", [Request]),
    {stop, cloudi_string:format("Unknown call \"~p\"", [Request]),
     error, State}.

handle_cast(Request, State) ->
    ?LOG_WARN("Unknown cast \"~p\"", [Request]),
    {noreply, State}.

handle_info({start, ChildSpecs}, #state{supervisor = Supervisor} = State) ->
    Pids = cloudi_lists:itera(fun(Child, L, F) ->
        case supervisor:start_child(Supervisor, Child) of
            {ok, Pid} ->
                F([Pid | L]);
            {ok, Pid, _} ->
                F([Pid | L]);
            {error, _} = Error ->
                {stop, Error, State}
        end
    end, [], ChildSpecs),
    if
        Pids == [] ->
            {stop, {error, noproc}, State};
        is_list(Pids) ->
            erlang:put(current, 1),
            {noreply, State#state{pool = erlang:list_to_tuple(Pids),
                                  count = erlang:length(Pids)}};
        true ->
            Pids
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

update(I, #state{supervisor = Supervisor} = State) ->
    Pids = lists:foldl(fun
        ({cloudi_pool, _, _, _}, L) ->
            L;
        ({_, undefined, _, _}, L) ->
            L;
        ({_, Pid, _, _}, L) ->
            [Pid | L]
    end, [], supervisor:which_children(Supervisor)),
    Count = erlang:length(Pids),
    NewState = State#state{pool = erlang:list_to_tuple(Pids),
                           count = Count},
    if
        I > Count ->
            erlang:put(current, if 1 == Count -> 1; true -> 2 end),
            {1, NewState};
        true ->
            {I, NewState}
    end.

