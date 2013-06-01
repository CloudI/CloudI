%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CPG ETS Cache.==
%%% Use ETS to avoid contention for CPG scope processes.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2013, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2013 Michael Truog
%%% @version 1.2.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cpg_ets).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).

%% external interface
-export([start_link/0,
         table_create/0,
         table_owners/2,
         get/1,
         put/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("cpg_constants.hrl").

-record(state,
    {
        parent_sup
    }).

-ifdef(CPG_ETS_CACHE).
-define(CPG_ETS_TABLE, ?CPG_ETS_CACHE).
-else.
-define(CPG_ETS_TABLE, ?MODULE).
-endif.

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [self()], []).

table_create() ->
    ets:new(?CPG_ETS_TABLE,
            [set,
             public,
             named_table,
             {keypos, 1},
             {heir, none},
             {read_concurrency, true}]),
    ok.

table_owners(OwnerPid, HeirPid)
    when is_pid(OwnerPid), is_pid(HeirPid) ->
    HeirData = undefined,
    true = ets:setopts(?CPG_ETS_TABLE, [{heir, HeirPid, HeirData}]),
    GiftData = undefined,
    true = ets:give_away(?CPG_ETS_TABLE, OwnerPid, GiftData),
    ok.

get(Scope)
    when is_atom(Scope) ->
    ets:lookup_element(?CPG_ETS_TABLE, Scope, 2).

put(Scope, Groups)
    when is_atom(Scope) ->
    true = ets:insert(?CPG_ETS_TABLE, {Scope, Groups}),
    Groups.

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([ParentSup]) ->
    {ok, #state{parent_sup = ParentSup}}.

handle_call(_, _, State) ->
    {stop, unknown_call, error, State}.

handle_cast(_, State) ->
    {stop, unknown_cast, State}.

handle_info({'ETS-TRANSFER', _, ParentSup, _GiftData},
            #state{parent_sup = ParentSup} = State) ->
    {noreply, State};

handle_info({'ETS-TRANSFER', _, _OldPid, HeirData},
            #state{parent_sup = ParentSup} = State) ->
    NewHeirPid = get_heir(ParentSup),
    true = is_pid(NewHeirPid),
    ets:setopts(?CPG_ETS_TABLE, [{heir, NewHeirPid, HeirData}]),
    {noreply, State};

handle_info(_, State) ->
    {stop, unknown_info, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

get_heir(ParentSup) ->
    Self = self(),
    Choices = lists:filter(fun({_Id, Pid, _Type, Modules}) ->
        if
            [?MODULE] == Modules, Self /= Pid, is_pid(Pid) ->
                is_process_alive(Pid);
            true ->
                false
        end
    end, supervisor:which_children(ParentSup)),
    case Choices of
        [] ->
            undefined;
        [{_, Pid, _, _} | _] ->
            Pid
    end.

