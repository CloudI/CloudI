%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CPG Remote Node Monitor Process.==
%%% Handle remote node pid monitors to reduce the messages the cpg
%%% scope process needs to manage.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2017, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2017 Michael Truog
%%% @version 1.7.1 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cpg_node_monitor).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([start_link/1,
         stop_link/1,
         died/1,
         add/2,
         remove/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         code_change/3, terminate/2]).

-record(state,
    {
        parent :: pid(),
        monitors :: #{pid() := reference()}
    }).

-type process() :: pid().
-export_type([process/0]).

-include("cpg_logging.hrl").
-include("cpg_constants.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-spec start_link(Pid :: pid()) ->
    {ok, pid()} | {error, any()}.

start_link(Pid) ->
    gen_server:start_link(?MODULE, [self(), Pid], []).

-spec stop_link(Process :: process()) ->
    ok.

stop_link(Process) ->
    true = erlang:unlink(Process),
    true = erlang:exit(Process, normal),
    ok.

-spec died(Process :: process()) ->
    list(pid()).

died(Process) ->
    monitors_flush(gen_server:call(Process, died)).

-spec add(Process :: process(),
          Pid :: pid()) ->
    ok.

add(Process, Pid) ->
    gen_server:cast(Process, {add, Pid}).

-spec remove(Process :: process(),
             Pid :: pid()) ->
    ok.

remove(Process, Pid) ->
    gen_server:cast(Process, {remove, Pid}).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([Parent, Pid]) ->
    MonitorRef = erlang:monitor(process, Pid),
    {ok, #state{parent = Parent,
                monitors = #{Pid => MonitorRef}}}.

handle_call(died, {Client, _},
            #state{parent = Parent,
                   monitors = Monitors} = State) ->
    true = Client =:= Parent,
    {stop, normal, maps:keys(Monitors), State};

handle_call(Request, _, State) ->
    {stop, lists:flatten(io_lib:format("Unknown call \"~p\"", [Request])),
     error, State}.

handle_cast({add, Pid},
            #state{monitors = Monitors} = State) ->
    case maps:find(Pid, Monitors) of
        {ok, _} ->
            {noreply, State};
        error ->
            MonitorRef = erlang:monitor(process, Pid),
            {noreply,
             State#state{monitors = maps:put(Pid, MonitorRef, Monitors)}}
    end;

handle_cast({remove, Pid},
            #state{monitors = Monitors} = State) ->
    case maps:find(Pid, Monitors) of
        {ok, MonitorRef} ->
            true = erlang:demonitor(MonitorRef, [flush]),
            {noreply, State#state{monitors = maps:remove(Pid, Monitors)}};
        error ->
            {noreply, State}
    end;

handle_cast(Request, State) ->
    {stop, lists:flatten(io_lib:format("Unknown cast \"~p\"", [Request])),
     State}.

handle_info({'DOWN', _, process, _, _} = DOWN, State) ->
    {noreply, monitors_send(DOWN, [], State)};

handle_info(Request, State) ->
    {stop, lists:flatten(io_lib:format("Unknown info \"~p\"", [Request])),
     State}.

terminate(_, #state{}) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

monitors_send({'DOWN', _MonitorRef, process, Pid, Info}, DOWNS,
              #state{parent = Parent,
                     monitors = Monitors} = State) ->
    NewDOWNS = [{Pid, {exit, Info}} | DOWNS],
    NewState = State#state{monitors = maps:remove(Pid, Monitors)},
    receive
        {'DOWN', _, process, _, _} = DOWN ->
            monitors_send(DOWN, NewDOWNS, NewState)
    after
        ?MONITORS_ACCUMULATE_DELAY ->
            Parent ! {'DOWNS', NewDOWNS},
            NewState
    end.

monitors_flush(Pids) ->
    receive
        {'DOWNS', PidReasons} ->
            monitors_flush(lists:foldl(fun({Pid, _}, NewPids) ->
                [Pid | NewPids]
            end, Pids, PidReasons))
    after
        0 ->
            Pids
    end.

