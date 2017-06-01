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
%%% MIT License
%%%
%%% Copyright (c) 2017 Michael Truog <mjtruog at gmail dot com>
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

