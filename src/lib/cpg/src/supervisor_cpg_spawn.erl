%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CPG Supervisor Spawn.==
%%% Helper process for cpg_supervisor.
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2013-2018 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2013-2018 Michael Truog
%%% @version 1.7.4 {@date} {@time}
%%%------------------------------------------------------------------------

-module(supervisor_cpg_spawn).
-author('mjtruog at protonmail dot com').

-behaviour(gen_server).

%% external interface
-export([start_link/4,
         start_nomad_child/4,
         start_remote_child/2,
         start_child/2,
         restart_child/2,
         delete_child/2,
         terminate_child/2,
         which_children/1,
         count_children/1,
         cpg_name/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("cpg_constants.hrl").

-record(state_nomad,
    {
        name,
        pid,
        restarts = [],
        max_r,
        max_t,
        child_spec
    }).

-record(state,
    {
        parent_sup,
        child_sup,
        nomads = dict:new()
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

start_link(SpawnName, MaxR, MaxT, ChildSpecs) ->
    gen_server:start_link(SpawnName, ?MODULE,
                          [self(), MaxR, MaxT, ChildSpecs], []).

start_nomad_child(_, MaxR, MaxT, _)
    when is_integer(MaxR) =:= false; is_integer(MaxT) =:= false ->
    erlang:exit(badarg);
start_nomad_child(Name, MaxR, MaxT, ChildSpec)
    when MaxR >= 0, MaxT > 0 ->
    gen_server:call(cpg_random_pid(Name),
                    {start_nomad_child, Name, MaxR, MaxT, ChildSpec}, infinity).

start_remote_child(Name, ChildSpec) ->
    gen_server:call(cpg_random_pid(Name),
                    {start_child, ChildSpec}, infinity).

start_child(Name, ChildSpec) ->
    gen_server:call(cpg_name(Name),
                    {start_child, ChildSpec}, infinity).

terminate_child(Name, Id) ->
    gen_server:call(cpg_name(Name),
                    {terminate_child, Id}, infinity).

delete_child(Name, Id) ->
    gen_server:call(cpg_name(Name),
                    {delete_child, Id}, infinity).

restart_child(Name, Id) ->
    gen_server:call(cpg_name(Name),
                    {restart_child, Id}, infinity).

which_children(Name) ->
    gen_server:call(cpg_name(Name),
                    which_children, infinity).

count_children(Name) ->
    gen_server:call(cpg_name(Name),
                    count_children, infinity).

cpg_name({global, _, _, _}) ->
    erlang:exit(badarg);
cpg_name({local, _, _, Instances})
    when is_integer(Instances) ->
    erlang:exit(badarg);
cpg_name({global, _, _}) ->
    erlang:exit(badarg);
cpg_name({local, _, Instances})
    when is_integer(Instances) ->
    erlang:exit(badarg);
cpg_name({local, _, _} = Name) ->
    {via, cpg, Name};
cpg_name({global, _}) ->
    erlang:exit(badarg);
cpg_name({local, _} = Name) ->
    {via, cpg, Name};
cpg_name({_, _} = Name) ->
    {via, cpg, Name};
cpg_name(Name) ->
    {via, cpg, Name}.

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([ParentSup, MaxR, MaxT, ChildSpecs]) ->
    self() ! {child_sup, MaxR, MaxT, ChildSpecs},
    {ok, #state{parent_sup = ParentSup}}.

handle_call({start_nomad_child, Name, MaxR, MaxT,
             {Id, StartFunc, _, Shutdown, Type, Modules} = ChildSpec}, _,
            #state{child_sup = ChildSup,
                   nomads = Nomads} = State) ->
    NomadChildSpec = {Id, StartFunc, temporary, Shutdown, Type, Modules},
    case supervisor:start_child(ChildSup, NomadChildSpec) of
        {ok, NomadChild} = Success ->
            NomadState = #state_nomad{name = Name,
                                      pid = NomadChild,
                                      max_r = MaxR,
                                      max_t = MaxT,
                                      child_spec = ChildSpec},
            MonitorRef = erlang:monitor(process, NomadChild),
            {reply, Success,
             State#state{nomads = dict:store(MonitorRef,
                                             NomadState,
                                             Nomads)}};
        {error, _} = Error ->
            {reply, Error, State}
    end;

handle_call({start_child, ChildSpec}, _,
            #state{child_sup = ChildSup} = State) ->
    {reply, supervisor:start_child(ChildSup, ChildSpec), State};

handle_call({terminate_child, Id}, _,
            #state{child_sup = ChildSup} = State) ->
    {reply, supervisor:terminate_child(ChildSup, Id), State};

handle_call({delete_child, Id}, _,
            #state{child_sup = ChildSup} = State) ->
    {reply, supervisor:delete_child(ChildSup, Id), State};

handle_call({restart_child, Id}, _,
            #state{child_sup = ChildSup} = State) ->
    {reply, supervisor:restart_child(ChildSup, Id), State};

handle_call(which_children, _,
            #state{child_sup = ChildSup} = State) ->
    {reply, supervisor:which_children(ChildSup), State};

handle_call(count_children, _,
            #state{child_sup = ChildSup} = State) ->
    {reply, supervisor:count_children(ChildSup), State};

handle_call(_, _, State) ->
    {stop, unknown_call, error, State}.

handle_cast({restart_nomad_child,
             #state_nomad{child_spec = {Id, StartFunc,
                                        _, Shutdown,
                                        Type, Modules}} = NomadState},
            #state{child_sup = ChildSup,
                   nomads = Nomads} = State) ->
    NomadChildSpec = {Id, StartFunc, temporary, Shutdown, Type, Modules},
    case supervisor:start_child(ChildSup, NomadChildSpec) of
        {ok, NomadChild} ->
            NewNomadState = NomadState#state_nomad{pid = NomadChild},
            MonitorRef = erlang:monitor(process, NomadChild),
            {noreply,
             State#state{nomads = dict:store(MonitorRef,
                                             NewNomadState,
                                             Nomads)}};
        {error, Reason} ->
            report_error(start_error, Reason,
                         NomadState#state_nomad{pid = undefined}),
            {noreply, State}
    end;

handle_cast(_, State) ->
    {stop, unknown_cast, State}.

handle_info({child_sup, MaxR, MaxT, ChildSpecs},
            #state{parent_sup = ParentSup} = State) ->
    case supervisor:start_child(ParentSup,
                                {supervisor_cpg_sup,
                                 {supervisor_cpg_sup, start_link,
                                  [MaxR, MaxT, ChildSpecs]},
                                 permanent, infinity, supervisor,
                                 [supervisor_cpg_sup]}) of
        {ok, ChildSup} ->
            {noreply, State#state{child_sup = ChildSup}};
        {ok, ChildSup, _} ->
            {noreply, State#state{child_sup = ChildSup}};
        {error, Reason} ->
            {stop, Reason, State}
    end;

handle_info({'DOWN', MonitorRef, process, _Object, Reason},
            #state{nomads = Nomads} = State) ->
    % supervisor behaviour logs this as child_terminated
    nomad_restart(Reason, dict:fetch(MonitorRef, Nomads)),
    {noreply, State#state{nomads = dict:erase(MonitorRef, Nomads)}};

handle_info(_, State) ->
    {stop, unknown_info, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

cpg_random_pid({global, _, _, _}) ->
    erlang:exit(badarg);
cpg_random_pid({local, _, _, Instances})
    when is_integer(Instances) ->
    erlang:exit(badarg);
cpg_random_pid({global, _, _}) ->
    erlang:exit(badarg);
cpg_random_pid({local, _, Instances})
    when is_integer(Instances) ->
    erlang:exit(badarg);
cpg_random_pid({local, Scope, GroupName}) ->
    cpg_random_pid(Scope, GroupName, undefined);
cpg_random_pid({global, _}) ->
    erlang:exit(badarg);
cpg_random_pid({local, GroupName}) ->
    cpg_random_pid(?DEFAULT_SCOPE, GroupName, undefined);
cpg_random_pid({Scope, GroupName}) ->
    cpg_random_pid(Scope, GroupName, undefined);
cpg_random_pid(GroupName) ->
    cpg_random_pid(?DEFAULT_SCOPE, GroupName, undefined).

cpg_random_pid({global, _, _, _}, _) ->
    erlang:exit(badarg);
cpg_random_pid({local, _, _, Instances}, _)
    when is_integer(Instances) ->
    erlang:exit(badarg);
cpg_random_pid({global, _, _}, _) ->
    erlang:exit(badarg);
cpg_random_pid({local, _, Instances}, _)
    when is_integer(Instances) ->
    erlang:exit(badarg);
cpg_random_pid({local, Scope, GroupName}, Exclude) ->
    cpg_random_pid(Scope, GroupName, Exclude);
cpg_random_pid({global, _}, _) ->
    erlang:exit(badarg);
cpg_random_pid({local, GroupName}, Exclude) ->
    cpg_random_pid(?DEFAULT_SCOPE, GroupName, Exclude);
cpg_random_pid({Scope, GroupName}, Exclude) ->
    cpg_random_pid(Scope, GroupName, Exclude);
cpg_random_pid(GroupName, Exclude) ->
    cpg_random_pid(?DEFAULT_SCOPE, GroupName, Exclude).

cpg_random_pid(Scope, GroupName, undefined) ->
    case cpg:get_random_pid(Scope, GroupName) of
        {ok, _, Pid} ->
            Pid;
        {error, _} ->
            undefined
    end;
cpg_random_pid(Scope, GroupName, Exclude) ->
    case cpg:get_random_pid(Scope, GroupName, Exclude) of
        {ok, _, Pid} ->
            Pid;
        {error, _} ->
            undefined
    end.

nomad_restart(_,
              #state_nomad{max_r = 0}) ->
    ok;
nomad_restart(_,
              #state_nomad{child_spec = {_, _, temporary, _, _, _}}) ->
    ok;
nomad_restart(normal,
              #state_nomad{child_spec = {_, _, transient, _, _, _}}) ->
    ok;
nomad_restart(shutdown,
              #state_nomad{child_spec = {_, _, transient, _, _, _}}) ->
    ok;
nomad_restart({shutdown, _},
              #state_nomad{child_spec = {_, _, transient, _, _, _}}) ->
    ok;
nomad_restart(_Reason,
              #state_nomad{name = Name,
                           restarts = Restarts,
                           max_r = MaxR,
                           max_t = MaxT} = NomadState) ->
    Now = timestamp(),
    NewRestarts = lists:dropwhile(fun(T) ->
        timer:now_diff(Now, T) / 1000000 > MaxT
    end, Restarts) ++ [Now],
    NextSupPid = cpg_random_pid(Name, self()),
    if
        erlang:length(NewRestarts) > MaxR ->
            report_error(shutdown, reached_max_restart_intensity, NomadState);
        NextSupPid =:= undefined ->
            report_error(restart_error, noproc, NomadState);
        true ->
            gen_server:cast(NextSupPid,
                {restart_nomad_child,
                 NomadState#state_nomad{restarts = NewRestarts}})
    end.

% based on OTP supervisor code
report_error(Error, Reason, #state_nomad{name = SupName} = NomadState) ->
    ErrorMsg = [{supervisor, {via, cpg, SupName}},
                {errorContext, Error},
                {reason, Reason},
                {offender, extract_child(NomadState)}],
    error_logger:error_report(supervisor_report, ErrorMsg).

% based on OTP supervisor code
extract_child(#state_nomad{pid = Pid,
                           child_spec = {Id, StartFunc,
                                         Restart, Shutdown,
                                         Type, _}}) ->
    [{pid, Pid},
     {name, Id},
     {mfargs, StartFunc},
     {restart_type, Restart},
     {shutdown, Shutdown},
     {child_type, Type}].

-spec timestamp() -> erlang:timestamp().

timestamp() ->
    erlang:timestamp().
