%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Local Variable Pool Supervisor==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2015, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2015 Michael Truog
%%% @version 0.1.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(varpool_sup).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([start_link/4,
         stop_link/1]).

%% internal callbacks
-export([monitor_up/6,
         stop_link_process/0]).

%% supervisor callbacks
-export([init/1]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Start the Pool supervisor.===
%% @end
%%-------------------------------------------------------------------------

-spec start_link(Parent :: pid(),
                 MaxR :: non_neg_integer(),
                 MaxT :: pos_integer(),
                 Pool :: nonempty_list()) ->
    {'ok', pid()} |
    {'error', any()}.

start_link(Parent, MaxR, MaxT, [_ | _] = Pool)
    when is_pid(Parent), is_integer(MaxR), MaxR >= 0,
         is_integer(MaxT), MaxT >= 1 ->
    supervisor:start_link(?MODULE, [Parent, MaxR, MaxT, Pool]).

%%-------------------------------------------------------------------------
%% @doc
%% ===Stop the Pool supervisor.===
%% @end
%%-------------------------------------------------------------------------

-spec stop_link(Supervisor :: pid()) ->
    'ok'.

stop_link(Supervisor)
    when is_pid(Supervisor) ->
    erlang:unlink(Supervisor),
    {ok, _} = supervisor:start_child(Supervisor,
                                     {shutdown,
                                      {?MODULE, stop_link_process, []},
                                      permanent, brutal_kill, worker, []}),
    ok.

%%-------------------------------------------------------------------------
%% @hidden
%% Internal callback
%% @end
%%-------------------------------------------------------------------------

-spec monitor_up(Parent :: pid(),
                 M :: module(),
                 F :: atom(),
                 A :: list(),
                 Group :: any(),
                 I :: non_neg_integer()) ->
    any().

monitor_up(Parent, M, F, A, Group, I) ->
    case erlang:apply(M, F, A) of
        {ok, Child} = Success ->
            monitor_up_message(Parent, Child, Group, I),
            Success;
        {ok, Child, _Info} = Success ->
            monitor_up_message(Parent, Child, Group, I),
            Success;
        ignore = Ignore ->
            Ignore;
        Error ->
            Error
    end.

%%-------------------------------------------------------------------------
%% @hidden
%% Internal callback
%% @end
%%-------------------------------------------------------------------------

-spec stop_link_process() ->
    {ok, pid()}.

stop_link_process() ->
    Child = erlang:spawn_link(fun() ->
        erlang:exit(shutdown)
    end),
    {ok, Child}.

%%%------------------------------------------------------------------------
%%% Callback functions from supervisor
%%%------------------------------------------------------------------------

init([Parent, MaxR, MaxT, Pool]) ->
    {ok, {{one_for_one, MaxR, MaxT}, child_specs(Pool, Parent)}}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

monitor_up_message(Parent, Child, Group, I) ->
    Parent ! {'UP', self(), process, Child, {Group, I}}.

child_specs([], ChildSpecs, _) ->
    ChildSpecs;
child_specs([{Group, Count, M, F, A, Shutdown} | Pool], ChildSpecs, Parent) ->
    NewChildSpecs = case module_behaviour(M) of
        undefined ->
            child_spec(Count - 1, Group, Parent, M, F, A,
                       Shutdown, worker, [M]);
        gen_event ->
            child_spec(Count - 1, Group, Parent, M, F, A,
                       Shutdown, worker, dynamic);
        supervisor ->
            child_spec(Count - 1, Group, Parent, M, F, A,
                       infinity, supervisor, [M])
    end ++ ChildSpecs,
    child_specs(Pool, NewChildSpecs, Parent).

child_specs(Pool, Parent) ->
    child_specs(Pool, [], Parent).

child_spec_entry(I, Group, Parent, M, F, A, Shutdown, Type, Modules) ->
    {{Group, I}, {?MODULE, monitor_up, [Parent, M, F, A, Group, I]},
     permanent, Shutdown, Type, Modules}.

child_spec(0 = I, Group, Parent, M, F, A, Shutdown, Type, Modules) ->
    [child_spec_entry(I, Group, Parent, M, F, A, Shutdown, Type, Modules)];
child_spec(I, Group, Parent, M, F, A, Shutdown, Type, Modules) ->
    [child_spec_entry(I, Group, Parent, M, F, A, Shutdown, Type, Modules) |
     child_spec(I - 1, Group, Parent, M, F, A, Shutdown, Type, Modules)].

special_behaviour([]) ->
    undefined;
special_behaviour([supervisor = Behaviour | _]) ->
    Behaviour;
special_behaviour([gen_event = Behaviour | _]) ->
    Behaviour;
special_behaviour([_ | L]) ->
    special_behaviour(L).

module_behaviour(M) ->
    Attributes = M:module_info(attributes),
    case lists:keyfind(behaviour, 1, Attributes) of
        false ->
            case lists:keyfind(behavior, 1, Attributes) of
                false ->
                    undefined;
                {_, L} ->
                    special_behaviour(L)
            end;
        {_, L} ->
            special_behaviour(L)
    end.
