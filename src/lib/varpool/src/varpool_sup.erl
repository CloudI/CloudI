%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Local Variable Pool Supervisor==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2015-2017 Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2015-2017 Michael Truog
%%% @version 1.7.1 {@date} {@time}
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
    special_behaviour(reltool_util:module_behaviours(M)).
