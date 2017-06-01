%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Supervisor Test Server==
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

-module(supool_test_server).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([start_link/0,
         ping/1]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

start_link() ->
    {ok, erlang:spawn_link(fun loop/0)}.

ping(Pid) ->
    Pid ! {self(), ping},
    receive
        pong ->
            pong
    after
        5000 ->
            pang
    end.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

loop() ->
    receive
        {Pid, ping} ->
            Pid ! pong
    end,
    loop().

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

internal_test() ->
    %application:start(sasl),
    application:start(supool),
    ChildSpec = {undefined, {supool_test_server, start_link, []},
                 permanent, brutal_kill, worker, [supool_test_server]},
    {ok, Supervisor} = supool:start_link(group_0, 2, ChildSpec),
    erlang:unlink(Supervisor),
    Child0 = supool:get(group_0),
    Child1 = supool:get(group_0),
    true = (Child0 /= Child1),
    true = is_pid(Child0),
    true = is_pid(Child1),
    pong = supool_test_server:ping(Child0),
    pong = supool_test_server:ping(Child1),
    erlang:exit(Child0, kill),
    receive after 500 -> ok end,
    Child0New = supool:get(group_0),
    true = (Child0 /= Child0New),
    true = is_pid(Child0New),
    erlang:exit(erlang:whereis(group_0), kill),
    receive after 500 -> ok end,
    Child0New = supool:get(group_0),
    Child1 = supool:get(group_0),
    Child0New = supool:get(group_0),
    [{2, Child1, _, _},
     {1, Child0New, _, _},
     {supool, _, _, _}] = supervisor:which_children(Supervisor),
    [Child0New, Child1] = supool_sup:which_children(Supervisor),
    erlang:exit(Supervisor, shutdown),
    ok.

-endif.
