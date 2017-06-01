%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Local Variable Pool Test Server==
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

-module(varpool_test_server).
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
    application:start(varpool),
    P0 = varpool:new([{groups,
                       [{group_0, {varpool_test_server, start_link, []}, 
                         [{count_hash, 4}, {count_random, 4}]}
                        ]}]),
    P3 = lists:foldl(fun(_, P1) ->
        receive
            Up0 ->
                {'UP', _, process, _, {_, _}} = Up0,
                {updated, P2} = varpool:update(Up0, P1),
                P2
        end
    end, P0, lists:seq(1, 16)),
    Child0 = varpool:get(group_0, 0, P3),
    Child1 = varpool:get(group_0, 1, P3),
    true = (Child0 /= Child1),
    true = is_pid(Child0),
    true = is_pid(Child1),
    pong = varpool_test_server:ping(Child0),
    pong = varpool_test_server:ping(Child1),
    erlang:exit(Child0, kill),
    P5 = receive
        Down0 ->
            {'DOWN', _, process, _, _} = Down0,
            {updated, P4} = varpool:update(Down0, P3),
            P4
    end,
    PN = receive
        Up1 ->
            {'UP', _, process, _, {_, _}} = Up1,
            {updated, P6} = varpool:update(Up1, P5),
            P6
    end,
    Child0New = varpool:get(group_0, 0, PN),
    true = (Child0 /= Child0New),
    true = is_pid(Child0New),
    ok = varpool:destroy(PN),
    ok.

-endif.
