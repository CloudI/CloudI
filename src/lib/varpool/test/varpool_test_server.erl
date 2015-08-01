%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Local Variable Pool Test Server==
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
