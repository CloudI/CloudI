%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Supervisor Test Server==
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
%%% @version 1.5.1 {@date} {@time}
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
