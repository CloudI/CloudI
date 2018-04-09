%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Router Service Client==
%%% @end
%%%
%%% MIT License
%%%
%%% Copyright (c) 2018 Michael Truog <mjtruog at protonmail dot com>
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
%%% @copyright 2018 Michael Truog
%%% @version 1.7.4 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_router_client).
-author('mjtruog at protonmail dot com').

%% external interface
-export([forward/11,
         new/3]).

-define(DEFAULT_TYPE,                               ssh).

-type state() :: cloudi_service_router_ssh_client:state().
-type options() :: list({type, ssh}) |
                   cloudi_service_router_ssh_client:options().
-export_type([state/0,
              options/0]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-spec forward(Type :: cloudi_service:request_type(),
              Name :: cloudi_service:service_name(),
              Pattern :: cloudi_service:service_name_pattern(),
              NewName :: cloudi_service:service_name(),
              RequestInfo :: cloudi_service:request_info(),
              Request :: cloudi_service:request(),
              Timeout :: cloudi_service:timeout_value_milliseconds(),
              Priority :: cloudi_service:priority_value(),
              TransId :: cloudi_service:trans_id(),
              Source :: cloudi_service:source(),
              State :: state()) ->
    ok | timeout.

forward(Type, Name, Pattern, NewName, RequestInfo, Request,
        Timeout, Priority, TransId, Source, State)
    when element(1, State) =:= ssh_client ->
    cloudi_service_router_ssh_client:forward(Type, Name, Pattern, NewName,
                                             RequestInfo, Request,
                                             Timeout, Priority,
                                             TransId, Source, State).

-spec new(Options :: options() | undefined,
          Environment :: cloudi_environment:lookup(),
          SSH :: cloudi_service_router_ssh_server:state() | undefined) ->
    state() | undefined.

new(undefined, _, _) ->
    undefined;
new(Options, Environment, SSH)
    when is_list(Options) ->
    Defaults = [
        {type,                          ?DEFAULT_TYPE}],
    [Type | OptionsRest] = cloudi_proplists:take_values(Defaults, Options),
    if
        Type =:= ssh ->
            cloudi_service_router_ssh_client:new(OptionsRest, Environment, SSH)
    end.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

