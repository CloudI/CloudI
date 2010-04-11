%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Cloudi JSON RPC Server Supervisor==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2010, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2010 Michael Truog
%%% @version 0.0.10 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloud_jsonrpc_sup).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([start_link/1,
         init/1,
         dispatch/1]).

-include("cloud_configuration.hrl").

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Start the Cloudi JSON RPC supervisor.===
%% @end
%%-------------------------------------------------------------------------

start_link(Config) when is_record(Config, config) ->
    supervisor:start_link(?MODULE, [Config#config.json_rpc_port]).

%%%------------------------------------------------------------------------
%%% Callback functions from supervisor
%%%------------------------------------------------------------------------

init([Port]) ->
    MaxRestarts = 5,
    MaxTime = 60, % seconds (1 minute)
    {ok,
     {{one_for_all, MaxRestarts, MaxTime},
      [{mochiweb_http,
        {mochiweb_http, start, [[{loop, {?MODULE, dispatch}}, {port, Port}]]},
        permanent, 2000, worker, [mochiweb_http]},
       {cloud_jsonrpc,
        {cloud_jsonrpc, start_link, [cloud_api, <<"0.0.10">>]},
        permanent, 2000, worker, [cloud_jsonrpc]}]}}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

dispatch(Req) ->
    case Req:get(method) of
        'GET' ->
            handle_json(Req);
        'POST' ->
            handle_json(Req);
        _ ->
            Headers = [{"Allow", "GET,POST"}],
            Req:respond({405, Headers, "405 Method Not Allowed\r\n"})
    end.

handle_json(Req) ->
    case rfc4627_jsonrpc_mochiweb:handle("/jsonrpc", Req) of
         {ok, Response} ->
             Req:respond(Response);
         no_match ->
             Req:respond({404, [], "404 Method not found\r\n"})
    end.

