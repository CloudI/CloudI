%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Configuration API==
%%% A service that exposes dynamic configuration of CloudI.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2011, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011 Michael Truog
%%% @version 0.1.4 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_job_api).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_job).

%% external interface

%% cloudi_job callbacks
-export([cloudi_job_init/3,
         cloudi_job_handle_request/8,
         cloudi_job_handle_info/3,
         cloudi_job_terminate/2]).

-include("cloudi_logger.hrl").

-record(state,
    {
        functions = trie:new([{"acl_add",
                               {fun cloudi_configurator:acl_add/2, 2}},
                              {"acl_remove",
                               {fun cloudi_configurator:acl_remove/2, 2}},
                              {"jobs_add",
                               {fun cloudi_configurator:jobs_add/2, 2}},
                              {"jobs_remove",
                               {fun cloudi_configurator:jobs_remove/2, 2}},
                              {"jobs",
                               {fun cloudi_configurator:jobs/1, 1}},
                              {"nodes_add",
                               {fun cloudi_configurator:nodes_add/2, 2}},
                              {"nodes_remove",
                               {fun cloudi_configurator:nodes_remove/2, 2}},
                              {"nodes_alive",
                               {fun cloudi_nodes:alive/1, 1}},
                              {"nodes_dead",
                               {fun cloudi_nodes:dead/1, 1}},
                              {"nodes",
                               {fun cloudi_nodes:nodes/1, 1}}]),
        formats = trie:new([{"erlang", fun format_erlang/4},
                            {"json_rpc", fun format_json_rpc/4}])
    }).
 
%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_job
%%%------------------------------------------------------------------------

cloudi_job_init(_Args, _Prefix, Dispatcher) ->
    % names are [prefix]format/[method] (i.e., request format)
    cloudi_job:subscribe(Dispatcher, "erlang/acl_add"),
    cloudi_job:subscribe(Dispatcher, "erlang/acl_remove"),
    cloudi_job:subscribe(Dispatcher, "erlang/jobs_add"),
    cloudi_job:subscribe(Dispatcher, "erlang/jobs_remove"),
    cloudi_job:subscribe(Dispatcher, "erlang/nodes_add"),
    cloudi_job:subscribe(Dispatcher, "erlang/nodes_remove"),
    cloudi_job:subscribe(Dispatcher, "erlang/nodes_alive"),
    cloudi_job:subscribe(Dispatcher, "erlang/nodes_dead"),
    cloudi_job:subscribe(Dispatcher, "erlang/nodes"),
    cloudi_job:subscribe(Dispatcher, "json_rpc/"),
    {ok, #state{}}.

cloudi_job_handle_request(_Type, Name, Request, Timeout, _TransId, _Pid,
                          #state{functions = Functions,
                                 formats = Formats} = State, _Dispatcher) ->
    {Path1, Method} = string2:splitr($/, Name),
    {_, Format} = string2:splitr($/, Path1),
    FunctionArity = if
        Method == [] ->
            undefined;
        true ->
            trie:fetch(Method, Functions)
    end,
    FormatF = trie:fetch(Format, Formats),
    Response = FormatF(FunctionArity, Request, Timeout, Functions),
    {reply, cloudi_response:new(Request, Response), State}.

cloudi_job_handle_info(Request, State, _) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_job_terminate(_, #state{}) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

format_erlang({F, 2}, Input, Timeout, _) ->
    if
        is_binary(Input) ->
            string2:term_to_binary(F(string2:binary_to_term(Input), Timeout));
        is_list(Input) ->
            string2:term_to_list(F(string2:list_to_term(Input), Timeout))
    end;

format_erlang({F, 1}, Input, Timeout, _) ->
    if
        is_binary(Input) ->
            string2:term_to_binary(F(Timeout));
        is_list(Input) ->
            string2:term_to_list(F(Timeout))
    end.

format_json_rpc(undefined, Input, Timeout, Functions) ->
    {Method, Params, Id} = cloudi_json_rpc:request_to_term(Input),
    try (case trie:fetch(erlang:binary_to_list(Method), Functions) of
        {F, 1} when Params == [] ->
            F(Timeout);
        {F, 2} when length(Params) == 1 ->
            F(string2:binary_to_term(erlang:hd(Params)), Timeout)
         end) of
        Result when is_binary(Result) ->
            cloudi_json_rpc:response_to_json(Result, Id);
        Result ->
            cloudi_json_rpc:response_to_json(string2:term_to_binary(Result), Id)
    catch
        _:Error ->
            cloudi_json_rpc:response_to_json(null,
                                             string2:term_to_binary(Error), Id)
    end.

