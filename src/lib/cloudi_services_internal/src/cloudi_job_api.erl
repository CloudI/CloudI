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
%%% Copyright (c) 2011-2012, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011-2012 Michael Truog
%%% @version 0.2.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_job_api).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_job).

%% external interface

%% cloudi_job callbacks
-export([cloudi_job_init/3,
         cloudi_job_handle_request/11,
         cloudi_job_handle_info/3,
         cloudi_job_terminate/2]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-record(state,
    {
        functions = trie:new([
            {"acl_add",
             {fun cloudi_configurator:acl_add/2, 2}},
            {"acl_remove",
             {fun cloudi_configurator:acl_remove/2, 2}},
            {"jobs_add",
             {fun cloudi_configurator:jobs_add/2, 2}},
            {"jobs_remove",
             {fun cloudi_configurator:jobs_remove/2, 2}},
            {"jobs_restart",
             {fun cloudi_configurator:jobs_restart/2, 2}},
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
             {fun cloudi_nodes:nodes/1, 1}},
            {"loglevel_set",
             {fun loglevel_set/2, 2}},
            {"log_redirect",
             {fun log_redirect/2, 2}},
            {"code_path_add",
             {fun code_path_add/2, 2}},
            {"code_path_remove",
             {fun code_path_remove/2, 2}},
            {"code_path",
             {fun code_path/1, 1}}
        ]),
        formats = trie:new([
            {"erlang",
             fun format_erlang/4},
            {"json_rpc",
             fun format_json_rpc/4}
        ]),
        suffix_index = undefined
    }).
 
%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_job
%%%------------------------------------------------------------------------

cloudi_job_init(_Args, Prefix, Dispatcher) ->
    % names are [prefix]format/[method] (i.e., request format)
    cloudi_job:subscribe(Dispatcher, "erlang/acl_add"),
    cloudi_job:subscribe(Dispatcher, "erlang/acl_add/post"),
    cloudi_job:subscribe(Dispatcher, "erlang/acl_remove"),
    cloudi_job:subscribe(Dispatcher, "erlang/acl_remove/post"),
    cloudi_job:subscribe(Dispatcher, "erlang/jobs_add"),
    cloudi_job:subscribe(Dispatcher, "erlang/jobs_add/post"),
    cloudi_job:subscribe(Dispatcher, "erlang/jobs_remove"),
    cloudi_job:subscribe(Dispatcher, "erlang/jobs_remove/post"),
    cloudi_job:subscribe(Dispatcher, "erlang/jobs_restart"),
    cloudi_job:subscribe(Dispatcher, "erlang/jobs_restart/post"),
    cloudi_job:subscribe(Dispatcher, "erlang/jobs"),
    cloudi_job:subscribe(Dispatcher, "erlang/jobs/get"),
    cloudi_job:subscribe(Dispatcher, "erlang/nodes_add"),
    cloudi_job:subscribe(Dispatcher, "erlang/nodes_add/post"),
    cloudi_job:subscribe(Dispatcher, "erlang/nodes_remove"),
    cloudi_job:subscribe(Dispatcher, "erlang/nodes_remove/post"),
    cloudi_job:subscribe(Dispatcher, "erlang/nodes_alive"),
    cloudi_job:subscribe(Dispatcher, "erlang/nodes_alive/get"),
    cloudi_job:subscribe(Dispatcher, "erlang/nodes_dead"),
    cloudi_job:subscribe(Dispatcher, "erlang/nodes_dead/get"),
    cloudi_job:subscribe(Dispatcher, "erlang/nodes"),
    cloudi_job:subscribe(Dispatcher, "erlang/nodes/get"),
    cloudi_job:subscribe(Dispatcher, "erlang/loglevel_set"),
    cloudi_job:subscribe(Dispatcher, "erlang/loglevel_set/post"),
    cloudi_job:subscribe(Dispatcher, "erlang/log_redirect"),
    cloudi_job:subscribe(Dispatcher, "erlang/log_redirect/post"),
    cloudi_job:subscribe(Dispatcher, "erlang/code_path_add"),
    cloudi_job:subscribe(Dispatcher, "erlang/code_path_add/post"),
    cloudi_job:subscribe(Dispatcher, "erlang/code_path_remove"),
    cloudi_job:subscribe(Dispatcher, "erlang/code_path_remove/post"),
    cloudi_job:subscribe(Dispatcher, "erlang/code_path"),
    cloudi_job:subscribe(Dispatcher, "erlang/code_path/get"),
    cloudi_job:subscribe(Dispatcher, "json_rpc/"),
    cloudi_job:subscribe(Dispatcher, "json_rpc//post"),
    {ok, #state{suffix_index = erlang:length(Prefix) + 1}}.

cloudi_job_handle_request(_Type, _Name, Pattern, _RequestInfo, Request,
                          Timeout, _Priority, _TransId, _Pid,
                          #state{suffix_index = SuffixIndex,
                                 functions = Functions,
                                 formats = Formats} = State, _Dispatcher) ->
    {Format, Suffix} = cloudi_string:splitl(
        $/, string:sub_string(Pattern, SuffixIndex)
    ),
    FunctionArity = case cloudi_string:beforel($/, Suffix, input) of
        [] ->
            undefined;
        Method ->
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
            case F(cloudi_string:binary_to_term(Input), Timeout) of
                Result when is_binary(Result) ->
                    Result;
                Result ->
                    cloudi_string:term_to_binary(Result)
            end;
        is_list(Input) ->
            case F(cloudi_string:list_to_term(Input), Timeout) of
                Result when is_binary(Result) ->
                    erlang:binary_to_list(Result);
                Result ->
                    cloudi_string:term_to_list(Result)
            end
    end;

format_erlang({F, 1}, Input, Timeout, _) ->
    if
        is_binary(Input) ->
            case F(Timeout) of
                Result when is_binary(Result) ->
                    Result;
                Result ->
                    cloudi_string:term_to_binary(Result)
            end;
        is_list(Input) ->
            case F(Timeout) of
                Result when is_binary(Result) ->
                    erlang:binary_to_list(Result);
                Result ->
                    cloudi_string:term_to_list(Result)
            end
    end.

-spec format_json_rpc('undefined',
                      Input :: binary() | string(),
                      Timeout :: integer(),
                      Functions :: any()) -> binary().

format_json_rpc(undefined, Input, Timeout, Functions) ->
    {Method, Params, Id} = cloudi_json_rpc:request_to_term(Input),
    try (case trie:fetch(erlang:binary_to_list(Method), Functions) of
        {F, 1} when Params == [] ->
            F(Timeout);
        {F, 2} when length(Params) == 1 ->
            F(cloudi_string:binary_to_term(erlang:hd(Params)), Timeout)
         end) of
        Result when is_binary(Result) ->
            cloudi_json_rpc:response_to_json(
                Result, Id
            );
        Result ->
            cloudi_json_rpc:response_to_json(
                cloudi_string:term_to_binary(Result), Id
            )
    catch
        _:Error ->
            cloudi_json_rpc:response_to_json(
                null, 0, cloudi_string:term_to_binary(Error), Id
            )
    end.

loglevel_set(Level, _Timeout) ->
    cloudi_logger:change_loglevel(Level).

log_redirect(Node, _Timeout) ->
    cloudi_nodes:logger_redirect(Node).

code_path_add(Dir, _Timeout) ->
    code:add_pathz(Dir).

code_path_remove(Dir, _Timeout) ->
    code:del_path(Dir).

code_path(_Timeout) ->
    code:get_path().

