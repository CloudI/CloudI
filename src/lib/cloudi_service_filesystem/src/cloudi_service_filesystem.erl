%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Filesystem==
%%% A service that caches filesystem data for quick responses.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2011-2013, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2011-2013 Michael Truog
%%% @version 1.2.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_filesystem).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_service).

%% external interface

%% cloudi_service callbacks
-export([cloudi_service_init/3,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/2]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-define(DEFAULT_USE_HTTP_GET_SUFFIX,      true). % get as a name suffix

-record(state,
    {
        directory,
        files
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, Prefix, Dispatcher) ->
    Defaults = [
        {directory,              undefined},
        {use_http_get_suffix,    ?DEFAULT_USE_HTTP_GET_SUFFIX}],
    [Directory, UseHttpGetSuffix] =
        cloudi_proplists:take_values(Defaults, Args),
    true = is_list(Directory),
    Files = filelib:fold_files(Directory, ".*", true, fun(FilePath, D1) ->
        {ok, Contents} = file:read_file(FilePath),
        FileName = case lists:split(erlang:length(Directory), FilePath) of
            {Directory, [$/ | S]} ->
                S;
            {Directory, S} ->
                S
        end,
        D2 = if
            FileName == "index.htm"; FileName == "index.html" ->
                if
                    UseHttpGetSuffix =:= true ->
                        cloudi_service:subscribe(Dispatcher, "/get"),
                        cloudi_x_trie:store(Prefix ++ "/get", Contents, D1);
                    true ->
                        cloudi_service:subscribe(Dispatcher, ""),
                        cloudi_x_trie:store(Prefix, Contents, D1)
                end;
            true ->
                D1
        end,
        Name = if
            UseHttpGetSuffix =:= true ->
                FileName ++ "/get";
            true ->
                FileName
        end,
        cloudi_service:subscribe(Dispatcher, Name),
        cloudi_x_trie:store(Prefix ++ Name, Contents, D2)
    end, cloudi_x_trie:new()),
    {ok, #state{directory = Directory,
                files = Files}}.

cloudi_service_handle_request(_Type, _Name, Pattern, _RequestInfo, _Request,
                              _Timeout, _Priority, _TransId, _Pid,
                              #state{files = Files} = State, _Dispatcher) ->
    {reply, cloudi_x_trie:fetch(Pattern, Files), State}.

cloudi_service_handle_info(Request, State, _) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(_, #state{}) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

