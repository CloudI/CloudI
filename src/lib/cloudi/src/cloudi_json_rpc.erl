%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI JSON RPC==
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

-module(cloudi_json_rpc).
-author('mjtruog [at] gmail (dot) com').

%% external interface
-export([request_to_term/1,
         request_to_json/3,
         response_to_term/1,
         response_to_json/2,
         response_to_json/3]).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

request_to_term(Data) ->
    DataBin = if
        is_list(Data) ->
            erlang:list_to_binary(Data);
        is_binary(Data) ->
            Data
    end,
    RPC0 = jsx:json_to_term(DataBin),
    {value, {_, MethodBin}, RPC1} = lists:keytake(<<"method">>, 1, RPC0),
    {value, {_, ParamsBin}, RPC2} = lists:keytake(<<"params">>, 1, RPC1),
    {value, {_, Id}, _} = lists:keytake(<<"id">>, 1, RPC2),
    {MethodBin, ParamsBin, Id}.

request_to_json(Method, Params, Id) ->
    MethodBin = if
        is_atom(Method) ->
            erlang:list_to_binary(erlang:atom_to_list(Method));
        is_list(Method) ->
            erlang:list_to_binary(Method);
        is_binary(Method) ->
            Method
    end,
    ParamsBin = lists:map(fun(E) -> string2:term_to_binary(E) end, Params),
    jsx:term_to_json([{<<"method">>, MethodBin},
                      {<<"params">>, ParamsBin},
                      {<<"id">>, Id},
                      {<<"jsonrpc">>, <<"2.0">>}]).

response_to_term(Data) ->
    DataBin = if
        is_list(Data) ->
            erlang:list_to_binary(Data);
        is_binary(Data) ->
            Data
    end,
    RPC0 = jsx:json_to_term(DataBin),
    {value, {_, Result}, RPC1} = lists:keytake(<<"result">>, 1, RPC0),
    {value, {_, Error}, RPC2} = lists:keytake(<<"error">>, 1, RPC1),
    {value, {_, Id}, _} = lists:keytake(<<"id">>, 1, RPC2),
    {Result, Error, Id}.

response_to_json(Result, Id) ->
    response_to_json(Result, null, Id).

response_to_json(Result, Error, Id) ->
    jsx:term_to_json([{<<"result">>, Result}, % string result is binary
                      {<<"error">>, Error},
                      {<<"id">>, Id},
                      {<<"jsonrpc">>, <<"2.0">>}]).

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

