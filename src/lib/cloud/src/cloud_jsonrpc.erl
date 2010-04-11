%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Cloudi JSON RPC Server==
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

-module(cloud_jsonrpc).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).

%% external interface
-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("cloud_logger.hrl").
-include("../../rfc4627_jsonrpc/include/rfc4627_jsonrpc.hrl").

-record(state,
    {
    module = undefined,
    reply_key = undefined}).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

start_link(Module, Version)
    when is_atom(Module), is_binary(Version) ->
    gen_server:start_link(?MODULE, [Module, Version], []).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([Module, Version]) ->
    rfc4627_jsonrpc:register_service(self(),
                                     service_definition(Module, Version)),
    ReplyKey = list_to_binary(atom_to_list(Module) ++ ": "),
    {ok, #state{module = Module,
                reply_key = ReplyKey}}.
    
handle_call({jsonrpc, Name, _RequestInfo, Args}, _,
            #state{module = Module,
                   reply_key = ReplyKey} = State) ->
    Function = list_to_existing_atom(binary_to_list(Name)),
    case erlang:apply(Module, Function, Args) of
        ok ->
            {reply, {result, <<ReplyKey/binary, "ok">>}, State};
        {error, Reason} ->
            {reply, {error, <<ReplyKey/binary, Reason/binary>>}, State}
    end;
    
handle_call(Request, _, State) ->
    ?LOG_WARNING("Unknown call \"~p\"", [Request]),
    {stop, string_extensions:format("Unknown call \"~p\"", [Request]),
     error, State}.

handle_cast(Request, State) ->
    ?LOG_WARNING("Unknown cast \"~p\"", [Request]),
    {noreply, State}.

handle_info(Request, State) ->
    ?LOG_WARNING("Unknown info \"~p\"", [Request]),
    {noreply, State}.

terminate(_, _) ->
    ok.

code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

service_definition(Module, ApiVersion)
    when is_atom(Module), is_binary(ApiVersion) ->
    AbstractCode = erlang:apply(Module, '$abstract_code$', []),
    rfc4627_jsonrpc:service(list_to_binary(atom_to_list(Module)),
                            module_uuid(cloud_api),
                            ApiVersion,
                            module_function_calls(AbstractCode, [])).

module_function_calls([], FunctionList) ->
    FunctionList;

module_function_calls([{function, _, FName, _, ClauseList} | Remaining],
                      FunctionList) ->
    % simplify by not supporting API service function calls 
    % with more than one type specified for a parameter, so
    % only consider the first valid guard in any of the
    % function clause definitions
    [{clause, _, Args, [Guard], _} | _] = lists:filter(fun
    ({clause, _, _, [L | _] = GuardList, _}) ->
        if
            erlang:length(GuardList) == 1 ->
                lists:all(fun(E) ->
                    case E of
                        {call, _, {atom, _, is_alive}, _} ->
                            false;
                        {call, _, {atom, _, is_atom}, [{var, _, _}]} ->
                            false;
                        {call, _, {atom, _, is_builtin}, _} ->
                            false;
                        {call, _, {atom, _, is_float}, [{var, _, _}]} ->
                            false;
                        {call, _, {atom, _, is_function}, _} ->
                            false;
                        {call, _, {atom, _, is_list}, [{var, _, _}]} ->
                            false;
                        {call, _, {atom, _, is_number}, [{var, _, _}]} ->
                            false;
                        {call, _, {atom, _, is_pid}, [{var, _, _}]} ->
                            false;
                        {call, _, {atom, _, is_port}, [{var, _, _}]} ->
                            false;
                        {call, _, {atom, _, is_process_alive}, [{var, _, _}]} ->
                            false;
                        {call, _, {atom, _, is_record}, _} ->
                            false;
                        {call, _, {atom, _, is_reference}, [{var, _, _}]} ->
                            false;
                        _ ->
                            true
                    end
                end, L);
            true ->
                false
        end
    end, ClauseList),
    
    % simplify by only allowing valid "is_" erlang guard calls
    Params = lists:foldr(fun({call, _, {atom, _, F}, [{var, _, V}]}, L) ->
        Type = if
            F == is_binary ->
                <<"str">>;
            F == is_bitstring ->
                <<"str">>;
            F == is_tuple ->
                <<"obj">>;
            % is_list should be <<"arr">>, but actually becomes a tuple
            % since they are both decoded in the same way,
            % only <<"obj">> is needed to handle <<"arr">> data
            F == is_boolean ->
                <<"bit">>;
            F == is_integer ->
                <<"num">>
        end,
        [#service_proc_param{name = list_to_binary(atom_to_list(V)),
                             type = Type} | L]
    end, [], Guard),

    % make sure every argument has a guard that specifies the argument type
    true = erlang:length(Args) == erlang:length(Params),

    module_function_calls(Remaining,
        [#service_proc{name = list_to_binary(atom_to_list(FName)),
                       idempotent = true,
                       params = Params} | FunctionList]);

module_function_calls([{attribute, _, _, _} | Remaining], FunctionList) ->
    module_function_calls(Remaining, FunctionList);

module_function_calls([{eof, _} | Remaining], FunctionList) ->
    module_function_calls(Remaining, FunctionList).

module_uuid(Module) when is_atom(Module) ->
    code:soft_purge(Module),
    {Module, Binary, _} = code:get_object_code(Module),
    % assume only one version is loaded
    {ok, {Module, [Version]}} = beam_lib:version(Binary),
    list_to_binary("urn:uuid:" ++
                   binary_to_hex(erlang:md5(term_to_binary(Version)))).

% taken from the rfc4627_jsonrpc module
binary_to_hex(<<>>) ->
    [];
binary_to_hex(<<B, Rest/binary>>) ->
    [rfc4627:hex_digit((B bsr 4) band 15),
     rfc4627:hex_digit(B band 15) |
     binary_to_hex(Rest)].
    
