%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Database==
%%% Provide a simple in-memory database.
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2013, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2013 Michael Truog
%%% @version 1.3.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_db).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_service).

%% external interface

%% cloudi_service callbacks
-export([cloudi_service_init/3,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/2]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-define(DEFAULT_TABLE_MODULE,             dict). % dict API
-define(DEFAULT_OUTPUT,               internal).
-define(DEFAULT_ENDIAN,                 native). % when output == external
-define(DEFAULT_USE_KEY_VALUE,            true). % functionality

-record(state,
    {
        tables = cloudi_x_trie:new() :: cloudi_x_trie:trie(),
        prefix_length :: integer(),
        uuid_generator :: cloudi_x_uuid:state(),
        table_module :: atom(),
        output_type :: internal | external,
        endian :: big | little | native,
        use_key_value :: boolean()
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init(Args, Prefix, Dispatcher) ->
    Defaults = [
        {table_module,             ?DEFAULT_TABLE_MODULE},
        {output,                   ?DEFAULT_OUTPUT},
        {endian,                   ?DEFAULT_ENDIAN},
        {use_key_value,            ?DEFAULT_USE_KEY_VALUE}],
    [TableModule, OutputType, Endian, UseKeyValue] =
        cloudi_proplists:take_values(Defaults, Args),
    true = is_atom(TableModule),
    {file, _} = code:is_loaded(TableModule),
    true = OutputType =:= internal orelse OutputType =:= external,
    true = (Endian =:= big orelse
            Endian =:= little orelse
            Endian =:= native),
    true = is_boolean(UseKeyValue),
    cloudi_service:subscribe(Dispatcher, "*"), % dynamic database name
    Self = cloudi_service:self(Dispatcher),
    {ok, #state{prefix_length = erlang:length(Prefix),
                uuid_generator = cloudi_x_uuid:new(Self),
                table_module = TableModule,
                output_type = OutputType,
                endian = Endian,
                use_key_value = UseKeyValue}}.

cloudi_service_handle_request(_Type, Name, _Pattern, _RequestInfo, Request,
                              _Timeout, _Priority, _TransId, _Pid,
                              #state{prefix_length = PrefixLength} = State,
                              _Dispatcher) ->
    do_query(lists:nthtail(PrefixLength, Name), Request, State).

cloudi_service_handle_info(Request, State, _) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(_, #state{}) ->
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

do_query(Database, Request,
         #state{use_key_value = true} = State) ->
    key_value(cloudi_request:new(Request, internal),
              Database, Request, State).
    
key_value({new, Table},
          Database, Request, State) ->
    key_value({new, Table, undefined},
              Database, Request, State);
key_value({new, Table, Value},
          Database, Request,
          #state{tables = Tables,
                 uuid_generator = UUID,
                 table_module = TableModule} = State)
    when is_list(Table) ->
    Key = cloudi_x_uuid:get_v1(UUID),
    TableName = Database ++ [$/ | Table],
    NewTables = cloudi_x_trie:update(TableName, fun(Old) ->
            TableModule:store(Key, Value, Old)
        end, TableModule:store(Key, Value, TableModule:new()), Tables),
    {reply,
     response(Request, {ok, Key}, State),
     State#state{tables = NewTables}};
key_value({delete, Table},
          Database, Request,
          #state{tables = Tables} = State)
    when is_list(Table) ->
    TableName = Database ++ [$/ | Table],
    NewTables = cloudi_x_trie:erase(TableName, Tables),
    {reply,
     response(Request, ok, State),
     State#state{tables = NewTables}};
key_value({delete, Table, Key},
          Database, Request,
          #state{tables = Tables,
                 table_module = TableModule} = State)
    when is_list(Table) ->
    TableName = Database ++ [$/ | Table],
    NewTables = cloudi_x_trie:update(TableName, fun(Old) ->
            TableModule:erase(Key, Old)
        end, TableModule:new(), Tables),
    {reply,
     response(Request, ok, State),
     State#state{tables = NewTables}};
key_value({get, Table},
          Database, Request,
          #state{tables = Tables,
                 table_module = TableModule} = State)
    when is_list(Table) ->
    TableName = Database ++ [$/ | Table],
    case cloudi_x_trie:find(TableName, Tables) of
        {ok, TableValues} ->
            {reply,
             response(Request, {ok, TableModule:to_list(TableValues)}, State),
             State};
        error ->
            {reply,
             response(Request, {ok, []}, State),
             State}
    end;
key_value({get, Table, Key},
          Database, Request,
          #state{tables = Tables,
                 table_module = TableModule} = State)
    when is_list(Table) ->
    TableName = Database ++ [$/ | Table],
    Response = case cloudi_x_trie:find(TableName, Tables) of
        {ok, TableValues} ->
            case TableModule:find(Key, TableValues) of
                {ok, Value} ->
                    response(Request, {ok, Value}, State);
                error ->
                    response(Request, {ok, undefined}, State)
            end;
        error ->
            response(Request, {ok, undefined}, State)
    end,
    {reply, Response, State};
key_value({put, Table, Key, Value},
          Database, Request,
          #state{tables = Tables,
                 table_module = TableModule} = State)
    when is_list(Table) ->
    TableName = Database ++ [$/ | Table],
    NewTables = cloudi_x_trie:update(TableName, fun(Old) ->
            TableModule:store(Key, Value, Old)
        end, TableModule:store(Key, Value, TableModule:new()), Tables),
    {reply,
     response(Request, ok, State),
     State#state{tables = NewTables}};
key_value(Command,
          _Database, Request, State) ->
    {reply,
     response(Request, {error, {invalid_command, Command}}, State),
     State}.

-compile({inline, [{response, 3}]}).
response(Request, Response,
         #state{output_type = internal}) ->
    cloudi_response:new_internal(Request, Response);
response(Request, Response,
         #state{output_type = external,
                endian = Endian}) ->
    cloudi_response:new_external(Request, Response, Endian).

