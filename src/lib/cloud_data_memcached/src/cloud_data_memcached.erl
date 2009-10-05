%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Cloudi Memcached Data Module==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2009, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2009 Michael Truog
%%% @version 0.0.4 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloud_data_memcached).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).
-behaviour(cloud_data_interface).

%% do_queries_group/5 interface
-export([do_queries_sequentially/2]).

%% cloud_data_interface callbacks
-export([start_link/2, handle_stop/1, handle_do_queries/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("cloud_logger.hrl").

-define(MEMCACHED_TIMEOUT, 20000). % 20 seconds

-record(state,
    {
    data_title = undefined,
    process = undefined}).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% Callback functions from cloud_data_interface
%%%------------------------------------------------------------------------

-spec start_link(DataTitle :: atom(),
                 Arguments :: list({atom(), any()})) ->
    {'ok', pid()} |
    {'error', any()}.

start_link(DataTitle, Arguments) when is_atom(DataTitle), is_list(Arguments) ->
    gen_server:start_link({local, DataTitle}, ?MODULE,
        [DataTitle, Arguments], []).

-spec handle_stop(DataTitle :: atom()) -> any().

handle_stop(DataTitle) when is_atom(DataTitle) ->
    gen_server:call(DataTitle, stop).

-spec handle_do_queries(DataTitle :: atom(),
                        QueryList :: list({atom(), string()})) ->
    {'ok', list({atom(), string()})} |
    {'error', list({atom(), string()})}.

handle_do_queries(DataTitle, QueryList)
    when is_atom(DataTitle), is_list(QueryList) ->
    % depend on MEMCACHED_TIMEOUT for a database communication timeout
    gen_server:call(DataTitle, {do_queries, QueryList}, infinity).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([DataTitle, Arguments]) when is_atom(DataTitle), is_list(Arguments) ->
    init_state(DataTitle, Arguments).
handle_call(stop, _,
            #state{data_title = DataTitle,
                   process = Pid} = State) ->
    erlang:exit(Pid, normal),
    {stop, atom_to_list(DataTitle) ++ " was requested to stop", State};
handle_call({do_queries, QueryList}, _,
            #state{data_title = DataTitle,
                   process = Pid} = State) ->
    Response = cloud_data_interface:do_queries_group(QueryList,
        cloud_data_memcached, do_queries_sequentially,
        Pid, DataTitle),
    {reply, Response, State, hibernate};
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
terminate(_, #state{process = Pid}) ->
    erlang:exit(Pid, normal),
    ok.
code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

%% initialize the client state
init_state(DataTitle, [{database, _, [{_,_,_}|_] = HostList}])
    when is_atom(DataTitle) ->
    case ememcached:start_link(HostList) of
        {ok, Pid} when is_pid(Pid) ->
            {ok, #state{data_title = DataTitle,
                        process = Pid}};
        {error, Reason} ->
            {stop, Reason}
    end.

%% do a single query and return a boolean to determine if the query succeeded
do_query(Query, Process) ->
    try (case string_extensions:list_to_term(binary_to_list(Query)) of
            %{'get', Key} ->
            %    ememcached:get(Process,
            %       Key, ?MEMCACHED_TIMEOUT);
            %{'get_many', Keys} when is_list(Keys) ->
            %    ememcached:get_many(Process,
            %        Keys, ?MEMCACHED_TIMEOUT);
            {'add', Key, Value}
                when is_binary(Value) ->
                ememcached:add(Process,
                    Key, Value, ?MEMCACHED_TIMEOUT);
            {'add', Key, Value, Expiration}
                when is_binary(Value), is_integer(Expiration) ->
                ememcached:add_exp(Process,
                    Key, Value, Expiration, ?MEMCACHED_TIMEOUT);
            {'set', Key, Value}
                when is_binary(Value) ->
                ememcached:set(Process,
                    Key, Value, ?MEMCACHED_TIMEOUT);
            {'set', Key, Value, Expiration}
                when is_binary(Value), is_integer(Expiration) ->
                ememcached:set_exp(Process,
                    Key, Value, Expiration, ?MEMCACHED_TIMEOUT);
            {'replace', Key, Value}
                when is_binary(Value) ->
                ememcached:replace(Process,
                    Key, Value, ?MEMCACHED_TIMEOUT);
            {'replace', Key, Value, Expiration}
                when is_binary(Value), is_integer(Expiration) ->
                ememcached:replace_exp(Process,
                    Key, Value, Expiration, ?MEMCACHED_TIMEOUT);
            {'delete', Key} ->
                ememcached:delete(Process,
                    Key, ?MEMCACHED_TIMEOUT);
            {'increment', Key, Value, Initial, Expiration}
                when is_binary(Value), is_binary(Initial),
                     is_integer(Expiration) ->
                ememcached:increment_exp(Process,
                    Key, Value, Initial, Expiration, ?MEMCACHED_TIMEOUT);
            {'decrement', Key, Value, Initial, Expiration}
                when is_binary(Value), is_binary(Initial),
                     is_integer(Expiration) ->
                ememcached:decrement_exp(Process,
                    Key, Value, Initial, Expiration, ?MEMCACHED_TIMEOUT);
            {'append', Key, Value}
                when is_binary(Value) ->
                ememcached:append(Process,
                    Key, Value, ?MEMCACHED_TIMEOUT);
            {'prepend', Key, Value}
                when is_binary(Value) ->
                ememcached:prepend(Process,
                    Key, Value, ?MEMCACHED_TIMEOUT);
            %{'stats'} ->
            %    ememcached:stats(Process,
            %        ?MEMCACHED_TIMEOUT);
            {'flush'} ->
                ememcached:flush(Process,
                    ?MEMCACHED_TIMEOUT);
            {'flush', Expiration}
                when is_integer(Expiration) ->
                ememcached:flush_exp(Process,
                    Expiration, ?MEMCACHED_TIMEOUT);
            %{'quit'} ->
            %    ememcached:quit(Process,
            %        ?MEMCACHED_TIMEOUT);
            %{'version'} ->
            %    ememcached:version(Process,
            %        ?MEMCACHED_TIMEOUT);
            _ ->
                {error, invalid_call}
    
        end) of
        {error, invalid_call} ->
            ?LOG_DEBUG("Invalid memcached command tuple ~p",
                       [binary_to_list(Query)]),
            false;
        _ ->
            true
    catch
        _:Reason ->
            ?LOG_DEBUG("exception when processing "
                       "memcached command tuple ~p: ~p",
                       [binary_to_list(Query), Reason]),
            false
    end.

%% do all queries in the list until an error is encountered
%% return the remaining list if there is an error, else an empty list
do_queries_sequentially(QueryList, Process) when is_list(QueryList) ->
    lists:dropwhile(fun(Query) -> do_query(Query, Process) end, QueryList).

