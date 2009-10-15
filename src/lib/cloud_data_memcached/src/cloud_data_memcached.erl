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
%%% @version 0.0.7 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloud_data_memcached).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).
-behaviour(cloud_data_interface).

%% external interface
-export([get/2, get/3,
         get_many/2, get_many/3,
         add/3, add/4,
         add_exp/4, add_exp/5,
         set/3, set/4,
         set_exp/4, set_exp/5,
         replace/3, replace/4,
         replace_exp/4, replace_exp/5,
         delete/2, delete/3,
         increment/5, increment/6,
         decrement/5, decrement/6,
         append/3, append/4,
         prepend/3, prepend/4,
         stats/1, stats/2,
         flush/1, flush/2,
         flush_exp/2, flush_exp/3,
         quit/1, quit/2,
         version/1, version/2]).

%% do_queries_group/5 interface
-export([do_queries_sequentially/2]).

%% cloud_data_interface callbacks
-export([start_link/2, handle_stop/1, handle_do_queries/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("cloud_logger.hrl").
-include("cloud_types.hrl").

-define(MEMCACHED_TIMEOUT, 20000). % 20 seconds

-record(state,
    {
    data_title = undefined,
    process = undefined,
    timeout = undefined}).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the binary value stored for the key.===
%% @end
%%-------------------------------------------------------------------------

-spec get(DataTitle :: atom(),
          Key :: any()) ->
    any().

get(DataTitle, Key) ->
    get(DataTitle, Key, undefined).

-spec get(DataTitle :: atom(),
          Key :: any(),
          Timeout :: 'undefined' | pos_integer()) ->
    any().

get(DataTitle, Key, Timeout)
    when is_atom(DataTitle) ->
    gen_server:call(DataTitle,
        {get, Key, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a list of binary values for a list of keys.===
%% @end
%%-------------------------------------------------------------------------

-spec get_many(DataTitle :: atom(),
               Keys :: list()) ->
    any().

get_many(DataTitle, Keys) ->
    get_many(DataTitle, Keys, undefined).

-spec get_many(DataTitle :: atom(),
               Keys :: list(),
               Timeout :: 'undefined' | pos_integer()) ->
    any().

get_many(DataTitle, Keys, Timeout)
    when is_atom(DataTitle), is_list(Keys) ->
    gen_server:call(DataTitle,
        {get_many, Keys, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Store a binary value for a key if the server doesn't hold data for the key.===
%% @end
%%-------------------------------------------------------------------------

-spec add(DataTitle :: atom(),
          Key :: any(),
          Value :: binary()) ->
    any().

add(DataTitle, Key, Value) ->
    add(DataTitle, Key, Value, undefined).

-spec add(DataTitle :: atom(),
          Key :: any(),
          Value :: binary(),
          Timeout :: 'undefined' | pos_integer()) ->
    any().

add(DataTitle, Key, Value, Timeout)
    when is_atom(DataTitle), is_binary(Value) ->
    gen_server:call(DataTitle,
        {add, Key, Value, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Store a binary value with an expiration for a key if the server doesn't hold data for the key.===
%% @end
%%-------------------------------------------------------------------------

-spec add_exp(DataTitle :: atom(),
              Key :: any(),
              Value :: binary(),
              Expiration :: non_neg_integer()) ->
    any().

add_exp(DataTitle, Key, Value, Expiration) ->
    add_exp(DataTitle, Key, Value, Expiration, undefined).

-spec add_exp(DataTitle :: atom(),
              Key :: any(),
              Value :: binary(),
              Expiration :: non_neg_integer(),
              Timeout :: 'undefined' | pos_integer()) ->
    any().

add_exp(DataTitle, Key, Value, Expiration, Timeout)
    when is_atom(DataTitle), is_binary(Value),
         is_integer(Expiration) ->
    gen_server:call(DataTitle,
        {add, Key, Value, Expiration, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Store a binary value for a key.===
%% @end
%%-------------------------------------------------------------------------

-spec set(DataTitle :: atom(),
          Key :: any(),
          Value :: binary()) ->
    any().

set(DataTitle, Key, Value) ->
    set(DataTitle, Key, Value, undefined).

-spec set(DataTitle :: atom(),
          Key :: any(),
          Value :: binary(),
          Timeout :: 'undefined' | pos_integer()) ->
    any().

set(DataTitle, Key, Value, Timeout)
    when is_atom(DataTitle), is_binary(Value) ->
    gen_server:call(DataTitle,
        {set, Key, Value, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Store a binary value with an expiration for a key.===
%% @end
%%-------------------------------------------------------------------------

-spec set_exp(DataTitle :: atom(),
              Key :: any(),
              Value :: binary(),
              Expiration :: non_neg_integer()) ->
    any().

set_exp(DataTitle, Key, Value, Expiration) ->
    set_exp(DataTitle, Key, Value, Expiration, undefined).

-spec set_exp(DataTitle :: atom(),
              Key :: any(),
              Value :: binary(),
              Expiration :: non_neg_integer(),
              Timeout :: 'undefined' | pos_integer()) ->
    any().

set_exp(DataTitle, Key, Value, Expiration, Timeout)
    when is_atom(DataTitle), is_binary(Value),
         is_integer(Expiration) ->
    gen_server:call(DataTitle,
        {set, Key, Value, Expiration, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Store a binary value for a key if the server already holds data for the key.===
%% @end
%%-------------------------------------------------------------------------

-spec replace(DataTitle :: atom(),
              Key :: any(),
              Value :: binary()) ->
    any().

replace(DataTitle, Key, Value) ->
    replace(DataTitle, Key, Value, undefined).

-spec replace(DataTitle :: atom(),
              Key :: any(),
              Value :: binary(),
              Timeout :: 'undefined' | pos_integer()) ->
    any().

replace(DataTitle, Key, Value, Timeout)
    when is_atom(DataTitle), is_binary(Value) ->
    gen_server:call(DataTitle,
        {replace, Key, Value, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Store a binary value with an expiration for a key if the server already holds data for the key.===
%% @end
%%-------------------------------------------------------------------------

-spec replace_exp(DataTitle :: atom(),
                  Key :: any(),
                  Value :: binary(),
                  Expiration :: non_neg_integer()) ->
    any().

replace_exp(DataTitle, Key, Value, Expiration) ->
    replace_exp(DataTitle, Key, Value, Expiration, undefined).

-spec replace_exp(DataTitle :: atom(),
                  Key :: any(),
                  Value :: binary(),
                  Expiration :: non_neg_integer(),
                  Timeout :: 'undefined' | pos_integer()) ->
    any().

replace_exp(DataTitle, Key, Value, Expiration, Timeout)
    when is_atom(DataTitle), is_binary(Value),
         is_integer(Expiration) ->
    gen_server:call(DataTitle,
        {replace, Key, Value, Expiration, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove the binary value stored for a key.===
%% @end
%%-------------------------------------------------------------------------

-spec delete(DataTitle :: atom(),
             Key :: any()) ->
    any().

delete(DataTitle, Key) ->
    delete(DataTitle, Key, undefined).

-spec delete(DataTitle :: atom(),
             Key :: any(),
             Timeout :: 'undefined' | pos_integer()) ->
    any().

delete(DataTitle, Key, Timeout)
    when is_atom(DataTitle) ->
    gen_server:call(DataTitle,
        {delete, Key, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Increment an integer value stored for a key with the provided initial value and expiration.===
%% @end
%%-------------------------------------------------------------------------

-spec increment(DataTitle :: atom(),
                Key :: any(),
                Value :: binary(),
                Initial :: binary(),
                Expiration :: non_neg_integer()) ->
    any().

increment(DataTitle, Key, Value, Initial, Expiration) ->
    increment(DataTitle, Key, Value, Initial, Expiration, undefined).

-spec increment(DataTitle :: atom(),
                Key :: any(),
                Value :: binary(),
                Initial :: binary(),
                Expiration :: non_neg_integer(),
                Timeout :: 'undefined' | pos_integer()) ->
    any().

increment(DataTitle, Key, Value, Initial, Expiration, Timeout)
    when is_atom(DataTitle), is_binary(Value), is_binary(Initial),
         is_integer(Expiration) ->
    gen_server:call(DataTitle,
        {increment, Key, Value, Initial, Expiration, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Decrement an integer value stored for a key with the provided initial value and expiration.===
%% @end
%%-------------------------------------------------------------------------

-spec decrement(DataTitle :: atom(),
                Key :: any(),
                Value :: binary(),
                Initial :: binary(),
                Expiration :: non_neg_integer()) ->
    any().

decrement(DataTitle, Key, Value, Initial, Expiration) ->
    decrement(DataTitle, Key, Value, Initial, Expiration, undefined).

-spec decrement(DataTitle :: atom(),
                Key :: any(),
                Value :: binary(),
                Initial :: binary(),
                Expiration :: non_neg_integer(),
                Timeout :: 'undefined' | pos_integer()) ->
    any().

decrement(DataTitle, Key, Value, Initial, Expiration, Timeout)
    when is_atom(DataTitle), is_binary(Value), is_binary(Initial),
         is_integer(Expiration) ->
    gen_server:call(DataTitle,
        {decrement, Key, Value, Initial, Expiration, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add the binary value after the existing value for the key.===
%% @end
%%-------------------------------------------------------------------------

-spec append(DataTitle :: atom(),
             Key :: any(),
             Value :: binary()) ->
    any().

append(DataTitle, Key, Value) ->
    append(DataTitle, Key, Value, undefined).

-spec append(DataTitle :: atom(),
             Key :: any(),
             Value :: binary(),
             Timeout :: 'undefined' | pos_integer()) ->
    any().

append(DataTitle, Key, Value, Timeout)
    when is_atom(DataTitle), is_binary(Value) ->
    gen_server:call(DataTitle,
        {append, Key, Value, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add the binary value before the existing value for the key.===
%% @end
%%-------------------------------------------------------------------------

-spec prepend(DataTitle :: atom(),
              Key :: any(),
              Value :: binary()) ->
    any().

prepend(DataTitle, Key, Value) ->
    prepend(DataTitle, Key, Value, undefined).

-spec prepend(DataTitle :: atom(),
              Key :: any(),
              Value :: binary(),
              Timeout :: 'undefined' | pos_integer()) ->
    any().

prepend(DataTitle, Key, Value, Timeout)
    when is_atom(DataTitle), is_binary(Value) ->
    gen_server:call(DataTitle,
        {prepend, Key, Value, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get internal server statistics===
%% @end
%%-------------------------------------------------------------------------

-spec stats(DataTitle :: atom()) ->
    any().

stats(DataTitle) ->
    stats(DataTitle, undefined).

-spec stats(DataTitle :: atom(),
            Timeout :: 'undefined' | pos_integer()) ->
    any().

stats(DataTitle, Timeout)
    when is_atom(DataTitle) ->
    gen_server:call(DataTitle,
        {stats, Timeout}, infinity).
    
%%-------------------------------------------------------------------------
%% @doc
%% ===Invalidate all data currently stored.===
%% @end
%%-------------------------------------------------------------------------

-spec flush(DataTitle :: atom()) ->
    any().

flush(DataTitle) ->
    flush(DataTitle, undefined).

-spec flush(DataTitle :: atom(),
            Timeout :: 'undefined' | pos_integer()) ->
    any().

flush(DataTitle, Timeout)
    when is_atom(DataTitle) ->
    gen_server:call(DataTitle,
        {flush, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Invalidate all data currently stored after an expiration.===
%% @end
%%-------------------------------------------------------------------------

-spec flush_exp(DataTitle :: atom(),
                Expiration :: non_neg_integer()) ->
    any().

flush_exp(DataTitle, Expiration) ->
    flush_exp(DataTitle, Expiration, undefined).

-spec flush_exp(DataTitle :: atom(),
                Expiration :: non_neg_integer(),
                Timeout :: 'undefined' | pos_integer()) ->
    any().

flush_exp(DataTitle, Expiration, Timeout)
    when is_atom(DataTitle), is_integer(Expiration) ->
    gen_server:call(DataTitle,
        {flush, Expiration, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Make the server close the connection.===
%% @end
%%-------------------------------------------------------------------------

-spec quit(DataTitle :: atom()) ->
    any().

quit(DataTitle) ->
    quit(DataTitle, undefined).

-spec quit(DataTitle :: atom(),
           Timeout :: 'undefined' | pos_integer()) ->
    any().

quit(DataTitle, Timeout)
    when is_atom(DataTitle) ->
    gen_server:call(DataTitle,
        {quit, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the server's version string.===
%% @end
%%-------------------------------------------------------------------------

-spec version(DataTitle :: atom()) ->
    any().

version(DataTitle) ->
    version(DataTitle, undefined).

-spec version(DataTitle :: atom(),
              Timeout :: 'undefined' | pos_integer()) ->
    any().

version(DataTitle, Timeout)
    when is_atom(DataTitle) ->
    gen_server:call(DataTitle,
        {version, Timeout}, infinity).

%%%------------------------------------------------------------------------
%%% Callback functions from cloud_data_interface
%%%------------------------------------------------------------------------

-spec start_link(DataTitle :: atom(),
                 Arguments :: list({atom(), string(), list({_,_,_})})) ->
    {'ok', pid()} |
    {'error', any()}.

start_link(DataTitle, Arguments) when is_atom(DataTitle), is_list(Arguments) ->
    gen_server:start_link({local, DataTitle}, ?MODULE,
        [DataTitle, Arguments], []).

-spec handle_stop(DataTitle :: atom()) -> any().

handle_stop(DataTitle) when is_atom(DataTitle) ->
    gen_server:call(DataTitle, stop).

-spec handle_do_queries(DataTitle :: atom(),
                        QueryList :: data_list()) ->
    {'ok', data_list()} |
    {'error', data_list()}.

handle_do_queries(DataTitle, QueryList)
    when is_atom(DataTitle), is_list(QueryList) ->
    % depend on MEMCACHED_TIMEOUT for a database communication timeout
    gen_server:call(DataTitle, {do_queries, QueryList}, infinity).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([DataTitle, Arguments]) when is_atom(DataTitle), is_list(Arguments) ->
    init_state(DataTitle, Arguments).
handle_call({'get', Key, undefined}, _,
            #state{process = Process,
                   timeout = Timeout} = State) ->
    {reply, ememcached:get(Process, Key, Timeout), State};
handle_call({'get', Key, Timeout}, _,
            #state{process = Process} = State) ->
    {reply, ememcached:get(Process, Key, Timeout), State};
handle_call({'get_many', Keys, undefined}, _,
            #state{process = Process,
                   timeout = Timeout} = State) ->
    {reply, ememcached:get_many(Process, Keys, Timeout), State};
handle_call({'get_many', Keys, Timeout}, _,
            #state{process = Process} = State) ->
    {reply, ememcached:get_many(Process, Keys, Timeout), State};
handle_call({'add', Key, Value, undefined}, _,
            #state{process = Process,
                   timeout = Timeout} = State) ->
    {reply, ememcached:add(Process, Key, Value, Timeout), State};
handle_call({'add', Key, Value, Timeout}, _,
            #state{process = Process} = State) ->
    {reply, ememcached:add(Process, Key, Value, Timeout), State};
handle_call({'add', Key, Value, Expiration, undefined}, _,
            #state{process = Process,
                   timeout = Timeout} = State) ->
    {reply, ememcached:add_exp(Process,
        Key, Value, Expiration, Timeout), State};
handle_call({'add', Key, Value, Expiration, Timeout}, _,
            #state{process = Process} = State) ->
    {reply, ememcached:add_exp(Process,
        Key, Value, Expiration, Timeout), State};
handle_call({'set', Key, Value, undefined}, _,
            #state{process = Process,
                   timeout = Timeout} = State) ->
    {reply, ememcached:set(Process, Key, Value, Timeout), State};
handle_call({'set', Key, Value, Timeout}, _,
            #state{process = Process} = State) ->
    {reply, ememcached:set(Process, Key, Value, Timeout), State};
handle_call({'set', Key, Value, Expiration, undefined}, _,
            #state{process = Process,
                   timeout = Timeout} = State) ->
    {reply, ememcached:set_exp(Process,
        Key, Value, Expiration, Timeout), State};
handle_call({'set', Key, Value, Expiration, Timeout}, _,
            #state{process = Process} = State) ->
    {reply, ememcached:set_exp(Process,
        Key, Value, Expiration, Timeout), State};
handle_call({'replace', Key, Value, undefined}, _,
            #state{process = Process,
                   timeout = Timeout} = State) ->
    {reply, ememcached:replace(Process, Key, Value, Timeout), State};
handle_call({'replace', Key, Value, Timeout}, _,
            #state{process = Process} = State) ->
    {reply, ememcached:replace(Process, Key, Value, Timeout), State};
handle_call({'replace', Key, Value, Expiration, undefined}, _,
            #state{process = Process,
                   timeout = Timeout} = State) ->
    {reply, ememcached:replace_exp(Process,
        Key, Value, Expiration, Timeout), State};
handle_call({'replace', Key, Value, Expiration, Timeout}, _,
            #state{process = Process} = State) ->
    {reply, ememcached:replace_exp(Process,
        Key, Value, Expiration, Timeout), State};
handle_call({'delete', Key, undefined}, _,
            #state{process = Process,
                   timeout = Timeout} = State) ->
    {reply, ememcached:delete(Process, Key, Timeout), State};
handle_call({'delete', Key, Timeout}, _,
            #state{process = Process} = State) ->
    {reply, ememcached:delete(Process, Key, Timeout), State};
handle_call({'increment', Key, Value, Initial, Expiration, undefined}, _,
            #state{process = Process,
                   timeout = Timeout} = State) ->
    {reply, ememcached:increment_exp(Process,
        Key, Value, Initial, Expiration, Timeout), State};
handle_call({'increment', Key, Value, Initial, Expiration, Timeout}, _,
            #state{process = Process} = State) ->
    {reply, ememcached:increment_exp(Process,
        Key, Value, Initial, Expiration, Timeout), State};
handle_call({'decrement', Key, Value, Initial, Expiration, undefined}, _,
            #state{process = Process,
                   timeout = Timeout} = State) ->
    {reply, ememcached:decrement_exp(Process,
        Key, Value, Initial, Expiration, Timeout), State};
handle_call({'decrement', Key, Value, Initial, Expiration, Timeout}, _,
            #state{process = Process} = State) ->
    {reply, ememcached:decrement_exp(Process,
        Key, Value, Initial, Expiration, Timeout), State};
handle_call({'append', Key, Value, undefined}, _,
            #state{process = Process,
                   timeout = Timeout} = State) ->
    {reply, ememcached:append(Process, Key, Value, Timeout), State};
handle_call({'append', Key, Value, Timeout}, _,
            #state{process = Process} = State) ->
    {reply, ememcached:append(Process, Key, Value, Timeout), State};
handle_call({'prepend', Key, Value, undefined}, _,
            #state{process = Process,
                   timeout = Timeout} = State) ->
    {reply, ememcached:prepend(Process, Key, Value, Timeout), State};
handle_call({'prepend', Key, Value, Timeout}, _,
            #state{process = Process} = State) ->
    {reply, ememcached:prepend(Process, Key, Value, Timeout), State};
handle_call({'stats', undefined}, _,
            #state{process = Process,
                   timeout = Timeout} = State) ->
    {reply, ememcached:stats(Process, Timeout), State};
handle_call({'stats', Timeout}, _,
            #state{process = Process} = State) ->
    {reply, ememcached:stats(Process, Timeout), State};
handle_call({'flush', undefined}, _,
            #state{process = Process,
                   timeout = Timeout} = State) ->
    {reply, ememcached:flush(Process, Timeout), State};
handle_call({'flush', Timeout}, _,
            #state{process = Process} = State) ->
    {reply, ememcached:flush(Process, Timeout), State};
handle_call({'flush', Expiration, undefined}, _,
            #state{process = Process,
                   timeout = Timeout} = State) ->
    {reply, ememcached:flush_exp(Process, Expiration, Timeout), State};
handle_call({'flush', Expiration, Timeout}, _,
            #state{process = Process} = State) ->
    {reply, ememcached:flush_exp(Process, Expiration, Timeout), State};
handle_call({'quit', undefined}, _,
            #state{process = Process,
                   timeout = Timeout} = State) ->
    {reply, ememcached:quit(Process, Timeout), State};
handle_call({'quit', Timeout}, _,
            #state{process = Process} = State) ->
    {reply, ememcached:quit(Process, Timeout), State};
handle_call({'version', undefined}, _,
            #state{process = Process,
                   timeout = Timeout} = State) ->
    {reply, ememcached:version(Process, Timeout), State};
handle_call({'version', Timeout}, _,
            #state{process = Process} = State) ->
    {reply, ememcached:version(Process, Timeout), State};
handle_call(stop, _,
            #state{data_title = DataTitle,
                   process = Pid} = State) ->
    erlang:exit(Pid, normal),
    {stop, atom_to_list(DataTitle) ++ " was requested to stop", State};
handle_call({do_queries, QueryList}, _,
            #state{data_title = DataTitle} = State) ->
    Response = cloud_data_interface:do_queries_group(QueryList,
        cloud_data_memcached, do_queries_sequentially, State, DataTitle),
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
    Timeout = ?MEMCACHED_TIMEOUT,
    case ememcached:start_link(HostList, Timeout) of
        {ok, Pid} when is_pid(Pid) ->
            {ok, #state{data_title = DataTitle,
                        process = Pid,
                        timeout = Timeout}};
        {error, Reason} ->
            {stop, Reason}
    end.

%% do a single query and return a boolean to determine if the query succeeded
do_query(Query, Process, Timeout) ->
    try (case string_extensions:binary_to_term(Query) of
            %{'get', Key} ->
            %    ememcached:get(Process,
            %       Key, Timeout);
            %{'get_many', Keys} when is_list(Keys) ->
            %    ememcached:get_many(Process,
            %        Keys, Timeout);
            {'add', Key, Value}
                when is_binary(Value) ->
                ememcached:add(Process,
                    Key, Value, Timeout);
            {'add', Key, Value, Expiration}
                when is_binary(Value), is_integer(Expiration) ->
                ememcached:add_exp(Process,
                    Key, Value, Expiration, Timeout);
            {'set', Key, Value}
                when is_binary(Value) ->
                ememcached:set(Process,
                    Key, Value, Timeout);
            {'set', Key, Value, Expiration}
                when is_binary(Value), is_integer(Expiration) ->
                ememcached:set_exp(Process,
                    Key, Value, Expiration, Timeout);
            {'replace', Key, Value}
                when is_binary(Value) ->
                ememcached:replace(Process,
                    Key, Value, Timeout);
            {'replace', Key, Value, Expiration}
                when is_binary(Value), is_integer(Expiration) ->
                ememcached:replace_exp(Process,
                    Key, Value, Expiration, Timeout);
            {'delete', Key} ->
                ememcached:delete(Process,
                    Key, Timeout);
            {'increment', Key, Value, Initial, Expiration}
                when is_binary(Value), is_binary(Initial),
                     is_integer(Expiration) ->
                ememcached:increment_exp(Process,
                    Key, Value, Initial, Expiration, Timeout);
            {'decrement', Key, Value, Initial, Expiration}
                when is_binary(Value), is_binary(Initial),
                     is_integer(Expiration) ->
                ememcached:decrement_exp(Process,
                    Key, Value, Initial, Expiration, Timeout);
            {'append', Key, Value}
                when is_binary(Value) ->
                ememcached:append(Process,
                    Key, Value, Timeout);
            {'prepend', Key, Value}
                when is_binary(Value) ->
                ememcached:prepend(Process,
                    Key, Value, Timeout);
            %{'stats'} ->
            %    ememcached:stats(Process,
            %        Timeout);
            {'flush'} ->
                ememcached:flush(Process,
                    Timeout);
            {'flush', Expiration}
                when is_integer(Expiration) ->
                ememcached:flush_exp(Process,
                    Expiration, Timeout);
            %{'quit'} ->
            %    ememcached:quit(Process,
            %        Timeout);
            %{'version'} ->
            %    ememcached:version(Process,
            %        Timeout);
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
do_queries_sequentially(QueryList,
                        #state{process = Process,
                               timeout = Timeout}) when is_list(QueryList) ->
    lists:dropwhile(fun(Query) ->
        do_query(Query, Process, Timeout)
    end, QueryList).

