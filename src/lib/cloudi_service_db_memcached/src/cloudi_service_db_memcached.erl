%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Memcached Data Module==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2009-2013, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2009-2013 Michael Truog
%%% @version 1.3.0 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_service_db_memcached).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_service).

%% external interface
-export([get/3, get/4,
         get_many/3, get_many/4,
         add/4, add/5,
         add_exp/5, add_exp/6,
         set/4, set/5,
         set_exp/5, set_exp/6,
         replace/4, replace/5,
         replace_exp/5, replace_exp/6,
         delete/3, delete/4,
         increment/6, increment/7,
         decrement/6, decrement/7,
         append/4, append/5,
         prepend/4, prepend/5,
         stats/2, stats/3,
         flush/2, flush/3,
         flush_exp/3, flush_exp/4,
         quit/2, quit/3,
         version/2, version/3]).

%% cloudi_service callbacks
-export([cloudi_service_init/3,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/2]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-record(state,
    {
        process = undefined
    }).

-type dispatcher() :: cloudi_service:dispatcher() | cloudi:context().

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the binary value stored for the key.===
%% @end
%%-------------------------------------------------------------------------

-spec get(Dispatcher :: dispatcher(),
          Name :: string(),
          Key :: any()) ->
    any().

get(Dispatcher, Name, Key)
    when is_list(Name) ->
    cloudi:send_sync(Dispatcher, Name,
                     {get, Key}).

-spec get(Dispatcher :: dispatcher(),
          Name :: string(),
          Key :: any(),
          Timeout :: pos_integer()) ->
    any().

get(Dispatcher, Name, Key, Timeout)
    when is_list(Name),
         is_integer(Timeout) ->
    cloudi:send_sync(Dispatcher, Name,
                     {get, Key}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a list of binary values for a list of keys.===
%% @end
%%-------------------------------------------------------------------------

-spec get_many(Dispatcher :: dispatcher(),
               Name :: string(),
               Keys :: list()) ->
    any().

get_many(Dispatcher, Name, Keys)
    when is_list(Name),
         is_list(Keys) ->
    cloudi:send_sync(Dispatcher, Name,
                     {get_many, Keys}).

-spec get_many(Dispatcher :: dispatcher(),
               Name :: string(),
               Keys :: list(),
               Timeout :: pos_integer()) ->
    any().

get_many(Dispatcher, Name, Keys, Timeout)
    when is_list(Name),
         is_list(Keys), is_integer(Timeout) ->
    cloudi:send_sync(Dispatcher, Name,
                     {get_many, Keys}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Store a binary value for a key if the server doesn't hold data for the key.===
%% @end
%%-------------------------------------------------------------------------

-spec add(Dispatcher :: dispatcher(),
          Name :: string(),
          Key :: any(),
          Value :: binary()) ->
    any().

add(Dispatcher, Name, Key, Value)
    when is_list(Name),
         is_binary(Value) ->
    cloudi:send_sync(Dispatcher, Name,
                     {add, Key, Value}).

-spec add(Dispatcher :: dispatcher(),
          Name :: string(),
          Key :: any(),
          Value :: binary(),
          Timeout :: pos_integer()) ->
    any().

add(Dispatcher, Name, Key, Value, Timeout)
    when is_list(Name),
         is_binary(Value), is_integer(Timeout) ->
    cloudi:send_sync(Dispatcher, Name,
                     {add, Key, Value}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Store a binary value with an expiration for a key if the server doesn't hold data for the key.===
%% @end
%%-------------------------------------------------------------------------

-spec add_exp(Dispatcher :: dispatcher(),
              Name :: string(),
              Key :: any(),
              Value :: binary(),
              Expiration :: non_neg_integer()) ->
    any().

add_exp(Dispatcher, Name, Key, Value, Expiration)
    when is_list(Name),
         is_binary(Value), is_integer(Expiration) ->
    cloudi:send_sync(Dispatcher, Name,
                     {add, Key, Value, Expiration}).

-spec add_exp(Dispatcher :: dispatcher(),
              Name :: string(),
              Key :: any(),
              Value :: binary(),
              Expiration :: non_neg_integer(),
              Timeout :: pos_integer()) ->
    any().

add_exp(Dispatcher, Name, Key, Value, Expiration, Timeout)
    when is_list(Name),
         is_binary(Value), is_integer(Expiration), is_integer(Timeout) ->
    cloudi:send_sync(Dispatcher, Name,
                     {add, Key, Value, Expiration}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Store a binary value for a key.===
%% @end
%%-------------------------------------------------------------------------

-spec set(Dispatcher :: dispatcher(),
          Name :: string(),
          Key :: any(),
          Value :: binary()) ->
    any().

set(Dispatcher, Name, Key, Value)
    when is_list(Name),
         is_binary(Value) ->
    cloudi:send_sync(Dispatcher, Name,
                     {set, Key, Value}).

-spec set(Dispatcher :: dispatcher(),
          Name :: string(),
          Key :: any(),
          Value :: binary(),
          Timeout :: pos_integer()) ->
    any().

set(Dispatcher, Name, Key, Value, Timeout)
    when is_list(Name),
         is_binary(Value), is_integer(Timeout) ->
    cloudi:send_sync(Dispatcher, Name,
                     {set, Key, Value}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Store a binary value with an expiration for a key.===
%% @end
%%-------------------------------------------------------------------------

-spec set_exp(Dispatcher :: dispatcher(),
              Name :: string(),
              Key :: any(),
              Value :: binary(),
              Expiration :: non_neg_integer()) ->
    any().

set_exp(Dispatcher, Name, Key, Value, Expiration)
    when is_list(Name),
         is_binary(Value), is_integer(Expiration) ->
    cloudi:send_sync(Dispatcher, Name,
                     {set, Key, Value, Expiration}).

-spec set_exp(Dispatcher :: dispatcher(),
              Name :: string(),
              Key :: any(),
              Value :: binary(),
              Expiration :: non_neg_integer(),
              Timeout :: pos_integer()) ->
    any().

set_exp(Dispatcher, Name, Key, Value, Expiration, Timeout)
    when is_list(Name),
         is_binary(Value), is_integer(Expiration), is_integer(Timeout) ->
    cloudi:send_sync(Dispatcher, Name,
                     {set, Key, Value, Expiration}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Store a binary value for a key if the server already holds data for the key.===
%% @end
%%-------------------------------------------------------------------------

-spec replace(Dispatcher :: dispatcher(),
              Name :: string(),
              Key :: any(),
              Value :: binary()) ->
    any().

replace(Dispatcher, Name, Key, Value)
    when is_list(Name),
         is_binary(Value) ->
    cloudi:send_sync(Dispatcher, Name,
                             {replace, Key, Value}).

-spec replace(Dispatcher :: dispatcher(),
              Name :: string(),
              Key :: any(),
              Value :: binary(),
              Timeout :: pos_integer()) ->
    any().

replace(Dispatcher, Name, Key, Value, Timeout)
    when is_list(Name),
         is_binary(Value), is_integer(Timeout) ->
    cloudi:send_sync(Dispatcher, Name,
                             {replace, Key, Value}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Store a binary value with an expiration for a key if the server already holds data for the key.===
%% @end
%%-------------------------------------------------------------------------

-spec replace_exp(Dispatcher :: dispatcher(),
                  Name :: string(),
                  Key :: any(),
                  Value :: binary(),
                  Expiration :: non_neg_integer()) ->
    any().

replace_exp(Dispatcher, Name, Key, Value, Expiration)
    when is_list(Name),
         is_binary(Value), is_integer(Expiration) ->
    cloudi:send_sync(Dispatcher, Name,
                             {replace, Key, Value, Expiration}).

-spec replace_exp(Dispatcher :: dispatcher(),
                  Name :: string(),
                  Key :: any(),
                  Value :: binary(),
                  Expiration :: non_neg_integer(),
                  Timeout :: pos_integer()) ->
    any().

replace_exp(Dispatcher, Name, Key, Value, Expiration, Timeout)
    when is_list(Name),
         is_binary(Value), is_integer(Expiration), is_integer(Timeout) ->
    cloudi:send_sync(Dispatcher, Name,
                             {replace, Key, Value, Expiration}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove the binary value stored for a key.===
%% @end
%%-------------------------------------------------------------------------

-spec delete(Dispatcher :: dispatcher(),
             Name :: string(),
             Key :: any()) ->
    any().

delete(Dispatcher, Name, Key)
    when is_list(Name) ->
    cloudi:send_sync(Dispatcher, Name,
                             {delete, Key}).

-spec delete(Dispatcher :: dispatcher(),
             Name :: string(),
             Key :: any(),
             Timeout :: pos_integer()) ->
    any().

delete(Dispatcher, Name, Key, Timeout)
    when is_list(Name),
         is_integer(Timeout) ->
    cloudi:send_sync(Dispatcher, Name,
                             {delete, Key}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Increment an integer value stored for a key with the provided initial value and expiration.===
%% @end
%%-------------------------------------------------------------------------

-spec increment(Dispatcher :: dispatcher(),
                Name :: string(),
                Key :: any(),
                Value :: binary(),
                Initial :: binary(),
                Expiration :: non_neg_integer()) ->
    any().

increment(Dispatcher, Name, Key, Value, Initial, Expiration)
    when is_list(Name),
         is_binary(Value), is_binary(Initial), is_integer(Expiration) ->
    cloudi:send_sync(Dispatcher, Name,
                             {increment, Key, Value,
                              Initial, Expiration}).

-spec increment(Dispatcher :: dispatcher(),
                Name :: string(),
                Key :: any(),
                Value :: binary(),
                Initial :: binary(),
                Expiration :: non_neg_integer(),
                Timeout :: pos_integer()) ->
    any().

increment(Dispatcher, Name, Key, Value, Initial, Expiration, Timeout)
    when is_list(Name),
         is_binary(Value), is_binary(Initial), is_integer(Expiration),
         is_integer(Timeout) ->
    cloudi:send_sync(Dispatcher, Name,
                             {increment, Key, Value,
                              Initial, Expiration}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Decrement an integer value stored for a key with the provided initial value and expiration.===
%% @end
%%-------------------------------------------------------------------------

-spec decrement(Dispatcher :: dispatcher(),
                Name :: string(),
                Key :: any(),
                Value :: binary(),
                Initial :: binary(),
                Expiration :: non_neg_integer()) ->
    any().

decrement(Dispatcher, Name, Key, Value, Initial, Expiration)
    when is_list(Name),
         is_binary(Value), is_binary(Initial), is_integer(Expiration) ->
    cloudi:send_sync(Dispatcher, Name,
                             {decrement, Key, Value,
                              Initial, Expiration}).

-spec decrement(Dispatcher :: dispatcher(),
                Name :: string(),
                Key :: any(),
                Value :: binary(),
                Initial :: binary(),
                Expiration :: non_neg_integer(),
                Timeout :: pos_integer()) ->
    any().

decrement(Dispatcher, Name, Key, Value, Initial, Expiration, Timeout)
    when is_list(Name),
         is_binary(Value), is_binary(Initial), is_integer(Expiration),
         is_integer(Timeout) ->
    cloudi:send_sync(Dispatcher, Name,
                             {decrement, Key, Value,
                              Initial, Expiration}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add the binary value after the existing value for the key.===
%% @end
%%-------------------------------------------------------------------------

-spec append(Dispatcher :: dispatcher(),
             Name :: string(),
             Key :: any(),
             Value :: binary()) ->
    any().

append(Dispatcher, Name, Key, Value)
    when is_list(Name),
         is_binary(Value) ->
    cloudi:send_sync(Dispatcher, Name,
                             {append, Key, Value}).

-spec append(Dispatcher :: dispatcher(),
             Name :: string(),
             Key :: any(),
             Value :: binary(),
             Timeout :: pos_integer()) ->
    any().

append(Dispatcher, Name, Key, Value, Timeout)
    when is_list(Name),
         is_binary(Value), is_integer(Timeout) ->
    cloudi:send_sync(Dispatcher, Name,
                             {append, Key, Value}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add the binary value before the existing value for the key.===
%% @end
%%-------------------------------------------------------------------------

-spec prepend(Dispatcher :: dispatcher(),
              Name :: string(),
              Key :: any(),
              Value :: binary()) ->
    any().

prepend(Dispatcher, Name, Key, Value)
    when is_list(Name),
         is_binary(Value) ->
    cloudi:send_sync(Dispatcher, Name,
                             {prepend, Key, Value}).

-spec prepend(Dispatcher :: dispatcher(),
              Name :: string(),
              Key :: any(),
              Value :: binary(),
              Timeout :: pos_integer()) ->
    any().

prepend(Dispatcher, Name, Key, Value, Timeout)
    when is_list(Name),
         is_binary(Value), is_integer(Timeout) ->
    cloudi:send_sync(Dispatcher, Name,
                             {prepend, Key, Value}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get internal server statistics===
%% @end
%%-------------------------------------------------------------------------

-spec stats(Dispatcher :: dispatcher(),
            Name :: string()) ->
    any().

stats(Dispatcher, Name)
    when is_list(Name) ->
    cloudi:send_sync(Dispatcher, Name,
                             stats).

-spec stats(Dispatcher :: dispatcher(),
            Name :: string(),
            Timeout :: pos_integer()) ->
    any().

stats(Dispatcher, Name, Timeout)
    when is_list(Name) ->
    cloudi:send_sync(Dispatcher, Name,
                             stats, Timeout).
    
%%-------------------------------------------------------------------------
%% @doc
%% ===Invalidate all data currently stored.===
%% @end
%%-------------------------------------------------------------------------

-spec flush(Dispatcher :: dispatcher(),
            Name :: string()) ->
    any().

flush(Dispatcher, Name)
    when is_list(Name) ->
    cloudi:send_sync(Dispatcher, Name,
                             flush).

-spec flush(Dispatcher :: dispatcher(),
            Name :: string(),
            Timeout :: pos_integer()) ->
    any().

flush(Dispatcher, Name, Timeout)
    when is_list(Name),
         is_integer(Timeout) ->
    cloudi:send_sync(Dispatcher, Name,
                             flush, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Invalidate all data currently stored after an expiration.===
%% @end
%%-------------------------------------------------------------------------

-spec flush_exp(Dispatcher :: dispatcher(),
                Name :: string(),
                Expiration :: non_neg_integer()) ->
    any().

flush_exp(Dispatcher, Name, Expiration)
    when is_list(Name),
         is_integer(Expiration) ->
    cloudi:send_sync(Dispatcher, Name,
                             {flush, Expiration}).

-spec flush_exp(Dispatcher :: dispatcher(),
                Name :: string(),
                Expiration :: non_neg_integer(),
                Timeout :: pos_integer()) ->
    any().

flush_exp(Dispatcher, Name, Expiration, Timeout)
    when is_list(Name),
         is_integer(Expiration), is_integer(Timeout) ->
    cloudi:send_sync(Dispatcher, Name,
                             {flush, Expiration}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Make the server close the connection.===
%% @end
%%-------------------------------------------------------------------------

-spec quit(Dispatcher :: dispatcher(),
           Name :: string()) ->
    any().

quit(Dispatcher, Name)
    when is_list(Name) ->
    cloudi:send_sync(Dispatcher, Name,
                             quit).

-spec quit(Dispatcher :: dispatcher(),
           Name :: string(),
           Timeout :: pos_integer()) ->
    any().

quit(Dispatcher, Name, Timeout)
    when is_list(Name),
         is_integer(Timeout) ->
    cloudi:send_sync(Dispatcher, Name,
                             quit, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the server's version string.===
%% @end
%%-------------------------------------------------------------------------

-spec version(Dispatcher :: dispatcher(),
           Name :: string()) ->
    any().

version(Dispatcher, Name)
    when is_list(Name) ->
    cloudi:send_sync(Dispatcher, Name,
                             version).

-spec version(Dispatcher :: dispatcher(),
           Name :: string(),
           Timeout :: pos_integer()) ->
    any().

version(Dispatcher, Name, Timeout)
    when is_list(Name),
         is_integer(Timeout) ->
    cloudi:send_sync(Dispatcher, Name,
                             version, Timeout).

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init([{database, DatabaseName,
                    [{_,_,_}|_] = HostList}], _Prefix, Dispatcher) ->
    case cloudi_x_ememcached:start_link(HostList) of
        {ok, Pid} when is_pid(Pid) ->
            cloudi_service:subscribe(Dispatcher, DatabaseName),
            {ok, #state{process = Pid}};
        {error, Reason} ->
            {stop, Reason}
    end.

cloudi_service_handle_request(_Type, _Name, _Pattern, _RequestInfo, Request,
                              Timeout, _Priority, _TransId, _Pid,
                              #state{process = Process} = State,
                              _Dispatcher) ->
    case Request of
        Command when is_binary(Command) ->
            {reply, do_query(Command, Process, Timeout), State};
        {'get', Key} ->
            {reply, cloudi_x_ememcached:get(Process, Key, Timeout), State};
        {'get_many', Keys} ->
            {reply, cloudi_x_ememcached:get_many(Process, Keys, Timeout), State};
        {'add', Key, Value} ->
            {reply, cloudi_x_ememcached:add(Process, Key, Value, Timeout), State};
        {'add', Key, Value, Expiration} ->
            {reply, cloudi_x_ememcached:add_exp(Process, Key, Value, Expiration,
                                       Timeout), State};
        {'set', Key, Value} ->
            {reply, cloudi_x_ememcached:set(Process, Key, Value, Timeout), State};
        {'set', Key, Value, Expiration} ->
            {reply, cloudi_x_ememcached:set_exp(Process, Key, Value, Expiration,
                                       Timeout), State};
        {'replace', Key, Value} ->
            {reply, cloudi_x_ememcached:replace(Process, Key, Value, Timeout), State};
        {'replace', Key, Value, Expiration} ->
            {reply, cloudi_x_ememcached:replace_exp(Process, Key, Value, Expiration,
                                           Timeout), State};
        {'delete', Key} ->
            {reply, cloudi_x_ememcached:delete(Process, Key, Timeout), State};
        {'increment', Key, Value, Initial, Expiration} ->
            {reply, cloudi_x_ememcached:increment_exp(Process, Key, Value, Initial,
                                             Expiration, Timeout), State};
        {'decrement', Key, Value, Initial, Expiration} ->
            {reply, cloudi_x_ememcached:decrement_exp(Process, Key, Value, Initial,
                                             Expiration, Timeout), State};
        {'append', Key, Value} ->
            {reply, cloudi_x_ememcached:append(Process, Key, Value, Timeout), State};
        {'prepend', Key, Value} ->
            {reply, cloudi_x_ememcached:prepend(Process, Key, Value, Timeout), State};
        'stats' ->
            {reply, cloudi_x_ememcached:stats(Process, Timeout), State};
        'flush' ->
            {reply, cloudi_x_ememcached:flush(Process, Timeout), State};
        {'flush', Expiration} ->
            {reply, cloudi_x_ememcached:flush_exp(Process, Expiration, Timeout), State};
        'quit' ->
            {reply, cloudi_x_ememcached:quit(Process, Timeout), State};
        'version' ->
            {reply, cloudi_x_ememcached:version(Process, Timeout), State}
    end.

cloudi_service_handle_info(Request, State, _) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(_, undefined) ->
    ok;
cloudi_service_terminate(_, #state{process = Process}) ->
    erlang:exit(Process, normal),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

%% do a single query and return a boolean to determine if the query succeeded
do_query(Query, Process, Timeout) ->
    try (case cloudi_string:binary_to_term(Query) of
        {'get', Key} ->
            cloudi_x_ememcached:get(Process, Key, Timeout);
        {'get_many', Keys} when is_list(Keys) ->
            cloudi_x_ememcached:get_many(Process, Keys, Timeout);
        {'add', Key, Value}
            when is_binary(Value) ->
            cloudi_x_ememcached:add(Process, Key, Value, Timeout);
        {'add', Key, Value, Expiration}
            when is_binary(Value), is_integer(Expiration) ->
            cloudi_x_ememcached:add_exp(Process, Key, Value, Expiration, Timeout);
        {'set', Key, Value}
            when is_binary(Value) ->
            cloudi_x_ememcached:set(Process, Key, Value, Timeout);
        {'set', Key, Value, Expiration}
            when is_binary(Value), is_integer(Expiration) ->
            cloudi_x_ememcached:set_exp(Process, Key, Value, Expiration, Timeout);
        {'replace', Key, Value}
            when is_binary(Value) ->
            cloudi_x_ememcached:replace(Process, Key, Value, Timeout);
        {'replace', Key, Value, Expiration}
            when is_binary(Value), is_integer(Expiration) ->
            cloudi_x_ememcached:replace_exp(Process, Key, Value, Expiration, Timeout);
        {'delete', Key} ->
            cloudi_x_ememcached:delete(Process, Key, Timeout);
        {'increment', Key, Value, Initial, Expiration}
            when is_binary(Value), is_binary(Initial),
                 is_integer(Expiration) ->
            cloudi_x_ememcached:increment_exp(Process, Key, Value, Initial,
                                     Expiration, Timeout);
        {'decrement', Key, Value, Initial, Expiration}
            when is_binary(Value), is_binary(Initial),
                 is_integer(Expiration) ->
            cloudi_x_ememcached:decrement_exp(Process, Key, Value, Initial,
                                     Expiration, Timeout);
        {'append', Key, Value}
            when is_binary(Value) ->
            cloudi_x_ememcached:append(Process, Key, Value, Timeout);
        {'prepend', Key, Value}
            when is_binary(Value) ->
            cloudi_x_ememcached:prepend(Process, Key, Value, Timeout);
        'stats' ->
            cloudi_x_ememcached:stats(Process, Timeout);
        'flush' ->
            cloudi_x_ememcached:flush(Process, Timeout);
        {'flush', Expiration}
            when is_integer(Expiration) ->
            cloudi_x_ememcached:flush_exp(Process, Expiration, Timeout);
        'quit' ->
            cloudi_x_ememcached:quit(Process, Timeout);
        'version' ->
            cloudi_x_ememcached:version(Process, Timeout);
        _ ->
            {error, invalid_call}
        end) of
        {error, invalid_call} ->
            ?LOG_DEBUG("Invalid memcached command tuple ~p",
                       [binary_to_list(Query)]),
            <<>>;
        Result when is_binary(Result) ->
            Result
    catch
        _:Reason ->
            ?LOG_DEBUG("exception when processing "
                       "memcached command tuple ~p: ~p",
                       [binary_to_list(Query), Reason]),
            <<>>
    end.

