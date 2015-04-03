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
%%% Copyright (c) 2009-2015, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2009-2015 Michael Truog
%%% @version 1.5.0 {@date} {@time}
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
-export([cloudi_service_init/4,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-record(state,
    {
        process = undefined
    }).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

-type agent() :: cloudi:agent().
-type service_name() :: cloudi:service_name().
-type timeout_milliseconds() :: cloudi:timeout_milliseconds().
-type external_response(Result) ::
    {{ok, Result}, NewAgent :: agent()} |
    {{error, cloudi:error_reason_sync()}, NewAgent :: agent()}.

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the binary value stored for the key.===
%% @end
%%-------------------------------------------------------------------------

-spec get(Agent :: agent(),
          Name :: service_name(),
          Key :: any()) ->
    external_response(any()).

get(Agent, Name, Key) ->
    cloudi:send_sync(Agent, Name,
                     {get, Key}).

-spec get(Agent :: agent(),
          Name :: service_name(),
          Key :: any(),
          Timeout :: timeout_milliseconds()) ->
    external_response(any()).

get(Agent, Name, Key, Timeout) ->
    cloudi:send_sync(Agent, Name,
                     {get, Key}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a list of binary values for a list of keys.===
%% @end
%%-------------------------------------------------------------------------

-spec get_many(Agent :: agent(),
               Name :: service_name(),
               Keys :: list()) ->
    external_response(any()).

get_many(Agent, Name, Keys)
    when is_list(Keys) ->
    cloudi:send_sync(Agent, Name,
                     {get_many, Keys}).

-spec get_many(Agent :: agent(),
               Name :: service_name(),
               Keys :: list(),
               Timeout :: timeout_milliseconds()) ->
    external_response(any()).

get_many(Agent, Name, Keys, Timeout)
    when is_list(Keys) ->
    cloudi:send_sync(Agent, Name,
                     {get_many, Keys}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Store a binary value for a key if the server doesn't hold data for the key.===
%% @end
%%-------------------------------------------------------------------------

-spec add(Agent :: agent(),
          Name :: service_name(),
          Key :: any(),
          Value :: binary()) ->
    external_response(any()).

add(Agent, Name, Key, Value)
    when is_binary(Value) ->
    cloudi:send_sync(Agent, Name,
                     {add, Key, Value}).

-spec add(Agent :: agent(),
          Name :: service_name(),
          Key :: any(),
          Value :: binary(),
          Timeout :: timeout_milliseconds()) ->
    external_response(any()).

add(Agent, Name, Key, Value, Timeout)
    when is_binary(Value) ->
    cloudi:send_sync(Agent, Name,
                     {add, Key, Value}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Store a binary value with an expiration for a key if the server doesn't hold data for the key.===
%% @end
%%-------------------------------------------------------------------------

-spec add_exp(Agent :: agent(),
              Name :: service_name(),
              Key :: any(),
              Value :: binary(),
              Expiration :: non_neg_integer()) ->
    external_response(any()).

add_exp(Agent, Name, Key, Value, Expiration)
    when is_binary(Value), is_integer(Expiration) ->
    cloudi:send_sync(Agent, Name,
                     {add, Key, Value, Expiration}).

-spec add_exp(Agent :: agent(),
              Name :: service_name(),
              Key :: any(),
              Value :: binary(),
              Expiration :: non_neg_integer(),
              Timeout :: timeout_milliseconds()) ->
    external_response(any()).

add_exp(Agent, Name, Key, Value, Expiration, Timeout)
    when is_binary(Value), is_integer(Expiration) ->
    cloudi:send_sync(Agent, Name,
                     {add, Key, Value, Expiration}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Store a binary value for a key.===
%% @end
%%-------------------------------------------------------------------------

-spec set(Agent :: agent(),
          Name :: service_name(),
          Key :: any(),
          Value :: binary()) ->
    external_response(any()).

set(Agent, Name, Key, Value)
    when is_binary(Value) ->
    cloudi:send_sync(Agent, Name,
                     {set, Key, Value}).

-spec set(Agent :: agent(),
          Name :: service_name(),
          Key :: any(),
          Value :: binary(),
          Timeout :: timeout_milliseconds()) ->
    external_response(any()).

set(Agent, Name, Key, Value, Timeout)
    when is_binary(Value) ->
    cloudi:send_sync(Agent, Name,
                     {set, Key, Value}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Store a binary value with an expiration for a key.===
%% @end
%%-------------------------------------------------------------------------

-spec set_exp(Agent :: agent(),
              Name :: service_name(),
              Key :: any(),
              Value :: binary(),
              Expiration :: non_neg_integer()) ->
    external_response(any()).

set_exp(Agent, Name, Key, Value, Expiration)
    when is_binary(Value), is_integer(Expiration) ->
    cloudi:send_sync(Agent, Name,
                     {set, Key, Value, Expiration}).

-spec set_exp(Agent :: agent(),
              Name :: service_name(),
              Key :: any(),
              Value :: binary(),
              Expiration :: non_neg_integer(),
              Timeout :: timeout_milliseconds()) ->
    external_response(any()).

set_exp(Agent, Name, Key, Value, Expiration, Timeout)
    when is_binary(Value), is_integer(Expiration) ->
    cloudi:send_sync(Agent, Name,
                     {set, Key, Value, Expiration}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Store a binary value for a key if the server already holds data for the key.===
%% @end
%%-------------------------------------------------------------------------

-spec replace(Agent :: agent(),
              Name :: service_name(),
              Key :: any(),
              Value :: binary()) ->
    external_response(any()).

replace(Agent, Name, Key, Value)
    when is_binary(Value) ->
    cloudi:send_sync(Agent, Name,
                     {replace, Key, Value}).

-spec replace(Agent :: agent(),
              Name :: service_name(),
              Key :: any(),
              Value :: binary(),
              Timeout :: timeout_milliseconds()) ->
    external_response(any()).

replace(Agent, Name, Key, Value, Timeout)
    when is_binary(Value) ->
    cloudi:send_sync(Agent, Name,
                     {replace, Key, Value}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Store a binary value with an expiration for a key if the server already holds data for the key.===
%% @end
%%-------------------------------------------------------------------------

-spec replace_exp(Agent :: agent(),
                  Name :: service_name(),
                  Key :: any(),
                  Value :: binary(),
                  Expiration :: non_neg_integer()) ->
    external_response(any()).

replace_exp(Agent, Name, Key, Value, Expiration)
    when is_binary(Value), is_integer(Expiration) ->
    cloudi:send_sync(Agent, Name,
                     {replace, Key, Value, Expiration}).

-spec replace_exp(Agent :: agent(),
                  Name :: service_name(),
                  Key :: any(),
                  Value :: binary(),
                  Expiration :: non_neg_integer(),
                  Timeout :: timeout_milliseconds()) ->
    external_response(any()).

replace_exp(Agent, Name, Key, Value, Expiration, Timeout)
    when is_binary(Value), is_integer(Expiration) ->
    cloudi:send_sync(Agent, Name,
                     {replace, Key, Value, Expiration}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Remove the binary value stored for a key.===
%% @end
%%-------------------------------------------------------------------------

-spec delete(Agent :: agent(),
             Name :: service_name(),
             Key :: any()) ->
    external_response(any()).

delete(Agent, Name, Key) ->
    cloudi:send_sync(Agent, Name,
                     {delete, Key}).

-spec delete(Agent :: agent(),
             Name :: service_name(),
             Key :: any(),
             Timeout :: timeout_milliseconds()) ->
    external_response(any()).

delete(Agent, Name, Key, Timeout) ->
    cloudi:send_sync(Agent, Name,
                     {delete, Key}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Increment an integer value stored for a key with the provided initial value and expiration.===
%% @end
%%-------------------------------------------------------------------------

-spec increment(Agent :: agent(),
                Name :: service_name(),
                Key :: any(),
                Value :: binary(),
                Initial :: binary(),
                Expiration :: non_neg_integer()) ->
    external_response(any()).

increment(Agent, Name, Key, Value, Initial, Expiration)
    when is_binary(Value), is_binary(Initial), is_integer(Expiration) ->
    cloudi:send_sync(Agent, Name,
                     {increment, Key, Value, Initial, Expiration}).

-spec increment(Agent :: agent(),
                Name :: service_name(),
                Key :: any(),
                Value :: binary(),
                Initial :: binary(),
                Expiration :: non_neg_integer(),
                Timeout :: timeout_milliseconds()) ->
    external_response(any()).

increment(Agent, Name, Key, Value, Initial, Expiration, Timeout)
    when is_binary(Value), is_binary(Initial), is_integer(Expiration) ->
    cloudi:send_sync(Agent, Name,
                     {increment, Key, Value, Initial, Expiration}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Decrement an integer value stored for a key with the provided initial value and expiration.===
%% @end
%%-------------------------------------------------------------------------

-spec decrement(Agent :: agent(),
                Name :: service_name(),
                Key :: any(),
                Value :: binary(),
                Initial :: binary(),
                Expiration :: non_neg_integer()) ->
    external_response(any()).

decrement(Agent, Name, Key, Value, Initial, Expiration)
    when is_binary(Value), is_binary(Initial), is_integer(Expiration) ->
    cloudi:send_sync(Agent, Name,
                     {decrement, Key, Value, Initial, Expiration}).

-spec decrement(Agent :: agent(),
                Name :: service_name(),
                Key :: any(),
                Value :: binary(),
                Initial :: binary(),
                Expiration :: non_neg_integer(),
                Timeout :: timeout_milliseconds()) ->
    external_response(any()).

decrement(Agent, Name, Key, Value, Initial, Expiration, Timeout)
    when is_binary(Value), is_binary(Initial), is_integer(Expiration) ->
    cloudi:send_sync(Agent, Name,
                     {decrement, Key, Value, Initial, Expiration}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add the binary value after the existing value for the key.===
%% @end
%%-------------------------------------------------------------------------

-spec append(Agent :: agent(),
             Name :: service_name(),
             Key :: any(),
             Value :: binary()) ->
    external_response(any()).

append(Agent, Name, Key, Value)
    when is_binary(Value) ->
    cloudi:send_sync(Agent, Name,
                     {append, Key, Value}).

-spec append(Agent :: agent(),
             Name :: service_name(),
             Key :: any(),
             Value :: binary(),
             Timeout :: timeout_milliseconds()) ->
    external_response(any()).

append(Agent, Name, Key, Value, Timeout)
    when is_binary(Value) ->
    cloudi:send_sync(Agent, Name,
                     {append, Key, Value}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add the binary value before the existing value for the key.===
%% @end
%%-------------------------------------------------------------------------

-spec prepend(Agent :: agent(),
              Name :: service_name(),
              Key :: any(),
              Value :: binary()) ->
    external_response(any()).

prepend(Agent, Name, Key, Value)
    when is_binary(Value) ->
    cloudi:send_sync(Agent, Name,
                     {prepend, Key, Value}).

-spec prepend(Agent :: agent(),
              Name :: service_name(),
              Key :: any(),
              Value :: binary(),
              Timeout :: timeout_milliseconds()) ->
    external_response(any()).

prepend(Agent, Name, Key, Value, Timeout)
    when is_binary(Value) ->
    cloudi:send_sync(Agent, Name,
                     {prepend, Key, Value}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get internal server statistics===
%% @end
%%-------------------------------------------------------------------------

-spec stats(Agent :: agent(),
            Name :: service_name()) ->
    external_response(any()).

stats(Agent, Name) ->
    cloudi:send_sync(Agent, Name,
                     stats).

-spec stats(Agent :: agent(),
            Name :: service_name(),
            Timeout :: timeout_milliseconds()) ->
    external_response(any()).

stats(Agent, Name, Timeout) ->
    cloudi:send_sync(Agent, Name,
                     stats, Timeout).
    
%%-------------------------------------------------------------------------
%% @doc
%% ===Invalidate all data currently stored.===
%% @end
%%-------------------------------------------------------------------------

-spec flush(Agent :: agent(),
            Name :: service_name()) ->
    external_response(any()).

flush(Agent, Name) ->
    cloudi:send_sync(Agent, Name,
                     flush).

-spec flush(Agent :: agent(),
            Name :: service_name(),
            Timeout :: timeout_milliseconds()) ->
    external_response(any()).

flush(Agent, Name, Timeout) ->
    cloudi:send_sync(Agent, Name,
                     flush, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Invalidate all data currently stored after an expiration.===
%% @end
%%-------------------------------------------------------------------------

-spec flush_exp(Agent :: agent(),
                Name :: service_name(),
                Expiration :: non_neg_integer()) ->
    external_response(any()).

flush_exp(Agent, Name, Expiration)
    when is_integer(Expiration) ->
    cloudi:send_sync(Agent, Name,
                     {flush, Expiration}).

-spec flush_exp(Agent :: agent(),
                Name :: service_name(),
                Expiration :: non_neg_integer(),
                Timeout :: timeout_milliseconds()) ->
    external_response(any()).

flush_exp(Agent, Name, Expiration, Timeout)
    when is_integer(Expiration) ->
    cloudi:send_sync(Agent, Name,
                     {flush, Expiration}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Make the server close the connection.===
%% @end
%%-------------------------------------------------------------------------

-spec quit(Agent :: agent(),
           Name :: service_name()) ->
    external_response(any()).

quit(Agent, Name) ->
    cloudi:send_sync(Agent, Name,
                     quit).

-spec quit(Agent :: agent(),
           Name :: service_name(),
           Timeout :: timeout_milliseconds()) ->
    external_response(any()).

quit(Agent, Name, Timeout) ->
    cloudi:send_sync(Agent, Name,
                     quit, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the server's version string.===
%% @end
%%-------------------------------------------------------------------------

-spec version(Agent :: agent(),
              Name :: service_name()) ->
    external_response(any()).

version(Agent, Name) ->
    cloudi:send_sync(Agent, Name,
                     version).

-spec version(Agent :: agent(),
              Name :: service_name(),
              Timeout :: timeout_milliseconds()) ->
    external_response(any()).

version(Agent, Name, Timeout) ->
    cloudi:send_sync(Agent, Name,
                     version, Timeout).

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init([{database, DatabaseName,
                    [{_,_,_}|_] = HostList}], _Prefix, _Timeout, Dispatcher) ->
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
            {reply, cloudi_x_ememcached:
                    get(Process, Key, Timeout), State};
        {'get_many', Keys} ->
            {reply, cloudi_x_ememcached:
                    get_many(Process, Keys, Timeout), State};
        {'add', Key, Value} ->
            {reply, cloudi_x_ememcached:
                    add(Process, Key, Value, Timeout), State};
        {'add', Key, Value, Expiration} ->
            {reply, cloudi_x_ememcached:
                    add_exp(Process, Key, Value, Expiration, Timeout), State};
        {'set', Key, Value} ->
            {reply, cloudi_x_ememcached:
                    set(Process, Key, Value, Timeout), State};
        {'set', Key, Value, Expiration} ->
            {reply, cloudi_x_ememcached:
                    set_exp(Process, Key, Value, Expiration, Timeout), State};
        {'replace', Key, Value} ->
            {reply, cloudi_x_ememcached:
                    replace(Process, Key, Value, Timeout), State};
        {'replace', Key, Value, Expiration} ->
            {reply, cloudi_x_ememcached:
                    replace_exp(Process, Key, Value,
                                Expiration, Timeout), State};
        {'delete', Key} ->
            {reply, cloudi_x_ememcached:
                    delete(Process, Key, Timeout), State};
        {'increment', Key, Value, Initial, Expiration} ->
            {reply, cloudi_x_ememcached:
                    increment_exp(Process, Key, Value, Initial,
                                  Expiration, Timeout), State};
        {'decrement', Key, Value, Initial, Expiration} ->
            {reply, cloudi_x_ememcached:
                    decrement_exp(Process, Key, Value, Initial,
                                  Expiration, Timeout), State};
        {'append', Key, Value} ->
            {reply, cloudi_x_ememcached:
                    append(Process, Key, Value, Timeout), State};
        {'prepend', Key, Value} ->
            {reply, cloudi_x_ememcached:
                    prepend(Process, Key, Value, Timeout), State};
        'stats' ->
            {reply, cloudi_x_ememcached:
                    stats(Process, Timeout), State};
        'flush' ->
            {reply, cloudi_x_ememcached:
                    flush(Process, Timeout), State};
        {'flush', Expiration} ->
            {reply, cloudi_x_ememcached:
                    flush_exp(Process, Expiration, Timeout), State};
        'quit' ->
            {reply, cloudi_x_ememcached:
                    quit(Process, Timeout), State};
        'version' ->
            {reply, cloudi_x_ememcached:
                    version(Process, Timeout), State}
    end.

cloudi_service_handle_info(Request, State, _Dispatcher) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(_Reason, _Timeout,
                         undefined) ->
    ok;
cloudi_service_terminate(_Reason, _Timeout,
                         #state{process = Process}) ->
    erlang:exit(Process, normal),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

%% do a single query and return a boolean to determine if the query succeeded
do_query(Query, Process, Timeout) ->
    try (case cloudi_string:binary_to_term(Query) of
        {'get', Key} ->
            cloudi_x_ememcached:
            get(Process, Key, Timeout);
        {'get_many', Keys} when is_list(Keys) ->
            cloudi_x_ememcached:
            get_many(Process, Keys, Timeout);
        {'add', Key, Value}
            when is_binary(Value) ->
            cloudi_x_ememcached:
            add(Process, Key, Value, Timeout);
        {'add', Key, Value, Expiration}
            when is_binary(Value), is_integer(Expiration) ->
            cloudi_x_ememcached:
            add_exp(Process, Key, Value, Expiration, Timeout);
        {'set', Key, Value}
            when is_binary(Value) ->
            cloudi_x_ememcached:
            set(Process, Key, Value, Timeout);
        {'set', Key, Value, Expiration}
            when is_binary(Value), is_integer(Expiration) ->
            cloudi_x_ememcached:
            set_exp(Process, Key, Value, Expiration, Timeout);
        {'replace', Key, Value}
            when is_binary(Value) ->
            cloudi_x_ememcached:
            replace(Process, Key, Value, Timeout);
        {'replace', Key, Value, Expiration}
            when is_binary(Value), is_integer(Expiration) ->
            cloudi_x_ememcached:
            replace_exp(Process, Key, Value, Expiration, Timeout);
        {'delete', Key} ->
            cloudi_x_ememcached:
            delete(Process, Key, Timeout);
        {'increment', Key, Value, Initial, Expiration}
            when is_binary(Value), is_binary(Initial),
                 is_integer(Expiration) ->
            cloudi_x_ememcached:
            increment_exp(Process, Key, Value, Initial, Expiration, Timeout);
        {'decrement', Key, Value, Initial, Expiration}
            when is_binary(Value), is_binary(Initial),
                 is_integer(Expiration) ->
            cloudi_x_ememcached:
            decrement_exp(Process, Key, Value, Initial, Expiration, Timeout);
        {'append', Key, Value}
            when is_binary(Value) ->
            cloudi_x_ememcached:
            append(Process, Key, Value, Timeout);
        {'prepend', Key, Value}
            when is_binary(Value) ->
            cloudi_x_ememcached:
            prepend(Process, Key, Value, Timeout);
        'stats' ->
            cloudi_x_ememcached:
            stats(Process, Timeout);
        'flush' ->
            cloudi_x_ememcached:
            flush(Process, Timeout);
        {'flush', Expiration}
            when is_integer(Expiration) ->
            cloudi_x_ememcached:
            flush_exp(Process, Expiration, Timeout);
        'quit' ->
            cloudi_x_ememcached:
            quit(Process, Timeout);
        'version' ->
            cloudi_x_ememcached:
            version(Process, Timeout);
        _ ->
            {error, invalid_call}
        end) of
        {error, invalid_call} ->
            ?LOG_DEBUG("Invalid memcached command tuple ~p",
                       [erlang:binary_to_list(Query)]),
            <<>>;
        Result when is_binary(Result) ->
            Result
    catch
        _:Reason ->
            ?LOG_DEBUG("exception when processing "
                       "memcached command tuple ~p: ~p",
                       [erlang:binary_to_list(Query), Reason]),
            <<>>
    end.

