%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Tokyo Tyrant Data Module==
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

-module(cloudi_service_db_tokyotyrant).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_service).

%% external interface

%% basic tokyo tyrant API
-export([addint/4, addint/5, 
         adddouble/4, adddouble/5, 
         adddouble_parts/5, adddouble_parts/6, 
         copy/3, copy/4, 
         fwmkeys/4, fwmkeys/5, 
         get/3, get/4, 
         iterinit/2, iterinit/3, 
         iternext/2, iternext/3, 
         mget/3, mget/4, 
         optimize/3, optimize/4, 
         out/3, out/4, 
         put/4, put/5, 
         putcat/4, putcat/5, 
         putkeep/4, putkeep/5, 
         putnr/4, putnr/5, 
         putshl/5, putshl/6, 
         restore/4, restore/5, 
         rnum/2, rnum/3, 
         setmst/4, setmst/5, 
         size/2, size/3, 
         stat/2, stat/3, 
         sync/2, sync/3, 
         vanish/2, vanish/3, 
         vsiz/3, vsiz/4]).

%% table tokyo tyrant API
-export([genuid/2, genuid/3, 
         query_add_condition/6, query_add_condition/7, 
         query_limit/4, query_limit/5, 
         query_limit_skip/5, query_limit_skip/6, 
         query_order/5, query_order/6, 
         search/3, search/4, 
         searchcount/3, searchcount/4, 
         searchout/3, searchout/4, 
         setindex/4, setindex/5, 
         update/4, update/5]).

%% cloudi_service callbacks
-export([cloudi_service_init/4,
         cloudi_service_handle_request/11,
         cloudi_service_handle_info/3,
         cloudi_service_terminate/3]).

-include_lib("cloudi_core/include/cloudi_logger.hrl").

-define(DEFAULT_HOST_NAME,    "127.0.0.1").
-define(DEFAULT_PORT,         1978).
-define(DEFAULT_TIMEOUT,      20000). % 20 seconds
-define(DEFAULT_CONNECTIONS,  8). % number of work threads in tyrant server
-define(DEFAULT_AUTO_SYNC,    false). % or number of seconds
-define(DEFAULT_AUTO_TUNE,    false). % or number of seconds
-define(DEFAULT_NATIVE,       false). % if true then erlang term storage
-define(DEFAULT_CONNECT_OPTS, []). % gen_tcp:connect/3 options
% automate database backups {"/path/to/backup/database/file", 60}
% with a 60 second minimum interval
-define(DEFAULT_AUTO_COPY,    undefined).
% run the tokyo tyrant server as an erlang port and manage
% it by reading its log output on stdout (should not be used)
% uses a proplist with:
%     tyrant_bin, tyrant_opts, data_file, tuning_opts, and port_opts
-define(DEFAULT_RUN_SERVER,   undefined).

-record(state,
    {
        connection
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
%% ===Add an integer to the value in the database.===
%% @end
%%-------------------------------------------------------------------------

-spec addint(Agent :: agent(),
             Name :: service_name(),
             Key :: iolist(),
             Integer :: integer()) ->
    external_response(any()).

addint(Agent, Name, Key, Integer)
    when is_integer(Integer) ->
    cloudi:send_sync(Agent, Name,
                     {addint, Key, Integer}).

-spec addint(Agent :: agent(),
             Name :: service_name(),
             Key :: iolist(),
             Integer :: integer(),
             Timeout :: timeout_milliseconds()) ->
    external_response(any()).

addint(Agent, Name, Key, Integer, Timeout)
    when is_integer(Integer) ->
    true = is_list(Key) orelse is_binary(Key),
    cloudi:send_sync(Agent, Name,
                     {addint, Key, Integer}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add a double to the value in the database.===
%% @end
%%-------------------------------------------------------------------------

-spec adddouble(Agent :: agent(),
                Name :: service_name(),
                Key :: iolist(),
                Double :: float()) ->
    external_response(any()).

adddouble(Agent, Name, Key, Double)
    when is_float(Double) ->
    true = is_list(Key) orelse is_binary(Key),
    cloudi:send_sync(Agent, Name,
                     {adddouble, Key, Double}).

-spec adddouble(Agent :: agent(),
                Name :: service_name(),
                Key :: iolist(),
                Double :: float(),
                Timeout :: timeout_milliseconds()) ->
    external_response(any()).

adddouble(Agent, Name, Key, Double, Timeout)
    when is_float(Double) ->
    true = is_list(Key) orelse is_binary(Key),
    cloudi:send_sync(Agent, Name,
                     {adddouble, Key, Double}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add a double in separate parts to the value in the database.===
%% @end
%%-------------------------------------------------------------------------

-spec adddouble_parts(Agent :: agent(),
                      Name :: service_name(),
                      Key :: iolist(),
                      IntegerPart :: integer(),
                      FractionalPart :: integer()) ->
    external_response(any()).

adddouble_parts(Agent, Name, Key, IntegerPart, FractionalPart)
    when is_integer(IntegerPart), is_integer(FractionalPart) ->
    true = is_list(Key) orelse is_binary(Key),
    cloudi:send_sync(Agent, Name,
                     {adddouble_parts, Key,
                      IntegerPart, FractionalPart}).

-spec adddouble_parts(Agent :: agent(),
                      Name :: service_name(),
                      Key :: iolist(),
                      IntegerPart :: integer(),
                      FractionalPart :: integer(),
                      Timeout :: timeout_milliseconds()) ->
    external_response(any()).

adddouble_parts(Agent, Name, Key, IntegerPart, FractionalPart, Timeout)
    when is_integer(IntegerPart), is_integer(FractionalPart) ->
    true = is_list(Key) orelse is_binary(Key),
    cloudi:send_sync(Agent, Name,
                     {adddouble_parts, Key,
                      IntegerPart, FractionalPart},
                     Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Copy the database file.===
%% @end
%%-------------------------------------------------------------------------

-spec copy(Agent :: agent(),
           Name :: service_name(),
           PathName :: string()) ->
    external_response(any()).

copy(Agent, Name, PathName) ->
    true = is_list(PathName) orelse is_binary(PathName),
    cloudi:send_sync(Agent, Name,
                     {copy, PathName}).

-spec copy(Agent :: agent(),
           Name :: service_name(),
           PathName :: string(),
           Timeout :: timeout_milliseconds()) ->
    external_response(any()).

copy(Agent, Name, PathName, Timeout) ->
    true = is_list(PathName) orelse is_binary(PathName),
    cloudi:send_sync(Agent, Name,
                     {copy, PathName}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Match some number of keys on a prefix.===
%% @end
%%-------------------------------------------------------------------------

-spec fwmkeys(Agent :: agent(),
              Name :: service_name(),
              Prefix :: iolist(),
              MaxKeys :: non_neg_integer()) ->
    external_response(any()).

fwmkeys(Agent, Name, Prefix, MaxKeys)
    when is_integer(MaxKeys) ->
    true = is_list(Prefix) orelse is_binary(Prefix),
    cloudi:send_sync(Agent, Name,
                     {fwmkeys, Prefix, MaxKeys}).

-spec fwmkeys(Agent :: agent(),
              Name :: service_name(),
              Prefix :: iolist(),
              MaxKeys :: non_neg_integer(),
              Timeout :: timeout_milliseconds()) ->
    external_response(any()).

fwmkeys(Agent, Name, Prefix, MaxKeys, Timeout)
    when is_integer(MaxKeys) ->
    true = is_list(Prefix) orelse is_binary(Prefix),
    cloudi:send_sync(Agent, Name,
                     {fwmkeys, Prefix, MaxKeys}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a value from the database.===
%% @end
%%-------------------------------------------------------------------------

-spec get(Agent :: agent(),
          Name :: service_name(),
          Key :: iolist()) ->
    external_response(any()).

get(Agent, Name, Key) ->
    true = is_list(Key) orelse is_binary(Key),
    cloudi:send_sync(Agent, Name,
                     {get, Key}).

-spec get(Agent :: agent(),
          Name :: service_name(),
          Key :: iolist(),
          Timeout :: timeout_milliseconds()) ->
    external_response(any()).

get(Agent, Name, Key, Timeout) ->
    true = is_list(Key) orelse is_binary(Key),
    cloudi:send_sync(Agent, Name,
                     {get, Key}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Initialize an iterator on all the keys in the database.===
%% @end
%%-------------------------------------------------------------------------

-spec iterinit(Agent :: agent(),
               Name :: service_name()) ->
    external_response(any()).

iterinit(Agent, Name) ->
    cloudi:send_sync(Agent, Name,
                     iterinit).

-spec iterinit(Agent :: agent(),
               Name :: service_name(),
               Timeout :: timeout_milliseconds()) ->
    external_response(any()).

iterinit(Agent, Name, Timeout) ->
    cloudi:send_sync(Agent, Name,
                     iterinit, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the next key from the iterator on all the keys in the database.===
%% @end
%%-------------------------------------------------------------------------

-spec iternext(Agent :: agent(),
               Name :: service_name()) ->
    external_response(any()).

iternext(Agent, Name) ->
    cloudi:send_sync(Agent, Name,
                     iternext).

-spec iternext(Agent :: agent(),
               Name :: service_name(),
               Timeout :: timeout_milliseconds()) ->
    external_response(any()).

iternext(Agent, Name, Timeout) ->
    cloudi:send_sync(Agent, Name,
                     iternext, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get multiple values from the database.===
%% @end
%%-------------------------------------------------------------------------

-spec mget(Agent :: agent(),
           Name :: service_name(),
           KeyList :: list(iolist())) ->
    external_response(any()).

mget(Agent, Name, KeyList)
    when is_list(KeyList) ->
    cloudi:send_sync(Agent, Name,
                     {mget, KeyList}).

-spec mget(Agent :: agent(),
           Name :: service_name(),
           KeyList :: list(iolist()),
           Timeout :: timeout_milliseconds()) ->
    external_response(any()).

mget(Agent, Name, KeyList, Timeout)
    when is_list(KeyList) ->
    cloudi:send_sync(Agent, Name,
                     {mget, KeyList}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Optimize the storage of values in the database.===
%% @end
%%-------------------------------------------------------------------------

-spec optimize(Agent :: agent(),
               Name :: service_name(),
               TuningOptions :: iolist()) ->
    external_response(any()).

optimize(Agent, Name, TuningOptions) ->
    true = is_list(TuningOptions) orelse is_binary(TuningOptions),
    cloudi:send_sync(Agent, Name,
                     {optimize, TuningOptions}).

-spec optimize(Agent :: agent(),
               Name :: service_name(),
               TuningOptions :: iolist(),
               Timeout :: timeout_milliseconds()) ->
    external_response(any()).

optimize(Agent, Name, TuningOptions, Timeout) ->
    true = is_list(TuningOptions) orelse is_binary(TuningOptions),
    cloudi:send_sync(Agent, Name,
                     {optimize, TuningOptions}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Erase a value from the database.===
%% @end
%%-------------------------------------------------------------------------

-spec out(Agent :: agent(),
          Name :: service_name(),
          Key :: iolist()) ->
    external_response(any()).

out(Agent, Name, Key) ->
    true = is_list(Key) orelse is_binary(Key),
    cloudi:send_sync(Agent, Name,
                     {out, Key}).

-spec out(Agent :: agent(),
          Name :: service_name(),
          Key :: iolist(),
          Timeout :: timeout_milliseconds()) ->
    external_response(any()).

out(Agent, Name, Key, Timeout) ->
    true = is_list(Key) orelse is_binary(Key),
    cloudi:send_sync(Agent, Name,
                     {out, Key}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Store a value in the database.===
%% @end
%%-------------------------------------------------------------------------

-spec put(Agent :: agent(),
          Name :: service_name(),
          Key :: iolist(),
          Value :: integer() | float() | list() | binary()) ->
    external_response(any()).

put(Agent, Name, Key, Value) ->
    true = is_list(Key) orelse is_binary(Key),
    true = is_integer(Value) orelse is_float(Value) or
           is_list(Value) orelse is_binary(Value),
    cloudi:send_sync(Agent, Name,
                     {put, Key, Value}).

-spec put(Agent :: agent(),
          Name :: service_name(),
          Key :: iolist(),
          Value :: integer() | float() | list() | binary(),
          Timeout :: timeout_milliseconds()) ->
    external_response(any()).

put(Agent, Name, Key, Value, Timeout) ->
    true = is_list(Key) orelse is_binary(Key),
    true = is_integer(Value) orelse is_float(Value) or
           is_list(Value) orelse is_binary(Value),
    cloudi:send_sync(Agent, Name,
                     {put, Key, Value}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Append a value to the existing value in the database.===
%% @end
%%-------------------------------------------------------------------------

-spec putcat(Agent :: agent(),
             Name :: service_name(),
             Key :: iolist(),
             Value :: integer() | float() | list() | binary()) ->
    external_response(any()).

putcat(Agent, Name, Key, Value) ->
    true = is_list(Key) orelse is_binary(Key),
    true = is_integer(Value) orelse is_float(Value) or
           is_list(Value) orelse is_binary(Value),
    cloudi:send_sync(Agent, Name,
                     {putcat, Key, Value}).

-spec putcat(Agent :: agent(),
             Name :: service_name(),
             Key :: iolist(),
             Value :: integer() | float() | list() | binary(),
             Timeout :: timeout_milliseconds()) ->
    external_response(any()).

putcat(Agent, Name, Key, Value, Timeout) ->
    true = is_list(Key) orelse is_binary(Key),
    true = is_integer(Value) orelse is_float(Value) or
           is_list(Value) orelse is_binary(Value),
    cloudi:send_sync(Agent, Name,
                     {putcat, Key, Value}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Store a value in the database only if there currently is no value for the given key.===
%% @end
%%-------------------------------------------------------------------------

-spec putkeep(Agent :: agent(),
              Name :: service_name(),
              Key :: iolist(),
              Value :: integer() | float() | list() | binary()) ->
    external_response(any()).

putkeep(Agent, Name, Key, Value) ->
    true = is_list(Key) orelse is_binary(Key),
    true = is_integer(Value) orelse is_float(Value) or
           is_list(Value) orelse is_binary(Value),
    cloudi:send_sync(Agent, Name,
                     {putkeep, Key, Value}).

-spec putkeep(Agent :: agent(),
              Name :: service_name(),
              Key :: iolist(),
              Value :: integer() | float() | list() | binary(),
              Timeout :: timeout_milliseconds()) ->
    external_response(any()).

putkeep(Agent, Name, Key, Value, Timeout) ->
    true = is_list(Key) orelse is_binary(Key),
    true = is_integer(Value) orelse is_float(Value) or
           is_list(Value) orelse is_binary(Value),
    cloudi:send_sync(Agent, Name,
                     {putkeep, Key, Value}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Store a value in the database but do not wait for the database to acknowledge the change.===
%% @end
%%-------------------------------------------------------------------------

-spec putnr(Agent :: agent(),
            Name :: service_name(),
            Key :: iolist(),
            Value :: integer() | float() | list() | binary()) ->
    external_response(any()).

putnr(Agent, Name, Key, Value) ->
    true = is_list(Key) orelse is_binary(Key),
    true = is_integer(Value) orelse is_float(Value) or
           is_list(Value) orelse is_binary(Value),
    cloudi:send_sync(Agent, Name,
                     {putnr, Key, Value}).

-spec putnr(Agent :: agent(),
            Name :: service_name(),
            Key :: iolist(),
            Value :: integer() | float() | list() | binary(),
            Timeout :: timeout_milliseconds()) ->
    external_response(any()).

putnr(Agent, Name, Key, Value, Timeout) ->
    true = is_list(Key) orelse is_binary(Key),
    true = is_integer(Value) orelse is_float(Value) or
           is_list(Value) orelse is_binary(Value),
    cloudi:send_sync(Agent, Name,
                     {putnr, Key, Value}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Append a value to the existing value in the database and shift it left.===
%% @end
%%-------------------------------------------------------------------------

-spec putshl(Agent :: agent(),
             Name :: service_name(),
             Key :: iolist(),
             Value :: integer() | float() | list() | binary(),
             Width :: integer()) ->
    external_response(any()).

putshl(Agent, Name, Key, Value, Width)
    when is_integer(Width) ->
    true = is_list(Key) orelse is_binary(Key),
    true = is_integer(Value) orelse is_float(Value) or
           is_list(Value) orelse is_binary(Value),
    cloudi:send_sync(Agent, Name,
                     {putshl, Key, Value, Width}).


-spec putshl(Agent :: agent(),
             Name :: service_name(),
             Key :: iolist(),
             Value :: integer() | float() | list() | binary(),
             Width :: integer(),
             Timeout :: timeout_milliseconds()) ->
    external_response(any()).

putshl(Agent, Name, Key, Value, Width, Timeout)
    when is_integer(Width) ->
    true = is_list(Key) orelse is_binary(Key),
    true = is_integer(Value) orelse is_float(Value) or
           is_list(Value) orelse is_binary(Value),
    cloudi:send_sync(Agent, Name,
                     {putshl, Key, Value, Width}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Restore a database file from the update log.===
%% @end
%%-------------------------------------------------------------------------

-spec restore(Agent :: agent(),
              Name :: service_name(),
              PathName :: iolist(),
              TimeStamp :: integer()) ->
    external_response(any()).

restore(Agent, Name, PathName, TimeStamp)
    when is_integer(TimeStamp) ->
    true = is_list(PathName) orelse is_binary(PathName),
    cloudi:send_sync(Agent, Name,
                     {restore, PathName, TimeStamp}).

-spec restore(Agent :: agent(),
              Name :: service_name(),
              PathName :: iolist(),
              TimeStamp :: integer(),
              Timeout :: timeout_milliseconds()) ->
    external_response(any()).

restore(Agent, Name, PathName, TimeStamp, Timeout)
    when is_integer(TimeStamp) ->
    true = is_list(PathName) orelse is_binary(PathName),
    cloudi:send_sync(Agent, Name,
                     {restore, PathName, TimeStamp}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the number of values currently in the database.===
%% @end
%%-------------------------------------------------------------------------

-spec rnum(Agent :: agent(),
           Name :: service_name()) ->
    external_response(any()).

rnum(Agent, Name) ->
    cloudi:send_sync(Agent, Name,
                     rnum).

-spec rnum(Agent :: agent(),
           Name :: service_name(),
           Timeout :: timeout_milliseconds()) ->
    external_response(any()).

rnum(Agent, Name, Timeout) ->
    cloudi:send_sync(Agent, Name,
                     rnum, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Set the replication master of the database.===
%% @end
%%-------------------------------------------------------------------------

-spec setmst(Agent :: agent(),
             Name :: service_name(),
             Host :: iolist(),
             Port :: integer()) ->
    external_response(any()).

setmst(Agent, Name, HostName, Port)
    when is_integer(Port) ->
    true = is_list(HostName) orelse is_binary(HostName),
    cloudi:send_sync(Agent, Name,
                     {setmst, HostName, Port}).

-spec setmst(Agent :: agent(),
             Name :: service_name(),
             Host :: iolist(),
             Port :: integer(),
             Timeout :: timeout_milliseconds()) ->
    external_response(any()).

setmst(Agent, Name, HostName, Port, Timeout)
    when is_integer(Port) ->
    true = is_list(HostName) orelse is_binary(HostName),
    cloudi:send_sync(Agent, Name,
                     {setmst, HostName, Port}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the size of the database.===
%% @end
%%-------------------------------------------------------------------------

-spec size(Agent :: agent(),
           Name :: service_name()) ->
    external_response(any()).

size(Agent, Name) ->
    cloudi:send_sync(Agent, Name,
                     size).

-spec size(Agent :: agent(),
           Name :: service_name(),
           Timeout :: timeout_milliseconds()) ->
    external_response(any()).

size(Agent, Name, Timeout) ->
    cloudi:send_sync(Agent, Name,
                     size, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the status string from the database.===
%% @end
%%-------------------------------------------------------------------------

-spec stat(Agent :: agent(),
           Name :: service_name()) ->
    external_response(any()).

stat(Agent, Name) ->
    cloudi:send_sync(Agent, Name,
                     stat).

-spec stat(Agent :: agent(),
           Name :: service_name(),
           Timeout :: timeout_milliseconds()) ->
    external_response(any()).

stat(Agent, Name, Timeout) ->
    cloudi:send_sync(Agent, Name,
                     stat, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Synchronize the updated contents of the database with the filesystem.===
%% @end
%%-------------------------------------------------------------------------

-spec sync(Agent :: agent(),
           Name :: service_name()) ->
    external_response(any()).

sync(Agent, Name) ->
    cloudi:send_sync(Agent, Name,
                     sync).

-spec sync(Agent :: agent(),
           Name :: service_name(),
           Timeout :: timeout_milliseconds()) ->
    external_response(any()).

sync(Agent, Name, Timeout) ->
    cloudi:send_sync(Agent, Name,
                     sync, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Erase all values from the database.===
%% @end
%%-------------------------------------------------------------------------

-spec vanish(Agent :: agent(),
             Name :: service_name()) ->
    external_response(any()).

vanish(Agent, Name) ->
    cloudi:send_sync(Agent, Name,
                     vanish).

-spec vanish(Agent :: agent(),
             Name :: service_name(),
             Timeout :: timeout_milliseconds()) ->
    external_response(any()).

vanish(Agent, Name, Timeout) ->
    cloudi:send_sync(Agent, Name,
                     vanish, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the size of a value in the database.===
%% @end
%%-------------------------------------------------------------------------

-spec vsiz(Agent :: agent(),
           Name :: service_name(),
           Key :: iolist()) ->
    external_response(any()).

vsiz(Agent, Name, Key) ->
    true = is_list(Key) orelse is_binary(Key),
    cloudi:send_sync(Agent, Name,
                     {vsiz, Key}).

-spec vsiz(Agent :: agent(),
           Name :: service_name(),
           Key :: iolist(),
           Timeout :: timeout_milliseconds()) ->
    external_response(any()).

vsiz(Agent, Name, Key, Timeout) ->
    true = is_list(Key) orelse is_binary(Key),
    cloudi:send_sync(Agent, Name,
                     {vsiz, Key}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Generate a unique ID number for the database.===
%% @end
%%-------------------------------------------------------------------------

-spec genuid(Agent :: agent(),
             Name :: service_name()) ->
    external_response(any()).

genuid(Agent, Name) ->
    cloudi:send_sync(Agent, Name,
                     genuid).

-spec genuid(Agent :: agent(),
             Name :: service_name(),
             Timeout :: timeout_milliseconds()) ->
    external_response(any()).

genuid(Agent, Name, Timeout) ->
    cloudi:send_sync(Agent, Name,
                     genuid, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add a constraint on a query.===
%% @end
%%-------------------------------------------------------------------------

-spec query_add_condition(Agent :: agent(),
                          Name :: service_name(),
                          OldQuery :: list(),
                          Column :: iolist(),
                          Op :: atom(),
                          ExprList :: list()) ->
    external_response(any()).

query_add_condition(Agent, Name, OldQuery, Column, Op, ExprList)
    when is_list(OldQuery), is_list(ExprList) ->
    true = is_list(Column) orelse is_binary(Column),
    true = is_atom(Op) orelse is_tuple(Op),
    cloudi:send_sync(Agent, Name,
                     {query_add_condition,
                      OldQuery, Column, Op, ExprList}).

-spec query_add_condition(Agent :: agent(),
                              Name :: service_name(),
                              OldQuery :: list(),
                              Column :: iolist(),
                              Op :: atom(),
                              ExprList :: list(),
                              Timeout :: timeout_milliseconds()) ->
    external_response(any()).

query_add_condition(Agent, Name, OldQuery, Column, Op, ExprList, Timeout)
    when is_list(OldQuery), is_list(ExprList) ->
    true = is_list(Column) orelse is_binary(Column),
    true = is_atom(Op) orelse is_tuple(Op),
    cloudi:send_sync(Agent, Name,
                     {query_add_condition,
                      OldQuery, Column, Op, ExprList}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Limit the number of values returned by a query.===
%% @end
%%-------------------------------------------------------------------------

-spec query_limit(Agent :: agent(),
                  Name :: service_name(),
                  OldQuery :: list(),
                  Max :: integer()) ->
    external_response(any()).

query_limit(Agent, Name, OldQuery, Max)
    when is_list(OldQuery), is_integer(Max) ->
    cloudi:send_sync(Agent, Name,
                     {query_limit, OldQuery, Max}).

-spec query_limit(Agent :: agent(),
                  Name :: service_name(),
                  OldQuery :: list(),
                  Max :: integer(),
                  Timeout :: timeout_milliseconds()) ->
    external_response(any()).

query_limit(Agent, Name, OldQuery, Max, Timeout)
    when is_list(OldQuery), is_integer(Max) ->
    cloudi:send_sync(Agent, Name,
                     {query_limit, OldQuery, Max}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===After applying an offset into the values returned by a query, limit the remaining results.===
%% @end
%%-------------------------------------------------------------------------

-spec query_limit_skip(Agent :: agent(),
                       Name :: service_name(),
                       OldQuery :: list(),
                       Max :: integer(),
                       Skip :: integer()) ->
    external_response(any()).

query_limit_skip(Agent, Name, OldQuery, Max, Skip)
    when is_list(OldQuery), is_integer(Max), is_integer(Skip) ->
    cloudi:send_sync(Agent, Name,
                     {query_limit_skip, OldQuery, Max, Skip}).

-spec query_limit_skip(Agent :: agent(),
                       Name :: service_name(),
                       OldQuery :: list(),
                       Max :: integer(),
                       Skip :: integer(),
                       Timeout :: timeout_milliseconds()) ->
    external_response(any()).

query_limit_skip(Agent, Name, OldQuery, Max, Skip, Timeout)
    when is_list(OldQuery), is_integer(Max), is_integer(Skip) ->
    cloudi:send_sync(Agent, Name,
                     {query_limit_skip, OldQuery, Max, Skip}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Set the order of a query.===
%% @end
%%-------------------------------------------------------------------------

-spec query_order(Agent :: agent(),
                  Name :: service_name(),
                  OldQuery :: list(),
                  Column :: 'primary' | iolist(),
                  Type :: atom()) ->
    external_response(any()).

query_order(Agent, Name, OldQuery, Column, Type)
    when is_list(OldQuery), is_atom(Type) ->
    true = (Column == primary) orelse is_list(Column) orelse is_binary(Column),
    cloudi:send_sync(Agent, Name,
                     {query_order, OldQuery, Column, Type}).

-spec query_order(Agent :: agent(),
                  Name :: service_name(),
                  OldQuery :: list(),
                  Column :: 'primary' | iolist(),
                  Type :: atom(),
                  Timeout :: timeout_milliseconds()) ->
    external_response(any()).

query_order(Agent, Name, OldQuery, Column, Type, Timeout)
    when is_list(OldQuery), is_atom(Type) ->
    true = (Column == primary) orelse is_list(Column) orelse is_binary(Column),
    cloudi:send_sync(Agent, Name,
                     {query_order, OldQuery, Column, Type}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Execute the search of a query.===
%% @end
%%-------------------------------------------------------------------------

-spec search(Agent :: agent(),
             Name :: service_name(),
             Query :: list()) ->
    external_response(any()).

search(Agent, Name, Query)
    when is_list(Query) ->
    cloudi:send_sync(Agent, Name,
                     {search, Query}).

-spec search(Agent :: agent(),
             Name :: service_name(),
             Query :: list(),
             Timeout :: timeout_milliseconds()) ->
    external_response(any()).

search(Agent, Name, Query, Timeout)
    when is_list(Query) ->
    cloudi:send_sync(Agent, Name,
                     {search, Query}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the number of values returned from a query.===
%% @end
%%-------------------------------------------------------------------------

-spec searchcount(Agent :: agent(),
                  Name :: service_name(),
                  Query :: list()) ->
    external_response(any()).

searchcount(Agent, Name, Query)
    when is_list(Query) ->
    cloudi:send_sync(Agent, Name,
                     {searchcount, Query}).

-spec searchcount(Agent :: agent(),
                  Name :: service_name(),
                  Query :: list(),
                  Timeout :: timeout_milliseconds()) ->
    external_response(any()).

searchcount(Agent, Name, Query, Timeout)
    when is_list(Query) ->
    cloudi:send_sync(Agent, Name,
                     {searchcount, Query}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Erase all values that match the query.===
%% @end
%%-------------------------------------------------------------------------

-spec searchout(Agent :: agent(),
                Name :: service_name(),
                Query :: list()) ->
    external_response(any()).

searchout(Agent, Name, Query)
    when is_list(Query) ->
    cloudi:send_sync(Agent, Name,
                     {searchout, Query}).

-spec searchout(Agent :: agent(),
                Name :: service_name(),
                Query :: list(),
                Timeout :: timeout_milliseconds()) ->
    external_response(any()).

searchout(Agent, Name, Query, Timeout)
    when is_list(Query) ->
    cloudi:send_sync(Agent, Name,
                     {searchout, Query}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Set an index on a database table.===
%% @end
%%-------------------------------------------------------------------------

-spec setindex(Agent :: agent(),
               Name :: service_name(),
               Column :: 'primary' | iolist(),
               Type :: atom()) ->
    external_response(any()).

setindex(Agent, Name, Column, Type)
    when is_atom(Type) ->
    true = (Column == primary) orelse is_list(Column) orelse is_binary(Column),
    cloudi:send_sync(Agent, Name,
                     {setindex, Column, Type}).

-spec setindex(Agent :: agent(),
               Name :: service_name(),
               Column :: 'primary' | iolist(),
               Type :: atom(),
               Timeout :: timeout_milliseconds()) ->
    external_response(any()).

setindex(Agent, Name, Column, Type, Timeout)
    when is_atom(Type) ->
    true = (Column == primary) orelse is_list(Column) orelse is_binary(Column),
    cloudi:send_sync(Agent, Name,
                     {setindex, Column, Type}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Append column data to an existing database table.===
%% @end
%%-------------------------------------------------------------------------

-spec update(Agent :: agent(),
             Name :: service_name(),
             Key :: iolist(),
             NewCols :: list()) ->
    external_response(any()).

update(Agent, Name, Key, NewCols)
    when is_list(NewCols) ->
    true = is_list(Key) orelse is_binary(Key),
    cloudi:send_sync(Agent, Name,
                     {update, Key, NewCols}).

-spec update(Agent :: agent(),
             Name :: service_name(),
             Key :: iolist(),
             NewCols :: list(),
             Timeout :: timeout_milliseconds()) ->
    external_response(any()).

update(Agent, Name, Key, NewCols, Timeout)
    when is_list(NewCols) ->
    true = is_list(Key) orelse is_binary(Key),
    cloudi:send_sync(Agent, Name,
                     {update, Key, NewCols}, Timeout).

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_service
%%%------------------------------------------------------------------------

cloudi_service_init([{database, DatabaseName, Args}],
                    _Prefix, _Timeout, Dispatcher) ->
    Defaults = [
        {hostname, ?DEFAULT_HOST_NAME},
        {port, ?DEFAULT_PORT},
        {timeout, ?DEFAULT_TIMEOUT},
        {num_connections, ?DEFAULT_CONNECTIONS},
        {auto_sync, ?DEFAULT_AUTO_SYNC},
        {auto_tune, ?DEFAULT_AUTO_TUNE},
        {native, ?DEFAULT_NATIVE},
        {connect_opts, ?DEFAULT_CONNECT_OPTS},
        {auto_copy, ?DEFAULT_AUTO_COPY},
        {run_server, ?DEFAULT_RUN_SERVER}],
    [HostName, Port, Timeout, Connections, AutoSync, AutoTune,
     Native, ConnectOpts, AutoCopy, RunServer] =
        cloudi_proplists:take_values(Defaults, Args),
    MediciOptions = [   
        {hostname, HostName},
        {port, Port},
        {timeout, Timeout},
        {num_connections, Connections},
        {auto_sync, AutoSync},
        {auto_tune, AutoTune},
        {native, Native},
        {connect_opts, ConnectOpts},
        {auto_copy, AutoCopy},
        {run_server, RunServer}],
    true = is_integer(Timeout), % do not use "infinity" timeout value
    case cloudi_x_medici:connect(MediciOptions) of
        {ok, Connection} ->
            cloudi_service:subscribe(Dispatcher, DatabaseName),
            {ok, #state{connection = Connection}};
        {error, Reason} ->
            {stop, Reason}
    end.

cloudi_service_handle_request(_Type, _Name, _Pattern, _RequestInfo, Request,
                              Timeout, _Priority, _TransId, _Pid,
                              #state{connection = Connection} = State,
                              _Dispatcher) ->
    case Request of
        %Command when is_binary(Command) ->
        %    Output = do_query(Command, Timeout, Connection),
        %    {reply, cloudi_response:new(Request, Output), State};
        {'addint', Key, Integer} ->
            {reply, cloudi_x_medici:
                    addint(Connection, Key, Integer, Timeout), State};
        {'adddouble', Key, Double} ->
            {reply, cloudi_x_medici:
                    adddouble(Connection, Key, Double, Timeout), State};
        {'adddouble_parts', Key, IntegerPart, FractionalPart} ->
            {reply, cloudi_x_medici:
                    adddouble_parts(Connection, Key, IntegerPart,
                                    FractionalPart, Timeout), State};
        {'copy', PathName} ->
            {reply, cloudi_x_medici:
                    copy(Connection, PathName, Timeout), State};
        {'fwmkeys', Prefix, MaxKeys} ->
            {reply, cloudi_x_medici:
                    fwmkeys(Connection, Prefix, MaxKeys, Timeout), State};
        {'get', Key} ->
            {reply, cloudi_x_medici:
                    get(Connection, Key, Timeout), State};
        'iterinit' ->
            {reply, cloudi_x_medici:
                    iterinit(Connection, Timeout), State};
        'iternext' ->
            {reply, cloudi_x_medici:
                    iternext(Connection, Timeout), State};
        {'mget', KeyList} ->
            {reply, cloudi_x_medici:
                    mget(Connection, KeyList, Timeout), State};
        {'optimize', TuningOptions} ->
            {reply, cloudi_x_medici:
                    optimize(Connection, TuningOptions, Timeout), State};
        {'out', Key} ->
            {reply, cloudi_x_medici:
                    out(Connection, Key, Timeout), State};
        {'put', Key, Value} ->
            {reply, cloudi_x_medici:
                    put(Connection, Key, Value, Timeout), State};
        {'putcat', Key, Value} ->
            {reply, cloudi_x_medici:
                    putcat(Connection, Key, Value, Timeout), State};
        {'putkeep', Key, Value} ->
            {reply, cloudi_x_medici:
                    putkeep(Connection, Key, Value, Timeout), State};
        {'putnr', Key, Value} ->
            {reply, cloudi_x_medici:
                    putnr(Connection, Key, Value, Timeout), State};
        {'putshl', Key, Value, Width} ->
            {reply, cloudi_x_medici:
                    putshl(Connection, Key, Value, Width, Timeout), State};
        {'restore', PathName, TimeStamp} ->
            {reply, cloudi_x_medici:
                    restore(Connection, PathName, TimeStamp, Timeout), State};
        'rnum' ->
            {reply, cloudi_x_medici:
                    rnum(Connection, Timeout), State};
        {'setmst', HostName, Port} ->
            {reply, cloudi_x_medici:
                    setmst(Connection, HostName, Port, Timeout), State};
        'size' ->
            {reply, cloudi_x_medici:
                    size(Connection, Timeout), State};
        'stat' ->
            {reply, cloudi_x_medici:
                    stat(Connection, Timeout), State};
        'sync' ->
            {reply, cloudi_x_medici:
                    sync(Connection, Timeout), State};
        'vanish' ->
            {reply, cloudi_x_medici:
                    vanish(Connection, Timeout), State};
        {'vsiz', Key} ->
            {reply, cloudi_x_medici:
                    vsiz(Connection, Key, Timeout), State};
        'genuid' ->
            {reply, cloudi_x_medici:
                    genuid(Connection, Timeout), State};
        {'query_add_condition', OldQuery, Column, Op, ExprList} ->
            {reply, cloudi_x_medici:
                    query_add_condition(Connection, OldQuery, Column,
                                        Op, ExprList, Timeout), State};
        {'query_limit', OldQuery, Max} ->
            {reply, cloudi_x_medici:
                    query_limit(Connection, OldQuery, Max, Timeout), State};
        {'query_limit_skip', OldQuery, Max, Skip} ->
            {reply, cloudi_x_medici:
                    query_limit_skip(Connection, OldQuery, Max,
                                     Skip, Timeout), State};
        {'query_order', OldQuery, Column, Type} ->
            {reply, cloudi_x_medici:
                    query_order(Connection, OldQuery, Column,
                                Type, Timeout), State};
        {'search', Query} ->
            {reply, cloudi_x_medici:
                    search(Connection, Query, Timeout), State};
        {'searchcount', Query} ->
            {reply, cloudi_x_medici:
                    searchcount(Connection, Query, Timeout), State};
        {'searchout', Query} ->
            {reply, cloudi_x_medici:
                    searchout(Connection, Query, Timeout), State};
        {'setindex', Column, Type} ->
            {reply, cloudi_x_medici:
                    setindex(Connection, Column, Type, Timeout), State};
        {'update', Key, NewCols} ->
            {reply, cloudi_x_medici:
                    update(Connection, Key, NewCols, Timeout), State}
    end.

cloudi_service_handle_info(Request, State, _Dispatcher) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_service_terminate(_Reason, _Timeout,
                         undefined) ->
    ok;
cloudi_service_terminate(_Reason, _Timeout,
                         #state{connection = Connection}) ->
    cloudi_x_medici:close(Connection),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

%% do a single query and return a boolean to determine if the query succeeded
%do_query(Query, Timeout, Connection) ->
%    try (case cloudi_string:binary_to_term(Query) of
%        % basic cloudi_x_medici API
%        {'addint', Key, Integer}
%            when is_list(Key), is_integer(Integer);
%                 is_binary(Key), is_integer(Integer) ->
%            cloudi_x_medici:
%            addint(Connection, Key, Integer, Timeout);
%        {'adddouble', Key, Double}
%            when is_list(Key), is_float(Double);
%                 is_binary(Key), is_float(Double) ->
%            cloudi_x_medici:
%            adddouble(Connection, Key, Double, Timeout);
%        {'adddouble_parts', Key, IntegerPart, FractionalPart}
%            when is_list(Key), is_integer(IntegerPart),
%                               is_integer(FractionalPart);
%                 is_binary(Key), is_integer(IntegerPart),
%                                 is_integer(FractionalPart) ->
%            cloudi_x_medici:
%            adddouble_parts(Connection, Key,
%                            IntegerPart, FractionalPart, Timeout);
%        {'copy', PathName}
%            when is_list(PathName); is_binary(PathName) ->
%            cloudi_x_medici:
%            copy(Connection, PathName, Timeout);
%        {'fwmkeys', Prefix, MaxKeys}
%            when is_list(Prefix), is_integer(MaxKeys);
%                 is_binary(Prefix), is_integer(MaxKeys) ->
%            cloudi_x_medici:
%            fwmkeys(Connection, Prefix, MaxKeys, Timeout);
%        {'get', Key}
%            when is_list(Key); is_binary(Key)  ->
%            cloudi_x_medici:
%            get(Connection, Key, Timeout);
%        'iterinit' ->
%            cloudi_x_medici:
%            iterinit(Connection, Timeout);
%        'iternext' ->
%            cloudi_x_medici:
%            iternext(Connection, Timeout);
%        {'mget', KeyList}
%            when is_list(KeyList) ->
%            cloudi_x_medici:
%            mget(Connection, KeyList, Timeout);
%        {'optimize', TuningOptions}
%            when is_list(TuningOptions); is_binary(TuningOptions) ->
%            cloudi_x_medici:
%            optimize(Connection, TuningOptions, Timeout);
%        {'out', Key}
%            when is_list(Key); is_binary(Key) ->
%            cloudi_x_medici:
%            out(Connection, Key, Timeout);
%        {'put', Key, Value}
%            when is_list(Key), is_integer(Value);
%                 is_list(Key), is_float(Value);
%                 is_list(Key), is_list(Value);
%                 is_list(Key), is_binary(Value);
%                 is_binary(Key), is_integer(Value);
%                 is_binary(Key), is_float(Value);
%                 is_binary(Key), is_list(Value);
%                 is_binary(Key), is_binary(Value) ->
%            cloudi_x_medici:
%            put(Connection, Key, Value, Timeout);
%        {'putcat', Key, Value}
%            when is_list(Key), is_integer(Value);
%                 is_list(Key), is_float(Value);
%                 is_list(Key), is_list(Value);
%                 is_list(Key), is_binary(Value);
%                 is_binary(Key), is_integer(Value);
%                 is_binary(Key), is_float(Value);
%                 is_binary(Key), is_list(Value);
%                 is_binary(Key), is_binary(Value) ->
%            cloudi_x_medici:
%            putcat(Connection, Key, Value, Timeout);
%        {'putkeep', Key, Value}
%            when is_list(Key), is_integer(Value);
%                 is_list(Key), is_float(Value);
%                 is_list(Key), is_list(Value);
%                 is_list(Key), is_binary(Value);
%                 is_binary(Key), is_integer(Value);
%                 is_binary(Key), is_float(Value);
%                 is_binary(Key), is_list(Value);
%                 is_binary(Key), is_binary(Value) ->
%            cloudi_x_medici:
%            putkeep(Connection, Key, Value, Timeout);
%        {'putnr', Key, Value}
%            when is_list(Key), is_integer(Value);
%                 is_list(Key), is_float(Value);
%                 is_list(Key), is_list(Value);
%                 is_list(Key), is_binary(Value);
%                 is_binary(Key), is_integer(Value);
%                 is_binary(Key), is_float(Value);
%                 is_binary(Key), is_list(Value);
%                 is_binary(Key), is_binary(Value) ->
%            cloudi_x_medici:
%            putnr(Connection, Key, Value, Timeout);
%        {'putshl', Key, Value, Width}
%            when is_list(Key), is_integer(Value), is_integer(Width);
%                 is_list(Key), is_float(Value), is_integer(Width);
%                 is_list(Key), is_list(Value), is_integer(Width);
%                 is_list(Key), is_binary(Value), is_integer(Width);
%                 is_binary(Key), is_integer(Value), is_integer(Width);
%                 is_binary(Key), is_float(Value), is_integer(Width);
%                 is_binary(Key), is_list(Value), is_integer(Width);
%                 is_binary(Key), is_binary(Value), is_integer(Width) ->
%            cloudi_x_medici:
%            putshl(Connection, Key, Value, Timeout);
%        {'restore', PathName, TimeStamp}
%            when is_list(PathName), is_integer(TimeStamp);
%                 is_binary(PathName), is_integer(TimeStamp) ->
%            cloudi_x_medici:
%            restore(Connection, PathName, TimeStamp, Timeout);
%        'rnum' ->
%            cloudi_x_medici:
%            rnum(Connection, Timeout);
%        {'setmst', HostName, Port}
%            when is_list(HostName), is_integer(Port);
%                 is_binary(HostName), is_integer(Port) ->
%            cloudi_x_medici:
%            setmst(Connection, HostName, Port, Timeout);
%        'size' ->
%            cloudi_x_medici:
%            size(Connection, Timeout);
%        'stat' ->
%            cloudi_x_medici:
%            stat(Connection, Timeout);
%        'sync' ->
%            cloudi_x_medici:
%            sync(Connection, Timeout);
%        'vanish' ->
%            cloudi_x_medici:
%            vanish(Connection, Timeout);
%        {'vsiz', Key}
%            when is_list(Key); is_binary(Key) ->
%            cloudi_x_medici:
%            vsiz(Connection, Key, Timeout);
%
%        % table cloudi_x_medici API
%        'genuid' ->
%            cloudi_x_medici:
%            genuid(Connection, Timeout);
%        {'query_add_condition', OldQuery, Column, Op, ExprList}
%            when is_list(OldQuery), is_list(Column),
%                 is_atom(Op), is_list(ExprList);
%                 is_list(OldQuery), is_binary(Column),
%                 is_atom(Op), is_list(ExprList);
%                 is_list(OldQuery), is_list(Column),
%                 is_tuple(Op), is_list(ExprList);
%                 is_list(OldQuery), is_binary(Column),
%                 is_tuple(Op), is_list(ExprList) ->
%            cloudi_x_medici:
%            query_add_condition(Connection, OldQuery, Column, Op,
%                                ExprList, Timeout);
%        {'query_limit', OldQuery, Max}
%            when is_list(OldQuery), is_integer(Max) ->
%            cloudi_x_medici:
%            query_limit(Connection, OldQuery, Max, Timeout);
%        {'query_limit_skip', OldQuery, Max, Skip}
%            when is_list(OldQuery), is_integer(Max), is_integer(Skip) ->
%            cloudi_x_medici:
%            query_limit_skip(Connection, OldQuery, Max, Skip, Timeout);
%        {'query_order', OldQuery, Column, Type}
%            when is_list(OldQuery), Column == primary, is_atom(Type);
%                 is_list(OldQuery), is_list(Column), is_atom(Type);
%                 is_list(OldQuery), is_binary(Column), is_atom(Type) ->
%            cloudi_x_medici:
%            query_order(Connection, OldQuery, Column, Type, Timeout);
%        {'search', SearchQuery}
%            when is_list(SearchQuery) ->
%            cloudi_x_medici:
%            search(Connection, SearchQuery, Timeout);
%        {'searchcount', SearchQuery}
%            when is_list(SearchQuery) ->
%            cloudi_x_medici:
%            searchcount(Connection, SearchQuery, Timeout);
%        {'searchout', SearchQuery}
%            when is_list(SearchQuery) ->
%            cloudi_x_medici:
%            searchout(Connection, SearchQuery, Timeout);
%        {'setindex', Column, Type}
%            when Column == primary, is_atom(Type);
%                 is_list(Column), is_atom(Type);
%                 is_binary(Column), is_atom(Type) ->
%            cloudi_x_medici:
%            setindex(Connection, Column, Type, Timeout);
%        {'update', Key, NewCols}
%            when is_list(Key), is_list(NewCols);
%                 is_binary(Key), is_list(NewCols) ->
%            cloudi_x_medici:
%            update(Connection, Key, NewCols, Timeout);
%        _ ->
%            {error, invalid_call}
%        end) of
%        {error, invalid_call} ->
%            ?LOG_ERROR("Invalid tokyotyrant command tuple ~p",
%                       [erlang:binary_to_list(Query)]),
%            <<>>;
%            % returns either
%            % iolist or a proplist that has binaries for keys and values
%        Result when is_binary(Result) ->
%            Result;
%        Result when is_list(Result) ->
%            cloudi_string:term_to_list(Result)
%            
%    catch
%        _:Reason ->
%            ?LOG_ERROR("exception when processing "
%                       "tokyotyrant command tuple ~p: ~p",
%                       [erlang:binary_to_list(Query), Reason]),
%            <<>> 
%    end.

