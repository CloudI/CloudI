%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==Cloudi Tokyo Tyrant Data Module==
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

-module(cloud_data_tokyotyrant).
-author('mjtruog [at] gmail (dot) com').

-behaviour(gen_server).
-behaviour(cloud_data_interface).

%% external interface

%% basic tokyo tyrant API
-export([addint/3, addint/4, 
         adddouble/3, adddouble/4, 
         adddouble_parts/4, adddouble_parts/5, 
         copy/2, copy/3, 
         fwmkeys/3, fwmkeys/4, 
         get/2, get/3, 
         iterinit/1, iterinit/2, 
         iternext/1, iternext/2, 
         mget/2, mget/3, 
         optimize/2, optimize/3, 
         out/2, out/3, 
         put/3, put/4, 
         putcat/3, putcat/4, 
         putkeep/3, putkeep/4, 
         putnr/3, putnr/4, 
         putshl/4, putshl/5, 
         restore/3, restore/4, 
         rnum/1, rnum/2, 
         setmst/3, setmst/4, 
         size/1, size/2, 
         stat/1, stat/2, 
         sync/1, sync/2, 
         vanish/1, vanish/2, 
         vsiz/2, vsiz/3]).

%% table tokyo tyrant API
-export([genuid/1, genuid/2, 
         query_add_condition/5, query_add_condition/6, 
         query_limit/3, query_limit/4, 
         query_limit_skip/4, query_limit_skip/5, 
         query_order/4, query_order/5, 
         search/2, search/3, 
         searchcount/2, searchcount/3, 
         searchout/2, searchout/3, 
         setindex/3, setindex/4, 
         update/3, update/4]).

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
    data_title = undefined,
    connection = undefined,
    timeout = undefined}).

%%%------------------------------------------------------------------------
%%% External interface functions
%%%------------------------------------------------------------------------

%%-------------------------------------------------------------------------
%% @doc
%% ===Add an integer to the value in the database.===
%% @end
%%-------------------------------------------------------------------------

-spec addint(DataTitle :: atom(),
             Key :: iolist(),
             Integer :: integer()) ->
    any().

addint(DataTitle, Key, Integer) ->
    addint(DataTitle, Key, Integer, undefined).

-spec addint(DataTitle :: atom(),
             Key :: iolist(),
             Integer :: integer(),
             Timeout :: 'undefined' | pos_integer()) ->
    any().

addint(DataTitle, Key, Integer, Timeout)
    when is_list(Key), is_integer(Integer);
         is_binary(Key), is_integer(Integer) ->
    gen_server:call(DataTitle,
        {addint, Key, Integer, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add a double to the value in the database.===
%% @end
%%-------------------------------------------------------------------------

-spec adddouble(DataTitle :: atom(),
                Key :: iolist(),
                Double :: float()) ->
    any().

adddouble(DataTitle, Key, Double) ->
    adddouble(DataTitle, Key, Double, undefined).

-spec adddouble(DataTitle :: atom(),
                Key :: iolist(),
                Double :: float(),
                Timeout :: 'undefined' | pos_integer()) ->
    any().

adddouble(DataTitle, Key, Double, Timeout)
    when is_list(Key), is_float(Double);
         is_binary(Key), is_float(Double) ->
    gen_server:call(DataTitle,
        {adddouble, Key, Double, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add a double in separate parts to the value in the database.===
%% @end
%%-------------------------------------------------------------------------

-spec adddouble_parts(DataTitle :: atom(),
                      Key :: iolist(),
                      IntegerPart :: integer(),
                      FractionalPart :: integer()) ->
    any().

adddouble_parts(DataTitle, Key, IntegerPart, FractionalPart) ->
    adddouble_parts(DataTitle, Key, IntegerPart, FractionalPart, undefined).

-spec adddouble_parts(DataTitle :: atom(),
                      Key :: iolist(),
                      IntegerPart :: integer(),
                      FractionalPart :: integer(),
                      Timeout :: 'undefined' | pos_integer()) ->
    any().

adddouble_parts(DataTitle, Key, IntegerPart, FractionalPart, Timeout)
    when is_list(Key), is_integer(IntegerPart),
                       is_integer(FractionalPart);
         is_binary(Key), is_integer(IntegerPart),
                         is_integer(FractionalPart) ->
    gen_server:call(DataTitle,
        {adddouble_parts, Key,
         IntegerPart, FractionalPart, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Copy the database file.===
%% @end
%%-------------------------------------------------------------------------

-spec copy(DataTitle :: atom(),
           PathName :: string()) ->
    any().

copy(DataTitle, PathName) ->
    copy(DataTitle, PathName, undefined).

-spec copy(DataTitle :: atom(),
           PathName :: string(),
           Timeout :: 'undefined' | pos_integer()) ->
    any().

copy(DataTitle, PathName, Timeout)
    when is_list(PathName); is_binary(PathName) ->
    gen_server:call(DataTitle,
        {copy, PathName, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Match some number of keys on a prefix.===
%% @end
%%-------------------------------------------------------------------------

-spec fwmkeys(DataTitle :: atom(),
              Prefix :: iolist(),
              MaxKeys :: non_neg_integer()) ->
    any().

fwmkeys(DataTitle, Prefix, MaxKeys) ->
    fwmkeys(DataTitle, Prefix, MaxKeys, undefined).

-spec fwmkeys(DataTitle :: atom(),
              Prefix :: iolist(),
              MaxKeys :: non_neg_integer(),
              Timeout :: 'undefined' | pos_integer()) ->
    any().

fwmkeys(DataTitle, Prefix, MaxKeys, Timeout)
    when is_list(Prefix), is_integer(MaxKeys);
         is_binary(Prefix), is_integer(MaxKeys) ->
    gen_server:call(DataTitle,
        {fwmkeys, Prefix, MaxKeys, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a value from the database.===
%% @end
%%-------------------------------------------------------------------------

-spec get(DataTitle :: atom(),
          Key :: iolist()) ->
    any().

get(DataTitle, Key) ->
    get(DataTitle, Key, undefined).

-spec get(DataTitle :: atom(),
          Key :: iolist(),
          Timeout :: 'undefined' | pos_integer()) ->
    any().

get(DataTitle, Key, Timeout)
    when is_list(Key); is_binary(Key)  ->
    gen_server:call(DataTitle,
        {get, Key, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Initialize an iterator on all the keys in the database.===
%% @end
%%-------------------------------------------------------------------------

-spec iterinit(DataTitle :: atom()) ->
    any().

iterinit(DataTitle) ->
    iterinit(DataTitle, undefined).

-spec iterinit(DataTitle :: atom(),
               Timeout :: 'undefined' | pos_integer()) ->
    any().

iterinit(DataTitle, Timeout) ->
    gen_server:call(DataTitle,
        {iterinit, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the next key from the iterator on all the keys in the database.===
%% @end
%%-------------------------------------------------------------------------

-spec iternext(DataTitle :: atom()) ->
    any().

iternext(DataTitle) ->
    iternext(DataTitle, undefined).

-spec iternext(DataTitle :: atom(),
               Timeout :: 'undefined' | pos_integer()) ->
    any().

iternext(DataTitle, Timeout) ->
    gen_server:call(DataTitle,
        {iternext, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get multiple values from the database.===
%% @end
%%-------------------------------------------------------------------------

-spec mget(DataTitle :: atom(),
           KeyList :: list(iolist())) ->
    any().

mget(DataTitle, KeyList) ->
    mget(DataTitle, KeyList, undefined).

-spec mget(DataTitle :: atom(),
           KeyList :: list(iolist()),
           Timeout :: 'undefined' | pos_integer()) ->
    any().

mget(DataTitle, KeyList, Timeout)
    when is_list(KeyList) ->
    gen_server:call(DataTitle,
        {mget, KeyList, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Optimize the storage of values in the database.===
%% @end
%%-------------------------------------------------------------------------

-spec optimize(DataTitle :: atom(),
               TuningOptions :: iolist()) ->
    any().

optimize(DataTitle, TuningOptions) ->
    optimize(DataTitle, TuningOptions, undefined).

-spec optimize(DataTitle :: atom(),
               TuningOptions :: iolist(),
               Timeout :: 'undefined' | pos_integer()) ->
    any().

optimize(DataTitle, TuningOptions, Timeout)
    when is_list(TuningOptions); is_binary(TuningOptions) ->
    gen_server:call(DataTitle,
        {optimize, TuningOptions, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Erase a value from the database.===
%% @end
%%-------------------------------------------------------------------------

-spec out(DataTitle :: atom(),
          Key :: iolist()) ->
    any().

out(DataTitle, Key) ->
    out(DataTitle, Key, undefined).

-spec out(DataTitle :: atom(),
          Key :: iolist(),
          Timeout :: 'undefined' | pos_integer()) ->
    any().

out(DataTitle, Key, Timeout)
    when is_list(Key); is_binary(Key) ->
    gen_server:call(DataTitle,
        {out, Key, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Store a value in the database.===
%% @end
%%-------------------------------------------------------------------------

-spec put(DataTitle :: atom(),
          Key :: iolist(),
          Value :: integer() | float() | list() | binary()) ->
    any().

put(DataTitle, Key, Value) ->
    put(DataTitle, Key, Value, undefined).

-spec put(DataTitle :: atom(),
          Key :: iolist(),
          Value :: integer() | float() | list() | binary(),
          Timeout :: 'undefined' | pos_integer()) ->
    any().

put(DataTitle, Key, Value, Timeout)
    when is_list(Key), is_integer(Value);
         is_list(Key), is_float(Value);
         is_list(Key), is_list(Value);
         is_list(Key), is_binary(Value);
         is_binary(Key), is_integer(Value);
         is_binary(Key), is_float(Value);
         is_binary(Key), is_list(Value);
         is_binary(Key), is_binary(Value) ->
    gen_server:call(DataTitle,
        {put, Key, Value, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Append a value to the existing value in the database.===
%% @end
%%-------------------------------------------------------------------------

-spec putcat(DataTitle :: atom(),
             Key :: iolist(),
             Value :: integer() | float() | list() | binary()) ->
    any().

putcat(DataTitle, Key, Value) ->
    putcat(DataTitle, Key, Value, undefined).

-spec putcat(DataTitle :: atom(),
             Key :: iolist(),
             Value :: integer() | float() | list() | binary(),
             Timeout :: 'undefined' | pos_integer()) ->
    any().

putcat(DataTitle, Key, Value, Timeout)
    when is_list(Key), is_integer(Value);
         is_list(Key), is_float(Value);
         is_list(Key), is_list(Value);
         is_list(Key), is_binary(Value);
         is_binary(Key), is_integer(Value);
         is_binary(Key), is_float(Value);
         is_binary(Key), is_list(Value);
         is_binary(Key), is_binary(Value) ->
    gen_server:call(DataTitle,
        {putcat, Key, Value, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Store a value in the database only if there currently is no value for the given key.===
%% @end
%%-------------------------------------------------------------------------

-spec putkeep(DataTitle :: atom(),
              Key :: iolist(),
              Value :: integer() | float() | list() | binary()) ->
    any().

putkeep(DataTitle, Key, Value) ->
    putkeep(DataTitle, Key, Value, undefined).

-spec putkeep(DataTitle :: atom(),
              Key :: iolist(),
              Value :: integer() | float() | list() | binary(),
              Timeout :: 'undefined' | pos_integer()) ->
    any().

putkeep(DataTitle, Key, Value, Timeout)
    when is_list(Key), is_integer(Value);
         is_list(Key), is_float(Value);
         is_list(Key), is_list(Value);
         is_list(Key), is_binary(Value);
         is_binary(Key), is_integer(Value);
         is_binary(Key), is_float(Value);
         is_binary(Key), is_list(Value);
         is_binary(Key), is_binary(Value) ->
    gen_server:call(DataTitle,
        {putkeep, Key, Value, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Store a value in the database but do not wait for the database to acknowledge the change.===
%% @end
%%-------------------------------------------------------------------------

-spec putnr(DataTitle :: atom(),
            Key :: iolist(),
            Value :: integer() | float() | list() | binary()) ->
    any().

putnr(DataTitle, Key, Value) ->
    putnr(DataTitle, Key, Value, undefined).

-spec putnr(DataTitle :: atom(),
            Key :: iolist(),
            Value :: integer() | float() | list() | binary(),
            Timeout :: 'undefined' | pos_integer()) ->
    any().

putnr(DataTitle, Key, Value, Timeout)
    when is_list(Key), is_integer(Value);
         is_list(Key), is_float(Value);
         is_list(Key), is_list(Value);
         is_list(Key), is_binary(Value);
         is_binary(Key), is_integer(Value);
         is_binary(Key), is_float(Value);
         is_binary(Key), is_list(Value);
         is_binary(Key), is_binary(Value) ->
    gen_server:call(DataTitle,
        {putnr, Key, Value, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Append a value to the existing value in the database and shift it left.===
%% @end
%%-------------------------------------------------------------------------

-spec putshl(DataTitle :: atom(),
             Key :: iolist(),
             Value :: integer() | float() | list() | binary(),
             Width :: integer()) ->
    any().

putshl(DataTitle, Key, Value, Width) ->
    putshl(DataTitle, Key, Value, Width, undefined).

-spec putshl(DataTitle :: atom(),
             Key :: iolist(),
             Value :: integer() | float() | list() | binary(),
             Width :: integer(),
             Timeout :: 'undefined' | pos_integer()) ->
    any().

putshl(DataTitle, Key, Value, Width, Timeout)
    when is_list(Key), is_integer(Value), is_integer(Width);
         is_list(Key), is_float(Value), is_integer(Width);
         is_list(Key), is_list(Value), is_integer(Width);
         is_list(Key), is_binary(Value), is_integer(Width);
         is_binary(Key), is_integer(Value), is_integer(Width);
         is_binary(Key), is_float(Value), is_integer(Width);
         is_binary(Key), is_list(Value), is_integer(Width);
         is_binary(Key), is_binary(Value), is_integer(Width) ->
    gen_server:call(DataTitle,
        {putshl, Key, Value, Width, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Restore a database file from the update log.===
%% @end
%%-------------------------------------------------------------------------

-spec restore(DataTitle :: atom(),
              PathName :: iolist(),
              TimeStamp :: integer()) ->
    any().

restore(DataTitle, PathName, TimeStamp) ->
    restore(DataTitle, PathName, TimeStamp, undefined).

-spec restore(DataTitle :: atom(),
              PathName :: iolist(),
              TimeStamp :: integer(),
              Timeout :: 'undefined' | pos_integer()) ->
    any().

restore(DataTitle, PathName, TimeStamp, Timeout)
    when is_list(PathName), is_integer(TimeStamp);
         is_binary(PathName), is_integer(TimeStamp) ->
    gen_server:call(DataTitle,
        {restore, PathName, TimeStamp, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the number of values currently in the database.===
%% @end
%%-------------------------------------------------------------------------

-spec rnum(DataTitle :: atom()) ->
    any().

rnum(DataTitle) ->
    rnum(DataTitle, undefined).

-spec rnum(DataTitle :: atom(),
           Timeout :: 'undefined' | pos_integer()) ->
    any().

rnum(DataTitle, Timeout) ->
    gen_server:call(DataTitle,
        {rnum, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Set the replication master of the database.===
%% @end
%%-------------------------------------------------------------------------

-spec setmst(DataTitle :: atom(),
             Host :: iolist(),
             Port :: integer()) ->
    any().

setmst(DataTitle, HostName, Port) ->
    setmst(DataTitle, HostName, Port, undefined).

-spec setmst(DataTitle :: atom(),
             Host :: iolist(),
             Port :: integer(),
             Timeout :: 'undefined' | pos_integer()) ->
    any().

setmst(DataTitle, HostName, Port, Timeout)
    when is_list(HostName), is_integer(Port);
         is_binary(HostName), is_integer(Port) ->
    gen_server:call(DataTitle,
        {setmst, HostName, Port, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the size of the database.===
%% @end
%%-------------------------------------------------------------------------

-spec size(DataTitle :: atom()) ->
    any().

size(DataTitle) ->
    size(DataTitle, undefined).

-spec size(DataTitle :: atom(),
           Timeout :: 'undefined' | pos_integer()) ->
    any().

size(DataTitle, Timeout) ->
    gen_server:call(DataTitle,
        {size, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the status string from the database.===
%% @end
%%-------------------------------------------------------------------------

-spec stat(DataTitle :: atom()) ->
    any().

stat(DataTitle) ->
    stat(DataTitle, undefined).

-spec stat(DataTitle :: atom(),
           Timeout :: 'undefined' | pos_integer()) ->
    any().

stat(DataTitle, Timeout) ->
    gen_server:call(DataTitle,
        {stat, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Synchronize the updated contents of the database with the filesystem.===
%% @end
%%-------------------------------------------------------------------------

-spec sync(DataTitle :: atom()) ->
    any().

sync(DataTitle) ->
    sync(DataTitle, undefined).

-spec sync(DataTitle :: atom(),
           Timeout :: 'undefined' | pos_integer()) ->
    any().

sync(DataTitle, Timeout) ->
    gen_server:call(DataTitle,
        {sync, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Erase all values from the database.===
%% @end
%%-------------------------------------------------------------------------

-spec vanish(DataTitle :: atom()) ->
    any().

vanish(DataTitle) ->
    vanish(DataTitle, undefined).

-spec vanish(DataTitle :: atom(),
             Timeout :: 'undefined' | pos_integer()) ->
    any().

vanish(DataTitle, Timeout) ->
    gen_server:call(DataTitle,
        {vanish, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the size of a value in the database.===
%% @end
%%-------------------------------------------------------------------------

-spec vsiz(DataTitle :: atom(),
           Key :: iolist()) ->
    any().

vsiz(DataTitle, Key) ->
    vsiz(DataTitle, Key, undefined).

-spec vsiz(DataTitle :: atom(),
           Key :: iolist(),
           Timeout :: 'undefined' | pos_integer()) ->
    any().

vsiz(DataTitle, Key, Timeout)
    when is_list(Key); is_binary(Key) ->
    gen_server:call(DataTitle,
        {vsiz, Key, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Generate a unique ID number for the database.===
%% @end
%%-------------------------------------------------------------------------

-spec genuid(DataTitle :: atom()) ->
    any().

genuid(DataTitle) ->
    genuid(DataTitle, undefined).

-spec genuid(DataTitle :: atom(),
             Timeout :: 'undefined' | pos_integer()) ->
    any().

genuid(DataTitle, Timeout) ->
    gen_server:call(DataTitle,
        {genuid, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add a constraint on a query.===
%% @end
%%-------------------------------------------------------------------------

-spec query_add_condition(DataTitle :: atom(),
                          OldQuery :: list(),
                          Column :: iolist(),
                          Op :: atom(),
                          ExprList :: list()) ->
    any().

query_add_condition(DataTitle, OldQuery, Column, Op, ExprList) ->
    query_add_condition(DataTitle, OldQuery, Column, Op, ExprList, undefined).

-spec query_add_condition(DataTitle :: atom(),
                          OldQuery :: list(),
                          Column :: iolist(),
                          Op :: atom(),
                          ExprList :: list(),
                          Timeout :: 'undefined' | pos_integer()) ->
    any().

query_add_condition(DataTitle, OldQuery, Column, Op, ExprList, Timeout)
    when is_list(OldQuery), is_list(Column),
         is_atom(Op), is_list(ExprList);
         is_list(OldQuery), is_binary(Column),
         is_atom(Op), is_list(ExprList);
         is_list(OldQuery), is_list(Column),
         is_tuple(Op), is_list(ExprList);
         is_list(OldQuery), is_binary(Column),
         is_tuple(Op), is_list(ExprList) ->
    gen_server:call(DataTitle,
        {query_add_condition,
         OldQuery, Column, Op, ExprList, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Limit the number of values returned by a query.===
%% @end
%%-------------------------------------------------------------------------

-spec query_limit(DataTitle :: atom(),
                  OldQuery :: list(),
                  Max :: integer()) ->
    any().

query_limit(DataTitle, OldQuery, Max) ->
    query_limit(DataTitle, OldQuery, Max, undefined).

-spec query_limit(DataTitle :: atom(),
                  OldQuery :: list(),
                  Max :: integer(),
                  Timeout :: 'undefined' | pos_integer()) ->
    any().

query_limit(DataTitle, OldQuery, Max, Timeout)
    when is_list(OldQuery), is_integer(Max) ->
    gen_server:call(DataTitle,
        {query_limit, OldQuery, Max, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===After applying an offset into the values returned by a query, limit the remaining results.===
%% @end
%%-------------------------------------------------------------------------

-spec query_limit_skip(DataTitle :: atom(),
                       OldQuery :: list(),
                       Max :: integer(),
                       Skip :: integer()) ->
    any().

query_limit_skip(DataTitle, OldQuery, Max, Skip) ->
    query_limit_skip(DataTitle, OldQuery, Max, Skip, undefined).

-spec query_limit_skip(DataTitle :: atom(),
                       OldQuery :: list(),
                       Max :: integer(),
                       Skip :: integer(),
                       Timeout :: 'undefined' | pos_integer()) ->
    any().

query_limit_skip(DataTitle, OldQuery, Max, Skip, Timeout)
    when is_list(OldQuery), is_integer(Max), is_integer(Skip) ->
    gen_server:call(DataTitle,
        {query_limit_skip, OldQuery, Max, Skip, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Set the order of a query.===
%% @end
%%-------------------------------------------------------------------------

-spec query_order(DataTitle :: atom(),
                  OldQuery :: list(),
                  Column :: 'primary' | iolist(),
                  Type :: atom()) ->
    any().

query_order(DataTitle, OldQuery, Column, Type) ->
    query_order(DataTitle, OldQuery, Column, Type, undefined).

-spec query_order(DataTitle :: atom(),
                  OldQuery :: list(),
                  Column :: 'primary' | iolist(),
                  Type :: atom(),
                  Timeout :: 'undefined' | pos_integer()) ->
    any().

query_order(DataTitle, OldQuery, Column, Type, Timeout)
    when is_list(OldQuery), Column == primary, is_atom(Type);
         is_list(OldQuery), is_list(Column), is_atom(Type);
         is_list(OldQuery), is_binary(Column), is_atom(Type) ->
    gen_server:call(DataTitle,
        {query_order, OldQuery, Column, Type, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Execute the search of a query.===
%% @end
%%-------------------------------------------------------------------------

-spec search(DataTitle :: atom(),
             Query :: list()) ->
    any().

search(DataTitle, Query) ->
    search(DataTitle, Query, undefined).

-spec search(DataTitle :: atom(),
             Query :: list(),
             Timeout :: 'undefined' | pos_integer()) ->
    any().

search(DataTitle, Query, Timeout)
    when is_list(Query) ->
    gen_server:call(DataTitle,
        {search, Query, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the number of values returned from a query.===
%% @end
%%-------------------------------------------------------------------------

-spec searchcount(DataTitle :: atom(),
                  Query :: list()) ->
    any().

searchcount(DataTitle, Query) ->
    searchcount(DataTitle, Query, undefined).

-spec searchcount(DataTitle :: atom(),
                  Query :: list(),
                  Timeout :: 'undefined' | pos_integer()) ->
    any().

searchcount(DataTitle, Query, Timeout)
    when is_list(Query) ->
    gen_server:call(DataTitle,
        {searchcount, Query, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Erase all values that match the query.===
%% @end
%%-------------------------------------------------------------------------

-spec searchout(DataTitle :: atom(),
                Query :: list()) ->
    any().

searchout(DataTitle, Query) ->
    searchout(DataTitle, Query, undefined).

-spec searchout(DataTitle :: atom(),
                Query :: list(),
                Timeout :: 'undefined' | pos_integer()) ->
    any().

searchout(DataTitle, Query, Timeout)
    when is_list(Query) ->
    gen_server:call(DataTitle,
        {searchout, Query, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Set an index on a database table.===
%% @end
%%-------------------------------------------------------------------------

-spec setindex(DataTitle :: atom(),
               Column :: 'primary' | iolist(),
               Type :: atom()) ->
    any().

setindex(DataTitle, Column, Type) ->
    setindex(DataTitle, Column, Type, undefined).

-spec setindex(DataTitle :: atom(),
               Column :: 'primary' | iolist(),
               Type :: atom(),
               Timeout :: 'undefined' | pos_integer()) ->
    any().

setindex(DataTitle, Column, Type, Timeout)
    when Column == primary, is_atom(Type);
         is_list(Column), is_atom(Type);
         is_binary(Column), is_atom(Type) ->
    gen_server:call(DataTitle,
        {setindex, Column, Type, Timeout}, infinity).

%%-------------------------------------------------------------------------
%% @doc
%% ===Append column data to an existing database table.===
%% @end
%%-------------------------------------------------------------------------

-spec update(DataTitle :: atom(),
             Key :: iolist(),
             NewCols :: list()) ->
    any().

update(DataTitle, Key, NewCols) ->
    update(DataTitle, Key, NewCols, undefined).

-spec update(DataTitle :: atom(),
             Key :: iolist(),
             NewCols :: list(),
             Timeout :: 'undefined' | pos_integer()) ->
    any().

update(DataTitle, Key, NewCols, Timeout)
    when is_list(Key), is_list(NewCols);
         is_binary(Key), is_list(NewCols) ->
    gen_server:call(DataTitle,
        {update, Key, NewCols, Timeout}, infinity).

%%%------------------------------------------------------------------------
%%% Callback functions from cloud_data_interface
%%%------------------------------------------------------------------------

-spec start_link(DataTitle :: atom(),
                 Arguments :: list({atom(), string(), list({_,_})})) ->
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
    % depend on DEFAULT_TIMEOUT for a database communication timeout
    gen_server:call(DataTitle, {do_queries, QueryList}, infinity).

%%%------------------------------------------------------------------------
%%% Callback functions from gen_server
%%%------------------------------------------------------------------------

init([DataTitle, Arguments]) when is_atom(DataTitle), is_list(Arguments) ->
    init_state(DataTitle, Arguments).
handle_call({'addint', Key, Integer, undefined}, _,
            #state{connection = Connection,
                   timeout = Timeout} = State) ->
    {reply, medici:addint(Connection, Key, Integer, Timeout), State};
handle_call({'addint', Key, Integer, Timeout}, _,
            #state{connection = Connection} = State) ->
    {reply, medici:addint(Connection, Key, Integer, Timeout), State};
handle_call({'adddouble', Key, Double, undefined}, _,
            #state{connection = Connection,
                   timeout = Timeout} = State) ->
    {reply, medici:adddouble(Connection, Key, Double, Timeout), State};
handle_call({'adddouble', Key, Double, Timeout}, _,
            #state{connection = Connection} = State) ->
    {reply, medici:adddouble(Connection, Key, Double, Timeout), State};
handle_call({'adddouble_parts', Key, IntegerPart, FractionalPart, undefined}, _,
            #state{connection = Connection,
                   timeout = Timeout} = State) ->
    {reply, medici:adddouble_parts(
        Connection, Key, IntegerPart, FractionalPart, Timeout), State};
handle_call({'adddouble_parts', Key, IntegerPart, FractionalPart, Timeout}, _,
            #state{connection = Connection} = State) ->
    {reply, medici:adddouble_parts(
        Connection, Key, IntegerPart, FractionalPart, Timeout), State};
handle_call({'copy', PathName, undefined}, _,
            #state{connection = Connection,
                   timeout = Timeout} = State) ->
    {reply, medici:copy(Connection, PathName, Timeout), State};
handle_call({'copy', PathName, Timeout}, _,
            #state{connection = Connection} = State) ->
    {reply, medici:copy(Connection, PathName, Timeout), State};
handle_call({'fwmkeys', Prefix, MaxKeys, undefined}, _,
            #state{connection = Connection,
                   timeout = Timeout} = State) ->
    {reply, medici:fwmkeys(Connection, Prefix, MaxKeys, Timeout), State};
handle_call({'fwmkeys', Prefix, MaxKeys, Timeout}, _,
            #state{connection = Connection} = State) ->
    {reply, medici:fwmkeys(Connection, Prefix, MaxKeys, Timeout), State};
handle_call({'get', Key, undefined}, _,
            #state{connection = Connection,
                   timeout = Timeout} = State) ->
    {reply, medici:get(Connection, Key, Timeout), State};
handle_call({'get', Key, Timeout}, _,
            #state{connection = Connection} = State) ->
    {reply, medici:get(Connection, Key, Timeout), State};
handle_call({'iterinit', undefined}, _,
            #state{connection = Connection,
                   timeout = Timeout} = State) ->
    {reply, medici:iterinit(Connection, Timeout), State};
handle_call({'iterinit', Timeout}, _,
            #state{connection = Connection} = State) ->
    {reply, medici:iterinit(Connection, Timeout), State};
handle_call({'iternext', undefined}, _,
            #state{connection = Connection,
                   timeout = Timeout} = State) ->
    {reply, medici:iternext(Connection, Timeout), State};
handle_call({'iternext', Timeout}, _,
            #state{connection = Connection} = State) ->
    {reply, medici:iternext(Connection, Timeout), State};
handle_call({'mget', KeyList, undefined}, _,
            #state{connection = Connection,
                   timeout = Timeout} = State) ->
    {reply, medici:mget(Connection, KeyList, Timeout), State};
handle_call({'mget', KeyList, Timeout}, _,
            #state{connection = Connection} = State) ->
    {reply, medici:mget(Connection, KeyList, Timeout), State};
handle_call({'optimize', TuningOptions, undefined}, _,
            #state{connection = Connection,
                   timeout = Timeout} = State) ->
    {reply, medici:optimize(Connection, TuningOptions, Timeout), State};
handle_call({'optimize', TuningOptions, Timeout}, _,
            #state{connection = Connection} = State) ->
    {reply, medici:optimize(Connection, TuningOptions, Timeout), State};
handle_call({'out', Key, undefined}, _,
            #state{connection = Connection,
                   timeout = Timeout} = State) ->
    {reply, medici:out(Connection, Key, Timeout), State};
handle_call({'out', Key, Timeout}, _,
            #state{connection = Connection} = State) ->
    {reply, medici:out(Connection, Key, Timeout), State};
handle_call({'put', Key, Value, undefined}, _,
            #state{connection = Connection,
                   timeout = Timeout} = State) ->
    {reply, medici:put(Connection, Key, Value, Timeout), State};
handle_call({'put', Key, Value, Timeout}, _,
            #state{connection = Connection} = State) ->
    {reply, medici:put(Connection, Key, Value, Timeout), State};
handle_call({'putcat', Key, Value, undefined}, _,
            #state{connection = Connection,
                   timeout = Timeout} = State) ->
    {reply, medici:putcat(Connection, Key, Value, Timeout), State};
handle_call({'putcat', Key, Value, Timeout}, _,
            #state{connection = Connection} = State) ->
    {reply, medici:putcat(Connection, Key, Value, Timeout), State};
handle_call({'putkeep', Key, Value, undefined}, _,
            #state{connection = Connection,
                   timeout = Timeout} = State) ->
    {reply, medici:putkeep(Connection, Key, Value, Timeout), State};
handle_call({'putkeep', Key, Value, Timeout}, _,
            #state{connection = Connection} = State) ->
    {reply, medici:putkeep(Connection, Key, Value, Timeout), State};
handle_call({'putnr', Key, Value, undefined}, _,
            #state{connection = Connection,
                   timeout = Timeout} = State) ->
    {reply, medici:putnr(Connection, Key, Value, Timeout), State};
handle_call({'putnr', Key, Value, Timeout}, _,
            #state{connection = Connection} = State) ->
    {reply, medici:putnr(Connection, Key, Value, Timeout), State};
handle_call({'putshl', Key, Value, Width, undefined}, _,
            #state{connection = Connection,
                   timeout = Timeout} = State) ->
    {reply, medici:putshl(Connection, Key, Value, Width, Timeout), State};
handle_call({'putshl', Key, Value, Width, Timeout}, _,
            #state{connection = Connection} = State) ->
    {reply, medici:putshl(Connection, Key, Value, Width, Timeout), State};
handle_call({'restore', PathName, TimeStamp, undefined}, _,
            #state{connection = Connection,
                   timeout = Timeout} = State) ->
    {reply, medici:restore(Connection, PathName, TimeStamp, Timeout), State};
handle_call({'restore', PathName, TimeStamp, Timeout}, _,
            #state{connection = Connection} = State) ->
    {reply, medici:restore(Connection, PathName, TimeStamp, Timeout), State};
handle_call({'rnum', undefined}, _,
            #state{connection = Connection,
                   timeout = Timeout} = State) ->
    {reply, medici:rnum(Connection, Timeout), State};
handle_call({'rnum', Timeout}, _,
            #state{connection = Connection} = State) ->
    {reply, medici:rnum(Connection, Timeout), State};
handle_call({'setmst', HostName, Port, undefined}, _,
            #state{connection = Connection,
                   timeout = Timeout} = State) ->
    {reply, medici:setmst(Connection, HostName, Port, Timeout), State};
handle_call({'setmst', HostName, Port, Timeout}, _,
            #state{connection = Connection} = State) ->
    {reply, medici:setmst(Connection, HostName, Port, Timeout), State};
handle_call({'size', undefined}, _,
            #state{connection = Connection,
                   timeout = Timeout} = State) ->
    {reply, medici:size(Connection, Timeout), State};
handle_call({'size', Timeout}, _,
            #state{connection = Connection} = State) ->
    {reply, medici:size(Connection, Timeout), State};
handle_call({'stat', undefined}, _,
            #state{connection = Connection,
                   timeout = Timeout} = State) ->
    {reply, medici:stat(Connection, Timeout), State};
handle_call({'stat', Timeout}, _,
            #state{connection = Connection} = State) ->
    {reply, medici:stat(Connection, Timeout), State};
handle_call({'sync', undefined}, _,
            #state{connection = Connection,
                   timeout = Timeout} = State) ->
    {reply, medici:sync(Connection, Timeout), State};
handle_call({'sync', Timeout}, _,
            #state{connection = Connection} = State) ->
    {reply, medici:sync(Connection, Timeout), State};
handle_call({'vanish', undefined}, _,
            #state{connection = Connection,
                   timeout = Timeout} = State) ->
    {reply, medici:vanish(Connection, Timeout), State};
handle_call({'vanish', Timeout}, _,
            #state{connection = Connection} = State) ->
    {reply, medici:vanish(Connection, Timeout), State};
handle_call({'vsiz', Key, undefined}, _,
            #state{connection = Connection,
                   timeout = Timeout} = State) ->
    {reply, medici:vsiz(Connection, Key, Timeout), State};
handle_call({'vsiz', Key, Timeout}, _,
            #state{connection = Connection} = State) ->
    {reply, medici:vsiz(Connection, Key, Timeout), State};
handle_call({'genuid', undefined}, _,
            #state{connection = Connection,
                   timeout = Timeout} = State) ->
    {reply, medici:genuid(Connection, Timeout), State};
handle_call({'genuid', Timeout}, _,
            #state{connection = Connection} = State) ->
    {reply, medici:genuid(Connection, Timeout), State};
handle_call({'query_add_condition', OldQuery, Column, Op, ExprList, undefined},
            _, #state{connection = Connection,
                      timeout = Timeout} = State) ->
    {reply, medici:query_add_condition(
        Connection, OldQuery, Column, Op, ExprList, Timeout), State};
handle_call({'query_add_condition', OldQuery, Column, Op, ExprList, Timeout},
            _, #state{connection = Connection} = State) ->
    {reply, medici:query_add_condition(
        Connection, OldQuery, Column, Op, ExprList, Timeout), State};
handle_call({'query_limit', OldQuery, Max, undefined}, _,
            #state{connection = Connection,
                   timeout = Timeout} = State) ->
    {reply, medici:query_limit(Connection, OldQuery, Max, Timeout), State};
handle_call({'query_limit', OldQuery, Max, Timeout}, _,
            #state{connection = Connection} = State) ->
    {reply, medici:query_limit(Connection, OldQuery, Max, Timeout), State};
handle_call({'query_limit_skip', OldQuery, Max, Skip, undefined}, _,
            #state{connection = Connection,
                   timeout = Timeout} = State) ->
    {reply, medici:query_limit_skip(
        Connection, OldQuery, Max, Skip, Timeout), State};
handle_call({'query_limit_skip', OldQuery, Max, Skip, Timeout}, _,
            #state{connection = Connection} = State) ->
    {reply, medici:query_limit_skip(
        Connection, OldQuery, Max, Skip, Timeout), State};
handle_call({'query_order', OldQuery, Column, Type, undefined}, _,
            #state{connection = Connection,
                   timeout = Timeout} = State) ->
    {reply, medici:query_order(
        Connection, OldQuery, Column, Type, Timeout), State};
handle_call({'query_order', OldQuery, Column, Type, Timeout}, _,
            #state{connection = Connection} = State) ->
    {reply, medici:query_order(
        Connection, OldQuery, Column, Type, Timeout), State};
handle_call({'search', Query, undefined}, _,
            #state{connection = Connection,
                   timeout = Timeout} = State) ->
    {reply, medici:search(Connection, Query, Timeout), State};
handle_call({'search', Query, Timeout}, _,
            #state{connection = Connection} = State) ->
    {reply, medici:search(Connection, Query, Timeout), State};
handle_call({'searchcount', Query, undefined}, _,
            #state{connection = Connection,
                   timeout = Timeout} = State) ->
    {reply, medici:searchcount(Connection, Query, Timeout), State};
handle_call({'searchcount', Query, Timeout}, _,
            #state{connection = Connection} = State) ->
    {reply, medici:searchcount(Connection, Query, Timeout), State};
handle_call({'searchout', Query, undefined}, _,
            #state{connection = Connection,
                   timeout = Timeout} = State) ->
    {reply, medici:searchout(Connection, Query, Timeout), State};
handle_call({'searchout', Query, Timeout}, _,
            #state{connection = Connection} = State) ->
    {reply, medici:searchout(Connection, Query, Timeout), State};
handle_call({'setindex', Column, Type, undefined}, _,
            #state{connection = Connection,
                   timeout = Timeout} = State) ->
    {reply, medici:setindex(Connection, Column, Type, Timeout), State};
handle_call({'setindex', Column, Type, Timeout}, _,
            #state{connection = Connection} = State) ->
    {reply, medici:setindex(Connection, Column, Type, Timeout), State};
handle_call({'update', Key, NewCols, undefined}, _,
            #state{connection = Connection,
                   timeout = Timeout} = State) ->
    {reply, medici:update(Connection, Key, NewCols, Timeout), State};
handle_call({'update', Key, NewCols, Timeout}, _,
            #state{connection = Connection} = State) ->
    {reply, medici:update(Connection, Key, NewCols, Timeout), State};
handle_call(stop, _,
            #state{data_title = DataTitle,
                   connection = Connection} = State) ->
    medici:close(Connection),
    {stop, atom_to_list(DataTitle) ++ " was requested to stop", State};
handle_call({do_queries, QueryList}, _,
            #state{data_title = DataTitle} = State) ->
    Response = cloud_data_interface:do_queries_group(QueryList,
        cloud_data_tokyotyrant, do_queries_sequentially,
        State, DataTitle),
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
terminate(_, #state{connection = Connection}) ->
    medici:close(Connection),
    ok.
code_change(_, State, _) ->
    {ok, State}.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

%% initialize the client state
init_state(DataTitle, [{database, _, Args}])
    when is_atom(DataTitle), is_list(Args) ->
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
     Native, ConnectOpts, AutoCopy, RunServer, []] =
        proplists_extensions:take_values(Defaults, Args),
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
    case medici:connect(MediciOptions) of
        {ok, Connection} ->
            {ok, #state{data_title = DataTitle,
                        timeout = Timeout,
                        connection = Connection}};
        {error, Reason} ->
            {stop, Reason}
    end.

%% do a single query and return a boolean to determine if the query succeeded
do_query(Query, Timeout, Connection) ->
    try (case string_extensions:binary_to_term(Query) of
            % basic medici API
            {'addint', Key, Integer}
                when is_list(Key), is_integer(Integer);
                     is_binary(Key), is_integer(Integer) ->
                medici:addint(Connection, Key, Integer, Timeout);
            {'adddouble', Key, Double}
                when is_list(Key), is_float(Double);
                     is_binary(Key), is_float(Double) ->
                medici:adddouble(Connection, Key, Double, Timeout);
            {'adddouble_parts', Key, IntegerPart, FractionalPart}
                when is_list(Key), is_integer(IntegerPart),
                                   is_integer(FractionalPart);
                     is_binary(Key), is_integer(IntegerPart),
                                     is_integer(FractionalPart) ->
                medici:adddouble_parts(Connection, Key,
                                       IntegerPart, FractionalPart, Timeout);
            {'copy', PathName}
                when is_list(PathName); is_binary(PathName) ->
                medici:copy(Connection, PathName, Timeout);
            {'fwmkeys', Prefix, MaxKeys}
                when is_list(Prefix), is_integer(MaxKeys);
                     is_binary(Prefix), is_integer(MaxKeys) ->
                medici:fwmkeys(Connection, Prefix, MaxKeys, Timeout);
            {'get', Key}
                when is_list(Key); is_binary(Key)  ->
                medici:get(Connection, Key, Timeout);
            {'iterinit'} ->
                medici:iterinit(Connection, Timeout);
            'iterinit' ->
                medici:iterinit(Connection, Timeout);
            {'iternext'} ->
                medici:iternext(Connection, Timeout);
            'iternext' ->
                medici:iternext(Connection, Timeout);
            {'mget', KeyList}
                when is_list(KeyList) ->
                medici:mget(Connection, KeyList, Timeout);
            {'optimize', TuningOptions}
                when is_list(TuningOptions); is_binary(TuningOptions) ->
                medici:optimize(Connection, TuningOptions, Timeout);
            {'out', Key}
                when is_list(Key); is_binary(Key) ->
                medici:out(Connection, Key, Timeout);
            {'put', Key, Value}
                when is_list(Key), is_integer(Value);
                     is_list(Key), is_float(Value);
                     is_list(Key), is_list(Value);
                     is_list(Key), is_binary(Value);
                     is_binary(Key), is_integer(Value);
                     is_binary(Key), is_float(Value);
                     is_binary(Key), is_list(Value);
                     is_binary(Key), is_binary(Value) ->
                medici:put(Connection, Key, Value, Timeout);
            {'putcat', Key, Value}
                when is_list(Key), is_integer(Value);
                     is_list(Key), is_float(Value);
                     is_list(Key), is_list(Value);
                     is_list(Key), is_binary(Value);
                     is_binary(Key), is_integer(Value);
                     is_binary(Key), is_float(Value);
                     is_binary(Key), is_list(Value);
                     is_binary(Key), is_binary(Value) ->
                medici:putcat(Connection, Key, Value, Timeout);
            {'putkeep', Key, Value}
                when is_list(Key), is_integer(Value);
                     is_list(Key), is_float(Value);
                     is_list(Key), is_list(Value);
                     is_list(Key), is_binary(Value);
                     is_binary(Key), is_integer(Value);
                     is_binary(Key), is_float(Value);
                     is_binary(Key), is_list(Value);
                     is_binary(Key), is_binary(Value) ->
                medici:putkeep(Connection, Key, Value, Timeout);
            {'putnr', Key, Value}
                when is_list(Key), is_integer(Value);
                     is_list(Key), is_float(Value);
                     is_list(Key), is_list(Value);
                     is_list(Key), is_binary(Value);
                     is_binary(Key), is_integer(Value);
                     is_binary(Key), is_float(Value);
                     is_binary(Key), is_list(Value);
                     is_binary(Key), is_binary(Value) ->
                medici:putnr(Connection, Key, Value, Timeout);
            {'putshl', Key, Value, Width}
                when is_list(Key), is_integer(Value), is_integer(Width);
                     is_list(Key), is_float(Value), is_integer(Width);
                     is_list(Key), is_list(Value), is_integer(Width);
                     is_list(Key), is_binary(Value), is_integer(Width);
                     is_binary(Key), is_integer(Value), is_integer(Width);
                     is_binary(Key), is_float(Value), is_integer(Width);
                     is_binary(Key), is_list(Value), is_integer(Width);
                     is_binary(Key), is_binary(Value), is_integer(Width) ->
                medici:putshl(Connection, Key, Value, Timeout);
            {'restore', PathName, TimeStamp}
                when is_list(PathName), is_integer(TimeStamp);
                     is_binary(PathName), is_integer(TimeStamp) ->
                medici:restore(Connection, PathName, TimeStamp, Timeout);
            {'rnum'} ->
                medici:rnum(Connection, Timeout);
            'rnum' ->
                medici:rnum(Connection, Timeout);
            {'setmst', HostName, Port}
                when is_list(HostName), is_integer(Port);
                     is_binary(HostName), is_integer(Port) ->
                medici:setmst(Connection, HostName, Port, Timeout);
            {'size'} ->
                medici:size(Connection, Timeout);
            'size' ->
                medici:size(Connection, Timeout);
            {'stat'} ->
                medici:stat(Connection, Timeout);
            'stat' ->
                medici:stat(Connection, Timeout);
            {'sync'} ->
                medici:sync(Connection, Timeout);
            'sync' ->
                medici:sync(Connection, Timeout);
            {'vanish'} ->
                medici:vanish(Connection, Timeout);
            'vanish' ->
                medici:vanish(Connection, Timeout);
            {'vsiz', Key}
                when is_list(Key); is_binary(Key) ->
                medici:vsiz(Connection, Key, Timeout);

            % table medici API
            {'genuid'} ->
                medici:genuid(Connection, Timeout);
            'genuid' ->
                medici:genuid(Connection, Timeout);
            {'query_add_condition', OldQuery, Column, Op, ExprList}
                when is_list(OldQuery), is_list(Column),
                     is_atom(Op), is_list(ExprList);
                     is_list(OldQuery), is_binary(Column),
                     is_atom(Op), is_list(ExprList);
                     is_list(OldQuery), is_list(Column),
                     is_tuple(Op), is_list(ExprList);
                     is_list(OldQuery), is_binary(Column),
                     is_tuple(Op), is_list(ExprList) ->
                medici:query_add_condition(
                    Connection, OldQuery, Column, Op, ExprList, Timeout);
            {'query_limit', OldQuery, Max}
                when is_list(OldQuery), is_integer(Max) ->
                medici:query_limit(Connection, OldQuery, Max, Timeout);
            {'query_limit_skip', OldQuery, Max, Skip}
                when is_list(OldQuery), is_integer(Max), is_integer(Skip) ->
                medici:query_limit_skip(
                    Connection, OldQuery, Max, Skip, Timeout);
            {'query_order', OldQuery, Column, Type}
                when is_list(OldQuery), Column == primary, is_atom(Type);
                     is_list(OldQuery), is_list(Column), is_atom(Type);
                     is_list(OldQuery), is_binary(Column), is_atom(Type) ->
                medici:query_order(Connection, OldQuery, Column, Type, Timeout);
            {'search', SearchQuery}
                when is_list(SearchQuery) ->
                medici:search(Connection, SearchQuery, Timeout);
            {'searchcount', SearchQuery}
                when is_list(SearchQuery) ->
                medici:searchcount(Connection, SearchQuery, Timeout);
            {'searchout', SearchQuery}
                when is_list(SearchQuery) ->
                medici:searchout(Connection, SearchQuery, Timeout);
            {'setindex', Column, Type}
                when Column == primary, is_atom(Type);
                     is_list(Column), is_atom(Type);
                     is_binary(Column), is_atom(Type) ->
                medici:setindex(Connection, Column, Type, Timeout);
            {'update', Key, NewCols}
                when is_list(Key), is_list(NewCols);
                     is_binary(Key), is_list(NewCols) ->
                medici:update(Connection, Key, NewCols, Timeout);
            _ ->
                {error, invalid_call}
    
        end) of
        {error, invalid_call} ->
            ?LOG_DEBUG("Invalid memcached command tuple ~p",
                       [binary_to_list(Query)]),
            false;
        _ ->
            % returns either
            % iolist or a proplist that has binaries for keys and values
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
do_queries_sequentially(QueryList, #state{connection = Connection,
                                          timeout = Timeout})
    when is_list(QueryList) ->
    lists:dropwhile(fun(Query) ->
        do_query(Query, Timeout, Connection)
    end, QueryList).

