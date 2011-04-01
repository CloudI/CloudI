%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
%%%
%%%------------------------------------------------------------------------
%%% @doc
%%% ==CloudI Tokyo Tyrant Data Module==
%%% @end
%%%
%%% BSD LICENSE
%%% 
%%% Copyright (c) 2009-2011, Michael Truog <mjtruog at gmail dot com>
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
%%% @copyright 2009-2011 Michael Truog
%%% @version 0.1.2 {@date} {@time}
%%%------------------------------------------------------------------------

-module(cloudi_job_db_tokyotyrant).
-author('mjtruog [at] gmail (dot) com').

-behaviour(cloudi_job).

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

%% cloudi_job callbacks
-export([cloudi_job_init/3,
         cloudi_job_handle_request/8,
         cloudi_job_handle_info/3,
         cloudi_job_terminate/2]).

-include("cloudi_logger.hrl").

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

%%-------------------------------------------------------------------------
%% @doc
%% ===Add an integer to the value in the database.===
%% @end
%%-------------------------------------------------------------------------

-spec addint(Dispatcher :: pid(),
             Name :: string(),
             Key :: iolist(),
             Integer :: integer()) ->
    any().

addint(Dispatcher, Name, Key, Integer)
    when is_pid(Dispatcher), is_list(Name),
         is_integer(Integer) ->
    cloudi_job:send_sync(Dispatcher, Name,
                         {addint, Key, Integer}).

-spec addint(Dispatcher :: pid(),
             Name :: string(),
             Key :: iolist(),
             Integer :: integer(),
             Timeout :: pos_integer()) ->
    any().

addint(Dispatcher, Name, Key, Integer, Timeout)
    when is_pid(Dispatcher), is_list(Name),
         is_integer(Integer), is_integer(Timeout) ->
    true = is_list(Key) or is_binary(Key),
    cloudi_job:send_sync(Dispatcher, Name,
                         {addint, Key, Integer}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add a double to the value in the database.===
%% @end
%%-------------------------------------------------------------------------

-spec adddouble(Dispatcher :: pid(),
                Name :: string(),
                Key :: iolist(),
                Double :: float()) ->
    any().

adddouble(Dispatcher, Name, Key, Double)
    when is_pid(Dispatcher), is_list(Name),
         is_float(Double) ->
    true = is_list(Key) or is_binary(Key),
    cloudi_job:send_sync(Dispatcher, Name,
                         {adddouble, Key, Double}).

-spec adddouble(Dispatcher :: pid(),
                Name :: string(),
                Key :: iolist(),
                Double :: float(),
                Timeout :: pos_integer()) ->
    any().

adddouble(Dispatcher, Name, Key, Double, Timeout)
    when is_pid(Dispatcher), is_list(Name),
         is_float(Double), is_integer(Timeout) ->
    true = is_list(Key) or is_binary(Key),
    cloudi_job:send_sync(Dispatcher, Name,
                         {adddouble, Key, Double}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add a double in separate parts to the value in the database.===
%% @end
%%-------------------------------------------------------------------------

-spec adddouble_parts(Dispatcher :: pid(),
                      Name :: string(),
                      Key :: iolist(),
                      IntegerPart :: integer(),
                      FractionalPart :: integer()) ->
    any().

adddouble_parts(Dispatcher, Name, Key, IntegerPart, FractionalPart)
    when is_pid(Dispatcher), is_list(Name),
         is_integer(IntegerPart), is_integer(FractionalPart) ->
    true = is_list(Key) or is_binary(Key),
    cloudi_job:send_sync(Dispatcher, Name,
                         {adddouble_parts, Key, IntegerPart, FractionalPart}).

-spec adddouble_parts(Dispatcher :: pid(),
                      Name :: string(),
                      Key :: iolist(),
                      IntegerPart :: integer(),
                      FractionalPart :: integer(),
                      Timeout :: pos_integer()) ->
    any().

adddouble_parts(Dispatcher, Name, Key, IntegerPart, FractionalPart, Timeout)
    when is_pid(Dispatcher), is_list(Name),
         is_integer(IntegerPart), is_integer(FractionalPart),
         is_integer(Timeout) ->
    true = is_list(Key) or is_binary(Key),
    cloudi_job:send_sync(Dispatcher, Name,
                         {adddouble_parts, Key, IntegerPart, FractionalPart},
                         Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Copy the database file.===
%% @end
%%-------------------------------------------------------------------------

-spec copy(Dispatcher :: pid(),
           Name :: string(),
           PathName :: string()) ->
    any().

copy(Dispatcher, Name, PathName)
    when is_pid(Dispatcher), is_list(Name) ->
    true = is_list(PathName) or is_binary(PathName),
    cloudi_job:send_sync(Dispatcher, Name,
                         {copy, PathName}).

-spec copy(Dispatcher :: pid(),
           Name :: string(),
           PathName :: string(),
           Timeout :: pos_integer()) ->
    any().

copy(Dispatcher, Name, PathName, Timeout)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout) ->
    true = is_list(PathName) or is_binary(PathName),
    cloudi_job:send_sync(Dispatcher, Name,
                         {copy, PathName}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Match some number of keys on a prefix.===
%% @end
%%-------------------------------------------------------------------------

-spec fwmkeys(Dispatcher :: pid(),
              Name :: string(),
              Prefix :: iolist(),
              MaxKeys :: non_neg_integer()) ->
    any().

fwmkeys(Dispatcher, Name, Prefix, MaxKeys)
    when is_pid(Dispatcher), is_list(Name),
         is_integer(MaxKeys) ->
    true = is_list(Prefix) or is_binary(Prefix),
    cloudi_job:send_sync(Dispatcher, Name,
                         {fwmkeys, Prefix, MaxKeys}).

-spec fwmkeys(Dispatcher :: pid(),
              Name :: string(),
              Prefix :: iolist(),
              MaxKeys :: non_neg_integer(),
              Timeout :: pos_integer()) ->
    any().

fwmkeys(Dispatcher, Name, Prefix, MaxKeys, Timeout)
    when is_pid(Dispatcher), is_list(Name),
         is_integer(MaxKeys), is_integer(Timeout) ->
    true = is_list(Prefix) or is_binary(Prefix),
    cloudi_job:send_sync(Dispatcher, Name,
                         {fwmkeys, Prefix, MaxKeys}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get a value from the database.===
%% @end
%%-------------------------------------------------------------------------

-spec get(Dispatcher :: pid(),
          Name :: string(),
          Key :: iolist()) ->
    any().

get(Dispatcher, Name, Key)
    when is_pid(Dispatcher), is_list(Name) ->
    true = is_list(Key) or is_binary(Key),
    cloudi_job:send_sync(Dispatcher, Name,
                         {get, Key}).

-spec get(Dispatcher :: pid(),
          Name :: string(),
          Key :: iolist(),
          Timeout :: pos_integer()) ->
    any().

get(Dispatcher, Name, Key, Timeout)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout) ->
    true = is_list(Key) or is_binary(Key),
    cloudi_job:send_sync(Dispatcher, Name,
                         {get, Key}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Initialize an iterator on all the keys in the database.===
%% @end
%%-------------------------------------------------------------------------

-spec iterinit(Dispatcher :: pid(),
               Name :: string()) ->
    any().

iterinit(Dispatcher, Name)
    when is_pid(Dispatcher), is_list(Name) ->
    cloudi_job:send_sync(Dispatcher, Name,
                         iterinit).

-spec iterinit(Dispatcher :: pid(),
               Name :: string(),
               Timeout :: pos_integer()) ->
    any().

iterinit(Dispatcher, Name, Timeout)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout) ->
    cloudi_job:send_sync(Dispatcher, Name,
                         iterinit, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the next key from the iterator on all the keys in the database.===
%% @end
%%-------------------------------------------------------------------------

-spec iternext(Dispatcher :: pid(),
               Name :: string()) ->
    any().

iternext(Dispatcher, Name)
    when is_pid(Dispatcher), is_list(Name) ->
    cloudi_job:send_sync(Dispatcher, Name,
                         iternext).

-spec iternext(Dispatcher :: pid(),
               Name :: string(),
               Timeout :: pos_integer()) ->
    any().

iternext(Dispatcher, Name, Timeout)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout) ->
    cloudi_job:send_sync(Dispatcher, Name,
                         iternext, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get multiple values from the database.===
%% @end
%%-------------------------------------------------------------------------

-spec mget(Dispatcher :: pid(),
           Name :: string(),
           KeyList :: list(iolist())) ->
    any().

mget(Dispatcher, Name, KeyList)
    when is_pid(Dispatcher), is_list(Name),
         is_list(KeyList) ->
    cloudi_job:send_sync(Dispatcher, Name,
                         {mget, KeyList}).

-spec mget(Dispatcher :: pid(),
           Name :: string(),
           KeyList :: list(iolist()),
           Timeout :: pos_integer()) ->
    any().

mget(Dispatcher, Name, KeyList, Timeout)
    when is_pid(Dispatcher), is_list(Name),
         is_list(KeyList), is_integer(Timeout) ->
    cloudi_job:send_sync(Dispatcher, Name,
                         {mget, KeyList}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Optimize the storage of values in the database.===
%% @end
%%-------------------------------------------------------------------------

-spec optimize(Dispatcher :: pid(),
               Name :: string(),
               TuningOptions :: iolist()) ->
    any().

optimize(Dispatcher, Name, TuningOptions)
    when is_pid(Dispatcher), is_list(Name) ->
    true = is_list(TuningOptions) or is_binary(TuningOptions),
    cloudi_job:send_sync(Dispatcher, Name,
                         {optimize, TuningOptions}).

-spec optimize(Dispatcher :: pid(),
               Name :: string(),
               TuningOptions :: iolist(),
               Timeout :: pos_integer()) ->
    any().

optimize(Dispatcher, Name, TuningOptions, Timeout)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout) ->
    true = is_list(TuningOptions) or is_binary(TuningOptions),
    cloudi_job:send_sync(Dispatcher, Name,
                         {optimize, TuningOptions}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Erase a value from the database.===
%% @end
%%-------------------------------------------------------------------------

-spec out(Dispatcher :: pid(),
          Name :: string(),
          Key :: iolist()) ->
    any().

out(Dispatcher, Name, Key)
    when is_pid(Dispatcher), is_list(Name) ->
    true = is_list(Key) or is_binary(Key),
    cloudi_job:send_sync(Dispatcher, Name,
                         {out, Key}).

-spec out(Dispatcher :: pid(),
          Name :: string(),
          Key :: iolist(),
          Timeout :: pos_integer()) ->
    any().

out(Dispatcher, Name, Key, Timeout)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout) ->
    true = is_list(Key) or is_binary(Key),
    cloudi_job:send_sync(Dispatcher, Name,
                         {out, Key}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Store a value in the database.===
%% @end
%%-------------------------------------------------------------------------

-spec put(Dispatcher :: pid(),
          Name :: string(),
          Key :: iolist(),
          Value :: integer() | float() | list() | binary()) ->
    any().

put(Dispatcher, Name, Key, Value)
    when is_pid(Dispatcher), is_list(Name) ->
    true = is_list(Key) or is_binary(Key),
    true = is_integer(Value) or is_float(Value) or
           is_list(Value) or is_binary(Value),
    cloudi_job:send_sync(Dispatcher, Name,
                         {put, Key, Value}).

-spec put(Dispatcher :: pid(),
          Name :: string(),
          Key :: iolist(),
          Value :: integer() | float() | list() | binary(),
          Timeout :: pos_integer()) ->
    any().

put(Dispatcher, Name, Key, Value, Timeout)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout) ->
    true = is_list(Key) or is_binary(Key),
    true = is_integer(Value) or is_float(Value) or
           is_list(Value) or is_binary(Value),
    cloudi_job:send_sync(Dispatcher, Name,
                         {put, Key, Value}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Append a value to the existing value in the database.===
%% @end
%%-------------------------------------------------------------------------

-spec putcat(Dispatcher :: pid(),
             Name :: string(),
             Key :: iolist(),
             Value :: integer() | float() | list() | binary()) ->
    any().

putcat(Dispatcher, Name, Key, Value)
    when is_pid(Dispatcher), is_list(Name) ->
    true = is_list(Key) or is_binary(Key),
    true = is_integer(Value) or is_float(Value) or
           is_list(Value) or is_binary(Value),
    cloudi_job:send_sync(Dispatcher, Name,
                         {putcat, Key, Value}).

-spec putcat(Dispatcher :: pid(),
             Name :: string(),
             Key :: iolist(),
             Value :: integer() | float() | list() | binary(),
             Timeout :: pos_integer()) ->
    any().

putcat(Dispatcher, Name, Key, Value, Timeout)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout) ->
    true = is_list(Key) or is_binary(Key),
    true = is_integer(Value) or is_float(Value) or
           is_list(Value) or is_binary(Value),
    cloudi_job:send_sync(Dispatcher, Name,
                         {putcat, Key, Value}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Store a value in the database only if there currently is no value for the given key.===
%% @end
%%-------------------------------------------------------------------------

-spec putkeep(Dispatcher :: pid(),
              Name :: string(),
              Key :: iolist(),
              Value :: integer() | float() | list() | binary()) ->
    any().

putkeep(Dispatcher, Name, Key, Value)
    when is_pid(Dispatcher), is_list(Name) ->
    true = is_list(Key) or is_binary(Key),
    true = is_integer(Value) or is_float(Value) or
           is_list(Value) or is_binary(Value),
    cloudi_job:send_sync(Dispatcher, Name,
                         {putkeep, Key, Value}).

-spec putkeep(Dispatcher :: pid(),
              Name :: string(),
              Key :: iolist(),
              Value :: integer() | float() | list() | binary(),
              Timeout :: pos_integer()) ->
    any().

putkeep(Dispatcher, Name, Key, Value, Timeout)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout) ->
    true = is_list(Key) or is_binary(Key),
    true = is_integer(Value) or is_float(Value) or
           is_list(Value) or is_binary(Value),
    cloudi_job:send_sync(Dispatcher, Name,
                         {putkeep, Key, Value}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Store a value in the database but do not wait for the database to acknowledge the change.===
%% @end
%%-------------------------------------------------------------------------

-spec putnr(Dispatcher :: pid(),
            Name :: string(),
            Key :: iolist(),
            Value :: integer() | float() | list() | binary()) ->
    any().

putnr(Dispatcher, Name, Key, Value)
    when is_pid(Dispatcher), is_list(Name) ->
    true = is_list(Key) or is_binary(Key),
    true = is_integer(Value) or is_float(Value) or
           is_list(Value) or is_binary(Value),
    cloudi_job:send_sync(Dispatcher, Name,
                         {putnr, Key, Value}).

-spec putnr(Dispatcher :: pid(),
            Name :: string(),
            Key :: iolist(),
            Value :: integer() | float() | list() | binary(),
            Timeout :: pos_integer()) ->
    any().

putnr(Dispatcher, Name, Key, Value, Timeout)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout) ->
    true = is_list(Key) or is_binary(Key),
    true = is_integer(Value) or is_float(Value) or
           is_list(Value) or is_binary(Value),
    cloudi_job:send_sync(Dispatcher, Name,
                         {putnr, Key, Value}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Append a value to the existing value in the database and shift it left.===
%% @end
%%-------------------------------------------------------------------------

-spec putshl(Dispatcher :: pid(),
             Name :: string(),
             Key :: iolist(),
             Value :: integer() | float() | list() | binary(),
             Width :: integer()) ->
    any().

putshl(Dispatcher, Name, Key, Value, Width)
    when is_pid(Dispatcher), is_list(Name),
         is_integer(Width) ->
    true = is_list(Key) or is_binary(Key),
    true = is_integer(Value) or is_float(Value) or
           is_list(Value) or is_binary(Value),
    cloudi_job:send_sync(Dispatcher, Name,
                         {putshl, Key, Value, Width}).


-spec putshl(Dispatcher :: pid(),
             Name :: string(),
             Key :: iolist(),
             Value :: integer() | float() | list() | binary(),
             Width :: integer(),
             Timeout :: pos_integer()) ->
    any().

putshl(Dispatcher, Name, Key, Value, Width, Timeout)
    when is_pid(Dispatcher), is_list(Name),
         is_integer(Width), is_integer(Timeout) ->
    true = is_list(Key) or is_binary(Key),
    true = is_integer(Value) or is_float(Value) or
           is_list(Value) or is_binary(Value),
    cloudi_job:send_sync(Dispatcher, Name,
                         {putshl, Key, Value, Width}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Restore a database file from the update log.===
%% @end
%%-------------------------------------------------------------------------

-spec restore(Dispatcher :: pid(),
              Name :: string(),
              PathName :: iolist(),
              TimeStamp :: integer()) ->
    any().

restore(Dispatcher, Name, PathName, TimeStamp)
    when is_pid(Dispatcher), is_list(Name),
         is_integer(TimeStamp) ->
    true = is_list(PathName) or is_binary(PathName),
    cloudi_job:send_sync(Dispatcher, Name,
                         {restore, PathName, TimeStamp}).

-spec restore(Dispatcher :: pid(),
              Name :: string(),
              PathName :: iolist(),
              TimeStamp :: integer(),
              Timeout :: pos_integer()) ->
    any().

restore(Dispatcher, Name, PathName, TimeStamp, Timeout)
    when is_pid(Dispatcher), is_list(Name),
         is_integer(TimeStamp), is_integer(Timeout) ->
    true = is_list(PathName) or is_binary(PathName),
    cloudi_job:send_sync(Dispatcher, Name,
                         {restore, PathName, TimeStamp}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the number of values currently in the database.===
%% @end
%%-------------------------------------------------------------------------

-spec rnum(Dispatcher :: pid(),
           Name :: string()) ->
    any().

rnum(Dispatcher, Name)
    when is_pid(Dispatcher), is_list(Name) ->
    cloudi_job:send_sync(Dispatcher, Name,
                         rnum).

-spec rnum(Dispatcher :: pid(),
           Name :: string(),
           Timeout :: pos_integer()) ->
    any().

rnum(Dispatcher, Name, Timeout)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout) ->
    cloudi_job:send_sync(Dispatcher, Name,
                         rnum, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Set the replication master of the database.===
%% @end
%%-------------------------------------------------------------------------

-spec setmst(Dispatcher :: pid(),
             Name :: string(),
             Host :: iolist(),
             Port :: integer()) ->
    any().

setmst(Dispatcher, Name, HostName, Port)
    when is_pid(Dispatcher), is_list(Name),
         is_integer(Port) ->
    true = is_list(HostName) or is_binary(HostName),
    cloudi_job:send_sync(Dispatcher, Name,
                         {setmst, HostName, Port}).

-spec setmst(Dispatcher :: pid(),
             Name :: string(),
             Host :: iolist(),
             Port :: integer(),
             Timeout :: pos_integer()) ->
    any().

setmst(Dispatcher, Name, HostName, Port, Timeout)
    when is_pid(Dispatcher), is_list(Name),
         is_integer(Port), is_integer(Timeout) ->
    true = is_list(HostName) or is_binary(HostName),
    cloudi_job:send_sync(Dispatcher, Name,
                         {setmst, HostName, Port}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the size of the database.===
%% @end
%%-------------------------------------------------------------------------

-spec size(Dispatcher :: pid(),
           Name :: string()) ->
    any().

size(Dispatcher, Name)
    when is_pid(Dispatcher), is_list(Name) ->
    cloudi_job:send_sync(Dispatcher, Name,
                         size).

-spec size(Dispatcher :: pid(),
           Name :: string(),
           Timeout :: pos_integer()) ->
    any().

size(Dispatcher, Name, Timeout)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout) ->
    cloudi_job:send_sync(Dispatcher, Name,
                         size, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the status string from the database.===
%% @end
%%-------------------------------------------------------------------------

-spec stat(Dispatcher :: pid(),
           Name :: string()) ->
    any().

stat(Dispatcher, Name)
    when is_pid(Dispatcher), is_list(Name) ->
    cloudi_job:send_sync(Dispatcher, Name,
                         stat).

-spec stat(Dispatcher :: pid(),
           Name :: string(),
           Timeout :: pos_integer()) ->
    any().

stat(Dispatcher, Name, Timeout)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout) ->
    cloudi_job:send_sync(Dispatcher, Name,
                         stat, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Synchronize the updated contents of the database with the filesystem.===
%% @end
%%-------------------------------------------------------------------------

-spec sync(Dispatcher :: pid(),
           Name :: string()) ->
    any().

sync(Dispatcher, Name)
    when is_pid(Dispatcher), is_list(Name) ->
    cloudi_job:send_sync(Dispatcher, Name,
                         sync).

-spec sync(Dispatcher :: pid(),
           Name :: string(),
           Timeout :: pos_integer()) ->
    any().

sync(Dispatcher, Name, Timeout)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout) ->
    cloudi_job:send_sync(Dispatcher, Name,
                         sync, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Erase all values from the database.===
%% @end
%%-------------------------------------------------------------------------

-spec vanish(Dispatcher :: pid(),
             Name :: string()) ->
    any().

vanish(Dispatcher, Name)
    when is_pid(Dispatcher), is_list(Name) ->
    cloudi_job:send_sync(Dispatcher, Name,
                         vanish).

-spec vanish(Dispatcher :: pid(),
             Name :: string(),
             Timeout :: pos_integer()) ->
    any().

vanish(Dispatcher, Name, Timeout)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout) ->
    cloudi_job:send_sync(Dispatcher, Name,
                         vanish, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the size of a value in the database.===
%% @end
%%-------------------------------------------------------------------------

-spec vsiz(Dispatcher :: pid(),
           Name :: string(),
           Key :: iolist()) ->
    any().

vsiz(Dispatcher, Name, Key)
    when is_pid(Dispatcher), is_list(Name) ->
    true = is_list(Key) or is_binary(Key),
    cloudi_job:send_sync(Dispatcher, Name,
                         {vsiz, Key}).

-spec vsiz(Dispatcher :: pid(),
           Name :: string(),
           Key :: iolist(),
           Timeout :: pos_integer()) ->
    any().

vsiz(Dispatcher, Name, Key, Timeout)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout) ->
    true = is_list(Key) or is_binary(Key),
    cloudi_job:send_sync(Dispatcher, Name,
                         {vsiz, Key}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Generate a unique ID number for the database.===
%% @end
%%-------------------------------------------------------------------------

-spec genuid(Dispatcher :: pid(),
             Name :: string()) ->
    any().

genuid(Dispatcher, Name)
    when is_pid(Dispatcher), is_list(Name) ->
    cloudi_job:send_sync(Dispatcher, Name,
                         genuid).

-spec genuid(Dispatcher :: pid(),
             Name :: string(),
             Timeout :: pos_integer()) ->
    any().

genuid(Dispatcher, Name, Timeout)
    when is_pid(Dispatcher), is_list(Name), is_integer(Timeout) ->
    cloudi_job:send_sync(Dispatcher, Name,
                         genuid, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Add a constraint on a query.===
%% @end
%%-------------------------------------------------------------------------

-spec query_add_condition(Dispatcher :: pid(),
                          Name :: string(),
                          OldQuery :: list(),
                          Column :: iolist(),
                          Op :: atom(),
                          ExprList :: list()) ->
    any().

query_add_condition(Dispatcher, Name, OldQuery, Column, Op, ExprList)
    when is_pid(Dispatcher), is_list(Name),
         is_list(OldQuery), is_list(ExprList) ->
    true = is_list(Column) or is_binary(Column),
    true = is_atom(Op) or is_tuple(Op),
    cloudi_job:send_sync(Dispatcher, Name,
                         {query_add_condition,
                          OldQuery, Column, Op, ExprList}).

-spec query_add_condition(Dispatcher :: pid(),
                          Name :: string(),
                          OldQuery :: list(),
                          Column :: iolist(),
                          Op :: atom(),
                          ExprList :: list(),
                          Timeout :: pos_integer()) ->
    any().

query_add_condition(Dispatcher, Name, OldQuery, Column, Op, ExprList, Timeout)
    when is_pid(Dispatcher), is_list(Name),
         is_list(OldQuery), is_list(ExprList), is_integer(Timeout) ->
    true = is_list(Column) or is_binary(Column),
    true = is_atom(Op) or is_tuple(Op),
    cloudi_job:send_sync(Dispatcher, Name,
                         {query_add_condition,
                          OldQuery, Column, Op, ExprList}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Limit the number of values returned by a query.===
%% @end
%%-------------------------------------------------------------------------

-spec query_limit(Dispatcher :: pid(),
                  Name :: string(),
                  OldQuery :: list(),
                  Max :: integer()) ->
    any().

query_limit(Dispatcher, Name, OldQuery, Max)
    when is_pid(Dispatcher), is_list(Name),
         is_list(OldQuery), is_integer(Max) ->
    cloudi_job:send_sync(Dispatcher, Name,
                         {query_limit, OldQuery, Max}).

-spec query_limit(Dispatcher :: pid(),
                  Name :: string(),
                  OldQuery :: list(),
                  Max :: integer(),
                  Timeout :: pos_integer()) ->
    any().

query_limit(Dispatcher, Name, OldQuery, Max, Timeout)
    when is_pid(Dispatcher), is_list(Name),
         is_list(OldQuery), is_integer(Max), is_integer(Timeout) ->
    cloudi_job:send_sync(Dispatcher, Name,
                         {query_limit, OldQuery, Max}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===After applying an offset into the values returned by a query, limit the remaining results.===
%% @end
%%-------------------------------------------------------------------------

-spec query_limit_skip(Dispatcher :: pid(),
                       Name :: string(),
                       OldQuery :: list(),
                       Max :: integer(),
                       Skip :: integer()) ->
    any().

query_limit_skip(Dispatcher, Name, OldQuery, Max, Skip)
    when is_pid(Dispatcher), is_list(Name),
         is_list(OldQuery), is_integer(Max), is_integer(Skip) ->
    cloudi_job:send_sync(Dispatcher, Name,
                         {query_limit_skip, OldQuery, Max, Skip}).

-spec query_limit_skip(Dispatcher :: pid(),
                       Name :: string(),
                       OldQuery :: list(),
                       Max :: integer(),
                       Skip :: integer(),
                       Timeout :: pos_integer()) ->
    any().

query_limit_skip(Dispatcher, Name, OldQuery, Max, Skip, Timeout)
    when is_pid(Dispatcher), is_list(Name),
         is_list(OldQuery), is_integer(Max), is_integer(Skip),
         is_integer(Timeout) ->
    cloudi_job:send_sync(Dispatcher, Name,
                         {query_limit_skip, OldQuery, Max, Skip}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Set the order of a query.===
%% @end
%%-------------------------------------------------------------------------

-spec query_order(Dispatcher :: pid(),
                  Name :: string(),
                  OldQuery :: list(),
                  Column :: 'primary' | iolist(),
                  Type :: atom()) ->
    any().

query_order(Dispatcher, Name, OldQuery, Column, Type)
    when is_pid(Dispatcher), is_list(Name),
         is_list(OldQuery), is_atom(Type) ->
    true = (Column == primary) or is_list(Column) or is_binary(Column),
    cloudi_job:send_sync(Dispatcher, Name,
                         {query_order, OldQuery, Column, Type}).

-spec query_order(Dispatcher :: pid(),
                  Name :: string(),
                  OldQuery :: list(),
                  Column :: 'primary' | iolist(),
                  Type :: atom(),
                  Timeout :: pos_integer()) ->
    any().

query_order(Dispatcher, Name, OldQuery, Column, Type, Timeout)
    when is_pid(Dispatcher), is_list(Name),
         is_list(OldQuery), is_atom(Type), is_integer(Timeout) ->
    true = (Column == primary) or is_list(Column) or is_binary(Column),
    cloudi_job:send_sync(Dispatcher, Name,
                         {query_order, OldQuery, Column, Type}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Execute the search of a query.===
%% @end
%%-------------------------------------------------------------------------

-spec search(Dispatcher :: pid(),
             Name :: string(),
             Query :: list()) ->
    any().

search(Dispatcher, Name, Query)
    when is_pid(Dispatcher), is_list(Name),
         is_list(Query) ->
    cloudi_job:send_sync(Dispatcher, Name,
                         {search, Query}).

-spec search(Dispatcher :: pid(),
             Name :: string(),
             Query :: list(),
             Timeout :: pos_integer()) ->
    any().

search(Dispatcher, Name, Query, Timeout)
    when is_pid(Dispatcher), is_list(Name),
         is_list(Query), is_integer(Timeout) ->
    cloudi_job:send_sync(Dispatcher, Name,
                         {search, Query}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Get the number of values returned from a query.===
%% @end
%%-------------------------------------------------------------------------

-spec searchcount(Dispatcher :: pid(),
                  Name :: string(),
                  Query :: list()) ->
    any().

searchcount(Dispatcher, Name, Query)
    when is_pid(Dispatcher), is_list(Name),
         is_list(Query) ->
    cloudi_job:send_sync(Dispatcher, Name,
                         {searchcount, Query}).

-spec searchcount(Dispatcher :: pid(),
                  Name :: string(),
                  Query :: list(),
                  Timeout :: pos_integer()) ->
    any().

searchcount(Dispatcher, Name, Query, Timeout)
    when is_pid(Dispatcher), is_list(Name),
         is_list(Query), is_integer(Timeout) ->
    cloudi_job:send_sync(Dispatcher, Name,
                         {searchcount, Query}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Erase all values that match the query.===
%% @end
%%-------------------------------------------------------------------------

-spec searchout(Dispatcher :: pid(),
                Name :: string(),
                Query :: list()) ->
    any().

searchout(Dispatcher, Name, Query)
    when is_pid(Dispatcher), is_list(Name),
         is_list(Query) ->
    cloudi_job:send_sync(Dispatcher, Name,
                         {searchout, Query}).

-spec searchout(Dispatcher :: pid(),
                Name :: string(),
                Query :: list(),
                Timeout :: pos_integer()) ->
    any().

searchout(Dispatcher, Name, Query, Timeout)
    when is_pid(Dispatcher), is_list(Name),
         is_list(Query), is_integer(Timeout) ->
    cloudi_job:send_sync(Dispatcher, Name,
                         {searchout, Query}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Set an index on a database table.===
%% @end
%%-------------------------------------------------------------------------

-spec setindex(Dispatcher :: pid(),
               Name :: string(),
               Column :: 'primary' | iolist(),
               Type :: atom()) ->
    any().

setindex(Dispatcher, Name, Column, Type)
    when is_pid(Dispatcher), is_list(Name),
         is_atom(Type) ->
    true = (Column == primary) or is_list(Column) or is_binary(Column),
    cloudi_job:send_sync(Dispatcher, Name,
                         {setindex, Column, Type}).

-spec setindex(Dispatcher :: pid(),
               Name :: string(),
               Column :: 'primary' | iolist(),
               Type :: atom(),
               Timeout :: 'undefined' | pos_integer()) ->
    any().

setindex(Dispatcher, Name, Column, Type, Timeout)
    when is_pid(Dispatcher), is_list(Name),
         is_atom(Type), is_integer(Timeout) ->
    true = (Column == primary) or is_list(Column) or is_binary(Column),
    cloudi_job:send_sync(Dispatcher, Name,
                         {setindex, Column, Type}, Timeout).

%%-------------------------------------------------------------------------
%% @doc
%% ===Append column data to an existing database table.===
%% @end
%%-------------------------------------------------------------------------

-spec update(Dispatcher :: pid(),
             Name :: string(),
             Key :: iolist(),
             NewCols :: list()) ->
    any().

update(Dispatcher, Name, Key, NewCols)
    when is_pid(Dispatcher), is_list(Name),
         is_list(NewCols) ->
    true = is_list(Key) or is_binary(Key),
    cloudi_job:send_sync(Dispatcher, Name,
                         {update, Key, NewCols}).

-spec update(Dispatcher :: pid(),
             Name :: string(),
             Key :: iolist(),
             NewCols :: list(),
             Timeout :: pos_integer()) ->
    any().

update(Dispatcher, Name, Key, NewCols, Timeout)
    when is_pid(Dispatcher), is_list(Name),
         is_list(NewCols), is_integer(Timeout) ->
    true = is_list(Key) or is_binary(Key),
    cloudi_job:send_sync(Dispatcher, Name,
                         {update, Key, NewCols}, Timeout).

%%%------------------------------------------------------------------------
%%% Callback functions from cloudi_job
%%%------------------------------------------------------------------------

cloudi_job_init([{database, DatabaseName, Args}], _Prefix, Dispatcher) ->
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
        proplists2:take_values(Defaults, Args),
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
            cloudi_job:subscribe(Dispatcher, DatabaseName),
            {ok, #state{connection = Connection}};
        {error, Reason} ->
            {stop, Reason}
    end.

cloudi_job_handle_request(_Type, _Name, Request, Timeout, _TransId, _Pid,
                          #state{connection = Connection} = State,
                          _Dispatcher) ->
    case Request of
        Command when is_binary(Command) ->
            Output = do_query(Command, Timeout, Connection),
            {reply, cloudi_response:new(Request, Output), State};
        {'addint', Key, Integer} ->
            {reply, medici:addint(Connection, Key, Integer, Timeout), State};
        {'adddouble', Key, Double} ->
            {reply, medici:adddouble(Connection, Key, Double, Timeout), State};
        {'adddouble_parts', Key, IntegerPart, FractionalPart} ->
            {reply, medici:adddouble_parts(Connection, Key, IntegerPart,
                                           FractionalPart, Timeout), State};
        {'copy', PathName} ->
            {reply, medici:copy(Connection, PathName, Timeout), State};
        {'fwmkeys', Prefix, MaxKeys} ->
            {reply, medici:fwmkeys(Connection, Prefix, MaxKeys,
                                   Timeout), State};
        {'get', Key} ->
            {reply, medici:get(Connection, Key, Timeout), State};
        'iterinit' ->
            {reply, medici:iterinit(Connection, Timeout), State};
        'iternext' ->
            {reply, medici:iternext(Connection, Timeout), State};
        {'mget', KeyList} ->
            {reply, medici:mget(Connection, KeyList, Timeout), State};
        {'optimize', TuningOptions} ->
            {reply, medici:optimize(Connection, TuningOptions, Timeout), State};
        {'out', Key} ->
            {reply, medici:out(Connection, Key, Timeout), State};
        {'put', Key, Value} ->
            {reply, medici:put(Connection, Key, Value, Timeout), State};
        {'putcat', Key, Value} ->
            {reply, medici:putcat(Connection, Key, Value, Timeout), State};
        {'putkeep', Key, Value} ->
            {reply, medici:putkeep(Connection, Key, Value, Timeout), State};
        {'putnr', Key, Value} ->
            {reply, medici:putnr(Connection, Key, Value, Timeout), State};
        {'putshl', Key, Value, Width} ->
            {reply, medici:putshl(Connection, Key, Value, Width,
                    Timeout), State};
        {'restore', PathName, TimeStamp} ->
            {reply, medici:restore(Connection, PathName, TimeStamp,
                                   Timeout), State};
        'rnum' ->
            {reply, medici:rnum(Connection, Timeout), State};
        {'setmst', HostName, Port} ->
            {reply, medici:setmst(Connection, HostName, Port, Timeout), State};
        'size' ->
            {reply, medici:size(Connection, Timeout), State};
        'stat' ->
            {reply, medici:stat(Connection, Timeout), State};
        'sync' ->
            {reply, medici:sync(Connection, Timeout), State};
        'vanish' ->
            {reply, medici:vanish(Connection, Timeout), State};
        {'vsiz', Key} ->
            {reply, medici:vsiz(Connection, Key, Timeout), State};
        'genuid' ->
            {reply, medici:genuid(Connection, Timeout), State};
        {'query_add_condition', OldQuery, Column, Op, ExprList} ->
            {reply, medici:query_add_condition(Connection, OldQuery, Column,
                                               Op, ExprList, Timeout), State};
        {'query_limit', OldQuery, Max} ->
            {reply, medici:query_limit(Connection, OldQuery, Max,
                                       Timeout), State};
        {'query_limit_skip', OldQuery, Max, Skip} ->
            {reply, medici:query_limit_skip(Connection, OldQuery, Max, Skip,
                                            Timeout), State};
        {'query_order', OldQuery, Column, Type} ->
            {reply, medici:query_order(Connection, OldQuery, Column, Type,
                                       Timeout), State};
        {'search', Query} ->
            {reply, medici:search(Connection, Query, Timeout), State};
        {'searchcount', Query} ->
            {reply, medici:searchcount(Connection, Query, Timeout), State};
        {'searchout', Query} ->
            {reply, medici:searchout(Connection, Query, Timeout), State};
        {'setindex', Column, Type} ->
            {reply, medici:setindex(Connection, Column, Type, Timeout), State};
        {'update', Key, NewCols} ->
            {reply, medici:update(Connection, Key, NewCols, Timeout), State}
    end.

cloudi_job_handle_info(Request, State, _) ->
    ?LOG_WARN("Unknown info \"~p\"", [Request]),
    {noreply, State}.

cloudi_job_terminate(_, #state{connection = Connection}) ->
    medici:close(Connection),
    ok.

%%%------------------------------------------------------------------------
%%% Private functions
%%%------------------------------------------------------------------------

%% do a single query and return a boolean to determine if the query succeeded
do_query(Query, Timeout, Connection) ->
    try (case string2:binary_to_term(Query) of
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
        'iterinit' ->
            medici:iterinit(Connection, Timeout);
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
        'rnum' ->
            medici:rnum(Connection, Timeout);
        {'setmst', HostName, Port}
            when is_list(HostName), is_integer(Port);
                 is_binary(HostName), is_integer(Port) ->
            medici:setmst(Connection, HostName, Port, Timeout);
        'size' ->
            medici:size(Connection, Timeout);
        'stat' ->
            medici:stat(Connection, Timeout);
        'sync' ->
            medici:sync(Connection, Timeout);
        'vanish' ->
            medici:vanish(Connection, Timeout);
        {'vsiz', Key}
            when is_list(Key); is_binary(Key) ->
            medici:vsiz(Connection, Key, Timeout);

        % table medici API
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
            ?LOG_ERROR("Invalid tokyotyrant command tuple ~p",
                       [binary_to_list(Query)]),
            <<>>;
            % returns either
            % iolist or a proplist that has binaries for keys and values
        Result when is_binary(Result) ->
            Result;
        Result when is_list(Result) ->
            string2:term_to_list(Result)
            
    catch
        _:Reason ->
            ?LOG_ERROR("exception when processing "
                       "tokyotyrant command tuple ~p: ~p",
                       [binary_to_list(Query), Reason]),
            <<>> 
    end.

