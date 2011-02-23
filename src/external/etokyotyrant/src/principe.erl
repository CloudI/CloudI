%%% The contents of this file are subject to the Erlang Public License,
%%% Version 1.1, (the "License"); you may not use this file except in
%%% compliance with the License. You should have received a copy of the
%%% Erlang Public License along with this software. If not, it can be
%%% retrieved via the world wide web at http://www.erlang.org/.
%%%
%%% Software distributed under the License is distributed on an "AS IS"
%%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%%% the License for the specific language governing rights and limitations
%%% under the License.
%%%-------------------------------------------------------------------
%%% File:      principe.erl
%%% @author    Jim McCoy <mccoy@mad-scientist.com>
%%% @copyright Copyright (c) 2009, Jim McCoy.  All Rights Reserved.
%%%
%%% @doc
%%% A thin Erlang wrapper for the Tokyo Tyrant network database protocol.
%%% Requires a Tyrant server that uses the 0.91 protocol version (Tyrant
%%% servers of version 1.1.23 and beyond.)
%%%
%%% Note: The Tyrant protocol is sensitive to endianness. Specifically, while
%%% the server will take in data in network-order it will store it internally
%%% in big or little endianness depending on the architecture that the Tyrant
%%% server is running on.  For many use-cases this is not a problem, but if you
%%% plan on storing an int or float and then operating on it using the addint,
%%% adddouble, or script extension functions which may just grab the internal
%%% data directly then it is necessary to store numbers in the proper format for
%%% the remote database. In these cases use the put* and adddouble functions which
%%% take an additional parameter to specify endianness of the remote database.
%%% There are also versions of the misc() and misc_no_update() functions that
%%% specify remote database byte-order for proper encoding of integers and floats
%%% in those functions.
%%% @end
%%%-------------------------------------------------------------------

-module(principe).
-export([connect/0,
         connect/1,
         put/4,
         put/5,
         putkeep/4,
         putkeep/5,
         putcat/4,
         putcat/5,
         putshl/5,
         putshl/6,
         putnr/4,
         putnr/5,
         out/3,
         get/3,
         mget/3,
         vsiz/3,
         iterinit/2,
         iternext/2,
         fwmkeys/4,
         addint/4,
         adddouble/4,
         adddouble/5,
         adddouble/6,
         sync/2,
         optimize/3,
         vanish/2,
         rnum/2,
         size/2,
         stat/2,
         copy/3,
         restore/4,
         restore_with_check/4,
         setmst/4,
         misc/4,
         misc/5,
         misc_no_update/4,
         misc_no_update/5,
         ext/6]).

-include("principe.hrl").

%%====================================================================
%% The Tokyo Tyrant access functions
%%====================================================================

%% @spec connect() -> {ok, port()} | error()
%%
%% @doc
%% Establish a connection to the tyrant service.
%% @end
connect() ->
    connect([]).

%% @spec connect(Options::proplist()) -> {ok, port()} | error()
%%
%% @doc
%% Establish a connection to the tyrant service using properties in the
%% ConnectProps proplist to determine the hostname, port number and tcp
%% socket options for the connection.  Any missing parameters are filled
%% in using the module defaults.
%% @end
connect(Options) ->
    Hostname = proplists:get_value(hostname, Options, ?TSERVER),
    Port = proplists:get_value(port, Options, ?TPORT),
    Timeout = proplists:get_value(timeout, Options, infinity),
    ConnectionOptions =
        proplists:get_value(connect_opts, Options, []) ++ ?TOPTS,
    ExtraConnectionOptions = if
        Timeout /= infinity ->
            [{send_timeout, Timeout}];
        true ->
            []
    end,
    gen_tcp:connect(Hostname, Port,
        ConnectionOptions ++ ExtraConnectionOptions, Timeout).

%% @spec put(Socket::port(),
%%           Key::key(),
%%           Value::value_or_num(),
%%           Timeout::integer()) -> ok | error()
%%
%% @doc
%% Call the Tyrant server to store a new value for the given key.
%% @end
put(Socket, Key, Value, Timeout) when is_integer(Value), Value < 4294967296 ->
    put(Socket, Key, <<Value:32>>, Timeout);
put(Socket, Key, Value, Timeout) when is_float(Value) ->
    put(Socket, Key, <<Value:64/float>>, Timeout);
put(Socket, Key, Value, Timeout) ->
    ?T2(?PUT),
    ?R_SUCCESS(Timeout).

%% @spec put(Socket::port(),
%%           Key::key(),
%%           Value::value_or_num(),
%%           endian(),
%%           Timeout::integer()) -> ok | error()
%%
%% @doc
%% Call the Tyrant server to store a new value for the given key, the
%% fourth parameter determines how integer and float values are stored
%% on the remote database.
%% @end
put(Socket, Key, Value, big, Timeout) ->
    put(Socket, Key, Value, Timeout);
put(Socket, Key, Value, little, Timeout) when is_integer(Value) ->
    put(Socket, Key, <<Value:32/little>>, Timeout);
put(Socket, Key, Value, little, Timeout) when is_float(Value) ->
    put(Socket, Key, <<Value:64/little-float>>, Timeout);
put(Socket, Key, Value, little, Timeout) ->
    put(Socket, Key, Value, Timeout).

%% @spec putkeep(Socket::port(),
%%               Key::key(),
%%               Value::value_or_num(),
%%               Timeout::integer()) -> ok | error()
%%
%% @doc
%% Call the Tyrant server to put a new key/value pair into the remote
%% database.  Will return an error if there is already a value for the
%% Key provided.
%% @end
putkeep(Socket, Key, Value, Timeout) when is_integer(Value), Value < 4294967296 ->
    putkeep(Socket, Key, <<Value:32>>, Timeout);
putkeep(Socket, Key, Value, Timeout) when is_float(Value) ->
    putkeep(Socket, Key, <<Value:64/float>>, Timeout);
putkeep(Socket, Key, Value, Timeout) ->
    ?T2(?PUTKEEP),
    ?R_SUCCESS(Timeout).

%% @spec putkeep(Socket::port(),
%%               Key::key(),
%%               Value::value_or_num()
%%               endian(),
%%               Timeout::integer()) -> ok | error()
%%
%% @doc
%% Call the Tyrant server to put a new key/value pair into the remote
%% database.  Will return an error if there is already a value for the
%% Key provided.  The fourth parameter determines endianness for encoding
%% numbers on the remote database.
%% @end
putkeep(Socket, Key, Value, big, Timeout) ->
    putkeep(Socket, Key, Value, Timeout);
putkeep(Socket, Key, Value, little, Timeout) when is_integer(Value), Value < 4294967296 ->
    putkeep(Socket, Key, <<Value:32/little>>, Timeout);
putkeep(Socket, Key, Value, little, Timeout) when is_float(Value) ->
    putkeep(Socket, Key, <<Value:64/little-float>>, Timeout);
putkeep(Socket, Key, Value, little, Timeout) ->
    putkeep(Socket, Key, Value, Timeout).

%% @spec putcat(Socket::port(),
%%              Key::key(),
%%              Value::value_or_num(),
%%              Timeout::integer()) -> ok | error()
%%
%% @doc
%% Concatenate a value to the end of the current value for a given key
%% that is stored in the remote database.  If Key does not already
%% exist in the database then this call will operate the same as put().
%% @end
putcat(Socket, Key, Value, Timeout) when is_integer(Value), Value < 4294967296 ->
    putcat(Socket, Key, <<Value:32>>, Timeout);
putcat(Socket, Key, Value, Timeout) when is_float(Value) ->
    putcat(Socket, Key, <<Value:64/float>>, Timeout);
putcat(Socket, Key, Value, Timeout) ->
    ?T2(?PUTCAT),
    ?R_SUCCESS(Timeout).

%% @spec putcat(Socket::port(),
%%              Key::key(),
%%              Value::value_or_num(),
%%              endian(),
%%              Timeout::integer()) -> ok | error()
%%
%% @doc
%% Concatenate a value to the end of the current value for a given key
%% that is stored in the remote database.  If Key does not already
%% exist in the database then this call will operate the same as put().
%% The last parameter determines endian encoding on the remote database.
%% @end
putcat(Socket, Key, Value, big, Timeout) ->
    putcat(Socket, Key, Value, Timeout);
putcat(Socket, Key, Value, little, Timeout) when is_integer(Value), Value < 4294967296 ->
    putcat(Socket, Key, <<Value:32/little>>, Timeout);
putcat(Socket, Key, Value, little, Timeout) when is_float(Value) ->
    putcat(Socket, Key, <<Value:64/little-float>>, Timeout);
putcat(Socket, Key, Value, little, Timeout) ->
    putcat(Socket, Key, Value, Timeout).

%% @spec putshl(Socket::port(),
%%              Key::key(),
%%              Value::value_or_num(),
%%              Width::integer(),
%%              Timeout::integer()) -> ok | error()
%%
%% @doc
%% Concatenate a value to a given key in the remote database and shift the
%% resulting value to the left until it is Width bytes long.
%% @end
putshl(Socket, Key, Value, Width, Timeout) when is_integer(Value), Value < 4294967296 ->
    putshl(Socket, Key, <<Value:32>>, Width, Timeout);
putshl(Socket, Key, Value, Width, Timeout) when is_float(Value) ->
    putshl(Socket, Key, <<Value:64/float>>, Width, Timeout);
putshl(Socket, Key, Value, Width, Timeout) when is_integer(Width) ->
    gen_tcp:send(Socket, [<<?PUTSHL:16>>,
              <<(iolist_size(Key)):32>>,
              <<(iolist_size(Value)):32>>,
              <<Width:32>>, Key, Value]),
    ?R_SUCCESS(Timeout).

%% @spec putshl(Socket::port(),
%%              Key::key(),
%%              Value::value_or_num(),
%%              Width::integer(),
%%              endian(),
%%              Timeout::integer()) -> ok | error()
%%
%% @doc
%% Concatenate a value to a given key in the remote database and shift the
%% resulting value to the left until it is Width bytes long.  The last
%% parameter determines byte-order encoding for the remote database.
%% @end
putshl(Socket, Key, Value, Width, big, Timeout) ->
    putshl(Socket, Key, Value, Width, Timeout);
putshl(Socket, Key, Value, Width, little, Timeout) when is_integer(Value), Value < 4294967296 ->
    putshl(Socket, Key, <<Value:32/little>>, Width, Timeout);
putshl(Socket, Key, Value, Width, little, Timeout) when is_float(Value) ->
    putshl(Socket, Key, <<Value:64/little-float>>, Width, Timeout);
putshl(Socket, Key, Value, Width, little, Timeout) ->
    putshl(Socket, Key, Value, Width, Timeout).

%% @spec putnr(Socket::port(),
%%             Key::key(),
%%             Value::value_or_num(),
%%             Timeout::integer()) -> ok
%%
%% @doc
%% Put a key/value pair to the remote database and do not wait for a response.
%% @end
putnr(Socket, Key, Value, Timeout) when is_integer(Value), Value < 4294967296 ->
    putnr(Socket, Key, <<Value:32>>, Timeout);
putnr(Socket, Key, Value, Timeout) when is_float(Value) ->
    putnr(Socket, Key, <<Value:64/float>>, Timeout);
putnr(Socket, Key, Value, _) ->
    ?T2(?PUTNR),
    ok.

%% @spec putnr(Socket::port(),
%%             Key::key(),
%%             Value::value_or_num(),
%%             endian(),
%%             Timeout::integer()) -> ok
%%
%% @doc
%% Put a key/value pair to the remote database and do not wait for a response.
%% The fourth parameter will decide how numbers are encoded for the remote
%% database.
%% @end
putnr(Socket, Key, Value, big, Timeout) ->
    putnr(Socket, Key, Value, Timeout);
putnr(Socket, Key, Value, little, Timeout) when is_integer(Value), Value < 4294967296 ->
    putnr(Socket, Key, <<Value:32/little>>, Timeout);
putnr(Socket, Key, Value, little, Timeout) when is_float(Value) ->
    putnr(Socket, Key, <<Value:64/little-float>>, Timeout);
putnr(Socket, Key, Value, little, Timeout) ->
    putnr(Socket, Key, Value, Timeout).

%% @spec out(Socket::port(),
%%           Key::key(),
%%           Timeout::integer()) -> ok | error()
%%
%% @doc
%% Remove a key from the remote database.  Will return an error if Key is
%% not in the database.
%% @end
out(Socket, Key, Timeout) ->
    ?T1(?OUT),
    ?R_SUCCESS(Timeout).

%% @spec get(Socket::port(),
%%           Key::key(),
%%           Timeout::integer()) -> binary() | error()
%%
%% @doc Get the value for a given key
get(Socket, Key, Timeout) ->
    ?T1(?GET),
    ?R_SIZE_DATA(Timeout).

%% @spec mget(Socket::port(),
%%            KeyList::keylist(),
%%            Timeout::integer()) -> [{Key::binary(), Value::binary()}] | error()
%%
%% @doc Get the values for a list of keys
mget(Socket, KeyList, Timeout) when is_list(KeyList) ->
    gen_tcp:send(Socket, [<<?MGET:16>>,
              <<(length(KeyList)):32>>,
              [[<<(iolist_size(Key)):32>>, Key] || Key <- KeyList]
             ]),
    ?R_4TUPLE(Timeout).

%% @spec vsiz(Socket::port(),
%%            Key::key(),
%%            Timeout::integer()) -> integer()
%%
%% @doc Get the size of the value for a given key.
vsiz(Socket, Key, Timeout) ->
    ?T1(?VSIZ),
    ?R_INT32(Timeout).

%% @spec iterinit(Socket::port(), Timeout::integer()) -> ok | error()
%%
%% @doc Start iteration protocol.  WARNING: The tyrant iteration protocol has no
%% concurrency controls whatsoever, so if multiple clients try to do iteration
%% they will stomp all over each other!
%% @end
iterinit(Socket, Timeout) ->
    ?T0(?ITERINIT),
    ?R_SUCCESS(Timeout).

%% @spec iternext(Socket::port(), Timeout::integer()) -> Key::binary() | error()
%%
%% @doc Get the next key/value pair in the iteration protocol
iternext(Socket, Timeout) ->
    ?T0(?ITERNEXT),
    ?R_SIZE_DATA(Timeout).

%% @spec fwmkeys(Socket::port(),
%%               Prefix::iolist(),
%%               MaxKeys::integer(),
%%               Timeout::integer()) -> [binary()]
%%
%% @doc Return a number of keys that match a given prefix.
fwmkeys(Socket, Prefix, MaxKeys, Timeout) when is_integer(MaxKeys) ->
    gen_tcp:send(Socket, [<<?FWMKEYS:16>>,
              <<(iolist_size(Prefix)):32>>,
              <<MaxKeys:32>>, Prefix]),
    ?R_2TUPLE(Timeout).

%% @spec addint(Socket::port(),
%%              Key::key(),
%%              Int::integer(),
%%              Timeout::integer()) -> integer() | error()
%%
%% @doc Add an integer value to the existing value of a key, returns new value
addint(Socket, Key, Int, Timeout) when is_integer(Int) ->
    gen_tcp:send(Socket, [<<?ADDINT:16>>, <<(iolist_size(Key)):32>>, <<Int:32>>, Key]),
    ?R_INT32(Timeout).

%% @spec adddouble(Socket::port(),
%%                 Key::key(),
%%                 Double::float(),
%%                 Timeout::integer()) -> {Integral::integer(), Fractional::integer()} | error()
%%
%% @doc Add a float to the existing value of a key, returns new value.
adddouble(Socket, Key, Double, Timeout) when is_float(Double) ->
    IntPart = trunc(Double),
    FracPart = trunc((Double - IntPart) * 1.0e12),
    gen_tcp:send(Socket, [<<?ADDDOUBLE:16>>,
              <<(iolist_size(Key)):32>>,
              <<IntPart:64>>,
              <<FracPart:64>>,
              Key]),
    ?R_SIZE64_SIZE64(Timeout).

%% @spec adddouble(Socket::port(),
%%                 Key::key(),
%%                 Double::float() | IntPart::integer(),
%%                 endian() | FracPart::integer(),
%%                 Timeout::integer()) -> {Integral::integer(), Fractional::integer()} | error()
%%
%% @doc
%% Add a float to the existing value of a key, returns new value. The byte-order
%% of the remote database is specified by the last parameter.
%% @end
adddouble(Socket, Key, Double, big, Timeout) ->
    adddouble(Socket, Key, Double, Timeout);
adddouble(Socket, Key, Double, little, Timeout) when is_float(Double) ->
    IntPart = trunc(Double),
    FracPart = trunc((Double - IntPart) * 1.0e12),
    gen_tcp:send(Socket, [<<?ADDDOUBLE:16>>,
              <<(iolist_size(Key)):32>>,
              <<IntPart:64/little>>,
              <<FracPart:64/little>>,
              Key]),
    ?R_SIZE64_SIZE64(Timeout);
%% Need to stuff this one in here because the arity is 4
adddouble(Socket, Key, IntPart, FracPart, Timeout) when is_integer(IntPart), is_integer(FracPart) ->
    gen_tcp:send(Socket, [<<?ADDDOUBLE:16>>,
              <<(iolist_size(Key)):32>>,
              <<IntPart:64>>,
              <<FracPart:64>>,
              Key]),
    ?R_SIZE64_SIZE64(Timeout).

%% @spec adddouble(Socket::port(),
%%                 Key::key(),
%%                 Integral::integer(),
%%                 Fractional::integer(),
%%                 endian(),
%%                 Timeout::integer()) -> {Integral::integer(), Fractional::integer()} | error()
%%
%% @doc
%% The raw adddouble function for those who need a bit more control on float adds
%% The last parameter determines remote database byte-order.
%% @end
adddouble(Socket, Key, IntPart, FracPart, little, Timeout) when is_integer(IntPart), is_integer(FracPart) ->
    gen_tcp:send(Socket, [<<?ADDDOUBLE:16>>,
              <<(iolist_size(Key)):32>>,
              <<IntPart:64/little>>,
              <<FracPart:64/little>>,
              Key]),
    ?R_SIZE64_SIZE64(Timeout);
adddouble(Socket, Key, IntPart, FracPart, big, Timeout) when is_integer(IntPart), is_integer(FracPart) ->
    gen_tcp:send(Socket, [<<?ADDDOUBLE:16>>,
              <<(iolist_size(Key)):32>>,
              <<IntPart:64>>,
              <<FracPart:64>>,
              Key]),
    ?R_SIZE64_SIZE64(Timeout).

%% @spec sync(Socket::port(), Timeout::integer()) -> ok | error()
%%
%% @doc Call sync() on the remote database
sync(Socket, Timeout) ->
    ?T0(?SYNC),
    ?R_SUCCESS(Timeout).

%% @spec vanish(Socket::port(), Timeout::integer()) -> ok | error()
%%
%% @doc Remove all records from the remote database.
vanish(Socket, Timeout) ->
    ?T0(?VANISH),
    ?R_SUCCESS(Timeout).

%% @spec optimize(Socket::port(),
%%                iolist(),
%%                Timeout::integer()) -> ok | error()
%%
%% @doc Change the remote database tuning parameters.  The second parameter
%%      should be a list of the database tuning parameters that will be applied
%%      at the remote end (e.g. "#bnum=1000000#opts=ld").
%% @end
optimize(Socket, Key, Timeout) ->
    ?T1(?OPTIMIZE), % Using 'Key' so that the macro binds properly...
    ?R_SUCCESS(Timeout).

%% @spec rnum(Socket::port(), Timeout::integer()) -> integer() | error()
%%
%% @doc Get the number of records in the remote database.
rnum(Socket, Timeout) ->
    ?T0(?RNUM),
    ?R_INT64(Timeout).

%% @spec size(Socket::port(), Timeout::integer()) -> integer() | error()
%%
%% @doc Get the size in bytes of the remote database.
size(Socket, Timeout) ->
    ?T0(?SIZE),
    ?R_INT64(Timeout).

%% @spec stat(Socket::port(), Timeout) -> proplist() | error()
%%
%% @doc Get the status string of a remote database.
stat(Socket, Timeout) ->
    ?T0(?STAT),
    StatString = ?R_SIZE_DATA(Timeout),
    case StatString of
    {error, Reason} ->
        {error, Reason};
    GoodStat ->
        stat_to_proplist(GoodStat)
    end.

stat_to_proplist(StatBin) ->
    stat_to_proplist(string:tokens(binary_to_list(StatBin), "\n\t"), []).

stat_to_proplist([], Acc) ->
    Acc;
stat_to_proplist([H1, H2 | T], Acc) ->
    stat_to_proplist(T, [{list_to_atom(H1), H2} | Acc]).

%% @spec copy(Socket::port(),
%%            iolist(),
%%            Timeout::integer()) -> ok | error()
%%
%% @doc Make a copy of the database file of the remote database.
copy(Socket, Key, Timeout) when is_binary(Key) ->
    ?T1(?COPY), % Using 'Key' so that the macro binds properly...
    ?R_SUCCESS(Timeout).

%% @spec restore(Socket::port(),
%%               PathName::iolist(),
%%               TimeStamp::integer,
%%               Timeout::integer()) -> ok | error()
%%
%% @doc Restore the database to a particular point in time from the update log.
restore(Socket, PathName, TimeStamp, Timeout) ->
    gen_tcp:send(Socket, [<<?RESTORE:16>>,
              <<(iolist_size(PathName)):32>>,
              <<TimeStamp:64>>,
              <<0:32>>,
              PathName]),
    ?R_SUCCESS(Timeout).

%% @spec restore_with_check(Socket::port(),
%%                          PathName::iolist(),
%%                          TimeStamp::integer,
%%                          Timeout::integer()) -> ok | error()
%%
%% @doc Restore the database to a particular point in time from the update log and
%%      perform a consistency check
%% @end
restore_with_check(Socket, PathName, TimeStamp, Timeout) ->
    gen_tcp:send(Socket, [<<?RESTORE:16>>,
              <<(iolist_size(PathName)):32>>,
              <<TimeStamp:64>>,
              <<1:32>>,
              PathName]),
    ?R_SUCCESS(Timeout).

%% @spec setmst(Socket::port(),
%%              HostName::iolist(),
%%              Port::integer,
%%              Timeout::integer()) -> ok | error()
%%
%% @doc Set the replication master of a remote database server.
setmst(Socket, HostName, Port, Timeout) when is_integer(Port) ->
    gen_tcp:send(Socket, [<<?SETMST:16>>,
              <<(iolist_size(HostName)):32>>,
              <<Port:32>>, HostName]),
    ?R_SUCCESS(Timeout).

%% @spec repl(Socket::port(),
%%            TimeStamp::integer(),
%%            Sid::integer(),
%%            Timeout::integer())
%%
%% @doc Initiate master->slave replication to the server id provided starting at a
%%      given timestamp.
%% repl(Socket, TimeStamp, Sid) ->
%%     gen_tcp:send(Socket, [<<?SETMST:16>>,
%%               <<TimeStamp:64>>,
%%               <<Sid:32>>]),
%%     ?R_SUCCESS(Timeout).

%% @spec misc(Socket::port(),
%%            Func::iolist(),
%%            Args::arglist(),
%%            Timeout::integer()) -> [binary()] | error()
%% @type arglist() = [iolist()]
%%
%% @doc
%% Tyrant misc() call that writes to the update logs
%% All database types support putlist, outlist, and getlist.
%%    putlist -> store records, Args is list of sequential keys and values, returns []
%%    outlist -> remove records, Args is list of keys, returns []
%%    getlist -> retrieve records, args is list of keys, returns list of values
%% Table database supports setindex, search, and genuid.
%%    setindex -> set the column index, Arg is name of col and type of col data, returns success val
%%    search -> run a search on the columns, returns list of values
%%    genuid -> generate unique ID number, returns integer
%% @end
misc(Socket, Func, Args, Timeout) when length(Args) > 0 ->
    gen_tcp:send(Socket, [<<?MISC:16>>,
              <<(iolist_size(Func)):32>>, <<0:32>>,
              <<(length(Args)):32>>,
              Func,
              misc_arg_encode(big, Args)
             ]),
    ?R_2TUPLE(Timeout);
misc(Socket, Func, _Args, Timeout) ->
    gen_tcp:send(Socket, [<<?MISC:16>>,
              <<(iolist_size(Func)):32>>, <<0:32>>,
              <<0:32>>,
              Func]),
    ?R_2TUPLE(Timeout).

%% @spec misc(Socket::port(),
%%            Func::iolist(),
%%            Args::arglist(),
%%            endian(),
%%            Timeout::integer()) -> [binary()] | error()
%% @type arglist() = [iolist()]
%%
%% @doc
%% Tyrant misc() call that writes to the update logs with a specific
%% byte-order for encoding
%% All database types support putlist, outlist, and getlist.
%%    putlist -> store records, Args is list of sequential keys and values, returns []
%%    outlist -> remove records, Args is list of keys, returns []
%%    getlist -> retrieve records, args is list of keys, returns list of values
%% Table database supports setindex, search, and genuid.
%%    setindex -> set the column index, Arg is name of col and type of col data, returns success val
%%    search -> run a search on the columns, returns list of values
%%    genuid -> generate unique ID number, returns integer
%% @end
misc(Socket, Func, Args, big, Timeout) ->
    misc(Socket, Func, Args, Timeout);
misc(Socket, Func, Args, little, Timeout) when length(Args) > 0 ->
    gen_tcp:send(Socket, [<<?MISC:16>>,
              <<(iolist_size(Func)):32>>, <<0:32>>,
              <<(length(Args)):32>>,
              Func,
              misc_arg_encode(little, Args)
             ]),
    ?R_2TUPLE(Timeout);
misc(Socket, Func, Args, _Endian, Timeout) ->
    misc(Socket, Func, Args, Timeout).

%% @spec misc_no_update(Socket::port(),
%%                      Func::iolist(),
%%                      Args::arglist(),
%%                      Timeout::integer()) -> [binary()] | error()
%% @type arglist() = [iolist()]
%%
%% @doc Tyrant misc() call that does not write to the update logs
misc_no_update(Socket, Func, Args, Timeout) when length(Args) > 0 ->
    gen_tcp:send(Socket, [<<?MISC:16>>,
              <<(iolist_size(Func)):32>>, <<1:32>>,
              <<(length(Args)):32>>,
              Func,
              misc_arg_encode(big, Args)
             ]),
    ?R_2TUPLE(Timeout);
misc_no_update(Socket, Func, _Args, Timeout) ->
    gen_tcp:send(Socket, [<<?MISC:16>>,
              <<(iolist_size(Func)):32>>, <<1:32>>,
              <<0:32>>,
              Func]),
    ?R_2TUPLE(Timeout).

%% @spec misc_no_update(Socket::port(),
%%                      Func::iolist(),
%%                      Args::arglist(),
%%                      endian(),
%%                      Timeout::integer()) -> [binary()] | error()
%% @type arglist() = [iolist()]
%%
%% @doc Tyrant misc() call that does not write to the update logs.
misc_no_update(Socket, Func, Args, big, Timeout) ->
    misc_no_update(Socket, Func, Args, Timeout);
misc_no_update(Socket, Func, Args, little, Timeout) when length(Args) > 0 ->
    gen_tcp:send(Socket, [<<?MISC:16>>,
              <<(iolist_size(Func)):32>>, <<1:32>>,
              <<(length(Args)):32>>,
              Func,
              misc_arg_encode(little, Args)
             ]),
    ?R_2TUPLE(Timeout);
misc_no_update(Socket, Func, Args, little, Timeout) ->
    misc_no_update(Socket, Func, Args, Timeout).

%% Encoding helper for misc() that tries to keep integers in the
%% proper form for the remote database.
misc_arg_encode(Endian, ArgList) ->
    misc_arg_encode(Endian, ArgList, []).

misc_arg_encode(_Endian, [], ArgList) ->
    lists:reverse(ArgList);
misc_arg_encode(big, [Arg | Tail], ArgList) when is_integer(Arg), Arg < 4294967296 ->
    misc_arg_encode(big, Tail, [[<<4:32>>, <<Arg:32>>] | ArgList]);
misc_arg_encode(little, [Arg | Tail], ArgList) when is_integer(Arg), Arg < 4294967296 ->
    misc_arg_encode(little, Tail, [[<<4:32>>, <<Arg:32/little>>] | ArgList]);
misc_arg_encode(big, [Arg | Tail], ArgList) when is_float(Arg) ->
    misc_arg_encode(big, Tail, [[<<8:32>>, <<Arg:64/float>>] | ArgList]);
misc_arg_encode(little, [Arg | Tail], ArgList) when is_float(Arg) ->
    misc_arg_encode(little, Tail, [[<<8:32>>, <<Arg:64/float-little>>] | ArgList]);
misc_arg_encode(Endian, [Arg | Tail], ArgList) ->
    misc_arg_encode(Endian, Tail, [[<<(iolist_size(Arg)):32>>, Arg] | ArgList]).

%% @spec ext(Socket::port(),
%%            Func::iolist(),
%%            Opts::proplist(),
%%            Key::iolist(),
%%            Value::iolist(),
%%            Timeout::integer()) -> ok | error()
%%
%% @doc Call a function defined by the Tyrant script language extensions.
ext(Socket, Func, Opts, Key, Value, Timeout) ->
    %% TODO: Opts needs to be parsed.  Probably as a proplist [record_lock, global_lock, neither...]
    gen_tcp:send(Socket, [<<?EXT:16>>, <<(iolist_size(Func)):32>>, <<Opts:32>>,
              <<(iolist_size(Key)):32>>, <<(iolist_size(Value)):32>>,
              Func, Key, Value]),
    ?R_SIZE_DATA(Timeout).

%%====================================================================
%% Handle response from the server
%%====================================================================

%% @spec (ResponseHandler::function()) -> ok | error() | term()
%%
%% @private Get the response from a Tyrant request, parse it, and return the
%% data or error code.
%% @end
tyrant_response(ResponseHandler, Timeout) ->
    receive
    {tcp, _, <<1:8, _Rest/binary>>} ->
        {error, invalid_operation};
    {tcp, _, <<2:8, _Rest/binary>>} ->
        {error, no_host_found};
    {tcp, _, <<3:8, _Rest/binary>>} ->
        {error, connection_refused};
    {tcp, _, <<4:8, _Rest/binary>>} ->
        {error, send_error};
    {tcp, _, <<5:8, _Rest/binary>>} ->
        {error, recv_error};
    {tcp, _, <<6:8, _Rest/binary>>} ->
        {error, existing_record};
    {tcp, _, <<7:8, _Rest/binary>>} ->
        {error, no_such_record};
        {tcp, _, <<ErrorCode:8, _Rest/binary>>} when ErrorCode =/= 0 ->
        {error, ErrorCode};
        {tcp_closed, _} ->
        {error, conn_closed};
        {tcp_error, _, _} ->
        {error, conn_error};
        Data ->
        ResponseHandler(Data, Timeout)
    after Timeout ->
        {error, timeout}
    end.

%% receive 8-bit success flag
recv_success({tcp, _, <<0:8>>}, _) ->
    ok;

%% TODO: find out why principe_table:search enters this clause
%% as table becomes large. {Update from Jim to Jim: this was probably
%% due to data chunks that was recently fixed due to the bug Bhasker
%% spotted -- try yanking this and checking again...}
recv_success({tcp, _, _}, _)->
   ok.

%% receive 8-bit success flag + 32-bit int (endianness determined by remote database)
recv_size({tcp, _, <<0:8, ValSize:32>>}, _) ->
    ValSize;
recv_size({tcp, _, <<0:8, SmallBin/binary>>}, Timeout) ->
    {ValSize, _Rest} = recv_until(SmallBin, 4, Timeout),
    ValSize.

%% receive 8-bit success flag + 64-bit int
recv_size64({tcp, _, <<0:8, ValSize:64>>}, _) ->
    ValSize;
recv_size64({tcp, _, <<0:8, SmallBin/binary>>}, Timeout) ->
    {ValSize, _Rest} = recv_until(SmallBin, 8, Timeout),
    ValSize.

%% receive 8-bit success flag + 64-bit int + 64-bit int
recv_size64_size64({tcp, _, <<0:8, V1:64, V2:64>>}, _) ->
    {V1, V2};
recv_size64_size64({tcp, _, <<0:8, SmallBin/binary>>}, Timeout) ->
    %% Did not get a full chunk of data, so get more.
    {V1, V2Bin} = recv_until(SmallBin, 8, Timeout),
    {V2, _Rest} = recv_until(V2Bin, 8, Timeout),
    {V1, V2}.

%% receive 8-bit success flag + length1 + data1
recv_size_data({tcp, _, <<0:8, Size:32, Data/binary>>}, _)
    when byte_size(Data) >= Size ->
    <<Value:Size/binary, _Rest/binary>> = Data,
    Value;
recv_size_data({tcp, _, <<0:8, Size:32, Data/binary>>}, Timeout) ->
    %% Have at least the size, need to pull more for the data payload.
    {Value, _Rest} = recv_until(Data, Size, Timeout),
    Value;
recv_size_data({tcp, _, <<0:8, SmallBin/binary>>}, Timeout) ->
    %% Did not even get the size, pull size, then pull data.
    {<<Size:32>>, TailBin} = recv_until(SmallBin, 4, Timeout),
    {Value, _Rest} = recv_until(TailBin, Size, Timeout),
    Value.

%% receive 8-bit success flag + count + (length1, length2, data1, data2)*count
recv_count_4tuple({tcp, _, <<0:8, 0:32, _Rest/binary>>}, _) ->
    [];
recv_count_4tuple({tcp, _, <<0:8, Cnt:32, Rest/binary>>}, Timeout) ->
    {KeyVals, _} = lists:foldl(
             %% This fold should grab/process one value per iteration.
             fun(_IterCount, {Vals, <<KeySize:32, ValSize:32, Bin/binary>>}) ->
                 %% We have at least the key and value sizes, so make recv_until's
                 %% job easier and just ask it to split/pull the data elements.
                 {Key, ValBin} = recv_until(Bin, KeySize, Timeout),
                 {Value, RestBin} = recv_until(ValBin, ValSize, Timeout),
                 {[{Key, Value}] ++ Vals, RestBin};
            (_IterCount, {Vals, <<SmallBin/binary>>}) ->
                 %% Not enough in SmallBin to even get the sizes, read the key and
                 %% value sizes then read enough to get the new data elements.
                 {<<KeySize:32>>, ValSizeAndDataBin} = recv_until(SmallBin, 4, Timeout),
                 {<<ValSize:32>>, DataBin} = recv_until(ValSizeAndDataBin, 4, Timeout),
                 {Key, ValBin} = recv_until(DataBin, KeySize, Timeout),
                 {Value, RestBin} = recv_until(ValBin, ValSize, Timeout),
                 {[{Key, Value}] ++ Vals, RestBin}
             end,
             {[], Rest}, lists:seq(1, Cnt)
            ),
    lists:reverse(KeyVals).

%% receive 8-bit success flag + count + (length1, data1)*count
recv_count_2tuple({tcp, _, <<0:8, 0:32, _Rest/binary>>}, _) ->
    [];
recv_count_2tuple({tcp, _, <<0:8, Cnt:32, Rest/binary>>}, Timeout) ->
    {Data, _} = lists:foldl(
          %% This fold should grab/process one value per iteration.
          fun(_IterCount, {Vals, <<Size:32, Bin/binary>>}) ->
              %% We have at least the key sizes, so make recv_until's job
              %% easier and just ask it to split/pull the data element.
              {NewVal, RestBin} = recv_until(Bin, Size, Timeout),
              {[NewVal] ++ Vals, RestBin};
             (_IterCount, {Vals, <<SmallBin/binary>>}) ->
              %% Not enough in SmallBin to even get the size, read the size then read
              %% enough to get the new data element.
              {<<Size:32>>, RestBin} = recv_until(SmallBin, 4, Timeout),
              {NewVal, SecondRestBin} = recv_until(RestBin, Size, Timeout),
              {[NewVal] ++ Vals, SecondRestBin}
          end,
          {[], Rest}, lists:seq(1, Cnt)
         ),
    lists:reverse(Data).

%% receive length-delimited data that may require multiple pulls from the socket
recv_until(Bin, ReqLength, Timeout) when byte_size(Bin) < ReqLength ->
    receive
        {tcp, _, Data} ->
            Combined = <<Bin/binary, Data/binary>>,
            recv_until(Combined, ReqLength, Timeout);
        {tcp_closed, _} ->
        {error, conn_closed};
    {error, closed} ->
        {error, conn_closed}
    after Timeout ->
        {error, timeout}
    end;
recv_until(Bin, ReqLength, _) ->
    <<Required:ReqLength/binary, Rest/binary>> = Bin,
    {Required, Rest}.

%% Some standard types for edoc
%%
%% @type key() = iolist()
%% @type value() = iolist()
%% @type value_or_num() = iolist() | integer() | float()
%% @type keylist() = [key()]
%% @type error() = {error, term()}
%% @type endian() = little | big

%% EUnit tests
%%
-ifdef(EUNIT).
test_setup() ->
    {ok, Socket} = ?MODULE:connect(),
    ok = ?MODULE:vanish(Socket),
    Socket.

get_random_count() ->
    get_random_count(1000).

get_random_count(Max) ->
    crypto:start(),
    {A1,A2,A3} = now(),
    random:seed(A1, A2, A3),
    round(Max * random:uniform()).

put_get_test() ->
    Socket = test_setup(),
    ?assert(?MODULE:put(Socket, "put_get1", "testval") =:= ok),
    ?assert(?MODULE:put(Socket, <<"put_get2">>, <<32,145,56,0,14>>) =:= ok),
    ?assert(?MODULE:get(Socket, <<"put_get1">>) =:= <<"testval">>),
    ?assert(?MODULE:get(Socket, "put_get2") =:= <<32, 145, 56, 0, 14>>),
    case proplists:get_value(bigend, ?MODULE:stat(Socket)) of
    "0" ->
        ?assert(?MODULE:put(Socket, <<"put_get3">>, 42, little) =:= ok),
        ?assert(?MODULE:get(Socket, <<"put_get3">>) =:= <<42:32/little>>);
    "1" ->
        ?assert(?MODULE:put(Socket, <<"put_get3">>, 42, big) =:= ok),
        ?assert(?MODULE:get(Socket, <<"put_get3">>) =:= <<42:32>>)
    end.

put_get_random_test() ->
    Socket = test_setup(),
    ElementCount = get_random_count(),
    PutVals = lists:foldl(fun(_Seq, Acc) ->
                  KeySize = random:uniform(1024),
                  Key = crypto:rand_bytes(KeySize),
                  ValSize = random:uniform(65536),
                  Val = crypto:rand_bytes(ValSize),
                  ok = ?MODULE:put(Socket, Key, Val),
                  [{Key, Val} | Acc]
              end, [], lists:seq(1, ElementCount)),
    lists:foreach(fun({K, V}) ->
              ?assert(?MODULE:get(Socket, K) =:= V)
          end, PutVals),
    ok.

putkeep_test() ->
    Socket = test_setup(),
    ok = ?MODULE:put(Socket, <<"test">>, <<"foo">>),
    ?assert(?MODULE:get(Socket, <<"test">>) =:= <<"foo">>),
    ?assertMatch({error, _}, ?MODULE:putkeep(Socket, <<"test">>, <<"bar">>)),
    ?assert(?MODULE:get(Socket, <<"test">>) =:= <<"foo">>), % no effect if key already exists before putkeep
    ok = ?MODULE:putkeep(Socket, <<"another">>, <<"baz">>),
    ?assert(?MODULE:get(Socket, <<"another">>) =:= <<"baz">>), % puts the key if key does not exist already
    ok.

putcat_test() ->
    Socket = test_setup(),
    ok = ?MODULE:put(Socket, <<"putcat1">>, <<"foo">>),
    % append "bar" to the end
    ok = ?MODULE:putcat(Socket, <<"putcat1">>, <<"bar">>),
    ?assert(?MODULE:get(Socket, <<"putcat1">>) =:= <<"foobar">>),
    ok.

putshl_test() ->
    Socket = test_setup(),
    ok = ?MODULE:put(Socket, <<"putshl">>, <<"foo">>),
    % append "bar" to the end and shift to the left to retain the width of "4"
    ok = ?MODULE:putshl(Socket, <<"putshl">>, <<"bar">>, 4),
    ?assert(?MODULE:get(Socket, <<"putshl">>) =:= <<"obar">>),
    ok.

putnr_test() ->
    Socket = test_setup(),
    ?MODULE:putnr(Socket, <<"putnr1">>, <<"no reply">>),
    ?assert(?MODULE:get(Socket, <<"putnr1">>) =:= <<"no reply">>),
    ok.

out_test() ->
    Socket = test_setup(),
    ok = ?MODULE:put(Socket, <<"out1">>, <<"to remove">>),
    ?assert(?MODULE:get(Socket, <<"out1">>) =:= <<"to remove">>),
    ok = ?MODULE:out(Socket, <<"out1">>),
    ?assertMatch({error, _}, ?MODULE:get(Socket, <<"out1">>)),
    ok.

mget_test() ->
    Socket = test_setup(),
    ok = ?MODULE:put(Socket, <<"mget1">>, <<"alice">>),
    ok = ?MODULE:put(Socket, <<"mget2">>, <<"bob">>),
    ok = ?MODULE:put(Socket, <<"mget3">>, <<"carol">>),
    ok = ?MODULE:put(Socket, <<"mget4">>, <<"trent">>),
    ?assert(?MODULE:mget(Socket, [<<"mget1">>, <<"mget2">>,
                   <<"mget3">>, <<"mget4">>]) =:=
        [{<<"mget1">>, <<"alice">>},
         {<<"mget2">>, <<"bob">>},
         {<<"mget3">>, <<"carol">>},
         {<<"mget4">>, <<"trent">>}]),
    ok.

vsiz_test() ->
    Socket = test_setup(),
    ok = ?MODULE:put(Socket, <<"vsiz1">>, <<"vsiz test">>),
    ?assert(?MODULE:vsiz(Socket, <<"vsiz1">>) =:= 9),
    ok.

vanish_test() ->
    Socket = test_setup(),
    ok = ?MODULE:put(Socket, <<"vanish1">>, <<"going away">>),
    ok = ?MODULE:vanish(Socket),
    ?assertMatch({error, _}, ?MODULE:get(Socket, <<"vanish1">>)),
    ok.

iter_test() ->
    Socket = test_setup(),
    ok = ?MODULE:put(Socket, <<"a">>, <<"first">>),
    ok = ?MODULE:iterinit(Socket),
    <<"a">> = ?MODULE:iternext(Socket), % "a" should be the first key
    % Now to test a bit of real iteration
    ok = ?MODULE:put(Socket, <<"b">>, <<"second">>),
    ok = ?MODULE:put(Socket, <<"c">>, <<"third">>),
    ok = ?MODULE:iterinit(Socket),
    One = ?MODULE:iternext(Socket),
    Two = ?MODULE:iternext(Socket),
    Three = ?MODULE:iternext(Socket),
    ?assertMatch({error, _}, ?MODULE:iternext(Socket)),
    ?assertMatch([<<"a">>, <<"b">>, <<"c">>], lists:sort([One, Two, Three])),
    ok.

fwmkeys_test() ->
    Socket = test_setup(),
    ok = ?MODULE:put(Socket, <<"fwmkeys1">>, <<"1">>),
    ok = ?MODULE:put(Socket, <<"fwmkeys2">>, <<"2">>),
    ok = ?MODULE:put(Socket, <<"fwmkeys3">>, <<"3">>),
    ok = ?MODULE:put(Socket, <<"fwmkeys4">>, <<"4">>),
    Keys1 = ?MODULE:fwmkeys(Socket, <<"fwmkeys">>, 4),
    ?assert(length(Keys1) =:= 4),
    ?assert(lists:member(<<"fwmkeys1">>, Keys1)),
    ?assert(lists:member(<<"fwmkeys2">>, Keys1)),
    ?assert(lists:member(<<"fwmkeys3">>, Keys1)),
    ?assert(lists:member(<<"fwmkeys4">>, Keys1)),
    Keys2 = ?MODULE:fwmkeys(Socket, <<"fwmkeys">>, 2),
    ?assert(length(Keys2) =:= 2),
    ok.

addint_test() ->
    Socket = test_setup(),
    case proplists:get_value(bigend, ?MODULE:stat(Socket)) of
    "0" ->
        ?MODULE:put(Socket, <<"addint1">>, 100, little);
    "1" ->
        ?MODULE:put(Socket, <<"addint1">>, 100)
    end,
    ?assert(?MODULE:addint(Socket, <<"addint1">>, 20) =:= 120),
    ok.

sync_test() ->
    Socket = test_setup(),
    ok = ?MODULE:sync(Socket),
    ok.

rnum_test() ->
    Socket = test_setup(),
    ok = ?MODULE:put(Socket, <<"rnum1">>, <<"foo">>),
    ok = ?MODULE:put(Socket, <<"rnum2">>, <<"foo">>),
    ?assert(?MODULE:rnum(Socket) =:= 2),
    ok = ?MODULE:vanish(Socket),
    ?assert(?MODULE:rnum(Socket) =:= 0),
    ok.

size_test() ->
    Socket = test_setup(),
    OldSize = ?MODULE:size(Socket),
    ok = ?MODULE:put(Socket, <<"size">>, <<"foo">>),
    NewSize = ?MODULE:size(Socket),
    ?assert(NewSize > OldSize),
    ok.

stat_test() ->
    Socket = test_setup(),
    ?MODULE:stat(Socket).

optimize_test() ->
    Socket = test_setup(),
    ok = ?MODULE:optimize(Socket, "#bnum=1000000#opts=ld").

misc_test() ->
    Socket = test_setup(),
    [] = ?MODULE:misc(Socket, "putlist",
               ["key1", "value1",
                "key2", "value2",
                "key3", "value3",
                "key4", "value4"]),
    ?assert(?MODULE:rnum(Socket) =:= 4),
    ?assert(?MODULE:get(Socket, "key1") =:= <<"value1">>),
    [] = ?MODULE:misc(Socket, "outlist",
               ["key1", "key2", "key3"]),
    ?assert(?MODULE:rnum(Socket) =:= 1),
    ?MODULE:put(Socket, "key5", "value5"),
    GetlistOut = ?MODULE:misc(Socket, "getlist", ["key4", "key5"]),
    ?assert(lists:all(fun (K) -> lists:member(K, GetlistOut) end,
              [<<"key4">>, <<"value4">>,
               <<"key5">>, <<"value5">>])),
    ok.
-endif.
