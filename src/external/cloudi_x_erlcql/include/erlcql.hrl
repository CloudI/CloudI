%% Copyright (c) 2013-2014 Krzysztof Rutka
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
%% IN THE SOFTWARE.

%% @author Krzysztof Rutka <krzysztof.rutka@gmail.com>

-define(APP, erlcql).
-define(VERSION, 1).

%% Directions
-define(REQUEST, 0).
-define(RESPONSE, 1).

%% Encode/decode types
-define(INT, 4/big-signed-integer-unit:8).
-define(SHORT, 2/big-unsigned-integer-unit:8).

-define(int(X), <<X:?INT>>).
-define(short(X), <<X:?SHORT>>).

%% Parser
-record(parser, {
          buffer = <<>> :: binary()
         }).
-type parser() :: #parser{}.

-type event_fun() :: fun((event()) -> any()).

%%-----------------------------------------------------------------------------
%% Types
%%-----------------------------------------------------------------------------

-type proplist() :: proplists:proplist().
-type socket() :: inet:socket().
-type ets() :: ets:tid().

-type request() :: {Opcode :: request_opcode(), Data :: iolist()}.
-type request_opcode() :: startup
                        | credentials
                        | 'query'
                        | prepare
                        | execute
                        | options
                        | register.

-type response() :: cql_error()
                  | ready()
                  | authenticate()
                  | supported()
                  | event_res()
                  | result().

-type cql_error() :: {error, {Code :: error_code(),
                              Message :: bitstring(),
                              Extra :: term()}}.
-type ready() :: ready.
-type authenticate() :: {authenticate, AuthClass :: bitstring()}.
-type supported() :: {ok, [{Name :: bitstring(), [Value :: bitstring()]}]}.
-type event() :: {Kind :: atom(), Type :: atom(), Extra :: term()}.
-type event_res() :: {event, event()}.
-type result() :: void()
                | rows()
                | set_keyspace()
                | prepared()
                | schema_change().

-type void() :: {ok, void}.
-type rows() :: {ok, {Rows :: [[type()]], Cols :: column_specs()}}.
-type set_keyspace() :: {ok, Keyspace :: bitstring()}.
-type prepared() :: {ok, PreparedQueryId :: binary()}.
-type schema_change() :: {ok, created | updated | dropped}.

-type response_opcode() :: error
                         | ready
                         | authenticate
                         | supported
                         | result
                         | event.

-type consistency() :: any
                     | one
                     | two
                     | three
                     | quorum
                     | all
                     | local_quorum
                     | each_quorum.

-type event_type() :: topology_change
                    | status_change
                    | schema_change.

-type error_code() :: server_error
                    | protocol_error
                    | bad_credentials
                    | unavailable_exception
                    | overloaded
                    | is_bootstrapping
                    | truncate_error
                    | write_timeout
                    | read_timeout
                    | syntax_error
                    | unauthorized
                    | invalid
                    | config_error
                    | already_exists
                    | unprepared.

-type result_kind() :: void
                     | rows
                     | set_keyspace
                     | prepared
                     | schema_change.

-type option_id() :: custom
                   | ascii
                   | bigint
                   | blob
                   | boolean
                   | counter
                   | decimal
                   | double
                   | float
                   | int
                   | timestamp
                   | uuid
                   | varchar
                   | varint
                   | timeuuid
                   | inet
                   | list
                   | map
                   | set.

-type option() :: option_id()
                | {list, option_id()}
                | {map, option_id(), option_id()}
                | {set, option_id()}
                | {custom, bitstring()}.

-type compression() :: snappy
                     | lz4
                     | false.

-type column_specs() :: [{Name :: bitstring(), Type :: option()}].

-type inet() :: {inet:ip_address(), inet:port_number()}.

%% Types ----------------------------------------------------------------------

-type uuid() :: bitstring().

-type native_type() :: binary()
                     | bitstring()
                     | boolean()
                     | integer()
                     | float()
                     | uuid()
                     | inet:ip_address().

-type erlcql_list() :: [native_type()].
-type erlcql_set() :: [native_type()].
-type erlcql_map() :: [{native_type(), native_type()}].

-type collection_type() :: erlcql_list()
                         | erlcql_set()
                         | erlcql_map().

-type type() :: native_type()
              | collection_type().

-type values() :: [binary() | {option(), type()}].

%%-----------------------------------------------------------------------------
%% Logging macros
%%-----------------------------------------------------------------------------

-ifdef(ERLCQL_NO_LOGS).
-define(ERROR(_Format, _Data), ok).
-define(EMERGENCY(_Format, _Data), ok).
-define(ALERT(_Format, _Data), ok).
-define(CRITICAL(_Format, _Data), ok).
-define(WARNING(_Format, _Data), ok).
-define(INFO(_Format, _Data), ok).
-define(NOTICE(_Format, _Data), ok).
-define(DEBUG(_Format, _Data), ok).
-else.
-ifdef(ERLCQL_LAGER).
-compile({parse_transform, lager_transform}).
-define(EMERGENCY(Format, Data), lager:emergency(Format, Data)).
-define(ALERT(Format, Data), lager:alert(Format, Data)).
-define(CRITICAL(Format, Data), lager:critical(Format, Data)).
-define(ERROR(Format, Data), lager:error(Format, Data)).
-define(WARNING(Format, Data), lager:warning(Format, Data)).
-define(NOTICE(Format, Data), lager:notice(Format, Data)).
-define(INFO(Format, Data), lager:info(Format, Data)).
-define(DEBUG(Format, Data), lager:debug(Format, Data)).
-else.
-define(ERROR(Format, Data), error_logger:error_msg(Format ++ "~n", Data)).
-define(EMERGENCY(Format, Data), ?ERROR(Format, Data)).
-define(ALERT(Format, Data), ?ERROR(Format, Data)).
-define(CRITICAL(Format, Data), ?ERROR(Format, Data)).
-define(WARNING(Format, Data), error_logger:warning_msg(Format ++ "~n", Data)).
-define(INFO(Format, Data), error_logger:info_msg(Format ++ "~n", Data)).
-define(NOTICE(Format, Data), ?INFO(Format, Data)).
-define(DEBUG(Format, Data), ?INFO(Format, Data)).
-endif.
-endif.

-define(EMERGENCY(Format), ?EMERGENCY(Format, [])).
-define(ALERT(Format), ?ALERT(Format, [])).
-define(CRITICAL(Format), ?CRITICAL(Format, [])).
-define(ERROR(Format), ?ERROR(Format, [])).
-define(WARNING(Format), ?WARNING(Format, [])).
-define(NOTICE(Format), ?NOTICE(Format, [])).
-define(INFO(Format), ?INFO(Format, [])).
-define(DEBUG(Format), ?DEBUG(Format, [])).
