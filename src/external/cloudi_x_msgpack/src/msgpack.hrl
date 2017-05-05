%%
%% MessagePack for Erlang
%%
%% Copyright (C) 2009-2016 UENISHI Kota
%%
%%    Licensed under the Apache License, Version 2.0 (the "License");
%%    you may not use this file except in compliance with the License.
%%    You may obtain a copy of the License at
%%
%%        http://www.apache.org/licenses/LICENSE-2.0
%%
%%    Unless required by applicable law or agreed to in writing, software
%%    distributed under the License is distributed on an "AS IS" BASIS,
%%    WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%    See the License for the specific language governing permissions and
%%    limitations under the License.
%%

-type msgpack_map_jsx() :: [{msgpack_term(), msgpack_term()}] | [{}].
-type msgpack_map_jiffy() :: {[{msgpack_term(), msgpack_term()}]}.
-type msgpack_map() :: msgpack_map_jsx() | msgpack_map_jiffy() | map().

-type msgpack_map_unpacker() ::
        fun((binary(), non_neg_integer(), msgpack:opt_record()) ->
                   {msgpack_map(), binary()} | no_return() ).

%% Erlang representation of msgpack data.
-type msgpack_term() :: [msgpack_term()] | msgpack_map() |
                        integer() | float() | boolean() | binary() | string() | {string, string()}.

-type format_type() :: jsx|jiffy|map.

-define(DEFAULT_MAP_FORMAT, map).
-define(DEFAULT_MAP_UNPACKER_FUN, fun msgpack_unpacker:unpack_map/3).

-record(options_v4, {
          spec = new :: new | old,
          allow_atom = pack  :: none | pack, %% allows atom when packing
          known_atoms = [] :: [atom()],
          unpack_str = as_list :: as_binary | as_list | as_tagged_list,
          validate_string = false :: boolean(),
          pack_str = from_list :: from_binary | from_list | from_tagged_list | none,
          map_format = ?DEFAULT_MAP_FORMAT :: format_type(),
          map_unpack_fun = ?DEFAULT_MAP_UNPACKER_FUN :: msgpack_map_unpacker(),
          ext_packer = undefined   :: msgpack:ext_packer()   | undefined,
          ext_unpacker = undefined :: msgpack:ext_unpacker() | undefined,
          original_list = []       :: msgpack:options()
         }).

-define(OPTION, #options_v4).
