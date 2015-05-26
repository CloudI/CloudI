%%
%% MessagePack for Erlang
%%
%% Copyright (C) 2009-2013 UENISHI Kota
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

-ifdef(without_map).
-type msgpack_map() :: msgpack_map_jsx() | msgpack_map_jiffy().
-else.
-type msgpack_map() :: msgpack_map_jsx() | msgpack_map_jiffy() | map().
-endif.

-type msgpack_map_unpacker() ::
        fun((binary(), non_neg_integer(), msgpack_option()) ->
                   {msgpack_map(), binary()} | no_return() ).

%% Erlang representation of msgpack data.
-type msgpack_term() :: [msgpack_term()] | msgpack_map() |
                        integer() | float() | binary().

%% @doc ext_packer that packs only tuples with length > 2
-type msgpack_ext_packer()   :: fun((tuple(), msgpack:options()) ->
                                           {ok, {Type::byte(), Data::binary()}} |
                                           {error, any()}).
-type msgpack_ext_unpacker() ::
        fun((byte(), binary(), msgpack:options()) ->
                   {ok, msgpack_term()} | {error, any()})
      | fun((byte(), binary()) ->
                   {ok, msgpack_term()} | {error, any()}).

-type format_type() :: jsx|jiffy|map.

-type msgpack_list_options() :: [
                                 {format, format_type()} |
                                 jsx | jiffy |
                                 {allow_atom, none|pack} |
                                 {enable_str, boolean()} |
                                 {ext, {msgpack_ext_packer(),msgpack_ext_unpacker()} | module()}
                                ].

-record(options_v1, {
          interface = jiffy :: jiffy | jsx,
          map_unpack_fun = fun msgpack_unpacker:unpack_map_jiffy/3 ::
                                 msgpack_map_unpacker(),
          impl = erlang     :: erlang | nif
         }).

-record(options_v2, {
          interface = jiffy :: jiffy | jsx,
          map_unpack_fun = fun msgpack_unpacker:unpack_map_jiffy/3 ::
                                 msgpack_map_unpacker(),
          impl = erlang      :: erlang | nif,
          allow_atom = none  :: none | pack, %% allows atom when packing
          enable_str = false :: boolean(), %% true for new spec
          ext_packer = undefined   :: msgpack_ext_packer()   | undefined,
          ext_unpacker = undefined :: msgpack_ext_unpacker() | undefined,
          original_list = []       :: msgpack_list_options()
         }).

-ifdef(without_map).

-define(OPTION, #options_v2).
-type msgpack_option() :: #options_v2{}.

-else.
-record(options_v3, {
          interface = jiffy :: format_type(),
          map_unpack_fun = fun msgpack_unpacker:unpack_map_jiffy/3 ::
                                 msgpack_map_unpacker(),
          impl = erlang      :: erlang | nif,
          allow_atom = none  :: none | pack, %% allows atom when packing
          enable_str = false :: boolean(), %% true for new spec
          ext_packer = undefined   :: msgpack_ext_packer()   | undefined,
          ext_unpacker = undefined :: msgpack_ext_unpacker() | undefined,
          original_list = []       :: msgpack_list_options()
         }).

-define(OPTION, #options_v3).
-type msgpack_option() :: #options_v3{}.

-endif.

