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
-module(msgpack_ext).

%%
%% @doc identity constraint
%% {ok, {Type, Data} = msgpack_ext_module:pack_ext(Tuple, [{ext, msgpack_ext_module}]),
%% {ok, Tuple} = msgpack_ext_module:unpack_ext(Type, Data)
%% 
-callback pack_ext(any(), msgpack:options()) ->
    {ok, {Type::byte(), Data::binary()}} |
    {error, any()}.

-callback unpack_ext(Type::byte(), Data::binary(), msgpack:options()) ->
    {ok, any()} | {error, any()}.
