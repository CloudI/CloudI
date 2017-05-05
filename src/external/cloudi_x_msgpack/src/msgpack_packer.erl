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

-module(msgpack_packer).

-export([pack/2, pack_ext/3]).

-include("msgpack.hrl").
-include_lib("eunit/include/eunit.hrl").

%% pack them all
-spec pack(msgpack:object(), ?OPTION{}) -> binary().
pack(I, _) when is_integer(I) andalso I < 0 ->
    pack_int(I);
pack(I, _) when is_integer(I) ->
    pack_uint(I);
pack(F, _) when is_float(F) ->
    pack_double(F);
pack(null, _Opt) ->
    << 16#C0:8 >>;
pack(false, _) ->
    << 16#C2:8 >>;
pack(true, _) ->
    << 16#C3:8 >>;

pack(Bin, Opt) when is_binary(Bin) ->
    handle_binary(Bin, Opt);

pack(Atom, ?OPTION{allow_atom=pack} = Opt) when is_atom(Atom) ->
    handle_binary(erlang:atom_to_binary(Atom, utf8), Opt);
pack(Atom, ?OPTION{known_atoms=Atoms} = Opt) when is_atom(Atom) ->
    case lists:member(Atom, Atoms) of
        true ->
            handle_binary(erlang:atom_to_binary(Atom, utf8), Opt);
        false ->
            handle_ext(Atom, Opt)
    end;

%% jiffy interface
pack({Map}, Opt = ?OPTION{map_format=jiffy}) when is_list(Map) ->
    pack_map(Map, Opt);

%% jsx interface
pack(Map, Opt = ?OPTION{map_format=jsx}) when Map =:= [{}]->
    pack_map([], Opt);
pack([{_,_}|_] = Map, Opt = ?OPTION{map_format=jsx}) ->
    pack_map(Map, Opt);

pack({string, String}, ?OPTION{spec=new, pack_str=from_tagged_list}=Opt) ->
    case pack_string(String, Opt) of
        {error, _} -> throw({badarg, String});
        Bin when is_binary(Bin) -> Bin
    end;
pack(List, ?OPTION{spec=new, pack_str=from_list}=Opt)  when is_list(List) ->
    try
        case lists:all(fun is_integer/1, List) of
            true ->
                case pack_string(List, Opt) of
                    %% NOTE: due to erlang string format, msgpack can't
                    %% tell the difference between string and list of
                    %% integers. Thus users have to take care not to
                    %% include invalid unicode characters.
                    %% Here to fallback into list(int()).
                    {error, _} -> pack_array(List, Opt);
                    Bin when is_binary(Bin) -> Bin
                end;
            false ->
                pack_array(List, Opt)
        end
    catch error:badarg -> pack_array(List, Opt)
    end;

pack(List, Opt)  when is_list(List) ->
    pack_array(List, Opt);

pack(Other, ?OPTION{spec=new} = Opt) ->
    handle_ext(Other, Opt).

handle_binary(Bin, ?OPTION{spec=old}) ->
    pack_raw(Bin);
handle_binary(Bin, ?OPTION{spec=new, pack_str=from_list}) ->
    pack_raw2(Bin);
handle_binary(Bin, ?OPTION{spec=new, pack_str=from_binary} = Opt) ->
    pack_string(unicode:characters_to_list(Bin), Opt);
handle_binary(Bin, ?OPTION{spec=new}) ->
    pack_raw2(Bin).

%% %% map interface
handle_ext(Map, Opt) when is_map(Map) ->
    pack_map(maps:to_list(Map), Opt);

handle_ext(Any, _Opt = ?OPTION{ext_packer=Packer,
                               original_list=Orig})
  when is_function(Packer) ->

    case pack_ext(Any, Packer, Orig) of
        {ok, Binary} -> Binary;
        {error, E} -> throw({error, E})
    end;

handle_ext(Other, _) ->
    throw({badarg, Other}).

-spec pack_int(integer()) -> binary().
%% negative fixnum
pack_int(N) when N >= -32->
    << 2#111:3, N:5 >>;
%% int 8
pack_int(N) when N >= -128 ->
    << 16#D0:8, N:8/big-signed-integer-unit:1 >>;
%% int 16
pack_int(N) when N >= -16#8000 ->
    << 16#D1:8, N:16/big-signed-integer-unit:1 >>;
%% int 32
pack_int(N) when N >= -16#80000000 ->
    << 16#D2:8, N:32/big-signed-integer-unit:1 >>;
%% int 64
pack_int(N) when N >= -16#8000000000000000 ->
    << 16#D3:8, N:64/big-signed-integer-unit:1 >>;
%% too big int
pack_int(N) ->
    throw({badarg, N}).


-spec pack_uint(non_neg_integer()) -> binary().
%% positive fixnum
pack_uint(N) when N < 128 ->
    << 2#0:1, N:7 >>;
%% uint 8
pack_uint(N) when (N band 16#FF) =:= N ->
    << 16#CC:8, N:8 >>;
%% uint 16
pack_uint(N) when (N band 16#FFFF) =:= N ->
    << 16#CD:8, N:16/big-unsigned-integer-unit:1 >>;
%% uint 32
pack_uint(N) when (N band 16#FFFFFFFF) =:= N->
    << 16#CE:8, N:32/big-unsigned-integer-unit:1 >>;
%% uint 64
pack_uint(N) when (N band 16#FFFFFFFFFFFFFFFF) =:= N ->
    << 16#CF:8, N:64/big-unsigned-integer-unit:1 >>;
%% too big unit
pack_uint(N) ->
    throw({badarg, N}).


%% @doc float : erlang's float is always IEEE 754 64bit format. Thus it
%% never generates << 16#CA:8, F:32/big-float-unit:1 >>.
-spec pack_double(float()) -> binary().
pack_double(F) ->
    << 16#CB:8, F:64/big-float-unit:1 >>.

%% @doc raw bytes in old spec
-spec pack_raw(binary()) -> binary().
pack_raw(Bin) ->
    case byte_size(Bin) of
        Len when Len < 32->
            << 2#101:3, Len:5, Bin/binary >>;
        Len when Len < 16#10000 -> % 65536
            << 16#DA:8, Len:16/big-unsigned-integer-unit:1, Bin/binary >>;
        Len when Len < 16#100000000 ->
            << 16#DB:8, Len:32/big-unsigned-integer-unit:1, Bin/binary >>;
        _ ->
            throw({badarg, Bin})
    end.

-spec pack_raw2(binary()) -> binary().
%% raw bytes in new spec
pack_raw2(Bin) ->
    case byte_size(Bin) of
        Len when Len < 256->
            << 16#C4:8, Len:8/big-unsigned-integer-unit:1, Bin/binary>>;
        Len when Len < 16#10000 -> % 65536
            << 16#C5:8, Len:16/big-unsigned-integer-unit:1, Bin/binary >>;
        Len when Len < 16#100000000 ->
            << 16#C6:8, Len:32/big-unsigned-integer-unit:1, Bin/binary >>;
        _ ->
            throw({badarg, Bin})
    end.

%% @doc String MAY be unicode. Or may be EUC-JP, SJIS, UTF-1024 or anything.
%% EVERY implementation must show its binary length just after type indicator
%% to skip the damn string if its unreadable.
-spec pack_string(list(), ?OPTION{}) -> binary() | {error, atom()}.
pack_string(String, _Opt) ->
    case unicode:characters_to_binary(String) of
        {error, _Bin, _} -> {error, broken_unicode};
        {incomplete, _Bin, _} -> {error, incomplete_unicode};
        Bin ->
            case byte_size(Bin) of
                Len when Len < 32->
                    << 2#101:3, Len:5, Bin/binary >>;
                Len when Len < 256 ->
                    << 16#D9:8, Len:8/big-unsigned-integer-unit:1, Bin/binary >>;
                Len when Len < 16#10000 -> % 65536
                    << 16#DA:8, Len:16/big-unsigned-integer-unit:1, Bin/binary >>;
                Len when Len < 16#100000000 ->
                    << 16#DB:8, Len:32/big-unsigned-integer-unit:1, Bin/binary >>;
                _ ->
                    throw({badarg, String})
            end
    end.

-spec pack_array([msgpack:object()], ?OPTION{}) -> binary() | no_return().
pack_array([], _) ->
    << 2#1001:4, 0:4/integer-unit:1 >>;

pack_array([A], Opt) ->
    << 2#1001:4, 1:4/integer-unit:1, (pack(A, Opt))/binary >>;

pack_array([A, B], Opt) ->
    << 2#1001:4, 2:4/integer-unit:1, (pack(A, Opt))/binary, (pack(B, Opt))/binary >>;

pack_array([A, B, C], Opt) ->
    << 2#1001:4, 3:4/integer-unit:1, (pack(A, Opt))/binary, (pack(B, Opt))/binary, (pack(C, Opt))/binary >>;

pack_array([A, B, C, D], Opt) ->
    << 2#1001:4, 4:4/integer-unit:1,
       (pack(A, Opt))/binary, (pack(B, Opt))/binary, (pack(C, Opt))/binary, (pack(D, Opt))/binary >>;

pack_array([A, B, C, D, E], Opt) ->
    << 2#1001:4, 5:4/integer-unit:1,
       (pack(A, Opt))/binary, (pack(B, Opt))/binary, (pack(C, Opt))/binary, (pack(D, Opt))/binary,
       (pack(E, Opt))/binary >>;

pack_array([A, B, C, D, E, F], Opt) ->
    << 2#1001:4, 6:4/integer-unit:1,
       (pack(A, Opt))/binary, (pack(B, Opt))/binary, (pack(C, Opt))/binary, (pack(D, Opt))/binary,
       (pack(E, Opt))/binary, (pack(F, Opt))/binary >>;

pack_array([A, B, C, D, E, F, G], Opt) ->
    << 2#1001:4, 7:4/integer-unit:1,
       (pack(A, Opt))/binary, (pack(B, Opt))/binary, (pack(C, Opt))/binary, (pack(D, Opt))/binary,
       (pack(E, Opt))/binary, (pack(F, Opt))/binary, (pack(G, Opt))/binary >>;

pack_array([A, B, C, D, E, F, G, H], Opt) ->
    << 2#1001:4, 8:4/integer-unit:1,
       (pack(A, Opt))/binary, (pack(B, Opt))/binary, (pack(C, Opt))/binary, (pack(D, Opt))/binary,
       (pack(E, Opt))/binary, (pack(F, Opt))/binary, (pack(G, Opt))/binary, (pack(H, Opt))/binary >>;

pack_array([A, B, C, D, E, F, G, H, I], Opt) ->
    << 2#1001:4, 9:4/integer-unit:1,
       (pack(A, Opt))/binary, (pack(B, Opt))/binary, (pack(C, Opt))/binary, (pack(D, Opt))/binary,
       (pack(E, Opt))/binary, (pack(F, Opt))/binary, (pack(G, Opt))/binary, (pack(H, Opt))/binary,
       (pack(I, Opt))/binary >>;

pack_array([A, B, C, D, E, F, G, H, I, J], Opt) ->
    << 2#1001:4, 10:4/integer-unit:1,
       (pack(A, Opt))/binary, (pack(B, Opt))/binary, (pack(C, Opt))/binary, (pack(D, Opt))/binary,
       (pack(E, Opt))/binary, (pack(F, Opt))/binary, (pack(G, Opt))/binary, (pack(H, Opt))/binary,
       (pack(I, Opt))/binary, (pack(J, Opt))/binary >>;

pack_array([A, B, C, D, E, F, G, H, I, J, K], Opt) ->
    << 2#1001:4, 11:4/integer-unit:1,
       (pack(A, Opt))/binary, (pack(B, Opt))/binary, (pack(C, Opt))/binary, (pack(D, Opt))/binary,
       (pack(E, Opt))/binary, (pack(F, Opt))/binary, (pack(G, Opt))/binary, (pack(H, Opt))/binary,
       (pack(I, Opt))/binary, (pack(J, Opt))/binary, (pack(K, Opt))/binary >>;

pack_array([A, B, C, D, E, F, G, H, I, J, K, L], Opt) ->
    << 2#1001:4, 12:4/integer-unit:1,
       (pack(A, Opt))/binary, (pack(B, Opt))/binary, (pack(C, Opt))/binary, (pack(D, Opt))/binary,
       (pack(E, Opt))/binary, (pack(F, Opt))/binary, (pack(G, Opt))/binary, (pack(H, Opt))/binary,
       (pack(I, Opt))/binary, (pack(J, Opt))/binary, (pack(K, Opt))/binary, (pack(L, Opt))/binary >>;

pack_array([A, B, C, D, E, F, G, H, I, J, K, L, M], Opt) ->
    << 2#1001:4, 13:4/integer-unit:1,
       (pack(A, Opt))/binary, (pack(B, Opt))/binary, (pack(C, Opt))/binary, (pack(D, Opt))/binary,
       (pack(E, Opt))/binary, (pack(F, Opt))/binary, (pack(G, Opt))/binary, (pack(H, Opt))/binary,
       (pack(I, Opt))/binary, (pack(J, Opt))/binary, (pack(K, Opt))/binary, (pack(L, Opt))/binary,
       (pack(M, Opt))/binary >>;

pack_array([A, B, C, D, E, F, G, H, I, J, K, L, M, N], Opt) ->
    << 2#1001:4, 14:4/integer-unit:1,
       (pack(A, Opt))/binary, (pack(B, Opt))/binary, (pack(C, Opt))/binary, (pack(D, Opt))/binary,
       (pack(E, Opt))/binary, (pack(F, Opt))/binary, (pack(G, Opt))/binary, (pack(H, Opt))/binary,
       (pack(I, Opt))/binary, (pack(J, Opt))/binary, (pack(K, Opt))/binary, (pack(L, Opt))/binary,
       (pack(M, Opt))/binary, (pack(N, Opt))/binary >>;

pack_array([A, B, C, D, E, F, G, H, I, J, K, L, M, N, O], Opt) ->
    << 2#1001:4, 15:4/integer-unit:1,
       (pack(A, Opt))/binary, (pack(B, Opt))/binary, (pack(C, Opt))/binary, (pack(D, Opt))/binary,
       (pack(E, Opt))/binary, (pack(F, Opt))/binary, (pack(G, Opt))/binary, (pack(H, Opt))/binary,
       (pack(I, Opt))/binary, (pack(J, Opt))/binary, (pack(K, Opt))/binary, (pack(L, Opt))/binary,
       (pack(M, Opt))/binary, (pack(N, Opt))/binary, (pack(O, Opt))/binary >>;

pack_array(L, Opt) ->
    case length(L) of
        Len when Len < 16#10000 ->
            <<16#DC:8, Len:16/big-unsigned-integer-unit:1, (<< <<(pack(E, Opt))/binary>> || E <- L >>)/binary>>;
        Len when Len < 16#100000000 ->
            <<16#DD:8, Len:32/big-unsigned-integer-unit:1, (<< <<(pack(E, Opt))/binary>> || E <- L >>)/binary>>;
        _ ->
            throw({badarg, L})
    end.

-spec pack_map(msgpack:msgpack_map(), ?OPTION{}) -> binary() | no_return().
pack_map([{Ka, Va}], Opt)->
    << 2#1000:4, 1:4/integer-unit:1,
       (pack(Ka, Opt))/binary, (pack(Va, Opt))/binary >>;

pack_map([{Ka, Va}, {Kb, Vb}], Opt)->
    << 2#1000:4, 2:4/integer-unit:1,
       (pack(Ka, Opt))/binary, (pack(Va, Opt))/binary,
       (pack(Kb, Opt))/binary, (pack(Vb, Opt))/binary >>;

pack_map([{Ka, Va}, {Kb, Vb}, {Kc, Vc}], Opt)->
    << 2#1000:4, 3:4/integer-unit:1,
       (pack(Ka, Opt))/binary, (pack(Va, Opt))/binary,
       (pack(Kb, Opt))/binary, (pack(Vb, Opt))/binary,
       (pack(Kc, Opt))/binary, (pack(Vc, Opt))/binary >>;

pack_map([{Ka, Va}, {Kb, Vb}, {Kc, Vc}, {Kd, Vd}], Opt)->
    << 2#1000:4, 4:4/integer-unit:1,
       (pack(Ka, Opt))/binary, (pack(Va, Opt))/binary,
       (pack(Kb, Opt))/binary, (pack(Vb, Opt))/binary,
       (pack(Kc, Opt))/binary, (pack(Vc, Opt))/binary,
       (pack(Kd, Opt))/binary, (pack(Vd, Opt))/binary >>;

pack_map(M, Opt)->
    case length(M) of
        Len when Len < 16 ->
            <<2#1000:4, Len:4/integer-unit:1,
              (<< <<(pack(K, Opt))/binary, (pack(V, Opt))/binary>> || {K, V} <- M >>)/binary>>;
        Len when Len < 16#10000 -> % 65536
            <<16#DE:8, Len:16/big-unsigned-integer-unit:1,
              (<< <<(pack(K, Opt))/binary, (pack(V, Opt))/binary>> || {K, V} <- M >>)/binary>>;
        Len when Len < 16#100000000->
            <<16#DF:8, Len:32/big-unsigned-integer-unit:1,
              (<< <<(pack(K, Opt))/binary, (pack(V, Opt))/binary>> || {K, V} <- M >>)/binary>>;
        _ ->
            throw({badarg, M})
    end.

-spec pack_ext(any(), msgpack:ext_packer(), msgpack:options()) -> {ok, binary()} | {error, any()}.
pack_ext(Any, Packer, Opt) ->
    case Packer(Any, Opt) of
        {ok, {Type, Data}} when -16#80 =< Type andalso Type =< 16#7F ->
            Bin = case byte_size(Data) of
                      1  -> <<16#D4, Type:1/signed-integer-unit:8, Data/binary>>;
                      2  -> <<16#D5, Type:1/signed-integer-unit:8, Data/binary>>;
                      4  -> <<16#D6, Type:1/signed-integer-unit:8, Data/binary>>;
                      8  -> <<16#D7, Type:1/signed-integer-unit:8, Data/binary>>;
                      16 -> <<16#D8, Type:1/signed-integer-unit:8, Data/binary>>;
                      Len when Len < 16#100 ->
                          <<16#C7, Len:8, Type:1/signed-integer-unit:8, Data/binary>>;
                      Len when Len < 16#10000 ->
                          <<16#C8, Len:16, Type:1/signed-integer-unit:8, Data/binary>>;
                      Len when Len < 16#100000000 ->
                          <<16#C9, Len:32, Type:1/signed-integer-unit:8, Data/binary>>
                  end,
            {ok, Bin};
        {error, E} ->
            {error, E}
    end.
