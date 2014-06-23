%% -------------------------------------------------------------------
%%
%% riakc_obj: Container for Riak data and metadata
%%
%% Copyright (c) 2007-2010 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc riakc_obj is used to wrap bucket/key/value data sent to the
%%                server on put and received on get.  It provides
%%                accessors for retrieving the data and metadata
%%                and copes with siblings if multiple values are allowed.


-module(riakc_obj).
-export([new/2, new/3, new/4,
         bucket/1,
         key/1,
         vclock/1,
         value_count/1,
         select_sibling/2,
         get_contents/1,
         get_metadata/1,
         get_metadatas/1,
         get_content_type/1,
         get_content_types/1,
         get_value/1,
         get_values/1,
         update_metadata/2,
         update_value/2,
         update_value/3,
         update_content_type/2,
         get_update_metadata/1,
         get_update_content_type/1,
         get_update_value/1,
         md_ctype/1,
         set_vclock/2,
         get_user_metadata_entry/2,
         get_user_metadata_entries/1,
         clear_user_metadata_entries/1,
         delete_user_metadata_entry/2,
         set_user_metadata_entry/2,
         get_secondary_index/2,
         get_secondary_indexes/1,
         clear_secondary_indexes/1,
         delete_secondary_index/2,
         set_secondary_index/2,
         add_secondary_index/2,
         get_links/2,
         get_all_links/1,
         clear_links/1,
         delete_links/2,
         set_link/2,
         add_link/2
        ]).
%% Internal library use only
-export([new_obj/4,index_id_to_bin/1]).

-include_lib("riak_pb/include/riak_pb_kv_codec.hrl").
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-type bucket() :: binary(). %% A bucket name
-type key() :: binary() | 'undefined'. %% A key name
-type id() :: {bucket(), key()}.
-type vclock() :: binary(). %% An opaque vector clock
-type metadata() :: dict(). %% Value metadata
-type content_type() :: string(). %% The media type of a value
-type value() :: binary(). %% An opaque value
-type contents() :: [{metadata(), value()}]. %% All metadata/value pairs in a `riakc_obj'.
-type binary_index_id() :: {binary_index, string()}.
-type binary_index_value() :: binary().
-type binary_index() :: {binary_index_id(), [binary_index_value()]}.
-type integer_index_id() :: {integer_index, string()}.
-type integer_index_value() :: integer().
-type integer_index() :: {integer_index_id(), [integer_index_value()]}.
-type secondary_index_id() :: binary_index_id() | integer_index_id().
-type secondary_index_value() :: integer_index_value() | binary_index_value().
-type secondary_index() :: binary_index() | integer_index().
-type metadata_key() :: binary().
-type metadata_value() :: binary().
-type metadata_entry() :: {metadata_key(), metadata_value()}.
-type tag() :: binary().
-type link() :: {tag(), [id()]}.

-record(riakc_obj, {
          bucket :: bucket(),
          key :: key(),
          vclock :: vclock(),
          contents :: contents(),
          updatemetadata :: dict(),
          updatevalue :: value()
         }).

-type riakc_obj() :: #riakc_obj{}. %% The record/type containing the entire Riak object.
-export_type([riakc_obj/0, bucket/0, key/0, vclock/0, contents/0, metadata/0, value/0,
              binary_index_id/0, binary_index/0, integer_index_id/0, integer_index/0, 
              secondary_index/0, metadata_key/0, metadata_value/0, metadata_entry/0]).

%% ====================================================================
%% object functions
%% ====================================================================

%% @doc Constructor for new riak client objects.
-spec new(bucket(), key()) -> riakc_obj().
new(Bucket, Key) ->
    build_client_object(Bucket, Key, undefined).

%% @doc Constructor for new riak client objects with an update value.
-spec new(bucket(), key(), value()) -> riakc_obj().
new(Bucket, Key, Value) ->
    build_client_object(Bucket, Key, Value).

%% @doc Constructor for new riak client objects with an update value and content type.
-spec new(bucket(), key(), value(), content_type()) -> riakc_obj().
new(Bucket, Key, Value, ContentType) ->
    case build_client_object(Bucket, Key, Value) of
        {error, Reason} ->
            {error, Reason};
        O ->
            update_content_type(O, ContentType)
    end.

%% @doc Build a new riak client object with non-empty key
-spec build_client_object(bucket(), key(), undefined | value()) ->
                                 riakc_obj() | {error, atom()}.
build_client_object(<<>>, K, _) when is_binary(K) ->
    {error, zero_length_bucket};
build_client_object(B, <<>>, _) when is_binary(B) ->
    {error, zero_length_key};
build_client_object(B, K, V) when is_binary(B), is_binary(K) orelse undefined =:= K ->
    #riakc_obj{bucket = B, key = K, contents = [], updatevalue = V}.

%% @doc Return the containing bucket for this riakc_obj.
-spec bucket(Object::riakc_obj()) -> bucket().
bucket(O) ->
    O#riakc_obj.bucket.

%% @doc  Return the key for this riakc_obj.
-spec key(Object::riakc_obj()) -> key().
key(O) ->
    O#riakc_obj.key.

%% @doc  Return the vector clock for this riakc_obj.
-spec vclock(Object::riakc_obj()) -> vclock() | undefined.
vclock(O) ->
    O#riakc_obj.vclock.

%% @doc  Return the number of values (siblings) of this riakc_obj.
-spec value_count(Object::riakc_obj()) -> non_neg_integer().
value_count(#riakc_obj{contents=Contents}) -> 
    length(Contents).

%% @doc  Select the sibling to use for update - starting from 1.
-spec select_sibling(pos_integer(), Object::riakc_obj()) -> riakc_obj().
select_sibling(Index, O) ->
    {MD,V} = lists:nth(Index, O#riakc_obj.contents),
    O#riakc_obj{updatemetadata=MD, updatevalue=V}.

%% @doc  Return the contents (a list of {metadata, value} tuples) for
%%       this riakc_obj.
-spec get_contents(Object::riakc_obj()) -> contents().
get_contents(O) ->
    O#riakc_obj.contents.

%% @doc Assert that this riakc_obj has no siblings and return its
%%       associated metadata.  This function will throw `siblings' if
%%       the object has siblings (value_count() > 1).
%% @throws siblings
-spec get_metadata(Object::riakc_obj()) -> metadata().
get_metadata(O=#riakc_obj{}) ->
    case get_contents(O) of
        [] ->
            dict:new();
        [{MD,_V}] ->
            MD;
        _ ->
            throw(siblings)
    end.

%% @doc  Return a list of the metadata values for this riakc_obj.
-spec get_metadatas(Object::riakc_obj()) -> [metadata()].
get_metadatas(#riakc_obj{contents=Contents}) ->
    [M || {M,_V} <- Contents].

%% @doc Return the content type of the value if there are no siblings.
%% @see get_metadata/1
%% @throws siblings
-spec get_content_type(Object::riakc_obj()) -> content_type().
get_content_type(Object=#riakc_obj{}) ->
    UM = get_metadata(Object),
    md_ctype(UM).

%% @doc Return a list of content types for all siblings.
-spec get_content_types(Object::riakc_obj()) -> [content_type()].
get_content_types(Object=#riakc_obj{}) ->
    F = fun({M,_}) -> md_ctype(M) end,
    [F(C) || C<- get_contents(Object)].


%% @doc Assert that this riakc_obj has no siblings and return its
%%       associated value.  This function will throw `siblings' if the
%%       object has siblings (value_count() > 1), or `no_value' if the
%%       object has no value.
%% @throws siblings | no_value
-spec get_value(Object::riakc_obj()) -> value().
get_value(#riakc_obj{}=O) ->
    case get_contents(O) of
        [] ->
            throw(no_value);
        [{_MD,V}] ->
            V;
        _ ->
            throw(siblings)
    end.

%% @doc  Return a list of object values for this riakc_obj.
-spec get_values(Object::riakc_obj()) -> [value()].
get_values(#riakc_obj{contents=Contents}) ->
    [V || {_,V} <- Contents].

%% @doc  Set the updated metadata of an object to M.
-spec update_metadata(riakc_obj(), metadata()) -> riakc_obj().
update_metadata(Object=#riakc_obj{}, M) ->
    Object#riakc_obj{updatemetadata=M}.

%% @doc  Set the updated content-type of an object to CT.
-spec update_content_type(riakc_obj(),content_type()|binary()) -> riakc_obj().
update_content_type(Object=#riakc_obj{}, CT) when is_binary(CT) ->
    update_content_type(Object, binary_to_list(CT));
update_content_type(Object=#riakc_obj{}, CT) when is_list(CT) ->
    M1 = get_update_metadata(Object),
    Object#riakc_obj{updatemetadata=dict:store(?MD_CTYPE, CT, M1)}.

%% @doc  Set the updated value of an object to V
-spec update_value(riakc_obj(), value()) -> riakc_obj().
update_value(Object=#riakc_obj{}, V) -> Object#riakc_obj{updatevalue=V}.

%% @doc  Set the updated value of an object to V
-spec update_value(riakc_obj(), value(), content_type()) -> riakc_obj().
update_value(Object=#riakc_obj{}, V, CT) -> 
    O1 = update_content_type(Object, CT),
    O1#riakc_obj{updatevalue=V}.

%% @doc  Return the updated metadata of this riakc_obj.
-spec get_update_metadata(Object::riakc_obj()) -> metadata().
get_update_metadata(#riakc_obj{updatemetadata=UM}=Object) ->
    case UM of
        undefined ->
            get_metadata(Object);
        UM ->
            UM
    end.
           
%% @doc Return the content type of the update value
-spec get_update_content_type(riakc_obj()) -> content_type().
get_update_content_type(Object=#riakc_obj{}) ->
    UM = get_update_metadata(Object),
    md_ctype(UM).

%% @doc  Return the updated value of this riakc_obj.
-spec get_update_value(Object::riakc_obj()) -> value().
get_update_value(#riakc_obj{updatevalue=UV}=Object) -> 
    case UV of
        undefined ->
            get_value(Object);
        UV ->
            UV
    end.

%% @doc  Return the content type from metadata
-spec md_ctype(dict()) -> undefined | content_type().
md_ctype(MetaData) ->
    case dict:find(?MD_CTYPE, MetaData) of
        error ->
            undefined;
        {ok, Ctype} ->
            Ctype
    end.

%% @doc  Set the vector clock of an object
-spec set_vclock(riakc_obj(), vclock()) -> riakc_obj().
set_vclock(Object=#riakc_obj{}, Vclock) ->
    Object#riakc_obj{vclock=Vclock}.

%% @doc  Get specific metadata entry
-spec get_user_metadata_entry(metadata(), metadata_key()) -> metadata_value() | notfound.
get_user_metadata_entry(MD, Key) ->
    case dict:find(?MD_USERMETA, MD) of
        {ok, Entries} -> 
            case lists:keyfind(Key, 1, Entries) of
                false ->
                    notfound;
                {Key, Value} ->
                    Value
            end;
        error ->
            notfound
    end.

%% @doc  Get all metadata entries
-spec get_user_metadata_entries(metadata()) -> [metadata_entry()].
get_user_metadata_entries(MD) ->
    case dict:find(?MD_USERMETA, MD) of
        {ok, Entries} -> 
            Entries;
        error ->
            []
    end.

%% @doc  Clear all metadata entries
-spec clear_user_metadata_entries(metadata()) -> metadata().
clear_user_metadata_entries(MD) ->
    dict:erase(?MD_USERMETA, MD).

%% @doc  Delete specific metadata entry
-spec delete_user_metadata_entry(metadata(), metadata_key()) -> metadata().
delete_user_metadata_entry(MD, Key) ->
    case dict:find(?MD_USERMETA, MD) of
        {ok, Entries} -> 
            case [{K, V} || {K, V} <- Entries, K /= Key] of
                [] ->
                    dict:erase(?MD_USERMETA, MD);
                NewList ->
                    dict:store(?MD_USERMETA, NewList, MD)
            end;
        error ->
            MD
    end.

%% @doc  Set a metadata entry
-spec set_user_metadata_entry(metadata(), metadata_entry()) -> metadata().
set_user_metadata_entry(MD, {Key, Value}) ->
    case dict:find(?MD_USERMETA, MD) of
        {ok, Entries} ->
            case [{K, V} || {K, V} <- Entries, K /= Key] of
                [] ->
                    dict:store(?MD_USERMETA, [{Key, Value}], MD);
                List ->
                    dict:store(?MD_USERMETA, [{Key, Value} | List], MD)
            end;
        error ->
            dict:store(?MD_USERMETA, [{Key, Value}], MD)
    end.

%% @doc  Get value(s) for specific secondary index
-spec get_secondary_index(metadata(), secondary_index_id()) -> [secondary_index_value()] | notfound.
get_secondary_index(MD, {Type, Name}) ->
    IndexName = index_id_to_bin({Type, Name}),
    case dict:find(?MD_INDEX, MD) of
        {ok, Entries} ->
            case {Type, [V || {K, V} <- Entries, K == IndexName]} of
                {_, []} ->
                    notfound;
                {binary_index, List} ->
                    List;
                {integer_index, List} ->
                    [list_to_integer(binary_to_list(I)) || I <- List]
            end;
        error ->
            notfound
    end.

%% @doc  Get all secondary indexes
-spec get_secondary_indexes(metadata()) -> [secondary_index()].
get_secondary_indexes(MD) ->
    case dict:find(?MD_INDEX, MD) of
        {ok, Entries} ->
            dict:to_list(lists:foldl(fun({N, V}, D) ->
                                        case bin_to_index_id(N) of
                                            {binary_index, Name} ->
                                                dict:append({binary_index, Name}, V, D);
                                            {integer_index, Name} ->
                                                Int = list_to_integer(binary_to_list(V)),
                                                dict:append({integer_index, Name}, Int, D)
                                        end
                                    end, dict:new(), Entries));
        error ->
            []
    end.

%% @doc  Clear all secondary indexes 
-spec clear_secondary_indexes(metadata()) -> metadata().
clear_secondary_indexes(MD) ->
    dict:erase(?MD_INDEX, MD).

%% @doc  Delete specific secondary index
-spec delete_secondary_index(metadata(), secondary_index_id()) -> metadata().
delete_secondary_index(MD, IndexId) ->
    IndexName = index_id_to_bin(IndexId),
    case dict:find(?MD_INDEX, MD) of
        {ok, Entries} ->
            List = [{N, V} || {N, V} <- Entries, N /= IndexName],
            dict:store(?MD_INDEX, List, MD);
        error ->
            MD
    end.

%% @doc  Set a secondary index
-spec set_secondary_index(metadata(), secondary_index() | [secondary_index()]) -> metadata().
set_secondary_index(MD, []) ->
    MD;
set_secondary_index(MD, {{binary_index, Name}, BinList}) ->
    set_secondary_index(MD, [{{binary_index, Name}, BinList}]);
set_secondary_index(MD, {{integer_index, Name}, IntList}) ->
    set_secondary_index(MD, [{{integer_index, Name}, IntList}]);
set_secondary_index(MD, [{{binary_index, Name}, BinList} | Rest]) ->
    IndexName = index_id_to_bin({binary_index, Name}),
    set_secondary_index(MD, [{IndexName, BinList} | Rest]);
set_secondary_index(MD, [{{integer_index, Name}, IntList} | Rest]) ->
    IndexName = index_id_to_bin({integer_index, Name}),
    set_secondary_index(MD, [{IndexName, [list_to_binary(integer_to_list(I)) || I <- IntList]} | Rest]);
set_secondary_index(MD, [{Id, BinList} | Rest]) when is_binary(Id) ->
    List = [{Id, V} || V <- BinList],
    case dict:find(?MD_INDEX, MD) of
        {ok, Entries} ->
            OtherEntries = [{N, V} || {N, V} <- Entries, N /= Id],
            NewList = lists:usort(lists:append(OtherEntries, List)),
            MD2 = dict:store(?MD_INDEX, NewList, MD),
            set_secondary_index(MD2, Rest);
        error ->
            NewList = lists:usort(List),
            MD2 = dict:store(?MD_INDEX, NewList, MD),
            set_secondary_index(MD2, Rest)
    end.

%% @doc  Add a secondary index
-spec add_secondary_index(metadata(), secondary_index() | [secondary_index()]) -> metadata().
add_secondary_index(MD, []) ->
    MD;
add_secondary_index(MD, {{binary_index, Name}, BinList}) ->
    add_secondary_index(MD, [{{binary_index, Name}, BinList}]);
add_secondary_index(MD, {{integer_index, Name}, IntList}) ->
    add_secondary_index(MD, [{{integer_index, Name}, IntList}]);
add_secondary_index(MD, [{{binary_index, Name}, BinList} | Rest]) ->
    IndexName = index_id_to_bin({binary_index, Name}),
    add_secondary_index(MD, [{IndexName, BinList} | Rest]);
add_secondary_index(MD, [{{integer_index, Name}, IntList} | Rest]) ->
    IndexName = index_id_to_bin({integer_index, Name}),
    add_secondary_index(MD, [{IndexName, [list_to_binary(integer_to_list(I)) || I <- IntList]} | Rest]);
add_secondary_index(MD, [{Id, BinList} | Rest]) when is_binary(Id) ->
    List = [{Id, V} || V <- BinList],
    case dict:find(?MD_INDEX, MD) of
        {ok, Entries} ->
            NewList = lists:usort(lists:append(Entries, List)),
            MD2 = dict:store(?MD_INDEX, NewList, MD),
            add_secondary_index(MD2, Rest);
        error ->
            NewList = lists:usort(List),
            MD2 = dict:store(?MD_INDEX, NewList, MD),
            add_secondary_index(MD2, Rest)
    end.

%% @doc  Get links for a specific tag
-spec get_links(metadata(), tag()) -> [id()] | notfound.
get_links(MD, Tag) ->
    case dict:find(?MD_LINKS, MD) of
        {ok, Links} ->
            case [I || {I, T} <- Links, T == Tag] of
                [] ->
                    notfound;
                List ->
                    List
            end;
        error ->
            notfound
    end.

%% @doc  Get all links
-spec get_all_links(metadata()) -> [link()].
get_all_links(MD) ->
    case dict:find(?MD_LINKS, MD) of
        {ok, Links} ->
            dict:to_list(lists:foldl(fun({I, T}, D) ->
                                        dict:append(T, I, D) 
                                    end, dict:new(), Links));
        error ->
            []
    end.

%% @doc  Clear all links
-spec clear_links(metadata()) -> metadata().
clear_links(MD) ->
    dict:erase(?MD_LINKS, MD).

%% @doc  Delete links for a specific tag
-spec delete_links(metadata(), tag()) -> metadata().
delete_links(MD, Tag) ->
    case dict:find(?MD_LINKS, MD) of
        {ok, Links} ->
            List = [{I, T} || {I, T} <- Links, T /= Tag],
            dict:store(?MD_LINKS, List, MD);
        error ->
            MD
    end.

%% @doc  Set links for a specific tag
-spec set_link(metadata(), link() | [link()]) -> metadata().
set_link(MD, []) ->
    MD;
set_link(MD, Link) when is_tuple(Link) ->
    set_link(MD, [Link]);
set_link(MD, [{T, IdList} | Rest]) ->
    List = [{I, T} || I <- IdList],
    case dict:find(?MD_LINKS, MD) of
        {ok, Links} ->
            OtherLinks = [{N, Tag} || {N, Tag} <- Links, Tag /= T],
            NewList = lists:usort(lists:append(OtherLinks, List)),
            MD2 = dict:store(?MD_LINKS, NewList, MD),
            set_link(MD2, Rest);
        error ->
            NewList = lists:usort(List),
            MD2 = dict:store(?MD_LINKS, NewList, MD),
            set_link(MD2, Rest)
    end.

%% @doc  Add links for a specific tag
-spec add_link(metadata(), secondary_index() | [secondary_index()]) -> metadata().
add_link(MD, []) ->
    MD;
add_link(MD, Link) when is_tuple(Link) ->
    add_link(MD, [Link]);
add_link(MD, [{T, IdList} | Rest]) ->
    List = [{I, T} || I <- IdList],
    case dict:find(?MD_LINKS, MD) of
        {ok, Links} ->
            NewList = lists:usort(lists:append(Links, List)),
            MD2 = dict:store(?MD_LINKS, NewList, MD),
            add_link(MD2, Rest);
        error ->
            NewList = lists:usort(List),
            MD2 = dict:store(?MD_LINKS, NewList, MD),
            add_link(MD2, Rest)
    end.

%% @doc  INTERNAL USE ONLY.  Set the contents of riakc_obj to the
%%       {Metadata, Value} pairs in MVs. Normal clients should use the
%%       set_update_[value|metadata]() + apply_updates() method for changing
%%       object contents.
%% @private
-spec new_obj(bucket(), key(), vclock(), contents()) -> riakc_obj().
new_obj(Bucket, Key, Vclock, Contents) ->
    #riakc_obj{bucket = Bucket, key = Key, vclock = Vclock, contents = Contents}.

%% @doc  INTERNAL USE ONLY.  Convert binary secondary index name to index id tuple.
%% @private
-spec bin_to_index_id(binary()) -> secondary_index_id().
bin_to_index_id(Index) ->
    Str = binary_to_list(Index),
    case lists:split((length(Str) - 4), Str) of
        {Name, "_bin"} ->
            {binary_index, Name};
        {Name, "_int"} ->
            {integer_index, Name}
    end.

%% @doc  INTERNAL USE ONLY.  Convert index id tuple to binary index name
%% @private
-spec index_id_to_bin(secondary_index_id()) -> binary().
index_id_to_bin({binary_index, Name}) ->
    list_to_binary([Name, "_bin"]);
index_id_to_bin({integer_index, Name}) ->
    list_to_binary([Name, "_int"]).

%% ===================================================================
%% Unit Tests
%% ===================================================================
-ifdef(TEST).

bucket_test() ->
    O = riakc_obj:new(<<"b">>, <<"k">>),
    ?assertEqual(<<"b">>, bucket(O)).

key_test() ->
    O = riakc_obj:new(<<"b">>, <<"k">>),
    ?assertEqual(<<"k">>, key(O)).

invalid_key_test() ->
    ?assertMatch({error, _}, riakc_obj:new(<<"b">>, <<>>)),
    ?assertMatch({error, _}, riakc_obj:new(<<"b">>, <<>>, <<"v">>)),
    ?assertMatch({error, _}, riakc_obj:new(<<"b">>, <<>>, <<"v">>, <<"application/x-foo">>)),
    ?assertMatch({error, _}, riakc_obj:new(<<>>, <<"k">>)),
    ?assertMatch({error, _}, riakc_obj:new(<<>>, <<"k">>, <<"v">>)),
    ?assertMatch({error, _}, riakc_obj:new(<<>>, <<"k">>, <<"v">>, <<"application/x-foo">>)),
    ?assertMatch({error, _}, riakc_obj:new(<<>>, <<>>)),
    ?assertMatch({error, _}, riakc_obj:new(<<>>, <<>>, <<"v">>)),
    ?assertMatch({error, _}, riakc_obj:new(<<>>, <<>>, <<"v">>, <<"application/x-foo">>)),
    ?assertMatch(#riakc_obj{}, riakc_obj:new(<<"b">>, undefined)),
    ?assertError(function_clause, riakc_obj:new("bucket","key")).

vclock_test() ->
    %% For internal use only
    O = riakc_obj:new_obj(<<"b">>, <<"k">>, <<"vclock">>, []),
    ?assertEqual(<<"vclock">>, vclock(O)).

newcontent0_test() ->
    O = riakc_obj:new(<<"b">>, <<"k">>),
    ?assertEqual(0, value_count(O)),
    ?assertEqual([], get_metadatas(O)),
    ?assertEqual([], get_values(O)),
    ?assertEqual([], get_contents(O)),
    ?assertEqual(dict:new(), get_metadata(O)),
    ?assertThrow(no_value, get_value(O)).    

contents0_test() ->
    O = riakc_obj:new_obj(<<"b">>, <<"k">>, <<"vclock">>, []),
    ?assertEqual(0, value_count(O)),
    ?assertEqual([], get_metadatas(O)),
    ?assertEqual([], get_values(O)),
    ?assertEqual([], get_contents(O)),
    ?assertEqual(dict:new(), get_metadata(O)),
    ?assertThrow(no_value, get_value(O)).

contents1_test() ->
    M1 = dict:from_list([{?MD_VTAG, "tag1"}]),
    O = riakc_obj:new_obj(<<"b">>, <<"k">>, <<"vclock">>,
                      [{M1, <<"val1">>}]),
    ?assertEqual(1, value_count(O)),
    ?assertEqual([M1], get_metadatas(O)),
    ?assertEqual([<<"val1">>], get_values(O)),
    ?assertEqual([{M1,<<"val1">>}], get_contents(O)),
    ?assertEqual(M1, get_metadata(O)),
    ?assertEqual(<<"val1">>, get_value(O)).

contents2_test() ->
    M1 = dict:from_list([{?MD_VTAG, "tag1"}]),
    M2 = dict:from_list([{?MD_VTAG, "tag1"}]),
    O = riakc_obj:new_obj(<<"b">>, <<"k">>, <<"vclock">>,
                      [{M1, <<"val1">>},
                       {M2, <<"val2">>}]),
    ?assertEqual(2, value_count(O)),
    ?assertEqual([M1, M2], get_metadatas(O)),
    ?assertEqual([<<"val1">>, <<"val2">>], get_values(O)),
    ?assertEqual([{M1,<<"val1">>},{M2,<<"val2">>}], get_contents(O)),
    ?assertThrow(siblings, get_metadata(O)),
    ?assertThrow(siblings, get_value(O)).

update_metadata_test() ->
    O = riakc_obj:new(<<"b">>, <<"k">>),
    UM = riakc_obj:get_update_metadata(O),
    ?assertEqual([], dict:to_list(UM)).

update_value_test() ->
    O = riakc_obj:new(<<"b">>, <<"k">>),
    ?assertThrow(no_value, get_update_value(O)),
    O1 = riakc_obj:update_value(O, <<"v">>),
    ?assertEqual(<<"v">>, get_update_value(O1)),
    M1 = dict:from_list([{?MD_VTAG, "tag1"}]),
    O2 = riakc_obj:update_metadata(O1, M1),
    ?assertEqual(M1, get_update_metadata(O2)).

updatevalue_ct_test() ->
    O = riakc_obj:new(<<"b">>, <<"k">>),
    ?assertThrow(no_value, get_update_value(O)),
    O1 = riakc_obj:update_value(O, <<"v">>, "x-application/custom"),
    ?assertEqual(<<"v">>, get_update_value(O1)),
    M1 = dict:from_list([{?MD_VTAG, "tag1"}]),
    O2 = riakc_obj:update_metadata(O1, M1),
    ?assertEqual(M1, get_update_metadata(O2)),
    ?assertEqual("x-application/custom", get_update_content_type(O1)).

update_content_type_test() ->
    O = riakc_obj:new(<<"b">>, <<"k">>),
    undefined = get_update_content_type(O),
    O1 = update_content_type(O, "application/json"),
    ?assertEqual("application/json", get_update_content_type(O1)).

binary_content_type_test() ->
    O = riakc_obj:new(<<"b">>, <<"k">>, <<"v">>, <<"application/x-foo">>),
    ?assertEqual("application/x-foo", get_update_content_type(O)),
    O1 = update_content_type(O, <<"application/x-bar">>),
    ?assertEqual("application/x-bar", get_update_content_type(O1)).

get_update_data_test() ->
    MD0 = dict:from_list([{?MD_CTYPE, "text/plain"}]),
    MD1 = dict:from_list([{?MD_CTYPE, "application/json"}]),
    O = new_obj(<<"b">>, <<"k">>, <<"">>, 
                [{MD0, <<"v">>}]),
    %% Create an updated metadata object
    Oumd = update_metadata(O, MD1),
    %% Create an updated value object
    Ouv = update_value(O, <<"valueonly">>),
    %% Create updated both object
    Oboth = update_value(Oumd, <<"both">>),

    %% dbgh:start(),
    %% dbgh:trace(?MODULE)
    io:format("O=~p\n", [O]),
    ?assertEqual(<<"v">>, get_update_value(O)),
    MD2 = get_update_metadata(O),
    io:format("MD2=~p\n", [MD2]),
    ?assertEqual("text/plain", md_ctype(MD2)),

    MD3 = get_update_metadata(Oumd),
    ?assertEqual("application/json", md_ctype(MD3)),

    ?assertEqual(<<"valueonly">>, get_update_value(Ouv)),
    MD4 = get_update_metadata(Ouv),
    ?assertEqual("text/plain", md_ctype(MD4)),

    ?assertEqual(<<"both">>, get_update_value(Oboth)),
    MD5 = get_update_metadata(Oboth),
    ?assertEqual("application/json", md_ctype(MD5)).
   
%% get_update_data_sibs_test() ->
%%     MD0 = dict:from_list([{?MD_CTYPE, "text/plain"}]),
%%     MD1 = dict:from_list([{?MD_CTYPE, "application/json"}]),
%%     O = new_obj(<<"b">>, <<"k">>, <<"">>, 
%%                 [{MD0, <<"v">>},{MD1, <<"sibling">>}]),
%%     %% Create an updated metadata object
%%     Oumd = update_metadata(O, MD1),
%%     %% Create an updated value object
%%     Ouv = update_value(O, <<"valueonly">>),
%%     %% Create updated both object
%%     Oboth = update_value(Oumd, <<"both">>),
    
%%     ?assertThrow({error, siblings}, get_update_data(O)),
%%     ?assertThrow({error, siblings}, get_update_data(Oumd)),
%%     ?assertEqual({error, siblings}, get_update_data(Ouv)),
%%     ?assertEqual({ok, {MD1, <<"both">>}}, get_update_data(Oboth)).
                              
select_sibling_test() ->
    MD0 = dict:from_list([{?MD_CTYPE, "text/plain"}]),
    MD1 = dict:from_list([{?MD_CTYPE, "application/json"}]),
    O = new_obj(<<"b">>, <<"k">>, <<"">>, 
                [{MD0, <<"sib_one">>},
                 {MD1, <<"sib_two">>}]),
    O1 = select_sibling(1, O),
    O2 = select_sibling(2, O),
    ?assertEqual("text/plain", get_update_content_type(O1)),
    ?assertEqual(<<"sib_one">>, get_update_value(O1)),
    ?assertEqual("application/json", get_update_content_type(O2)),
    ?assertEqual(<<"sib_two">>, get_update_value(O2)).
   
user_metadata_utilities_test() ->
    MD0 = dict:new(),
    ?assertEqual(dict:to_list(MD0), dict:to_list(delete_user_metadata_entry(MD0, <<"None">>))),
    MD1 = set_user_metadata_entry(MD0,{<<"Key1">>, <<"Value0">>}),
    ?assertEqual([{<<"Key1">>, <<"Value0">>}], get_user_metadata_entries(MD1)),
    MD2 = set_user_metadata_entry(MD1,{<<"Key1">>, <<"Value1">>}),
    ?assertEqual([{<<"Key1">>, <<"Value1">>}], get_user_metadata_entries(MD2)),
    ?assertEqual(notfound, get_user_metadata_entry(MD2, <<"WrongKey">>)),
    MD3 = set_user_metadata_entry(MD2,{<<"Key2">>, <<"Value2">>}),
    ?assertEqual(<<"Value2">>, get_user_metadata_entry(MD3, <<"Key2">>)),
    ?assertEqual(2, length(get_user_metadata_entries(MD3))),
    MD4 = delete_user_metadata_entry(MD3, <<"Key1">>),
    ?assertEqual([{<<"Key2">>, <<"Value2">>}], get_user_metadata_entries(MD4)),
    MD5 = clear_user_metadata_entries(MD4),
    ?assertEqual([], get_user_metadata_entries(MD5)),
    MD6 = delete_user_metadata_entry(MD1, <<"Key1">>),
    ?assertEqual([], get_user_metadata_entries(MD6)),
    ?assertEqual(notfound, get_user_metadata_entry(MD6, <<"Key1">>)).

link_utilities_test() ->
    MD0 = dict:new(),
    ?assertEqual(notfound, get_links(MD0, <<"Tag1">>)),
    ?assertEqual([], get_all_links(MD0)),
    MD1 = set_link(MD0, [{<<"Tag1">>, [{<<"B">>,<<"K1">>},{<<"B">>,<<"K2">>}]}]),
    ?assertEqual([{<<"B">>,<<"K1">>},{<<"B">>,<<"K2">>}], lists:sort(get_links(MD1,<<"Tag1">>))),
    MD2 = add_link(MD1, [{<<"Tag1">>, [{<<"B">>,<<"K1">>},{<<"B">>,<<"K3">>}]}]),
    ?assertEqual([{<<"B">>,<<"K1">>},{<<"B">>,<<"K2">>},{<<"B">>,<<"K3">>}], lists:sort(get_links(MD2,<<"Tag1">>))),
    MD3 = set_link(MD2, [{<<"Tag1">>, [{<<"B">>,<<"K4">>}]}]),
    ?assertEqual([{<<"B">>,<<"K4">>}], lists:sort(get_links(MD3,<<"Tag1">>))),
    ?assertEqual([{<<"Tag1">>,[{<<"B">>,<<"K4">>}]}], get_all_links(MD3)),
    MD4 = set_link(MD3, [{<<"Tag2">>, [{<<"B">>,<<"K1">>}]}]),
    ?assertEqual([{<<"B">>,<<"K1">>}], lists:sort(get_links(MD4,<<"Tag2">>))),
    MD5 = delete_links(MD4,<<"Tag1">>),
    ?assertEqual([{<<"Tag2">>,[{<<"B">>,<<"K1">>}]}], get_all_links(MD5)),
    MD6 = clear_links(MD5),
    ?assertEqual([], get_all_links(MD6)).
   
secondary_index_utilities_test() ->
    MD0 = dict:new(),
    ?assertEqual([], get_secondary_indexes(MD0)),
    ?assertEqual(notfound, get_secondary_index(MD0, {binary_index,"none"})),
    ?assertEqual(notfound, get_secondary_index(MD0, {integer_index,"none"})),
    MD1 = set_secondary_index(MD0, [{{integer_index,"idx"}, [12,4,56]}]),
    ?assertEqual([4,12,56], lists:sort(get_secondary_index(MD1,{integer_index,"idx"}))),
    MD2 = add_secondary_index(MD1, [{{integer_index,"idx"}, [4,15,34]}]),
    ?assertEqual([4,12,15,34,56], lists:sort(get_secondary_index(MD2,{integer_index,"idx"}))),
    MD3 = set_secondary_index(MD2, {{integer_index,"idx"}, [7]}),
    ?assertEqual([7], lists:sort(get_secondary_index(MD3,{integer_index,"idx"}))),
    MD4 = set_secondary_index(MD3, [{{binary_index,"idx"}, [<<"12">>,<<"4">>,<<"56">>]}]),
    ?assertEqual([<<"12">>,<<"4">>,<<"56">>], lists:sort(get_secondary_index(MD4,{binary_index,"idx"}))),
    MD5 = add_secondary_index(MD4, [{{binary_index,"idx"}, [<<"4">>,<<"15">>,<<"34">>]}]),
    ?assertEqual([<<"12">>,<<"15">>,<<"34">>,<<"4">>,<<"56">>], lists:sort(get_secondary_index(MD5,{binary_index,"idx"}))),
    MD6 = set_secondary_index(MD5, {{binary_index,"idx"}, [<<"7">>]}),
    ?assertEqual([<<"7">>], lists:sort(get_secondary_index(MD6,{binary_index,"idx"}))),
    ?assertEqual(2, length(get_secondary_indexes(MD6))),
    ?assertEqual(notfound, get_secondary_index(MD6,{binary_index,"error"})),
    MD7 = delete_secondary_index(MD6,{binary_index,"idx"}),
    ?assertEqual([{{integer_index,"idx"},[7]}], get_secondary_indexes(MD7)),
    MD8 = clear_secondary_indexes(MD7),
    ?assertEqual([], get_secondary_indexes(MD8)),
    MD9 = delete_secondary_index(MD8,{binary_index,"none"}),
    ?assertEqual([], get_secondary_indexes(MD9)),
    MD10 = add_secondary_index(MD9, [{{integer_index,"idx2"}, [23,4,34]}]),
    ?assertEqual([4,23,34], lists:sort(get_secondary_index(MD10,{integer_index,"idx2"}))),
    MD11 = dict:new(),
    ?assertEqual([], get_secondary_indexes(MD11)),
    ?assertEqual(notfound, get_secondary_index(MD11, {binary_index,"none"})),
    ?assertEqual(notfound, get_secondary_index(MD11, {integer_index,"none"})),
    MD12 = set_secondary_index(MD11, [{{integer_index,"itest"}, [12,4,56]},{{binary_index,"btest"}, [<<"test1">>]}]),
    ?assertEqual([4,12,56], lists:sort(get_secondary_index(MD12,{integer_index,"itest"}))),
    ?assertEqual([<<"test1">>], lists:sort(get_secondary_index(MD12,{binary_index,"btest"}))),
    MD13 = add_secondary_index(MD12, [{{integer_index,"itest"}, [4,15,34]},{{binary_index,"btest"}, [<<"test2">>]}]),
    ?assertEqual([4,12,15,34,56], lists:sort(get_secondary_index(MD13,{integer_index,"itest"}))),
    ?assertEqual([<<"test1">>,<<"test2">>], lists:sort(get_secondary_index(MD13,{binary_index,"btest"}))).

-endif.
