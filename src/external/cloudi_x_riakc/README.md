<p align="right">
    <img src="http://wiki.basho.com/images/riaklogo.png">
</p>
# Riak Protocol Buffers Client Introduction

[![Build Status](https://secure.travis-ci.org/basho/riak-erlang-client.png?branch=master)](http://travis-ci.org/basho/riak-erlang-client)

This document assumes that you have already started your Riak cluster.
For instructions on that prerequisite, refer to
[Installation and Setup](https://wiki.basho.com/Installation-and-Setup.html)
in the [Riak Wiki](https://wiki.basho.com). You can also view the Riak Erlang Client EDocs [here](http://basho.github.com/riak-erlang-client/).

Dependencies
=========

To build the riak-erlang-client you will need Erlang OTP R13B04 or later, and Git.

Debian
------

On a Debian based system (Debian, Ubuntu, ...) you will need to make sure that certain packages are installed:

    # apt-get install erlang-parsetools erlang-dev erlang-syntax-tools


Installing
=========

        $ git clone git://github.com/basho/riak-erlang-client.git
        $ cd riak-erlang-client
        $ make


Connecting
=======

To talk to riak, all you need is an Erlang node with the riak-erlang-client library (riakc) in its code path.

        $ erl -pa $PATH_TO_RIAKC/ebin $PATH_TO_RIAKC/deps/*/ebin


You'll know you've done this correctly if you can execute the following commands and get a path to a beam file, instead of the atom 'non_existing':

       1> code:which(riakc_pb_socket).
       ".../riak-erlang-client/ebin/riakc_pb_socket.beam"


Once you have your node running, pass your Riak server nodename to `riakc_pb_socket:start_link/2` to connect and get a client. This can be as simple as:

       1> {ok, Pid} = riakc_pb_socket:start_link("127.0.0.1", 8087).
       {ok,<0.56.0>}

Verify connectivity with the server using `ping/1`.

       2> riakc_pb_socket:ping(Pid).
       pong


Storing New Data
=========

Each bit of data in Riak is stored in a "bucket" at a "key" that is unique to that bucket. The bucket is intended as an organizational aid, for example to help segregate data by type, but Riak doesn't care what values it stores, so choose whatever scheme suits you. Buckets, keys and values are all binaries.

Before storing your data, you must wrap it in a riakc_obj:

    3> Object = riakc_obj:new(<<"groceries">>, <<"mine">>, <<"eggs & bacon">>).
    {riakc_obj,<<"groceries">>,<<"mine">>,undefined,undefined,
    {dict,0,16,16,8,80,48,
    {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],...},
    {{[],[],[],[],[],[],[],[],[],[],[],[],[],...}}},
    <<"eggs & bacon">>}

If you want to have the server generate you a key (similar to the REST API) pass the atom `undefined` as the second parameter to new().

The Object refers to a key `<<"mine">>` in a bucket named `<<"groceries">>` with the value `<<"eggs & bacon">>`. Using the client you opened earlier, store the object:

    5> riakc_pb_socket:put(Pid, Object).
    ok

If the return value of the last command was anything but the atom `ok` (or `{ok, Key}` when you instruct the server to generate the key), then the store failed. The return value may give you a clue as to why the store failed, but check the Troubleshooting section below if not.

The object is now stored in Riak. `put/2` uses default parameters for storing the object. There is also a `put/3` call that takes a proplist of options.

<table border="1">
    <th>Option</th>
    <th>Description</th>
    <tr>
        <td><code>{w, W}</code></td>
        <td>the minimum number of nodes that must respond with success for the write to be considered successful. The default is currently set on the server at 2</td>
    </tr>
    <tr>
        <td><code>{dw, DW}</code></td>
        <td>  the minimum number of nodes that must respond with success * *after durably storing* the object for the write to be considered successful. The default is currently set on the server at 0. </td>
    </tr>
    <tr>
        <td><code>return_body </code></td>
        <td> immediately do a get after the put and return a
        riakc_obj.</td>
    </tr>
</table>

    6> AnotherObject = riakc_obj:new(<<"my bucket">>, <<"my key">>, <<"my binary data">>).
    7> riakc_pb_socket:put(Pid, AnotherObject, [{w, 2}, {dw, 1}, return_body]).
    {ok,{riakc_obj,<<"my bucket">>,<<"my key">>,
    <<107,206,97,96,96,96,206,96,202,5,82,44,140,62,169,115,
    50,152,18,25,243,88,25,...>>,
    [{{dict,2,16,16,8,80,48,
    {[],[],[],[],[],[],[],[],[],[],[],[],...},
    {{[],[],[],[],[],[],[],[],[],[],...}}},
    <<"my binary data">>}],
    {dict,0,16,16,8,80,48,
    {[],[],[],[],[],[],[],[],[],[],[],[],[],...},
    {{[],[],[],[],[],[],[],[],[],[],[],...}}},
    undefined}}

Would make sure at least two nodes responded successfully to the put and at least one node has durably stored the value and an updated object is returned.

See [riak/doc/architecture.txt](https://github.com/basho/riak/blob/master/doc/architecture.txt) for more information about W and DW
values.


Fetching Data
==================

At some point you'll want that data back. Using the same bucket and key you used before:

    8> {ok, O} = riakc_pb_socket:get(Pid, <<"groceries">>, <<"mine">>).
    {ok,{riakc_obj,<<"groceries">>,<<"mine">>,
    <<107,206,97,96,96,96,204,96,202,5,82,44,12,143,167,115,
    103,48,37,50,230,177,50,...>>,
    [{{dict,2,16,16,8,80,48,
    {[],[],[],[],[],[],[],[],[],[],[],[],...},
    {{[],[],[],[],[],[],[],[],[],[],...}}},
    <<"eggs & bacon">>}],
    {dict,0,16,16,8,80,48,
    {[],[],[],[],[],[],[],[],[],[],[],[],[],...},
    {{[],[],[],[],[],[],[],[],[],[],[],...}}},
    undefined}}

Like `put/3`, there is a `get/4` function that takes options.

<table border="1">
    <th>Option</th>
    <th>Description</th>
    <tr>
        <td><code>{r, R}</code></td>
        <td>the minimum number of nodes that must respond with success for the read to be considered successfu2</td>
    </tr>
</table>

If the data was originally stored using the distributed erlang client (riak_client), the server
will automatically term_to_binary/1 the value before sending it, with the content
type set to application/x-erlang-binary (replacing any user-set value).  The application is
responsible for calling binary_to_term to access the content and calling term_to_binary
when modifying it.

Modifying Data
==================

Say you had the "grocery list" from the examples above, reminding you to get `<<"eggs & bacon">>`, and you want to add `<<"milk">>` to it. The easiest way is:

    9> {ok, Oa} = riakc_pb_socket:get(Pid, <<"groceries">>, <<"mine">>).
    ...
    10> Ob = riakc_obj:update_value(Oa, <<"milk, ", (riakc_obj:get_value(Oa))/binary>>).
    11> {ok, Oc} = riakc_pb_socket:put(Pid, Ob, [return_body]).
    {ok,{riakc_obj,<<"groceries">>,<<"mine">>,
    <<107,206,97,96,96,96,206,96,202,5,82,44,12,143,167,115,
    103,48,37,50,230,177,50,...>>,
    [{{dict,2,16,16,8,80,48,
    {[],[],[],[],[],[],[],[],[],[],[],[],...},
    {{[],[],[],[],[],[],[],[],[],[],...}}},
    <<"milk, eggs & bacon">>}],
    {dict,0,16,16,8,80,48,
    {[],[],[],[],[],[],[],[],[],[],[],[],[],...},
    {{[],[],[],[],[],[],[],[],[],[],[],...}}},
    undefined}}


That is, fetch the object from Riak, modify its value with `riakc_obj:update_value/2`, then store the modified object back in Riak. You can get your updated object to convince yourself that your list is updated:

Deleting Data
==================

Throwing away data is quick and simple: just use the `delete/3` function.

    10> riakc_pb_socket:delete(Pid, <<"groceries">>, <<"mine">>).
    ok

As with get and put, delete can also take options

<table border="1">
    <th>Option</th>
    <th>Description</th>
    <tr>
        <td><code>{rw, RW}</code></td>
        <td>the number of nodes to wait for responses from</td>
    </tr>
</table>

Issuing a delete for an object that does not exist returns just returns ok.

Encoding
==================

The initial release of the erlang protocol buffers client treats all values as binaries. The caller needs to make sure data is serialized and deserialized correctly. The content type stored along with the object may be used to store the encoding. For example

    decode_term(Object) ->
      case riakc_obj:get_content_type(Object) of
        <<"application/x-erlang-term">> ->
          try
            {ok, binary_to_term(riakc_obj:get_value(Object))}
          catch
            _:Reason ->
              {error, Reason}
          end;
        Ctype ->
          {error, {unknown_ctype, Ctype}}
      end.

    encode_term(Object, Term) ->
      riakc_obj:update_value(Object, term_to_binary(Term, [compressed]),
      <<"application/x-erlang-term">>).

Siblings
==================

If a bucket is configured to allow conflicts (allow_mult=true) then the result object may contain more than one result. The number of values can be returned with

    1> riakc_obj:value_count(Obj).
    2

The values can be listed with

    2> riakc_obj:get_values(Obj).
    \[<<"{\"k1\":\"v1\"}">>,<<"{\"k1\":\"v2\"}">>\]

And the content types as

    3> riakc_obj:get_content_types(Obj).
    []

If resolution simply requires one of the existing siblings to be selected, this can be done through the `riakc_obj:select_sibling` function. This function updates the record with the value and metadata of the selected Nth sibling.

It is also possible to get a list of tuples representing all the siblings through the `riakc_obj:get_contents` function. This returns a list of tuples in the form `{metadata(), value()}` which can be used when more complex sibling resolution is required.

Once the correct combination of metadata and value has been determined, the record can be updated with these using the `riakc_obj:update_value` and `riakc_obj:update_metadata` functions. If the resulting content type needs to be updated, the `riakc_obj:update_content_type` can be used.   

Listing Keys
=============

Most uses of key-value stores are structured in such a way that requests know which keys they want in a bucket. Sometimes, though, it's necessary to find out what keys are available (when debugging, for example). For that, there is list_keys:

    1> riakc_pb_socket:list_keys(Pid, <<"groceries">>).
    {ok,[<<"mine">>]}

Note that keylist updates are asynchronous to the object storage primitives, and may not be updated immediately after a put or delete. This function is primarily intended as a debugging aid.

`list_keys/2` is just a convenience function around the streaming version of the call `stream_list_keys(Pid, Bucket)`.

    2> riakc_pb_socket:stream_list_keys(Pid, <<"groceries">>).
    {ok,87009603}
    3> receive Msg1 \-> Msg1 end.
    {87009603,{keys,[]}}
    4> receive Msg2 \-> Msg2 end.
    {87009603,done}

See [`riakc_pb_socket:wait_for_listkeys`](https://github.com/basho/riak-erlang-client/blob/master/src/riakc_pb_socket.erl#L1087) for an example of receiving.

Bucket Properties
==================

Bucket properties can be retrieved and modified using `get_bucket/2` and `set_bucket/3`. The bucket properties are represented as a proplist. Only a subset of the properties can be retrieved and set using the protocol buffers interface - currently only n_val and allow_mult.

Here's an example of getting/setting properties

    3> riakc_pb_socket:get_bucket(Pid, <<"groceries">>).
    {ok,[{n_val,3},{allow_mult,false}]}
    4> riakc_pb_socket:set_bucket(Pid, <<"groceries">>, [{n_val, 5}]).
    ok
    5> riakc_pb_socket:get_bucket(Pid, <<"groceries">>).
    {ok,[{n_val,5},{allow_mult,false}]}
    6> riakc_pb_socket:set_bucket(Pid, <<"groceries">>, [{n_val, 7}, {allow_mult, true}]).
    ok
    7> riakc_pb_socket:get_bucket(Pid, <<"groceries">>).
    {ok,[{n_val,7},{allow_mult,true}]}


User Metadata
==================

User metadata are stored in the object metadata dictionary, and can be manipulated by using the `get_user_metadata_entry/2`, `get_user_metadata_entries/1`, `clear_user_metadata_entries/1`, `delete_user_metadata_entry/2` and `set_user_metadata_entry/2` functions.

These functions act upon the dictionary retuened by the `get_metadata/1`, `get_metadatas/1` and `get_update_metadata/1` functions.

The following example illustrates setting and getting metadata.

    %% Create new object
    13> Object = riakc_obj:new(<<"test">>, <<"usermeta">>, <<"data">>).
    {riakc_obj,<<"test">>,<<"usermeta">>,undefined,[],undefined,
           <<"data">>}
    14> MD1 = riakc_obj:get_update_metadata(Object).
    {dict,0,16,16,8,80,48,
      {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
      {{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]}}}
    15> riakc_obj:get_user_metadata_entries(MD1).
    []
    16> MD2 = riakc_obj:set_user_metadata_entry(MD1,{<<"Key1">>,<<"Value1">>}).
    {dict,1,16,16,8,80,48,
      {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
      {{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],
        [[<<"X-Ri"...>>,{...}]]}}}
    17> MD3 = riakc_obj:set_user_metadata_entry(MD2,{<<"Key2">>,<<"Value2">>}).  
    {dict,1,16,16,8,80,48,
      {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
      {{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],
        [[<<"X-Ri"...>>,{...}|...]]}}}
    18> riakc_obj:get_user_metadata_entry(MD3, <<"Key1">>).
    <<"Value1">>
    19> MD4 = riakc_obj:delete_user_metadata_entry(MD3, <<"Key1">>).
    {dict,1,16,16,8,80,48,
      {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
      {{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],
        [[<<"X-Ri"...>>,{...}]]}}}
    20> riakc_obj:get_user_metadata_entries(MD4).
    [{<<"Key2">>,<<"Value2">>}]
    %% Store updated metadata back to the object 
    21> Object2 = riakc_obj:update_metadata(Object,MD4).
    {riakc_obj,<<"test">>,<<"usermeta">>,undefined,[],
           {dict,1,16,16,8,80,48,
                 {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],...},
                 {{[],[],[],[],[],[],[],[],[],[],[],[],[],...}}},
           <<"data">>}
    22> riakc_pb_socket:put(Pid, Object2).
    ok
    23> {ok, O1} = riakc_pb_socket:get(Pid, <<"test">>, <<"usermeta">>).
    {ok,{riakc_obj,<<"test">>,<<"usermeta">>,
               <<107,206,97,96,96,96,204,96,202,5,82,28,202,156,255,126,
                 6,220,157,173,153,193,148,...>>,
               [{{dict,3,16,16,8,80,48,
                       {[],[],[],[],[],[],[],[],[],[],[],[],...},
                       {{[],[],[],[],[],[],[],[],[],[],...}}},
                 <<"data">>}],
               undefined,undefined}}
    24> riakc_obj:get_user_metadata_entries(riakc_obj:get_update_metadata(O1)).
    [{<<"Key2">>,<<"Value2">>}]


Secondary Indexes
==================

Secondary indexes are set through the object metadata dictionary, and can be manipulated by using the `get_secondary_index/2`, `get_secondary_indexes/1`, `clear_secondary_indexes/1`, `delete_secondary_index/2`, `set_secondary_index/2` and `add_secondary_index/2` functions. These functions act upon the dictionary retuened by the `get_metadata/1`, `get_metadatas/1` and `get_update_metadata/1` functions.

When using these functions, secondary indexes are identified by a tuple, `{binary_index, string()}` or `{integer_index, string()}`, where the string is the name of the index. `{integer_index, "id"}` therefore corresponds to the index "id_int". As secondary indexes may have more than one value, the index values are specified as lists of integers or binaries, depending on index type.

The following example illustrates getting and setting secondary indexes.

    %% Create new object
    13> Obj = riakc_obj:new(<<"test">>, <<"2i_1">>, <<"John Robert Doe, 25">>).
    {riakc_obj,<<"test">>,<<"2i_1">>,undefined,[],undefined,
           <<"John Robert Doe, 25">>}
    14> MD1 = riakc_obj:get_update_metadata(Obj).
    {dict,0,16,16,8,80,48,
      {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
      {{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]}}}
    15> MD2 = riakc_obj:set_secondary_index(MD1, [{{integer_index, "age"}, [25]},{{binary_index, "name"}, [<<"John">>,<<"Doe">>]}]).
    {dict,1,16,16,8,80,48,
      {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
      {{[],[],[],[],[],[],[],[],[],[],[],
        [[<<"index">>,
          {<<"name_bin">>,<<"Doe">>},
          {<<"name_bin">>,<<"John">>},
          {<<"age_"...>>,<<...>>}]],
        [],[],[],[]}}}
    16> riakc_obj:get_secondary_index(MD2, {binary_index, "name"}).
    [<<"Doe">>,<<"John">>]
    17> MD3 = riakc_obj:add_secondary_index(MD2, [{{binary_index, "name"}, [<<"Robert">>]}]).
    {dict,1,16,16,8,80,48,
      {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
      {{[],[],[],[],[],[],[],[],[],[],[],
        [[<<"index">>,
          {<<"name_bin">>,<<"Doe">>},
          {<<"name_bin">>,<<"John">>},
          {<<"age_"...>>,<<...>>},
          {<<...>>,...}]],
        [],[],[],[]}}}
    18> riakc_obj:get_secondary_indexes(MD3).
    [{{binary_index,"name"},[<<"Doe">>,<<"John">>,<<"Robert">>]},{{integer_index,"age"},[25]}]
    19> Obj2 = riakc_obj:update_metadata(Obj,MD3).
    {riakc_obj,<<"test">>,<<"2i_1">>,undefined,[],
           {dict,1,16,16,8,80,48,
                 {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],...},
                 {{[],[],[],[],[],[],[],[],[],[],[],[[...]],[],...}}},
           <<"John Robert Doe, 25">>}    
    20> riakc_pb_socket:put(Pid, Obj2).
    
In order to query based on secondary indexes, the `riakc_pb_socket:get_index/4`, `riakc_pb_socket:get_index/5`, `riakc_pb_socket:get_index/6` and `riakc_pb_socket:get_index/7` functions can be used. These functions also allows secondary indexes to be specifiued using the tuple described above.

The following example illustrates how to perform exact match as well as range queries based on the record and associated indexes created above.
    
    21> riakc_pb_socket:get_index(Pid, <<"test">>, {binary_index, "name"}, <<"John">>).
    {ok,[<<"2i_1">>]}
    22> riakc_pb_socket:get_index(Pid, <<"test">>, {integer_index, "age"}, 20, 30).
    {ok,[<<"2i_1">>]}

Links
=====

Links are also stored in the object metadata dictionary, and can be manipulated by using the `get_links/2`, `get_all_links/1`, `clear_links/1`, `delete_links/2`, `set_link/2` and `add_link/2` functions. When using these functions, a link is identified by a tag, and may therefore contain multiple record IDs.

These functions act upon the dictionary retuened by the `get_metadata/1`, `get_metadatas/1` and `get_update_metadata/1` functions.

The following example illustrates setting and getting links.

    %% Create new object
    10> Obj = riakc_obj:new(<<"person">>, <<"sarah">>, <<"Sarah, 30">>).
    {riakc_obj,<<"person">>,<<"sarah">>,undefined,[],undefined,
           <<"Sarah, 30">>}
    11> MD1 = riakc_obj:get_update_metadata(Obj).
    {dict,0,16,16,8,80,48,
      {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
      {{[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]}}}
    12> riakc_obj:get_all_links(MD1).
    []
    13> MD2 = riakc_obj:set_link(MD1, [{<<"friend">>, [{<<"person">>,<<"jane">>},{<<"person">>,<<"richard">>}]}]).
    {dict,1,16,16,8,80,48,
      {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
      {{[],[],
        [[<<"Links">>,
          {{<<"person">>,<<"jane">>},<<"friend">>},
          {{<<"person">>,<<"richard">>},<<"friend">>}]],
        [],[],[],[],[],[],[],[],[],[],[],[],[]}}}
    14> MD3 = riakc_obj:add_link(MD2, [{<<"sibling">>, [{<<"person">>,<<"mark">>}]}]).
    {dict,1,16,16,8,80,48,
      {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]},
      {{[],[],
        [[<<"Links">>,
          {{<<"person">>,<<"jane">>},<<"friend">>},
          {{<<"person">>,<<"richard">>},<<"friend">>},
          {{<<"person">>,<<"mark">>},<<"sibling">>}]],
        [],[],[],[],[],[],[],[],[],[],[],[],[]}}}
    15> riakc_obj:get_all_links(MD3).
    [{<<"friend">>,
        [{<<"person">>,<<"jane">>},{<<"person">>,<<"richard">>}]},
         {<<"sibling">>,[{<<"person">>,<<"mark">>}]}]
    16> Obj2 = riakc_obj:update_metadata(Obj,MD3).
    {riakc_obj,<<"person">>,<<"sarah">>,undefined,[],
           {dict,1,16,16,8,80,48,
                 {[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],...},
                 {{[],[],
                   [[<<"Links">>,
                     {{<<"person">>,<<"jane">>},<<"friend">>},
                     {{<<"person">>,<<"richard">>},<<"friend">>},
                     {{<<"person">>,<<"mark">>},<<"sibling">>}]],
                   [],[],[],[],[],[],[],[],[],[],...}}},
           <<"Sarah, 30">>}
    17> riakc_pb_socket:put(Pid, Obj2).
    ok

MapReduce
=========

MapReduce jobs can be executed using the `riakc_pb_socket:mapred` function. This takes an input specification as well as a list of mapreduce phase specifications as arguments. It also allows a non-default timeout to be specified if required.

The function `riakc_pb_socket:mapred` uses `riakc_pb_socket:mapred_stream` under the hood, and if results need to be processed as they are streamed to the client, this function can be used instead. The implementation of `riakc_pb_socket:mapred` provides a good example of how to implement this.

It is possible to define a wide range of inputs for a mapreduce job. Some examples are given below: 

**Bucket/Key list:** `[{<<"bucket1">>,<<"key1">>},{<<"bucket1">>,<<"key2">>}]`

**All keys in a bucket:** `<<"bucket1">>`

**Result of exact secondary index match:** `{index, <<"bucket1">>, {binary_index, "idx"}, <<"key">>}`, `{index, <<"bucket1">>, <<"idx_bin">>, <<"key">>}`

**Result of secondary index range query:** `{index, <<"bucket1">>, {integer_index, "idx"}, 1, 100}`, `{index, <<"bucket1">>, <<"idx_int">>, <<"1">>, <<"100">>}`

The query is given as a list of `map`, `reduce` and `link` phases. Map and reduce phases are each expressed as tuples in the following form:

`{Type, FunTerm, Arg, Keep}`

Type is an atom, either map or reduce. Arg is a static argument (any Erlang term) to pass to each execution of the phase. Keep is either true or false and determines whether results from the phase will be included in the final value of the query. Riak assumes the final phase will return results.

`FunTerm` is a reference to the function that the phase will execute and takes any of the following forms:

`{modfun, Module, Function}` where Module and Function are atoms that name an Erlang function in a specific module.

`{qfun,Fun}` where Fun is a callable fun term (closure or anonymous function).

`{jsfun,Name}` where Name is a binary that, when evaluated in Javascript, points to a built-in Javascript function.

`{jsanon, Source}` where Source is a binary that, when evaluated in Javascript is an anonymous function.

`{jsanon, {Bucket, Key}}` where the object at {Bucket, Key} contains the source for an anonymous Javascript function.

Below are a few examples of different types of mapreduce queries. These assume that the following test data has been created:

**Test Data**

Create two test records in the `<<"mr">>` bucket with secondary indexes and a link as follows:

    12> O1 = riakc_obj:new(<<"mr">>, <<"bob">>, <<"Bob, 26">>).
    13> M0 = dict:new().
    14> M1 = riakc_obj:set_secondary_index(M0, {{integer_index,"age"}, [26]}).
    15> O2 = riakc_obj:update_metadata(O1, M1).
    16> riakc_pb_socket:put(Pid, O2).
    17> O3 = riakc_obj:new(<<"mr">>, <<"john">>, <<"John, 23">>).
    18> M2 = riakc_obj:set_secondary_index(M0, {{integer_index,"age"}, [23]}).
    19> M3 = riakc_obj:set_link(M2, [{<<"friend">>, [{<<"mr">>,<<"bob">>}]}]).
    20> O4 = riakc_obj:update_metadata(O3, M3).
    21> riakc_pb_socket:put(Pid, O4).

**Example 1: Link Walk**

Get all friends linked to *john* in the *mr* bucket.

    6> {ok, [{N1, R1}]} = riakc_pb_socket:mapred(Pid,[{<<"mr">>, <<"john">>}],[{link, <<"mr">>, <<"friend">>, true}]).
    {ok,[{0,[[<<"mr">>,<<"bob">>,<<"friend">>]]}]}

As expected, the link information for `bob` is returned.

**Example 2: Determine total object size using a qfun**

Create a qfun that returns the size of the record and feed this into the existing reduce function `riak_kv_mapreduce:reduce_sum` to get total size.

    6> RecSize = fun(G, _, _) -> [byte_size(riak_object:get_value(G))] end.
    #Fun<erl_eval.18.82930912>
    7> {ok, [{N2, R2}]} = riakc_pb_socket:mapred(Pid,
                {index, <<"mr">>, {integer_index, "age"}, 20, 30},
                [{map, {qfun, RecSize}, none, false},
                 {reduce, {modfun, 'riak_kv_mapreduce', 'reduce_sum'}, none, true}]).
    {ok,[{1,[15]}]}
 
 As expected, total size of data is 15 bytes.   

Troubleshooting
==================

If `start/2` or `start_link/2` return `{error,econnrefused}` the client could not connect to the server - make sure the protocol buffers interface is enabled on the server and the address/port is correct.
