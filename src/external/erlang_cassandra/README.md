erlang_cassandra
================

Erlang thrift interface to Cassandra

- This is just a tiny shim on the  complete list of (current) cassandra thrift commands, as [listed here](http://wiki.apache.org/cassandra/API10)
  - Yes, the returned data has all sorts of records in it. Sorry. Like I said, its a shim

- Keyspaces correspond to [poolboy](https://github.com/devinus/poolboy) pools which are automagically started for you when you set them.  These pools are clobbered when  the keyspace is deleted (or the application is restarted).

- ```CQL``` commands can be sent using the ```execute_cql_command```, ```prepare_cql_command```, and ```execute_prepared_cql_command``` methods.  Note that if you are using prepared cql commands, you will need to expliclty start a cql_pool and reference it (these have one and only one worker). Check out ```start_cql_pool/1``` for more info.


Liberally takes ideas from
[Louis-Philippe Gauthier's](https://github.com/lpgauth) truly excellent [cassanderl](https://github.com/lpgauth/cassanderl), but with a more recent version of [thrift](https://github.com/dieswaytoofast/thrift) (19.36.0), and [poolboy](https://github.com/devinus/poolboy) instead of [dispcount](https://github.com/ferd/dispcount)

Howto
============

*  Make sure you have Cassandra running.
*  Start up erlang_cassandra

```erlang
erlang_cassandra@paglierino)1> erlang_cassandra:start().
```  

* Create a keyspace

```erlang
(erlang_cassandra@paglierino)2> erlang_cassandra:system_add_keyspace(erlang_cassandra:keyspace_definition(<<"foo">>)).
{ok,<<"098e77e3-5e01-39e9-b840-ba0aba74f743">>}
```


* Set the keyspace for future commands (note that this also starts up a pool of thrift worker-bees)

```erlang
(erlang_cassandra@paglierino)3> erlang_cassandra:set_keyspace(<<"foo">>).
{ok,ok}
```
* Do something

```erlang
(erlang_cassandra@paglierino)4> erlang_cassandra:describe_ring(<<"foo">>).
{ok,[{tokenRange,<<"-4487853658134717437">>,
                 <<"-4487853658134717437">>,
                 [<<"127.0.0.1">>],
                 [<<"127.0.0.1">>],
                 [{endpointDetails,<<"127.0.0.1">>,<<"datacenter1">>,
                                   <<"rack1">>}]}]}
```


Misc
============

* There is a one-to-one mapping between keyspaces and pools. If you want something other than the default number of workers for a given keyspace, use ```start_pool``` *first* with the keyspace_name as an argument, and *then* access the keyspace.  e.g.
 
``` erlang
(erlang_cassandra@paglierino)5> erlang_cassandra:start_pool(<<"bar">>, [{max_size, 10}]).
{ok,<0.120.0>}
(erlang_cassandra@paglierino)6> erlang_cassandra:set_keyspace(<<"bar">>).
{ok,ok}
```

* Take a look at the exports in ```erlang_cassara.erl```
   * In general, whenever you reference a Keyspace (e.g. ```insert(Keyspace, RowKey, ColumnParent, Column, ConsistencyLevel)```), you can use a **Destination** instead of a **Keyspace**.
   * A Destination is simply a triplet, i.e., ```{Host, Port, Keyspace}``` (this is relevant for when you might be running multiple Cassandra instances, though lord knows why you would do that).
   * As such, you could also use ```insert({Host, Port, Keyspace}, RowKey, ColumnParent, Column, ConsistencyLevel)```
   * If you are relying on **Destination**, _always pass in the **Destination** in your commands_. e.g.
       * instead of ```erlang_cassandra:set_keyspace(<<"bar">>).```, use
       * ```erlang_cassandra:set_keyspace({Host, Port, <<"bar">>}, <<"bar">>).```
   
*  For all the thrift commands that _don't_ take a Keyspace (e.g. ```system_update_keyspace```) you can either let the application deduce the Keyspace from the command, or you can explicitly specific it yourself.  e.g. any of the following are acceptable

   * ```system_update_keyspace(KeyspaceDefinition)```
   * ```system_update_keyspace(Keyspace, KeyspaceDefinition)``` 
   * ```system_update_keyspace({Host, Port, Keyspace}, KeyspaceDefinition)``` 