erlang_cassandra
================

Erlang thrift interface to Cassandra

- This is just a tiny shim on the  complete list of (current) cassandra thrift commands, as [listed here](http://wiki.apache.org/cassandra/API10)
  - Yes, the returned data has all sorts of records in it. Sorry. Like I said, its a shim

- ```CQL``` commands can be sent using the ```execute_cql_command```, ```prepare_cql_command```, and ```execute_prepared_cql_command``` metnds

- pools are garbage-collected when  the keyspace is deleted (or the application is restarted)


Liberally takes ideas from
[Louis-Philippe Gauthier's](https://github.com/lpgauth) truly excellent [cassanderl](https://github.com/lpgauth/cassanderl), but with a more recent version of [thrift](https://github.com/dieswaytoofast/thrift) (19.36.0), and [poolboy](https://github.com/devinus/poolboy) instead of [dispcount](https://github.com/ferd/dispcount)

***NOTE***

There is a one-to-one mapping between keyspaces and pools. If you want something other than the default number of workers for a given keyspace, use ```start_pool``` *first*, and *then* access the keyspace, i.e.

HOWTO
============

1. Make sure you have Cassandra running.
2. Start up a erlang_cassandra
```erlang
erlang_cassandra@paglierino)1> erlang_cassandra:start().
```  
3. Take a look at the exports in ```erlang_cassara.erl```.  In general, whenever you reference a Keyspace (e.g. ```insert(Keyspace, RowKey, ColumnParent, Column, ConsistencyLevel)```), you can use a **Destination** instead of a **Keyspace**.
A Destination is simply a triplet, i.e., ```{Host, Port, Keyspace}``` (this is relevant for when you might be running multiple Cassandra instances, though lord knows why you would do that). So, you would say ```insert({Host, Port, Keyspace}, RowKey, ColumnParent, Column, ConsistencyLevel)```
4. For all the thrift commands that _don't_ take a Keyspace (e.g. ```system_update_keyspace```) you can either let the application deduce the Keyspace from the command, or you can explicitly specific it yourself.  e.g. both ```system_update_keyspace(KeyspaceDefinition)```,  ```system_update_keyspace(Keyspace, KeyspaceDefinition)``` and ```system_update_keyspace({Host, Port, Keyspace}, KeyspaceDefinition)``` are acceptable