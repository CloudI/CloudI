erlang_cassandra
================

Erlang thrift interface to Cassandra

Liberally takes ideas from
[Louis-Philippe Gauthier's](https://github.com/lpgauth) truly excellent [cassanderl](https://github.com/lpgauth/cassanderl), but with 

- a more recent version of [thrift](https://github.com/dieswaytoofast/thrift) (19.36.0), and [poolboy](https://github.com/devinus/poolboy) instead of [dispcount](https://github.com/ferd/dispcount)

***NOTE***

- There is a one-to-one mapping between keyspaces and pools. If you want something other than the default number of workers for a given keyspace, use ```start_pool``` *first*, and *then* access the keyspace

- This implements complete list of (current) cassandra thrift commands.

- ```CQL``` commands can be sent using the ```execute_cql_command```, ```prepare_cql_command```, and ```execute_prepared_cql_command``` metnds

- pools are garbage-collected when  the keyspace is deleted (or the application is restarted)