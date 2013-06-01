Erlang 2-way Set Associative Map
================================

An interface is provided which takes an Erlang data structure module name
that contains a dict API to provide a 2-way set associate map.  The resulting
data structure would be used when 2 different key values need to refer to the
same value (e.g., using both a UUID and an Erlang pid to reference
configuration data).

Author
------

Michael Truog (mjtruog [at] gmail (dot) com)

License
-------

BSD
