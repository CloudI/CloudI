## Dependencies

Binary protocol memcached (version >= 1.3)

## External Documentation

Text Protocol Spec <http://code.sixapart.com/svn/memcached/trunk/server/doc/protocol.txt>

Binary Protocol Spec <http://code.google.com/p/memcached/wiki/MemcacheBinaryProtocol>

## Quick Start

**You must have the binary protocol branch of memcached running as mentioned above**

	$> make
	$> make test
	$> sudo make install
	$> memcached -d -m 1024 -p 11211 -l localhost
	$> memcached -d -m 1024 -p 11121 -l localhost

	1> {ok, Pid} = ememcached:start_link([{"localhost", 11211, 1}, {"localhost", 11121, 1}]).
	{ok,<0.37.0>}

	2> ememcached:stats(Pid).
	[{{"localhost",11211},
	  [{evictions,"0"},
	   {total_items,"0"},
	   {curr_items,"0"},
	   {bytes,"0"},
	   {...}|...]},
	 {{"localhost",11121},
	  [{evictions,"0"},
	   {total_items,"0"},
	   {curr_items,"0"},
	   {bytes,"0"},
	   {...}|...]}]

	3> ememcached:set(Pid, hello, <<"World">>).
	<<>>

	4> ememcached:get(Pid, hello).
	<<"World">>

	5> ememcached:add(Pid, "foo", <<"bar">>).
	<<>>

	6> ememcached:get(Pid, "foo").
	<<"bar">>

## Commands

* **get**(Process::pid(), Key::any()) -> Val::binary()
* **get_many**(Process::pid(), [Key::any()]) -> [Val::binary()]
* **add**(Process::pid(), Key::any(), Val::binary()) -> Response::binary()
* **add_exp**(Process::pid(), Key::any(), Val::binary(), Expiration::integer()) -> Response::binary()
* **set**(Process::pid(), Key::any(), Val::binary()) -> Response::binary()
* **set_exp**(Process::pid(), Key::any(), Val::binary(), Expiration::integer()) -> Response::binary()
* **replace**(Process::pid(), Key::any(), Val::binary()) -> Response::binary()
* **replace_exp**(Process::pid(), Key::any(), Val::binary(), Expiration::integer()) -> Response::binary()
* **delete**(Process::pid(), Key::any()) -> Response::binary()
* **increment_exp**(Process::pid(), Key::any(), Val::binary(), Initial::binary(), Expiration::integer()) -> Response::binary()
* **decrement_exp**(Process::pid(), Key::any(), Val::binary(), Initial::binary(), Expiration::integer()) -> Response::binary()
* **append**(Process::pid(), Key::any(), Val::binary()) -> Response::binary()
* **prepend**(Process::pid(), Key::any(), Val::binary()) -> Response::binary()
* **stat**(Process::pid()) -> [{{Host::string(), Port::integer()}, Response::binary()}]
* **flush**(Process::pid()) -> [{{Host::string(), Port::integer()}, Response::binary()}]
* **flush_exp**(Process::pid(), Expiration::integer()) -> [{{Host::string(), Port::integer()}, Response::binary()}]
* **quit**(Process::pid()) -> [{{Host::string(), Port::integer()}, Response::binary()}]
* **version**(Process::pid()) -> [{{Host::string(), Port::integer()}, Response::binary()}]

## History

The ememcached code was derived from a previous version here <http://github.com/JacobVorreuter/mcerlang>.  However, the ememcached code will not use ets for hash storage since the benefits are negligible/non-existent and create side-effects.

