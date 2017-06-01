supool: Erlang Process Pool as a Supervisor
===========================================

Purpose
-------

Make it simple to start a single Erlang/OTP supervisor ChildSpec as a pool of
processes with each process selected in round-robin order.

(If you need process pools stored as process heap data,
 see [varpool](https://github.com/okeuday/varpool#readme))

Build
-----

    rebar compile

Test
----

    rebar eunit

Author
------

Michael Truog (mjtruog [at] gmail (dot) com)

License
-------

MIT License

