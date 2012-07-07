erlzmq2
=======
NIF based Erlang bindings for the ZeroMQ messaging library.

Copyright (c) 2011 Yurii Rashkovskii, Evax Software and Michael Truog

Overview
--------

The erlzmq2 application provides high-performance NIF based Erlang
bindings for the ZeroMQ messaging library.

Downloading
-----------

The erlzmq2 source code can be found on
[GitHub](https://github.com/zeromq/erlzmq2)

    $ git clone http://github.com/zeromq/erlzmq2.git

Building
--------

Please note that to behave properly on your system ZeroMQ might
require [some tuning](http://www.zeromq.org/docs:tuning-zeromq).

Build the code

    $ make

If you want to build against a specific version of ZeroMQ in the 3.1
series (not `v3.1.0`), use this:

    $ ZEROMQ_VERSION=v<VERSION> make

Be aware that this will almost assuredly not work correctly for any
versions of zeromq that are not in the 3.1 series.

Build the docs

    $ make docs

Run the test suite

    $ make test

Run the benchmarks (requires [python](http://www.python.org) and
[matplotlib](http://matplotlib.sourceforge.net/))

    $ make bench

This will run performance tests and output png graphs in the graphs
directory.

Architecture
------------

The bindings use Erlang's
[NIF (native implemented functions)](http://www.erlang.org/doc/man/erl_nif.html)
interface to achieve the best performance. One extra OS thread and one
pair of inproc sockets by context are used to simulate blocking recv
calls without affecting the Erlang virtual machine's responsiveness.

License
-------

The project is released under the MIT license.

