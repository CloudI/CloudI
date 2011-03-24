erlzmq2
====
NIF based Erlang bindings for the ZeroMQ messaging library.

Copyright (c) 2011 Yurii Rashkovskii, Michael Truog and Evax Software

Overview
========

The erlzmq2 application provides high-performance NIF based Erlang bindings
for the ZeroMQ messaging library.

Downloading
===========

The erlzmq2 source code can be found on [GitHub](https://github.com/yrashk/erlzmq2)

    $ git clone http://github.com/yrashk/erlzmq2.git

It is also available on [Agner](http://erlagner.org/):

    $ agner build erlzmq

In order to build erlzmq2 against a specific version of ZeroMQ (not `master`), use this:

    $ ZEROMQ_VERSION=v<VERSION> agner build erlzmq

Building
========

Build the code

    $ make

If you want to build against a specific version of ZeroMQ (not `master`), use this:

    $ ZEROMQ_VERSION=v<VERSION> make

Build the docs

    $ make docs

Run the test suite

    $ make test

Please note that to behave properly on your system ZeroMQ might require [some tuning](http://www.zeromq.org/docs:tuning-zeromq).

Architecture
============

The bindings use Erlang's [NIF (native implemented functions)](http://www.erlang.org/doc/man/erl_nif.html) interface to achieve the best performance. One extra OS thread and one pair of inproc sockets by context are used to simulate blocking recv calls without affecting the Erlang virtual machine's responsiveness.

License
=======

The project is released under the MIT license.

