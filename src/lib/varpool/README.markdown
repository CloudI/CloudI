varpool: Erlang Process Pools as a Local Variable
=================================================

"Let messages ride together to minimize computational costs"

Purpose
-------

There are two main approaches to process pooling in Erlang:

1. Treat the pool as shared data to operate on the pool in ways commonly used
   with imperative programming languages:  A process is removed from the pool,
   the process is used and the process is added back to the same pool
   (interacting with the pool as mutable data).
2. Use the pool with Flow-Based-Programming (FBP) to use Erlang in a simpler
   way:  The pool returns a process without modifying the pool and a
   message is sent to the process to utilize the process.  Either an
   asynchronous or synchronous message is used with the process based on
   the requirements of the process and the effect of the request rate on the
   process message queue.

The #1 approach is unfortunately very common in Erlang source code and is
normally (erroneously) regarded as the correct approach when writing Erlang
source code.  A major negative aspect of the #1 approach is the impact on
fault-tolerance.  How is the process fault-tolerance handled when the process
is removed from the pool (to be used)?  If the process is still linked to the
pool in a way where the pool will restart the process if it crashes while not
being within the pool, it is impossible to isolate the cause of the crash
in the implementation.  The crash could have occurred due to the external
usage of the process or it could have occurred due to an internal error
within the pool source code.  While you can easily argue that a stacktrace
will show you the cause of the error, you will be unable to easily determine
if the stacktrace occurred during or after the error on one side
or the other.  The ambiguity and complexity in the #1 approach makes it
error-prone.

A second major negative aspect of the #1 approach is the scalability impact
when constantly mutating data to get a process from a pool.  As the request
rate increases, the latency associated with altering the pool becomes
more significant.  The extra latency helps to put artifical limits on any
source code that depends on pooling with the #1 approach.

The #2 approach is a more natural fit to the fault-tolerance and
concurrency Erlang provides (the pool handles the process fault-tolerance
and relying on the process message queue utilizes Erlang process concurrency).
The pool does not become a scalability bottleneck while it remains
immutable data.  An additional benefit is that the implementation is simpler
and easier to understand.

The most common example of the #1 approach is probably
[poolboy](https://github.com/devinus/poolboy).  The varpool library
provides the #2 approach as local variable data (for efficient access).

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

