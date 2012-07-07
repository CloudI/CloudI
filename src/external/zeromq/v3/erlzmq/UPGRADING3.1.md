Upgrading From 2.1 to 3.1
=========================

See the general upgrading guidelines [here](http://www.zeromq.org/docs:3-1-upgrade).

Things to watchout for in erlzmq2
---------------------------------

The 'timeout' flag has been removed from the send receive flags. In
3.1 the native zeromq `sndtimeo` and `rcvtimeo` flags where added. You
should use these instead.

Also, is in zeromq as a whole the hwm flag has been replaced with
sndhwm and rcvhwm.

Things not to worry about
-------------------------

The rest of the things in that upgrade guide can be ignored. The
erlzmq2 nif binding abstracts that away for you.

