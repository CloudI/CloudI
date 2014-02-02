`reltool_util`
==============

Erlang reltool utility functionality application

`release` is an escript which provides the same basic functionality as
`rebar generate`, with additional error reporting coming from the reltool OTP
source code which rebar suppresses.

`scope` is an escript which moves your Erlang application dependencies
into their own "scope", so in Erlang, this means that it adds a prefix to
the application name and all the modules used by the application.  The general
sequence of operations you would want to use with the scope script are: 1) use the `-r` replace mode to modify internal source code, 2) modify the internal source code manually, if necessary, 3) run the `scope` script normally before compilation, 4) if it is necessary to return the source back to the original state, use the `-u` undo mode to remove the scoped source code and replace it with the original source code.  The `scope` script is meant to be part of a build process 
so the dependency modifications should be automatic (if the dependencies
are not convoluted).  Example arguments for normal operation are: `-s cloudi_x_ -p cloudi -b _original -d directory1 -d directory2`.

The `reltool_util` source code provides a simple way to start an Erlang
application manually, outside of a release (which is useful when regression
testing requires that an application be started, especially if the tests are
part of a build process like `rebar eunit` or `rebar ct`).

