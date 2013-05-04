`reltool_util`
==============

Erlang reltool utility functionality application

`release` is an escript which provides the same basic functionality as
`rebar generate`, with additional error reporting coming from the reltool OTP
source code which rebar suppresses.

The `reltool_util` source code provides a simple way to start an Erlang
application manually, outside of a release (which is useful when regression
testing requires that an application be started, especially if the tests are
part of a build process like `rebar eunit` or `rebar ct`).

