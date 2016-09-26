# Hello World 5 Example

## PURPOSE

Provide the simplest examples of an Erlang application running
CloudI services programmatically (so without a cloudi.conf file)
with a CloudI service (`hello_world5.erl`), using rebar (rebar version 2.x).

## DETAILS

The approach with the Hello World 5 Example is for rebar to build a release
of the `hello_world5` Erlang application using either CloudI as a remote
dependency for Erlang/Elixir-only development or as
a local dependency from the default system installation location
(e.g., ./configure --prefix="/usr/local/").  No cloudi.conf file
is used, but instead services are started and stopped as a dependency of
the `hello_world5` Erlang application.  Each CloudI service that relies on
the generated release for Erlang application loading and starting
(i.e., if a CloudI service is specified as a dependency of
 `hello_world5` in the .app file) needs to have the
service configuration option `automatic_loading` set to false to completely
rely on the release for handling the Erlang service module loading.

If more Erlang CloudI services were added to the `hello_world5` Erlang
application, any of the service modules with names that do not equal
`hello_world5` would set the `application_name` service configuration option
to make sure the application ownership is clear
(e.g., for `application:get_env/1`).

Please keep in mind that this approach depends on the Erlang application
dependency hierarchy for the startup/shutdown order when multiple
Erlang applications are starting many CloudI services within a single release.
The release generation will determine the actual order of startup or shutdown
which it will output to its .script/.boot file, but as the source code changes
the release may change the order.  So, the changes to the startup order of
CloudI services will be different from the sequential order provided within the
single cloudi.conf file.  If CloudI service dependencies aren't accurately
reflected in the Erlang application dependencies in this approach, the system
may fail without failing fast due to service startup order being ambiguous,
but this is a problem with Erlang application dependencies in general.
This problem is more obvious as more Erlang applications utilize CloudI
services that have dependencies on each other and can be avoided by switching
to the single cloudi.conf file with the same CloudI service modules.

The usage of the service configuration option `automatic_loading` set to false
in this example depends on the release being ran with `-mode embedded` as is
typically done when running an Erlang release.  If the `-mode` command line
argument is omitted or is provided as `-mode interactive` then
`automatic_loading` can be set to true or omitted to receive true as the
default setting.  However, it is irregular to attempt using `-mode interactive`
when running an Erlang release since it does not pursue fail-fast operation
with a production deployment.

## USAGE

To use CloudI services with CloudI as a remote dependency:

    $ cp rebar.config.remote_dependency rebar.config
    $ cp reltool.config.remote_dependency reltool.config
    $ rebar get-deps compile generate
    $ cd release
    $ bin/cloudi_hello_world5 start
    $ curl http://localhost:6464/examples/hello_world5
    Hello World!
    $ bin/cloudi_hello_world5 stop

To use CloudI services with CloudI as a local installation:

    $ cp rebar.config.local_install rebar.config
    $ cp reltool.config.local_install reltool.config
    $ rebar compile generate
    $ cd release
    $ bin/cloudi_hello_world5 start
    $ curl http://localhost:6464/examples/hello_world5
    Hello World!
    $ bin/cloudi_hello_world5 stop

