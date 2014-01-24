# Hello World 5 Example

## PURPOSE

Provide the simplest example of an Erlang application running
CloudI services programmatically (so without a cloudi.conf file)
with a CloudI service (`hello_world5.erl`), using rebar.

## DETAILS

The approach with the Hello World 5 Example is for rebar to build
a release of the `hello_world5` Erlang application using the CloudI
installation on the system at the default installation location
(e.g., ./configure --prefix="/usr/local/").  No cloudi.conf file
is used, but instead services are started and stopped as a dependency of
the `hello_world5` Erlang application.  Each CloudI service that relies on
the generated release for Erlang application loading and starting
(i.e., if a CloudI service is specified as a dependency of
 `hello_world5` in the .app file) needs to have the
service configuration option `automatic_loading` set to false to completely
rely on the release for handling the Erlang service module loading.

Please keep in mind that this approach depends on the Erlang application
dependency hierarchy for the startup/shutdown order when multiple
Erlang applications are starting many CloudI services within a single release.
The release generation will determine the actual order of startup or shutdown
which it will output to its .script file, but as the source code changes the
release may change the order.  So, the changes to the startup order of
CloudI services will be different from the linear order provided within the
single cloudi.conf file.  If CloudI service dependencies aren't accurately
reflected in the Erlang application dependencies in this approach, the system
may fail without failing fast (running the release, after the release
was generate successfully), but this is a problem with Erlang application
dependencies in general.

## USAGE

To use CloudI services programmatically:

    $ rebar compile
    $ rebar generate
    $ cd release
    $ bin/cloudi_hello_world5 start
    $ curl http://localhost:6467/examples/hello_world5
    Hello World!
    $ bin/cloudi_hello_world5 stop

