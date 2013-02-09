## CloudI Quick Start

### Install Quick Start Tools

* Ubuntu: apt-get install wget curl
* OSX: port install wget curl

### Get CloudI Running

    $ wget http://sourceforge.net/projects/cloudi/files/latest/download -O cloudi-1.2.0.tar.gz
    $ tar zxvf cloudi-1.2.0.tar.gz
    $ cd cloudi-1.2.0/src
    $ ./configure
    $ make
    $ sudo make install
    $ cd ../..
    $ sudo cloudi start

The CloudI tests are now running and consuming your available CPUs.

The Quick Start guide below shows how to create both an internal (Erlang) CloudI service and an external (Python) CloudI service.

### Create an Internal (Erlang) CloudI Service

    $ mkdir cloudi-quickstart
    $ cd cloudi-quickstart
    $ cat << EOF > hello_world.erl
    -module(hello_world).
    -behaviour(cloudi_service).

    %% cloudi_service callbacks
    -export([cloudi_service_init/3,
             cloudi_service_handle_request/11,
             cloudi_service_handle_info/3,
             cloudi_service_terminate/2]).
    
    -include_lib("cloudi_core/include/cloudi_logger.hrl").
    
    -record(state,
        {
        }).
    
    cloudi_service_init(_Args, _Prefix, Dispatcher) ->
        cloudi_service:subscribe(Dispatcher, "hello_world/get"),
        {ok, #state{}}.
    
    cloudi_service_handle_request(_Type, _Name, _Pattern, _RequestInfo, _Request,
                                  _Timeout, _Priority, _TransId, _Pid,
                                  #state{} = State, _Dispatcher) ->
        {reply, <<"Hello World!">>, State}.
    
    cloudi_service_handle_info(Request, State, _) ->
        ?LOG_WARN("Unknown info \"~p\"", [Request]),
        {noreply, State}.
    
    cloudi_service_terminate(_, #state{}) ->
        ok.
    EOF
    $ erlc -pz /usr/local/lib/cloudi-1.1.1/lib/cloudi_core-1.1.1 hello_world.erl

You now have a compiled internal CloudI service which is ready to run.  You can also provide an OTP application file with the same name, if the internal CloudI service has application dependencies.

### Run the Internal (Erlang) CloudI Service

While you are still in the cloudi-quickstart directory, use the CloudI Service API to run the internal CloudI service.

    $ curl -X POST -d '"'`pwd`'"' http://localhost:6467/cloudi/api/erlang/code_path_add
    $ cat << EOF > hello_world.conf
    [{internal,
      "/quickstart/hello/",
      hello_world,
      [],
      lazy_closest,
      5000, 5000, 5000, [api], undefined, 1, 5, 300, []}]
    EOF
    $ curl -X POST -d @hello_world.conf http://localhost:6467/cloudi/api/erlang/services_add

These HTTP requests communicate with `src/lib/cloudi_services_internal/src/cloudi_service_http_cowboy.erl` which runs the cowboy HTTP webserver on port 6467, because of the default CloudI configuration (installed at `/usr/local/etc/cloudi/cloudi.conf`).  The request becomes a CloudI request, within the `cloudi_service_http_cowboy` internal CloudI service, which is sent to `lib/cloudi_services_internal/src/cloudi_service_api.erl`.  The `cloudi_service_api` internal CloudI service provides runtime configuration of CloudI.

You will notice that the syntax used to start the CloudI service in the `hello_world.conf` file is the same as what is specified in the "services" section of `/usr/local/etc/cloudi/cloudi.conf`.

### Use the Internal (Erlang) CloudI Service

    $ curl http://localhost:6467/quickstart/hello/hello_world
    Hello World!

The HTTP GET request has received the "Hello World!" message from your new internal CloudI service.

You can get the same behavior with an external CloudI service, which is written in a supported programming language, currently: C/C++, Java, Python, or Ruby.

### Use an External (Python) CloudI Service

    $ cat << EOF > hello_world.py
    EOF

