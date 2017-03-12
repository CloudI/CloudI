## CloudI Quick Start

* [Install](#install)
* [C](#c)
* [C++](#c-1)
* [Elixir](#elixir)
* [Erlang](#erlang)
* [Go](#go)
* [Java](#java)
* [JavaScript](#javascript)
* [Perl](#perl)
* [PHP](#php)
* [Python](#python)
* [Ruby](#ruby)

### Install

1.  Get wget and curl if you don't already have them

    * **(Ubuntu)** `sudo apt-get install wget curl`
    * **(OSX)**    `sudo port install wget curl`

1.  Get CloudI running (need [./configure help?](http://cloudi.org/faq.html#3_Options)):

        wget --content-disposition \
            http://sourceforge.net/projects/cloudi/files/latest/download
        tar zxvf cloudi-1.6.0.tar.gz
        cd cloudi-1.6.0/src
        ./configure
        make
        sudo make install
        cd ../..
        sudo cloudi start

1.  The CloudI integration tests are now running and consuming your
    available CPUs.  The /usr/local/var/log/cloudi/cloudi.log file provides
    integration test output.  You can now select a programming language
    below to create a CloudI service.

### C

1.  A CloudI service written in C is called an "external" service
    because the service is ran inside an Operating System process
    (external to the Erlang VM).  The example C service can be
    created by executing the following inside your shell:

        cat << EOF > hello_world.c
        #include "cloudi.h"
        #include <string.h>
        #include <assert.h>
        static void hello_world(int const command,
                                char const * const name,
                                char const * const pattern,
                                void const * const request_info,
                                uint32_t const request_info_size,
                                void const * const request,
                                uint32_t const request_size,
                                uint32_t timeout,
                                int8_t priority,
                                char const * const trans_id,
                                char const * const pid,
                                uint32_t const pid_size,
                                void * state,
                                cloudi_instance_t * api)
        {
            char const * const message = "Hello World!";
            uint32_t const message_size = strlen(message);
            cloudi_return(api, command, name, pattern, "", 0,
                          message, message_size,
                          timeout, trans_id, pid, pid_size);
        }
        int main(int argc, char ** argv)
        {
            unsigned int thread_count;
            int result = cloudi_initialize_thread_count(&thread_count);
            assert(result == cloudi_success);
            assert(thread_count == 1);
            cloudi_instance_t api;
            result = cloudi_initialize(&api, 0, 0);
            assert(result == cloudi_success);
            result = cloudi_subscribe(&api, "hello_world/get",
                                      &hello_world);
            assert(result == cloudi_success);
            result = cloudi_poll(&api, -1);
            cloudi_destroy(&api);
            return result;
        }
        EOF

1.  Compile the CloudI service executable:

        gcc -I/usr/local/lib/cloudi-1.6.0/api/c \
            -L/usr/local/lib/cloudi-1.6.0/api/c \
            -g -O0 -fexceptions hello_world.c -o hello_world_c -lcloudi

1.  Now it is necessary to create the CloudI service configuration that
    specifies both the initialization and fault-tolerance constraints
    the CloudI service should be executed with
    (with the proplist format to rely on defaults):

        export PWD=`pwd`
        cat << EOF > hello_world.conf
        [[{prefix, "/quickstart/c/"},
          {file_path, "$PWD/hello_world_c"},
          {env, [{"LD_LIBRARY_PATH",
                  "/usr/local/lib/cloudi-1.6.0/api/c/"},
                 {"DYLD_LIBRARY_PATH",
                  "/usr/local/lib/cloudi-1.6.0/api/c/"}]}]]
        EOF

1.  To dynamically add the CloudI service configuration that
    starts the service's execution use:

        curl -X POST -d @hello_world.conf \
            http://localhost:6464/cloudi/api/rpc/services_add.erl

1.  The curl requests have been using the cowboy HTTP server that is
    running within the default CloudI configuration to allow the
    CloudI Service API to be used over HTTP.  The same HTTP server can
    be used to make a CloudI service request to the hello_world service
    with:

        curl http://localhost:6464/quickstart/c/hello_world

1.  If there was a problem during the service creation there would be an
    ERROR entry within the /usr/local/var/log/cloudi/cloudi.log file.
    If an error occurred with a curl command it would be displayed
    in the shell.  The available service configuration parameters are
    described in the [services_add documentation](http://cloudi.org/api.html#2_services_add).
    More complex C [examples are listed here](http://cloudi.org/faq.html#6_C).

### C++

1.  A CloudI service written in C++ is called an "external" service
    because the service is ran inside an Operating System process
    (external to the Erlang VM).  The example C++ service can be
    created by executing the following inside your shell:

        cat << EOF > hello_world.cpp
        #include "cloudi.hpp"
        #include <string>
        #include <cassert>
        class Task
        {
            public:
                Task(unsigned int const thread_index) :
                    m_api(thread_index)
                {
                    int result = 0;
                    result = m_api.subscribe("hello_world/get",
                                             *this, &Task::hello_world);
                    assert(result == CloudI::API::return_value::success);
                }
        
                int run()
                {
                    return m_api.poll();
                }
            private:
                void hello_world(CloudI::API const & api,
                                 int const command,
                                 std::string const & name,
                                 std::string const & pattern,
                                 void const * const /*request_info*/,
                                 uint32_t const /*request_info_size*/,
                                 void const * const request,
                                 uint32_t const request_size,
                                 uint32_t timeout,
                                 int8_t /*priority*/,
                                 char const * const trans_id,
                                 char const * const pid,
                                 uint32_t const pid_size)
                {
                    std::string message("Hello World!");
                    api.return_(command, name, pattern, "", 0,
                                message.c_str(), message.size(),
                                timeout, trans_id, pid, pid_size);
                }
                CloudI::API m_api;
        };
        int main(int, char **)
        {
            unsigned int const thread_count = CloudI::API::thread_count();
            assert(thread_count == 1);
            Task t(0);
            return t.run();
        }
        EOF

1.  Compile the CloudI service executable:

        g++ -I/usr/local/lib/cloudi-1.6.0/api/c \
            -L/usr/local/lib/cloudi-1.6.0/api/c \
            -g -O0 hello_world.cpp -o hello_world_cxx -lcloudi

1.  Now it is necessary to create the CloudI service configuration that
    specifies both the initialization and fault-tolerance constraints
    the CloudI service should be executed with
    (with the proplist format to rely on defaults):

        export PWD=`pwd`
        cat << EOF > hello_world.conf
        [[{prefix, "/quickstart/cxx/"},
          {file_path, "$PWD/hello_world_cxx"},
          {env, [{"LD_LIBRARY_PATH",
                  "/usr/local/lib/cloudi-1.6.0/api/c/"},
                 {"DYLD_LIBRARY_PATH",
                  "/usr/local/lib/cloudi-1.6.0/api/c/"}]}]]
        EOF

1.  To dynamically add the CloudI service configuration that
    starts the service's execution use:

        curl -X POST -d @hello_world.conf \
            http://localhost:6464/cloudi/api/rpc/services_add.erl

1.  The curl requests have been using the cowboy HTTP server that is
    running within the default CloudI configuration to allow the
    CloudI Service API to be used over HTTP.  The same HTTP server can
    be used to make a CloudI service request to the hello_world service
    with:

        curl http://localhost:6464/quickstart/cxx/hello_world

1.  If there was a problem during the service creation there would be an
    ERROR entry within the /usr/local/var/log/cloudi/cloudi.log file.
    If an error occurred with a curl command it would be displayed
    in the shell.  The available service configuration parameters are
    described in the [services_add documentation](http://cloudi.org/api.html#2_services_add).
    More complex C++ [examples are listed here](http://cloudi.org/faq.html#6_C).

### Elixir

1.  A CloudI service written in Elixir is called an "internal" service
    because the service is ran inside the Erlang VM.
    The example Elixir service can be created by executing the following
    inside your shell:

        mkdir lib
        cat << EOF > lib/HelloWorld.ex
        defmodule HelloWorld do
        
            import CloudILogger
        
            def cloudi_service_init(_args, _prefix, _timeout,
                                    dispatcher) do
                :cloudi_service.subscribe(dispatcher, 'hello_world/get')
                {:ok, :undefined}
            end
        
            def cloudi_service_handle_request(_type, _name, _pattern,
                                              _requestinfo, _request,
                                              _timeout, _priority,
                                              _transid, _pid, state,
                                              _dispatcher) do
                {:reply, "Hello World!", state}
            end
        
            def cloudi_service_handle_info(request, state, _dispatcher) do
                log_warn('Unknown info "~p"', [request])
                {:noreply, state}
            end
        
            def cloudi_service_terminate(_reason, _timeout, _state) do
                :ok
            end
        end
        EOF
        cat << EOF > mix.exs
        defmodule HelloWorld.Mixfile do
            use Mix.Project
        
            def project do
                [app: :Elixir.HelloWorld,
                 version: "1.6.0",
                 elixirc_paths: ["lib/"],
                 deps: []]
            end
        
            def application do
                [applications: [:cloudi_core]]
            end
        end
        EOF

1.  Now compile the CloudI service module.  If the CloudI
    service needed to utilize other Elixir dependencies they would
    be added to the mix.exs file.

        mix compile

1.  You now have a CloudI service contained within a single Elixir module
    that may look familiar if you remember how a GenServer behavior works.
    Instead of using the GenServer behavior, we are using a cloudi_service
    behavior which provides more features with CloudI service requests.
    To allow the Erlang VM to find the CloudI service Elixir module that has
    been compiled, it is necessary to add the current directory to the
    code path:

        export PWD=`pwd`
        curl -X POST -d '"'$PWD'/_build/dev/lib/Elixir.HelloWorld/ebin"' \
            http://localhost:6464/cloudi/api/rpc/code_path_add.erl

1.  If Elixir isn't already in the Erlang VM code search path, add it:

        curl -X POST -d '"/usr/local/lib/elixir/lib/elixir/ebin"' \
            http://localhost:6464/cloudi/api/rpc/code_path_add.erl

1.  Now it is necessary to create the CloudI service configuration that
    specifies both the initialization and fault-tolerance constraints
    the CloudI service should be executed with
    (with the proplist format to rely on defaults):

        cat << EOF > hello_world.conf
        [[{prefix, "/quickstart/elixir/"},
          {module, 'Elixir.HelloWorld'}]]
        EOF

1.  To dynamically add the CloudI service configuration that
    starts the service's execution use:

        curl -X POST -d @hello_world.conf \
            http://localhost:6464/cloudi/api/rpc/services_add.erl

1.  The curl requests have been using the cowboy HTTP server that is
    running within the default CloudI configuration to allow the
    CloudI Service API to be used over HTTP.  The same HTTP server can
    be used to make a CloudI service request to the hello_world service
    with:

        curl http://localhost:6464/quickstart/elixir/hello_world

1.  If there was a problem during the service creation there would be an
    ERROR entry within the /usr/local/var/log/cloudi/cloudi.log file.
    If an error occurred with a curl command it would be displayed
    in the shell.
    The available service configuration parameters are described in
    the [services_add documentation](http://cloudi.org/api.html#2_services_add).

### Erlang

1.  A CloudI service written in Erlang (or using a language based on
    core Erlang like Elixir) is called an "internal" service
    because the service is ran inside the Erlang VM.
    The example Erlang service can be created by executing the following
    inside your shell:

        cat << EOF > hello_world.erl
        -module(hello_world).
        -behaviour(cloudi_service).
        
        %% cloudi_service callbacks
        -export([cloudi_service_init/4,
                 cloudi_service_handle_request/11,
                 cloudi_service_handle_info/3,
                 cloudi_service_terminate/3]).
        
        -include_lib("cloudi_core/include/cloudi_logger.hrl").
        
        -record(state,
            {
            }).
        
        cloudi_service_init(_Args, _Prefix, _Timeout, Dispatcher) ->
            cloudi_service:subscribe(Dispatcher, "hello_world/get"),
            {ok, #state{}}.
        
        cloudi_service_handle_request(_Type, _Name, _Pattern,
                                      _RequestInfo, _Request,
                                      _Timeout, _Priority,
                                      _TransId, _Pid,
                                      #state{} = State, _Dispatcher) ->
            {reply, <<"Hello World!">>, State}.
        
        cloudi_service_handle_info(Request, State, _Dispatcher) ->
            ?LOG_WARN("Unknown info \"~p\"", [Request]),
            {noreply, State}.
        
        cloudi_service_terminate(_Reason, _Timeout, #state{}) ->
            ok.
        EOF

1.  Now compile the CloudI service module.  If the CloudI
    service needed to utilize other Erlang dependencies an Erlang/OTP .app
    file would be added with the same filename
    (see the examples [for more details](https://github.com/CloudI/CloudI/tree/develop/examples/hello_world1#readme)).

        erlc -pz /usr/local/lib/cloudi-1.6.0/lib/cloudi_core-1.6.0/ebin \
            hello_world.erl

1.  You now have a CloudI service contained within a single Erlang module
    that may look familiar if you remember how a gen_server behavior works.
    Instead of using the gen_server behavior, we are using a cloudi_service
    behavior which provides more features with CloudI service requests.
    To allow the Erlang VM to find the CloudI service Erlang module that has
    been compiled, it is necessary to add the current directory to the
    code path:

        curl -X POST -d '"'`pwd`'"' \
            http://localhost:6464/cloudi/api/rpc/code_path_add.erl

1.  Now it is necessary to create the CloudI service configuration that
    specifies both the initialization and fault-tolerance constraints
    the CloudI service should be executed with
    (with the proplist format to rely on defaults):

        cat << EOF > hello_world.conf
        [[{prefix, "/quickstart/erlang/"},
          {module, hello_world}]]
        EOF

1.  To dynamically add the CloudI service configuration that
    starts the service's execution use:

        curl -X POST -d @hello_world.conf \
            http://localhost:6464/cloudi/api/rpc/services_add.erl

1.  The curl requests have been using the cowboy HTTP server that is
    running within the default CloudI configuration to allow the
    CloudI Service API to be used over HTTP.  The same HTTP server can
    be used to make a CloudI service request to the hello_world service
    with:

        curl http://localhost:6464/quickstart/erlang/hello_world

1.  If there was a problem during the service creation there would be an
    ERROR entry within the /usr/local/var/log/cloudi/cloudi.log file.
    If an error occurred with a curl command it would be displayed
    in the shell.  To get more details on CloudI Erlang integration
    (i.e., CloudI runtime usage with Erlang) see the
    [examples in the source code repository](https://github.com/CloudI/CloudI/tree/develop/examples).
    The available service configuration parameters are described in
    the [services_add documentation](http://cloudi.org/api.html#2_services_add).
    More complex Erlang [examples are listed here](http://cloudi.org/faq.html#6_Erlang).

### Go

1.  A CloudI service written in Go is called an "external" service
    because the service is ran inside an Operating System process
    (external to the Erlang VM).  The example Go service can be
    created by executing the following inside your shell:

        mkdir -p src/hello_world_go/
        cat << EOF > src/hello_world_go/main.go
        package main
        
        import (
            "github.com/CloudI/cloudi_api_go/src/cloudi"
            "os"
            "sync"
        )
        
        func helloWorld(requestType int, name, pattern string, requestInfo, request []byte, timeout uint32, priority int8, transId [16]byte, pid cloudi.Source, state interface{}, api *cloudi.Instance) ([]byte, []byte, error) {
            return nil, []byte("Hello World!"), nil
        }
        
        func task(threadIndex uint32, execution *sync.WaitGroup) {
            defer execution.Done()
            api, err := cloudi.API(threadIndex, nil)
            if err != nil {
                cloudi.ErrorWrite(os.Stderr, err)
                return
            }
            err = api.Subscribe("hello_world/get", helloWorld)
            if err != nil {
                cloudi.ErrorWrite(os.Stderr, err)
                return
            }
            _, err = api.Poll(-1)
            if err != nil {
                cloudi.ErrorWrite(os.Stderr, err)
            }
        }
        
        func main() {
            threadCount, err := cloudi.ThreadCount()
            if err != nil {
                cloudi.ErrorExit(os.Stderr, err)
            }
            var execution sync.WaitGroup
            for threadIndex := uint32(0); threadIndex < threadCount; threadIndex++ {
                execution.Add(1)
                go task(threadIndex, &execution)
            }
            execution.Wait()
        }
        EOF

1.  Compile the CloudI service executable:

        GOPATH=`pwd` GOBIN=$GOPATH/bin go get -x hello_world_go

1.  Now it is necessary to create the CloudI service configuration that
    specifies both the initialization and fault-tolerance constraints
    the CloudI service should be executed with
    (with the proplist format to rely on defaults):

        export PWD=`pwd`
        cat << EOF > hello_world.conf
        [[{prefix, "/quickstart/go/"},
          {file_path, "$PWD/bin/hello_world_go"}]]
        EOF

1.  To dynamically add the CloudI service configuration that
    starts the service's execution use:

        curl -X POST -d @hello_world.conf \
            http://localhost:6464/cloudi/api/rpc/services_add.erl

1.  The curl requests have been using the cowboy HTTP server that is
    running within the default CloudI configuration to allow the
    CloudI Service API to be used over HTTP.  The same HTTP server can
    be used to make a CloudI service request to the hello_world service
    with:

        curl http://localhost:6464/quickstart/go/hello_world

1.  If there was a problem during the service creation there would be an
    ERROR entry within the /usr/local/var/log/cloudi/cloudi.log file.
    If an error occurred with a curl command it would be displayed
    in the shell.  The available service configuration parameters are
    described in the [services_add documentation](http://cloudi.org/api.html#2_services_add).
    More complex Go [examples are listed here](http://cloudi.org/faq.html#6_Go).

### Java

1.  A CloudI service written in Java is called an "external" service
    because the service is ran inside an Operating System process
    (external to the Erlang VM).  The example Java service can be
    created by executing the following inside your shell:

        mkdir -p org/cloudi/tests/hello_world/
        cat << EOF > org/cloudi/tests/hello_world/Main.java
        package org.cloudi.tests.hello_world;
        
        import org.cloudi.API;
        
        public class Main
        {
            public static void main(String[] args)
            {
                try
                {
                    final int thread_count = API.thread_count();
                    assert(thread_count == 1);
                    Task t = new Task(0);
                    t.run();
                }
                catch (API.InvalidInputException e)
                {
                    e.printStackTrace(API.err);
                }
            }
        }
        EOF
        cat << EOF > org/cloudi/tests/hello_world/Task.java
        package org.cloudi.tests.hello_world;
        
        import com.ericsson.otp.erlang.OtpErlangPid;
        import org.cloudi.API;
        
        public class Task
        {
            private API api;
        
            public Task(final int thread_index)
            {
                try
                {
                    this.api = new API(thread_index);
                }
                catch (API.InvalidInputException e)
                {
                    e.printStackTrace(API.err);
                    System.exit(1);
                }
                catch (API.MessageDecodingException e)
                {
                    e.printStackTrace(API.err);
                    System.exit(1);
                }
                catch (API.TerminateException e)
                {
                    System.exit(1);
                }
            }
        
            public Object hello_world(Integer command,
                                      String name, String pattern,
                                      byte[] request_info,
                                      byte[] request,
                                      Integer timeout, Byte priority,
                                      byte[] trans_id, OtpErlangPid pid)
            {
                return ("Hello World!").getBytes();
            }
        
            public void run()
            {
                try
                {
                    this.api.subscribe("hello_world/get",
                                       this, "hello_world");
                    this.api.poll();
                }
                catch (API.TerminateException e)
                {
                }
                catch (Exception e)
                {
                    e.printStackTrace(API.err);
                }
            }
        }
        
        EOF
        cat << EOF > manifest.txt
        Main-Class: org.cloudi.tests.hello_world.Main
        Class-Path: /usr/local/lib/cloudi-1.6.0/api/java/cloudi.jar
        
        EOF

1.  Compile the CloudI service jar:

        cd org/cloudi/tests/hello_world/
        CLASSPATH=/usr/local/lib/cloudi-1.6.0\
        /api/java/cloudi.jar:${CLASSPATH} javac Task.java Main.java
        cd ../../../../
        jar cvfm hello_world.jar manifest.txt org

1.  Now it is necessary to create the CloudI service configuration that
    specifies both the initialization and fault-tolerance constraints
    the CloudI service should be executed with
    (with the proplist format to rely on defaults):

        export JAVA=`which java`
        export PWD=`pwd`
        cat << EOF > hello_world.conf
        [[{prefix, "/quickstart/java/"},
          {file_path, "$JAVA"},
          {args, "-cp /usr/local/lib/cloudi-1.6.0/api/java/ "
                 "-ea:org.cloudi... -jar $PWD/hello_world.jar"}]]
        EOF

1.  To dynamically add the CloudI service configuration that
    starts the service's execution use:

        curl -X POST -d @hello_world.conf \
            http://localhost:6464/cloudi/api/rpc/services_add.erl

1.  The curl requests have been using the cowboy HTTP server that is
    running within the default CloudI configuration to allow the
    CloudI Service API to be used over HTTP.  The same HTTP server can
    be used to make a CloudI service request to the hello_world service
    with:

        curl http://localhost:6464/quickstart/java/hello_world

1.  If there was a problem during the service creation there would be an
    ERROR entry within the /usr/local/var/log/cloudi/cloudi.log file.
    If an error occurred with a curl command it would be displayed
    in the shell.  The available service configuration parameters are
    described in the
    [services_add documentation](http://cloudi.org/api.html#2_services_add).
    More complex Java
    [examples are listed here](http://cloudi.org/faq.html#6_Java).

### JavaScript

1.  A CloudI service written in JavaScript is called an "external" service
    because the service is ran inside an Operating System process
    (external to the Erlang VM).  The example JavaScript service can be
    created by executing the following inside your shell:

        cat << EOF > hello_world.js
        var CloudI = require('/usr/local/lib/cloudi-1.6.0/' +
                             'api/javascript/CloudI.js').CloudI;
        var assert = require('assert');
        
        Task = function Task (thread_index) {
            var Task = this;
            Task._thread_index = thread_index;
        };
        Task.prototype.run = function () {
            var Task = this;
            try {
                new CloudI.API(Task._thread_index, function (api) {
                Task._api = api;
                Task._api.subscribe('hello_world/get', Task,
                                    Task.hello_world,
                                    function () {
                Task._api.poll(function (timeout) {
                });
                });});
            }
            catch (err) {
                if (typeof err.stack !== 'undefined') {
                    process.stderr.write(err.stack + '\n');
                }
                else {
                    process.stderr.write(err + '\n');
                }
            }
        };
        Task.prototype.hello_world = function (command, name, pattern,
                                               request_info, request,
                                               timeout, priority,
                                               trans_id, pid) {
            return 'Hello World!';
        };
        
        assert(CloudI.API.thread_count() == 1);
        var thread = new Task(0);
        thread.run();
        EOF

1.  Now it is necessary to create the CloudI service configuration that
    specifies both the initialization and fault-tolerance constraints
    the CloudI service should be executed with
    (with the proplist format to rely on defaults):

        export NODE=`which node`
        export PWD=`pwd`
        cat << EOF > hello_world.conf
        [[{prefix, "/quickstart/javascript/"},
          {file_path, "$NODE"},
          {args, "$PWD/hello_world.js"}]]
        EOF

1.  To dynamically add the CloudI service configuration that
    starts the service's execution use:

        curl -X POST -d @hello_world.conf \
            http://localhost:6464/cloudi/api/rpc/services_add.erl

1.  The curl requests have been using the cowboy HTTP server that is
    running within the default CloudI configuration to allow the
    CloudI Service API to be used over HTTP.  The same HTTP server can
    be used to make a CloudI service request to the hello_world service
    with:

        curl http://localhost:6464/quickstart/javascript/hello_world

1.  If there was a problem during the service creation there would be an
    ERROR entry within the /usr/local/var/log/cloudi/cloudi.log file.
    If an error occurred with a curl command it would be displayed
    in the shell.  The available service configuration parameters are
    described in the
    [services_add documentation](http://cloudi.org/api.html#2_services_add).
    More complex JavaScript
    [examples are listed here](http://cloudi.org/faq.html#6_JavaScript).

### Perl

1.  A CloudI service written in Perl is called an "external" service
    because the service is ran inside an Operating System process
    (external to the Erlang VM).  The example Perl service can be
    created by executing the following inside your shell:

        cat << EOF > hello_world.pl
        use strict;
        use warnings;
        
        require CloudI::API;
        require CloudI::TerminateException;
        
        sub task
        {
            my (\$api) = @_;
            eval
            {
                my \$task_hello_world = sub
                {
                    my (\$command, \$name, \$pattern,
                        \$request_info, \$request,
                        \$timeout, \$priority, \$trans_id, \$pid) = @_;
                    return 'Hello World!';
                };
                \$api->subscribe('hello_world/get', \$task_hello_world);
                \$api->poll();
            };
            my \$e = \$@;
            if (\$e)
            {
                if (\$e->isa('CloudI::TerminateException'))
                {
                    1;
                }
                else
                {
                    print "\$e";
                }
            }
        }
        {
            CloudI::API->assert(CloudI::API->thread_count() == 1);
            task(CloudI::API->new(0));
        }
        EOF

1.  Now it is necessary to create the CloudI service configuration that
    specifies both the initialization and fault-tolerance constraints
    the CloudI service should be executed with
    (with the proplist format to rely on defaults):

        export PERL=`which perl`
        export PWD=`pwd`
        cat << EOF > hello_world.conf
        [[{prefix, "/quickstart/perl/"},
          {file_path, "$PERL"},
          {args, "$PWD/hello_world.pl"},
          {env, [{"PERL5LIB", "/usr/local/lib/cloudi-1.6.0/api/perl"}]}]]
        EOF

1.  To dynamically add the CloudI service configuration that
    starts the service's execution use:

        curl -X POST -d @hello_world.conf \
            http://localhost:6464/cloudi/api/rpc/services_add.erl

1.  The curl requests have been using the cowboy HTTP server that is
    running within the default CloudI configuration to allow the
    CloudI Service API to be used over HTTP.  The same HTTP server can
    be used to make a CloudI service request to the hello_world service
    with:

        curl http://localhost:6464/quickstart/perl/hello_world

1.  If there was a problem during the service creation there would be an
    ERROR entry within the /usr/local/var/log/cloudi/cloudi.log file.
    If an error occurred with a curl command it would be displayed
    in the shell.  The available service configuration parameters are
    described in the
    [services_add documentation](http://cloudi.org/api.html#2_services_add).
    More complex Perl
    [examples are listed here](http://cloudi.org/faq.html#6_Perl).

### PHP

1.  A CloudI service written in PHP is called an "external" service
    because the service is ran inside an Operating System process
    (external to the Erlang VM).  The example PHP service can be
    created by executing the following inside your shell:

        cat << EOF > hello_world.php
        <?php
        
        require '/usr/local/lib/cloudi-1.6.0/api/php/CloudI.php';
        
        class Task
        {
            private \$api;
        
            public function __construct(\$api)
            {
                \$this->api = \$api;
            }
        
            public function run()
            {
                try
                {
                    \$this->api->subscribe('hello_world/get',
                                           \$this, 'hello_world');
                    \$this->api->poll();
                }
                catch (\CloudI\TerminateException \$e)
                {
                }
                catch (Exception \$e)
                {
                    error_log("{\$e->getMessage()}\n{\$e}\n");
                }
            }
        
            public function hello_world(\$command, \$name, \$pattern,
                                        \$request_info, \$request,
                                        \$timeout, \$priority,
                                        \$trans_id, \$pid)
            {
                return 'Hello World!';
            }
        }
        
        \$thread_count = \CloudI\API::thread_count();
        assert(\$thread_count == 1);
        \$main_thread = new Task(new \CloudI\API(0));
        \$main_thread->run();
        
        ?>
        EOF

1.  Now it is necessary to create the CloudI service configuration that
    specifies both the initialization and fault-tolerance constraints
    the CloudI service should be executed with
    (with the proplist format to rely on defaults):

        export PHP=`which php`
        export PWD=`pwd`
        cat << EOF > hello_world.conf
        [[{prefix, "/quickstart/php/"},
          {file_path, "$PHP"},
          {args, "$PWD/hello_world.php"}]]
        EOF

1.  To dynamically add the CloudI service configuration that
    starts the service's execution use:

        curl -X POST -d @hello_world.conf \
            http://localhost:6464/cloudi/api/rpc/services_add.erl

1.  The curl requests have been using the cowboy HTTP server that is
    running within the default CloudI configuration to allow the
    CloudI Service API to be used over HTTP.  The same HTTP server can
    be used to make a CloudI service request to the hello_world service
    with:

        curl http://localhost:6464/quickstart/php/hello_world

1.  If there was a problem during the service creation there would be an
    ERROR entry within the /usr/local/var/log/cloudi/cloudi.log file.
    If an error occurred with a curl command it would be displayed
    in the shell.  The available service configuration parameters are
    described in the
    [services_add documentation](http://cloudi.org/api.html#2_services_add).
    More complex PHP
    [examples are listed here](http://cloudi.org/faq.html#6_PHP).

### Python

1.  A CloudI service written in Python is called an "external" service
    because the service is ran inside an Operating System process
    (external to the Erlang VM).  The example Python service can be
    created by executing the following inside your shell:

        cat << EOF > hello_world.py
        import sys
        sys.path.append('/usr/local/lib/cloudi-1.6.0/api/python/')
        import traceback
        from cloudi import API, terminate_exception
        
        class Task(object):
            def __init__(self):
                self.__api = API(0) # first/only thread == 0
        
            def run(self):
                try:
                    self.__api.subscribe("hello_world/get",
                                         self.__hello_world)
                    self.__api.poll()
                except terminate_exception:
                    pass
                except:
                    traceback.print_exc(file=sys.stderr)
        
            def __hello_world(self, command, name, pattern,
                              request_info, request,
                              timeout, priority, trans_id, pid):
                return 'Hello World!'
        
        if __name__ == '__main__':
            assert API.thread_count() == 1
            task = Task()
            task.run()
        EOF

1.  Now it is necessary to create the CloudI service configuration that
    specifies both the initialization and fault-tolerance constraints
    the CloudI service should be executed with
    (with the proplist format to rely on defaults):

        export PYTHON=`which python`
        export PWD=`pwd`
        cat << EOF > hello_world.conf
        [[{prefix, "/quickstart/python/"},
          {file_path, "$PYTHON"},
          {args, "$PWD/hello_world.py"}]]
        EOF

1.  To dynamically add the CloudI service configuration that
    starts the service's execution use:

        curl -X POST -d @hello_world.conf \
            http://localhost:6464/cloudi/api/rpc/services_add.erl

1.  The curl requests have been using the cowboy HTTP server that is
    running within the default CloudI configuration to allow the
    CloudI Service API to be used over HTTP.  The same HTTP server can
    be used to make a CloudI service request to the hello_world service
    with:

        curl http://localhost:6464/quickstart/python/hello_world

1.  If there was a problem during the service creation there would be an
    ERROR entry within the /usr/local/var/log/cloudi/cloudi.log file.
    If an error occurred with a curl command it would be displayed
    in the shell.  The available service configuration parameters are
    described in the
    [services_add documentation](http://cloudi.org/api.html#2_services_add).
    More complex Python
    [examples are listed here](http://cloudi.org/faq.html#6_Python).

### Ruby

1.  A CloudI service written in Ruby is called an "external" service
    because the service is ran inside an Operating System process
    (external to the Erlang VM).  The example Ruby service can be
    created by executing the following inside your shell:

        cat << EOF > hello_world.rb
        \$:.unshift '/usr/local/lib/cloudi-1.6.0/api/ruby'
        
        \$DEBUG = false
        
        require 'cloudi'
        
        if __FILE__ == \$PROGRAM_NAME
            thread_count = CloudI::API.thread_count()
            CloudI::API.assert{thread_count == 1}
        
            class Task
                def initialize(thread_index)
                    @api = CloudI::API.new(thread_index)
                end
        
                def run
                    begin
                        @api.subscribe('hello_world/get',
                                       method(:hello_world))
        
                        @api.poll
                    rescue CloudI::TerminateException
                        #
                    rescue
                        \$stderr.puts \$!.message
                        \$stderr.puts \$!.backtrace
                    end
                end
        
                private
        
                def hello_world(command, name, pattern,
                                request_info, request,
                                timeout, priority, trans_id, pid)
                    return 'Hello World!';
                end
            end
            begin
                object = Task.new(0)
                object.run
            rescue
                \$stderr.puts \$!.message
                \$stderr.puts \$!.backtrace
            end
        end
        EOF

1.  Now it is necessary to create the CloudI service configuration that
    specifies both the initialization and fault-tolerance constraints
    the CloudI service should be executed with
    (with the proplist format to rely on defaults):

        export RUBY=`which ruby` # must be >= 1.9
        export PWD=`pwd`
        cat << EOF > hello_world.conf
        [[{prefix, "/quickstart/ruby/"},
          {file_path, "$RUBY"},
          {args, "$PWD/hello_world.rb"}]]
        EOF

1.  To dynamically add the CloudI service configuration that
    starts the service's execution use:

        curl -X POST -d @hello_world.conf \
            http://localhost:6464/cloudi/api/rpc/services_add.erl

1.  The curl requests have been using the cowboy HTTP server that is
    running within the default CloudI configuration to allow the
    CloudI Service API to be used over HTTP.  The same HTTP server can
    be used to make a CloudI service request to the hello_world service
    with:

        curl http://localhost:6464/quickstart/ruby/hello_world

1.  If there was a problem during the service creation there would be an
    ERROR entry within the /usr/local/var/log/cloudi/cloudi.log file.
    If an error occurred with a curl command it would be displayed
    in the shell.  The available service configuration parameters are
    described in the
    [services_add documentation](http://cloudi.org/api.html#2_services_add).
    More complex Ruby
    [examples are listed here](http://cloudi.org/faq.html#6_Ruby).

