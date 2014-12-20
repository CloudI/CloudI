## CloudI Quick Start

### Install

1.  Get wget and curl if you don't already have them

    * **(Ubuntu)** `sudo apt-get install wget curl`
    * **(OSX)**    `sudo port install wget curl`

1.  Get CloudI running (need [./configure help?](http://cloudi.org/faq.html#3_Options)):

        wget --content-disposition \
            http://sourceforge.net/projects/cloudi/files/latest/download
        tar zxvf cloudi-1.4.0.tar.gz
        cd cloudi-1.4.0/src
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

        cat &lt;&lt; EOF &gt; hello_world.c
        #include "cloudi.h"
        #include &lt;string.h&gt;
        #include &lt;assert.h&gt;
        static void hello_world(cloudi_instance_t * api,
                                int const command,
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
                                uint32_t const pid_size)
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
            int result = cloudi_initialize_thread_count(&amp;thread_count);
            assert(result == cloudi_success);
            assert(thread_count == 1);
            cloudi_instance_t api;
            result = cloudi_initialize(&amp;api, 0);
            assert(result == cloudi_success);
            result = cloudi_subscribe(&amp;api, "hello_world/get",
                                      &amp;hello_world);
            assert(result == cloudi_success);
            result = cloudi_poll(&amp;api, -1);
            cloudi_destroy(&amp;api);
            return result;
        }
        EOF

1.  Compile the CloudI service executable:

        gcc -I/usr/local/lib/cloudi-1.4.0/api/c \
            -L/usr/local/lib/cloudi-1.4.0/api/c \
            -g -O0 -fexceptions hello_world.c -o hello_world_c -lcloudi

1.  Now it is necessary to create the CloudI service configuration that
    specifies both the initialization and fault-tolerance constraints
    the CloudI service should be executed with
    (with the proplist format to rely on defaults):

        export PWD=`pwd`
        cat &lt;&lt; EOF &gt; hello_world.conf
        [[{prefix, "/quickstart/c/"},
          {file_path, "$PWD/hello_world_c"},
          {env, [{"LD_LIBRARY_PATH",
                  "/usr/local/lib/cloudi-1.4.0/api/c/"},
                 {"DYLD_LIBRARY_PATH",
                  "/usr/local/lib/cloudi-1.4.0/api/c/"}]}]]
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


