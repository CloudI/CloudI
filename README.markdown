# ABOUT

CloudI 0.0.2 (alpha)

The project is still being developed and is alpha, so please be patient if you
have troubles with the setup.  The setup will soon become more
dynamic and straight-forward.

CloudI facilitates a cloud of processes for solving embarrassingly parallel
and divide and conquer problems with dynamic load balancing work pools
while maintaining fault-tolerant workers.  CloudI requires pool data from
databases or from an Erlang work module that will automatically generate
the work.  CloudI was designed to be a generic cloud for internal
distributed processing and could facilitate work supplied by
an external server.  CloudI is currently focused on being an abstract
type of cloud and interfacing to work.  External applications could manage
security and a user interface for CloudI's functionality.  CloudI is meant
to be the bare essentials for efficient fault-tolerant processing on a cloud.

CloudI uses Erlang/OTP to provide fault-tolerance.  CloudI Work consists
of an Erlang module to handle allocating the work as tasks and
a dynamic library created with C/C++ to execute the work.
An example of using CloudI can be found in "src/lib/cloud_job_tests/src/",
which defines the work title "cloud_job_tests" referenced in src/cloud.conf.
"cloud_job_tests" finds hexadecimal digits of the constant PI using the
Bailey-Borwein-Plouffe formula and verifies that they are correct.
"cloud_job_tests" requires that Postgres is configured and setup because
it stores the results.

CloudI ensures that the C/C++ work code is executed in a fault-tolerant way.
Failover is handled with multiple CloudI instances using separate epmd daemons.
However, the coordination of the failover of running work is not yet
implemented, since that will occur in a separate external application that
is not yet written.  Current failover requires knowledge of the cloud_api
module and commands fed to the Erlang VM shell, so it is a manual process.

# CONFIGURATION

CloudI does require some configuration and a brief guide exists here:
    src/docs/CONFIGURATION.txt

Much of the local setup is currently in the files:
    src/Makefile
    src/cloud.conf
    src/config/failover_hosts

Configuration will require changing the value "host1"
(and possibly "host2" which requires uncommenting
 ssh commands in the Makefile, adding more?) in both files.

The paths for Erlang and ErlWare need to be specified in the Makefile:
    ERLANG_PATH=/home/user/installed
    ERLWARE_PATH=/home/user/installed/erlware

The Makefile is currently assuming Erlang R13B01 is used.  If you are using
a different version, you will need to change:
    ERTS_VERSION=5.7.2
    ERL_INTERFACE_VERSION=3.6.2

# RUNNING

Once configuration has been completed, you can run "make" and it will
compile any specific dependencies CloudI requires
(it does build its own version of g++, boost, etc., so it takes awhile
 but is only done once) and then CloudI itself.

To run CloudI you can run "make run" and an Erlang shell will be opened.
To exit CloudI use the statement "q:q()." and the cloud will be shutdown
without leaving any nodes running.

CloudI does have an API in "src/lib/cloud/src/cloud_api.erl".  Be careful
when using the cloud_api:remove_data/1 function.  If a data repository is
removed that a work module is using, bad things will happen.  The usage of the
CloudI API should respect running work that is not removed.

# LICENSE

BSD License

# CONTACT

Michael Truog (mjtruog [at] gmail (dot) com)


