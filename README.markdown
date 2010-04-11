#[CloudI 0.0.10 (alpha)](http://cloudi.org)

## ABOUT

CloudI is an open-source private cloud computing framework for secure,
internal data processing.  CloudI facilitates a cloud of processes for solving
embarrassingly parallel and divide and conquer problems with dynamic load
balancing work pools while maintaining fault-tolerant workers.
[To support CloudI development donate here.](http://pledgie.com/campaigns/9269)

CloudI requires pool data from databases or from an Erlang work module that
will automatically generate the work.  CloudI was designed to be a multi-purpose
cloud for internal distributed processing and could facilitate work supplied by
an external server.  CloudI is currently focused on being an abstract
type of cloud and interfacing to work.  External applications could manage
security and a user interface for CloudI's functionality.  CloudI is meant
to be the bare essentials for efficient fault-tolerant processing on a cloud.

CloudI uses Erlang/OTP to provide fault-tolerance.  CloudI work consists
of an Erlang module to handle allocating the work as tasks and
a dynamic library created with C/C++ to execute the work.
An example of using CloudI can be found in "src/lib/cloud_job_tests/src/",
which defines the work title "cloud_job_tests" referenced in src/cloud.conf.
"cloud_job_tests" finds hexadecimal digits of the constant PI using the
Bailey-Borwein-Plouffe formula and verifies that they are correct.
"cloud_job_tests" requires that PostgreSQL is configured and setup because
it stores the results.  Memcached is also used by "cloud_job_tests", but any
results that would go to memcached are discarded if memcached isn't configured.
CouchDB, MySQL and Tokyo Tyrant are used by "cloud_job_tests" in the same way
as memcached (i.e., just to test basic data storage functionality).

CloudI currently supports the following databases:

* CouchDB (>= 0.9.0)
* memcached (>= 1.3)
* MySQL (>= 4.0)
* PostgreSQL (>= 7.4)
* Tokyo Tyrant (>= 1.1.23)

CloudI ensures that the C/C++ work code is executed in a fault-tolerant way.
Failover is handled with multiple CloudI instances using separate epmd daemons.
However, the coordination of the failover of running work is not yet
implemented, since that will occur in a separate external application that
is not yet written.  Current failover requires knowledge of the cloud_api
module and commands fed to the Erlang VM shell, so it is a manual process.

CloudI fault-tolerance test cases include:

* a local death of a CloudI instance coordinating Erlang VM
* a local death of C/C++ work code (within a cloud_worker_port OS process) due to a signal
* a local stop of C/C++ work code (within a cloud_worker_port OS process) due to a signal
* a remote death of a CloudI instance Erlang VM
* a remote death of C/C++ work code (within a cloud_worker_port OS process) due to a signal
* a remote stop of C/C++ work code (within a cloud_worker_port OS process) due to a signal

## CONFIGURATION

The project is still being developed and is alpha, so please be patient if you
have troubles with the setup.  The setup will soon become more
dynamic and straight-forward.

CloudI does require some configuration and a brief guide exists here:
    src/docs/CONFIGURATION.txt

Much of the local setup is currently in the files:
    src/build_configuration.mk
    src/Makefile
    src/cloud.conf
    src/config/failover_hosts

Configuration will require changing the value "host1"
(and possibly "host2" which requires uncommenting
 ssh commands in the Makefile, adding more?) in all the files.
Long node names are not currently supported, so make sure to
not use a domain (just the hostname) in the "machines" section of cloud.conf.
All Cloudi nodes should be of the same byte order because the native byte
order of the Cloudi leader node is used by all other nodes
(so the Cloudi cluster can be very heterogeneous but all machines
 for an instance do need to be either little endian or big endian).

The paths for Erlang and ErlWare need to be specified in build_configuration.mk:
    ERLANG_PATH=/home/user/installed
    ERLWARE_PATH=/home/user/installed/erlware

Erlang (version >= 12B5) is required.  Faxien and Sinan are also required.
The build_configuration.mk file is currently assuming Erlang R13B02 is used.
If you are using a different version, you will need to change:
    ERTS_VERSION=5.7.3
    ERL_INTERFACE_VERSION=3.6.3

## RUNNING

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

## KNOWN BUGS/PROBLEMS

* Long node names are not currently supported in the cloud.conf file
  (so do not use a domain name in the node name, only the hostname).
* Database drivers may bring down the Cloudi instance leader if the
  host can not be reached (ememcached, epgsql, etc.).
* If a job ignores the task size and does not alter its work load
  based on its value (within the range (0..1)) the job runs the risk of
  creating results faster than they can be consumed by the database and
  the database may timeout and cause the Cloudi instance leader to crash.
  However, with a properly implemented job, the task size alters the work load
  so that tasks converge on the task time target and stagger the database
  results (so the results can be consumed gradually).
* cloud_api:remove_data/1 causes undefined behavior if a data repository is
  removed that a running job is using.

## LICENSE

BSD License

## CONTACT

Michael Truog (mjtruog [at] gmail (dot) com)

