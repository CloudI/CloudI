CPG (CloudI Process Groups)
===========================

Purpose
-------

CPG provides a process group interface that is similar to the pg2 module
within Erlang OTP.  The pg Erlang OTP module is experimental and people
have avoided using it.  However, the pg2 module is used internally by
Erlang OTP, and is currently the most common approach to the combination of
availability and partition tolerance in Erlang (as they relate to the
CAP theorem).  When comparing these goals with gproc (and its usage of
gen_leader), gproc is focused on availability and consistency (as it relates to
the CAP theorem), which makes its goals similar to mnesia.

The cpg interface was created to avoid some problems with pg2 while pursuing
better availability and partition tolerance.  pg2 utilizes ets (global
key/value storage in Erlang which requires internal memory locking,
which limits scalability) but cpg uses internal process memory.  By default,
cpg utilizes Erlang strings for group names (list of integers) and provides
the ability to set a pattern string as a group name.  A pattern string
is a string that includes the`"*"`wildcard character (equivalent to ".+"
regex while`"**"`is forbidden).  When a group name is a pattern string,
a process can be retrieved by matching the pattern.  This behavior can
be changed with the macros set in src/cpg_constants.hrl, if a user would
prefer to limit the functionality to what pg2 provides.

The cpg interface provides more error checking than the pg2 module, and it
allows the user to obtain the groups state so that group name lookups do not
require a message to the cpg scope process.  The cpg scope is a locally
registered process name used to provide all the group names with a scope.
By avoiding a message to the cpg scope process, contention for the single
process message queue can be avoided.

The process group solutions for Erlang discussed here depend on
the distributed Erlang functionality, provided natively by Erlang OTP.
The distributed Erlang functionality automatically creates a fully-connected
network topology and is only meant for a Local Area Network (LAN).
Since a fully-connected network topology is created that requires a
net tick time average of 60 seconds (the net tick time is not changed
because of the problems that occur due to the assumptions based on its
default value, which impacts typical Erlang OTP functionality) the
distributed Erlang node connections are limited to roughly 50-100 nodes.
So, that means these process group solutions are only targeting a cluster
of Erlang nodes, given the constraints of distributed Erlang and a
fully-connected network topology.

Example
-------

    (cpg@localhost)1> application:start(cpg).
    ok
    (cpg@localhost)2> cpg:join(process_group1, "Hello", self()).      
    ok
    (cpg@localhost)3> cpg:join(process_group1, "World", self()).
    ok
    (cpg@localhost)4> cpg:get_local_members(process_group1, "Hello"). 
    {ok,"Hello",[<0.43.0>]}
    (cpg@localhost)5> cpg:get_local_members(process_group1, "World").
    {ok,"World",[<0.43.0>]}
    (cpg@localhost)6> cpg:which_groups(process_group1).              
    ["Hello","World"]
    (cpg@localhost)7> cpg:which_groups(process_group2).
    []
    
Author
------

Michael Truog (mjtruog [at] gmail (dot) com)

License
-------

BSD (src/cpg.erl is under the Erlang Public License)

