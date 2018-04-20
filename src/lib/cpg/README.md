# CPG ([CloudI](https://cloudi.org) Process Groups)

[![Build Status](https://secure.travis-ci.org/okeuday/cpg.png?branch=master)](http://travis-ci.org/okeuday/cpg)

## Purpose

CPG provides a process group interface that is similar to the pg2 module
within Erlang OTP.  The pg2 module is used internally by
Erlang/OTP, and is currently the most common approach to the combination of
availability and partition tolerance in Erlang (as they relate to the
CAP theorem).  When comparing these goals with gproc (and its usage of
`gen_leader`), gproc is focused on availability and consistency (as it relates
to the CAP theorem), which makes its goals similar to mnesia.

The cpg interface was created to avoid some problems with pg2 while pursuing
better availability and partition tolerance.  pg2 utilizes ets (global
key/value storage in Erlang which requires internal memory locking,
which limits scalability) but cpg uses internal process memory
(see the [Design](#design) section for more information).  By default,
cpg utilizes Erlang strings for group names (list of integers) and provides
the ability to set a pattern string as a group name.  A pattern string
is a string that includes the`"*"`wildcard character (equivalent to ".+"
regex while`"**"`is forbidden).  When a group name is a pattern string,
a process can be retrieved by matching the pattern.  To change the behavior
to be compatible with pg2 usage (or gproc), see the [Usage](#usage) section below.

The cpg interface provides more error checking than the pg2 module, and it
allows the user to obtain the groups state so that group name lookups do not
require a message to the cpg scope process.  The cpg scope is a locally
registered process name used to provide all the group names with a scope.
By avoiding a message to the cpg scope process, contention for the single
process message queue can be avoided.

The process group solutions for Erlang discussed here depend on
the distributed Erlang functionality, provided natively by Erlang/OTP.
The distributed Erlang functionality automatically creates a fully-connected
network topology and is only meant for a Local Area Network (LAN).
Since a fully-connected network topology is created that requires a
net tick time average of 60 seconds (the net tick time is not increased
to ensure distributed Erlang nodes fail-fast) the distributed
Erlang node connections are limited to roughly 50-100 nodes.  So, that
means these process group solutions are only targeting a cluster of Erlang
nodes, given the constraints of distributed Erlang and a fully-connected
network topology.

## Design

cpg is a Commutative/Convergent Replicated Data-Type (CRDT) that uses
node ownership of Erlang processes to ensure a set of keys has
add and remove operations that commute with an internal map data structure.
The cpg module provides add and remove operations with the function names
join and leave, that may only be called on the node that owns the
Erlang process which is the value for the join or leave operation.
The key is the process group name which represents a list of Erlang processes
(with an single Erlang process being able to be added or removed any
number of times).

All cpg join and leave operations change global state as a
Commutative Replicated Data-Type (CmRDT) by sending the operation to the
associated cpg Erlang process as a distributed Erlang message to all remote
nodes after the operation successfully completes on the local node.

cpg also uses distributed Erlang node monitoring to handle netsplits as a
Convergent Replicated Data-Type (CvRDT) by sending all of the internal
cpg state to remote nodes that have recently connected.  The associated
cpg Erlang process on the remote node then performs a merge operation to
make sure the count of each Erlang pid is consistent with the internal
cpg state it received.

The CRDT functionality in cpg is most similar to the
[POLog (Partially Ordered Log of operations)](#references)
though the cpg approach would instead be called an
"Ordered Log of operations" because it is depending on Erlang messaging on
a local node to have causal ordering (no vclocks are necessary to establish
causality on the local node with the cpg scope Erlang process message queue
providing an "Ordered Log of operations").  After a cpg operation
completes successfully on the local node, it is sent to all remote nodes which
act as read-only views of the local node.

The cpg scope process on the local node enforces causality by existing as the
only read/write store of the local process memberships (i.e., serialized
mutability similar to a mutex lock) while the remote nodes obtain the
process memberships as soon as possible.  If a remote node is down due to a
netsplit, it will obtain the local node's state once it reconnects as
described above.

## Build

    rebar get-deps
    rebar compile

## Usage

If you need non-string (not a list of integers) group names
(e.g., when replacing gproc), you can change the cpg application
`group_storage` env setting by providing a module name that provides a
dict module interface (or just set to `dict`).

## Example

    $ erl -sname cpg@localhost -pz ebin/ -pz deps/*/ebin/
    
    (cpg@localhost)1> reltool_util:application_start(cpg).
    ok
    (cpg@localhost)2> cpg:join(groups_scope1, "Hello", self()).
    ok
    (cpg@localhost)3> cpg:join(groups_scope1, "World!", self()).
    ok
    (cpg@localhost)4> cpg:get_local_members(groups_scope1, "Hello").
    {ok,"Hello",[<0.39.0>]}
    (cpg@localhost)5> cpg:get_local_members(groups_scope1, "World!").
    {ok,"World!",[<0.39.0>]}
    (cpg@localhost)6> cpg:which_groups(groups_scope1).
    ["Hello","World!"]
    (cpg@localhost)7> cpg:which_groups(groups_scope2).
    []

What does this example mean?  The cpg interface allows you to define groups of
Erlang processes and each group exists within a scope.  A scope is represented
as an atom which is used to locally register a cpg Erlang process using
`start_link/1`.  For a given cpg scope, any Erlang process can join or leave
a group.  The group name is a string (list of integers) due to the default
usage of the trie data structure, but that can be changed
(see the [Usage](#usage) section above).  If the scope is not specified, the default
scope is used: `cpg_default_scope`.

In the example, both the process group "Hello" and the process group "World!"
are created within the `groups_scope1` scope.  Within both progress groups,
a single Erlang process is added once.  If more scopes were required, they
could be created automatically by being provided within the cpg application
scope list.  There is no restriction on the number of process groups that
can be created within a scope, and there is nothing limiting the number
of Erlang processes that can be added to a single group.  A single Erlang
process can be added to a single process group in a single scope multiple times
to change the probability of returning a particular Erlang process, when
only a single process is requested from the cpg interface (e.g., from
the `get_closest_pid` function).
    
## Tests

    rebar get-deps
    rebar compile
    ERL_LIBS="/path/to/proper" rebar eunit

## Author

Michael Truog (mjtruog at protonmail dot com)

## License

MIT License

## References

1. Carlos Baquero, Paulo Sérgio Almeida, Ali Shoker.  Making operation-based crdts operation-based. In Proceedings of the First Workshop on Principles and Practice of Eventual Consistency, page 7. ACM, 2014. [http://haslab.uminho.pt/ashoker/files/opbaseddais14.pdf](http://haslab.uminho.pt/ashoker/files/opbaseddais14.pdf)
1. Carlos Baquero, Paulo Sérgio Almeida, Ali Shoker.  Pure Operation-Based Replicated Data Types. 2017. [https://arxiv.org/abs/1710.04469](https://arxiv.org/abs/1710.04469)

