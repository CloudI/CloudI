nodefinder: Strategies for automatic node discovery in Erlang
=============================================================

Description
-----------

All strategies have been combined into this Erlang application after being
refactored.

(The source code is a fork of http://code.google.com/p/nodefinder/)

EC2 tags and groups selection are selected with the following syntax
(if a tuple doesn't make the boolean expression
 explicitly AND or OR the list is implicitly an OR):

    [Entry... |
     {'AND', [Entry...]} |
     {'OR', [Entry...]}]
     
      tags: Entry == "key" |
                     ["key1", "key2"] |
                     {"key3", "value3"} |
                     {["key4", "key5"], "value4"} |
                     {"key6", ["value6", "value7"]} |
                     {["key8", "key9"], ["value9", "value8"]}
     
    groups: Entry == "security_group_name"


Build
-----

    rebar compile

Example
-------

**LAN multicast (requires that ntpd is running)**

    $ erl -sname foo1@localhost -pz ebin/
    (foo1@localhost)1> application:ensure_all_started(nodefinder).
    {ok,[inets,xmerl,crypto,asn1,public_key,ssl,nodefinder]}
    (foo1@localhost)2> nodes(). 
    []
    (foo1@localhost)3> nodefinder:multicast_start().
    {ok,<0.52.0>}
    (foo1@localhost)4> nodes().
    [foo2@localhost]
    (foo1@localhost)5> nodefinder:multicast_discover(5000).
    ok
    (foo1@localhost)6> nodes().
    [foo2@localhost]
    (foo1@localhost)7> nodefinder:multicast_stop().
    ok
    (foo1@localhost)8> nodefinder:multicast_stop().
    {error,not_found}
    
    $ erl -sname foo2@localhost -pz ebin/
    (foo2@localhost)1> application:ensure_all_started(nodefinder).
    {ok,[inets,xmerl,crypto,asn1,public_key,ssl,nodefinder]}
    (foo2@localhost)2> nodes(). 
    []
    (foo2@localhost)3> nodefinder:multicast_start().
    {ok,<0.52.0>}
    (foo2@localhost)4> nodes().
    [foo1@localhost]
    (foo2@localhost)5> nodefinder:multicast_discover(5000).
    ok
    (foo2@localhost)6> nodes().
    [foo1@localhost]
    (foo2@localhost)7> nodefinder:multicast_stop().
    ok


**EC2**

Functionality details:

* The same Erlang distributed node name is used
  (separate Erlang VMs must be on separate EC2 instances)
* Connections between regions are not supported due to the high latency

Add security group TCP rules for:

* port 4369 (`epmd`) [10.0.0.0/8](http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Subnets.html)
* ports 4374-4474 (`inet_dist_listen`) [10.0.0.0/8](http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_Subnets.html)

Example:

    $ cat << EOF > sys.config
    [{kernel, [
        {inet_dist_listen_min, 4374},
        {inet_dist_listen_max, 4474}]}].
    EOF
    $ erl -name test -config sys.config -pz ebin/
    > application:ensure_all_started(nodefinder).
    > nodes().
    > Host = "ec2.amazonaws.com".
    > Key = "".
    > SecretKey = "".
    > nodefinder:ec2_start(Key, SecretKey, Host, ["www"], []).
    > nodefinder:ec2_stop().
    > nodes().
    > [net_kernel:disconnect(N) || N <- nodes()].
    > nodefinder:ec2_start(Key, SecretKey, Host, [], [{"Tag1", "Value1"}]).
    > nodefinder:ec2_stop().
    > nodes().
    > [net_kernel:disconnect(N) || N <- nodes()].
    > nodefinder:ec2_start(Key, SecretKey, Host, [], ["Tag1"]).
    > nodefinder:ec2_stop().
    > nodes().
    > [net_kernel:disconnect(N) || N <- nodes()].
    

* First, connect to all EC2 instances in the "www" security group
  that are running with a 'test' node name
* Second, connect to all EC2 instances with a tag Tag1=Value1
  that are running with a 'test' node name
* Third, connect to all EC2 instances with a tag Tag1
  that are running with a 'test' node name

Tests
-----

    rebar compile
    rebar eunit skip_deps=true

License
-------

MIT License

