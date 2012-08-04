CPG (CloudI Process Groups)
===========================

Documentation, error handling, and tests will be coming soon!

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

