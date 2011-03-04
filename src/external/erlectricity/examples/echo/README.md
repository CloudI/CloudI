This is a simple echo server example showing off basic Erlectricity usage.

    $ cd examples/echo
    $ erl
    Erlang (BEAM) emulator version 5.6.4 [source] [smp:2] [async-threads:0] [kernel-poll:false]

    Eshell V5.6.4  (abort with ^G)
    1> c(echo).
    {ok,echo}
    2> echo:test().
    <<"You said: hello world!">>
    ok
