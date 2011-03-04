This is a more advanced example of Erlectricity that shows how you can
integrate with Campfire via the Ruby "tinder" gem.

    $ cd examples/tinderl
    $ erl
    Erlang (BEAM) emulator version 5.6.4 [source] [smp:2] [async-threads:0] [kernel-poll:false]

    Eshell V5.6.4  (abort with ^G)
    1> c(tinderl).
    {ok,tinderl}
    2> tinderl:start("example.campfireapp.com", "tom@example.com", "secret", "My Room").
    <0.38.0>
    5> tinderl:speak("Hello, World!").
    {speak,<0.31.0>,<<"Hello, World!">>}
