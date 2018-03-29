Quick Random Number Generation
==============================

[![Build Status](https://secure.travis-ci.org/okeuday/quickrand.png?branch=master)](http://travis-ci.org/okeuday/quickrand)

Provides a simple interface to call efficient random number generation
functions based on the context.  Proper random number seeding is enforced.

`random_wh82` is provided as an alternative to the `random` module
which is scheduled to be removed in Erlang/OTP 20.

Build
-----

    rebar compile

Author
------

Michael Truog (mjtruog at protonmail dot com)

Thanks
------

* Raimo Niskanen (Box-Muller transformation floating-point period)
* Richard O'Keefe (floating-point random number period insights)

License
-------

MIT License

