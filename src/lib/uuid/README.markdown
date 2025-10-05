Erlang UUID Implementation
==========================

[![Build Status](https://app.travis-ci.com/okeuday/uuid.svg?branch=master)](https://app.travis-ci.com/okeuday/uuid)

http://www.ietf.org/rfc/rfc9562.txt is the reference for UUIDs
(replacing http://www.ietf.org/rfc/rfc4122.txt).

This implementation provides a version 1 UUID that includes both the Erlang pid
identifier (ID, Serial, Creation) and the distributed Erlang node name within
the 48 bit node ID.  To make room for the Erlang pid identifier, the 48 bits
from the MAC address (i.e., 3 OCI (Organizationally Unique Identifier) bytes and
3 NIC (Network Interface Controller) specific bytes) and the distributed Erlang
node name are bitwise-XORed down to 16 bits. The Erlang pid is 
bitwise-XORed from 72 bits down to 32 bits.

The version 3 (MD5), version 4 (random), and version 5 (SHA)
methods are provided as specified within the RFC 4122.

The ordered version 1 variant was used before the version 6 UUID
was defined by RFC 9562.  Use the version 6 UUID instead, if possible.

The version 6 (ordered v1), version 7 (UNIX epoch), and version 8 (custom)
methods are provided as specified within RFC 9562.  The version 7 UUID
provides less time precision than version 1 and version 6 UUIDs.

Requires `Erlang >= R16B01`

Usage
-----

Certain `uuid` functions require initializing the Erlang process before
the function is called.  The initialization ensures the `quickrand` dependency
is able to provide randomness for the `uuid` module use.

If you use the functions `uuid:new/1`, `uuid:new/2` or `uuid:get_v4_urandom/0`,
you should call the function `quickrand:seed/0` or `quickrand:seed/1` first.

If you use the function `uuid:get_v4/1` with a `cached` argument,
you should call the function `quickrand_cache:init/0` or
`quickrand_cache:init/1` first.  If you use the function `uuid:get_v4/1` with
`quickrand_cache` state, you would have called the function
`quickrand_cache:new/0` or `quickrand_cache:new/1` first.
Using the `quickrand_cache` `init` function means cached random data is stored
in the process dictionary and using the `quickrand_cache` `new` function
(instead of `init`) means cached random data is kept in a state variable.

Build
-----

    rebar get-deps
    rebar compile

Author
------

Michael Truog (mjtruog at protonmail dot com)

License
-------

MIT License

