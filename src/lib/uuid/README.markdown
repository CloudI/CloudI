Erlang UUID Implementation
==========================

[![Build Status](https://secure.travis-ci.org/okeuday/uuid.png?branch=master)](http://travis-ci.org/okeuday/uuid)

http://www.ietf.org/rfc/rfc4122.txt is the reference for official UUIDs.
This implementation provides a version 1 UUID that includes both the Erlang pid
identifier (ID, Serial, Creation) and the distributed Erlang node name within
the 48 bit node ID.  To make room for the Erlang pid identifier, the 48 bits
from the MAC address (i.e., 3 OCI (Organizationally Unique Identifier) bytes and
3 NIC (Network Interface Controller) specific bytes) and the distributed Erlang
node name are bitwise-XORed down to 16 bits. The Erlang pid is 
bitwise-XORed from 72 bits down to 32 bits.
The version 3 (MD5), version 4 (random), and version 5 (SHA)
methods are provided as specified within the RFC.

Requires `Erlang >= R16B01`

If you use the functions `uuid:new/1`, `uuid:new/2` or `uuid:get_v4_urandom/0`
in an Erlang process, you must have called the function `quickrand:seed/0`
first (within the same Erlang process).

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

