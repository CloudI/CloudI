## Version 2.6.3.1

 * Reverse breaking exception change in `Network.Socket.ByteString.recv`
   [#215](https://github.com/haskell/network/issues/215)

## Version 2.6.3.0

 * New maintainers: Evan Borden (@eborden) and Kazu Yamamoto (@kazu-yamamoto).
   The maintainer for a long period, Johan Tibell (@tibbe) stepped down.
   Thank you, Johan, for your hard work for a long time.

 * New APIs: ntohl, htonl,hostAddressToTuple{,6} and tupleToHostAddress{,6}.
   [#210](https://github.com/haskell/network/pull/210)

 * Added a Read instance for PortNumber. [#145](https://github.com/haskell/network/pull/145)

 * We only set the IPV6_V6ONLY flag to 0 for stream and datagram socket types,
   as opposed to all of them. This makes it possible to use ICMPv6.
   [#180](https://github.com/haskell/network/pull/180)
   [#181](https://github.com/haskell/network/pull/181)

 * Work around GHC bug #12020. Socket errors no longer cause segfaults or
   hangs on Windows. [#192](https://github.com/haskell/network/pull/192)

 * Various documentation improvements and the deprecated pragmas.
   [#186](https://github.com/haskell/network/pull/186)
   [#201](https://github.com/haskell/network/issues/201)
   [#205](https://github.com/haskell/network/pull/205)
   [#206](https://github.com/haskell/network/pull/206)
   [#211](https://github.com/haskell/network/issues/211)

 * Various internal improvements.
   [#193](https://github.com/haskell/network/pull/193)
   [#200](https://github.com/haskell/network/pull/200)

## Version 2.6.2.1

 * Regenerate configure and HsNetworkConfig.h.in.

 * Better detection of CAN sockets.

## Version 2.6.2.0

 * Add support for TCP_USER_TIMEOUT.

 * Don't conditionally export the SockAddr constructors.

 * Add isSupportSockAddr to allow checking for supported address types
   at runtime.
