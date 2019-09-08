## Version 3.1.0.1

* getAddrInfo: raise exception if no AddrInfo returned.
  [#410](https://github.com/haskell/network/pull/410)
* Avoid catching SomeException.
  [#411](https://github.com/haskell/network/pull/411)

## Version 3.1.0.0

* Making GC of socket safer.
  [#399](https://github.com/haskell/network/pull/399)
* Deprecating fdSocket. Use withFdSocket instead to ensure
  that sockets are GCed in proper time.
  [#399](https://github.com/haskell/network/pull/399)

## Version 3.0.1.1

* Fix blocking `if_nametoindex` errors on Windows
  [#391](https://github.com/haskell/network/pull/391)

## Version 3.0.1.0

* Added `getSocketType :: Socket -> IO SocketType`.
  [#372](https://github.com/haskell/network/pull/372)
* Correcting manual and brushing up test cases
  [#375](https://github.com/haskell/network/pull/375)
* Fixed longstanded bug in `getContents` on mac
  [#375](https://github.com/haskell/network/pull/375)
* Fixing regression: set correct sockaddr length for abstract addresses
  for Linux.
  [#374](https://github.com/haskell/network/pull/374)

## Version 3.0.0.1

* Fixed a bug in `connect` where exceptions were not thrown
  [#368](https://github.com/haskell/network/pull/368)

## Version 3.0.0.0

* Breaking change: the Network and Network.BSD are removed.
  Network.BSD is provided a new package: network-bsd.
* Breaking change: the signatures are changed:
```
old fdSocket :: Socket -> CInt
new fdSocket :: Socket -> IO CInt

old mkSocket :: CInt -> Family -> SocketType -> ProtocolNumber -> SocketStatus -> IO Socket
new mkSocket :: CInt -> IO Socket
```
* Breaking change: the deprecated APIs are removed: send, sendTo, recv, recvFrom, recvLen, htonl, ntohl, inet_addr, int_ntoa, bindSocket, sClose, SocketStatus, isConnected, isBound, isListening, isReadable, isWritable, sIsConnected, sIsBound, sIsListening, sIsReadable, sIsWritable, aNY_PORT, iNADDR_ANY, iN6ADDR_ANY, sOMAXCONN, sOL_SOCKET, sCM_RIGHTS, packSocketType, getPeerCred.
* Breaking change: SockAddrCan is removed from SockAddr.
* Socket addresses are extendable with Network.Socket.Address.
* "socket" is now asynchronous-exception-safe.
  [#336](https://github.com/haskell/network/pull/336)
* "recvFrom" returns (0, addr) instead of throwing an error on EOF.
  [#360](https://github.com/haskell/network/pull/360)
* All APIs are available on any platforms.
* Build system is simplified.
* Bug fixes.

## Version 2.8.0.1

* Eensuring that accept returns a correct sockaddr for unix domain.
  [#400](https://github.com/haskell/network/pull/400)
* Avoid out of bounds writes in pokeSockAddr.
  [#400](https://github.com/haskell/network/pull/400)

## Version 2.8.0.0

* Breaking change: PortNumber originally contained Word16 in network
  byte order and used "deriving Ord". This results in strange behavior
  on the Ord instance. Now PortNumber holds Word16 in host byte order.
  [#347](https://github.com/haskell/network/pull/347)
* Breaking change: stopping the export of the PortNum constructor in
  PortNumber.
* Use bytestring == 0.10.* only.
* Use base >= 4.7 && < 5.

## Version 2.7.0.2

* Removing withMVar to avoid the deadlock between "accept" and "close"
   [#330](https://github.com/haskell/network/pull/330)
* "close" does not throw exceptions. A new API: "close'" throws
   exceptions when necessary.
   [#337](https://github.com/haskell/network/pull/337)
* Fixing the hang of lazy sendAll.
   [#340](https://github.com/haskell/network/pull/340)
* Installing NetDef.h (#334)
   [#334](https://github.com/haskell/network/pull/334)

## Version 2.7.0.1

 * A new API: socketPortSafe.
   [#319](https://github.com/haskell/network/pull/319)
 * Fixing a drain bug of sendAll.
   [#320](https://github.com/haskell/network/pull/320)
 * Porting the new CALLCONV convention from master.
   [#313](https://github.com/haskell/network/pull/313)
 * Withdrawing the deprecations of packFamily and unpackFamily.
   [#324](https://github.com/haskell/network/pull/324)

## Version 2.7.0.0

 * Obsoleting the Network module.
 * Obsoleting the Network.BSD module.
 * Obsoleting APIs: MkSocket, htonl, ntohl,
              getPeerCred, getPeerEid,
              send, sendTo, recv, recvFrom, recvLen,
              inet_addr, inet_ntoa,
              isConnected, isBound, isListening, isReadable, isWritable,
              aNY_PORT, iNADDR_ANY, iN6ADDR_ANY, sOMAXCONN,
              sOL_SOCKET, sCM_RIGHTS,
              packFamily, unpackFamily, packSocketType
 * Breaking change: do not closeFd within sendFd.
   [#271](https://github.com/haskell/network/pull/271)
 * Exporting ifNameToIndex and ifIndexToName from Network.Socket.
 * New APIs: setCloseOnExecIfNeeded, getCloseOnExec and getNonBlock
 * New APIs: isUnixDomainSocketAvailable and getPeerCredential
 * socketPair, sendFd and recvFd are exported even on Windows.

## Version 2.6.3.5

 * Reverting "Do not closeFd within sendFd"
   [#271](https://github.com/haskell/network/pull/271)

## Version 2.6.3.4

 * Don't touch IPv6Only when running on OpenBSD
   [#227](https://github.com/haskell/network/pull/227)
 * Do not closeFd within sendFd
   [#271](https://github.com/haskell/network/pull/271)
 * Updating examples and docs.

## Version 2.6.3.3

 * Adds a function to show the defaultHints without reading their undefined fields
   [#291](https://github.com/haskell/network/pull/292)
 * Improve exception error messages for getAddrInfo and getNameInfo
   [#289](https://github.com/haskell/network/pull/289)

## Version 2.6.3.2

 * Zero memory of `sockaddr_un` if abstract socket
   [#220](https://github.com/haskell/network/pull/220)

 * Improving error messages
   [#232](https://github.com/haskell/network/pull/232)

 * Allow non-blocking file descriptors via `setNonBlockIfNeeded`
   [#242](https://github.com/haskell/network/pull/242)

 * Update config.{guess,sub} to latest version
   [#244](https://github.com/haskell/network/pull/244)

 * Rename `my_inet_ntoa` to avoid symbol conflicts
   [#228](https://github.com/haskell/network/pull/228)

 * Test infrastructure improvements
   [#219](https://github.com/haskell/network/pull/219)
   [#217](https://github.com/haskell/network/pull/217)
   [#218](https://github.com/haskell/network/pull/218)

 * House keeping and cleanup
   [#238](https://github.com/haskell/network/pull/238)
   [#237](https://github.com/haskell/network/pull/237)

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

 * Regenerate configure and `HsNetworkConfig.h.in`.

 * Better detection of CAN sockets.

## Version 2.6.2.0

 * Add support for `TCP_USER_TIMEOUT`.

 * Don't conditionally export the `SockAddr` constructors.

 * Add `isSupportSockAddr` to allow checking for supported address types
   at runtime.
