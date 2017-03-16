/* -----------------------------------------------------------------------------
 *
 * Definitions for package `net' which are visible in Haskell land.
 *
 * ---------------------------------------------------------------------------*/

#ifndef HSNET_H
#define HSNET_H

#include "HsNetworkConfig.h"

#ifdef NEED_WINVER
# define WINVER 0x0501
#endif

/* ultra-evil... */
#undef PACKAGE_BUGREPORT
#undef PACKAGE_NAME
#undef PACKAGE_STRING
#undef PACKAGE_TARNAME
#undef PACKAGE_VERSION

#ifndef INLINE
# if defined(_MSC_VER)
#  define INLINE extern __inline
# elif defined(__GNUC_GNU_INLINE__)
#  define INLINE extern inline
# else
#  define INLINE inline
# endif
#endif

#ifdef HAVE_GETADDRINFO
# define IPV6_SOCKET_SUPPORT 1
#else
# undef IPV6_SOCKET_SUPPORT
#endif

#if defined(HAVE_WINSOCK2_H)
#include <winsock2.h>
# ifdef HAVE_WS2TCPIP_H
#  include <ws2tcpip.h>
// fix for MingW not defining IPV6_V6ONLY
#  define IPV6_V6ONLY 27
# endif

extern int   initWinSock ();
extern const char* getWSErrorDescr(int err);
extern void* newAcceptParams(int sock,
			     int sz,
			     void* sockaddr);
extern int   acceptNewSock(void* d);
extern int   acceptDoProc(void* param);

#else

#ifdef HAVE_LIMITS_H
# include <limits.h>
#endif
#ifdef HAVE_STDLIB_H
# include <stdlib.h>
#endif
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_TYPES_H
# include <sys/types.h>
#endif
#ifdef HAVE_FCNTL_H
# include <fcntl.h>
#endif
#ifdef HAVE_SYS_UIO_H
# include <sys/uio.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
# include <sys/socket.h>
#endif
#ifdef HAVE_LINUX_TCP_H
# include <linux/tcp.h>
#elif HAVE_NETINET_TCP_H
# include <netinet/tcp.h>
#endif
#ifdef HAVE_NETINET_IN_H
# include <netinet/in.h>
#endif
#ifdef HAVE_SYS_UN_H
# include <sys/un.h>
#endif
#ifdef HAVE_ARPA_INET_H
# include <arpa/inet.h>
#endif
#ifdef HAVE_NETDB_H
#include <netdb.h>
#endif
#ifdef HAVE_LINUX_CAN_H
# include <linux/can.h>
# define CAN_SOCKET_SUPPORT 1
#endif
#ifdef HAVE_NET_IF
# include <net/if.h>
#endif

#ifdef HAVE_BSD_SENDFILE
#include <sys/uio.h>
#endif
#ifdef HAVE_LINUX_SENDFILE
#if !defined(__USE_FILE_OFFSET64)
#include <sys/sendfile.h>
#endif
#endif

extern int
sendFd(int sock, int outfd);

extern int
recvFd(int sock);

#endif /* HAVE_WINSOCK2_H */

INLINE char *
my_inet_ntoa(
#if defined(HAVE_WINSOCK2_H)
             u_long addr
#elif defined(HAVE_IN_ADDR_T)
             in_addr_t addr
#elif defined(HAVE_INTTYPES_H)
             u_int32_t addr
#else
             unsigned long addr
#endif
	    )
{ 
    struct in_addr a;
    a.s_addr = addr;
    return inet_ntoa(a);
}

#ifdef HAVE_GETADDRINFO
INLINE int
hsnet_getnameinfo(const struct sockaddr* a,socklen_t b, char* c,
# if defined(HAVE_WINSOCK2_H)
                  DWORD d, char* e, DWORD f, int g)
# else
                  socklen_t d, char* e, socklen_t f, int g)
# endif
{
  return getnameinfo(a,b,c,d,e,f,g);
}

INLINE int
hsnet_getaddrinfo(const char *hostname, const char *servname,
		  const struct addrinfo *hints, struct addrinfo **res)
{
    return getaddrinfo(hostname, servname, hints, res);
}

INLINE void
hsnet_freeaddrinfo(struct addrinfo *ai)
{
    freeaddrinfo(ai);
}
#endif

#if defined(HAVE_WINSOCK2_H)
# define WITH_WINSOCK  1
#endif

#if !defined(mingw32_HOST_OS) && !defined(_WIN32)
# define DOMAIN_SOCKET_SUPPORT 1
#endif

#if !defined(CALLCONV)
# if defined(WITH_WINSOCK)
#  define CALLCONV stdcall
# else
#  define CALLCONV ccall
# endif
#endif

#if !defined(IOV_MAX)
# define IOV_MAX 1024
#endif

#if !defined(SOCK_NONBLOCK) // Missing define in Bionic libc (Android)
# define SOCK_NONBLOCK O_NONBLOCK
#endif

#endif /* HSNET_H */
