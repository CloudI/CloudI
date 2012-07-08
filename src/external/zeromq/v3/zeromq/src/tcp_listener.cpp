/*
    Copyright (c) 2009-2011 250bpm s.r.o.
    Copyright (c) 2007-2010 iMatix Corporation
    Copyright (c) 2007-2011 Other contributors as noted in the AUTHORS file

    This file is part of 0MQ.

    0MQ is free software; you can redistribute it and/or modify it under
    the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation; either version 3 of the License, or
    (at your option) any later version.

    0MQ is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
*/

#include <new>

#include <string>

#include "platform.hpp"
#include "tcp_listener.hpp"
#include "stream_engine.hpp"
#include "io_thread.hpp"
#include "session_base.hpp"
#include "config.hpp"
#include "err.hpp"
#include "ip.hpp"
#include "socket_base.hpp"

#ifdef ZMQ_HAVE_WINDOWS
#include "windows.hpp"
#else
#include <unistd.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/tcp.h>
#include <netinet/in.h>
#include <netdb.h>
#include <fcntl.h>
#endif

#ifdef ZMQ_HAVE_OPENVMS
#include <ioctl.h>
#endif

zmq::tcp_listener_t::tcp_listener_t (io_thread_t *io_thread_,
      socket_base_t *socket_, const options_t &options_) :
    own_t (io_thread_, options_),
    io_object_t (io_thread_),
    s (retired_fd),
    socket (socket_)
{
}

zmq::tcp_listener_t::~tcp_listener_t ()
{
    if (s != retired_fd)
        close ();
}

void zmq::tcp_listener_t::process_plug ()
{
    //  Start polling for incoming connections.
    handle = add_fd (s);
    set_pollin (handle);
}

void zmq::tcp_listener_t::process_term (int linger_)
{
    rm_fd (handle);
    close ();
    own_t::process_term (linger_);
}

void zmq::tcp_listener_t::in_event ()
{
    fd_t fd = accept ();

    //  If connection was reset by the peer in the meantime, just ignore it.
    //  TODO: Handle specific errors like ENFILE/EMFILE etc.
    if (fd == retired_fd) {
        socket->monitor_event (ZMQ_EVENT_ACCEPT_FAILED, endpoint.c_str(), zmq_errno());
        return;
    }

    tune_tcp_socket (fd);
    tune_tcp_keepalives (fd, options.tcp_keepalive, options.tcp_keepalive_cnt, options.tcp_keepalive_idle, options.tcp_keepalive_intvl);

    //  Create the engine object for this connection.
    stream_engine_t *engine = new (std::nothrow) stream_engine_t (fd, options);
    alloc_assert (engine);

    //  Choose I/O thread to run connecter in. Given that we are already
    //  running in an I/O thread, there must be at least one available.
    io_thread_t *io_thread = choose_io_thread (options.affinity);
    zmq_assert (io_thread);

    //  Create and launch a session object. 
    session_base_t *session = session_base_t::create (io_thread, false, socket,
        options, NULL);
    errno_assert (session);
    session->inc_seqnum ();
    launch_child (session);
    send_attach (session, engine, false);
    socket->monitor_event (ZMQ_EVENT_ACCEPTED, endpoint.c_str(), fd);
}

void zmq::tcp_listener_t::close ()
{
    zmq_assert (s != retired_fd);
#ifdef ZMQ_HAVE_WINDOWS
    int rc = closesocket (s);
    if (unlikely (rc != SOCKET_ERROR))
        socket->monitor_event (ZMQ_EVENT_CLOSE_FAILED, endpoint.c_str(), zmq_errno());
    wsa_assert (rc != SOCKET_ERROR);
#else
    int rc = ::close (s);
    if (unlikely (rc == 0))
        socket->monitor_event (ZMQ_EVENT_CLOSE_FAILED, endpoint.c_str(), zmq_errno());
    errno_assert (rc == 0);
#endif
    socket->monitor_event (ZMQ_EVENT_CLOSED, endpoint.c_str(), s);
    s = retired_fd;
}

int zmq::tcp_listener_t::get_address (std::string &addr_)
{
    // Get the details of the TCP socket
    struct sockaddr_storage ss;
    socklen_t sl = sizeof (ss);
    int rc = getsockname (s, (struct sockaddr *) &ss, &sl);

    if (rc != 0) {
        addr_.clear ();
        return rc;
    }

    tcp_address_t addr ((struct sockaddr *) &ss, sl);
    return addr.to_string (addr_);
}

int zmq::tcp_listener_t::set_address (const char *addr_)
{
    //  Convert the textual address into address structure.
    int rc = address.resolve (addr_, true, options.ipv4only ? true : false);
    if (rc != 0)
        return -1;

    //  Create a listening socket.
    s = open_socket (address.family (), SOCK_STREAM, IPPROTO_TCP);
#ifdef ZMQ_HAVE_WINDOWS
    if (s == INVALID_SOCKET)
        errno = wsa_error_to_errno (WSAGetLastError ());
#endif

    //  IPv6 address family not supported, try automatic downgrade to IPv4.
    if (address.family () == AF_INET6 && errno == EAFNOSUPPORT &&
          !options.ipv4only) {
        rc = address.resolve (addr_, true, true);
        if (rc != 0)
            return rc;
        s = ::socket (address.family (), SOCK_STREAM, IPPROTO_TCP);
    }

#ifdef ZMQ_HAVE_WINDOWS
    if (s == INVALID_SOCKET) {
        errno = wsa_error_to_errno (WSAGetLastError ());
        return -1;
    }
    //  On Windows, preventing sockets to be inherited by child processes.
    BOOL brc = SetHandleInformation ((HANDLE) s, HANDLE_FLAG_INHERIT, 0);
    win_assert (brc);
#else
    if (s == -1)
        return -1;
#endif

    //  On some systems, IPv4 mapping in IPv6 sockets is disabled by default.
    //  Switch it on in such cases.
    if (address.family () == AF_INET6)
        enable_ipv4_mapping (s);

    //  Allow reusing of the address.
    int flag = 1;
#ifdef ZMQ_HAVE_WINDOWS
    rc = setsockopt (s, SOL_SOCKET, SO_EXCLUSIVEADDRUSE,
        (const char*) &flag, sizeof (int));
    wsa_assert (rc != SOCKET_ERROR);
#else
    rc = setsockopt (s, SOL_SOCKET, SO_REUSEADDR, &flag, sizeof (int));
    errno_assert (rc == 0);
#endif

    address.to_string (endpoint);

    //  Bind the socket to the network interface and port.
    rc = bind (s, address.addr (), address.addrlen ());
#ifdef ZMQ_HAVE_WINDOWS
    if (rc == SOCKET_ERROR) {
        errno = wsa_error_to_errno (WSAGetLastError ());
        return -1;
    }
#else
    if (rc != 0)
        return -1;
#endif

    //  Listen for incomming connections.
    rc = listen (s, options.backlog);
#ifdef ZMQ_HAVE_WINDOWS
    if (rc == SOCKET_ERROR) {
        errno = wsa_error_to_errno (WSAGetLastError ());
        return -1;
    }
#else
    if (rc != 0)
        return -1;
#endif

    socket->monitor_event (ZMQ_EVENT_LISTENING, addr_, s);
    return 0;
}

zmq::fd_t zmq::tcp_listener_t::accept ()
{
    //  Accept one connection and deal with different failure modes.
    zmq_assert (s != retired_fd);

    struct sockaddr_storage ss = {0};
    socklen_t ss_len = sizeof (ss);
    fd_t sock = ::accept (s, (struct sockaddr *) &ss, &ss_len);

#ifdef ZMQ_HAVE_WINDOWS
    if (sock == INVALID_SOCKET) {
        wsa_assert (WSAGetLastError () == WSAEWOULDBLOCK ||
            WSAGetLastError () == WSAECONNRESET);
        return retired_fd;
    }
    //  On Windows, preventing sockets to be inherited by child processes.
    BOOL brc = SetHandleInformation ((HANDLE) sock, HANDLE_FLAG_INHERIT, 0);
    win_assert (brc);
#else
    if (sock == -1) {
        errno_assert (errno == EAGAIN || errno == EWOULDBLOCK ||
            errno == EINTR || errno == ECONNABORTED || errno == EPROTO ||
            errno == ENOBUFS);
        return retired_fd;
    }
#endif

    if (!options.tcp_accept_filters.empty ()) {
        bool matched = false;
        for (options_t::tcp_accept_filters_t::size_type i = 0; i != options.tcp_accept_filters.size (); ++i) {
            if (options.tcp_accept_filters[i].match_address ((struct sockaddr *) &ss, ss_len)) {
                matched = true;
                break;
            }
        }
        if (!matched) {
#ifdef ZMQ_HAVE_WINDOWS
            int rc = closesocket (sock);
            wsa_assert (rc != SOCKET_ERROR);
#else
            int rc = ::close (sock);
            errno_assert (rc == 0);
#endif
            return retired_fd;
        }
    }

    return sock;
}
