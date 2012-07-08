/*
    Copyright (c) 2009-2011 250bpm s.r.o.
    Copyright (c) 2007-2009 iMatix Corporation
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

#include "tcp_connecter.hpp"
#include "stream_engine.hpp"
#include "io_thread.hpp"
#include "platform.hpp"
#include "random.hpp"
#include "err.hpp"
#include "ip.hpp"
#include "address.hpp"
#include "tcp_address.hpp"
#include "session_base.hpp"

#if defined ZMQ_HAVE_WINDOWS
#include "windows.hpp"
#else
#include <unistd.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <netinet/tcp.h>
#include <netinet/in.h>
#include <netdb.h>
#include <fcntl.h>
#ifdef ZMQ_HAVE_OPENVMS
#include <ioctl.h>
#endif
#endif

zmq::tcp_connecter_t::tcp_connecter_t (class io_thread_t *io_thread_,
      class session_base_t *session_, const options_t &options_,
      const address_t *addr_, bool wait_) :
    own_t (io_thread_, options_),
    io_object_t (io_thread_),
    addr (addr_),
    s (retired_fd),
    handle_valid (false),
    wait (wait_),
    session (session_),
    current_reconnect_ivl(options.reconnect_ivl)
{
    zmq_assert (addr);
    zmq_assert (addr->protocol == "tcp");
    addr->to_string (endpoint);
}

zmq::tcp_connecter_t::~tcp_connecter_t ()
{
    if (wait)
        cancel_timer (reconnect_timer_id);
    if (handle_valid)
        rm_fd (handle);

    if (s != retired_fd)
        close ();
}

void zmq::tcp_connecter_t::process_plug ()
{
    if (wait)
        add_reconnect_timer();
    else
        start_connecting ();
}

void zmq::tcp_connecter_t::in_event ()
{
    //  We are not polling for incomming data, so we are actually called
    //  because of error here. However, we can get error on out event as well
    //  on some platforms, so we'll simply handle both events in the same way.
    out_event ();
}

void zmq::tcp_connecter_t::out_event ()
{
    fd_t fd = connect ();
    rm_fd (handle);
    handle_valid = false;

    //  Handle the error condition by attempt to reconnect.
    if (fd == retired_fd) {
        close ();
        wait = true;
        add_reconnect_timer();
        return;
    }

    tune_tcp_socket (fd);
    tune_tcp_keepalives (fd, options.tcp_keepalive, options.tcp_keepalive_cnt, options.tcp_keepalive_idle, options.tcp_keepalive_intvl);

    //  Create the engine object for this connection.
    stream_engine_t *engine = new (std::nothrow) stream_engine_t (fd, options);
    alloc_assert (engine);

    //  Attach the engine to the corresponding session object.
    send_attach (session, engine);

    //  Shut the connecter down.
    terminate ();

    session->monitor_event (ZMQ_EVENT_CONNECTED, endpoint.c_str(), fd);
}

void zmq::tcp_connecter_t::timer_event (int id_)
{
    zmq_assert (id_ == reconnect_timer_id);
    wait = false;
    start_connecting ();
}

void zmq::tcp_connecter_t::start_connecting ()
{
    //  Open the connecting socket.
    int rc = open ();

    //  Connect may succeed in synchronous manner.
    if (rc == 0) {
        handle = add_fd (s);
        handle_valid = true;
        out_event ();
        return;
    }

    //  Connection establishment may be delayed. Poll for its completion.
    else if (rc == -1 && errno == EINPROGRESS) {
        handle = add_fd (s);
        handle_valid = true;
        set_pollout (handle);
        session->monitor_event (ZMQ_EVENT_CONNECT_DELAYED, endpoint.c_str(), zmq_errno());
        return;
    }

    //  Handle any other error condition by eventual reconnect.
    close ();
    wait = true;
    add_reconnect_timer();
}

void zmq::tcp_connecter_t::add_reconnect_timer()
{
    int rc_ivl = get_new_reconnect_ivl();
    add_timer (rc_ivl, reconnect_timer_id);
    session->monitor_event (ZMQ_EVENT_CONNECT_RETRIED, endpoint.c_str(), rc_ivl);
}

int zmq::tcp_connecter_t::get_new_reconnect_ivl ()
{
    //  The new interval is the current interval + random value.
    int this_interval = current_reconnect_ivl +
        (generate_random () % options.reconnect_ivl);

    //  Only change the current reconnect interval  if the maximum reconnect
    //  interval was set and if it's larger than the reconnect interval.
    if (options.reconnect_ivl_max > 0 && 
        options.reconnect_ivl_max > options.reconnect_ivl) {

        //  Calculate the next interval
        current_reconnect_ivl = current_reconnect_ivl * 2;
        if(current_reconnect_ivl >= options.reconnect_ivl_max) {
            current_reconnect_ivl = options.reconnect_ivl_max;
        }   
    }
    return this_interval;
}

int zmq::tcp_connecter_t::open ()
{
    zmq_assert (s == retired_fd);

    //  Create the socket.
    s = open_socket (addr->resolved.tcp_addr->family (), SOCK_STREAM, IPPROTO_TCP);
#ifdef ZMQ_HAVE_WINDOWS
    if (s == INVALID_SOCKET) {
        errno = wsa_error_to_errno (WSAGetLastError ());
        return -1;
    }
#else
    if (s == -1)
        return -1;
#endif

    //  On some systems, IPv4 mapping in IPv6 sockets is disabled by default.
    //  Switch it on in such cases.
    if (addr->resolved.tcp_addr->family () == AF_INET6)
        enable_ipv4_mapping (s);

    // Set the socket to non-blocking mode so that we get async connect().
    unblock_socket (s);

    //  Connect to the remote peer.
    int rc = ::connect (
        s, addr->resolved.tcp_addr->addr (),
        addr->resolved.tcp_addr->addrlen ());

    //  Connect was successfull immediately.
    if (rc == 0)
        return 0;

    //  Translate error codes indicating asynchronous connect has been
    //  launched to a uniform EINPROGRESS.
#ifdef ZMQ_HAVE_WINDOWS
    const int error_code = WSAGetLastError ();
    if (error_code == WSAEINPROGRESS || error_code == WSAEWOULDBLOCK)
        errno = EINPROGRESS;
    else
        errno = wsa_error_to_errno (error_code);
#else
    if (errno == EINTR)
        errno = EINPROGRESS;
#endif
    return -1;
}

zmq::fd_t zmq::tcp_connecter_t::connect ()
{
    //  Async connect have finished. Check whether an error occured.
    int err = 0;
#if defined ZMQ_HAVE_HPUX
    int len = sizeof (err);
#else
    socklen_t len = sizeof (err);
#endif

    int rc = getsockopt (s, SOL_SOCKET, SO_ERROR, (char*) &err, &len);

    //  Assert if the error was caused by 0MQ bug.
    //  Networking problems are OK. No need to assert.
#ifdef ZMQ_HAVE_WINDOWS
    zmq_assert (rc == 0);
    if (err != 0) {
        if (err == WSAECONNREFUSED || err == WSAETIMEDOUT ||
              err == WSAECONNABORTED || err == WSAEHOSTUNREACH ||
              err == WSAENETUNREACH || err == WSAENETDOWN)
            return retired_fd;
        wsa_assert_no (err);
    }
#else

    //  Following code should handle both Berkeley-derived socket
    //  implementations and Solaris.
    if (rc == -1)
        err = errno;
    if (err != 0) {
        errno = err;
        errno_assert (errno == ECONNREFUSED || errno == ECONNRESET ||
            errno == ETIMEDOUT || errno == EHOSTUNREACH ||
            errno == ENETUNREACH || errno == ENETDOWN);
        return retired_fd;
    }
#endif

    //  Return the newly connected socket.
    fd_t result = s;
    s = retired_fd;
    return result;
}

void zmq::tcp_connecter_t::close ()
{
    zmq_assert (s != retired_fd);
#ifdef ZMQ_HAVE_WINDOWS
    int rc = closesocket (s);
    if (unlikely (rc != SOCKET_ERROR))
        session->monitor_event (ZMQ_EVENT_CLOSE_FAILED, endpoint.c_str(), zmq_errno());
    wsa_assert (rc != SOCKET_ERROR);
#else
    int rc = ::close (s);
    if (unlikely (rc == 0))
        session->monitor_event (ZMQ_EVENT_CLOSE_FAILED, endpoint.c_str(), zmq_errno());
    errno_assert (rc == 0);
#endif
    session->monitor_event (ZMQ_EVENT_CLOSED, endpoint.c_str(), s);
    s = retired_fd;
}
