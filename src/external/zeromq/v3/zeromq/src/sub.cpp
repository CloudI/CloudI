/*
    Copyright (c) 2007-2012 iMatix Corporation
    Copyright (c) 2009-2011 250bpm s.r.o.
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

#include "sub.hpp"
#include "msg.hpp"

zmq::sub_t::sub_t (class ctx_t *parent_, uint32_t tid_, int sid_) :
    xsub_t (parent_, tid_, sid_)
{
    options.type = ZMQ_SUB;

    //  Switch filtering messages on (as opposed to XSUB which where the
    //  filtering is off).
    options.filter = true;
}

zmq::sub_t::~sub_t ()
{
}

int zmq::sub_t::xsetsockopt (int option_, const void *optval_,
    size_t optvallen_)
{
    if (option_ != ZMQ_SUBSCRIBE && option_ != ZMQ_UNSUBSCRIBE) {
        errno = EINVAL;
        return -1;
    }

    //  Create the subscription message.
    msg_t msg;
    int rc = msg.init_size (optvallen_ + 1);
    errno_assert (rc == 0);
    unsigned char *data = (unsigned char*) msg.data ();
    if (option_ == ZMQ_SUBSCRIBE)
        *data = 1;
    else if (option_ == ZMQ_UNSUBSCRIBE)
        *data = 0;
    memcpy (data + 1, optval_, optvallen_);

    //  Pass it further on in the stack.
    int err = 0;
    rc = xsub_t::xsend (&msg, 0);
    if (rc != 0)
        err = errno;
    int rc2 = msg.close ();
    errno_assert (rc2 == 0);
    if (rc != 0)
        errno = err;
    return rc;
}

int zmq::sub_t::xsend (msg_t *msg_, int flags_)
{
    //  Overload the XSUB's send.
    errno = ENOTSUP;
    return -1;
}

bool zmq::sub_t::xhas_out ()
{
    //  Overload the XSUB's send.
    return false;
}

zmq::sub_session_t::sub_session_t (io_thread_t *io_thread_, bool connect_,
      socket_base_t *socket_, const options_t &options_,
      const address_t *addr_) :
    xsub_session_t (io_thread_, connect_, socket_, options_, addr_)
{
}

zmq::sub_session_t::~sub_session_t ()
{
}

