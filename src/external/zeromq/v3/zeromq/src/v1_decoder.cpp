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

#include <stdlib.h>
#include <string.h>

#include "platform.hpp"
#ifdef ZMQ_HAVE_WINDOWS
#include "windows.hpp"
#endif

#include "v1_protocol.hpp"
#include "v1_decoder.hpp"
#include "likely.hpp"
#include "wire.hpp"
#include "err.hpp"

zmq::v1_decoder_t::v1_decoder_t (size_t bufsize_,
      int64_t maxmsgsize_, i_msg_sink *msg_sink_) :
    decoder_base_t <v1_decoder_t> (bufsize_),
    msg_sink (msg_sink_),
    msg_flags (0),
    maxmsgsize (maxmsgsize_)
{
    int rc = in_progress.init ();
    errno_assert (rc == 0);

    //  At the beginning, read one byte and go to flags_ready state.
    next_step (tmpbuf, 1, &v1_decoder_t::flags_ready);
}

zmq::v1_decoder_t::~v1_decoder_t ()
{
    int rc = in_progress.close ();
    errno_assert (rc == 0);
}

void zmq::v1_decoder_t::set_msg_sink (i_msg_sink *msg_sink_)
{
    msg_sink = msg_sink_;
}

bool zmq::v1_decoder_t::flags_ready ()
{
    msg_flags = 0;
    if (tmpbuf [0] & v1_protocol_t::more_flag)
        msg_flags |= msg_t::more;

    //  The payload length is either one or eight bytes,
    //  depending on whether the 'large' bit is set.
    if (tmpbuf [0] & v1_protocol_t::large_flag)
        next_step (tmpbuf, 8, &v1_decoder_t::eight_byte_size_ready);
    else
        next_step (tmpbuf, 1, &v1_decoder_t::one_byte_size_ready);

    return true;
}

bool zmq::v1_decoder_t::one_byte_size_ready ()
{
    int rc = 0;

    //  Message size must not exceed the maximum allowed size.
    if (maxmsgsize >= 0)
        if (unlikely (tmpbuf [0] > static_cast <uint64_t> (maxmsgsize)))
            goto error;

    //  in_progress is initialised at this point so in theory we should
    //  close it before calling zmq_msg_init_size, however, it's a 0-byte
    //  message and thus we can treat it as uninitialised...
    rc = in_progress.init_size (tmpbuf [0]);
    if (unlikely (rc)) {
        errno_assert (errno == ENOMEM);
        int rc = in_progress.init ();
        errno_assert (rc == 0);
        goto error;
    }

    in_progress.set_flags (msg_flags);
    next_step (in_progress.data (), in_progress.size (),
        &v1_decoder_t::message_ready);

    return true;

error:
    decoding_error ();
    return false;
}

bool zmq::v1_decoder_t::eight_byte_size_ready ()
{
    int rc = 0;

    //  The payload size is encoded as 64-bit unsigned integer.
    //  The most significant byte comes first.
    const uint64_t msg_size = get_uint64 (tmpbuf);

    //  Message size must not exceed the maximum allowed size.
    if (maxmsgsize >= 0)
        if (unlikely (msg_size > static_cast <uint64_t> (maxmsgsize)))
            goto error;

    //  Message size must fit into size_t data type.
    if (unlikely (msg_size != static_cast <size_t> (msg_size)))
        goto error;

    //  in_progress is initialised at this point so in theory we should
    //  close it before calling init_size, however, it's a 0-byte
    //  message and thus we can treat it as uninitialised.
    rc = in_progress.init_size (static_cast <size_t> (msg_size));
    if (unlikely (rc)) {
        errno_assert (errno == ENOMEM);
        int rc = in_progress.init ();
        errno_assert (rc == 0);
        goto error;
    }

    in_progress.set_flags (msg_flags);
    next_step (in_progress.data (), in_progress.size (),
        &v1_decoder_t::message_ready);

    return true;

error:
    decoding_error ();
    return false;
}

bool zmq::v1_decoder_t::message_ready ()
{
    //  Message is completely read. Push it further and start reading
    //  new message. (in_progress is a 0-byte message after this point.)
    if (unlikely (!msg_sink))
        return false;
    int rc = msg_sink->push_msg (&in_progress);
    if (unlikely (rc != 0)) {
        if (errno != EAGAIN)
            decoding_error ();
        return false;
    }

    next_step (tmpbuf, 1, &v1_decoder_t::flags_ready);
    return true;
}
