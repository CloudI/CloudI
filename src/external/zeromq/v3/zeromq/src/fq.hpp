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

#ifndef __ZMQ_FQ_HPP_INCLUDED__
#define __ZMQ_FQ_HPP_INCLUDED__

#include "array.hpp"
#include "pipe.hpp"
#include "msg.hpp"

namespace zmq
{

    //  Class manages a set of inbound pipes. On receive it performs fair
    //  queueing so that senders gone berserk won't cause denial of
    //  service for decent senders.

    class fq_t
    {
    public:

        fq_t ();
        ~fq_t ();

        void attach (pipe_t *pipe_);
        void activated (pipe_t *pipe_);
        void terminated (pipe_t *pipe_);

        int recv (msg_t *msg_);
        int recvpipe (msg_t *msg_, pipe_t **pipe_);
        bool has_in ();

    private:

        //  Inbound pipes.
        typedef array_t <pipe_t, 1> pipes_t;
        pipes_t pipes;

        //  Number of active pipes. All the active pipes are located at the
        //  beginning of the pipes array.
        pipes_t::size_type active;

        //  Index of the next bound pipe to read a message from.
        pipes_t::size_type current;

        //  If true, part of a multipart message was already received, but
        //  there are following parts still waiting in the current pipe.
        bool more;

        fq_t (const fq_t&);
        const fq_t &operator = (const fq_t&);
    };

}

#endif
