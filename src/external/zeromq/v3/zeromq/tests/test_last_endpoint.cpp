/*
    Copyright (c) 2007-2012 iMatix Corporation
    Copyright (c) 2011 250bpm s.r.o.
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

#include <assert.h>
#include <string.h>

#include "../include/zmq.h"

static void do_bind_and_verify (void *s, const char *endpoint)
{
    int rc = zmq_bind (s, endpoint);
    assert (rc == 0);

    char test [255];
    size_t siz = 255;
    rc = zmq_getsockopt (s, ZMQ_LAST_ENDPOINT, test, &siz);
    assert (rc == 0 && strcmp (test, endpoint) == 0);
}

int main (int argc, char *argv [])
{
    //  Create the infrastructure
    void *ctx = zmq_init (1);
    assert (ctx);

    void *sb = zmq_socket (ctx, ZMQ_ROUTER);
    assert (sb);

    do_bind_and_verify (sb, "tcp://127.0.0.1:5560");
    do_bind_and_verify (sb, "tcp://127.0.0.1:5561");
    do_bind_and_verify (sb, "ipc:///tmp/testep");

    return 0 ;
}

