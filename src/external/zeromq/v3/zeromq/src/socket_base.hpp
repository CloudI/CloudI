/*
    Copyright (c) 2007-2012 iMatix Corporation
    Copyright (c) 2009-2011 250bpm s.r.o.
    Copyright (c) 2011 VMware, Inc.
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

#ifndef __ZMQ_SOCKET_BASE_HPP_INCLUDED__
#define __ZMQ_SOCKET_BASE_HPP_INCLUDED__

#include <string>
#include <map>

#include "own.hpp"
#include "array.hpp"
#include "stdint.hpp"
#include "poller.hpp"
#include "atomic_counter.hpp"
#include "i_poll_events.hpp"
#include "mailbox.hpp"
#include "stdint.hpp"
#include "clock.hpp"
#include "pipe.hpp"

namespace zmq
{

    class ctx_t;
    class msg_t;
    class pipe_t;

    class socket_base_t :
        public own_t,
        public array_item_t <>,
        public i_poll_events,
        public i_pipe_events
    {
        friend class reaper_t;

    public:

        //  Returns false if object is not a socket.
        bool check_tag ();

        //  Create a socket of a specified type.
        static socket_base_t *create (int type_, zmq::ctx_t *parent_,
            uint32_t tid_, int sid_);

        //  Returns the mailbox associated with this socket.
        mailbox_t *get_mailbox ();

        //  Interrupt blocking call if the socket is stuck in one.
        //  This function can be called from a different thread!
        void stop ();

        //  Interface for communication with the API layer.
        int setsockopt (int option_, const void *optval_, size_t optvallen_);
        int getsockopt (int option_, void *optval_, size_t *optvallen_);
        int bind (const char *addr_);
        int connect (const char *addr_);
        int term_endpoint (const char *addr_);
        int send (zmq::msg_t *msg_, int flags_);
        int recv (zmq::msg_t *msg_, int flags_);
        int close ();

        //  These functions are used by the polling mechanism to determine
        //  which events are to be reported from this socket.
        bool has_in ();
        bool has_out ();

        //  Using this function reaper thread ask the socket to regiter with
        //  its poller.
        void start_reaping (poller_t *poller_);

        //  i_poll_events implementation. This interface is used when socket
        //  is handled by the poller in the reaper thread.
        void in_event ();
        void out_event ();
        void timer_event (int id_);

        //  i_pipe_events interface implementation.
        void read_activated (pipe_t *pipe_);
        void write_activated (pipe_t *pipe_);
        void hiccuped (pipe_t *pipe_);
        void terminated (pipe_t *pipe_);
        void lock();
        void unlock();

        void monitor_event (int event_, ...);

    protected:

        socket_base_t (zmq::ctx_t *parent_, uint32_t tid_, int sid_);
        virtual ~socket_base_t ();

        //  Concrete algorithms for the x- methods are to be defined by
        //  individual socket types.
        virtual void xattach_pipe (zmq::pipe_t *pipe_,
            bool icanhasall_ = false) = 0;

        //  The default implementation assumes there are no specific socket
        //  options for the particular socket type. If not so, overload this
        //  method.
        virtual int xsetsockopt (int option_, const void *optval_,
            size_t optvallen_);

        //  The default implementation assumes that send is not supported.
        virtual bool xhas_out ();
        virtual int xsend (zmq::msg_t *msg_, int flags_);

        //  The default implementation assumes that recv in not supported.
        virtual bool xhas_in ();
        virtual int xrecv (zmq::msg_t *msg_, int flags_);

        //  i_pipe_events will be forwarded to these functions.
        virtual void xread_activated (pipe_t *pipe_);
        virtual void xwrite_activated (pipe_t *pipe_);
        virtual void xhiccuped (pipe_t *pipe_);
        virtual void xterminated (pipe_t *pipe_) = 0;

        //  Delay actual destruction of the socket.
        void process_destroy ();

    private:
        //  Creates new endpoint ID and adds the endpoint to the map.
        void add_endpoint (const char *addr_, own_t *endpoint_);

        //  Map of open endpoints.
        typedef std::multimap <std::string, own_t *> endpoints_t;
        endpoints_t endpoints;

        //  To be called after processing commands or invoking any command
        //  handlers explicitly. If required, it will deallocate the socket.
        void check_destroy ();

        //  Moves the flags from the message to local variables,
        //  to be later retrieved by getsockopt.
        void extract_flags (msg_t *msg_);

        //  Used to check whether the object is a socket.
        uint32_t tag;

        //  If true, associated context was already terminated.
        bool ctx_terminated;

        //  If true, object should have been already destroyed. However,
        //  destruction is delayed while we unwind the stack to the point
        //  where it doesn't intersect the object being destroyed.
        bool destroyed;

        //  Parse URI string.
        int parse_uri (const char *uri_, std::string &protocol_,
            std::string &address_);

        //  Check whether transport protocol, as specified in connect or
        //  bind, is available and compatible with the socket type.
        int check_protocol (const std::string &protocol_);

        //  Register the pipe with this socket.
        void attach_pipe (zmq::pipe_t *pipe_, bool icanhasall_ = false);

        //  Processes commands sent to this socket (if any). If timeout is -1,
        //  returns only after at least one command was processed.
        //  If throttle argument is true, commands are processed at most once
        //  in a predefined time period.
        int process_commands (int timeout_, bool throttle_);

        //  Handlers for incoming commands.
        void process_stop ();
        void process_bind (zmq::pipe_t *pipe_);
        void process_term (int linger_);

        //  Socket's mailbox object.
        mailbox_t mailbox;

        //  List of attached pipes.
        typedef array_t <pipe_t, 3> pipes_t;
        pipes_t pipes;

        //  Reaper's poller and handle of this socket within it.
        poller_t *poller;
        poller_t::handle_t handle;

        //  Timestamp of when commands were processed the last time.
        uint64_t last_tsc;

        //  Number of messages received since last command processing.
        int ticks;

        //  True if the last message received had MORE flag set.
        bool rcvmore;

        //  Improves efficiency of time measurement.
        clock_t clock;

        socket_base_t (const socket_base_t&);
        const socket_base_t &operator = (const socket_base_t&);
        mutex_t sync;
    };

}

#endif
