// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
// ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab:

// GENERIC ERLANG PORT VERSION 0.7
// automatically create Erlang bindings to C++/C that requires an OS process

//////////////////////////////////////////////////////////////////////////////
// BSD LICENSE
// 
// Copyright (c) 2009-2011, Michael Truog <mjtruog at gmail dot com>
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in
//       the documentation and/or other materials provided with the
//       distribution.
//     * All advertising materials mentioning features or use of this
//       software must display the following acknowledgment:
//         This product includes software developed by Michael Truog
//     * The name of the author may not be used to endorse or promote
//       products derived from this software without specific prior
//       written permission
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
// CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
// INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
// OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
// BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
// WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
// DAMAGE.
//////////////////////////////////////////////////////////////////////////////

#ifndef PORT_HPP
#define PORT_HPP

#include <poll.h>
#include "realloc_ptr.hpp"

namespace GEPD
{
    namespace ExitStatus
    {
        int const min        =  78; // all GEPD values >=
        int const ready      =  78; // external fd is ready
        int const timeout    =  79; // timeout on all fds
        int const errors_min =  80; // errors >=
        int const error_HUP  = 110;
        int const errors_max = 128; // errors <
    }

    int consume_stream(int fd, short & revents,
                       char const * const name, unsigned long const pid,
                       realloc_ptr<unsigned char> & send_buffer,
                       realloc_ptr<unsigned char> & stream, size_t & i);

    int flush_stream(int fd, short revents,
                     char const * const name, unsigned long const pid,
                     realloc_ptr<unsigned char> & send_buffer,
                     realloc_ptr<unsigned char> & stream, size_t & i);

    extern realloc_ptr<struct pollfd> fds;
    extern nfds_t nfds;

    int default_main();
    int init();
    int wait(int & count, int const timeout,
             realloc_ptr<unsigned char> & buffer,
             realloc_ptr<unsigned char> & stream1,
             realloc_ptr<unsigned char> & stream2);
}

#endif // PORT_HPP

