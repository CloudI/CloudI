//-*-Mode:C++;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=cpp fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
//////////////////////////////////////////////////////////////////////////////
//
// GENERIC ERLANG PORT [DRIVER]
// automatically create Erlang bindings to C++/C that requires an OS process
//
// MIT License
//
// Copyright (c) 2009-2020 Michael Truog <mjtruog at protonmail dot com>
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
//////////////////////////////////////////////////////////////////////////////

#ifndef GEPD_PORT_HPP
#define GEPD_PORT_HPP

#include <poll.h>
#include "realloc_ptr.hpp"

namespace GEPD
{
    namespace ExitStatus
    {
        int const success           =   0;
        int const min               =  78; // all GEPD values >=
        int const ready             =  78; // external fd is ready
        int const timeout           =  79; // timeout on all fds
        int const errors_min        =  80; // errors >=
        int const erlang_exit       = errors_min;
        int const read_EAGAIN       = errors_min +  1;
        int const read_EBADF        = errors_min +  2;
        int const read_EFAULT       = errors_min +  3;
        int const read_EINTR        = errors_min +  4;
        int const read_EINVAL       = errors_min +  5;
        int const read_EIO          = errors_min +  6;
        int const read_EISDIR       = errors_min +  7;
        int const read_null         = errors_min +  8;
        int const read_overflow     = errors_min +  9;
        int const read_unknown      = errors_min + 10;
        int const write_EAGAIN      = errors_min + 11;
        int const write_EBADF       = errors_min + 12;
        int const write_EFAULT      = errors_min + 13;
        int const write_EFBIG       = errors_min + 14;
        int const write_EINTR       = errors_min + 15;
        int const write_EINVAL      = errors_min + 16;
        int const write_EIO         = errors_min + 17;
        int const write_ENOSPC      = errors_min + 18;
        int const write_EPIPE       = errors_min + 19;
        int const write_null        = errors_min + 20;
        int const write_overflow    = errors_min + 21;
        int const write_unknown     = errors_min + 22;
        int const ei_encode_error   = errors_min + 23;
        int const poll_EBADF        = errors_min + 24;
        int const poll_EFAULT       = errors_min + 25;
        int const poll_EINTR        = errors_min + 26;
        int const poll_EINVAL       = errors_min + 27;
        int const poll_ENOMEM       = errors_min + 28;
        int const poll_ERR          = errors_min + 29;
        int const poll_HUP          = errors_min + 30;
        int const poll_NVAL         = errors_min + 31;
        int const poll_unknown      = errors_min + 32;
        int const pipe_EFAULT       = errors_min + 33;
        int const pipe_EINVAL       = errors_min + 34;
        int const pipe_EMFILE       = errors_min + 35;
        int const pipe_ENFILE       = errors_min + 36;
        int const pipe_unknown      = errors_min + 37;
        int const dup_EBADF         = errors_min + 38;
        int const dup_EBUSY         = errors_min + 39;
        int const dup_EINTR         = errors_min + 40;
        int const dup_EINVAL        = errors_min + 41;
        int const dup_EMFILE        = errors_min + 42;
        int const dup_unknown       = errors_min + 43;
        int const close_EBADF       = errors_min + 44;
        int const close_EINTR       = errors_min + 45;
        int const close_EIO         = errors_min + 46;
        int const close_unknown     = errors_min + 47;
        int const errors_max        = errors_min + 48; // errors <
        int const error_HUP         = poll_HUP;
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
    int wait(int & count,
             realloc_ptr<unsigned char> & buffer,
             realloc_ptr<unsigned char> & stream1,
             realloc_ptr<unsigned char> & stream2);
    int wait(int & count, int const timeout,
             realloc_ptr<unsigned char> & buffer,
             realloc_ptr<unsigned char> & stream1,
             realloc_ptr<unsigned char> & stream2);
}

#endif // GEPD_PORT_HPP

