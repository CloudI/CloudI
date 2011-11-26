// -*- coding: utf-8; Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
// ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
//
// BSD LICENSE
// 
// Copyright (c) 2011, Michael Truog <mjtruog at gmail dot com>
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
//

#include "cloudi.h"
#include "realloc_ptr.hpp"
#include <unistd.h>
#include <stdlib.h>
#include <errno.h>
#include <poll.h>
#include <ei.h>
#include <boost/unordered_map.hpp>
#include <map>
#include <string>
#include <cstring>
#include <iostream>
#include "assert.hpp"

#define CLOUDI_ASYNC     1
#define CLOUDI_SYNC     -1

namespace
{
    typedef boost::unordered_map<std::string, cloudi_callback_t> lookup_t;
    typedef realloc_ptr<char> buffer_t;

    int errno_read()
    {
        switch (errno)
        {
            case EAGAIN:
                return cloudi_error_read_EAGAIN;
            case EBADF:
                return cloudi_error_read_EBADF;
            case EFAULT:
                return cloudi_error_read_EFAULT;
            case EINTR:
                return cloudi_error_read_EINTR;
            case EINVAL:
                return cloudi_error_read_EINVAL;
            case EIO:
                return cloudi_error_read_EIO;
            case EISDIR:
                return cloudi_error_read_EISDIR;
            default:
                return cloudi_error_read_unknown;
        }
    }

    int errno_write()
    {
        switch (errno)
        {
            case EAGAIN:
                return cloudi_error_write_EAGAIN;
            case EBADF:
                return cloudi_error_write_EBADF;
            case EFAULT:
                return cloudi_error_write_EFAULT;
            case EFBIG:
                return cloudi_error_write_EFBIG;
            case EINTR:
                return cloudi_error_write_EINTR;
            case EINVAL:
                return cloudi_error_write_EINVAL;
            case EIO:
                return cloudi_error_write_EIO;
            case ENOSPC:
                return cloudi_error_write_ENOSPC;
            case EPIPE:
                return cloudi_error_write_EPIPE;
            default:
                return cloudi_error_write_unknown;
        }
    }

    int errno_poll()
    {
        switch (errno)
        {
            case EBADF:
                return cloudi_error_poll_EBADF;
            case EFAULT:
                return cloudi_error_poll_EFAULT;
            case EINTR:
                return cloudi_error_poll_EINTR;
            case EINVAL:
                return cloudi_error_poll_EINVAL;
            case ENOMEM:
                return cloudi_error_poll_ENOMEM;
            default:
                return cloudi_error_poll_unknown;
        }
    }

    int data_ready(int fd, bool & ready)
    {
        struct pollfd fds[1] = {{fd, POLLIN | POLLPRI, 0}};
        int const count = poll(fds, 1, 0);
        if (count == -1)
            return errno_poll();
        ready = (count == 1);
        return cloudi_success;
    }

    int read_all(int fd, buffer_t & buffer, uint32_t & total,
                 uint32_t const buffer_size)
    {
        bool ready = true;
        while (ready)
        {
            if (buffer.reserve(total + buffer_size) == false)
                return cloudi_out_of_memory;
            ssize_t i = ::read(fd, &buffer[total], buffer_size);
            if (i < 0)
                return errno_read();
            total += i;
            ready = (i == static_cast<signed>(buffer_size)) ||
                    (i == 0 && total == 0);
            if (ready)
            {
                int const status = data_ready(fd, ready);
                if (status)
                    return status;
            }
        }

        return cloudi_success;
    }

    int write_exact(int fd, char const * const buffer, uint32_t const length)
    {
        uint32_t total = 0;
        while (total < length)
        {
            ssize_t const i = ::write(fd, buffer + total, length - total);
            if (i <= 0)
            {
                if (i == -1)
                    return errno_write();
                else
                    return cloudi_error_write_null;
            }
            total += i;
        }
        if (total > length)
            return cloudi_error_write_overflow;
        return cloudi_success;
    }

    class return_async_exception : public std::exception
    {
        public:
            return_async_exception() throw() {}
            virtual ~return_async_exception() throw() {}
            virtual char const * what() const throw()
            {
                return "return_async exception";
            }
    };

    class return_sync_exception : public std::exception
    {
        public:
            return_sync_exception() throw() {}
            virtual ~return_sync_exception() throw() {}
            virtual char const * what() const throw()
            {
                return "return_sync exception";
            }
    };

}

extern "C" {

static void exit_handler()
{
    ::fflush(stdout);
    ::fflush(stderr);
    std::cout.flush();
    std::cerr.flush();
    std::clog.flush();
}

int cloudi_initialize(cloudi_instance_t * p,
                      int index,
                      char const * const /*protocol*/,
                      uint32_t buffer_size)
{
    assert(index >= 0);
    p->fd = index + 3;
    p->buffer_size = buffer_size;
    p->lookup = new lookup_t();
    p->buffer_send = new buffer_t(32768, CLOUDI_MAX_BUFFERSIZE);
    p->buffer_recv = new buffer_t(32768, CLOUDI_MAX_BUFFERSIZE);
    p->buffer_recv_index = 0;
    p->prefix = 0;

    ::atexit(&exit_handler);

    // attempt initialization
    buffer_t & buffer = *reinterpret_cast<buffer_t *>(p->buffer_send);
    index = 0;
    if (ei_encode_version(buffer.get<char>(), &index))
        return cloudi_error_ei_encode;
    if (ei_encode_atom(buffer.get<char>(), &index, "init"))
        return cloudi_error_ei_encode;
    int result = write_exact(p->fd, buffer.get<char>(), index);
    if (result)
        return result;
    result = cloudi_poll(p, -1);
    if (result)
        return result;

    return cloudi_success;
}

void cloudi_destroy(cloudi_instance_t * p)
{
    if (p->fd != 0)
    {
        ::close(p->fd);
        delete reinterpret_cast<lookup_t *>(p->lookup);
        delete reinterpret_cast<buffer_t *>(p->buffer_send);
        delete reinterpret_cast<buffer_t *>(p->buffer_recv);
        if (p->prefix)
            delete p->prefix;
    }
}

int cloudi_subscribe(cloudi_instance_t * p,
                     char const * const name,
                     cloudi_callback_t f)
{
    lookup_t & lookup = *reinterpret_cast<lookup_t *>(p->lookup);
    lookup.insert(std::pair<std::string,
                            cloudi_callback_t>(std::string(p->prefix) + name,
                                               f));

    buffer_t & buffer = *reinterpret_cast<buffer_t *>(p->buffer_send);
    int index = 0;
    if (ei_encode_version(buffer.get<char>(), &index))
        return cloudi_error_ei_encode;
    if (ei_encode_tuple_header(buffer.get<char>(), &index, 2))
        return cloudi_error_ei_encode;
    if (ei_encode_atom(buffer.get<char>(), &index, "subscribe"))
        return cloudi_error_ei_encode;
    if (buffer.reserve(index + strlen(name) + 1) == false)
        return cloudi_error_write_overflow;
    if (ei_encode_string(buffer.get<char>(), &index, name))
        return cloudi_error_ei_encode;
    int result = write_exact(p->fd, buffer.get<char>(), index);
    if (result)
        return result;
    return cloudi_success;
}

int cloudi_unsubscribe(cloudi_instance_t * p,
                       char const * const name)
{
    std::string str(p->prefix);
    str += name;
    lookup_t & lookup = *reinterpret_cast<lookup_t *>(p->lookup);
    lookup_t::iterator itr = lookup.find(str);
    if (itr == lookup.end())
    {
        return cloudi_error_function_parameter;
    }
    else
    {
        lookup.erase(itr);

        buffer_t & buffer = *reinterpret_cast<buffer_t *>(p->buffer_send);
        int index = 0;
        if (ei_encode_version(buffer.get<char>(), &index))
            return cloudi_error_ei_encode;
        if (ei_encode_tuple_header(buffer.get<char>(), &index, 2))
            return cloudi_error_ei_encode;
        if (ei_encode_atom(buffer.get<char>(), &index, "unsubscribe"))
            return cloudi_error_ei_encode;
        if (buffer.reserve(index + strlen(name) + 1) == false)
            return cloudi_error_write_overflow;
        if (ei_encode_string(buffer.get<char>(), &index, name))
            return cloudi_error_ei_encode;
        int result = write_exact(p->fd, buffer.get<char>(), index);
        if (result)
            return result;
        return cloudi_success;
    }
}

static int cloudi_send_(cloudi_instance_t * p,
                        char const * const command_name,
                        char const * const name,
                        void const * const request_info,
                        uint32_t const request_info_size,
                        void const * const request,
                        uint32_t const request_size,
                        uint32_t timeout,
                        int8_t priority)
{
    buffer_t & buffer = *reinterpret_cast<buffer_t *>(p->buffer_send);
    int index = 0;
    if (ei_encode_version(buffer.get<char>(), &index))
        return cloudi_error_ei_encode;
    if (ei_encode_tuple_header(buffer.get<char>(), &index, 6))
        return cloudi_error_ei_encode;
    if (ei_encode_atom(buffer.get<char>(), &index, command_name))
        return cloudi_error_ei_encode;
    if (buffer.reserve(index + strlen(name) + 1 +
                       request_info_size + request_size) == false)
        return cloudi_error_write_overflow;
    if (ei_encode_string(buffer.get<char>(), &index, name))
        return cloudi_error_ei_encode;
    if (ei_encode_binary(buffer.get<char>(), &index,
                         request_info, request_info_size))
        return cloudi_error_ei_encode;
    if (ei_encode_binary(buffer.get<char>(), &index, request, request_size))
        return cloudi_error_ei_encode;
    if (ei_encode_ulong(buffer.get<char>(), &index, timeout))
        return cloudi_error_ei_encode;
    if (ei_encode_long(buffer.get<char>(), &index, priority))
        return cloudi_error_ei_encode;
    int result = write_exact(p->fd, buffer.get<char>(), index);
    if (result)
        return result;
    result = cloudi_poll(p, -1);
    if (result)
        return result;
    return cloudi_success;
}

int cloudi_send_async(cloudi_instance_t * p,
                      char const * const name,
                      void const * const request,
                      uint32_t const request_size)
{
    return cloudi_send_(p, "send_async", name, "", 0,
                        request, request_size, p->timeout_async, 0);
}

int cloudi_send_async_(cloudi_instance_t * p,
                       char const * const name,
                       void const * const request_info,
                       uint32_t const request_info_size,
                       void const * const request,
                       uint32_t const request_size,
                       uint32_t timeout,
                       int8_t priority)
{
    return cloudi_send_(p, "send_async", name,
                        request_info, request_info_size,
                        request, request_size, timeout, priority);
}

int cloudi_send_sync(cloudi_instance_t * p,
                     char const * const name,
                     void const * const request,
                     uint32_t const request_size)
{
    return cloudi_send_(p, "send_sync", name, "", 0,
                        request, request_size, p->timeout_sync, 0);
}

int cloudi_send_sync_(cloudi_instance_t * p,
                      char const * const name,
                      void const * const request_info,
                      uint32_t const request_info_size,
                      void const * const request,
                      uint32_t const request_size,
                      uint32_t timeout,
                      int8_t priority)
{
    return cloudi_send_(p, "send_sync", name,
                        request_info, request_info_size,
                        request, request_size, timeout, priority);
}

int cloudi_mcast_async(cloudi_instance_t * p,
                       char const * const name,
                       void const * const request,
                       uint32_t const request_size)
{
    return cloudi_send_(p, "mcast_async", name, "", 0,
                        request, request_size, p->timeout_async, 0);
}

int cloudi_mcast_async_(cloudi_instance_t * p,
                        char const * const name,
                        void const * const request_info,
                        uint32_t const request_info_size,
                        void const * const request,
                        uint32_t const request_size,
                        uint32_t timeout,
                        int8_t priority)
{
    return cloudi_send_(p, "mcast_async", name,
                        request_info, request_info_size,
                        request, request_size, timeout, priority);
}

static int cloudi_forward_(cloudi_instance_t * p,
                           char const * const command_name,
                           char const * const name,
                           void const * const request_info,
                           uint32_t const request_info_size,
                           void const * const request,
                           uint32_t const request_size,
                           uint32_t timeout,
                           int8_t priority,
                           char const * const trans_id,
                           char const * const pid,
                           uint32_t const pid_size)
{
    buffer_t & buffer = *reinterpret_cast<buffer_t *>(p->buffer_send);
    int index = 0;
    if (ei_encode_version(buffer.get<char>(), &index))
        return cloudi_error_ei_encode;
    if (ei_encode_tuple_header(buffer.get<char>(), &index, 8))
        return cloudi_error_ei_encode;
    if (ei_encode_atom(buffer.get<char>(), &index, command_name))
        return cloudi_error_ei_encode;
    if (buffer.reserve(index + strlen(name) + 1 +
                       request_info_size + request_size + pid_size) == false)
        return cloudi_error_write_overflow;
    if (ei_encode_string(buffer.get<char>(), &index, name))
        return cloudi_error_ei_encode;
    if (ei_encode_binary(buffer.get<char>(), &index,
                         request_info, request_info_size))
        return cloudi_error_ei_encode;
    if (ei_encode_binary(buffer.get<char>(), &index, request, request_size))
        return cloudi_error_ei_encode;
    if (ei_encode_ulong(buffer.get<char>(), &index, timeout))
        return cloudi_error_ei_encode;
    if (ei_encode_long(buffer.get<char>(), &index, priority))
        return cloudi_error_ei_encode;
    if (ei_encode_binary(buffer.get<char>(), &index, trans_id, 16))
        return cloudi_error_ei_encode;
    int version;
    int pid_index = 0;
    if (ei_decode_version(pid, &pid_index, &version))
        return cloudi_error_ei_decode;
    int const pid_data_size = pid_size - pid_index;
    ::memcpy(&(buffer[index]), &(pid[pid_index]), pid_data_size);
    index += pid_data_size;

    int result = write_exact(p->fd, buffer.get<char>(), index);
    if (result)
        return result;
    return cloudi_success;
}

int cloudi_forward(cloudi_instance_t * p,
                   int const command,
                   char const * const name,
                   void const * const request_info,
                   uint32_t const request_info_size,
                   void const * const request,
                   uint32_t const request_size,
                   uint32_t timeout,
                   int8_t priority,
                   char const * const trans_id,
                   char const * const pid,
                   uint32_t const pid_size)
{
    int result;
    if (command > 0)   // CLOUDI_ASYNC
    {
        result = cloudi_forward_(p, "forward_async", name,
                                 request_info, request_info_size,
                                 request, request_size,
                                 timeout, priority, trans_id, pid, pid_size);
        assert(result == cloudi_success);
        throw return_async_exception();
    }
    else               // CLOUDI_SYNC
    {
        result = cloudi_forward_(p, "forward_sync", name,
                                 request_info, request_info_size,
                                 request, request_size,
                                 timeout, priority, trans_id, pid, pid_size);
        assert(result == cloudi_success);
        throw return_sync_exception();
    }
    return result;
}

int cloudi_forward_async(cloudi_instance_t * p,
                         char const * const name,
                         void const * const request_info,
                         uint32_t const request_info_size,
                         void const * const request,
                         uint32_t const request_size,
                         uint32_t timeout,
                         int8_t priority,
                         char const * const trans_id,
                         char const * const pid,
                         uint32_t const pid_size)
{
    int const result = cloudi_forward_(p, "forward_async", name,
                                       request_info, request_info_size,
                                       request, request_size,
                                       timeout, priority,
                                       trans_id, pid, pid_size);
    assert(result == cloudi_success);
    throw return_async_exception();
    return result;
}

int cloudi_forward_sync(cloudi_instance_t * p,
                        char const * const name,
                        void const * const request_info,
                        uint32_t const request_info_size,
                        void const * const request,
                        uint32_t const request_size,
                        uint32_t timeout,
                        int8_t priority,
                        char const * const trans_id,
                        char const * const pid,
                        uint32_t const pid_size)
{
    int const result = cloudi_forward_(p, "forward_sync", name,
                                       request_info, request_info_size,
                                       request, request_size,
                                       timeout, priority,
                                       trans_id, pid, pid_size);
    assert(result == cloudi_success);
    throw return_sync_exception();
    return result;
}

static int cloudi_return_(cloudi_instance_t * p,
                          char const * const command_name,
                          char const * const name,
                          void const * const response_info,
                          uint32_t const response_info_size,
                          void const * const response,
                          uint32_t const response_size,
                          uint32_t timeout,
                          char const * const trans_id,
                          char const * const pid,
                          uint32_t const pid_size)
{
    buffer_t & buffer = *reinterpret_cast<buffer_t *>(p->buffer_send);
    int index = 0;
    if (ei_encode_version(buffer.get<char>(), &index))
        return cloudi_error_ei_encode;
    if (ei_encode_tuple_header(buffer.get<char>(), &index, 7))
        return cloudi_error_ei_encode;
    if (ei_encode_atom(buffer.get<char>(), &index, command_name))
        return cloudi_error_ei_encode;
    if (buffer.reserve(index + strlen(name) + 1 +
                       response_info_size + response_size + pid_size) == false)
        return cloudi_error_write_overflow;
    if (ei_encode_string(buffer.get<char>(), &index, name))
        return cloudi_error_ei_encode;
    if (ei_encode_binary(buffer.get<char>(), &index,
                         response_info, response_info_size))
        return cloudi_error_ei_encode;
    if (ei_encode_binary(buffer.get<char>(), &index, response, response_size))
        return cloudi_error_ei_encode;
    if (ei_encode_ulong(buffer.get<char>(), &index, timeout))
        return cloudi_error_ei_encode;
    if (ei_encode_binary(buffer.get<char>(), &index, trans_id, 16))
        return cloudi_error_ei_encode;
    int version;
    int pid_index = 0;
    if (ei_decode_version(pid, &pid_index, &version))
        return cloudi_error_ei_decode;
    int const pid_data_size = pid_size - pid_index;
    ::memcpy(&(buffer[index]), &(pid[pid_index]), pid_data_size);
    index += pid_data_size;

    int result = write_exact(p->fd, buffer.get<char>(), index);
    if (result)
        return result;
    return cloudi_success;
}

int cloudi_return(cloudi_instance_t * p,
                  int const command,
                  char const * const name,
                  void const * const response_info,
                  uint32_t const response_info_size,
                  void const * const response,
                  uint32_t const response_size,
                  uint32_t timeout,
                  char const * const trans_id,
                  char const * const pid,
                  uint32_t const pid_size)
{
    int result;
    if (command > 0)   // CLOUDI_ASYNC
    {
        result = cloudi_return_(p, "return_async", name,
                                response_info, response_info_size,
                                response, response_size,
                                timeout, trans_id, pid, pid_size);
        assert(result == cloudi_success);
        throw return_async_exception();
    }
    else               // CLOUDI_SYNC
    {
        result = cloudi_return_(p, "return_sync", name,
                                response_info, response_info_size,
                                response, response_size,
                                timeout, trans_id, pid, pid_size);
        assert(result == cloudi_success);
        throw return_sync_exception();
    }
    return result;
}

int cloudi_return_async(cloudi_instance_t * p,
                        char const * const name,
                        void const * const response_info,
                        uint32_t const response_info_size,
                        void const * const response,
                        uint32_t const response_size,
                        uint32_t timeout,
                        char const * const trans_id,
                        char const * const pid,
                        uint32_t const pid_size)
{
    int const result = cloudi_return_(p, "return_async", name,
                                      response_info, response_info_size,
                                      response, response_size,
                                      timeout, trans_id, pid, pid_size);
    assert(result == cloudi_success);
    throw return_async_exception();
    return result;
}

int cloudi_return_sync(cloudi_instance_t * p,
                       char const * const name,
                       void const * const response_info,
                       uint32_t const response_info_size,
                       void const * const response,
                       uint32_t const response_size,
                       uint32_t timeout,
                       char const * const trans_id,
                       char const * const pid,
                       uint32_t const pid_size)
{
    int const result = cloudi_return_(p, "return_sync", name,
                                      response_info, response_info_size,
                                      response, response_size,
                                      timeout, trans_id, pid, pid_size);
    assert(result == cloudi_success);
    throw return_sync_exception();
    return result;
}

int cloudi_recv_async(cloudi_instance_t * p,
                      uint32_t timeout,
                      char const * const trans_id)
{
    buffer_t & buffer = *reinterpret_cast<buffer_t *>(p->buffer_send);
    int index = 0;
    if (ei_encode_version(buffer.get<char>(), &index))
        return cloudi_error_ei_encode;
    if (ei_encode_tuple_header(buffer.get<char>(), &index, 3))
        return cloudi_error_ei_encode;
    if (ei_encode_atom(buffer.get<char>(), &index, "recv_async"))
        return cloudi_error_ei_encode;
    if (ei_encode_ulong(buffer.get<char>(), &index, timeout))
        return cloudi_error_ei_encode;
    if (ei_encode_binary(buffer.get<char>(), &index, trans_id, 16))
        return cloudi_error_ei_encode;
    int result = write_exact(p->fd, buffer.get<char>(), index);
    if (result)
        return result;
    result = cloudi_poll(p, -1);
    if (result)
        return result;
    return cloudi_success;
}

static int keepalive(cloudi_instance_t * p)
{
    buffer_t & buffer = *reinterpret_cast<buffer_t *>(p->buffer_send);
    int index = 0;
    if (ei_encode_version(buffer.get<char>(), &index))
        return cloudi_error_ei_encode;
    if (ei_encode_atom(buffer.get<char>(), &index, "keepalive"))
        return cloudi_error_ei_encode;
    int result = write_exact(p->fd, buffer.get<char>(), index);
    if (result)
        return result;
    return cloudi_success;
}

#define MESSAGE_INIT           1
#define MESSAGE_SEND_ASYNC     2
#define MESSAGE_SEND_SYNC      3
#define MESSAGE_RECV_ASYNC     4
#define MESSAGE_RETURN_ASYNC   5
#define MESSAGE_RETURN_SYNC    6
#define MESSAGE_RETURNS_ASYNC  7
#define MESSAGE_KEEPALIVE      8

static void callback(cloudi_instance_t * p,
                     int const command,
                     char const * const name,
                     void const * const request_info,
                     uint32_t const request_info_size,
                     void const * const request,
                     uint32_t const request_size,
                     uint32_t timeout,
                     int8_t priority,
                     char const * const trans_id,
                     char const * const pid,
                     uint32_t const pid_size)
{
    lookup_t & lookup = *reinterpret_cast<lookup_t *>(p->lookup);
    lookup_t::iterator itr = lookup.find(std::string(name));
    assert(itr != lookup.end());
    cloudi_callback_t f = itr->second;
    
    if (command == MESSAGE_SEND_ASYNC)
    {
        try
        {
            f(p, CLOUDI_ASYNC, name,
              request_info, request_info_size,
              request, request_size,
              timeout, priority, trans_id, pid, pid_size);
        }
        catch (return_async_exception const &)
        {
            return;
        }
        catch (return_sync_exception const &)
        {
            assert(false);
            return;
        }
        catch (...)
        {
            // exception is ignored at this level
        }
        cloudi_return_async(p, name, "", 0, "", 0, timeout,
                            trans_id, pid, pid_size);
    }
    else if (command == MESSAGE_SEND_SYNC)
    {
        try
        {
            f(p, CLOUDI_SYNC, name,
              request_info, request_info_size,
              request, request_size,
              timeout, priority, trans_id, pid, pid_size);
        }
        catch (return_async_exception const &)
        {
            assert(false);
            return;
        }
        catch (return_sync_exception const &)
        {
            return;
        }
        catch (...)
        {
            // exception is ignored at this level
        }
        cloudi_return_sync(p, name, "", 0, "", 0, timeout,
                           trans_id, pid, pid_size);
    }
    else
    {
        assert(false);
    }
}

static void store_incoming_binary(buffer_t const & buffer,
                                  uint32_t & index,
                                  char * & p)
{
    uint32_t size = *reinterpret_cast<uint32_t *>(&buffer[index]);
    index += sizeof(uint32_t);
    p = new char[size];
    memcpy(p, &buffer[index], size);
    index += size;
}

static void store_incoming_uint32(buffer_t const & buffer,
                                  uint32_t & index,
                                  uint32_t & i)
{
    i = *reinterpret_cast<uint32_t *>(&buffer[index]);
    index += sizeof(uint32_t);
}

static void store_incoming_int8(buffer_t const & buffer,
                                uint32_t & index,
                                int8_t & i)
{
    i = *reinterpret_cast<int8_t *>(&buffer[index]);
    index += sizeof(int8_t);
}

int cloudi_poll(cloudi_instance_t * p,
                int timeout)
{
    buffer_t & buffer = *reinterpret_cast<buffer_t *>(p->buffer_recv);
    struct pollfd fds[1] = {{p->fd, POLLIN | POLLPRI, 0}};
    int count = ::poll(fds, 1, timeout);
    if (count == 0)
        return cloudi_timeout;
    else if (count < 0)
        return errno_poll();

    int result = read_all(p->fd, buffer,
                          p->buffer_recv_index,
                          p->buffer_size);
    if (result)
        return result;
        
    while (true)
    {
        if (p->buffer_recv_index == 0)
            ::exit(cloudi_error_read_underflow);

        fds[0].revents = 0;
        uint32_t index = 0;
        uint32_t command;
        store_incoming_uint32(buffer, index, command);
        switch (command)
        {
            case MESSAGE_INIT:
            {
                store_incoming_binary(buffer, index, p->prefix);
                store_incoming_uint32(buffer, index, p->timeout_async);
                store_incoming_uint32(buffer, index, p->timeout_sync);
                if (index != p->buffer_recv_index)
                    ::exit(cloudi_error_read_underflow);
                p->buffer_recv_index = 0;
                return cloudi_success;
            }
            case MESSAGE_SEND_ASYNC:
            case MESSAGE_SEND_SYNC:
            {
                uint32_t name_size;
                store_incoming_uint32(buffer, index, name_size);
                char * name = &buffer[index];
                index += name_size;
                uint32_t request_info_size;
                store_incoming_uint32(buffer, index, request_info_size);
                char * request_info = &buffer[index];
                index += request_info_size + 1;
                uint32_t request_size;
                store_incoming_uint32(buffer, index, request_size);
                char * request = &buffer[index];
                index += request_size + 1;
                uint32_t timeout;
                store_incoming_uint32(buffer, index, timeout);
                int8_t priority;
                store_incoming_int8(buffer, index, priority);
                char * trans_id = &buffer[index];
                index += 16;
                uint32_t pid_size;
                store_incoming_uint32(buffer, index, pid_size);
                char * pid = &buffer[index];
                index += pid_size;
                if (index != p->buffer_recv_index)
                    return cloudi_error_read_underflow;
                p->buffer_recv_index = 0;
                callback(p, command, name,
                         request_info, request_info_size,
                         request, request_size,
                         timeout, priority, trans_id, pid, pid_size);
                break;
            }
            case MESSAGE_RECV_ASYNC:
            case MESSAGE_RETURN_SYNC:
            {
                store_incoming_uint32(buffer, index, p->response_info_size);
                p->response_info = &buffer[index];
                index += p->response_info_size + 1;
                store_incoming_uint32(buffer, index, p->response_size);
                p->response = &buffer[index];
                index += p->response_size + 1;
                p->trans_id_count = 1;
                p->trans_id = &buffer[index];
                index += 16;
                if (index != p->buffer_recv_index)
                    ::exit(cloudi_error_read_underflow);
                p->buffer_recv_index = 0;
                return cloudi_success;
            }
            case MESSAGE_RETURN_ASYNC:
            {
                p->trans_id_count = 1;
                p->trans_id = &buffer[index];
                index += 16;
                if (index != p->buffer_recv_index)
                    ::exit(cloudi_error_read_underflow);
                p->buffer_recv_index = 0;
                return cloudi_success;
            }
            case MESSAGE_RETURNS_ASYNC:
            {
                store_incoming_uint32(buffer, index, p->trans_id_count);
                p->trans_id = &buffer[index];
                index += 16 * p->trans_id_count;
                if (index != p->buffer_recv_index)
                    ::exit(cloudi_error_read_underflow);
                p->buffer_recv_index = 0;
                return cloudi_success;
            }
            case MESSAGE_KEEPALIVE:
            {
                if (index > p->buffer_recv_index)
                    ::exit(cloudi_error_read_underflow);
                result = keepalive(p);
                if (result)
                    ::exit(result);
                if (index < p->buffer_recv_index) {
                    p->buffer_recv_index -= index;
                    buffer.move(index, p->buffer_recv_index, 0);
                    count = ::poll(fds, 1, 0);
                    if (count < 0)
                        return errno_poll();
                    else if (count == 0)
                        continue;
                }
                else {
                    p->buffer_recv_index = 0;
                }
                break;
            }
            default:
            {
                ::exit(cloudi_error_read_underflow);
            }
        }

        fds[0].revents = 0;
        count = ::poll(fds, 1, timeout);
        if (count == 0)
            return cloudi_timeout;
        else if (count < 0)
            return errno_poll();

        result = read_all(p->fd, buffer,
                          p->buffer_recv_index,
                          p->buffer_size);
        if (result)
            return result;
    }
}

// CloudI helper functions

char const ** cloudi_request_http_qs_parse(void const * const request,
                                           uint32_t const request_size)
{
    char const * http_qs = reinterpret_cast<char const * const>(request);
    realloc_ptr<char const *> result(16, 8192);
    result[0] = http_qs;
    size_t i = 1;
    for (size_t request_i = 1; request_i < request_size; ++request_i)
    {
        if (http_qs[request_i] == '\0')
        {
            result[i] = &http_qs[++request_i];
            result.reserve(++i + 1);
        }
    }
    result[i] = 0;
    return result.release();
}

}

