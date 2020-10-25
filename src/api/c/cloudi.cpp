//-*-Mode:C++;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=cpp fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
//
// MIT License
//
// Copyright (c) 2011-2020 Michael Truog <mjtruog at protonmail dot com>
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
//

#include "cloudi.hpp"
#undef CLOUDI_HPP // avoid header warning
#include "cloudi.h"
#define CLOUDI_HPP
#include "config.h"
#include "realloc_ptr.hpp"
#include "copy_ptr.hpp"
#include "timer.hpp"
#include <unistd.h>
#include <errno.h>
#include <poll.h>
#include <ei.h>
#include <boost/shared_ptr.hpp>
#include <boost/unordered_map.hpp>
#include <boost/exception/all.hpp>
#define BACKTRACE_FRAMES 32
#define BACKTRACE_FRAME_OFFSET 2
#if defined(BACKTRACE_USE_BACKWARD)
#include <backward.hpp>
#elif defined(BACKTRACE_USE_BOOSTER)
#include <booster/backtrace.h>
#endif
#include <string>
#include <list>
#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <iostream>
#include "assert.hpp"

extern "C" {

static std::string backtrace_string()
{
#if defined(BACKTRACE_USE_BACKWARD)
    backward::StackTrace st;
    st.load_here(BACKTRACE_FRAMES);
    std::ostringstream result;
    backward::TraceResolver  resolver;
    result << "trace (most recent call last)";
    unsigned int const thread_id = st.thread_id();
    if (thread_id)
    {
        result << " in thread " << thread_id << ":" << std::endl;
    }
    else
    {
        result << ":" << std::endl;
    }
    resolver.load_stacktrace(st);
    for (size_t i = BACKTRACE_FRAME_OFFSET; i < st.size(); ++i)
    {
        backward::ResolvedTrace const & trace = resolver.resolve(st[i]);
        bool indented = true;

        result << "#" <<
            std::setfill(' ') << std::setw(2) << std::left <<
            std::dec << (trace.idx - BACKTRACE_FRAME_OFFSET) << " ";
        if (trace.source.filename.empty())
        {
            result <<
                std::setfill(' ') << std::setw(18) << std::right <<
                std::hex << trace.addr << " in " <<
                trace.object_function << std::endl <<
                "   at " << trace.object_filename << std::endl;
            indented = false;
        }
        for (size_t j = 0; j < trace.inliners.size(); ++j)
        {
            if (not indented)
                result << "    ";
            backward::ResolvedTrace::SourceLoc const & location =
                trace.inliners[j];
            result <<
                "     (inlined)     "
                "in " << location.function << std::endl <<
                "   at " << location.filename << ":" <<
                std::dec << location.line << std::endl;
            indented = false;
        }
        if (not trace.source.filename.empty())
        {
            if (not indented)
                result << "    ";
            result <<
                std::setfill(' ') << std::setw(18) << std::right <<
                std::hex << trace.addr << " in " <<
                trace.source.function << std::endl <<
                "   at " << trace.source.filename << ":" <<
                std::dec << trace.source.line << std::endl;
        }
    }
    return result.str();
#elif defined(BACKTRACE_USE_BOOSTER)
    booster::backtrace b(BACKTRACE_FRAMES);
    std::ostringstream result;
    result << "trace (most recent call last):" << std::endl;
    for (unsigned int i = BACKTRACE_FRAME_OFFSET; i < b.stack_size(); ++i)
    {
        result << "#" <<
            std::setfill(' ') << std::setw(2) << std::left <<
            std::dec << (i - BACKTRACE_FRAME_OFFSET) << " ";
        b.trace_line(i, result);
    }
    return result.str();
#else
    return std::string("");
#endif
}

}

namespace
{
    class callback_function
    {
        private:
            class callback_function_c :
                public CloudI::API::callback_function_generic
            {
                public:
                    callback_function_c(cloudi_instance_t * api,
                                        cloudi_callback_t f) :
                        m_api(api), m_f(f) {}
                    virtual ~callback_function_c() throw() {}

                    virtual void operator () (int const request_type,
                                              char const * const name,
                                              char const * const pattern,
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
                        m_f(request_type,
                            name,
                            pattern,
                            request_info,
                            request_info_size,
                            request,
                            request_size,
                            timeout,
                            priority,
                            trans_id,
                            pid,
                            pid_size,
                            m_api->state,
                            m_api);
                    }
                private:
                    cloudi_instance_t * m_api;
                    cloudi_callback_t m_f;
            };
            class callback_null_response :
                public CloudI::API::callback_function_generic
            {
                public:
                    callback_null_response() {}
                    virtual ~callback_null_response() throw() {}

                    virtual void operator () (int const,
                                              char const * const,
                                              char const * const,
                                              void const * const,
                                              uint32_t const,
                                              void const * const,
                                              uint32_t const,
                                              uint32_t,
                                              int8_t,
                                              char const * const,
                                              char const * const,
                                              uint32_t const)
                    {
                    }
            };

        public:
            callback_function(CloudI::API::callback_function_generic * p) :
                m_function(p) {}

            callback_function(cloudi_instance_t * api,
                              cloudi_callback_t f) :
                m_function(new callback_function_c(api, f)) {}

            static callback_function null_response()
            {
                callback_function null_response(new callback_null_response());
                return null_response;
            }

            void operator () (int const request_type,
                              char const * const name,
                              char const * const pattern,
                              void const * const request_info,
                              uint32_t const request_info_size,
                              void const * const request,
                              uint32_t const request_size,
                              uint32_t timeout,
                              int8_t const priority,
                              char const * const trans_id,
                              char const * const pid,
                              uint32_t const pid_size) const
            {
                (*m_function)(request_type,
                              name,
                              pattern,
                              request_info,
                              request_info_size,
                              request,
                              request_size,
                              timeout,
                              priority,
                              trans_id,
                              pid,
                              pid_size);
            }

        private:
            boost::shared_ptr<CloudI::API::callback_function_generic>
                m_function;
    };

    class callback_function_lookup
    {
        private:
            class callback_function_queue
            {
                private:
                    typedef std::list<callback_function> queue_t;
                public:
                    callback_function_queue(callback_function const & f) :
                        m_queue(new queue_t()),
                        m_size(1)
                    {
                        m_queue->push_back(f);
                    }

                    void push_back(callback_function const & f)
                    {
                        m_queue->push_back(f);
                        m_size++;
                    }

                    void pop_front()
                    {
                        m_queue->pop_front();
                        assert(m_size > 0);
                        m_size--;
                    }

                    bool empty() const
                    {
                        return (m_size == 0);
                    }

                    callback_function const & cycle()
                    {
                        queue_t & queue = *m_queue;
                        if (m_size == 1)
                            return queue.front();
                        queue.push_back(queue.front());
                        queue.pop_front();
                        return queue.back();
                    }
                private:
                    boost::shared_ptr<queue_t> m_queue;
                    size_t m_size;
            };

            typedef boost::unordered_map<std::string, callback_function_queue>
                lookup_queue_t;
            typedef std::pair<std::string, callback_function_queue>
                lookup_queue_pair_t;
        public:
            void insert(std::string const & pattern,
                        callback_function const & f)
            {
                lookup_queue_t::iterator itr = m_lookup.find(pattern);
                if (itr == m_lookup.end())
                {
                    m_lookup.insert(lookup_queue_pair_t(pattern, f));
                }
                else
                {
                    itr->second.push_back(f);
                }
            }

            bool remove(std::string const & pattern)
            {
                lookup_queue_t::iterator itr = m_lookup.find(pattern);
                if (itr == m_lookup.end())
                    return false;
                itr->second.pop_front();
                if (itr->second.empty())
                    m_lookup.erase(itr);
                return true;
            }

            callback_function find(std::string const & pattern)
            {
                lookup_queue_t::iterator itr = m_lookup.find(pattern);
                if (itr == m_lookup.end())
                    return m_null_response;
                return itr->second.cycle();
            }

        private:
            static callback_function const m_null_response;
            lookup_queue_t m_lookup;

    };
    callback_function const callback_function_lookup::m_null_response =
        callback_function::null_response();
    typedef callback_function_lookup lookup_t;
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

    int read_exact(int fd,
                   unsigned char * const buffer,
                   uint32_t const length)
    {
        uint32_t total = 0;
        while (total < length)
        {
            ssize_t const i = ::read(fd, buffer + total, length - total);
            if (i <= 0)
            {
                if (i == -1)
                    return errno_read();
                else
                    return cloudi_error_read_null;
            }
            total += i;
        }
        if (total > length)
            return cloudi_error_read_overflow;
        return cloudi_success;
    }

    int read_all(int fd, int const use_header,
                 buffer_t & buffer, uint32_t & total,
                 uint32_t const buffer_size)
    {
        total = 0;
        if (use_header)
        {
            unsigned char header[4];
            int const status = read_exact(fd, header, 4);
            if (status)
                return status;
            uint32_t const length = (header[0] << 24) |
                                    (header[1] << 16) |
                                    (header[2] <<  8) |
                                     header[3];
            if (buffer.reserve(length) == false)
                return cloudi_out_of_memory;
            total = length;
            return read_exact(fd, buffer.get<unsigned char>(), length);
        }
        else
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
        }

        return cloudi_success;
    }

    int write_exact(int fd, int const use_header,
                    char * const buffer, uint32_t const length)
    {
        if (use_header)
        {
            uint32_t const length_body = length - 4;
            buffer[0] = (length_body & 0xff000000) >> 24;
            buffer[1] = (length_body & 0x00ff0000) >> 16;
            buffer[2] = (length_body & 0x0000ff00) >> 8;
            buffer[3] =  length_body & 0x000000ff;
        }

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

} // anonymous namespace

extern "C" {

static void exit_handler()
{
    ::fflush(stdout);
    ::fflush(stderr);
    std::cout.flush();
    std::cerr.flush();
    std::clog.flush();
}

static void exception_unknown()
{
    std::cerr << backtrace_string();
    ::abort();
}

static int poll_request(cloudi_instance_t * api,
                        int timeout,
                        int external);

int cloudi_initialize(cloudi_instance_t * api,
                      unsigned int const thread_index,
                      void * state)
{
    if (api == 0)
        return cloudi_out_of_memory;
    char const * const protocol = ::getenv("CLOUDI_API_INIT_PROTOCOL");
    if (protocol == 0)
    {
        std::cerr <<
            "CloudI service execution must occur in CloudI" << std::endl;
        return cloudi_invalid_input;
    }
    char const * const buffer_size_p = ::getenv("CLOUDI_API_INIT_BUFFER_SIZE");
    if (buffer_size_p == 0)
        return cloudi_invalid_input;
    ::memset(api, 0, sizeof(cloudi_instance_t));
    api->state = state;
    uint32_t const buffer_size = ::atoi(buffer_size_p);
    if (::strcmp(protocol, "tcp") == 0)
    {
        api->fd = thread_index + 3;
        api->use_header = 1;
    }
    else if (::strcmp(protocol, "udp") == 0)
    {
        api->fd = thread_index + 3;
        //api->use_header = 0;
    }
    else if (::strcmp(protocol, "local") == 0)
    {
        api->fd = thread_index + 3;
        api->use_header = 1;
    }
    else
    {
        return cloudi_invalid_input;
    }
    //api->initialization_complete = 0;
    //api->terminate = 0;
    //api->cxx_terminate_exception = 0;
    //api->free_with_delete = 0;
    //api->free_name = 0;
    //api->free_pattern = 0;
    //api->free_request_info = 0;
    //api->free_request = 0;
    //api->free_response_info = 0;
    //api->free_response = 0;

    api->buffer_size = buffer_size;
    api->lookup = new lookup_t();
    api->buffer_send = new buffer_t(32768, CLOUDI_MAX_BUFFERSIZE);
    api->buffer_recv = new buffer_t(32768, CLOUDI_MAX_BUFFERSIZE);
    //api->buffer_recv_index = 0;
    api->buffer_call = new buffer_t(32768, CLOUDI_MAX_BUFFERSIZE);
    api->poll_timer = new timer();
    //api->prefix = 0;
    api->timeout_terminate = 10; // TIMEOUT_TERMINATE_MIN

    // termination handling
    ::atexit(&exit_handler);
    std::set_terminate(exception_unknown);

    // unbuffered stdout (stderr is always unbuffered)
    ::setbuf(stdout, NULL);
    std::cout.setf(std::ios::unitbuf);

    // attempt initialization
    buffer_t & buffer = *reinterpret_cast<buffer_t *>(api->buffer_send);
    int index = 0;
    if (api->use_header)
        index = 4;
    if (ei_encode_version(buffer.get<char>(), &index))
        return cloudi_error_ei_encode;
    if (ei_encode_atom(buffer.get<char>(), &index, "init"))
        return cloudi_error_ei_encode;
    int result = write_exact(api->fd, api->use_header,
                             buffer.get<char>(), index);
    if (result)
        return result;

    while (cloudi_timeout == (result = poll_request(api, 1000, 0)))
    {
    }

    return result;
}

void * cloudi_destroy(cloudi_instance_t * api)
{
    if (api && api->fd)
    {
        ::close(api->fd);
        delete reinterpret_cast<lookup_t *>(api->lookup);
        delete reinterpret_cast<buffer_t *>(api->buffer_send);
        delete reinterpret_cast<buffer_t *>(api->buffer_recv);
        delete reinterpret_cast<buffer_t *>(api->buffer_call);
        delete reinterpret_cast<timer *>(api->poll_timer);
        if (api->prefix)
            delete [] api->prefix;
        return api->state;
    }
    return 0;
}

int cloudi_initialize_thread_count(unsigned int * const thread_count)
{
    char const * const value_str = ::getenv("CLOUDI_API_INIT_THREAD_COUNT");
    if (value_str == 0)
        return cloudi_invalid_input;
    int const value = ::atoi(value_str);
    if (value < 0)
        return cloudi_invalid_input;
    *thread_count = static_cast<unsigned int>(value);
    return cloudi_success;
}

static int cloudi_subscribe_(cloudi_instance_t * api,
                             char const * const pattern,
                             callback_function const & f)
{
    lookup_t & lookup = *reinterpret_cast<lookup_t *>(api->lookup);
    lookup.insert(std::string(api->prefix) + pattern, f);

    buffer_t & buffer = *reinterpret_cast<buffer_t *>(api->buffer_send);
    int index = 0;
    if (api->use_header)
        index = 4;
    if (ei_encode_version(buffer.get<char>(), &index))
        return cloudi_error_ei_encode;
    if (ei_encode_tuple_header(buffer.get<char>(), &index, 2))
        return cloudi_error_ei_encode;
    if (ei_encode_atom(buffer.get<char>(), &index, "subscribe"))
        return cloudi_error_ei_encode;
    if (buffer.reserve(index + strlen(pattern) + 128) == false)
        return cloudi_error_write_overflow;
    if (ei_encode_string(buffer.get<char>(), &index, pattern))
        return cloudi_error_ei_encode;
    int result = write_exact(api->fd, api->use_header,
                             buffer.get<char>(), index);
    if (result)
        return result;
    return cloudi_success;
}

int cloudi_subscribe(cloudi_instance_t * api,
                     char const * const pattern,
                     cloudi_callback_t f)
{
    return cloudi_subscribe_(api,
                             pattern,
                             callback_function(api, f));
}

int cloudi_subscribe_count(cloudi_instance_t * api,
                           char const * const pattern)
{
    buffer_t & buffer = *reinterpret_cast<buffer_t *>(api->buffer_send);
    int index = 0;
    if (api->use_header)
        index = 4;
    if (ei_encode_version(buffer.get<char>(), &index))
        return cloudi_error_ei_encode;
    if (ei_encode_tuple_header(buffer.get<char>(), &index, 2))
        return cloudi_error_ei_encode;
    if (ei_encode_atom(buffer.get<char>(), &index, "subscribe_count"))
        return cloudi_error_ei_encode;
    if (buffer.reserve(index + strlen(pattern) + 128) == false)
        return cloudi_error_write_overflow;
    if (ei_encode_string(buffer.get<char>(), &index, pattern))
        return cloudi_error_ei_encode;
    int result = write_exact(api->fd, api->use_header,
                             buffer.get<char>(), index);
    if (result)
        return result;
    result = poll_request(api, -1, 0);
    if (result)
        return result;
    return cloudi_success;
}

int cloudi_unsubscribe(cloudi_instance_t * api,
                       char const * const pattern)
{
    std::string str(api->prefix);
    str += pattern;
    lookup_t & lookup = *reinterpret_cast<lookup_t *>(api->lookup);
    if (lookup.remove(str))
    {
        buffer_t & buffer = *reinterpret_cast<buffer_t *>(api->buffer_send);
        int index = 0;
        if (api->use_header)
            index = 4;
        if (ei_encode_version(buffer.get<char>(), &index))
            return cloudi_error_ei_encode;
        if (ei_encode_tuple_header(buffer.get<char>(), &index, 2))
            return cloudi_error_ei_encode;
        if (ei_encode_atom(buffer.get<char>(), &index, "unsubscribe"))
            return cloudi_error_ei_encode;
        if (buffer.reserve(index + strlen(pattern) + 128) == false)
            return cloudi_error_write_overflow;
        if (ei_encode_string(buffer.get<char>(), &index, pattern))
            return cloudi_error_ei_encode;
        int result = write_exact(api->fd, api->use_header,
                                 buffer.get<char>(), index);
        if (result)
            return result;
        return cloudi_success;
    }
    else
    {
        return cloudi_error_function_parameter;
    }
}

static int cloudi_send_(cloudi_instance_t * api,
                        char const * const command_name,
                        char const * const name,
                        void const * const request_info,
                        uint32_t const request_info_size,
                        void const * const request,
                        uint32_t const request_size,
                        uint32_t timeout,
                        int8_t const priority)
{
    buffer_t & buffer = *reinterpret_cast<buffer_t *>(api->buffer_send);
    int index = 0;
    if (api->use_header)
        index = 4;
    if (ei_encode_version(buffer.get<char>(), &index))
        return cloudi_error_ei_encode;
    if (ei_encode_tuple_header(buffer.get<char>(), &index, 6))
        return cloudi_error_ei_encode;
    if (ei_encode_atom(buffer.get<char>(), &index, command_name))
        return cloudi_error_ei_encode;
    if (buffer.reserve(index + strlen(name) +
                       request_info_size + request_size + 128) == false)
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
    int result = write_exact(api->fd, api->use_header,
                             buffer.get<char>(), index);
    if (result)
        return result;
    result = poll_request(api, -1, 0);
    if (result)
        return result;
    return cloudi_success;
}

int cloudi_send_async(cloudi_instance_t * api,
                      char const * const name,
                      void const * const request,
                      uint32_t const request_size)
{
    return cloudi_send_(api, "send_async", name, "", 0,
                        request, request_size,
                        api->timeout_async, api->priority_default);
}

int cloudi_send_async_(cloudi_instance_t * api,
                       char const * const name,
                       void const * const request_info,
                       uint32_t const request_info_size,
                       void const * const request,
                       uint32_t const request_size,
                       uint32_t timeout,
                       int8_t const priority)
{
    if (timeout == 0)
        timeout = api->timeout_async;
    return cloudi_send_(api, "send_async", name,
                        request_info, request_info_size,
                        request, request_size, timeout, priority);
}

int cloudi_send_sync(cloudi_instance_t * api,
                     char const * const name,
                     void const * const request,
                     uint32_t const request_size)
{
    return cloudi_send_(api, "send_sync", name, "", 0,
                        request, request_size,
                        api->timeout_sync, api->priority_default);
}

int cloudi_send_sync_(cloudi_instance_t * api,
                      char const * const name,
                      void const * const request_info,
                      uint32_t const request_info_size,
                      void const * const request,
                      uint32_t const request_size,
                      uint32_t timeout,
                      int8_t const priority)
{
    if (timeout == 0)
        timeout = api->timeout_sync;
    return cloudi_send_(api, "send_sync", name,
                        request_info, request_info_size,
                        request, request_size, timeout, priority);
}

int cloudi_mcast_async(cloudi_instance_t * api,
                       char const * const name,
                       void const * const request,
                       uint32_t const request_size)
{
    return cloudi_send_(api, "mcast_async", name, "", 0,
                        request, request_size,
                        api->timeout_async, api->priority_default);
}

int cloudi_mcast_async_(cloudi_instance_t * api,
                        char const * const name,
                        void const * const request_info,
                        uint32_t const request_info_size,
                        void const * const request,
                        uint32_t const request_size,
                        uint32_t timeout,
                        int8_t const priority)
{
    if (timeout == 0)
        timeout = api->timeout_async;
    return cloudi_send_(api, "mcast_async", name,
                        request_info, request_info_size,
                        request, request_size, timeout, priority);
}

static void cloudi_forward_free(cloudi_instance_t * api,
                                char const * const name,
                                void const * const request_info,
                                void const * const request)
{
    if (api->free_name)
    {
        char * const name_p = const_cast<char *>(name);
        if (api->free_with_delete)
            delete [] name_p;
        else
            ::free(name_p);
        api->free_name = 0;
    }
    assert(api->free_pattern == 0);
    if (api->free_request_info)
    {
        char * const request_info_p =
            const_cast<char *>(reinterpret_cast<char const *>(request_info));
        if (api->free_with_delete)
            delete [] request_info_p;
        else
            ::free(request_info_p);
        api->free_request_info = 0;
    }
    if (api->free_request)
    {
        char * const request_p =
            const_cast<char *>(reinterpret_cast<char const *>(request));
        if (api->free_with_delete)
            delete [] request_p;
        else
            ::free(request_p);
        api->free_request = 0;
    }
    assert(api->free_response_info == 0);
    assert(api->free_response == 0);
}

static int cloudi_forward_(cloudi_instance_t * api,
                           char const * const command_name,
                           char const * const name,
                           void const * const request_info,
                           uint32_t const request_info_size,
                           void const * const request,
                           uint32_t const request_size,
                           uint32_t timeout,
                           int8_t const priority,
                           char const * const trans_id,
                           char const * const pid,
                           uint32_t const pid_size)
{
    buffer_t & buffer = *reinterpret_cast<buffer_t *>(api->buffer_send);
    int index = 0;
    if (api->use_header)
        index = 4;
    if (ei_encode_version(buffer.get<char>(), &index))
        return cloudi_error_ei_encode;
    if (ei_encode_tuple_header(buffer.get<char>(), &index, 8))
        return cloudi_error_ei_encode;
    if (ei_encode_atom(buffer.get<char>(), &index, command_name))
        return cloudi_error_ei_encode;
    if (buffer.reserve(index + strlen(name) +
                       request_info_size + request_size + 128) == false)
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
    int const result = write_exact(api->fd, api->use_header,
                                   buffer.get<char>(), index);
    cloudi_forward_free(api, name, request_info, request);
    return result;
}

int cloudi_forward(cloudi_instance_t * api,
                   int const request_type,
                   char const * const name,
                   void const * const request_info,
                   uint32_t const request_info_size,
                   void const * const request,
                   uint32_t const request_size,
                   uint32_t timeout,
                   int8_t const priority,
                   char const * const trans_id,
                   char const * const pid,
                   uint32_t const pid_size)
{
    int result;
    if (request_type == CLOUDI_ASYNC)
    {
        result = cloudi_forward_(api,
                                 "forward_async", name,
                                 request_info, request_info_size,
                                 request, request_size,
                                 timeout, priority,
                                 trans_id, pid, pid_size);
        if (result == cloudi_success)
        {
            throw CloudI::API::forward_async_exception();
        }
    }
    else if (request_type == CLOUDI_SYNC)
    {
        result = cloudi_forward_(api,
                                 "forward_sync", name,
                                 request_info, request_info_size,
                                 request, request_size,
                                 timeout, priority,
                                 trans_id, pid, pid_size);
        if (result == cloudi_success)
        {
            throw CloudI::API::forward_sync_exception();
        }
    }
    else
    {
        result = cloudi_error_function_parameter;
    }
    return result;
}

int cloudi_forward_async(cloudi_instance_t * api,
                         char const * const name,
                         void const * const request_info,
                         uint32_t const request_info_size,
                         void const * const request,
                         uint32_t const request_size,
                         uint32_t timeout,
                         int8_t const priority,
                         char const * const trans_id,
                         char const * const pid,
                         uint32_t const pid_size)
{
    int result = cloudi_forward_(api,
                                 "forward_async", name,
                                 request_info, request_info_size,
                                 request, request_size,
                                 timeout, priority,
                                 trans_id, pid, pid_size);
    if (result == cloudi_success)
    {
        throw CloudI::API::forward_async_exception();
    }
    return result;
}

int cloudi_forward_sync(cloudi_instance_t * api,
                        char const * const name,
                        void const * const request_info,
                        uint32_t const request_info_size,
                        void const * const request,
                        uint32_t const request_size,
                        uint32_t timeout,
                        int8_t const priority,
                        char const * const trans_id,
                        char const * const pid,
                        uint32_t const pid_size)
{
    int result = cloudi_forward_(api,
                                 "forward_sync", name,
                                 request_info, request_info_size,
                                 request, request_size,
                                 timeout, priority,
                                 trans_id, pid, pid_size);
    if (result == cloudi_success)
    {
        throw CloudI::API::forward_sync_exception();
    }
    return result;
}

static void cloudi_return_free(cloudi_instance_t * api,
                               char const * const name,
                               char const * const pattern,
                               void const * const response_info,
                               void const * const response)
{
    if (api->free_name)
    {
        char * const name_p = const_cast<char *>(name);
        if (api->free_with_delete)
            delete [] name_p;
        else
            ::free(name_p);
        api->free_name = 0;
    }
    if (api->free_pattern)
    {
        char * const pattern_p = const_cast<char *>(pattern);
        if (api->free_with_delete)
            delete [] pattern_p;
        else
            ::free(pattern_p);
        api->free_pattern = 0;
    }
    assert(api->free_request_info == 0);
    assert(api->free_request == 0);
    if (api->free_response_info)
    {
        char * const response_info_p =
            const_cast<char *>(reinterpret_cast<char const *>(response_info));
        if (api->free_with_delete)
            delete [] response_info_p;
        else
            ::free(response_info_p);
        api->free_response_info = 0;
    }
    if (api->free_response)
    {
        char * const response_p =
            const_cast<char *>(reinterpret_cast<char const *>(response));
        if (api->free_with_delete)
            delete [] response_p;
        else
            ::free(response_p);
        api->free_response = 0;
    }
}

static int cloudi_return_(cloudi_instance_t * api,
                          char const * const command_name,
                          char const * const name,
                          char const * const pattern,
                          void const * const response_info,
                          uint32_t response_info_size,
                          void const * const response,
                          uint32_t response_size,
                          uint32_t timeout,
                          char const * const trans_id,
                          char const * const pid,
                          uint32_t const pid_size)
{
    buffer_t & buffer = *reinterpret_cast<buffer_t *>(api->buffer_send);
    int index = 0;
    if (api->use_header)
        index = 4;
    if (ei_encode_version(buffer.get<char>(), &index))
        return cloudi_error_ei_encode;
    if (ei_encode_tuple_header(buffer.get<char>(), &index, 8))
        return cloudi_error_ei_encode;
    if (ei_encode_atom(buffer.get<char>(), &index, command_name))
        return cloudi_error_ei_encode;
    if (buffer.reserve(index + strlen(name) + strlen(pattern) +
                       response_info_size + response_size + 128) == false)
        return cloudi_error_write_overflow;
    if (ei_encode_string(buffer.get<char>(), &index, name))
        return cloudi_error_ei_encode;
    if (ei_encode_string(buffer.get<char>(), &index, pattern))
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
    int const result = write_exact(api->fd, api->use_header,
                                   buffer.get<char>(), index);
    cloudi_return_free(api, name, pattern, response_info, response);
    return result;
}

int cloudi_return(cloudi_instance_t * api,
                  int const request_type,
                  char const * const name,
                  char const * const pattern,
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
    if (request_type == CLOUDI_ASYNC)
    {
        result = cloudi_return_(api,
                                "return_async", name, pattern,
                                response_info, response_info_size,
                                response, response_size,
                                timeout, trans_id, pid, pid_size);
        if (result == cloudi_success)
        {
            throw CloudI::API::return_async_exception();
        }
    }
    else if (request_type == CLOUDI_SYNC)
    {
        result = cloudi_return_(api,
                                "return_sync", name, pattern,
                                response_info, response_info_size,
                                response, response_size,
                                timeout, trans_id, pid, pid_size);
        if (result == cloudi_success)
        {
            throw CloudI::API::return_sync_exception();
        }
    }
    else
    {
        result = cloudi_error_function_parameter;
    }
    return result;
}

int cloudi_return_async(cloudi_instance_t * api,
                        char const * const name,
                        char const * const pattern,
                        void const * const response_info,
                        uint32_t const response_info_size,
                        void const * const response,
                        uint32_t const response_size,
                        uint32_t timeout,
                        char const * const trans_id,
                        char const * const pid,
                        uint32_t const pid_size)
{
    int result = cloudi_return_(api,
                                "return_async", name, pattern,
                                response_info, response_info_size,
                                response, response_size,
                                timeout, trans_id, pid, pid_size);
    if (result == cloudi_success)
    {
        throw CloudI::API::return_async_exception();
    }
    return result;
}

int cloudi_return_sync(cloudi_instance_t * api,
                       char const * const name,
                       char const * const pattern,
                       void const * const response_info,
                       uint32_t const response_info_size,
                       void const * const response,
                       uint32_t const response_size,
                       uint32_t timeout,
                       char const * const trans_id,
                       char const * const pid,
                       uint32_t const pid_size)
{
    int result = cloudi_return_(api,
                                "return_sync",
                                name, pattern,
                                response_info, response_info_size,
                                response, response_size,
                                timeout, trans_id, pid, pid_size);
    if (result == cloudi_success)
    {
        throw CloudI::API::return_sync_exception();
    }
    return result;
}

int cloudi_recv_async(cloudi_instance_t * api,
                      uint32_t timeout,
                      char const * const trans_id,
                      int consume)
{
    char const trans_id_null[16] = {0, 0, 0, 0, 0, 0, 0, 0,
                                    0, 0, 0, 0, 0, 0, 0, 0};
    buffer_t & buffer = *reinterpret_cast<buffer_t *>(api->buffer_send);
    int index = 0;
    if (api->use_header)
        index = 4;

    if (ei_encode_version(buffer.get<char>(), &index))
        return cloudi_error_ei_encode;
    if (ei_encode_tuple_header(buffer.get<char>(), &index, 4))
        return cloudi_error_ei_encode;
    if (ei_encode_atom(buffer.get<char>(), &index, "recv_async"))
        return cloudi_error_ei_encode;
    if (timeout == 0)
        timeout = api->timeout_sync;
    if (ei_encode_ulong(buffer.get<char>(), &index, timeout))
        return cloudi_error_ei_encode;
    if (trans_id == 0)
    {
        if (ei_encode_binary(buffer.get<char>(), &index, trans_id_null, 16))
            return cloudi_error_ei_encode;
    }
    else
    {
        if (ei_encode_binary(buffer.get<char>(), &index, trans_id, 16))
            return cloudi_error_ei_encode;
    }
    if (consume)
    {
        if (ei_encode_atom(buffer.get<char>(), &index, "true"))
            return cloudi_error_ei_encode;
    }
    else
    {
        if (ei_encode_atom(buffer.get<char>(), &index, "false"))
            return cloudi_error_ei_encode;
    }
    int result = write_exact(api->fd, api->use_header,
                             buffer.get<char>(), index);
    if (result)
        return result;
    result = poll_request(api, -1, 0);
    if (result)
        return result;
    return cloudi_success;
}

static int polling(cloudi_instance_t * api)
{
    assert(! api->initialization_complete);
    buffer_t & buffer = *reinterpret_cast<buffer_t *>(api->buffer_send);
    int index = 0;
    if (api->use_header)
        index = 4;
    if (ei_encode_version(buffer.get<char>(), &index))
        return cloudi_error_ei_encode;
    if (ei_encode_atom(buffer.get<char>(), &index, "polling"))
        return cloudi_error_ei_encode;
    int result = write_exact(api->fd, api->use_header,
                             buffer.get<char>(), index);
    if (result)
        return result;
    return cloudi_success;
}

static int keepalive(cloudi_instance_t * api)
{
    buffer_t & buffer = *reinterpret_cast<buffer_t *>(api->buffer_send);
    int index = 0;
    if (api->use_header)
        index = 4;
    if (ei_encode_version(buffer.get<char>(), &index))
        return cloudi_error_ei_encode;
    if (ei_encode_atom(buffer.get<char>(), &index, "keepalive"))
        return cloudi_error_ei_encode;
    int result = write_exact(api->fd, api->use_header,
                             buffer.get<char>(), index);
    if (result)
        return result;
    return cloudi_success;
}

#define MESSAGE_INIT                1
#define MESSAGE_SEND_ASYNC          2
#define MESSAGE_SEND_SYNC           3
#define MESSAGE_RECV_ASYNC          4
#define MESSAGE_RETURN_ASYNC        5
#define MESSAGE_RETURN_SYNC         6
#define MESSAGE_RETURNS_ASYNC       7
#define MESSAGE_KEEPALIVE           8
#define MESSAGE_REINIT              9
#define MESSAGE_SUBSCRIBE_COUNT    10
#define MESSAGE_TERM               11

static void callback(cloudi_instance_t * api,
                     int const command,
                     char const * const name,
                     char const * const pattern,
                     void const * const request_info,
                     uint32_t const request_info_size,
                     void const * const request,
                     uint32_t const request_size,
                     uint32_t timeout,
                     int8_t const priority,
                     char const * const trans_id,
                     char const * const pid,
                     uint32_t const pid_size)
{
    lookup_t & lookup = *reinterpret_cast<lookup_t *>(api->lookup);
    callback_function f = lookup.find(std::string(pattern));
    int result = cloudi_success;

    if (command == MESSAGE_SEND_ASYNC)
    {
        try
        {
            f(CLOUDI_ASYNC, name, pattern,
              request_info, request_info_size,
              request, request_size,
              timeout, priority, trans_id, pid, pid_size);
        }
        catch (CloudI::API::terminate_exception const &)
        {
        }
        catch (CloudI::API::return_async_exception const &)
        {
            return;
        }
        catch (CloudI::API::return_sync_exception const & e)
        {
            api->terminate = 1;
            std::cerr << boost::diagnostic_information(e);
            return;
        }
        catch (CloudI::API::forward_async_exception const &)
        {
            return;
        }
        catch (CloudI::API::forward_sync_exception const & e)
        {
            api->terminate = 1;
            std::cerr << boost::diagnostic_information(e);
            return;
        }
        catch (CloudI::API::fatal_error const & e)
        {
            std::cerr << boost::diagnostic_information(e);
            ::exit(1);
        }
        catch (boost::exception const & e)
        {
            std::cerr << boost::diagnostic_information(e);
        }
        catch (std::exception const & e)
        {
            std::cerr << boost::diagnostic_information(e);
        }
        try
        {
            result = cloudi_return(api,
                                   CLOUDI_ASYNC, name, pattern, "", 0, "", 0,
                                   timeout, trans_id, pid, pid_size);
            assert(result == cloudi_success);
        }
        catch (CloudI::API::return_async_exception const & e)
        {
            return;
        }
        assert(false);
    }
    else if (command == MESSAGE_SEND_SYNC)
    {
        try
        {
            f(CLOUDI_SYNC, name, pattern,
              request_info, request_info_size,
              request, request_size,
              timeout, priority, trans_id, pid, pid_size);
        }
        catch (CloudI::API::terminate_exception const &)
        {
        }
        catch (CloudI::API::return_sync_exception const &)
        {
            return;
        }
        catch (CloudI::API::return_async_exception const & e)
        {
            api->terminate = 1;
            std::cerr << boost::diagnostic_information(e);
            return;
        }
        catch (CloudI::API::forward_sync_exception const &)
        {
            return;
        }
        catch (CloudI::API::forward_async_exception const & e)
        {
            api->terminate = 1;
            std::cerr << boost::diagnostic_information(e);
            return;
        }
        catch (CloudI::API::fatal_error const & e)
        {
            std::cerr << boost::diagnostic_information(e);
            ::exit(1);
        }
        catch (boost::exception const & e)
        {
            std::cerr << boost::diagnostic_information(e);
        }
        catch (std::exception const & e)
        {
            std::cerr << boost::diagnostic_information(e);
        }
        try
        {
            result = cloudi_return(api,
                                   CLOUDI_SYNC, name, pattern, "", 0, "", 0,
                                   timeout, trans_id, pid, pid_size);
            assert(result == cloudi_success);
        }
        catch (CloudI::API::return_sync_exception const & e)
        {
            return;
        }
        assert(false);
    }
    else
    {
        assert(false);
    }
    // not executed, avoids warning
    if (result == cloudi_success)
    {
        return;
    }
}

static void store_incoming_binary(buffer_t const & buffer,
                                  uint32_t & index,
                                  char * & p)
{
    uint32_t size = *reinterpret_cast<uint32_t *>(&buffer[index]);
    index += sizeof(uint32_t);
    p = new char[size];
    ::memcpy(p, &buffer[index], size);
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

static bool handle_events(cloudi_instance_t * api,
                          int external,
                          uint32_t index,
                          int & result,
                          uint32_t command = 0)
{
    buffer_t & buffer_recv = *reinterpret_cast<buffer_t *>(api->buffer_recv);
    if (command == 0)
    {
        if (index > api->buffer_recv_index)
        {
            result = cloudi_error_read_underflow;
            return false;
        }
        store_incoming_uint32(buffer_recv, index, command);
    }
    while (true)
    {
        switch (command)
        {
            case MESSAGE_TERM:
            {
                api->terminate = 1;
                if (external)
                    result = cloudi_success;
                else
                    result = cloudi_terminate;
                return false;
            }
            case MESSAGE_REINIT:
            {
                store_incoming_uint32(buffer_recv, index, api->process_count);
                store_incoming_uint32(buffer_recv, index, api->timeout_async);
                store_incoming_uint32(buffer_recv, index, api->timeout_sync);
                store_incoming_int8(buffer_recv, index, api->priority_default);
                break;
            }
            case MESSAGE_KEEPALIVE:
            {
                result = keepalive(api);
                if (result)
                    return false;
                break;
            }
            default:
            {
                result = cloudi_error_read_underflow;
                return false;
            }
        }
        if (index > api->buffer_recv_index)
        {
            result = cloudi_error_read_underflow;
            return false;
        }
        else if (index == api->buffer_recv_index)
        {
            return true;
        }
        store_incoming_uint32(buffer_recv, index, command);
    }
}

static int poll_request(cloudi_instance_t * api,
                        int timeout,
                        int external)
{
    int result;
    if (api->terminate)
    {
        if (external)
            return cloudi_success;
        else
            return cloudi_terminate;
    }
    else if (external && ! api->initialization_complete)
    {
        result = polling(api);
        if (result)
            return result;
        api->initialization_complete = 1;
    }

    buffer_t & buffer_recv = *reinterpret_cast<buffer_t *>(api->buffer_recv);
    buffer_t & buffer_call = *reinterpret_cast<buffer_t *>(api->buffer_call);

    timer & poll_timer = *reinterpret_cast<timer *>(api->poll_timer);
    if (timeout > 0)
    {
        poll_timer.restart();
    }
    struct pollfd fds[1] = {{api->fd, POLLIN | POLLPRI, 0}};
    int count = ::poll(fds, 1, timeout);
    if (count == 0)
        return cloudi_timeout;
    else if (count < 0)
        return errno_poll();

    result = read_all(api->fd, api->use_header,
                      buffer_recv, api->buffer_recv_index,
                      api->buffer_size);
    if (result)
        return result;
    if (api->buffer_recv_index == 0)
        return cloudi_error_read_underflow;
    uint32_t index = 0;

    while (true)
    {
        uint32_t command = 0;
        store_incoming_uint32(buffer_recv, index, command);
        switch (command)
        {
            case MESSAGE_INIT:
            {
                store_incoming_uint32(buffer_recv, index, api->process_index);
                store_incoming_uint32(buffer_recv, index, api->process_count);
                store_incoming_uint32(buffer_recv, index, api->process_count_max);
                store_incoming_uint32(buffer_recv, index, api->process_count_min);
                store_incoming_binary(buffer_recv, index, api->prefix);
                store_incoming_uint32(buffer_recv, index,
                                      api->timeout_initialize);
                store_incoming_uint32(buffer_recv, index, api->timeout_async);
                store_incoming_uint32(buffer_recv, index, api->timeout_sync);
                store_incoming_uint32(buffer_recv, index, api->timeout_terminate);
                store_incoming_int8(buffer_recv, index, api->priority_default);
                if (index != api->buffer_recv_index)
                {
                    assert(! external);
                    if (! handle_events(api, external, index, result))
                        return result;
                }
                api->buffer_recv_index = 0;
                return cloudi_success;
            }
            case MESSAGE_SEND_ASYNC:
            case MESSAGE_SEND_SYNC:
            {
                buffer_call.copy(buffer_recv);
                uint32_t name_size;
                store_incoming_uint32(buffer_call, index, name_size);
                char * name = &buffer_call[index];
                index += name_size;
                uint32_t pattern_size;
                store_incoming_uint32(buffer_call, index, pattern_size);
                char * pattern = &buffer_call[index];
                index += pattern_size;
                uint32_t request_info_size;
                store_incoming_uint32(buffer_call, index, request_info_size);
                char * request_info = &buffer_call[index];
                index += request_info_size + 1;
                uint32_t request_size;
                store_incoming_uint32(buffer_call, index, request_size);
                char * request = &buffer_call[index];
                index += request_size + 1;
                uint32_t request_timeout;
                store_incoming_uint32(buffer_call, index, request_timeout);
                int8_t priority;
                store_incoming_int8(buffer_call, index, priority);
                char * trans_id = &buffer_call[index];
                index += 16;
                uint32_t pid_size;
                store_incoming_uint32(buffer_call, index, pid_size);
                char * pid = &buffer_call[index];
                index += pid_size;
                if (index != api->buffer_recv_index)
                {
                    assert(external);
                    if (! handle_events(api, external, index, result))
                        return result;
                }
                api->buffer_recv_index = 0;
                callback(api, command, name, pattern,
                         request_info, request_info_size,
                         request, request_size, request_timeout,
                         priority, trans_id, pid, pid_size);
                if (api->terminate)
                    return cloudi_success;
                break;
            }
            case MESSAGE_RECV_ASYNC:
            case MESSAGE_RETURN_SYNC:
            {
                store_incoming_uint32(buffer_recv, index,
                                      api->response_info_size);
                api->response_info = &buffer_recv[index];
                index += api->response_info_size + 1;
                store_incoming_uint32(buffer_recv, index, api->response_size);
                api->response = &buffer_recv[index];
                index += api->response_size + 1;
                api->trans_id_count = 1;
                api->trans_id = &buffer_recv[index];
                index += 16;
                if (index != api->buffer_recv_index)
                {
                    assert(! external);
                    if (! handle_events(api, external, index, result))
                        return result;
                }
                api->buffer_recv_index = 0;
                return cloudi_success;
            }
            case MESSAGE_RETURN_ASYNC:
            {
                api->trans_id_count = 1;
                api->trans_id = &buffer_recv[index];
                index += 16;
                if (index != api->buffer_recv_index)
                {
                    assert(! external);
                    if (! handle_events(api, external, index, result))
                        return result;
                }
                api->buffer_recv_index = 0;
                return cloudi_success;
            }
            case MESSAGE_RETURNS_ASYNC:
            {
                store_incoming_uint32(buffer_recv, index, api->trans_id_count);
                api->trans_id = &buffer_recv[index];
                index += 16 * api->trans_id_count;
                if (index != api->buffer_recv_index)
                {
                    assert(! external);
                    if (! handle_events(api, external, index, result))
                        return result;
                }
                api->buffer_recv_index = 0;
                return cloudi_success;
            }
            case MESSAGE_SUBSCRIBE_COUNT:
            {
                store_incoming_uint32(buffer_recv, index, api->subscribe_count);
                if (index != api->buffer_recv_index)
                {
                    assert(! external);
                    if (! handle_events(api, external, index, result))
                        return result;
                }
                api->buffer_recv_index = 0;
                return cloudi_success;
            }
            case MESSAGE_TERM:
            {
                if (! handle_events(api, external, index, result, command))
                    return result;
                assert(false);
                break;
            }
            case MESSAGE_REINIT:
            {
                store_incoming_uint32(buffer_recv, index, api->process_count);
                store_incoming_uint32(buffer_recv, index, api->timeout_async);
                store_incoming_uint32(buffer_recv, index, api->timeout_sync);
                store_incoming_int8(buffer_recv, index, api->priority_default);
                if (index == api->buffer_recv_index)
                {
                    api->buffer_recv_index = 0;
                    break;
                }
                else if (index < api->buffer_recv_index)
                {
                    continue;
                }
                else
                {
                    return cloudi_error_read_underflow;
                }
            }
            case MESSAGE_KEEPALIVE:
            {
                result = keepalive(api);
                if (result)
                    return false;
                if (index == api->buffer_recv_index)
                {
                    api->buffer_recv_index = 0;
                    break;
                }
                else if (index < api->buffer_recv_index)
                {
                    continue;
                }
                else
                {
                    return cloudi_error_read_underflow;
                }
            }
            default:
            {
                return cloudi_error_read_underflow;
            }
        }

        if (timeout > 0)
        {
            timeout -= std::min(static_cast<int>(::round(poll_timer.elapsed() *
                                                         1000.0)), timeout);
        }
        if (timeout == 0)
        {
            return cloudi_timeout;
        }
        else if (timeout > 0)
        {
            poll_timer.restart();
        }
        fds[0].revents = 0;
        count = ::poll(fds, 1, timeout);
        if (count == 0)
            return cloudi_timeout;
        else if (count < 0)
            return errno_poll();

        result = read_all(api->fd, api->use_header,
                          buffer_recv, api->buffer_recv_index,
                          api->buffer_size);
        if (result)
            return result;
        if (api->buffer_recv_index == 0)
            return cloudi_error_read_underflow;
        index = 0;
    }
}

int cloudi_poll(cloudi_instance_t * api,
                int timeout)
{
    return poll_request(api, timeout, 1);
}

int cloudi_shutdown(cloudi_instance_t * api,
                    char const * const reason)
{
    buffer_t & buffer = *reinterpret_cast<buffer_t *>(api->buffer_send);
    int index = 0;
    if (api->use_header)
        index = 4;

    if (ei_encode_version(buffer.get<char>(), &index))
        return cloudi_error_ei_encode;
    if (ei_encode_tuple_header(buffer.get<char>(), &index, 2))
        return cloudi_error_ei_encode;
    if (ei_encode_atom(buffer.get<char>(), &index, "shutdown"))
        return cloudi_error_ei_encode;
    if (reason == 0)
    {
        if (ei_encode_string(buffer.get<char>(), &index, ""))
            return cloudi_error_ei_encode;
    }
    else
    {
        if (ei_encode_string(buffer.get<char>(), &index, reason))
            return cloudi_error_ei_encode;
    }
    int result = write_exact(api->fd, api->use_header,
                             buffer.get<char>(), index);
    if (result)
        return result;
    return cloudi_success;
}

static char const ** text_pairs_parse(void const * const text,
                                      uint32_t const text_size)
{
    char const * text_p = reinterpret_cast<char const *>(text);
    realloc_ptr<char const *> pairs(16, 268435456);
    if (text_size > 1)
    {
        size_t i = 1;
        pairs[0] = text_p;
        for (size_t text_i = 1; text_i < text_size - 1; ++text_i)
        {
            if (text_p[text_i] == '\0')
            {
                pairs[i] = &text_p[++text_i];
                if (! pairs.reserve(++i + 1))
                    break;
            }
        }
        pairs[i] = 0;
    }
    else
    {
        pairs[0] = 0;
        pairs[1] = 0;
    }
    return pairs.release(true);
}

static void text_pairs_parse_destroy(char const ** pairs)
{
    ::free(pairs);
}

static char const * text_pairs_new(char const ** pairs,
                                   uint32_t & text_size,
                                   bool response)
{
    realloc_ptr<char> text(1024, 1073741824);
    size_t size = 0;
    if (pairs)
    {
        char const * key;
        for (size_t i = 0; (key = pairs[i]); i += 2)
        {
            char const * const value = pairs[i + 1];
            assert(value);
            size_t const key_size = ::strlen(key) + 1;
            size_t const value_size = ::strlen(value) + 1;
            if (! text.reserve(size + key_size + value_size))
                break;
            ::memcpy(&text[size], key, key_size);
            size += key_size;
            ::memcpy(&text[size], value, value_size);
            size += value_size;
        }
    }
    if (response && size == 0)
    {
        text[0] = '\0';
        size = 1;
    }
    text_size = size;
    return text.release(true);
}

// CloudI helper functions

char const ** cloudi_info_key_value_parse(void const * const info,
                                          uint32_t const info_size)
{
    return text_pairs_parse(info, info_size);
}

void cloudi_info_key_value_parse_destroy(char const ** pairs)
{
    text_pairs_parse_destroy(pairs);
}

char const * cloudi_info_key_value_new(char const ** pairs,
                                       uint32_t * info_size,
                                       int response)
{
    return text_pairs_new(pairs, *info_size, response);
}

void cloudi_free_name(cloudi_instance_t * api)
{
    assert(api->free_name == 0);
    api->free_name = 1;
}

void cloudi_free_pattern(cloudi_instance_t * api)
{
    assert(api->free_pattern == 0);
    api->free_pattern = 1;
}

void cloudi_free_request_info(cloudi_instance_t * api)
{
    assert(api->free_request_info == 0);
    api->free_request_info = 1;
}

void cloudi_free_request(cloudi_instance_t * api)
{
    assert(api->free_request == 0);
    api->free_request = 1;
}

void cloudi_free_response_info(cloudi_instance_t * api)
{
    assert(api->free_response_info == 0);
    api->free_response_info = 1;
}

void cloudi_free_response(cloudi_instance_t * api)
{
    assert(api->free_response == 0);
    api->free_response = 1;
}

} // extern C

// C++ API
namespace CloudI
{

API::API(unsigned int const thread_index,
         bool const terminate_return_value)
{
    cloudi_instance_t * const api = m_impl.api();
    int const result = cloudi_initialize(api, thread_index, 0);
    if (result == return_value::success)
    {
        api->cxx_terminate_exception = (terminate_return_value == false);
    }
    else
    {
        if (result == return_value::terminate)
            throw terminate_exception(api->timeout_terminate);
        else
            throw invalid_input_exception(result);
    }
}

unsigned int API::thread_count()
{
    unsigned int thread_count;
    int const result = cloudi_initialize_thread_count(&thread_count);
    if (result != return_value::success)
        throw invalid_input_exception(result);
    return thread_count;
}

int API::subscribe(char const * const pattern,
                   callback_function_generic * p) const
{
    return cloudi_subscribe_(m_impl.api(),
                             pattern,
                             callback_function(p));
}

int API::subscribe_count(char const * const pattern) const
{
    cloudi_instance_t * const api = m_impl.api();
    int const result = cloudi_subscribe_count(api,
                                              pattern);
    if (result == return_value::terminate && api->cxx_terminate_exception)
        throw terminate_exception(api->timeout_terminate);
    return result;
}

int API::unsubscribe(char const * const pattern) const
{
    return cloudi_unsubscribe(m_impl.api(),
                              pattern);
}

int API::send_async(char const * const name,
                    void const * const request,
                    uint32_t const request_size) const
{
    cloudi_instance_t * const api = m_impl.api();
    int const result = cloudi_send_async(api,
                                         name,
                                         request,
                                         request_size);
    if (result == return_value::terminate && api->cxx_terminate_exception)
        throw terminate_exception(api->timeout_terminate);
    return result;
}

int API::send_async(char const * const name,
                    void const * const request_info,
                    uint32_t const request_info_size,
                    void const * const request,
                    uint32_t const request_size,
                    uint32_t timeout,
                    int8_t const priority) const
{
    cloudi_instance_t * const api = m_impl.api();
    int const result = cloudi_send_async_(api,
                                          name,
                                          request_info,
                                          request_info_size,
                                          request,
                                          request_size,
                                          timeout,
                                          priority);
    if (result == return_value::terminate && api->cxx_terminate_exception)
        throw terminate_exception(api->timeout_terminate);
    return result;
}

int API::send_sync(char const * const name,
                   void const * const request,
                   uint32_t const request_size) const
{
    cloudi_instance_t * const api = m_impl.api();
    int const result = cloudi_send_sync(api,
                                        name,
                                        request,
                                        request_size);
    if (result == return_value::terminate && api->cxx_terminate_exception)
        throw terminate_exception(api->timeout_terminate);
    return result;
}

int API::send_sync(char const * const name,
                   void const * const request_info,
                   uint32_t const request_info_size,
                   void const * const request,
                   uint32_t const request_size,
                   uint32_t timeout,
                   int8_t const priority) const
{
    cloudi_instance_t * const api = m_impl.api();
    int const result = cloudi_send_sync_(api,
                                         name,
                                         request_info,
                                         request_info_size,
                                         request,
                                         request_size,
                                         timeout,
                                         priority);
    if (result == return_value::terminate && api->cxx_terminate_exception)
        throw terminate_exception(api->timeout_terminate);
    return result;
}

int API::mcast_async(char const * const name,
                     void const * const request,
                     uint32_t const request_size) const
{
    cloudi_instance_t * const api = m_impl.api();
    int const result = cloudi_mcast_async(api,
                                          name,
                                          request,
                                          request_size);
    if (result == return_value::terminate && api->cxx_terminate_exception)
        throw terminate_exception(api->timeout_terminate);
    return result;
}

int API::mcast_async(char const * const name,
                     void const * const request_info,
                     uint32_t const request_info_size,
                     void const * const request,
                     uint32_t const request_size,
                     uint32_t timeout,
                     int8_t const priority) const
{
    cloudi_instance_t * const api = m_impl.api();
    int const result = cloudi_mcast_async_(api,
                                           name,
                                           request_info,
                                           request_info_size,
                                           request,
                                           request_size,
                                           timeout,
                                           priority);
    if (result == return_value::terminate && api->cxx_terminate_exception)
        throw terminate_exception(api->timeout_terminate);
    return result;
}

char const * API::get_response() const
{
    return m_impl.api()->response;
}

uint32_t API::get_response_size() const
{
    return m_impl.api()->response_size;
}

char const * API::get_response_info() const
{
    return m_impl.api()->response_info;
}

uint32_t API::get_response_info_size() const
{
    return m_impl.api()->response_info_size;
}

uint32_t API::get_trans_id_count() const
{
    return m_impl.api()->trans_id_count;
}

char const * API::get_trans_id(unsigned int const i) const
{
    cloudi_instance_t * const api = m_impl.api();
    if (i >= api->trans_id_count)
        return 0;
    return &(api->trans_id[i * 16]);
}

bool API::get_trans_id_null(unsigned int const i) const
{
    char const * const null = "\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0\0";
    char const * const trans_id = get_trans_id(i);
    assert(trans_id != 0);
    return (memcmp(null, trans_id, 16) == 0);
}

uint32_t API::get_subscribe_count() const
{
    return m_impl.api()->subscribe_count;
}

int API::forward_(int const request_type,
                  char const * const name,
                  void const * const request_info,
                  uint32_t const request_info_size,
                  void const * const request,
                  uint32_t const request_size,
                  uint32_t timeout,
                  int8_t const priority,
                  char const * const trans_id,
                  char const * const pid,
                  uint32_t const pid_size) const
{
    return cloudi_forward(m_impl.api(),
                          request_type,
                          name,
                          request_info,
                          request_info_size,
                          request,
                          request_size,
                          timeout,
                          priority,
                          trans_id,
                          pid,
                          pid_size);
}

int API::forward_async(char const * const name,
                       void const * const request_info,
                       uint32_t const request_info_size,
                       void const * const request,
                       uint32_t const request_size,
                       uint32_t timeout,
                       int8_t const priority,
                       char const * const trans_id,
                       char const * const pid,
                       uint32_t const pid_size) const
{
    return cloudi_forward_async(m_impl.api(),
                                name,
                                request_info,
                                request_info_size,
                                request,
                                request_size,
                                timeout,
                                priority,
                                trans_id,
                                pid,
                                pid_size);
}

int API::forward_sync(char const * const name,
                      void const * const request_info,
                      uint32_t const request_info_size,
                      void const * const request,
                      uint32_t const request_size,
                      uint32_t timeout,
                      int8_t const priority,
                      char const * const trans_id,
                      char const * const pid,
                      uint32_t const pid_size) const
{
    return cloudi_forward_sync(m_impl.api(),
                               name,
                               request_info,
                               request_info_size,
                               request,
                               request_size,
                               timeout,
                               priority,
                               trans_id,
                               pid,
                               pid_size);
}

int API::return_(int const request_type,
                 char const * const name,
                 char const * const pattern,
                 void const * const response_info,
                 uint32_t const response_info_size,
                 void const * const response,
                 uint32_t const response_size,
                 uint32_t timeout,
                 char const * const trans_id,
                 char const * const pid,
                 uint32_t const pid_size) const
{
    return cloudi_return(m_impl.api(),
                         request_type,
                         name,
                         pattern,
                         response_info,
                         response_info_size,
                         response,
                         response_size,
                         timeout,
                         trans_id,
                         pid,
                         pid_size);
}

int API::return_async(char const * const name,
                      char const * const pattern,
                      void const * const response_info,
                      uint32_t const response_info_size,
                      void const * const response,
                      uint32_t const response_size,
                      uint32_t timeout,
                      char const * const trans_id,
                      char const * const pid,
                      uint32_t const pid_size) const
{
    return cloudi_return_async(m_impl.api(),
                               name,
                               pattern,
                               response_info,
                               response_info_size,
                               response,
                               response_size,
                               timeout,
                               trans_id,
                               pid,
                               pid_size);
}

int API::return_sync(char const * const name,
                     char const * const pattern,
                     void const * const response_info,
                     uint32_t const response_info_size,
                     void const * const response,
                     uint32_t const response_size,
                     uint32_t timeout,
                     char const * const trans_id,
                     char const * const pid,
                     uint32_t const pid_size) const
{
    return cloudi_return_sync(m_impl.api(),
                              name,
                              pattern,
                              response_info,
                              response_info_size,
                              response,
                              response_size,
                              timeout,
                              trans_id,
                              pid,
                              pid_size);
}

int API::recv_async() const
{
    cloudi_instance_t * const api = m_impl.api();
    int const result = cloudi_recv_async(api,
                                         api->timeout_sync,
                                         0,
                                         1);
    if (result == return_value::terminate && api->cxx_terminate_exception)
        throw terminate_exception(api->timeout_terminate);
    return result;
}

int API::recv_async(uint32_t timeout) const
{
    cloudi_instance_t * const api = m_impl.api();
    int const result = cloudi_recv_async(api,
                                         timeout,
                                         0,
                                         1);
    if (result == return_value::terminate && api->cxx_terminate_exception)
        throw terminate_exception(api->timeout_terminate);
    return result;
}

int API::recv_async(char const * const trans_id) const
{
    cloudi_instance_t * const api = m_impl.api();
    int const result = cloudi_recv_async(api,
                                         api->timeout_sync,
                                         trans_id,
                                         1);
    if (result == return_value::terminate && api->cxx_terminate_exception)
        throw terminate_exception(api->timeout_terminate);
    return result;
}

int API::recv_async(uint32_t timeout,
                    char const * const trans_id) const
{
    cloudi_instance_t * const api = m_impl.api();
    int const result = cloudi_recv_async(api,
                                         timeout,
                                         trans_id,
                                         1);
    if (result == return_value::terminate && api->cxx_terminate_exception)
        throw terminate_exception(api->timeout_terminate);
    return result;
}

int API::recv_async(uint32_t timeout,
                    bool consume) const
{
    cloudi_instance_t * const api = m_impl.api();
    int const result = cloudi_recv_async(api,
                                         timeout,
                                         0,
                                         static_cast<int>(consume));
    if (result == return_value::terminate && api->cxx_terminate_exception)
        throw terminate_exception(api->timeout_terminate);
    return result;
}

int API::recv_async(char const * const trans_id,
                    bool consume) const
{
    cloudi_instance_t * const api = m_impl.api();
    int const result = cloudi_recv_async(api,
                                         api->timeout_sync,
                                         trans_id,
                                         static_cast<int>(consume));
    if (result == return_value::terminate && api->cxx_terminate_exception)
        throw terminate_exception(api->timeout_terminate);
    return result;
}

int API::recv_async(uint32_t timeout,
                    char const * const trans_id,
                    bool consume) const
{
    cloudi_instance_t * const api = m_impl.api();
    int const result = cloudi_recv_async(api,
                                         timeout,
                                         trans_id,
                                         static_cast<int>(consume));
    if (result == return_value::terminate && api->cxx_terminate_exception)
        throw terminate_exception(api->timeout_terminate);
    return result;
}

uint32_t API::process_index() const
{
    return m_impl.api()->process_index;
}

uint32_t API::process_count() const
{
    return m_impl.api()->process_count;
}

uint32_t API::process_count_max() const
{
    return m_impl.api()->process_count_max;
}

uint32_t API::process_count_min() const
{
    return m_impl.api()->process_count_min;
}

char const * API::prefix() const
{
    return m_impl.api()->prefix;
}

uint32_t API::timeout_initialize() const
{
    return m_impl.api()->timeout_initialize;
}

uint32_t API::timeout_async() const
{
    return m_impl.api()->timeout_async;
}

uint32_t API::timeout_sync() const
{
    return m_impl.api()->timeout_sync;
}

uint32_t API::timeout_terminate() const
{
    return m_impl.api()->timeout_terminate;
}

int8_t API::priority_default() const
{
    return m_impl.api()->priority_default;
}

int API::poll(int timeout) const
{
    return cloudi_poll(m_impl.api(),
                       timeout);
}

int API::shutdown() const
{
    return cloudi_shutdown(m_impl.api(),
                           "");
}

int API::shutdown(char const * const reason) const
{
    return cloudi_shutdown(m_impl.api(),
                           reason);
}

char const ** API::info_key_value_parse(void const * const info,
                                        uint32_t const info_size)
{
    return cloudi_info_key_value_parse(info, info_size);
}

void API::info_key_value_parse_destroy(char const ** pairs)
{
    cloudi_info_key_value_parse_destroy(pairs);
}

char const * API::info_key_value_new(char const ** pairs,
                                     uint32_t & info_size)
{
    return cloudi_info_key_value_new(pairs, &info_size, true);
}

char const * API::info_key_value_new(char const ** pairs,
                                     uint32_t & info_size,
                                     bool response)
{
    return cloudi_info_key_value_new(pairs, &info_size, response);
}

void API::free_with_delete() const
{
    m_impl.api()->free_with_delete = 1;
}

void API::free_name() const
{
    cloudi_free_name(m_impl.api());
}

void API::free_pattern() const
{
    cloudi_free_pattern(m_impl.api());
}

void API::free_request_info() const
{
    cloudi_free_request_info(m_impl.api());
}

void API::free_request() const
{
    cloudi_free_request(m_impl.api());
}

void API::free_response_info() const
{
    cloudi_free_response_info(m_impl.api());
}

void API::free_response() const
{
    cloudi_free_response(m_impl.api());
}

std::string API::backtrace()
{
    return backtrace_string();
}

class pimpl_t
{
    public:
        pimpl_t() : count(0)
        {
        }
        ~pimpl_t()
        {
            cloudi_destroy(&api);
        }

        cloudi_instance_t api;
        int count; // api shared reference count
};

API::impl_t::impl_t() :
    m_p(new pimpl_t())
{
    reinterpret_cast<pimpl_t *>(m_p)->count++;
}

API::impl_t::impl_t(API::impl_t const & impl) :
    m_p(impl.m_p)
{
    reinterpret_cast<pimpl_t *>(m_p)->count++;
}

API::impl_t::~impl_t()
{
    pimpl_t * p = reinterpret_cast<pimpl_t *>(m_p);
    if (--(p->count) == 0)
    {
        delete p;
    }
}

cloudi_instance_t * API::impl_t::api() const
{
    return &(reinterpret_cast<pimpl_t *>(m_p)->api);
}

} // namespace CloudI

