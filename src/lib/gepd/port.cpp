// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
// ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab:

// GENERIC ERLANG PORT VERSION 0.6
// automatically create Erlang bindings to C++/C that requires an OS process

//////////////////////////////////////////////////////////////////////////////
// BSD LICENSE
// 
// Copyright (c) 2009, Michael Truog <mjtruog at gmail dot com>
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
#include "port.h"
#include "port_main.h"

#if defined(PORT_C_FUNCTIONS_HEADER_FILE)
extern "C"
{
#include PORT_C_FUNCTIONS_HEADER_FILE
}
#elif defined(PORT_CXX_FUNCTIONS_HEADER_FILE)
#include PORT_CXX_FUNCTIONS_HEADER_FILE
#else
#error Neither PORT_C_FUNCTIONS_HEADER_FILE nor \
       PORT_CXX_FUNCTIONS_HEADER_FILE are defined
#endif

#include <time.h>

#define CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_void \
    (void)
#define STORE_RETURN_VALUE_TYPE_void \
    if (ei_encode_version((char *) buffer, &index)) \
        return InternalExitStatus::ei_encode_error; \
    if (ei_encode_atom((char *) buffer, &index, "ok")) \
        return InternalExitStatus::ei_encode_error;

#define GET_TYPE_SIZE_FROM_TYPE_char(N) \
    sizeof(char)
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_char(OFFSET) \
    *((char *) &(buffer[(OFFSET)]))
#define CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_char \
    char returnValue = 
#define STORE_RETURN_VALUE_TYPE_char \
    if (ei_encode_version((char *) buffer, &index)) \
        return InternalExitStatus::ei_encode_error; \
    if (ei_encode_long((char *) buffer, &index, returnValue)) \
        return InternalExitStatus::ei_encode_error;

#define GET_TYPE_SIZE_FROM_TYPE_uchar(N) \
    sizeof(unsigned char)
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_uchar(OFFSET) \
    *((unsigned char *) &(buffer[(OFFSET)]))
#define CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_uchar \
    unsigned char returnValue = 
#define STORE_RETURN_VALUE_TYPE_uchar \
    if (ei_encode_version((char *) buffer, &index)) \
        return InternalExitStatus::ei_encode_error; \
    if (ei_encode_char((char *) buffer, &index, (char) returnValue)) \
        return InternalExitStatus::ei_encode_error;

#define GET_TYPE_SIZE_FROM_TYPE_bool(N) \
    sizeof(uint8_t)
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_bool(OFFSET) \
    *((uint8_t *) &(buffer[(OFFSET)]))
#define CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_bool \
    bool returnValue = 
#define STORE_RETURN_VALUE_TYPE_bool \
    if (ei_encode_version((char *) buffer, &index)) \
        return InternalExitStatus::ei_encode_error; \
    if (ei_encode_boolean((char *) buffer, &index, (int) returnValue)) \
        return InternalExitStatus::ei_encode_error;

#define GET_TYPE_SIZE_FROM_TYPE_int8_t(N) \
    sizeof(int8_t)
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_int8_t(OFFSET) \
    *((int8_t *) &(buffer[(OFFSET)]))
#define CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_int8_t \
    int8_t returnValue = 
#define STORE_RETURN_VALUE_TYPE_int8_t \
    if (ei_encode_version((char *) buffer, &index)) \
        return InternalExitStatus::ei_encode_error; \
    if (ei_encode_long((char *) buffer, &index, returnValue)) \
        return InternalExitStatus::ei_encode_error;

#define GET_TYPE_SIZE_FROM_TYPE_uint8_t(N) \
    sizeof(uint8_t)
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_uint8_t(OFFSET) \
    *((uint8_t *) &(buffer[(OFFSET)]))
#define CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_uint8_t \
    uint8_t returnValue = 
#define STORE_RETURN_VALUE_TYPE_uint8_t \
    if (ei_encode_version((char *) buffer, &index)) \
        return InternalExitStatus::ei_encode_error; \
    if (ei_encode_ulong((char *) buffer, &index, returnValue)) \
        return InternalExitStatus::ei_encode_error;

#define GET_TYPE_SIZE_FROM_TYPE_int16_t(N) \
    sizeof(int16_t)
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_int16_t(OFFSET) \
    *((int16_t *) &(buffer[(OFFSET)]))
#define CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_int16_t \
    int16_t returnValue = 
#define STORE_RETURN_VALUE_TYPE_int16_t \
    if (ei_encode_version((char *) buffer, &index)) \
        return InternalExitStatus::ei_encode_error; \
    if (ei_encode_long((char *) buffer, &index, returnValue)) \
        return InternalExitStatus::ei_encode_error;

#define GET_TYPE_SIZE_FROM_TYPE_uint16_t(N) \
    sizeof(uint16_t)
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_uint16_t(OFFSET) \
    *((uint16_t *) &(buffer[(OFFSET)]))
#define CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_uint16_t \
    uint16_t returnValue = 
#define STORE_RETURN_VALUE_TYPE_uint16_t \
    if (ei_encode_version((char *) buffer, &index)) \
        return InternalExitStatus::ei_encode_error; \
    if (ei_encode_ulong((char *) buffer, &index, returnValue)) \
        return InternalExitStatus::ei_encode_error;

#define GET_TYPE_SIZE_FROM_TYPE_int32_t(N) \
    sizeof(int32_t)
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_int32_t(OFFSET) \
    *((int32_t *) &(buffer[(OFFSET)]))
#define CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_int32_t \
    int32_t returnValue = 
#define STORE_RETURN_VALUE_TYPE_int32_t \
    if (ei_encode_version((char *) buffer, &index)) \
        return InternalExitStatus::ei_encode_error; \
    if (ei_encode_long((char *) buffer, &index, returnValue)) \
        return InternalExitStatus::ei_encode_error;

#define GET_TYPE_SIZE_FROM_TYPE_uint32_t(N) \
    sizeof(uint32_t)
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_uint32_t(OFFSET) \
    *((uint32_t *) &(buffer[(OFFSET)]))
#define CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_uint32_t \
    uint32_t returnValue = 
#define STORE_RETURN_VALUE_TYPE_uint32_t \
    if (ei_encode_version((char *) buffer, &index)) \
        return InternalExitStatus::ei_encode_error; \
    if (ei_encode_ulong((char *) buffer, &index, returnValue)) \
        return InternalExitStatus::ei_encode_error;

#define GET_TYPE_SIZE_FROM_TYPE_int64_t(N) \
    sizeof(int64_t)
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_int64_t(OFFSET) \
    *((int64_t *) &(buffer[(OFFSET)]))
#define CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_int64_t \
    int64_t returnValue = 
#define STORE_RETURN_VALUE_TYPE_int64_t \
    if (ei_encode_version((char *) buffer, &index)) \
        return InternalExitStatus::ei_encode_error; \
    if (ei_encode_longlong((char *) buffer, &index, returnValue)) \
        return InternalExitStatus::ei_encode_error;

#define GET_TYPE_SIZE_FROM_TYPE_uint64_t(N) \
    sizeof(uint64_t)
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_uint64_t(OFFSET) \
    *((uint64_t *) &(buffer[(OFFSET)]))
#define CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_uint64_t \
    uint64_t returnValue = 
#define STORE_RETURN_VALUE_TYPE_uint64_t \
    if (ei_encode_version((char *) buffer, &index)) \
        return InternalExitStatus::ei_encode_error; \
    if (ei_encode_ulonglong((char *) buffer, &index, returnValue)) \
        return InternalExitStatus::ei_encode_error;

#define GET_TYPE_SIZE_FROM_TYPE_time_t(N) \
    sizeof(uint32_t) // enforced
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_time_t(OFFSET) \
    *((uint32_t *) &(buffer[(OFFSET)]))
#define CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_time_t \
    uint32_t returnValue = 0xffffffff & 
#define STORE_RETURN_VALUE_TYPE_time_t \
    if (ei_encode_version((char *) buffer, &index)) \
        return InternalExitStatus::ei_encode_error; \
    if (ei_encode_ulong((char *) buffer, &index, returnValue)) \
        return InternalExitStatus::ei_encode_error;

#define GET_TYPE_SIZE_FROM_TYPE_float(N) \
    sizeof(double)
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_float(OFFSET) \
    ((float) *((double *) &(buffer[(OFFSET)])))
#define CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_float \
    double returnValue =
#define STORE_RETURN_VALUE_TYPE_float \
    if (ei_encode_version((char *) buffer, &index)) \
        return InternalExitStatus::ei_encode_error; \
    if (ei_encode_double((char *) buffer, &index, returnValue)) \
        return InternalExitStatus::ei_encode_error;

#define GET_TYPE_SIZE_FROM_TYPE_double(N) \
    sizeof(double)
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_double(OFFSET) \
    *((double *) &(buffer[(OFFSET)]))
#define CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_double \
    double returnValue =
#define STORE_RETURN_VALUE_TYPE_double \
    if (ei_encode_version((char *) buffer, &index)) \
        return InternalExitStatus::ei_encode_error; \
    if (ei_encode_double((char *) buffer, &index, returnValue)) \
        return InternalExitStatus::ei_encode_error;

#define GET_TYPE_SIZE_FROM_TYPE_pchar_len(N) \
    sizeof(uint32_t) + *((uint32_t *) &(buffer[(\
        BOOST_PP_CAT(offset_arg, N) \
    )]))
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_pchar_len(OFFSET) \
    ((char *) &(buffer[(OFFSET + sizeof(uint32_t))])), \
    *((uint32_t *) &(buffer[(OFFSET)]))
#define CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_pchar \
    char const * returnValue =
#define STORE_RETURN_VALUE_TYPE_pchar \
    if (ei_encode_version((char *) buffer, &index)) \
        return InternalExitStatus::ei_encode_error; \
    if (ei_encode_string((char *) buffer, &index, returnValue)) \
        return InternalExitStatus::ei_encode_error;
    

#include <errno.h>
namespace
{
    // list of non-fatal errors that can be sent back

    // port will send an error for protocol problems
    namespace Error
    {
        char const * const invalid_function = "Invalid function call";
    }

    // provide a single list of exit_status values
    // (use exit_status as an option to erlang:open_port/2)

    // port will exit for conditions related to reading/writing
    namespace InternalExitStatus
    {
        int const port_will_no_exit = 0; // do not exit

        enum
        {
            read_EAGAIN = GEPD::ExitStatus::errors_min,     // 0
            read_EBADF,
            read_EFAULT,
            read_EINTR,
            read_EINVAL,
            read_EIO,
            read_EISDIR,
            read_null,
            read_overflow,
            read_unknown,
            write_EAGAIN,       // 10
            write_EBADF,
            write_EFAULT,
            write_EFBIG,
            write_EINTR,
            write_EINVAL,
            write_EIO,
            write_ENOSPC,
            write_EPIPE,
            write_null,
            write_overflow,     // 20
            write_unknown,
            ei_encode_error,
            poll_EBADF,
            poll_EFAULT,
            poll_EINTR,
            poll_EINVAL,
            poll_ENOMEM,
            poll_ERR,
            poll_NVAL,
            poll_unknown        // 30
        };
        int read_errno()
        {
            switch (errno)
            {
                case EAGAIN:
                    return read_EAGAIN;
                case EBADF:
                    return read_EBADF;
                case EFAULT:
                    return read_EFAULT;
                case EINTR:
                    return read_EINTR;
                case EINVAL:
                    return read_EINVAL;
                case EIO:
                    return read_EIO;
                case EISDIR:
                    return read_EISDIR;
                default:
                    return read_unknown;
            }
        }
        int write_errno()
        {
            switch (errno)
            {
                case EAGAIN:
                    return write_EAGAIN;
                case EBADF:
                    return write_EBADF;
                case EFAULT:
                    return write_EFAULT;
                case EFBIG:
                    return write_EFBIG;
                case EINTR:
                    return write_EINTR;
                case EINVAL:
                    return write_EINVAL;
                case EIO:
                    return write_EIO;
                case ENOSPC:
                    return write_ENOSPC;
                case EPIPE:
                    return write_EPIPE;
                default:
                    return write_unknown;
            }
        }
        int poll_errno()
        {
            switch (errno)
            {
                case EBADF:
                    return poll_EBADF;
                case EFAULT:
                    return poll_EFAULT;
                case EINTR:
                    return poll_EINTR;
                case EINVAL:
                    return poll_EINVAL;
                case ENOMEM:
                    return poll_ENOMEM;
                default:
                    return poll_unknown;
            }
        }
    }
}

#include <unistd.h>

// code below depends on these prefix types
#define INPUT_PREFIX_TYPE   uint16_t // function identifier
#define OUTPUT_PREFIX_TYPE  uint16_t // output size

static int read_exact(unsigned char * buffer, uint16_t const length)
{
    uint32_t got = 0;
    while (got < length)
    {
        ssize_t const i = read(PORT_READ_FILE_DESCRIPTOR,
                               buffer + got, length - got);
        if (i <= 0 || (got + i) > 0xffff)
        {
            if (i == -1)
                return InternalExitStatus::read_errno();
            else if (i == 0)
                return InternalExitStatus::read_null;
            else
                return InternalExitStatus::read_overflow;
        }
        got += (i & 0xffff);
    }
    if (got > length)
        return InternalExitStatus::read_overflow;
    return 0;
}

static int write_exact(unsigned char const * const buffer,
                       uint16_t const length)
{
    uint32_t wrote = 0;
    while (wrote < length)
    {
        ssize_t const i = write(PORT_WRITE_FILE_DESCRIPTOR,
                                buffer + wrote, length - wrote);
        if (i <= 0 || (wrote + i) > 0xffff)
        {
            if (i == -1)
                return InternalExitStatus::write_errno();
            else if (i == 0)
                return InternalExitStatus::write_null;
            else
                return InternalExitStatus::write_overflow;
        }
        wrote += (i & 0xffff);
    }
    if (wrote > length)
        return InternalExitStatus::write_overflow;
    return 0;
}

static int read_cmd(unsigned char * buffer)
{
    unsigned char length[2];
    int const status = read_exact(length, 2);
    if (status)
        return status;
    return read_exact(buffer, (length[0] << 8) | length[1]);
}

static int write_cmd(unsigned char * buffer, uint16_t length)
{
    buffer[0] = (length & 0xff00) >> 8;
    buffer[1] = length & 0xff;
    return write_exact(buffer, length + 2);
}

#include <cstring>
#include <ei.h>

#include <boost/preprocessor/cat.hpp>
#include <boost/preprocessor/repetition/enum.hpp>
#include <boost/preprocessor/repetition/repeat_from_to.hpp>
#include <boost/preprocessor/seq/for_each.hpp>
#include <boost/preprocessor/seq/size.hpp>
#include <boost/preprocessor/seq/elem.hpp>
#include <boost/preprocessor/tuple/to_seq.hpp>
#include <boost/preprocessor/tuple/elem.hpp>
#include <boost/preprocessor/punctuation/paren.hpp>
#include <boost/preprocessor/arithmetic/dec.hpp>
#include <boost/preprocessor/facilities/empty.hpp>
#include <boost/preprocessor/control/if.hpp>

#define STORE_RETURN_VALUE(TYPE) \
    BOOST_PP_CAT(STORE_RETURN_VALUE_TYPE_, TYPE)

#define GET_FUNCTION_ARGUMENT(TYPE, OFFSET) \
    BOOST_PP_CAT(\
        GET_FUNCTION_ARGUMENT_FROM_TYPE_, TYPE \
    )(OFFSET)

#define CREATE_FUNCTION_RETURN_VALUE_STORE(TYPE) \
    BOOST_PP_CAT(\
        CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_, TYPE\
    )

#define GET_TYPE_SIZE(TYPE, N) \
    BOOST_PP_CAT(GET_TYPE_SIZE_FROM_TYPE_, TYPE)(N)

#define STORE_FUNCTION_ARGUMENT_SIZE(TYPE, N, OFFSET) \
    size_t const BOOST_PP_CAT(offset_arg, N) = OFFSET ;

#define STORE_FUNCTION_ARGUMENTS(Z, N, DATA) \
    STORE_FUNCTION_ARGUMENT_SIZE( \
        BOOST_PP_SEQ_ELEM(N, BOOST_PP_SEQ_ELEM(0, DATA)), \
        BOOST_PP_INC(N), \
        BOOST_PP_CAT(offset_arg, N) \
        + GET_TYPE_SIZE(BOOST_PP_SEQ_ELEM(N, BOOST_PP_SEQ_ELEM(0, DATA)), N) \
    )

#define CREATE_FUNCTION_ARGUMENTS(Z, N, DATA) \
    GET_FUNCTION_ARGUMENT(\
        BOOST_PP_SEQ_ELEM(N, BOOST_PP_SEQ_ELEM(0, DATA)), \
        BOOST_PP_CAT(offset_arg, N) \
    )

#define CREATE_FUNCTION(I, OFFSETS, FUNCTION) \
case BOOST_PP_DEC(I):\
{\
    BOOST_PP_IF(\
        GET_ARGC(FUNCTION),\
        size_t const offset_arg0 = BOOST_PP_SEQ_ELEM(0, OFFSETS);, \
        BOOST_PP_EMPTY() \
    )\
    BOOST_PP_REPEAT_FROM_TO( \
        0, \
        BOOST_PP_DEC(GET_ARGC(FUNCTION)), \
        STORE_FUNCTION_ARGUMENTS, \
        (BOOST_PP_TUPLE_TO_SEQ(GET_ARGC(FUNCTION), GET_ARGV(FUNCTION)))\
        (BOOST_PP_SEQ_ELEM(0, OFFSETS)) \
    ) \
    CREATE_FUNCTION_RETURN_VALUE_STORE(GET_RETURN(FUNCTION)) \
    GET_NAME(FUNCTION) \
    BOOST_PP_LPAREN() \
    BOOST_PP_ENUM( \
        GET_ARGC(FUNCTION), \
        CREATE_FUNCTION_ARGUMENTS, \
        (BOOST_PP_TUPLE_TO_SEQ(GET_ARGC(FUNCTION), GET_ARGV(FUNCTION)))\
        (BOOST_PP_SEQ_ELEM(0, OFFSETS)) \
    ) \
    BOOST_PP_RPAREN() \
    ; \
    int index = BOOST_PP_SEQ_ELEM(1, OFFSETS); \
    STORE_RETURN_VALUE(GET_RETURN(FUNCTION)) \
    if ((status = write_cmd(buffer, index - BOOST_PP_SEQ_ELEM(1, OFFSETS)))) \
        return status; \
    break;\
}

static int reply_error_string(unsigned char * buffer,
                              int &index, char const * const str)
{
    if (ei_encode_version((char *) buffer, &index))
        return InternalExitStatus::ei_encode_error;
    if (ei_encode_tuple_header((char *) buffer, &index, 2))
        return InternalExitStatus::ei_encode_error;
    if (ei_encode_atom((char *) buffer, &index, "error"))
        return InternalExitStatus::ei_encode_error;
    if (ei_encode_string((char *) buffer, &index, str))
        return InternalExitStatus::ei_encode_error;
    return InternalExitStatus::port_will_no_exit;
}

// main loop for handling inherently synchronous function calls
// (a linked-in Erlang port driver that makes synchronous calls with
//  driver level locking should be similar to an Erlang port, except that
//  the port driver is a VM process and the port is an OS process)

int GEPD::nonpolling_main()
{
    struct pollfd fds[] = {{PORT_READ_FILE_DESCRIPTOR, POLLIN | POLLPRI, 0}};
    int const nfds = 1;
    int const timeout = 0; // milliseconds
    unsigned char buffer[32768];
    return polling_main(fds, nfds, timeout, buffer);
}

// pollfd should be allocated to include the fd PORT_READ_FILE_DESCRIPTOR
// as: {PORT_READ_FILE_DESCRIPTOR, POLLIN | POLLPRI, 0}
// in the first element
//
// buffer should be stack allocated as:
// unsigned char buffer[32768];
// (assuming the option {packet, 2} for open_port/2)
int GEPD::polling_main(struct pollfd *fds, nfds_t const & nfds, int timeout,
                       unsigned char *buffer)
{
    int status;
    while ((status = poll(fds, nfds, timeout)) > 0)
    {
        short & revents = fds[0].revents;
        if (revents == 0)
            return GEPD::ExitStatus::external_fd_ready;
        else if (revents & POLLERR)
            return InternalExitStatus::poll_ERR;
        else if (revents & POLLHUP)
            return 0; // erlang exited
        else if (revents & POLLNVAL)
            return InternalExitStatus::poll_NVAL;
        revents = 0;
        if (! (status = read_cmd(buffer)))
        {
            switch (*((INPUT_PREFIX_TYPE *) buffer))
            {
                BOOST_PP_SEQ_FOR_EACH(CREATE_FUNCTION,
                                      (sizeof(INPUT_PREFIX_TYPE))
                                      (sizeof(OUTPUT_PREFIX_TYPE)),
                                      PORT_FUNCTIONS)
    
                default:
                    int index = sizeof(OUTPUT_PREFIX_TYPE);
                    if ((status = reply_error_string(
                         buffer, index, Error::invalid_function)))
                        return status;
                    if ((status = write_cmd(
                         buffer, index - sizeof(OUTPUT_PREFIX_TYPE))))
                        return status;
                    break;
            }
        }
    }
    if (status == 0)
        return GEPD::ExitStatus::poll_timeout;
    else
        return InternalExitStatus::poll_errno();
}

