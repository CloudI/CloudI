//-*-Mode:C++;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=cpp fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
//////////////////////////////////////////////////////////////////////////////
//
// GENERIC ERLANG PORT [DRIVER]
// automatically create Erlang bindings to C++/C that requires an OS process
//
// MIT License
//
// Copyright (c) 2009-2017 Michael Truog <mjtruog at protonmail dot com>
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

#include <cstring>
#include <cstdio>
#include <cstdlib>
#include <time.h>
#include <unistd.h>
#include <errno.h>
#include <ei.h>
#include <boost/preprocessor/cat.hpp>
#include <boost/preprocessor/repetition/enum.hpp>
#include <boost/preprocessor/repetition/repeat_from_to.hpp>
#include <boost/preprocessor/seq/for_each.hpp>
#include <boost/preprocessor/seq/size.hpp>
#include <boost/preprocessor/seq/elem.hpp>
#include <boost/preprocessor/seq/transform.hpp>
//#include <boost/preprocessor/tuple/to_seq.hpp> // broken with boost >= 1.5?
#include <boost/preprocessor/tuple/to_list.hpp>
#include <boost/preprocessor/tuple/elem.hpp>
#include <boost/preprocessor/list/for_each.hpp>
#include <boost/preprocessor/punctuation/paren.hpp>
#include <boost/preprocessor/arithmetic/dec.hpp>
#include <boost/preprocessor/facilities/empty.hpp>
#include <boost/preprocessor/control/if.hpp>

// work-around instead of BOOST_PP_TUPLE_TO_SEQ to correctly handle arity 0
#define TUPLE_TO_SEQ_E(r, data, elem) (elem)
#define TUPLE_TO_SEQ(I, T) \
    BOOST_PP_LIST_FOR_EACH(TUPLE_TO_SEQ_E, _, BOOST_PP_TUPLE_TO_LIST(I, T))

#include "port.hpp"
#include "realloc_ptr.hpp"
#include "pchar_len_t.h"

// erlang:open_port/2 option nouse_stdio
#define PORT_READ_FILE_DESCRIPTOR 3
#define PORT_WRITE_FILE_DESCRIPTOR 4

// code below depends on these prefix types
#define INPUT_PREFIX_TYPE    uint16_t // function identifier
#define OUTPUT_PREFIX_TYPE   uint32_t // maximum length


#if ! defined(PORT_NAME)
#error Define PORT_NAME within the functions header file to specify the \
       executable name
#endif
#if ! defined(PORT_FUNCTIONS)
#if defined(PORT_DRIVER_FUNCTIONS)
// Using PORT_DRIVER_FUNCTIONS to determine PORT_FUNCTIONS

#define CREATE_PORT_FUNCTIONS_DEFINITION(S, DATA, ELEMENT) (\
    BOOST_PP_TUPLE_ELEM(5, 0, ELEMENT),\
    BOOST_PP_TUPLE_ELEM(5, 1, ELEMENT),\
    BOOST_PP_TUPLE_ELEM(5, 2, ELEMENT),\
    BOOST_PP_TUPLE_ELEM(5, 3, ELEMENT))\

#define PORT_FUNCTIONS \
    BOOST_PP_SEQ_TRANSFORM(CREATE_PORT_FUNCTIONS_DEFINITION, _, \
                           PORT_DRIVER_FUNCTIONS)
#else
#error Define PORT_FUNCTIONS within the functions header file to specify \
       the functions and their types
#endif
#endif

// define the structure of the PORT_FUNCTIONS macro data
// (sequence of tuples)

// 4 tuple elements in the PORT_FUNCTIONS sequence
#define PORT_FUNCTION_ENTRY_LENGTH   4
// specific tuple elements in the PORT_FUNCTIONS sequence
#define PORT_FUNCTION_ENTRY_NAME     0
#define PORT_FUNCTION_ENTRY_ARGC     1
#define PORT_FUNCTION_ENTRY_ARGV     2
#define PORT_FUNCTION_ENTRY_RETURN   3

// macros to access function data in a PORT_FUNCTIONS tuple entry

#define GET_NAME(FUNCTION) \
    BOOST_PP_TUPLE_ELEM(\
        PORT_FUNCTION_ENTRY_LENGTH, \
        PORT_FUNCTION_ENTRY_NAME, FUNCTION\
    )
#define GET_ARGC(FUNCTION) \
    BOOST_PP_TUPLE_ELEM(\
        PORT_FUNCTION_ENTRY_LENGTH, \
        PORT_FUNCTION_ENTRY_ARGC, FUNCTION\
    )
#define GET_ARGV(FUNCTION) \
    BOOST_PP_TUPLE_ELEM(\
        PORT_FUNCTION_ENTRY_LENGTH, \
        PORT_FUNCTION_ENTRY_ARGV, FUNCTION\
    )
#define GET_RETURN(FUNCTION) \
    BOOST_PP_TUPLE_ELEM(\
        PORT_FUNCTION_ENTRY_LENGTH, \
        PORT_FUNCTION_ENTRY_RETURN, FUNCTION\
    )
#define GET_ASYNC(FUNCTION) 0

// enforce inherent implementation limits

#if BOOST_PP_SEQ_SIZE(PORT_FUNCTIONS) > 32767
#error Limited to 32767 port functions (type uint16_t is used for "cmd")
#endif

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

#define CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_void                          \
    (void)
#define STORE_RETURN_VALUE_TYPE_void(CMD)                                     \
    if (ei_encode_version(buffer.get<char>(), &index))                        \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_tuple_header(buffer.get<char>(), &index, 2))                \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_ulong(buffer.get<char>(), &index, CMD))                     \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_atom(buffer.get<char>(), &index, "ok"))                     \
        return GEPD::ExitStatus::ei_encode_error;

#define GET_TYPE_SIZE_FROM_TYPE_char(N)                                       \
    sizeof(char)
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_char(OFFSET)                          \
    *((char *) &(buffer[(OFFSET)]))
#define CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_char                          \
    char returnValue = 
#define STORE_RETURN_VALUE_TYPE_char(CMD)                                     \
    if (ei_encode_version(buffer.get<char>(), &index))                        \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_tuple_header(buffer.get<char>(), &index, 2))                \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_ulong(buffer.get<char>(), &index, CMD))                     \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_long(buffer.get<char>(), &index, returnValue))              \
        return GEPD::ExitStatus::ei_encode_error;

#define GET_TYPE_SIZE_FROM_TYPE_uchar(N)                                      \
    sizeof(unsigned char)
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_uchar(OFFSET)                         \
    *((unsigned char *) &(buffer[(OFFSET)]))
#define CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_uchar                         \
    unsigned char returnValue = 
#define STORE_RETURN_VALUE_TYPE_uchar(CMD)                                    \
    if (ei_encode_version(buffer.get<char>(), &index))                        \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_tuple_header(buffer.get<char>(), &index, 2))                \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_ulong(buffer.get<char>(), &index, CMD))                     \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_char(buffer.get<char>(), &index, (char) returnValue))       \
        return GEPD::ExitStatus::ei_encode_error;

#define GET_TYPE_SIZE_FROM_TYPE_bool(N)                                       \
    sizeof(uint8_t)
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_bool(OFFSET)                          \
    *((uint8_t *) &(buffer[(OFFSET)]))
#define CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_bool                          \
    bool returnValue = 
#define STORE_RETURN_VALUE_TYPE_bool(CMD)                                     \
    if (ei_encode_version(buffer.get<char>(), &index))                        \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_tuple_header(buffer.get<char>(), &index, 2))                \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_ulong(buffer.get<char>(), &index, CMD))                     \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_boolean(buffer.get<char>(), &index, (int) returnValue))     \
        return GEPD::ExitStatus::ei_encode_error;

#define GET_TYPE_SIZE_FROM_TYPE_int8_t(N)                                     \
    sizeof(int8_t)
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_int8_t(OFFSET)                        \
    *((int8_t *) &(buffer[(OFFSET)]))
#define CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_int8_t                        \
    int8_t returnValue = 
#define STORE_RETURN_VALUE_TYPE_int8_t(CMD)                                   \
    if (ei_encode_version(buffer.get<char>(), &index))                        \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_tuple_header(buffer.get<char>(), &index, 2))                \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_ulong(buffer.get<char>(), &index, CMD))                     \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_long(buffer.get<char>(), &index, returnValue))              \
        return GEPD::ExitStatus::ei_encode_error;

#define GET_TYPE_SIZE_FROM_TYPE_uint8_t(N)                                    \
    sizeof(uint8_t)
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_uint8_t(OFFSET)                       \
    *((uint8_t *) &(buffer[(OFFSET)]))
#define CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_uint8_t                       \
    uint8_t returnValue = 
#define STORE_RETURN_VALUE_TYPE_uint8_t(CMD)                                  \
    if (ei_encode_version(buffer.get<char>(), &index))                        \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_tuple_header(buffer.get<char>(), &index, 2))                \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_ulong(buffer.get<char>(), &index, CMD))                     \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_ulong(buffer.get<char>(), &index, returnValue))             \
        return GEPD::ExitStatus::ei_encode_error;

#define GET_TYPE_SIZE_FROM_TYPE_int16_t(N)                                    \
    sizeof(int16_t)
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_int16_t(OFFSET)                       \
    *((int16_t *) &(buffer[(OFFSET)]))
#define CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_int16_t                       \
    int16_t returnValue = 
#define STORE_RETURN_VALUE_TYPE_int16_t(CMD)                                  \
    if (ei_encode_version(buffer.get<char>(), &index))                        \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_tuple_header(buffer.get<char>(), &index, 2))                \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_ulong(buffer.get<char>(), &index, CMD))                     \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_long(buffer.get<char>(), &index, returnValue))              \
        return GEPD::ExitStatus::ei_encode_error;

#define GET_TYPE_SIZE_FROM_TYPE_uint16_t(N)                                   \
    sizeof(uint16_t)
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_uint16_t(OFFSET)                      \
    *((uint16_t *) &(buffer[(OFFSET)]))
#define CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_uint16_t                      \
    uint16_t returnValue = 
#define STORE_RETURN_VALUE_TYPE_uint16_t(CMD)                                 \
    if (ei_encode_version(buffer.get<char>(), &index))                        \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_tuple_header(buffer.get<char>(), &index, 2))                \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_ulong(buffer.get<char>(), &index, CMD))                     \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_ulong(buffer.get<char>(), &index, returnValue))             \
        return GEPD::ExitStatus::ei_encode_error;

#define GET_TYPE_SIZE_FROM_TYPE_int32_t(N)                                    \
    sizeof(int32_t)
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_int32_t(OFFSET)                       \
    *((int32_t *) &(buffer[(OFFSET)]))
#define CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_int32_t                       \
    int32_t returnValue = 
#define STORE_RETURN_VALUE_TYPE_int32_t(CMD)                                  \
    if (ei_encode_version(buffer.get<char>(), &index))                        \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_tuple_header(buffer.get<char>(), &index, 2))                \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_ulong(buffer.get<char>(), &index, CMD))                     \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_long(buffer.get<char>(), &index, returnValue))              \
        return GEPD::ExitStatus::ei_encode_error;

#define GET_TYPE_SIZE_FROM_TYPE_uint32_t(N)                                   \
    sizeof(uint32_t)
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_uint32_t(OFFSET)                      \
    *((uint32_t *) &(buffer[(OFFSET)]))
#define CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_uint32_t                      \
    uint32_t returnValue = 
#define STORE_RETURN_VALUE_TYPE_uint32_t(CMD)                                 \
    if (ei_encode_version(buffer.get<char>(), &index))                        \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_tuple_header(buffer.get<char>(), &index, 2))                \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_ulong(buffer.get<char>(), &index, CMD))                     \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_ulong(buffer.get<char>(), &index, returnValue))             \
        return GEPD::ExitStatus::ei_encode_error;

#define GET_TYPE_SIZE_FROM_TYPE_int64_t(N)                                    \
    sizeof(int64_t)
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_int64_t(OFFSET)                       \
    *((int64_t *) &(buffer[(OFFSET)]))
#define CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_int64_t                       \
    int64_t returnValue = 
#define STORE_RETURN_VALUE_TYPE_int64_t(CMD)                                  \
    if (ei_encode_version(buffer.get<char>(), &index))                        \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_tuple_header(buffer.get<char>(), &index, 2))                \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_ulong(buffer.get<char>(), &index, CMD))                     \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_longlong(buffer.get<char>(), &index, returnValue))          \
        return GEPD::ExitStatus::ei_encode_error;

#define GET_TYPE_SIZE_FROM_TYPE_uint64_t(N)                                   \
    sizeof(uint64_t)
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_uint64_t(OFFSET)                      \
    *((uint64_t *) &(buffer[(OFFSET)]))
#define CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_uint64_t                      \
    uint64_t returnValue = 
#define STORE_RETURN_VALUE_TYPE_uint64_t(CMD)                                 \
    if (ei_encode_version(buffer.get<char>(), &index))                        \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_tuple_header(buffer.get<char>(), &index, 2))                \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_ulong(buffer.get<char>(), &index, CMD))                     \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_ulonglong(buffer.get<char>(), &index, returnValue))         \
        return GEPD::ExitStatus::ei_encode_error;

#define GET_TYPE_SIZE_FROM_TYPE_time_t(N)                                     \
    sizeof(uint64_t)
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_time_t(OFFSET)                        \
    *((uint64_t *) &(buffer[(OFFSET)]))
#define CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_time_t                        \
    uint64_t returnValue = 
#define STORE_RETURN_VALUE_TYPE_time_t(CMD)                                   \
    if (ei_encode_version(buffer.get<char>(), &index))                        \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_tuple_header(buffer.get<char>(), &index, 2))                \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_ulong(buffer.get<char>(), &index, CMD))                     \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_ulonglong(buffer.get<char>(), &index, returnValue))         \
        return GEPD::ExitStatus::ei_encode_error;

#define GET_TYPE_SIZE_FROM_TYPE_float(N)                                      \
    sizeof(double)
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_float(OFFSET)                         \
    ((float) *((double *) &(buffer[(OFFSET)])))
#define CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_float                         \
    double returnValue =
#define STORE_RETURN_VALUE_TYPE_float(CMD)                                    \
    if (ei_encode_version(buffer.get<char>(), &index))                        \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_tuple_header(buffer.get<char>(), &index, 2))                \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_ulong(buffer.get<char>(), &index, CMD))                     \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_double(buffer.get<char>(), &index, returnValue))            \
        return GEPD::ExitStatus::ei_encode_error;

#define GET_TYPE_SIZE_FROM_TYPE_double(N)                                     \
    sizeof(double)
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_double(OFFSET)                        \
    *((double *) &(buffer[(OFFSET)]))
#define CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_double                        \
    double returnValue =
#define STORE_RETURN_VALUE_TYPE_double(CMD)                                   \
    if (ei_encode_version(buffer.get<char>(), &index))                        \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_tuple_header(buffer.get<char>(), &index, 2))                \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_ulong(buffer.get<char>(), &index, CMD))                     \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_double(buffer.get<char>(), &index, returnValue))            \
        return GEPD::ExitStatus::ei_encode_error;

#define GET_TYPE_SIZE_FROM_TYPE_pchar_len(N)                                  \
    sizeof(uint32_t) + *((uint32_t *) &(buffer[(                              \
        BOOST_PP_CAT(offset_arg, N)                                           \
    )]))
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_pchar_len(OFFSET)                     \
    ((char *) &(buffer[(OFFSET + sizeof(uint32_t))])),                        \
    *((uint32_t *) &(buffer[(OFFSET)]))
#define CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_pchar_len_t_nofree            \
    pchar_len_t returnValue =
#define STORE_RETURN_VALUE_TYPE_pchar_len_t_nofree(CMD)                       \
    if (ei_encode_version(buffer.get<char>(), &index))                        \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_tuple_header(buffer.get<char>(), &index, 2))                \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_ulong(buffer.get<char>(), &index, CMD))                     \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (buffer.reserve(index + returnValue.length) == false)                  \
        return GEPD::ExitStatus::write_overflow;                              \
    if (ei_encode_binary(buffer.get<char>(), &index,                          \
                         returnValue.pchar, returnValue.length))              \
        return GEPD::ExitStatus::ei_encode_error;
#define CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_pchar_len_t_free              \
    CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_pchar_len_t_nofree
#define STORE_RETURN_VALUE_TYPE_pchar_len_t_free(CMD)                         \
    STORE_RETURN_VALUE_TYPE_pchar_len_t_nofree(CMD)                           \
    free(returnValue.pchar);
#define CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_pchar_len_t                   \
    CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_pchar_len_t_free
#define STORE_RETURN_VALUE_TYPE_pchar_len_t(CMD)                              \
    STORE_RETURN_VALUE_TYPE_pchar_len_t_free(CMD)

#define CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_pchar_nofree                  \
    char const * returnValue =
#define STORE_RETURN_VALUE_TYPE_pchar_nofree(CMD)                             \
    if (ei_encode_version(buffer.get<char>(), &index))                        \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_tuple_header(buffer.get<char>(), &index, 2))                \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (ei_encode_ulong(buffer.get<char>(), &index, CMD))                     \
        return GEPD::ExitStatus::ei_encode_error;                             \
    if (buffer.reserve(index + strlen(returnValue) + 1) == false)             \
        return GEPD::ExitStatus::write_overflow;                              \
    if (ei_encode_string(buffer.get<char>(), &index, returnValue))            \
        return GEPD::ExitStatus::ei_encode_error;
#define CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_pchar_free                    \
    CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_pchar_nofree
#define STORE_RETURN_VALUE_TYPE_pchar_free(CMD)                               \
    STORE_RETURN_VALUE_TYPE_pchar_nofree(CMD)                                 \
    free(returnValue);
#define CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_pchar                         \
    CREATE_FUNCTION_RETURN_VALUE_STORE_TYPE_pchar_nofree
#define STORE_RETURN_VALUE_TYPE_pchar(CMD)                                    \
    STORE_RETURN_VALUE_TYPE_pchar_nofree(CMD)
    
#define GET_TYPE_SIZE_FROM_TYPE_puint32_len(N)                                \
    sizeof(uint32_t) + *((uint32_t *) &(buffer[(                              \
        BOOST_PP_CAT(offset_arg, N)                                           \
    )])) * sizeof(uint32_t)
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_puint32_len(OFFSET)                   \
    ((uint32_t *) &(buffer[(OFFSET + sizeof(uint32_t))])),                    \
    *((uint32_t *) &(buffer[(OFFSET)]))
    
namespace
{
    // list of non-fatal errors that can be sent back

    // port will send an error for protocol problems
    namespace Error
    {
        char const * const invalid_function = "Invalid function call";
    }

    int errno_read()
    {
        switch (errno)
        {
            case EAGAIN:
                return GEPD::ExitStatus::read_EAGAIN;
            case EBADF:
                return GEPD::ExitStatus::read_EBADF;
            case EFAULT:
                return GEPD::ExitStatus::read_EFAULT;
            case EINTR:
                return GEPD::ExitStatus::read_EINTR;
            case EINVAL:
                return GEPD::ExitStatus::read_EINVAL;
            case EIO:
                return GEPD::ExitStatus::read_EIO;
            case EISDIR:
                return GEPD::ExitStatus::read_EISDIR;
            default:
                return GEPD::ExitStatus::read_unknown;
        }
    }

    int errno_write()
    {
        switch (errno)
        {
            case EAGAIN:
                return GEPD::ExitStatus::write_EAGAIN;
            case EBADF:
                return GEPD::ExitStatus::write_EBADF;
            case EFAULT:
                return GEPD::ExitStatus::write_EFAULT;
            case EFBIG:
                return GEPD::ExitStatus::write_EFBIG;
            case EINTR:
                return GEPD::ExitStatus::write_EINTR;
            case EINVAL:
                return GEPD::ExitStatus::write_EINVAL;
            case EIO:
                return GEPD::ExitStatus::write_EIO;
            case ENOSPC:
                return GEPD::ExitStatus::write_ENOSPC;
            case EPIPE:
                return GEPD::ExitStatus::write_EPIPE;
            default:
                return GEPD::ExitStatus::write_unknown;
        }
    }

    int errno_poll()
    {
        switch (errno)
        {
            case EBADF:
                return GEPD::ExitStatus::poll_EBADF;
            case EFAULT:
                return GEPD::ExitStatus::poll_EFAULT;
            case EINTR:
                return GEPD::ExitStatus::poll_EINTR;
            case EINVAL:
                return GEPD::ExitStatus::poll_EINVAL;
            case ENOMEM:
                return GEPD::ExitStatus::poll_ENOMEM;
            default:
                return GEPD::ExitStatus::poll_unknown;
        }
    }

    int errno_pipe()
    {
        switch (errno)
        {
            case EFAULT:
                return GEPD::ExitStatus::pipe_EFAULT;
            case EINVAL:
                return GEPD::ExitStatus::pipe_EINVAL;
            case EMFILE:
                return GEPD::ExitStatus::pipe_EMFILE;
            case ENFILE:
                return GEPD::ExitStatus::pipe_ENFILE;
            default:
                return GEPD::ExitStatus::pipe_unknown;
        }
    }

    int errno_dup()
    {
        switch (errno)
        {
            case EBADF:
                return GEPD::ExitStatus::dup_EBADF;
            case EBUSY:
                return GEPD::ExitStatus::dup_EBUSY;
            case EINTR:
                return GEPD::ExitStatus::dup_EINTR;
            case EINVAL:
                return GEPD::ExitStatus::dup_EINVAL;
            case EMFILE:
                return GEPD::ExitStatus::dup_EMFILE;
            default:
                return GEPD::ExitStatus::dup_unknown;
        }
    }

    int errno_close()
    {
        switch (errno)
        {
            case EBADF:
                return GEPD::ExitStatus::close_EBADF;
            case EINTR:
                return GEPD::ExitStatus::close_EINTR;
            case EIO:
                return GEPD::ExitStatus::close_EIO;
            default:
                return GEPD::ExitStatus::close_unknown;
        }
    }

    int read_exact(unsigned char * const buffer,
                   uint32_t const length)
    {
        uint32_t total = 0;
        while (total < length)
        {
            ssize_t const i = read(PORT_READ_FILE_DESCRIPTOR,
                                   buffer + total, length - total);
            if (i <= 0)
            {
                if (i == -1)
                    return errno_read();
                else
                    return GEPD::ExitStatus::read_null;
            }
            total += i;
        }
        if (total > length)
            return GEPD::ExitStatus::read_overflow;
        return GEPD::ExitStatus::success;
    }
    
    int write_exact(unsigned char const * const buffer,
                    uint32_t const length)
    {
        uint32_t total = 0;
        while (total < length)
        {
            ssize_t const i = write(PORT_WRITE_FILE_DESCRIPTOR,
                                    buffer + total, length - total);
            if (i <= 0)
            {
                if (i == -1)
                    return errno_write();
                else
                    return GEPD::ExitStatus::write_null;
            }
            total += i;
        }
        if (total > length)
            return GEPD::ExitStatus::write_overflow;
        return GEPD::ExitStatus::success;
    }
    
    int read_cmd(realloc_ptr<unsigned char> & buffer)
    {
        unsigned char lengthData[4];
        int const status = read_exact(lengthData, 4);
        if (status)
            return status;
        uint32_t const length = (lengthData[0] << 24) |
                                (lengthData[1] << 16) |
                                (lengthData[2] <<  8) |
                                 lengthData[3];
        buffer.reserve(length);
        return read_exact(buffer.get(), length);
    }
    
    int write_cmd(realloc_ptr<unsigned char> & buffer, uint32_t length)
    {
        buffer[0] = (length & 0xff000000) >> 24;
        buffer[1] = (length & 0x00ff0000) >> 16;
        buffer[2] = (length & 0x0000ff00) >> 8;
        buffer[3] =  length & 0x000000ff;
        return write_exact(buffer.get(), length + 4);
    }
    
    int reply_error_string(realloc_ptr<unsigned char> & buffer,
                           int & index, uint16_t cmd, char const * const str)
    {
        if (ei_encode_version(buffer.get<char>(), &index))
            return GEPD::ExitStatus::ei_encode_error;
        if (ei_encode_tuple_header(buffer.get<char>(), &index, 3))
            return GEPD::ExitStatus::ei_encode_error;
        if (ei_encode_atom(buffer.get<char>(), &index, "error"))
            return GEPD::ExitStatus::ei_encode_error;
        if (ei_encode_ulong(buffer.get<char>(), &index, cmd))
            return GEPD::ExitStatus::ei_encode_error;
        if (buffer.reserve(index + strlen(str) + 1) == false)
            return GEPD::ExitStatus::write_overflow;
        if (ei_encode_string(buffer.get<char>(), &index, str))
            return GEPD::ExitStatus::ei_encode_error;
        return GEPD::ExitStatus::success;
    }

#define STORE_RETURN_VALUE(TYPE, CMD) \
    BOOST_PP_CAT(STORE_RETURN_VALUE_TYPE_, TYPE)(CMD)

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
        (TUPLE_TO_SEQ(GET_ARGC(FUNCTION), GET_ARGV(FUNCTION)))\
        (BOOST_PP_SEQ_ELEM(0, OFFSETS)) \
    ) \
    CREATE_FUNCTION_RETURN_VALUE_STORE(GET_RETURN(FUNCTION)) \
    GET_NAME(FUNCTION) \
    BOOST_PP_LPAREN() \
    BOOST_PP_ENUM( \
        GET_ARGC(FUNCTION), \
        CREATE_FUNCTION_ARGUMENTS, \
        (TUPLE_TO_SEQ(GET_ARGC(FUNCTION), GET_ARGV(FUNCTION)))\
        (BOOST_PP_SEQ_ELEM(0, OFFSETS)) \
    ) \
    BOOST_PP_RPAREN() \
    ; \
    int index = BOOST_PP_SEQ_ELEM(1, OFFSETS); \
    STORE_RETURN_VALUE(GET_RETURN(FUNCTION), BOOST_PP_DEC(I)) \
    if ((status = write_cmd(buffer, index - BOOST_PP_SEQ_ELEM(1, OFFSETS)))) \
        return status; \
    return GEPD::ExitStatus::success;\
}

    int consume_erlang(short & revents, realloc_ptr<unsigned char> & buffer)
    {
        if (revents & POLLERR)
            return GEPD::ExitStatus::poll_ERR;
        else if (revents & POLLHUP)
            return GEPD::ExitStatus::erlang_exit;
        else if (revents & POLLNVAL)
            return GEPD::ExitStatus::poll_NVAL;
        revents = 0;
        int status;
        if ((status = read_cmd(buffer)))
        {
            return status;
        }
        else
        {
            INPUT_PREFIX_TYPE cmd = *((INPUT_PREFIX_TYPE *) buffer.get());
            switch (cmd)
            {
                BOOST_PP_SEQ_FOR_EACH(CREATE_FUNCTION,
                                      (sizeof(INPUT_PREFIX_TYPE))
                                      (sizeof(OUTPUT_PREFIX_TYPE)),
                                      PORT_FUNCTIONS)
    
                default:
                    int index = sizeof(OUTPUT_PREFIX_TYPE);
                    if ((status = reply_error_string(buffer, index, cmd, 
                                                     Error::invalid_function)))
                        return status;
                    if ((status = write_cmd(buffer, index -
                                            sizeof(OUTPUT_PREFIX_TYPE))))
                        return status;
                    return GEPD::ExitStatus::success;
            }
        }
    }

    int store_standard_fd(int in, int & out)
    {
        int fds[2] = {-1, -1};
        if (pipe(fds) == -1)
            return errno_pipe();
        if (dup2(fds[1], in) == -1)
            return errno_dup();
        if (close(fds[1]) == -1)
            return errno_close();
        out = fds[0];
        return GEPD::ExitStatus::success;
    }

    int data_ready(int fd, bool & ready)
    {
        struct pollfd fds[1] = {{fd, POLLIN | POLLPRI, 0}};
        int const count = poll(fds, 1, 0);
        if (count == -1)
            return errno_poll();
        ready = (count == 1);
        return GEPD::ExitStatus::success;
    }

    enum
    {
        INDEX_STDOUT = 0,
        INDEX_STDERR,
        INDEX_ERLANG
    };
}

int GEPD::consume_stream(int fd, short & revents,
                         char const * const name, unsigned long const pid,
                         realloc_ptr<unsigned char> & send_buffer,
                         realloc_ptr<unsigned char> & stream, size_t & i)
{
    if (revents & POLLERR)
        return GEPD::ExitStatus::poll_ERR;
    else if (revents & POLLHUP)
        return GEPD::ExitStatus::poll_HUP;
    else if (revents & POLLNVAL)
        return GEPD::ExitStatus::poll_NVAL;
    revents = 0;

    ssize_t left = stream.size() - i;
    ssize_t readBytes;
    while ((readBytes = read(fd, &stream[i], left)) == left &&
           stream.grow())
    {
        i += left;
        left = stream.size() - i;
        bool ready = true;
        data_ready(fd, ready);
        if (ready == false)
            break;
    }
    if (readBytes == 0 && i == 0)
        return GEPD::ExitStatus::success;
    else if (readBytes == -1)
        return errno_read();
    i += readBytes; // i is the next index to read at, always

    // only send stderr output before the last newline character
    bool foundNewline = false;
    size_t iNewline = 0;
    for (ssize_t j = i - 1; ! foundNewline && j >= 0; --j)
    {
        if (stream[j] == '\n')
        {
            foundNewline = true;
            iNewline = j;
        }
    }

    if (foundNewline)
    {
        int index = sizeof(OUTPUT_PREFIX_TYPE);
        if (ei_encode_version(send_buffer.get<char>(), &index))
            return GEPD::ExitStatus::ei_encode_error;
        if (ei_encode_tuple_header(send_buffer.get<char>(), &index, 3))
            return GEPD::ExitStatus::ei_encode_error;
        if (ei_encode_atom(send_buffer.get<char>(), &index, name))
            return GEPD::ExitStatus::ei_encode_error;
        if (ei_encode_ulong(send_buffer.get<char>(), &index, pid))
            return GEPD::ExitStatus::ei_encode_error;
        if (send_buffer.reserve(index + (iNewline + 1) + 1) == false)
            return GEPD::ExitStatus::write_overflow;
        if (ei_encode_string_len(send_buffer.get<char>(), &index,
                                 stream.get<char>(), iNewline + 1))
            return GEPD::ExitStatus::ei_encode_error;
        int status;
        if ((status = write_cmd(send_buffer, index -
                                sizeof(OUTPUT_PREFIX_TYPE))))
            return status;
        // keep any data not yet sent (waiting for a newline)
        if (iNewline == i - 1)
        {
            i = 0;
        }
        else
        {
            size_t const remainingBytes = i - iNewline - 1;
            stream.move(iNewline + 1, remainingBytes, 0);
            i = remainingBytes;
        }
    }
    return GEPD::ExitStatus::success;
}

int GEPD::flush_stream(int fd, short revents,
                       char const * const name, unsigned long const pid,
                       realloc_ptr<unsigned char> & send_buffer,
                       realloc_ptr<unsigned char> & stream, size_t & i)
{
    if ((revents & POLLIN) == false)
        return GEPD::ExitStatus::success;

    ssize_t left = stream.size() - i;
    ssize_t readBytes;
    while ((readBytes = read(fd, &stream[i], left)) == left &&
           stream.grow())
    {
        i += left;
        left = stream.size() - i;
        bool ready = true;
        data_ready(fd, ready);
        if (ready == false)
            break;
    }
    if (readBytes == 0 && i == 0)
        return GEPD::ExitStatus::success;
    else if (readBytes != -1)
        i += readBytes; // i is the next index to read at, always

    size_t const total = i - 1;
    i = 0;

    int index = sizeof(OUTPUT_PREFIX_TYPE);
    if (ei_encode_version(send_buffer.get<char>(), &index))
        return GEPD::ExitStatus::ei_encode_error;
    if (ei_encode_tuple_header(send_buffer.get<char>(), &index, 3))
        return GEPD::ExitStatus::ei_encode_error;
    if (ei_encode_atom(send_buffer.get<char>(), &index, name))
        return GEPD::ExitStatus::ei_encode_error;
    if (ei_encode_ulong(send_buffer.get<char>(), &index, pid))
        return GEPD::ExitStatus::ei_encode_error;
    if (send_buffer.reserve(index + total + 1) == false)
        return GEPD::ExitStatus::write_overflow;
    if (ei_encode_string_len(send_buffer.get<char>(), &index,
                             stream.get<char>(), total))
        return GEPD::ExitStatus::ei_encode_error;
    int status;
    if ((status = write_cmd(send_buffer, index -
                            sizeof(OUTPUT_PREFIX_TYPE))))
        return status;

    return GEPD::ExitStatus::success;
}

realloc_ptr<struct pollfd> GEPD::fds(4, 65536);
nfds_t GEPD::nfds = 0;

// main loop for handling inherently synchronous function calls
// (a linked-in Erlang port driver that makes synchronous calls with
//  driver level locking should be similar to an Erlang port, except that
//  the port driver is a VM process and the port is an OS process)

int GEPD::default_main()
{
    int const timeout = -1; // milliseconds
    // use the option {packet, 4} for open_port/2
    // (limited by 4MB buffer size below)
    realloc_ptr<unsigned char> buffer(32768, 4194304);
    realloc_ptr<unsigned char> stream1(1, 16384);
    realloc_ptr<unsigned char> stream2(1, 16384);
    int status;
    if ((status = GEPD::init()))
        return status;
    int count;
    return GEPD::wait(count, timeout, buffer, stream1, stream2);
}

int GEPD::init()
{
    if (nfds > 0)
        fds.move(0, nfds, 3);

    int status;
    if ((status = store_standard_fd(1, fds[INDEX_STDOUT].fd)))
        return status;
    fds[INDEX_STDOUT].events = POLLIN | POLLPRI;
    fds[INDEX_STDOUT].revents = 0;
    if ((status = store_standard_fd(2, fds[INDEX_STDERR].fd)))
        return status;
    fds[INDEX_STDERR].events = POLLIN | POLLPRI;
    fds[INDEX_STDERR].revents = 0;
    fds[INDEX_ERLANG].fd = PORT_READ_FILE_DESCRIPTOR;
    fds[INDEX_ERLANG].events = POLLIN | POLLPRI;
    fds[INDEX_ERLANG].revents = 0;
    nfds += 3;
    return GEPD::ExitStatus::success;
}

int GEPD::wait(int & count, int const timeout,
               realloc_ptr<unsigned char> & buffer,
               realloc_ptr<unsigned char> & stream1,
               realloc_ptr<unsigned char> & stream2)
{
    static unsigned long const pid = getpid();
    static size_t index_stream1 = 0;
    static size_t index_stream2 = 0;
    while ((count = poll(fds.get(), nfds, timeout)) > 0)
    {
        int status;
        if (count > 0 && fds[INDEX_ERLANG].revents != 0)
        {
            if ((status = consume_erlang(fds[INDEX_ERLANG].revents, buffer)))
                return status;
            --count;
        }
        fflush(stdout);
        fflush(stderr);
        if (count > 0 && fds[INDEX_STDERR].revents != 0)
        {
            if ((status = consume_stream(fds[INDEX_STDERR].fd, 
                                         fds[INDEX_STDERR].revents,
                                         "stderr", pid, buffer,
                                         stream2, index_stream2)))
                return status;
            --count;
        }
        if (count > 0 && fds[INDEX_STDOUT].revents != 0)
        {
            if ((status = consume_stream(fds[INDEX_STDOUT].fd, 
                                         fds[INDEX_STDOUT].revents,
                                         "stdout", pid, buffer,
                                         stream1, index_stream1)))
                return status;
            --count;
        }
        if (count > 0)
            return GEPD::ExitStatus::ready;
    }
    if (count == 0)
        return GEPD::ExitStatus::timeout;
    else
        return errno_poll();
}

