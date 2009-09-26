// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
// ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab:

// GENERIC ERLANG PORT DRIVER VERSION 0.6
// automatically create efficient Erlang bindings to C++/C 

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
#include "port_driver.h"

#if defined(PORT_DRIVER_C_FUNCTIONS_HEADER_FILE)
extern "C"
{
#include PORT_DRIVER_C_FUNCTIONS_HEADER_FILE
}
#elif defined(PORT_DRIVER_CXX_FUNCTIONS_HEADER_FILE)
#include PORT_DRIVER_CXX_FUNCTIONS_HEADER_FILE
#else
#error Neither PORT_DRIVER_C_FUNCTIONS_HEADER_FILE nor \
       PORT_DRIVER_CXX_FUNCTIONS_HEADER_FILE are defined
#endif

// types available to generate bindings for
// (limited list to provide efficient bindings that do not copy the arguments
//  to a temporary (char *) buffer, only real types that are usable as both
//  argument types and return value types)
#include <time.h>
#define PORT_DRIVER_AVAILABLE_TYPES \
    (bool)(char)(int8_t)(uint8_t)\
    (int16_t)(uint16_t)\
    (int32_t)(uint32_t)(time_t)\
    (int64_t)(uint64_t)(double)
    // possible return value types include:    void, pchar, uchar, float
    // possible argument types include:        pchar_len, uchar

// macros that define type handling in the bindings for
// the function arguments and return value
// (adding to PORT_DRIVER_AVAILABLE_TYPES requires additions below)

// bool
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_bool(PREFIX) \
    BOOST_PP_CAT(PREFIX, _._bool)
#define CREATE_FUNCTION_INPUT_EV_STORE_TYPE_bool(ASYNC, PREFIX, ARG) \
    if (EV_GET_UINT8(ev, &(ARG), p, q))\
    {\
        BOOST_PP_IF(ASYNC, driver_free(c); , BOOST_PP_EMPTY())\
        reply_data_error_string(desc, Error::unable_to_decode_arguments);\
        return;\
    }
#define CREATE_FUNCTION_INPUT_PROCESS_TYPE_bool(PREFIX, ARG) \
    BOOST_PP_EMPTY()
#define CREATE_FUNCTION_OUTPUT_PROCESS_TYPE_bool(PREFIX, ARG) \
    BOOST_PP_EMPTY()
#define CREATE_INVOKE_FUNCTION_RETURN_VALUE_STORE_TYPE_bool(PREFIX) \
    BOOST_PP_CAT(PREFIX, bool) =
#define CREATE_FUNCTION_OUTPUT_RETURN_VALUE_TYPE_bool(PREFIX) \
    reply_data_boolean(desc, BOOST_PP_CAT(PREFIX, bool));
#define CREATE_INVOKE_FUNCTION_RETURN_VALUE_PROCESSING_CODE_TYPE_bool \
    BOOST_PP_EMPTY()

// char
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_char(PREFIX) \
    BOOST_PP_CAT(PREFIX, _._char)
#define CREATE_FUNCTION_INPUT_EV_STORE_TYPE_char(ASYNC, PREFIX, ARG) \
    if (EV_GET_UINT8(ev, &(ARG), p, q))\
    {\
        BOOST_PP_IF(ASYNC, driver_free(c); , BOOST_PP_EMPTY())\
        reply_data_error_string(desc, Error::unable_to_decode_arguments);\
        return;\
    }
#define CREATE_FUNCTION_INPUT_PROCESS_TYPE_char(PREFIX, ARG) \
    BOOST_PP_EMPTY()
#define CREATE_FUNCTION_OUTPUT_PROCESS_TYPE_char(PREFIX, ARG) \
    BOOST_PP_EMPTY()
#define CREATE_INVOKE_FUNCTION_RETURN_VALUE_STORE_TYPE_char(PREFIX) \
    BOOST_PP_CAT(PREFIX, char) =
#define CREATE_FUNCTION_OUTPUT_RETURN_VALUE_TYPE_char(PREFIX) \
    reply_data_integer(desc, BOOST_PP_CAT(PREFIX, char));
#define CREATE_INVOKE_FUNCTION_RETURN_VALUE_PROCESSING_CODE_TYPE_char \
    BOOST_PP_EMPTY()

// uchar, unsigned char
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_uchar(PREFIX) \
    BOOST_PP_CAT(PREFIX, _._uchar)
#define CREATE_FUNCTION_INPUT_EV_STORE_TYPE_uchar(ASYNC, PREFIX, ARG) \
    if (EV_GET_UINT8(ev, &(ARG), p, q))\
    {\
        BOOST_PP_IF(ASYNC, driver_free(c); , BOOST_PP_EMPTY())\
        reply_data_error_string(desc, Error::unable_to_decode_arguments);\
        return;\
    }
#define CREATE_FUNCTION_INPUT_PROCESS_TYPE_uchar(PREFIX, ARG) \
    BOOST_PP_EMPTY()
#define CREATE_FUNCTION_OUTPUT_PROCESS_TYPE_uchar(PREFIX, ARG) \
    BOOST_PP_EMPTY()
#define CREATE_INVOKE_FUNCTION_RETURN_VALUE_STORE_TYPE_uchar(PREFIX) \
    BOOST_PP_CAT(PREFIX, uchar) =
#define CREATE_FUNCTION_OUTPUT_RETURN_VALUE_TYPE_uchar(PREFIX) \
    reply_data_integer(desc, BOOST_PP_CAT(PREFIX, uchar));
#define CREATE_INVOKE_FUNCTION_RETURN_VALUE_PROCESSING_CODE_TYPE_uchar \
    BOOST_PP_EMPTY()

// int8_t
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_int8_t(PREFIX) \
    BOOST_PP_CAT(PREFIX, _._int8_t)
#define CREATE_FUNCTION_INPUT_EV_STORE_TYPE_int8_t(ASYNC, PREFIX, ARG) \
    if (EV_GET_UINT8(ev, &(ARG), p, q))\
    {\
        BOOST_PP_IF(ASYNC, driver_free(c); , BOOST_PP_EMPTY())\
        reply_data_error_string(desc, Error::unable_to_decode_arguments);\
        return;\
    }
#define CREATE_FUNCTION_INPUT_PROCESS_TYPE_int8_t(PREFIX, ARG) \
    BOOST_PP_EMPTY()
#define CREATE_FUNCTION_OUTPUT_PROCESS_TYPE_int8_t(PREFIX, ARG) \
    BOOST_PP_EMPTY()
#define CREATE_INVOKE_FUNCTION_RETURN_VALUE_STORE_TYPE_int8_t(PREFIX) \
    BOOST_PP_CAT(PREFIX, int8_t) =
#define CREATE_FUNCTION_OUTPUT_RETURN_VALUE_TYPE_int8_t(PREFIX) \
    reply_data_integer(desc, BOOST_PP_CAT(PREFIX, int8_t));
#define CREATE_INVOKE_FUNCTION_RETURN_VALUE_PROCESSING_CODE_TYPE_int8_t \
    BOOST_PP_EMPTY()

// uint8_t
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_uint8_t(PREFIX) \
    BOOST_PP_CAT(PREFIX, _._uint8_t)
#define CREATE_FUNCTION_INPUT_EV_STORE_TYPE_uint8_t(ASYNC, PREFIX, ARG) \
    if (EV_GET_UINT8(ev, &(ARG), p, q))\
    {\
        BOOST_PP_IF(ASYNC, driver_free(c); , BOOST_PP_EMPTY())\
        reply_data_error_string(desc, Error::unable_to_decode_arguments);\
        return;\
    }
#define CREATE_FUNCTION_INPUT_PROCESS_TYPE_uint8_t(PREFIX, ARG) \
    BOOST_PP_EMPTY()
#define CREATE_FUNCTION_OUTPUT_PROCESS_TYPE_uint8_t(PREFIX, ARG) \
    BOOST_PP_EMPTY()
#define CREATE_INVOKE_FUNCTION_RETURN_VALUE_STORE_TYPE_uint8_t(PREFIX) \
    BOOST_PP_CAT(PREFIX, uint8_t) =
#define CREATE_FUNCTION_OUTPUT_RETURN_VALUE_TYPE_uint8_t(PREFIX) \
    reply_data_integer(desc, BOOST_PP_CAT(PREFIX, uint8_t));
#define CREATE_INVOKE_FUNCTION_RETURN_VALUE_PROCESSING_CODE_TYPE_uint8_t \
    BOOST_PP_EMPTY()

// int16_t
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_int16_t(PREFIX) \
    BOOST_PP_CAT(PREFIX, _._int16_t)
#define CREATE_FUNCTION_INPUT_EV_STORE_TYPE_int16_t(ASYNC, PREFIX, ARG) \
    if (EV_GET_UINT16(ev, &(ARG), p, q))\
    {\
        BOOST_PP_IF(ASYNC, driver_free(c); , BOOST_PP_EMPTY())\
        reply_data_error_string(desc, Error::unable_to_decode_arguments);\
        return;\
    }
#define CREATE_FUNCTION_INPUT_PROCESS_TYPE_int16_t(PREFIX, ARG) \
    BOOST_PP_EMPTY()
#define CREATE_FUNCTION_OUTPUT_PROCESS_TYPE_int16_t(PREFIX, ARG) \
    BOOST_PP_EMPTY()
#define CREATE_INVOKE_FUNCTION_RETURN_VALUE_STORE_TYPE_int16_t(PREFIX) \
    BOOST_PP_CAT(PREFIX, int16_t) =
#define CREATE_FUNCTION_OUTPUT_RETURN_VALUE_TYPE_int16_t(PREFIX) \
    reply_data_integer(desc, BOOST_PP_CAT(PREFIX, int16_t));
#define CREATE_INVOKE_FUNCTION_RETURN_VALUE_PROCESSING_CODE_TYPE_int16_t \
    BOOST_PP_EMPTY()

// uint16_t
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_uint16_t(PREFIX) \
    BOOST_PP_CAT(PREFIX, _._uint16_t)
#define CREATE_FUNCTION_INPUT_EV_STORE_TYPE_uint16_t(ASYNC, PREFIX, ARG) \
    if (EV_GET_UINT16(ev, &(ARG), p, q))\
    {\
        BOOST_PP_IF(ASYNC, driver_free(c); , BOOST_PP_EMPTY())\
        reply_data_error_string(desc, Error::unable_to_decode_arguments);\
        return;\
    }
#define CREATE_FUNCTION_INPUT_PROCESS_TYPE_uint16_t(PREFIX, ARG) \
    BOOST_PP_EMPTY()
#define CREATE_FUNCTION_OUTPUT_PROCESS_TYPE_uint16_t(PREFIX, ARG) \
    BOOST_PP_EMPTY()
#define CREATE_INVOKE_FUNCTION_RETURN_VALUE_STORE_TYPE_uint16_t(PREFIX) \
    BOOST_PP_CAT(PREFIX, uint16_t) =
#define CREATE_FUNCTION_OUTPUT_RETURN_VALUE_TYPE_uint16_t(PREFIX) \
    reply_data_integer(desc, BOOST_PP_CAT(PREFIX, uint16_t));
#define CREATE_INVOKE_FUNCTION_RETURN_VALUE_PROCESSING_CODE_TYPE_uint16_t \
    BOOST_PP_EMPTY()

// int32_t
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_int32_t(PREFIX) \
    BOOST_PP_CAT(PREFIX, _._int32_t)
#define CREATE_FUNCTION_INPUT_EV_STORE_TYPE_int32_t(ASYNC, PREFIX, ARG) \
    if (EV_GET_UINT32(ev, &(ARG), p, q))\
    {\
        BOOST_PP_IF(ASYNC, driver_free(c); , BOOST_PP_EMPTY())\
        reply_data_error_string(desc, Error::unable_to_decode_arguments);\
        return;\
    }
#define CREATE_FUNCTION_INPUT_PROCESS_TYPE_int32_t(PREFIX, ARG) \
    BOOST_PP_EMPTY()
#define CREATE_FUNCTION_OUTPUT_PROCESS_TYPE_int32_t(PREFIX, ARG) \
    BOOST_PP_EMPTY()
#define CREATE_INVOKE_FUNCTION_RETURN_VALUE_STORE_TYPE_int32_t(PREFIX) \
    BOOST_PP_CAT(PREFIX, int32_t) =
#define CREATE_FUNCTION_OUTPUT_RETURN_VALUE_TYPE_int32_t(PREFIX) \
    reply_data_integer(desc, BOOST_PP_CAT(PREFIX, int32_t));
#define CREATE_INVOKE_FUNCTION_RETURN_VALUE_PROCESSING_CODE_TYPE_int32_t \
    BOOST_PP_EMPTY()

// uint32_t
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_uint32_t(PREFIX) \
    BOOST_PP_CAT(PREFIX, _._uint32_t)
#define CREATE_FUNCTION_INPUT_EV_STORE_TYPE_uint32_t(ASYNC, PREFIX, ARG) \
    if (EV_GET_UINT32(ev, &(ARG), p, q))\
    {\
        BOOST_PP_IF(ASYNC, driver_free(c); , BOOST_PP_EMPTY())\
        reply_data_error_string(desc, Error::unable_to_decode_arguments);\
        return;\
    }
#define CREATE_FUNCTION_INPUT_PROCESS_TYPE_uint32_t(PREFIX, ARG) \
    BOOST_PP_EMPTY()
#define CREATE_FUNCTION_OUTPUT_PROCESS_TYPE_uint32_t(PREFIX, ARG) \
    BOOST_PP_EMPTY()
#define CREATE_INVOKE_FUNCTION_RETURN_VALUE_STORE_TYPE_uint32_t(PREFIX) \
    BOOST_PP_CAT(PREFIX, uint32_t) =
#define CREATE_FUNCTION_OUTPUT_RETURN_VALUE_TYPE_uint32_t(PREFIX) \
    reply_data_integer(desc, BOOST_PP_CAT(PREFIX, uint32_t));
#define CREATE_INVOKE_FUNCTION_RETURN_VALUE_PROCESSING_CODE_TYPE_uint32_t \
    BOOST_PP_EMPTY()

// time_t, from time.h
// (can be 64bit, but treated as if it is always 32bit, so initialize to 0)
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_time_t(PREFIX) \
    BOOST_PP_CAT(PREFIX, _._time_t)
#define CREATE_FUNCTION_INPUT_EV_STORE_TYPE_time_t(ASYNC, PREFIX, ARG) \
    ARG = 0; \
    if (EV_GET_UINT32(ev, &(ARG), p, q))  \
    {\
        BOOST_PP_IF(ASYNC, driver_free(c); , BOOST_PP_EMPTY())\
        reply_data_error_string(desc, Error::unable_to_decode_arguments);\
        return;\
    }
#define CREATE_FUNCTION_INPUT_PROCESS_TYPE_time_t(PREFIX, ARG) \
    BOOST_PP_EMPTY()
#define CREATE_FUNCTION_OUTPUT_PROCESS_TYPE_time_t(PREFIX, ARG) \
    BOOST_PP_EMPTY()
#define CREATE_INVOKE_FUNCTION_RETURN_VALUE_STORE_TYPE_time_t(PREFIX) \
    BOOST_PP_CAT(PREFIX, time_t) =
#define CREATE_FUNCTION_OUTPUT_RETURN_VALUE_TYPE_time_t(PREFIX) \
    reply_data_integer(desc, BOOST_PP_CAT(PREFIX, time_t));
#define CREATE_INVOKE_FUNCTION_RETURN_VALUE_PROCESSING_CODE_TYPE_time_t \
    BOOST_PP_EMPTY()

// int64_t
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_int64_t(PREFIX) \
    BOOST_PP_CAT(PREFIX, _._int64_t)
#define CREATE_FUNCTION_INPUT_EV_STORE_TYPE_int64_t(ASYNC, PREFIX, ARG) \
    if (EV_GET_UINT64(ev, &(ARG), p, q))\
    {\
        BOOST_PP_IF(ASYNC, driver_free(c); , BOOST_PP_EMPTY())\
        reply_data_error_string(desc, Error::unable_to_decode_arguments);\
        return;\
    }
#define CREATE_FUNCTION_INPUT_PROCESS_TYPE_int64_t(PREFIX, ARG) \
    BOOST_PP_EMPTY()
#define CREATE_FUNCTION_OUTPUT_PROCESS_TYPE_int64_t(PREFIX, ARG) \
    BOOST_PP_EMPTY()
#if NATIVE_64BIT_TYPES
#define CREATE_INVOKE_FUNCTION_RETURN_VALUE_STORE_TYPE_int64_t(PREFIX) \
    BOOST_PP_CAT(PREFIX, int64_t) =
#define CREATE_FUNCTION_OUTPUT_RETURN_VALUE_TYPE_int64_t(PREFIX) \
    reply_data_integer(desc, BOOST_PP_CAT(PREFIX, int64_t));
#define CREATE_INVOKE_FUNCTION_RETURN_VALUE_PROCESSING_CODE_TYPE_int64_t \
    BOOST_PP_EMPTY()
#endif

// uint64_t
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_uint64_t(PREFIX) \
    BOOST_PP_CAT(PREFIX, _._uint64_t)
#define CREATE_FUNCTION_INPUT_EV_STORE_TYPE_uint64_t(ASYNC, PREFIX, ARG) \
    if (EV_GET_UINT64(ev, &(ARG), p, q))\
    {\
        BOOST_PP_IF(ASYNC, driver_free(c); , BOOST_PP_EMPTY())\
        reply_data_error_string(desc, Error::unable_to_decode_arguments);\
        return;\
    }
#define CREATE_FUNCTION_INPUT_PROCESS_TYPE_uint64_t(PREFIX, ARG) \
    BOOST_PP_EMPTY()
#define CREATE_FUNCTION_OUTPUT_PROCESS_TYPE_uint64_t(PREFIX, ARG) \
    BOOST_PP_EMPTY()
#if NATIVE_64BIT_TYPES
#define CREATE_INVOKE_FUNCTION_RETURN_VALUE_STORE_TYPE_uint64_t(PREFIX) \
    BOOST_PP_CAT(PREFIX, uint64_t) =
#define CREATE_FUNCTION_OUTPUT_RETURN_VALUE_TYPE_uint64_t(PREFIX) \
    reply_data_integer(desc, BOOST_PP_CAT(PREFIX, uint64_t));
#define CREATE_INVOKE_FUNCTION_RETURN_VALUE_PROCESSING_CODE_TYPE_uint64_t \
    BOOST_PP_EMPTY()
#endif

// double
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_double(PREFIX) \
    BOOST_PP_CAT(PREFIX, _._double)
#define CREATE_FUNCTION_INPUT_EV_STORE_TYPE_double(ASYNC, PREFIX, ARG) \
    if (EV_GET_UINT64(ev, &(ARG), p, q))\
    {\
        BOOST_PP_IF(ASYNC, driver_free(c); , BOOST_PP_EMPTY())\
        reply_data_error_string(desc, Error::unable_to_decode_arguments);\
        return;\
    }
#define CREATE_FUNCTION_INPUT_PROCESS_TYPE_double(PREFIX, ARG) \
    BOOST_PP_EMPTY()
#define CREATE_FUNCTION_OUTPUT_PROCESS_TYPE_double(PREFIX, ARG) \
    BOOST_PP_EMPTY()
#define CREATE_INVOKE_FUNCTION_RETURN_VALUE_STORE_TYPE_double(PREFIX) \
    BOOST_PP_CAT(PREFIX, double) =
#define CREATE_FUNCTION_OUTPUT_RETURN_VALUE_TYPE_double(PREFIX) \
    reply_data_double(desc, BOOST_PP_CAT(PREFIX, double));
#define CREATE_INVOKE_FUNCTION_RETURN_VALUE_PROCESSING_CODE_TYPE_double \
    BOOST_PP_EMPTY()

// pchar_len, (char *, length) handled as one parameter
// (does not copy the Erlang data, but uses the reference count)
#define GET_FUNCTION_ARGUMENT_FROM_TYPE_pchar_len(PREFIX) \
    BOOST_PP_CAT(PREFIX, _._bin.ptr._char) \
    BOOST_PP_COMMA() \
    BOOST_PP_CAT(PREFIX, _._bin.length)
#define CREATE_FUNCTION_INPUT_EV_STORE_TYPE_pchar_len(ASYNC, PREFIX, PTR, LEN) \
    if (EV_GET_UINT32(ev, &(LEN), p, q)) \
    {\
        BOOST_PP_IF(ASYNC, driver_free(c); , BOOST_PP_EMPTY())\
        reply_data_error_string(desc, Error::unable_to_decode_arguments);\
        return;\
    }\
    BOOST_PP_CAT(PREFIX, _._bin.ptr._void) = EV_GETPOS(ev, p, q);\
    BOOST_PP_CAT(PREFIX, _._bin.ref) = ev->binv[q]; \
    if (ev_incr(ev, LEN, p, q) < 0) \
    {\
        BOOST_PP_IF(ASYNC, driver_free(c); , BOOST_PP_EMPTY())\
        reply_data_error_string(desc, Error::unable_to_decode_arguments);\
        return;\
    }
#define CREATE_FUNCTION_INPUT_PROCESS_TYPE_pchar_len(PREFIX, PTR, LEN) \
    driver_binary_inc_refc(BOOST_PP_CAT(PREFIX, _._bin.ref));
#define CREATE_FUNCTION_OUTPUT_PROCESS_TYPE_pchar_len(PREFIX, PTR, LEN) \
    driver_binary_dec_refc(BOOST_PP_CAT(PREFIX, _._bin.ref));
#define CREATE_INVOKE_FUNCTION_RETURN_VALUE_STORE_TYPE_pchar(PREFIX) \
    BOOST_PP_CAT(PREFIX, bin.ptr._char) =
#define CREATE_INVOKE_FUNCTION_RETURN_VALUE_PROCESSING_CODE_TYPE_pchar \
    c->o._bin.length = strlen(c->o._bin.ptr._char);
#define CREATE_FUNCTION_OUTPUT_RETURN_VALUE_TYPE_pchar(PREFIX) \
    reply_data_string(\
        desc, \
        BOOST_PP_CAT(PREFIX, bin.ptr._char),\
        BOOST_PP_CAT(PREFIX, bin.length)\
    );

// float
#define CREATE_INVOKE_FUNCTION_RETURN_VALUE_STORE_TYPE_float(PREFIX) \
    BOOST_PP_CAT(PREFIX, float) =
#define CREATE_FUNCTION_OUTPUT_RETURN_VALUE_TYPE_float(PREFIX) \
    reply_data_double(desc, BOOST_PP_CAT(PREFIX, float));
#define CREATE_INVOKE_FUNCTION_RETURN_VALUE_PROCESSING_CODE_TYPE_float \
    BOOST_PP_EMPTY()

// void, return value handling
#define CREATE_INVOKE_FUNCTION_RETURN_VALUE_STORE_TYPE_void(PREFIX) \
    BOOST_PP_EMPTY()
#define CREATE_INVOKE_FUNCTION_RETURN_VALUE_PROCESSING_CODE_TYPE_void \
    BOOST_PP_EMPTY()
#define CREATE_FUNCTION_OUTPUT_RETURN_VALUE_TYPE_void(PREFIX) \
    reply_data_ok(desc);

//////////////////////////////////////////////////////////////////////////////
// preprocessing macros to generate function specific bindings code
//////////////////////////////////////////////////////////////////////////////

#include <erl_driver.h>
#include <stdint.h>
#include <errno.h>
#include <cstring>

#include <boost/preprocessor/tuple/elem.hpp>
#include <boost/preprocessor/seq/for_each.hpp>
#include <boost/preprocessor/tuple/to_seq.hpp>
#include <boost/preprocessor/cat.hpp>
#include <boost/preprocessor/repetition/enum.hpp>
#include <boost/preprocessor/punctuation/comma.hpp>
#include <boost/preprocessor/control/if.hpp>
#include <boost/preprocessor/facilities/empty.hpp>
#include <boost/preprocessor/comparison/equal.hpp>
#include <boost/preprocessor/punctuation/paren.hpp>
#include <boost/preprocessor/seq/for_each_i.hpp>
#include <boost/preprocessor/facilities/expand.hpp>
#include <boost/preprocessor/repetition/repeat_from_to.hpp>
#include <boost/preprocessor/seq/size.hpp>
#include <boost/preprocessor/stringize.hpp>

// create the local invocations of the port driver functions

#define CREATE_INVOKE_FUNCTION_RETURN_VALUE_STORE(TYPE, PREFIX) \
    BOOST_PP_CAT(\
        CREATE_INVOKE_FUNCTION_RETURN_VALUE_STORE_TYPE_, TYPE\
    )(PREFIX)

#define CREATE_INVOKE_FUNCTION_RETURN_VALUE_PROCESSING_CODE(TYPE) \
    BOOST_PP_CAT(\
        CREATE_INVOKE_FUNCTION_RETURN_VALUE_PROCESSING_CODE_TYPE_, TYPE\
    )

#define GET_FUNCTION_ARGUMENT(TYPE, PREFIX) \
    BOOST_PP_CAT(\
        GET_FUNCTION_ARGUMENT_FROM_TYPE_, TYPE\
    )(PREFIX)

#define CREATE_INVOKE_FUNCTION_ARGUMENTS(Z, N, ARGUMENTS) \
    GET_FUNCTION_ARGUMENT(\
        BOOST_PP_SEQ_ELEM(N, ARGUMENTS),\
        BOOST_PP_CAT(c->i.arg, N)\
    )

#define CREATE_INVOKE_FUNCTION(I, DATA, FUNCTION) \
static void BOOST_PP_CAT(invoke_, GET_NAME(FUNCTION)) (void * data) \
{ \
    callstate_t *c = (callstate_t *) data; \
    CREATE_INVOKE_FUNCTION_RETURN_VALUE_STORE(GET_RETURN(FUNCTION), c->o._) \
    GET_NAME(FUNCTION) \
    BOOST_PP_LPAREN() \
    BOOST_PP_ENUM( \
        GET_ARGC(FUNCTION), \
        CREATE_INVOKE_FUNCTION_ARGUMENTS, \
        BOOST_PP_TUPLE_TO_SEQ(GET_ARGC(FUNCTION), GET_ARGV(FUNCTION)) \
    ) \
    BOOST_PP_RPAREN() \
    ; \
    CREATE_INVOKE_FUNCTION_RETURN_VALUE_PROCESSING_CODE(GET_RETURN(FUNCTION))\
}

// create the case statements for each function for the initial call
// (Erlang -> C++/C)

#define CREATE_FUNCTION_INPUT_EV_STORE(TYPE, ARGUMENT) \
    BOOST_PP_EXPAND( \
        CREATE_FUNCTION_INPUT_EV_STORE_TYPE_ ## TYPE ARGUMENT \
    )

#define CREATE_FUNCTION_INPUT_ARGUMENT_HANDLING(R, FUNCTION, I, TYPE) \
    CREATE_FUNCTION_INPUT_EV_STORE(\
        TYPE, (\
            GET_ASYNC(FUNCTION),\
            BOOST_PP_CAT(c->i.arg, I),\
            GET_FUNCTION_ARGUMENT(TYPE, BOOST_PP_CAT(c->i.arg, I))\
        )\
    )

#define CREATE_FUNCTION_INPUT_PROCESS(TYPE, ARGUMENT) \
    BOOST_PP_EXPAND( \
        CREATE_FUNCTION_INPUT_PROCESS_TYPE_ ## TYPE ARGUMENT \
    )

#define CREATE_FUNCTION_INPUT_ARGUMENT_PROCESSING(R, DATA, I, TYPE) \
    CREATE_FUNCTION_INPUT_PROCESS(\
        TYPE, (\
            BOOST_PP_CAT(c->i.arg, I),\
            GET_FUNCTION_ARGUMENT(TYPE, BOOST_PP_CAT(c->i.arg, I))\
        )\
    )

#define CREATE_FUNCTION_INPUT_CASE(I, DATA, FUNCTION) \
case BOOST_PP_DEC(I):\
{\
    BOOST_PP_IF(\
        GET_ASYNC(FUNCTION),\
        callstate_t * c = \
            reinterpret_cast<callstate_t *>(driver_alloc(sizeof(callstate_t)));\
        if (! c)\
        {\
           driver_failure_posix(desc->port, ENOMEM); \
           return;\
        } ,\
        callstate_t sync_call; \
        callstate_t * c = &sync_call;\
    )\
    c->desc = desc;\
    c->cmd = cmd;\
    c->invoke = BOOST_PP_CAT(invoke_, GET_NAME(FUNCTION)); \
    BOOST_PP_SEQ_FOR_EACH_I(\
        CREATE_FUNCTION_INPUT_ARGUMENT_HANDLING, FUNCTION,\
        BOOST_PP_TUPLE_TO_SEQ(GET_ARGC(FUNCTION), GET_ARGV(FUNCTION))\
    )\
    BOOST_PP_SEQ_FOR_EACH_I(\
        CREATE_FUNCTION_INPUT_ARGUMENT_PROCESSING, _,\
        BOOST_PP_TUPLE_TO_SEQ(GET_ARGC(FUNCTION), GET_ARGV(FUNCTION))\
    )\
    BOOST_PP_IF(\
        GET_ASYNC(FUNCTION),\
        driver_async(desc->port, 0, c->invoke, c, driver_free);,\
        (*(c->invoke))(reinterpret_cast<void *>(c));\
        driver_entry_ready_async(\
            reinterpret_cast<ErlDrvData>(desc),\
            reinterpret_cast<ErlDrvThreadData>(c)\
        );\
    )\
}\
return;

// create the case statements for each function's return value
// (C++/C -> Erlang)

#define CREATE_FUNCTION_OUTPUT_RETURN_VALUE(TYPE, PREFIX) \
    BOOST_PP_CAT(CREATE_FUNCTION_OUTPUT_RETURN_VALUE_TYPE_, TYPE)(PREFIX)


#define CREATE_FUNCTION_OUTPUT_PROCESS(TYPE, ARGUMENT) \
    BOOST_PP_EXPAND( \
        CREATE_FUNCTION_OUTPUT_PROCESS_TYPE_ ## TYPE ARGUMENT \
    )

#define CREATE_FUNCTION_OUTPUT_ARGUMENT_PROCESSING(R, DATA, I, TYPE) \
    CREATE_FUNCTION_OUTPUT_PROCESS(\
        TYPE, (\
            BOOST_PP_CAT(c->i.arg, I),\
            GET_FUNCTION_ARGUMENT(TYPE, BOOST_PP_CAT(c->i.arg, I))\
        )\
    )

#define CREATE_FUNCTION_OUTPUT_CASE(I, DATA, FUNCTION) \
case BOOST_PP_DEC(I):\
    CREATE_FUNCTION_OUTPUT_RETURN_VALUE(GET_RETURN(FUNCTION), c->o._) \
    BOOST_PP_SEQ_FOR_EACH_I(\
        CREATE_FUNCTION_OUTPUT_ARGUMENT_PROCESSING, _,\
        BOOST_PP_TUPLE_TO_SEQ(GET_ARGC(FUNCTION), GET_ARGV(FUNCTION))\
    )\
    BOOST_PP_IF(GET_ASYNC(FUNCTION), driver_free(c); , BOOST_PP_EMPTY())\
    return;

//////////////////////////////////////////////////////////////////////////////
// code to handle access to ErlIOVec
//////////////////////////////////////////////////////////////////////////////

// macros based on erts/emulator/drivers/common/efile_drv.c

// char *EV_CHAR_P(ErlIOVec *ev, char p, int q)
#define EV_CHAR_P(ev, p, q)                                           \
    (((char *)(ev)->iov[(q)].iov_base) + (p))

// char *EV_UCHAR_P(ErlIOVec *ev, unsigned char p, int q)
#define EV_UCHAR_P(ev, p, q)                                          \
    (((unsigned char *)(ev)->iov[(q)].iov_base) + (p))

// int EV_GET_CHAR(ErlIOVec *ev, char *p, size_t &p, size_t &q)
#define EV_GET_CHAR(ev, ptr, p, q)                                    \
    (p + 1 <= (ev)->iov[q].iov_len                              \
     ? (*(ptr) = *EV_CHAR_P(ev, p, q),                          \
             p =   (p + 1 < (ev)->iov[q].iov_len               \
                 ?  p + 1                                          \
                 : (q++, 0)),                                   \
        0)                                                           \
     : !0)

#define EV_GET_UINT8(ev, p, pp, qp) EV_GET_CHAR(ev, p, pp, qp)

// int EV_GET_UINT16(ErlIOVec *ev, uint16_t *p, size_t &p, size_t &q)
#define EV_GET_UINT16(ev, ptr, p, q)                                  \
    (p + 2 <= (ev)->iov[q].iov_len                              \
     ? (*((uint16_t *) ptr) = (*((uint16_t *) EV_UCHAR_P(ev, p, q))),   \
             p =   (p + 2 < (ev)->iov[q].iov_len               \
                 ?  p + 2                                          \
                 : (q++, 0)),                                   \
        0)                                                           \
     : !0)

// int EV_GET_UINT32(ErlIOVec *ev, uint32_t *p, size_t &p, size_t &q)
#define EV_GET_UINT32(ev, ptr, p, q)                                  \
    (p + 4 <= (ev)->iov[q].iov_len                              \
     ? (*((uint32_t *) ptr) = (*((uint32_t *) EV_UCHAR_P(ev, p, q))),   \
             p =   (p + 4 < (ev)->iov[q].iov_len               \
                 ?  p + 4                                          \
                 : (q++, 0)),                                   \
        0) \
     : !0)

// int EV_GET_UINT64(ErlIOVec *ev, uint64_t *p, size_t &p, size_t &q)
#define EV_GET_UINT64(ev, ptr, p, q)                                  \
    (p + 8 <= (ev)->iov[q].iov_len                              \
     ? (*((uint64_t *) ptr) = (*((uint64_t *) EV_UCHAR_P(ev, p, q))),   \
             p =   (p + 8 < (ev)->iov[q].iov_len               \
                 ?  p + 8                                          \
                 : (q++, 0)),                                   \
        0) \
     : !0)

// void * EV_GETPOS(ErlIOVec *ev, size_t &p, size_t &q)
#define EV_GETPOS(ev, p, q)                                           \
    ((q) < (ev)->vsize                                                \
    ? ((ev)->iov[(q)].iov_base + p)                                   \
    : 0)

/// increment the position within the ErlIOVec by the size n
/// 
/// @return -1 on error, 0 no more data, 1 more data
static int ev_incr(ErlIOVec *ev, int n, size_t & p, size_t & q)
{
    const int pos = p + n;

    if (q >= ev->vsize)
        return -1;

    if (pos < ev->iov[q].iov_len)
    {
        p += n;
        return 1;
    }
    else if (pos == ev->iov[q].iov_len)
    {
        q++;
        p = 0;
        if (q < ev->vsize)
            return 1;
        else
            return 0;
    }
    else
    {
        return -1;
    }
}

//////////////////////////////////////////////////////////////////////////////
// data structures for managing driver data and function call data
//////////////////////////////////////////////////////////////////////////////

// port instance of driver
typedef struct
{
    ErlDrvPort          port;
    ErlDrvTermData      port_term;
    ErlDrvMutex        *driver_output_term_lock;
} descriptor_t;

// port driver function call state
#define CREATE_COMMON_TYPE_ENTRIES(I, DATA, TYPE) TYPE BOOST_PP_CAT(_, TYPE) ;
typedef struct
{
    descriptor_t        *desc;
    uint16_t             cmd;
    void                (*invoke)(void *);

    // function input parameters
    struct
    {
#define CREATE_CALLSTATE_ARGUMENT(Z, X, DATA)                                \
        union                                                                \
        {                                                                    \
           BOOST_PP_SEQ_FOR_EACH(CREATE_COMMON_TYPE_ENTRIES, _,              \
                                 PORT_DRIVER_AVAILABLE_TYPES)                \
           unsigned char _uchar;                                             \
           struct                                                            \
           {                                                                 \
               uint32_t length;                                              \
               union                                                         \
               {                                                             \
                   void * _void;                                             \
                   char * _char;                                             \
               } ptr;                                                        \
               ErlDrvBinary *ref;                                            \
           } _bin;                                                           \
        } arg##X##_;

        BOOST_PP_REPEAT_FROM_TO(0, PORT_DRIVER_FUNCTIONS_MAXIMUM_ARGUMENTS,
                                CREATE_CALLSTATE_ARGUMENT, _) 

    } i;

    // function output return value
    union {
        BOOST_PP_SEQ_FOR_EACH(CREATE_COMMON_TYPE_ENTRIES, _, 
                              PORT_DRIVER_AVAILABLE_TYPES)    
        unsigned char _uchar;
        float _float;
        struct
        {
            uint32_t length;
            union
            {
               char * _char;
               void * _void;
            } ptr;
        } _bin;
    } o;

} callstate_t;

//////////////////////////////////////////////////////////////////////////////
// reply handling
//////////////////////////////////////////////////////////////////////////////

static ErlDrvTermData const atom_value_ok =
    driver_mk_atom(const_cast<char *>("ok"));
static ErlDrvTermData const atom_value_error =
    driver_mk_atom(const_cast<char *>("error"));
static ErlDrvTermData const atom_value_data =
    driver_mk_atom(const_cast<char *>("data"));
static ErlDrvTermData const atom_value_true =
    driver_mk_atom(const_cast<char *>("true"));
static ErlDrvTermData const atom_value_false =
    driver_mk_atom(const_cast<char *>("false"));

static int driver_output_term_locked(ErlDrvMutex * mutex, 
                                     ErlDrvPort port,
                                     ErlDrvTermData * term,
                                     int n)
{
    // Regarding driver_output_term() in R12B-5:
    // "Note that this function is not thread-safe, 
    //  not even when the emulator with SMP support is used."
    // (http://erlang.org/doc/man/erl_driver.html#driver_output_term)
    
    // since asynchronous thread pool threads could call this function,
    // in addition to a callback emulator thread, unstable behavior
    // could occur without a mutex lock
    erl_drv_mutex_lock(mutex);
    int returnValue = driver_output_term(port, term, n);
    erl_drv_mutex_unlock(mutex);
    return returnValue;
}

static int reply_data_ok(descriptor_t *desc)
{
    ErlDrvTermData spec[] = {
        ERL_DRV_PORT, desc->port_term,
        ERL_DRV_ATOM, atom_value_data,
        ERL_DRV_ATOM, atom_value_ok,
        ERL_DRV_TUPLE, 2,
        ERL_DRV_TUPLE, 2
    };
    return driver_output_term_locked(desc->driver_output_term_lock, desc->port, 
                                     spec, sizeof(spec) / sizeof(spec[0]));
}

#define CREATE_REPLY_OK_INTEGER(TYPE, ERLTYPE)                               \
static int reply_data_integer(descriptor_t *desc, TYPE number)               \
{                                                                            \
    ErlDrvTermData spec[] = {                                                \
        ERL_DRV_PORT, desc->port_term,                                       \
        ERL_DRV_ATOM, atom_value_data,                                       \
        ERLTYPE, number,                                                     \
        ERL_DRV_TUPLE, 2,                                                    \
        ERL_DRV_TUPLE, 2                                                     \
    };                                                                       \
    return driver_output_term_locked(desc->driver_output_term_lock,          \
                                     desc->port, spec,                       \
                                     sizeof(spec) / sizeof(spec[0]));        \
}
CREATE_REPLY_OK_INTEGER(char,     ERL_DRV_INT)
CREATE_REPLY_OK_INTEGER(int8_t,   ERL_DRV_INT)
CREATE_REPLY_OK_INTEGER(uint8_t,  ERL_DRV_UINT)
CREATE_REPLY_OK_INTEGER(int16_t,  ERL_DRV_INT)
CREATE_REPLY_OK_INTEGER(uint16_t, ERL_DRV_UINT)
CREATE_REPLY_OK_INTEGER(int32_t,  ERL_DRV_INT)
CREATE_REPLY_OK_INTEGER(uint32_t, ERL_DRV_UINT)
#if NATIVE_64BIT_TYPES
CREATE_REPLY_OK_INTEGER(int64_t,  ERL_DRV_INT)
CREATE_REPLY_OK_INTEGER(uint64_t, ERL_DRV_UINT)
#endif

static int reply_data_boolean(descriptor_t *desc, bool value)
{
    ErlDrvTermData spec[] = {
        ERL_DRV_PORT, desc->port_term,
        ERL_DRV_ATOM, atom_value_data,
        ERL_DRV_ATOM, (value ? atom_value_true : atom_value_false),
        ERL_DRV_TUPLE, 2,
        ERL_DRV_TUPLE, 2
    };
    return driver_output_term_locked(desc->driver_output_term_lock, desc->port,
                                     spec, sizeof(spec) / sizeof(spec[0]));
}

static int reply_data_double(descriptor_t *desc, double number)
{
    ErlDrvTermData spec[] = {
        ERL_DRV_PORT, desc->port_term,
        ERL_DRV_ATOM, atom_value_data,
        ERL_DRV_FLOAT, reinterpret_cast<ErlDrvTermData>(&number),
        ERL_DRV_TUPLE, 2,
        ERL_DRV_TUPLE, 2
    };
    return driver_output_term_locked(desc->driver_output_term_lock, desc->port,
                                     spec, sizeof(spec) / sizeof(spec[0]));
}

static int reply_data_binary(descriptor_t *desc, ErlDrvBinary *ptr)
{
    ErlDrvTermData spec[] = {
        ERL_DRV_PORT, desc->port_term,
        ERL_DRV_ATOM, atom_value_data,
        ERL_DRV_BINARY, reinterpret_cast<ErlDrvTermData>(ptr),
                        ptr->orig_size, 0,
        ERL_DRV_TUPLE, 2,
        ERL_DRV_TUPLE, 2
    };
    return driver_output_term_locked(desc->driver_output_term_lock, desc->port,
                                     spec, sizeof(spec) / sizeof(spec[0]));
}

static int reply_data_binary(descriptor_t *desc, void *ptr, uint32_t length)
{
    
    ErlDrvTermData spec[] = {
        ERL_DRV_PORT, desc->port_term,
        ERL_DRV_ATOM, atom_value_data,
        ERL_DRV_BUF2BINARY, reinterpret_cast<ErlDrvTermData>(ptr),
                            length,
        ERL_DRV_TUPLE, 2,
        ERL_DRV_TUPLE, 2
    };
    return driver_output_term_locked(desc->driver_output_term_lock, desc->port,
                                     spec, sizeof(spec) / sizeof(spec[0]));
}

static int reply_data_string(descriptor_t *desc, char *ptr, uint32_t length)
{
    ErlDrvTermData spec[] = {
        ERL_DRV_PORT, desc->port_term,
        ERL_DRV_ATOM, atom_value_data,
        ERL_DRV_STRING, reinterpret_cast<ErlDrvTermData>(ptr),
                        length,
        ERL_DRV_TUPLE, 2,
        ERL_DRV_TUPLE, 2
    };
    return driver_output_term_locked(desc->driver_output_term_lock, desc->port,
                                     spec, sizeof(spec) / sizeof(spec[0]));
}

namespace
{
    // list of non-fatal errors that can be sent back

    // port driver will send an error for protocol problems
    namespace Error
    {
        char const * const invalid_function =
            "Invalid function call";
        char const * const unable_to_decode_identifier =
            "Unable to decode function identifier";
        char const * const unable_to_decode_arguments =
            "Unable to decode arguments";
    }

}

static int reply_data_error_string(descriptor_t *desc, char const * const ptr)
{
    uint32_t const length = strlen(ptr);
    ErlDrvTermData spec[] = {
        ERL_DRV_PORT, desc->port_term,
        ERL_DRV_ATOM, atom_value_data,
        ERL_DRV_ATOM, atom_value_error,
        ERL_DRV_STRING, reinterpret_cast<ErlDrvTermData>(ptr),
                        length,
        ERL_DRV_TUPLE, 2,
        ERL_DRV_TUPLE, 2,
        ERL_DRV_TUPLE, 2
    };
    return driver_output_term_locked(desc->driver_output_term_lock, desc->port,
                                     spec, sizeof(spec) / sizeof(spec[0]));
}

//////////////////////////////////////////////////////////////////////////////
// driver implementation functions and data structure
//////////////////////////////////////////////////////////////////////////////

static int driver_entry_init()
{
    return 0;
}

static ErlDrvData driver_entry_start(ErlDrvPort port, char *args)
{
    descriptor_t *desc =
        reinterpret_cast<descriptor_t *>(driver_alloc(sizeof(descriptor_t)));
    if (! desc)
        return ERL_DRV_ERROR_GENERAL;
    desc->port = port;
    desc->port_term = driver_mk_port(port);
    desc->driver_output_term_lock = erl_drv_mutex_create(
        const_cast<char *>("driver_output_term_lock"));
    return reinterpret_cast<ErlDrvData>(desc);
}

static void driver_entry_stop(ErlDrvData driver_data)
{
    descriptor_t *desc = reinterpret_cast<descriptor_t *>(driver_data);
    if (desc)
    {
        erl_drv_mutex_destroy(desc->driver_output_term_lock);
        driver_free(desc);
    }
}

static void driver_entry_ready_async(ErlDrvData driver_data,
                                     ErlDrvThreadData thread_data)
{
    descriptor_t *desc = reinterpret_cast<descriptor_t *>(driver_data);
    callstate_t *c = reinterpret_cast<callstate_t *>(thread_data);

    if (! desc || ! c)
        return;

    switch (c->cmd)
    {
        // create the case statements for each function
        BOOST_PP_SEQ_FOR_EACH(CREATE_FUNCTION_OUTPUT_CASE, _,
                              PORT_DRIVER_FUNCTIONS)
        default:
            reply_data_error_string(desc, Error::invalid_function);
            return;
    }
}

// create the invoke functions that handle the call state and argument storage
BOOST_PP_SEQ_FOR_EACH(CREATE_INVOKE_FUNCTION, _, PORT_DRIVER_FUNCTIONS)

static void driver_entry_outputv(ErlDrvData driver_data, ErlIOVec *ev)
{
    descriptor_t *desc = reinterpret_cast<descriptor_t *>(driver_data);
    if (! desc || ! ev || ev->size < 1)
        return;

    uint16_t cmd;
    size_t q = 1; // index into ev->iov
    size_t p = 0; // index into ev->iov[q].iov_base
    if (EV_GET_UINT16(ev, &cmd, p, q))
    {
        reply_data_error_string(desc, Error::unable_to_decode_identifier);
        return;
    }

    switch (cmd)
    {
        // create the case statements for each function
        BOOST_PP_SEQ_FOR_EACH(CREATE_FUNCTION_INPUT_CASE, _, 
                              PORT_DRIVER_FUNCTIONS)

        default:
            reply_data_error_string(desc, Error::invalid_function);
            return;
    }
}

// provide port driver data to erlang interface

static ErlDrvEntry driver_entry_functions = {
    // init
    //
    // initialize global data
    driver_entry_init,
    // start
    //
    // called when port is opened
    driver_entry_start,
    // stop
    //
    // called when port is closed
    driver_entry_stop,
    // output
    //
    // called when erlang has sent
    0,
    // ready_input
    //
    // called when input descriptor ready
    0,
    // ready_output
    //
    // called when output descriptor ready
    0,
    // driver_name
    //
    // the argument to open_port
    const_cast<char *>(BOOST_PP_STRINGIZE(PORT_DRIVER_NAME)),
    // finish
    //
    // called when unloaded
    0,
    // handle
    //
    // reserved -- used by emulator internally
    0,
    // control
    //
    // "ioctl" for drivers - invoked by port_control/3
    0,
    // timeout
    //
    // handling of timeout in driver
    0,
    // outputv
    //
    // called when we have output from erlang to the port instead of output
    // if outputv is defined to handle vectorized erlang IO output (ErlIOVec)
    driver_entry_outputv,
    // ready_async
    driver_entry_ready_async,
    // flush
    // 
    // called when the port is about to be closed, and
    // there is data in the driver queue that needs to be
    // flushed before ’stop’ can be called
    0,
    // call
    //
    // Works mostly like ’control’, a synchronous call into the driver
    0,
    // event
    //
    // Called when an event selected by driver_event() has occurred
    0,
    // extended_marker
    ERL_DRV_EXTENDED_MARKER,
    // major_version
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    // minor_version
    ERL_DRV_EXTENDED_MINOR_VERSION,
    // driver_flags
    ERL_DRV_FLAG_USE_PORT_LOCKING,
    // handle
    //
    // reserved -- used by emulator internally
    0,
    // process_exit
    //
    // Called when a process monitor fires
    0
};

extern "C" DRIVER_INIT(PORT_DRIVER_NAME)
{
    return &driver_entry_functions;
}

