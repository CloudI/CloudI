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

// port driver documentation:
// http://erlang.org/doc/man/erl_driver.html
// http://erlang.org/doc/man/erl_ddll.html

// limit on the number of function arguments handled in the bindings
// (increasing this increases the (heap) memory consumption for
//  asynchronous function calls and the stack size of synchronous calls)
#define PORT_DRIVER_FUNCTIONS_MAXIMUM_ARGUMENTS 9


#if ! defined(PORT_DRIVER_NAME)
#error Define PORT_DRIVER_NAME within the functions header file to specify the \
       dynamic library name (should not be static)
#endif
#if ! defined(PORT_DRIVER_FUNCTIONS)
#if defined(PORT_FUNCTIONS)
#include <boost/preprocessor/seq/transform.hpp>
#include <boost/preprocessor/tuple/elem.hpp>

#define CREATE_PORT_DRIVER_FUNCTIONS_DEFINITION(S, DATA, ELEMENT) (\
    BOOST_PP_TUPLE_ELEM(4, 0, ELEMENT),\
    BOOST_PP_TUPLE_ELEM(4, 1, ELEMENT),\
    BOOST_PP_TUPLE_ELEM(4, 2, ELEMENT),\
    BOOST_PP_TUPLE_ELEM(4, 3, ELEMENT), 0)\

#define PORT_DRIVER_FUNCTIONS \
    BOOST_PP_SEQ_TRANSFORM(CREATE_PORT_DRIVER_FUNCTIONS_DEFINITION, _, \
                           PORT_FUNCTIONS)
#warning Using PORT_FUNCTIONS to determine PORT_DRIVER_FUNCTIONS \
         (all synchronous function calls)
#else
#error Define PORT_DRIVER_FUNCTIONS within the functions header file to \
       specify the functions and their types
#endif
#endif

// 64bit return values are only possible on 64bit machines
// because of the types ErlDrvSInt and ErlDrvUInt
#define NATIVE_64BIT_TYPES (   defined(__alpha__)                            \
                            || defined(__ia64__)                             \
                            || defined(__ppc64__)                            \
                            || defined(__s390x__)                            \
                            || defined(__x86_64__))

#include <boost/preprocessor/seq/size.hpp>
#include <boost/preprocessor/tuple/elem.hpp>

// define the structure of the PORT_DRIVER_FUNCTIONS macro data
// (sequence of tuples)

// 5 tuple elements in the PORT_DRIVER_FUNCTIONS sequence
#define PORT_DRIVER_FUNCTION_ENTRY_LENGTH   5
// specific tuple elements in the PORT_DRIVER_FUNCTIONS sequence
#define PORT_DRIVER_FUNCTION_ENTRY_NAME     0
#define PORT_DRIVER_FUNCTION_ENTRY_ARGC     1
#define PORT_DRIVER_FUNCTION_ENTRY_ARGV     2
#define PORT_DRIVER_FUNCTION_ENTRY_RETURN   3
#define PORT_DRIVER_FUNCTION_ENTRY_ASYNC    4

// macros to access function data in a PORT_DRIVER_FUNCTIONS tuple entry

#define GET_NAME(FUNCTION) \
    BOOST_PP_TUPLE_ELEM(\
        PORT_DRIVER_FUNCTION_ENTRY_LENGTH, \
        PORT_DRIVER_FUNCTION_ENTRY_NAME, FUNCTION\
    )
#define GET_ARGC(FUNCTION) \
    BOOST_PP_TUPLE_ELEM(\
        PORT_DRIVER_FUNCTION_ENTRY_LENGTH, \
        PORT_DRIVER_FUNCTION_ENTRY_ARGC, FUNCTION\
    )
#define GET_ARGV(FUNCTION) \
    BOOST_PP_TUPLE_ELEM(\
        PORT_DRIVER_FUNCTION_ENTRY_LENGTH, \
        PORT_DRIVER_FUNCTION_ENTRY_ARGV, FUNCTION\
    )
#define GET_RETURN(FUNCTION) \
    BOOST_PP_TUPLE_ELEM(\
        PORT_DRIVER_FUNCTION_ENTRY_LENGTH, \
        PORT_DRIVER_FUNCTION_ENTRY_RETURN, FUNCTION\
    )
#define GET_ASYNC(FUNCTION) \
    BOOST_PP_TUPLE_ELEM(\
        PORT_DRIVER_FUNCTION_ENTRY_LENGTH, \
        PORT_DRIVER_FUNCTION_ENTRY_ASYNC, FUNCTION\
    )

// enforce inherent implementation limits

#if BOOST_PP_SEQ_SIZE(PORT_DRIVER_FUNCTIONS) > 32767
#error Limited to 32767 port driver functions (type uint16_t is used for "cmd")
#endif

