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

#if ! defined(PORT_NAME)
#error Define PORT_NAME within the functions header file to specify the \
       executable name
#endif
#if ! defined(PORT_FUNCTIONS)
#if defined(PORT_DRIVER_FUNCTIONS)
#include <boost/preprocessor/seq/transform.hpp>
#include <boost/preprocessor/tuple/elem.hpp>

#define CREATE_PORT_FUNCTIONS_DEFINITION(S, DATA, ELEMENT) (\
    BOOST_PP_TUPLE_ELEM(5, 0, ELEMENT),\
    BOOST_PP_TUPLE_ELEM(5, 1, ELEMENT),\
    BOOST_PP_TUPLE_ELEM(5, 2, ELEMENT),\
    BOOST_PP_TUPLE_ELEM(5, 3, ELEMENT))\

#define PORT_FUNCTIONS \
    BOOST_PP_SEQ_TRANSFORM(CREATE_PORT_FUNCTIONS_DEFINITION, _, \
                           PORT_DRIVER_FUNCTIONS)
#warning Using PORT_DRIVER_FUNCTIONS to determine PORT_FUNCTIONS
#else
#error Define PORT_FUNCTIONS within the functions header file to specify \
       the functions and their types
#endif
#endif

#include <boost/preprocessor/seq/size.hpp>
#include <boost/preprocessor/tuple/elem.hpp>
#include <boost/preprocessor/facilities/empty.hpp>

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
