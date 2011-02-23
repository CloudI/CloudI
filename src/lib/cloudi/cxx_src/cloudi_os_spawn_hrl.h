// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
// ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab:

// GENERIC ERLANG PORT [DRIVER] VERSION 0.7
// automatically create Erlang functions for C/C++ bindings

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

#include <boost/preprocessor/stringize.hpp>
#include <boost/preprocessor/seq/for_each.hpp>
#include <boost/preprocessor/seq/elem.hpp>
#include <boost/preprocessor/seq/size.hpp>
#include <boost/preprocessor/repetition/repeat_from_to.hpp>
#include <boost/preprocessor/repetition/enum.hpp>
#include <boost/preprocessor/cat.hpp>
#include <boost/preprocessor/arithmetic/dec.hpp>
#include <boost/preprocessor/punctuation/paren.hpp>
#include <boost/preprocessor/tuple/to_seq.hpp>
#include <boost/preprocessor/control/if.hpp>
#include <boost/preprocessor/punctuation/comma.hpp>

#define ENCODE_ARGUMENT_AS_BINARY_FROM_TYPE_char(N) \
    <<CREATE_FUNCTION_ARGUMENTS(_, N, _):8/signed-integer-native>>
#define ENCODE_ARGUMENT_AS_BINARY_FROM_TYPE_uchar(N) \
    <<CREATE_FUNCTION_ARGUMENTS(_, N, _):8/unsigned-integer-native>>
#define ENCODE_ARGUMENT_AS_BINARY_FROM_TYPE_bool(N) \
    <<CREATE_FUNCTION_ARGUMENTS(_, N, _):8/unsigned-integer-native>>
#define ENCODE_ARGUMENT_AS_BINARY_FROM_TYPE_int8_t(N) \
    <<CREATE_FUNCTION_ARGUMENTS(_, N, _):8/signed-integer-native>>
#define ENCODE_ARGUMENT_AS_BINARY_FROM_TYPE_uint8_t(N) \
    <<CREATE_FUNCTION_ARGUMENTS(_, N, _):8/unsigned-integer-native>>
#define ENCODE_ARGUMENT_AS_BINARY_FROM_TYPE_int16_t(N) \
    <<CREATE_FUNCTION_ARGUMENTS(_, N, _):16/signed-integer-native>>
#define ENCODE_ARGUMENT_AS_BINARY_FROM_TYPE_uint16_t(N) \
    <<CREATE_FUNCTION_ARGUMENTS(_, N, _):16/unsigned-integer-native>>
#define ENCODE_ARGUMENT_AS_BINARY_FROM_TYPE_int32_t(N) \
    <<CREATE_FUNCTION_ARGUMENTS(_, N, _):32/signed-integer-native>>
#define ENCODE_ARGUMENT_AS_BINARY_FROM_TYPE_uint32_t(N) \
    <<CREATE_FUNCTION_ARGUMENTS(_, N, _):32/unsigned-integer-native>>
#define ENCODE_ARGUMENT_AS_BINARY_FROM_TYPE_time_t(N) \
    <<CREATE_FUNCTION_ARGUMENTS(_, N, _):64/unsigned-integer-native>>
#define ENCODE_ARGUMENT_AS_BINARY_FROM_TYPE_int64_t(N) \
    <<CREATE_FUNCTION_ARGUMENTS(_, N, _):64/signed-integer-native>>
#define ENCODE_ARGUMENT_AS_BINARY_FROM_TYPE_uint64_t(N) \
    <<CREATE_FUNCTION_ARGUMENTS(_, N, _):64/unsigned-integer-native>>
#define ENCODE_ARGUMENT_AS_BINARY_FROM_TYPE_float(N) \
    <<CREATE_FUNCTION_ARGUMENTS(_, N, _):64/float-native>>
#define ENCODE_ARGUMENT_AS_BINARY_FROM_TYPE_double(N) \
    <<CREATE_FUNCTION_ARGUMENTS(_, N, _):64/float-native>>
#define ENCODE_ARGUMENT_AS_BINARY_FROM_TYPE_pchar_len(N) \
    encode_uint8(CREATE_FUNCTION_ARGUMENTS(_, N, _))
#define ENCODE_ARGUMENT_AS_BINARY_FROM_TYPE_puint32_len(N) \
    encode_uint32(CREATE_FUNCTION_ARGUMENTS(_, N, _))

#if defined(PORT_DRIVER_NAME)

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

-define(ERL_PORT_DRIVER_NAME, \
        BOOST_PP_STRINGIZE(PORT_DRIVER_NAME)).
-define(ERL_PORT_DRIVER_NAME_PREFIX, \
        BOOST_PP_STRINGIZE(PORT_DRIVER_NAME_PREFIX)).
#define FUNCTIONS_SEQUENCE PORT_DRIVER_FUNCTIONS
#if defined(PORT_NAME)
-define(ERL_PORT_NAME, \
        BOOST_PP_STRINGIZE(PORT_NAME)).
-define(ERL_PORT_NAME_PREFIX, \
        BOOST_PP_STRINGIZE(PORT_NAME_PREFIX)).
#endif
#elif defined(PORT_NAME)

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

-define(ERL_PORT_NAME, \
        BOOST_PP_STRINGIZE(PORT_NAME)).
-define(ERL_PORT_NAME_PREFIX, \
        BOOST_PP_STRINGIZE(PORT_NAME_PREFIX)).
#define FUNCTIONS_SEQUENCE PORT_FUNCTIONS
#else
#error Neither PORT_DRIVER_NAME nor PORT_NAME defined
#endif

#define EXPORT_FUNCTION(Z, N, FUNCTIONS) \
    GET_NAME(BOOST_PP_SEQ_ELEM(N, FUNCTIONS))\
    / \
    BOOST_PP_ADD(GET_ARGC(BOOST_PP_SEQ_ELEM(N, FUNCTIONS)), 1)

#define CREATE_FUNCTION_ARGUMENTS(Z, N, ARGUMENTS) \
    BOOST_PP_CAT(Arg, N)

#define ENCODE_ARGUMENT_AS_BINARY(Z, N, ARGUMENTS) \
    BOOST_PP_COMMA() \
    BOOST_PP_CAT(\
        ENCODE_ARGUMENT_AS_BINARY_FROM_TYPE_, \
        BOOST_PP_SEQ_ELEM(N, ARGUMENTS) \
    )(N)

#define CREATE_FUNCTION(I, DATA, FUNCTION) \
    GET_NAME(FUNCTION) \
    BOOST_PP_LPAREN() \
    Process BOOST_PP_COMMA_IF(GET_ARGC(FUNCTION)) \
    BOOST_PP_ENUM( \
        GET_ARGC(FUNCTION), \
        CREATE_FUNCTION_ARGUMENTS, \
        BOOST_PP_TUPLE_TO_SEQ(GET_ARGC(FUNCTION), GET_ARGV(FUNCTION)) \
    ) \
    BOOST_PP_RPAREN() \
    -> \
    BOOST_PP_IF(\
        GET_ASYNC(FUNCTION), \
        call_port_async, \
        call_port_sync \
    )\
    BOOST_PP_LPAREN() \
    Process, \
    BOOST_PP_DEC(I), \
    [\
        <<BOOST_PP_DEC(I):16/unsigned-integer-native>>\
        BOOST_PP_REPEAT_FROM_TO(\
            0,\
            GET_ARGC(FUNCTION),\
            ENCODE_ARGUMENT_AS_BINARY,\
            BOOST_PP_TUPLE_TO_SEQ(GET_ARGC(FUNCTION), GET_ARGV(FUNCTION))\
        )\
    ]\
    BOOST_PP_RPAREN().


-export([
BOOST_PP_ENUM(BOOST_PP_SEQ_SIZE(FUNCTIONS_SEQUENCE),
              EXPORT_FUNCTION, FUNCTIONS_SEQUENCE)
]).

-vsn(BOOST_PP_STRINGIZE(CURRENT_VERSION)).

BOOST_PP_SEQ_FOR_EACH(CREATE_FUNCTION, _, FUNCTIONS_SEQUENCE)

encode_uint8(Value) when erlang:is_list(Value) ->
    ValueList = [<<E:8/unsigned-integer-native>> || E <- Value],
    Data = erlang:list_to_binary(ValueList),
    DataSize = erlang:length(ValueList),
    <<DataSize:32/unsigned-integer-native, Data/binary>>.

encode_uint32(Value) when erlang:is_list(Value) ->
    ValueList = [<<E:32/unsigned-integer-native>> || E <- Value],
    Data = erlang:list_to_binary(ValueList),
    DataSize = erlang:length(ValueList),
    <<DataSize:32/unsigned-integer-native, Data/binary>>.

