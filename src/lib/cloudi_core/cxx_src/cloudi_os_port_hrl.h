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

#include <boost/preprocessor/stringize.hpp>
#include <boost/preprocessor/seq/for_each.hpp>
#include <boost/preprocessor/seq/elem.hpp>
#include <boost/preprocessor/seq/size.hpp>
#include <boost/preprocessor/repetition/repeat_from_to.hpp>
#include <boost/preprocessor/repetition/enum.hpp>
#include <boost/preprocessor/cat.hpp>
#include <boost/preprocessor/arithmetic/dec.hpp>
#include <boost/preprocessor/punctuation/paren.hpp>
//#include <boost/preprocessor/tuple/to_seq.hpp> // broken with boost >= 1.5?
#include <boost/preprocessor/tuple/to_list.hpp>
#include <boost/preprocessor/list/for_each.hpp>
#include <boost/preprocessor/control/if.hpp>
#include <boost/preprocessor/punctuation/comma.hpp>

// work-around instead of BOOST_PP_TUPLE_TO_SEQ to correctly handle arity 0
#define TUPLE_TO_SEQ_E(r, data, elem) (elem)
#define TUPLE_TO_SEQ(I, T) \
    BOOST_PP_LIST_FOR_EACH(TUPLE_TO_SEQ_E, _, BOOST_PP_TUPLE_TO_LIST(I, T))

#define ENCODE_ARGUMENT_AS_BINARY_FROM_TYPE_char(N) \
    <<CREATE_FUNCTION_ARGUMENTS(_, N, _):8/signed-integer-native>>
#define ENCODE_ARGUMENT_AS_BINARY_FROM_TYPE_uchar(N) \
    <<CREATE_FUNCTION_ARGUMENTS(_, N, _):8/unsigned-integer-native>>
#define ENCODE_ARGUMENT_AS_BINARY_FROM_TYPE_bool(N) \
    encode_bool(CREATE_FUNCTION_ARGUMENTS(_, N, _))
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
    encode_pchar_len(CREATE_FUNCTION_ARGUMENTS(_, N, _))
#define ENCODE_ARGUMENT_AS_BINARY_FROM_TYPE_puint32_len(N) \
    encode_puint32_len(CREATE_FUNCTION_ARGUMENTS(_, N, _))

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
-define(ERL_PORT_MODULE, cloudi_core_i_os_port). % no async support!
#define FUNCTIONS_SEQUENCE PORT_DRIVER_FUNCTIONS
#if defined(PORT_NAME)
-define(ERL_PORT_NAME, \
        BOOST_PP_STRINGIZE(PORT_NAME)).
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
-define(ERL_PORT_MODULE, cloudi_core_i_os_port).
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
        TUPLE_TO_SEQ(GET_ARGC(FUNCTION), GET_ARGV(FUNCTION)) \
    ) \
    BOOST_PP_RPAREN() \
    -> \
    BOOST_PP_IF(\
        GET_ASYNC(FUNCTION), \
        ?ERL_PORT_MODULE:async, \
        ?ERL_PORT_MODULE:sync \
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
            TUPLE_TO_SEQ(GET_ARGC(FUNCTION), GET_ARGV(FUNCTION))\
        )\
    ]\
    BOOST_PP_RPAREN().


-export([
BOOST_PP_ENUM(BOOST_PP_SEQ_SIZE(FUNCTIONS_SEQUENCE),
              EXPORT_FUNCTION, FUNCTIONS_SEQUENCE)
]).

BOOST_PP_SEQ_FOR_EACH(CREATE_FUNCTION, _, FUNCTIONS_SEQUENCE)

-compile({nowarn_unused_function,
          [{encode_bool, 1},
           {encode_pchar_len, 1},
           {encode_puint32_len, 1}]}).

encode_bool(true) ->
    <<1>>;
encode_bool(false) ->
    <<0>>.

encode_pchar_len(Value) when is_binary(Value) ->
    DataSize = erlang:byte_size(Value),
    <<DataSize:32/unsigned-integer-native, Value/binary>>;
encode_pchar_len(Value) when is_list(Value) ->
    ValueList = [<<E:8/unsigned-integer-native>> || E <- Value],
    Data = erlang:list_to_binary(ValueList),
    DataSize = erlang:byte_size(Data),
    <<DataSize:32/unsigned-integer-native, Data/binary>>.

encode_puint32_len(Value) when is_list(Value) ->
    ValueList = [<<E:32/unsigned-integer-native>> || E <- Value],
    Data = erlang:list_to_binary(ValueList),
    DataSize = erlang:byte_size(Data) div 4,
    <<DataSize:32/unsigned-integer-native, Data/binary>>.

