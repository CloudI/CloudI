# -*- coding: utf-8; Mode: python; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
# ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
#
# BSD LICENSE
# 
# Copyright (c) 2011, Michael Truog <mjtruog at gmail dot com>
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# 
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in
#       the documentation and/or other materials provided with the
#       distribution.
#     * All advertising materials mentioning features or use of this
#       software must display the following acknowledgment:
#         This product includes software developed by Michael Truog
#     * The name of the author may not be used to endorse or promote
#       products derived from this software without specific prior
#       written permission
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
# CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
# INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
# OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
# CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
# BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
# WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
# NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
# DAMAGE.
#

import struct, string, types, math

__all__ = ['OtpErlangAtom',
           'OtpErlangBinary',
           'OtpErlangFunction',
           'OtpErlangReference',
           'OtpErlangPort',
           'OtpErlangPid',
           'binary_to_term',
           'term_to_binary',
           'ParseException',
           'OutputException']

# tag values here http://www.erlang.org/doc/apps/erts/erl_ext_dist.html
_TAG_VERSION = 131
_TAG_NEW_FLOAT_EXT = 70
_TAG_BIT_BINARY_EXT = 77
_TAG_ATOM_CACHE_REF = 78
_TAG_SMALL_INTEGER_EXT = 97
_TAG_INTEGER_EXT = 98
_TAG_FLOAT_EXT = 99
_TAG_ATOM_EXT = 100
_TAG_REFERENCE_EXT = 101
_TAG_PORT_EXT = 102
_TAG_PID_EXT = 103
_TAG_SMALL_TUPLE_EXT = 104
_TAG_LARGE_TUPLE_EXT = 105
_TAG_NIL_EXT = 106
_TAG_STRING_EXT = 107
_TAG_LIST_EXT = 108
_TAG_BINARY_EXT = 109
_TAG_SMALL_BIG_EXT = 110
_TAG_LARGE_BIG_EXT = 111
_TAG_NEW_FUN_EXT = 112
_TAG_EXPORT_EXT = 113
_TAG_NEW_REFERENCE_EXT = 114
_TAG_SMALL_ATOM_EXT = 115
_TAG_FUN_EXT = 117

class OtpErlangAtom(object):
    def __init__(self, value):
        self.value = value
    def __str__(self):
        if type(self.value) == types.IntType:
            return chr(_TAG_ATOM_CACHE_REF) + chr(self.value)
        elif type(self.value) == types.StringType:
            size = len(self.value)
            if size < 256:
                return chr(_TAG_SMALL_ATOM_EXT) + chr(size) + self.value
            else:
                return chr(_TAG_ATOM_EXT) + struct.pack('>H', size) + self.value
        else:
            raise OutputException('unknown atom type')

class OtpErlangBinary(object):
    def __init__(self, value, bits = 8):
        self.value = value
        self.bits = bits # bits in last byte
    def __str__(self):
        size = len(self.value)
        if self.bits != 8:
            return (chr(_TAG_BIT_BINARY_EXT) + struct.pack('>I', size) +
                chr(self.bits) + self.value
            )
        else:
            return chr(_TAG_BINARY_EXT) + struct.pack('>I', size) + self.value

class OtpErlangFunction(object):
    def __init__(self, tag, value):
        self.tag = tag
        self.value = value
    def __str__(self):
        return chr(self.tag) + self.value

class OtpErlangReference(object):
    def __init__(self, node, id, creation):
        self.node = node
        self.id = id
        self.creation = creation
    def __str__(self):
        size = len(self.id) / 4
        if size > 1:
            return (chr(_TAG_NEW_REFERENCE_EXT) + struct.pack('>H', size) +
                str(self.node) + self.creation + self.id
            )
        else:
            return (chr(_TAG_REFERENCE_EXT) +
                str(self.node) + self.id + self.creation
            )

class OtpErlangPort(object):
    def __init__(self, node, id, creation):
        self.node = node
        self.id = id
        self.creation = creation
    def __str__(self):
        return chr(_TAG_PORT_EXT) + str(self.node) + self.id + self.creation

class OtpErlangPid(object):
    def __init__(self, node, id, serial, creation):
        self.node = node
        self.id = id
        self.serial = serial
        self.creation = creation
    def __str__(self):
        return (chr(_TAG_PID_EXT) +
            str(self.node) + self.id + self.serial + self.creation
        )

# binary_to_term

def binary_to_term(data):
    if ord(data[0]) != _TAG_VERSION:
        raise ParseException('invalid version')
    i, term = _binary_to_term(1, data)
    if i != len(data):
        raise ParseException('unparsed data')
    return term

def _binary_to_term(i, data):
    tag = ord(data[i])
    i += 1
    if tag == _TAG_NEW_FLOAT_EXT:
        return (i + 8, struct.unpack('>d', data[i:i + 8])[0])
    elif tag == _TAG_BIT_BINARY_EXT:
        j = struct.unpack('>I', data[i:i + 4])[0]
        i += 4
        bits = ord(data[i])
        i += 1
        return (i + j, OtpErlangBinary(data[i:i + j], bits))
    elif tag == _TAG_ATOM_CACHE_REF:
        return (i + 1, OtpErlangAtom(ord(data[i:i + 1])))
    elif tag == _TAG_SMALL_INTEGER_EXT:
        return (i + 1, ord(data[i]))
    elif tag == _TAG_INTEGER_EXT:
        return (i + 4, struct.unpack('>i', data[i:i + 4])[0])
    elif tag == _TAG_FLOAT_EXT:
        value = float(data[i:i + 31].partition(chr(0))[0])
        return (i + 31, value)
    elif tag == _TAG_ATOM_EXT:
        j = struct.unpack('>H', data[i:i + 2])[0]
        i += 2
        return (i + j, OtpErlangAtom(data[i:i + j]))
    elif tag == _TAG_REFERENCE_EXT or tag == _TAG_PORT_EXT:
        i, node = _binary_to_atom(i, data)
        id = data[i:i + 4]
        i += 4
        creation = data[i]
        i += 1
        if tag == _TAG_REFERENCE_EXT:
            return (i, OtpErlangReference(node, id, creation))
        elif tag == _TAG_PORT_EXT:
            return (i, OtpErlangPort(node, id, creation))
    elif tag == _TAG_PID_EXT:
        i, node = _binary_to_atom(i, data)
        id = data[i:i + 4]
        i += 4
        serial = data[i:i + 4]
        i += 4
        creation = data[i]
        i += 1
        return (i, OtpErlangPid(node, id, serial, creation))
    elif tag == _TAG_SMALL_TUPLE_EXT or tag == _TAG_LARGE_TUPLE_EXT:
        if tag == _TAG_SMALL_TUPLE_EXT:
            arity = ord(data[i])
            i += 1
        elif tag == _TAG_LARGE_TUPLE_EXT:
            arity = struct.unpack('>I', data[i:i + 4])[0]
            i += 4
        i, tmp = _binary_to_term_sequence(i, arity, data)
        return (i, tuple(tmp))
    elif tag == _TAG_NIL_EXT:
        return (i, [])
    elif tag == _TAG_STRING_EXT:
        j = struct.unpack('>H', data[i:i + 2])[0]
        i += 2
        return (i + j, data[i:i + j])
    elif tag == _TAG_LIST_EXT:
        arity = struct.unpack('>I', data[i:i + 4])[0]
        i += 4
        i, tmp = _binary_to_term_sequence(i, arity, data)
        i, tail = _binary_to_term(i, data)
        if tail <> []: 
            tmp.append(tail)
        return (i, tmp)
    elif tag == _TAG_BINARY_EXT:
        j = struct.unpack('>I', data[i:i + 4])[0]
        i += 4
        return (i + j, OtpErlangBinary(data[i:i + j], 8))
    elif tag == _TAG_SMALL_BIG_EXT or tag == _TAG_LARGE_BIG_EXT:
        if tag == _TAG_SMALL_BIG_EXT:
            j = ord(data[i])
            i += 1
        elif tag == _TAG_LARGE_BIG_EXT:
            j = struct.unpack('>I', data[i:i + 4])[0]
            i += 4
        sign = ord(data[i])
        i += 1
        bignum = 0L
        for bignum_index in range(j):
            digit = ord(data[i + j - bignum_index])
            bignum = bignum * 256L + long(digit)
        if sign:
            bignum *= -1L
        return (i + j, bignum)
    elif tag == _TAG_NEW_FUN_EXT:
        size = struct.unpack('>I', data[i:i + 4])[0]
        return (i + size, OtpErlangFunction(tag, data[i:i + size]))
    elif tag == _TAG_EXPORT_EXT:
        old_i = i
        i, module = _binary_to_atom(i, data)
        i, function = _binary_to_atom(i, data)
        if ord(data[i]) != _TAG_SMALL_INTEGER_EXT:
            raise ParseException('invalid small integer tag')
        i += 1
        arity = ord(data[i])
        i += 1
        return (i, OtpErlangFunction(tag, data[old_i:i]))
    elif tag == _TAG_NEW_REFERENCE_EXT:
        j = struct.unpack('>H', data[i:i + 2])[0] * 4
        i += 2
        i, node = _binary_to_atom(i, data)
        creation = data[i]
        i += 1
        return (i + j, OtpErlangReference(node, data[i: i + j], creation))
    elif tag == _TAG_SMALL_ATOM_EXT:
        j = ord(data[i:i + 1])
        i += 1
        return (i + j, OtpErlangAtom(data[i:i + j]))
    elif tag == _TAG_FUN_EXT:
        old_i = i
        numfree = struct.unpack('>I', data[i:i + 4])[0]
        i += 4
        i, pid = _binary_to_pid(i, data)
        i, module = _binary_to_atom(i, data)
        i, index = _binary_to_integer(i, data)
        i, uniq = _binary_to_integer(i, data)
        i, free = _binary_to_term_sequence(i, numfree, data)
        return (i, OtpErlangFunction(tag, data[old_i:i]))
    else:
        raise ParseException('invalid tag')

def _binary_to_term_sequence(i, arity, data):
    sequence = []
    for arity_index in range(arity):
        i, element = _binary_to_term(i, data)
        sequence.append(element)
    return (i, sequence)
        
def _binary_to_integer(i, data):
    tag = ord(data[i])
    i += 1
    if tag == _TAG_SMALL_INTEGER_EXT:
        return (i + 1, ord(data[i]))
    elif tag == _TAG_INTEGER_EXT:
        return (i + 4, struct.unpack('>i', data[i:i + 4])[0])
    else:
        raise ParseException('invalid integer tag')

def _binary_to_pid(i, data):
    tag = ord(data[i])
    i += 1
    if tag == _TAG_PID_EXT:
        i, node = _binary_to_atom(i, data)
        id = data[i:i + 4]
        i += 4
        serial = data[i:i + 4]
        i += 4
        creation = data[i]
        i += 1
        return (i, OtpErlangPid(node, id, serial, creation))
    else:
        raise ParseException('invalid pid tag')

def _binary_to_atom(i, data):
    tag = ord(data[i])
    i += 1
    if tag == _TAG_ATOM_EXT:
        j = struct.unpack('>H', data[i:i + 2])[0]
        i += 2
        return (i + j, OtpErlangAtom(data[i:i + j]))
    elif tag == _TAG_ATOM_CACHE_REF:
        return (i + 1, OtpErlangAtom(ord(data[i:i + 1])))
    elif tag == _TAG_SMALL_ATOM_EXT:
        j = ord(data[i:i + 1])
        i += 1
        return (i + j, OtpErlangAtom(data[i:i + j]))
    else:
        raise ParseException('invalid atom tag')

# term_to_binary

def term_to_binary(term):
    return chr(_TAG_VERSION) + _term_to_binary(term)

def _term_to_binary(term):
    if type(term) == types.StringType:
        return _string_to_binary(term)
    elif type(term) == types.ListType:
        return _list_to_binary(term)
    elif type(term) == types.TupleType:
        return _tuple_to_binary(term)
    elif type(term) == types.LongType:
        return _long_to_binary(term)
    elif type(term) == types.FloatType:
        return _float_to_binary(term)
    elif type(term) == types.IntType:
        return _integer_to_binary(term)
    elif type(term) == types.BooleanType:
        return str(OtpErlangAtom(term and "true" or "false"))
    elif isinstance(term, OtpErlangAtom):
        return str(term)
    elif isinstance(term, OtpErlangBinary):
        return str(term)
    elif isinstance(term, OtpErlangFunction):
        return str(term)
    elif isinstance(term, OtpErlangReference):
        return str(term)
    elif isinstance(term, OtpErlangPort):
        return str(term)
    elif isinstance(term, OtpErlangPid):
        return str(term)
    else:
        raise OutputException('unknown python type')

def _string_to_binary(term):
    arity = len(term)
    if arity == 0:
        return chr(_TAG_NIL_EXT)
    elif arity < 65536:
        return chr(_TAG_STRING_EXT) + struct.pack('>H', arity) + term
    else:
        return (chr(_TAG_LIST_EXT) + struct.pack('>I', arity) +
            ''.join(map(lambda c: chr(_TAG_SMALL_INTEGER_EXT) + c, term))
        )

def _list_to_binary(term):
    arity = len(term)
    if arity == 0:
        return chr(_TAG_NIL_EXT)
    else:
        return (chr(_TAG_LIST_EXT) + struct.pack('>I', arity) +
            ''.join([_term_to_binary(element) for element in term])
        )

def _tuple_to_binary(term):
    arity = len(term)
    if arity < 256:
        return (chr(_TAG_SMALL_TUPLE_EXT) + chr(arity) +
            ''.join([_term_to_binary(element) for element in term])
        )
    else:
        return (chr(_TAG_LARGE_TUPLE_EXT) + struct.pack('>I', arity) +
            ''.join([_term_to_binary(element) for element in term])
        )

def _integer_to_binary(term):
    if 0 <= term <= 255:
        return chr(_TAG_SMALL_INTEGER_EXT) + chr(term)
    else:
        return chr(_TAG_INTEGER_EXT) + struct.pack('>i', term)

def _long_to_binary(term):
    if -2147483648L <= term <= 2147483647L:
        _integer_to_binary(term)
    else:
        _bignum_to_binary(term)

def _bignum_to_binary(term):
    bignum = abs(term)
    size = int(math.log(bignum) / math.log(256)) + 1
    if term < 0:
        sign = chr(1)
    else:
        sign = chr(0)
    L = [sign]
    for byte in range(0, size):
        L.append(chr(bignum & 255))
        bignum >>= 8
    if size < 256:
        return chr(_TAG_SMALL_BIG_EXT) + chr(size) + ''.join(L)
    else:
        return chr(_TAG_LARGE_BIG_EXT) + struct.pack('>I', size) + ''.join(L)

def _float_to_binary(term):
    return chr(_TAG_NEW_FLOAT_EXT) + struct.pack('>d', term)

# exceptions

class ParseException(SyntaxError):
    def __init__(self, s):
        self.__s = str(s)
    def __str__(self):
        return self.__s

class OutputException(TypeError):
    def __init__(self, s):
        self.__s = str(s)
    def __str__(self):
        return self.__s

# provide file:consult/1 functionality with python types
def consult(string_in):
    # manually parse textual erlang data to avoid external dependencies
    list_out = []
    tuple_binary = False   # binaries become tuples of integers
    quoted_string = False  # strings become python string
    atom_string = False    # atoms become python string
    number = False
    whitespace = frozenset(('\n', '\t', ' '))
    i = 0
    while i < len(string_in):
        c = string_in[i]
        if c == ',':
            if atom_string:
                list_out.append('"')
                atom_string = False
            list_out.append(',')
            number = string_in[i + 1].isdigit()
        elif c == '{':
            list_out.append('(')
            number = string_in[i + 1].isdigit()
        elif c == '}':
            if atom_string:
                list_out.append('"')
                atom_string = False
            list_out.append(')')
            number = False
        elif c == '[':
            list_out.append('[')
            number = string_in[i + 1].isdigit()
        elif c == ']':
            if atom_string:
                list_out.append('"')
                atom_string = False
            list_out.append(']')
            number = False
        elif c == '<' and string_in[i + 1] == '<':
            list_out.append('(')
            tuple_binary = True
            i += 1
        elif c == '>' and string_in[i + 1] == '>':
            list_out.append(')')
            tuple_binary = False
            i += 1
        elif not quoted_string and not atom_string and c in whitespace:
            number = string_in[i + 1].isdigit()
        elif tuple_binary or number:
            list_out.append(c)
        elif c == '"':
            if quoted_string:
                quoted_string = False
            else:
                quoted_string = True
            list_out.append('"')
        elif c == "'":
            if atom_string:
                atom_string = False
            else:
                atom_string = True
            list_out.append('"')
        elif not quoted_string and not atom_string:
            atom_string = True
            list_out.append('"')
            list_out.append(c)
        else:
            list_out.append(c)
        i += 1
    return eval(''.join(list_out))

