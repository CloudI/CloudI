#!/usr/bin/env python
#-*-Mode:python;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=python fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# BSD LICENSE
# 
# Copyright (c) 2011-2016, Michael Truog <mjtruog at gmail dot com>
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

import sys, struct, string, math, zlib, copy
if int(sys.version[0]) >= 3:
    long = int
    unicode = str
    def b_chr(x):
        return bytes([x])
    def b_ord(x):
        return x
else:
    b_chr = chr
    b_ord = ord
__all__ = ['OtpErlangAtom',
           'OtpErlangList',
           'OtpErlangBinary',
           'OtpErlangFunction',
           'OtpErlangReference',
           'OtpErlangPort',
           'OtpErlangPid',
           'binary_to_term',
           'term_to_binary',
           'ParseException',
           'InputException',
           'OutputException']

# tag values here http://www.erlang.org/doc/apps/erts/erl_ext_dist.html
_TAG_VERSION = 131
_TAG_COMPRESSED_ZLIB = 80
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
_TAG_MAP_EXT = 116
_TAG_FUN_EXT = 117
_TAG_ATOM_UTF8_EXT = 118
_TAG_SMALL_ATOM_UTF8_EXT = 119

class OtpErlangAtom(object):
    def __init__(self, value):
        self.value = value
    def binary(self):
        if type(self.value) == int:
            return b_chr(_TAG_ATOM_CACHE_REF) + b_chr(self.value)
        elif type(self.value) == bytes:
            size = len(self.value)
            if size < 256:
                return b_chr(_TAG_SMALL_ATOM_EXT) + b_chr(size) + self.value
            else:
                return (b_chr(_TAG_ATOM_EXT) + struct.pack(b'>H', size) +
                    self.value
                )
        elif type(self.value) == unicode:
            value_encoded = self.value.encode('utf-8')
            size = len(value_encoded)
            if size < 256:
                return (b_chr(_TAG_SMALL_ATOM_UTF8_EXT) + b_chr(size) +
                    value_encoded
                )
            else:
                return (b_chr(_TAG_ATOM_UTF8_EXT) + struct.pack(b'>H', size) +
                    value_encoded
                )
        else:
            raise OutputException('unknown atom type')
    def __repr__(self):
        return '%s(%s)' % (self.__class__.__name__, repr(self.value))
    def __hash__(self):
        return hash(self.binary())
    def __eq__(self, other):
        return self.binary() == other.binary()

class OtpErlangList(object):
    def __init__(self, value, improper = False):
        self.value = value
        self.improper = improper # no empty list tail?
    def binary(self):
        if type(self.value) == list:
            length = len(self.value)
            if length == 0:
                return b_chr(_TAG_NIL_EXT)
            elif self.improper:
                return (b_chr(_TAG_LIST_EXT) + struct.pack(b'>I', length - 1) +
                    b''.join([_term_to_binary(element)
                              for element in self.value])
                )
            else:
                return (b_chr(_TAG_LIST_EXT) + struct.pack(b'>I', length) +
                    b''.join([_term_to_binary(element)
                              for element in self.value]) +
                    b_chr(_TAG_NIL_EXT)
                )
        else:
            raise OutputException('unknown list type')
    def __repr__(self):
        return '%s(%s,improper=%s)' % (
            self.__class__.__name__, repr(self.value), repr(self.improper)
        )
    def __hash__(self):
        return hash(self.binary())
    def __eq__(self, other):
        return self.binary() == other.binary()

class OtpErlangBinary(object):
    def __init__(self, value, bits = 8):
        self.value = value
        self.bits = bits # bits in last byte
    def binary(self):
        if type(self.value) == bytes:
            size = len(self.value)
            if self.bits != 8:
                return (b_chr(_TAG_BIT_BINARY_EXT) + struct.pack(b'>I', size) +
                    b_chr(self.bits) + self.value
                )
            else:
                return (b_chr(_TAG_BINARY_EXT) + struct.pack(b'>I', size) +
                    self.value
                )
        else:
            raise OutputException('unknown binary type')
    def __repr__(self):
        return '%s(%s,bits=%s)' % (
            self.__class__.__name__, repr(self.value), repr(self.bits)
        )
    def __hash__(self):
        return hash(self.binary())
    def __eq__(self, other):
        return self.binary() == other.binary()

class OtpErlangFunction(object):
    def __init__(self, tag, value):
        self.tag = tag
        self.value = value
    def binary(self):
        return b_chr(self.tag) + self.value
    def __repr__(self):
        return '%s(%s,%s)' % (
            self.__class__.__name__,
            repr(self.tag), repr(self.value)
        )
    def __hash__(self):
        return hash(self.binary())
    def __eq__(self, other):
        return self.binary() == other.binary()

class OtpErlangReference(object):
    def __init__(self, node, id, creation):
        self.node = node
        self.id = id
        self.creation = creation
    def binary(self):
        size = len(self.id) / 4
        if size > 1:
            return (b_chr(_TAG_NEW_REFERENCE_EXT) + struct.pack(b'>H', size) +
                self.node.binary() + self.creation + self.id
            )
        else:
            return (b_chr(_TAG_REFERENCE_EXT) +
                self.node.binary() + self.id + self.creation
            )
    def __repr__(self):
        return '%s(%s,%s,%s)' % (
            self.__class__.__name__,
            repr(self.node), repr(self.id), repr(self.creation)
        )
    def __hash__(self):
        return hash(self.binary())
    def __eq__(self, other):
        return self.binary() == other.binary()

class OtpErlangPort(object):
    def __init__(self, node, id, creation):
        self.node = node
        self.id = id
        self.creation = creation
    def binary(self):
        return (b_chr(_TAG_PORT_EXT) +
            self.node.binary() + self.id + self.creation
        )
    def __repr__(self):
        return '%s(%s,%s,%s)' % (
            self.__class__.__name__,
            repr(self.node), repr(self.id), repr(self.creation)
        )
    def __hash__(self):
        return hash(self.binary())
    def __eq__(self, other):
        return self.binary() == other.binary()

class OtpErlangPid(object):
    def __init__(self, node, id, serial, creation):
        self.node = node
        self.id = id
        self.serial = serial
        self.creation = creation
    def binary(self):
        return (b_chr(_TAG_PID_EXT) +
            self.node.binary() + self.id + self.serial + self.creation
        )
    def __repr__(self):
        return '%s(%s,%s,%s,%s)' % (
            self.__class__.__name__,
            repr(self.node), repr(self.id), repr(self.serial),
            repr(self.creation)
        )
    def __hash__(self):
        return hash(self.binary())
    def __eq__(self, other):
        return self.binary() == other.binary()

# frozendict is under the PSF (Python Software Foundation) License
# (from http://code.activestate.com/recipes/414283-frozen-dictionaries/)
class frozendict(dict):
    def _blocked_attribute(obj):
        raise AttributeError('A frozendict cannot be modified.')
    _blocked_attribute = property(_blocked_attribute)
    __delitem__ = __setitem__ = clear = _blocked_attribute
    pop = popitem = setdefault = update = _blocked_attribute
    def __new__(cls, *args, **kw):
        new = dict.__new__(cls)
        args_ = []
        for arg in args:
            if isinstance(arg, dict):
                arg = copy.copy(arg)
                for k, v in arg.items():
                    if isinstance(v, dict):
                        arg[k] = frozendict(v)
                    elif isinstance(v, list):
                        v_ = list()
                        for elm in v:
                            if isinstance(elm, dict):
                                v_.append( frozendict(elm) )
                            else:
                                v_.append( elm )
                        arg[k] = tuple(v_)
                args_.append( arg )
            else:
                args_.append( arg )
        dict.__init__(new, *args_, **kw)
        return new
    def __init__(self, *args, **kw):
        pass
    def __hash__(self):
        try:
            return self._cached_hash
        except AttributeError:
            h = self._cached_hash = hash(frozenset(self.items()))
            return h
    def __repr__(self):
        return "frozendict(%s)" % dict.__repr__(self)

def binary_to_term(data):
    if type(data) != bytes:
        raise ParseException('not bytes input')
    size = len(data)
    if size <= 1:
        raise ParseException('null input')
    if b_ord(data[0]) != _TAG_VERSION:
        raise ParseException('invalid version')
    try:
        i, term = _binary_to_term(1, data)
        if i != size:
            raise ParseException('unparsed data')
        return term
    except struct.error:
        raise ParseException('missing data')
    except IndexError:
        raise ParseException('missing data')

def term_to_binary(term, compressed=False):
    data_uncompressed = _term_to_binary(term)
    if compressed is False:
        return b_chr(_TAG_VERSION) + data_uncompressed
    else:
        if compressed is True:
            compressed = 6
        if compressed < 0 or compressed > 9:
            raise InputException('compressed in [0..9]')
        data_compressed = zlib.compress(data_uncompressed, compressed)
        size_uncompressed = len(data_uncompressed)
        return (
            b_chr(_TAG_VERSION) + b_chr(_TAG_COMPRESSED_ZLIB) +
            struct.pack(b'>I', size_uncompressed) + data_compressed
        )

def _binary_to_term(i, data):
    tag = b_ord(data[i])
    i += 1
    if tag == _TAG_NEW_FLOAT_EXT:
        return (i + 8, struct.unpack(b'>d', data[i:i + 8])[0])
    elif tag == _TAG_BIT_BINARY_EXT:
        j = struct.unpack(b'>I', data[i:i + 4])[0]
        i += 4
        bits = b_ord(data[i])
        i += 1
        return (i + j, OtpErlangBinary(data[i:i + j], bits))
    elif tag == _TAG_ATOM_CACHE_REF:
        return (i + 1, OtpErlangAtom(b_ord(data[i:i + 1])))
    elif tag == _TAG_SMALL_INTEGER_EXT:
        return (i + 1, b_ord(data[i]))
    elif tag == _TAG_INTEGER_EXT:
        return (i + 4, struct.unpack(b'>i', data[i:i + 4])[0])
    elif tag == _TAG_FLOAT_EXT:
        value = float(data[i:i + 31].partition(b_chr(0))[0])
        return (i + 31, value)
    elif tag == _TAG_ATOM_EXT:
        j = struct.unpack(b'>H', data[i:i + 2])[0]
        i += 2
        return (i + j, OtpErlangAtom(data[i:i + j]))
    elif tag == _TAG_REFERENCE_EXT or tag == _TAG_PORT_EXT:
        i, node = _binary_to_atom(i, data)
        id = data[i:i + 4]
        i += 4
        creation = data[i:i + 1]
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
        creation = data[i:i + 1]
        i += 1
        return (i, OtpErlangPid(node, id, serial, creation))
    elif tag == _TAG_SMALL_TUPLE_EXT or tag == _TAG_LARGE_TUPLE_EXT:
        if tag == _TAG_SMALL_TUPLE_EXT:
            arity = b_ord(data[i])
            i += 1
        elif tag == _TAG_LARGE_TUPLE_EXT:
            arity = struct.unpack(b'>I', data[i:i + 4])[0]
            i += 4
        i, tmp = _binary_to_term_sequence(i, arity, data)
        return (i, tuple(tmp))
    elif tag == _TAG_NIL_EXT:
        return (i, [])
    elif tag == _TAG_STRING_EXT:
        j = struct.unpack(b'>H', data[i:i + 2])[0]
        i += 2
        return (i + j, data[i:i + j])
    elif tag == _TAG_LIST_EXT:
        arity = struct.unpack(b'>I', data[i:i + 4])[0]
        i += 4
        i, tmp = _binary_to_term_sequence(i, arity, data)
        i, tail = _binary_to_term(i, data)
        if type(tail) != list or tail != []: 
            tmp.append(tail)
            tmp = OtpErlangList(tmp, improper=True)
        return (i, tmp)
    elif tag == _TAG_BINARY_EXT:
        j = struct.unpack(b'>I', data[i:i + 4])[0]
        i += 4
        return (i + j, OtpErlangBinary(data[i:i + j], 8))
    elif tag == _TAG_SMALL_BIG_EXT or tag == _TAG_LARGE_BIG_EXT:
        if tag == _TAG_SMALL_BIG_EXT:
            j = b_ord(data[i])
            i += 1
        elif tag == _TAG_LARGE_BIG_EXT:
            j = struct.unpack(b'>I', data[i:i + 4])[0]
            i += 4
        sign = b_ord(data[i])
        bignum = 0
        for bignum_index in range(j):
            digit = b_ord(data[i + j - bignum_index])
            bignum = bignum * 256 + int(digit)
        if sign == 1:
            bignum *= -1
        i += 1
        return (i + j, bignum)
    elif tag == _TAG_NEW_FUN_EXT:
        size = struct.unpack(b'>I', data[i:i + 4])[0]
        return (i + size, OtpErlangFunction(tag, data[i:i + size]))
    elif tag == _TAG_EXPORT_EXT:
        old_i = i
        i, module = _binary_to_atom(i, data)
        i, function = _binary_to_atom(i, data)
        if b_ord(data[i]) != _TAG_SMALL_INTEGER_EXT:
            raise ParseException('invalid small integer tag')
        i += 1
        arity = b_ord(data[i])
        i += 1
        return (i, OtpErlangFunction(tag, data[old_i:i]))
    elif tag == _TAG_NEW_REFERENCE_EXT:
        j = struct.unpack(b'>H', data[i:i + 2])[0] * 4
        i += 2
        i, node = _binary_to_atom(i, data)
        creation = data[i:i + 1]
        i += 1
        return (i + j, OtpErlangReference(node, data[i: i + j], creation))
    elif tag == _TAG_SMALL_ATOM_EXT:
        j = b_ord(data[i])
        i += 1
        atom_name = data[i:i + j]
        if atom_name == b'true':
            tmp = True
        elif atom_name == b'false':
            tmp = False
        else:
            tmp = OtpErlangAtom(atom_name)
        return (i + j, tmp)
    elif tag == _TAG_MAP_EXT:
        arity = struct.unpack(b'>I', data[i:i + 4])[0]
        i += 4
        pairs = {}
        for arity_index in range(arity):
            i, key = _binary_to_term(i, data)
            i, value = _binary_to_term(i, data)
            if type(key) == dict:
                pairs[frozendict(key)] = value
            elif type(key) == list:
                pairs[OtpErlangList(key)] = value
            else:
                pairs[key] = value
        return (i, pairs)
    elif tag == _TAG_FUN_EXT:
        old_i = i
        numfree = struct.unpack(b'>I', data[i:i + 4])[0]
        i += 4
        i, pid = _binary_to_pid(i, data)
        i, module = _binary_to_atom(i, data)
        i, index = _binary_to_integer(i, data)
        i, uniq = _binary_to_integer(i, data)
        i, free = _binary_to_term_sequence(i, numfree, data)
        return (i, OtpErlangFunction(tag, data[old_i:i]))
    elif tag == _TAG_ATOM_UTF8_EXT:
        j = struct.unpack(b'>H', data[i:i + 2])[0]
        i += 2
        atom_name = unicode(data[i:i + j], encoding='utf-8', errors='strict') 
        return (i + j, OtpErlangAtom(atom_name))
    elif tag == _TAG_SMALL_ATOM_UTF8_EXT:
        j = b_ord(data[i:i + 1])
        i += 1
        atom_name = unicode(data[i:i + j], encoding='utf-8', errors='strict')
        return (i + j, OtpErlangAtom(atom_name))
    elif tag == _TAG_COMPRESSED_ZLIB:
        size_uncompressed = struct.unpack(b'>I', data[i:i + 4])[0]
        if size_uncompressed == 0:
            raise ParseException('compressed data null')
        i += 4
        data_compressed = data[i:]
        j = len(data_compressed)
        data_uncompressed = zlib.decompress(data_compressed)
        if size_uncompressed != len(data_uncompressed):
            raise ParseException('compression corrupt')
        (i_new, term) = _binary_to_term(0, data_uncompressed)
        if i_new != size_uncompressed:
            raise ParseException('unparsed data')
        return (i + j, term)
    else:
        raise ParseException('invalid tag')

def _binary_to_term_sequence(i, arity, data):
    sequence = []
    for arity_index in range(arity):
        i, element = _binary_to_term(i, data)
        sequence.append(element)
    return (i, sequence)
        
def _binary_to_integer(i, data):
    tag = b_ord(data[i])
    i += 1
    if tag == _TAG_SMALL_INTEGER_EXT:
        return (i + 1, b_ord(data[i]))
    elif tag == _TAG_INTEGER_EXT:
        return (i + 4, struct.unpack(b'>i', data[i:i + 4])[0])
    else:
        raise ParseException('invalid integer tag')

def _binary_to_pid(i, data):
    tag = b_ord(data[i])
    i += 1
    if tag == _TAG_PID_EXT:
        i, node = _binary_to_atom(i, data)
        id = data[i:i + 4]
        i += 4
        serial = data[i:i + 4]
        i += 4
        creation = data[i:i + 1]
        i += 1
        return (i, OtpErlangPid(node, id, serial, creation))
    else:
        raise ParseException('invalid pid tag')

def _binary_to_atom(i, data):
    tag = b_ord(data[i])
    i += 1
    if tag == _TAG_ATOM_EXT:
        j = struct.unpack(b'>H', data[i:i + 2])[0]
        i += 2
        return (i + j, OtpErlangAtom(data[i:i + j]))
    elif tag == _TAG_ATOM_CACHE_REF:
        return (i + 1, OtpErlangAtom(b_ord(data[i:i + 1])))
    elif tag == _TAG_SMALL_ATOM_EXT:
        j = b_ord(data[i:i + 1])
        i += 1
        return (i + j, OtpErlangAtom(data[i:i + j]))
    elif tag == _TAG_ATOM_UTF8_EXT:
        j = struct.unpack(b'>H', data[i:i + 2])[0]
        i += 2
        atom_name = unicode(data[i:i + j], encoding='utf-8', errors='strict') 
        return (i + j, OtpErlangAtom(atom_name))
    elif tag == _TAG_SMALL_ATOM_UTF8_EXT:
        j = b_ord(data[i:i + 1])
        i += 1
        atom_name = unicode(data[i:i + j], encoding='utf-8', errors='strict')
        return (i + j, OtpErlangAtom(atom_name))
    else:
        raise ParseException('invalid atom tag')

def _term_to_binary(term):
    if type(term) == bytes:
        return _string_to_binary(term)
    elif type(term) == unicode:
        return _string_to_binary(term.encode(encoding='utf-8', errors='strict'))
    elif type(term) == list:
        return OtpErlangList(term).binary()
    elif type(term) == tuple:
        return _tuple_to_binary(term)
    elif type(term) == int or type(term) == long:
        return _long_to_binary(term)
    elif type(term) == float:
        return _float_to_binary(term)
    elif type(term) == dict:
        return _dict_to_binary(term)
    elif type(term) == bool:
        return OtpErlangAtom(term and b'true' or b'false').binary()
    elif isinstance(term, OtpErlangAtom):
        return term.binary()
    elif isinstance(term, OtpErlangList):
        return term.binary()
    elif isinstance(term, OtpErlangBinary):
        return term.binary()
    elif isinstance(term, OtpErlangFunction):
        return term.binary()
    elif isinstance(term, OtpErlangReference):
        return term.binary()
    elif isinstance(term, OtpErlangPort):
        return term.binary()
    elif isinstance(term, OtpErlangPid):
        return term.binary()
    else:
        raise OutputException('unknown python type')

def _string_to_binary(term):
    arity = len(term)
    if arity == 0:
        return b_chr(_TAG_NIL_EXT)
    elif arity < 65536:
        return b_chr(_TAG_STRING_EXT) + struct.pack(b'>H', arity) + term
    else:
        return (b_chr(_TAG_LIST_EXT) + struct.pack(b'>I', arity) +
            b''.join([b_chr(_TAG_SMALL_INTEGER_EXT) + b_chr(b_ord(c))
                      for c in term]) +
            b_chr(_TAG_NIL_EXT)
        )

def _tuple_to_binary(term):
    arity = len(term)
    if arity < 256:
        return (b_chr(_TAG_SMALL_TUPLE_EXT) + b_chr(arity) +
            b''.join([_term_to_binary(element) for element in term])
        )
    else:
        return (b_chr(_TAG_LARGE_TUPLE_EXT) + struct.pack(b'>I', arity) +
            b''.join([_term_to_binary(element) for element in term])
        )

def _integer_to_binary(term):
    if 0 <= term <= 255:
        return b_chr(_TAG_SMALL_INTEGER_EXT) + b_chr(term)
    else:
        return b_chr(_TAG_INTEGER_EXT) + struct.pack(b'>i', term)

def _long_to_binary(term):
    if -2147483648 <= term <= 2147483647:
        return _integer_to_binary(term)
    else:
        return _bignum_to_binary(term)

def _bignum_to_binary(term):
    bignum = abs(term)
    size = int(math.ceil(_bignum_bit_length(bignum) / 8.0))
    if term < 0:
        sign = b_chr(1)
    else:
        sign = b_chr(0)
    L = [sign]
    for byte in range(0, size):
        L.append(b_chr(bignum & 255))
        bignum >>= 8
    if size < 256:
        return (b_chr(_TAG_SMALL_BIG_EXT) +
            b_chr(size) + b''.join(L)
        )
    else:
        return (b_chr(_TAG_LARGE_BIG_EXT) +
            struct.pack(b'>I', size) + b''.join(L)
        )

def _bignum_bit_length(bignum):
    return len(bin(bignum).lstrip('-0b'))

def _float_to_binary(term):
    return b_chr(_TAG_NEW_FLOAT_EXT) + struct.pack(b'>d', term)

def _dict_to_binary(term):
    arity = len(term)
    return (b_chr(_TAG_MAP_EXT) + struct.pack(b'>I', arity) +
        b''.join([_term_to_binary(key) + _term_to_binary(value)
                  for key, value in term.items()])
    )

# exceptions

class ParseException(SyntaxError):
    def __init__(self, s):
        self.__s = str(s)
    def __str__(self):
        return self.__s

class InputException(ValueError):
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

