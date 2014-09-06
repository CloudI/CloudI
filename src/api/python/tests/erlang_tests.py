#!/usr/bin/env python
#-*-Mode:python;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=python fenc=utf-8 sts=4 ts=4 sw=4 et:
#
# BSD LICENSE
# 
# Copyright (c) 2014, Michael Truog <mjtruog at gmail dot com>
# Copyright (c) 2009-2013, Dmitry Vasiliev <dima@hlabs.org>
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

import sys, os
sys.path.append(
    os.path.sep.join(
        os.path.dirname(os.path.abspath(__file__))
               .split(os.path.sep)[:-1]
    )
)
import unittest
import erlang

# many of the test cases were adapted
# from erlport (https://github.com/hdima/erlport)
# to make the tests more exhaustive

class AtomTestCase(unittest.TestCase):
    def test_atom(self):
        atom1 = erlang.OtpErlangAtom('test')
        self.assertEqual(erlang.OtpErlangAtom, type(atom1))
        self.assertEqual(erlang.OtpErlangAtom('test'), atom1)
        self.assertEqual("OtpErlangAtom('test')", repr(atom1))
        self.assertTrue(type(atom1) is erlang.OtpErlangAtom)
        atom2 = erlang.OtpErlangAtom('test2')
        atom1_new = erlang.OtpErlangAtom('test')
        self.assertFalse(atom1 is atom2)
        self.assertNotEqual(hash(atom1), hash(atom2))
        self.assertFalse(atom1 is atom1_new)
        self.assertEqual(hash(atom1), hash(atom1_new))
        self.assertEqual('X' * 255, erlang.OtpErlangAtom('X' * 255).value)
        self.assertEqual('X' * 256, erlang.OtpErlangAtom('X' * 256).value)

    def test_invalid_atom(self):
        self.assertRaises(erlang.OutputException,
                          str, erlang.OtpErlangAtom([1, 2]))

class ListTestCase(unittest.TestCase):
    def test_list(self):
        lst = erlang.OtpErlangList([116, 101, 115, 116])
        self.assertTrue(type(lst) is erlang.OtpErlangList)
        self.assertEqual(erlang.OtpErlangList([116, 101, 115, 116]), lst)
        self.assertEqual([116, 101, 115, 116], lst.value)
        self.assertEqual("OtpErlangList([116, 101, 115, 116])", repr(lst))

class ImproperListTestCase(unittest.TestCase):
    def test_improper_list(self):
        lst = erlang.OtpErlangList([1, 2, 3, 4], improper=True)
        self.assertTrue(type(lst) is erlang.OtpErlangList)
        self.assertEqual([1, 2, 3, 4], lst.value)
        self.assertEqual(4, lst.value[-1])
        self.assertEqual('OtpErlangList([1, 2, 3, 4])', repr(lst))

    def test_comparison(self):
        lst = erlang.OtpErlangList([1, 2, 3, 4])
        self.assertEqual(lst, lst)
        self.assertEqual(lst, erlang.OtpErlangList([1, 2, 3, 4]))
        self.assertNotEqual(lst, erlang.OtpErlangList([1, 2, 3, 5]))
        self.assertNotEqual(lst, erlang.OtpErlangList([1, 2, 3]))

    def test_errors(self):
        self.assertRaises(erlang.OutputException,
                          str, erlang.OtpErlangList("invalid"))

class DecodeTestCase(unittest.TestCase):
    def test_decode(self):
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\0')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83z')

    def test_decode_atom(self):
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83d')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83d\0')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83d\0\1')
        self.assertEqual(erlang.OtpErlangAtom(''),
                         erlang.binary_to_term('\x83s\x00'))
        self.assertEqual(erlang.OtpErlangAtom('test'),
                         erlang.binary_to_term('\x83s\x04test'))

    def test_decode_predefined_atoms(self):
        self.assertEqual(erlang.OtpErlangAtom('true'),
                         erlang.binary_to_term('\x83d\0\4true'))
        self.assertEqual(erlang.OtpErlangAtom('false'),
                         erlang.binary_to_term('\x83d\0\5false'))
        self.assertEqual(erlang.OtpErlangAtom('undefined'),
                         erlang.binary_to_term('\x83d\0\11undefined'))

    def test_decode_empty_list(self):
        self.assertEqual([], erlang.binary_to_term('\x83j'))

    def test_decode_string_list(self):
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83k')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83k\0')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83k\0\1')
        self.assertEqual('', erlang.binary_to_term('\x83k\0\0'))
        self.assertEqual('test', erlang.binary_to_term('\x83k\0\4test'))

    def test_decode_list(self):
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83l')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83l\0')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83l\0\0')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83l\0\0\0')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83l\0\0\0\0')
        self.assertEqual([], erlang.binary_to_term('\x83l\0\0\0\0j'))
        self.assertEqual([[], []], erlang.binary_to_term('\x83l\0\0\0\2jjj'))

    def test_decode_improper_list(self):
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83l\0\0\0\0k')
        lst = erlang.binary_to_term('\x83l\0\0\0\1jd\0\4tail')
        self.assertEqual(erlang.OtpErlangList, type(lst))
        self.assertEqual([[], erlang.OtpErlangAtom('tail')], lst.value)

    def test_decode_small_tuple(self):
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83h')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83h\1')
        self.assertEqual((), erlang.binary_to_term('\x83h\0'))
        self.assertEqual(([], []), erlang.binary_to_term("\x83h\2jj"))

    def test_decode_large_tuple(self):
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83i')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83i\0')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83i\0\0')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83i\0\0\0')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83i\0\0\0\1')
        self.assertEqual((), erlang.binary_to_term('\x83i\0\0\0\0'))
        self.assertEqual(([], []), erlang.binary_to_term("\x83i\0\0\0\2jj"))

    def test_decode_small_integer(self):
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83a')
        self.assertEqual(0, erlang.binary_to_term('\x83a\0'))
        self.assertEqual(255, erlang.binary_to_term('\x83a\xff'))

    def test_decode_integer(self):
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83b')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83b\0')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83b\0\0')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83b\0\0\0')
        self.assertEqual(0, erlang.binary_to_term('\x83b\0\0\0\0'))
        self.assertEqual(2147483647,
                         erlang.binary_to_term('\x83b\x7f\xff\xff\xff'))
        self.assertEqual(-1, erlang.binary_to_term('\x83b\xff\xff\xff\xff'))

    def test_decode_binary(self):
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83m')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83m\0')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83m\0\0')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83m\0\0\0')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83m\0\0\0\1')
        self.assertEqual(erlang.OtpErlangBinary(''),
                         erlang.binary_to_term('\x83m\0\0\0\0'))
        self.assertEqual(erlang.OtpErlangBinary('data'),
                         erlang.binary_to_term('\x83m\0\0\0\4data'))

    def test_erlang_binary_to_term_float(self):
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83F')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83F\0')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83F\0\0')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83F\0\0\0')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83F\0\0\0\0')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83F\0\0\0\0\0')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83F\0\0\0\0\0\0')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83F\0\0\0\0\0\0\0')
        self.assertEqual(0.0, erlang.binary_to_term('\x83F\0\0\0\0\0\0\0\0'))
        self.assertEqual(1.5, erlang.binary_to_term('\x83F?\xf8\0\0\0\0\0\0'))

    def test_erlang_binary_to_term_small_big_integer(self):
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83n')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83n\0')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83n\1\0')
        self.assertEqual(0, erlang.binary_to_term('\x83n\0\0'))
        self.assertEqual(6618611909121,
                         erlang.binary_to_term('\x83n\6\0\1\2\3\4\5\6'))
        self.assertEqual(-6618611909121,
                         erlang.binary_to_term('\x83n\6\1\1\2\3\4\5\6'))

    def test_erlang_binary_to_term_big_integer(self):
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83o')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83o\0')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83o\0\0')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83o\0\0\0')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83o\0\0\0\0')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83o\0\0\0\1\0')
        self.assertEqual(0, erlang.binary_to_term('\x83o\0\0\0\0\0'))
        self.assertEqual(6618611909121,
                         erlang.binary_to_term('\x83o\0\0\0\6\0\1\2\3\4\5\6'))
        self.assertEqual(-6618611909121,
                         erlang.binary_to_term('\x83o\0\0\0\6\1\1\2\3\4\5\6'))

    def test_erlang_binary_to_term_compressed_term(self):
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83P')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83P\0')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83P\0\0')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83P\0\0\0')
        self.assertRaises(erlang.ParseException,
                          erlang.binary_to_term, '\x83P\0\0\0\0')
        self.assertRaises(
            erlang.ParseException, erlang.binary_to_term,
            '\x83P\0\0\0\x16\x78\xda\xcb\x66\x10\x49\xc1\2\0\x5d\x60\x08\x50'
        )
        self.assertEqual('d' * 20,
            erlang.binary_to_term('\x83P\0\0\0\x17\x78\xda\xcb\x66'
                                  '\x10\x49\xc1\2\0\x5d\x60\x08\x50')
        )

class EncodeTestCase(unittest.TestCase):
    def test_erlang_term_to_binary_tuple(self):
        self.assertEqual('\x83h\0', erlang.term_to_binary(()))
        self.assertEqual('\x83h\2h\0h\0', erlang.term_to_binary(((), ())))
        self.assertEqual('\x83h\xff' + 'h\0' * 255,
                         erlang.term_to_binary(tuple([()] * 255)))
        self.assertEqual('\x83i\0\0\1\0' + 'h\0' * 256,
                         erlang.term_to_binary(tuple([()] * 256)))

    def test_erlang_term_to_binary_empty_list(self):
        self.assertEqual('\x83j', erlang.term_to_binary([]))

    def test_erlang_term_to_binary_string_list(self):
        self.assertEqual('\x83k\0\1\0', erlang.term_to_binary('\0'))
        s = ''.join(map(chr, range(0, 256)))
        self.assertEqual('\x83k\1\0' + s, erlang.term_to_binary(s))

    def test_erlang_term_to_binary_list_basic(self):
        self.assertEqual('\x83\x6A', erlang.term_to_binary([]))
        self.assertEqual('\x83\x6C\x00\x00\x00\x01\x6A\x6A',
                         erlang.term_to_binary(['']))
        self.assertEqual('\x83\x6C\x00\x00\x00\x01\x61\x01\x6A',
                         erlang.term_to_binary([1]))
        self.assertEqual('\x83\x6C\x00\x00\x00\x01\x61\xFF\x6A',
                         erlang.term_to_binary([255]))
        self.assertEqual('\x83\x6C\x00\x00\x00\x01\x62\x00\x00\x01\x00\x6A',
                         erlang.term_to_binary([256]))
        self.assertEqual('\x83\x6C\x00\x00\x00\x01\x62\x7F\xFF\xFF\xFF\x6A',
                         erlang.term_to_binary([2147483647]))
        self.assertEqual(
            '\x83\x6C\x00\x00\x00\x01\x6E\x04\x00\x00\x00\x00\x80\x6A',
            erlang.term_to_binary([2147483648])
        )
        self.assertEqual('\x83\x6C\x00\x00\x00\x01\x61\x00\x6A',
                         erlang.term_to_binary([0]))
        self.assertEqual('\x83\x6C\x00\x00\x00\x01\x62\xFF\xFF\xFF\xFF\x6A',
                         erlang.term_to_binary([-1]))
        self.assertEqual('\x83\x6C\x00\x00\x00\x01\x62\xFF\xFF\xFF\x00\x6A',
                         erlang.term_to_binary([-256]))
        self.assertEqual('\x83\x6C\x00\x00\x00\x01\x62\xFF\xFF\xFE\xFF\x6A',
                         erlang.term_to_binary([-257]))
        self.assertEqual('\x83\x6C\x00\x00\x00\x01\x62\x80\x00\x00\x00\x6A',
                         erlang.term_to_binary([-2147483648]))
        self.assertEqual(   
            '\x83\x6C\x00\x00\x00\x01\x6E\x04\x01\x01\x00\x00\x80\x6A',
            erlang.term_to_binary([-2147483649])
        )
        self.assertEqual(
            '\x83\x6C\x00\x00\x00\x01\x6B\x00\x04\x74\x65\x73\x74\x6A',
            erlang.term_to_binary(['test'])
        )
        self.assertEqual(
            '\x83\x6C\x00\x00\x00\x02\x62\x00\x00\x01\x75\x62\x00\x00'
            '\x01\xC7\x6A',
            erlang.term_to_binary([373, 455])
        )
        self.assertEqual('\x83\x6C\x00\x00\x00\x01\x6A\x6A',
                         erlang.term_to_binary([[]]))
        self.assertEqual('\x83\x6C\x00\x00\x00\x02\x6A\x6A\x6A',
                         erlang.term_to_binary([[], []]))
        self.assertEqual(
            '\x83\x6C\x00\x00\x00\x03\x6C\x00\x00\x00\x02\x6B\x00\x04\x74\x68'
            '\x69\x73\x6B\x00\x02\x69\x73\x6A\x6C\x00\x00\x00\x01\x6C\x00\x00'
            '\x00\x01\x6B\x00\x01\x61\x6A\x6A\x6B\x00\x04\x74\x65\x73\x74\x6A',
            erlang.term_to_binary([['this', 'is'], [['a']], 'test'])
        )

    def test_erlang_term_to_binary_list(self):
        self.assertEqual('\x83l\0\0\0\1jj', erlang.term_to_binary([[]]))
        self.assertEqual('\x83l\0\0\0\5jjjjjj',
                         erlang.term_to_binary([[], [], [], [], []]))
        self.assertEqual('\x83l\0\0\0\5jjjjjj',
            erlang.term_to_binary(erlang.OtpErlangList([
                erlang.OtpErlangList([]),
                erlang.OtpErlangList([]),
                erlang.OtpErlangList([]),
                erlang.OtpErlangList([]),
                erlang.OtpErlangList([])]
            ))
        )

    def test_erlang_term_to_binary_improper_list(self):
        self.assertEqual('\x83l\0\0\0\1h\0h\0',
            erlang.term_to_binary(
                erlang.OtpErlangList([(), ()], improper=True)
            )
        )
        self.assertEqual('\x83l\0\0\0\1a\0a\1',
            erlang.term_to_binary(
                erlang.OtpErlangList([0, 1], improper=True)
            )
        )

    def test_erlang_term_to_binary_unicode(self):
        self.assertEqual('\x83j', erlang.term_to_binary(u''))
        self.assertEqual('\x83k\0\4test', erlang.term_to_binary(u'test'))
        self.assertEqual('\x83k\0\3\x00\xc3\xbf',
                         erlang.term_to_binary(u'\0\xff'))
        self.assertEqual('\x83k\0\2\xc4\x80',
                         erlang.term_to_binary(u'\u0100'))
        self.assertEqual('\x83k\0\x08\xd1\x82\xd0\xb5\xd1\x81\xd1\x82',
            erlang.term_to_binary(u'\u0442\u0435\u0441\u0442')
        )
        self.assertEqual(u'\u0410'.encode(encoding='utf-8', errors='strict'),
                         '\xd0\x90')
        s = u'\u0410' * 65536 # becomes a list of small integers
        self.assertEqual(
            '\x83l\x00\x02\x00\x00' + ('a\xd0a\x90' * 65536),
            erlang.term_to_binary(u'\u0410' * 65536)
        )

    def test_erlang_term_to_binary_atom(self):
        self.assertEqual('\x83s\0',
                         erlang.term_to_binary(erlang.OtpErlangAtom('')))
        self.assertEqual('\x83s\4test',
            erlang.term_to_binary(erlang.OtpErlangAtom('test'))
        )

    def test_erlang_term_to_binary_string_basic(self):
        self.assertEqual('\x83\x6A', erlang.term_to_binary(''))
        self.assertEqual('\x83\x6B\x00\x04\x74\x65\x73\x74',
                         erlang.term_to_binary('test'))
        self.assertEqual(
            '\x83\x6B\x00\x09\x74\x77\x6F\x20\x77\x6F\x72\x64\x73',
            erlang.term_to_binary('two words')
        )
        self.assertEqual(
            '\x83\x6B\x00\x16\x74\x65\x73\x74\x69\x6E\x67\x20\x6D\x75\x6C\x74'
            '\x69\x70\x6C\x65\x20\x77\x6F\x72\x64\x73',
            erlang.term_to_binary('testing multiple words')
        )
        self.assertEqual('\x83\x6B\x00\x01\x20',
                         erlang.term_to_binary(' '))
        self.assertEqual('\x83\x6B\x00\x02\x20\x20',
                         erlang.term_to_binary('  '))
        self.assertEqual('\x83\x6B\x00\x01\x31',
                         erlang.term_to_binary('1'))
        self.assertEqual('\x83\x6B\x00\x02\x33\x37',
                         erlang.term_to_binary('37'))
        self.assertEqual('\x83\x6B\x00\x07\x6F\x6E\x65\x20\x3D\x20\x31',
                         erlang.term_to_binary('one = 1'))
        self.assertEqual(
            '\x83\x6B\x00\x20\x21\x40\x23\x24\x25\x5E\x26\x2A\x28\x29\x5F\x2B'
            '\x2D\x3D\x5B\x5D\x7B\x7D\x5C\x7C\x3B\x27\x3A\x22\x2C\x2E\x2F\x3C'
            '\x3E\x3F\x7E\x60',
            erlang.term_to_binary('!@#$%^&*()_+-=[]{}\\|;\':",./<>?~`')
        )
        self.assertEqual(
            '\x83\x6B\x00\x09\x22\x08\x0C\x0A\x0D\x09\x0B\x53\x12',
            erlang.term_to_binary('\"\b\f\n\r\t\v\123\x12')
        )

    def test_erlang_term_to_binary_string(self):
        self.assertEqual('\x83j', erlang.term_to_binary(''))
        self.assertEqual('\x83k\0\4test', erlang.term_to_binary('test'))

    def test_erlang_term_to_binary_boolean(self):
        self.assertEqual('\x83s\4true', erlang.term_to_binary(True))
        self.assertEqual('\x83s\5false', erlang.term_to_binary(False))

    def test_erlang_term_to_binary_short_integer(self):
        self.assertEqual('\x83a\0', erlang.term_to_binary(0))
        self.assertEqual('\x83a\xff', erlang.term_to_binary(255))

    def test_erlang_term_to_binary_integer(self):
        self.assertEqual('\x83b\xff\xff\xff\xff', erlang.term_to_binary(-1))
        self.assertEqual('\x83b\x80\0\0\0',
                         erlang.term_to_binary(-2147483648))
        self.assertEqual('\x83b\0\0\1\0', erlang.term_to_binary(256))
        self.assertEqual('\x83b\x7f\xff\xff\xff',
                         erlang.term_to_binary(2147483647))

    def test_erlang_term_to_binary_long_integer(self):
        self.assertEqual('\x83n\4\0\0\0\0\x80',
                         erlang.term_to_binary(2147483648))
        self.assertEqual('\x83n\4\1\1\0\0\x80',
                         erlang.term_to_binary(-2147483649))
        self.assertEqual('\x83o\0\0\1\0\0' + '\0' * 255 + '\1',
                         erlang.term_to_binary(2 ** 2040))
        self.assertEqual('\x83o\0\0\1\0\1' + '\0' * 255 + '\1',
                         erlang.term_to_binary(-2 ** 2040))

    def test_erlang_term_to_binary_float(self):
        self.assertEqual('\x83F\0\0\0\0\0\0\0\0', erlang.term_to_binary(0.0))
        self.assertEqual('\x83F?\xe0\0\0\0\0\0\0', erlang.term_to_binary(0.5))
        self.assertEqual('\x83F\xbf\xe0\0\0\0\0\0\0',
                         erlang.term_to_binary(-0.5))
        self.assertEqual('\x83F@\t!\xfbM\x12\xd8J',
                         erlang.term_to_binary(3.1415926))
        self.assertEqual('\x83F\xc0\t!\xfbM\x12\xd8J',
                         erlang.term_to_binary(-3.1415926))

    def test_erlang_term_to_binary_compressed_term(self):
        self.assertEqual('\x83P\x00\x00\x00\x15'
                         'x\x9c\xcba``\xe0\xcfB\x03\x00B@\x07\x1c',
                         erlang.term_to_binary([[]] * 15, compressed=True))
        self.assertEqual('\x83P\x00\x00\x00\x15'
                         'x\x9c\xcba``\xe0\xcfB\x03\x00B@\x07\x1c',
                         erlang.term_to_binary([[]] * 15, compressed=6))
        self.assertEqual('\x83P\x00\x00\x00\x15'
                         'x\xda\xcba``\xe0\xcfB\x03\x00B@\x07\x1c',
                         erlang.term_to_binary([[]] * 15, compressed=9))
        self.assertEqual('\x83P\x00\x00\x00\x15'
                         'x\x01\x01\x15\x00\xea\xffl\x00\x00\x00'
                         '\x0fjjjjjjjjjjjjjjjjB@\x07\x1c',
                         erlang.term_to_binary([[]] * 15, compressed=0))
        self.assertEqual('\x83P\x00\x00\x00\x15'
                         'x\x01\xcba``\xe0\xcfB\x03\x00B@\x07\x1c',
                         erlang.term_to_binary([[]] * 15, 1))
        self.assertEqual('\x83P\0\0\0\x17\x78\xda\xcb\x66'
                         '\x10\x49\xc1\2\0\x5d\x60\x08\x50',
                         erlang.term_to_binary('d' * 20, compressed=9))

if __name__ == '__main__':
    try:
        import coverage
    except ImportError:
        coverage = None

    if coverage is None:
        unittest.main()
    else:
        cov = coverage.coverage()
        cov.start()
        unittest.main()
        cov.stop()
        cov.save()
        modules = [erlang.__file__, __file__]
        cov.report(morfs=modules, show_missing=False)
        cov.html_report(morfs=modules, directory='.cover')

