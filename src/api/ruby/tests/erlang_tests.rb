#!/usr/bin/env ruby
#-*-Mode:ruby;coding:binary;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=ruby fenc=binary sts=4 ts=4 sw=4 et nomod:
#
# MIT License
#
# Copyright (c) 2014-2019 Michael Truog <mjtruog at protonmail dot com>
# Copyright (c) 2009-2013 Dmitry Vasiliev <dima@hlabs.org>
#
# Permission is hereby granted, free of charge, to any person obtaining a
# copy of this software and associated documentation files (the "Software"),
# to deal in the Software without restriction, including without limitation
# the rights to use, copy, modify, merge, publish, distribute, sublicense,
# and/or sell copies of the Software, and to permit persons to whom the
# Software is furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
# FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
# DEALINGS IN THE SOFTWARE.
#

path = File.split(File.dirname(__FILE__)); path.pop(1)
$:.unshift File.join(*path)
require 'test/unit'
require 'erlang'

# many of the test cases were adapted
# from erlport (https://github.com/hdima/erlport)
# to make the tests more exhaustive

class AtomTestCase < Test::Unit::TestCase
    def test_atom
        atom1 = Erlang::OtpErlangAtom.new('test')
        assert(atom1.kind_of?(Erlang::OtpErlangAtom))
        assert_equal(Erlang::OtpErlangAtom.new('test'), atom1)
        assert_equal("Erlang::OtpErlangAtom('test')", atom1.to_s)
        atom2 = Erlang::OtpErlangAtom.new('test2')
        atom1_new = Erlang::OtpErlangAtom.new('test')
        assert_not_equal(atom1, atom2)
        assert_not_equal(atom1.hash, atom2.hash)
        assert_equal(atom1, atom1_new)
        assert_equal(atom1.hash, atom1_new.hash)
        assert_equal('X' * 255, Erlang::OtpErlangAtom.new('X' * 255).value)
        assert_equal('X' * 256, Erlang::OtpErlangAtom.new('X' * 256).value)
    end
    def test_invalid_atom
        assert_raise(Erlang::OutputException){
            Erlang::OtpErlangAtom.new([1, 2]).binary
        }
    end
end

class ListTestCase < Test::Unit::TestCase
    def test_list
        lst = Erlang::OtpErlangList.new([116, 101, 115, 116])
        assert(lst.kind_of?(Erlang::OtpErlangList))
        assert_equal(Erlang::OtpErlangList.new([116, 101, 115, 116]), lst)
        assert_equal([116, 101, 115, 116], lst.value)
        assert_equal('Erlang::OtpErlangList([116, 101, 115, 116],' \
                                           'improper=false)', lst.to_s)
    end
end

class ImproperListTestCase < Test::Unit::TestCase
    def test_improper_list
        lst = Erlang::OtpErlangList.new([1, 2, 3, 4], true)
        assert(lst.kind_of?(Erlang::OtpErlangList))
        assert_equal(Erlang::OtpErlangList.new([1, 2, 3, 4]).value, lst.value)
        assert_equal(4, lst.value[-1])
        assert_equal('Erlang::OtpErlangList([1, 2, 3, 4],' \
                                           'improper=true)', lst.to_s)
    end
    def test_comparison
        lst = Erlang::OtpErlangList.new([1, 2, 3, 4], improper=true)
        assert_equal(lst, lst)
        assert_equal(lst,
                     Erlang::OtpErlangList.new([1, 2, 3, 4], improper=true))
        assert_not_equal(lst,
                         Erlang::OtpErlangList.new([1, 2, 3, 5], improper=true))
        assert_not_equal(lst,
                         Erlang::OtpErlangList.new([1, 2, 3], improper=true))
    end
    def test_errors
        assert_raise(Erlang::OutputException){
            Erlang::OtpErlangList.new('invalid', true).binary
        }
    end
end

class DecodeTestCase < Test::Unit::TestCase
    def test_binary_to_term
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\0")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83z")
        }
    end
    def test_binary_to_term_atom
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83d")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83d\0")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83d\0\1")
        }
        assert_equal(:'', Erlang::binary_to_term("\x83d\0\0"))
        assert_equal(:'', Erlang::binary_to_term("\x83s\0"))
        assert_equal(:test, Erlang::binary_to_term("\x83d\0\4test"))
        assert_equal(:test, Erlang::binary_to_term("\x83s\4test"))
    end
    def test_binary_to_term_predefined_atoms
        assert_equal(true,
                     Erlang::binary_to_term("\x83s\4true"))
        assert_equal(false,
                     Erlang::binary_to_term("\x83s\5false"))
        assert_equal(:undefined,
                     Erlang::binary_to_term("\x83d\0\11undefined"))
    end
    def test_binary_to_term_empty_list
        assert_equal(Erlang::OtpErlangList.new([]),
                     Erlang::binary_to_term("\x83j"))
    end
    def test_binary_to_term_string_list
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83k")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83k\0")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83k\0\1")
        }
        assert_equal('', Erlang::binary_to_term("\x83k\0\0"))
        assert_equal('test', Erlang::binary_to_term("\x83k\0\4test"))
    end
    def test_binary_to_term_list
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83l")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83l\0")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83l\0\0")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83l\0\0\0")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83l\0\0\0\0")
        }
        assert_equal(Erlang::OtpErlangList.new([]),
                     Erlang::binary_to_term("\x83l\0\0\0\0j"))
        assert_equal(Erlang::OtpErlangList.new([Erlang::OtpErlangList.new([]),
                                                Erlang::OtpErlangList.new([])]),
                     Erlang::binary_to_term("\x83l\0\0\0\2jjj"))
    end
    def test_binary_to_term_improper_list
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83l\0\0\0\0k")
        }
        lst = Erlang::binary_to_term("\x83l\0\0\0\1jd\0\4tail")
        assert(lst.kind_of?(Erlang::OtpErlangList))
        assert_equal([Erlang::OtpErlangList.new([]), :tail], lst.value)
        assert_equal(true, lst.improper)
    end
    def test_binary_to_term_small_tuple
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83h")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83h\1")
        }
        assert(Erlang::binary_to_term("\x83h\0").kind_of?(Array))
        assert_equal([], Erlang::binary_to_term("\x83h\0"))
        assert_equal([Erlang::OtpErlangList.new([]),
                      Erlang::OtpErlangList.new([])],
                     Erlang::binary_to_term("\x83h\2jj"))
    end
    def test_binary_to_term_large_tuple
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83i")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83i\0")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83i\0\0")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83i\0\0\0")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83i\0\0\0\1")
        }
        assert_equal([], Erlang::binary_to_term("\x83i\0\0\0\0"))
        assert_equal([Erlang::OtpErlangList.new([]),
                      Erlang::OtpErlangList.new([])],
                     Erlang::binary_to_term("\x83i\0\0\0\2jj"))
    end
    def test_binary_to_term_small_integer
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83a")
        }
        assert_equal(0, Erlang::binary_to_term("\x83a\0"))
        assert_equal(255, Erlang::binary_to_term("\x83a\xff"))
    end
    def test_binary_to_term_integer
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83b")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83b\0")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83b\0\0")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83b\0\0\0")
        }
        assert_equal(0, Erlang::binary_to_term("\x83b\0\0\0\0"))
        assert_equal(2147483647,
                     Erlang::binary_to_term("\x83b\x7f\xff\xff\xff"))
        assert_equal(-2147483648,
                     Erlang::binary_to_term("\x83b\x80\0\0\0"))
        assert_equal(-1, Erlang::binary_to_term("\x83b\xff\xff\xff\xff"))
    end
    def test_binary_to_term_binary
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83m")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83m\0")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83m\0\0")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83m\0\0\0")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83m\0\0\0\1")
        }
        assert_equal(Erlang::OtpErlangBinary.new(""),
                     Erlang::binary_to_term("\x83m\0\0\0\0"))
        assert_equal(Erlang::OtpErlangBinary.new("data"),
                     Erlang::binary_to_term("\x83m\0\0\0\4data"))
    end
    def test_binary_to_term_float
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83F")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83F\0")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83F\0\0")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83F\0\0\0")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83F\0\0\0\0")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83F\0\0\0\0\0")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83F\0\0\0\0\0\0")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83F\0\0\0\0\0\0\0")
        }
        assert_equal(0.0, Erlang::binary_to_term("\x83F\0\0\0\0\0\0\0\0"))
        assert_equal(1.5, Erlang::binary_to_term("\x83F?\xf8\0\0\0\0\0\0"))
    end
    def test_binary_to_term_small_big_integer
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83n")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83n\0")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83n\1\0")
        }
        assert_equal(0, Erlang::binary_to_term("\x83n\0\0"))
        assert_equal(6618611909121,
                     Erlang::binary_to_term("\x83n\6\0\1\2\3\4\5\6"))
        assert_equal(-6618611909121,
                     Erlang::binary_to_term("\x83n\6\1\1\2\3\4\5\6"))
    end
    def test_binary_to_term_big_integer
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83o")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83o\0")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83o\0\0")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83o\0\0\0")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83o\0\0\0\0")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83o\0\0\0\1\0")
        }
        assert_equal(0, Erlang::binary_to_term("\x83o\0\0\0\0\0"))
        assert_equal(6618611909121,
                     Erlang::binary_to_term("\x83o\0\0\0\6\0\1\2\3\4\5\6"))
        assert_equal(-6618611909121,
                     Erlang::binary_to_term("\x83o\0\0\0\6\1\1\2\3\4\5\6"))
    end
    def test_binary_to_term_pid
        pid_old_binary = (
            "\x83\x67\x64\x00\x0D\x6E\x6F\x6E\x6F\x64\x65\x40\x6E\x6F" \
            "\x68\x6F\x73\x74\x00\x00\x00\x4E\x00\x00\x00\x00\x00"
        )
        pid_old = Erlang::binary_to_term(pid_old_binary)
        assert(pid_old.kind_of?(Erlang::OtpErlangPid))
        assert_equal(Erlang::term_to_binary(pid_old),
                     "\x83gs\rnonode@nohost\x00\x00\x00N" \
                     "\x00\x00\x00\x00\x00")
        pid_new_binary = (
            "\x83\x58\x64\x00\x0D\x6E\x6F\x6E\x6F\x64\x65\x40\x6E\x6F\x68" \
            "\x6F\x73\x74\x00\x00\x00\x4E\x00\x00\x00\x00\x00\x00\x00\x00"
        )
        pid_new = Erlang::binary_to_term(pid_new_binary)
        assert(pid_new.kind_of?(Erlang::OtpErlangPid))
        assert_equal(Erlang::term_to_binary(pid_new),
                     "\x83Xs\rnonode@nohost\x00\x00\x00N" \
                     "\x00\x00\x00\x00\x00\x00\x00\x00")
    end
    def test_binary_to_term_port
        port_old_binary = (
            "\x83\x66\x64\x00\x0D\x6E\x6F\x6E\x6F\x64\x65\x40\x6E\x6F\x68" \
            "\x6F\x73\x74\x00\x00\x00\x06\x00"
        )
        port_old = Erlang::binary_to_term(port_old_binary)
        assert(port_old.kind_of?(Erlang::OtpErlangPort))
        assert_equal(Erlang::term_to_binary(port_old),
                     "\x83fs\rnonode@nohost\x00\x00\x00\x06\x00")
        port_new_binary = (
            "\x83\x59\x64\x00\x0D\x6E\x6F\x6E\x6F\x64\x65\x40\x6E\x6F\x68" \
            "\x6F\x73\x74\x00\x00\x00\x06\x00\x00\x00\x00"
        )
        port_new = Erlang::binary_to_term(port_new_binary)
        assert(port_new.kind_of?(Erlang::OtpErlangPort))
        assert_equal(Erlang::term_to_binary(port_new),
                     "\x83Ys\rnonode@nohost\x00\x00\x00\x06" \
                     "\x00\x00\x00\x00")
    end
    def test_binary_to_term_ref
        ref_new_binary = (
            "\x83\x72\x00\x03\x64\x00\x0D\x6E\x6F\x6E\x6F\x64\x65\x40\x6E" \
            "\x6F\x68\x6F\x73\x74\x00\x00\x03\xE8\x4E\xE7\x68\x00\x02\xA4" \
            "\xC8\x53\x40"
        )
        ref_new = Erlang::binary_to_term(ref_new_binary)
        assert(ref_new.kind_of?(Erlang::OtpErlangReference))
        assert_equal(Erlang::term_to_binary(ref_new),
                     "\x83r\x00\x03s\rnonode@nohost\x00\x00\x03\xe8" \
                     "N\xe7h\x00\x02\xa4\xc8S@")
        ref_newer_binary = (
            "\x83\x5A\x00\x03\x64\x00\x0D\x6E\x6F\x6E\x6F\x64\x65\x40\x6E" \
            "\x6F\x68\x6F\x73\x74\x00\x00\x00\x00\x00\x01\xAC\x03\xC7\x00" \
            "\x00\x04\xBB\xB2\xCA\xEE"
        )
        ref_newer = Erlang::binary_to_term(ref_newer_binary)
        assert(ref_newer.kind_of?(Erlang::OtpErlangReference))
        assert_equal(Erlang::term_to_binary(ref_newer),
                     "\x83Z\x00\x03s\rnonode@nohost\x00\x00\x00\x00\x00" \
                     "\x01\xac\x03\xc7\x00\x00\x04\xbb\xb2\xca\xee")
    end
    def test_binary_to_term_compressed_term
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83P")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83P\0")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83P\0\0")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83P\0\0\0")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83P\0\0\0\0")
        }
        assert_raise(Erlang::ParseException){
            Erlang::binary_to_term("\x83P\0\0\0\x16\x78\xda\xcb\x66" \
                                   "\x10\x49\xc1\2\0\x5d\x60\x08\x50")
        }
        assert_equal("d" * 20,
            Erlang::binary_to_term("\x83P\0\0\0\x17\x78\xda\xcb\x66" \
                                   "\x10\x49\xc1\2\0\x5d\x60\x08\x50")
        )
    end
end

class EncodeTestCase < Test::Unit::TestCase
    def test_term_to_binary_tuple
        assert_equal("\x83h\0", Erlang::term_to_binary([]))
        assert_equal("\x83h\2h\0h\0",
                     Erlang::term_to_binary([[], []]))
        assert_equal("\x83h\xff" + "h\0" * 255,
                     Erlang::term_to_binary([[]] * 255))
        assert_equal("\x83i\0\0\1\0" + "h\0" * 256,
                     Erlang::term_to_binary([[]] * 256))
    end
    def test_term_to_binary_empty_list
        assert_equal("\x83j",
                     Erlang::term_to_binary(Erlang::OtpErlangList.new([])))
    end
    def test_term_to_binary_string_list
        assert_equal("\x83k\0\1\0", Erlang::term_to_binary("\0"))
        s = (0...256).to_a.map{ |i| i.chr }.join
        assert_equal("\x83k\1\0" + s, Erlang::term_to_binary(s))
    end
    def test_term_to_binary_list_basic
        assert_equal("\x83\x6A",
                     Erlang::term_to_binary(Erlang::OtpErlangList.new([])))
        assert_equal("\x83\x6C\x00\x00\x00\x01\x6A\x6A",
                     Erlang::term_to_binary(Erlang::OtpErlangList.new([""])))
        assert_equal("\x83\x6C\x00\x00\x00\x01\x61\x01\x6A",
                     Erlang::term_to_binary(Erlang::OtpErlangList.new([1])))
        assert_equal("\x83\x6C\x00\x00\x00\x01\x61\xFF\x6A",
                     Erlang::term_to_binary(Erlang::OtpErlangList.new([255])))
        assert_equal("\x83\x6C\x00\x00\x00\x01\x62\x00\x00\x01\x00\x6A",
                     Erlang::term_to_binary(Erlang::OtpErlangList.new([256])))
        assert_equal(
            "\x83\x6C\x00\x00\x00\x01\x62\x7F\xFF\xFF\xFF\x6A",
            Erlang::term_to_binary(Erlang::OtpErlangList.new([2147483647]))
        )
        assert_equal(
            "\x83\x6C\x00\x00\x00\x01\x6E\x04\x00\x00\x00\x00\x80\x6A",
            Erlang::term_to_binary(Erlang::OtpErlangList.new([2147483648]))
        )
        assert_equal("\x83\x6C\x00\x00\x00\x01\x61\x00\x6A",
                     Erlang::term_to_binary(Erlang::OtpErlangList.new([0])))
        assert_equal("\x83\x6C\x00\x00\x00\x01\x62\xFF\xFF\xFF\xFF\x6A",
                     Erlang::term_to_binary(Erlang::OtpErlangList.new([-1])))
        assert_equal("\x83\x6C\x00\x00\x00\x01\x62\xFF\xFF\xFF\x00\x6A",
                     Erlang::term_to_binary(Erlang::OtpErlangList.new([-256])))
        assert_equal("\x83\x6C\x00\x00\x00\x01\x62\xFF\xFF\xFE\xFF\x6A",
                     Erlang::term_to_binary(Erlang::OtpErlangList.new([-257])))
        assert_equal(
            "\x83\x6C\x00\x00\x00\x01\x62\x80\x00\x00\x00\x6A",
            Erlang::term_to_binary(Erlang::OtpErlangList.new([-2147483648]))
        )
        assert_equal(
            "\x83\x6C\x00\x00\x00\x01\x6E\x04\x01\x01\x00\x00\x80\x6A",
            Erlang::term_to_binary(Erlang::OtpErlangList.new([-2147483649]))
        )
        assert_equal(
            "\x83\x6C\x00\x00\x00\x01\x6B\x00\x04\x74\x65\x73\x74\x6A",
            Erlang::term_to_binary(Erlang::OtpErlangList.new(["test"]))
        )
        assert_equal(
            "\x83\x6C\x00\x00\x00\x02\x62\x00\x00\x01\x75\x62\x00\x00" \
            "\x01\xC7\x6A",
            Erlang::term_to_binary(Erlang::OtpErlangList.new([373, 455]))
        )
        assert_equal(
            "\x83\x6C\x00\x00\x00\x01\x6A\x6A",
            Erlang::term_to_binary(
                Erlang::OtpErlangList.new([Erlang::OtpErlangList.new([])])
            )
        )
        assert_equal(
            "\x83\x6C\x00\x00\x00\x02\x6A\x6A\x6A",
            Erlang::term_to_binary(
                Erlang::OtpErlangList.new([Erlang::OtpErlangList.new([]),
                                           Erlang::OtpErlangList.new([])])
            )
        )
        assert_equal(
            "\x83\x6C\x00\x00\x00\x03\x6C\x00\x00\x00\x02\x6B\x00\x04\x74\x68" \
            "\x69\x73\x6B\x00\x02\x69\x73\x6A\x6C\x00\x00\x00\x01\x6C\x00\x00" \
            "\x00\x01\x6B\x00\x01\x61\x6A\x6A\x6B\x00\x04\x74\x65\x73\x74\x6A",
            Erlang::term_to_binary(Erlang::OtpErlangList.new([
                Erlang::OtpErlangList.new(["this", "is"]),
                Erlang::OtpErlangList.new([
                    Erlang::OtpErlangList.new(["a"])
                ]),
                "test"
            ]))
        )
    end
    def test_term_to_binary_list
        assert_equal(
            "\x83l\0\0\0\1jj",
            Erlang::term_to_binary(Erlang::OtpErlangList.new([
                Erlang::OtpErlangList.new([])
            ]))
        )
        assert_equal(
            "\x83l\0\0\0\5jjjjjj",
            Erlang::term_to_binary(Erlang::OtpErlangList.new([
                Erlang::OtpErlangList.new([]),
                Erlang::OtpErlangList.new([]),
                Erlang::OtpErlangList.new([]),
                Erlang::OtpErlangList.new([]),
                Erlang::OtpErlangList.new([])
            ]))
        )
    end
    def test_term_to_binary_improper_list
        assert_equal(
            "\x83l\0\0\0\1h\0h\0",
            Erlang::term_to_binary(Erlang::OtpErlangList.new([
                [], []
            ], improper=true))
        )
        assert_equal(
            "\x83l\0\0\0\1a\0a\1",
            Erlang::term_to_binary(Erlang::OtpErlangList.new([
                0, 1
            ], improper=true))
        )
    end
    def test_term_to_binary_atom
        assert_equal("\x83s\0", Erlang::term_to_binary(:""))
        assert_equal("\x83s\4test", Erlang::term_to_binary(:test))
    end
    def test_term_to_binary_string_basic
        assert_equal("\x83\x6A", Erlang::term_to_binary(""))
        assert_equal("\x83\x6B\x00\x04\x74\x65\x73\x74",
                     Erlang::term_to_binary("test"))
        assert_equal(
            "\x83\x6B\x00\x09\x74\x77\x6F\x20\x77\x6F\x72\x64\x73",
            Erlang::term_to_binary("two words")
        )
        assert_equal(
            "\x83\x6B\x00\x16\x74\x65\x73\x74\x69\x6E\x67\x20\x6D\x75\x6C\x74" \
            "\x69\x70\x6C\x65\x20\x77\x6F\x72\x64\x73",
            Erlang::term_to_binary("testing multiple words")
        )
        assert_equal("\x83\x6B\x00\x01\x20",
                     Erlang::term_to_binary(" "))
        assert_equal("\x83\x6B\x00\x02\x20\x20",
                     Erlang::term_to_binary("  "))
        assert_equal("\x83\x6B\x00\x01\x31",
                     Erlang::term_to_binary("1"))
        assert_equal("\x83\x6B\x00\x02\x33\x37",
                     Erlang::term_to_binary("37"))
        assert_equal("\x83\x6B\x00\x07\x6F\x6E\x65\x20\x3D\x20\x31",
                     Erlang::term_to_binary("one = 1"))
        assert_equal(
            "\x83\x6B\x00\x20\x21\x40\x23\x24\x25\x5E\x26\x2A\x28\x29\x5F\x2B" \
            "\x2D\x3D\x5B\x5D\x7B\x7D\x5C\x7C\x3B\x27\x3A\x22\x2C\x2E\x2F\x3C" \
            "\x3E\x3F\x7E\x60",
            Erlang::term_to_binary("!@#\$%^&*()_+-=[]{}\\|;':\",./<>?~`")
        )
        assert_equal(
            "\x83\x6B\x00\x09\x22\x08\x0C\x0A\x0D\x09\x0B\x53\x12",
            Erlang::term_to_binary("\"\b\f\n\r\t\v\123\x12")
        )
    end
    def test_term_to_binary_string
        assert_equal("\x83j", Erlang::term_to_binary(''))
        assert_equal("\x83k\0\1\0",
                     Erlang::term_to_binary("\0"))
        assert_equal("\x83k\0\4test",
                     Erlang::term_to_binary('test'))
    end
    def test_term_to_binary_predefined_atom
        assert_equal("\x83s\4true", Erlang::term_to_binary(true))
        assert_equal("\x83s\5false", Erlang::term_to_binary(false))
        assert_equal("\x83s\x09undefined", Erlang::term_to_binary(nil))
    end
    def test_term_to_binary_short_integer
        assert_equal("\x83a\0", Erlang::term_to_binary(0))
        assert_equal("\x83a\xff", Erlang::term_to_binary(255))
    end
    def test_term_to_binary_integer
        assert_equal("\x83b\xff\xff\xff\xff", Erlang::term_to_binary(-1))
        assert_equal("\x83b\x80\0\0\0",
                     Erlang::term_to_binary(-2147483648))
        assert_equal("\x83b\0\0\1\0",
                     Erlang::term_to_binary(256))
        assert_equal("\x83b\x7f\xff\xff\xff",
                     Erlang::term_to_binary(2147483647))
    end
    def test_term_to_binary_long_integer
        assert_equal("\x83n\4\0\0\0\0\x80",
                     Erlang::term_to_binary(2147483648))
        assert_equal("\x83n\4\1\1\0\0\x80",
                     Erlang::term_to_binary(-2147483649))
        assert_equal("\x83o\0\0\1\0\0" + "\0" * 255 + "\1",
                     Erlang::term_to_binary(2 ** 2040))
        assert_equal("\x83o\0\0\1\0\1" + "\0" * 255 + "\1",
                     Erlang::term_to_binary(-2 ** 2040))
    end
    def test_term_to_binary_float
        assert_equal("\x83F\0\0\0\0\0\0\0\0",
                     Erlang::term_to_binary(0.0))
        assert_equal("\x83F?\xe0\0\0\0\0\0\0",
                     Erlang::term_to_binary(0.5))
        assert_equal("\x83F\xbf\xe0\0\0\0\0\0\0",
                     Erlang::term_to_binary(-0.5))
        assert_equal("\x83F@\t!\xfbM\x12\xd8J",
                     Erlang::term_to_binary(3.1415926))
        assert_equal("\x83F\xc0\t!\xfbM\x12\xd8J",
                     Erlang::term_to_binary(-3.1415926))
    end
    def test_term_to_binary_compressed_term
        assert_equal(
            "\x83P\x00\x00\x00\x15x\x9c\xcba``\xe0\xcfB\x03\x00B@\x07\x1c",
            Erlang::term_to_binary(
                Erlang::OtpErlangList.new([
                    Erlang::OtpErlangList.new([])
                ] * 15), compressed=true
            )
        )
        assert_equal(
            "\x83P\x00\x00\x00\x15x\x9c\xcba``\xe0\xcfB\x03\x00B@\x07\x1c",
            Erlang::term_to_binary(
                Erlang::OtpErlangList.new([
                    Erlang::OtpErlangList.new([])
                ] * 15), compressed=6
            )
        )
        assert_equal(
            "\x83P\x00\x00\x00\x15x\xda\xcba``\xe0\xcfB\x03\x00B@\x07\x1c",
            Erlang::term_to_binary(
                Erlang::OtpErlangList.new([
                    Erlang::OtpErlangList.new([])
                ] * 15), compressed=9
            )
        )
        assert_equal(
            "\x83P\x00\x00\x00\x15x\x01\x01\x15\x00\xea\xffl\x00\x00\x00" \
            "\x0fjjjjjjjjjjjjjjjjB@\x07\x1c",
            Erlang::term_to_binary(
                Erlang::OtpErlangList.new([
                    Erlang::OtpErlangList.new([])
                ] * 15), compressed=0
            )
        )
        assert_equal(
            "\x83P\0\0\0\x17\x78\xda\xcb\x66\x10\x49\xc1\2\0\x5d\x60\x08\x50",
            Erlang::term_to_binary("d" * 20, compressed=9)
        )
    end
end

