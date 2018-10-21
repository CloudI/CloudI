<?php //-*-coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=php fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
//
// MIT License
//
// Copyright (c) 2014-2018 Michael Truog <mjtruog at protonmail dot com>
// Copyright (c) 2009-2013 Dmitry Vasiliev <dima@hlabs.org>
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
//
require dirname(__FILE__) . '/../Erlang.php';

class ErlangTests
{
    public static function suite()
    {
        $suite = new PHPUnit\Framework\TestSuite();
        $cases = array('AtomTestCase',
                       'ListTestCase',
                       'ImproperListTestCase',
                       'DecodeTestCase',
                       'EncodeTestCase');
        foreach ($cases as $case)
        {
            foreach (get_class_methods($case) as $name)
            {
                if (substr($name, 0, 5) === 'test_')
                {
                    $suite->addTest(new $case($name));
                }
            }
        }
        return $suite;
    }
}

// many of the test cases were adapted
// from erlport (https://github.com/hdima/erlport)
// to make the tests more exhaustive

class AtomTestCase extends PHPUnit\Framework\TestCase
{
    public function test_atom()
    {
        $atom1 = new \Erlang\OtpErlangAtom('test');
        $this->assertEquals(get_class($atom1), 'Erlang\OtpErlangAtom');
        $this->assertEquals(new \Erlang\OtpErlangAtom('test'), $atom1);
        $this->assertEquals('Erlang\OtpErlangAtom(test,utf8=false)',
                            (string) $atom1);
        $atom2 = new \Erlang\OtpErlangAtom('test2');
        $atom1_new = new \Erlang\OtpErlangAtom('test');
        $this->assertNotEquals($atom1, $atom2);
        $this->assertEquals($atom1, $atom1_new);
        $atom3 = new \Erlang\OtpErlangAtom(str_repeat('X', 256));
        $this->assertEquals(str_repeat('X', 256), $atom3->value);
    }
    public function test_invalid_atom()
    {
        $this->expectException('\Erlang\OutputException');
        $this->expectExceptionMessage('unknown atom type');
        $atom_invalid = new \Erlang\OtpErlangAtom(array(1, 2));
        $atom_invalid->binary();
    }
}

class ListTestCase extends PHPUnit\Framework\TestCase
{
    public function test_list()
    {
        $lst = new \Erlang\OtpErlangList(array(116, 101, 115, 116));
        $this->assertEquals(get_class($lst), 'Erlang\OtpErlangList');
        $this->assertEquals(new \Erlang\OtpErlangList(array(116, 101,
                                                            115, 116)), $lst);
        $this->assertEquals(array(116, 101, 115, 116), $lst->value);
        $this->assertEquals('Erlang\OtpErlangList(array(116,101,115,116),' .
                                                 'improper=false)',
                            (string) $lst);
    }
}

class ImproperListTestCase extends PHPUnit\Framework\TestCase
{
    public function test_improper_list()
    {
        $lst = new \Erlang\OtpErlangList(array(1, 2, 3, 4), true);
        $this->assertEquals(get_class($lst), 'Erlang\OtpErlangList');
        $this->assertEquals(array(1, 2, 3, 4), $lst->value);
        $this->assertEquals(4, end($lst->value)); reset($lst->value);
        $this->assertEquals('Erlang\OtpErlangList(array(1,2,3,4),' .
                                                 'improper=true)',
                            (string) $lst);
    }
    public function test_comparison()
    {
        $lst = new \Erlang\OtpErlangList(array(1, 2, 3, 4), true);
        $this->assertEquals($lst, $lst);
        $this->assertEquals($lst,
                            new \Erlang\OtpErlangList(array(1, 2, 3, 4), true));
        $this->assertNotEquals($lst,
                               new \Erlang\OtpErlangList(array(1, 2, 3, 4,
                                                               5), true));
        $this->assertNotEquals($lst,
                               new \Erlang\OtpErlangList(array(1, 2, 3), true));
    }
    public function test_errors()
    {
        $this->expectException('\Erlang\OutputException');
        $this->expectExceptionMessage('unknown list type');
        $list_invalid = new \Erlang\OtpErlangList('invalid', true);
        $list_invalid->binary();
    }
}

class DecodeTestCase extends PHPUnit\Framework\TestCase
{
    public function test_binary_to_term_1()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term('');
    }
    public function test_binary_to_term_2()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\0");
    }
    public function test_binary_to_term_3()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83");
    }
    public function test_binary_to_term_4()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83z");
    }
    public function test_binary_to_term_atom_1()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83d");
    }
    public function test_binary_to_term_atom_2()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83d\0");
    }
    public function test_binary_to_term_atom_3()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83d\0\1");
    }
    public function test_binary_to_term_atom()
    {
        $this->assertEquals(new \Erlang\OtpErlangAtom(''),
                            \Erlang\binary_to_term("\x83d\0\0"));
        $this->assertEquals(new \Erlang\OtpErlangAtom(''),
                            \Erlang\binary_to_term("\x83s\0"));
        $this->assertEquals(new \Erlang\OtpErlangAtom('test'),
                            \Erlang\binary_to_term("\x83d\0\4test"));
        $this->assertEquals(new \Erlang\OtpErlangAtom('test'),
                            \Erlang\binary_to_term("\x83s\4test"));
    }
    public function test_binary_to_term_predefined_atoms()
    {
        $this->assertEquals(true,
                            \Erlang\binary_to_term("\x83s\4true"));
        $this->assertEquals(false,
                            \Erlang\binary_to_term("\x83s\5false"));
        $this->assertEquals(new \Erlang\OtpErlangAtom('undefined'),
                            \Erlang\binary_to_term("\x83d\0\11undefined"));
    }
    public function test_binary_to_term_empty_list()
    {
        $this->assertEquals(new \Erlang\OtpErlangList(array()),
                            \Erlang\binary_to_term("\x83j"));
    }
    public function test_binary_to_term_string_list_1()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83l");
    }
    public function test_binary_to_term_string_list_2()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83l\0");
    }
    public function test_binary_to_term_string_list_3()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83l\0\0");
    }
    public function test_binary_to_term_string_list_4()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83l\0\0\0");
    }
    public function test_binary_to_term_string_list_5()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83l\0\0\0\0");
    }
    public function test_binary_to_term_string_list()
    {
        $this->assertEquals(new \Erlang\OtpErlangList(array()),
                            \Erlang\binary_to_term("\x83l\0\0\0\0j"));
        $this->assertEquals(new \Erlang\OtpErlangList(array(
                                new \Erlang\OtpErlangList(array()),
                                new \Erlang\OtpErlangList(array()))),
                            \Erlang\binary_to_term("\x83l\0\0\0\2jjj"));
    }
    public function test_binary_to_term_improper_list_1()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83l\0\0\0\0k");
    }
    public function test_binary_to_term_improper_list()
    {
        $lst = \Erlang\binary_to_term("\x83l\0\0\0\1jd\0\4tail");
        $this->assertEquals(get_class($lst), 'Erlang\OtpErlangList');
        $this->assertEquals(array(new \Erlang\OtpErlangList(array()),
                                  new \Erlang\OtpErlangAtom('tail')),
                            $lst->value);
        $this->assertEquals(true, $lst->improper);
    }
    public function test_binary_to_term_small_tuple_1()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83h");
    }
    public function test_binary_to_term_small_tuple_2()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83h\1");
    }
    public function test_binary_to_term_small_tuple()
    {
        $tuple = \Erlang\binary_to_term("\x83h\0");
        $this->assertEquals(is_array($tuple), true);
        $this->assertEquals(array(), $tuple);
        $this->assertEquals(array(new \Erlang\OtpErlangList(array()),
                                  new \Erlang\OtpErlangList(array())),
                            \Erlang\binary_to_term("\x83h\2jj"));
    }
    public function test_binary_to_term_large_tuple_1()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83i");
    }
    public function test_binary_to_term_large_tuple_2()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83i\0");
    }
    public function test_binary_to_term_large_tuple_3()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83i\0\0");
    }
    public function test_binary_to_term_large_tuple_4()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83i\0\0\0");
    }
    public function test_binary_to_term_large_tuple_5()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83i\0\0\0\1");
    }
    public function test_binary_to_term_large_tuple()
    {
        $tuple = \Erlang\binary_to_term("\x83i\0\0\0\0");
        $this->assertEquals(array(), $tuple);
        $this->assertEquals(array(new \Erlang\OtpErlangList(array()),
                                  new \Erlang\OtpErlangList(array())),
                            \Erlang\binary_to_term("\x83i\0\0\0\2jj"));
    }
    public function test_binary_to_term_small_integer_1()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83a");
    }
    public function test_binary_to_term_small_integer()
    {
        $this->assertEquals(0, \Erlang\binary_to_term("\x83a\0"));
        $this->assertEquals(255, \Erlang\binary_to_term("\x83a\xff"));
    }
    public function test_binary_to_term_integer_1()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83b");
    }
    public function test_binary_to_term_integer_2()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83b\0");
    }
    public function test_binary_to_term_integer_3()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83b\0\0");
    }
    public function test_binary_to_term_integer_4()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83b\0\0\0");
    }
    public function test_binary_to_term_integer()
    {
        $this->assertEquals(0, \Erlang\binary_to_term("\x83b\0\0\0\0"));
        $this->assertEquals(2147483647,
                            \Erlang\binary_to_term("\x83b\x7f\xff\xff\xff"));
        $this->assertEquals(-2147483648,
                            \Erlang\binary_to_term("\x83b\x80\x00\x00\x00"));
        $this->assertEquals(-1,
                            \Erlang\binary_to_term("\x83b\xff\xff\xff\xff"));
    }
    public function test_binary_to_term_binary_1()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83m");
    }
    public function test_binary_to_term_binary_2()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83m\0");
    }
    public function test_binary_to_term_binary_3()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83m\0\0");
    }
    public function test_binary_to_term_binary_4()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83m\0\0\0");
    }
    public function test_binary_to_term_binary_5()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83m\0\0\0\1");
    }
    public function test_binary_to_term_binary()
    {
        $this->assertEquals(new \Erlang\OtpErlangBinary(''),
                            \Erlang\binary_to_term("\x83m\0\0\0\0"));
        $this->assertEquals(new \Erlang\OtpErlangBinary('data'),
                            \Erlang\binary_to_term("\x83m\0\0\0\4data"));
    }
    public function test_binary_to_term_float_1()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83F");
    }
    public function test_binary_to_term_float_2()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83F\0");
    }
    public function test_binary_to_term_float_3()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83F\0\0");
    }
    public function test_binary_to_term_float_4()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83F\0\0\0");
    }
    public function test_binary_to_term_float_5()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83F\0\0\0\0");
    }
    public function test_binary_to_term_float_6()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83F\0\0\0\0\0");
    }
    public function test_binary_to_term_float_7()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83F\0\0\0\0\0\0");
    }
    public function test_binary_to_term_float_8()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83F\0\0\0\0\0\0\0");
    }
    public function test_binary_to_term_float()
    {
        $this->assertEquals(0.0,
                            \Erlang\binary_to_term("\x83F\0\0\0\0\0\0\0\0"));
        $this->assertEquals(1.5,
                            \Erlang\binary_to_term("\x83F?\xf8\0\0\0\0\0\0"));
    }
    public function test_binary_to_term_small_big_integer_1()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83n");
    }
    public function test_binary_to_term_small_big_integer_2()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83n\0");
    }
    public function test_binary_to_term_small_big_integer_3()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83n\1\0");
    }
    public function test_binary_to_term_small_big_integer()
    {
        $this->assertEquals(0,
                            \Erlang\binary_to_term("\x83n\0\0"));
        $this->assertEquals(6618611909121,
                            \Erlang\binary_to_term("\x83n\6\0\1\2\3\4\5\6"));
        $this->assertEquals(-6618611909121,
                            \Erlang\binary_to_term("\x83n\6\1\1\2\3\4\5\6"));
    }
    public function test_binary_to_term_big_integer_1()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83o");
    }
    public function test_binary_to_term_big_integer_2()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83o\0");
    }
    public function test_binary_to_term_big_integer_3()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83o\0\0");
    }
    public function test_binary_to_term_big_integer_4()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83o\0\0\0");
    }
    public function test_binary_to_term_big_integer_5()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83o\0\0\0\0");
    }
    public function test_binary_to_term_big_integer_6()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83o\0\0\0\1\0");
    }
    public function test_binary_to_term_big_integer()
    {
        $this->assertEquals(0,
                            \Erlang\binary_to_term("\x83o\0\0\0\0\0"));
        $this->assertEquals(6618611909121,
                            \Erlang\binary_to_term("\x83o\0\0\0\6" .
                                                   "\0\1\2\3\4\5\6"));
        $this->assertEquals(-6618611909121,
                            \Erlang\binary_to_term("\x83o\0\0\0\6" .
                                                   "\1\1\2\3\4\5\6"));
    }
    public function test_binary_to_term_compressed_term_1()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83P");
    }
    public function test_binary_to_term_compressed_term_2()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83P\0");
    }
    public function test_binary_to_term_compressed_term_3()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83P\0\0");
    }
    public function test_binary_to_term_compressed_term_4()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83P\0\0\0");
    }
    public function test_binary_to_term_compressed_term_5()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83P\0\0\0\0");
    }
    public function test_binary_to_term_compressed_term_6()
    {
        $this->expectException('\Erlang\ParseException');
        \Erlang\binary_to_term("\x83P\0\0\0\x16\x78\xda\xcb\x66" .
                               "\x10\x49\xc1\2\0\x5d\x60\x08\x50");
    }
    public function test_binary_to_term_compressed_term()
    {
        $this->assertEquals(str_repeat('d', 20),
            \Erlang\binary_to_term("\x83P\0\0\0\x17\x78\xda\xcb\x66" .
                                   "\x10\x49\xc1\2\0\x5d\x60\x08\x50"));
    }
}

class EncodeTestCase extends PHPUnit\Framework\TestCase
{
    public function test_term_to_binary_tuple()
    {
        $this->assertEquals("\x83h\0",
            \Erlang\term_to_binary(array()));
        $this->assertEquals("\x83h\2h\0h\0",
            \Erlang\term_to_binary(array(array(), array())));
        $this->assertEquals("\x83h\xff" . str_repeat("h\0", 255),
            \Erlang\term_to_binary(array_fill(0, 255, array())));
        $this->assertEquals("\x83i\0\0\1\0" . str_repeat("h\0", 256),
            \Erlang\term_to_binary(array_fill(0, 256, array())));
    }
    public function test_term_to_binary_empty_list()
    {
        $this->assertEquals("\x83j",
            \Erlang\term_to_binary(new \Erlang\OtpErlangList(array())));
    }
    public function test_term_to_binary_string_list()
    {
        $this->assertEquals("\x83k\0\1\0",
            \Erlang\term_to_binary("\0"));
        $s = '';
        foreach (range(0, 256 - 1) as $c)
            $s .= chr($c);
        $this->assertEquals("\x83k\1\0" . $s,
            \Erlang\term_to_binary($s));
    }
    public function test_term_to_binary_list_basic()
    {
        $this->assertEquals("\x83\x6A",
            \Erlang\term_to_binary(new \Erlang\OtpErlangList(array())));
        $this->assertEquals("\x83\x6C\x00\x00\x00\x01\x6A\x6A",
            \Erlang\term_to_binary(new \Erlang\OtpErlangList(array(''))));
        $this->assertEquals("\x83\x6C\x00\x00\x00\x01\x61\x01\x6A",
            \Erlang\term_to_binary(new \Erlang\OtpErlangList(array(1))));
        $this->assertEquals("\x83\x6C\x00\x00\x00\x01\x61\xFF\x6A",
            \Erlang\term_to_binary(new \Erlang\OtpErlangList(array(255))));
        $this->assertEquals("\x83\x6C\x00\x00\x00\x01\x62\x00\x00\x01\x00\x6A",
            \Erlang\term_to_binary(new \Erlang\OtpErlangList(array(256))));
        $this->assertEquals(
            "\x83\x6C\x00\x00\x00\x01\x62\x7F\xFF\xFF\xFF\x6A",
            \Erlang\term_to_binary(new \Erlang\OtpErlangList(
                array(2147483647))));
        $this->assertEquals(
            "\x83\x6C\x00\x00\x00\x01\x6E\x04\x00\x00\x00\x00\x80\x6A",
            \Erlang\term_to_binary(new \Erlang\OtpErlangList(
                array(2147483648))));
        $this->assertEquals("\x83\x6C\x00\x00\x00\x01\x61\x00\x6A",
            \Erlang\term_to_binary(new \Erlang\OtpErlangList(array(0))));
        $this->assertEquals("\x83\x6C\x00\x00\x00\x01\x62\xFF\xFF\xFF\xFF\x6A",
            \Erlang\term_to_binary(new \Erlang\OtpErlangList(array(-1))));
        $this->assertEquals("\x83\x6C\x00\x00\x00\x01\x62\xFF\xFF\xFF\x00\x6A",
            \Erlang\term_to_binary(new \Erlang\OtpErlangList(array(-256))));
        $this->assertEquals("\x83\x6C\x00\x00\x00\x01\x62\xFF\xFF\xFE\xFF\x6A",
            \Erlang\term_to_binary(new \Erlang\OtpErlangList(array(-257))));
        $this->assertEquals(
            "\x83\x6C\x00\x00\x00\x01\x62\x80\x00\x00\x00\x6A",
            \Erlang\term_to_binary(new \Erlang\OtpErlangList(
                array(-2147483648))));
        $this->assertEquals(
            "\x83\x6C\x00\x00\x00\x01\x6E\x04\x01\x01\x00\x00\x80\x6A",
            \Erlang\term_to_binary(new \Erlang\OtpErlangList(
                array(-2147483649))));
        $this->assertEquals(
            "\x83\x6C\x00\x00\x00\x01\x6B\x00\x04\x74\x65\x73\x74\x6A",
            \Erlang\term_to_binary(new \Erlang\OtpErlangList(
                array('test'))));
        $this->assertEquals(
            "\x83\x6C\x00\x00\x00\x02\x62\x00\x00\x01\x75\x62\x00\x00" .
            "\x01\xC7\x6A",
            \Erlang\term_to_binary(new \Erlang\OtpErlangList(
                array(373, 455))));
        $this->assertEquals(
            "\x83\x6C\x00\x00\x00\x01\x6A\x6A",
            \Erlang\term_to_binary(new \Erlang\OtpErlangList(
                array(new \Erlang\OtpErlangList(array())))));
        $this->assertEquals(
            "\x83\x6C\x00\x00\x00\x02\x6A\x6A\x6A",
            \Erlang\term_to_binary(new \Erlang\OtpErlangList(
                array(new \Erlang\OtpErlangList(array()),
                      new \Erlang\OtpErlangList(array())))));
        $this->assertEquals(
            "\x83\x6C\x00\x00\x00\x03\x6C\x00\x00\x00\x02\x6B\x00\x04\x74\x68" .
            "\x69\x73\x6B\x00\x02\x69\x73\x6A\x6C\x00\x00\x00\x01\x6C\x00\x00" .
            "\x00\x01\x6B\x00\x01\x61\x6A\x6A\x6B\x00\x04\x74\x65\x73\x74\x6A",
            \Erlang\term_to_binary(new \Erlang\OtpErlangList(
                array(new \Erlang\OtpErlangList(array('this', 'is')),
                      new \Erlang\OtpErlangList(array(
                        new \Erlang\OtpErlangList(array('a')))),
                      'test'))));
    }
    public function test_term_to_binary_list()
    {
        $this->assertEquals("\x83l\0\0\0\1jj",
            \Erlang\term_to_binary(new \Erlang\OtpErlangList(
                array(new \Erlang\OtpErlangList(array())))));
        $this->assertEquals("\x83l\0\0\0\5jjjjjj",
            \Erlang\term_to_binary(new \Erlang\OtpErlangList(
                array(new \Erlang\OtpErlangList(array()),
                      new \Erlang\OtpErlangList(array()),
                      new \Erlang\OtpErlangList(array()),
                      new \Erlang\OtpErlangList(array()),
                      new \Erlang\OtpErlangList(array())))));
    }
    public function test_term_to_binary_improper_list()
    {
        $this->assertEquals("\x83l\0\0\0\1h\0h\0",
            \Erlang\term_to_binary(new \Erlang\OtpErlangList(
                array(array(), array()), true)));
        $this->assertEquals("\x83l\0\0\0\1a\0a\1",
            \Erlang\term_to_binary(new \Erlang\OtpErlangList(
                array(0, 1), true)));
    }
    public function test_term_to_binary_atom()
    {
        $this->assertEquals("\x83s\0",
            \Erlang\term_to_binary(new \Erlang\OtpErlangAtom('')));
        $this->assertEquals("\x83s\4test",
            \Erlang\term_to_binary(new \Erlang\OtpErlangAtom('test')));
    }
    public function test_term_to_binary_string_basic()
    {
        $this->assertEquals("\x83\x6A",
            \Erlang\term_to_binary(''));
        $this->assertEquals("\x83\x6B\x00\x04\x74\x65\x73\x74",
            \Erlang\term_to_binary('test'));
        $this->assertEquals(
            "\x83\x6B\x00\x09\x74\x77\x6F\x20\x77\x6F\x72\x64\x73",
            \Erlang\term_to_binary('two words'));
        $this->assertEquals(
            "\x83\x6B\x00\x16\x74\x65\x73\x74\x69\x6E\x67\x20\x6D\x75\x6C\x74" .
            "\x69\x70\x6C\x65\x20\x77\x6F\x72\x64\x73",
            \Erlang\term_to_binary('testing multiple words'));
        $this->assertEquals("\x83\x6B\x00\x01\x20",
            \Erlang\term_to_binary(' '));
        $this->assertEquals("\x83\x6B\x00\x02\x20\x20",
            \Erlang\term_to_binary('  '));
        $this->assertEquals("\x83\x6B\x00\x01\x31",
            \Erlang\term_to_binary('1'));
        $this->assertEquals("\x83\x6B\x00\x02\x33\x37",
            \Erlang\term_to_binary('37'));
        $this->assertEquals("\x83\x6B\x00\x07\x6F\x6E\x65\x20\x3D\x20\x31",
            \Erlang\term_to_binary('one = 1'));
        $this->assertEquals(
            "\x83\x6B\x00\x20\x21\x40\x23\x24\x25\x5E\x26\x2A\x28\x29\x5F\x2B" .
            "\x2D\x3D\x5B\x5D\x7B\x7D\x5C\x7C\x3B\x27\x3A\x22\x2C\x2E\x2F\x3C" .
            "\x3E\x3F\x7E\x60",
            \Erlang\term_to_binary("!@#\$%^&*()_+-=[]{}\\|;':\",./<>?~`"));
        $this->assertEquals(
            "\x83\x6B\x00\x09\x22\x08\x0C\x0A\x0D\x09\x0B\x53\x12",
            \Erlang\term_to_binary("\"\x8\f\n\r\t\v\123\x12"));
    }
    public function test_term_to_binary_string()
    {
        $this->assertEquals("\x83j", \Erlang\term_to_binary(''));
        $this->assertEquals("\x83k\0\1\0",
            \Erlang\term_to_binary("\0"));
        $this->assertEquals("\x83k\0\4test",
            \Erlang\term_to_binary('test'));
    }
    public function test_term_to_binary_predefined_atoms()
    {
        $this->assertEquals("\x83s\4true", \Erlang\term_to_binary(true));
        $this->assertEquals("\x83s\5false", \Erlang\term_to_binary(false));
        $this->assertEquals("\x83s\x09undefined", \Erlang\term_to_binary(NULL));
    }
    public function test_term_to_binary_short_integer()
    {
        $this->assertEquals("\x83a\0", \Erlang\term_to_binary(0));
        $this->assertEquals("\x83a\xff", \Erlang\term_to_binary(255));
    }
    public function test_term_to_binary_integer()
    {
        $this->assertEquals("\x83b\xff\xff\xff\xff",
                            \Erlang\term_to_binary(-1));
        $this->assertEquals("\x83b\x80\0\0\0",
                            \Erlang\term_to_binary(-2147483648));
        $this->assertEquals("\x83b\0\0\1\0",
                            \Erlang\term_to_binary(256));
        $this->assertEquals("\x83b\x7f\xff\xff\xff",
                            \Erlang\term_to_binary(2147483647));
    }
    public function test_term_to_binary_long_integer()
    {
        $this->assertEquals("\x83n\4\0\0\0\0\x80",
                            \Erlang\term_to_binary(2147483648));
        $this->assertEquals("\x83n\4\1\1\0\0\x80",
                            \Erlang\term_to_binary(-2147483649));
    }
    public function test_term_to_binary_float()
    {
        $this->assertEquals("\x83F\0\0\0\0\0\0\0\0",
                            \Erlang\term_to_binary(0.0));
        $this->assertEquals("\x83F?\xe0\0\0\0\0\0\0",
                            \Erlang\term_to_binary(0.5));
        $this->assertEquals("\x83F\xbf\xe0\0\0\0\0\0\0",
                            \Erlang\term_to_binary(-0.5));
        $this->assertEquals("\x83F@\t!\xfbM\x12\xd8J",
                            \Erlang\term_to_binary(3.1415926));
        $this->assertEquals("\x83F\xc0\t!\xfbM\x12\xd8J",
                            \Erlang\term_to_binary(-3.1415926));
    }
    public function test_term_to_compressed_term()
    {
        $this->assertEquals(
            "\x83P\x00\x00\x00\x15x\x9c\xcba``\xe0\xcfB\x03\x00B@\x07\x1c",
            \Erlang\term_to_binary(new \Erlang\OtpErlangList(
                array_fill(0, 15, new \Erlang\OtpErlangList(array()))),
                true));
        $this->assertEquals(
            "\x83P\x00\x00\x00\x15x\x9c\xcba``\xe0\xcfB\x03\x00B@\x07\x1c",
            \Erlang\term_to_binary(new \Erlang\OtpErlangList(
                array_fill(0, 15, new \Erlang\OtpErlangList(array()))),
                6));
        $this->assertEquals(
            "\x83P\x00\x00\x00\x15x\xda\xcba``\xe0\xcfB\x03\x00B@\x07\x1c",
            \Erlang\term_to_binary(new \Erlang\OtpErlangList(
                array_fill(0, 15, new \Erlang\OtpErlangList(array()))),
                9));
        $this->assertEquals(
            "\x83P\x00\x00\x00\x15x\x01\x01\x15\x00\xea\xffl\x00\x00\x00" .
            "\x0fjjjjjjjjjjjjjjjjB@\x07\x1c",
            \Erlang\term_to_binary(new \Erlang\OtpErlangList(
                array_fill(0, 15, new \Erlang\OtpErlangList(array()))),
                0));
        $this->assertEquals(
            "\x83P\0\0\0\x17\x78\xda\xcb\x66\x10\x49\xc1\2\0\x5d\x60\x08\x50",
            \Erlang\term_to_binary(str_repeat('d', 20), 9));
    }
}

?>
