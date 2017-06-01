//-*-Mode:javascript;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=javascript fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
//
// MIT License
//
// Copyright (c) 2014-2017 Michael Truog <mjtruog at gmail dot com>
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

var Erlang = require('./../Erlang.js').Erlang; 
var assert = require('assert');

// many of the test cases were adapted
// from erlport (https://github.com/hdima/erlport)
// to make the tests more exhaustive

var toNativeString = {}.toString;
String.prototype.repeat = function(i) {
    return new Array(i + 1).join(this);
};
Array.prototype.fill = function(value) {
    for (var i = 0; i < this.length; i++) {
        this[i] = value;
    }
    return this;
};
var hex = function hex(buffer) {
    var output = '';
    for (var i = 0; i < buffer.length; i++)
    {
        var code = buffer[i];
        if (code >= 32 && code <= 126) {
            output += String.fromCharCode(code);
        }
        else {
            var c = code.toString(16);
            if (c.length == 1) {
                c = '0' + c;
            }
            output += '\\x' + c;
        }
    }
    return output;
};

(function AtomTestCase () {
    (function test_atom () {
        var atom1 = new Erlang.OtpErlangAtom('test');
        assert.equal(toNativeString.call(atom1), '[object Object]');
        assert.deepEqual(new Erlang.OtpErlangAtom('test'), atom1);
        assert.equal('OtpErlangAtom(test,false)', '' + atom1);
        assert.ok(atom1 instanceof Erlang.OtpErlangAtom);
        var atom2 = new Erlang.OtpErlangAtom('test2');
        var atom1_new = new Erlang.OtpErlangAtom('test');
        assert.notDeepEqual(atom1, atom2);
        assert.deepEqual(atom1, atom1_new);
        assert.notStrictEqual(atom1, atom1_new);
        assert.equal('X'.repeat(255),
                     (new Erlang.OtpErlangAtom('X'.repeat(255))).value);
        assert.equal('X'.repeat(256),
                     (new Erlang.OtpErlangAtom('X'.repeat(256))).value);
    }).call(this);
    (function test_atom () {
        assert.throws(function() {
                          (new Erlang.OtpErlangAtom([1, 2])).binary();
                      }, Erlang.OutputException);
    }).call(this);
}).call(this);

(function ListTestCase () {
    (function test_list () {
        var lst = new Erlang.OtpErlangList([116, 101, 115, 116]);
        assert.ok(lst instanceof Erlang.OtpErlangList);
        assert.deepEqual(new Erlang.OtpErlangList([116, 101, 115, 116]), lst);
        assert.deepEqual([116, 101, 115, 116], lst.value);
        assert.equal('OtpErlangList([116,101,115,116],false)', lst.toString());
    }).call(this);
}).call(this);

(function ImproperListTestCase () {
    (function test_improper_list () {
        var lst = new Erlang.OtpErlangList([1, 2, 3, 4], true);
        assert.ok(lst instanceof Erlang.OtpErlangList);
        assert.deepEqual([1, 2, 3, 4], lst.value);
        assert.equal(4, lst.value[lst.value.length - 1]);
        assert.equal('OtpErlangList([1,2,3,4],true)', lst.toString());
    }).call(this);
    (function test_comparison () {
        var lst = new Erlang.OtpErlangList([1, 2, 3, 4], true);
        assert.deepEqual(lst, lst);
        assert.deepEqual(lst, new Erlang.OtpErlangList([1, 2, 3, 4], true));
        assert.notDeepEqual(lst, new Erlang.OtpErlangList([1, 2, 3, 5], true));
        assert.notDeepEqual(lst, new Erlang.OtpErlangList([1, 2, 3], true));
    }).call(this);
    (function test_errors () {
        assert.throws(function() {
                          (new Erlang.OtpErlangList('invalid')).binary();
                      }, Erlang.OutputException);
    }).call(this);
}).call(this);

(function DecodeTestCase () {
    (function test_binary_to_term () {
        Erlang.binary_to_term('', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\0', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83z', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
    }).call(this);
    (function test_binary_to_term_atom () {
        Erlang.binary_to_term('\x83d', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83d\0', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83d\0\1', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83d\0\0', function(err, term) {
            assert.strictEqual(err, undefined);
            assert.deepEqual(term, new Erlang.OtpErlangAtom(''));
        });
        Erlang.binary_to_term('\x83s\0', function(err, term) {
            assert.strictEqual(err, undefined);
            assert.deepEqual(term, new Erlang.OtpErlangAtom(''));
        });
        Erlang.binary_to_term('\x83d\0\4test', function(err, term) {
            assert.strictEqual(err, undefined);
            assert.deepEqual(term, new Erlang.OtpErlangAtom('test'));
        });
        Erlang.binary_to_term('\x83s\4test', function(err, term) {
            assert.strictEqual(err, undefined);
            assert.deepEqual(term, new Erlang.OtpErlangAtom('test'));
        });
    }).call(this);
    (function test_binary_to_term_predefined_atom () {
        Erlang.binary_to_term('\x83s\4true', function(err, term) {
            assert.strictEqual(err, undefined);
            assert.strictEqual(term, true);
        });
        Erlang.binary_to_term('\x83s\5false', function(err, term) {
            assert.strictEqual(err, undefined);
            assert.strictEqual(term, false);
        });
        Erlang.binary_to_term('\x83d\0\11undefined', function(err, term) {
            assert.strictEqual(err, undefined);
            assert.deepEqual(term, new Erlang.OtpErlangAtom('undefined'));
        });
    }).call(this);
    (function test_binary_to_term_empty_list () {
        Erlang.binary_to_term('\x83j', function(err, term) {
            assert.strictEqual(err, undefined);
            assert.deepEqual(term, new Erlang.OtpErlangList([]));
        });
    }).call(this);
    (function test_binary_to_term_string_list () {
        Erlang.binary_to_term('\x83k', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83k\0', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83k\0\1', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83k\0\0', function(err, term) {
            assert.strictEqual(err, undefined);
            assert.equal(term, '');
        });
        Erlang.binary_to_term('\x83k\0\4test', function(err, term) {
            assert.strictEqual(err, undefined);
            assert.equal(term, 'test');
        });
    }).call(this);
    (function test_binary_to_term_list () {
        Erlang.binary_to_term('\x83l', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83l\0', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83l\0\0', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83l\0\0\0', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83l\0\0\0\0', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83l\0\0\0\0j', function(err, term) {
            assert.strictEqual(err, undefined);
            assert.deepEqual(term, new Erlang.OtpErlangList([]));
        });
        Erlang.binary_to_term('\x83l\0\0\0\2jjj', function(err, term) {
            assert.strictEqual(err, undefined);
            assert.deepEqual(term, new Erlang.OtpErlangList([
                                       new Erlang.OtpErlangList([]),
                                       new Erlang.OtpErlangList([])]));
        });
    }).call(this);
    (function test_binary_to_term_improper_list () {
        Erlang.binary_to_term('\x83l\0\0\0\0k', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83l\0\0\0\1jd\0\4tail', function(err, lst) {
            assert.strictEqual(err, undefined);
            assert.ok(lst instanceof Erlang.OtpErlangList);
            assert.deepEqual([new Erlang.OtpErlangList([]),
                              new Erlang.OtpErlangAtom('tail')], lst.value);
            assert.equal(true, lst.improper);
        });
    }).call(this);
    (function test_binary_to_term_small_tuple () {
        Erlang.binary_to_term('\x83h', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83h\1', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83h\0', function(err, term) {
            assert.strictEqual(err, undefined);
            assert.deepEqual(term, []);
        });
        Erlang.binary_to_term('\x83h\2jj', function(err, term) {
            assert.strictEqual(err, undefined);
            assert.deepEqual(term, [new Erlang.OtpErlangList([]),
                                    new Erlang.OtpErlangList([])]);
        });
    }).call(this);
    (function test_binary_to_term_large_tuple () {
        Erlang.binary_to_term('\x83i', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83i\0', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83i\0\0', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83i\0\0\0', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83i\0\0\0\1', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83i\0\0\0\0', function(err, term) {
            assert.strictEqual(err, undefined);
            assert.deepEqual(term, []);
        });
        Erlang.binary_to_term('\x83i\0\0\0\2jj', function(err, term) {
            assert.strictEqual(err, undefined);
            assert.deepEqual(term, [new Erlang.OtpErlangList([]),
                                    new Erlang.OtpErlangList([])]);
        });
    }).call(this);
    (function test_binary_to_term_small_integer () {
        Erlang.binary_to_term('\x83a', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83a\0', function(err, term) {
            assert.strictEqual(err, undefined);
            assert.deepEqual(term, 0);
        });
        Erlang.binary_to_term('\x83a\xff', function(err, term) {
            assert.strictEqual(err, undefined);
            assert.deepEqual(term, 255);
        });
    }).call(this);
    (function test_binary_to_term_integer () {
        Erlang.binary_to_term('\x83b', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83b\0', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83b\0\0', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83b\0\0\0', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83b\0\0\0\0', function(err, term) {
            assert.strictEqual(err, undefined);
            assert.equal(term, 0);
        });
        Erlang.binary_to_term('\x83b\x7f\xff\xff\xff', function(err, term) {
            assert.strictEqual(err, undefined);
            assert.equal(term, 2147483647);
        });
        Erlang.binary_to_term('\x83b\x80\0\0\0', function(err, term) {
            assert.strictEqual(err, undefined);
            assert.equal(term, -2147483648);
        });
        Erlang.binary_to_term('\x83b\xff\xff\xff\xff', function(err, term) {
            assert.strictEqual(err, undefined);
            assert.equal(term, -1);
        });
    }).call(this);
    (function test_binary_to_term_binary () {
        Erlang.binary_to_term('\x83m', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83m\0', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83m\0\0', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83m\0\0\0', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83m\0\0\0\1', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83m\0\0\0\0', function(err, term) {
            assert.strictEqual(err, undefined);
            assert.deepEqual(term, new Erlang.OtpErlangBinary(new Buffer([])));
        });
        Erlang.binary_to_term('\x83m\0\0\0\4data', function(err, term) {
            assert.strictEqual(err, undefined);
            assert.deepEqual(term, new Erlang.OtpErlangBinary(
                                       new Buffer('data', 'binary')));
        });
    }).call(this);
    (function test_binary_to_term_float () {
        Erlang.binary_to_term('\x83F', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83F\0', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83F\0', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83F\0\0', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83F\0\0\0', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83F\0\0\0\0', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83F\0\0\0\0\0', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83F\0\0\0\0\0\0', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83F\0\0\0\0\0\0\0', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83F\0\0\0\0\0\0\0\0', function(err, term) {
            assert.strictEqual(err, undefined);
            assert.equal(term, 0.0);
        });
        Erlang.binary_to_term('\x83F?\xf8\0\0\0\0\0\0', function(err, term) {
            assert.strictEqual(err, undefined);
            assert.equal(term, 1.5);
        });
    }).call(this);
    (function test_binary_to_term_small_big_integer () {
        Erlang.binary_to_term('\x83n', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83n\0', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83n\1\0', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83n\0\0', function(err, term) {
            assert.strictEqual(err, undefined);
            assert.equal(term, 0);
        });
        Erlang.binary_to_term('\x83n\6\0\1\2\3\4\5\6', function(err, term) {
            assert.strictEqual(err, undefined);
            assert.equal(term, 6618611909121);
        });
        Erlang.binary_to_term('\x83n\6\1\1\2\3\4\5\6', function(err, term) {
            assert.strictEqual(err, undefined);
            assert.equal(term, -6618611909121);
        });
    }).call(this);
    (function test_binary_to_term_big_integer () {
        Erlang.binary_to_term('\x83o', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83o\0', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83o\0\0', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83o\0\0\0', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83o\0\0\0\0', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83o\0\0\0\1\0', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83o\0\0\0\0\0', function(err, term) {
            assert.strictEqual(err, undefined);
            assert.equal(term, 0);
        });
        Erlang.binary_to_term('\x83o\0\0\0\6\0\1\2\3\4\5\6',
                              function(err, term) {
            assert.strictEqual(err, undefined);
            assert.equal(term, 6618611909121);
        });
        Erlang.binary_to_term('\x83o\0\0\0\6\1\1\2\3\4\5\6',
                              function(err, term) {
            assert.strictEqual(err, undefined);
            assert.equal(term, -6618611909121);
        });
    }).call(this);
    (function test_binary_to_term_compressed_term () {
        Erlang.binary_to_term('\x83P', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83P\0', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83P\0\0', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83P\0\0\0', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term('\x83P\0\0\0\0', function(err, term) {
            assert.ok(err instanceof Erlang.ParseException);
            assert.strictEqual(term, undefined);
        });
        Erlang.binary_to_term(
            '\x83P\0\0\0\x16\x78\xda\xcb\x66\x10\x49\xc1\2\0\x5d\x60\x08\x50',
            function(err, term) {
                assert.ok(err instanceof Erlang.ParseException);
                assert.strictEqual(term, undefined);
            }
        );
        Erlang.binary_to_term(
            '\x83P\0\0\0\x17\x78\xda\xcb\x66\x10\x49\xc1\2\0\x5d\x60\x08\x50',
            function(err, term) {
                assert.strictEqual(err, undefined);
                assert.equal(term, 'd'.repeat(20));
            }
        );
    }).call(this);
}).call(this);

(function EncodeTestCase () {
    (function test_term_to_binary_tuple () {
        Erlang.term_to_binary([], function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'), '\x83h\0');
        });
        Erlang.term_to_binary([[], []], function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'), '\x83h\2h\0h\0');
        });
        Erlang.term_to_binary(new Array(255).fill([]), function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'),
                         '\x83h\xff' + 'h\0'.repeat(255));
        });
        Erlang.term_to_binary(new Array(256).fill([]), function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'),
                         '\x83i\0\0\1\0' + 'h\0'.repeat(256));
        });
    }).call(this);
    (function test_term_to_binary_empty_list () {
        Erlang.term_to_binary(new Erlang.OtpErlangList([]),
                              function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'), '\x83j');
        });
    }).call(this);
    (function test_term_to_binary_string_list () {
        Erlang.term_to_binary('', function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'), '\x83j');
        });
        Erlang.term_to_binary('\0', function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'), '\x83k\0\1\0');
        });
        var s = '\x00\x01\x02\x03\x04\x05\x06\x07\x08\t\n\x0b\x0c\r' +
                '\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a' +
                '\x1b\x1c\x1d\x1e\x1f !"#$%&\'()*+,-./0123456789:;<=>' +
                '?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopq' +
                'rstuvwxyz{|}~\x7f\x80\x81\x82\x83\x84\x85\x86\x87\x88' +
                '\x89\x8a\x8b\x8c\x8d\x8e\x8f\x90\x91\x92\x93\x94\x95' +
                '\x96\x97\x98\x99\x9a\x9b\x9c\x9d\x9e\x9f\xa0\xa1\xa2' +
                '\xa3\xa4\xa5\xa6\xa7\xa8\xa9\xaa\xab\xac\xad\xae\xaf' +
                '\xb0\xb1\xb2\xb3\xb4\xb5\xb6\xb7\xb8\xb9\xba\xbb\xbc' +
                '\xbd\xbe\xbf\xc0\xc1\xc2\xc3\xc4\xc5\xc6\xc7\xc8\xc9' +
                '\xca\xcb\xcc\xcd\xce\xcf\xd0\xd1\xd2\xd3\xd4\xd5\xd6' +
                '\xd7\xd8\xd9\xda\xdb\xdc\xdd\xde\xdf\xe0\xe1\xe2\xe3' +
                '\xe4\xe5\xe6\xe7\xe8\xe9\xea\xeb\xec\xed\xee\xef\xf0' +
                '\xf1\xf2\xf3\xf4\xf5\xf6\xf7\xf8\xf9\xfa\xfb\xfc\xfd\xfe\xff';
        Erlang.term_to_binary(s, function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'), '\x83k\1\0' + s);
        });
    }).call(this);
    (function test_term_to_binary_list_basic () {
        Erlang.term_to_binary(new Erlang.OtpErlangList([]),
                              function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'), '\x83\x6A');
        });
        Erlang.term_to_binary(new Erlang.OtpErlangList(['']),
                              function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'),
                '\x83\x6C\x00\x00\x00\x01\x6A\x6A');
        });
        Erlang.term_to_binary(new Erlang.OtpErlangList([1]),
                              function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'),
                '\x83\x6C\x00\x00\x00\x01\x61\x01\x6A');
        });
        Erlang.term_to_binary(new Erlang.OtpErlangList([255]),
                              function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'),
                '\x83\x6C\x00\x00\x00\x01\x61\xFF\x6A');
        });
        Erlang.term_to_binary(new Erlang.OtpErlangList([256]),
                              function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'),
                '\x83\x6C\x00\x00\x00\x01\x62\x00\x00\x01\x00\x6A');
        });
        Erlang.term_to_binary(new Erlang.OtpErlangList([2147483647]),
                              function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'),
                '\x83\x6C\x00\x00\x00\x01\x62\x7F\xFF\xFF\xFF\x6A');
        });
        Erlang.term_to_binary(new Erlang.OtpErlangList([2147483648]),
                              function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'),
                '\x83\x6C\x00\x00\x00\x01\x6E\x04\x00\x00\x00\x00\x80\x6A');
        });
        Erlang.term_to_binary(new Erlang.OtpErlangList([0]),
                              function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'),
                '\x83\x6C\x00\x00\x00\x01\x61\x00\x6A');
        });
        Erlang.term_to_binary(new Erlang.OtpErlangList([-1]),
                              function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'),
                '\x83\x6C\x00\x00\x00\x01\x62\xFF\xFF\xFF\xFF\x6A');
        });
        Erlang.term_to_binary(new Erlang.OtpErlangList([-256]),
                              function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'),
                '\x83\x6C\x00\x00\x00\x01\x62\xFF\xFF\xFF\x00\x6A');
        });
        Erlang.term_to_binary(new Erlang.OtpErlangList([-257]),
                              function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'),
                '\x83\x6C\x00\x00\x00\x01\x62\xFF\xFF\xFE\xFF\x6A');
        });
        Erlang.term_to_binary(new Erlang.OtpErlangList([-2147483648]),
                              function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'),
                '\x83\x6C\x00\x00\x00\x01\x62\x80\x00\x00\x00\x6A');
        });
        Erlang.term_to_binary(new Erlang.OtpErlangList([-2147483649]),
                              function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'),
                '\x83\x6C\x00\x00\x00\x01\x6E\x04\x01\x01\x00\x00\x80\x6A');
        });
        Erlang.term_to_binary(new Erlang.OtpErlangList(['test']),
                              function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'),
                '\x83\x6C\x00\x00\x00\x01\x6B\x00\x04\x74\x65\x73\x74\x6A');
        });
        Erlang.term_to_binary(new Erlang.OtpErlangList([373, 455]),
                              function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'),
                '\x83\x6C\x00\x00\x00\x02\x62\x00\x00\x01\x75\x62\x00\x00' +
                '\x01\xC7\x6A');
        });
        Erlang.term_to_binary(new Erlang.OtpErlangList([
                                  new Erlang.OtpErlangList([])]),
                              function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'),
                '\x83\x6C\x00\x00\x00\x01\x6A\x6A');
        });
        Erlang.term_to_binary(new Erlang.OtpErlangList([
                                  new Erlang.OtpErlangList([]),
                                  new Erlang.OtpErlangList([])]),
                              function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'),
                '\x83\x6C\x00\x00\x00\x02\x6A\x6A\x6A');
        });
        Erlang.term_to_binary(new Erlang.OtpErlangList([
                                  new Erlang.OtpErlangList(['this', 'is']),
                                  new Erlang.OtpErlangList([
                                      new Erlang.OtpErlangList(['a'])]),
                                  'test']),
                              function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'),
                '\x83\x6C\x00\x00\x00\x03\x6C\x00\x00\x00\x02\x6B\x00\x04' +
                '\x74\x68\x69\x73\x6B\x00\x02\x69\x73\x6A\x6C\x00\x00\x00' +
                '\x01\x6C\x00\x00\x00\x01\x6B\x00\x01\x61\x6A\x6A\x6B\x00' +
                '\x04\x74\x65\x73\x74\x6A');
        });
    }).call(this);
    (function test_term_to_binary_list () {
        Erlang.term_to_binary(new Erlang.OtpErlangList([
                                  new Erlang.OtpErlangList([])]),
                              function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'), '\x83l\0\0\0\1jj');
        });
        Erlang.term_to_binary(new Erlang.OtpErlangList([
                                  new Erlang.OtpErlangList([]),
                                  new Erlang.OtpErlangList([]),
                                  new Erlang.OtpErlangList([]),
                                  new Erlang.OtpErlangList([]),
                                  new Erlang.OtpErlangList([])]),
                              function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'), '\x83l\0\0\0\5jjjjjj');
        });
    }).call(this);
    (function test_term_to_binary_improper_list () {
        Erlang.term_to_binary(new Erlang.OtpErlangList([[], []], true),
                              function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'), '\x83l\0\0\0\1h\0h\0');
        });
        Erlang.term_to_binary(new Erlang.OtpErlangList([0, 1], true),
                              function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'), '\x83l\0\0\0\1a\0a\1');
        });
    }).call(this);
    (function test_term_to_binary_unicode () {
        Erlang.term_to_binary('', function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'), '\x83j');
        });
        Erlang.term_to_binary('test', function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'), '\x83k\0\4test');
        });
        Erlang.term_to_binary('\x00\xc3\xbf', function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'), '\x83k\0\3\x00\xc3\xbf');
        });
        Erlang.term_to_binary('\xc4\x80', function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'), '\x83k\0\2\xc4\x80');
        });
        Erlang.term_to_binary('\xd1\x82\xd0\xb5\xd1\x81\xd1\x82',
                              function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'),
                '\x83k\0\x08\xd1\x82\xd0\xb5\xd1\x81\xd1\x82');
        });
        // becomes a list of small integers
        Erlang.term_to_binary('\xd0\x90'.repeat(65536),
                              function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'),
                '\x83l\x00\x02\x00\x00' + 'a\xd0a\x90'.repeat(65536) + 'j');
                         
        });
    }).call(this);
    (function test_term_to_binary_atom () {
        Erlang.term_to_binary(new Erlang.OtpErlangAtom(''),
                              function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'), '\x83s\0');
        });
        Erlang.term_to_binary(new Erlang.OtpErlangAtom('test'),
                              function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'), '\x83s\4test');
        });
    }).call(this);
    (function test_term_to_binary_string_basic () {
        Erlang.term_to_binary('', function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'), '\x83\x6A');
        });
        Erlang.term_to_binary('test', function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'),
                '\x83\x6B\x00\x04\x74\x65\x73\x74');
        });
        Erlang.term_to_binary('two words', function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'),
                '\x83\x6B\x00\x09\x74\x77\x6F\x20\x77\x6F\x72\x64\x73');
        });
        Erlang.term_to_binary('testing multiple words', function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'),
                '\x83\x6B\x00\x16\x74\x65\x73\x74\x69\x6E\x67\x20\x6D' +
                '\x75\x6C\x74\x69\x70\x6C\x65\x20\x77\x6F\x72\x64\x73');
        });
        Erlang.term_to_binary(' ', function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'), '\x83\x6B\x00\x01\x20');
        });
        Erlang.term_to_binary('  ', function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'), '\x83\x6B\x00\x02\x20\x20');
        });
        Erlang.term_to_binary('1', function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'), '\x83\x6B\x00\x01\x31');
        });
        Erlang.term_to_binary('37', function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'), '\x83\x6B\x00\x02\x33\x37');
        });
        Erlang.term_to_binary('one = 1', function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'),
                '\x83\x6B\x00\x07\x6F\x6E\x65\x20\x3D\x20\x31');
        });
        Erlang.term_to_binary('!@#$%^&*()_+-=[]{}\\|;\':",./<>?~`',
                              function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'),
                '\x83\x6B\x00\x20\x21\x40\x23\x24\x25\x5E\x26\x2A\x28' +
                '\x29\x5F\x2B\x2D\x3D\x5B\x5D\x7B\x7D\x5C\x7C\x3B\x27' +
                '\x3A\x22\x2C\x2E\x2F\x3C\x3E\x3F\x7E\x60');
        });
        Erlang.term_to_binary('\"\b\f\n\r\t\v\123\x12',
                              function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'),
                '\x83\x6B\x00\x09\x22\x08\x0C\x0A\x0D\x09\x0B\x53\x12');
        });
    }).call(this);
    (function test_term_to_binary_string () {
        Erlang.term_to_binary('', function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'), '\x83j');
        });
        Erlang.term_to_binary('test', function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'), '\x83k\0\4test');
        });
    }).call(this);
    (function test_term_to_binary_boolean () {
        Erlang.term_to_binary(true, function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'), '\x83s\4true');
        });
        Erlang.term_to_binary(false, function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'), '\x83s\5false');
        });
    }).call(this);
    (function test_term_to_binary_short_integer () {
        Erlang.term_to_binary(0, function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'), '\x83a\0');
        });
        Erlang.term_to_binary(255, function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'), '\x83a\xff');
        });
    }).call(this);
    (function test_term_to_binary_integer () {
        Erlang.term_to_binary(-1, function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'), '\x83b\xff\xff\xff\xff');
        });
        Erlang.term_to_binary(-2147483648, function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'), '\x83b\x80\0\0\0');
        });
        Erlang.term_to_binary(256, function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'), '\x83b\0\0\1\0');
        });
        Erlang.term_to_binary(2147483647, function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'), '\x83b\x7f\xff\xff\xff');
        });
    }).call(this);
    (function test_term_to_binary_long_integer () {
        Erlang.term_to_binary(2147483648, function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'), '\x83n\4\0\0\0\0\x80');
        });
        Erlang.term_to_binary(-2147483649, function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'), '\x83n\4\1\1\0\0\x80');
        });
    }).call(this);
    (function test_term_to_binary_float () {
        // javascript makes 0.0 === 0, so it becomes an integer
        Erlang.term_to_binary(0.0, function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'), '\x83a\0');
        });
        Erlang.term_to_binary(0.5, function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'),
                         '\x83F?\xe0\0\0\0\0\0\0');
        });
        Erlang.term_to_binary(-0.5, function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'),
                         '\x83F\xbf\xe0\0\0\0\0\0\0');
        });
        Erlang.term_to_binary(3.1415926, function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'),
                         '\x83F@\t!\xfbM\x12\xd8J');
        });
        Erlang.term_to_binary(-3.1415926, function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'),
                         '\x83F\xc0\t!\xfbM\x12\xd8J');
        });
    }).call(this);
    (function test_term_to_binary_float () {
        Erlang.term_to_binary(new Erlang.OtpErlangList(new Array(15).fill(
                                  new Erlang.OtpErlangList([]))),
                              function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'),
                '\x83P\x00\x00\x00\x15x\x9c\xcba``\xe0\xcfB\x03\x00B@\x07\x1c');
        }, true);
        Erlang.term_to_binary(new Erlang.OtpErlangList(new Array(15).fill(
                                  new Erlang.OtpErlangList([]))),
                              function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'),
                '\x83P\x00\x00\x00\x15x\x9c\xcba``\xe0\xcfB\x03\x00B@\x07\x1c');
        }, 6);
        Erlang.term_to_binary(new Erlang.OtpErlangList(new Array(15).fill(
                                  new Erlang.OtpErlangList([]))),
                              function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'),
                '\x83P\x00\x00\x00\x15x\xda\xcba``\xe0\xcfB\x03\x00B@\x07\x1c');
        }, 9);
        // node.js zlib is unable to use compression level 0 properly
        // (in version 0.6.12) so showing compression level 1 instead
        Erlang.term_to_binary(new Erlang.OtpErlangList(new Array(15).fill(
                                  new Erlang.OtpErlangList([]))),
                              function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'),
                '\x83P\x00\x00\x00\x15x\x01\xcba``\xe0\xcfB\x03\x00B@\x07\x1c');
        }, 1);
        Erlang.term_to_binary('d'.repeat(20), function(err, binary) {
            assert.strictEqual(err, undefined);
            assert.equal(binary.toString('binary'),
                '\x83P\0\0\0\x17\x78\xda\xcb\x66\x10\x49\xc1\2\0' +
                '\x5d\x60\x08\x50');
        }, 9);
    }).call(this);
}).call(this);
