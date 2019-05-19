package erlang

//-*-Mode:Go;coding:utf-8;tab-width:4;c-basic-offset:4-*-
// ex: set ft=go fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:
//
// MIT License
//
// Copyright (c) 2017-2019 Michael Truog <mjtruog at protonmail dot com>
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

import (
	"fmt"
	"log"
	"math/big"
	"reflect"
	"strings"
	"testing"
)

func assertEqual(t *testing.T, expect interface{}, result interface{}, message string) {
	// Go doesn't believe in assertions (https://golang.org/doc/faq#assertions)
	// (Go isn't pursuing fail-fast development or fault-tolerance)
	if reflect.DeepEqual(expect, result) {
		return
	}
	if len(message) == 0 {
		message = fmt.Sprintf("%#v != %#v", expect, result)
	}
	t.Fail()
	log.SetPrefix("\t")
	log.SetFlags(log.Lshortfile)
	log.Output(2, message)
}

func assertDecodeError(t *testing.T, expectedError, b string, message string) {
	_, err := BinaryToTerm([]byte(b))
	if err == nil {
		log.SetPrefix("\t")
		log.SetFlags(log.Lshortfile)
		log.Output(2, fmt.Sprintf("no error to compare with \"%s\"", expectedError))
		t.FailNow()
		return
	}
	resultError := err.Error()
	if expectedError == resultError {
		return
	}
	if len(message) == 0 {
		message = fmt.Sprintf("\"%s\" != \"%s\"", expectedError, resultError)
	}
	t.Fail()
	log.SetPrefix("\t")
	log.SetFlags(log.Lshortfile)
	log.Output(2, message)
}

func decode(t *testing.T, b string) interface{} {
	term, err := BinaryToTerm([]byte(b))
	if err != nil {
		log.SetPrefix("\t")
		log.SetFlags(log.Lshortfile)
		log.Output(2, err.Error())
		t.FailNow()
		return nil
	}
	return term
}

func encode(t *testing.T, term interface{}, compressed int) string {
	b, err := TermToBinary(term, compressed)
	if err != nil {
		log.SetPrefix("\t")
		log.SetFlags(log.Lshortfile)
		log.Output(2, err.Error())
		t.FailNow()
		return ""
	}
	return string(b)
}

func TestAtom(t *testing.T) {
	atom1 := OtpErlangAtom("test")
	assertEqual(t, OtpErlangAtom("test"), atom1, "")
	assertEqual(t, strings.Repeat("X", 255), string(OtpErlangAtom(strings.Repeat("X", 255))), "")
	assertEqual(t, strings.Repeat("X", 256), string(OtpErlangAtom(strings.Repeat("X", 256))), "")
}

func TestPid(t *testing.T) {
	pid1 := OtpErlangPid{NodeTag: 100, Node: []uint8{0x0, 0xd, 0x6e, 0x6f, 0x6e, 0x6f, 0x64, 0x65, 0x40, 0x6e, 0x6f, 0x68, 0x6f, 0x73, 0x74}, ID: []uint8{0x0, 0x0, 0x0, 0x3b}, Serial: []uint8{0x0, 0x0, 0x0, 0x0}, Creation: []uint8{0x0}}
	binary := "\x83\x67\x64\x00\x0D\x6E\x6F\x6E\x6F\x64\x65\x40\x6E\x6F\x68\x6F\x73\x74\x00\x00\x00\x3B\x00\x00\x00\x00\x00"
	assertEqual(t, pid1, decode(t, binary), "")
	assertEqual(t, binary, encode(t, pid1, -1), "")

	pidOldBinary := "\x83\x67\x64\x00\x0D\x6E\x6F\x6E\x6F\x64\x65\x40\x6E\x6F\x68\x6F\x73\x74\x00\x00\x00\x4E\x00\x00\x00\x00\x00"
	pidOld := decode(t, pidOldBinary)
	assertEqual(t, "\x83gd\x00\rnonode@nohost\x00\x00\x00N\x00\x00\x00\x00\x00", encode(t, pidOld, -1), "")
	pidNewBinary := "\x83\x58\x64\x00\x0D\x6E\x6F\x6E\x6F\x64\x65\x40\x6E\x6F\x68\x6F\x73\x74\x00\x00\x00\x4E\x00\x00\x00\x00\x00\x00\x00\x00"
	pidNew := decode(t, pidNewBinary)
	assertEqual(t, "\x83Xd\x00\rnonode@nohost\x00\x00\x00N\x00\x00\x00\x00\x00\x00\x00\x00", encode(t, pidNew, -1), "")
}

func TestPort(t *testing.T) {
	portOldBinary := "\x83\x66\x64\x00\x0D\x6E\x6F\x6E\x6F\x64\x65\x40\x6E\x6F\x68\x6F\x73\x74\x00\x00\x00\x06\x00"
	portOld := decode(t, portOldBinary)
	assertEqual(t, "\x83fd\x00\rnonode@nohost\x00\x00\x00\x06\x00", encode(t, portOld, -1), "")
	portNewBinary := "\x83\x59\x64\x00\x0D\x6E\x6F\x6E\x6F\x64\x65\x40\x6E\x6F\x68\x6F\x73\x74\x00\x00\x00\x06\x00\x00\x00\x00"
	portNew := decode(t, portNewBinary)
	assertEqual(t, "\x83Yd\x00\rnonode@nohost\x00\x00\x00\x06\x00\x00\x00\x00", encode(t, portNew, -1), "")
}

func TestFunction(t *testing.T) {
	fun1 := OtpErlangFunction{Tag: 113, Value: []uint8{0x64, 0x0, 0x5, 0x6c, 0x69, 0x73, 0x74, 0x73, 0x64, 0x0, 0x6, 0x6d, 0x65, 0x6d, 0x62, 0x65, 0x72, 0x61, 0x2}}
	binary := "\x83\x71\x64\x00\x05\x6C\x69\x73\x74\x73\x64\x00\x06\x6D\x65\x6D\x62\x65\x72\x61\x02"
	assertEqual(t, fun1, decode(t, binary), "")
	assertEqual(t, binary, encode(t, fun1, -1), "")
}

func TestReference(t *testing.T) {
	ref1 := OtpErlangReference{NodeTag: 100, Node: []uint8{0x0, 0xd, 0x6e, 0x6f, 0x6e, 0x6f, 0x64, 0x65, 0x40, 0x6e, 0x6f, 0x68, 0x6f, 0x73, 0x74}, ID: []uint8{0x0, 0x0, 0x0, 0xaf, 0x0, 0x0, 0x0, 0x3, 0x0, 0x0, 0x0, 0x0}, Creation: []uint8{0x0}}
	binary := "\x83\x72\x00\x03\x64\x00\x0D\x6E\x6F\x6E\x6F\x64\x65\x40\x6E\x6F\x68\x6F\x73\x74\x00\x00\x00\x00\xAF\x00\x00\x00\x03\x00\x00\x00\x00"
	assertEqual(t, ref1, decode(t, binary), "")
	assertEqual(t, binary, encode(t, ref1, -1), "")

	refNewBinary := "\x83\x72\x00\x03\x64\x00\x0D\x6E\x6F\x6E\x6F\x64\x65\x40\x6E\x6F\x68\x6F\x73\x74\x00\x00\x03\xE8\x4E\xE7\x68\x00\x02\xA4\xC8\x53\x40"
	refNew := decode(t, refNewBinary)
	assertEqual(t, "\x83r\x00\x03d\x00\rnonode@nohost\x00\x00\x03\xe8N\xe7h\x00\x02\xa4\xc8S@", encode(t, refNew, -1), "")
	refNewerBinary := "\x83\x5A\x00\x03\x64\x00\x0D\x6E\x6F\x6E\x6F\x64\x65\x40\x6E\x6F\x68\x6F\x73\x74\x00\x00\x00\x00\x00\x01\xAC\x03\xC7\x00\x00\x04\xBB\xB2\xCA\xEE"
	refNewer := decode(t, refNewerBinary)
	assertEqual(t, "\x83Z\x00\x03d\x00\rnonode@nohost\x00\x00\x00\x00\x00\x01\xac\x03\xc7\x00\x00\x04\xbb\xb2\xca\xee", encode(t, refNewer, -1), "")
}

func TestDecodeBinaryToTerm(t *testing.T) {
	assertDecodeError(t, "null input", "", "")
	assertDecodeError(t, "null input", "\x00", "")
	assertDecodeError(t, "null input", "\x83", "")
	assertDecodeError(t, "invalid tag", "\x83z", "")
}
func TestDecodeBinaryToTermAtom(t *testing.T) {
	assertDecodeError(t, "EOF", "\x83d", "")
	assertDecodeError(t, "unexpected EOF", "\x83d\x00", "")
	assertDecodeError(t, "EOF", "\x83d\x00\x01", "")
	assertDecodeError(t, "EOF", "\x83s\x01", "")
	assertEqual(t, OtpErlangAtom(""), decode(t, "\x83d\x00\x00"), "")
	assertEqual(t, OtpErlangAtom(""), decode(t, "\x83s\x00"), "")
	assertEqual(t, OtpErlangAtom("test"), decode(t, "\x83d\x00\x04test"), "")
	assertEqual(t, OtpErlangAtom("test"), decode(t, "\x83s\x04test"), "")
}
func TestDecodeBinaryToTermPredefinedAtom(t *testing.T) {
	assertEqual(t, OtpErlangAtom("true"), decode(t, "\x83s\x04true"), "")
	assertEqual(t, OtpErlangAtom("false"), decode(t, "\x83s\x05false"), "")
	assertEqual(t, OtpErlangAtom("undefined"), decode(t, "\x83d\x00\x09undefined"), "")
}
func TestDecodeBinaryToTermEmptyList(t *testing.T) {
	assertEqual(t, OtpErlangList{Value: make([]interface{}, 0), Improper: false}, decode(t, "\x83j"), "")
}
func TestDecodeBinaryToTermStringList(t *testing.T) {
	assertDecodeError(t, "EOF", "\x83k", "")
	assertDecodeError(t, "unexpected EOF", "\x83k\x00", "")
	assertDecodeError(t, "EOF", "\x83k\x00\x01", "")
	assertEqual(t, "", decode(t, "\x83k\x00\x00"), "")
	assertEqual(t, "test", decode(t, "\x83k\x00\x04test"), "")
}
func TestDecodeBinaryToTermList(t *testing.T) {
	assertDecodeError(t, "EOF", "\x83l", "")
	assertDecodeError(t, "unexpected EOF", "\x83l\x00", "")
	assertDecodeError(t, "unexpected EOF", "\x83l\x00\x00", "")
	assertDecodeError(t, "unexpected EOF", "\x83l\x00\x00\x00", "")
	assertDecodeError(t, "EOF", "\x83l\x00\x00\x00\x00", "")
	assertEqual(t, OtpErlangList{Value: make([]interface{}, 0), Improper: false}, decode(t, "\x83l\x00\x00\x00\x00j"), "")
	list1 := make([]interface{}, 2)
	list1[0] = OtpErlangList{Value: make([]interface{}, 0), Improper: false}
	list1[1] = OtpErlangList{Value: make([]interface{}, 0), Improper: false}
	assertEqual(t, OtpErlangList{Value: list1, Improper: false}, decode(t, "\x83l\x00\x00\x00\x02jjj"), "")
}
func TestDecodeBinaryToTermImproperList(t *testing.T) {
	assertDecodeError(t, "EOF", "\x83l\x00\x00\x00k", "")
	lst := decode(t, "\x83l\x00\x00\x00\x01jd\x00\x04tail")
	lstCmp := make([]interface{}, 2)
	lstCmp[0] = OtpErlangList{Value: make([]interface{}, 0), Improper: false}
	lstCmp[1] = OtpErlangAtom("tail")
	assertEqual(t, OtpErlangList{Value: lstCmp, Improper: true}, lst, "")
}
func TestDecodeBinaryToTermSmallTuple(t *testing.T) {
	assertDecodeError(t, "EOF", "\x83h", "")
	assertDecodeError(t, "EOF", "\x83h\x01", "")
	assertEqual(t, OtpErlangTuple(make([]interface{}, 0)), decode(t, "\x83h\x00"), "")
	tuple1 := make([]interface{}, 2)
	tuple1[0] = OtpErlangList{Value: make([]interface{}, 0), Improper: false}
	tuple1[1] = OtpErlangList{Value: make([]interface{}, 0), Improper: false}
	assertEqual(t, OtpErlangTuple(tuple1), decode(t, "\x83h\x02jj"), "")
}
func TestDecodeBinaryToTermLargeTuple(t *testing.T) {
	assertDecodeError(t, "EOF", "\x83i", "")
	assertDecodeError(t, "unexpected EOF", "\x83i\x00", "")
	assertDecodeError(t, "unexpected EOF", "\x83i\x00\x00", "")
	assertDecodeError(t, "unexpected EOF", "\x83i\x00\x00\x00", "")
	assertDecodeError(t, "EOF", "\x83i\x00\x00\x00\x01", "")
	assertEqual(t, OtpErlangTuple(make([]interface{}, 0)), decode(t, "\x83i\x00\x00\x00\x00"), "")
	tuple1 := make([]interface{}, 2)
	tuple1[0] = OtpErlangList{Value: make([]interface{}, 0), Improper: false}
	tuple1[1] = OtpErlangList{Value: make([]interface{}, 0), Improper: false}
	assertEqual(t, OtpErlangTuple(tuple1), decode(t, "\x83i\x00\x00\x00\x02jj"), "")
}
func TestDecodeBinaryToTermSmallInteger(t *testing.T) {
	assertDecodeError(t, "EOF", "\x83a", "")
	assertEqual(t, uint8(0), decode(t, "\x83a\x00"), "")
	assertEqual(t, uint8(255), decode(t, "\x83a\xff"), "")
}
func TestDecodeBinaryToTermInteger(t *testing.T) {
	assertDecodeError(t, "EOF", "\x83b", "")
	assertDecodeError(t, "unexpected EOF", "\x83b\x00", "")
	assertDecodeError(t, "unexpected EOF", "\x83b\x00\x00", "")
	assertDecodeError(t, "unexpected EOF", "\x83b\x00\x00\x00", "")
	assertEqual(t, int32(0), decode(t, "\x83b\x00\x00\x00\x00"), "")
	assertEqual(t, int32(2147483647), decode(t, "\x83b\x7f\xff\xff\xff"), "")
	assertEqual(t, int32(-2147483648), decode(t, "\x83b\x80\x00\x00\x00"), "")
	assertEqual(t, int32(-1), decode(t, "\x83b\xff\xff\xff\xff"), "")
}
func TestDecodeBinaryToTermBinary(t *testing.T) {
	assertDecodeError(t, "EOF", "\x83m", "")
	assertDecodeError(t, "unexpected EOF", "\x83m\x00", "")
	assertDecodeError(t, "unexpected EOF", "\x83m\x00\x00", "")
	assertDecodeError(t, "unexpected EOF", "\x83m\x00\x00\x00", "")
	assertEqual(t, OtpErlangBinary{Value: []byte(""), Bits: 8}, decode(t, "\x83m\x00\x00\x00\x00"), "")
	assertEqual(t, OtpErlangBinary{Value: []byte("data"), Bits: 8}, decode(t, "\x83m\x00\x00\x00\x04data"), "")
}
func TestDecodeBinaryToTermFloat(t *testing.T) {
	assertDecodeError(t, "EOF", "\x83F", "")
	assertDecodeError(t, "unexpected EOF", "\x83F\x00", "")
	assertDecodeError(t, "unexpected EOF", "\x83F\x00\x00", "")
	assertDecodeError(t, "unexpected EOF", "\x83F\x00\x00\x00", "")
	assertDecodeError(t, "unexpected EOF", "\x83F\x00\x00\x00\x00", "")
	assertDecodeError(t, "unexpected EOF", "\x83F\x00\x00\x00\x00\x00", "")
	assertDecodeError(t, "unexpected EOF", "\x83F\x00\x00\x00\x00\x00\x00", "")
	assertDecodeError(t, "unexpected EOF", "\x83F\x00\x00\x00\x00\x00\x00\x00", "")
	assertEqual(t, 0.0, decode(t, "\x83F\x00\x00\x00\x00\x00\x00\x00\x00"), "")
	assertEqual(t, 1.5, decode(t, "\x83F?\xf8\x00\x00\x00\x00\x00\x00"), "")
}
func TestDecodeBinaryToTermSmallBigInteger(t *testing.T) {
	assertDecodeError(t, "EOF", "\x83n", "")
	assertDecodeError(t, "EOF", "\x83n\x00", "")
	assertDecodeError(t, "EOF", "\x83n\x01\x00", "")
	assertEqual(t, big.NewInt(0), decode(t, "\x83n\x00\x00"), "")
	bignum1, _ := big.NewInt(0).SetString("6618611909121", 10)
	assertEqual(t, bignum1, decode(t, "\x83n\x06\x00\x01\x02\x03\x04\x05\x06"), "")
	bignum2, _ := big.NewInt(0).SetString("-6618611909121", 10)
	assertEqual(t, bignum2, decode(t, "\x83n\x06\x01\x01\x02\x03\x04\x05\x06"), "")
}
func TestDecodeBinaryToTermLargeBigInteger(t *testing.T) {
	assertDecodeError(t, "EOF", "\x83o", "")
	assertDecodeError(t, "unexpected EOF", "\x83o\x00", "")
	assertDecodeError(t, "unexpected EOF", "\x83o\x00\x00", "")
	assertDecodeError(t, "unexpected EOF", "\x83o\x00\x00\x00", "")
	assertDecodeError(t, "EOF", "\x83o\x00\x00\x00\x00", "")
	assertDecodeError(t, "EOF", "\x83o\x00\x00\x00\x01\x00", "")
	assertEqual(t, big.NewInt(0), decode(t, "\x83o\x00\x00\x00\x00\x00"), "")
	bignum1, _ := big.NewInt(0).SetString("6618611909121", 10)
	assertEqual(t, bignum1, decode(t, "\x83o\x00\x00\x00\x06\x00\x01\x02\x03\x04\x05\x06"), "")
	bignum2, _ := big.NewInt(0).SetString("-6618611909121", 10)
	assertEqual(t, bignum2, decode(t, "\x83o\x00\x00\x00\x06\x01\x01\x02\x03\x04\x05\x06"), "")
}
func TestDecodeBinaryToTermCompressedTerm(t *testing.T) {
	assertDecodeError(t, "EOF", "\x83P", "")
	assertDecodeError(t, "unexpected EOF", "\x83P\x00", "")
	assertDecodeError(t, "unexpected EOF", "\x83P\x00\x00", "")
	assertDecodeError(t, "unexpected EOF", "\x83P\x00\x00\x00", "")
	assertDecodeError(t, "compressed data null", "\x83P\x00\x00\x00\x00", "")
	assertEqual(t, strings.Repeat("d", 20), decode(t, "\x83P\x00\x00\x00\x17\x78\xda\xcb\x66\x10\x49\xc1\x02\x00\x5d\x60\x08\x50"), "")
}

func TestEncodeTermToBinaryTuple(t *testing.T) {
	assertEqual(t, "\x83h\x00", encode(t, []interface{}{}, -1), "")
	assertEqual(t, "\x83h\x00", encode(t, OtpErlangTuple{}, -1), "")
	assertEqual(t, "\x83h\x02h\x00h\x00", encode(t, []interface{}{[]interface{}{}, []interface{}{}}, -1), "")
	tuple1 := make(OtpErlangTuple, 255)
	for i := 0; i < 255; i++ {
		tuple1[i] = OtpErlangTuple{}
	}
	assertEqual(t, "\x83h\xff"+strings.Repeat("h\x00", 255), encode(t, tuple1, -1), "")
	tuple2 := make(OtpErlangTuple, 256)
	for i := 0; i < 256; i++ {
		tuple2[i] = OtpErlangTuple{}
	}
	assertEqual(t, "\x83i\x00\x00\x01\x00"+strings.Repeat("h\x00", 256), encode(t, tuple2, -1), "")
}
func TestEncodeTermToBinaryEmptyList(t *testing.T) {
	assertEqual(t, "\x83j", encode(t, OtpErlangList{}, -1), "")
	assertEqual(t, "\x83j", encode(t, OtpErlangList{Value: []interface{}{}}, -1), "")
	assertEqual(t, "\x83j", encode(t, OtpErlangList{Value: []interface{}{}, Improper: false}, -1), "")
}
func TestEncodeTermToBinaryStringList(t *testing.T) {
	assertEqual(t, "\x83j", encode(t, "", -1), "")
	assertEqual(t, "\x83k\x00\x01\x00", encode(t, "\x00", -1), "")
	s := "\x00\x01\x02\x03\x04\x05\x06\x07\x08\t\n\x0b\x0c\r\x0e\x0f\x10\x11\x12\x13\x14\x15\x16\x17\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\\]^_`abcdefghijklmnopqrstuvwxyz{|}~\x7f\x80\x81\x82\x83\x84\x85\x86\x87\x88\x89\x8a\x8b\x8c\x8d\x8e\x8f\x90\x91\x92\x93\x94\x95\x96\x97\x98\x99\x9a\x9b\x9c\x9d\x9e\x9f\xa0\xa1\xa2\xa3\xa4\xa5\xa6\xa7\xa8\xa9\xaa\xab\xac\xad\xae\xaf\xb0\xb1\xb2\xb3\xb4\xb5\xb6\xb7\xb8\xb9\xba\xbb\xbc\xbd\xbe\xbf\xc0\xc1\xc2\xc3\xc4\xc5\xc6\xc7\xc8\xc9\xca\xcb\xcc\xcd\xce\xcf\xd0\xd1\xd2\xd3\xd4\xd5\xd6\xd7\xd8\xd9\xda\xdb\xdc\xdd\xde\xdf\xe0\xe1\xe2\xe3\xe4\xe5\xe6\xe7\xe8\xe9\xea\xeb\xec\xed\xee\xef\xf0\xf1\xf2\xf3\xf4\xf5\xf6\xf7\xf8\xf9\xfa\xfb\xfc\xfd\xfe\xff"
	assertEqual(t, "\x83k\x01\x00"+s, encode(t, s, -1), "")
}
func TestEncodeTermToBinaryListBasic(t *testing.T) {
	assertEqual(t, "\x83\x6A", encode(t, OtpErlangList{}, -1), "")
	assertEqual(t, "\x83\x6C\x00\x00\x00\x01\x6A\x6A", encode(t, OtpErlangList{Value: []interface{}{""}}, -1), "")
	assertEqual(t, "\x83\x6C\x00\x00\x00\x01\x61\x01\x6A", encode(t, OtpErlangList{Value: []interface{}{1}}, -1), "")
	assertEqual(t, "\x83\x6C\x00\x00\x00\x01\x61\xFF\x6A", encode(t, OtpErlangList{Value: []interface{}{255}}, -1), "")
	assertEqual(t, "\x83\x6C\x00\x00\x00\x01\x62\x00\x00\x01\x00\x6A", encode(t, OtpErlangList{Value: []interface{}{256}}, -1), "")
	i1 := 2147483647
	assertEqual(t, "\x83\x6C\x00\x00\x00\x01\x62\x7F\xFF\xFF\xFF\x6A", encode(t, OtpErlangList{Value: []interface{}{i1}}, -1), "")
	assertEqual(t, "\x83\x6C\x00\x00\x00\x01\x6E\x04\x00\x00\x00\x00\x80\x6A", encode(t, OtpErlangList{Value: []interface{}{i1 + 1}}, -1), "")
	assertEqual(t, "\x83\x6C\x00\x00\x00\x01\x61\x00\x6A", encode(t, OtpErlangList{Value: []interface{}{0}}, -1), "")
	assertEqual(t, "\x83\x6C\x00\x00\x00\x01\x62\xFF\xFF\xFF\xFF\x6A", encode(t, OtpErlangList{Value: []interface{}{-1}}, -1), "")
	assertEqual(t, "\x83\x6C\x00\x00\x00\x01\x62\xFF\xFF\xFF\x00\x6A", encode(t, OtpErlangList{Value: []interface{}{-256}}, -1), "")
	assertEqual(t, "\x83\x6C\x00\x00\x00\x01\x62\xFF\xFF\xFE\xFF\x6A", encode(t, OtpErlangList{Value: []interface{}{-257}}, -1), "")
	i2 := -2147483648
	assertEqual(t, "\x83\x6C\x00\x00\x00\x01\x62\x80\x00\x00\x00\x6A", encode(t, OtpErlangList{Value: []interface{}{i2}}, -1), "")
	assertEqual(t, "\x83\x6C\x00\x00\x00\x01\x6E\x04\x01\x01\x00\x00\x80\x6A", encode(t, OtpErlangList{Value: []interface{}{i2 - 1}}, -1), "")
	assertEqual(t, "\x83\x6C\x00\x00\x00\x01\x6B\x00\x04\x74\x65\x73\x74\x6A", encode(t, OtpErlangList{Value: []interface{}{"test"}}, -1), "")
	assertEqual(t, "\x83\x6C\x00\x00\x00\x02\x62\x00\x00\x01\x75\x62\x00\x00\x01\xC7\x6A", encode(t, OtpErlangList{Value: []interface{}{373, 455}}, -1), "")
	list1 := OtpErlangList{}
	list2 := OtpErlangList{Value: []interface{}{list1}}
	assertEqual(t, "\x83\x6C\x00\x00\x00\x01\x6A\x6A", encode(t, list2, -1), "")
	list3 := OtpErlangList{Value: []interface{}{list1, list1}}
	assertEqual(t, "\x83\x6C\x00\x00\x00\x02\x6A\x6A\x6A", encode(t, list3, -1), "")
	list4 := OtpErlangList{Value: []interface{}{OtpErlangList{Value: []interface{}{"this", "is"}}, OtpErlangList{Value: []interface{}{OtpErlangList{Value: []interface{}{"a"}}}}, "test"}}
	assertEqual(t, "\x83\x6C\x00\x00\x00\x03\x6C\x00\x00\x00\x02\x6B\x00\x04\x74\x68\x69\x73\x6B\x00\x02\x69\x73\x6A\x6C\x00\x00\x00\x01\x6C\x00\x00\x00\x01\x6B\x00\x01\x61\x6A\x6A\x6B\x00\x04\x74\x65\x73\x74\x6A", encode(t, list4, -1), "")
}
func TestEncodeTermToBinaryList(t *testing.T) {
	list1 := OtpErlangList{}
	list2 := OtpErlangList{Value: []interface{}{list1}}
	assertEqual(t, "\x83l\x00\x00\x00\x01jj", encode(t, list2, -1), "")
	list3 := OtpErlangList{Value: []interface{}{list1, list1, list1, list1, list1}}
	assertEqual(t, "\x83l\x00\x00\x00\x05jjjjjj", encode(t, list3, -1), "")
}
func TestEncodeTermToBinaryImproperList(t *testing.T) {
	list1 := OtpErlangList{Value: []interface{}{OtpErlangTuple{}, []interface{}{}}, Improper: true}
	assertEqual(t, "\x83l\x00\x00\x00\x01h\x00h\x00", encode(t, list1, -1), "")
	list2 := OtpErlangList{Value: []interface{}{0, 1}, Improper: true}
	assertEqual(t, "\x83l\x00\x00\x00\x01a\x00a\x01", encode(t, list2, -1), "")
}
func TestEncodeTermToBinaryUnicode(t *testing.T) {
	assertEqual(t, "\x83j", encode(t, "", -1), "")
	assertEqual(t, "\x83k\x00\x04test", encode(t, "test", -1), "")
	assertEqual(t, "\x83k\x00\x03\x00\xc3\xbf", encode(t, "\x00\xc3\xbf", -1), "")
	assertEqual(t, "\x83k\x00\x02\xc4\x80", encode(t, "\xc4\x80", -1), "")
	assertEqual(t, "\x83k\x00\x08\xd1\x82\xd0\xb5\xd1\x81\xd1\x82", encode(t, "\xd1\x82\xd0\xb5\xd1\x81\xd1\x82", -1), "")
	// becomes a list of small integers
	assertEqual(t, "\x83l\x00\x02\x00\x00"+strings.Repeat("a\xd0a\x90", 65536)+"j", encode(t, strings.Repeat("\xd0\x90", 65536), -1), "")
}
func TestEncodeTermToBinaryAtom(t *testing.T) {
	assertEqual(t, "\x83s\x00", encode(t, OtpErlangAtom(""), -1), "")
	assertEqual(t, "\x83s\x04test", encode(t, OtpErlangAtom("test"), -1), "")
}
func TestEncodeTermToBinaryStringBasic(t *testing.T) {
	assertEqual(t, "\x83\x6A", encode(t, "", -1), "")
	assertEqual(t, "\x83\x6B\x00\x04\x74\x65\x73\x74", encode(t, "test", -1), "")
	assertEqual(t, "\x83\x6B\x00\x09\x74\x77\x6F\x20\x77\x6F\x72\x64\x73", encode(t, "two words", -1), "")
	assertEqual(t, "\x83\x6B\x00\x16\x74\x65\x73\x74\x69\x6E\x67\x20\x6D\x75\x6C\x74\x69\x70\x6C\x65\x20\x77\x6F\x72\x64\x73", encode(t, "testing multiple words", -1), "")
	assertEqual(t, "\x83\x6B\x00\x01\x20", encode(t, " ", -1), "")
	assertEqual(t, "\x83\x6B\x00\x02\x20\x20", encode(t, "  ", -1), "")
	assertEqual(t, "\x83\x6B\x00\x01\x31", encode(t, "1", -1), "")
	assertEqual(t, "\x83\x6B\x00\x02\x33\x37", encode(t, "37", -1), "")
	assertEqual(t, "\x83\x6B\x00\x07\x6F\x6E\x65\x20\x3D\x20\x31", encode(t, "one = 1", -1), "")
	assertEqual(t, "\x83\x6B\x00\x20\x21\x40\x23\x24\x25\x5E\x26\x2A\x28\x29\x5F\x2B\x2D\x3D\x5B\x5D\x7B\x7D\x5C\x7C\x3B\x27\x3A\x22\x2C\x2E\x2F\x3C\x3E\x3F\x7E\x60", encode(t, "!@#$%^&*()_+-=[]{}\\|;':\",./<>?~`", -1), "")
	assertEqual(t, "\x83\x6B\x00\x09\x22\x08\x0C\x0A\x0D\x09\x0B\x53\x12", encode(t, "\"\b\f\n\r\t\v\123\x12", -1), "")
}
func TestEncodeTermToBinaryString(t *testing.T) {
	assertEqual(t, "\x83j", encode(t, "", -1), "")
	assertEqual(t, "\x83k\x00\x04test", encode(t, "test", -1), "")
}
func TestEncodeTermToBinaryPredefinedAtoms(t *testing.T) {
	assertEqual(t, "\x83s\x04true", encode(t, true, -1), "")
	assertEqual(t, "\x83s\x05false", encode(t, false, -1), "")
	assertEqual(t, "\x83s\x09undefined", encode(t, nil, -1), "")
}
func TestEncodeTermToBinaryShortInteger(t *testing.T) {
	assertEqual(t, "\x83a\x00", encode(t, 0, -1), "")
	assertEqual(t, "\x83a\xff", encode(t, 255, -1), "")
}
func TestEncodeTermToBinaryInteger(t *testing.T) {
	assertEqual(t, "\x83b\xff\xff\xff\xff", encode(t, -1, -1), "")
	assertEqual(t, "\x83b\x80\x00\x00\x00", encode(t, -2147483648, -1), "")
	assertEqual(t, "\x83b\x00\x00\x01\x00", encode(t, 256, -1), "")
	assertEqual(t, "\x83b\x7f\xff\xff\xff", encode(t, 2147483647, -1), "")
}
func TestEncodeTermToBinaryLongInteger(t *testing.T) {
	assertEqual(t, "\x83n\x04\x00\x00\x00\x00\x80", encode(t, 2147483648, -1), "")
	assertEqual(t, "\x83n\x04\x01\x01\x00\x00\x80", encode(t, -2147483649, -1), "")
	i1 := big.NewInt(0)
	i1.Exp(big.NewInt(2), big.NewInt(2040), nil)
	assertEqual(t, "\x83o\x00\x00\x01\x00\x00"+strings.Repeat("\x00", 255)+"\x01", encode(t, i1, -1), "")
	i2 := big.NewInt(0).Neg(i1)
	assertEqual(t, "\x83o\x00\x00\x01\x00\x01"+strings.Repeat("\x00", 255)+"\x01", encode(t, i2, -1), "")
}
func TestEncodeTermToBinaryFloat(t *testing.T) {
	assertEqual(t, "\x83F\x00\x00\x00\x00\x00\x00\x00\x00", encode(t, 0.0, -1), "")
	assertEqual(t, "\x83F?\xe0\x00\x00\x00\x00\x00\x00", encode(t, 0.5, -1), "")
	assertEqual(t, "\x83F\xbf\xe0\x00\x00\x00\x00\x00\x00", encode(t, -0.5, -1), "")
	assertEqual(t, "\x83F@\t!\xfbM\x12\xd8J", encode(t, 3.1415926, -1), "")
	assertEqual(t, "\x83F\xc0\t!\xfbM\x12\xd8J", encode(t, -3.1415926, -1), "")
}
func TestEncodeTermToBinaryCompressedTerm(t *testing.T) {
	list1 := OtpErlangList{}
	list2 := OtpErlangList{Value: []interface{}{list1, list1, list1, list1, list1, list1, list1, list1, list1, list1, list1, list1, list1, list1, list1}}
	assertEqual(t, "\x83P\x00\x00\x00\x15x\x9c\xcaa``\xe0\xcfB\x03\x80\x00\x00\x00\xff\xffB@\a\x1c", encode(t, list2, 6), "")
	assertEqual(t, "\x83P\x00\x00\x00\x15x\xda\xcaa``\xe0\xcfB\x03\x80\x00\x00\x00\xff\xffB@\a\x1c", encode(t, list2, 9), "")
	assertEqual(t, "\x83P\x00\x00\x00\x15x\x01\x00\x15\x00\xea\xffl\x00\x00\x00\x0fjjjjjjjjjjjjjjjj\x01\x00\x00\xff\xffB@\a\x1c", encode(t, list2, 0), "")
	assertEqual(t, "\x83P\x00\x00\x00\x17x\xda\xcaf\x10I\xc1\x02\x00\x01\x00\x00\xff\xff]`\bP", encode(t, strings.Repeat("d", 20), 9), "")
}
