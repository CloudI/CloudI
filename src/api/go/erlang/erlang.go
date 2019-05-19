package erlang

//-*-Mode:Go;coding:utf-8;tab-width:4;c-basic-offset:4-*-
// ex: set ft=go fenc=utf-8 sts=4 ts=4 sw=4 noet nomod:
//
// MIT License
//
// Copyright (c) 2017-2019 Michael Truog <mjtruog at protonmail dot com>
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
	"bytes"
	"compress/zlib"
	"encoding/binary"
	"io"
	"math"
	"math/big"
	"reflect"
	"strconv"
)

const (
	// tag values here http://www.erlang.org/doc/apps/erts/erl_ext_dist.html
	tagVersion           = 131
	tagCompressedZlib    = 80
	tagNewFloatExt       = 70
	tagBitBinaryExt      = 77
	tagAtomCacheRef      = 78
	tagNewPidExt         = 88
	tagNewPortExt        = 89
	tagNewerReferenceExt = 90
	tagSmallIntegerExt   = 97
	tagIntegerExt        = 98
	tagFloatExt          = 99
	tagAtomExt           = 100
	tagReferenceExt      = 101
	tagPortExt           = 102
	tagPidExt            = 103
	tagSmallTupleExt     = 104
	tagLargeTupleExt     = 105
	tagNilExt            = 106
	tagStringExt         = 107
	tagListExt           = 108
	tagBinaryExt         = 109
	tagSmallBigExt       = 110
	tagLargeBigExt       = 111
	tagNewFunExt         = 112
	tagExportExt         = 113
	tagNewReferenceExt   = 114
	tagSmallAtomExt      = 115
	tagMapExt            = 116
	tagFunExt            = 117
	tagAtomUtf8Ext       = 118
	tagSmallAtomUtf8Ext  = 119

	bufferSize = 65536
)

// Erlang term structs listed alphabetically

// OtpErlangAtom represents SMALL_ATOM_EXT or ATOM_EXT
type OtpErlangAtom string

// OtpErlangAtomCacheRef represents ATOM_CACHE_REF
type OtpErlangAtomCacheRef uint8

// OtpErlangAtomUTF8 represents SMALL_ATOM_UTF8_EXT or ATOM_UTF8_EXT
type OtpErlangAtomUTF8 string

// OtpErlangBinary represents BIT_BINARY_EXT or BINARY_EXT
type OtpErlangBinary struct {
	Value []byte
	Bits  uint8
}

// OtpErlangFunction represents EXPORT_EXT, FUN_EXT or NEW_FUN_EXT
type OtpErlangFunction struct {
	Tag   uint8
	Value []byte
}

// OtpErlangList represents NIL_EXT or LIST_EXT
type OtpErlangList struct {
	Value    []interface{}
	Improper bool
}

// OtpErlangMap represents MAP_EXT
type OtpErlangMap map[interface{}]interface{}

// OtpErlangPid represents NEW_PID_EXT or PID_EXT
type OtpErlangPid struct {
	NodeTag  uint8
	Node     []byte
	ID       []byte
	Serial   []byte
	Creation []byte
}

// OtpErlangPort represents NEW_PORT_EXT or PORT_EXT
type OtpErlangPort struct {
	NodeTag  uint8
	Node     []byte
	ID       []byte
	Creation []byte
}

// OtpErlangReference represents
// NEWER_REFERENCE_EXT, REFERENCE_EXT or NEW_REFERENCE_EXT
type OtpErlangReference struct {
	NodeTag  uint8
	Node     []byte
	ID       []byte
	Creation []byte
}

// OtpErlangTuple represents SMALL_TUPLE_EXT or LARGE_TUPLE_EXT
type OtpErlangTuple []interface{}

// Error structs listed alphabetically

// InputError describes problems with function input parameters
type InputError struct {
	message string
}

func inputErrorNew(message string) error {
	return &InputError{message}
}
func (e *InputError) Error() string {
	return e.message
}

// OutputError describes problems with creating function output data
type OutputError struct {
	message string
}

func outputErrorNew(message string) error {
	return &OutputError{message}
}
func (e *OutputError) Error() string {
	return e.message
}

// ParseError provides specific parsing failure information
type ParseError struct {
	message string
}

func parseErrorNew(message string) error {
	return &ParseError{message}
}
func (e *ParseError) Error() string {
	return e.message
}

// core functionality

// BinaryToTerm decodes the Erlang Binary Term Format into Go types
func BinaryToTerm(data []byte) (interface{}, error) {
	size := len(data)
	if size <= 1 {
		return nil, parseErrorNew("null input")
	}
	reader := bytes.NewReader(data)
	version, err := reader.ReadByte()
	if err != nil {
		return nil, err
	}
	if version != tagVersion {
		return nil, parseErrorNew("invalid version")
	}
	var i int
	var term interface{}
	i, term, err = binaryToTerms(1, reader)
	if err != nil {
		return nil, err
	}
	if i != size {
		return nil, parseErrorNew("unparsed data")
	}
	return term, nil
}

// TermToBinary encodes Go types into the Erlang Binary Term Format
func TermToBinary(term interface{}, compressed int) ([]byte, error) {
	if compressed < -1 || compressed > 9 {
		return nil, inputErrorNew("compressed in [-1..9]")
	}
	var dataBuffer = new(bytes.Buffer)
	dataBuffer.Grow(bufferSize)
	dataUncompressed, err := termsToBinary(term, new(bytes.Buffer))
	if err != nil {
		return nil, err
	}
	if compressed == -1 {
		return append([]byte{tagVersion}, dataUncompressed.Bytes()...), nil
	}
	var length = dataUncompressed.Len()
	var dataCompressed *bytes.Buffer = new(bytes.Buffer)
	dataCompressed.Grow(length)
	var compress *zlib.Writer
	compress, err = zlib.NewWriterLevel(dataCompressed, compressed)
	if err != nil {
		return nil, err
	}
	_, err = compress.Write(dataUncompressed.Bytes())
	if err != nil {
		return nil, err
	}
	err = compress.Close()
	if err != nil {
		return nil, err
	}
	var result *bytes.Buffer = new(bytes.Buffer)
	_, err = result.Write([]byte{tagVersion, tagCompressedZlib})
	if err != nil {
		return nil, err
	}
	err = binary.Write(result, binary.BigEndian, uint32(length))
	if err != nil {
		return nil, err
	}
	_, err = result.Write(dataCompressed.Bytes())
	if err != nil {
		return nil, err
	}
	return result.Bytes(), nil
}

// BinaryToTerm implementation functions

func binaryToTerms(i int, reader *bytes.Reader) (int, interface{}, error) {
	tag, err := reader.ReadByte()
	if err != nil {
		return i, nil, err
	}
	i += 1
	switch tag {
	case tagNewFloatExt:
		var value float64
		err = binary.Read(reader, binary.BigEndian, &value)
		return i + 8, value, err
	case tagBitBinaryExt:
		var j uint32
		err = binary.Read(reader, binary.BigEndian, &j)
		if err != nil {
			return i, nil, err
		}
		i += 4
		var bits uint8
		bits, err = reader.ReadByte()
		if err != nil {
			return i, nil, err
		}
		i += 1
		value := make([]byte, j)
		if j > 0 {
			_, err = reader.Read(value)
			if err != nil {
				return i, nil, err
			}
		}
		return i + int(j), OtpErlangBinary{Value: value, Bits: bits}, nil
	case tagAtomCacheRef:
		var value uint8
		value, err = reader.ReadByte()
		if err != nil {
			return i, nil, err
		}
		return i + 1, OtpErlangAtomCacheRef(value), nil
	case tagSmallIntegerExt:
		var value uint8
		value, err = reader.ReadByte()
		if err != nil {
			return i, nil, err
		}
		return i + 1, uint8(value), nil
	case tagIntegerExt:
		var value int32
		err = binary.Read(reader, binary.BigEndian, &value)
		if err != nil {
			return i, nil, err
		}
		return i + 4, value, nil
	case tagFloatExt:
		valueRaw := make([]byte, 31)
		_, err = reader.Read(valueRaw)
		if err != nil {
			return i, nil, err
		}
		var value float64
		value, err = strconv.ParseFloat(string(bytes.TrimRight(valueRaw, "\x00")), 64)
		if err != nil {
			return i, nil, err
		}
		return i + 31, value, nil
	case tagAtomExt:
		var j uint16
		err = binary.Read(reader, binary.BigEndian, &j)
		if err != nil {
			return i, nil, err
		}
		i += 2
		value := make([]byte, j)
		if j > 0 {
			_, err = reader.Read(value)
			if err != nil {
				return i, nil, err
			}
		}
		return i + int(j), OtpErlangAtom(value), nil
	case tagNewPortExt:
		fallthrough
	case tagReferenceExt:
		fallthrough
	case tagPortExt:
		var nodeTag uint8
		var node []byte
		i, nodeTag, node, err = binaryToAtom(i, reader)
		if err != nil {
			return i, nil, err
		}
		id := make([]byte, 4)
		_, err = reader.Read(id)
		if err != nil {
			return i, nil, err
		}
		i += 4
		var creation []byte
		switch tag {
		case tagNewPortExt:
			creation = make([]byte, 4)
			_, err = reader.Read(creation)
			if err != nil {
				return i, nil, err
			}
			i += 4
		default:
			creation = make([]byte, 1)
			_, err = reader.Read(creation)
			if err != nil {
				return i, nil, err
			}
			i += 1
			if tag == tagReferenceExt {
				return i, OtpErlangReference{NodeTag: nodeTag, Node: node, ID: id, Creation: creation}, nil
			}
		}
		// tag == tagNewPortExt || tag == tagPortExt
		return i, OtpErlangPort{NodeTag: nodeTag, Node: node, ID: id, Creation: creation}, nil
	case tagNewPidExt:
		fallthrough
	case tagPidExt:
		var nodeTag uint8
		var node []byte
		i, nodeTag, node, err = binaryToAtom(i, reader)
		if err != nil {
			return i, nil, err
		}
		id := make([]byte, 4)
		_, err = reader.Read(id)
		if err != nil {
			return i, nil, err
		}
		i += 4
		serial := make([]byte, 4)
		_, err = reader.Read(serial)
		if err != nil {
			return i, nil, err
		}
		i += 4
		var creation []byte
		switch tag {
		case tagNewPidExt:
			creation = make([]byte, 4)
			_, err = reader.Read(creation)
			if err != nil {
				return i, nil, err
			}
			i += 4
		case tagPidExt:
			creation = make([]byte, 1)
			_, err = reader.Read(creation)
			if err != nil {
				return i, nil, err
			}
			i += 1
		}
		return i, OtpErlangPid{NodeTag: nodeTag, Node: node, ID: id, Serial: serial, Creation: creation}, nil
	case tagSmallTupleExt:
		fallthrough
	case tagLargeTupleExt:
		var length int
		switch tag {
		case tagSmallTupleExt:
			var lengthValue uint8
			lengthValue, err = reader.ReadByte()
			if err != nil {
				return i, nil, err
			}
			i += 1
			length = int(lengthValue)
		case tagLargeTupleExt:
			var lengthValue uint32
			err = binary.Read(reader, binary.BigEndian, &lengthValue)
			if err != nil {
				return i, nil, err
			}
			i += 4
			length = int(lengthValue)
		default:
			return i, nil, parseErrorNew("invalid tag case")
		}
		var tmp []interface{}
		i, tmp, err = binaryToTermSequence(i, length, reader)
		if err != nil {
			return i, nil, err
		}
		return i, OtpErlangTuple(tmp), nil
	case tagNilExt:
		value := make([]interface{}, 0)
		return i, OtpErlangList{Value: value, Improper: false}, nil
	case tagStringExt:
		var j uint16
		err = binary.Read(reader, binary.BigEndian, &j)
		if err != nil {
			return i, nil, err
		}
		i += 2
		value := make([]byte, j)
		if j > 0 {
			_, err = reader.Read(value)
			if err != nil {
				return i, nil, err
			}
		}
		return i + int(j), string(value), nil
	case tagListExt:
		var length uint32
		err = binary.Read(reader, binary.BigEndian, &length)
		if err != nil {
			return i, nil, err
		}
		i += 4
		var tmp []interface{}
		i, tmp, err = binaryToTermSequence(i, int(length), reader)
		if err != nil {
			return i, nil, err
		}
		var tail interface{}
		i, tail, err = binaryToTerms(i, reader)
		if err != nil {
			return i, nil, err
		}
		var improper bool
		switch tail.(type) {
		case OtpErlangList:
			improper = (len(tail.(OtpErlangList).Value) != 0)
		default:
			improper = true
		}
		if improper {
			tmp = append(tmp, tail)
		}
		return i, OtpErlangList{Value: tmp, Improper: improper}, nil
	case tagBinaryExt:
		var j uint32
		err = binary.Read(reader, binary.BigEndian, &j)
		if err != nil {
			return i, nil, err
		}
		i += 4
		value := make([]byte, j)
		if j > 0 {
			_, err = reader.Read(value)
			if err != nil {
				return i, nil, err
			}
		}
		return i + int(j), OtpErlangBinary{Value: value, Bits: 8}, nil
	case tagSmallBigExt:
		fallthrough
	case tagLargeBigExt:
		var j int
		switch tag {
		case tagSmallBigExt:
			var jValue uint8
			jValue, err = reader.ReadByte()
			if err != nil {
				return i, nil, err
			}
			i += 1
			j = int(jValue)
		case tagLargeBigExt:
			var jValue uint32
			err = binary.Read(reader, binary.BigEndian, &jValue)
			if err != nil {
				return i, nil, err
			}
			i += 4
			j = int(jValue)
		default:
			return i, nil, parseErrorNew("invalid tag case")
		}
		var sign uint8
		sign, err = reader.ReadByte()
		if err != nil {
			return i, nil, err
		}
		bignum := big.NewInt(0)
		digit := make([]byte, 1)
		for bignumIndex := 0; bignumIndex < j; bignumIndex++ {
			_, err = reader.ReadAt(digit, int64(i+j-bignumIndex))
			if err != nil {
				return i, nil, err
			}
			bignum.Lsh(bignum, 8)
			bignum.Add(bignum, big.NewInt(int64(digit[0])))
		}
		if sign == 1 {
			bignum.Neg(bignum)
		}
		i += 1
		_, err = reader.Seek(int64(j), 1)
		if err != nil {
			return i, nil, err
		}
		return i + j, bignum, nil
	case tagNewFunExt:
		var length uint32
		err = binary.Read(reader, binary.BigEndian, &length)
		if err != nil {
			return i, nil, err
		}
		i += 4
		value := make([]byte, length)
		if length > 0 {
			_, err = reader.Read(value)
			if err != nil {
				return i, nil, err
			}
		}
		return i + int(length), OtpErlangFunction{Tag: tag, Value: value}, nil
	case tagExportExt:
		iOld := i
		i, _, _, err = binaryToAtom(i, reader) // module
		if err != nil {
			return i, nil, err
		}
		i, _, _, err = binaryToAtom(i, reader) // function
		if err != nil {
			return i, nil, err
		}
		var arityTag uint8
		arityTag, err = reader.ReadByte()
		if err != nil {
			return i, nil, err
		}
		if arityTag != tagSmallIntegerExt {
			return i, nil, parseErrorNew("invalid small integer tag")
		}
		i += 1
		_, err = reader.ReadByte() // arity
		if err != nil {
			return i, nil, err
		}
		i += 1
		value := make([]byte, i-iOld)
		_, err = reader.ReadAt(value, int64(iOld))
		if err != nil {
			return i, nil, err
		}
		return i, OtpErlangFunction{Tag: tag, Value: value}, nil
	case tagNewerReferenceExt:
		fallthrough
	case tagNewReferenceExt:
		var j uint16
		err = binary.Read(reader, binary.BigEndian, &j)
		if err != nil {
			return i, nil, err
		}
		j *= 4
		i += 2
		var nodeTag uint8
		var node []byte
		i, nodeTag, node, err = binaryToAtom(i, reader)
		if err != nil {
			return i, nil, err
		}
		var creation []byte
		switch tag {
		case tagNewerReferenceExt:
			creation = make([]byte, 4)
			_, err = reader.Read(creation)
			if err != nil {
				return i, nil, err
			}
			i += 4
		case tagNewReferenceExt:
			creation = make([]byte, 1)
			_, err = reader.Read(creation)
			if err != nil {
				return i, nil, err
			}
			i += 1
		}
		id := make([]byte, j)
		if j > 0 {
			_, err = reader.Read(id)
			if err != nil {
				return i, nil, err
			}
		}
		return i + int(j), OtpErlangReference{NodeTag: nodeTag, Node: node, ID: id, Creation: creation}, nil
	case tagSmallAtomExt:
		var j uint8
		j, err = reader.ReadByte()
		if err != nil {
			return i, nil, err
		}
		i += 1
		value := make([]byte, j)
		if j > 0 {
			_, err = reader.Read(value)
			if err != nil {
				return i, nil, err
			}
		}
		return i + int(j), OtpErlangAtom(value), nil
	case tagMapExt:
		var length uint32
		err = binary.Read(reader, binary.BigEndian, &length)
		if err != nil {
			return i, nil, err
		}
		i += 4
		var pairs map[interface{}]interface{}
		for lengthIndex := 0; lengthIndex < int(length); lengthIndex++ {
			var key interface{}
			i, key, err = binaryToTerms(i, reader)
			if err != nil {
				return i, nil, err
			}
			if !reflect.TypeOf(key).Comparable() {
				// no way to solve this properly in Go while preserving
				// the Erlang type information
				return i, nil, parseErrorNew("map key not comparable")
			}
			var value interface{}
			i, value, err = binaryToTerms(i, reader)
			if err != nil {
				return i, nil, err
			}
			pairs[key] = value
		}
		return i, OtpErlangMap(pairs), nil
	case tagFunExt:
		iOld := i
		var numfree uint32
		err = binary.Read(reader, binary.BigEndian, &numfree)
		if err != nil {
			return i, nil, err
		}
		i += 4
		i, _, err = binaryToPid(i, reader) // pid
		if err != nil {
			return i, nil, err
		}
		i, _, _, err = binaryToAtom(i, reader) // module
		if err != nil {
			return i, nil, err
		}
		i, _, err = binaryToInteger(i, reader) // index
		if err != nil {
			return i, nil, err
		}
		i, _, err = binaryToInteger(i, reader) // uniq
		if err != nil {
			return i, nil, err
		}
		i, _, err = binaryToTermSequence(i, int(numfree), reader) // free
		if err != nil {
			return i, nil, err
		}
		value := make([]byte, i-iOld)
		_, err = reader.ReadAt(value, int64(iOld))
		if err != nil {
			return i, nil, err
		}
		return i, OtpErlangFunction{Tag: tag, Value: value}, nil
	case tagAtomUtf8Ext:
		var j uint16
		err = binary.Read(reader, binary.BigEndian, &j)
		if err != nil {
			return i, nil, err
		}
		i += 2
		value := make([]byte, j)
		if j > 0 {
			_, err = reader.Read(value)
			if err != nil {
				return i, nil, err
			}
		}
		return i + int(j), OtpErlangAtomUTF8(value), nil
	case tagSmallAtomUtf8Ext:
		var j uint8
		j, err = reader.ReadByte()
		if err != nil {
			return i, nil, err
		}
		i += 1
		value := make([]byte, j)
		if j > 0 {
			_, err = reader.Read(value)
			if err != nil {
				return i, nil, err
			}
		}
		return i + int(j), OtpErlangAtomUTF8(value), nil
	case tagCompressedZlib:
		var sizeUncompressed uint32
		err = binary.Read(reader, binary.BigEndian, &sizeUncompressed)
		if err != nil {
			return i, nil, err
		}
		i += 4
		if sizeUncompressed == 0 {
			return i, nil, parseErrorNew("compressed data null")
		}
		j := reader.Len()
		var compress io.ReadCloser
		compress, err = zlib.NewReader(reader)
		if err != nil {
			return i, nil, err
		}
		dataUncompressed := make([]byte, sizeUncompressed)
		var sizeUncompressedStored int
		sizeUncompressedStored, _ = compress.Read(dataUncompressed)
		err = compress.Close()
		if err != nil {
			return i, nil, err
		}
		if int(sizeUncompressed) != sizeUncompressedStored {
			return i, nil, parseErrorNew("compression corrupt")
		}
		var iNew int
		var term interface{}
		iNew, term, err = binaryToTerms(0, bytes.NewReader(dataUncompressed))
		if err != nil {
			return i, nil, err
		}
		if iNew != int(sizeUncompressed) {
			return i, nil, parseErrorNew("unparsed data")
		}
		return i + j, term, nil
	default:
		return i, nil, parseErrorNew("invalid tag")
	}
}

func binaryToTermSequence(i, length int, reader *bytes.Reader) (int, []interface{}, error) {
	sequence := make([]interface{}, length)
	var err error
	for lengthIndex := 0; lengthIndex < length; lengthIndex++ {
		var element interface{}
		i, element, err = binaryToTerms(i, reader)
		if err != nil {
			return i, nil, err
		}
		sequence[lengthIndex] = element
	}
	return i, sequence, nil
}

// (BinaryToTerm Erlang term primitive type functions)

func binaryToInteger(i int, reader *bytes.Reader) (int, interface{}, error) {
	tag, err := reader.ReadByte()
	if err != nil {
		return i, nil, err
	}
	i += 1
	switch tag {
	case tagSmallIntegerExt:
		var value uint8
		value, err = reader.ReadByte()
		if err != nil {
			return i, nil, err
		}
		return i + 1, uint8(value), nil
	case tagIntegerExt:
		var value int32
		err = binary.Read(reader, binary.BigEndian, &value)
		if err != nil {
			return i, nil, err
		}
		return i + 4, value, nil
	default:
		return i, nil, parseErrorNew("invalid integer tag")
	}
}

func binaryToPid(i int, reader *bytes.Reader) (int, interface{}, error) {
	tag, err := reader.ReadByte()
	if err != nil {
		return i, nil, err
	}
	i += 1
	switch tag {
	case tagNewPidExt:
		var nodeTag uint8
		var node []byte
		i, nodeTag, node, err = binaryToAtom(i, reader)
		if err != nil {
			return i, nil, err
		}
		id := make([]byte, 4)
		_, err = reader.Read(id)
		if err != nil {
			return i, nil, err
		}
		i += 4
		serial := make([]byte, 4)
		_, err = reader.Read(serial)
		if err != nil {
			return i, nil, err
		}
		i += 4
		creation := make([]byte, 4)
		_, err = reader.Read(creation)
		if err != nil {
			return i, nil, err
		}
		i += 4
		return i, OtpErlangPid{NodeTag: nodeTag, Node: node, ID: id, Serial: serial, Creation: creation}, nil
	case tagPidExt:
		var nodeTag uint8
		var node []byte
		i, nodeTag, node, err = binaryToAtom(i, reader)
		if err != nil {
			return i, nil, err
		}
		id := make([]byte, 4)
		_, err = reader.Read(id)
		if err != nil {
			return i, nil, err
		}
		i += 4
		serial := make([]byte, 4)
		_, err = reader.Read(serial)
		if err != nil {
			return i, nil, err
		}
		i += 4
		creation := make([]byte, 1)
		_, err = reader.Read(creation)
		if err != nil {
			return i, nil, err
		}
		i += 1
		return i, OtpErlangPid{NodeTag: nodeTag, Node: node, ID: id, Serial: serial, Creation: creation}, nil
	default:
		return i, nil, parseErrorNew("invalid pid tag")
	}
}

func binaryToAtom(i int, reader *bytes.Reader) (int, uint8, []byte, error) {
	tag, err := reader.ReadByte()
	if err != nil {
		return i, 0, nil, err
	}
	i += 1
	switch tag {
	case tagAtomExt:
		fallthrough
	case tagAtomUtf8Ext:
		var j uint16
		err = binary.Read(reader, binary.BigEndian, &j)
		if err != nil {
			return i, tag, nil, err
		}
		value := make([]byte, 2+j)
		_, err = reader.ReadAt(value, int64(i))
		if err != nil {
			return i, tag, nil, err
		}
		i += 2
		_, err = reader.Seek(int64(j), 1)
		if err != nil {
			return i, tag, nil, err
		}
		return i + int(j), tag, value, nil
	case tagSmallAtomExt:
		fallthrough
	case tagSmallAtomUtf8Ext:
		var j uint8
		j, err = reader.ReadByte()
		if err != nil {
			return i, tag, nil, err
		}
		value := make([]byte, 1+j)
		_, err = reader.ReadAt(value, int64(i))
		if err != nil {
			return i, tag, nil, err
		}
		i += 1
		_, err = reader.Seek(int64(j), 1)
		if err != nil {
			return i, tag, nil, err
		}
		return i + int(j), tag, value, nil
	case tagAtomCacheRef:
		var value uint8
		value, err = reader.ReadByte()
		if err != nil {
			return i, tag, nil, err
		}
		return i + 1, tag, []byte{value}, nil
	default:
		return i, tag, nil, parseErrorNew("invalid atom tag")
	}
}

// TermToBinary implementation functions

func termsToBinary(termI interface{}, buffer *bytes.Buffer) (*bytes.Buffer, error) {
	switch term := termI.(type) {
	case uint8:
		_, err := buffer.Write([]byte{tagSmallIntegerExt, term})
		return buffer, err
	case uint16:
		return integerToBinary(int32(term), buffer)
	case uint32:
		return bignumToBinary(big.NewInt(int64(term)), buffer)
	case uint64:
		var value *big.Int = new(big.Int)
		value.SetUint64(term)
		return bignumToBinary(value, buffer)
	case int8:
		return integerToBinary(int32(term), buffer)
	case int16:
		return integerToBinary(int32(term), buffer)
	case int32:
		return integerToBinary(term, buffer)
	case int64:
		return bignumToBinary(big.NewInt(term), buffer)
	case int:
		switch {
		case term >= 0 && term <= math.MaxUint8:
			return termsToBinary(uint8(term), buffer)
		case term >= math.MinInt32 && term <= math.MaxInt32:
			return integerToBinary(int32(term), buffer)
		default:
			return termsToBinary(int64(term), buffer)
		}
	case *big.Int:
		return bignumToBinary(term, buffer)
	case float32:
		return floatToBinary(float64(term), buffer)
	case float64:
		return floatToBinary(term, buffer)
	case bool:
		if term {
			return atomToBinary("true", buffer)
		}
		return atomToBinary("false", buffer)
	case nil:
		return atomToBinary("undefined", buffer)
	case OtpErlangAtom:
		return atomToBinary(string(term), buffer)
	case OtpErlangAtomUTF8:
		return atomUtf8ToBinary(string(term), buffer)
	case OtpErlangAtomCacheRef:
		_, err := buffer.Write([]byte{tagAtomCacheRef, uint8(term)})
		return buffer, err
	case []byte:
		return binaryObjectToBinary(OtpErlangBinary{Value: term, Bits: 8}, buffer)
	case OtpErlangBinary:
		return binaryObjectToBinary(term, buffer)
	case OtpErlangFunction:
		return functionToBinary(term, buffer)
	case OtpErlangPid:
		return pidToBinary(term, buffer)
	case OtpErlangPort:
		return portToBinary(term, buffer)
	case OtpErlangReference:
		return referenceToBinary(term, buffer)
	case string:
		return stringToBinary(term, buffer)
	case OtpErlangTuple:
		return tupleToBinary(term, buffer)
	case []interface{}:
		return tupleToBinary(term, buffer)
	case OtpErlangMap:
		return mapToBinary(term, buffer)
	case map[interface{}]interface{}:
		return mapToBinary(term, buffer)
	case OtpErlangList:
		return listToBinary(term, buffer)
	default:
		return buffer, outputErrorNew("unknown go type")
	}
}

// (TermToBinary Erlang term composite type functions)

func stringToBinary(term string, buffer *bytes.Buffer) (*bytes.Buffer, error) {
	switch length := len(term); {
	case length == 0:
		err := buffer.WriteByte(tagNilExt)
		return buffer, err
	case length <= math.MaxUint16:
		err := buffer.WriteByte(tagStringExt)
		if err != nil {
			return buffer, err
		}
		err = binary.Write(buffer, binary.BigEndian, uint16(length))
		if err != nil {
			return buffer, err
		}
		_, err = buffer.WriteString(term)
		return buffer, err
	case uint64(length) <= math.MaxUint32:
		err := buffer.WriteByte(tagListExt)
		if err != nil {
			return buffer, err
		}
		err = binary.Write(buffer, binary.BigEndian, uint32(length))
		if err != nil {
			return buffer, err
		}
		for i := 0; i < length; i++ {
			_, err = buffer.Write([]byte{tagSmallIntegerExt, term[i]})
			if err != nil {
				return buffer, err
			}
		}
		err = buffer.WriteByte(tagNilExt)
		return buffer, err
	default:
		return buffer, outputErrorNew("uint32 overflow")
	}
}

func tupleToBinary(term []interface{}, buffer *bytes.Buffer) (*bytes.Buffer, error) {
	var length int
	var err error
	switch length = len(term); {
	case length <= math.MaxUint8:
		_, err = buffer.Write([]byte{tagSmallTupleExt, byte(length)})
		if err != nil {
			return buffer, err
		}
	case uint64(length) <= math.MaxUint32:
		err = buffer.WriteByte(tagLargeTupleExt)
		if err != nil {
			return buffer, err
		}
		err = binary.Write(buffer, binary.BigEndian, uint32(length))
		if err != nil {
			return buffer, err
		}
	default:
		return buffer, outputErrorNew("uint32 overflow")
	}
	for i := 0; i < length; i++ {
		buffer, err = termsToBinary(term[i], buffer)
		if err != nil {
			return buffer, err
		}
	}
	return buffer, nil
}

func mapToBinary(term map[interface{}]interface{}, buffer *bytes.Buffer) (*bytes.Buffer, error) {
	var length int
	var err error
	switch length = len(term); {
	case uint64(length) <= math.MaxUint32:
		err = buffer.WriteByte(tagMapExt)
		if err != nil {
			return buffer, err
		}
		err = binary.Write(buffer, binary.BigEndian, uint32(length))
		if err != nil {
			return buffer, err
		}
	default:
		return buffer, outputErrorNew("uint32 overflow")
	}
	for key, value := range term {
		buffer, err = termsToBinary(key, buffer)
		if err != nil {
			return buffer, err
		}
		buffer, err = termsToBinary(value, buffer)
		if err != nil {
			return buffer, err
		}
	}
	return buffer, nil
}

func listToBinary(term OtpErlangList, buffer *bytes.Buffer) (*bytes.Buffer, error) {
	var length int
	var err error
	switch length = len(term.Value); {
	case length == 0:
		err = buffer.WriteByte(tagNilExt)
		return buffer, err
	case uint64(length) <= math.MaxUint32:
		err = buffer.WriteByte(tagListExt)
		if err != nil {
			return buffer, err
		}
		if term.Improper {
			err = binary.Write(buffer, binary.BigEndian, uint32(length-1))
			if err != nil {
				return buffer, err
			}
		} else {
			err = binary.Write(buffer, binary.BigEndian, uint32(length))
			if err != nil {
				return buffer, err
			}
		}
	default:
		return buffer, outputErrorNew("uint32 overflow")
	}
	for i := 0; i < length; i++ {
		buffer, err = termsToBinary(term.Value[i], buffer)
		if err != nil {
			return buffer, err
		}
	}
	if !term.Improper {
		err = buffer.WriteByte(tagNilExt)
	}
	return buffer, err
}

// (TermToBinary Erlang term primitive type functions)

func integerToBinary(term int32, buffer *bytes.Buffer) (*bytes.Buffer, error) {
	err := buffer.WriteByte(tagIntegerExt)
	if err != nil {
		return buffer, err
	}
	err = binary.Write(buffer, binary.BigEndian, term)
	return buffer, err
}

func bignumToBinary(term *big.Int, buffer *bytes.Buffer) (*bytes.Buffer, error) {
	var sign uint8
	if term.Sign() < 0 {
		sign = 1
	} else {
		sign = 0
	}
	value := term.Bytes()
	var length int
	var err error
	switch length = len(value); {
	case length <= math.MaxUint8:
		_, err = buffer.Write([]byte{tagSmallBigExt, uint8(length)})
		if err != nil {
			return buffer, err
		}
	case uint64(length) <= math.MaxUint32:
		err = buffer.WriteByte(tagLargeBigExt)
		if err != nil {
			return buffer, err
		}
		err = binary.Write(buffer, binary.BigEndian, uint32(length))
		if err != nil {
			return buffer, err
		}
	default:
		return buffer, outputErrorNew("uint32 overflow")
	}
	err = buffer.WriteByte(sign)
	if err != nil {
		return buffer, err
	}
	// little-endian is required
	half := length >> 1
	iLast := length - 1
	for i := 0; i < half; i++ {
		j := iLast - i
		value[i], value[j] = value[j], value[i]
	}
	_, err = buffer.Write(value)
	return buffer, err
}

func floatToBinary(term float64, buffer *bytes.Buffer) (*bytes.Buffer, error) {
	err := buffer.WriteByte(tagNewFloatExt)
	if err != nil {
		return buffer, err
	}
	err = binary.Write(buffer, binary.BigEndian, term)
	return buffer, err
}

func atomToBinary(term string, buffer *bytes.Buffer) (*bytes.Buffer, error) {
	switch length := len(term); {
	case length <= math.MaxUint8:
		_, err := buffer.Write([]byte{tagSmallAtomExt, uint8(length)})
		if err != nil {
			return buffer, err
		}
		_, err = buffer.WriteString(term)
		return buffer, err
	case length <= math.MaxUint16:
		err := buffer.WriteByte(tagAtomExt)
		if err != nil {
			return buffer, err
		}
		err = binary.Write(buffer, binary.BigEndian, uint16(length))
		if err != nil {
			return buffer, err
		}
		_, err = buffer.WriteString(term)
		return buffer, err
	default:
		return buffer, outputErrorNew("uint16 overflow")
	}
}

func atomUtf8ToBinary(term string, buffer *bytes.Buffer) (*bytes.Buffer, error) {
	switch length := len(term); {
	case length <= math.MaxUint8:
		_, err := buffer.Write([]byte{tagSmallAtomUtf8Ext, uint8(length)})
		if err != nil {
			return buffer, err
		}
		_, err = buffer.WriteString(term)
		return buffer, err
	case length <= math.MaxUint16:
		err := buffer.WriteByte(tagAtomUtf8Ext)
		if err != nil {
			return buffer, err
		}
		err = binary.Write(buffer, binary.BigEndian, uint16(length))
		if err != nil {
			return buffer, err
		}
		_, err = buffer.WriteString(term)
		return buffer, err
	default:
		return buffer, outputErrorNew("uint16 overflow")
	}
}

func binaryObjectToBinary(term OtpErlangBinary, buffer *bytes.Buffer) (*bytes.Buffer, error) {
	var err error
	switch length := len(term.Value); {
	case term.Bits < 1 || term.Bits > 8:
		return buffer, outputErrorNew("invalid OtpErlangBinary.Bits")
	case uint64(length) <= math.MaxUint32:
		if term.Bits != 8 {
			err = buffer.WriteByte(tagBitBinaryExt)
			if err != nil {
				return buffer, err
			}
			err = binary.Write(buffer, binary.BigEndian, uint32(length))
			if err != nil {
				return buffer, err
			}
			err = buffer.WriteByte(term.Bits)
			if err != nil {
				return buffer, err
			}
		} else {
			err = buffer.WriteByte(tagBinaryExt)
			if err != nil {
				return buffer, err
			}
			err = binary.Write(buffer, binary.BigEndian, uint32(length))
			if err != nil {
				return buffer, err
			}
		}
	default:
		return buffer, outputErrorNew("uint32 overflow")
	}
	_, err = buffer.Write(term.Value)
	return buffer, err
}

func functionToBinary(term OtpErlangFunction, buffer *bytes.Buffer) (*bytes.Buffer, error) {
	err := buffer.WriteByte(term.Tag)
	if err != nil {
		return buffer, err
	}
	_, err = buffer.Write(term.Value)
	return buffer, err
}

func pidToBinary(term OtpErlangPid, buffer *bytes.Buffer) (*bytes.Buffer, error) {
	var err error
	switch creationSize := len(term.Creation); {
	case creationSize == 1:
		err = buffer.WriteByte(tagPidExt)
		if err != nil {
			return buffer, err
		}
	case creationSize == 4:
		err = buffer.WriteByte(tagNewPidExt)
		if err != nil {
			return buffer, err
		}
	default:
		return buffer, outputErrorNew("unknown pid type")
	}
	err = buffer.WriteByte(term.NodeTag)
	if err != nil {
		return buffer, err
	}
	_, err = buffer.Write(term.Node)
	if err != nil {
		return buffer, err
	}
	_, err = buffer.Write(term.ID)
	if err != nil {
		return buffer, err
	}
	_, err = buffer.Write(term.Serial)
	if err != nil {
		return buffer, err
	}
	_, err = buffer.Write(term.Creation)
	return buffer, err
}

func portToBinary(term OtpErlangPort, buffer *bytes.Buffer) (*bytes.Buffer, error) {
	var err error
	switch creationSize := len(term.Creation); {
	case creationSize == 1:
		err = buffer.WriteByte(tagPortExt)
		if err != nil {
			return buffer, err
		}
	case creationSize == 4:
		err = buffer.WriteByte(tagNewPortExt)
		if err != nil {
			return buffer, err
		}
	default:
		return buffer, outputErrorNew("unknown port type")
	}
	err = buffer.WriteByte(term.NodeTag)
	if err != nil {
		return buffer, err
	}
	_, err = buffer.Write(term.Node)
	if err != nil {
		return buffer, err
	}
	_, err = buffer.Write(term.ID)
	if err != nil {
		return buffer, err
	}
	_, err = buffer.Write(term.Creation)
	return buffer, err
}

func referenceToBinary(term OtpErlangReference, buffer *bytes.Buffer) (*bytes.Buffer, error) {
	switch length := len(term.ID) / 4; {
	case length == 0:
		err := buffer.WriteByte(tagReferenceExt)
		if err != nil {
			return buffer, err
		}
		err = buffer.WriteByte(term.NodeTag)
		if err != nil {
			return buffer, err
		}
		_, err = buffer.Write(term.Node)
		if err != nil {
			return buffer, err
		}
		_, err = buffer.Write(term.ID)
		if err != nil {
			return buffer, err
		}
		_, err = buffer.Write(term.Creation)
		return buffer, err
	case length <= math.MaxUint16:
		var err error
		switch creationSize := len(term.Creation); {
		case creationSize == 1:
			err = buffer.WriteByte(tagNewReferenceExt)
			if err != nil {
				return buffer, err
			}
		case creationSize == 4:
			err = buffer.WriteByte(tagNewerReferenceExt)
			if err != nil {
				return buffer, err
			}
		default:
			return buffer, outputErrorNew("unknown reference type")
		}
		err = binary.Write(buffer, binary.BigEndian, uint16(length))
		if err != nil {
			return buffer, err
		}
		err = buffer.WriteByte(term.NodeTag)
		if err != nil {
			return buffer, err
		}
		_, err = buffer.Write(term.Node)
		if err != nil {
			return buffer, err
		}
		_, err = buffer.Write(term.Creation)
		if err != nil {
			return buffer, err
		}
		_, err = buffer.Write(term.ID)
		return buffer, err
	default:
		return buffer, outputErrorNew("uint16 overflow")
	}
}
