<?php //-*-coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=php fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
//
// MIT License
//
// Copyright (c) 2014-2023 Michael Truog <mjtruog at protonmail dot com>
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

namespace Erlang;

$UNDEFINED = 'undefined'; // Change with set_undefined

// tag values here http://www.erlang.org/doc/apps/erts/erl_ext_dist.html
define(__NAMESPACE__ . '\TAG_VERSION', 131);
define(__NAMESPACE__ . '\TAG_COMPRESSED_ZLIB', 80);
define(__NAMESPACE__ . '\TAG_NEW_FLOAT_EXT', 70);
define(__NAMESPACE__ . '\TAG_BIT_BINARY_EXT', 77);
define(__NAMESPACE__ . '\TAG_ATOM_CACHE_REF', 78);
define(__NAMESPACE__ . '\TAG_NEW_PID_EXT', 88);
define(__NAMESPACE__ . '\TAG_NEW_PORT_EXT', 89);
define(__NAMESPACE__ . '\TAG_NEWER_REFERENCE_EXT', 90);
define(__NAMESPACE__ . '\TAG_SMALL_INTEGER_EXT', 97);
define(__NAMESPACE__ . '\TAG_INTEGER_EXT', 98);
define(__NAMESPACE__ . '\TAG_FLOAT_EXT', 99);
define(__NAMESPACE__ . '\TAG_ATOM_EXT', 100);
define(__NAMESPACE__ . '\TAG_REFERENCE_EXT', 101);
define(__NAMESPACE__ . '\TAG_PORT_EXT', 102);
define(__NAMESPACE__ . '\TAG_PID_EXT', 103);
define(__NAMESPACE__ . '\TAG_SMALL_TUPLE_EXT', 104);
define(__NAMESPACE__ . '\TAG_LARGE_TUPLE_EXT', 105);
define(__NAMESPACE__ . '\TAG_NIL_EXT', 106);
define(__NAMESPACE__ . '\TAG_STRING_EXT', 107);
define(__NAMESPACE__ . '\TAG_LIST_EXT', 108);
define(__NAMESPACE__ . '\TAG_BINARY_EXT', 109);
define(__NAMESPACE__ . '\TAG_SMALL_BIG_EXT', 110);
define(__NAMESPACE__ . '\TAG_LARGE_BIG_EXT', 111);
define(__NAMESPACE__ . '\TAG_NEW_FUN_EXT', 112);
define(__NAMESPACE__ . '\TAG_EXPORT_EXT', 113);
define(__NAMESPACE__ . '\TAG_NEW_REFERENCE_EXT', 114);
define(__NAMESPACE__ . '\TAG_SMALL_ATOM_EXT', 115);
define(__NAMESPACE__ . '\TAG_MAP_EXT', 116);
define(__NAMESPACE__ . '\TAG_FUN_EXT', 117);
define(__NAMESPACE__ . '\TAG_ATOM_UTF8_EXT', 118);
define(__NAMESPACE__ . '\TAG_SMALL_ATOM_UTF8_EXT', 119);
define(__NAMESPACE__ . '\TAG_V4_PORT_EXT', 120);
define(__NAMESPACE__ . '\TAG_LOCAL_EXT', 121);

// Erlang term classes listed alphabetically

class OtpErlangAtom
{
    public $value;
    public $utf8;
    public function __construct($value, $utf8 = true)
    {
        $this->value = $value;
        $this->utf8 = $utf8;
    }
    public function binary()
    {
        if (is_int($this->value))
        {
            return pack('CC', TAG_ATOM_CACHE_REF, $this->value);
        }
        elseif (is_string($this->value))
        {
            $length = strlen($this->value);
            if ($this->utf8)
            {
                if ($length <= 255)
                {
                    return pack('CC', TAG_SMALL_ATOM_UTF8_EXT, $length) .
                           $this->value;
                }
                elseif ($length <= 65535)
                {
                    return pack('Cn', TAG_ATOM_UTF8_EXT, $length) .
                           $this->value;
                }
                else
                {
                    throw new OutputException('uint16 overflow');
                }
            }
            else
            {
                // deprecated
                // (not used in Erlang/OTP 26, i.e., minor_version 2)
                if ($length <= 255)
                {
                    return pack('CC', TAG_SMALL_ATOM_EXT, $length) .
                           $this->value;
                }
                elseif ($length <= 65535)
                {
                    return pack('Cn', TAG_ATOM_EXT, $length) .
                           $this->value;
                }
                else
                {
                    throw new OutputException('uint16 overflow');
                }
            }
        }
        else
        {
            throw new OutputException('unknown atom type');
        }
    }
    public function __toString()
    {
        return sprintf('%s(%s,utf8=%s)', get_class(),
                       $this->value, $this->utf8 ? 'true' : 'false');
    }
}

class OtpErlangBinary
{
    public $value;
    public $bits;
    public function __construct($value, $bits = 8)
    {
        $this->value = $value;
        $this->bits = $bits;
    }
    public function binary()
    {
        if (is_string($this->value))
        {
            $length = strlen($this->value);
            if ($length > 4294967295)
            {
                throw new OutputException('uint32 overflow');
            }
            elseif ($this->bits != 8)
            {
                return pack('CNC', TAG_BIT_BINARY_EXT, $length,
                            $this->bits) . $this->value;
            }
            else
            {
                return pack('CN', TAG_BINARY_EXT, $length) . $this->value;
            }
        }
        else
        {
            throw new OutputException('unknown binary type');
        }
    }
    public function __toString()
    {
        return sprintf('%s(%s,bits=%d)', get_class(),
                       $this->value, $this->bits);
    }
}

class OtpErlangList
{
    public $value;
    public $improper;
    public function __construct($value, $improper = false)
    {
        $this->value = $value;
        $this->improper = $improper;
    }
    public function binary()
    {
        if (is_array($this->value))
        {
            $length = count($this->value);
            if ($length == 0)
            {
                return chr(TAG_NIL_EXT);
            }
            elseif ($length > 4294967295)
            {
                throw new OutputException('uint32 overflow');
            }
            elseif ($this->improper)
            {
                $contents = '';
                foreach ($this->value as $element)
                    $contents .= _term_to_binary($element);
                return pack('CN', TAG_LIST_EXT, $length - 1) . $contents;
            }
            else
            {
                $contents = '';
                foreach ($this->value as $element)
                    $contents .= _term_to_binary($element);
                return pack('CN', TAG_LIST_EXT, $length) . $contents .
                       chr(TAG_NIL_EXT);
            }
        }
        else
        {
            throw new OutputException('unknown list type');
        }
    }
    public function __toString()
    {
        return sprintf('%s(array(%s),improper=%s)', get_class(),
                       implode(',', $this->value),
                       $this->improper ? 'true' : 'false');
    }
}

class OtpErlangMap
{
    public $pairs;
    public function __construct($pairs)
    {
        $this->pairs = $pairs;
    }
    public function binary()
    {
        $length = count($this->pairs);
        if ($length <= 4294967295)
        {
            $term_packed = '';
            foreach ($this->pairs as $pair)
            {
                list($key, $value) = $pair;
                $key_packed = _term_to_binary($key);
                $value_packed = _term_to_binary($value);
                $term_packed .= $key_packed . $value_packed;
            }
            return pack('CN', TAG_MAP_EXT, $length) . $term_packed;
        }
        else
        {
            throw new OutputException('uint32 overflow');
        }
    }
    public function __toString()
    {
        return sprintf('%s(%d)', get_class(),
                       count($this->pairs));
    }
}

class OtpErlangPid
{
    public $node;
    public $id;
    public $serial;
    public $creation;
    public function __construct($node, $id, $serial, $creation)
    {
        $this->node = $node;
        $this->id = $id;
        $this->serial = $serial;
        $this->creation = $creation;
    }
    public function binary()
    {
        $creation_size = strlen($this->creation);
        if ($creation_size == 1) {
            return chr(TAG_PID_EXT) . $this->node->binary() .
                   $this->id . $this->serial . $this->creation;
        }
        elseif ($creation_size == 4) {
            return chr(TAG_NEW_PID_EXT) . $this->node->binary() .
                   $this->id . $this->serial . $this->creation;
        }
        else {
            throw new OutputException('unknown pid type');
        }
    }
    public function __toString()
    {
        return sprintf('%s(%s,%s,%s,%s)', get_class(),
                       $this->node, $this->id, $this->serial, $this->creation);
    }
}

class OtpErlangPort
{
    public $node;
    public $id;
    public $creation;
    public function __construct($node, $id, $creation)
    {
        $this->node = $node;
        $this->id = $id;
        $this->creation = $creation;
    }
    public function binary()
    {
        $id_size = strlen($this->id);
        if ($id_size == 8) {
            return chr(TAG_V4_PORT_EXT) .
                   $this->node->binary() . $this->id . $this->creation;
        }
        $creation_size = strlen($this->creation);
        if ($creation_size == 4) {
            return chr(TAG_NEW_PORT_EXT) .
                   $this->node->binary() . $this->id . $this->creation;
        }
        elseif ($creation_size == 1) {
            return chr(TAG_PORT_EXT) .
                   $this->node->binary() . $this->id . $this->creation;
        }
        else {
            throw new OutputException('unknown port type');
        }
    }
    public function __toString()
    {
        return sprintf('%s(%s,%s,%s)', get_class(),
                       $this->node, $this->id, $this->creation);
    }
}

class OtpErlangReference
{
    public $node;
    public $id;
    public $creation;
    public function __construct($node, $id, $creation)
    {
        $this->node = $node;
        $this->id = $id;
        $this->creation = $creation;
    }
    public function binary()
    {
        $length = intval(strlen($this->id) / 4);
        if ($length == 0)
        {
            return chr(TAG_REFERENCE_EXT) .
                   $this->node->binary() . $this->id . $this->creation;
        }
        elseif ($length <= 65535)
        {
            $creation_size = strlen($this->creation);
            if ($creation_size == 1) {
                return pack('Cn', TAG_NEW_REFERENCE_EXT, $length) .
                       $this->node->binary() . $this->creation . $this->id;
            }
            elseif ($creation_size == 4) {
                return pack('Cn', TAG_NEWER_REFERENCE_EXT, $length) .
                       $this->node->binary() . $this->creation . $this->id;
            }
            else {
                throw new OutputException('unknown reference type');
            }
        }
        else
        {
            throw new OutputException('uint16 overflow');
        }
    }
    public function __toString()
    {
        return sprintf('%s(%s,%s,%s)', get_class(),
                       $this->node, $this->id, $this->creation);
    }
}

class OtpErlangFunction
{
    public $tag;
    public $value;
    public function __construct($tag, $value)
    {
        $this->tag = $tag;
        $this->value = $value;
    }
    public function binary()
    {
        return chr($this->tag) . $this->value;
    }
    public function __toString()
    {
        return sprintf('%s(%s,%s)', get_class(),
                       $this->tag, $this->value);
    }
}

function _error_handler($errno = 0, $errstr = null,
                        $errfile = null, $errline = null)
{
    // If error is suppressed with @, don't throw an exception
    if (error_reporting() === 0)
        return true; // return true to continue through the others handlers
    throw new \ErrorException($errstr, 0, $errno, $errfile, $errline);
}

// core functionality

function binary_to_term($data)
{
    if (! is_string($data))
        throw new ParseException('not bytes input');
    $size = strlen($data);
    if ($size <= 1)
        throw new ParseException('null input');
    if (ord($data[0]) != TAG_VERSION)
        throw new ParseException('invalid version');
    set_error_handler('Erlang\_error_handler');
    try
    {
        list($i, $term) = _binary_to_term(1, $data);
        restore_error_handler();
        if ($i != $size)
            throw new ParseException('unparsed data');
        return $term;
    }
    catch (\ErrorException $e)
    {
        restore_error_handler();
        throw new ParseException((string) $e);
    }
}

function term_to_binary($term, $compressed = false)
{
    $data_uncompressed = _term_to_binary($term);
    if ($compressed === false)
    {
        return chr(TAG_VERSION) . $data_uncompressed;
    }
    else
    {
        if ($compressed === true)
            $compressed = 6;
        if ($compressed < 0 || $compressed > 9)
            throw new InputException('compressed in [0..9]');
        $data_compressed = gzcompress($data_uncompressed, $compressed);
        $size_uncompressed = strlen($data_uncompressed);
        if ($size_uncompressed > 4294967295)
        {
            throw new OutputException('uint32 overflow');
        }
        return pack('CCN', TAG_VERSION, TAG_COMPRESSED_ZLIB,
                    $size_uncompressed) . $data_compressed;
    }
}

// binary_to_term implementation functions

function _binary_to_term($i, $data)
{
    $tag = ord($data[$i]);
    $i += 1;
    switch ($tag)
    {
        case TAG_NEW_FLOAT_EXT:
            if (unpack('S', "\x01\x00") == array(1 => 1)) // little endian
                list(, $value) = unpack('d', strrev(substr($data, $i, 8)));
            else
                list(, $value) = unpack('d', substr($data, $i, 8));
            return array($i + 8, $value);
        case TAG_BIT_BINARY_EXT:
            list(, $j) = unpack('N', substr($data, $i, 4));
            $i += 4;
            $bits = ord($data[$i]);
            $i += 1;
            return array($i + $j,
                         new OtpErlangBinary(substr($data, $i, $j), $bits));
        case TAG_ATOM_CACHE_REF:
            return array($i + 1, new OtpErlangAtom(ord($data[$i])));
        case TAG_SMALL_INTEGER_EXT:
            return array($i + 1, ord($data[$i]));
        case TAG_INTEGER_EXT:
            list(, $value) = unpack('N', substr($data, $i, 4));
            if ($value & 0x80000000)
                $value = -2147483648 + ($value & 0x7fffffff);
            return array($i + 4, $value);
        case TAG_FLOAT_EXT:
            return array($i + 31, floatval(substr($data, $i, 31)));
        case TAG_V4_PORT_EXT:
        case TAG_NEW_PORT_EXT:
        case TAG_REFERENCE_EXT:
        case TAG_PORT_EXT:
            list($i, $node) = _binary_to_atom($i, $data);
            if ($tag == TAG_V4_PORT_EXT) {
                $id = substr($data, $i, 8);
                $i += 8;
            }
            else {
                $id = substr($data, $i, 4);
                $i += 4;
            }
            if ($tag == TAG_V4_PORT_EXT || $tag == TAG_NEW_PORT_EXT) {
                $creation = substr($data, $i, 4);
                $i += 4;
            }
            else {
                $creation = $data[$i];
                $i += 1;
                if ($tag == TAG_REFERENCE_EXT)
                    return array($i, new OtpErlangReference($node, $id,
                                                            $creation));
            }
            // $tag == TAG_V4_PORT_EXT || $tag == TAG_NEW_PORT_EXT ||
            // $tag == TAG_PORT_EXT)
            return array($i, new OtpErlangPort($node, $id, $creation));
        case TAG_NEW_PID_EXT:
        case TAG_PID_EXT:
            list($i, $node) = _binary_to_atom($i, $data);
            $id = substr($data, $i, 4);
            $i += 4;
            $serial = substr($data, $i, 4);
            $i += 4;
            if ($tag == TAG_NEW_PID_EXT) {
                $creation = substr($data, $i, 4);
                $i += 4;
            }
            elseif ($tag == TAG_PID_EXT) {
                $creation = $data[$i];
                $i += 1;
            }
            return array($i, new OtpErlangPid($node, $id, $serial, $creation));
        case TAG_SMALL_TUPLE_EXT:
        case TAG_LARGE_TUPLE_EXT:
            if ($tag == TAG_SMALL_TUPLE_EXT)
            {
                $length = ord($data[$i]);
                $i += 1;
            }
            elseif ($tag == TAG_LARGE_TUPLE_EXT)
            {
                list(, $length) = unpack('N', substr($data, $i, 4));
                $i += 4;
            }
            return _binary_to_term_sequence($i, $length, $data);
        case TAG_NIL_EXT:
            return array($i, new OtpErlangList(array()));
        case TAG_STRING_EXT:
            list(, $j) = unpack('n', substr($data, $i, 2));
            $i += 2;
            return array($i + $j, substr($data, $i, $j));
        case TAG_LIST_EXT:
            list(, $length) = unpack('N', substr($data, $i, 4));
            $i += 4;
            list($i, $tmp) = _binary_to_term_sequence($i, $length, $data);
            list($i, $tail) = _binary_to_term($i, $data);
            if (get_class($tail) != 'Erlang\OtpErlangList' or
                $tail->value != array())
            {
                $tmp[] = $tail;
                $tmp = new OtpErlangList($tmp, true);
            }
            else
            {
                $tmp = new OtpErlangList($tmp);
            }
            return array($i, $tmp);
        case TAG_BINARY_EXT:
            list(, $j) = unpack('N', substr($data, $i, 4));
            $i += 4;
            return array($i + $j,
                         new OtpErlangBinary(substr($data, $i, $j), 8));
        case TAG_SMALL_BIG_EXT:
        case TAG_LARGE_BIG_EXT:
            if ($tag == TAG_SMALL_BIG_EXT)
            {
                $j = ord($data[$i]);
                $i += 1;
            }
            elseif ($tag == TAG_LARGE_BIG_EXT)
            {
                list(, $j) = unpack('N', substr($data, $i, 4));
                $i += 4;
            }
            $sign = ord($data[$i]);
            $bignum = 0;
            if ($j > 0)
            {
                foreach (range(0, $j - 1) as $bignum_index)
                {
                    $digit = ord($data[$i + $j - $bignum_index]);
                    $bignum = $bignum * 256 + $digit;
                }
            }
            if ($sign == 1)
                $bignum *= -1;
            $i += 1;
            return array($i + $j, $bignum);
        case TAG_NEW_FUN_EXT:
            list(, $length) = unpack('N', substr($data, $i, 4));
            return array($i + $length,
                         new OtpErlangFunction($tag, substr($data, $i,
                                                            $length)));
        case TAG_EXPORT_EXT:
            $old_i = $i;
            list($i, $module) = _binary_to_atom($i, $data);
            list($i, $function) = _binary_to_atom($i, $data);
            if (ord($data[$i]) != TAG_SMALL_INTEGER_EXT)
                throw new ParseException('invalid small integer tag');
            $i += 1;
            $arity = ord($data[$i]);
            $i += 1;
            return array($i,
                         new OtpErlangFunction($tag,
                                               substr($data,
                                                      $old_i, $i - $old_i)));
        case TAG_NEWER_REFERENCE_EXT:
        case TAG_NEW_REFERENCE_EXT:
            list(, $j) = unpack('n', substr($data, $i, 2));
            $j *= 4;
            $i += 2;
            list($i, $node) = _binary_to_atom($i, $data);
            if ($tag == TAG_NEWER_REFERENCE_EXT) {
                $creation = substr($data, $i, 4);
                $i += 4;
            }
            elseif ($tag == TAG_NEW_REFERENCE_EXT) {
                $creation = $data[$i];
                $i += 1;
            }
            $id = substr($data, $i, $j);
            return array($i + $j,
                         new OtpErlangReference($node, $id, $creation));
        case TAG_MAP_EXT:
            list(, $length) = unpack('N', substr($data, $i, 4));
            $i += 4;
            $pairs = array();
            if ($length > 0)
            {
                foreach (range(0, $length - 1) as $length_index)
                {
                    list($i, $key) = _binary_to_term($i, $data);
                    list($i, $value) = _binary_to_term($i, $data);
                    $pairs[] = array($key, $value);
                }
            }
            return array($i, new OtpErlangMap($pairs));
        case TAG_FUN_EXT:
            $old_i = $i;
            list(, $numfree) = unpack('N', substr($data, $i, 4));
            $i += 4;
            list($i, $pid) = _binary_to_pid($i, $data);
            list($i, $name_module) = _binary_to_atom($i, $data);
            list($i, $index) = _binary_to_integer($i, $data);
            list($i, $uniq) = _binary_to_integer($i, $data);
            list($i, $free) = _binary_to_term_sequence($i, $numfree, $data);
            return array($i,
                         new OtpErlangFunction($tag,
                                               substr($data,
                                                      $old_i, $i - $old_i)));
        case TAG_ATOM_UTF8_EXT:
        case TAG_ATOM_EXT:
            list(, $j) = unpack('n', substr($data, $i, 2));
            $i += 2;
            $atom_name = substr($data, $i, $j);
            global $UNDEFINED;
            if ($atom_name == 'true') {
                $tmp = true;
            }
            elseif ($atom_name == 'false') {
                $tmp = false;
            }
            elseif ($atom_name == $UNDEFINED) {
                $tmp = null;
            }
            else {
                $utf8 = ($tag == TAG_ATOM_UTF8_EXT);
                $tmp = new OtpErlangAtom($atom_name, $utf8);
            }
            return array($i + $j, $tmp);
        case TAG_SMALL_ATOM_UTF8_EXT:
        case TAG_SMALL_ATOM_EXT:
            $j = ord($data[$i]);
            $i += 1;
            $atom_name = substr($data, $i, $j);
            global $UNDEFINED;
            if ($atom_name == 'true') {
                $tmp = true;
            }
            elseif ($atom_name == 'false') {
                $tmp = false;
            }
            elseif ($atom_name == $UNDEFINED) {
                $tmp = null;
            }
            else {
                $utf8 = ($tag == TAG_SMALL_ATOM_UTF8_EXT);
                $tmp = new OtpErlangAtom($atom_name, $utf8);
            }
            return array($i + $j, $tmp);
        case TAG_COMPRESSED_ZLIB:
            list(, $size_uncompressed) = unpack('N', substr($data, $i, 4));
            if ($size_uncompressed == 0)
                throw new ParseException('compressed data null');
            $i += 4;
            $data_compressed = substr($data, $i);
            $j = strlen($data_compressed);
            $data_uncompressed = gzuncompress($data_compressed);
            if ($size_uncompressed != strlen($data_uncompressed))
                throw new ParseException('compression corrupt');
            list($i_new, $term) = _binary_to_term(0, $data_uncompressed);
            if ($i_new != $size_uncompressed)
                throw new ParseException('unparsed data');
            return array($i + $j, $term);
        case TAG_LOCAL_EXT:
            throw new ParseException('LOCAL_EXT is opaque');
        default:
            throw new ParseException('invalid tag');
    }
}

function _binary_to_term_sequence($i, $length, $data)
{
    $sequence = array();
    if ($length > 0)
    {
        foreach (range(0, $length - 1) as $length_index)
        {
            list($i, $element) = _binary_to_term($i, $data);
            $sequence[] = $element;
        }
    }
    return array($i, $sequence);
}

// (binary_to_term Erlang term primitive type functions)

function _binary_to_integer($i, $data)
{
    $tag = ord($data[$i]);
    $i += 1;
    if ($tag == TAG_SMALL_INTEGER_EXT)
    {
        return array($i + 1, ord($data[$i]));
    }
    elseif ($tag == TAG_INTEGER_EXT)
    {
        list(, $value) = unpack('N', substr($data, $i, 4));
        if ($value & 0x80000000)
            $value = -2147483648 + ($value & 0x7fffffff);
        return array($i + 4, $value);
    }
    else
    {
        throw new ParseException('invalid integer tag');
    }
}

function _binary_to_pid($i, $data)
{
    $tag = ord($data[$i]);
    $i += 1;
    switch ($tag)
    {
        case TAG_NEW_PID_EXT:
            list($i, $node) = _binary_to_atom($i, $data);
            $id = substr($data, $i, 4);
            $i += 4;
            $serial = substr($data, $i, 4);
            $i += 4;
            $creation = substr($data, $i, 4);
            $i += 4;
            return array($i, new OtpErlangPid($node, $id, $serial, $creation));
        case TAG_PID_EXT:
            list($i, $node) = _binary_to_atom($i, $data);
            $id = substr($data, $i, 4);
            $i += 4;
            $serial = substr($data, $i, 4);
            $i += 4;
            $creation = $data[$i];
            $i += 1;
            return array($i, new OtpErlangPid($node, $id, $serial, $creation));
        default:
            throw new ParseException('invalid pid tag');
    }
}

function _binary_to_atom($i, $data)
{
    $tag = ord($data[$i]);
    $i += 1;
    switch ($tag)
    {
        case TAG_ATOM_EXT:
            list(, $j) = unpack('n', substr($data, $i, 2));
            $i += 2;
            return array($i + $j,
                         new OtpErlangAtom(substr($data, $i, $j), false));
        case TAG_ATOM_CACHE_REF:
            return array($i + 1, new OtpErlangAtom(ord($data[$i])));
        case TAG_SMALL_ATOM_EXT:
            $j = ord($data[$i]);
            $i += 1;
            return array($i + $j,
                         new OtpErlangAtom(substr($data, $i, $j), false));
        case TAG_ATOM_UTF8_EXT:
            list(, $j) = unpack('n', substr($data, $i, 2));
            $i += 2;
            return array($i + $j, new OtpErlangAtom(substr($data, $i, $j)));
        case TAG_SMALL_ATOM_UTF8_EXT:
            $j = ord($data[$i]);
            $i += 1;
            return array($i + $j, new OtpErlangAtom(substr($data, $i, $j)));
        default:
            throw new ParseException('invalid atom tag');
    }
}

// term_to_binary implementation functions

function _term_to_binary($term)
{
    if (is_string($term))
        return _string_to_binary($term);
    elseif (is_array($term))
        return _tuple_to_binary($term);
    elseif (is_int($term))
        return _integer_to_binary($term);
    elseif (is_float($term))
        return _float_to_binary($term);
    elseif (is_bool($term))
    {
        if ($term)
            $object = new OtpErlangAtom('true');
        else
            $object = new OtpErlangAtom('false');
        return $object->binary();
    }
    elseif (is_null($term))
    {
        global $UNDEFINED;
        $object = new OtpErlangAtom($UNDEFINED);
        return $object->binary();
    }
    elseif (is_object($term))
    {
        switch (get_class($term))
        {
            case 'Erlang\OtpErlangAtom':
            case 'Erlang\OtpErlangList':
            case 'Erlang\OtpErlangBinary':
            case 'Erlang\OtpErlangFunction':
            case 'Erlang\OtpErlangReference':
            case 'Erlang\OtpErlangPort':
            case 'Erlang\OtpErlangPid':
            case 'Erlang\OtpErlangMap':
                return $term->binary();
            default:
                throw new OutputException('unknown php object');
        }
    }
    else
    {
        throw new OutputException('unknown php type');
    }
}

// (term_to_binary Erlang term composite type functions)

function _string_to_binary($term)
{
    $length = strlen($term);
    if ($length == 0)
    {
        return chr(TAG_NIL_EXT);
    }
    elseif ($length <= 65535)
    {
        return pack('Cn', TAG_STRING_EXT, $length) . $term;
    }
    elseif ($length <= 4294967295)
    {
        $term_packed = '';
        foreach (str_split($term) as $c)
        {
            $term_packed .= chr(TAG_SMALL_INTEGER_EXT) . $c;
        }
        return pack('CN', TAG_LIST_EXT, $length) . $term_packed .
               chr(TAG_NIL_EXT);
    }
    else
    {
        throw new OutputException('uint32 overflow');
    }
}

function _tuple_to_binary($term)
{
    $length = count($term);
    $term_packed = '';
    foreach ($term as $element)
    {
        $term_packed .= _term_to_binary($element);
    }
    if ($length <= 255)
        return pack('CC', TAG_SMALL_TUPLE_EXT, $length) . $term_packed;
    elseif ($length <= 4294967295)
        return pack('CN', TAG_LARGE_TUPLE_EXT, $length) . $term_packed;
    else
        throw new OutputException('uint32 overflow');
}

// (term_to_binary Erlang term primitive type functions)

function _integer_to_binary($term)
{
    if (0 <= $term and $term <= 255)
        return pack('CC', TAG_SMALL_INTEGER_EXT, $term);
    elseif (-2147483648 <= $term and $term <= 2147483647)
        return pack('CN', TAG_INTEGER_EXT, $term);
    else
        return _bignum_to_binary($term);
}

function _bignum_to_binary($term)
{
    // in PHP only for supporting integers > 32 bits (no native bignums)
    $bignum = abs($term);
    if ($term < 0)
        $sign = 1;
    else
        $sign = 0;
    $l = '';
    while ($bignum > 0)
    {
        $l .= chr($bignum & 255);
        $bignum >>= 8;
    }
    $length = strlen($l);
    if ($length <= 255)
        return pack('CCC', TAG_SMALL_BIG_EXT, $length, $sign) . $l;
    elseif ($length <= 4294967295)
        return pack('CNC', TAG_LARGE_BIG_EXT, $length, $sign) . $l;
    else
        throw new OutputException('uint32 overflow');
}

function _float_to_binary($term)
{
    if (unpack('S', "\x01\x00") == array(1 => 1)) // little endian
        return chr(TAG_NEW_FLOAT_EXT) . strrev(pack('d', $term));
    else
        return chr(TAG_NEW_FLOAT_EXT) . pack('d', $term);
}

// Elixir use can set to 'nil'
function set_undefined($value)
{
    global $UNDEFINED;
    $UNDEFINED = $value;
}

// Exception classes listed alphabetically

class InputException extends \Exception
{
    public function __construct($message, $code = 0, Exception $previous = null)
    {
        parent::__construct($message, $code, $previous);
    }
}

class OutputException extends \Exception
{
    public function __construct($message, $code = 0, Exception $previous = null)
    {
        parent::__construct($message, $code, $previous);
    }
}

class ParseException extends \Exception
{
    public function __construct($message, $code = 0, Exception $previous = null)
    {
        parent::__construct($message, $code, $previous);
    }
}

?>
