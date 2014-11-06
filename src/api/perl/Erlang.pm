#!/usr/bin/env perl
#-*-Mode:perl;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=perl fenc=utf-8 sts=4 ts=4 sw=4 et:
#
# BSD LICENSE
# 
# Copyright (c) 2014, Michael Truog <mjtruog at gmail dot com>
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

package Erlang;
use strict;
use warnings;
use 5.010;

$Erlang::VERSION = '1.40';

require Compress::Zlib;
# bigint/bignum caused slowness without enough features to be useful
use POSIX;

# tag values here http://www.erlang.org/doc/apps/erts/erl_ext_dist.html
use constant TAG_VERSION => 131;
use constant TAG_COMPRESSED_ZLIB => 80;
use constant TAG_NEW_FLOAT_EXT => 70;
use constant TAG_BIT_BINARY_EXT => 77;
use constant TAG_ATOM_CACHE_REF => 78;
use constant TAG_SMALL_INTEGER_EXT => 97;
use constant TAG_INTEGER_EXT => 98;
use constant TAG_FLOAT_EXT => 99;
use constant TAG_ATOM_EXT => 100;
use constant TAG_REFERENCE_EXT => 101;
use constant TAG_PORT_EXT => 102;
use constant TAG_PID_EXT => 103;
use constant TAG_SMALL_TUPLE_EXT => 104;
use constant TAG_LARGE_TUPLE_EXT => 105;
use constant TAG_NIL_EXT => 106;
use constant TAG_STRING_EXT => 107;
use constant TAG_LIST_EXT => 108;
use constant TAG_BINARY_EXT => 109;
use constant TAG_SMALL_BIG_EXT => 110;
use constant TAG_LARGE_BIG_EXT => 111;
use constant TAG_NEW_FUN_EXT => 112;
use constant TAG_EXPORT_EXT => 113;
use constant TAG_NEW_REFERENCE_EXT => 114;
use constant TAG_SMALL_ATOM_EXT => 115;
use constant TAG_MAP_EXT => 116;
use constant TAG_FUN_EXT => 117;
use constant TAG_ATOM_UTF8_EXT => 118;
use constant TAG_SMALL_ATOM_UTF8_EXT => 119;

require Erlang::OtpErlangAtom;
require Erlang::OtpErlangList;
require Erlang::OtpErlangBinary;
require Erlang::OtpErlangFunction;
require Erlang::OtpErlangReference;
require Erlang::OtpErlangPort;
require Erlang::OtpErlangPid;
require Erlang::OtpErlangString;
require Erlang::ParseException;
require Erlang::InputException;
require Erlang::OutputException;

sub binary_to_term
{
    my ($data) = @_;
    if (! defined($data) || ref($data) ne '')
    {
        die Erlang::ParseException->new('not bytes input');
    }
    my $size = length($data);
    if ($size <= 1)
    {
        die Erlang::ParseException->new('null input');
    }
    if (ord(substr($data, 0, 1)) != TAG_VERSION)
    {
        die Erlang::ParseException->new('invalid version');
    }
    my $result = eval
    {
        my ($i, $term) = _binary_to_term(1, $data);
        if ($i != $size)
        {
            die Erlang::ParseException->new('unparsed data');
        }
        return $term;
    };
    my $e = $@;
    if ($e)
    {
        if ($e->isa('Erlang::ParseException') ||
            $e->isa('Erlang::InputException'))
        {
            die $e;
        }
        else
        {
            die Erlang::ParseException->new('missing data');
        }
    }
    return $result;
}

sub term_to_binary
{
    my ($term, $compressed) = @_;
    my $data_uncompressed = _term_to_binary($term);
    if (! defined($compressed))
    {
        return chr(TAG_VERSION) . $data_uncompressed;
    }
    else
    {
        if (ref($compressed) ne '')
        {
            die Erlang::InputException->new('compressed in [0..9]');
        }
        if ($compressed !~ /^[0-9]$/)
        {
            $compressed = 6;
        }
        my $data_compressed = Compress::Zlib::compress($data_uncompressed,
                                                       $compressed);
        if (! defined($data_compressed))
        {
            die Erlang::InputException->new('compression failed');
        }
        my $size_uncompressed = length($data_uncompressed);
        return pack('CCN', TAG_VERSION, TAG_COMPRESSED_ZLIB,
                    $size_uncompressed) . $data_compressed;
    }
}

sub _binary_to_term
{
    no warnings 'all';
    my ($i, $data) = @_;
    my $tag = ord(substr($data, $i, 1));
    $i += 1;
    if ($tag == TAG_NEW_FLOAT_EXT)
    {
        my $value;
        if (! (unpack('S', "\x01\x00") cmp (1))) # little endian
        {
            ($value) = unpack('d', reverse(substr($data, $i, 8)));
        }
        else
        {
            ($value) = unpack('d', substr($data, $i, 8));
        }
        return ($i + 8, $value);
    }
    elsif ($tag == TAG_BIT_BINARY_EXT)
    {
        my ($j) = unpack('N', substr($data, $i, 4));
        $i += 4;
        my $bits = ord(substr($data, $i, 1));
        $i += 1;
        return ($i + $j,
                Erlang::OtpErlangBinary->new(substr($data, $i, $j), $bits));
    }
    elsif ($tag == TAG_ATOM_CACHE_REF)
    {
        return ($i + 1, Erlang::OtpErlangAtom->new(ord(substr($data, $i, 1))));
    }
    elsif ($tag == TAG_SMALL_INTEGER_EXT)
    {
        return ($i + 1, ord(substr($data, $i, 1)));
    }
    elsif ($tag == TAG_INTEGER_EXT)
    {
        my ($value) = unpack('N', substr($data, $i, 4));
        if ($value & 0x80000000)
        {
            $value = -2147483648 + ($value & 0x7fffffff);
        }
        return ($i + 4, $value);
    }
    elsif ($tag == TAG_FLOAT_EXT)
    {
        return ($i + 31, substr($data, $i, 31));
    }
    elsif ($tag == TAG_ATOM_EXT)
    {
        my ($j) = unpack('n', substr($data, $i, 2));
        $i += 2;
        return ($i + $j, Erlang::OtpErlangAtom->new(substr($data, $i, $j)));
    }
    elsif ($tag == TAG_REFERENCE_EXT || $tag == TAG_PORT_EXT)
    {
        my $node;
        ($i, $node) = _binary_to_atom($i, $data);
        my $id = substr($data, $i, 4);
        $i += 4;
        my $creation = substr($data, $i, 1);
        $i += 1;
        if ($tag == TAG_REFERENCE_EXT)
        {
            return ($i, Erlang::OtpErlangReference->new($node, $id, $creation));
        }
        elsif ($tag == TAG_PORT_EXT)
        {
            return ($i, Erlang::OtpErlangPort->new($node, $id, $creation));
        }
    }
    elsif ($tag == TAG_PID_EXT)
    {
        my $node;
        ($i, $node) = _binary_to_atom($i, $data);
        my $id = substr($data, $i, 4);
        $i += 4;
        my $serial = substr($data, $i, 4);
        $i += 4;
        my $creation = substr($data, $i, 1);
        $i += 1;
        return ($i, Erlang::OtpErlangPid->new($node, $id, $serial, $creation));
    }
    elsif ($tag == TAG_SMALL_TUPLE_EXT || $tag == TAG_LARGE_TUPLE_EXT)
    {
        my $arity;
        if ($tag == TAG_SMALL_TUPLE_EXT)
        {
            $arity = ord(substr($data, $i, 1));
            $i += 1;
        }
        elsif ($tag == TAG_LARGE_TUPLE_EXT)
        {
            ($arity) = unpack('N', substr($data, $i, 4));
            $i += 4;
        }
        return _binary_to_term_sequence($i, $arity, $data);
    }
    elsif ($tag == TAG_NIL_EXT)
    {
        return ($i, Erlang::OtpErlangList->new([]));
    }
    elsif ($tag == TAG_STRING_EXT)
    {
        my ($j) = unpack('n', substr($data, $i, 2));
        $i += 2;
        return ($i + $j, substr($data, $i, $j));
    }
    elsif ($tag == TAG_LIST_EXT)
    {
        my ($arity) = unpack('N', substr($data, $i, 4));
        $i += 4;
        my $tmp;
        ($i, $tmp) = _binary_to_term_sequence($i, $arity, $data);
        my $tail;
        ($i, $tail) = _binary_to_term($i, $data);
        if (ref($tail) ne 'Erlang::OtpErlangList' || $tail->count() != 0)
        {
            push(@$tmp, $tail);
            return ($i, Erlang::OtpErlangList->new($tmp, 1));
        }
        else
        {
            return ($i, Erlang::OtpErlangList->new($tmp));
        }
    }
    elsif ($tag == TAG_BINARY_EXT)
    {
        my ($j) = unpack('N', substr($data, $i, 4));
        $i += 4;
        return ($i + $j,
                Erlang::OtpErlangBinary->new(substr($data, $i, $j), 8));
    }
    elsif ($tag == TAG_SMALL_BIG_EXT || $tag == TAG_LARGE_BIG_EXT)
    {
        my $j;
        if ($tag == TAG_SMALL_BIG_EXT)
        {
            $j = ord(substr($data, $i, 1));
            $i += 1;
        }
        elsif ($tag == TAG_LARGE_BIG_EXT)
        {
            ($j) = unpack('N', substr($data, $i, 4));
            $i += 4;
        }
        my $sign = ord(substr($data, $i, 1));
        my $bignum = 0;
        if ($j > 0)
        {
            my $digit;
            for my $bignum_index (0 .. ($j - 1))
            {
                $digit = ord(substr($data, $i + $j - $bignum_index, 1));
                $bignum = $bignum * 256 + $digit;
            }
        }
        if ($sign == 1)
        {
            $bignum *= -1;
        }
        $i += 1;
        return ($i + $j, $bignum);
    }
    elsif ($tag == TAG_NEW_FUN_EXT)
    {
        my ($size) = unpack('N', substr($data, $i, 4));
        return ($i + $size,
                Erlang::OtpErlangFunction->new($tag, substr($data, $i, $size)));
    }
    elsif ($tag == TAG_EXPORT_EXT)
    {
        my $old_i = $i;
        my $module;
        ($i, $module) = _binary_to_atom($i, $data);
        my $function;
        ($i, $function) = _binary_to_atom($i, $data);
        if (ord(substr($data, $i, 1)) != TAG_SMALL_INTEGER_EXT)
        {
            die Erlang::ParseException->new('invalid small integer tag');
        }
        $i += 1;
        my $arity = ord(substr($data, $i, 1));
        $i += 1;
        return ($i, Erlang::OtpErlangFunction->new($tag,
                                                   substr($data, $old_i,
                                                          $i - $old_i)));
    }
    elsif ($tag == TAG_NEW_REFERENCE_EXT)
    {
        my ($j) = unpack('n', substr($data, $i, 2));
        $j *= 4;
        $i += 2;
        my $node;
        ($i, $node) = _binary_to_atom($i, $data);
        my $creation = substr($data, $i, 1);
        $i += 1;
        my $id = substr($data, $i, $j);
        return ($i + $j,
                Erlang::OtpErlangReference->new($node, $id, $creation));
    }
    elsif ($tag == TAG_SMALL_ATOM_EXT)
    {
        my $j = ord(substr($data, $i, 1));
        $i += 1;
        my $atom_name = substr($data, $i, $j);
        return ($i + $j, Erlang::OtpErlangAtom->new($atom_name));
    }
    elsif ($tag == TAG_MAP_EXT)
    {
        my ($arity) = unpack('N', substr($data, $i, 4));
        $i += 4;
        my %pairs = ();
        if ($arity > 0)
        {
            my $key;
            my $value;
            for my $arity_index (0 .. ($arity - 1))
            {
                ($i, $key) = _binary_to_term($i, $data);
                ($i, $value) = _binary_to_term($i, $data);
                $pairs{$key} = $value;
            }
        }
        return ($i, %pairs);
    }
    elsif ($tag == TAG_FUN_EXT)
    {
        my $old_i = $i;
        my ($numfree) = unpack('N', substr($data, $i, 4));
        $i += 4;
        my $pid;
        ($i, $pid) = _binary_to_pid($i, $data);
        my $name_module;
        ($i, $name_module) = _binary_to_atom($i, $data);
        my $index;
        ($i, $index) = _binary_to_integer($i, $data);
        my $uniq;
        ($i, $uniq) = _binary_to_integer($i, $data);
        my $free;
        ($i, $free) = _binary_to_term_sequence($i, $numfree, $data);
        return ($i, Erlang::OtpErlangFunction->new($tag,
                                                   substr($data, $old_i,
                                                          $i - $old_i)));
    }
    elsif ($tag == TAG_ATOM_UTF8_EXT)
    {
        my ($j) = unpack('n', substr($data, $i, 2));
        $i += 2;
        my $atom_name = substr($data, $i, $j);
        return ($i + $j, Erlang::OtpErlangAtom->new($atom_name, 1));
    }
    elsif ($tag == TAG_SMALL_ATOM_UTF8_EXT)
    {
        my $j = ord(substr($data, $i, 1));
        $i += 1;
        my $atom_name = substr($data, $i, $j);
        return ($i + $j, Erlang::OtpErlangAtom->new($atom_name, 1));
    }
    elsif ($tag == TAG_COMPRESSED_ZLIB)
    {
        my ($size_uncompressed) = unpack('N', substr($data, $i, 4));
        if ($size_uncompressed == 0)
        {
            die Erlang::ParseException->new('compressed data null');
        }
        $i += 4;
        my $data_compressed = substr($data, $i);
        my $j = length($data_compressed);
        my $data_uncompressed = Compress::Zlib::uncompress($data_compressed);
        if (! defined($data_uncompressed) ||
            $size_uncompressed != length($data_uncompressed))
        {
            die Erlang::ParseException->new('compression corrupt');
        }
        my ($i_new, $term) = _binary_to_term(0, $data_uncompressed);
        if ($i_new != $size_uncompressed)
        {
            die Erlang::ParseException->new('unparsed data');
        }
        return ($i + $j, $term);
    }
    else
    {
        die Erlang::ParseException->new('invalid tag');
    }
}

sub _binary_to_term_sequence
{
    no warnings 'all';
    my ($i, $arity, $data) = @_;
    my @sequence = ();
    if ($arity > 0)
    {
        my $element;
        for my $arity_index (0 .. ($arity - 1))
        {
            ($i, $element) = _binary_to_term($i, $data);
            push(@sequence, $element);
        }
    }
    return ($i, \@sequence);
}

sub _binary_to_integer
{
    no warnings 'all';
    my ($i, $data) = @_;
    my $tag = ord(substr($data, $i, 1));
    $i += 1;
    if ($tag == TAG_SMALL_INTEGER_EXT)
    {
        return ($i + 1, ord(substr($data, $i, 1)));
    }
    elsif ($tag == TAG_INTEGER_EXT)
    {
        my ($value) = unpack('N', substr($data, $i, 4));
        if ($value & 0x80000000)
        {
            $value = -2147483648 + ($value & 0x7fffffff);
        }
        return ($i + 4, $value);
    }
    else
    {
        die Erlang::ParseException->new('invalid integer tag');
    }
}

sub _binary_to_pid
{
    no warnings 'all';
    my ($i, $data) = @_;
    my $tag = ord(substr($data, $i, 1));
    $i += 1;
    if ($tag == TAG_PID_EXT)
    {
        my $node;
        ($i, $node) = _binary_to_atom($i, $data);
        my $id = substr($data, $i, 4);
        $i += 4;
        my $serial = substr($data, $i, 4);
        $i += 4;
        my $creation = substr($data, $i, 1);
        $i += 1;
        return ($i, Erlang::OtpErlangPid->new($node, $id, $serial, $creation));
    }
    else
    {
        die Erlang::ParseException->new('invalid pid tag');
    }
}

sub _binary_to_atom
{
    no warnings 'all';
    my ($i, $data) = @_;
    my $tag = ord(substr($data, $i, 1));
    $i += 1;
    if ($tag == TAG_ATOM_EXT)
    {
        my ($j) = unpack('n', substr($data, $i, 2));
        $i += 2;
        return ($i + $j, Erlang::OtpErlangAtom->new(substr($data, $i, $j)));
    }
    elsif ($tag == TAG_ATOM_CACHE_REF)
    {
        return ($i + 1, Erlang::OtpErlangAtom->new(ord(substr($data, $i, 1))));
    }
    elsif ($tag == TAG_SMALL_ATOM_EXT)
    {
        my $j = ord(substr($data, $i, 1));
        $i += 1;
        return ($i + $j, Erlang::OtpErlangAtom->new(substr($data, $i, $j)));
    }
    elsif ($tag == TAG_ATOM_UTF8_EXT)
    {
        my ($j) = unpack('n', substr($data, $i, 2));
        $i += 2;
        return ($i + $j, Erlang::OtpErlangAtom->new(substr($data, $i, $j), 1));
    }
    elsif ($tag == TAG_SMALL_ATOM_UTF8_EXT)
    {
        my $j = ord(substr($data, $i, 1));
        $i += 1;
        return ($i + $j, Erlang::OtpErlangAtom->new(substr($data, $i, $j), 1));
    }
    else
    {
        die Erlang::ParseException->new('invalid atom tag');
    }
}

sub _term_to_binary
{
    my ($term) = @_;
    my $ref = ref($term);
    if ($ref eq '')
    {
        if (scalar($term) ne $term) # list
        {
            return _tuple_to_binary($term);
        }
        elsif ($term =~ /^[+-]?\d+$/)
        {
            return _integer_to_binary($term);
        }
        elsif ($term =~ /^([+-]?)(?=\d|\.\d)\d*(\.\d*)?([Ee]([+-]?\d+))?$/)
        {
            return _float_to_binary($term);
        }
        else
        {
            return _string_to_binary($term);
        }
    }
    elsif ($ref eq 'ARRAY')
    {
        return _tuple_to_binary($term);
    }
    elsif ($ref eq 'HASH')
    {
        return _hash_to_binary($term);
    }
    elsif ($ref eq 'Erlang::OtpErlangAtom' ||
           $ref eq 'Erlang::OtpErlangList' ||
           $ref eq 'Erlang::OtpErlangBinary' ||
           $ref eq 'Erlang::OtpErlangFunction' ||
           $ref eq 'Erlang::OtpErlangReference' ||
           $ref eq 'Erlang::OtpErlangPort' ||
           $ref eq 'Erlang::OtpErlangPid' ||
           $ref eq 'Erlang::OtpErlangString')
    {
        return $term->binary();
    }
    else
    {
        die Erlang::OutputException->new('unknown perl type');
    }
}

sub _string_to_binary
{
    my ($term) = @_;
    my $arity = length($term);
    if ($arity == 0)
    {
        return chr(TAG_NIL_EXT);
    }
    elsif ($arity < 65536)
    {
        return pack('Cn', TAG_STRING_EXT, $arity) . $term;
    }
    else
    {
        my $term_packed = '';
        for my $c (split(//, $term))
        {
            $term_packed .= chr(TAG_SMALL_INTEGER_EXT) . $c;
        }
        return pack('CN', TAG_LIST_EXT, $arity) . $term_packed .
               chr(TAG_NIL_EXT);
    }
}

sub _tuple_to_binary
{
    my ($term_ref) = @_;
    my @term = @$term_ref;
    my $arity = scalar(@term);
    my $term_packed = '';
    for my $element (@term)
    {
        $term_packed .= _term_to_binary($element);
    }
    if ($arity < 256)
    {
        return pack('CC', TAG_SMALL_TUPLE_EXT, $arity) . $term_packed;
    }
    else
    {
        return pack('CN', TAG_LARGE_TUPLE_EXT, $arity) . $term_packed;
    }
}

sub _integer_to_binary
{
    my ($term) = @_;
    if (0 <= $term && $term <= 255)
    {
        return pack('CC', TAG_SMALL_INTEGER_EXT, $term);
    }
    elsif (-2147483648 <= $term and $term <= 2147483647)
    {
        return pack('CN', TAG_INTEGER_EXT, $term);
    }
    else
    {
        return _bignum_to_binary($term);
    }
}

sub _bignum_to_binary
{
    my ($term) = @_;
    my $bignum = abs($term);
    my $size = ceil(_bignum_bit_length($bignum) / 8.0);
    my $sign;
    if ($term < 0)
    {
        $sign = chr(1);
    }
    else
    {
        $sign = chr(0);
    }
    my $l = $sign;
    if ($size > 0)
    {
        for my $byte (0 .. ($size - 1))
        {
            $l .= chr($bignum & 255);
            $bignum = $bignum >> 8;
        }
    }
    if ($size < 256)
    {
        return pack('CC', TAG_SMALL_BIG_EXT, $size) . $l;
    }
    else
    {
        return pack('CN', TAG_LARGE_BIG_EXT, $size) . $l;
    }
}

sub _bignum_bit_length
{
    my ($bignum) = @_;
    my $count = 0;
    while ($bignum)
    {
        $bignum = $bignum >> 1;
        $count++;
    }
    return $count;
}

sub _float_to_binary
{
    my ($term) = @_;
    if (! (unpack('S', "\x01\x00") cmp (1))) # little endian
    {
        return chr(TAG_NEW_FLOAT_EXT) . reverse(pack('d', $term));
    }
    else
    {
        return chr(TAG_NEW_FLOAT_EXT) . pack('d', $term);
    }
}

sub _hash_to_binary
{
    my (%term) = @_;
    my $term_packed = '';
    my $arity = 0;
    while (my ($key, $value) = each(%term))
    {
        $term_packed .= _term_to_binary($key) . _term_to_binary($value);
        $arity++;
    }
    return pack('CN', TAG_MAP_EXT, $arity) . $term_packed;
}

1;
