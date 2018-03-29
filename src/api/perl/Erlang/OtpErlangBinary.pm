#-*-Mode:perl;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=perl fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# MIT License
#
# Copyright (c) 2014-2017 Michael Truog <mjtruog at protonmail dot com>
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

package Erlang::OtpErlangBinary;
use strict;
use warnings;

use constant TAG_BIT_BINARY_EXT => 77;
use constant TAG_BINARY_EXT => 109;

require Erlang::OutputException;

use overload
    '""'     => sub { $_[0]->as_string };

sub new
{
    my $class = shift;
    my ($value, $bits) = @_;
    if (! defined($bits))
    {
        $bits = 8;
    }
    my $self = bless {
        value => $value,
        bits => $bits,
    }, $class;
    return $self;
}

sub binary
{
    my $self = shift;
    my $value = $self->{value};
    if (ref($value) eq '')
    {
        my $length = length($value);
        if ($length > 4294967295)
        {
            die Erlang::OutputException->new('uint32 overflow');
        }
        my $bits = $self->{bits};
        if ($bits != 8)
        {
            return pack('CNC', TAG_BIT_BINARY_EXT, $length, $bits) . $value;
        }
        else
        {
            return pack('CN', TAG_BINARY_EXT, $length) . $value;
        }
    }
    else
    {
        die Erlang::OutputException->new('unknown binary type');
    }
}

sub as_string
{
    my $self = shift;
    my $class = ref($self);
    return "$class($self->{value},$self->{bits})";
}

1;
