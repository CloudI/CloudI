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

package Erlang::OtpErlangList;
use strict;
use warnings;

use constant TAG_NIL_EXT => 106;
use constant TAG_LIST_EXT => 108;

require Erlang;
require Erlang::OutputException;

use overload
    '""'     => sub { $_[0]->as_string };

sub new
{
    my $class = shift;
    my ($value_ref, $improper) = @_;
    if (! defined($improper))
    {
        $improper = 0;
    }
    my $self = bless {
        value => $value_ref,
        improper => $improper,
    }, $class;
    return $self;
}

sub binary
{
    my $self = shift;
    my $value_ref = $self->{value};
    if (ref($value_ref) eq 'ARRAY')
    {
        my @value = @$value_ref;
        my $length = scalar(@value);
        if ($length == 0)
        {
            return chr(TAG_NIL_EXT);
        }
        elsif ($length > 4294967295)
        {
            die Erlang::OutputException->new('uint32 overflow');
        }
        elsif ($self->{improper})
        {
            my $contents = '';
            for my $element (@value)
            {
                $contents .= Erlang::_term_to_binary($element);
            }
            return pack('CN', TAG_LIST_EXT, $length - 1) . $contents;
        }
        else
        {
            my $contents = '';
            for my $element (@value)
            {
                $contents .= Erlang::_term_to_binary($element);
            }
            return pack('CN', TAG_LIST_EXT, $length) . $contents .
                   chr(TAG_NIL_EXT);
        }
    }
    else
    {
        die Erlang::OutputException->new('unknown list type');
    }
}

sub as_string
{
    my $self = shift;
    my $class = ref($self);
    my $value_ref = $self->{value};
    my $list = join(',', @$value_ref);
    return "$class([$list],$self->{improper})";
}

sub count
{
    my $self = shift;
    my $value_ref = $self->{value};
    return scalar(@$value_ref);
}

1;
