#-*-Mode:perl;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=perl fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# MIT License
#
# Copyright (c) 2014-2019 Michael Truog <mjtruog at protonmail dot com>
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

package Erlang::OtpErlangPid;
use strict;
use warnings;

use constant TAG_NEW_PID_EXT => 88;
use constant TAG_PID_EXT => 103;

require Erlang::OutputException;

use overload
    '""'     => sub { $_[0]->as_string };

sub new
{
    my $class = shift;
    my ($node, $id, $serial, $creation) = @_;
    my $self = bless {
        node => $node,
        id => $id,
        serial => $serial,
        creation => $creation,
    }, $class;
    return $self;
}

sub binary
{
    my $self = shift;
    my $creation_size = length($self->{creation});
    if ($creation_size == 1)
    {
        return chr(TAG_PID_EXT) . $self->{node}->binary() .
               $self->{id} . $self->{serial} . $self->{creation};
    }
    elsif ($creation_size == 4)
    {
        return chr(TAG_NEW_PID_EXT) . $self->{node}->binary() .
               $self->{id} . $self->{serial} . $self->{creation};
    }
    else
    {
        die Erlang::OutputException->new('unknown pid type');
    }
}

sub as_string
{
    my $self = shift;
    my $class = ref($self);
    return "$class($self->{node},$self->{id},$self->{serial}," .
                  "$self->{creation})";
}

1;
