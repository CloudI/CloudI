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

package Erlang::Exception;
use strict;
use warnings;

use overload
    '""'     => sub { $_[0]->as_string },
    'bool'   => sub { 1 },
    fallback => 1;

sub new
{
    my $class = shift;
    my ($message) = @_;
    my $self = bless {
        traceback => _traceback(),
        message => $message,
    }, $class;
    return $self;
}

sub as_string
{
    my $self = shift;
    my $name = ref($self);
    my $output = $self->{traceback};
    if (defined($self->{message}))
    {
        $output = "$output$name: $self->{message}\n";
    }
    else
    {
        $output = "$output$name\n";
    }
    return $output;
}

sub _traceback
{
    # provide a Pythonic traceback
    my $result = '';
    my $frame_i = 1;
    while (my ($package, $filename, $line) = caller($frame_i++))
    {
        $result = "  File \"$filename\", line $line, in $package\n$result";
    }
    return "Traceback (most recent call last):\n$result";
}

1;
