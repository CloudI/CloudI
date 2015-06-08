#-*-Mode:perl;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=perl fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
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

package Erlang::OtpErlangAtom;
use strict;
use warnings;

use constant TAG_ATOM_CACHE_REF => 78;
use constant TAG_ATOM_EXT => 100;
use constant TAG_SMALL_ATOM_EXT => 115;
use constant TAG_ATOM_UTF8_EXT => 118;
use constant TAG_SMALL_ATOM_UTF8_EXT => 119;

require Erlang::OutputException;

use overload
    '""'     => sub { $_[0]->as_string };

sub new
{
    my $class = shift;
    my ($value, $utf8) = @_;
    if (! defined($utf8))
    {
        $utf8 = 0;
    }
    my $self = bless {
        value => $value,
        utf8 => $utf8,
    }, $class;
    return $self;
}

sub binary
{
    my $self = shift;
    my $value = $self->{value};
    if ($value =~ /^\d+$/)
    {
        return pack('CC', TAG_ATOM_CACHE_REF, $value);
    }
    elsif (ref($value) eq '')
    {
        my $size = length($value);
        if ($self->{utf8})
        {
            if ($size < 256)
            {
                return pack('CC', TAG_SMALL_ATOM_UTF8_EXT, $size) . $value;
            }
            else
            {
                return pack('Cn', TAG_ATOM_UTF8_EXT, $size) . $value;
            }
        }
        else
        {
            if ($size < 256)
            {
                return pack('CC', TAG_SMALL_ATOM_EXT, $size) . $value;
            }
            else
            {
                return pack('Cn', TAG_ATOM_EXT, $size) . $value;
            }
        }
    }
    else
    {
        die Erlang::OutputException->new('unknown atom type');
    }
}

sub as_string
{
    my $self = shift;
    my $class = ref($self);
    return "$class($self->{value},$self->{utf8})";
}

1;
