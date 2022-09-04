#-*-Mode:perl;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=perl fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# MIT License
#
# Copyright (c) 2017-2022 Michael Truog <mjtruog at protonmail dot com>
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

package CountTask;
use strict;
use warnings;

my $use_threads = eval 'use threads; 1';
require CloudI::API;
require CloudI::TerminateException;

sub new
{
    my $class = shift;
    my ($thread_index) = @_;
    my $self = bless {
        api => '',
        thread_index => $thread_index,
        count => 0,
    }, $class;
    return $self;
}

sub run
{
    my $self = shift;
    eval
    {
        $self->{api} = CloudI::API->new($self->{thread_index});
        $self->{api}->subscribe('perl/get', $self, '_request');
        my $result = $self->{api}->poll();
        assert($result == 0);
    };
    my $e = $@;
    if ($e)
    {
        if ($e->isa('CloudI::TerminateException'))
        {
            1;
        }
        else
        {
            print STDERR "$e";
        }
    }
    print "terminate count perl\n";
}

sub assert
{
    my ($test) = @_;
    CloudI::API::assert($test);
}

sub _request
{
    my $self = shift;
    my ($request_type, $name, $pattern, $request_info, $request,
        $timeout, $priority, $trans_id, $source) = @_;
    if ($self->{count} == 4294967295)
    {
        $self->{count} = 0;
    }
    else
    {
        $self->{count} += 1;
    }
    print "count == $self->{count} perl\n";
    my $response = "$self->{count}";
    my $response_info = CloudI::API::info_key_value_new({});
    $self->{api}->return_($request_type, $name, $pattern,
                          $response_info, $response,
                          $timeout, $trans_id, $source);
}

{
    assert($use_threads);
    my $thread_count = CloudI::API::thread_count();
    my @threads = ();
    for my $thread_index (0 .. ($thread_count - 1))
    {
        my $t = threads->create(sub
        {
            my $task = CountTask->new($thread_index);
            return $task->run();
        });
        assert(defined($t));
        push(@threads, $t);
    }
    for my $t (@threads)
    {
        $t->join();
    }
}

1;
