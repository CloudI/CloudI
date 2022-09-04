#-*-Mode:perl;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=perl fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# MIT License
#
# Copyright (c) 2014-2022 Michael Truog <mjtruog at protonmail dot com>
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

use strict;
use warnings;

my $use_threads = eval 'use threads; 1';
require CloudI::API;
require CloudI::TerminateException;

use constant DESTINATION => '/tests/msg_size/erlang';

sub task
{
    my ($thread_index) = @_;
    eval
    {
        my $api = CloudI::API->new($thread_index);
        my $task_request = sub
        {
            my ($request_type, $name, $pattern, $request_info, $request,
                $timeout, $priority, $trans_id, $source) = @_;
            my $i = unpack('L', substr($request, 0, 4));
            if ($i == 1073741823)
            {
                $i = 0;
            }
            else
            {
                $i += 1;
            }
            $request = pack('L', $i) . substr($request, 4);
            my $name_next = DESTINATION;
            print "forward #$i perl to $name_next " .
                  "(with timeout $timeout ms)\n";
            $api->forward_($request_type, $name_next, $request_info, $request,
                           $timeout, $priority, $trans_id, $source);
        };

        # run
        $api->subscribe('perl', $task_request);
        my $result = $api->poll();
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
    print "terminate msg_size perl\n";
}

sub assert
{
    my ($test) = @_;
    CloudI::API::assert($test);
}

{
    assert($use_threads);
    my $thread_count = CloudI::API::thread_count();
    my @threads = ();
    for my $thread_index (0 .. ($thread_count - 1))
    {
        my $t = threads->create(\&task, ($thread_index));
        assert(defined($t));
        push(@threads, $t);
    }
    for my $t (@threads)
    {
        $t->join();
    }
}

