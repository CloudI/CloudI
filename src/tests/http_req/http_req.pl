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

use strict;
use warnings;

my $use_threads = eval 'use threads; 1';
require CloudI::API;
require CloudI::TerminateException;

# not using a task object to keep the source code in a single file
sub task
{
    my ($api) = @_;
    eval
    {
        my $task_request = sub
        {
            my ($request_type, $name, $pattern, $request_info, $request,
                $timeout, $priority, $trans_id, $pid) = @_;
            my %http_qs = $api->info_key_value_parse($request);
            my $response;
            if (! defined($http_qs{'value'}))
            {
                $response =
"<http_test><error>no value specified</error></http_test>";
            }
            else
            {
                my $value = $http_qs{'value'};
                if (ref($value) eq 'ARRAY')
                {
                    $value = shift(@$value);
                }
                $response =
"<http_test><value>$value</value></http_test>";
            }
            $api->return_($request_type, $name, $pattern,
                          '', $response, $timeout, $trans_id, $pid);
        };

        # run
        $api->subscribe('perl.xml/get', $task_request);
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
            print "$e";
        }
    }
    print "terminate http_req perl\n";
}

sub assert
{
    my ($test) = @_;
    CloudI::API->assert($test);
}

{
    assert($use_threads);
    my $thread_count = CloudI::API->thread_count();
    my @threads = ();
    for my $i (0 .. ($thread_count - 1))
    {
        my $t = threads->create(\&task, (CloudI::API->new($i)));
        assert(defined($t));
        push(@threads, $t);
    }
    for my $t (@threads)
    {
        $t->join();
    }
}

