#-*-Mode:perl;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=perl fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# BSD LICENSE
# 
# Copyright (c) 2014-2017, Michael Truog <mjtruog at gmail dot com>
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
            my %http_qs = $api->request_http_qs_parse($request);
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

