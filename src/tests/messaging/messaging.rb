#!/usr/bin/env ruby
# -*- coding: utf-8; Mode: ruby; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
# ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
#
# BSD LICENSE
# 
# Copyright (c) 2011-2012, Michael Truog <mjtruog at gmail dot com>
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

path = File.split(File.dirname(__FILE__)); path.pop(2)
$:.unshift File.join(*path, *%w[api ruby])

$DEBUG = false

require 'cloudi'

if __FILE__ == $PROGRAM_NAME
    thread_count = CloudI::API.thread_count()

    threads = (0...thread_count).to_a.map{ |i| Thread.new(i){ |thread_index|
        class Task
            def initialize(thread_index)
                @api = CloudI::API.new(thread_index)
                @thread_index = thread_index
            end

            def run
                if @thread_index == 0
                    @api.send_async(@api.prefix + 'sequence1', 'start')
                end
                @api.subscribe('a/b/c/d', method(:sequence1_abcd))
                #@api.subscribe('a/b/c/*', method(:sequence1_abc_))
                #@api.subscribe('a/b/*/d', method(:sequence1_ab_d))
                #@api.subscribe('a/*/c/d', method(:sequence1_a_cd))
                #@api.subscribe('*/b/c/d', method(:sequence1__bcd))
                #@api.subscribe('a/b/*',   method(:sequence1_ab__))
                #@api.subscribe('a/*/d',   method(:sequence1_a__d))
                #@api.subscribe('*/c/d',   method(:sequence1___cd))
                #@api.subscribe('a/*',     method(:sequence1_a___))
                #@api.subscribe('*/d',     method(:sequence1____d))
                #@api.subscribe('*',       method(:sequence1_____))
                @api.subscribe('sequence1', method(:sequence1))
                #@api.subscribe('e', method(:sequence2_e1))
                #@api.subscribe('e', method(:sequence2_e2))
                #@api.subscribe('e', method(:sequence2_e3))
                #@api.subscribe('e', method(:sequence2_e4))
                #@api.subscribe('e', method(:sequence2_e5))
                #@api.subscribe('e', method(:sequence2_e6))
                #@api.subscribe('e', method(:sequence2_e7))
                #@api.subscribe('e', method(:sequence2_e8))
                #@api.subscribe('sequence2', method(:sequence2))
                #@api.subscribe('f1', method(:sequence3_f1))
                #@api.subscribe('f2', method(:sequence3_f2))
                #@api.subscribe('g1', method(:sequence3_g1))
                #@api.subscribe('sequence3', method(:sequence3))

                result = @api.poll
                $stdout.puts "exited thread: #{result}"
            end

            private

            def assert
                raise 'Assertion failed !' unless yield # if $DEBUG
            end

            def sequence1_abcd(command, name, pattern, requestInfo, request,
                               timeout, priority, transId, pid)
                assert{pattern == "#{@api.prefix}a/b/c/d"}
                assert{request == 'test1'}
                @api.return_(command, name, pattern,
                             '', request, timeout, transId, pid)
            end

            def sequence1(command, name, pattern, requestInfo, request,
                          timeout, priority, transId, pid)
                $stdout.puts 'messaging sequence1 start ruby'
                assert{request == 'start'}
                test1_id = @api.send_async("#{@api.prefix}a/b/c/d", 'test1')
                test1 = @api.recv_async()
                test1_check = test1[1]
                test1_id_check = test1[2]
                assert{test1_check == 'test1'}
                assert{test1_id_check == test1_id}
                $stdout.puts 'messaging sequence1 end ruby'
                @api.return_(command, name, pattern,
                             '', 'end', timeout, transId, pid)
            end
        end
        begin
            object = Task.new(thread_index)
            object.run
        rescue
            $stderr.puts $!.message
            $stderr.puts $!.backtrace
        end
    }}
    threads.each{ |t| t.join }
end

