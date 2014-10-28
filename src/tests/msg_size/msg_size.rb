#!/usr/bin/env ruby
#-*-Mode:ruby;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=ruby fenc=utf-8 sts=4 ts=4 sw=4 et:
#
# BSD LICENSE
# 
# Copyright (c) 2011-2014, Michael Truog <mjtruog at gmail dot com>
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
            end

            def run
                @api.subscribe('ruby', method(:request))

                result = @api.poll
                assert{result == false}
            end

            private

            DESTINATION = '/tests/msg_size/erlang'

            def assert(&test)
                CloudI::API.assert(&test)
            end

            def request(command, name, pattern, request_info, request,
                        timeout, priority, trans_id, pid)
                i = request[0,4].unpack('L')[0]
                if i == 4294967295
                    i = 0
                else
                    i += 1
                end
                request.slice!(0..3)
                request = [i].pack('L') + request
                name_next = DESTINATION
                $stdout.puts "forward ##{i} ruby to #{name_next} " \
                             "(with timeout #{timeout} ms)"
                @api.forward_(command, name_next, request_info, request,
                              timeout, priority, trans_id, pid)
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

