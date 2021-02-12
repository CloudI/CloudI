#!/usr/bin/env ruby
#-*-Mode:ruby;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=ruby fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# MIT License
#
# Copyright (c) 2011-2021 Michael Truog <mjtruog at protonmail dot com>
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

$DEBUG = false

require 'cloudi'

if __FILE__ == $PROGRAM_NAME
    thread_count = CloudI::API.thread_count()

    threads = (0...thread_count).to_a.map{ |i| Thread.new(i){ |thread_index|
        class Task
            def initialize(thread_index)
                @api = nil
                @thread_index = thread_index
            end

            def run
                @api = CloudI::API.new(@thread_index)
                @api.subscribe('ruby', method(:request))

                result = @api.poll
                assert{result == false}
            end

            private

            DESTINATION = '/tests/msg_size/erlang'

            def assert(&test)
                CloudI::API.assert(&test)
            end

            def request(request_type, name, pattern, request_info, request,
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
                @api.forward_(request_type, name_next, request_info, request,
                              timeout, priority, trans_id, pid)
            end
        end

        Task.new(thread_index).run
    }}
    threads.each{ |t| t.join }
end

