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

path = File.split(File.dirname(__FILE__)); path.pop(3)
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
                @api.subscribe("ruby", method(:flood))

                result = @api.poll
                $stdout.puts "exited thread: #{result}"
            end

            private

            def assert
                raise "Assertion failed !" unless yield # if $DEBUG
            end

            def flood(command, name, pattern, request_info, request,
                      timeout, priority, transId, pid)
                @api.return_(command, name, pattern,
                             "", "ruby", timeout, transId, pid)
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

