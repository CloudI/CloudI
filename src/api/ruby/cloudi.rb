#!/usr/bin/env ruby
# -*- coding: utf-8; Mode: ruby; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
# ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
#
# BSD LICENSE
# 
# Copyright (c) 2011, Michael Truog <mjtruog at gmail dot com>
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

$:.unshift File.join(File.dirname(__FILE__),
                     *%w[gems gems erlectricity-1.1.1 lib])

require 'erlectricity'

module Cloudi
    include Erlectricity
    class API

        MESSAGE_INIT           = 1
        MESSAGE_SEND_ASYNC     = 2
        MESSAGE_SEND_SYNC      = 3
        MESSAGE_RECV_ASYNC     = 4
        MESSAGE_RETURN_ASYNC   = 5
        MESSAGE_RETURN_SYNC    = 6
        MESSAGE_RETURNS_ASYNC  = 7

        def initialize(index, protocol, size)
            STDOUT.sync = true
            STDERR.sync = true

            @socket = IO.for_fd(index + 3, File::RDWR)
            @socket.sync = true
            @size = size
            @encoder = Erlectricity::Encoder.new(@socket)
        end

        def poll
            while true
                ready = false
                while ready == false
                    result = IO.select([@socket], nil, [@socket])
                    if result[2].length > 0
                        return nil
                    end
                    if result[0].length > 0
                        ready = true
                    end
                end

                data = fragment = @socket.read(@size)
                while fragment.bytesize == @size
                    fragment = @socket.read(@size)
                    data += fragment
                end

                if data.bytesize == 0
                    return nil
                end

                # WORK IN PROGRESS, NOT READY YET

                puts data
                Erlectricity::Decoder.decode(data)
            end
        end

        private

        def send(term)
            @encoder.write_any(term)
        end

    end
end

if __FILE__ == $PROGRAM_NAME
    if ARGV.size != 3
        puts "Usage: #{$PROGRAM_NAME} thread_count protocol buffer_size"
        exit 1
    end
    thread_count = Integer(ARGV[0])
    protocol = ARGV[1]
    buffer_size = Integer(ARGV[2])

    threads = (0...thread_count).to_a.map{ |i| Thread.new(i){ |index|
        puts "start"
        api = Cloudi::API.new(index, protocol, buffer_size)
        puts api.poll
        puts "end"
    }}
    threads.each{ |t| t.join}
end

