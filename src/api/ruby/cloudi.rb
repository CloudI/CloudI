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

$:.unshift File.dirname(__FILE__)

$stdout.sync = true
$stderr.sync = true

require 'erlang'

module Cloudi
    class API
        include Erlang
        # unbuffered output is with $stderr.puts '...'

        def initialize(index, protocol, size)
            @socket = IO.for_fd(index + 3, File::RDWR, autoclose: false)
            @socket.sync = true
            @use_header = (protocol == 'tcp')
            @size = size
            @callbacks = Hash.new
            send(term_to_binary(:init))
            poll
        end

        def subscribe(name, function)
            @callbacks[@prefix + name] = function
            send(term_to_binary([:subscribe, name]))
        end

        def unsubscribe(name, function)
            @callbacks.delete(@prefix + name)
            send(term_to_binary([:unsubscribe, name]))
        end

        def send_async(name, request)
            send_async(name, '', request, @timeoutAsync, 0)
        end

        def send_async(name, request_info, request,
                       timeout, priority)
            send(term_to_binary([:send_async, name,
                                  OtpErlangBinary.new(request_info),
                                  OtpErlangBinary.new(request),
                                  timeout, priority]))
            return poll
        end

        def send_sync(name, request)
            send_sync(name, '', request, @timeoutSync, 0)
        end

        def send_sync(name, request_info, request,
                      timeout, priority)
            send(term_to_binary([:send_sync, name,
                                  OtpErlangBinary.new(request_info),
                                  OtpErlangBinary.new(request),
                                  timeout, priority]))
            return poll
        end

        def mcast_async(name, request)
            mcast_async(name, '', request, @timeoutAsync, 0)
        end

        def mcast_async(name, request_info, request,
                        timeout, priority)
            send(term_to_binary([:mcast_async, name,
                                  OtpErlangBinary.new(request_info),
                                  OtpErlangBinary.new(request),
                                  timeout, priority]))
            return poll
        end

        def forward_(command, name, request_info, request,
                     timeout, priority, transId, pid)
            case command
            when ASYNC
                forward_async(name, request_info, request,
                              timeout, priority, transId, pid)
            when SYNC
                forward_sync(name, request_info, request,
                             timeout, priority, transId, pid)
            end
        end

        def forward_async(name, request_info, request,
                          timeout, priority, transId, pid)
            send(term_to_binary([:forward_async, name,
                                  OtpErlangBinary.new(request_info),
                                  OtpErlangBinary.new(request),
                                  timeout, priority,
                                  OtpErlangBinary.new(transId), pid]))
            raise ReturnAsyncException
        end

        def forward_sync(name, request_info, request,
                         timeout, priority, transId, pid)
            send(term_to_binary([:forward_sync, name, 
                                  OtpErlangBinary.new(request_info),
                                  OtpErlangBinary.new(request),
                                  timeout, priority,
                                  OtpErlangBinary.new(transId), pid]))
            raise ReturnSyncException
        end

        def return_(command, name, response_info, response,
                    timeout, transId, pid)
            case command
            when ASYNC
                return_async(name, response_info, response,
                             timeout, transId, pid)
            when SYNC
                return_sync(name, response_info, response,
                            timeout, transId, pid)
            end
        end

        def return_async(name, response_info, response,
                         timeout, transId, pid)
            return_async_nothrow(name, response_info, response,
                                 timeout, transId, pid)
            raise ReturnAsyncException
        end

        def return_async_nothrow(name, response_info, response,
                                 timeout, transId, pid)
            send(term_to_binary([:return_async, name,
                                  OtpErlangBinary.new(response_info),
                                  OtpErlangBinary.new(response),
                                  timeout,
                                  OtpErlangBinary.new(transId), pid]))
        end

        def return_sync(name, response_info, response,
                        timeout, transId, pid)
            return_sync_nothrow(name, response_info, response,
                                timeout, transId, pid)
            raise ReturnSyncException
        end

        def return_sync_nothrow(name, response_info, response,
                                timeout, transId, pid)
            send(term_to_binary([:return_sync, name,
                                  OtpErlangBinary.new(response_info),
                                  OtpErlangBinary.new(response),
                                  timeout,
                                  OtpErlangBinary.new(transId), pid]))
        end

        def recv_async(timeout, transId)
            send(term_to_binary([:recv_async, timeout,
                                  OtpErlangBinary.new(transId)]))
            return poll
        end

        def callback(command, name, requestInfo, request,
                     timeout, priority, transId, pid)
            function = @callbacks[name]
            assert{function != nil}
            case command
            when MESSAGE_SEND_ASYNC
                begin
                    response = function.call(ASYNC, name,
                                             requestInfo, request,
                                             timeout, priority, transId, pid)
                    if response.kind_of?(Array)
                        assert{response.length == 2}
                        responseInfo = response[0]
                        response = response[1]
                    else
                        responseInfo = ''
                    end
                rescue ReturnAsyncException
                    return
                rescue ReturnSyncException
                    assert{false}
                    return
                rescue
                    # exception is ignored at this level
                    responseInfo = ''
                    response = ''
                end
                return_async_nothrow(name, responseInfo, response,
                                     timeout, transId, pid)
            when MESSAGE_SEND_SYNC
                begin
                    response = function.call(SYNC, name,
                                             requestInfo, request,
                                             timeout, priority, transId, pid)
                    if response.kind_of?(Array)
                        assert{response.length == 2}
                        responseInfo = response[0]
                        response = response[1]
                    else
                        responseInfo = ''
                    end
                rescue ReturnSyncException
                    return
                rescue ReturnAsyncException
                    assert{false}
                    return
                rescue
                    # exception is ignored at this level
                    responseInfo = ''
                    response = ''
                end
                return_sync_nothrow(name, responseInfo, response,
                                    timeout, transId, pid)
            end
        end

        def poll
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

            data = recv('')
            if data.bytesize == 0
                return nil
            end

            loop do
                i = 0; j = 4
                command = data[i, j].unpack('L')[0]
                case command
                when MESSAGE_INIT
                    i += j; j = 4
                    prefixSize = data[i, j].unpack('L')[0]
                    i += j; j = prefixSize + 4 + 4
                    tmp = data[i, j].unpack("Z#{prefixSize}LL")
                    @prefix = tmp[0]
                    @timeoutAsync = tmp[1]
                    @timeoutSync = tmp[2]
                    i += j
                    assert{i == data.length}
                    return
                when MESSAGE_SEND_ASYNC, MESSAGE_SEND_SYNC
                    i += j; j = 4
                    nameSize = data[i, j].unpack('L')[0]
                    i += j; j = nameSize + 4
                    tmp = data[i, j].unpack("Z#{nameSize}L")
                    name = tmp[0]
                    requestInfoSize = tmp[1]
                    i += j; j = requestInfoSize + 1 + 4
                    tmp = data[i, j].unpack("a#{requestInfoSize}xL")
                    requestInfo = tmp[0]
                    requestSize = tmp[1]
                    i += j; j = requestSize + 1 + 4 + 1 + 16 + 4
                    tmp = data[i, j].unpack("a#{requestSize}xLca16L")
                    request = tmp[0]
                    timeout = tmp[1]
                    priority = tmp[2]
                    transId = tmp[3]
                    pidSize = tmp[4]
                    i += j; j = pidSize
                    pid = data[i, j].unpack("a#{pidSize}")[0]
                    i += j
                    assert{i == data.length}
                    data.clear()
                    callback(command, name, requestInfo, request,
                             timeout, priority, transId, binary_to_term(pid))
                when MESSAGE_RECV_ASYNC, MESSAGE_RETURN_SYNC
                    i += j; j = 4
                    responseInfoSize = data[i, j].unpack('L')[0]
                    i += j; j = responseInfoSize + 1 + 4
                    tmp = data[i, j].unpack("a#{responseInfoSize}xL")
                    responseInfo = tmp[0]
                    responseSize = tmp[1]
                    i += j; j = responseSize + 1 + 16
                    i += j
                    assert{i == data.length}
                    tmp = data[i, j].unpack("a#{responseSize}xa16")
                    response = tmp[0]
                    transId = tmp[1]
                    return [responseInfo, response, transId]
                when MESSAGE_RETURN_ASYNC
                    i += j; j = 16
                    i += j
                    assert{i == data.length}
                    return data[i, j].unpack('a16')[0]
                when MESSAGE_RETURNS_ASYNC
                    i += j; j = 4
                    transIdCount = data[i, j].unpack('L')[0]
                    i += j; j = 16 * transIdCount
                    i += j
                    assert{i == data.length}
                    return data[i, j].unpack('a16' * transIdCount)
                when MESSAGE_KEEPALIVE
                    send(term_to_binary(:keepalive))
                    i += j
                    assert{i >= data.length}
                    data.slice!(0, i)
                    if data.length > 0
                        if IO.select([@socket], nil, nil, 0).nil?
                            next
                        end
                    end
                else
                    assert{false}
                end

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
    
                data = recv(data)
                if data.bytesize == 0
                    return nil
                end
            end
        end

        def binary_key_value_parse(binary)
            result = {}
            data = binary.split(NULL.chr)
            (0...(data.length)).step(2).each do |i|
                result[data[i]] = data[i + 1]
            end
            result
        end

        def request_http_qs_parse(request)
            request_key_value_parse(request)
        end

        def request_info_key_value_parse(request_info)
            request_key_value_parse(request_info)
        end

        private :return_async_nothrow
        private :return_sync_nothrow
        private :callback
        private :binary_key_value_parse
        private

        def send(data)
            if @use_header
                data = [data.length].pack('N') + data
            end
            @socket.write(data)
        end

        def recv(data)
            if @use_header
                while data.length < 4
                    fragment = @socket.readpartial(@size)
                    data += fragment
                end
                total = data[0,4].unpack('N')[0]
                data.slice!(0..3)
                while data.length < total
                    fragment = @socket.readpartial(@size)
                    data += fragment
                end
            else
                ready = true
                while ready == true
                    fragment = @socket.readpartial(@size)
                    data += fragment
                    ready = (fragment.bytesize == @size)
    
                    if ready
                        ready = ! IO.select([@socket], nil, nil, 0).nil?
                    end
                end
            end
            data
        end

        ASYNC  =  1
        SYNC   = -1

        MESSAGE_INIT           = 1
        MESSAGE_SEND_ASYNC     = 2
        MESSAGE_SEND_SYNC      = 3
        MESSAGE_RECV_ASYNC     = 4
        MESSAGE_RETURN_ASYNC   = 5
        MESSAGE_RETURN_SYNC    = 6
        MESSAGE_RETURNS_ASYNC  = 7
        MESSAGE_KEEPALIVE      = 8

        NULL = 0

        def assert
            raise 'Assertion failed !' unless yield if $DEBUG
        end

        class ReturnSyncException < SystemExit
        end

        class ReturnAsyncException < SystemExit
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
        api = Cloudi::API.new(index, protocol, buffer_size)

        class Foobar
            def initialize(api)
                @api = api
            end
            def foobar(command, name, request, timeout, transId, pid)
                puts 'got foobar'
                @api.return_(command, name, 'bye', timeout, transId, pid)
            end
        end
        object = Foobar.new(api)

        api.subscribe('foobar', object.method(:foobar))
        api.poll
    }}
    threads.each{ |t| t.join}
end

