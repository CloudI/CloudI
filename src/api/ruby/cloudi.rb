#!/usr/bin/env ruby
#-*-Mode:ruby;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=ruby fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
#
# MIT License
#
# Copyright (c) 2011-2020 Michael Truog <mjtruog at protonmail dot com>
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

$:.unshift File.dirname(__FILE__)

$stdout.sync = true
$stderr.sync = true

require 'erlang'
require 'stringio'

module CloudI
    class API
        include Erlang
        # unbuffered output is with $stderr.puts '...'

        ASYNC  =  1
        SYNC   = -1

        def initialize(thread_index)
            protocol = API.getenv('CLOUDI_API_INIT_PROTOCOL')
            buffer_size_str = API.getenv('CLOUDI_API_INIT_BUFFER_SIZE')
            if protocol == 'tcp'
                @s = IO.for_fd(thread_index + 3, File::RDWR, autoclose: false)
                @s.sync = true
                @use_header = true
            elsif protocol == 'udp'
                @s = IO.for_fd(thread_index + 3, File::RDWR, autoclose: false)
                @s.sync = true
                @use_header = false
            elsif protocol == 'local'
                @s = IO.for_fd(thread_index + 3, File::RDWR, autoclose: false)
                @s.sync = true
                @use_header = true
            else
                raise InvalidInputException
            end
            @initialization_complete = false
            @terminate = false
            @size = buffer_size_str.to_i
            @callbacks = Hash.new
            @timeout_terminate = 10 # TIMEOUT_TERMINATE_MIN
            send(Erlang.term_to_binary(:init))
            poll_request(nil, false)
        end
        attr_reader :process_index
        attr_reader :process_count
        attr_reader :process_count_max
        attr_reader :process_count_min
        attr_reader :prefix
        attr_reader :timeout_initialize
        attr_reader :timeout_async
        attr_reader :timeout_sync
        attr_reader :timeout_terminate

        def self.thread_count
            s = getenv('CLOUDI_API_INIT_THREAD_COUNT')
            s.to_i
        end

        def subscribe(pattern, function)
            key = @prefix + pattern
            value = @callbacks.fetch(key, nil)
            if value.nil?
                @callbacks[key] = [function]
            else
                value.push(function)
            end
            send(Erlang.term_to_binary([:subscribe, pattern]))
        end

        def subscribe_count(pattern)
            send(Erlang.term_to_binary([:subscribe_count, pattern]))
            return poll_request(nil, false)
        end

        def unsubscribe(pattern)
            key = @prefix + pattern
            value = @callbacks.fetch(key, nil)
            API.assert{value != nil}
            value.shift
            if value.empty?
                @callbacks.delete(key)
            end
            send(Erlang.term_to_binary([:unsubscribe, pattern]))
        end

        def send_async(name, request,
                       timeout=nil, request_info=nil, priority=nil)
            if timeout.nil?
                timeout = @timeout_async
            end
            if request_info.nil?
                request_info = ''
            end
            if priority.nil?
                priority = @priority_default
            end
            send(Erlang.term_to_binary([:send_async, name,
                                        OtpErlangBinary.new(request_info),
                                        OtpErlangBinary.new(request),
                                        timeout, priority]))
            return poll_request(nil, false)
        end

        def send_sync(name, request,
                      timeout=nil, request_info=nil, priority=nil)
            if timeout.nil?
                timeout = @timeout_sync
            end
            if request_info.nil?
                request_info = ''
            end
            if priority.nil?
                priority = @priority_default
            end
            send(Erlang.term_to_binary([:send_sync, name,
                                        OtpErlangBinary.new(request_info),
                                        OtpErlangBinary.new(request),
                                        timeout, priority]))
            return poll_request(nil, false)
        end

        def mcast_async(name, request,
                        timeout=nil, request_info=nil, priority=nil)
            if timeout.nil?
                timeout = @timeout_async
            end
            if request_info.nil?
                request_info = ''
            end
            if priority.nil?
                priority = @priority_default
            end
            send(Erlang.term_to_binary([:mcast_async, name,
                                        OtpErlangBinary.new(request_info),
                                        OtpErlangBinary.new(request),
                                        timeout, priority]))
            return poll_request(nil, false)
        end

        def forward_(request_type, name, request_info, request,
                     timeout, priority, trans_id, pid)
            case request_type
            when ASYNC
                forward_async(name, request_info, request,
                              timeout, priority, trans_id, pid)
            when SYNC
                forward_sync(name, request_info, request,
                             timeout, priority, trans_id, pid)
            end
        end

        def forward_async(name, request_info, request,
                          timeout, priority, trans_id, pid)
            send(Erlang.term_to_binary([:forward_async, name,
                                        OtpErlangBinary.new(request_info),
                                        OtpErlangBinary.new(request),
                                        timeout, priority,
                                        OtpErlangBinary.new(trans_id), pid]))
            raise ForwardAsyncException
        end

        def forward_sync(name, request_info, request,
                         timeout, priority, trans_id, pid)
            send(Erlang.term_to_binary([:forward_sync, name,
                                        OtpErlangBinary.new(request_info),
                                        OtpErlangBinary.new(request),
                                        timeout, priority,
                                        OtpErlangBinary.new(trans_id), pid]))
            raise ForwardSyncException
        end

        def return_(request_type, name, pattern, response_info, response,
                    timeout, trans_id, pid)
            case request_type
            when ASYNC
                return_async(name, pattern, response_info, response,
                             timeout, trans_id, pid)
            when SYNC
                return_sync(name, pattern, response_info, response,
                            timeout, trans_id, pid)
            end
        end

        def return_async(name, pattern, response_info, response,
                         timeout, trans_id, pid)
            send(Erlang.term_to_binary([:return_async, name, pattern,
                                        OtpErlangBinary.new(response_info),
                                        OtpErlangBinary.new(response),
                                        timeout,
                                        OtpErlangBinary.new(trans_id), pid]))
            raise ReturnAsyncException
        end

        def return_sync(name, pattern, response_info, response,
                        timeout, trans_id, pid)
            send(Erlang.term_to_binary([:return_sync, name, pattern,
                                        OtpErlangBinary.new(response_info),
                                        OtpErlangBinary.new(response),
                                        timeout,
                                        OtpErlangBinary.new(trans_id), pid]))
            raise ReturnSyncException
        end

        def recv_async(timeout=nil, trans_id=nil, consume=true)
            if timeout.nil?
                timeout = @timeout_sync
            end
            if trans_id.nil?
                trans_id = 0.chr * 16
            end
            send(Erlang.term_to_binary([:recv_async, timeout,
                                        OtpErlangBinary.new(trans_id),
                                        consume]))
            return poll_request(nil, false)
        end

        def null_response(request_type, name, pattern, request_info, request,
                          timeout, priority, trans_id, pid)
            return ''
        end

        def callback(command, name, pattern, request_info, request,
                     timeout, priority, trans_id, pid)
            function_queue = @callbacks.fetch(pattern, nil)
            if function_queue.nil?
                function = method(:null_response)
            else
                function = function_queue.shift
                function_queue.push(function)
            end
            return_null_response = false
            case command
            when MESSAGE_SEND_ASYNC
                begin
                    response = function.call(ASYNC, name, pattern,
                                             request_info, request,
                                             timeout, priority, trans_id, pid)
                    if response.kind_of?(Array)
                        API.assert{response.length == 2}
                        response_info = response[0]
                        response = response[1]
                        if not response_info.kind_of?(String)
                            response_info = ''
                        end
                    else
                        response_info = ''
                    end
                    if not response.kind_of?(String)
                        response = ''
                    end
                rescue MessageDecodingException => e
                    @terminate = true
                    return_null_response = true
                rescue TerminateException => e
                    return_null_response = true
                rescue ReturnAsyncException
                    return
                rescue ReturnSyncException => e
                    @terminate = true
                    $stderr.puts e.message
                    $stderr.puts e.backtrace
                    return
                rescue ForwardAsyncException
                    return
                rescue ForwardSyncException => e
                    @terminate = true
                    $stderr.puts e.message
                    $stderr.puts e.backtrace
                    return
                rescue StandardError => e
                    return_null_response = true
                    $stderr.puts e.message
                    $stderr.puts e.backtrace
                rescue SystemExit => e
                    $stderr.puts e.message
                    $stderr.puts e.backtrace
                    raise
                rescue
                    $stderr.puts $!.message
                    $stderr.puts $!.backtrace
                    exit(1)
                end
                if return_null_response
                    response_info = ''
                    response = ''
                end
                begin
                    return_async(name, pattern, response_info, response,
                                 timeout, trans_id, pid)
                rescue ReturnAsyncException
                end
            when MESSAGE_SEND_SYNC
                begin
                    response = function.call(SYNC, name, pattern,
                                             request_info, request,
                                             timeout, priority, trans_id, pid)
                    if response.kind_of?(Array)
                        API.assert{response.length == 2}
                        response_info = response[0]
                        response = response[1]
                        if not response_info.kind_of?(String)
                            response_info = ''
                        end
                    else
                        response_info = ''
                    end
                    if not response.kind_of?(String)
                        response = ''
                    end
                rescue MessageDecodingException => e
                    @terminate = true
                    return_null_response = true
                rescue TerminateException => e
                    return_null_response = true
                rescue ReturnSyncException
                    return
                rescue ReturnAsyncException => e
                    @terminate = true
                    $stderr.puts e.message
                    $stderr.puts e.backtrace
                    return
                rescue ForwardSyncException
                    return
                rescue ForwardAsyncException => e
                    @terminate = true
                    $stderr.puts e.message
                    $stderr.puts e.backtrace
                    return
                rescue StandardError => e
                    return_null_response = true
                    $stderr.puts e.message
                    $stderr.puts e.backtrace
                rescue SystemExit => e
                    $stderr.puts e.message
                    $stderr.puts e.backtrace
                    raise
                rescue
                    $stderr.puts $!.message
                    $stderr.puts $!.backtrace
                    exit(1)
                end
                if return_null_response
                    response_info = ''
                    response = ''
                end
                begin
                    return_sync(name, pattern, response_info, response,
                                timeout, trans_id, pid)
                rescue ReturnSyncException
                end
            else
                raise MessageDecodingException
            end
        end

        def handle_events(external, data, data_size, i, command=nil)
            if command.nil?
                if i > data_size
                    raise MessageDecodingException
                end
                j = 4
                command = data[i, j].unpack('L')[0]
            else
                j = 4
            end
            loop do
                case command
                when MESSAGE_TERM
                    @terminate = true
                    if external
                        return false
                    else
                        raise TerminateException.new(@timeout_terminate)
                    end
                when MESSAGE_REINIT
                    i += j; j = 4 + 4 + 4 + 1
                    tmp = data[i, j].unpack("LLLc")
                    @process_count = tmp[0]
                    @timeout_async = tmp[1]
                    @timeout_sync = tmp[2]
                    @priority_default = tmp[3]
                    i += j
                when MESSAGE_KEEPALIVE
                    send(Erlang.term_to_binary(:keepalive))
                    i += j
                else
                    raise MessageDecodingException
                end
                if i > data_size
                    raise MessageDecodingException
                elsif i == data_size
                    return true
                end
                j = 4
                command = data[i, j].unpack('L')[0]
            end
        end

        def poll_request(timeout, external)
            if @terminate
                if external
                    return false
                else
                    raise TerminateException.new(@timeout_terminate)
                end
            elsif external and not @initialization_complete
                send(Erlang.term_to_binary(:polling))
                @initialization_complete = true
            end

            poll_timer = nil
            if timeout.nil? or timeout < 0
                timeout_value = nil
            elsif timeout == 0
                timeout_value = 0
            elsif timeout > 0
                poll_timer = Time.now
                timeout_value = timeout * 0.001
            end
            result = IO.select([@s], nil, [@s], timeout_value)
            if result.nil?
                return true
            end
            if result[2].length > 0
                return false
            end

            data = recv('')
            data_size = data.bytesize
            if data_size == 0
                return false
            end
            i = 0; j = 4

            loop do
                command = data[i, j].unpack('L')[0]
                case command
                when MESSAGE_INIT
                    i += j; j = 4 + 4 + 4 + 4 + 4
                    tmp = data[i, j].unpack('LLLLL')
                    @process_index = tmp[0]
                    @process_count = tmp[1]
                    @process_count_max = tmp[2]
                    @process_count_min = tmp[3]
                    prefix_size = tmp[4]
                    i += j; j = prefix_size + 4 + 4 + 4 + 4 + 1
                    tmp = data[i, j].unpack("Z#{prefix_size}LLLLc")
                    @prefix = tmp[0]
                    @timeout_initialize = tmp[1]
                    @timeout_async = tmp[2]
                    @timeout_sync = tmp[3]
                    @timeout_terminate = tmp[4]
                    @priority_default = tmp[5]
                    i += j
                    if i != data_size
                        API.assert{external == false}
                        handle_events(external, data, data_size, i)
                    end
                    return
                when MESSAGE_SEND_ASYNC, MESSAGE_SEND_SYNC
                    i += j; j = 4
                    name_size = data[i, j].unpack('L')[0]
                    i += j; j = name_size + 4
                    tmp = data[i, j].unpack("Z#{name_size}L")
                    name = tmp[0]
                    pattern_size = tmp[1]
                    i += j; j = pattern_size + 4
                    tmp = data[i, j].unpack("Z#{pattern_size}L")
                    pattern = tmp[0]
                    request_info_size = tmp[1]
                    i += j; j = request_info_size + 1 + 4
                    tmp = data[i, j].unpack("a#{request_info_size}xL")
                    request_info = tmp[0]
                    request_size = tmp[1]
                    i += j; j = request_size + 1 + 4 + 1 + 16 + 4
                    tmp = data[i, j].unpack("a#{request_size}xLca16L")
                    request = tmp[0]
                    request_timeout = tmp[1]
                    priority = tmp[2]
                    trans_id = tmp[3]
                    pid_size = tmp[4]
                    i += j; j = pid_size
                    pid = data[i, j]
                    i += j
                    if i != data_size
                        API.assert{external == true}
                        if not handle_events(external, data, data_size, i)
                            return false
                        end
                    end
                    data.clear()
                    callback(command, name, pattern, request_info, request,
                             request_timeout, priority, trans_id,
                             Erlang.binary_to_term(pid))
                    if @terminate
                        return false
                    end
                when MESSAGE_RECV_ASYNC, MESSAGE_RETURN_SYNC
                    i += j; j = 4
                    response_info_size = data[i, j].unpack('L')[0]
                    i += j; j = response_info_size + 1 + 4
                    tmp = data[i, j].unpack("a#{response_info_size}xL")
                    response_info = tmp[0]
                    response_size = tmp[1]
                    i += j; j = response_size + 1 + 16
                    tmp = data[i, j].unpack("a#{response_size}xa16")
                    response = tmp[0]
                    trans_id = tmp[1]
                    i += j
                    if i != data_size
                        API.assert{external == false}
                        handle_events(external, data, data_size, i)
                    end
                    return [response_info, response, trans_id]
                when MESSAGE_RETURN_ASYNC
                    i += j; j = 16
                    trans_id = data[i, j]
                    i += j
                    if i != data_size
                        API.assert{external == false}
                        handle_events(external, data, data_size, i)
                    end
                    return trans_id
                when MESSAGE_RETURNS_ASYNC
                    i += j; j = 4
                    trans_id_count = data[i, j].unpack('L')[0]
                    i += j; j = 16 * trans_id_count
                    trans_ids = data[i, j].unpack('a16' * trans_id_count)
                    i += j
                    if i != data_size
                        API.assert{external == false}
                        handle_events(external, data, data_size, i)
                    end
                    return trans_ids
                when MESSAGE_SUBSCRIBE_COUNT
                    i += j; j = 4
                    count = data[i, j].unpack('L')[0]
                    i += j
                    if i != data_size
                        API.assert{external == false}
                        handle_events(external, data, data_size, i)
                    end
                    return count
                when MESSAGE_TERM
                    if not handle_events(external, data, data_size, i, command)
                        return false
                    end
                    API.assert{false}
                when MESSAGE_REINIT
                    i += j; j = 4 + 4 + 4 + 1
                    tmp = data[i, j].unpack("LLLc")
                    @process_count = tmp[0]
                    @timeout_async = tmp[1]
                    @timeout_sync = tmp[2]
                    @priority_default = tmp[3]
                    i += j; j = 4
                    if i == data_size
                        data.clear()
                    elsif i < data_size
                        next
                    else
                        raise MessageDecodingException
                    end
                when MESSAGE_KEEPALIVE
                    send(Erlang.term_to_binary(:keepalive))
                    i += j; j = 4
                    if i == data_size
                        data.clear()
                    elsif i < data_size
                        next
                    else
                        raise MessageDecodingException
                    end
                else
                    raise MessageDecodingException
                end

                if not poll_timer.nil?
                    poll_timer_new = Time.now
                    elapsed = [0,
                               ((poll_timer_new -
                                 poll_timer) * 1000.0).floor].max
                    poll_timer = poll_timer_new
                    if elapsed >= timeout
                        timeout = 0
                    else
                        timeout -= elapsed
                    end
                end
                if not timeout_value.nil?
                    if timeout == 0
                        return true
                    elsif timeout > 0
                        timeout_value = timeout * 0.001
                    end
                end
                result = IO.select([@s], nil, [@s], timeout_value)
                if result.nil?
                    return true
                end
                if result[2].length > 0
                    return false
                end

                data = recv(data)
                data_size = data.bytesize
                if data_size == 0
                    return false
                end
                i = 0; j = 4
            end
        end

        def poll(timeout=nil)
            if timeout.nil?
                timeout = -1
            end
            return poll_request(timeout, true)
        end

        def shutdown(reason=nil)
            if reason.nil?
                reason = ''
            end
            send(Erlang.term_to_binary([:shutdown, reason]))
        end

        def self.text_pairs_parse(text)
            pairs = {}
            text_segments = text.split(NULL.chr)
            (0...(text_segments.length)).step(2).each do |i|
                value = pairs[text_segments[i]]
                if value == nil
                    pairs[text_segments[i]] = text_segments[i + 1]
                elsif value.kind_of?(Array)
                    value << text_segments[i + 1]
                else
                    pairs[text_segments[i]] = [value, text_segments[i + 1]]
                end
            end
            return pairs
        end

        def self.text_pairs_new(pairs, response)
            text_stream = StringIO.new
            pairs.each do |key, values|
                if values.kind_of?(String)
                    text_stream.write "#{key}\0#{values}\0"
                else
                    values.each do |value|
                        text_stream.write "#{key}\0#{value}\0"
                    end
                end
            end
            text = text_stream.string
            if response and text == ''
                return "\0"
            else
                return text
            end
        end

        def self.info_key_value_parse(info)
            return text_pairs_parse(info)
        end

        def self.info_key_value_new(pairs, response = true)
            return text_pairs_new(pairs, response)
        end

        def self.assert
            raise AssertionError unless yield # if $DEBUG
        end

        private :null_response
        private :callback
        private :handle_events
        private :poll_request
        private_class_method :text_pairs_parse
        private_class_method :text_pairs_new
        private

        def send(data)
            if @use_header
                data = [data.length].pack('N') + data
            end
            @s.write(data)
        end

        def recv(data_old)
            data = ''
            if @use_header
                i = 0
                while i < 4
                    fragment = @s.readpartial(4 - i)
                    data += fragment
                    i += fragment.length
                end
                total = data.unpack('N')[0]
                data = data_old
                i = 0
                while i < total
                    fragment = @s.readpartial([total - i, @size].min)
                    data += fragment
                    i += fragment.length
                end
            else
                data = data_old
                ready = true
                while ready == true
                    fragment = @s.readpartial(@size)
                    data += fragment
                    ready = (fragment.bytesize == @size)

                    if ready
                        ready = ! IO.select([@s], nil, nil, 0).nil?
                    end
                end
            end
            return data
        end

        MESSAGE_INIT                = 1
        MESSAGE_SEND_ASYNC          = 2
        MESSAGE_SEND_SYNC           = 3
        MESSAGE_RECV_ASYNC          = 4
        MESSAGE_RETURN_ASYNC        = 5
        MESSAGE_RETURN_SYNC         = 6
        MESSAGE_RETURNS_ASYNC       = 7
        MESSAGE_KEEPALIVE           = 8
        MESSAGE_REINIT              = 9
        MESSAGE_SUBSCRIBE_COUNT     = 10
        MESSAGE_TERM                = 11

        NULL = 0

        def self.getenv(key)
            ENV[key] or raise InvalidInputException
        end
    end

    class InvalidInputException < Exception
    end

    class ReturnSyncException < Exception
    end

    class ReturnAsyncException < Exception
    end

    class ForwardSyncException < Exception
    end

    class ForwardAsyncException < Exception
    end

    class MessageDecodingException < Exception
    end

    class TerminateException < Exception
        def initialize(timeout)
            @timeout = timeout
        end
        attr_reader :timeout
    end

    class AssertionError < Exception
    end

    class FatalError < Exception
    end
end

