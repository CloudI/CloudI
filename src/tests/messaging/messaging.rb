#!/usr/bin/env ruby
#-*-Mode:ruby;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
# ex: set ft=ruby fenc=utf-8 sts=4 ts=4 sw=4 et:
#
# BSD LICENSE
# 
# Copyright (c) 2012-2014, Michael Truog <mjtruog at gmail dot com>
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
                begin
                    @api.subscribe('a/b/c/d', method(:sequence1_abcd))
                    @api.subscribe('a/b/c/*', method(:sequence1_abc_))
                    @api.subscribe('a/b/*/d', method(:sequence1_ab_d))
                    @api.subscribe('a/*/c/d', method(:sequence1_a_cd))
                    @api.subscribe('*/b/c/d', method(:sequence1__bcd))
                    @api.subscribe('a/b/*',   method(:sequence1_ab__))
                    @api.subscribe('a/*/d',   method(:sequence1_a__d))
                    @api.subscribe('*/c/d',   method(:sequence1___cd))
                    @api.subscribe('a/*',     method(:sequence1_a___))
                    @api.subscribe('*/d',     method(:sequence1____d))
                    @api.subscribe('*',       method(:sequence1_____))
                    @api.subscribe('sequence1', method(:sequence1))
                    @api.subscribe('e', method(:sequence2_e1))
                    @api.subscribe('e', method(:sequence2_e2))
                    @api.subscribe('e', method(:sequence2_e3))
                    @api.subscribe('e', method(:sequence2_e4))
                    @api.subscribe('e', method(:sequence2_e5))
                    @api.subscribe('e', method(:sequence2_e6))
                    @api.subscribe('e', method(:sequence2_e7))
                    @api.subscribe('e', method(:sequence2_e8))
                    @api.subscribe('sequence2', method(:sequence2))
                    @api.subscribe('f1', method(:sequence3_f1))
                    @api.subscribe('f2', method(:sequence3_f2))
                    @api.subscribe('g1', method(:sequence3_g1))
                    @api.subscribe('sequence3', method(:sequence3))
                    if @thread_index == 0
                        # start sequence1
                        @api.send_async(@api.prefix + 'sequence1', 'start')
                    end
    
                    result = @api.poll
                    assert{result == nil}
                rescue CloudI::TerminateException
                    #
                rescue
                    $stderr.puts $!.message
                    $stderr.puts $!.backtrace
                end
                $stdout.puts 'terminate messaging ruby'
            end

            private

            def assert(&test)
                CloudI::API.assert(&test)
            end

            def sequence1_abcd(command, name, pattern, request_info, request,
                               timeout, priority, trans_id, pid)
                assert{pattern == "#{@api.prefix}a/b/c/d"}
                assert{request == 'test1'}
                @api.return_(command, name, pattern,
                             '', request, timeout, trans_id, pid)
            end

            def sequence1_abc_(command, name, pattern, request_info, request,
                               timeout, priority, trans_id, pid)
                assert{pattern == "#{@api.prefix}a/b/c/*"}
                assert{request == 'test2' or request == 'test3'}
                @api.return_(command, name, pattern,
                             '', request, timeout, trans_id, pid)
            end

            def sequence1_ab_d(command, name, pattern, request_info, request,
                               timeout, priority, trans_id, pid)
                assert{pattern == "#{@api.prefix}a/b/*/d"}
                assert{request == 'test4' or request == 'test5'}
                @api.return_(command, name, pattern,
                             '', request, timeout, trans_id, pid)
            end

            def sequence1_a_cd(command, name, pattern, request_info, request,
                               timeout, priority, trans_id, pid)
                assert{pattern == "#{@api.prefix}a/*/c/d"}
                assert{request == 'test6' or request == 'test7'}
                @api.return_(command, name, pattern,
                             '', request, timeout, trans_id, pid)
            end

            def sequence1__bcd(command, name, pattern, request_info, request,
                               timeout, priority, trans_id, pid)
                assert{pattern == "#{@api.prefix}*/b/c/d"}
                assert{request == 'test8' or request == 'test9'}
                @api.return_(command, name, pattern,
                             '', request, timeout, trans_id, pid)
            end

            def sequence1_ab__(command, name, pattern, request_info, request,
                               timeout, priority, trans_id, pid)
                assert{pattern == "#{@api.prefix}a/b/*"}
                assert{request == 'test10'}
                @api.return_(command, name, pattern,
                             '', request, timeout, trans_id, pid)
            end

            def sequence1_a__d(command, name, pattern, request_info, request,
                               timeout, priority, trans_id, pid)
                assert{pattern == "#{@api.prefix}a/*/d"}
                assert{request == 'test11'}
                @api.return_(command, name, pattern,
                             '', request, timeout, trans_id, pid)
            end

            def sequence1___cd(command, name, pattern, request_info, request,
                               timeout, priority, trans_id, pid)
                assert{pattern == "#{@api.prefix}*/c/d"}
                assert{request == 'test12'}
                @api.return_(command, name, pattern,
                             '', request, timeout, trans_id, pid)
            end

            def sequence1_a___(command, name, pattern, request_info, request,
                               timeout, priority, trans_id, pid)
                assert{pattern == "#{@api.prefix}a/*"}
                assert{request == 'test13'}
                @api.return_(command, name, pattern,
                             '', request, timeout, trans_id, pid)
            end

            def sequence1____d(command, name, pattern, request_info, request,
                               timeout, priority, trans_id, pid)
                assert{pattern == "#{@api.prefix}*/d"}
                assert{request == 'test14'}
                @api.return_(command, name, pattern,
                             '', request, timeout, trans_id, pid)
            end

            def sequence1_____(command, name, pattern, request_info, request,
                               timeout, priority, trans_id, pid)
                assert{pattern == "#{@api.prefix}*"}
                assert{request == 'test15'}
                @api.return_(command, name, pattern,
                             '', request, timeout, trans_id, pid)
            end

            def sequence1(command, name, pattern, request_info, request,
                          timeout, priority, trans_id, pid)
                while @api.recv_async(1000)[1] == 'end'
                    # consume "end" and sleep
                end
                $stdout.puts 'messaging sequence1 start ruby'
                assert{request == 'start'}
                # n.b., depends on cloudi_constants.hrl having
                # SERVICE_NAME_PATTERN_MATCHING defined
                test1_id = @api.send_async("#{@api.prefix}a/b/c/d", 'test1')
                test2_id = @api.send_async("#{@api.prefix}a/b/c/z", 'test2')
                test3_id = @api.send_async("#{@api.prefix}a/b/c/dd", 'test3')
                test4_id = @api.send_async("#{@api.prefix}a/b/z/d", 'test4')
                test5_id = @api.send_async("#{@api.prefix}a/b/cc/d", 'test5')
                test6_id = @api.send_async("#{@api.prefix}a/z/c/d", 'test6')
                test7_id = @api.send_async("#{@api.prefix}a/bb/c/d", 'test7')
                test8_id = @api.send_async("#{@api.prefix}z/b/c/d", 'test8')
                test9_id = @api.send_async("#{@api.prefix}aa/b/c/d", 'test9')
                test10_id = @api.send_async("#{@api.prefix}a/b/czd", 'test10')
                test11_id = @api.send_async("#{@api.prefix}a/bzc/d", 'test11')
                test12_id = @api.send_async("#{@api.prefix}azb/c/d", 'test12')
                test13_id = @api.send_async("#{@api.prefix}a/bzczd", 'test13')
                test14_id = @api.send_async("#{@api.prefix}azbzc/d", 'test14')
                test15_id = @api.send_async("#{@api.prefix}azbzczd", 'test15')
                # n.b., depends on cloudi_constants.hrl having
                # RECV_ASYNC_STRATEGY == recv_async_select_oldest
                @api.recv_async(nil, test1_id, false)
                test1 = @api.recv_async()
                test1_check = test1[1]
                test1_id_check = test1[2]
                assert{test1_check == 'test1'}
                assert{test1_id_check == test1_id}
                @api.recv_async(nil, test2_id, false)
                test2 = @api.recv_async()
                test2_check = test2[1]
                test2_id_check = test2[2]
                assert{test2_check == 'test2'}
                assert{test2_id_check == test2_id}
                @api.recv_async(nil, test3_id, false)
                test3 = @api.recv_async()
                test3_check = test3[1]
                test3_id_check = test3[2]
                assert{test3_check == 'test3'}
                assert{test3_id_check == test3_id}
                @api.recv_async(nil, test4_id, false)
                test4 = @api.recv_async()
                test4_check = test4[1]
                test4_id_check = test4[2]
                assert{test4_check == 'test4'}
                assert{test4_id_check == test4_id}
                @api.recv_async(nil, test5_id, false)
                test5 = @api.recv_async()
                test5_check = test5[1]
                test5_id_check = test5[2]
                assert{test5_check == 'test5'}
                assert{test5_id_check == test5_id}
                @api.recv_async(nil, test6_id, false)
                test6 = @api.recv_async()
                test6_check = test6[1]
                test6_id_check = test6[2]
                assert{test6_check == 'test6'}
                assert{test6_id_check == test6_id}
                @api.recv_async(nil, test7_id, false)
                test7 = @api.recv_async()
                test7_check = test7[1]
                test7_id_check = test7[2]
                assert{test7_check == 'test7'}
                assert{test7_id_check == test7_id}
                @api.recv_async(nil, test8_id, false)
                test8 = @api.recv_async()
                test8_check = test8[1]
                test8_id_check = test8[2]
                assert{test8_check == 'test8'}
                assert{test8_id_check == test8_id}
                @api.recv_async(nil, test9_id, false)
                test9 = @api.recv_async()
                test9_check = test9[1]
                test9_id_check = test9[2]
                assert{test9_check == 'test9'}
                assert{test9_id_check == test9_id}
                @api.recv_async(nil, test10_id, false)
                test10 = @api.recv_async()
                test10_check = test10[1]
                test10_id_check = test10[2]
                assert{test10_check == 'test10'}
                assert{test10_id_check == test10_id}
                @api.recv_async(nil, test11_id, false)
                test11 = @api.recv_async()
                test11_check = test11[1]
                test11_id_check = test11[2]
                assert{test11_check == 'test11'}
                assert{test11_id_check == test11_id}
                @api.recv_async(nil, test12_id, false)
                test12 = @api.recv_async()
                test12_check = test12[1]
                test12_id_check = test12[2]
                assert{test12_check == 'test12'}
                assert{test12_id_check == test12_id}
                @api.recv_async(nil, test13_id, false)
                test13 = @api.recv_async()
                test13_check = test13[1]
                test13_id_check = test13[2]
                assert{test13_check == 'test13'}
                assert{test13_id_check == test13_id}
                @api.recv_async(nil, test14_id, false)
                test14 = @api.recv_async()
                test14_check = test14[1]
                test14_id_check = test14[2]
                assert{test14_check == 'test14'}
                assert{test14_id_check == test14_id}
                @api.recv_async(nil, test15_id, false)
                test15 = @api.recv_async()
                test15_check = test15[1]
                test15_id_check = test15[2]
                assert{test15_check == 'test15'}
                assert{test15_id_check == test15_id}
                $stdout.puts 'messaging sequence1 end ruby'
                # start sequence2
                @api.send_async("#{@api.prefix}sequence2", 'start')
                @api.return_(command, name, pattern,
                             '', 'end', timeout, trans_id, pid)
            end

            def sequence2_e1(command, name, pattern, request_info, request,
                             timeout, priority, trans_id, pid)
                @api.return_(command, name, pattern,
                             '', '1', timeout, trans_id, pid)
            end

            def sequence2_e2(command, name, pattern, request_info, request,
                             timeout, priority, trans_id, pid)
                @api.return_(command, name, pattern,
                             '', '2', timeout, trans_id, pid)
            end

            def sequence2_e3(command, name, pattern, request_info, request,
                             timeout, priority, trans_id, pid)
                @api.return_(command, name, pattern,
                             '', '3', timeout, trans_id, pid)
            end

            def sequence2_e4(command, name, pattern, request_info, request,
                             timeout, priority, trans_id, pid)
                @api.return_(command, name, pattern,
                             '', '4', timeout, trans_id, pid)
            end

            def sequence2_e5(command, name, pattern, request_info, request,
                             timeout, priority, trans_id, pid)
                @api.return_(command, name, pattern,
                             '', '5', timeout, trans_id, pid)
            end

            def sequence2_e6(command, name, pattern, request_info, request,
                             timeout, priority, trans_id, pid)
                @api.return_(command, name, pattern,
                             '', '6', timeout, trans_id, pid)
            end

            def sequence2_e7(command, name, pattern, request_info, request,
                             timeout, priority, trans_id, pid)
                @api.return_(command, name, pattern,
                             '', '7', timeout, trans_id, pid)
            end

            def sequence2_e8(command, name, pattern, request_info, request,
                             timeout, priority, trans_id, pid)
                @api.return_(command, name, pattern,
                             '', '8', timeout, trans_id, pid)
            end

            def sequence2(command, name, pattern, request_info, request,
                          timeout, priority, trans_id, pid)
                $stdout.puts 'messaging sequence2 start ruby'
                assert{request == 'start'}
                # the sending process is excluded from the services that
                # receive the asynchronous message, so in this case, the
                # receiving thread will not be called, despite the fact it
                # has subscribed to 'e', to prevent a process (in this case
                # thread) from deadlocking with itself.
                e_ids = @api.mcast_async("#{@api.prefix}e", ' ')
                # 4 * 8 == 32, but only 3 out of 4 threads can receive messages,
                # since 1 thread is sending the mcast_async, so 3 * 8 == 24
                assert{e_ids.length == 24}
                e_check_list = []
                e_ids.each{ |e_id|
                    tmp = @api.recv_async(nil, e_id)
                    e_check = tmp[1]
                    e_id_check = tmp[2]
                    assert{e_id == e_id_check}
                    e_check_list.push(e_check)
                }
                e_check_list.sort!
                assert{e_check_list.join('') == '111222333444555666777888'}
                $stdout.puts 'messaging sequence2 end ruby'
                # start sequence3
                @api.send_async("#{@api.prefix}sequence3", 'start')
                @api.return_(command, name, pattern,
                             '', 'end', timeout, trans_id, pid)
            end

            def sequence3_f1(command, name, pattern, request_info, request,
                             timeout, priority, trans_id, pid)
                request_i = request.to_i
                if request_i == 4
                    return 'done'
                end
                request_new = request_i + 2 # two steps forward
                @api.forward_(command, "#{@api.prefix}f2",
                              '', request_new.to_s,
                              timeout, priority, trans_id, pid)
            end

            def sequence3_f2(command, name, pattern, request_info, request,
                             timeout, priority, trans_id, pid)
                request_i = request.to_i
                request_new = request_i - 1 # one step back
                @api.forward_(command, "#{@api.prefix}f1",
                              '', request_new.to_s,
                              timeout, priority, trans_id, pid)
            end

            def sequence3_g1(command, name, pattern, request_info, request,
                             timeout, priority, trans_id, pid)
                @api.return_(command, name, pattern,
                             '', request + 'suffix', timeout, trans_id, pid)
            end

            def sequence3(command, name, pattern, request_info, request,
                          timeout, priority, trans_id, pid)
                $stdout.puts 'messaging sequence3 start ruby'
                assert{request == 'start'}
                test1_id = @api.send_async("#{@api.prefix}f1", '0')
                tmp = @api.recv_async(nil, test1_id)
                test1_check = tmp[1]
                test1_id_check = tmp[2]
                assert{test1_id_check == test1_id}
                assert{test1_check == 'done'}
                tmp = @api.send_sync("#{@api.prefix}g1", 'prefix_')
                test2_check = tmp[1]
                assert{test2_check == 'prefix_suffix'}
                $stdout.puts 'messaging sequence3 end ruby'
                # loop to find any infrequent problems, restart sequence1
                @api.send_async(@api.prefix + 'sequence1', 'start')
                @api.return_(command, name, pattern,
                             '', 'end', timeout, trans_id, pid)
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

