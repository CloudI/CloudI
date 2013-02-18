// -*- coding: utf-8; Mode: java; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
// ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
//
// BSD LICENSE
// 
// Copyright (c) 2012-2013, Michael Truog <mjtruog at gmail dot com>
// All rights reserved.
// 
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
// 
//     * Redistributions of source code must retain the above copyright
//       notice, this list of conditions and the following disclaimer.
//     * Redistributions in binary form must reproduce the above copyright
//       notice, this list of conditions and the following disclaimer in
//       the documentation and/or other materials provided with the
//       distribution.
//     * All advertising materials mentioning features or use of this
//       software must display the following acknowledgment:
//         This product includes software developed by Michael Truog
//     * The name of the author may not be used to endorse or promote
//       products derived from this software without specific prior
//       written permission
// 
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
// CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
// INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
// OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
// CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
// SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
// BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
// INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
// WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
// NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
// DAMAGE.
//

package org.cloudi.tests.messaging;

import java.util.List;
import java.util.Iterator;
import java.util.Arrays;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import com.ericsson.otp.erlang.OtpErlangPid;
import org.cloudi.API;

public class Task implements Runnable
{
    private API api;
    private int thread_index;
     
    public Task(final int thread_index)
                throws API.InvalidInputException,
                       API.MessageDecodingException
    {
        this.api = new API(thread_index);
        this.thread_index = thread_index;
    }

    public void sequence1_abcd(Integer command, String name, String pattern,
                               byte[] request_info, byte[] request,
                               Integer timeout, Byte priority,
                               byte[] trans_id, OtpErlangPid pid)
                               throws API.ReturnAsyncException,
                                      API.ReturnSyncException,
                                      API.InvalidInputException
    {
        assert pattern == (this.api.prefix() + "a/b/c/d");
        assert new String(request) == "test1";
        this.api.return_(command, name, pattern,
                         ("").getBytes(), request,
                         timeout, trans_id, pid);
    }
 
    public void sequence1_abc_(Integer command, String name, String pattern,
                               byte[] request_info, byte[] request,
                               Integer timeout, Byte priority,
                               byte[] trans_id, OtpErlangPid pid)
                               throws API.ReturnAsyncException,
                                      API.ReturnSyncException,
                                      API.InvalidInputException
    {
        assert pattern == (this.api.prefix() + "a/b/c/*");
        assert (new String(request) == "test2") ||
               (new String(request) == "test3");
        this.api.return_(command, name, pattern,
                         ("").getBytes(), request,
                         timeout, trans_id, pid);
    }
 
    public void sequence1_ab_d(Integer command, String name, String pattern,
                               byte[] request_info, byte[] request,
                               Integer timeout, Byte priority,
                               byte[] trans_id, OtpErlangPid pid)
                               throws API.ReturnAsyncException,
                                      API.ReturnSyncException,
                                      API.InvalidInputException
    {
        assert pattern == (this.api.prefix() + "a/b/*/d");
        assert (new String(request) == "test4") ||
               (new String(request) == "test5");
        this.api.return_(command, name, pattern,
                         ("").getBytes(), request,
                         timeout, trans_id, pid);
    }
 
    public void sequence1_a_cd(Integer command, String name, String pattern,
                               byte[] request_info, byte[] request,
                               Integer timeout, Byte priority,
                               byte[] trans_id, OtpErlangPid pid)
                               throws API.ReturnAsyncException,
                                      API.ReturnSyncException,
                                      API.InvalidInputException
    {
        assert pattern == (this.api.prefix() + "a/*/c/d");
        assert (new String(request) == "test6") ||
               (new String(request) == "test7");
        this.api.return_(command, name, pattern,
                         ("").getBytes(), request,
                         timeout, trans_id, pid);
    }
 
    public void sequence1__bcd(Integer command, String name, String pattern,
                               byte[] request_info, byte[] request,
                               Integer timeout, Byte priority,
                               byte[] trans_id, OtpErlangPid pid)
                               throws API.ReturnAsyncException,
                                      API.ReturnSyncException,
                                      API.InvalidInputException
    {
        assert pattern == (this.api.prefix() + "*/b/c/d");
        assert (new String(request) == "test8") ||
               (new String(request) == "test9");
        this.api.return_(command, name, pattern,
                         ("").getBytes(), request,
                         timeout, trans_id, pid);
    }
 
    public void sequence1_ab__(Integer command, String name, String pattern,
                               byte[] request_info, byte[] request,
                               Integer timeout, Byte priority,
                               byte[] trans_id, OtpErlangPid pid)
                               throws API.ReturnAsyncException,
                                      API.ReturnSyncException,
                                      API.InvalidInputException
    {
        assert pattern == (this.api.prefix() + "a/b/*");
        assert new String(request) == "test10";
        this.api.return_(command, name, pattern,
                         ("").getBytes(), request,
                         timeout, trans_id, pid);
    }
 
    public void sequence1_a__d(Integer command, String name, String pattern,
                               byte[] request_info, byte[] request,
                               Integer timeout, Byte priority,
                               byte[] trans_id, OtpErlangPid pid)
                               throws API.ReturnAsyncException,
                                      API.ReturnSyncException,
                                      API.InvalidInputException
    {
        assert pattern == (this.api.prefix() + "a/*/d");
        assert new String(request) == "test11";
        this.api.return_(command, name, pattern,
                         ("").getBytes(), request,
                         timeout, trans_id, pid);
    }
 
    public void sequence1___cd(Integer command, String name, String pattern,
                               byte[] request_info, byte[] request,
                               Integer timeout, Byte priority,
                               byte[] trans_id, OtpErlangPid pid)
                               throws API.ReturnAsyncException,
                                      API.ReturnSyncException,
                                      API.InvalidInputException
    {
        assert pattern == (this.api.prefix() + "*/c/d");
        assert new String(request) == "test12";
        this.api.return_(command, name, pattern,
                         ("").getBytes(), request,
                         timeout, trans_id, pid);
    }
 
    public void sequence1_a___(Integer command, String name, String pattern,
                               byte[] request_info, byte[] request,
                               Integer timeout, Byte priority,
                               byte[] trans_id, OtpErlangPid pid)
                               throws API.ReturnAsyncException,
                                      API.ReturnSyncException,
                                      API.InvalidInputException
    {
        assert pattern == (this.api.prefix() + "a/*");
        assert new String(request) == "test13";
        this.api.return_(command, name, pattern,
                         ("").getBytes(), request,
                         timeout, trans_id, pid);
    }
 
    public void sequence1____d(Integer command, String name, String pattern,
                               byte[] request_info, byte[] request,
                               Integer timeout, Byte priority,
                               byte[] trans_id, OtpErlangPid pid)
                               throws API.ReturnAsyncException,
                                      API.ReturnSyncException,
                                      API.InvalidInputException
    {
        assert pattern == (this.api.prefix() + "*/d");
        assert new String(request) == "test14";
        this.api.return_(command, name, pattern,
                         ("").getBytes(), request,
                         timeout, trans_id, pid);
    }
 
    public void sequence1_____(Integer command, String name, String pattern,
                               byte[] request_info, byte[] request,
                               Integer timeout, Byte priority,
                               byte[] trans_id, OtpErlangPid pid)
                               throws API.ReturnAsyncException,
                                      API.ReturnSyncException,
                                      API.InvalidInputException
    {
        assert pattern == (this.api.prefix() + "*");
        assert new String(request) == "test15";
        this.api.return_(command, name, pattern,
                         ("").getBytes(), request,
                         timeout, trans_id, pid);
    }
 
    public void sequence1(Integer command, String name, String pattern,
                          byte[] request_info, byte[] request,
                          Integer timeout, Byte priority,
                          byte[] trans_id, OtpErlangPid pid)
                          throws API.ReturnAsyncException,
                                 API.ReturnSyncException,
                                 API.InvalidInputException,
                                 API.MessageDecodingException
    {
        API.Response end_check = this.api.recv_async(1000);
        while (new String(end_check.response) == "end")
        {
            // consume "end" and sleep
            end_check = this.api.recv_async(1000);
        }
        API.out.println("messaging sequence1 start java");
        assert new String(request) == "start";
        // n.b., depends on cloudi_constants.hrl having
        // SERVICE_NAME_PATTERN_MATCHING defined
        API.TransId test1_id = this.api.send_async(
            this.api.prefix() + "a/b/c/d", ("test1").getBytes());
        API.TransId test2_id = this.api.send_async(
            this.api.prefix() + "a/b/c/z", ("test2").getBytes());
        API.TransId test3_id = this.api.send_async(
            this.api.prefix() + "a/b/c/dd", ("test3").getBytes());
        API.TransId test4_id = this.api.send_async(
            this.api.prefix() + "a/b/z/d", ("test4").getBytes());
        API.TransId test5_id = this.api.send_async(
            this.api.prefix() + "a/b/cc/d", ("test5").getBytes());
        API.TransId test6_id = this.api.send_async(
            this.api.prefix() + "a/z/c/d", ("test6").getBytes());
        API.TransId test7_id = this.api.send_async(
            this.api.prefix() + "a/bb/c/d", ("test7").getBytes());
        API.TransId test8_id = this.api.send_async(
            this.api.prefix() + "z/b/c/d", ("test8").getBytes());
        API.TransId test9_id = this.api.send_async(
            this.api.prefix() + "aa/b/c/d", ("test9").getBytes());
        API.TransId test10_id = this.api.send_async(
            this.api.prefix() + "a/b/czd", ("test10").getBytes());
        API.TransId test11_id = this.api.send_async(
            this.api.prefix() + "a/bzc/d", ("test11").getBytes());
        API.TransId test12_id = this.api.send_async(
            this.api.prefix() + "azb/c/d", ("test12").getBytes());
        API.TransId test13_id = this.api.send_async(
            this.api.prefix() + "a/bzczd", ("test13").getBytes());
        API.TransId test14_id = this.api.send_async(
            this.api.prefix() + "azbzc/d", ("test14").getBytes());
        API.TransId test15_id = this.api.send_async(
            this.api.prefix() + "azbzczd", ("test15").getBytes());
        // n.b., depends on cloudi_constants.hrl having
        // RECV_ASYNC_STRATEGY == recv_async_select_oldest
        this.api.recv_async(test1_id.id, false);
        API.Response test1_check = this.api.recv_async();
        assert new String(test1_check.response) == "test1";
        assert test1_id.equals(test1_check.id);
        this.api.recv_async(test2_id.id, false);
        API.Response test2_check = this.api.recv_async();
        assert new String(test2_check.response) == "test2";
        assert test2_id.equals(test2_check.id);
        this.api.recv_async(test3_id.id, false);
        API.Response test3_check = this.api.recv_async();
        assert new String(test3_check.response) == "test3";
        assert test3_id.equals(test3_check.id);
        this.api.recv_async(test4_id.id, false);
        API.Response test4_check = this.api.recv_async();
        assert new String(test4_check.response) == "test4";
        assert test4_id.equals(test4_check.id);
        this.api.recv_async(test5_id.id, false);
        API.Response test5_check = this.api.recv_async();
        assert new String(test5_check.response) == "test5";
        assert test5_id.equals(test5_check.id);
        this.api.recv_async(test6_id.id, false);
        API.Response test6_check = this.api.recv_async();
        assert new String(test6_check.response) == "test6";
        assert test6_id.equals(test6_check.id);
        this.api.recv_async(test7_id.id, false);
        API.Response test7_check = this.api.recv_async();
        assert new String(test7_check.response) == "test7";
        assert test7_id.equals(test7_check.id);
        this.api.recv_async(test8_id.id, false);
        API.Response test8_check = this.api.recv_async();
        assert new String(test8_check.response) == "test8";
        assert test8_id.equals(test8_check.id);
        this.api.recv_async(test9_id.id, false);
        API.Response test9_check = this.api.recv_async();
        assert new String(test9_check.response) == "test9";
        assert test9_id.equals(test9_check.id);
        this.api.recv_async(test10_id.id, false);
        API.Response test10_check = this.api.recv_async();
        assert new String(test10_check.response) == "test10";
        assert test10_id.equals(test10_check.id);
        this.api.recv_async(test11_id.id, false);
        API.Response test11_check = this.api.recv_async();
        assert new String(test11_check.response) == "test11";
        assert test11_id.equals(test11_check.id);
        this.api.recv_async(test12_id.id, false);
        API.Response test12_check = this.api.recv_async();
        assert new String(test12_check.response) == "test12";
        assert test12_id.equals(test12_check.id);
        this.api.recv_async(test13_id.id, false);
        API.Response test13_check = this.api.recv_async();
        assert new String(test13_check.response) == "test13";
        assert test13_id.equals(test13_check.id);
        this.api.recv_async(test14_id.id, false);
        API.Response test14_check = this.api.recv_async();
        assert new String(test14_check.response) == "test14";
        assert test14_id.equals(test14_check.id);
        this.api.recv_async(test15_id.id, false);
        API.Response test15_check = this.api.recv_async();
        assert new String(test15_check.response) == "test15";
        assert test15_id.equals(test15_check.id);
        API.out.println("messaging sequence1 end java");
        // start sequence2
        this.api.send_async(this.api.prefix() + "sequence2",
                            ("start").getBytes());
        this.api.return_(command, name, pattern,
                         ("").getBytes(), ("end").getBytes(),
                         timeout, trans_id, pid);
    }

    public void sequence2_e1(Integer command, String name, String pattern,
                             byte[] request_info, byte[] request,
                             Integer timeout, Byte priority,
                             byte[] trans_id, OtpErlangPid pid)
                             throws API.ReturnAsyncException,
                                    API.ReturnSyncException,
                                    API.InvalidInputException
    {
        this.api.return_(command, name, pattern,
                         ("").getBytes(), ("1").getBytes(),
                         timeout, trans_id, pid);
    }

    public void sequence2_e2(Integer command, String name, String pattern,
                             byte[] request_info, byte[] request,
                             Integer timeout, Byte priority,
                             byte[] trans_id, OtpErlangPid pid)
                             throws API.ReturnAsyncException,
                                    API.ReturnSyncException,
                                    API.InvalidInputException
    {
        this.api.return_(command, name, pattern,
                         ("").getBytes(), ("2").getBytes(),
                         timeout, trans_id, pid);
    }

    public void sequence2_e3(Integer command, String name, String pattern,
                             byte[] request_info, byte[] request,
                             Integer timeout, Byte priority,
                             byte[] trans_id, OtpErlangPid pid)
                             throws API.ReturnAsyncException,
                                    API.ReturnSyncException,
                                    API.InvalidInputException
    {
        this.api.return_(command, name, pattern,
                         ("").getBytes(), ("3").getBytes(),
                         timeout, trans_id, pid);
    }

    public void sequence2_e4(Integer command, String name, String pattern,
                             byte[] request_info, byte[] request,
                             Integer timeout, Byte priority,
                             byte[] trans_id, OtpErlangPid pid)
                             throws API.ReturnAsyncException,
                                    API.ReturnSyncException,
                                    API.InvalidInputException
    {
        this.api.return_(command, name, pattern,
                         ("").getBytes(), ("4").getBytes(),
                         timeout, trans_id, pid);
    }

    public void sequence2_e5(Integer command, String name, String pattern,
                             byte[] request_info, byte[] request,
                             Integer timeout, Byte priority,
                             byte[] trans_id, OtpErlangPid pid)
                             throws API.ReturnAsyncException,
                                    API.ReturnSyncException,
                                    API.InvalidInputException
    {
        this.api.return_(command, name, pattern,
                         ("").getBytes(), ("5").getBytes(),
                         timeout, trans_id, pid);
    }

    public void sequence2_e6(Integer command, String name, String pattern,
                             byte[] request_info, byte[] request,
                             Integer timeout, Byte priority,
                             byte[] trans_id, OtpErlangPid pid)
                             throws API.ReturnAsyncException,
                                    API.ReturnSyncException,
                                    API.InvalidInputException
    {
        this.api.return_(command, name, pattern,
                         ("").getBytes(), ("6").getBytes(),
                         timeout, trans_id, pid);
    }

    public void sequence2_e7(Integer command, String name, String pattern,
                             byte[] request_info, byte[] request,
                             Integer timeout, Byte priority,
                             byte[] trans_id, OtpErlangPid pid)
                             throws API.ReturnAsyncException,
                                    API.ReturnSyncException,
                                    API.InvalidInputException
    {
        this.api.return_(command, name, pattern,
                         ("").getBytes(), ("7").getBytes(),
                         timeout, trans_id, pid);
    }

    public void sequence2_e8(Integer command, String name, String pattern,
                             byte[] request_info, byte[] request,
                             Integer timeout, Byte priority,
                             byte[] trans_id, OtpErlangPid pid)
                             throws API.ReturnAsyncException,
                                    API.ReturnSyncException,
                                    API.InvalidInputException
    {
        this.api.return_(command, name, pattern,
                         ("").getBytes(), ("8").getBytes(),
                         timeout, trans_id, pid);
    }

    public void sequence2(Integer command, String name, String pattern,
                          byte[] request_info, byte[] request,
                          Integer timeout, Byte priority,
                          byte[] trans_id, OtpErlangPid pid)
                          throws API.ReturnAsyncException,
                                 API.ReturnSyncException,
                                 API.InvalidInputException,
                                 API.MessageDecodingException,
                                 InterruptedException
    {
        API.out.println("messaging sequence2 start java");
        assert new String(request) == "start";
        Thread.sleep(500);
        // the sending process is excluded from the services that receive
        // the asynchronous message, so in this case, the receiving thread
        // will not be called, despite the fact it has subscribed to 'e',
        // to prevent a process (in this case thread) from deadlocking
        // with itself.
        List<API.TransId> e_ids = this.api.mcast_async(this.api.prefix() + "e",
                                                       (" ").getBytes());
        // 4 * 8 == 32, but only 3 out of 4 threads can receive messages,
        // since 1 thread is sending the mcast_async, so 3 * 8 == 24
        assert e_ids.size() == 24;
        byte[] e_str_check = new byte[24];
        Iterator<API.TransId> e_ids_itr = e_ids.iterator(); 
        while (e_ids_itr.hasNext())
        {
            API.TransId e_id = e_ids_itr.next();
            API.Response e_check = this.api.recv_async(e_id.id);
            assert e_id.equals(e_check.id);
            final byte character = e_check.response[0];
            final int index = (character - ((byte) '1')) * 3;
            for (int offset = 0; offset < 3; offset++)
            {
                if (e_str_check[index + offset] == 0)
                {
                    e_str_check[index + offset] = character;
                    break;
                }
            }
        }
        assert new String(e_str_check) == "111222333444555666777888999";
        API.out.println("messaging sequence2 end java");
        // start sequence3
        this.api.send_async(this.api.prefix() + "sequence3",
                            ("start").getBytes());
        this.api.return_(command, name, pattern,
                         ("").getBytes(), ("end").getBytes(),
                         timeout, trans_id, pid);
    }

    public byte[] sequence3_f1(Integer command, String name, String pattern,
                               byte[] request_info, byte[] request,
                               Integer timeout, Byte priority,
                               byte[] trans_id, OtpErlangPid pid)
                               throws API.ForwardAsyncException,
                                      API.ForwardSyncException,
                                      API.InvalidInputException
    {
        int request_i = (int) request[0];
        if (request_i == 4)
        {
            return ("done").getBytes();
        }
        byte[] request_new = {(byte) (request_i + 2)}; // two steps forward
        this.api.forward_(command, this.api.prefix() + "f2",
                          request_info, request_new,
                          timeout, priority, trans_id, pid);
        return ("").getBytes(); // execution never gets here
    }

    public void sequence3_f2(Integer command, String name, String pattern,
                             byte[] request_info, byte[] request,
                             Integer timeout, Byte priority,
                             byte[] trans_id, OtpErlangPid pid)
                             throws API.ForwardAsyncException,
                                    API.ForwardSyncException,
                                    API.InvalidInputException
    {
        int request_i = (int) request[0];
        byte[] request_new = {(byte) (request_i - 1)}; // one step back
        this.api.forward_(command, this.api.prefix() + "f1",
                          request_info, request_new,
                          timeout, priority, trans_id, pid);
    }

    public void sequence3_g1(Integer command, String name, String pattern,
                             byte[] request_info, byte[] request,
                             Integer timeout, Byte priority,
                             byte[] trans_id, OtpErlangPid pid)
                             throws API.ReturnAsyncException,
                                    API.ReturnSyncException,
                                    API.InvalidInputException
    {
        String s = new String(request);
        this.api.return_(command, name, pattern,
                         ("").getBytes(), (s + "suffix").getBytes(),
                         timeout, trans_id, pid);
    }

    public void sequence3(Integer command, String name, String pattern,
                          byte[] request_info, byte[] request,
                          Integer timeout, Byte priority,
                          byte[] trans_id, OtpErlangPid pid)
                          throws API.ReturnAsyncException,
                                 API.ReturnSyncException,
                                 API.InvalidInputException,
                                 API.MessageDecodingException
    {
        API.out.println("messaging sequence3 start java");
        assert new String(request) == "start";
        API.TransId test1_id = this.api.send_async(this.api.prefix() + "f1",
                                                   ("0").getBytes());
        API.Response test1_check = this.api.recv_async(test1_id.id);
        assert test1_id.equals(test1_check.id);
        assert new String(test1_check.response) == "done";
        API.Response test2_check = this.api.send_sync(this.api.prefix() + "g1",
                                                      ("prefix_").getBytes());
        assert new String(test2_check.response) == "prefix_suffix";
        API.out.println("messaging sequence3 end java");
        // loop to find any infrequent problems, restart sequence1
        this.api.send_async(this.api.prefix() + "sequence1",
                            ("start").getBytes());
        this.api.return_(command, name, pattern,
                         ("").getBytes(), ("end").getBytes(),
                         timeout, trans_id, pid);
    }
 
    public void run()
    {
        try
        {
            this.api.subscribe("a/b/c/d", this, "sequence1_abcd");
            this.api.subscribe("a/b/c/*", this, "sequence1_abc_");
            this.api.subscribe("a/b/*/d", this, "sequence1_ab_d");
            this.api.subscribe("a/*/c/d", this, "sequence1_a_cd");
            this.api.subscribe("*/b/c/d", this, "sequence1__bcd");
            this.api.subscribe("a/b/*",   this, "sequence1_ab__");
            this.api.subscribe("a/*/d",   this, "sequence1_a__d");
            this.api.subscribe("*/c/d",   this, "sequence1___cd");
            this.api.subscribe("a/*",     this, "sequence1_a___");
            this.api.subscribe("*/d",     this, "sequence1____d");
            this.api.subscribe("*",       this, "sequence1_____");
            this.api.subscribe("sequence1", this, "sequence1");
            this.api.subscribe("e", this, "sequence2_e1");
            this.api.subscribe("e", this, "sequence2_e2");
            this.api.subscribe("e", this, "sequence2_e3");
            this.api.subscribe("e", this, "sequence2_e4");
            this.api.subscribe("e", this, "sequence2_e5");
            this.api.subscribe("e", this, "sequence2_e6");
            this.api.subscribe("e", this, "sequence2_e7");
            this.api.subscribe("e", this, "sequence2_e8");
            this.api.subscribe("sequence2", this, "sequence2");
            this.api.subscribe("f1", this, "sequence3_f1");
            this.api.subscribe("f2", this, "sequence3_f2");
            this.api.subscribe("g1", this, "sequence3_g1");
            this.api.subscribe("sequence3", this, "sequence3");

            if (this.thread_index == 0)
            {
                this.api.send_async(this.api.prefix() + "sequence1",
                                    ("start").getBytes());
            }

            Object result = api.poll();
            API.err.println("exited thread: " + result);
        }
        catch (API.MessageDecodingException e)
        {
            e.printStackTrace(API.err);
        }
    }
}

