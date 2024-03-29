//-*-Mode:java;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=java fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
//
// MIT License
//
// Copyright (c) 2012-2022 Michael Truog <mjtruog at protonmail dot com>
//
// Permission is hereby granted, free of charge, to any person obtaining a
// copy of this software and associated documentation files (the "Software"),
// to deal in the Software without restriction, including without limitation
// the rights to use, copy, modify, merge, publish, distribute, sublicense,
// and/or sell copies of the Software, and to permit persons to whom the
// Software is furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
// DEALINGS IN THE SOFTWARE.
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
    private final int thread_index;
     
    public Task(final int thread_index)
    {
        this.api = null;
        this.thread_index = thread_index;
    }

    public void sequence1_abcd(Integer request_type,
                               String name, String pattern,
                               byte[] request_info, byte[] request,
                               Integer timeout, Byte priority,
                               byte[] trans_id, OtpErlangPid source)
                               throws API.ReturnAsyncException,
                                      API.ReturnSyncException,
                                      API.InvalidInputException
    {
        String s = new String(request);
        assert pattern.equals(this.api.prefix() + "a/b/c/d");
        assert s.equals("test1");
        this.api.return_(request_type, name, pattern,
                         ("").getBytes(), request,
                         timeout, trans_id, source);
    }
 
    public void sequence1_abc_(Integer request_type,
                               String name, String pattern,
                               byte[] request_info, byte[] request,
                               Integer timeout, Byte priority,
                               byte[] trans_id, OtpErlangPid source)
                               throws API.ReturnAsyncException,
                                      API.ReturnSyncException,
                                      API.InvalidInputException
    {
        String s = new String(request);
        assert pattern.equals(this.api.prefix() + "a/b/c/*");
        assert s.equals("test2") || s.equals("test3");
        this.api.return_(request_type, name, pattern,
                         ("").getBytes(), request,
                         timeout, trans_id, source);
    }
 
    public void sequence1_ab_d(Integer request_type,
                               String name, String pattern,
                               byte[] request_info, byte[] request,
                               Integer timeout, Byte priority,
                               byte[] trans_id, OtpErlangPid source)
                               throws API.ReturnAsyncException,
                                      API.ReturnSyncException,
                                      API.InvalidInputException
    {
        String s = new String(request);
        assert pattern.equals(this.api.prefix() + "a/b/*/d");
        assert s.equals("test4") || s.equals("test5");
        this.api.return_(request_type, name, pattern,
                         ("").getBytes(), request,
                         timeout, trans_id, source);
    }
 
    public void sequence1_a_cd(Integer request_type,
                               String name, String pattern,
                               byte[] request_info, byte[] request,
                               Integer timeout, Byte priority,
                               byte[] trans_id, OtpErlangPid source)
                               throws API.ReturnAsyncException,
                                      API.ReturnSyncException,
                                      API.InvalidInputException
    {
        String s = new String(request);
        assert pattern.equals(this.api.prefix() + "a/*/c/d");
        assert s.equals("test6") || s.equals("test7");
        this.api.return_(request_type, name, pattern,
                         ("").getBytes(), request,
                         timeout, trans_id, source);
    }
 
    public void sequence1__bcd(Integer request_type,
                               String name, String pattern,
                               byte[] request_info, byte[] request,
                               Integer timeout, Byte priority,
                               byte[] trans_id, OtpErlangPid source)
                               throws API.ReturnAsyncException,
                                      API.ReturnSyncException,
                                      API.InvalidInputException
    {
        String s = new String(request);
        assert pattern.equals(this.api.prefix() + "*/b/c/d");
        assert s.equals("test8") || s.equals("test9");
        this.api.return_(request_type, name, pattern,
                         ("").getBytes(), request,
                         timeout, trans_id, source);
    }
 
    public void sequence1_ab__(Integer request_type,
                               String name, String pattern,
                               byte[] request_info, byte[] request,
                               Integer timeout, Byte priority,
                               byte[] trans_id, OtpErlangPid source)
                               throws API.ReturnAsyncException,
                                      API.ReturnSyncException,
                                      API.InvalidInputException
    {
        String s = new String(request);
        assert pattern.equals(this.api.prefix() + "a/b/*");
        assert s.equals("test10");
        this.api.return_(request_type, name, pattern,
                         ("").getBytes(), request,
                         timeout, trans_id, source);
    }
 
    public void sequence1_a__d(Integer request_type,
                               String name, String pattern,
                               byte[] request_info, byte[] request,
                               Integer timeout, Byte priority,
                               byte[] trans_id, OtpErlangPid source)
                               throws API.ReturnAsyncException,
                                      API.ReturnSyncException,
                                      API.InvalidInputException
    {
        String s = new String(request);
        assert pattern.equals(this.api.prefix() + "a/*/d");
        assert s.equals("test11");
        this.api.return_(request_type, name, pattern,
                         ("").getBytes(), request,
                         timeout, trans_id, source);
    }
 
    public void sequence1___cd(Integer request_type,
                               String name, String pattern,
                               byte[] request_info, byte[] request,
                               Integer timeout, Byte priority,
                               byte[] trans_id, OtpErlangPid source)
                               throws API.ReturnAsyncException,
                                      API.ReturnSyncException,
                                      API.InvalidInputException
    {
        String s = new String(request);
        assert pattern.equals(this.api.prefix() + "*/c/d");
        assert s.equals("test12");
        this.api.return_(request_type, name, pattern,
                         ("").getBytes(), request,
                         timeout, trans_id, source);
    }
 
    public void sequence1_a___(Integer request_type,
                               String name, String pattern,
                               byte[] request_info, byte[] request,
                               Integer timeout, Byte priority,
                               byte[] trans_id, OtpErlangPid source)
                               throws API.ReturnAsyncException,
                                      API.ReturnSyncException,
                                      API.InvalidInputException
    {
        String s = new String(request);
        assert pattern.equals(this.api.prefix() + "a/*");
        assert s.equals("test13");
        this.api.return_(request_type, name, pattern,
                         ("").getBytes(), request,
                         timeout, trans_id, source);
    }
 
    public void sequence1____d(Integer request_type,
                               String name, String pattern,
                               byte[] request_info, byte[] request,
                               Integer timeout, Byte priority,
                               byte[] trans_id, OtpErlangPid source)
                               throws API.ReturnAsyncException,
                                      API.ReturnSyncException,
                                      API.InvalidInputException
    {
        String s = new String(request);
        assert pattern.equals(this.api.prefix() + "*/d");
        assert s.equals("test14");
        this.api.return_(request_type, name, pattern,
                         ("").getBytes(), request,
                         timeout, trans_id, source);
    }
 
    public void sequence1_____(Integer request_type,
                               String name, String pattern,
                               byte[] request_info, byte[] request,
                               Integer timeout, Byte priority,
                               byte[] trans_id, OtpErlangPid source)
                               throws API.ReturnAsyncException,
                                      API.ReturnSyncException,
                                      API.InvalidInputException
    {
        String s = new String(request);
        assert pattern.equals(this.api.prefix() + "*");
        assert s.equals("test15");
        this.api.return_(request_type, name, pattern,
                         ("").getBytes(), request,
                         timeout, trans_id, source);
    }
 
    public void sequence1(Integer request_type,
                          String name, String pattern,
                          byte[] request_info, byte[] request,
                          Integer timeout, Byte priority,
                          byte[] trans_id, OtpErlangPid source)
                          throws API.ReturnAsyncException,
                                 API.ReturnSyncException,
                                 API.InvalidInputException,
                                 API.MessageDecodingException,
                                 API.TerminateException
    {
        String s = new String(request);
        API.Response end_check = this.api.recv_async(1000);
        while (new String(end_check.response).equals("end"))
        {
            // consume "end" and sleep
            end_check = this.api.recv_async(1000);
        }
        API.out.println("messaging sequence1 start java (" + s + ")");
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
        // n.b., depends on cloudi_core_i_constants.hrl having
        // RECV_ASYNC_STRATEGY == recv_async_select_oldest
        this.api.recv_async(test1_id.id, false);
        API.Response test1_check = this.api.recv_async();
        assert new String(test1_check.response).equals("test1");
        assert test1_id.equals(test1_check.id);
        this.api.recv_async(test2_id.id, false);
        API.Response test2_check = this.api.recv_async();
        assert new String(test2_check.response).equals("test2");
        assert test2_id.equals(test2_check.id);
        this.api.recv_async(test3_id.id, false);
        API.Response test3_check = this.api.recv_async();
        assert new String(test3_check.response).equals("test3");
        assert test3_id.equals(test3_check.id);
        this.api.recv_async(test4_id.id, false);
        API.Response test4_check = this.api.recv_async();
        assert new String(test4_check.response).equals("test4");
        assert test4_id.equals(test4_check.id);
        this.api.recv_async(test5_id.id, false);
        API.Response test5_check = this.api.recv_async();
        assert new String(test5_check.response).equals("test5");
        assert test5_id.equals(test5_check.id);
        this.api.recv_async(test6_id.id, false);
        API.Response test6_check = this.api.recv_async();
        assert new String(test6_check.response).equals("test6");
        assert test6_id.equals(test6_check.id);
        this.api.recv_async(test7_id.id, false);
        API.Response test7_check = this.api.recv_async();
        assert new String(test7_check.response).equals("test7");
        assert test7_id.equals(test7_check.id);
        this.api.recv_async(test8_id.id, false);
        API.Response test8_check = this.api.recv_async();
        assert new String(test8_check.response).equals("test8");
        assert test8_id.equals(test8_check.id);
        this.api.recv_async(test9_id.id, false);
        API.Response test9_check = this.api.recv_async();
        assert new String(test9_check.response).equals("test9");
        assert test9_id.equals(test9_check.id);
        this.api.recv_async(test10_id.id, false);
        API.Response test10_check = this.api.recv_async();
        assert new String(test10_check.response).equals("test10");
        assert test10_id.equals(test10_check.id);
        this.api.recv_async(test11_id.id, false);
        API.Response test11_check = this.api.recv_async();
        assert new String(test11_check.response).equals("test11");
        assert test11_id.equals(test11_check.id);
        this.api.recv_async(test12_id.id, false);
        API.Response test12_check = this.api.recv_async();
        assert new String(test12_check.response).equals("test12");
        assert test12_id.equals(test12_check.id);
        this.api.recv_async(test13_id.id, false);
        API.Response test13_check = this.api.recv_async();
        assert new String(test13_check.response).equals("test13");
        assert test13_id.equals(test13_check.id);
        this.api.recv_async(test14_id.id, false);
        API.Response test14_check = this.api.recv_async();
        assert new String(test14_check.response).equals("test14");
        assert test14_id.equals(test14_check.id);
        this.api.recv_async(test15_id.id, false);
        API.Response test15_check = this.api.recv_async();
        assert new String(test15_check.response).equals("test15");
        assert test15_id.equals(test15_check.id);
        API.out.println("messaging sequence1 end java (" + s + ")");
        // start sequence2
        this.api.send_async(this.api.prefix() + "sequence2", request);
        this.api.return_(request_type, name, pattern,
                         ("").getBytes(), ("end").getBytes(),
                         timeout, trans_id, source);
    }

    public void sequence2_e1(Integer request_type,
                             String name, String pattern,
                             byte[] request_info, byte[] request,
                             Integer timeout, Byte priority,
                             byte[] trans_id, OtpErlangPid source)
                             throws API.ReturnAsyncException,
                                    API.ReturnSyncException,
                                    API.InvalidInputException
    {
        this.api.return_(request_type, name, pattern,
                         ("").getBytes(), ("1").getBytes(),
                         timeout, trans_id, source);
    }

    public void sequence2_e2(Integer request_type,
                             String name, String pattern,
                             byte[] request_info, byte[] request,
                             Integer timeout, Byte priority,
                             byte[] trans_id, OtpErlangPid source)
                             throws API.ReturnAsyncException,
                                    API.ReturnSyncException,
                                    API.InvalidInputException
    {
        this.api.return_(request_type, name, pattern,
                         ("").getBytes(), ("2").getBytes(),
                         timeout, trans_id, source);
    }

    public void sequence2_e3(Integer request_type,
                             String name, String pattern,
                             byte[] request_info, byte[] request,
                             Integer timeout, Byte priority,
                             byte[] trans_id, OtpErlangPid source)
                             throws API.ReturnAsyncException,
                                    API.ReturnSyncException,
                                    API.InvalidInputException
    {
        this.api.return_(request_type, name, pattern,
                         ("").getBytes(), ("3").getBytes(),
                         timeout, trans_id, source);
    }

    public void sequence2_e4(Integer request_type,
                             String name, String pattern,
                             byte[] request_info, byte[] request,
                             Integer timeout, Byte priority,
                             byte[] trans_id, OtpErlangPid source)
                             throws API.ReturnAsyncException,
                                    API.ReturnSyncException,
                                    API.InvalidInputException
    {
        this.api.return_(request_type, name, pattern,
                         ("").getBytes(), ("4").getBytes(),
                         timeout, trans_id, source);
    }

    public void sequence2_e5(Integer request_type,
                             String name, String pattern,
                             byte[] request_info, byte[] request,
                             Integer timeout, Byte priority,
                             byte[] trans_id, OtpErlangPid source)
                             throws API.ReturnAsyncException,
                                    API.ReturnSyncException,
                                    API.InvalidInputException
    {
        this.api.return_(request_type, name, pattern,
                         ("").getBytes(), ("5").getBytes(),
                         timeout, trans_id, source);
    }

    public void sequence2_e6(Integer request_type,
                             String name, String pattern,
                             byte[] request_info, byte[] request,
                             Integer timeout, Byte priority,
                             byte[] trans_id, OtpErlangPid source)
                             throws API.ReturnAsyncException,
                                    API.ReturnSyncException,
                                    API.InvalidInputException
    {
        this.api.return_(request_type, name, pattern,
                         ("").getBytes(), ("6").getBytes(),
                         timeout, trans_id, source);
    }

    public void sequence2_e7(Integer request_type,
                             String name, String pattern,
                             byte[] request_info, byte[] request,
                             Integer timeout, Byte priority,
                             byte[] trans_id, OtpErlangPid source)
                             throws API.ReturnAsyncException,
                                    API.ReturnSyncException,
                                    API.InvalidInputException
    {
        this.api.return_(request_type, name, pattern,
                         ("").getBytes(), ("7").getBytes(),
                         timeout, trans_id, source);
    }

    public void sequence2_e8(Integer request_type,
                             String name, String pattern,
                             byte[] request_info, byte[] request,
                             Integer timeout, Byte priority,
                             byte[] trans_id, OtpErlangPid source)
                             throws API.ReturnAsyncException,
                                    API.ReturnSyncException,
                                    API.InvalidInputException
    {
        this.api.return_(request_type, name, pattern,
                         ("").getBytes(), ("8").getBytes(),
                         timeout, trans_id, source);
    }

    public void sequence2(Integer request_type,
                          String name, String pattern,
                          byte[] request_info, byte[] request,
                          Integer timeout, Byte priority,
                          byte[] trans_id, OtpErlangPid source)
                          throws API.ReturnAsyncException,
                                 API.ReturnSyncException,
                                 API.InvalidInputException,
                                 API.MessageDecodingException,
                                 API.TerminateException,
                                 InterruptedException
    {
        String s = new String(request);
        API.out.println("messaging sequence2 start java (" + s + ")");
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
        byte[] e_str_check = {0, 0, 0, 0, 0, 0, 0, 0,
                              0, 0, 0, 0, 0, 0, 0, 0,
                              0, 0, 0, 0, 0, 0, 0, 0};
        Iterator<API.TransId> e_ids_itr = e_ids.iterator(); 
        while (e_ids_itr.hasNext())
        {
            API.TransId e_id = e_ids_itr.next();
            API.Response e_check = this.api.recv_async(e_id.id);
            assert e_id.equals(e_check.id);
            if (e_check.isEmpty())
                continue;
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
        assert new String(e_str_check).equals("111222333444555666777888");
        API.out.println("messaging sequence2 end java (" + s + ")");
        // start sequence3
        this.api.send_async(this.api.prefix() + "sequence3", request);
        this.api.return_(request_type, name, pattern,
                         ("").getBytes(), ("end").getBytes(),
                         timeout, trans_id, source);
    }

    public byte[] sequence3_f1(Integer request_type,
                               String name, String pattern,
                               byte[] request_info, byte[] request,
                               Integer timeout, Byte priority,
                               byte[] trans_id, OtpErlangPid source)
                               throws API.ForwardAsyncException,
                                      API.ForwardSyncException,
                                      API.InvalidInputException
    {
        int request_i = (int) request[0];
        final byte[] zero = ("0").getBytes();
        if (request_i == ((int) zero[0]) + 4)
        {
            return ("done").getBytes();
        }
        byte[] request_new = {(byte) (request_i + 2)}; // two steps forward
        this.api.forward_(request_type, this.api.prefix() + "f2",
                          request_info, request_new,
                          timeout, priority, trans_id, source);
        return ("").getBytes(); // execution never gets here
    }

    public void sequence3_f2(Integer request_type,
                             String name, String pattern,
                             byte[] request_info, byte[] request,
                             Integer timeout, Byte priority,
                             byte[] trans_id, OtpErlangPid source)
                             throws API.ForwardAsyncException,
                                    API.ForwardSyncException,
                                    API.InvalidInputException
    {
        int request_i = (int) request[0];
        byte[] request_new = {(byte) (request_i - 1)}; // one step back
        this.api.forward_(request_type, this.api.prefix() + "f1",
                          request_info, request_new,
                          timeout, priority, trans_id, source);
    }

    public void sequence3_g1(Integer request_type,
                             String name, String pattern,
                             byte[] request_info, byte[] request,
                             Integer timeout, Byte priority,
                             byte[] trans_id, OtpErlangPid source)
                             throws API.ReturnAsyncException,
                                    API.ReturnSyncException,
                                    API.InvalidInputException
    {
        String s = new String(request);
        this.api.return_(request_type, name, pattern,
                         ("").getBytes(), (s + "suffix").getBytes(),
                         timeout, trans_id, source);
    }

    public void sequence3(Integer request_type,
                          String name, String pattern,
                          byte[] request_info, byte[] request,
                          Integer timeout, Byte priority,
                          byte[] trans_id, OtpErlangPid source)
                          throws API.ReturnAsyncException,
                                 API.ReturnSyncException,
                                 API.InvalidInputException,
                                 API.MessageDecodingException,
                                 API.TerminateException
    {
        String s = new String(request);
        API.out.println("messaging sequence3 start java (" + s + ")");
        API.TransId test1_id = this.api.send_async(this.api.prefix() + "f1",
                                                   ("0").getBytes());
        API.Response test1_check = this.api.recv_async(test1_id.id);
        assert test1_id.equals(test1_check.id);
        assert new String(test1_check.response).equals("done");
        API.Response test2_check = this.api.send_sync(this.api.prefix() + "g1",
                                                      ("prefix_").getBytes());
        assert new String(test2_check.response).equals("prefix_suffix");
        API.out.println("messaging sequence3 end java (" + s + ")");
        // loop to find any infrequent problems, restart sequence1
        int iteration = Integer.parseInt(s) + 1;
        if (iteration == Integer.MAX_VALUE)
        {
            iteration = 0;
        }
        this.api.send_async(this.api.prefix() + "sequence1",
                            Integer.toString(iteration).getBytes());
        this.api.return_(request_type, name, pattern,
                         ("").getBytes(), ("end").getBytes(),
                         timeout, trans_id, source);
    }
 
    public void run()
    {
        try
        {
            this.api = new API(this.thread_index);
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
                                    ("1").getBytes());
            }

            boolean result = this.api.poll();
            assert result == false;
        }
        catch (API.TerminateException e)
        {
        }
        catch (Exception e)
        {
            e.printStackTrace(API.err);
        }
        API.out.println("terminate messaging java");
    }
}

