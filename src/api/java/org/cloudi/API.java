//-*-Mode:java;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=java fenc=utf-8 sts=4 ts=4 sw=4 et:
//
// BSD LICENSE
// 
// Copyright (c) 2011-2013, Michael Truog <mjtruog at gmail dot com>
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

package org.cloudi;

import java.io.FileDescriptor;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.PrintStream;
import java.util.HashMap;
import java.util.List;
import java.util.LinkedList;
import java.util.ArrayList;
import java.util.Arrays;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.Math;
import com.ericsson.otp.erlang.OtpExternal;
import com.ericsson.otp.erlang.OtpOutputStream;
import com.ericsson.otp.erlang.OtpInputStream;
import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangRangeException;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangUInt;
import com.ericsson.otp.erlang.OtpErlangInt;
import com.ericsson.otp.erlang.OtpErlangTuple;

public class API
{
    // unbuffered output is with API.err.printf(), etc.
    public static final PrintStream out = new PrintStream(System.out, true);
    public static final PrintStream err = new PrintStream(System.err, true);

    public static final int ASYNC  =  1;
    public static final int SYNC   = -1;

    private static final int MESSAGE_INIT            = 1;
    private static final int MESSAGE_SEND_ASYNC      = 2;
    private static final int MESSAGE_SEND_SYNC       = 3;
    private static final int MESSAGE_RECV_ASYNC      = 4;
    private static final int MESSAGE_RETURN_ASYNC    = 5;
    private static final int MESSAGE_RETURN_SYNC     = 6;
    private static final int MESSAGE_RETURNS_ASYNC   = 7;
    private static final int MESSAGE_KEEPALIVE       = 8;

    private FileDescriptor fd_in;
    private FileDescriptor fd_out;
    private boolean use_header;
    private FileOutputStream output;
    private FileInputStream input;
    private boolean initialization_complete;
    private HashMap<String,
                    LinkedList< Function9<Integer,
                                          String,
                                          String,
                                          byte[],
                                          byte[],
                                          Integer,
                                          Byte,
                                          byte[],
                                          OtpErlangPid> > > callbacks;
    private final int buffer_size;
    private long request_timer;
    private Integer request_timeout;
    private String prefix;
    private int timeout_async;
    private int timeout_sync;
    private byte priority_default;
    private boolean request_timeout_adjustment;

    public API(final int thread_index)
               throws InvalidInputException, MessageDecodingException
    {
        final String protocol = System.getenv("CLOUDI_API_INIT_PROTOCOL");
        if (protocol == null)
            throw new InvalidInputException();
        final String buffer_size_str =
            System.getenv("CLOUDI_API_INIT_BUFFER_SIZE");
        if (buffer_size_str == null)
            throw new InvalidInputException();
        if (protocol.compareTo("tcp") == 0)
        {
            this.fd_in = this.fd_out = API.storeFD(thread_index + 3);
            this.use_header = true;
        }
        else if (protocol.compareTo("udp") == 0)
        {
            this.fd_in = this.fd_out = API.storeFD(thread_index + 3);
            this.use_header = false;
        }
        else if (protocol.compareTo("local") == 0)
        {
            this.fd_in = this.fd_out = API.storeFD(thread_index + 3);
            this.use_header = true;
        }
        else
        {
            throw new InvalidInputException();
        }
        assert this.fd_in != null;
        assert this.fd_out != null;
        this.output = new FileOutputStream(this.fd_out);
        this.input = new FileInputStream(this.fd_in);
        this.initialization_complete = false;
        this.callbacks = new HashMap<String,
                                     LinkedList< Function9<Integer,
                                                           String,
                                                           String,
                                                           byte[],
                                                           byte[],
                                                           Integer,
                                                           Byte,
                                                           byte[],
                                                           OtpErlangPid> > >();
        this.buffer_size = Integer.parseInt(buffer_size_str);
        this.timeout_async = 5000;
        this.timeout_sync = 5000;
        this.priority_default = 0;

        // send the initialization message to the managing Erlang process
        OtpOutputStream init = new OtpOutputStream();
        init.write(OtpExternal.versionTag);
        init.write_any(new OtpErlangAtom("init"));
        send(init);
        poll_request(false);
    }

    public static int thread_count() throws InvalidInputException
    {
        final String s = System.getenv("CLOUDI_API_INIT_THREAD_COUNT");
        if (s == null)
            throw new InvalidInputException();
        final int thread_count = Integer.parseInt(s);
        return thread_count;
    }

    public void subscribe(final String pattern,
                          final Object instance,
                          final String methodName)
    {
        final String s = this.prefix + pattern;
        Function9<Integer,
                  String,
                  String,
                  byte[],
                  byte[],
                  Integer,
                  Byte,
                  byte[],
                  OtpErlangPid>
                  callback = new Function9<Integer,
                                           String,
                                           String,
                                           byte[],
                                           byte[],
                                           Integer,
                                           Byte,
                                           byte[],
                                           OtpErlangPid>(instance, methodName);
        LinkedList< Function9<Integer,
                              String,
                              String,
                              byte[],
                              byte[],
                              Integer,
                              Byte,
                              byte[],
                              OtpErlangPid> >
                    callback_list = this.callbacks.get(s);
        if (callback_list == null)
        {
            callback_list = new LinkedList< Function9<Integer,
                                                      String,
                                                      String,
                                                      byte[],
                                                      byte[],
                                                      Integer,
                                                      Byte,
                                                      byte[],
                                                      OtpErlangPid> >();
            callback_list.addLast(callback);
            this.callbacks.put(s, callback_list);
        }
        else
        {
            callback_list.addLast(callback);
        }
        OtpOutputStream subscribe = new OtpOutputStream();
        subscribe.write(OtpExternal.versionTag);
        final OtpErlangObject[] tuple = {new OtpErlangAtom("subscribe"),
                                         new OtpErlangString(pattern)};
        subscribe.write_any(new OtpErlangTuple(tuple));
        send(subscribe);
    }

    public void unsubscribe(final String pattern) throws InvalidInputException
    {
        final String s = this.prefix + pattern;
        LinkedList< Function9<Integer,
                              String,
                              String,
                              byte[],
                              byte[],
                              Integer,
                              Byte,
                              byte[],
                              OtpErlangPid> >
                    callback_list = this.callbacks.get(s);
        if (callback_list == null)
        {
            throw new InvalidInputException();
        }
        else
        {
            callback_list.removeFirst();
            if (callback_list.isEmpty())
            {
                this.callbacks.remove(s);
            }
        }
        OtpOutputStream unsubscribe = new OtpOutputStream();
        unsubscribe.write(OtpExternal.versionTag);
        final OtpErlangObject[] tuple = {new OtpErlangAtom("unsubscribe"),
                                         new OtpErlangString(pattern)};
        unsubscribe.write_any(new OtpErlangTuple(tuple));
        send(unsubscribe);
    }

    public TransId send_async(String name, byte[] request)
                              throws MessageDecodingException
    {
        return send_async(name, ("").getBytes(), request,
                          this.timeout_async, this.priority_default);
    }

    public TransId send_async(String name, byte[] request_info, byte[] request,
                              Integer timeout, Byte priority)
                              throws MessageDecodingException
    {
        try
        {
            OtpOutputStream send_async = new OtpOutputStream();
            send_async.write(OtpExternal.versionTag);
            final OtpErlangObject[] tuple = {new OtpErlangAtom("send_async"),
                                             new OtpErlangString(name),
                                             new OtpErlangBinary(request_info),
                                             new OtpErlangBinary(request),
                                             new OtpErlangUInt(timeout),
                                             new OtpErlangInt(priority)};
            send_async.write_any(new OtpErlangTuple(tuple));
            send(send_async);
            return (TransId) poll_request(false);
        }
        catch (OtpErlangRangeException e)
        {
            e.printStackTrace(API.err);
            return null;
        }
    }

    public Response send_sync(String name, byte[] request)
                              throws MessageDecodingException
    {
        return send_sync(name, ("").getBytes(), request,
                         this.timeout_sync, this.priority_default);
    }

    public Response send_sync(String name, byte[] request_info, byte[] request,
                              Integer timeout, Byte priority)
                              throws MessageDecodingException
    {
        try
        {
            OtpOutputStream send_sync = new OtpOutputStream();
            send_sync.write(OtpExternal.versionTag);
            final OtpErlangObject[] tuple = {new OtpErlangAtom("send_sync"),
                                             new OtpErlangString(name),
                                             new OtpErlangBinary(request_info),
                                             new OtpErlangBinary(request),
                                             new OtpErlangUInt(timeout),
                                             new OtpErlangInt(priority)};
            send_sync.write_any(new OtpErlangTuple(tuple));
            send(send_sync);
            return (Response) poll_request(false);
        }
        catch (OtpErlangRangeException e)
        {
            e.printStackTrace(API.err);
            return null;
        }
    }

    public List<TransId> mcast_async(String name, byte[] request)
                                     throws MessageDecodingException
    {
        return mcast_async(name, new byte[0], request,
                           this.timeout_async, this.priority_default);
    }

    @SuppressWarnings("unchecked")
    public List<TransId> mcast_async(String name,
                                     byte[] request_info, byte[] request,
                                     Integer timeout, Byte priority)
                                     throws MessageDecodingException
    {
        try
        {
            OtpOutputStream mcast_async = new OtpOutputStream();
            mcast_async.write(OtpExternal.versionTag);
            final OtpErlangObject[] tuple = {new OtpErlangAtom("mcast_async"),
                                             new OtpErlangString(name),
                                             new OtpErlangBinary(request_info),
                                             new OtpErlangBinary(request),
                                             new OtpErlangUInt(timeout),
                                             new OtpErlangInt(priority)};
            mcast_async.write_any(new OtpErlangTuple(tuple));
            send(mcast_async);
            return (List<TransId>) poll_request(false);
        }
        catch (OtpErlangRangeException e)
        {
            e.printStackTrace(API.err);
            return null;
        }
    }

    public void forward_(Integer command,
                         String name, byte[] request_info, byte[] request,
                         Integer timeout, Byte priority,
                         byte[] transId, OtpErlangPid pid)
                         throws ForwardAsyncException,
                                ForwardSyncException,
                                InvalidInputException
    {
        if (command == API.ASYNC)
            forward_async(name, request_info, request,
                          timeout, priority, transId, pid);
        else if (command == API.SYNC)
            forward_sync(name, request_info, request,
                         timeout, priority, transId, pid);
        else
            throw new InvalidInputException();
    }

    public void forward_async(String name, byte[] request_info, byte[] request,
                              Integer timeout, Byte priority,
                              byte[] transId, OtpErlangPid pid)
                              throws ForwardAsyncException
    {
        if (this.request_timeout_adjustment)
        {
            if (timeout == this.request_timeout)
            {
                final int elapsed = (int) java.lang.Math.max(0,
                    ((System.nanoTime() - this.request_timer) * 1e-6));
                if (elapsed > timeout)
                {
                    timeout = 0;
                }
                else
                {
                    timeout -= elapsed;
                }
            }
        }
        try
        {
            OtpOutputStream forward_async = new OtpOutputStream();
            forward_async.write(OtpExternal.versionTag);
            final OtpErlangObject[] tuple = {new OtpErlangAtom("forward_async"),
                                             new OtpErlangString(name),
                                             new OtpErlangBinary(request_info),
                                             new OtpErlangBinary(request),
                                             new OtpErlangUInt(timeout),
                                             new OtpErlangInt(priority),
                                             new OtpErlangBinary(transId), pid};
            forward_async.write_any(new OtpErlangTuple(tuple));
            send(forward_async);
        }
        catch (OtpErlangRangeException e)
        {
            e.printStackTrace(API.err);
            return;
        }
        throw new ForwardAsyncException();
    }

    public void forward_sync(String name, byte[] request_info, byte[] request,
                             Integer timeout, Byte priority,
                             byte[] transId, OtpErlangPid pid)
                             throws ForwardSyncException
    {
        if (this.request_timeout_adjustment)
        {
            if (timeout == this.request_timeout)
            {
                final int elapsed = (int) java.lang.Math.max(0,
                    ((System.nanoTime() - this.request_timer) * 1e-6));
                if (elapsed > timeout)
                {
                    timeout = 0;
                }
                else
                {
                    timeout -= elapsed;
                }
            }
        }
        try
        {
            OtpOutputStream forward_sync = new OtpOutputStream();
            forward_sync.write(OtpExternal.versionTag);
            final OtpErlangObject[] tuple = {new OtpErlangAtom("forward_sync"),
                                             new OtpErlangString(name),
                                             new OtpErlangBinary(request_info),
                                             new OtpErlangBinary(request),
                                             new OtpErlangUInt(timeout),
                                             new OtpErlangInt(priority),
                                             new OtpErlangBinary(transId), pid};
            forward_sync.write_any(new OtpErlangTuple(tuple));
            send(forward_sync);
        }
        catch (OtpErlangRangeException e)
        {
            e.printStackTrace(API.err);
            return;
        }
        throw new ForwardSyncException();
    }

    public void return_(Integer command,
                        String name, String pattern,
                        byte[] response_info, byte[] response,
                        Integer timeout, byte[] transId, OtpErlangPid pid)
                        throws ReturnAsyncException,
                               ReturnSyncException,
                               InvalidInputException
    {
        if (command == API.ASYNC)
            return_async(name, pattern, response_info, response,
                         timeout, transId, pid);
        else if (command == API.SYNC)
            return_sync(name, pattern, response_info, response,
                        timeout, transId, pid);
        else
            throw new InvalidInputException();
    }

    public void return_async(String name, String pattern,
                             byte[] response_info, byte[] response,
                             Integer timeout, byte[] transId, OtpErlangPid pid)
                             throws ReturnAsyncException
    {
        if (this.request_timeout_adjustment)
        {
            if (timeout == this.request_timeout)
            {
                final int elapsed = (int) java.lang.Math.max(0,
                    ((System.nanoTime() - this.request_timer) * 1e-6));
                if (elapsed > timeout)
                {
                    response_info = new byte[0];
                    response = new byte[0];
                    timeout = 0;
                }
                else
                {
                    timeout -= elapsed;
                }
            }
        }
        try
        {
            OtpOutputStream return_async = new OtpOutputStream();
            return_async.write(OtpExternal.versionTag);
            final OtpErlangObject[] tuple = {new OtpErlangAtom("return_async"),
                                             new OtpErlangString(name),
                                             new OtpErlangString(pattern),
                                             new OtpErlangBinary(response_info),
                                             new OtpErlangBinary(response),
                                             new OtpErlangUInt(timeout),
                                             new OtpErlangBinary(transId), pid};
            return_async.write_any(new OtpErlangTuple(tuple));
            send(return_async);
        }
        catch (OtpErlangRangeException e)
        {
            e.printStackTrace(API.err);
            return;
        }
        throw new ReturnAsyncException();
    }

    public void return_sync(String name, String pattern,
                            byte[] response_info, byte[] response,
                            Integer timeout, byte[] transId, OtpErlangPid pid)
                            throws ReturnSyncException
    {
        if (this.request_timeout_adjustment)
        {
            if (timeout == this.request_timeout)
            {
                final int elapsed = (int) java.lang.Math.max(0,
                    ((System.nanoTime() - this.request_timer) * 1e-6));
                if (elapsed > timeout)
                {
                    response_info = new byte[0];
                    response = new byte[0];
                    timeout = 0;
                }
                else
                {
                    timeout -= elapsed;
                }
            }
        }
        try
        {
            OtpOutputStream return_sync = new OtpOutputStream();
            return_sync.write(OtpExternal.versionTag);
            final OtpErlangObject[] tuple = {new OtpErlangAtom("return_sync"),
                                             new OtpErlangString(name),
                                             new OtpErlangString(pattern),
                                             new OtpErlangBinary(response_info),
                                             new OtpErlangBinary(response),
                                             new OtpErlangUInt(timeout),
                                             new OtpErlangBinary(transId), pid};
            return_sync.write_any(new OtpErlangTuple(tuple));
            send(return_sync);
        }
        catch (OtpErlangRangeException e)
        {
            e.printStackTrace(API.err);
            return;
        }
        throw new ReturnSyncException();
    }

    public Response recv_async()
                               throws MessageDecodingException
    {
        return recv_async(this.timeout_sync, TransIdNull, true);
    }

    public Response recv_async(Integer timeout)
                               throws MessageDecodingException
    {
        return recv_async(timeout, TransIdNull, true);
    }

    public Response recv_async(byte[] transId)
                               throws MessageDecodingException
    {
        return recv_async(this.timeout_sync, transId, true);
    }

    public Response recv_async(boolean consume)
                               throws MessageDecodingException
    {
        return recv_async(this.timeout_sync, TransIdNull, consume);
    }

    public Response recv_async(Integer timeout, byte[] transId)
                               throws MessageDecodingException
    {
        return recv_async(timeout, transId, true);
    }

    public Response recv_async(Integer timeout, boolean consume)
                               throws MessageDecodingException
    {
        return recv_async(timeout, TransIdNull, consume);
    }

    public Response recv_async(byte[] transId, boolean consume)
                               throws MessageDecodingException
    {
        return recv_async(this.timeout_sync, transId, consume);
    }

    public Response recv_async(Integer timeout, byte[] transId, boolean consume)
                               throws MessageDecodingException
    {
        try
        {
            OtpOutputStream recv_async = new OtpOutputStream();
            recv_async.write(OtpExternal.versionTag);
            final OtpErlangObject[] tuple = {new OtpErlangAtom("recv_async"),
                                             new OtpErlangUInt(timeout),
                                             new OtpErlangBinary(transId),
                                             consume ?
                                             new OtpErlangAtom("true") :
                                             new OtpErlangAtom("false")};
            recv_async.write_any(new OtpErlangTuple(tuple));
            send(recv_async);
            return (Response) poll_request(false);
        }
        catch (OtpErlangRangeException e)
        {
            e.printStackTrace(API.err);
            return null;
        }
    }

    public String prefix()
    {
        return this.prefix;
    }

    public int timeout_async()
    {
        return this.timeout_async;
    }

    public int timeout_sync()
    {
        return this.timeout_sync;
    }

    private void callback(int command, String name, String pattern,
                          byte[] request_info, byte[] request,
                          Integer timeout, Byte priority,
                          byte[] transId, OtpErlangPid pid)
                          throws MessageDecodingException
    {
        long request_time_start = 0;
        if (this.request_timeout_adjustment)
        {
            this.request_timer = System.nanoTime();
            this.request_timeout = timeout;
        }
        LinkedList< Function9<Integer,
                              String,
                              String,
                              byte[],
                              byte[],
                              Integer,
                              Byte,
                              byte[],
                              OtpErlangPid> >
                    callback_list = this.callbacks.get(pattern);
        callback_list.addLast(callback_list.removeFirst());
        Function9<Integer,
                  String,
                  String,
                  byte[],
                  byte[],
                  Integer,
                  Byte,
                  byte[],
                  OtpErlangPid> callback = callback_list.peekLast();
        if (command == MESSAGE_SEND_ASYNC)
        {
            try
            {
                Object response = callback.invoke(API.ASYNC, name, pattern,
                                                  request_info, request,
                                                  timeout, priority,
                                                  transId, pid);
                if (response.getClass() == byte[][].class)
                {
                    byte [][] responseArray = (byte[][]) response;
                    assert responseArray.length == 2 : "invalid response";
                    return_async(name, pattern,
                                 responseArray[0],
                                 responseArray[1],
                                 timeout, transId, pid);
                    
                }
                else if (response.getClass() == byte[].class)
                {
                    return_async(name, pattern,
                                 ("").getBytes(),
                                 (byte[]) response,
                                 timeout, transId, pid);
                }
                else
                {
                    return_async(name, pattern,
                                 ("").getBytes(),
                                 response.toString().getBytes(),
                                 timeout, transId, pid);
                }
                return;
            }
            catch (ReturnAsyncException e_return)
            {
                return;
            }
            catch (ForwardAsyncException e_forward)
            {
                return;
            }
            catch (Throwable e)
            {
                e.printStackTrace(API.err);
                try
                {
                    return_async(name, pattern,
                                 ("").getBytes(),
                                 ("").getBytes(),
                                 timeout, transId, pid);
                }
                catch (ReturnAsyncException e_return)
                {
                }
                return;
            }
        }
        else if (command == MESSAGE_SEND_SYNC)
        {
            try
            {
                Object response = callback.invoke(API.SYNC, name, pattern,
                                                  request_info, request,
                                                  timeout, priority,
                                                  transId, pid);
                if (response.getClass() == byte[][].class)
                {
                    byte [][] responseArray = (byte[][]) response;
                    assert responseArray.length == 2 : "invalid response";
                    return_sync(name, pattern,
                                responseArray[0],
                                responseArray[1],
                                timeout, transId, pid);
                    
                }
                else if (response.getClass() == byte[].class)
                {
                    return_sync(name, pattern,
                                ("").getBytes(),
                                (byte[]) response,
                                timeout, transId, pid);
                }
                else
                {
                    return_sync(name, pattern,
                                ("").getBytes(),
                                response.toString().getBytes(),
                                timeout, transId, pid);
                }
                return;
            }
            catch (ReturnSyncException e_return)
            {
                return;
            }
            catch (ForwardSyncException e_forward)
            {
                return;
            }
            catch (Throwable e)
            {
                e.printStackTrace(API.err);
                try
                {
                    return_sync(name, pattern,
                                ("").getBytes(),
                                ("").getBytes(),
                                timeout, transId, pid);
                }
                catch (ReturnSyncException e_return)
                {
                }
                return;
            }
        }
        else
        {
            throw new MessageDecodingException();
        }
    }

    private Object poll_request(boolean external)
                                throws MessageDecodingException
    {
        if (external && ! this.initialization_complete)
        {
            OtpOutputStream polling = new OtpOutputStream();
            polling.write(OtpExternal.versionTag);
            polling.write_any(new OtpErlangAtom("polling"));
            send(polling);
            this.initialization_complete = true;
        }

        ByteBuffer buffer = recv(null);
        if (buffer == null || buffer.remaining() == 0)
            return null;

        while (true)
        {
            try
            {
                int command;
                switch (command = buffer.getInt())
                {
                    case MESSAGE_INIT:
                    {
                        int prefixSize = buffer.getInt();
                        this.prefix = API.getString(buffer, prefixSize);
                        this.timeout_async = buffer.getInt();
                        this.timeout_sync = buffer.getInt();
                        this.priority_default = buffer.get();
                        this.request_timeout_adjustment =
                            (buffer.get() & 0xFF) != 0;
                        if (buffer.hasRemaining())
                            throw new MessageDecodingException();
                        return null;
                    }
                    case MESSAGE_SEND_ASYNC:
                    case MESSAGE_SEND_SYNC:
                    {
                        int nameSize = buffer.getInt();
                        String name = API.getString(buffer, nameSize);
                        int patternSize = buffer.getInt();
                        String pattern = API.getString(buffer, patternSize);
                        int requestInfoSize = buffer.getInt();
                        byte[] request_info = API.getBytes(buffer,
                                                           requestInfoSize);
                        buffer.get();
                        int requestSize = buffer.getInt();
                        byte[] request = API.getBytes(buffer, requestSize);
                        buffer.get();
                        int timeout = buffer.getInt();
                        byte priority = buffer.get();
                        byte[] transId = API.getBytes(buffer, 16);
                        int pidSize = buffer.getInt();
                        OtpErlangPid pid = API.getPid(buffer, pidSize);
                        if (buffer.hasRemaining())
                            throw new MessageDecodingException();
                        callback(command, name, pattern, request_info, request,
                                 timeout, priority, transId, pid);
                        break;
                    }
                    case MESSAGE_RECV_ASYNC:
                    case MESSAGE_RETURN_SYNC:
                    {
                        int responseInfoSize = buffer.getInt();
                        byte[] response_info = API.getBytes(buffer,
                                                            responseInfoSize);
                        buffer.get();
                        int responseSize = buffer.getInt();
                        byte[] response = API.getBytes(buffer, responseSize);
                        buffer.get();
                        byte[] transId = API.getBytes(buffer, 16);
                        if (buffer.hasRemaining())
                            throw new MessageDecodingException();
                        return new Response(response_info, response, transId);
                    }
                    case MESSAGE_RETURN_ASYNC:
                    {
                        byte[] transId = API.getBytes(buffer, 16);
                        if (buffer.hasRemaining())
                            throw new MessageDecodingException();
                        return new TransId(transId);
                    }
                    case MESSAGE_RETURNS_ASYNC:
                    {
                        int transIdCount = buffer.getInt();
                        List<TransId> transIdList = new ArrayList<TransId>();
                        for (int i = 0; i < transIdCount; ++i)
                        {
                            byte[] transId = API.getBytes(buffer, 16);
                            transIdList.add(new TransId(transId));
                        }
                        if (buffer.hasRemaining())
                            throw new MessageDecodingException();
                        return transIdList;
                    }
                    case MESSAGE_KEEPALIVE:
                    {
                        OtpOutputStream keepalive = new OtpOutputStream();
                        keepalive.write(OtpExternal.versionTag);
                        keepalive.write_any(new OtpErlangAtom("keepalive"));
                        send(keepalive);
                        if (buffer.hasRemaining() &&
                            this.input.available() == 0)
                            continue;
                        break;
                    }
                    default:
                        throw new MessageDecodingException();
                }
    
                buffer = recv(buffer);
                if (buffer == null || buffer.remaining() == 0)
                    return null;
            }
            catch (IOException e)
            {
                e.printStackTrace(API.err);
                return null;
            }
        }
    }

    public Object poll() throws MessageDecodingException
    {
        return poll_request(true);
    }

    private HashMap<String, List<String> > binary_key_value_parse(byte[] binary)
    {
        HashMap<String, List<String> > result =
            new HashMap<String, List<String> >();
        String key = null;
        int binary_i = 0;
        for (int binary_j = 0; binary_j < binary.length; ++binary_j)
        {
            if (binary[binary_j] == 0)
            {
                if (key == null)
                {
                    key = new String(binary, binary_i, binary_j - binary_i);
                }
                else
                {
                    List<String> value = result.get(key);
                    final String element = new String(binary, binary_i,
                                                      binary_j - binary_i);
                    if (value == null)
                    {
                        value = new ArrayList<String>();
                        value.add(element);
                        result.put(key, value);
                    }
                    else
                    {
                        value.add(element);
                    }
                    key = null;
                }
                binary_i = binary_j + 1;
            }
        }
        return result;
    }

    public HashMap<String, List<String> > request_http_qs_parse(byte[] request)
    {
        return binary_key_value_parse(request);
    }

    public HashMap<String, List<String> > info_key_value_parse(byte[] info)
    {
        return binary_key_value_parse(info);
    }

    private void send(OtpOutputStream command)
    {
        try
        {
            if (this.use_header)
            {
                final long length = command.size();
                final byte[] header = {(byte) ((length & 0xff000000) >> 24),
                                       (byte) ((length & 0x00ff0000) >> 16),
                                       (byte) ((length & 0x0000ff00) >> 8),
                                       (byte) ( length & 0x000000ff)};
                this.output.write(header);
            }
            command.writeTo(this.output);
        }
        catch (IOException e)
        {
            e.printStackTrace(API.err);
            return;
        }
    }

    private ByteBuffer recv(ByteBuffer buffer_in)
    {
        try
        {
            ByteArrayOutputStream output = new ByteArrayOutputStream();
            if (buffer_in != null && buffer_in.hasRemaining())
                output.write(buffer_in.array(),
                             buffer_in.position(),
                             buffer_in.limit());
            int read = 0;
            byte[] bytes = new byte[this.buffer_size];
            boolean consume = true;
            while (consume)
            {
                while ((read = this.input.read(bytes)) == this.buffer_size &&
                       this.input.available() > 0)
                    output.write(bytes, 0, this.buffer_size);
                if (read == -1)
                    return null;
                output.write(bytes, 0, read);
                if (this.use_header == false)
                    consume = false;
                else if (output.size() >= 4)
                    consume = false;
            }
            byte[] result = output.toByteArray();
            ByteBuffer buffer_out = null;
            if (this.use_header)
            {
                final long length = (result[0] << 24) |
                                    (result[1] << 16) |
                                    (result[2] <<  8) |
                                     result[3];
                if (output.size() != (length + 4))
                {
                    assert output.size() < (length + 4) : "recv overflow";
                    output = new ByteArrayOutputStream();
                    output.write(result, 4, result.length - 4);
                    while (output.size() < length)
                    {
                        read = this.input.read(bytes, 0,
                                               Math.min((int) (length -
                                                               output.size()),
                                                        this.buffer_size));
                        if (read == -1)
                            return null;
                        output.write(bytes, 0, read);
                    }
                    result = output.toByteArray();
                    buffer_out = ByteBuffer.wrap(result);
                }
                else
                {
                    buffer_out = ByteBuffer.wrap(result, 4, result.length - 4);
                }
            }
            else
            {
                buffer_out = ByteBuffer.wrap(result);
            }
            buffer_out.order(ByteOrder.nativeOrder());
            return buffer_out;
        }
        catch (IOException e)
        {
            e.printStackTrace(API.err);
            return null;
        }
    }

    private static String getString(ByteBuffer buffer, final int size)
    {
        String value = new String(API.getBytes(buffer, size - 1));
        buffer.position(buffer.position() + 1); // skip the '\0' terminator
        return value;
    }

    private static OtpErlangPid getPid(ByteBuffer buffer, final int size)
    {
        try
        {
            return (new OtpInputStream(API.getBytes(buffer, size))).read_pid();
        }
        catch (OtpErlangDecodeException e)
        {
            e.printStackTrace(API.err);
            return null;
        }
    }

    private static byte[] getBytes(ByteBuffer buffer, final int size)
    {
        byte[] data = new byte[size];
        buffer.get(data, 0, size);
        return data;
    }

    private static FileDescriptor storeFD(final int fd)
    {
        Class<FileDescriptor> clazz = FileDescriptor.class;

        Constructor<FileDescriptor> c;
        try
        {
            Class[] intarg = { Integer.TYPE };
            c = clazz.getDeclaredConstructor(intarg);
        }
        catch (SecurityException e)
        {
            e.printStackTrace(API.err);
            return null;
        }
        catch (NoSuchMethodException e)
        {
            e.printStackTrace(API.err);
            return null;
        }

        c.setAccessible(true);
        FileDescriptor object;
        try
        {
            object = c.newInstance(new Integer(fd));
        }
        catch (IllegalArgumentException e)
        {
            e.printStackTrace(API.err);
            return null;
        }
        catch (InstantiationException e)
        {
            e.printStackTrace(API.err);
            return null;
        }
        catch (IllegalAccessException e)
        {
            e.printStackTrace(API.err);
            return null;
        }
        catch (InvocationTargetException e)
        {
            e.printStackTrace(API.err);
            return null;
        }
        return object;
    }

    public class Response
    {
        public final byte[] response_info;
        public final byte[] response;
        public final byte[] id;

        Response(byte[] info, byte[] resp, byte[] transId)
        {
            this.response_info = info;
            this.response = resp;
            this.id = transId;
        }

        public boolean isEmpty()
        {
            return response.length == 0;
        }

        public boolean isTimeout()
        {
            return Arrays.equals(this.id, API.TransIdNull);
        }

        public String toString()
        {
            StringBuilder result = new StringBuilder();
            result.append("('");
            result.append(new String(this.response_info));
            result.append("', '");
            result.append(new String(this.response));
            result.append("', '");
            result.append(new String(this.id));
            result.append("')");
            return result.toString();
        }
    }

    public static final byte[] TransIdNull = {0, 0, 0, 0, 0, 0, 0, 0,
                                              0, 0, 0, 0, 0, 0, 0, 0};

    public class TransId
    {
        public final byte[] id;

        TransId(byte[] transId)
        {
            this.id = transId;
        }

        public boolean equals(byte[] bytes)
        {
            return Arrays.equals(this.id, bytes);
        }

        public boolean isTimeout()
        {
            return equals(API.TransIdNull);
        }

        public String toString()
        {
            return new String(this.id);
        }
    }

    public static class InvalidInputException extends Exception
    {
        private static final long serialVersionUID = 1L;
        InvalidInputException()
        {
            super("Invalid Input");
        }
    }

    public static class ReturnSyncException extends Exception
    {
        private static final long serialVersionUID = 3L;

        ReturnSyncException()
        {
            super("Synchronous Call Return Invalid");
        }
    }
    
    public static class ReturnAsyncException extends Exception
    {
        private static final long serialVersionUID = 3L;

        ReturnAsyncException()
        {
            super("Asynchronous Call Return Invalid");
        }
    }

    public static class ForwardSyncException extends Exception
    {
        private static final long serialVersionUID = 3L;

        ForwardSyncException()
        {
            super("Synchronous Call Forward Invalid");
        }
    }
    
    public static class ForwardAsyncException extends Exception
    {
        private static final long serialVersionUID = 3L;

        ForwardAsyncException()
        {
            super("Asynchronous Call Forward Invalid");
        }
    }

    public static class MessageDecodingException extends Exception
    {
        private static final long serialVersionUID = 1L;
        MessageDecodingException()
        {
            super("Message Decoding Error");
        }
    }

}

