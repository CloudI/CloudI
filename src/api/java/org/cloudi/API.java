// -*- coding: utf-8; Mode: java; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
// ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
//
// BSD LICENSE
// 
// Copyright (c) 2011, Michael Truog <mjtruog at gmail dot com>
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
import java.util.ArrayList;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
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
    private static final PrintStream out = new PrintStream(System.out, true);
    private static final PrintStream err = new PrintStream(System.err, true);

    private static final int ASYNC  =  1;
    private static final int SYNC   = -1;

    private static final int MESSAGE_INIT            = 1;
    private static final int MESSAGE_SEND_ASYNC      = 2;
    private static final int MESSAGE_SEND_SYNC       = 3;
    private static final int MESSAGE_RECV_ASYNC      = 4;
    private static final int MESSAGE_RETURN_ASYNC    = 5;
    private static final int MESSAGE_RETURN_SYNC     = 6;
    private static final int MESSAGE_RETURNS_ASYNC   = 7;
    private static final int MESSAGE_KEEPALIVE       = 8;

    private FileDescriptor socket;
    private boolean use_header;
    private FileOutputStream output;
    private FileInputStream input;
    private HashMap<String, Function8<Integer,
                                      String,
                                      byte[],
                                      byte[],
                                      Integer,
                                      Byte,
                                      byte[],
                                      OtpErlangPid> > callbacks;
    private final int buffer_size;
    private String prefix;
    private int timeout_sync;
    private int timeout_async;

    public API(final int index, final String protocol, final int size)
    {
        this.socket = API.storeFD(index + 3);
        assert this.socket != null : (index + 3);
        this.use_header = (protocol.compareTo("tcp") == 0);
        this.output = new FileOutputStream(this.socket);
        this.input = new FileInputStream(this.socket);
        this.callbacks = new HashMap<String, Function8<Integer,
                                                       String,
                                                       byte[],
                                                       byte[],
                                                       Integer,
                                                       Byte,
                                                       byte[],
                                                       OtpErlangPid> >();
        this.buffer_size = size;
        this.timeout_sync = 5000;
        this.timeout_async = 5000;

        // send the initialization message to the managing Erlang process
        OtpOutputStream init = new OtpOutputStream();
        init.write(OtpExternal.versionTag);
        init.write_any(new OtpErlangAtom("init"));
        send(init);
        poll();
    }

    public void subscribe(final String name,
                          final Object instance,
                          final String methodName)
    {
        this.callbacks.put(this.prefix + name,
                           new Function8<Integer,
                                         String,
                                         byte[],
                                         byte[],
                                         Integer,
                                         Byte,
                                         byte[],
                                         OtpErlangPid>(instance, methodName));
        OtpOutputStream subscribe = new OtpOutputStream();
        subscribe.write(OtpExternal.versionTag);
        final OtpErlangObject[] tuple = {new OtpErlangAtom("subscribe"),
                                         new OtpErlangString(name)};
        subscribe.write_any(new OtpErlangTuple(tuple));
        send(subscribe);
    }

    public void unsubscribe(final String name)
    {
        this.callbacks.remove(this.prefix + name);
        OtpOutputStream unsubscribe = new OtpOutputStream();
        unsubscribe.write(OtpExternal.versionTag);
        final OtpErlangObject[] tuple = {new OtpErlangAtom("unsubscribe"),
                                         new OtpErlangString(name)};
        unsubscribe.write_any(new OtpErlangTuple(tuple));
        send(unsubscribe);
    }

    public TransId send_async(String name, byte[] request)
    {
        return send_async(name, ("").getBytes(), request,
                          this.timeout_async, (byte) 0);
    }

    public TransId send_async(String name, byte[] requestInfo, byte[] request,
                              Integer timeout, Byte priority)
    {
        try
        {
            OtpOutputStream send_async = new OtpOutputStream();
            send_async.write(OtpExternal.versionTag);
            final OtpErlangObject[] tuple = {new OtpErlangAtom("send_async"),
                                             new OtpErlangString(name),
                                             new OtpErlangBinary(requestInfo),
                                             new OtpErlangBinary(request),
                                             new OtpErlangUInt(timeout),
                                             new OtpErlangInt(priority)};
            send_async.write_any(new OtpErlangTuple(tuple));
            send(send_async);
            return (TransId) poll();
        }
        catch (OtpErlangRangeException e)
        {
            e.printStackTrace(API.err);
            return null;
        }
    }

    public Response send_sync(String name, byte[] request)
    {
        return send_sync(name, ("").getBytes(), request,
                         this.timeout_sync, (byte) 0);
    }

    public Response send_sync(String name, byte[] requestInfo, byte[] request,
                              Integer timeout, Byte priority)
    {
        try
        {
            OtpOutputStream send_sync = new OtpOutputStream();
            send_sync.write(OtpExternal.versionTag);
            final OtpErlangObject[] tuple = {new OtpErlangAtom("send_sync"),
                                             new OtpErlangString(name),
                                             new OtpErlangBinary(requestInfo),
                                             new OtpErlangBinary(request),
                                             new OtpErlangUInt(timeout),
                                             new OtpErlangInt(priority)};
            send_sync.write_any(new OtpErlangTuple(tuple));
            send(send_sync);
            return (Response) poll();
        }
        catch (OtpErlangRangeException e)
        {
            e.printStackTrace(API.err);
            return null;
        }
    }

    public List<TransId> mcast_async(String name, byte[] request)
    {
        return mcast_async(name, new byte[0], request,
                           this.timeout_async, (byte) 0);
    }

    @SuppressWarnings("unchecked")
    public List<TransId> mcast_async(String name,
                                     byte[] requestInfo, byte[] request,
                                     Integer timeout, Byte priority)
    {
        try
        {
            OtpOutputStream mcast_async = new OtpOutputStream();
            mcast_async.write(OtpExternal.versionTag);
            final OtpErlangObject[] tuple = {new OtpErlangAtom("mcast_async"),
                                             new OtpErlangString(name),
                                             new OtpErlangBinary(requestInfo),
                                             new OtpErlangBinary(request),
                                             new OtpErlangUInt(timeout),
                                             new OtpErlangInt(priority)};
            mcast_async.write_any(new OtpErlangTuple(tuple));
            send(mcast_async);
            return (List<TransId>) poll();
        }
        catch (OtpErlangRangeException e)
        {
            e.printStackTrace(API.err);
            return null;
        }
    }

    public void forward_(Integer command,
                         String name, byte[] requestInfo, byte[] request,
                         Integer timeout, Byte priority,
                         byte[] transId, OtpErlangPid pid)
    {
        if (command == API.ASYNC)
            forward_async(name, requestInfo, request,
                          timeout, priority, transId, pid);
        else if (command == API.SYNC)
            forward_sync(name, requestInfo, request,
                         timeout, priority, transId, pid);
        else
            assert false : command;
    }

    public void forward_async(String name, byte[] requestInfo, byte[] request,
                              Integer timeout, Byte priority,
                              byte[] transId, OtpErlangPid pid)
    {
        try
        {
            OtpOutputStream forward_async = new OtpOutputStream();
            forward_async.write(OtpExternal.versionTag);
            final OtpErlangObject[] tuple = {new OtpErlangAtom("forward_async"),
                                             new OtpErlangString(name),
                                             new OtpErlangBinary(requestInfo),
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
    }

    public void forward_sync(String name, byte[] requestInfo, byte[] request,
                             Integer timeout, Byte priority,
                             byte[] transId, OtpErlangPid pid)
    {
        try
        {
            OtpOutputStream forward_sync = new OtpOutputStream();
            forward_sync.write(OtpExternal.versionTag);
            final OtpErlangObject[] tuple = {new OtpErlangAtom("forward_sync"),
                                             new OtpErlangString(name),
                                             new OtpErlangBinary(requestInfo),
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
    }

    public void return_(Integer command,
                        String name, byte[] responseInfo, byte[] response,
                        Integer timeout, byte[] transId, OtpErlangPid pid)
                        throws ReturnAsyncException, ReturnSyncException
    {
        if (command == API.ASYNC)
            return_async(name, responseInfo, response, timeout, transId, pid);
        else if (command == API.SYNC)
            return_sync(name, responseInfo, response, timeout, transId, pid);
        else
            assert false : command;
    }

    public void return_async(String name, byte[] responseInfo, byte[] response,
                             Integer timeout, byte[] transId, OtpErlangPid pid)
                             throws ReturnAsyncException
    {
        return_async_nothrow(name, responseInfo, response,
                             timeout, transId, pid);
        throw new ReturnAsyncException();
    }

    private void return_async_nothrow(String name,
                                      byte[] responseInfo, byte[] response,
                                      Integer timeout, byte[] transId,
                                      OtpErlangPid pid)
    {
        try
        {
            OtpOutputStream return_async = new OtpOutputStream();
            return_async.write(OtpExternal.versionTag);
            final OtpErlangObject[] tuple = {new OtpErlangAtom("return_async"),
                                             new OtpErlangString(name),
                                             new OtpErlangBinary(responseInfo),
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
    }

    public void return_sync(String name, byte[] responseInfo, byte[] response,
                            Integer timeout, byte[] transId, OtpErlangPid pid)
                            throws ReturnSyncException
    {
        return_sync_nothrow(name, responseInfo, response,
                            timeout, transId, pid);
        throw new ReturnSyncException();
    }

    private void return_sync_nothrow(String name,
                                     byte[] responseInfo, byte[] response,
                                     Integer timeout, byte[] transId,
                                     OtpErlangPid pid)
    {
        try
        {
            OtpOutputStream return_sync = new OtpOutputStream();
            return_sync.write(OtpExternal.versionTag);
            final OtpErlangObject[] tuple = {new OtpErlangAtom("return_sync"),
                                             new OtpErlangString(name),
                                             new OtpErlangBinary(responseInfo),
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
    }

    public Response recv_async(Integer timeout, byte[] transId)
    {
        try
        {
            OtpOutputStream recv_async = new OtpOutputStream();
            recv_async.write(OtpExternal.versionTag);
            final OtpErlangObject[] tuple = {new OtpErlangAtom("recv_async"),
                                             new OtpErlangUInt(timeout),
                                             new OtpErlangBinary(transId)};
            recv_async.write_any(new OtpErlangTuple(tuple));
            send(recv_async);
            return (Response) poll();
        }
        catch (OtpErlangRangeException e)
        {
            e.printStackTrace(API.err);
            return null;
        }
    }

    private void callback(int command, String name,
                          byte[] requestInfo, byte[] request,
                          Integer timeout, Byte priority,
                          byte[] transId, OtpErlangPid pid)
    {
        if (command == MESSAGE_SEND_ASYNC)
        {
            try
            {
                Object response = this.callbacks.get(name)
                                      .invoke(API.ASYNC, name,
                                              requestInfo, request,
                                              timeout, priority, transId, pid);
                if (response.getClass() == byte[][].class)
                {
                    byte [][] responseArray = (byte[][]) response;
                    assert responseArray.length == 2 : "invalid response";
                    return_async_nothrow(name,
                                         responseArray[0],
                                         responseArray[1],
                                         timeout, transId, pid);
                    
                }
                else if (response.getClass() == byte[].class)
                {
                    return_async_nothrow(name,
                                         ("").getBytes(),
                                         (byte[]) response,
                                         timeout, transId, pid);
                }
                else
                {
                    return_async_nothrow(name,
                                         ("").getBytes(),
                                         response.toString().getBytes(),
                                         timeout, transId, pid);
                }
            }
            catch (ReturnAsyncException e)
            {
                return;
            }
            catch (Throwable e)
            {
                e.printStackTrace(API.err);
                return_async_nothrow(name,
                                     ("").getBytes(),
                                     ("").getBytes(),
                                     timeout, transId, pid);
                return;
            }
        }
        else if (command == MESSAGE_SEND_SYNC)
        {
            try
            {
                Object response = this.callbacks.get(name)
                                      .invoke(API.SYNC, name,
                                              requestInfo, request,
                                              timeout, priority, transId, pid);
                if (response.getClass() == byte[][].class)
                {
                    byte [][] responseArray = (byte[][]) response;
                    assert responseArray.length == 2 : "invalid response";
                    return_sync_nothrow(name,
                                        responseArray[0],
                                        responseArray[1],
                                        timeout, transId, pid);
                    
                }
                else if (response.getClass() == byte[].class)
                {
                    return_sync_nothrow(name,
                                        ("").getBytes(),
                                        (byte[]) response,
                                        timeout, transId, pid);
                }
                else
                {
                    return_sync_nothrow(name,
                                        ("").getBytes(),
                                        response.toString().getBytes(),
                                        timeout, transId, pid);
                }
            }
            catch (ReturnSyncException e)
            {
                return;
            }
            catch (Throwable e)
            {
                e.printStackTrace(API.err);
                return_sync_nothrow(name,
                                    ("").getBytes(),
                                    ("").getBytes(),
                                    timeout, transId, pid);
                return;
            }
        }
    }

    public Object poll()
    {
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
                        assert ! buffer.hasRemaining() : "extra data";
                        return null;
                    }
                    case MESSAGE_SEND_ASYNC:
                    case MESSAGE_SEND_SYNC:
                    {
                        int nameSize = buffer.getInt();
                        String name = API.getString(buffer, nameSize);
                        int requestInfoSize = buffer.getInt();
                        byte[] requestInfo = API.getBytes(buffer,
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
                        assert ! buffer.hasRemaining() : "extra data";
                        callback(command, name, requestInfo, request,
                                 timeout, priority, transId, pid);
                        break;
                    }
                    case MESSAGE_RECV_ASYNC:
                    case MESSAGE_RETURN_SYNC:
                    {
                        int responseInfoSize = buffer.getInt();
                        byte[] responseInfo = API.getBytes(buffer,
                                                           responseInfoSize);
                        buffer.get();
                        int responseSize = buffer.getInt();
                        byte[] response = API.getBytes(buffer, responseSize);
                        buffer.get();
                        byte[] transId = API.getBytes(buffer, 16);
                        assert ! buffer.hasRemaining() : "extra data";
                        return new Response(responseInfo, response, transId);
                    }
                    case MESSAGE_RETURN_ASYNC:
                    {
                        byte[] transId = API.getBytes(buffer, 16);
                        assert ! buffer.hasRemaining() : "extra data";
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
                        assert ! buffer.hasRemaining() : "extra data";
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
                        return null;
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

    private HashMap<String, String> binary_key_value_parse(byte[] binary)
    {
        HashMap<String, String> result = new HashMap<String, String>();
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
                    result.put(key, new String(binary, binary_i,
                                               binary_j - binary_i));
                    key = null;
                }
                binary_i = binary_j + 1;
            }
        }
        return result;
    }

    public HashMap<String, String> request_http_qs_parse(byte[] request)
    {
        return binary_key_value_parse(request);
    }

    public HashMap<String, String> request_info_key_value_parse(byte[] 
                                                                request_info)
    {
        return binary_key_value_parse(request_info);
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
            while ((read = this.input.read(bytes, 0, this.buffer_size)) ==
                   this.buffer_size && this.input.available() > 0)
                output.write(bytes, 0, this.buffer_size);
            if (read == -1)
                return null;
            output.write(bytes, 0, read);
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
                        read = this.input.read(bytes, 0, this.buffer_size);
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
            c = clazz.getDeclaredConstructor(new Class[] { Integer.TYPE });
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
        public final byte[] responseInfo;
        public final byte[] response;
        public final byte[] id;

        Response(byte[] info, byte[] resp, byte[] transId)
        {
            this.responseInfo = info;
            this.response = resp;
            this.id = transId;
        }

        public boolean isEmpty()
        {
            return response.length == 0;
        }

        public boolean isTimeout()
        {
            return API.TransIdNull == this.id;
        }

        public String toString()
        {
            StringBuilder result = new StringBuilder();
            result.append("('");
            result.append(new String(this.responseInfo));
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

        public boolean isTimeout()
        {
            return API.TransIdNull == this.id;
        }

        public String toString()
        {
            return new String(this.id);
        }
    }

    public class ReturnSyncException extends Exception
    {
        private static final long serialVersionUID = 1L;
        ReturnSyncException()
        {
            super("Synchronous Call Return Invalid");
        }
    }
    
    public class ReturnAsyncException extends Exception
    {
        private static final long serialVersionUID = 1L;
        ReturnAsyncException()
        {
            super("Asynchronous Call Return Invalid");
        }
    }

}

