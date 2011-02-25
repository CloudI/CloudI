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
import java.util.HashMap;
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
import com.ericsson.otp.erlang.OtpErlangTuple;

public class API
{
    private static final int ASYNC  =  1;
    private static final int SYNC   = -1;

    private static final int MESSAGE_INIT         = 1;
    private static final int MESSAGE_SEND_ASYNC   = 2;
    private static final int MESSAGE_SEND_SYNC    = 3;
    private static final int MESSAGE_RECV_ASYNC   = 4;
    private static final int MESSAGE_RETURN_ASYNC = 5;
    private static final int MESSAGE_RETURN_SYNC  = 6;

    private FileDescriptor socket;
    private FileOutputStream output;
    private FileInputStream input;
    private HashMap<String, Function6<Integer,
                                      String,
                                      byte[],
                                      Integer,
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
        this.output = new FileOutputStream(this.socket);
        this.input = new FileInputStream(this.socket);
        this.callbacks = new HashMap<String, Function6<Integer,
                                                       String,
                                                       byte[],
                                                       Integer,
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
                           new Function6<Integer,
                                         String,
                                         byte[],
                                         Integer,
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
        return send_async(name, request, this.timeout_async);
    }

    public TransId send_async(String name, byte[] request, Integer timeout)
    {
        try
        {
            OtpOutputStream send_async = new OtpOutputStream();
            send_async.write(OtpExternal.versionTag);
            final OtpErlangObject[] tuple = {new OtpErlangAtom("send_async"),
                                             new OtpErlangString(name),
                                             new OtpErlangBinary(request),
                                             new OtpErlangUInt(timeout)};
            send_async.write_any(new OtpErlangTuple(tuple));
            send(send_async);
            return (TransId) poll();
        }
        catch (OtpErlangRangeException e)
        {
            e.printStackTrace();
            return null;
        }
    }

    public Response send_sync(String name, byte[] request)
    {
        return send_sync(name, request, this.timeout_sync);
    }

    public Response send_sync(String name, byte[] request, Integer timeout)
    {
        try
        {
            OtpOutputStream send_sync = new OtpOutputStream();
            send_sync.write(OtpExternal.versionTag);
            final OtpErlangObject[] tuple = {new OtpErlangAtom("send_sync"),
                                             new OtpErlangString(name),
                                             new OtpErlangBinary(request),
                                             new OtpErlangUInt(timeout)};
            send_sync.write_any(new OtpErlangTuple(tuple));
            send(send_sync);
            return (Response) poll();
        }
        catch (OtpErlangRangeException e)
        {
            e.printStackTrace();
            return null;
        }
    }

    public void forward_(Integer command,
                         String name, byte[] response, Integer timeout,
                         byte[] transId, OtpErlangPid pid)
    {
        if (command == API.ASYNC)
            forward_async(name, response, timeout, transId, pid);
        else if (command == API.SYNC)
            forward_sync(name, response, timeout, transId, pid);
        else
            assert false : command;
    }

    public void forward_async(String name, byte[] response, Integer timeout,
                              byte[] transId, OtpErlangPid pid)
    {
        try
        {
            OtpOutputStream forward_async = new OtpOutputStream();
            forward_async.write(OtpExternal.versionTag);
            final OtpErlangObject[] tuple = {new OtpErlangAtom("forward_async"),
                                             new OtpErlangString(name),
                                             new OtpErlangBinary(response),
                                             new OtpErlangUInt(timeout),
                                             new OtpErlangBinary(transId), pid};
            forward_async.write_any(new OtpErlangTuple(tuple));
            send(forward_async);
        }
        catch (OtpErlangRangeException e)
        {
            e.printStackTrace();
            return;
        }
    }

    public void forward_sync(String name, byte[] response, Integer timeout,
                             byte[] transId, OtpErlangPid pid)
    {
        try
        {
            OtpOutputStream forward_sync = new OtpOutputStream();
            forward_sync.write(OtpExternal.versionTag);
            final OtpErlangObject[] tuple = {new OtpErlangAtom("forward_sync"),
                                             new OtpErlangString(name),
                                             new OtpErlangBinary(response),
                                             new OtpErlangUInt(timeout),
                                             new OtpErlangBinary(transId), pid};
            forward_sync.write_any(new OtpErlangTuple(tuple));
            send(forward_sync);
        }
        catch (OtpErlangRangeException e)
        {
            e.printStackTrace();
            return;
        }
    }

    public void return_(Integer command,
                        String name, byte[] response, Integer timeout,
                        byte[] transId, OtpErlangPid pid)
                        throws ReturnAsyncException, ReturnSyncException
    {
        if (command == API.ASYNC)
            return_async(name, response, timeout, transId, pid);
        else if (command == API.SYNC)
            return_sync(name, response, timeout, transId, pid);
        else
            assert false : command;
    }

    public void return_async(String name, byte[] response, Integer timeout,
                             byte[] transId, OtpErlangPid pid)
                             throws ReturnAsyncException
    {
        try
        {
            OtpOutputStream return_async = new OtpOutputStream();
            return_async.write(OtpExternal.versionTag);
            final OtpErlangObject[] tuple = {new OtpErlangAtom("return_async"),
                                             new OtpErlangString(name),
                                             new OtpErlangBinary(response),
                                             new OtpErlangUInt(timeout),
                                             new OtpErlangBinary(transId), pid};
            return_async.write_any(new OtpErlangTuple(tuple));
            send(return_async);
            throw new ReturnAsyncException();
        }
        catch (OtpErlangRangeException e)
        {
            e.printStackTrace();
            return;
        }
    }

    public void return_sync(String name, byte[] response, Integer timeout,
                            byte[] transId, OtpErlangPid pid)
                            throws ReturnSyncException
    {
        try
        {
            OtpOutputStream return_sync = new OtpOutputStream();
            return_sync.write(OtpExternal.versionTag);
            final OtpErlangObject[] tuple = {new OtpErlangAtom("return_sync"),
                                             new OtpErlangString(name),
                                             new OtpErlangBinary(response),
                                             new OtpErlangUInt(timeout),
                                             new OtpErlangBinary(transId), pid};
            return_sync.write_any(new OtpErlangTuple(tuple));
            send(return_sync);
            throw new ReturnSyncException();
        }
        catch (OtpErlangRangeException e)
        {
            e.printStackTrace();
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
            e.printStackTrace();
            return null;
        }
    }

    private void callback(int command, String name, byte[] request,
                          Integer timeout, byte[] transId, OtpErlangPid pid)
    {
        if (command == MESSAGE_SEND_ASYNC)
        {
            try
            {
                Object response = this.callbacks.get(name)
                                      .invoke(API.ASYNC, name, request,
                                              timeout, transId, pid);
                if (response.getClass() == byte[].class)
                {
                    return_async(name, (byte[]) response,
                                 timeout, transId, pid);
                }
                else
                {
                    return_async(name, response.toString().getBytes(),
                                 timeout, transId, pid);
                }
                // to force the sync catch below to be valid
                return_sync(name, ("").getBytes(), timeout, transId, pid);
            }
            catch (ReturnAsyncException e)
            {
                return;
            }
            catch (ReturnSyncException e)
            {
                e.printStackTrace();
                return;
            }
            catch (Throwable e)
            {
                e.printStackTrace();
                return;
            }
        }
        else if (command == MESSAGE_SEND_SYNC)
        {
            try
            {
                Object response = this.callbacks.get(name)
                                      .invoke(API.SYNC, name, request,
                                              timeout, transId, pid);
                if (response.getClass() == byte[].class)
                {
                    return_sync(name, (byte[]) response,
                                timeout, transId, pid);
                }
                else
                {
                    return_sync(name, response.toString().getBytes(),
                                timeout, transId, pid);
                }
                // to force the async catch below to be valid
                return_async(name, ("").getBytes(), timeout, transId, pid);
            }
            catch (ReturnSyncException e)
            {
                return;
            }
            catch (ReturnAsyncException e)
            {
                e.printStackTrace();
                return;
            }
            catch (Throwable e)
            {
                e.printStackTrace();
                return;
            }
        }
    }

    public Object poll()
    {
        while (true)
        {
            try
            {
                byte[] data = API.recv(this.input, this.buffer_size);
                if (data == null || data.length == 0)
                    return null;

                ByteBuffer buffer = ByteBuffer.wrap(data);
                buffer.order(ByteOrder.nativeOrder());
                int command;
                switch (command = buffer.getInt())
                {
                    case MESSAGE_INIT:
                    {
                        int prefixSize = buffer.getInt();
                        this.prefix = API.getString(buffer, prefixSize);
                        this.timeout_async = buffer.getInt();
                        this.timeout_sync = buffer.getInt();
                        return null;
                    }
                    case MESSAGE_SEND_ASYNC:
                    case MESSAGE_SEND_SYNC:
                    {
                        int nameSize = buffer.getInt();
                        String name = API.getString(buffer, nameSize);
                        int requestSize = buffer.getInt();
                        byte[] request = API.getBytes(buffer, requestSize);
                        int timeout = buffer.getInt();
                        byte[] transId = API.getBytes(buffer, 16);
                        int pidSize = buffer.getInt();
                        OtpErlangPid pid = API.getPid(buffer, pidSize);
                        callback(command, name, request, timeout, transId, pid);
                        break;
                    }
                    case MESSAGE_RECV_ASYNC:
                    case MESSAGE_RETURN_SYNC:
                    {
                        int responseSize = buffer.getInt();
                        byte[] response = API.getBytes(buffer, responseSize);
                        byte[] transId = API.getBytes(buffer, 16);
                        return new Response(response, transId);
                    }
                    case MESSAGE_RETURN_ASYNC:
                    {
                        byte[] transId = API.getBytes(buffer, 16);
                        return new TransId(transId);
                    }
                    default:
                        return null;
                }
            }
            catch (IOException e)
            {
                e.printStackTrace();
                return null;
            }
        }
    }

    private void send(OtpOutputStream command)
    {
        try
        {
            command.writeTo(this.output);
        }
        catch (IOException e)
        {
            e.printStackTrace();
            return;
        }
    }

    private static byte[] recv(FileInputStream input,
                               final int size) throws IOException
    {
        byte[] bytes = new byte[size];
    
        ByteArrayOutputStream output = new ByteArrayOutputStream();
        int read = 0;
        while ((read = input.read(bytes, 0, size)) == size)
            output.write(bytes, 0, size);

        if (read == -1)
            return null;
        output.write(bytes, 0, read);
    
        return output.toByteArray();
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
            e.printStackTrace();
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
            e.printStackTrace();
            return null;
        }
        catch (NoSuchMethodException e)
        {
            e.printStackTrace();
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
            e.printStackTrace();
            return null;
        }
        catch (InstantiationException e)
        {
            e.printStackTrace();
            return null;
        }
        catch (IllegalAccessException e)
        {
            e.printStackTrace();
            return null;
        }
        catch (InvocationTargetException e)
        {
            e.printStackTrace();
            return null;
        }
        return object;
    }

    public class Response
    {
        public final byte[] response;
        public final byte[] id;

        Response(byte[] resp, byte[] transId)
        {
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

