//-*-Mode:java;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=java fenc=utf-8 sts=4 ts=4 sw=4 et:
//
// BSD LICENSE
// 
// Copyright (c) 2011-2014, Michael Truog <mjtruog at gmail dot com>
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
import java.util.concurrent.Executors;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Callable;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import java.nio.BufferUnderflowException;
import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.lang.Math;
import java.lang.Thread;
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

    private static final int MESSAGE_INIT                = 1;
    private static final int MESSAGE_SEND_ASYNC          = 2;
    private static final int MESSAGE_SEND_SYNC           = 3;
    private static final int MESSAGE_RECV_ASYNC          = 4;
    private static final int MESSAGE_RETURN_ASYNC        = 5;
    private static final int MESSAGE_RETURN_SYNC         = 6;
    private static final int MESSAGE_RETURNS_ASYNC       = 7;
    private static final int MESSAGE_KEEPALIVE           = 8;
    private static final int MESSAGE_REINIT              = 9;
    private static final int MESSAGE_SUBSCRIBE_COUNT     = 10;
    private static final int MESSAGE_TERM                = 11;

    private FileDescriptor fd_in;
    private FileDescriptor fd_out;
    private boolean use_header;
    private FileOutputStream output;
    private FileInputStream input;
    private ExecutorService poll_timer_executor;
    private boolean initialization_complete;
    private boolean terminate;
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
    private int process_index;
    private int process_count;
    private int process_count_max;
    private int process_count_min;
    private String prefix;
    private int timeout_initialize;
    private int timeout_async;
    private int timeout_sync;
    private int timeout_terminate;
    private byte priority_default;
    private boolean request_timeout_adjustment;

    public API(final int thread_index) throws InvalidInputException,
                                              MessageDecodingException,
                                              TerminateException
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
        this.poll_timer_executor = Executors.newFixedThreadPool(1);
        this.initialization_complete = false;
        this.terminate = false;
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
        this.process_index = 0;
        this.process_count = 0;
        this.process_count_max = 0;
        this.process_count_min = 0;
        this.timeout_initialize = 5000;
        this.timeout_async = 5000;
        this.timeout_sync = 5000;
        this.timeout_terminate = 1000; // TIMEOUT_TERMINATE_MIN
        this.priority_default = 0;

        // send the initialization message to the managing Erlang process
        OtpOutputStream init = new OtpOutputStream();
        init.write(OtpExternal.versionTag);
        init.write_any(new OtpErlangAtom("init"));
        send(init);
        poll_request(null, false);
    }

    /**
     * @return the number of threads to create per operating system process
     */
    public static int thread_count() throws InvalidInputException
    {
        final String s = System.getenv("CLOUDI_API_INIT_THREAD_COUNT");
        if (s == null)
            throw new InvalidInputException();
        final int thread_count = Integer.parseInt(s);
        return thread_count;
    }

    /**
     * Subscribes an object method to a service name pattern. 
     *
     * @param  pattern     the service name pattern
     * @param  instance    the object instance
     * @param  methodName  the object method to handle matching requests
     */ 
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

    /**
     * Determine how may service name pattern subscriptions have occurred.
     *
     * @param  pattern     the service name pattern
     */ 
    public int subscribe_count(final String pattern)
                               throws InvalidInputException,
                                      MessageDecodingException,
                                      TerminateException
    {
        OtpOutputStream subscribe_count = new OtpOutputStream();
        subscribe_count.write(OtpExternal.versionTag);
        final OtpErlangObject[] tuple = {new OtpErlangAtom("subscribe_count"),
                                         new OtpErlangString(pattern)};
        subscribe_count.write_any(new OtpErlangTuple(tuple));
        send(subscribe_count);
        try
        {
            return (Integer) poll_request(null, false);
        }
        catch (MessageDecodingException e)
        {
            e.printStackTrace(API.err);
            return -1;
        }
    }

    /**
     * Unsubscribes from a service name pattern. 
     *
     * @param pattern  the service name pattern
     */
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

    /**
     * Asynchronous point-to-point communication to a service
     * subscribed that matches the destination service <code>name</code>.
     *
     * @param name     the destination service name
     * @param request  the request data
     * @return         a transaction ID
     */
    public TransId send_async(String name, byte[] request)
                              throws InvalidInputException,
                                     MessageDecodingException,
                                     TerminateException
    {
        return send_async(name, ("").getBytes(), request,
                          this.timeout_async, this.priority_default);
    }

    /**
     * Asynchronous point-to-point communication to a service
     * subscribed that matches the destination service <code>name</code>.
     *
     * @param name          the destination service name
     * @param request_info  any request metadata
     * @param request       the request data
     * @param timeout       the request timeout in milliseconds
     * @param priority      the request priority
     * @return              a transaction ID
     */
    public TransId send_async(String name, byte[] request_info, byte[] request,
                              Integer timeout, Byte priority)
                              throws InvalidInputException,
                                     MessageDecodingException,
                                     TerminateException
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
            return (TransId) poll_request(null, false);
        }
        catch (OtpErlangRangeException e)
        {
            e.printStackTrace(API.err);
            return null;
        }
    }

    /**
     * Synchronous point-to-point communication to a service
     * subscribed that matches the destination service <code>name</code>.
     *
     * @param name          the destination service name
     * @param request       the request data
     * @return              the response
     */
    public Response send_sync(String name, byte[] request)
                              throws InvalidInputException,
                                     MessageDecodingException,
                                     TerminateException
    {
        return send_sync(name, ("").getBytes(), request,
                         this.timeout_sync, this.priority_default);
    }

    /**
     * Synchronous point-to-point communication to a service
     * subscribed that matches the destination service <code>name</code>.
     *
     * @param name          the destination service name
     * @param request_info  any request metadata
     * @param request       the request data
     * @param timeout       the request timeout in milliseconds
     * @param priority      the request priority
     * @return              the response
     */
    public Response send_sync(String name, byte[] request_info, byte[] request,
                              Integer timeout, Byte priority)
                              throws InvalidInputException,
                                     MessageDecodingException,
                                     TerminateException
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
            return (Response) poll_request(null, false);
        }
        catch (OtpErlangRangeException e)
        {
            e.printStackTrace(API.err);
            return null;
        }
    }

    /**
     * Asynchronous point-multicast communication to services
     * subscribed that matches the destination service <code>name</code>.
     *
     * @param name          the destination service name
     * @param request       the request data
     * @return              transaction IDs
     */
    public List<TransId> mcast_async(String name, byte[] request)
                                     throws InvalidInputException,
                                            MessageDecodingException,
                                            TerminateException
    {
        return mcast_async(name, new byte[0], request,
                           this.timeout_async, this.priority_default);
    }

    /**
     * Asynchronous point-multicast communication to services
     * subscribed that matches the destination service <code>name</code>.
     *
     * @param name          the destination service name
     * @param request_info  any request metadata
     * @param request       the request data
     * @param timeout       the request timeout in milliseconds
     * @param priority      the priority of this request
     * @return              transaction IDs
     */
    @SuppressWarnings("unchecked")
    public List<TransId> mcast_async(String name,
                                     byte[] request_info, byte[] request,
                                     Integer timeout, Byte priority)
                                     throws InvalidInputException,
                                            MessageDecodingException,
                                            TerminateException
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
            return (List<TransId>) poll_request(null, false);
        }
        catch (OtpErlangRangeException e)
        {
            e.printStackTrace(API.err);
            return null;
        }
    }

    /**
     * Forward a message to another service
     * subscribed that matches the destination service <code>name</code>.
     *
     * @param command       constant API.SYNC or API.ASYNC
     * @param name          the destination service name
     * @param request_info  any request metadata
     * @param request       the request data
     * @param timeout       the request timeout in milliseconds
     * @param priority      the priority of this request
     * @param trans_id      the transaction ID
     * @param pid           the request's source process ID
     */
    public void forward_(Integer command,
                         String name, byte[] request_info, byte[] request,
                         Integer timeout, Byte priority,
                         byte[] trans_id, OtpErlangPid pid)
                         throws ForwardAsyncException,
                                ForwardSyncException,
                                InvalidInputException
    {
        if (command == API.ASYNC)
            forward_async(name, request_info, request,
                          timeout, priority, trans_id, pid);
        else if (command == API.SYNC)
            forward_sync(name, request_info, request,
                         timeout, priority, trans_id, pid);
        else
            throw new InvalidInputException();
    }

    /**
     * Asynchronously forward a message to another service
     * subscribed that matches the destination service <code>name</code>.
     *
     * @param name          the destination service name
     * @param request_info  any request metadata
     * @param request       the request data
     * @param timeout       the request timeout in milliseconds
     * @param priority      the priority of this request
     * @param trans_id      the transaction ID
     * @param pid           the request's source process ID
     */
    public void forward_async(String name, byte[] request_info, byte[] request,
                              Integer timeout, Byte priority,
                              byte[] trans_id, OtpErlangPid pid)
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
                                             new OtpErlangBinary(trans_id),
                                             pid};
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

    /**
     * Synchronously forward a message to another service
     * subscribed that matches the destination service <code>name</code>.
     *
     * @param name          the destination service name
     * @param request_info  any request metadata
     * @param request       the request data
     * @param timeout       the request timeout in milliseconds
     * @param priority      the priority of this request
     * @param trans_id      the transaction ID
     * @param pid           the request's source process ID
     */
    public void forward_sync(String name, byte[] request_info, byte[] request,
                             Integer timeout, Byte priority,
                             byte[] trans_id, OtpErlangPid pid)
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
                                             new OtpErlangBinary(trans_id),
                                             pid};
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

    /**
     * Returns a response from a service request.
     *
     * @param command        constant API.SYNC or API.SYNC
     * @param name           the service name
     * @param pattern        the service name pattern
     * @param response_info  any response metadata
     * @param response       the response data
     * @param timeout        the request timeout in milliseconds
     * @param trans_id       the transaction ID
     * @param pid            the request's source process ID
     */
    public void return_(Integer command,
                        String name, String pattern,
                        byte[] response_info, byte[] response,
                        Integer timeout, byte[] trans_id, OtpErlangPid pid)
                        throws ReturnAsyncException,
                               ReturnSyncException,
                               InvalidInputException
    {
        if (command == API.ASYNC)
            return_async(name, pattern, response_info, response,
                         timeout, trans_id, pid);
        else if (command == API.SYNC)
            return_sync(name, pattern, response_info, response,
                        timeout, trans_id, pid);
        else
            throw new InvalidInputException();
    }

    /**
     * Asynchronously returns a response from a service request.
     *
     * @param name           the service name
     * @param pattern        the service name pattern
     * @param response_info  any response metadata
     * @param response       the response data
     * @param timeout        the request timeout in milliseconds
     * @param trans_id       the transaction ID
     * @param pid            the request's source process ID
     */
    public void return_async(String name, String pattern,
                             byte[] response_info, byte[] response,
                             Integer timeout, byte[] trans_id, OtpErlangPid pid)
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
                                             new OtpErlangBinary(trans_id),
                                             pid};
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

    /**
     * Synchronously returns a response from a service request.
     *
     * @param name           the service name
     * @param pattern        the service name pattern
     * @param response_info  any response metadata
     * @param response       the response data
     * @param timeout        the request timeout in milliseconds
     * @param trans_id       the transaction ID
     * @param pid            the request's source process ID
     */
    public void return_sync(String name, String pattern,
                            byte[] response_info, byte[] response,
                            Integer timeout, byte[] trans_id, OtpErlangPid pid)
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
                                             new OtpErlangBinary(trans_id),
                                             pid};
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

    /**
     * Asynchronously receive a response.
     *
     * @return the response
     */
    public Response recv_async()
                               throws InvalidInputException,
                                      MessageDecodingException,
                                      TerminateException
    {
        return recv_async(this.timeout_sync, TransIdNull, true);
    }

    /**
     * Asynchronously receive a response.
     *
     * @param timeout  the receive timeout in milliseconds
     * @return         the response
     */
    public Response recv_async(Integer timeout)
                               throws InvalidInputException,
                                      MessageDecodingException,
                                      TerminateException
    {
        return recv_async(timeout, TransIdNull, true);
    }

    /**
     * Asynchronously receive a response.
     *
     * @param trans_id the transaction ID to receive
     * @return         the response
     */
    public Response recv_async(byte[] trans_id)
                               throws InvalidInputException,
                                      MessageDecodingException,
                                      TerminateException
    {
        return recv_async(this.timeout_sync, trans_id, true);
    }

    /**
     * Asynchronously receive a response.
     *
     * @param consume  if <code>true</code>, will consume the service request
     *                 so it is not accessible with the same function call in
     *                 the future
     * @return         the response
     */
    public Response recv_async(boolean consume)
                               throws InvalidInputException,
                                      MessageDecodingException,
                                      TerminateException
    {
        return recv_async(this.timeout_sync, TransIdNull, consume);
    }

    /**
     * Asynchronously receive a response.
     *
     * @param timeout  the receive timeout in milliseconds
     * @param trans_id the transaction ID to receive
     * @return         the response
     */
    public Response recv_async(Integer timeout, byte[] trans_id)
                               throws InvalidInputException,
                                      MessageDecodingException,
                                      TerminateException
    {
        return recv_async(timeout, trans_id, true);
    }

    /**
     * Asynchronously receive a response.
     *
     * @param timeout  the receive timeout in milliseconds
     * @param consume  if <code>true</code>, will consume the service request
     *                 so it is not accessible with the same function call in
     *                 the future
     * @return         the response
     */
    public Response recv_async(Integer timeout, boolean consume)
                               throws InvalidInputException,
                                      MessageDecodingException,
                                      TerminateException
    {
        return recv_async(timeout, TransIdNull, consume);
    }

    /**
     * Asynchronously receive a response.
     *
     * @param trans_id the transaction ID to receive
     * @param consume  if <code>true</code>, will consume the service request
     *                 so it is not accessible with the same function call in
     *                 the future
     * @return         the response
     */
    public Response recv_async(byte[] trans_id, boolean consume)
                               throws InvalidInputException,
                                      MessageDecodingException,
                                      TerminateException
    {
        return recv_async(this.timeout_sync, trans_id, consume);
    }

    /**
     * Asynchronously receive a response.
     *
     * @param timeout  the receive timeout in milliseconds
     * @param trans_id the transaction ID to receive
     * @param consume  if <code>true</code>, will consume the service request
     *                 so it is not accessible with the same function call in
     *                 the future
     * @return         the response
     */
    public Response recv_async(Integer timeout, byte[] trans_id,
                               boolean consume)
                               throws InvalidInputException,
                                      MessageDecodingException,
                                      TerminateException
    {
        try
        {
            OtpOutputStream recv_async = new OtpOutputStream();
            recv_async.write(OtpExternal.versionTag);
            final OtpErlangObject[] tuple = {new OtpErlangAtom("recv_async"),
                                             new OtpErlangUInt(timeout),
                                             new OtpErlangBinary(trans_id),
                                             consume ?
                                             new OtpErlangAtom("true") :
                                             new OtpErlangAtom("false")};
            recv_async.write_any(new OtpErlangTuple(tuple));
            send(recv_async);
            return (Response) poll_request(null, false);
        }
        catch (OtpErlangRangeException e)
        {
            e.printStackTrace(API.err);
            return null;
        }
    }

    public int process_index()
    {
        return this.process_index;
    }

    public int process_count()
    {
        return this.process_count;
    }

    public int process_count_max()
    {
        return this.process_count_max;
    }

    public int process_count_min()
    {
        return this.process_count_min;
    }

    public String prefix()
    {
        return this.prefix;
    }

    public int timeout_initialize()
    {
        return this.timeout_initialize;
    }

    public int timeout_async()
    {
        return this.timeout_async;
    }

    public int timeout_sync()
    {
        return this.timeout_sync;
    }

    public int timeout_terminate()
    {
        return this.timeout_terminate;
    }

    private void callback(int command, String name, String pattern,
                          byte[] request_info, byte[] request,
                          Integer timeout, Byte priority,
                          byte[] trans_id, OtpErlangPid pid)
                          throws InvalidInputException,
                                 MessageDecodingException,
                                 TerminateException
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
                                                  trans_id, pid);
                if (response.getClass() == byte[][].class)
                {
                    byte [][] response_array = (byte[][]) response;
                    assert response_array.length == 2 : "invalid response";
                    return_async(name, pattern,
                                 response_array[0],
                                 response_array[1],
                                 timeout, trans_id, pid);
                    
                }
                else if (response.getClass() == byte[].class)
                {
                    return_async(name, pattern,
                                 ("").getBytes(),
                                 (byte[]) response,
                                 timeout, trans_id, pid);
                }
                else
                {
                    return_async(name, pattern,
                                 ("").getBytes(),
                                 response.toString().getBytes(),
                                 timeout, trans_id, pid);
                }
                return;
            }
            catch (InvalidInputException e)
            {
                throw e;
            }
            catch (MessageDecodingException e)
            {
                throw e;
            }
            catch (TerminateException e)
            {
                throw e;
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
                                 timeout, trans_id, pid);
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
                                                  trans_id, pid);
                if (response.getClass() == byte[][].class)
                {
                    byte [][] response_array = (byte[][]) response;
                    assert response_array.length == 2 : "invalid response";
                    return_sync(name, pattern,
                                response_array[0],
                                response_array[1],
                                timeout, trans_id, pid);
                    
                }
                else if (response.getClass() == byte[].class)
                {
                    return_sync(name, pattern,
                                ("").getBytes(),
                                (byte[]) response,
                                timeout, trans_id, pid);
                }
                else
                {
                    return_sync(name, pattern,
                                ("").getBytes(),
                                response.toString().getBytes(),
                                timeout, trans_id, pid);
                }
                return;
            }
            catch (InvalidInputException e)
            {
                throw e;
            }
            catch (MessageDecodingException e)
            {
                throw e;
            }
            catch (TerminateException e)
            {
                throw e;
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
                                timeout, trans_id, pid);
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

    private boolean handle_events(boolean external,
                                  ByteBuffer buffer)
                                  throws MessageDecodingException,
                                         TerminateException
    {
        return handle_events(external, buffer, 0);
    }

    private boolean handle_events(boolean external,
                                  ByteBuffer buffer,
                                  int command)
                                  throws MessageDecodingException,
                                         TerminateException,
                                         BufferUnderflowException
    {
        if (command == 0)
        {
            command = buffer.getInt();
        }
        while (true)
        {
            switch (command)
            {
                case MESSAGE_TERM:
                {
                    this.terminate = true;
                    if (external)
                        return false;
                    else
                        throw new TerminateException(this.timeout_terminate);
                }
                case MESSAGE_REINIT:
                {
                    this.process_count = buffer.getInt();
                    break;
                }
                case MESSAGE_KEEPALIVE:
                {
                    OtpOutputStream keepalive = new OtpOutputStream();
                    keepalive.write(OtpExternal.versionTag);
                    keepalive.write_any(new OtpErlangAtom("keepalive"));
                    send(keepalive);
                    break;
                }
                default:
                    throw new MessageDecodingException();
            }
            if (! buffer.hasRemaining())
                return true;
            command = buffer.getInt();
        }
    }

    private Object poll_request(Integer timeout, boolean external)
                                throws InvalidInputException,
                                       MessageDecodingException,
                                       TerminateException
    {
        if (this.terminate)
        {
            return Boolean.FALSE;
        }
        else if (external && ! this.initialization_complete)
        {
            OtpOutputStream polling = new OtpOutputStream();
            polling.write(OtpExternal.versionTag);
            polling.write_any(new OtpErlangAtom("polling"));
            send(polling);
            this.initialization_complete = true;
        }
        final int timeout_min = 10;
        Integer timeout_value = null;
        Long poll_timer = null;
        if (timeout == null || timeout < 0)
        {
            // blocking on read without a timeout
        }
        else if (timeout >= 0)
        {
            poll_timer = System.nanoTime();
            timeout_value = java.lang.Math.max(timeout_min, timeout);
        }

        try
        {
            ByteBuffer buffer = null;
            buffer = recv(buffer, timeout_value);
            if (buffer == null)
                return Boolean.TRUE;
            while (true)
            {
                int command;
                switch (command = buffer.getInt())
                {
                    case MESSAGE_INIT:
                    {
                        this.process_index = buffer.getInt();
                        this.process_count = buffer.getInt();
                        this.process_count_max = buffer.getInt();
                        this.process_count_min = buffer.getInt();
                        int prefix_size = buffer.getInt();
                        this.prefix = API.getString(buffer, prefix_size);
                        this.timeout_initialize = buffer.getInt();
                        this.timeout_async = buffer.getInt();
                        this.timeout_sync = buffer.getInt();
                        this.timeout_terminate = buffer.getInt();
                        this.priority_default = buffer.get();
                        this.request_timeout_adjustment =
                            (buffer.get() & 0xFF) != 0;
                        if (buffer.hasRemaining())
                        {
                            assert ! external;
                            handle_events(external, buffer);
                        }
                        return Boolean.FALSE;
                    }
                    case MESSAGE_SEND_ASYNC:
                    case MESSAGE_SEND_SYNC:
                    {
                        int name_size = buffer.getInt();
                        String name = API.getString(buffer, name_size);
                        int pattern_size = buffer.getInt();
                        String pattern = API.getString(buffer, pattern_size);
                        int request_info_size = buffer.getInt();
                        byte[] request_info = API.getBytes(buffer,
                                                           request_info_size);
                        buffer.get();
                        int request_size = buffer.getInt();
                        byte[] request = API.getBytes(buffer, request_size);
                        buffer.get();
                        int request_timeout = buffer.getInt();
                        byte priority = buffer.get();
                        byte[] trans_id = API.getBytes(buffer, 16);
                        int pid_size = buffer.getInt();
                        OtpErlangPid pid = API.getPid(buffer, pid_size);
                        if (buffer.hasRemaining())
                        {
                            assert external;
                            if (! handle_events(external, buffer))
                                return Boolean.FALSE;
                        }
                        callback(command, name, pattern, request_info, request,
                                 request_timeout, priority, trans_id, pid);
                        break;
                    }
                    case MESSAGE_RECV_ASYNC:
                    case MESSAGE_RETURN_SYNC:
                    {
                        int response_info_size = buffer.getInt();
                        byte[] response_info = API.getBytes(buffer,
                                                            response_info_size);
                        buffer.get();
                        int response_size = buffer.getInt();
                        byte[] response = API.getBytes(buffer, response_size);
                        buffer.get();
                        byte[] trans_id = API.getBytes(buffer, 16);
                        if (buffer.hasRemaining())
                        {
                            assert ! external;
                            handle_events(external, buffer);
                        }
                        return new Response(response_info, response, trans_id);
                    }
                    case MESSAGE_RETURN_ASYNC:
                    {
                        byte[] trans_id = API.getBytes(buffer, 16);
                        if (buffer.hasRemaining())
                        {
                            assert ! external;
                            handle_events(external, buffer);
                        }
                        return new TransId(trans_id);
                    }
                    case MESSAGE_RETURNS_ASYNC:
                    {
                        int trans_id_count = buffer.getInt();
                        List<TransId> trans_ids = new ArrayList<TransId>();
                        for (int i = 0; i < trans_id_count; ++i)
                        {
                            byte[] trans_id = API.getBytes(buffer, 16);
                            trans_ids.add(new TransId(trans_id));
                        }
                        if (buffer.hasRemaining())
                        {
                            assert ! external;
                            handle_events(external, buffer);
                        }
                        return trans_ids;
                    }
                    case MESSAGE_SUBSCRIBE_COUNT:
                    {
                        int count = buffer.getInt();
                        if (buffer.hasRemaining())
                        {
                            assert ! external;
                            handle_events(external, buffer);
                        }
                        return count;
                    }
                    case MESSAGE_TERM:
                    {
                        if (! handle_events(external, buffer, command))
                            return Boolean.FALSE;
                        assert false;
                        break;
                    }
                    case MESSAGE_REINIT:
                    {
                        this.process_count = buffer.getInt();
                        if (buffer.hasRemaining())
                            continue;
                        break;
                    }
                    case MESSAGE_KEEPALIVE:
                    {
                        OtpOutputStream keepalive = new OtpOutputStream();
                        keepalive.write(OtpExternal.versionTag);
                        keepalive.write_any(new OtpErlangAtom("keepalive"));
                        send(keepalive);
                        if (buffer.hasRemaining())
                            continue;
                        break;
                    }
                    default:
                        throw new MessageDecodingException();
                }
                if (poll_timer != null)
                {
                    long poll_timer_new = System.nanoTime();
                    final int elapsed = (int) java.lang.Math.max(0,
                        ((poll_timer_new - poll_timer) * 1e-6));
                    poll_timer = poll_timer_new;
                    if (elapsed >= timeout)
                        timeout = 0;
                    else
                        timeout -= elapsed;
                }
                if (timeout_value != null)
                {
                    if (timeout == 0)
                        return Boolean.TRUE;
                    else if (timeout > 0)
                        timeout_value = java.lang.Math.max(timeout_min,
                                                           timeout);
                }
                buffer = recv(buffer, timeout_value);
                if (buffer == null)
                    return Boolean.TRUE;
            }
        }
        catch (IOException e)
        {
            e.printStackTrace(API.err);
            throw new MessageDecodingException();
        }
        catch (BufferUnderflowException e)
        {
            throw new MessageDecodingException();
        }
    }

    public Object poll() throws InvalidInputException,
                                MessageDecodingException,
                                TerminateException
    {
        return poll(-1);
    }

    public Object poll(int timeout) throws InvalidInputException,
                                           MessageDecodingException,
                                           TerminateException
    {
        return poll_request(timeout, true);
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

    private ByteBuffer recv(ByteBuffer buffer_in, Integer timeout)
                            throws IOException
    {
        final ByteArrayOutputStream output =
            new ByteArrayOutputStream(this.buffer_size);
        int i = 0;
        if (buffer_in != null && buffer_in.hasRemaining())
        {
            i += buffer_in.limit() - buffer_in.position();
            output.write(buffer_in.array(),
                         buffer_in.position(),
                         buffer_in.limit());
        }
        int read = 0;
        final byte[] bytes = new byte[this.buffer_size];
        boolean consume;
        if (i == 0 && timeout != null)
        {
            // simulate poll
            final FileInputStream input = this.input;
            Callable<Boolean> poll_timer_task = new Callable<Boolean>()
            {
                @Override
                public Boolean call() throws Exception
                {
                    while (input.available() == 0)
                        Thread.sleep(10);
                    return Boolean.TRUE;
                }
            };
            Future<Boolean> poll_timer_future =
                poll_timer_executor.submit(poll_timer_task);
            try
            {
                poll_timer_future.get(timeout, TimeUnit.MILLISECONDS);
            }
            catch (TimeoutException e)
            {
                return null;
            }
            catch (Exception e)
            {
                e.printStackTrace(API.err);
                throw new IOException("poll exception");
            }
        }
        if (this.use_header)
            consume = (i < 4);
        else
            consume = (i < this.buffer_size);
        while (consume)
        {
            while ((read = this.input.read(bytes)) == this.buffer_size &&
                   this.input.available() > 0)
            {
                i += this.buffer_size;
                output.write(bytes, 0, this.buffer_size);
            }
            if (read == -1)
            {
                throw new IOException("consume read eof");
            }
            else if (read > 0)
            {
                i += read;
                output.write(bytes, 0, read);
            }
            if (this.use_header == false)
                consume = false;
            else if (i >= 4)
                consume = false;
        }
        final byte[] result = output.toByteArray();
        ByteBuffer buffer_out = null;
        if (this.use_header)
        {
            final int length = ((result[0] & 0xff) << 24) |
                               ((result[1] & 0xff) << 16) |
                               ((result[2] & 0xff) <<  8) |
                                (result[3] & 0xff);
            if (length < 0)
                throw new IOException("negative length");
            i -= 4;
            if (i < length)
            {
                buffer_out = ByteBuffer.allocate(length);
                buffer_out.put(result, 4, i);
                while (i < length)
                {
                    read = this.input.read(bytes, 0,
                                      Math.min(length - i, this.buffer_size));
                    if (read == -1)
                    {
                        throw new IOException("remaining read eof");
                    }
                    else if (read > 0)
                    {
                        i += read;
                        buffer_out.put(bytes, 0, read);
                    }
                }
                buffer_out.rewind();
            }
            else
            {
                buffer_out = ByteBuffer.wrap(result, 4, i);
            }
        }
        else
        {
            buffer_out = ByteBuffer.wrap(result);
        }
        buffer_out.order(ByteOrder.nativeOrder());
        return buffer_out;
    }

    private static String getString(ByteBuffer buffer, final int size)
    {
        final String value = new String(API.getBytes(buffer, size - 1));
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
        final byte[] data = new byte[size];
        buffer.get(data, 0, size);
        return data;
    }

    private static FileDescriptor storeFD(final int fd)
    {
        Class<FileDescriptor> clazz = FileDescriptor.class;

        Constructor<FileDescriptor> c;
        try
        {
            Class<?>[] intarg = { Integer.TYPE };
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

        Response(byte[] info, byte[] resp, byte[] trans_id)
        {
            this.response_info = info;
            this.response = resp;
            this.id = trans_id;
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

        TransId(byte[] trans_id)
        {
            this.id = trans_id;
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

    public static class TerminateException extends Exception
    {
        private static final long serialVersionUID = 0L;
        private int timeout;
        TerminateException(final int timeout)
        {
            super("Terminate");
            this.timeout = timeout;
        }
        public int timeout()
        {
            return this.timeout;
        }
    }

}

