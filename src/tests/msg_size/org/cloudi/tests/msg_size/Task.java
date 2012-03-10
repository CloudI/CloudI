// -*- coding: utf-8; Mode: java; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
// ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
//
// BSD LICENSE
// 
// Copyright (c) 2011-2012, Michael Truog <mjtruog at gmail dot com>
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

package org.cloudi.tests.msg_size;

import java.nio.ByteBuffer;
import java.nio.ByteOrder;
import com.ericsson.otp.erlang.OtpErlangPid;
import org.cloudi.API;

public class Task implements Runnable
{
    private API api;
    private static final String DESTINATION = "/tests/msg_size/erlang";
     
    public Task(final int thread_index)
                throws API.InvalidInputException,
                       API.MessageDecodingException
    {
        api = new API(thread_index);
    }

    public void request(Integer command, String name, String pattern,
                        byte[] requestInfo, byte[] request,
                        Integer timeout, Byte priority,
                        byte[] transId, OtpErlangPid pid)
                        throws API.ReturnAsyncException,
                               API.ReturnSyncException,
                               API.InvalidInputException
    {
        ByteBuffer buffer = ByteBuffer.wrap(request);
        buffer.order(ByteOrder.nativeOrder());
        int i = buffer.getInt(0);
        if (i == 4294967295L)
            i = 0;
        else
            i++;
        buffer.putInt(0, i);
        API.out.printf("forward #%d to %s (with timeout %d ms)\n",
                       i, Task.DESTINATION, timeout);
        api.forward_(command, Task.DESTINATION,
                     requestInfo, request,
                     timeout, priority, transId, pid);
    }
 
    public void run()
    {
        api.subscribe("java", this, "request");
        boolean running = true;
        while (running)
        {
            try
            {
                Object result = api.poll();
                if (result == null)
                    running = false;
            }
            catch (API.MessageDecodingException e)
            {
                e.printStackTrace(API.err);
                running = false;
            }
        }
        API.err.println("exited thread");
    }
}

