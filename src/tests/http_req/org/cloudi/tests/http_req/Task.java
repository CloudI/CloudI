//-*-Mode:java;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=java fenc=utf-8 sts=4 ts=4 sw=4 et:
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

package org.cloudi.tests.http_req;

import java.util.HashMap;
import java.util.List;
import com.ericsson.otp.erlang.OtpErlangPid;
import org.cloudi.API;

public class Task implements Runnable
{
    private API api;
     
    public Task(final int thread_index)
                throws API.InvalidInputException,
                       API.MessageDecodingException
    {
        api = new API(thread_index);
    }

    public void request(Integer command, String name, String pattern,
                        byte[] request_info, byte[] request,
                        Integer timeout, Byte priority,
                        byte[] trans_id, OtpErlangPid pid)
                        throws API.ReturnAsyncException,
                               API.ReturnSyncException,
                               API.InvalidInputException
    {
        HashMap<String, List<String> > http_qs =
            api.request_http_qs_parse(request);
        final List<String> value = http_qs.remove("value");
        String response = null;
        if (value != null)
        {
            try
            {
                final long value_long = Long.parseLong(value.get(0), 10);
                response =
                    "<http_test><value>" +
                    Long.toString(value_long) +
                    "</value></http_test>";
            }
            catch (java.lang.NumberFormatException e)
            {
                response = null;
            }
        }
        if (response == null)
        {
            response =
                "<http_test><error>no value specified</error></http_test>";
        }
        api.return_(command, name, pattern,
                    ("").getBytes(), response.getBytes(),
                    timeout, trans_id, pid);
    }
 
    public void run()
    {
        api.subscribe("java.xml/get", this, "request");
        boolean running = true;
        while (running)
        {
            try
            {
                Object result = api.poll();
                if (result == null)
                    running = false;
                else
                    API.out.println("(java) received: " + result.toString());
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

