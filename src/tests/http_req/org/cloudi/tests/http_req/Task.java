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

package org.cloudi.tests.http_req;

import java.util.HashMap;
import com.ericsson.otp.erlang.OtpErlangPid;
import org.cloudi.API;

public class Task implements Runnable
{
    private API api;
     
    public Task(final int index, final String protocol, final int buffer_size)
    {
        api = new API(index, protocol, buffer_size);
    }

    public void request(Integer command, String name,
                        byte[] requestInfo, byte[] request,
                        Integer timeout, Byte priority,
                        byte[] transId, OtpErlangPid pid)
                        throws API.ReturnAsyncException, API.ReturnSyncException
    {
        HashMap<String, String> http_qs = api.request_http_qs_parse(request);
        final String value = http_qs.remove("value");
        String response;
        if (value == null)
        {
            response =
                "<http_test><error>no value specified</error></http_test>";
        }
        else
        {
            final long value_long = Long.parseLong(value, 10);
            response =
                "<http_test><value>" +
                Long.toString(value_long) +
                "</value></http_test>";
        }
        api.return_(command, name,
                    ("").getBytes(), response.getBytes(),
                    timeout, transId, pid);
    }
 
    public void run()
    {
        api.subscribe("java.xml/get", this, "request");
        boolean running = true;
        while (running)
        {
            Object result = api.poll();
            if (result == null)
                running = false;
            else
                System.out.println("(java) received: " + result.toString());
        }
        System.out.println("exited thread");
    }
}

