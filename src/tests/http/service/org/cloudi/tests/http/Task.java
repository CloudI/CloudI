//-*-Mode:java;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=java fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
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

package org.cloudi.tests.http;

import com.ericsson.otp.erlang.OtpErlangPid;
import org.cloudi.API;

public class Task implements Runnable
{
    private API api;
     
    public Task(final int thread_index)
    {
        try
        {
            this.api = new API(thread_index);
        }
        catch (API.InvalidInputException e)
        {
            e.printStackTrace(API.err);
            System.exit(1);
        }
        catch (API.MessageDecodingException e)
        {
            e.printStackTrace(API.err);
            System.exit(1);
        }
        catch (API.TerminateException e)
        {
            API.err.println("terminate http java (before init)");
            System.exit(1);
        }
    }

    public void text(Integer command, String name, String pattern,
                     byte[] request_info, byte[] request,
                     Integer timeout, Byte priority,
                     byte[] trans_id, OtpErlangPid pid)
                     throws API.ReturnAsyncException,
                            API.ReturnSyncException,
                            API.InvalidInputException
    {
        final String value = new String(request);
        API.out.println("(" + value + ")");
        assert "Test Text" == value : value;
        this.api.return_(command, name, pattern,
                         ("").getBytes(), ("Test Response").getBytes(),
                         timeout, trans_id, pid);
    }
 
    public void run()
    {
        try
        {
            this.api.subscribe("text/post", this, "text");
            Object result = this.api.poll();
            assert result == Boolean.FALSE;
        }
        catch (API.TerminateException e)
        {
        }
        catch (Exception e)
        {
            e.printStackTrace(API.err);
        }
        API.out.println("terminate http java");
    }
}

