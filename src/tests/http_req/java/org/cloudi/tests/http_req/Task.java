//-*-Mode:java;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=java fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
//
// MIT License
//
// Copyright (c) 2011-2019 Michael Truog <mjtruog at protonmail dot com>
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

package org.cloudi.tests.http_req;

import java.util.HashMap;
import java.util.ArrayList;
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
            API.err.println("terminate http_req java (before init)");
            System.exit(1);
        }
    }

    public Object request(Integer request_type, String name, String pattern,
                          byte[] request_info, byte[] request,
                          Integer timeout, Byte priority,
                          byte[] trans_id, OtpErlangPid pid)
        throws API.ReturnAsyncException,
               API.ReturnSyncException,
               API.InvalidInputException
    {
        HashMap<String, ArrayList<String>> http_qs =
            this.api.info_key_value_parse(request);
        final ArrayList<String> value = http_qs.remove("value");
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
        this.api.return_(request_type, name, pattern,
                         ("").getBytes(), response.getBytes(),
                         timeout, trans_id, pid);
        return null;
    }
 
    public void run()
    {
        try
        {
            assert this.api.subscribe_count("java.xml/get") == 0;

            // possible with Java >= 8
            //this.api.subscribe("java.xml/get", this::request);

            // required with Java < 8
            this.api.subscribe("java.xml/get", this, "request");

            assert this.api.subscribe_count("java.xml/get") == 1;
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
        API.out.println("terminate http_req java");
    }
}

