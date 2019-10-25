//-*-Mode:java;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=java fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
//
// MIT License
//
// Copyright (c) 2016-2019 Michael Truog <mjtruog at protonmail dot com>
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

package org.cloudi;

import com.ericsson.otp.erlang.OtpErlangPid;

public class FunctionObject9 implements FunctionInterface9
{
    private final Function9<Integer,
                            String,
                            String,
                            byte[],
                            byte[],
                            Integer,
                            Byte,
                            byte[],
                            OtpErlangPid> function;

    public FunctionObject9(final Object instance,
                           final String methodName)
        throws NoSuchMethodException
    {
        // object instance method
        this.function = new Function9<Integer,
                                      String,
                                      String,
                                      byte[],
                                      byte[],
                                      Integer,
                                      Byte,
                                      byte[],
                                      OtpErlangPid>(instance, methodName);
    }

    public FunctionObject9(final API api,
                           final Class<?> clazz,
                           final String methodName)
        throws NoSuchMethodException
    {
        // class static method
        this.function = new Function9<Integer,
                                      String,
                                      String,
                                      byte[],
                                      byte[],
                                      Integer,
                                      Byte,
                                      byte[],
                                      OtpErlangPid>(api, clazz, methodName);
    }

    public Object invoke(Integer request_type, String name, String pattern,
                         byte[] request_info, byte[] request,
                         Integer timeout, Byte priority,
                         byte[] trans_id, OtpErlangPid pid)
        throws Throwable
    {
        return this.function.invoke(request_type, name, pattern,
                                    request_info, request,
                                    timeout, priority,
                                    trans_id, pid);
    }
}

