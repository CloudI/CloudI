//-*-Mode:java;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=java fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
//
// BSD LICENSE
// 
// Copyright (c) 2012, Michael Truog <mjtruog at gmail dot com>
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

import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;

public class Function9<C1, C2, C3, C4, C5, C6, C7, C8, C9>
{
    private Object instance;
    private String methodName;
    private Method method;

    public Function9(final Object instance, final String methodName)
    {
        this.instance = instance;
        this.methodName = methodName;
        this.method = null;
    }

    public Object invoke(C1 a1, C2 a2, C3 a3, C4 a4, C5 a5, C6 a6,
                         C7 a7, C8 a8, C9 a9)
                         throws Throwable,
                                IllegalAccessException,
                                NoSuchMethodException
    {
        try
        {
            if (this.method == null)
            {
                this.method = this.instance
                                  .getClass()
                                  .getDeclaredMethod(methodName,
                                                     a1.getClass(),
                                                     a2.getClass(),
                                                     a3.getClass(),
                                                     a4.getClass(),
                                                     a5.getClass(),
                                                     a6.getClass(),
                                                     a7.getClass(),
                                                     a8.getClass(),
                                                     a9.getClass());
            }
            return this.method.invoke(this.instance, a1, a2, a3, a4, a5, a6,
                                      a7, a8, a9);
        }
        catch (InvocationTargetException e)
        {
            throw e.getTargetException();
        }
    }
}

