//-*-Mode:java;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=java fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
//
// MIT License
//
// Copyright (c) 2012-2017 Michael Truog <mjtruog at gmail dot com>
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

import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;

public class Function9<C1, C2, C3, C4, C5, C6, C7, C8, C9>
{
    private final Object instance;
    private final String methodName;
    private Method method;

    public Function9(final Object instance, final String methodName)
        throws NoSuchMethodException
    {
        this.instance = instance;
        this.methodName = methodName;

        // make sure the method exists with the same arity
        // (unable to type check due to generic type usage in Function9)
        final Method[] methods = this.instance.getClass()
                                              .getMethods();
        boolean exists = false;
        for (Method possibleMethod : methods)
        {
            if (possibleMethod.getName().equals(this.methodName) &&
                possibleMethod.getParameterTypes().length == 9)
            {
                exists = true;
                break;
            }
        }
        if (! exists)
        {
            throw new NoSuchMethodException(this.instance.getClass()
                                                         .getCanonicalName() +
                                            "." + this.methodName + "(...)");
        }
    }

    public Object invoke(C1 a1, C2 a2, C3 a3, C4 a4, C5 a5,
                         C6 a6, C7 a7, C8 a8, C9 a9)
        throws Throwable
    {
        try
        {
            if (this.method == null)
            {
                this.method = this.instance
                                  .getClass()
                                  .getDeclaredMethod(this.methodName,
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
            return this.method.invoke(this.instance,
                                      a1, a2, a3, a4, a5, a6, a7, a8, a9);
        }
        catch (InvocationTargetException e)
        {
            throw e.getTargetException();
        }
    }
}

