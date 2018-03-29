//-*-Mode:C++;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=cpp fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
//
// MIT License
//
// Copyright (c) 2009-2017 Michael Truog <mjtruog at protonmail dot com>
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

#include "timer.hpp"

#if HAVE_CLOCK_GETTIME_MONOTONIC

timer::timer()
{
    ::clock_gettime(CLOCK_MONOTONIC, &m_start);
}

void timer::restart()
{
    ::clock_gettime(CLOCK_MONOTONIC, &m_start);
}

double timer::elapsed() const
{
    struct timespec end;
    ::clock_gettime(CLOCK_MONOTONIC, &end);
    return (static_cast<double>(end.tv_sec - m_start.tv_sec) +
            static_cast<double>(end.tv_nsec - m_start.tv_nsec) * 1.0e-9);
}

#else

timer::timer()
{
    ::gettimeofday(&m_start, 0);
}

void timer::restart()
{
    ::gettimeofday(&m_start, 0);
}

double timer::elapsed() const
{
    struct timeval end;
    ::gettimeofday(&end, 0);
    return (static_cast<double>(end.tv_sec - m_start.tv_sec) +
            static_cast<double>(end.tv_usec - m_start.tv_usec) * 1.0e-6);
}

#endif

