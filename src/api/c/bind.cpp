//-*-Mode:C++;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=cpp fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
//
// MIT License
//
// Copyright (c) 2023 Michael Truog <mjtruog at protonmail dot com>
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


#include "config.h"
#if BIND_USE_LINUX

#ifndef _GNU_SOURCE
#define _GNU_SOURCE
#endif
#include <sched.h>
#undef _GNU_SOURCE

#elif BIND_USE_FREEBSD

#include <sys/param.h>
#include <sys/cpuset.h>

#elif BIND_USE_PTHREAD

#if defined(__DragonFly__)
#include <pthread.h>
#include <pthread_np.h>
#elif defined(__NetBSD__)
#include <pthread.h>
#include <sched.h>
#define CPU_ZERO(p) cpuset_zero(p)
#define CPU_SET(p) (void)cpuset_set(p)
#else
#error "unsupported operating system!"
#endif

#endif
#include "bind.hpp"

namespace
{

#if BIND_USE_LINUX
    int bind_set_linux(uint32_t const logical_processor)
    {
        cpu_set_t cpuset;
        CPU_ZERO(&cpuset);
        CPU_SET(logical_processor, &cpuset);

        return ::sched_setaffinity(0, sizeof(cpuset), &cpuset);
    }
#elif BIND_USE_FREEBSD
    int bind_set_freebsd(uint32_t const logical_processor)
    {
        cpuset_t cpuset;
        CPU_ZERO(&cpuset);
        CPU_SET(logical_processor, &cpuset);
        return ::cpuset_setaffinity(CPU_LEVEL_WHICH, CPU_WHICH_TID, -1,
                                    sizeof(cpuset), &cpuset);
    }
#elif BIND_USE_PTHREAD
    int bind_set_pthread(uint32_t const logical_processor)
    {
        cpuset_t cpuset;
        CPU_ZERO(&cpuset);
        CPU_SET(logical_processor, &cpuset);
        return ::pthread_setaffinity_np(pthread_self(),
                                        sizeof(cpuset), &cpuset);
    }
#endif

} // anonymous namespace

int bind_set(int32_t const logical_processor)
{
    int result = 0;
    if (logical_processor >= 0)
    {
#if BIND_USE_LINUX
        result = bind_set_linux(static_cast<uint32_t>(logical_processor));
#elif BIND_USE_FREEBSD
        result = bind_set_freebsd(static_cast<uint32_t>(logical_processor));
#elif BIND_USE_PTHREAD
        result = bind_set_pthread(static_cast<uint32_t>(logical_processor));
#endif
    }
    return result;
}

