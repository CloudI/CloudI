//-*-Mode:C++;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
// ex: set ft=cpp fenc=utf-8 sts=4 ts=4 sw=4 et:
//
// BSD LICENSE
// 
// Copyright (c) 2015, Michael Truog <mjtruog at gmail dot com>
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

#include <sys/time.h>
#include <sys/resource.h>
#include <cstdlib>
#include <cstdio>
#include "cloudi_os_rlimit.hpp"
#include "assert.hpp"

// same defines used in the Erlang source code

// limit type
#define CLOUDI_RLIMIT_AS                1
#define CLOUDI_RLIMIT_CORE              2
#define CLOUDI_RLIMIT_CPU               3
#define CLOUDI_RLIMIT_DATA              4
#define CLOUDI_RLIMIT_FSIZE             5
#define CLOUDI_RLIMIT_MEMLOCK           6
#define CLOUDI_RLIMIT_MSGQUEUE          7
#define CLOUDI_RLIMIT_NICE              8
#define CLOUDI_RLIMIT_NOFILE            9
#define CLOUDI_RLIMIT_NPROC            10
#define CLOUDI_RLIMIT_RSS              11
#define CLOUDI_RLIMIT_RTPRIO           12
#define CLOUDI_RLIMIT_RTTIME           13
#define CLOUDI_RLIMIT_SIGPENDING       14
#define CLOUDI_RLIMIT_STACK            15
#define CLOUDI_RLIMIT_VMEM             16

// limit combination
#define CLOUDI_RLIMITS_CURRENT_ONLY     1
#define CLOUDI_RLIMITS_MAXIMUM_ONLY     2
#define CLOUDI_RLIMITS_CURRENT_MAXIMUM  3

// limit special values
#define CLOUDI_RLIMITS_VALUE_INFINITY   0xffffffffffffffff

static int type_value(uint8_t const value)
{
    switch (value)
    {
#ifdef RLIMIT_AS
        case CLOUDI_RLIMIT_AS:
            return RLIMIT_AS;
#endif
#ifdef RLIMIT_CORE
        case CLOUDI_RLIMIT_CORE:
            return RLIMIT_CORE;
#endif
#ifdef RLIMIT_CPU
        case CLOUDI_RLIMIT_CPU:
            return RLIMIT_CPU;
#endif
#ifdef RLIMIT_DATA
        case CLOUDI_RLIMIT_DATA:
            return RLIMIT_DATA;
#endif
#ifdef RLIMIT_FSIZE
        case CLOUDI_RLIMIT_FSIZE:
            return RLIMIT_FSIZE;
#endif
#ifdef RLIMIT_MEMLOCK
        case CLOUDI_RLIMIT_MEMLOCK:
            return RLIMIT_MEMLOCK;
#endif
#ifdef RLIMIT_MSGQUEUE
        case CLOUDI_RLIMIT_MSGQUEUE:
            return RLIMIT_MSGQUEUE;
#endif
#ifdef RLIMIT_NICE
        case CLOUDI_RLIMIT_NICE:
            return RLIMIT_NICE;
#endif
#ifdef RLIMIT_NOFILE
        case CLOUDI_RLIMIT_NOFILE:
            return RLIMIT_NOFILE;
#endif
#ifdef RLIMIT_NPROC
        case CLOUDI_RLIMIT_NPROC:
            return RLIMIT_NPROC;
#endif
#ifdef RLIMIT_RSS
        case CLOUDI_RLIMIT_RSS:
            return RLIMIT_RSS;
#endif
#ifdef RLIMIT_RTPRIO
        case CLOUDI_RLIMIT_RTPRIO:
            return RLIMIT_RTPRIO;
#endif
#ifdef RLIMIT_RTTIME
        case CLOUDI_RLIMIT_RTTIME:
            return RLIMIT_RTTIME;
#endif
#ifdef RLIMIT_SIGPENDING
        case CLOUDI_RLIMIT_SIGPENDING:
            return RLIMIT_SIGPENDING;
#endif
#ifdef RLIMIT_STACK
        case CLOUDI_RLIMIT_STACK:
            return RLIMIT_STACK;
#endif
#ifdef RLIMIT_VMEM
        case CLOUDI_RLIMIT_VMEM:
            return RLIMIT_VMEM;
#endif
        default:
            assert(false);
            return -1;
    }
}

static rlim_t rlimit_value(uint64_t const value)
{
    if (value == CLOUDI_RLIMITS_VALUE_INFINITY)
        return RLIM_INFINITY;
    else
        return static_cast<rlim_t>(value);
}

static int rlimit_current_get(int const type, struct rlimit & output)
{
    struct rlimit value;
    int result = ::getrlimit(type, &value);
    if (result == 0)
    {
        output.rlim_cur = value.rlim_cur;
    }
    return result;
}

static int rlimit_maximum_get(int const type, struct rlimit & output)
{
    struct rlimit value;
    int result = ::getrlimit(type, &value);
    if (result == 0)
    {
        output.rlim_max = value.rlim_max;
    }
    return result;
}

int rlimit(char * rlimits, uint32_t rlimits_len)
{
    int result = 0;
    for (size_t i = 0; i < rlimits_len; )
    {
        struct rlimit element;
        result = 0;
        uint8_t const type = *reinterpret_cast<uint8_t *>(&rlimits[i++]);
        uint8_t const format = *reinterpret_cast<uint8_t *>(&rlimits[i++]);
        int const resource_type = type_value(type);
        if (resource_type == -1)
            return -1;
        switch (format)
        {
            case CLOUDI_RLIMITS_CURRENT_ONLY:
                element.rlim_cur = rlimit_value(
                    *reinterpret_cast<uint64_t *>(&rlimits[i]));
                i += sizeof(uint64_t);
                result = rlimit_maximum_get(resource_type, element);
                break;
            case CLOUDI_RLIMITS_MAXIMUM_ONLY:
                element.rlim_max = rlimit_value(
                    *reinterpret_cast<uint64_t *>(&rlimits[i]));
                i += sizeof(uint64_t);
                result = rlimit_current_get(resource_type, element);
                break;
            case CLOUDI_RLIMITS_CURRENT_MAXIMUM:
                element.rlim_cur = rlimit_value(
                    *reinterpret_cast<uint64_t *>(&rlimits[i]));
                i += sizeof(uint64_t);
                element.rlim_max = rlimit_value(
                    *reinterpret_cast<uint64_t *>(&rlimits[i]));
                i += sizeof(uint64_t);
                break;
            default:
                assert(false);
                return -1;
        }
        if (result)
        {
            ::perror("getrlimit: ");
            return result;
        }
        result = ::setrlimit(resource_type, &element);
        if (result)
        {
            ::perror("setrlimit: ");
            return result;
        }
    }
    return 0;
}

