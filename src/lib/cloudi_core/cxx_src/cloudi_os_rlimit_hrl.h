// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
// ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab:
//
// MIT License
//
// Copyright (c) 2015-2017 Michael Truog <mjtruog at gmail dot com>
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

-define(OS_RLIMIT_DEFAULTS,
#ifdef HAVE_SYS_RESOURCE_H_RLIMIT_AS
        [{as, undefined}] ++
#endif
#ifdef HAVE_SYS_RESOURCE_H_RLIMIT_CORE
        [{core, undefined}] ++
#endif
#ifdef HAVE_SYS_RESOURCE_H_RLIMIT_CPU
        [{cpu, undefined}] ++ 
#endif
#ifdef HAVE_SYS_RESOURCE_H_RLIMIT_DATA
        [{data, undefined}] ++
#endif
#ifdef HAVE_SYS_RESOURCE_H_RLIMIT_FSIZE
        [{fsize, undefined}] ++
#endif
#ifdef HAVE_SYS_RESOURCE_H_RLIMIT_MEMLOCK
        [{memlock, undefined}] ++
#endif
#ifdef HAVE_SYS_RESOURCE_H_RLIMIT_MSGQUEUE
        [{msgqueue, undefined}] ++
#endif
#ifdef HAVE_SYS_RESOURCE_H_RLIMIT_NICE
        [{nice, undefined}] ++
#endif
#ifdef HAVE_SYS_RESOURCE_H_RLIMIT_NOFILE
        [{nofile, undefined}] ++
#endif
#ifdef HAVE_SYS_RESOURCE_H_RLIMIT_NPROC
        [{nproc, undefined}] ++
#endif
#ifdef HAVE_SYS_RESOURCE_H_RLIMIT_RSS
        [{rss, undefined}] ++
#endif
#ifdef HAVE_SYS_RESOURCE_H_RLIMIT_RTPRIO
        [{rtprio, undefined}] ++
#endif
#ifdef HAVE_SYS_RESOURCE_H_RLIMIT_RTTIME
        [{rttime, undefined}] ++
#endif
#ifdef HAVE_SYS_RESOURCE_H_RLIMIT_SIGPENDING
        [{sigpending, undefined}] ++
#endif
#ifdef HAVE_SYS_RESOURCE_H_RLIMIT_STACK
        [{stack, undefined}] ++
#endif
#ifdef HAVE_SYS_RESOURCE_H_RLIMIT_VMEM
        [{vmem, undefined}] ++
#endif
        []).

