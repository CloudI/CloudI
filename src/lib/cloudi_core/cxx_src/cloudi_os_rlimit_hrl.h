// -*- Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
// ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab:
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

