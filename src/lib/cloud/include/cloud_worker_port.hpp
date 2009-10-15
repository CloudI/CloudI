// -*- coding: utf-8; Mode: C++; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
// ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
//
// BSD LICENSE
// 
// Copyright (c) 2009, Michael Truog
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
#ifndef CLOUD_WORKER_PORT_H
#define CLOUD_WORKER_PORT_H

// GEP[D] (Generic Erlang Port [Driver]) port declaration

// specify the name of the port, as provided to port initialization
// (e.g., erlang:open_port/2, executable name)
#define PORT_NAME cloud_worker_port

// specify the C or C++ include file with the functions that will be called
// from within the Erlang code
#define PORT_CXX_FUNCTIONS_HEADER_FILE "cloud_worker.hpp"

// define all interface functions that will be used by the Erlang port
#define PORT_FUNCTIONS \
    ((  keep_alive, 0, (),                                              bool)) \
    ((worker_start, 5, (pchar_len,pchar_len,pchar_len,int32_t,int16_t), bool)) \
    ((worker_start, 4, (pchar_len,pchar_len,int32_t,int16_t),           bool)) \
    (( worker_stop, 0, (),                                              bool)) \
    ((    has_work, 3, (pchar_len,uint32_t,uint32_t),                   bool)) \
    ((    add_work, 3, (pchar_len,uint32_t,uint32_t),                   bool)) \
    (( remove_work, 1, (pchar_len),                                     bool))

#endif // CLOUD_WORKER_PORT_H

