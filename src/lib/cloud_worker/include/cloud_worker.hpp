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
#ifndef CLOUD_WORKER_HPP
#define CLOUD_WORKER_HPP

#include <stdint.h>

/// check to make sure the Erlang port is still responding
bool keep_alive();

/// start the cnode server using Erlang longnames
bool worker_start(char * nameStr, uint32_t nameLen,
                  char * domainStr, uint32_t domainLen,
                  char * cookieStr, uint32_t cookieLen,
                  int32_t port, int16_t instance);
/// start the cnode server using Erlang shortnames
bool worker_start(char * nameStr, uint32_t nameLen,
                  char * cookieStr, uint32_t cookieLen,
                  int32_t port, int16_t instance);
/// stop the cnode server
bool worker_stop();
/// add a work type to the worker
bool add_work(char * workTitleStr, uint32_t workTitleLen,
              uint32_t concurrentTaskIdOffset, uint32_t concurrentTasks);
/// check whether this worker has the work specified
bool has_work(char * workTitleStr, uint32_t workTitleLen,
              uint32_t concurrentTaskIdOffset, uint32_t concurrentTasks);
/// remove a work type from the worker
bool remove_work(char * workTitleStr, uint32_t workTitleLen);

#endif // CLOUD_WORKER_HPP

