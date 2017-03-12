/* -*-Mode:C++;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
 * ex: set ft=cpp fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
 *
 * BSD LICENSE
 * 
 * Copyright (c) 2011-2017, Michael Truog <mjtruog at gmail dot com>
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * 
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in
 *       the documentation and/or other materials provided with the
 *       distribution.
 *     * All advertising materials mentioning features or use of this
 *       software must display the following acknowledgment:
 *         This product includes software developed by Michael Truog
 *     * The name of the author may not be used to endorse or promote
 *       products derived from this software without specific prior
 *       written permission
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND
 * CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
 * CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 * WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
 * OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH
 * DAMAGE.
 */
#include "cloudi.h"
#include <iostream>
#include <cstring>
#include <cassert>
#include <cstdlib>

#define DESTINATION "/tests/msg_size/erlang"
#define MSG_SIZE 2097152 // 2 MB

static char buffer[MSG_SIZE];

static void request(int const request_type,
                    char const * const /*name*/,
                    char const * const /*pattern*/,
                    void const * const request_info,
                    uint32_t const request_info_size,
                    void const * const request,
                    uint32_t const request_size,
                    uint32_t timeout,
                    int8_t priority,
                    char const * const trans_id,
                    char const * const pid,
                    uint32_t const pid_size,
                    void * /*state*/,
                    cloudi_instance_t * api)
{
    assert(request_size == MSG_SIZE);
    ::memcpy(buffer, request, request_size);
    unsigned int *i = reinterpret_cast<unsigned int *>(buffer);
    if (*i == 4294967295U)
        *i = 0;
    else
        (*i)++;
    std::cout << "forward #" << *i << " c++ to " DESTINATION
        " (with timeout " << timeout << " ms)" << std::endl;
    cloudi_forward(api, request_type, DESTINATION,
                   request_info, request_info_size,
                   buffer, request_size,
                   timeout, priority, trans_id, pid, pid_size);
}

int main(int, char **)
{
    unsigned int thread_count;
    int result = cloudi_initialize_thread_count(&thread_count);
    assert(result == cloudi_success);
    assert(thread_count == 1);

    cloudi_instance_t api;
    result = cloudi_initialize(&api, 0, 0);
    assert(result == cloudi_success);

    assert(MSG_SIZE <= CLOUDI_MAX_BUFFERSIZE);
    char msg[MSG_SIZE];
    ::memset(msg, 0, sizeof(msg));
    result = cloudi_send_async_(&api, DESTINATION, 
                                "", 0,
                                msg, sizeof(msg),
                                600000, // 10 minutes
                                -5);
    assert(result == cloudi_success);

    result = cloudi_subscribe(&api, "cxx", &request);
    assert(result == cloudi_success);

    result = cloudi_poll(&api, -1);
    if (result != cloudi_success &&
        result != cloudi_terminate)
        std::cerr << "error " << result << std::endl;
    std::cout << "terminate msg_size c++" << std::endl;

    cloudi_destroy(&api);
    return 0;
}

