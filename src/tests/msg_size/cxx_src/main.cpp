/* -*- coding: utf-8; Mode: C; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
 * ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
 *
 * BSD LICENSE
 * 
 * Copyright (c) 2011, Michael Truog <mjtruog at gmail dot com>
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

static char buffer[CLOUDI_MAX_BUFFERSIZE];

static void request(cloudi_instance_t * api,
                    int const command,
                    char const * const name,
                    void const * const request_info,
                    uint32_t const request_info_size,
                    void const * const request,
                    uint32_t const request_size,
                    uint32_t timeout,
                    int8_t priority,
                    char const * const trans_id,
                    char const * const pid,
                    uint32_t const pid_size)
{
    assert(request_size == MSG_SIZE);
    ::memcpy(buffer, request, request_size);
    int *i = reinterpret_cast<int *>(buffer);
    if (*i == 4294967295)
        *i = 0;
    else
        (*i)++;
    std::cout << "forward #" << *i << " to " DESTINATION
        " (with timeout " << timeout << " ms)" << std::endl;
    cloudi_forward(api, command, DESTINATION,
                   request_info, request_info_size,
                   buffer, request_size,
                   timeout, priority, trans_id, pid, pid_size);
}

int main(int argc, char ** argv)
{
    if (argc != 4)
    {
        std::cout << "Usage: " << argv[0] <<
            " thread_count protocol buffer_size" << std::endl;
        return 1;
    }
    int const count = ::atoi(argv[1]);
    char const * const protocol = argv[2];
    int const buffer_size = ::atoi(argv[3]);

    assert(count == 1);

    cloudi_instance_t api;
    int result = cloudi_initialize(&api, 0, protocol, buffer_size);

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
    if (result != cloudi_success)
        std::cerr << "error " << result << std::endl;

    cloudi_destroy(&api);
    return 0;
}

