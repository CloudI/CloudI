/* -*-Mode:C++;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
 * ex: set ft=cpp fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
 *
 * MIT License
 *
 * Copyright (c) 2011-2017 Michael Truog <mjtruog at protonmail dot com>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
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

