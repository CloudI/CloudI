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
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

typedef struct
{
    int index;
    char const * protocol;
    int buffer_size;

} process_requests_t;

static void request(cloudi_instance_t * api,
                    int const command,
                    char const * const name,
                    void const * const request,
                    uint32_t const request_size,
                    uint32_t timeout,
                    char const * const trans_id,
                    char const * const pid,
                    uint32_t const pid_size)
{
    char const * const response =
"<html><head><title>Hello World</title></head>"
"<body><p>Hello World!</p></body></html>";
    
    cloudi_return(api, command, name,
                  response, strlen(response),
                  timeout, trans_id, pid, pid_size);
}

void process_requests(void * p)
{
    cloudi_instance_t api;
    process_requests_t * data = (process_requests_t *) p;
    int result = cloudi_initialize(&api,
                                   data->index,
                                   data->protocol,
                                   data->buffer_size);

    result = cloudi_subscribe(&api, "c.html", &request);
    assert(result == cloudi_success);

    result = cloudi_poll(&api, -1);
    if (result != cloudi_success)
        fprintf(stderr, "error %d\n", result);

    cloudi_destroy(&api);
}

int main(int argc, char ** argv)
{
    if (argc != 4)
    {
        printf("Usage: %s thread_count protocol buffer_size", argv[0]);
        return 1;
    }
    int const count = atoi(argv[1]);
    char const * const protocol = argv[2];
    int const buffer_size = atoi(argv[3]);

    assert(count == 1);

    process_requests_t data = {0, protocol, buffer_size};

    process_requests(&data);

    return 0;
}

