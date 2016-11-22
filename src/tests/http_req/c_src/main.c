/* -*-Mode:C;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
 * ex: set ft=c fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
 *
 * BSD LICENSE
 * 
 * Copyright (c) 2011-2015, Michael Truog <mjtruog at gmail dot com>
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
    int thread_index;

} process_requests_t;

static void request(cloudi_instance_t * api,
                    int const command,
                    char const * const name,
                    char const * const pattern,
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
    char response[128];
    char const ** http_qs = cloudi_request_http_qs_parse(request, request_size);
    char const * value = 0;
    size_t i;
    for (i = 0; http_qs[i]; i += 2)
    {
        if (strcmp(http_qs[i], "value") == 0)
        {
            value = http_qs[i + 1];
            break;
        }
    }
    cloudi_request_http_qs_destroy(http_qs);
    if (value)
    {
        snprintf(response, sizeof(response),
                 "<http_test><value>%d</value></http_test>", atoi(value));
    }
    else
    {
        memcpy(response,
               "<http_test><error>no value specified</error></http_test>", 57);
    }
    cloudi_return(api, command, name, pattern, "", 0,
                  response, strlen(response),
                  timeout, trans_id, pid, pid_size);
}

void process_requests(void * p)
{
    cloudi_instance_t api;
    process_requests_t * data = (process_requests_t *) p;
    int result = cloudi_initialize(&api, data->thread_index);
    assert(result == cloudi_success);

    result = cloudi_subscribe_count(&api, "c.xml/get");
    assert(result == cloudi_success);
    assert(cloudi_get_subscribe_count(&api) == 0);
    result = cloudi_subscribe(&api, "c.xml/get", &request);
    assert(result == cloudi_success);
    result = cloudi_subscribe_count(&api, "c.xml/get");
    assert(result == cloudi_success);
    assert(cloudi_get_subscribe_count(&api) == 1);

    result = cloudi_poll(&api, -1);
    if (result != cloudi_success &&
        result != cloudi_terminate)
        fprintf(stderr, "error %d\n", result);
    printf("terminate http_req c\n");

    cloudi_destroy(&api);
}

int main(int argc, char ** argv)
{
    unsigned int thread_count;
    int result = cloudi_initialize_thread_count(&thread_count);
    assert(result == cloudi_success);
    assert(thread_count == 1);

    process_requests_t data = {0};

    process_requests(&data);

    return 0;
}

