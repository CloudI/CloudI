/* -*-Mode:C;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
 * ex: set ft=c fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
 *
 * BSD LICENSE
 * 
 * Copyright (c) 2017, Michael Truog <mjtruog at gmail dot com>
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

typedef struct
{
    unsigned int count;

} service_state_t;

static void request(int const request_type,
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
                    uint32_t const pid_size,
                    void * state,
                    cloudi_instance_t * api)
{
    service_state_t *state_p = (service_state_t *) state;
    char response[128];
    if (state_p->count == 4294967295)
    {
        state_p->count = 0;
    }
    else
    {
        state_p->count += 1;
    }
    printf("count == %d c\n", state_p->count);
    snprintf(response, sizeof(response), "%d", state_p->count);
    cloudi_return(api, request_type, name, pattern, "", 0,
                  response, strlen(response),
                  timeout, trans_id, pid, pid_size);
}

void process_requests(void * p)
{
    cloudi_instance_t api;
    process_requests_t * data = (process_requests_t *) p;
    service_state_t state = {0};
    int result = cloudi_initialize(&api, data->thread_index, &state);
    assert(result == cloudi_success);

    result = cloudi_subscribe(&api, "c/get", &request);
    assert(result == cloudi_success);

    result = cloudi_poll(&api, -1);
    if (result != cloudi_success &&
        result != cloudi_terminate)
        fprintf(stderr, "error %d\n", result);
    printf("terminate count c\n");

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

