/* -*-Mode:C;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
 * ex: set ft=c fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
 *
 * MIT License
 *
 * Copyright (c) 2011-2022 Michael Truog <mjtruog at protonmail dot com>
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
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

typedef struct
{
    int thread_index;

} process_requests_t;

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
                    char const * const source,
                    uint32_t const source_size,
                    void * state,
                    cloudi_instance_t * api)
{
    char response[128];
    char const ** http_qs = cloudi_info_key_value_parse(request, request_size);
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
    cloudi_info_key_value_parse_destroy(http_qs);
    /* value is a pointer within request */
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
    char const * http_response_headers[] = {
        "content-type","text/xml; charset=utf-8",
        0};
    uint32_t response_info_size;
    char const * response_info;
    response_info = cloudi_info_key_value_new(http_response_headers,
                                              &response_info_size,
                                              1);
    cloudi_free_response_info(api);
    cloudi_return(api, request_type, name, pattern,
                  response_info, response_info_size,
                  response, strlen(response),
                  timeout, trans_id, source, source_size);
}

void process_requests(void * p)
{
    cloudi_instance_t api;
    process_requests_t * data = (process_requests_t *) p;
    int result = cloudi_initialize(&api, data->thread_index, 0);
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
    if (result != cloudi_success)
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

