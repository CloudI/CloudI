/* -*- coding: utf-8; Mode: C; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
 * ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:
 *
 * BSD LICENSE
 * 
 * Copyright (c) 2011-2014, Michael Truog <mjtruog at gmail dot com>
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

#ifdef CLOUDI_HPP
#warning "Only need to include cloudi.h (C), not cloudi.hpp (C++)"
#endif
#ifndef CLOUDI_H
#define CLOUDI_H

#include <stdint.h>

#if defined __cplusplus
extern "C"
{
#endif

#define CLOUDI_MAX_BUFFERSIZE 2147483648U /* 2GB */

typedef struct cloudi_instance_t
{
    int fd_in;
    int fd_out;
    int use_header;
    int initialization_complete;
    uint32_t buffer_size;
    void * lookup;
    void * buffer_send;
    void * buffer_recv;
    uint32_t buffer_recv_index;
    void * buffer_call;
    void * poll_timer;
    void * request_timer;
    uint32_t request_timeout;
    uint32_t process_index;
    uint32_t process_count;
    uint32_t process_count_max;
    uint32_t process_count_min;
    char * prefix;
    uint32_t timeout_async;
    uint32_t timeout_sync;
    int8_t priority_default;
    uint8_t request_timeout_adjustment;
    char * response_info;
    uint32_t response_info_size;
    char * response;
    uint32_t response_size;
    char * trans_id;          /* always 16 characters (128 bits) length */
    uint32_t trans_id_count;

} cloudi_instance_t;

/* command values */
#define CLOUDI_ASYNC     1
#define CLOUDI_SYNC     -1

typedef void (*cloudi_callback_t)(cloudi_instance_t * p,
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
                                  uint32_t const pid_size);


#define cloudi_get_response(p)               (p->response)
#define cloudi_get_response_size(p)          (p->response_size)
#define cloudi_get_response_info(p)          (p->response_info)
#define cloudi_get_response_info_size(p)     (p->response_info_size)
#define cloudi_get_trans_id_count(p)         (p->trans_id_count)
#define cloudi_get_trans_id(p, i)            (&(p->trans_id[i * 16]))
#define cloudi_get_process_index(p)          (p->process_index)
#define cloudi_get_process_count(p)          (p->process_count)
#define cloudi_get_process_count_max(p)      (p->process_count_max)
#define cloudi_get_process_count_min(p)      (p->process_count_min)
#define cloudi_get_prefix(p)                 (p->prefix)
#define cloudi_get_timeout_async(p)          (p->timeout_async)
#define cloudi_get_timeout_sync(p)           (p->timeout_sync)
#define cloudi_get_priority_default(p)       (p->priority_default)

int cloudi_initialize(cloudi_instance_t * p,
                      unsigned int const thread_index);

void cloudi_destroy(cloudi_instance_t * p);

int cloudi_initialize_thread_count(unsigned int * const thread_count);

int cloudi_subscribe(cloudi_instance_t * p,
                     char const * const pattern,
                     cloudi_callback_t f);

int cloudi_unsubscribe(cloudi_instance_t * p,
                       char const * const pattern);

int cloudi_send_async(cloudi_instance_t * p,
                      char const * const name,
                      void const * const request,
                      uint32_t const request_size);

int cloudi_send_async_(cloudi_instance_t * p,
                       char const * const name,
                       void const * const request_info,
                       uint32_t const request_info_size,
                       void const * const request,
                       uint32_t const request_size,
                       uint32_t timeout,
                       int8_t const priority);

int cloudi_send_sync(cloudi_instance_t * p,
                     char const * const name,
                     void const * const request,
                     uint32_t const request_size);

int cloudi_send_sync_(cloudi_instance_t * p,
                      char const * const name,
                      void const * const request_info,
                      uint32_t const request_info_size,
                      void const * const request,
                      uint32_t const request_size,
                      uint32_t timeout,
                      int8_t const priority);

int cloudi_mcast_async(cloudi_instance_t * p,
                       char const * const name,
                       void const * const request,
                       uint32_t const request_size);

int cloudi_mcast_async_(cloudi_instance_t * p,
                        char const * const name,
                        void const * const request_info,
                        uint32_t const request_info_size,
                        void const * const request,
                        uint32_t const request_size,
                        uint32_t timeout,
                        int8_t const priority);

int cloudi_forward(cloudi_instance_t * p,
                   int const command,
                   char const * const name,
                   void const * const request_info,
                   uint32_t const request_info_size,
                   void const * const request,
                   uint32_t const request_size,
                   uint32_t timeout,
                   int8_t const priority,
                   char const * const trans_id,
                   char const * const pid,
                   uint32_t const pid_size);

int cloudi_forward_async(cloudi_instance_t * p,
                         char const * const name,
                         void const * const request_info,
                         uint32_t const request_info_size,
                         void const * const request,
                         uint32_t const request_size,
                         uint32_t timeout,
                         int8_t const priority,
                         char const * const trans_id,
                         char const * const pid,
                         uint32_t const pid_size);

int cloudi_forward_sync(cloudi_instance_t * p,
                        char const * const name,
                        void const * const request_info,
                        uint32_t const request_info_size,
                        void const * const request,
                        uint32_t const request_size,
                        uint32_t timeout,
                        int8_t const priority,
                        char const * const trans_id,
                        char const * const pid,
                        uint32_t const pid_size);

int cloudi_return(cloudi_instance_t * p,
                  int const command,
                  char const * const name,
                  char const * const pattern,
                  void const * const response_info,
                  uint32_t const response_info_size,
                  void const * const response,
                  uint32_t const response_size,
                  uint32_t timeout,
                  char const * const trans_id,
                  char const * const pid,
                  uint32_t const pid_size);

int cloudi_return_async(cloudi_instance_t * p,
                        char const * const name,
                        char const * const pattern,
                        void const * const response_info,
                        uint32_t const response_info_size,
                        void const * const response,
                        uint32_t const response_size,
                        uint32_t timeout,
                        char const * const trans_id,
                        char const * const pid,
                        uint32_t const pid_size);

int cloudi_return_sync(cloudi_instance_t * p,
                       char const * const name,
                       char const * const pattern,
                       void const * const response_info,
                       uint32_t const response_info_size,
                       void const * const response,
                       uint32_t const response_size,
                       uint32_t timeout,
                       char const * const trans_id,
                       char const * const pid,
                       uint32_t const pid_size);

int cloudi_recv_async(cloudi_instance_t * p,
                      uint32_t timeout,
                      char const * const trans_id,
                      int consume);

int cloudi_poll(cloudi_instance_t * p,
                int timeout);

char const ** cloudi_request_http_qs_parse(void const * const request,
                                           uint32_t const request_size);
void cloudi_request_http_qs_destroy(char const ** p);

char const ** cloudi_info_key_value_parse(void const * const request_info,
                                          uint32_t const request_info_size);
void cloudi_info_key_value_destroy(char const ** p);

enum
{
    cloudi_success                             =   0,
    // programs can use exit status values [1..6] without conflicts
    // with internal cloudi error conditions

    // API specific errors
    cloudi_timeout                             =   7,
    cloudi_error_function_parameter            =   8,
    cloudi_error_read_underflow                =   9,
    cloudi_error_ei_decode                     =  10,
    // reuse some exit status values from os_spawn
    cloudi_invalid_input                       =  11,
    cloudi_out_of_memory                       =  12,
    // reuse some exit status values from GEPD
    cloudi_error_read_EAGAIN                   =  81,
    cloudi_error_read_EBADF                    =  82,
    cloudi_error_read_EFAULT                   =  83,
    cloudi_error_read_EINTR                    =  84,
    cloudi_error_read_EINVAL                   =  85,
    cloudi_error_read_EIO                      =  86,
    cloudi_error_read_EISDIR                   =  87,
    cloudi_error_read_null                     =  88,
    cloudi_error_read_overflow                 =  89,
    cloudi_error_read_unknown                  =  90,
    cloudi_error_write_EAGAIN                  =  91,
    cloudi_error_write_EBADF                   =  92,
    cloudi_error_write_EFAULT                  =  93,
    cloudi_error_write_EFBIG                   =  94,
    cloudi_error_write_EINTR                   =  95,
    cloudi_error_write_EINVAL                  =  96,
    cloudi_error_write_EIO                     =  97,
    cloudi_error_write_ENOSPC                  =  98,
    cloudi_error_write_EPIPE                   =  99,
    cloudi_error_write_null                    = 100,
    cloudi_error_write_overflow                = 101,
    cloudi_error_write_unknown                 = 102,
    cloudi_error_ei_encode                     = 103,
    cloudi_error_poll_EBADF                    = 104,
    cloudi_error_poll_EFAULT                   = 105,
    cloudi_error_poll_EINTR                    = 106,
    cloudi_error_poll_EINVAL                   = 107,
    cloudi_error_poll_ENOMEM                   = 108,
    cloudi_error_poll_ERR                      = 109,
    cloudi_error_poll_HUP                      = 110,
    cloudi_error_poll_NVAL                     = 111,
    cloudi_error_poll_unknown                  = 112
};

#if defined __cplusplus
}
#endif

#endif /* CLOUDI_H */

