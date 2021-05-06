(*

  MIT License

  Copyright (c) 2021 Michael Truog <mjtruog at protonmail dot com>

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation
  the rights to use, copy, modify, merge, publish, distribute, sublicense,
  and/or sell copies of the Software, and to permit persons to whom the
  Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
  DEALINGS IN THE SOFTWARE.

 *)

%{#
#ifndef CLOUDI_H
#define CLOUDI_H
#include <alloca.h> /* due to !defined(_ATS_CCOMP_EXCEPTION_NONE_) */
typedef struct cloudi_instance_t
{
    void * state;
    void * lookup;
    void * buffer_send;
    void * buffer_recv;
    void * buffer_call;
    void * poll_timer;
    char * prefix;
    char * response_info;
    char * response;
    char * trans_id;
    uint32_t buffer_size;
    uint32_t buffer_recv_index;
    uint32_t process_index;
    uint32_t process_count;
    uint32_t process_count_max;
    uint32_t process_count_min;
    uint32_t timeout_initialize;
    uint32_t timeout_async;
    uint32_t timeout_sync;
    uint32_t timeout_terminate;
    uint32_t response_info_size;
    uint32_t response_size;
    uint32_t trans_id_count;
    uint32_t subscribe_count;
    int fd;
    int8_t priority_default;
    unsigned int use_header:1;
    unsigned int initialization_complete:1;
    unsigned int terminate:1;
    unsigned int cxx_terminate_exception:1;
    unsigned int free_with_delete:1;
    unsigned int free_name:1;
    unsigned int free_pattern:1;
    unsigned int free_request_info:1;
    unsigned int free_request:1;
    unsigned int free_response_info:1;
    unsigned int free_response:1;

} cloudi_instance_t;
#endif /* CLOUDI_H */
%}

typedef c_instance = $extype"cloudi_instance_t"
absvtype instance_vtype(s:t@ype+) = ptr
vtypedef instance(s:t@ype) = instance_vtype(s)
datavtype
result (a:vt@ype+) =
  | Ok (a) of (a)
  | Error (a) of (int)
vtypedef Result (a:vt@ype) = result(a)

#define ASYNC 1
#define SYNC ~1
typedef request_type = [a:int | a == ASYNC || a == SYNC] int(a)

typedef
c_callback =
    (request_type,
     ptr,
     ptr,
     ptr,
     uint32,
     ptr,
     uint32,
     uint32,
     int8,
     ptr,
     ptr,
     uint32,
     ptr,
     ptr) -> void

(* XXX check usage *)
datavtype
response_ptr =
  | String of (string)
  | Ptr of (ptr, uint32)
datavtype
response =
  | Response of (response_ptr)
  | ResponseInfo of (response_ptr, response_ptr)
  | Forward of (response_ptr, response_ptr, response_ptr)
  | Forward_ of (response_ptr, response_ptr, response_ptr, uint, int)
  | Null of ()
  | NullError of (string)
vtypedef Response = response
exception FatalError of ()

typedef
callback (s:t@ype) =
    (request_type,
     string,
     string,
     ptr,
     uint,
     ptr,
     uint,
     uint,
     int,
     ptr,
     ptr,
     uint,
     s,
     !instance(s)) -> Response

fn{s:t@ype}
callback_attach
    (callback: callback(s),
     request_type: request_type,
     name: ptr,
     pattern: ptr,
     request_info: ptr,
     request_info_size: uint32,
     request: ptr,
     request_size: uint32,
     timeout: uint32,
     priority: int8,
     trans_id: ptr,
     pid: ptr,
     pid_size: uint32,
     state: ptr,
     c_api: ptr): void

fn
thread_count(): uint

fn
new {s:t@ype}{l:agz}
    (state_pfgc: !s @ l |
     thread_index: uint,
     state_p: ptr(l),
     terminate_return_value: bool): Result(instance(s))

fn
destroy {s:t@ype}
    (api: instance(s)): void

fn
subscribe {s:t@ype}
    (api: !instance(s),
     suffix: string,
     f: c_callback): Result(bool)

fn
poll {s:t@ype}
    (api: !instance(s),
     timeout: int): Result(bool)

