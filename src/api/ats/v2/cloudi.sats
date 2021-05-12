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

absvtype instance_vtype(s:vt@ype+) = ptr
vtypedef instance(s:vt@ype) = instance_vtype(s)
vtypedef stateptr(s:vt@ype) = aPtr1(s)

absvtype threads_vtype = ptr
vtypedef threads = threads_vtype

datavtype
result (a:vt@ype+) =
  | Ok (a) of (a)
  | Error (a) of (intGt(0))
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

datavtype
trans_id_ptr = TransId of (Ptr1)

datavtype
memory_ptr =
  | String of (string)    (* string literal/constant *)
  | Ptr of (Ptr1, uint32) (* ptr to be freed *)

fn
Strptr
    (str: Strptr1): memory_ptr

datavtype
response =
  | Response of (memory_ptr)
  | ResponseInfo of (memory_ptr, memory_ptr)
  | Forward of (memory_ptr, memory_ptr, memory_ptr)
  | Forward_ of (memory_ptr, memory_ptr, memory_ptr, uint, int)
  | Null of ()
  | NullError of (string)
vtypedef Response = response

exception Terminate of ()
exception FatalError of ()

typedef
callback (s:vt@ype) =
    (request_type,
     string,
     string,
     Ptr1,
     uint,
     Ptr1,
     uint,
     uint,
     int,
     !trans_id_ptr,
     Ptr1,
     uint,
     !stateptr(s),
     !instance(s)) -> Response

val trans_id_null: Ptr1

fn {s:vt@ype}
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
     c_state: ptr,
     c_api: ptr): void

fn
thread_count(): uintGt(0)

fn {s:vt@ype}
new
    (thread_index: uint,
     state_value: s,
     terminate_return_value: bool): Result(instance(s))

fn {s:vt@ype}
destroy
    (api: instance(s)): s

fn {s:vt@ype}
destroy2void
    (api: instance(s)): void

fn
subscribe {s:vt@ype}
    (api: !instance(s),
     suffix: string,
     f: c_callback): Result(unit)

fn
subscribe_count {s:vt@ype}
    (api: !instance(s),
     suffix: string): Result(uint)

fn
unsubscribe {s:vt@ype}
    (api: !instance(s),
     suffix: string): Result(unit)

(*
fn
mcast_async {s:vt@ype}
    (api: !instance(s),
     timeout: Option(uint),
     request_info: Option(memory_ptr),
     priority: Option(int8),
     request: memory_ptr): Result(list(trans_id_ptr))

fn
forward_async
    (api: !instance(s),
     request_type: request_type,
     name: string,
     request_info: memory_ptr,
     request: memory_ptr,
     timeout: uint,
     priority: int,
     trans_id: ptr,
     pid: ptr,
     pid_size: uint): void

fn
forward_sync
    (api: !instance(s),
     request_type: request_type,
     name: string,
     request_info: memory_ptr,
     request: memory_ptr,
     timeout: uint,
     priority: int,
     trans_id: ptr,
     pid: ptr,
     pid_size: uint): void

fn
forward_
    (api: !instance(s),
     request_type: request_type,
     name: string,
     request_info: memory_ptr,
     request: memory_ptr,
     timeout: uint,
     priority: int,
     trans_id: ptr,
     pid: ptr,
     pid_size: uint): void

fn
return_async
    (api: !instance(s),
     request_type: request_type,
     name: string,
     pattern: string,
     response_info: memory_ptr,
     response: memory_ptr,
     timeout: uint,
     trans_id: ptr,
     pid: ptr,
     pid_size: uint): void

fn
return_sync
    (api: !instance(s),
     request_type: request_type,
     name: string,
     pattern: string,
     response_info: memory_ptr,
     response: memory_ptr,
     timeout: uint,
     trans_id: ptr,
     pid: ptr,
     pid_size: uint): void

fn
return_
    (api: !instance(s),
     request_type: request_type,
     name: string,
     pattern: string,
     response_info: memory_ptr,
     response: memory_ptr,
     timeout: uint,
     trans_id: ptr,
     pid: ptr,
     pid_size: uint): void

fn
recv_async {s:vt@ype}
    (api: !instance(s),
     timeout: Option(uint),
     trans_id: Option(string),
     consume: Option(bool)): Result(@(memory_ptr, memory_ptr, trans_id_ptr))
*)

fn
process_index {s:vt@ype}
    (api: !instance(s)): uint

fn
process_count {s:vt@ype}
    (api: !instance(s)): uintGt(0)

fn
process_count_max {s:vt@ype}
    (api: !instance(s)): uintGt(0)

fn
process_count_min {s:vt@ype}
    (api: !instance(s)): uintGt(0)

fn
prefix_ {s:vt@ype}
    (api: !instance(s)): string

fn
timeout_initialize {s:vt@ype}
    (api: !instance(s)): uintBtwe(101, 4294967195)

fn
timeout_async {s:vt@ype}
    (api: !instance(s)): uintBtwe(499, 4294967295)

fn
timeout_sync {s:vt@ype}
    (api: !instance(s)): uintBtwe(499, 4294967295)

fn
timeout_terminate {s:vt@ype}
    (api: !instance(s)): uintBtwe(10, 60000)

fn
priority_default {s:vt@ype}
    (api: !instance(s)): int8

fn
poll {s:vt@ype}
    (api: !instance(s),
     timeout: int): Result(bool)

fn
shutdown {s:vt@ype}
    (api: !instance(s),
     reason: Option(string)): Result(unit)

(*
fn
info_key_value_parse
    (info: string): hashtbl(string, List1(string))

fn
info_key_value_new
    (pairs: hashtbl(string, List1(string)),
     response: Option(bool)): string
*)

fn
threads_create
    (thread_count: uintGt(0),
     f: (uint) -> void): threads

fn
threads_wait
    (threads: threads): void

