(*-*-Mode:ats;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
  ex: set ft=ats fenc=utf-8 sts=4 ts=4 sw=4 et nomod: *)

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
#define ATS_DYNLOADFLAG 0
#include "share/atspre_define.hats"
#include "share/atspre_staload.hats"
staload CLOUDI = "cloudi.sats"
staload UNSAFE = "prelude/SATS/unsafe.sats"
staload ATHREAD = "libats/SATS/athread.sats"
staload _(*ATHREAD*) = "libats/DATS/athread.dats"
staload _(*ATHREAD*) = "libats/DATS/athread_posix.dats"

%{^
#include <pthread.h> /* due to athread_posix.dats */
#include <time.h>    /* c_threads_yield */
#include <string.h>  /* c_memcpy/c_memcmp */

enum
{
    cloudi_success                             =   0,
    // programs can use exit status values [1..6] without conflicts
    // with internal cloudi error conditions

    // API specific errors
    cloudi_terminate                           = 110, // cloudi_error_poll_HUP
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

void c_threads_yield()
{
    struct timespec delay = {0, 1000};
    nanosleep(&delay, NULL);
}

void c_set_c_state(void * api_c, void * state)
{
    ((cloudi_instance_t *)api_c)->state = state;
}

#define C_GET_FUNCTION(T, NAME)                                               \
T c_get_##NAME(void * api_c)                                                  \
{                                                                             \
    return ((cloudi_instance_t *)api_c)->NAME;                                \
}

C_GET_FUNCTION(void *,   response)
C_GET_FUNCTION(uint32_t, response_size)
C_GET_FUNCTION(void *,   response_info)
C_GET_FUNCTION(uint32_t, response_info_size)
C_GET_FUNCTION(uint32_t, trans_id_count)

void * c_get_trans_id(void * api_c, unsigned int i)
{
    return &(((cloudi_instance_t *)api_c)->trans_id[i * 16]);
}

C_GET_FUNCTION(uint32_t, subscribe_count)
C_GET_FUNCTION(uint32_t, process_index)
C_GET_FUNCTION(uint32_t, process_count)
C_GET_FUNCTION(uint32_t, process_count_max)
C_GET_FUNCTION(uint32_t, process_count_min)
C_GET_FUNCTION(void *,   prefix)
C_GET_FUNCTION(uint32_t, timeout_initialize)
C_GET_FUNCTION(uint32_t, timeout_async)
C_GET_FUNCTION(uint32_t, timeout_sync)
C_GET_FUNCTION(uint32_t, timeout_terminate)
C_GET_FUNCTION(int8_t,   priority_default)
%}
macdef c_int_success = 0
macdef c_int_terminate = $extval(int, "cloudi_terminate")
macdef c_int_timeout = $extval(int, "cloudi_timeout")

extern castfn
sz_u32
    (x: size_t):<fun0>
    uint32
extern castfn
ssz_u32
    (x: ssize_t):<fun0>
    uint32
extern castfn
u_u32
    (x: uint):<fun0>
    uint32
extern castfn
u32_sz
    (x: uint32):<fun0>
    size_t
extern castfn
u32_timeout
    (x: uint32):<fun0>
    $CLOUDI.timeout
extern castfn
i_u32
    (x: int):<fun0>
    uint32
extern castfn
i8_priority
    (x: int8):<fun0>
    $CLOUDI.priority
extern castfn
i_i8
    (x: int):<fun0>
    int8

(* functions called in templates are required to be external (global)
 * (https://github.com/githwxi/ATS-Postiats/issues/180#issuecomment-363285569)
 * (Otherwise PMVerr appears in the C ATS output)
 *)
extern fn {s:vt@ype}
state_p_new
    (state_value: s):<!wrt>
    Ptr1
extern fn {s:vt@ype}
state_p_destroy {l:agz}
    (state_p: ptr(l)):<!wrt>
    s
extern fn
callback_name
    (api_c: ptr,
     name: $CLOUDI.memory_free_ptr):<!wrt>
    Ptr1
extern fn
callback_request_info
    (api_c: ptr,
     request_info: $CLOUDI.memory_free_ptr,
     size_c: &uint32):<!ref,!wrt>
    Ptr1
extern fn
callback_request
    (api_c: ptr,
     request: $CLOUDI.memory_free_ptr,
     size_c: &uint32):<!ref,!wrt>
    Ptr1
extern fn
callback_response_info
    (api_c: ptr,
     response_info: $CLOUDI.memory_free_ptr,
     size_c: &uint32):<!ref,!wrt>
    Ptr1
extern fn
callback_response
    (api_c: ptr,
     response: $CLOUDI.memory_free_ptr,
     size_c: &uint32):<!ref,!wrt>
    Ptr1
extern fn
api_c_get {s:vt@ype}
    (api: !$CLOUDI.instance(s)):<fun0>
    ptr
extern fn {a:t@ype}
result_value {s:vt@ype}
    (value: a,
     status: intGte(0),
     api: !$CLOUDI.instance(s)):<!exn>
    $CLOUDI.result(a)
extern fn
result_value_unit {s:vt@ype}
    (status: intGte(0),
     api: !$CLOUDI.instance(s)):<!exn>
    $CLOUDI.result(unit)
extern fn {s:vt@ype}
callback_attach
    (callback: $CLOUDI.callback(s),
     request_type: $CLOUDI.request_type,
     name_c: ptr,
     pattern_c: ptr,
     request_info_c: ptr,
     request_info_size_c: uint32,
     request_c: ptr,
     request_size_c: uint32,
     timeout_c: uint32,
     priority_c: int8,
     trans_id_c: ptr,
     pid_c: ptr,
     pid_size_c: uint32,
     state_c: ptr,
     api_c: ptr):<fun1>
    void

(* C CloudI API related functions
 *)
extern fn
c_threads_yield:
    () -<fun0>
    void = "ext#"
extern fn
c_set_c_state: {l1:agz}
    (ptr,
     ptr(l1)) -<fun,!wrt>
    void = "ext#"
extern fn
c_get_response:
    (ptr) -<fun0>
    [l1:agz] ptr(l1) = "ext#"
extern fn
c_get_response_size:
    (ptr) -<fun0>
    uint32 = "ext#"
extern fn
c_get_response_info:
    (ptr) -<fun0>
    [l1:agz] ptr(l1) = "ext#"
extern fn
c_get_response_info_size:
    (ptr) -<fun0>
    uint32 = "ext#"
extern fn
c_get_trans_id_count:
    (ptr) -<fun0>
    uintGt(0) = "ext#"
extern fn
c_get_trans_id:
    (ptr,
     uint) -<fun0>
    [l1:agz] ptr(l1) = "ext#"
extern fn
c_get_subscribe_count:
    (ptr) -<fun0>
    uintGte(0) = "ext#"
extern fn
c_get_process_index:
    (ptr) -<fun0>
    uintGte(0) = "ext#"
extern fn
c_get_process_count:
    (ptr) -<fun0>
    uintGt(0) = "ext#"
extern fn
c_get_process_count_max:
    (ptr) -<fun0>
    uintGt(0) = "ext#"
extern fn
c_get_process_count_min:
    (ptr) -<fun0>
    uintGt(0) = "ext#"
extern fn
c_get_prefix:
    (ptr) -<fun0>
    [l1:agz] ptr(l1) = "ext#"
extern fn
c_get_timeout_initialize:
    (ptr) -<fun0>
    $CLOUDI.timeout_initialize = "ext#"
extern fn
c_get_timeout_async:
    (ptr) -<fun0>
    $CLOUDI.timeout_async = "ext#"
extern fn
c_get_timeout_sync:
    (ptr) -<fun0>
    $CLOUDI.timeout_sync = "ext#"
extern fn
c_get_timeout_terminate:
    (ptr) -<fun0>
    $CLOUDI.timeout_terminate = "ext#"
extern fn
c_get_priority_default:
    (ptr) -<fun0>
    int8 = "ext#"
extern fn
c_initialize: {l1:agz}{l2:addr}
    (!$CLOUDI.c_instance? @ l1 >> $CLOUDI.c_instance @ l1 |
     ptr(l1),
     uint,
     ptr(l2)) -<fun,!ref,!wrt>
    intGte(0) = "ext#cloudi_initialize"
extern fn
c_destroy: {l1:agz}
    (!$CLOUDI.c_instance @ l1 |
     ptr(l1)) -<fun,!ref,!wrt>
    [l2:addr] ptr(l2) = "ext#cloudi_destroy"
extern fn
c_initialize_thread_count: {l1:agz}
    (!uintGt(0) @ l1 |
     ptr(l1)) -<fun,!wrt>
    intGte(0) = "ext#cloudi_initialize_thread_count"
extern fn
c_subscribe: {l1:agz}
    (ptr,
     ptr(l1),
     $CLOUDI.c_callback) -<fun,!ref,!wrt>
    intGte(0) = "ext#cloudi_subscribe"
extern fn
c_subscribe_count: {l1:agz}
    (ptr,
     ptr(l1)) -<fun,!ref,!wrt>
    intGte(0) = "ext#cloudi_subscribe_count"
extern fn
c_unsubscribe: {l1:agz}
    (ptr,
     ptr(l1)) -<fun,!ref,!wrt>
    intGte(0) = "ext#cloudi_unsubscribe"
extern fn
c_send_async_: {l1,l2,l3:agz}
    (ptr,
     ptr(l1),
     ptr(l2),
     uint32,
     ptr(l3),
     uint32,
     uint32,
     int8) -<fun,!ref,!wrt>
    intGte(0) = "ext#cloudi_send_async_"
extern fn
c_send_sync_: {l1,l2,l3:agz}
    (ptr,
     ptr(l1),
     ptr(l2),
     uint32,
     ptr(l3),
     uint32,
     uint32,
     int8) -<fun,!ref,!wrt>
    intGte(0) = "ext#cloudi_send_sync_"
extern fn
c_mcast_async_: {l1,l2,l3:agz}
    (ptr,
     ptr(l1),
     ptr(l2),
     uint32,
     ptr(l3),
     uint32,
     uint32,
     int8) -<fun,!ref,!wrt>
    intGte(0) = "ext#cloudi_mcast_async_"
extern fn
c_forward: {l1,l2,l3,l4,l5:agz}
    (ptr,
     int,
     ptr(l1),
     ptr(l2),
     uint32,
     ptr(l3),
     uint32,
     uint32,
     int8,
     ptr(l4),
     ptr(l5),
     uint32) -<fun,!exn,!wrt>
    intGte(0) = "ext#cloudi_forward"
extern fn
c_forward_async: {l1,l2,l3,l4,l5:agz}
    (ptr,
     ptr(l1),
     ptr(l2),
     uint32,
     ptr(l3),
     uint32,
     uint32,
     int8,
     ptr(l4),
     ptr(l5),
     uint32) -<fun,!exn,!wrt>
    intGte(0) = "ext#cloudi_forward_async"
extern fn
c_forward_sync: {l1,l2,l3,l4,l5:agz}
    (ptr,
     ptr(l1),
     ptr(l2),
     uint32,
     ptr(l3),
     uint32,
     uint32,
     int8,
     ptr(l4),
     ptr(l5),
     uint32) -<fun,!exn,!wrt>
    intGte(0) = "ext#cloudi_forward_sync"
extern fn
c_return: {l1,l2,l3,l4,l5,l6:agz}
    (ptr,
     int,
     ptr(l1),
     ptr(l2),
     ptr(l3),
     uint32,
     ptr(l4),
     uint32,
     uint32,
     ptr(l5),
     ptr(l6),
     uint32) -<fun,!exn,!wrt>
    intGte(0) = "ext#cloudi_return"
extern fn
c_return_async: {l1,l2,l3,l4,l5,l6:agz}
    (ptr,
     ptr(l1),
     ptr(l2),
     ptr(l3),
     uint32,
     ptr(l4),
     uint32,
     uint32,
     ptr(l5),
     ptr(l6),
     uint32) -<fun,!exn,!wrt>
    intGte(0) = "ext#cloudi_return_async"
extern fn
c_return_sync: {l1,l2,l3,l4,l5,l6:agz}
    (ptr,
     ptr(l1),
     ptr(l2),
     ptr(l3),
     uint32,
     ptr(l4),
     uint32,
     uint32,
     ptr(l5),
     ptr(l6),
     uint32) -<fun,!exn,!wrt>
    intGte(0) = "ext#cloudi_return_sync"
extern fn
c_recv_async: {l1:agez}
    (ptr,
     uint32,
     ptr(l1),
     int) -<fun,!ref,!wrt>
    intGte(0) = "ext#cloudi_recv_async"
extern fn
c_poll:
    (ptr,
     int) -<fun,!ntm,!ref,!wrt>
    intGte(0) = "ext#cloudi_poll"
extern fn
c_shutdown:
    (ptr,
     ptr) -<fun,!ref,!wrt>
    intGte(0) = "ext#cloudi_shutdown"
extern fn
c_free_name:
    (ptr) -<fun,!wrt>
    void = "ext#cloudi_free_name"
extern fn
c_free_pattern:
    (ptr) -<fun,!wrt>
    void = "ext#cloudi_free_pattern"
extern fn
c_free_request_info:
    (ptr) -<fun,!wrt>
    void = "ext#cloudi_free_request_info"
extern fn
c_free_request:
    (ptr) -<fun,!wrt>
    void = "ext#cloudi_free_request"
extern fn
c_free_response_info:
    (ptr) -<fun,!wrt>
    void = "ext#cloudi_free_response_info"
extern fn
c_free_response:
    (ptr) -<fun,!wrt>
    void = "ext#cloudi_free_response"
(* Standard C functions
 *)
extern fn
c_memcpy
    (destination: ptr,
     source: ptr,
     size: size_t):<!wrt>
    ptr = "mac#memcpy"
extern fn
c_memcmp
    (x1: Ptr1,
     x2: Ptr1,
     size: size_t):<fun0>
    int = "mac#memcmp"

(* ATS CloudI API types
 *)
datavtype
instance_(s:vt@ype) = {l1,l2:agz} INSTANCE of (@{
  api_c_ptr = ($CLOUDI.c_instance @ l1, mfree_gc_v(l1) | ptr(l1))
, state_p = ptr(l2)
, terminate_return_value = bool
})
assume $CLOUDI.instance_vtype(s:vt@ype) = instance_(s)
datavtype
trans_id_ = TransId of (Ptr1, bool)
assume $CLOUDI.trans_id_vtype = trans_id_

(* support for threads usage with the ATS CloudI API
 *)
datavtype
threads_ = {l1,l2:agz} THREADS of (@{
  thread_count = uintGt(0)
, running = intGte(0)
, mutex = $ATHREAD.mutex_vt(l1)
, condvar = $ATHREAD.condvar_vt(l2)
})
assume $CLOUDI.threads_vtype = threads_

var trans_id_null_data =  @[byte](i2byte(0), i2byte(0), i2byte(0), i2byte(0),
                                  i2byte(0), i2byte(0), i2byte(0), i2byte(0),
                                  i2byte(0), i2byte(0), i2byte(0), i2byte(0),
                                  i2byte(0), i2byte(0), i2byte(0), i2byte(0))

implement
$CLOUDI.trans_id_null
    () = let
    val trans_id_p = $UNSAFE.cast2Ptr1(addr@trans_id_null_data)
in
    TransId(trans_id_p, false)
end

implement
$CLOUDI.trans_id_is_null
    (trans_id) = let
    val trans_id_null_value = $CLOUDI.trans_id_null()
    val is_null = $CLOUDI.trans_id_eq(trans_id_null_value, trans_id)
    val () = $CLOUDI.trans_id_free(trans_id_null_value)
in
    is_null
end

implement
$CLOUDI.trans_id_isnot_null
    (trans_id) = let
    val is_null = $CLOUDI.trans_id_is_null(trans_id)
in
    ~is_null
end

fn
trans_id_compare
    (trans_id1: !$CLOUDI.trans_id,
     trans_id2: !$CLOUDI.trans_id):<fun0>
    int = let
    val TransId(trans_id1_p, _) = trans_id1
    val TransId(trans_id2_p, _) = trans_id2
in
    c_memcmp(trans_id1_p, trans_id2_p, i2sz(16))
end

implement
$CLOUDI.trans_id_eq
    (trans_id1,
     trans_id2) = (trans_id_compare(trans_id1, trans_id2) = 0)

implement
$CLOUDI.trans_id_neq
    (trans_id1,
     trans_id2) = (trans_id_compare(trans_id1, trans_id2) <> 0)

implement
$CLOUDI.trans_id_copy
    (trans_id) = let
    val TransId(trans_id_p, _) = trans_id
    val size_copy = i2sz(16)
    val (_, _ | trans_id_p_copy) = malloc_gc(size_copy)
    val _ = c_memcpy(trans_id_p_copy, trans_id_p, size_copy)
in
    TransId(trans_id_p_copy, true)
end

implement
$CLOUDI.trans_id_store
    (trans_id) = let
    val trans_id_copy = $CLOUDI.trans_id_copy(trans_id)
    val () = $CLOUDI.trans_id_free(trans_id)
in
    trans_id_copy
end

implement
$CLOUDI.trans_ids_store
    (trans_ids,
     trans_ids_size) = let
    fun
    trans_id_store {i,ni:nat | i <= ni} .<i>.
        (p: ptr,
         p_i: size_t(i),
         p_n: size_t(ni)):<!wrt>
        void = if (p_i > 0) then let
        val trans_id = $UNSAFE.ptr0_get<$CLOUDI.trans_id>(p)
        val trans_id_new = $CLOUDI.trans_id_store(trans_id)
        val () = $UNSAFE.ptr0_set<$CLOUDI.trans_id>(p, trans_id_new)
        val p_next = ptr0_succ<$CLOUDI.trans_id>(p)
    in
        trans_id_store(p_next, p_i - i2sz(1), p_n)
    end
    else
        ()
in
    if (trans_ids_size > i2sz(0)) then
        trans_id_store(ptrcast(trans_ids), trans_ids_size, trans_ids_size)
    else
        ()
end

fn
trans_id_free_ptr {l:agz}
    (trans_id_p: ptr(l)):<!wrt>
    void = let
    prval trans_id_pfgc = $UNSAFE.castview0{b0ytes_v(l, 16)}(0)
    prval trans_id_pfat = $UNSAFE.castview0{mfree_gc_v(l)}(0)
in
    mfree_gc{l}{16}(trans_id_pfgc, trans_id_pfat | trans_id_p)
end

implement
$CLOUDI.trans_id_free
    (trans_id) = let
    val ~TransId(trans_id_p, owned) = trans_id
in
    if (owned) then
        trans_id_free_ptr(trans_id_p)
    else
        ()
end

implement
$CLOUDI.trans_ids_free {l}{n}
    (trans_ids,
     trans_ids_size) = let
    fun
    trans_id_free {i,ni:nat | i <= ni} .<i>.
        (p: ptr,
         p_i: size_t(i),
         p_n: size_t(ni)):<!wrt>
        void = if (p_i > 0) then let
        val trans_id = $UNSAFE.ptr0_get<$CLOUDI.trans_id>(p)
        val () = $CLOUDI.trans_id_free(trans_id)
        val p_next = ptr0_succ<$CLOUDI.trans_id>(p)
    in
        trans_id_free(p_next, p_i - i2sz(1), p_n)
    end
    else
        ()
    val () = if (trans_ids_size > i2sz(0)) then
        trans_id_free(ptrcast(trans_ids), trans_ids_size, trans_ids_size)
    else
        ()
in
    arrayptr_free($UNSAFE.castvwtp0(trans_ids))
end

implement {s}
state_p_new
    (state_value) = let
    val (pfgc, pfat | p) = ptr_alloc<s>()
    val () = !p := state_value
in
    $UNSAFE.castvwtp0{Ptr1}((pfgc, pfat, p))
end

implement {s}
state_p_destroy {l}
    (state_p) = state where
{
    val state = $UNSAFE.ptr1_get<s>(state_p)
    prval state_pfgc = $UNSAFE.castview0{(s?) @ l}(0)
    prval state_pfat = $UNSAFE.castview0{mfree_gc_v(l)}(0)
    val () = ptr_free{s?}{l}(state_pfat, state_pfgc | state_p)
}

implement
$CLOUDI.string2read
    (str) = let
    val size: uint32 = sz_u32(string_length(str))
    val p: Ptr1 = string2ptr(str)
in
    $CLOUDI.Ptr(p, size)
end

implement
$CLOUDI.strptr2read
    (str) = $CLOUDI.string2read($UNSAFE.strptr2string(str))

implement
$CLOUDI.memory2string
    (ptr) = let
    val $CLOUDI.Ptr(p, _) = ptr
    val str = $UNSAFE.castvwtp0{string}(p)
in
    str
end

implement
$CLOUDI.memory2free
    (ptr) = let
    val ~$CLOUDI.Ptr(p, size) = ptr
    val size_copy = g1ofg0(u32_sz(size))
    val (_, _ | p_copy) = malloc_gc(size_copy)
    val _ = c_memcpy(p_copy, p, size_copy)
in
    $CLOUDI.PtrFree(p_copy, size)
end

implement
$CLOUDI.stropt2free
    (str,
     default) = if (stropt_is_some(str)) then let
    val str_n: [n:nat] string(n) = stropt_unsome(str)
    val size: uint32 = sz_u32(string1_length(str_n))
    val p: Ptr1 = string2ptr(str_n)
in
    $CLOUDI.PtrFree(p, size)
end
else
    $CLOUDI.StringLiteral(default)

implement
$CLOUDI.strptr2free
    (str) = let
    val size: uint32 = ssz_u32(strptr_length(str))
    val p: Ptr1 = strptr2ptr(str)
    prval () = $UNSAFE.cast2void(str)
in
    $CLOUDI.PtrFree(p, size)
end

implement
$CLOUDI.strnptr2free
    (str) = let
    val size: uint32 = ssz_u32(strnptr_length(str))
    val p: Ptr1 = strnptr2ptr(str)
    prval () = $UNSAFE.cast2void(str)
in
    $CLOUDI.PtrFree(p, size)
end

implement
callback_name
    (api_c,
     name) =
case+ name of
  | ~$CLOUDI.StringLiteral(s) =>
    string2ptr(s)
  | ~$CLOUDI.PtrFree(name_c, _) => let
    val () = c_free_name(api_c)
in
    name_c
end

implement
callback_request_info
    (api_c,
     request_info,
     size_c) =
case+ request_info of
  | ~$CLOUDI.StringLiteral(s) => let
    val () = size_c := sz_u32(string_length(s))
in
    string2ptr(s)
end
  | ~$CLOUDI.PtrFree(request_info_c, size) => let
    val () = c_free_request_info(api_c)
    val () = size_c := size
in
    request_info_c
end

implement
callback_request
    (api_c,
     request,
     size_c) =
case+ request of
  | ~$CLOUDI.StringLiteral(s) => let
    val () = size_c := sz_u32(string_length(s))
in
    string2ptr(s)
end
  | ~$CLOUDI.PtrFree(request_c, size) => let
    val () = c_free_request(api_c)
    val () = size_c := size
in
    request_c
end

implement
callback_response_info
    (api_c,
     response_info,
     size_c) =
case+ response_info of
  | ~$CLOUDI.StringLiteral(s) => let
    val () = size_c := sz_u32(string_length(s))
in
    string2ptr(s)
end
  | ~$CLOUDI.PtrFree(response_info_c, size) => let
    val () = c_free_response_info(api_c)
    val () = size_c := size
in
    response_info_c
end

implement
callback_response
    (api_c,
     response,
     size_c) =
case+ response of
  | ~$CLOUDI.StringLiteral(s) => let
    val () = size_c := sz_u32(string_length(s))
in
    string2ptr(s)
end
  | ~$CLOUDI.PtrFree(response_c, size) => let
    val () = c_free_response(api_c)
    val () = size_c := size
in
    response_c
end

implement
api_c_get
    (api) = let
    val INSTANCE(@{api_c_ptr = (_, _ | api_c), ...}) = api
in
    api_c
end

fn
optional_request_info
    (opt: Option_vt($CLOUDI.memory_ptr)):<!wrt>
    $CLOUDI.memory_ptr = case+ opt of
  | Some_vt(_) =>
    option_vt_unsome(opt)
  | ~None_vt() =>
    $CLOUDI.Ptr(string2ptr(""), i_u32(0))

fn
optional_trans_id
    (opt: Option_vt($CLOUDI.trans_id)):<!wrt>
    $CLOUDI.trans_id = case+ opt of
  | Some_vt(_) =>
    option_vt_unsome(opt)
  | ~None_vt() =>
    $CLOUDI.trans_id_null()

implement {a}
result_value
    (value,
     status,
     api) =
if (status = c_int_success) then
    $CLOUDI.Ok(value)
else let
    val INSTANCE(@{terminate_return_value = terminate_return_value, ...}) = api
in
    if (terminate_return_value = false && status = c_int_terminate) then
        $raise $CLOUDI.Terminate()
    else
        $CLOUDI.Error(status)
end

implement
result_value_unit
    (status,
     api) = result_value<unit>(unit(), status, api)

implement {s}
callback_attach
    (callback,
     request_type,
     name_c,
     pattern_c,
     request_info_c,
     request_info_size_c,
     request_c,
     request_size_c,
     timeout_c,
     priority_c,
     trans_id_c,
     pid_c,
     pid_size_c,
     state_c,
     api_c) = let
    val name: string = $UNSAFE.castvwtp0{string}(name_c)
    val pattern: string = $UNSAFE.castvwtp0{string}(pattern_c)
    val request_info = $CLOUDI.Ptr($UNSAFE.cast2Ptr1(request_info_c),
                                   request_info_size_c)
    val request = $CLOUDI.Ptr($UNSAFE.cast2Ptr1(request_c),
                              request_size_c)
    val timeout = u32_timeout(timeout_c)
    val priority = i8_priority(priority_c)
    val trans_id_p = $UNSAFE.cast2Ptr1(trans_id_c)
    val trans_id = TransId(trans_id_p, false)
    val pid: Ptr1 = $UNSAFE.cast2Ptr1(pid_c)
    val source = $CLOUDI.Ptr(pid, pid_size_c)
    val api = $UNSAFE.castvwtp1{$CLOUDI.instance(s)}(state_c)
    val INSTANCE(@{state_p = state_p, ...}) = api
    val state = $UNSAFE.castvwtp1{$CLOUDI.stateptr(s)}(state_p)
    val callback_result = try callback(request_type, name, pattern,
                                       request_info, request,
                                       timeout, priority, trans_id,
                                       source, state, api) with
      | ~$CLOUDI.Terminate() =>
        $CLOUDI.Null()
      | ~$CLOUDI.FatalError() => let
        val () = exit_errmsg_void(1, $mylocation)
    in
        $CLOUDI.Null()
    end
      | e:exn => let
        val () = fprintln!(stderr_ref, $mylocation)
        prval () = __vfree_exn(e)
    in
        $CLOUDI.Null()
    end
    val ~TransId(_, owned) = trans_id
    val () = assertloc(owned = false)
    val ~$CLOUDI.Ptr(_, _) = source
    prval () = $UNSAFE.cast2void(state)
    prval () = $UNSAFE.cast2void(api)
in
    case+ callback_result of
      | ~$CLOUDI.Response(response) => let
        var response_info_size_c: uint32 = i_u32(0)
        val response_info_c = callback_response_info(api_c,
                                                     $CLOUDI.StringLiteral(""),
                                                     response_info_size_c)
        var response_size_c: uint32 = i_u32(0)
        val response_c = callback_response(api_c,
                                           response,
                                           response_size_c)
        val status = c_return(api_c, request_type,
                              string2ptr(name), string2ptr(pattern),
                              response_info_c, response_info_size_c,
                              response_c, response_size_c,
                              timeout_c, trans_id_p, pid, pid_size_c)
    in
        assertloc(status = 0)
    end
      | ~$CLOUDI.ResponseInfo(response_info, response) => let
        var response_info_size_c: uint32 = i_u32(0)
        val response_info_c = callback_response_info(api_c,
                                                     response_info,
                                                     response_info_size_c)
        var response_size_c: uint32 = i_u32(0)
        val response_c = callback_response(api_c,
                                           response,
                                           response_size_c)
        val status = c_return(api_c, request_type,
                              string2ptr(name), string2ptr(pattern),
                              response_info_c, response_info_size_c,
                              response_c, response_size_c,
                              timeout_c, trans_id_p, pid, pid_size_c)
    in
        assertloc(status = 0)
    end
      | ~$CLOUDI.Forward(name_new, request_info_new, request_new) => let
        val name_new_c = callback_name(api_c, name_new)
        var request_info_size_new_c: uint32 = i_u32(0)
        val request_info_new_c = callback_request_info(api_c,
                                                       request_info_new,
                                                       request_info_size_new_c)
        var request_size_new_c: uint32 = i_u32(0)
        val request_new_c = callback_request(api_c,
                                             request_new,
                                             request_size_new_c)
        val status = c_forward(api_c, request_type, name_new_c,
                               request_info_new_c, request_info_size_new_c,
                               request_new_c, request_size_new_c,
                               timeout_c, priority_c,
                               trans_id_p, pid, pid_size_c)
    in
        assertloc(status = 0)
    end
      | ~$CLOUDI.Forward_(name_new, request_info_new, request_new,
                          timeout_new, priority_new) => let
        val name_new_c = callback_name(api_c, name_new)
        var request_info_size_new_c: uint32 = i_u32(0)
        val request_info_new_c = callback_request_info(api_c,
                                                       request_info_new,
                                                       request_info_size_new_c)
        var request_size_new_c: uint32 = i_u32(0)
        val request_new_c = callback_request(api_c,
                                             request_new,
                                             request_size_new_c)
        val status = c_forward(api_c, request_type, name_new_c,
                               request_info_new_c, request_info_size_new_c,
                               request_new_c, request_size_new_c,
                               u_u32(timeout_new), i_i8(priority_new),
                               trans_id_p, pid, pid_size_c)
    in
        assertloc(status = 0)
    end
      | ~$CLOUDI.Null() => let
        var response_info_size_c: uint32 = i_u32(0)
        val response_info_c = callback_response_info(api_c,
                                                     $CLOUDI.StringLiteral(""),
                                                     response_info_size_c)
        var response_size_c: uint32 = i_u32(0)
        val response_c = callback_response(api_c,
                                           $CLOUDI.StringLiteral(""),
                                           response_size_c)
        val status = c_return(api_c, request_type,
                              string2ptr(name), string2ptr(pattern),
                              response_info_c, response_info_size_c,
                              response_c, response_size_c,
                              timeout_c, trans_id_p, pid, pid_size_c)
    in
        assertloc(status = 0)
    end
      | ~$CLOUDI.NullError(error) => let
        val () = fprintln!(stderr_ref, error)
        var response_info_size_c: uint32 = i_u32(0)
        val response_info_c = callback_response_info(api_c,
                                                     $CLOUDI.StringLiteral(""),
                                                     response_info_size_c)
        var response_size_c: uint32 = i_u32(0)
        val response_c = callback_response(api_c,
                                           $CLOUDI.StringLiteral(""),
                                           response_size_c)
        val status = c_return(api_c, request_type,
                              string2ptr(name), string2ptr(pattern),
                              response_info_c, response_info_size_c,
                              response_c, response_size_c,
                              timeout_c, trans_id_p, pid, pid_size_c)
    in
        assertloc(status = 0)
    end
end

implement
$CLOUDI.thread_count() = let
    var count: uintGt(0) with count_pfgc = i2u(1)
    val status = $effmask_wrt(
        c_initialize_thread_count(count_pfgc | addr@count))
    val () = assertloc(status = 0)
in
    count
end

implement {s}
$CLOUDI.new
    (thread_index,
     state_value,
     terminate_return_value) = let
    val (api_c_pfgc, api_c_pfat | api_c) = ptr_alloc<$CLOUDI.c_instance>()
    val status = c_initialize(api_c_pfgc | api_c, thread_index, the_null_ptr)
in
    if (status = c_int_success) then let
        val state_p: Ptr1 = state_p_new<s>(state_value)
        val api = INSTANCE(@{
            api_c_ptr = (api_c_pfgc, api_c_pfat | api_c),
            state_p = state_p,
            terminate_return_value = terminate_return_value})
        val () = c_set_c_state(api_c, $UNSAFE.castvwtp1{Ptr1}(api))
    in
        $CLOUDI.Ok(api)
    end
    else let
        val () = ptr_free(api_c_pfat, api_c_pfgc | api_c)
        prval () = $UNSAFE.cast2void(state_value)
    in
        $CLOUDI.Error(status)
    end
end

implement {s}
$CLOUDI.destroy
    (api) = let
    val ~INSTANCE(@{
        api_c_ptr = (api_c_pfgc, api_c_pfat | api_c),
        state_p = state_p, ...}) = api
    val _ = c_destroy(api_c_pfgc | api_c)
    val () = ptr_free(api_c_pfat, api_c_pfgc | api_c)
in
    state_p_destroy<s>(state_p)
end

implement {s}
$CLOUDI.destroy2void
    (api) = let
    val state_value: s = $CLOUDI.destroy<s>(api)
    prval () = $UNSAFE.cast2void(state_value)
in
    ()
end

implement {s}
$CLOUDI.subscribe
    (api,
     suffix) = let
    val api_c = api_c_get(api)
    fn
    f_wrapper
        (request_type: $CLOUDI.request_type,
         name_c: ptr,
         pattern_c: ptr,
         request_info_c: ptr,
         request_info_size_c: uint32,
         request_c: ptr,
         request_size_c: uint32,
         timeout_c: uint32,
         priority_c: int8,
         trans_id_c: ptr,
         pid_c: ptr,
         pid_size_c: uint32,
         state_c: ptr,
         f_api_c: ptr):<fun1>
        void =
        callback_attach<s>($CLOUDI.subscribe$function<s>(),
                           request_type, name_c, pattern_c,
                           request_info_c, request_info_size_c,
                           request_c, request_size_c,
                           timeout_c, priority_c, trans_id_c,
                           pid_c, pid_size_c, state_c, f_api_c)
    val f_c: $CLOUDI.c_callback = f_wrapper
in
    result_value_unit(c_subscribe(api_c, string2ptr(suffix), f_c), api)
end

implement
$CLOUDI.subscribe_count
    (api,
     suffix) = let
    val api_c = api_c_get(api)
    val status = c_subscribe_count(api_c, string2ptr(suffix))
    val value = c_get_subscribe_count(api_c)
in
    result_value<uint>(value, status, api)
end

implement
$CLOUDI.unsubscribe
    (api,
     suffix) = let
    val api_c = api_c_get(api)
in
    result_value_unit(c_unsubscribe(api_c, string2ptr(suffix)), api)
end

implement
$CLOUDI.send_async
    (api,
     name,
     request,
     timeout_opt,
     request_info_opt,
     priority_opt) = let
    val api_c = api_c_get(api)
    val name_c = string2ptr(name)
    val timeout_c = case+ timeout_opt of
      | ~Some_vt(timeout_value) =>
        u_u32(timeout_value)
      | ~None_vt() =>
        u_u32(c_get_timeout_async(api_c))
    val request_info = optional_request_info(request_info_opt)
    val priority_c = case+ priority_opt of
      | ~Some_vt(priority_value) =>
        i_i8(priority_value)
      | ~None_vt() =>
        c_get_priority_default(api_c)
    val ~$CLOUDI.Ptr(request_info_c, request_info_size_c) = request_info
    val ~$CLOUDI.Ptr(request_c, request_size_c) = request
    val status = c_send_async_(api_c, name_c,
                               request_info_c, request_info_size_c,
                               request_c, request_size_c,
                               timeout_c, priority_c)
in
    case+ result_value_unit(status, api) of
      | ~$CLOUDI.Ok(_) => let
        val () = assertloc(c_get_trans_id_count(api_c) = i2u(1))
    in
        $CLOUDI.Ok(TransId(c_get_trans_id(api_c, i2u(0)), false))
    end
      | ~$CLOUDI.Error(status_error) =>
        $CLOUDI.Error(status_error)
end

implement
$CLOUDI.send_sync
    (api,
     name,
     request,
     timeout_opt,
     request_info_opt,
     priority_opt) = let
    val api_c = api_c_get(api)
    val name_c = string2ptr(name)
    val timeout_c = case+ timeout_opt of
      | ~Some_vt(timeout_value) =>
        u_u32(timeout_value)
      | ~None_vt() =>
        u_u32(c_get_timeout_sync(api_c))
    val request_info = optional_request_info(request_info_opt)
    val priority_c = case+ priority_opt of
      | ~Some_vt(priority_value) =>
        i_i8(priority_value)
      | ~None_vt() =>
        c_get_priority_default(api_c)
    val ~$CLOUDI.Ptr(request_info_c, request_info_size_c) = request_info
    val ~$CLOUDI.Ptr(request_c, request_size_c) = request
    val status = c_send_sync_(api_c, name_c,
                              request_info_c, request_info_size_c,
                              request_c, request_size_c,
                              timeout_c, priority_c)
in
    case+ result_value_unit(status, api) of
      | ~$CLOUDI.Ok(_) => let
        val () = assertloc(c_get_trans_id_count(api_c) = i2u(1))
    in
        $CLOUDI.Ok(@($CLOUDI.Ptr(c_get_response_info(api_c),
                                 c_get_response_info_size(api_c)),
                     $CLOUDI.Ptr(c_get_response(api_c),
                                 c_get_response_size(api_c)),
                     TransId(c_get_trans_id(api_c, i2u(0)), false)))
    end
      | ~$CLOUDI.Error(status_error) =>
        $CLOUDI.Error(status_error)
end

implement
$CLOUDI.mcast_async
    (api,
     name,
     request,
     timeout_opt,
     request_info_opt,
     priority_opt) = let
    val api_c = api_c_get(api)
    val name_c = string2ptr(name)
    val timeout_c = case+ timeout_opt of
      | ~Some_vt(timeout_value) =>
        u_u32(timeout_value)
      | ~None_vt() =>
        u_u32(c_get_timeout_async(api_c))
    val request_info = optional_request_info(request_info_opt)
    val priority_c = case+ priority_opt of
      | ~Some_vt(priority_value) =>
        i_i8(priority_value)
      | ~None_vt() =>
        c_get_priority_default(api_c)
    val ~$CLOUDI.Ptr(request_info_c, request_info_size_c) = request_info
    val ~$CLOUDI.Ptr(request_c, request_size_c) = request
    val status = c_mcast_async_(api_c, name_c,
                                request_info_c, request_info_size_c,
                                request_c, request_size_c,
                                timeout_c, priority_c)
in
    case+ result_value_unit(status, api) of
      | ~$CLOUDI.Ok(_) => let
        implement
        array_initize$init<$CLOUDI.trans_id>
            (i,
             x) =
            x := TransId(c_get_trans_id(api_c, sz2u(i)), false)
        val size = u2sz(c_get_trans_id_count(api_c))
        val trans_ids = arrayptr_make_uninitized<$CLOUDI.trans_id>(size)
        val () = $effmask_all(
            arrayptr_initize<$CLOUDI.trans_id>(trans_ids, size))
    in
        $CLOUDI.Ok(@(trans_ids, size))
    end
      | ~$CLOUDI.Error(status_error) =>
        $CLOUDI.Error(status_error)
end

implement
$CLOUDI.forward_async
    (api,
     name,
     request_info,
     request,
     timeout,
     priority,
     trans_id,
     source) = let
    val api_c = api_c_get(api)
    val name_c = callback_name(api_c, name)
    var request_info_size_c: uint32 = i_u32(0)
    val request_info_c = $effmask_ref(
        callback_request_info(api_c, request_info, request_info_size_c))
    var request_size_c: uint32 = i_u32(0)
    val request_c = $effmask_ref(
        callback_request(api_c, request, request_size_c))
    val timeout_c = u_u32(timeout)
    val priority_c = i_i8(priority)
    val ~TransId(trans_id_c, owned) =
        $UNSAFE.castvwtp1{$CLOUDI.trans_id}(trans_id)
    val () = assertloc(owned = false)
    val ~$CLOUDI.Ptr(pid_c, pid_size_c) =
        $UNSAFE.castvwtp1{$CLOUDI.memory_ptr}(source)
    val status = c_forward_async(api_c, name_c,
                                 request_info_c, request_info_size_c,
                                 request_c, request_size_c,
                                 timeout_c, priority_c,
                                 trans_id_c, pid_c, pid_size_c)
in
    assertloc(status = 0)
end

implement
$CLOUDI.forward_sync
    (api,
     name,
     request_info,
     request,
     timeout,
     priority,
     trans_id,
     source) = let
    val api_c = api_c_get(api)
    val name_c = callback_name(api_c, name)
    var request_info_size_c: uint32 = i_u32(0)
    val request_info_c = $effmask_ref(
        callback_request_info(api_c, request_info, request_info_size_c))
    var request_size_c: uint32 = i_u32(0)
    val request_c = $effmask_ref(
        callback_request(api_c, request, request_size_c))
    val timeout_c = u_u32(timeout)
    val priority_c = i_i8(priority)
    val ~TransId(trans_id_c, owned) =
        $UNSAFE.castvwtp1{$CLOUDI.trans_id}(trans_id)
    val () = assertloc(owned = false)
    val ~$CLOUDI.Ptr(pid_c, pid_size_c) =
        $UNSAFE.castvwtp1{$CLOUDI.memory_ptr}(source)
    val status = c_forward_sync(api_c, name_c,
                                request_info_c, request_info_size_c,
                                request_c, request_size_c,
                                timeout_c, priority_c,
                                trans_id_c, pid_c, pid_size_c)
in
    assertloc(status = 0)
end

implement
$CLOUDI.forward
    (api,
     request_type,
     name,
     request_info,
     request,
     timeout,
     priority,
     trans_id,
     source) = let
    val api_c = api_c_get(api)
    val name_c = callback_name(api_c, name)
    var request_info_size_c: uint32 = i_u32(0)
    val request_info_c = $effmask_ref(
        callback_request_info(api_c, request_info, request_info_size_c))
    var request_size_c: uint32 = i_u32(0)
    val request_c = $effmask_ref(
        callback_request(api_c, request, request_size_c))
    val timeout_c = u_u32(timeout)
    val priority_c = i_i8(priority)
    val ~TransId(trans_id_c, owned) =
        $UNSAFE.castvwtp1{$CLOUDI.trans_id}(trans_id)
    val () = assertloc(owned = false)
    val ~$CLOUDI.Ptr(pid_c, pid_size_c) =
        $UNSAFE.castvwtp1{$CLOUDI.memory_ptr}(source)
    val status = c_forward(api_c, request_type, name_c,
                           request_info_c, request_info_size_c,
                           request_c, request_size_c,
                           timeout_c, priority_c,
                           trans_id_c, pid_c, pid_size_c)
in
    assertloc(status = 0)
end

implement
$CLOUDI.return_async
    (api,
     name,
     pattern,
     response_info,
     response,
     timeout,
     trans_id,
     source) = let
    val api_c = api_c_get(api)
    val name_c = string2ptr(name)
    val pattern_c = string2ptr(pattern)
    var response_info_size_c: uint32 = i_u32(0)
    val response_info_c = $effmask_ref(
        callback_response_info(api_c, response_info, response_info_size_c))
    var response_size_c: uint32 = i_u32(0)
    val response_c = $effmask_ref(
        callback_response(api_c, response, response_size_c))
    val timeout_c = u_u32(timeout)
    val ~TransId(trans_id_c, owned) =
        $UNSAFE.castvwtp1{$CLOUDI.trans_id}(trans_id)
    val () = assertloc(owned = false)
    val ~$CLOUDI.Ptr(pid_c, pid_size_c) =
        $UNSAFE.castvwtp1{$CLOUDI.memory_ptr}(source)
    val status = c_return_async(api_c, name_c, pattern_c,
                                response_info_c, response_info_size_c,
                                response_c, response_size_c,
                                timeout_c, trans_id_c, pid_c, pid_size_c)
in
    assertloc(status = 0)
end

implement
$CLOUDI.return_sync
    (api,
     name,
     pattern,
     response_info,
     response,
     timeout,
     trans_id,
     source) = let
    val api_c = api_c_get(api)
    val name_c = string2ptr(name)
    val pattern_c = string2ptr(pattern)
    var response_info_size_c: uint32 = i_u32(0)
    val response_info_c = $effmask_ref(
        callback_response_info(api_c, response_info, response_info_size_c))
    var response_size_c: uint32 = i_u32(0)
    val response_c = $effmask_ref(
        callback_response(api_c, response, response_size_c))
    val timeout_c = u_u32(timeout)
    val ~TransId(trans_id_c, owned) =
        $UNSAFE.castvwtp1{$CLOUDI.trans_id}(trans_id)
    val () = assertloc(owned = false)
    val ~$CLOUDI.Ptr(pid_c, pid_size_c) =
        $UNSAFE.castvwtp1{$CLOUDI.memory_ptr}(source)
    val status = c_return_sync(api_c, name_c, pattern_c,
                               response_info_c, response_info_size_c,
                               response_c, response_size_c,
                               timeout_c, trans_id_c, pid_c, pid_size_c)
in
    assertloc(status = 0)
end

implement
$CLOUDI.return
    (api,
     request_type,
     name,
     pattern,
     response_info,
     response,
     timeout,
     trans_id,
     source) = let
    val api_c = api_c_get(api)
    val name_c = string2ptr(name)
    val pattern_c = string2ptr(pattern)
    var response_info_size_c: uint32 = i_u32(0)
    val response_info_c = $effmask_ref(
        callback_response_info(api_c, response_info, response_info_size_c))
    var response_size_c: uint32 = i_u32(0)
    val response_c = $effmask_ref(
        callback_response(api_c, response, response_size_c))
    val timeout_c = u_u32(timeout)
    val ~TransId(trans_id_c, owned) =
        $UNSAFE.castvwtp1{$CLOUDI.trans_id}(trans_id)
    val () = assertloc(owned = false)
    val ~$CLOUDI.Ptr(pid_c, pid_size_c) =
        $UNSAFE.castvwtp1{$CLOUDI.memory_ptr}(source)
    val status = c_return(api_c, request_type, name_c, pattern_c,
                          response_info_c, response_info_size_c,
                          response_c, response_size_c,
                          timeout_c, trans_id_c, pid_c, pid_size_c)
in
    assertloc(status = 0)
end

implement
$CLOUDI.recv_async
    (api,
     timeout_opt,
     trans_id_opt,
     consume_opt) = let
    val api_c = api_c_get(api)
    val timeout_c = case+ timeout_opt of
      | ~Some_vt(timeout_value) =>
        u_u32(timeout_value)
      | ~None_vt() =>
        u_u32(c_get_timeout_sync(api_c))
    val trans_id = optional_trans_id(trans_id_opt)
    val consume_c = case+ consume_opt of
      | ~Some_vt(consume_value) =>
        bool2int(consume_value)
      | ~None_vt() =>
        bool2int(true)
    val TransId(trans_id_c, _) = trans_id
    val status = c_recv_async(api_c, timeout_c, trans_id_c, consume_c)
    val () = $CLOUDI.trans_id_free(trans_id)
in
    case+ result_value_unit(status, api) of
      | ~$CLOUDI.Ok(_) => let
        val () = assertloc(c_get_trans_id_count(api_c) = i2u(1))
    in
        $CLOUDI.Ok(@($CLOUDI.Ptr(c_get_response_info(api_c),
                                 c_get_response_info_size(api_c)),
                     $CLOUDI.Ptr(c_get_response(api_c),
                                 c_get_response_size(api_c)),
                     TransId(c_get_trans_id(api_c, i2u(0)), false)))
    end
      | ~$CLOUDI.Error(status_error) =>
        $CLOUDI.Error(status_error)
end

implement
$CLOUDI.process_index
    (api) = let
    val api_c = api_c_get(api)
in
    c_get_process_index(api_c)
end

implement
$CLOUDI.process_count
    (api) = let
    val api_c = api_c_get(api)
in
    c_get_process_count(api_c)
end

implement
$CLOUDI.process_count_max
    (api) = let
    val api_c = api_c_get(api)
in
    c_get_process_count_max(api_c)
end

implement
$CLOUDI.process_count_min
    (api) = let
    val api_c = api_c_get(api)
in
    c_get_process_count_min(api_c)
end

implement
$CLOUDI.prefix_
    (api) = let
    val api_c = api_c_get(api)
in
    $UNSAFE.castvwtp0{string}(c_get_prefix(api_c))
end

implement
$CLOUDI.timeout_initialize
    (api) = let
    val api_c = api_c_get(api)
in
    c_get_timeout_initialize(api_c)
end

implement
$CLOUDI.timeout_async
    (api) = let
    val api_c = api_c_get(api)
in
    c_get_timeout_async(api_c)
end

implement
$CLOUDI.timeout_sync
    (api) = let
    val api_c = api_c_get(api)
in
    c_get_timeout_sync(api_c)
end

implement
$CLOUDI.timeout_terminate
    (api) = let
    val api_c = api_c_get(api)
in
    c_get_timeout_terminate(api_c)
end

implement
$CLOUDI.priority_default
    (api) = let
    val api_c = api_c_get(api)
in
    i8_priority(c_get_priority_default(api_c))
end

implement
$CLOUDI.poll
    (api,
     timeout) = let
    val api_c = api_c_get(api)
    val status = c_poll(api_c, timeout)
in
    if (status = c_int_timeout) then
        $CLOUDI.Ok(true)
    else if (status = c_int_success) then
        $CLOUDI.Ok(false)
    else
        $CLOUDI.Error(status)
end

implement
$CLOUDI.shutdown
    (api,
     reason) = let
    val api_c = api_c_get(api)
    val reason_c = case+ reason of
      | ~Some_vt(s) =>
        string2ptr(s)
      | ~None_vt() =>
        the_null_ptr
in
    result_value_unit(c_shutdown(api_c, reason_c), api)
end

fn
text_pairs_parse
    (info: $CLOUDI.memory_ptr):<!wrt>
    [l:addr][n:int]
    @(arrayptr(string, l, n), size_t(n)) = let
    val ~$CLOUDI.Ptr(text_p, text_size) = info
in
    if (text_size > i_u32(1)) then let
        val text_size_pred = g1ofg0(pred(u32_sz(text_size)))
        fun
        segment_count {i,ni:nat | i <= ni} .<i>.
            (count: size_t,
             p: Ptr1,
             p_i: size_t(i),
             p_n: size_t(ni)):<!wrt>
            [n:int]
            size_t(n) = if (p_i > 0) then let
            val p_next = $UNSAFE.cast2Ptr1(ptr1_succ<char>(p))
            val p_i_next = p_i - i2sz(1)
            val store_next = g0uint_iseqz_uint8($UNSAFE.ptr1_get<uint8>(p))
            val count_next = if (store_next || (p_i = p_n)) then
                count + i2sz(1)
            else
                count
        in
            segment_count(count_next, p_next, p_i_next, p_n)
        end
        else
            g1ofg0(count)
        fun
        segment_store {i,ni:nat | i <= ni} .<i>.
            (pairs_p: ptr,
             p: Ptr1,
             p_i: size_t(i),
             p_n: size_t(ni)):<!wrt>
            void = if (p_i > 0) then let
            val p_next = $UNSAFE.cast2Ptr1(ptr1_succ<char>(p))
            val p_i_next = p_i - i2sz(1)
            val store_next = g0uint_iseqz_uint8($UNSAFE.ptr1_get<uint8>(p))
            fn
            segment_store_text
                (pairs_p_incr: ptr,
                 segment: ptr):<!wrt>
                ptr = let
                val () = $UNSAFE.ptr0_set<ptr>(pairs_p_incr, segment)
            in
                $UNSAFE.cast2ptr(ptr0_succ<ptr>(pairs_p_incr))
            end
            val pairs_p_next = if (store_next) then
                segment_store_text(pairs_p, $UNSAFE.castvwtp1{ptr}(p_next))
            else if (p_i = p_n) then
                segment_store_text(pairs_p, $UNSAFE.castvwtp1{ptr}(p))
            else
                pairs_p
        in
            segment_store(pairs_p_next, p_next, p_i_next, p_n)
        end
        else
            ()
        val size = segment_count(i2sz(1), text_p,
                                 text_size_pred, text_size_pred)
        val pairs = arrayptr_make_elt<string>(size, "")
        val () = segment_store(ptrcast(pairs), text_p,
                               text_size_pred, text_size_pred)
    in
        @(pairs, size)
    end
    else let
        val size = i2sz(2)
        val pairs = arrayptr_make_elt<string>(size, "")
    in
        @(pairs, size)
    end
end

implement
$CLOUDI.info_key_value_parse
    (info) = text_pairs_parse(info)

fn
text_pairs_new {l:addr}{n:int}
    (pairs: arrayptr(string, l, n),
     size: size_t(n),
     response_opt: Option_vt(bool)):<!wrt>
    $CLOUDI.memory_free_ptr = let
    val response_format = case+ response_opt of
      | ~Some_vt(response_value) =>
        response_value
      | ~None_vt() =>
        true
in
    if (size > i2sz(0)) then let
        fun
        text_size_total {i,ni:nat | i <= ni} .<i>.
            (total: size_t,
             p: ptr,
             p_i: size_t(i),
             p_n: size_t(ni)):<!wrt>
            [nt:int]
            size_t(nt) = if (p_i > 0) then let
            val s = $UNSAFE.ptr0_get<string>(p)
            val s_size = string_length(s) + i2sz(1)
            val p_next = ptr0_succ<string>(p)
        in
            text_size_total(total + s_size, p_next, p_i - i2sz(1), p_n)
        end
        else
            g1ofg0(total)
        val text_size = text_size_total(i2sz(0), ptrcast(pairs), size, size)
        val (_, _ | text_p) = malloc_gc(text_size)
        fun
        text_store {i,ni:nat | i <= ni} .<i>.
            (p_destination: Ptr1,
             p_source: ptr,
             p_i: size_t(i),
             p_n: size_t(ni)):<!wrt>
            void = if (p_i > 0) then let
            val s = $UNSAFE.ptr0_get<string>(p_source)
            val s_size = g1ofg0(string_length(s) + i2sz(1))
            val _ = c_memcpy(p_destination, string2ptr(s), s_size)
            val p_destination_next = add_ptr1_bsz(p_destination, s_size)
            val p_source_next = ptr0_succ<string>(p_source)
        in
            text_store(p_destination_next, p_source_next, p_i - i2sz(1), p_n)
        end
        else
            ()
        val () = text_store(text_p, ptrcast(pairs), size, size)
        val () = arrayptr_free(pairs)
    in
        $CLOUDI.PtrFree(text_p, sz_u32(text_size))
    end
    else let
        val () = arrayptr_free(pairs)
    in
        if (response_format) then let
            val text_str = string0_copy("")
            val text_ptr: Ptr1 = strptr2ptr(text_str)
            prval () = $UNSAFE.cast2void(text_str)
        in
            $CLOUDI.PtrFree(text_ptr, i_u32(1))
        end
        else
            $CLOUDI.StringLiteral("")
    end
end

implement
$CLOUDI.info_key_value_new
    (pairs,
     size,
     response_opt) = text_pairs_new(pairs, size, response_opt)

implement
$CLOUDI.info_key_value_new1
    (key0,
     value0) = let
    val size = i2sz(2)
    val pairs = arrayptr_make_uninitized<string>(size)
    implement
    array_initize$init<string>
        (i,
         x) = case- sz2i(i) of
      | 0 => x := key0
      | 1 => x := value0
    val () = $effmask_all(arrayptr_initize<string>(pairs, size))
in
    text_pairs_new(pairs, size, None_vt())
end

implement
$CLOUDI.info_key_value_new2
    (key0,
     value0,
     key1,
     value1) = let
    val size = i2sz(4)
    val pairs = arrayptr_make_uninitized<string>(size)
    implement
    array_initize$init<string>
        (i,
         x) = case- sz2i(i) of
      | 0 => x := key0
      | 1 => x := value0
      | 2 => x := key1
      | 3 => x := value1
    val () = $effmask_all(arrayptr_initize<string>(pairs, size))
in
    text_pairs_new(pairs, size, None_vt())
end

implement
$CLOUDI.info_key_value_new3
    (key0,
     value0,
     key1,
     value1,
     key2,
     value2) = let
    val size = i2sz(6)
    val pairs = arrayptr_make_uninitized<string>(size)
    implement
    array_initize$init<string>
        (i,
         x) = case- sz2i(i) of
      | 0 => x := key0
      | 1 => x := value0
      | 2 => x := key1
      | 3 => x := value1
      | 4 => x := key2
      | 5 => x := value2
    val () = $effmask_all(arrayptr_initize<string>(pairs, size))
in
    text_pairs_new(pairs, size, None_vt())
end

fn
threads_new
    (thread_count: uintGt(0)):
    $CLOUDI.threads = let
    val mutex: $ATHREAD.mutex1 = $ATHREAD.mutex_create_exn()
    val condvar: $ATHREAD.condvar1 = $ATHREAD.condvar_create_exn()
in
    THREADS(@{
        thread_count = thread_count,
        running = 0,
        mutex = $ATHREAD.unsafe_mutex_t2vt(mutex),
        condvar = $ATHREAD.unsafe_condvar_t2vt(condvar)})
end

fn
thread
    (threads: $CLOUDI.threads,
     thread_index: uint,
     f: (uint) -<fun1> void):
    void = let
    val () = f(thread_index)
    val @THREADS(t) = threads
    val mutex = $ATHREAD.unsafe_mutex_vt2t(t.mutex)
    val condvar = $ATHREAD.unsafe_condvar_vt2t(t.condvar)
    val (lock | ()) = $ATHREAD.mutex_lock(mutex)
    val running = t.running
    val () = if (running >= 1) then
            t.running := running + ~1
        else
            assertloc(false)
    prval () = fold@(threads)
    prval () = $UNSAFE.cast2void(threads)
    val () = $ATHREAD.condvar_signal(condvar)
    val () = $ATHREAD.mutex_unlock(lock | mutex)
in
    ()
end

fun
threads_add
    (threads: !$CLOUDI.threads,
     f: (uint) -<fun1> void):
    void = let
    val @THREADS(t) = threads
    val mutex = $ATHREAD.unsafe_mutex_vt2t(t.mutex)
    val (lock | ()) = $ATHREAD.mutex_lock(mutex)
    val thread_index: uint = i2u(t.running)
    val () = t.running := t.running + 1
    val done: bool = t.thread_count = t.running
    val () = $ATHREAD.mutex_unlock(lock | mutex)
    prval () = fold@(threads)
    val threads_ref = $UNSAFE.castvwtp1{$CLOUDI.threads}(threads)
    val _ = $ATHREAD.athread_create_cloptr_exn(
        llam() => thread(threads_ref, thread_index, f))
in
    if (done) then
        ()
    else
        threads_add(threads, f)
end

fun
threads_remove
    (threads: !$CLOUDI.threads):
    void = let
    val @THREADS(t) = threads
    val mutex = $ATHREAD.unsafe_mutex_vt2t(t.mutex)
    val condvar = $ATHREAD.unsafe_condvar_vt2t(t.condvar)
    val (lock | ()) = $ATHREAD.mutex_lock(mutex)
    val done: bool = if (t.running > 0) then let
            val () = $ATHREAD.condvar_wait(lock | condvar, mutex)
        in
            t.running = 0
        end
        else
            true
    prval () = fold@(threads)
    val () = $ATHREAD.mutex_unlock(lock | mutex)
in
    if (done) then
        ()
    else
        threads_remove(threads)
end

fn
threads_destroy
    (threads: $CLOUDI.threads):
    void = let
    val ~THREADS(@{mutex = mutex, condvar = condvar, ...}) = threads
    val () = $ATHREAD.mutex_vt_destroy(mutex)
    val () = $ATHREAD.condvar_vt_destroy(condvar)
in
    ()
end

implement
$CLOUDI.threads_create
    (thread_count,
     f) = let
    val threads = $effmask_all(threads_new(thread_count))
    val () = $effmask_all(threads_add(threads, f))
in
    threads
end

implement
$CLOUDI.threads_wait
    (threads) = let
    val () = $effmask_all(threads_remove(threads))
    val () = $effmask_all(threads_destroy(threads))
in
    (* yield to ensure thread memory is freed and not seen by valgrind
     * ($ATHREAD only provides detached pthreads)
     *)
    c_threads_yield()
end
